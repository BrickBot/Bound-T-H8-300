-- Storage.Cell_Numbering (body)
--
-- A component of the Bound-T Worst-Case Execution Time Tool.
--
-------------------------------------------------------------------------------
-- Copyright (c) 1999 .. 2015 Tidorum Ltd
-- All rights reserved.
--
-- Redistribution and use in source and binary forms, with or without
-- modification, are permitted provided that the following conditions are met:
--
-- 1. Redistributions of source code must retain the above copyright notice, this
--    list of conditions and the following disclaimer.
-- 2. Redistributions in binary form must reproduce the above copyright notice,
--    this list of conditions and the following disclaimer in the documentation
--    and/or other materials provided with the distribution.
--
-- This software is provided by the copyright holders and contributors "as is" and
-- any express or implied warranties, including, but not limited to, the implied
-- warranties of merchantability and fitness for a particular purpose are
-- disclaimed. In no event shall the copyright owner or contributors be liable for
-- any direct, indirect, incidental, special, exemplary, or consequential damages
-- (including, but not limited to, procurement of substitute goods or services;
-- loss of use, data, or profits; or business interruption) however caused and
-- on any theory of liability, whether in contract, strict liability, or tort
-- (including negligence or otherwise) arising in any way out of the use of this
-- software, even if advised of the possibility of such damage.
--
-- Other modules (files) of this software composition should contain their
-- own copyright statements, which may have different copyright and usage
-- conditions. The above conditions apply to this file.
-------------------------------------------------------------------------------
--
-- $Revision: 1.7 $
-- $Date: 2015/10/24 20:05:51 $
--
-- $Log: storage-cell_numbering.adb,v $
-- Revision 1.7  2015/10/24 20:05:51  niklas
-- Moved to free licence.
--
-- Revision 1.6  2013-02-12 08:47:20  niklas
-- BT-CH-0245: Global volatile cells and "volatile" assertions.
--
-- Revision 1.5  2009-10-07 19:26:10  niklas
-- BT-CH-0183: Cell-sets are a tagged-type class.
--
-- Revision 1.4  2007/03/29 15:18:04  niklas
-- BT-CH-0056.
--
-- Revision 1.3  2007/01/25 21:25:19  niklas
-- BT-CH-0043.
--
-- Revision 1.2  2005/02/16 21:11:49  niklas
-- BT-CH-0002.
--
-- Revision 1.1  2004/04/25 09:33:22  niklas
-- First version.
--


with Ada.Text_IO;
with Unchecked_Deallocation;

with Faults;
with Storage.Cell_Numbering.Opt;
with Storage.Cell_Store;


package body Storage.Cell_Numbering is


   function Map (Cells : Cell_Set_T) return Map_T
   is

      Cell_List : constant Cell_List_T := To_List (Cells);
      -- The given Cells, as a list.

      Result : Map_T;
      -- The result.

   begin

      Result.Numbers := new Numbers_T (1 .. Cell_Index_T (Number_Of_Cells));
      --
      -- The conversion of Number_Of_Cells to Cell_Index_T will fail
      -- with constraint-error if no cells are defined at all. But
      -- that is a very unlikely case.

      -- Initialize Numbers to No_Number:

      for N in Result.Numbers'Range loop

         Result.Numbers(N) := No_Number;

      end loop;

      -- Create and enter the numbers of the Cells according to their
      -- order in Cell_List (which may be arbitrary with respect to
      -- the cell index, or related to it):

      Result.Last := 0;

      for C in Cell_List'Range loop

         Result.Last := Result.Last + 1;

         Result.Numbers(Index (Cell_List(C))) := Result.Last;

      end loop;

      return Result;

   end Map;


   function Numbered (Cell : Cell_T; Under : Map_T) return Boolean
   is
   begin

      return   Cell.Index in Under.Numbers'Range
      and then Under.Numbers(Cell.Index) /= No_Number;

   end Numbered;


   function Number (Cell : Cell_T; Under : Map_T) return Number_T
   is
   begin

      if Cell.Index not in Under.Numbers'Range
      or else Under.Numbers(Cell.Index) = No_Number
      then

         raise Not_Numbered;

      else

         return Under.Numbers(Cell.Index);

      end if;

   end Number;


   function First (Under : Map_T) return Number_T
   is
   begin

      return 1;

   end First;


   function Last  (Under : Map_T) return Number_T
   is
   begin

      return Under.Last;

   end Last;


   procedure Unchecked_Discard
   is new Unchecked_Deallocation (
      Object => Numbers_T,
      Name   => Numbers_Ref);


   procedure Discard (Map : in out Map_T)
   is
   begin

      if Opt.Deallocate then

         Unchecked_Discard (Map.Numbers);

      else

         Map.Numbers := null;

      end if;

   exception when others => Faults.Deallocation;

   end Discard;


   function Inverse (Map : Map_T) return List_T
   is

      Result : List_T (First(Map) .. Last(Map));
      -- The result.

      Number : Number_T;
      -- The number assigned to a cell.

   begin

      for M in Map.Numbers'Range loop

         Number := Map.Numbers(M);

         if Number /= No_Number then

            Result(Number) := Cell_Store.Cell_At (M);

         end if;

      end loop;

      return Result;

   end Inverse;


   procedure Show (Map : in Map_T)
   is
      use Ada.Text_IO;

      Cell_Num : List_T := Inverse (Map);
      -- Maps a cell number to the cell.

      Cell_Name_Col : constant := 10;
      -- Column for cell names, in cell-number table.

      Cell_Volatility_Col : constant := 30;
      -- Column for marking volatile cells, in cell-number table.

   begin

      New_Line;

      Put ("Cell#");
      Set_Col (Cell_Name_Col); Put ("Cell");
      Set_Col (Cell_Volatility_Col); Put ("Volatile?");
      New_Line;

      for C in Cell_Num'Range loop

         Put (Number_T'Image(C));
         Set_Col (Cell_Name_Col);
         Put (Image (Cell_Num (C)));

         if Is_Volatile (Cell_Num(C)) then

            Set_Col (Cell_Volatility_Col);
            Put ("volatile");

         end if;

         New_Line;

      end loop;

      New_Line;

   end Show;


end Storage.Cell_Numbering;
