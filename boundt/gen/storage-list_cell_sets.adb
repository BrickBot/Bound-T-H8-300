-- Storage.List_Cell_Sets (body)
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
-- $Revision: 1.2 $
-- $Date: 2015/10/24 20:05:52 $
--
-- $Log: storage-list_cell_sets.adb,v $
-- Revision 1.2  2015/10/24 20:05:52  niklas
-- Moved to free licence.
--
-- Revision 1.1  2009-10-07 19:26:10  niklas
-- BT-CH-0183: Cell-sets are a tagged-type class.
--


package body Storage.List_Cell_Sets is


   use Cell_Vectors;


   -- overriding
   function Empty return Set_T
   is
   begin

      return (Root_Cell_Set_T with List => Empty_List);

   end Empty;


   -- overriding
   procedure Erase (Set : in out Set_T)
   is
   begin

      Erase (Set.List);

   end Erase;


   -- overriding
   function Is_Empty (Item : Set_T) return Boolean
   is
   begin

      return Length (Item.List) = 0;

   end Is_Empty;


   -- overriding
   function Is_Member (
      Cell   : Cell_T;
      Of_Set : Set_T)
   return Boolean
   is
   begin

      return Is_Element (Of_Set.List, Cell);

   end Is_Member;


   -- overriding
   function Card (Set : Set_T) return Natural
   is
   begin

      return Length (Set.List);

   end Card;


   -- overriding
   function To_Set (List : Cell_List_T) return Set_T
   is

      Result : Set_T;
      -- The result, default initialized to empty set.

   begin

      for L in reverse List'Range loop

         Set (
            Vector => Result.List,
            Index  => L - List'First + 1,
            To     => List(L));

      end loop;

      return Result;

   end To_Set;


   -- overriding
   function To_List (Set : Set_T) return Cell_List_T
   is
   begin

      return To_Vector (Set.List);

   end To_List;


   -- overriding
   procedure Add (
      Cell : in     Cell_T;
      To   : in out Set_T)
   is
   begin

      Add (Value => Cell, To => To.List);

   end Add;


   -- overriding
   procedure Add (
      Cell  : in     Cell_T;
      To    : in out Set_T;
      Added : in out Boolean)
   is

      Orig_Length : constant Natural := Length (To.List);

   begin

      Add (Value => Cell, To => To.List);

      if Length (To.List) > Orig_Length then

         Added := True;

      end if;

   end Add;


   -- overriding
   procedure Remove (
      Cell : in     Cell_T;
      From : in out Set_T)
   is

      I : constant Natural := Index (From.List, Cell);
      -- The index of the Cell in the From set, if present,
      -- otherwise > Last (From).

   begin

      if I <= Last (From.List) then

         Drop (I, From.List);

      end if;

   end Remove;


   -- overriding
   procedure Move_Cells (
      From : in out Set_T;
      To   : in out Set_T)
   is
   begin

      Move_All (From => From.List, To => To.List);

   end Move_Cells;


   -- overriding
   function Copy (Item : Cell_Set_T) return Set_T
   is
   begin

      if Item in Set_T'Class then
         -- A plain copy without type conversion.

         return Set_T (Item);

      else
         -- A type conversion.

         return To_Set (To_List (Item));

      end if;

   end Copy;


   -- overriding
   function "=" (Left, Right : Set_T) return Boolean
   is
   begin

      return Left.List = Right.List;

   end "=";


   procedure Print_Debug_Pool
   is
   begin

      Cell_Vectors.Print_Debug_Pool;

   end Print_Debug_Pool;


end Storage.List_Cell_Sets;
