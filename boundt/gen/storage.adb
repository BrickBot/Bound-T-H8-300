-- Storage (body)
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
-- $Revision: 1.23 $
-- $Date: 2015/10/24 20:05:52 $
--
-- $Log: storage.adb,v $
-- Revision 1.23  2015/10/24 20:05:52  niklas
-- Moved to free licence.
--
-- Revision 1.22  2014/06/28 10:01:53  niklas
-- Added function Rainge() return Code_Address_Range_T.
--
-- Revision 1.21  2014/06/11 12:48:55  niklas
-- Moved Code_Address_Range_T and its operations to package Processor,
-- making this type target-specific. Reason: in Bound-T/OCL, Code_Address_T
-- is not scalar, therefore it has no 'First, 'Last, and other
-- necessary operations. Retained renamings of the most important
-- items here, to minimize changes to the interface.
--
-- Revision 1.20  2013-02-12 08:47:20  niklas
-- BT-CH-0245: Global volatile cells and "volatile" assertions.
--
-- Revision 1.19  2011-09-09 14:51:51  niklas
-- Added function Initial_Cell_Variables, for use in ALF export.
--
-- Revision 1.18  2011-09-06 17:40:33  niklas
-- Added Cell_At (Index), for use in ALF export.
--
-- Revision 1.17  2009-11-27 11:28:08  niklas
-- BT-CH-0184: Bit-widths, Word_T, failed modular analysis.
--
-- Revision 1.16  2009-10-07 19:26:10  niklas
-- BT-CH-0183: Cell-sets are a tagged-type class.
--
-- Revision 1.15  2008-06-18 20:52:57  niklas
-- BT-CH-0130: Data pointers relative to initial-value cells.
--
-- Revision 1.14  2007/12/17 13:54:41  niklas
-- BT-CH-0098: Assertions on stack usage and final stack height, etc.
--
-- Revision 1.13  2007/08/20 12:14:07  niklas
-- Support for Storage.Data: Added Image functions for Cell_Value_T and
-- Cell_Value_List_T, and added function Cells_Of (Cell_Value_List_T).
--
-- Revision 1.12  2007/03/29 15:18:04  niklas
-- BT-CH-0056.
--
-- Revision 1.11  2007/03/23 21:05:36  niklas
-- Added Small_Cell_Set_T operations Is_Empty, Remove (Cell_T),
-- Remove (Small_Cell_Set_T), Move_Cells, Image, and Discard.
--
-- Revision 1.10  2007/01/25 21:25:20  niklas
-- BT-CH-0043.
--
-- Revision 1.9  2007/01/21 19:31:48  niklas
-- BT-CH-0042.
--
-- Revision 1.8  2007/01/13 13:51:07  niklas
-- BT-CH-0041.
--
-- Revision 1.7  2006/08/22 12:08:20  niklas
-- Added the function Is_None, to check if a Cell_Set_T is
-- uninitialized or otherwise equal to No_Cell_Set (null).
-- Modified Image (Cell_Set_T) to accept No_Cell_Set and
-- display it as "[not defined]".
--
-- Revision 1.6  2005/09/17 06:15:53  niklas
-- Added function Cell_List (From, Except) to list the cells in a
-- difference set without constructing the difference set itself.
--
-- Revision 1.5  2005/09/16 12:57:41  niklas
-- Worked around the GNAT 3.15p problem in predefined "=" for
-- packed bit-vectors, by giving an overriding "=" operation
-- for Cell_Bitset_T.
--
-- Revision 1.4  2005/09/14 11:48:25  niklas
-- As an addition to BT-CH-0008, modified (now, corrected) the Image
-- function for Cell_Set_T not to compare the Item to No_Cell_Set
-- (because this uses the overridden "=" operator which no longer
-- works for null references like No_Cell_Set). Instead, Image
-- assumes that the Item is not null, as currently required of users
-- of Cell_Set_T.
--
-- Revision 1.3  2005/09/12 19:03:00  niklas
-- BT-CH-0008.
--
-- Revision 1.2  2005/02/16 21:11:49  niklas
-- BT-CH-0002.
--
-- Revision 1.1  2004/04/25 09:33:06  niklas
-- First Tidorum version.
--


with Ada.Tags;
with Output;
with Storage.Cell_Store;
with Storage.Opt;
with Storage.Volatiles;


package body Storage is


   --
   ---   Cells
   --


   --
   ---   Aliasing groups
   --


   --
   ---  Creating cells. Attributes of cells.
   --


   function Cell (
      Spec              : Processor.Cell_Spec_T;
      Initial_Value_For : Cell_T)
   return Cell_T
   is
   begin

      return Cell_Store.Cell_By_Spec (Spec, Initial_Value_For);

   end Cell;


   function Cell (Spec : Processor.Cell_Spec_T) return Cell_T
   is
   begin

      return Cell_Store.Cell_By_Spec (Spec, No_Cell);

   end Cell;


   function Number_Of_Cells return Natural
   is
   begin

      return Cell_Store.Number_Of_Cells;

   end Number_Of_Cells;


   function Spec_Of (Cell : Cell_T) return Processor.Cell_Spec_T
   is
   begin

      return Cell.Spec.all;

   end Spec_Of;


   function Width_Of (Cell : Cell_T) return Width_T
   is
   begin

      return Cell.Width;

   end Width_Of;


   function Counter_Cell (Cell : Cell_T) return Boolean
   is
   begin

      return Cell.Counter;

   end Counter_Cell;


   function Alias_Group (Cell : Cell_T) return Processor.Alias_Group_T
   is
   begin

      return Cell.Alias_Group;

   end Alias_Group;


   function Alias_Range (Cell : Cell_T) return Processor.Alias_Range_T
   is
   begin

      return Cell.Alias_Range;

   end Alias_Range;


   function May_Alias (This, That : Cell_T) return Boolean
   is
      use type Processor.Alias_Group_T;
   begin

      if This.Alias_Group = That.Alias_Group then

         return This = That;

      else

         return May_Alias (This.Alias_Range, That.Alias_Range);

      end if;

   end May_Alias;


   function Is_Initial (Cell : Cell_T) return Boolean
   is
   begin

      return Cell.Is_Initial;

   end Is_Initial;


   function Initial_Cell (Cell : Cell_T) return Cell_T
   is
   begin

      return Cell.Initial_Cell;

   end Initial_Cell;


   procedure Mark_As_Volatile (Cell : Cell_T)
   is
   begin

      if Cell.Volatile then
         -- Double marking!

         Output.Warning (
              "Marking the cell "
            & Name_Of (Cell)
            & " as volatile - again.");

      elsif Cell.Is_Initial then

         Output.Fault (
            Location => "Storage.Mark_As_Volatile",
            Text     =>
                 "Attempt to mark the initial-value cell "
               & Name_Of (Cell)
               & " as volatile.");

      else

         Cell.Volatile := True;

         if Opt.Trace_Volatile then

            Output.Trace (
                 "Volatile cell"
               & Output.Field_Separator
               & Name_Of (Cell));

         end if;

         if Cell.Counter then

            Output.Note (
                 "Volatile cell "
               & Name_Of (Cell)
               & " cannot be a counter.");

            Cell.Counter := False;

         end if;

      end if;

   end Mark_As_Volatile;


   procedure Mark_Range_As_Volatile (
      From, To    : in     Processor.Cell_Spec_T;
      Valid_Range :    out Boolean)
   renames Volatiles.Mark_Range;


   function Is_Volatile (Cell : Cell_T) return Boolean
   is
   begin

      return Cell.Volatile;

   end Is_Volatile;


   function Number_Of_Volatile_Cells return Natural
   is
   begin

      return Cell_Store.Number_Of_Volatile_Cells;

   end Number_Of_Volatile_Cells;


   function Volatile_Index (Cell : Cell_T) return Volatile_Cell_Index_T
   is
   begin

      if not Cell.Volatile then

         raise Not_A_Volatile_Cell;

      end if;

      return Cell.Vol_Index;

   end Volatile_Index;


   function Cell (Name : String) return Cell_T
   is
   begin

      return Cell_Store.Cell_By_Name (Name);

   end Cell;


   function Name_Of (Cell : Cell_T) return String
   is
   begin

      return Cell.Name.all;

   end Name_Of;


   function Index (Cell : Cell_T) return Cell_Index_T
   is
   begin

      return Cell.Index;

   end Index;


   function Cell_At (Index : Cell_Index_T) return Cell_T
   renames Storage.Cell_Store.Cell_At;



   function Image (Item : Cell_T) return String
   renames Name_Of;


   --
   ---  Cell lists
   --


   function Null_Cell_List return Cell_List_T
   is
   begin

      return (1 .. 0 => No_Cell);

   end Null_Cell_List;


   function Is_Member (
      Cell    : Cell_T;
      Of_List : Cell_List_T)
   return Boolean
   is
   begin

      for I in Of_List'Range loop

         if Of_List(I) = Cell then

            return True;

         end if;

      end loop;

      return False;

   end Is_Member;


   function Counter_Cells (List : Cell_List_T)
   return Cell_List_T
   is

      Counters : Cell_List_T (1 .. List'Length);
      Last     : Natural := 0;
      -- The result will be Counters(1 .. Last).

   begin

      for C in List'Range loop

         if List(C).Counter then

            Last := Last + 1;

            Counters(Last) := List(C);

         end if;

      end loop;

      return Counters(1 .. Last);

   end Counter_Cells;


   function Image (Item : Cell_List_T) return String
   is
   begin

      if Item'Length = 0 then

         return "";

      elsif Item'Length = 1 then

         return Image (Item(Item'First));

      else

         return Image (Item(Item'First))
              & ", "
              & Image (Item(Item'First + 1 .. Item'Last));

      end if;

   end Image;


   function Initial_Cell_Variables
   return  Cell_List_T
   renames Cell_Store.Initial_Cell_Variables;


   --
   ---   Cell sets
   --


   procedure Erase (Set : in out Root_Cell_Set_T)
   is
   begin

      Remove (
         Cells => To_List (Cell_Set_T (Set)),
         From  => Cell_Set_T (Set));

   end Erase;


   function Any_Member (
      Cells  : Cell_List_T;
      Of_Set : Root_Cell_Set_T)
   return Boolean
   is
   begin

      for C in Cells'Range loop

         if Is_Member (Cells(C), Cell_Set_T (Of_Set)) then

            return True;

         end if;

      end loop;

      return False;

   end Any_Member;


   function To_List (
      From   : Root_Cell_Set_T;
      Except : Cell_Set_T)
   return Cell_List_T
   is

      Tutti : constant Cell_List_T := To_List (Cell_Set_T (From));
      -- All the cells in From.

      Boni  : Cell_List_T (1 .. Tutti'Length);
      Last  : Natural := 0;
      -- The result will be Boni(1 .. Last).

   begin

      for T in Tutti'Range loop

         if not Is_Member (Cell => Tutti(T), Of_Set => Except) then
            -- This cell is not excepted.

            Last := Last + 1;

            Boni(Last) := Tutti(T);

         end if;

      end loop;

      return Boni(1 .. Last);

   end To_List;


   procedure Add (
      Cell  : in     Cell_T;
      To    : in out Root_Cell_Set_T;
      Added : in out Boolean)
   is
   begin

      if not Is_Member (Cell, Cell_Set_T (To)) then

         Added := True;

         Add (Cell => Cell, To => Cell_Set_T (To));

      end if;

   end Add;


   procedure Add (
      Cells : in     Cell_List_T;
      To    : in out Root_Cell_Set_T)
   is
   begin

      for C in Cells'Range loop

         Add (Cell => Cells(C), To => Cell_Set_T (To));

      end loop;

   end Add;


   procedure Add (
      Cells : in     Root_Cell_Set_T;
      To    : in out Root_Cell_Set_T)
   is
   begin

      Basic_Mixed.Add (
         Cells => Cell_Set_T (Cells),
         To    => Cell_Set_T (To   ));

   end Add;


   procedure Remove (
      Cells : in     Cell_List_T;
      From  : in out Root_Cell_Set_T)
   is
   begin

      for C in Cells'Range loop

         Remove (
            Cell => Cells(C),
            From => Cell_Set_T (From));

      end loop;

   end Remove;


   procedure Remove (
      Cells : in     Root_Cell_Set_T;
      From  : in out Root_Cell_Set_T)
   is
   begin

      Basic_Mixed.Remove (
         Cells => Cell_Set_T (Cells),
         From  => Cell_Set_T (From ));

   end Remove;


   procedure Move_Cells (
      From : in out Root_Cell_Set_T;
      To   : in out Root_Cell_Set_T)
   is
   begin

      Basic_Mixed.Move_Cells (
         From => Cell_Set_T (From),
         To   => Cell_Set_T (To  ));

   end Move_Cells;


   function Image (Item : Root_Cell_Set_T) return String
   is
   begin

      return Image (To_List (Cell_Set_T (Item)));

   end Image;


   function "=" (Left, Right : Root_Cell_Set_T) return Boolean
   is

      Equal : Boolean;

   begin

      Equal := Basic_Mixed."=" (
                Cell_Set_T (Left),
                Cell_Set_T (Right));

      return Equal;

   end "=";


   function Union (Left, Right : Root_Cell_Set_T) return Cell_Set_T
   is
   begin

      return Basic_Mixed.Union (
                Cell_Set_T (Left),
                Cell_Set_T (Right));

   end Union;


   function "-"  (Left, Right : Root_Cell_Set_T) return Cell_Set_T
   is
   begin

      return Basic_Mixed."-" (
                Cell_Set_T (Left),
                Cell_Set_T (Right));

   end "-";


   function Intersection (Left, Right : Root_Cell_Set_T) return Cell_Set_T
   is
   begin

      return Basic_Mixed.Intersection (
                Cell_Set_T (Left),
                Cell_Set_T (Right));

   end Intersection;


   procedure Discard (Set : in out Root_Cell_Set_T)
   is
   begin

      Erase (Cell_Set_T (Set));

   end Discard;


   function "<=" (Left : Cell_List_T; Right : Cell_Set_T)
   return Boolean
   --
   -- Whether Left is a subset of Right, that is, all cells in
   -- the Left list are members of the Right set.
   --
   is
   begin

      for L in Left'Range loop

         if not Is_Member (Left(L), Right) then

            return False;

         end if;

      end loop;

      return True;

   end "<=";


   --
   ---   Class-wide operations on sets of cells
   --


   package body Basic_Mixed
   is

      procedure Add (
         Cells : in     Cell_Set_T;
         To    : in out Cell_Set_T)
      is
      begin

         Add (Cells => To_List (Cells), To => To);
         -- Dispatch on To.

      end Add;


      procedure Remove (
         Cells : in     Cell_Set_T;
         From  : in out Cell_Set_T)
      is
      begin

         Remove (Cells => To_List (Cells), From => From);
         -- Dispatch on To.

      end Remove;


      procedure Move_Cells (
         From : in out Cell_Set_T;
         To   : in out Cell_Set_T)
      is

         Cells : constant Cell_List_T := To_List (From);
         -- The cells to be moved. Dispatch on From.

      begin

         Erase (From);
         -- Dispatch on From.

         Add (Cells => Cells, To => To);
         -- Dispatch on To.

      end Move_Cells;


      function "=" (Left, Right : Cell_Set_T) return Boolean
      is

         Equal : Boolean;
         -- The result.

      begin

         if Card (Left) /= Card (Right) then

            Equal := False;

         else

            Equal := To_List (Left) <= Right;

         end if;

         return Equal;

      end "=";


      function Union (Left, Right : Cell_Set_T)
      return Cell_Set_T
      is

         Uni : Cell_Set_T := Left;
         -- The union to be.

      begin

         Add (Cells => To_List (Right), To => Uni);
         -- Dispatch on Uni.

         return Uni;

      end Union;


      function "-" (Left, Right : Cell_Set_T)
      return Cell_Set_T
      is

         Diff : Cell_Set_T := Left;
         -- The difference to be.

      begin

         Remove (Cells => To_List (Right), From => Diff);
         -- Dispatch on Diff.

         return Diff;

      end "-";


      function Intersection (Left, Right : Cell_Set_T)
      return Cell_Set_T
      is

         Inter : Cell_Set_T := Left;
         -- The intersection to be.

         Lefts : constant Cell_List_T := To_List (Inter);
         -- All the elements in the Left set (= Inter set, here).

      begin

         for L in Lefts'Range loop

            if not Is_Member (Cell => Lefts(L), Of_Set => Right) then

               Remove (Cell => Lefts(L), From => Inter);

            end if;

         end loop;

         return Inter;

      end Intersection;


   end Basic_Mixed;


   --
   ---   Class-wide operations on sets of cells that use primitive
   ---   operations when both operand sets are of the same class.
   --


   package body Mixed
   is

      use type Ada.Tags.Tag;


      procedure Add (
         Cells : in     Cell_Set_T;
         To    : in out Cell_Set_T)
      is
      begin

         if Cells'Tag = To'Tag then

            Storage.Add (Cells, To);

         else

            Basic_Mixed.Add (Cells, To);

         end if;

      end Add;


      procedure Remove (
         Cells : in     Cell_Set_T;
         From  : in out Cell_Set_T)
      is
      begin

         if Cells'Tag = From'Tag then

            Storage.Remove (Cells, From);

         else

            Basic_Mixed.Remove (Cells, From);

         end if;

      end Remove;


      procedure Move_Cells (
         From : in out Cell_Set_T;
         To   : in out Cell_Set_T)
      is
      begin

         if From'Tag = To'Tag then

            Storage.Move_Cells (From, To);

         else

            Basic_Mixed.Move_Cells (From, To);

         end if;

      end Move_Cells;


      function "=" (Left, Right : Cell_Set_T) return Boolean
      is
      begin

         if Left'Tag = Right'Tag then

            return Storage."=" (Left, Right);

         else

            return Basic_Mixed."=" (Left, Right);

         end if;

      end "=";


      function Union (Left, Right : Cell_Set_T)
      return Cell_Set_T
      is
      begin

         if Left'Tag = Right'Tag then

            return Storage.Union (Left, Right);

         else

            return Basic_Mixed.Union (Left, Right);

         end if;

      end Union;


      function "-" (Left, Right : Cell_Set_T)
      return Cell_Set_T
      is
      begin

         if Left'Tag = Right'Tag then

            return Storage."-" (Left, Right);

         else

            return Basic_Mixed."-" (Left, Right);

         end if;

      end "-";


      function Intersection (Left, Right : Cell_Set_T)
      return Cell_Set_T
      is
      begin

         if Left'Tag = Right'Tag then

            return Storage.Intersection (Left, Right);

         else

            return Basic_Mixed.Intersection (Left, Right);

         end if;

      end Intersection;


   end Mixed;


   --
   ---   Locations and location maps
   --


   function Rainge (First, Last : Processor.Code_Address_T)
   return Code_Address_Range_T
   is

      Result : Code_Address_Range_T;

   begin

      Result := Processor.Singleton (First);

      Processor.Widen (
         Rainge     => Result,
         To_Include => Last);

      return Result;

   end Rainge;


   function Fixed_Location (Cell : in Cell_T)
   return Location_T
   is
   begin

      return (1 => (
         Address => Processor.Full_Code_Address_Range,
         Cell    => Cell));

   end Fixed_Location;


   function Image (Item : Location_Point_T) return String
   is
   begin

      return
           Processor.Image (Item.Address)
         & " => "
         & Image (Item.Cell);

   end Image;


   function Image (Item : Location_T) return String
   is
   begin

      if Item'Length = 0 then

         return "[none]";

      elsif Item'Length = 1 then

         return Image (Item(Item'First));

      else

         return
              Image (Item(Item'First))
            & ", "
            & Image (Item(Item'First + 1 .. Item'Last));

      end if;

   end Image;


   function Image (Item : Location_Ref) return String
   is
   begin

      return Image (Item.all);

   end Image;


   --
   ---   Cells bound to values
   --


   function Image (Item : Cell_Value_T) return String
   is
   begin

      return Image (Item.Cell) & '=' & Arithmetic_Base.Image (Item.Value);

   end Image;


   function Image (Item : Cell_Value_List_T) return String
   is
   begin

      if Item'Length = 0 then

         return "";

      elsif Item'Length = 1 then

         return Image (Item(Item'First));

      else

         return Image (Item(Item'First))
              & ", "
              & Image (Item(Item'First + 1 .. Item'Last));

      end if;

   end Image;


   function Cells_Of (Item : Cell_Value_List_T) return Cell_List_T
   is

      Cells : Cell_List_T (Item'Range);

   begin

      for C in Cells'Range loop

         Cells(C) := Item(C).Cell;

      end loop;

      return Cells;

   end Cells_Of;


end Storage;
