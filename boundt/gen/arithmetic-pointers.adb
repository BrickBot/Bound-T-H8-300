-- Arithmetic.Pointers (body)
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
-- $Revision: 1.6 $
-- $Date: 2015/10/24 20:05:45 $
--
-- $Log: arithmetic-pointers.adb,v $
-- Revision 1.6  2015/10/24 20:05:45  niklas
-- Moved to free licence.
--
-- Revision 1.5  2009-11-27 11:28:06  niklas
-- BT-CH-0184: Bit-widths, Word_T, failed modular analysis.
--
-- Revision 1.4  2007/10/28 09:32:44  niklas
-- BT-CH-0092: Arithmetic analysis of dynamic data refs is optional.
--
-- Revision 1.3  2006/02/27 10:03:06  niklas
-- Added function Image_Tag for use in function Image so that
-- derived types can use short "tags" without reimplementing
-- the whole Image function.
--
-- Revision 1.2  2005/03/04 09:36:41  niklas
-- Added implementation for Alias_Range (Boundable_Pointer_T) as
-- a redispatch to Alias_Range with an Intervals parameter, too.
-- Added type Interval_Pointer_T.
--
-- Revision 1.1  2005/02/16 21:11:36  niklas
-- BT-CH-0002.
--


with Ada.Strings.Unbounded;
with Ada.Tags;


package body Arithmetic.Pointers is


   function Alias_Range (Ref : Boundable_Pointer_T)
   return Storage.Alias_Range_T
   is
   begin

      return Alias_Range (
         Pointer  => Boundable_Pointer_T'Class (Ref),
         Interval => (Ref.Expr'Range => Storage.Bounds.Universal_Interval));

   end Alias_Range;


   procedure Constrain (
      Pointer  : in     Boundable_Pointer_T;
      Interval : in     Storage.Bounds.Interval_List_T;
      Giving   :    out Boundable_Pointer_Ref)
   is
   begin

      Giving := null;

   end Constrain;


   function Basis (Item : Boundable_Pointer_T) return Storage.Cell_List_T
   is
   begin

      return Cells_Used (By => Item.Expr);

   end Basis;


   procedure Add_Basis_Cells (
      From : in     Boundable_Pointer_T;
      To   : in out Storage.Cell_Set_T)
   is
   begin

      for E in From.Expr'Range loop

         Add_Cells_Used (
            By   => From.Expr(E),
            Refs => True,
            To   => To);

      end loop;

   end Add_Basis_Cells;


   procedure Add_Basis_Cells (
      From  : in     Boundable_Pointer_T;
      To    : in out Storage.Cell_Set_T;
      Added : in out Boolean)
   is
   begin

      for E in From.Expr'Range loop

         Add_Cells_Used (
            By    => From.Expr(E),
            Refs  => True,
            To    => To,
            Added => Added);

      end loop;

   end Add_Basis_Cells;


   function Is_Used (Cell : Storage.Cell_T; By : Boundable_Pointer_T)
   return Boolean
   is
   begin

      return Is_Used (Cell => Cell, By => By.Expr);

   end Is_Used;


   function Image_Tag (Item : Boundable_Pointer_T) return String
   is
   begin

      return Ada.Tags.Expanded_Name (Boundable_Pointer_T'Class (Item)'Tag);

   end Image_Tag;


   function Image (Item : Boundable_Pointer_T) return String
   is
      use Ada.Strings.Unbounded;

      Result : Unbounded_String :=
        To_Unbounded_String (Image_Tag (Boundable_Pointer_T'Class (Item)));
      -- The result.

   begin

      Append (Result, '[');

      for E in Item.Expr'Range loop

         Append (Result, Image (Item.Expr(E)));

         if E < Item.Expr'Last then

            Append (Result, ',');

         end if;

      end loop;

      Append (Result, ']');

      return To_String (Result);

   end Image;


   function Alias_Range (
      Ref   : Boundable_Pointer_T;
      Under : Storage.Bounds.Bounds_T'Class)
   return Storage.Alias_Range_T
   is
   begin

      if Under in Arithmetic.Bounds_T'Class then
         -- Goody, we can bound expressions.

         return Arithmetic_Alias_Range (
            Ref   => Boundable_Pointer_T'Class (Ref),
            Under => Arithmetic.Bounds_T'Class (Under));

      else
         -- Some wild type of bounds. Ignore them.

         return Alias_Range (Boundable_Pointer_T'Class (Ref));

      end if;

   end Alias_Range;


   function Arithmetic_Alias_Range (
      Ref   : Boundable_Pointer_T;
      Under : Arithmetic.Bounds_T'Class)
   return Storage.Alias_Range_T
   is

      Intervals : Storage.Bounds.Interval_List_T (Ref.Expr'Range);
      -- The possible range for each Expr, Under the given bounds.

      Unique : Boolean := True;
      -- Whether a unique value is defined for all Ref.Expr.
      -- Initial value is for conjunctive accumulation.

      Cell : Storage.Cell_T;
      -- The referent, if Unique.

      Group : Processor.Alias_Group_T;
      -- The alias group of the Cell, if Unique.

   begin

      for I in Intervals'Range loop

         Intervals(I) := Interval (Ref.Expr(I), Under);

         Unique := Unique and Storage.Bounds.Singular (Intervals(I));

      end loop;

      if Unique then
         -- All Ref.Expr are bounded to a single value.
         -- This defines a referent Cell which in turn has an alias
         -- group which has an alias range.

         Cell := Referent (
            Pointer => Boundable_Pointer_T'Class (Ref),
            Value   => Storage.Bounds.Single_Value (Intervals));

         Group := Processor.Alias_Group (Storage.Spec_Of (Cell));

         return Processor.Alias_Range (Group);

      else
         -- Several possible values for the Ref.Expr tuples.
         -- The Intervals define an encompassing alias range.

         return Alias_Range (
            Pointer  => Boundable_Pointer_T'Class (Ref),
            Interval => Intervals);

      end if;

   end Arithmetic_Alias_Range;


   function Referent (
      Ref   : Boundable_Pointer_T;
      Under : Storage.Bounds.Bounds_T'Class)
   return Storage.Cell_T
   is
   begin

      if Under in Arithmetic.Bounds_T'Class then
         -- Goody, we can bound expressions.

         return Arithmetic_Referent (
            Ref   => Boundable_Pointer_T'Class (Ref),
            Under => Arithmetic.Bounds_T'Class (Under));

      else
         -- Some wild type of bounds.

         return Storage.No_Cell;

      end if;

   end Referent;


   function Arithmetic_Referent (
      Ref   : Boundable_Pointer_T;
      Under : Arithmetic.Bounds_T'Class)
   return Storage.Cell_T
   is

      Value : Storage.Bounds.Value_List_T (Ref.Expr'Range);
      -- The single value (we hope) for each Ref.Expr, Under the bounds.

   begin

      for V in Value'Range loop

         Value(V) := Signed_Value (
            Word  => Single_Value (Ref.Expr(V), Under),
            Width => Width_Of (Ref.Expr(V)));

      end loop;

      -- All Ref.Expr are bounded to a single Value.

      return Referent (
         Pointer => Boundable_Pointer_T'Class (Ref),
         Value   => Value);

   exception

   when Storage.Bounds.Unbounded =>
      -- Some Ref.Expr is not bounded to a single value.

      return Storage.No_Cell;

   end Arithmetic_Referent;


   procedure Apply (
      Bounds : in     Storage.Bounds.Bounds_T'Class;
      Upon   : in     Boundable_Pointer_T;
      Giving :    out Storage.References.Boundable_Ref)
   is
   begin

      if Bounds in Arithmetic.Bounds_T'Class then
         -- Goody, we can bound expressions.

         Apply (
            Bounds => Arithmetic.Bounds_T'Class (Bounds),
            Upon   => Boundable_Pointer_T'Class (Upon),
            Giving => Boundable_Pointer_Ref (Giving));

      else
         -- Some wild type of bounds.

         Giving := null;

      end if;

   end Apply;


   procedure Apply (
      Bounds : in     Arithmetic.Bounds_T'Class;
      Upon   : in     Boundable_Pointer_T;
      Giving :    out Boundable_Pointer_Ref)
   is

      Intervals : Storage.Bounds.Interval_List_T (Upon.Expr'Range);
      -- The possible range for each Expr, Under the given bounds.

   begin

      for I in Intervals'Range loop

         Intervals(I) := Interval (Upon.Expr(I), Bounds);

      end loop;

      Constrain (
         Pointer  => Boundable_Pointer_T'Class (Upon),
         Interval => Intervals,
         Giving   => Giving);

   end Apply;


   --
   --    References based on (address) expressions which can
   --    be constrained to intervals
   --


   function Alias_Range (Ref : Interval_Pointer_T)
   return Storage.Alias_Range_T
   is
   begin

      return Reduced_Alias_Range (
         Pointer  => Interval_Pointer_T'Class (Ref),
         Interval => Ref.Interval);

   end Alias_Range;


   function Alias_Range (
      Pointer  : Interval_Pointer_T;
      Interval : Storage.Bounds.Interval_List_T)
   return Storage.Alias_Range_T
   is
      use Storage.Bounds;

      Joint : Interval_List_T (Pointer.Interval'Range);
      -- The intersected intervals.

   begin

      for J in Joint'Range loop

         Joint(J) := Pointer.Interval(J) and Interval(J);

      end loop;

      return Reduced_Alias_Range (
         Pointer => Interval_Pointer_T'Class (Pointer),
         Interval => Joint);

   end Alias_Range;


   procedure Constrain (
      Pointer  : in     Interval_Pointer_T;
      Interval : in     Storage.Bounds.Interval_List_T;
      Giving   :    out Boundable_Pointer_Ref)
   is
      use Storage.Bounds;

      New_Pointer : Interval_Pointer_Ref := null;
      -- The improved Pointer, if one is created.

   begin

      for D in 1 .. Pointer.Dim loop

         if not (Pointer.Interval(D) <= Interval(D)) then
            -- The intersection will be tighter.

            if New_Pointer = null then
               -- This was the first tighter dimension, so now
               -- we decide to make a new pointer.

               New_Pointer := new Interval_Pointer_T'Class'(
                  Interval_Pointer_T'Class (Pointer));

            end if;

            -- Constrain the new pointer to the intersection of
            -- the earlier and new Interval:

            New_Pointer.Interval(D) :=
               New_Pointer.Interval(D) and Interval(D);

         end if;

      end loop;

      Giving := Boundable_Pointer_Ref (New_Pointer);

   end Constrain;


end Arithmetic.Pointers;
