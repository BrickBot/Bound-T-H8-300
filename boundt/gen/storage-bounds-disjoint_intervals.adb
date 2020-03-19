-- Storage.Bounds.Disjoint_Intervals (body)
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
-- $Revision: 1.5 $
-- $Date: 2015/10/24 20:05:51 $
--
-- $Log: storage-bounds-disjoint_intervals.adb,v $
-- Revision 1.5  2015/10/24 20:05:51  niklas
-- Moved to free licence.
--
-- Revision 1.4  2015/04/18 10:26:03  niklas
-- Added the Is_Void predicate on Bounds_T.
--
-- Revision 1.3  2013/12/20 21:05:00  niklas
-- Made function Values (Sequence_T) return Value_List_T obey the
-- option Max_Listed_Values from Storage.Bounds.Opt.
--
-- Revision 1.2  2013/12/08 20:16:29  niklas
-- Added subtype Sequence_T, renaming Interval_List_T with a specific
-- meaning. Added operations on Sequence_T.
-- Added type Bounds_T, bounds on a cell based on a Sequence_T, with
-- several overridings of primitive operations on bounds.
--
-- Revision 1.1  2013-02-03 12:35:03  niklas
-- First version.
--


with Ada.Text_IO;
with Arithmetic;
with Storage.Bounds.Opt;


package body Storage.Bounds.Disjoint_Intervals is


   use type Arithmetic.Value_T;


   --
   ---   Lists of disjoint intervals in increasing order
   --


   function Bounding_Interval (Sequence : Sequence_T) return Interval_T
   is
   begin

      if Sequence'Last = 0 then

         return Void_Interval;

      else

         return Interval_T'(
            Min => Sequence(Sequence'First).Min,
            Max => Sequence(Sequence'Last ).Max);

      end if;

   end Bounding_Interval;


   function Bounded (Sequence : Sequence_T) return Boolean
   is
   begin

      return Bounded (Bounding_Interval (Sequence));

   end Bounded;


   function Number_Of_Values (Sequence : Sequence_T) return Arithmetic.Value_T
   is
      use type Arithmetic.Value_T;

      Number : Arithmetic.Value_T := 0;
      -- The count so far.

   begin

      if not Bounded (Sequence) then

         raise Unbounded;

      else

         for I in 1 .. Sequence'Last loop

            Number := Number + Number_Of_Values (Sequence(I));

         end loop;

      end if;

      return Number;

   end Number_Of_Values;


   Abs_Max_Listed_Values : constant Positive := 5_000;
   --
   -- The fixed absolute upper bound on the number of values
   -- that can be listed by Values (List_T).


   function Values (Sequence : Sequence_T) return Value_List_T
   is

      Num : constant Arithmetic.Value_T := Number_Of_Values (Sequence);
      -- The total number of values in the set defined by the Sequence.
      -- If the set is unbounded, this raises Unbounded.

      Allowed_Num : constant Natural :=
         Natural'Min (Opt.Max_Listed_Values, Abs_Max_Listed_Values);
      -- An upper bound on the allowed number of listed values.

      Vals : Value_List_T (1 .. Abs_Max_Listed_Values);
      Last : Natural := 0;
      -- The values in the List, as Vals(1 .. Last).

   begin

      if Num > Arithmetic.Value_T (Allowed_Num) then
         -- The Sequence contains more values than we are
         -- allowed to enumerate in a list.

         raise Unbounded;

      else

         for I in 1 .. Sequence'Last loop

            for V in Sequence(I).Min.Value
                  .. Sequence(I).Max.Value
            loop

               Last := Last + 1;
               Vals(Last) := V;

            end loop;

         end loop;

         return Vals(1 .. Last);

      end if;

   end Values;


   --
   ---   Lists of disjoint intervals of dynamic but bounded length
   --


   function Precedes (A, B : Interval_T) return Boolean
   --
   -- Whether the interval A entirely precedes B, with a
   -- gap in between.
   --
   -- Precondition: both intervals are non-void.
   --
   is
   begin

      return (Known (A.Max) and Known (B.Min))
      and then Max (A) + 1 < Min (B);

   end Precedes;


   function Touches (A, B : Interval_T) return Boolean
   --
   -- Whether the two intervals "touch" in the sense that they
   -- intersect or are contiguous.
   --
   -- Precondition: both intervals are non-void.
   --
   is
   begin

      return not (Precedes (A, B) or else Precedes (B, A));

   end Touches;


   procedure Merge (
      A, B : in     List_T;
      To   :    out List_T)
   is

      Ax : Positive := 1;
      Bx : Positive := 1;
      -- Indices of the next elements from the A and B lists.

      Union : Interval_T;
      -- A merged element under construction.

      Num : Natural := 0;
      -- The total number of intervals in the merged list (even
      -- if larger than its capacity).

   begin

      Num := 0;

      -- Compare and merge:

      while Ax <= A.Last
      and   Bx <= B.Last
      loop

         -- Initialize a new element for the merged list:

         if Precedes (A.Intervals(Ax), B.Intervals(Bx)) then
            Union := A.Intervals(Ax);
            Ax    := Ax + 1;
         elsif Precedes (B.Intervals(Bx), A.Intervals(Ax)) then
            Union := B.Intervals(Bx);
            Bx    := Bx + 1;
         else
            Union := A.Intervals(Ax) or B.Intervals(Bx);
            Ax    := Ax + 1;
            Bx    := Bx + 1;
         end if;

         -- Grow the new element for as long as either input
         -- list provides a contiguous or intersecting element:

         loop
            if Ax <= A.Last
            and then Touches (Union, A.Intervals(Ax)) then
               Union := Union or A.Intervals(Ax);
               Ax    := Ax + 1;
            elsif Bx <= B.Last
            and then Touches (Union, B.Intervals(Bx)) then
               Union := Union or B.Intervals(Bx);
               Bx    := Bx + 1;
            else
               exit;
            end if;
         end loop;

         -- Put the new element in the merged list:

         Num := Num + 1;

         if Num <= To.Max_Num_Intervals then

            To.Intervals(Num) := Union;

         end if;

      end loop;

      -- Now one of the input lists is exhausted, so we finish
      -- by copying the rest of the other list to the merged list:

      while Ax <= A.Last loop

         Num := Num + 1;

         if Num <= To.Max_Num_Intervals then
            To.Intervals(Num) := A.Intervals(Ax);
         end if;

         Ax := Ax + 1;

      end loop;

      while Bx <= B.Last loop

         Num := Num + 1;

         if Num <= To.Max_Num_Intervals then
            To.Intervals(Num) := B.Intervals(Bx);
         end if;

         Bx := Bx + 1;

      end loop;

      -- Report possible overflow:

      if Num > To.Max_Num_Intervals then

         Ada.Text_IO.Put_Line (
              "Merge overflow: merging lost"
            & Positive'Image (Num - To.Max_Num_Intervals)
            & " intervals.");

         Num := To.Max_Num_Intervals;

         if Num in To.Intervals'Range then
            To.Intervals(Num).Max := Not_Limited;
         end if;

      end if;

      To.Last := Num;

   end Merge;


   procedure Add (
      Interval : in     Interval_T;
      To       : in out List_T)
   is
      Addend : List_T := (
         Max_Num_Intervals => 1,
         Last              => 1,
         Intervals         => (1 => Interval));

      Sum : List_T (Max_Num_Intervals => To.Max_Num_Intervals);

   begin

      Merge (A => To, B => Addend, To => Sum);

      To := Sum;

   end Add;


   function To_Intervals (List : List_T)
   return Interval_List_T
   is
   begin

      return List.Intervals(1 .. List.Last);

   end To_Intervals;


   --
   ---   Bounds on one cell, defined by a fixed list of disjoint intervals
   --


   overriding
   function Basis (Item : Bounds_T) return Cell_List_T
   is
   begin

      return (1 => Item.Cell);

   end Basis;


   not overriding  -- but will later be
   function Is_Void (Item : Bounds_T) return Boolean
   is
   begin

      return Item.Num_Intervals = 0;

   end Is_Void;


   overriding
   function Interval (Cell : Cell_T; Under : Bounds_T)
   return Interval_T
   is
   begin

      if Cell = Under.Cell then

         return Bounding_Interval (Under.Values);

      else

         return Universal_Interval;

      end if;

   end Interval;


   overriding
   function Values (Cell : Cell_T; Under : Bounds_T)
   return Value_List_T
   is
   begin

      if Cell = Under.Cell then

         return Values (Under.Values);

      else

         raise Unbounded;

      end if;

   end Values;


   overriding
   function Image (Item : Bounds_T) return String
   is
   begin

      return "Disjoint-Interval-List"
           & '['
           & Image (Item.Cell)
           & ']';

   end Image;


   overriding
   function Full_Image (Item : Bounds_T) return String
   is
   begin

      return Image (Item.Values, Storage.Image (Item.Cell));

   end Full_Image;


end Storage.Bounds.Disjoint_Intervals;
