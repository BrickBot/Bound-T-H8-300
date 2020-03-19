-- Storage.Bounds.Disjoint_Intervals (decl)
--
-- Representing value bounds as ordered sets (lists) of disjoint integer
-- intervals, possibly with a bounded number of components.
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
-- $Revision: 1.4 $
-- $Date: 2015/10/24 19:36:52 $
--
-- $Log: storage-bounds-disjoint_intervals.ads,v $
-- Revision 1.4  2015/10/24 19:36:52  niklas
-- Moved to free licence.
--
-- Revision 1.3  2015/04/18 10:26:03  niklas
-- Added the Is_Void predicate on Bounds_T.
--
-- Revision 1.2  2013/12/08 20:16:30  niklas
-- Added subtype Sequence_T, renaming Interval_List_T with a specific
-- meaning. Added operations on Sequence_T.
-- Added type Bounds_T, bounds on a cell based on a Sequence_T, with
-- several overridings of primitive operations on bounds.
--
-- Revision 1.1  2013-02-03 12:35:05  niklas
-- First version.
--


with Arithmetic;


package Storage.Bounds.Disjoint_Intervals is


   --
   ---   Lists of disjoint intervals in increasing order
   --


   subtype Sequence_T is Interval_List_T;
   --
   -- A list of disjoint intervals in increasing order of their bounds.
   -- Consequently, only the first interval can be unbounded towards -inf,
   -- and only the last interval can be unbounded towards +inf.
   -- None of the intervals in the list is void. The empty (void) set
   -- is represented (canonical form) by an empty list.


   function Bounding_Interval (Sequence : Sequence_T) return Interval_T;
   --
   -- The smallest interval that contains all the intervals in the Sequence.
   -- If the Sequence is empty, the Void_Interval is returned.


   function Bounded (Sequence : Sequence_T) return Boolean;
   --
   -- Whether the set of values defined by the Sequence is bounded, and so
   -- contains a finite number of values.


   function Number_Of_Values (Sequence : Sequence_T) return Value_T;
   --
   -- The total number of values in the set defined by the Sequence, if
   -- the set is Bounded and not too large to be counted in Value_T.
   -- Otherwise, the function propagates Unbounded.


   function Values (Sequence : Sequence_T) return Value_List_T;
   --
   -- All the values within the intervals in the Sequence, if their number
   -- is finite and no larger than a set limit. Otherwise, the function
   -- propagates Unbounded.


   --
   ---   Lists of disjoint intervals of dynamic but bounded length
   --


   type List_T (Max_Num_Intervals : Natural)
   is record
      Intervals : Sequence_T (1 .. Max_Num_Intervals);
      Last      : Natural := 0;
   end record;
   --
   -- Bounds on the possible value of some variable or expression (not
   -- explicitly identified here) formed by the union of a number of
   -- disjoint, non-empty, integer Intervals(1..Last), listed in
   -- numerically increasing order.
   --
   -- If Last = 0, the empty set is represented.
   -- The universal set is represented by one Universal_Interval.
   --
   -- Representation invariant: the Intervals are non-void, neither
   -- intersecting nor contiguous, and ordered (i.e. Max of an interval
   -- is < Min of the next).
   -- Only Intervals(1) can be unbounded towards -inf.
   -- Only Intervals(Last) can be unbounded towards +inf.
   --
   -- (If a large number of intervals is used, a balanced binary tree
   -- could be a more efficient representation.)


   procedure Merge (
      A, B : in     List_T;
      To   :    out List_T);
   --
   -- Merges two given disjoint-interval lists into a combined list of
   -- also disjoint intervals. Thus, intervals from A that "touch"
   -- intervals from B will be united, and vice versa. The number of
   -- intervals in the merged list may be at most the sum of the number
   -- of intervals in the operands, but may also be reduced down to
   -- a single interval.
   --
   -- This corresponds to a set union operation: the merged list (To)
   -- represents the union of the sets represented by the operands.
   -- This union is exact, so long as the capacity of the merged list
   -- container is not exceeded.


   procedure Add (
      Interval : in     Interval_T;
      To       : in out List_T);
   --
   -- Adds an Interval To a list, increasing the possible values of
   -- the bounded quantity by those in the added Interval.


   function To_Intervals (List : List_T)
   return Interval_List_T;
   --
   -- All the defined intervals in the List, indexed 1 .. List.Last,
   -- possibly an empty set.


   --
   ---   Bounds on one cell, defined by a fixed list of disjoint intervals
   --


   type Bounds_T (Num_Intervals : Natural)
   is new Storage.Bounds.Bounds_T
   with record
      Cell   : Cell_T;
      Values : Sequence_T (1 .. Num_Intervals);
   end record;
   --
   -- Bounds the Cell to the listed Values.
   --
   -- The representation is the same as for List_T (Num_Intervals)
   -- with Last = Num_Intervals. Thus: an empty set (contradictory
   -- bounds) is represented by Num_Intervals = 0; the intervals are
   -- disjoint and listed in increasing order; only the first interval
   -- can be unbounded towards -inf; and only the last interval can be
   -- unbounded towards +inf.


   overriding
   function Basis (Item : Bounds_T) return Cell_List_T;
   --
   -- Returns Item.Cell, the only bounded cell.


   not overriding  -- but will later be
   function Is_Void (Item : Bounds_T) return Boolean;
   --
   -- Whether the bounds represent a void set, that is, the bounds
   -- are contradictory and cannot be satisfied for any values of
   -- the bounded cells. This generally indicates an infeasible path
   -- in the program under analysis.


   overriding
   function Interval (Cell : Cell_T; Under : Bounds_T)
   return Interval_T;
   --
   -- If the Cell is the bounded cell (Under.Cell), this function
   -- returns the Bounding_Interval of the list, otherwise it returns
   -- the Universal_Interval.


   overriding
   function Values (Cell : Cell_T; Under : Bounds_T)
   return Value_List_T;
   --
   -- If the Cell is the bounded cell (Under.Cell), this function
   -- returns the Value_List of the intervals, otherwise it propagates
   -- the Unbounded exception.


   overriding
   function Image (Item : Bounds_T) return String;
   --
   -- Returns "Disjoint-Interval-List[<image of Item.Cell>]".


   overriding
   function Full_Image (Item : Bounds_T) return String;
   --
   -- Returns Storage.Image (Item.List, Storage.Image (Item.Cell)).


end Storage.Bounds.Disjoint_Intervals;
