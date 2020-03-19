-- Options.Interval_Sets (decl)
--
-- Sets of (disjoint) integer intervals as options.
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
-- $Revision: 1.3 $
-- $Date: 2015/10/24 19:36:50 $
--
-- $Log: options-interval_sets.ads,v $
-- Revision 1.3  2015/10/24 19:36:50  niklas
-- Moved to free licence.
--
-- Revision 1.2  2014/06/01 10:35:35  niklas
-- Added the query function Is_Empty.
--
-- Revision 1.1  2013-02-03 12:32:38  niklas
-- First version.
--


with Storage.Bounds;
with Unbounded_Vectors;


package Options.Interval_Sets is


   type Interval_Set_T is private;
   --
   -- A set of non-empty, disjoint integer (Storage.Value_T) intervals.
   -- The default initial value of any Interval_Set_T is the empty set.
   -- Options of this kind are not enumerable.


   procedure Add (
      Item : in     Storage.Bounds.Interval_T;
      To   : in out Interval_Set_T);
   --
   -- Adds the Item To the set, taking care to keep the intervals
   -- in the set disjoint and non-emptz. If the new Item intersects
   -- some interval(s) already in the set, that/those interval(s)
   -- is/are replaced by the union of the intervals.


   function Is_Empty (Set : Interval_Set_T) return Boolean;
   --
   -- Whether the Set contains some intervals.


   function Max (
      Left  : Storage.Value_T;
      Right : Interval_Set_T)
   return Storage.Value_T;
   --
   -- The larger of the Left value and the largest value contained
   -- in any interval in the Right set. If the set is empty, the
   -- Left value is returned. If the set contains an interval that
   -- has no upper limit, the exception Storage.Bounds.Unbounded
   -- is propagated.


   type Option_T is new Options.Option_T with record
      Value : Interval_Set_T;
   end record;
   --
   -- An option that has an interval-set as its value.
   -- The default value is the null set.


   overriding
   function Type_And_Default (Option : access Option_T)
   return String;


   overriding
   procedure Reset (Option : access Option_T);
   --
   -- Clears the Option by setting its value to the null set.


   overriding
   procedure Set (
      Option : access Option_T;
      Value  : in     String);
   --
   -- The interval described by the Value string is added to
   -- the Option's value, merging intervals to keep them
   -- disjoint and rejecting any empty intervals.


   function To_List (Option : Option_T)
   return Storage.Bounds.Interval_List_T;
   --
   -- All the interval in the Option, as a list in numerically
   -- increasing order.


private


   Deallocate : Boolean := True;
   --
   -- Whether to use Unchecked_Deallocation for Interval_Set_Ts.


   package Interval_Vectors
   is new Unbounded_Vectors (
      Element_Type   => Storage.Bounds.Interval_T,
      Vector_Type    => Storage.Bounds.Interval_List_T,
      Initial_Size   => 10,
      Size_Increment => 50,
      Deallocate     => Deallocate);
   --
   -- The size and increment may not be good for all uses :-)


   type Interval_Set_T is new Interval_Vectors.Unbounded_Vector;
   --
   -- The vector of intervals is kept sorted into numerically
   -- ascending order, and the intervals are kept disjoint.


end Options.Interval_Sets;
