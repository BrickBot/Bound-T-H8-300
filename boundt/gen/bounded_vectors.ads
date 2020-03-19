-- Bounded_Vectors (decl)
--
-- Vector structure of dynamic but bounded length, generic
-- in the index type and element type.
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
-- $Date: 2015/10/24 19:36:47 $
--
-- $Log: bounded_vectors.ads,v $
-- Revision 1.3  2015/10/24 19:36:47  niklas
-- Moved to free licence.
--
-- Revision 1.2  2008-02-18 12:47:03  niklas
-- Added Drop_First and Add, for Programs.Call_Sets.
--
-- Revision 1.1  2000/06/28 08:36:33  holsti
-- First version.
--



generic

   type Index_Type is (<>);
   --
   -- The index type, e.g. Positive.

   type Element_Type is private;
   --
   -- The vector element type.

   type Vector_Type is array (Index_Type range <>) of Element_Type;
   --
   -- An unbounded vector can be converted to a "normal" vector
   -- of this type.


package Bounded_Vectors is


   type Bounded_Vector (First, Last : Index_Type) is private;
   --
   -- An ordered sequence of values of Element_Type, indexed
   -- by a dense sequence (range) of values of Index_Type.
   -- Given an index, the element at that index can be read
   -- or written. New elements can be added after the last one,
   -- but not before the first one.
   --
   -- The maximum index range is First .. Last. An attempt to
   -- add more elements will raise Constraint_Error.
   --
   -- The initial value of an ubounded vector is the null
   -- sequence (no elements).
   --
   -- This type has value semantics.


   function Length (V : Bounded_Vector) return Natural;
   --
   -- The number of elements in the vector.


   function First (V : Bounded_Vector) return Index_Type;
   --
   -- The index of the first element in the vector.
   -- This is always the discriminant "First".


   function Last (V : Bounded_Vector) return Index_Type;
   --
   -- The index of the last element in the vector.
   -- If the vector is null (length = 0), the returned
   -- value is Index_Type'Pred (First(V)), which may raise
   -- Constraint_Error.


   function Next (V : Bounded_Vector) return Index_Type;
   --
   -- The index that follows the last index in the vector.
   -- If the vector is null (length = 0) the return value is
   -- Index_Type'First, otherwise is it Index_Type'succ(Last(V)).


   procedure Set (
      Vector : in out Bounded_Vector;
      Index  : in     Index_Type;
      To     : in     Element_Type);
   --
   -- Changes the value of the Vector element at Index to
   -- the value To.
   --
   -- If Index is greater than Last(Vector), the values of
   -- elements for the indices between Last(Vector) and Index
   -- are undefined (unless Element_Type has a defined default
   -- value).
   --
   -- After Set, Length(Vector) and Last(Vector) will reflect the
   -- possible growth in the vector's length.


   procedure Append (
      To    : in out Bounded_Vector;
      Value : in     Element_Type);
   --
   -- Extends the To vector by one element, assigning it
   -- the given value.
   -- This is equivalent to Set (To, Next(To), Value).


   procedure Erase (Item : in out Bounded_Vector);
   --
   -- Truncates the vector to a null vector (zero length).


   function Element (
      Vector : Bounded_Vector;
      Index  : Index_Type)
   return Element_Type;
   --
   -- Returns the value of the element at the given Index in the
   -- given Vector.
   --
   -- If Index is not in the range First(Vector)..Last(Vector),
   -- Constraint_Error is raised.


   function Index (
      Source : Bounded_Vector;
      Value  : Element_Type)
   return Index_Type;
   --
   -- Searches the Source vector for an element with the given
   -- Value, and returns the (first) index where the value
   -- is found. If no element matches Value, the function
   -- returns Index_Type'Succ (Last(Vector)).


   procedure Drop_First (
      Value : in     Element_Type;
      From  : in out Bounded_Vector);
   --
   -- Removes the first element with the given Value from the given
   -- vector and fills the gap by shifting later elements down one index
   -- position. If no element matches the Value, the operation has no
   -- effect.


   function Is_Element (
      Source : Bounded_Vector;
      Value  : Element_Type)
   return Boolean;
   --
   -- Whether the given Value is an element of the Source vector.


   procedure Add (
      Value : in     Element_Type;
      To    : in out Bounded_Vector);
   --
   -- Adds the Value To the vector, if the Value is not in the
   -- vector originally. Propagates Constraint_Error if the Value
   -- does not fit in the vector.


   function To_Vector (V : Bounded_Vector) return Vector_Type;
   --
   -- Returns a normal (bounded) array that contains the same
   -- index range and the same values as the given vector.


private

   type Bounded_Vector (First, Last : Index_Type) is record
      Length : Natural := 0;
      Store  : Vector_Type (First .. Last);
   end record;

end Bounded_Vectors;

