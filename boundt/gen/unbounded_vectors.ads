-- Unbounded_Vectors (decl)
--
-- Vector structure of unbounded (dynamic) length, generic in
-- the element type, indexed by Positive.
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
-- $Revision: 1.15 $
-- $Date: 2015/10/24 19:36:53 $
--
-- $Log: unbounded_vectors.ads,v $
-- Revision 1.15  2015/10/24 19:36:53  niklas
-- Moved to free licence.
--
-- Revision 1.14  2013-02-03 12:27:27  niklas
-- Added Insert and Drop_Slice.
--
-- Revision 1.13  2008-01-13 21:32:09  niklas
-- BT-CH-0105: Correct Unbounded_Vectors.Grow.
--
-- Revision 1.12  2007/10/26 12:44:36  niklas
-- BT-CH-0091: Reanalyse a boundable edge only if its domain grows.
--
-- Revision 1.11  2007/04/26 11:28:05  niklas
-- BT-CH-0059.
--
-- Revision 1.10  2007/04/18 18:34:40  niklas
-- BT-CH-0057.
--
-- Revision 1.9  2007/03/29 15:18:05  niklas
-- BT-CH-0056.
--
-- Revision 1.8  2007/01/25 21:25:21  niklas
-- BT-CH-0043.
--
-- Revision 1.7  2006/05/17 20:06:09  niklas
-- Added Find_Or_Add.
--
-- Revision 1.6  2005/02/16 21:11:49  niklas
-- BT-CH-0002.
--
-- Revision 1.5  2004/04/25 07:29:48  niklas
-- First Tidorum version. Added Drop procedure.
--
-- Revision 1.4  2000/08/18 18:01:15  holsti
-- Index type Positive (not generic) to help null vectors.
--
-- Revision 1.3  2000/06/11 21:36:53  holsti
-- Added Erase procedure.
--
-- Revision 1.2  2000/06/11 19:04:34  holsti
-- Added Next, Append, Is_Element.
--
-- Revision 1.1  2000/05/19 13:37:04  holsti
-- First version.
--

--:dbpool with GNAT.Debug_Pools;


generic

   type Element_Type is private;
   --
   -- The vector element type.

   type Vector_Type is array (Positive range <>) of Element_Type;
   --
   -- An unbounded vector can be converted to a "normal" vector
   -- of this type.

   First_Index : Positive := 1;
   --
   -- The default first index of any unbounded vector.

   Initial_Size : Positive := 5;
   --
   -- The initial storage allocation for an unbounded vector,
   -- in terms of the number of elements it can hold.

   Size_Increment : Positive := 10;
   --
   -- When an unbounded vector grows past its current storage
   -- allocation, its allocation is increased by this number of
   -- elements.

   Deallocate : in out Boolean;
   --
   -- Option variable to enable or disable the use of
   -- Unchecked_Deallocation to discard unused heap memory.


package Unbounded_Vectors is


   subtype Index_Type is Positive;
   --
   -- Vectors are indexed by values of this type, perhaps further
   -- constrained by First_Index, below.


   subtype Real_Index_Type is Index_Type range First_Index .. Index_Type'Last;
   --
   -- The index range that can really be used.


   type Unbounded_Vector is private;
   --
   -- An ordered sequence of values of Element_Type, indexed
   -- by a dense sequence (range) of values of Real_Index_Type.
   -- Given an index, the element at that index can be read
   -- or written. New elements can be added after the last one,
   -- but not before the first one.
   -- Currently, the index range of a vector always starts at
   -- First_Index, with the upper end dynamically set.
   --
   -- The initial value of an ubounded vector is the null sequence
   -- (no elements). See also Null_Unbounded_Vector, below.
   --
   -- Partly reference semantics, should be derived from
   -- a controlled type.


   Null_Unbounded_Vector : constant Unbounded_Vector;
   --
   -- An empty sequence.
   --
   -- Null_Unbounded_Vector is the default initial value of every
   -- object object of type Unbounded_Vector.


   function Length (V : Unbounded_Vector) return Natural;
   --
   -- The number of elements in the vector.


   function First (V : Unbounded_Vector) return Index_Type;
   --
   -- The index of the first element in the vector.
   -- Currently, this is always First_Index, but this
   -- function is provided for symmetry and for possible
   -- future extensions.


   function Last (V : Unbounded_Vector) return Natural;
   --
   -- The index of the last element in the vector.
   -- If the vector is null (length = 0), the returned
   -- value is Index_Type'Pred (First(V)).
   -- This will currently be zero, since First_Index = 1.


   function Next (V : Unbounded_Vector) return Index_Type;
   --
   -- The index that follows the last index in the vector.
   -- If the vector is null (length = 0) the return value is
   -- First_Index, otherwise it is Index_Type'Succ(Last(V)).


   procedure Set (
      Vector : in out Unbounded_Vector;
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


   procedure Insert (
      Vector : in out Unbounded_Vector;
      Index  : in     Index_Type;
      Value  : in     Element_Type);
   --
   -- Insert the Value in the Vector at Index. If this Index position
   -- was aslready filled (<= Last (To)), shifts it and the later
   -- elements in the vector up by one position to make room for the
   -- new value.
   --
   -- If Index is greater than Last(Vector), the values of
   -- elements for the indices between Last(Vector) and Index
   -- are undefined (unless Element_Type has a defined default
   -- value).
   --
   -- After Insert, Length(Vector) and Last(Vector) will reflect the
   -- possible growth in the vector's length.


   procedure Append (
      To    : in out Unbounded_Vector;
      Value : in     Element_Type);
   --
   -- Extends the To vector by one element, assigning it
   -- the given value.
   -- This is equivalent to Set (To, Next(To), Value).


   procedure Truncate_Length (
      Vector : in out Unbounded_Vector;
      To     : in     Natural);
   --
   -- Truncates the Vector To the given length. Does not try to
   -- deallocate storage.
   --
   -- Propagates Constraint_Error if the Vector is shorter than
   -- the desired length (To), in other words if the operation
   -- tries to lengthen the Vector.


   procedure Erase (Item : in out Unbounded_Vector);
   --
   -- Truncates the vector to a null vector (zero length)
   -- and discards the allocated storage, if any.


   function Element (
      Vector : Unbounded_Vector;
      Index  : Index_Type)
   return Element_Type;
   --
   -- Returns the value of the element at the given Index in the
   -- given Vector.
   --
   -- If Index is not in the range First(Vector) .. Last(Vector),
   -- Constraint_Error is raised.


   function Index (
      Source : Unbounded_Vector;
      Value  : Element_Type)
   return Index_Type;
   --
   -- Searches the Source vector for an element with the given
   -- Value, and returns the (first) index where the value
   -- is found. If no element matches Value, the function
   -- returns Index_Type'Succ (Last(Vector)).


   procedure Drop (
      Index : in     Index_Type;
      From  : in out Unbounded_Vector);
   --
   -- Removes the element at the given Index From the given vector
   -- and fills the gap by shifting later elements down one index
   -- position. Raises Constraint_Error if the Index is not in the
   -- range First(From) .. Last(From).


   procedure Drop_Slice (
      First_Drop : in     Index_Type;
      Last_Drop  : in     Natural;
      From       : in out Unbounded_Vector);
   --
   -- Removes the elements in the given index range From the given
   -- vector and fills the gap by shifting later elements down the
   -- required number of index positions. Raises Constraint_Error if
   -- either First_Drop or Last_Drop is not in the range
   -- First(From) .. Last(From).
   -- If First_Drop > Last_Drop the operation does nothing (not even
   -- the check for indices in range).


   function Is_Element (
      Source : Unbounded_Vector;
      Value  : Element_Type)
   return Boolean;
   --
   -- Whether the given Value is an element of the Source vector.


   procedure Find_Or_Add (
      Value  : in     Element_Type;
      Vector : in out Unbounded_Vector;
      Index  :    out Index_Type);
   --
   -- Finds the Value in the Vector and returns the (first) Index
   -- where Element (Vector, Index) = Value. If the Value is not
   -- in the Vector originally, the procedure adds it to the Vector
   -- (using Append) and returns the new Index which in this case
   -- equals Last (Vector) on return. Note that Last (Vector) has
   -- in this case increased by one relative to its value before
   -- the call.


   procedure Add (
      Value : in     Element_Type;
      To    : in out Unbounded_Vector);
   --
   -- Adds the Value To the vector, if the Value is not in the
   -- vector originally. Same as Find_Or_Add but ignoring the Index.


   function To_Vector (V : Unbounded_Vector) return Vector_Type;
   --
   -- Returns a normal (bounded) array that contains the same
   -- index range and the same values as the given vector.


private

   type Vector_Ref is access Vector_Type;

   --:dbpool Vector_Pool : GNAT.Debug_Pools.Debug_Pool;

   --:dbpool for Vector_Ref'Storage_Pool use Vector_Pool;


   type Unbounded_Vector is record
      Length : Natural := 0;
      Store  : Vector_Ref;
   end record;
   --
   -- Length
   --    The number of elements in the vector, currently.
   -- Store
   --    The storage allocated for the elements.
   --    If Store /= null then Store'Length >= Length, but there
   --    can be some excess allocation with Store'Length > Length.
   --
   -- If Length > 0 the elements in the vector are
   -- Store(First_Index .. First_Index - Length - 1).
   --
   -- If Store = null then Length = 0. The opposite does not hold;
   -- we can have vectors with Length = 0 and Store /= null.


   Null_Unbounded_Vector : constant Unbounded_Vector := (
      Length => 0,
      Store  => null);


end Unbounded_Vectors;
