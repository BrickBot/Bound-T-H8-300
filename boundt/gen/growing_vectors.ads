-- Growing_Vectors (decl)
--
-- Vector structure of unbounded (dynamic) length, generic in
-- the element type, indexed by Positive, intended for vectors
-- that frequently grow beyond their current allocation.
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
-- $Date: 2015/10/24 19:36:49 $
--
-- $Log: growing_vectors.ads,v $
-- Revision 1.5  2015/10/24 19:36:49  niklas
-- Moved to free licence.
--
-- Revision 1.4  2007-10-26 12:44:36  niklas
-- BT-CH-0091: Reanalyse a boundable edge only if its domain grows.
--
-- Revision 1.3  2007/04/18 18:34:39  niklas
-- BT-CH-0057.
--
-- Revision 1.2  2007/03/29 15:18:03  niklas
-- BT-CH-0056.
--
-- Revision 1.1  2007/03/28 13:38:10  niklas
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

   Initial_Size : Positive := 5;
   --
   -- The initial storage allocation for an unbounded vector,
   -- in terms of the number of elements it can hold without
   -- any dynamic memory allocation.

   First_Increment : Positive := 10;
   --
   -- When an unbounded vector grows past its Initial_Size, an
   -- extension is dynamically allocated to hold this number of
   -- additional elements, for a total maximum length of Initial_Size
   -- plus First_Increment. Later increases follow a Fibonacci-like
   -- sequence so that the size of the new extension is the sum of
   -- of sizes of the last two extensions. The size of the second
   -- extension is thus Initial_Size + First_Increment, etc.
   -- However, see Max_Increment below.

   Max_Increment : Positive := 1_000;
   --
   -- An upper bound on the size of the dynamically allocated extensions
   -- to an unbounded vector.

   Deallocate : in out Boolean;
   --
   -- Option variable to enable or disable the use of
   -- Unchecked_Deallocation to discard unused heap memory.


package Growing_Vectors is


   type Unbounded_Vector is private;
   --
   -- An ordered sequence of values of Element_Type, indexed
   -- by a dense sequence (range) of values 1 .. Last (vector).
   -- Given an index, the element at that index can be read
   -- or written. New elements can be added after the last one,
   -- but not before the first one.
   --
   -- The initial value of an unbounded vector is the null sequence
   -- (no elements).
   --
   -- Partly reference semantics, should be derived from
   -- a controlled type.


   function Length (V : Unbounded_Vector) return Natural;
   --
   -- The number of elements in the vector.


   function First (V : Unbounded_Vector) return Positive;
   --
   -- The index of the first element in the vector.
   -- Currently, this is always 1, but this function is provided
   -- for symmetry and for possible future extensions.


   function Last (V : Unbounded_Vector) return Natural;
   --
   -- The index of the last element in the vector.
   --
   -- If the vector is null (length = 0), the returned
   -- value is Index_Type'Pred (First (V)).
   -- This will currently be zero, since First = 1.


   function Next (V : Unbounded_Vector) return Positive;
   --
   -- The index that follows the last index in the vector.
   -- If the vector is null (length = 0) the return value is
   -- First (V) otherwise is it Index_Type'succ (Last (V)).


   procedure Set (
      Vector : in out Unbounded_Vector;
      Index  : in     Positive;
      To     : in     Element_Type);
   --
   -- Changes the value of the Vector element at Index To a given value.
   --
   -- If Index is greater than Last (Vector), the values of elements for
   -- the indices between Last (Vector) and Index are undefined (unless
   -- Element_Type has a defined default value).
   --
   -- After Set, Length (Vector) and Last (Vector) will reflect the
   -- possible growth in the vector's length.


   procedure Append (
      To    : in out Unbounded_Vector;
      Value : in     Element_Type);
   --
   -- Extends the To vector by one element, assigning it the given Value.
   -- This is equivalent to Set (To, Next (To), Value).


   procedure Erase (Item : in out Unbounded_Vector);
   --
   -- Truncates the vector to a null vector (zero length), deallocating
   -- all dynamically allocated memory (if allowed by Deallocate).


   function Element (
      Vector : Unbounded_Vector;
      Index  : Positive)
   return Element_Type;
   --
   -- Returns the value of the element at the given Index in the
   -- given Vector.
   --
   -- If Index is not in the range First (Vector) .. Last (Vector),
   -- Constraint_Error is raised.


   function Index (
      Source : Unbounded_Vector;
      Value  : Element_Type)
   return Positive;
   --
   -- Searches the Source vector for an element with the given
   -- Value, and returns the (first) index where the value
   -- is found. If no element matches Value, the function
   -- returns Index_Type'Succ (Last(Vector)).


   procedure Drop (
      Index : in     Positive;
      From  : in out Unbounded_Vector);
   --
   -- Removes the element at the given Index From the given vector
   -- and fills the gap by shifting later elements down one index
   -- position. Raises Constraint_Error if the Index is not in the
   -- range First (From) .. Last (From).


   function Is_Element (
      Source : Unbounded_Vector;
      Value  : Element_Type)
   return Boolean;
   --
   -- Whether the given Value is an element of the Source vector.


   procedure Find_Or_Add (
      Value  : in     Element_Type;
      Vector : in out Unbounded_Vector;
      Index  :    out Positive);
   --
   -- Finds the Value in the Vector and returns the (first) Index
   -- where Element (Vector, Index) = Value). If the Value is not
   -- in the Vector originally, the procedure adds it to the Vector
   -- (using Append) and returns the new Index which in this case
   -- equals Last (Vector).


   procedure Add (
      Value : in     Element_Type;
      To    : in out Unbounded_Vector);
   --
   -- Adds the Value To the vector, if the Value is not in the
   -- vector originally. Same as Find_Or_Add but ignoring the Index.


   function To_Vector (Item : Unbounded_Vector) return Vector_Type;
   --
   -- Returns a normal (bounded) array that contains the same
   -- index range and the same values as the given vector.


private


   type Piece_Type (First, Last : Positive);

   type Piece_Ref is access Piece_Type;

   --:dbpool Piece_Pool : GNAT.Debug_Pools.Debug_Pool;

   --:dbpool for Piece_Ref'Storage_Pool use Piece_Pool;

   type Piece_Type (First, Last : Positive) is record
      Prev  : Piece_Ref;
      Store : Vector_Type (First .. Last);
   end record;
   --
   -- One piece of a growing vector.
   --
   -- Store holds the elements.
   -- Prev is a link to the preceding dynamically allocated piece,
   -- or (special case) to the last dynamically allocated piece.


   subtype First_Piece_Type is Piece_Type (First => 1, Last => Initial_Size);
   --
   -- The first piece, a direct component of the growing vector.
   -- Prev points to the last dynamically allocated piece.


   type Unbounded_Vector is record
      Length : Natural := 0;
      First  : First_Piece_Type;
   end record;
   --
   -- An unbounded vector object.
   --
   -- Length
   --    The current length. The valid indices are 1 .. Length.
   -- First
   --    The first piece. The head of the chain of dynamically added pieces.
   --
   -- The index ranges of all the pieces cover the total index range
   -- 1 .. Length with no gaps and no overlap. The index range of the
   -- highest piece usually extends beyond Length.
   --
   -- The pieces are linked in _descending_ order of index range. Thus,
   -- the piece First.Prev covers the highest part of the index range, and
   -- its Last component shows the total allocated capacity. The piece
   -- list ends with the first dynamically allocated piece which has a
   -- null Prev link.


end Growing_Vectors;
