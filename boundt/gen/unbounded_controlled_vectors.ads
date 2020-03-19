-- Unbounded_Controlled_Vectors (decl)
--
-- Vector structure of unbounded (dynamic) length, generic in
-- the element type, indexed by Positive, controlled for dynamic
-- memory usage.
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
-- $Date: 2015/10/24 19:36:53 $
--
-- $Log: unbounded_controlled_vectors.ads,v $
-- Revision 1.2  2015/10/24 19:36:53  niklas
-- Moved to free licence.
--
-- Revision 1.1  2009-10-07 19:26:11  niklas
-- BT-CH-0183: Cell-sets are a tagged-type class.
--


with Ada.Finalization;
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
   -- in terms of the number of elements it can hold.

   Size_Increment : Positive := 10;
   --
   -- When an unbounded vector grows past its current storage
   -- allocation, its allocation is increased by the necessary
   -- multiple of this number of elements.

   Deallocate : in out Boolean;
   --
   -- Option variable to enable or disable the use of
   -- Unchecked_Deallocation to discard unused heap memory.


package Unbounded_Controlled_Vectors is


   subtype Index_Type is Positive;
   --
   -- Vectors are indexed by values of this type, perhaps further
   -- constrained by First_Index, below.


   First_Index : constant := 1;
   --
   -- The indices always start with 1.


   subtype Real_Index_Type is Index_Type range First_Index .. Index_Type'Last;
   --
   -- The index range that can really be used.


   type Unbounded_Vector is new Ada.Finalization.Controlled with private;
   --
   -- An ordered sequence of values of Element_Type, indexed
   -- by a dense sequence (range) of values of Real_Index_Type.
   -- Given an index, the element at that index can be read
   -- or written. New elements can be added after the last one,
   -- but not before the first one.
   -- Currently, the index range of a vector always starts at
   -- First_Index, with the upper end dynamically set.
   --
   -- The initial value of an unbounded vector is the null sequence
   -- (no elements). See also Null_Unbounded_Vector, below.


   -- overriding
   procedure Adjust (Object : in out Unbounded_Vector);


   -- overriding
   procedure Finalize (Object : in out Unbounded_Vector);


   Null_Unbounded_Vector : constant Unbounded_Vector;
   --
   -- An empty sequence.
   --
   -- Null_Unbounded_Vector is the default initial value of every
   -- object object of type Unbounded_Vector.


   function "=" (Left, Right : Unbounded_Vector) return Boolean;
   --
   -- Whether the Left and Right have exactly equal elements
   -- and in the same order.


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
   -- After Set, Length (Vector) and Last (Vector) will reflect the
   -- possible growth in the vector's length.


   procedure Append (
      Value : in     Element_Type;
      To    : in out Unbounded_Vector);
   --
   -- Extends the To vector by one element, assigning it
   -- the given value.
   -- This is equivalent to Set (To, Next (To), Value).


   procedure Truncate_Length (
      Vector : in out Unbounded_Vector;
      To     : in     Natural);
   --
   -- Truncates the Vector To the given length, but leaves the
   -- "excess" storage (if any) allocated.
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
   -- If Index is not in the range First (Vector) .. Last (Vector),
   -- Constraint_Error is raised.


   function Index (
      Source : Unbounded_Vector;
      Value  : Element_Type)
   return Index_Type;
   --
   -- Searches the Source vector for an element with the given
   -- Value, and returns the (first) index where the value
   -- is found. If no element matches Value, the function
   -- returns Index_Type'Succ (Last (Vector)).


   procedure Drop (
      Index : in     Index_Type;
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


   procedure Move_All (
      From : in out Unbounded_Vector;
      To   : in out Unbounded_Vector);
   --
   -- Moves all elements From a given vector To another given
   -- vector when the element is not already in the other vector.
   -- That is, each element From the first vector is added To
   -- the second vector and removed from the first vector.
   -- If the two parameters are different vector objects, the
   -- procedure erases the From vector. If they are the same
   -- vector object, the procedure has no effect.


   function To_Vector (V : Unbounded_Vector) return Vector_Type;
   --
   -- Returns a normal (bounded) array that contains the same
   -- index range and the same values as the given vector.


   procedure Extend (
      Vector   : in out Unbounded_Vector;
      To_Index : in     Index_Type);
   --
   -- Extends the space allocated for the Vector at least To the
   -- given Index. The current length and contents of the Vector
   -- are not changed. Nothing is done if the Vector already has
   -- at least the required storage allocated.


   procedure Print_Debug_Pool;
   --
   -- Prints the information collected by GNAT.Debug_Pool, if enabled.


private


   type Shared_Vector_Type (Max_Length : Natural) is record
      Ref_Count : Natural;
      Length    : Natural := 0;
      Vector    : Vector_Type (1 .. Max_Length);
   end record;
   --
   -- A vector of at most Max_Length elements, currently with Length
   -- elements in Vector(1 .. Length). Thus Length <= Max_Length and
   -- may be < if there is some excess allocation of storage.
   --
   -- The vector may be shared as an element-container for several
   -- Unbounded_Vector objects. The Ref_Count component shows how
   -- many objects share this vector. A shared vector is immutable
   -- if Ref_Count is larger than 1. Any attempt to change such a
   -- shared vector creates a copy ("copy on change").


   type Shared_Vector_Ref is access Shared_Vector_Type;

   --:dbpool Shared_Vector_Pool : GNAT.Debug_Pools.Debug_Pool;

   --:dbpool for Shared_Vector_Ref'Storage_Pool use Shared_Vector_Pool;


   type Unbounded_Vector is new Ada.Finalization.Controlled with record
      Store : Shared_Vector_Ref;
   end record;
   --
   -- A vector of elements with an unbounded length.
   --
   -- Store
   --    The storage allocated for the elements, possibly shared with
   --    other Unbounded_Vector objects.
   --    If Store = null the vector is logically a null vector of
   --    zero length.


   Null_Unbounded_Vector : constant Unbounded_Vector := (
      Ada.Finalization.Controlled with Store => null);


end Unbounded_Controlled_Vectors;
