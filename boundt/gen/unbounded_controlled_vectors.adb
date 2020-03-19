-- Unbounded_Controlled_Vectors (body)
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
-- $Log: unbounded_controlled_vectors.adb,v $
-- Revision 1.2  2015/10/24 20:05:52  niklas
-- Moved to free licence.
--
-- Revision 1.1  2009-10-07 19:26:10  niklas
-- BT-CH-0183: Cell-sets are a tagged-type class.
--


with Unchecked_Deallocation;


package body Unbounded_Controlled_Vectors is


   procedure Free is new Unchecked_Deallocation (
      Object => Shared_Vector_Type,
      Name   => Shared_Vector_Ref);


   procedure Unreference (Item : in out Shared_Vector_Ref)
   --
   -- Removes one reference from the shared Item, which is
   -- not null on entry but may be null on return, if the
   -- Item was the last reference to this shared vector object.
   --
   is
   begin

       Item.Ref_Count := Item.Ref_Count - 1;

       if Item.Ref_Count > 0 or not Deallocate then
          -- There are other users of this Store, or we
          -- are asked not to deallocate useless storage.

          Item := null;

       else
          -- This Item was the last reference to Item.all, and
          -- we are asked to deallocate useless storage.

          Free (Item);

       end if;

   end Unreference;


   -- overriding
   procedure Adjust (Object : in out Unbounded_Vector)
   is
   begin

      if Object.Store /= null then

         Object.Store.Ref_Count := Object.Store.Ref_Count + 1;

      end if;

   end Adjust;


   -- overriding
   procedure Finalize (Object : in out Unbounded_Vector)
   is
   begin

      if Object.Store /= null then

         Unreference (Object.Store);

      end if;

   end Finalize;


   function "=" (Left, Right : Unbounded_Vector) return Boolean
   is

      Left_Length : constant Natural := Length (Left);

   begin

      return Left.Store = Right.Store
      or else (
         Left_Length = Length (Right)
         and then (
            Left_Length = 0
         or else
              Left.Store.Vector (1 .. Left_Length)
            = Right.Store.Vector(1 .. Left_Length)));
      --
      -- The check for Left_Length = 0 avoids a possible null
      -- access error in the comparison of the Xxx.Store.Vectors
      -- that could otherwise happen when one, but not both, of
      -- Left.Store and Right.Store is null.

   end "=";


   function Length (V : Unbounded_Vector) return Natural
   is
   begin

      if V.Store = null then

         return 0;

      else

         return V.Store.Length;

      end if;

   end Length;


   function First (V : Unbounded_Vector) return Index_Type
   is
   begin

      return First_Index;

   end First;


   function Last (V : Unbounded_Vector) return Natural
   is
   begin

      return Index_Type'Pred (First (V) + Length (V));

   end Last;


   function Next (V : Unbounded_Vector) return Index_Type
   is
   begin

      return First (V) + Length (V);

   end Next;


   function New_Length (Space : Natural)
   return Natural
   --
   -- The Max_Length for a vector that should have room for
   -- at least Space elements.
   --
   is

      Incs : Natural;
      -- The number of Size_Increments needed.

      Length : Natural;
      -- The result.

   begin

      if Space <= Initial_Size then

         Incs := 0;

      else

         Incs := (Space - Initial_Size + Size_Increment - 1)
                 / Size_Increment;

      end if;

      Length := Initial_Size + Incs * Size_Increment;

      if Length < Space then

         raise Program_Error;

      end if;

      return Length;

   end New_Length;


   function Length_To_Include (Index : Index_Type)
   return Natural
   --
   -- The minimum Max_Length for a Store such that the Store
   -- includes the given Index.
   --
   is
   begin

      return New_Length (Space => Index - First_Index + 1);

   end Length_To_Include;


   procedure Ensure_Ownership (
      Vector: in out Unbounded_Vector;
      Space : in     Natural)
   --
   -- Ensures that the Vector is mutable by not sharing its Store
   -- with any other object, and also that it has a Store with Space
   -- for at least a given number of elements.
   --
   is

      Old_Store : Shared_Vector_Ref;
      -- A reference to the old storage for the Vector.

   begin

      if Vector.Store = null then
         -- No storage, shared or private.

         if Space > 0 then
            -- Some storage is needed.

            Vector.Store := new Shared_Vector_Type'(
                  Max_Length => New_Length (Space),
                  Ref_Count  => 1,
                  Length     => 0,
                  Vector     => <>);

         end if;

      elsif Vector.Store.Ref_Count  > 1
      or    Vector.Store.Max_Length < Space
      then
         -- New storage must be allocated.

         Old_Store := Vector.Store;

         Vector.Store := new Shared_Vector_Type'(
               Max_Length => Natural'Max (
                  Old_Store.Max_Length,
                  New_Length (Space)),
               Ref_Count  => 1,
               Length     => Old_Store.Length,
               Vector     => <>);

         Vector.Store.Vector(First (Vector) .. Last (Vector)) :=
            Old_Store.Vector(First (Vector) .. Last (Vector));

         Unreference (Old_Store);

      end if;

   end Ensure_Ownership;


   procedure Set (
      Vector : in out Unbounded_Vector;
      Index  : in     Index_Type;
      To     : in     Element_Type)
   is
   begin

      Ensure_Ownership (Vector, Length_To_Include (Index));

      Vector.Store.Vector(Index) := To;

      if Index >= Last (Vector) then

         Vector.Store.Length := Index - First_Index + 1;

      end if;

   end Set;


   procedure Append (
      Value : in     Element_Type;
      To    : in out Unbounded_Vector)
   is
   begin

      Set (
         Vector => To,
         Index  => Next (To),
         To     => Value);

   end Append;


   procedure Truncate_Length (
      Vector : in out Unbounded_Vector;
      To     : in     Natural)
   is
   begin

      if To < Length (Vector) then
         -- Truncating the length of a non-null vector.

         Ensure_Ownership (Vector => Vector, Space => 0);

         Vector.Store.Length := To;

      elsif To > Length (Vector) then
         -- Attempting to "truncate" a vector to a longer length.

         raise Constraint_Error;
     --
     -- else To = Length (Vector) and so nothing need be done.

      end if;

   end Truncate_Length;


   procedure Erase (Item : in out Unbounded_Vector)
   is
   begin

      Finalize (Item);

   end Erase;


   function Element (
      Vector : Unbounded_Vector;
      Index  : Index_Type)
   return Element_Type
   is
   begin

      return Vector.Store.Vector(Index);

   end Element;


   function Index (
      Source : Unbounded_Vector;
      Value  : Element_Type)
   return Index_Type
   is
   begin

      for I in First (Source) .. Last (Source) loop

         if Source.Store.Vector(I) = Value then

            return I;

         end if;

      end loop;

      return Index_Type'Succ (Last (Source));

   end Index;


   procedure Drop (
      Index : in     Index_Type;
      From  : in out Unbounded_Vector)
   is

      Last_Index : constant Index_Type := Last (From);

   begin

      if Index in First (From) .. Last_Index then
         -- Valid drop.

         Ensure_Ownership (Vector => From, Space => 0);

         From.Store.Vector(Index .. Last_Index - 1) :=
            From.Store.Vector(Index + 1 .. Last_Index);

         From.Store.Length := From.Store.Length - 1;

      else
          -- Aaargh.

         raise Constraint_Error;

      end if;

   end Drop;


   function Is_Element (
      Source : Unbounded_Vector;
      Value  : Element_Type)
   return Boolean
   is
   begin

      for I in First(Source) .. Last(Source) loop

         if Source.Store.Vector(I) = Value then

            return True;

         end if;

      end loop;

      return False;

   end Is_Element;


   procedure Find_Or_Add (
      Value  : in     Element_Type;
      Vector : in out Unbounded_Vector;
      Index  :    out Index_Type)
   is
   begin

      Index := Unbounded_Controlled_Vectors.Index (
         Source => Vector,
         Value  => Value);

      if Index > Last (Vector) then
         -- This Value is not yet in the Vector.

         Append (To => Vector, Value => Value);

         Index := Last (Vector);

      end if;

   end Find_Or_Add;


   procedure Add (
      Value : in     Element_Type;
      To    : in out Unbounded_Vector)
   is

      Ignore : Index_Type;

   begin

      Find_Or_Add (Value => Value, Vector => To, Index => Ignore);

   end Add;


   procedure Move_All (
      From : in out Unbounded_Vector;
      To   : in out Unbounded_Vector)
   is

      From_Store : Shared_Vector_Ref := From.Store;

   begin

      if      From_Store = null
      or else From_Store.Length = 0
      then
         -- Nothing to move, From is empty.

         null;

      elsif To.Store = null then
         -- Simple case: just move the reference.

         To.Store := From_Store;
                     From.Store := null;

      elsif From_Store = To.Store then
         -- No effect on the To vector, but From may become
         -- empty.

         From.Store := null;

         if To.Store = null then
            -- Aha, From and To are the same object, thus
            -- From shall not change.

            From.Store := From_Store;
            -- This also sets To.Store := From_Store (= original To.Store).

         else
            -- From and To are different objects, so From
            -- shall be empty on return. We leave From.Store
            -- as null and remove the reference:

            Unreference (From_Store);

         end if;

      else
         -- From and To are different vector objects and From
         -- is not null nor empty (and To.Store is not null,
         -- but that is not important).

         for I in First (From) .. Last (From) loop

            Add (Value => Element (From, I),
                 To    => To);

         end loop;

         Erase (From);

      end if;

   end Move_All;


   Null_Vector : Vector_Type (First_Index .. 0) := (others => <>);
   --
   -- A vector of zero length.
   --
   -- pragma Warnings (Off, Null_Vector);
   --
   -- To suppress the Gnat warning that Null_Vector is not initialized
   -- and is never assigned a value.


   function To_Vector (V : Unbounded_Vector) return Vector_Type
   is
   begin

      if Length (V) = 0 then

         return Null_Vector;

      else

         return V.Store.Vector(First (V) .. Last (V));

      end if;

   end To_Vector;


   procedure Extend (
      Vector   : in out Unbounded_Vector;
      To_Index : in     Index_Type)
   is

      Min_Length : constant Natural := Length_To_Include (To_Index);
      -- The minimum length that covers To_Index.

   begin

      if      Vector.Store = null
      or else Vector.Store.Max_Length < Min_Length
      then

         Ensure_Ownership (Vector => Vector, Space => Min_Length);

      end if;

   end Extend;


   procedure Print_Debug_Pool
   is
   begin

      --:dbpool GNAT.Debug_Pools.Print_Info_Stdout (Shared_Vector_Pool);

      null;

   end Print_Debug_Pool;


end Unbounded_Controlled_Vectors;
