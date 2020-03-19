-- Unbounded_Vectors (body)
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
-- $Revision: 1.19 $
-- $Date: 2015/10/24 20:05:52 $
--
-- $Log: unbounded_vectors.adb,v $
-- Revision 1.19  2015/10/24 20:05:52  niklas
-- Moved to free licence.
--
-- Revision 1.18  2013-02-03 12:27:27  niklas
-- Added Insert and Drop_Slice.
--
-- Revision 1.17  2008-01-13 21:32:09  niklas
-- BT-CH-0105: Correct Unbounded_Vectors.Grow.
--
-- Revision 1.16  2007/10/26 12:44:36  niklas
-- BT-CH-0091: Reanalyse a boundable edge only if its domain grows.
--
-- Revision 1.15  2007/04/26 11:28:05  niklas
-- BT-CH-0059.
--
-- Revision 1.14  2007/03/29 15:18:05  niklas
-- BT-CH-0056.
--
-- Revision 1.13  2007/01/25 21:25:20  niklas
-- BT-CH-0043.
--
-- Revision 1.12  2006/05/17 20:06:08  niklas
-- Added Find_Or_Add.
--
-- Revision 1.11  2005/02/25 10:32:23  niklas
-- Stop warnings about uninitialized Null_Vector.
--
-- Revision 1.10  2004/04/25 07:29:48  niklas
-- First Tidorum version. Added Drop procedure.
--
-- Revision 1.9  2001/01/19 12:18:39  saarinen
-- Fixed Erase to null the store.
--
-- Revision 1.8  2000/10/26 10:49:33  saarinen
-- Fixed freeing of memory in Grow.
--
-- Revision 1.7  2000/10/26 09:31:54  saarinen
-- Fixed procedures Grow and Set.
--
-- Revision 1.6  2000/08/18 18:01:14  holsti
-- Index type Positive (not generic) to help null vectors.
--
-- Revision 1.5  2000/07/12 12:52:15  saarinen
-- Fixed 0-length vector handling.
--
-- Revision 1.4  2000/06/11 21:36:52  holsti
-- Added Erase procedure.
--
-- Revision 1.3  2000/06/11 19:04:50  holsti
-- Added Next, Append, Is_Element.
--
-- Revision 1.2  2000/06/09 09:13:49  holsti
-- Some typos corrected.
--
-- Revision 1.1  2000/05/19 13:37:03  holsti
-- First version.
--


with Faults;
with Unchecked_Deallocation;


package body Unbounded_Vectors is


   procedure Free is new Unchecked_Deallocation (
      Object => Vector_Type,
      Name   => Vector_Ref);


   function Length (V : Unbounded_Vector) return Natural
   is
   begin

      return V.Length;

   end Length;


   function First (V : Unbounded_Vector) return Index_Type
   is
   begin

      return First_Index;

   end First;


   function Last (V : Unbounded_Vector) return Natural
   is
   begin

      return Index_Type'Pred (First(V) + V.Length);

   end Last;


   function Next (V : Unbounded_Vector) return Index_Type
   is
   begin

      if V.Length = 0 then

         return First (V);

      else

         return Index_Type'succ (Last(V));

      end if;

   end Next;


   procedure Grow (
      Vector     : in out Unbounded_Vector;
      To_Include : in     Real_Index_Type)
   --
   -- Increase the storage allocated to the Vector so that it
   -- includes at least the index To_Include.
   --
   is

      Store_First : Real_Index_Type;
      -- The First index for the vector.

      New_Length : Positive;
      -- The new storage length (not the Vector length).

      New_Store_Last : Real_Index_Type;
      -- The last index in the new storage.

      New_Store  : Vector_Ref;
      -- The new storage.

   begin

      if Vector.Store = null then
         -- First allocation for this Vector.

         Store_First := First_Index;  -- Here we could set 'First.

         New_Length := Positive'Max (
            Initial_Size,
            To_Include - First_Index + 1);

      else
         -- A new allocation to increase the length.

         Store_First := First(Vector);

         New_Length := (To_Include - First (Vector) + 1) + Size_Increment;

      end if;

      New_Store_Last := Store_First + (New_Length - 1);

      New_Store := new Vector_Type(Store_First .. New_Store_Last);

      if Vector.Store /= null then
         -- The old storage may have to be copied and discarded.

         if Vector.Length > 0 then

            New_Store (Vector.Store'range) := Vector.Store.all;

         end if;

         if Deallocate then

            Free (Vector.Store);

         else

            Vector.Store := null;

         end if;

      end if;

      Vector.Store := New_Store;

   exception

   when others => Faults.Deallocation;

   end Grow;


   procedure Set (
      Vector : in out Unbounded_Vector;
      Index  : in     Index_Type;
      To     : in     Element_Type)
   is

      Offset : constant Natural := Index - First(Vector);
      -- The distance (length) from the start of the vector
      -- to the location being set.

   begin

      if Vector.Store = null
      or else Index > Vector.Store'Last
      then
         -- A virgin vector, or one that needs to grow.

         Grow (Vector => Vector, To_Include => Index);

      end if;

      Vector.Store(Index) := To;

      if Offset >= Vector.Length then

         Vector.Length := Offset + 1;

      end if;

   end Set;


   procedure Insert (
      Vector : in out Unbounded_Vector;
      Index  : in     Index_Type;
      Value  : in     Element_Type)
   is

      New_Last : Index_Type;
      -- The new Last index.

   begin

      -- Find the new Last index:

      if Index > Last (Vector) then
         -- No displaced element.

         New_Last := Index;

      else
         -- Some displaced elements.

         New_Last := Last (Vector) + 1;

      end if;

      -- Allocate more space if required:

      if Vector.Store = null
      or else New_Last > Vector.Store'Last
      then

         Grow (Vector => Vector, To_Include => New_Last);

      end if;

      -- Shift the displaced elements up:

      for V in reverse Index + 1.. New_Last loop

         Vector.Store(V) := Vector.Store(V - 1);

      end loop;

      -- Store the inserted element:

      Vector.Store(Index) := Value;

      Vector.Length := New_Last - First(Vector) + 1;

   end Insert;


   procedure Append (
      To    : in out Unbounded_Vector;
      Value : in     Element_Type)
   is
   begin

      Set (
         Vector => To,
         Index  => Next(To),
         To     => Value);

   end Append;


   procedure Truncate_Length (
      Vector : in out Unbounded_Vector;
      To     : in     Natural)
   is
   begin

      if To > Vector.Length then

         raise Constraint_Error;

      end if;

      Vector.Length := To;

   end Truncate_Length;


   procedure Erase (Item : in out Unbounded_Vector)
   is
   begin

      Item.Length := 0;

      if Deallocate then

         Free (Item.Store);

      else

         Item.Store := null;

      end if;

   end Erase;


   function Element (
      Vector : Unbounded_Vector;
      Index  : Index_Type)
   return Element_Type
   is
   begin

      return Vector.Store(Index);

   end Element;


   function Index (
      Source : Unbounded_Vector;
      Value  : Element_Type)
   return Index_Type
   is
   begin

      if Source.Length = 0 then
         -- The vector is empty.

         return First_Index;

      else
         -- The vector is not empty.

         for I in First(Source) .. Last(Source) loop

            if Source.Store(I) = Value then

               return I;

            end if;

         end loop;

         return Index_Type'Succ (Last(Source));

      end if;

   end Index;


   procedure Drop (
      Index : in     Index_Type;
      From  : in out Unbounded_Vector)
   is

      Last_Index : constant Index_Type := Last (From);

   begin

      if       From.Store /= null
      and then Index in First (From) .. Last_Index
      then
         -- Valid drop.

         From.Store(Index .. Last_Index - 1) :=
            From.Store(Index + 1 .. Last_Index);

         From.Length := From.Length - 1;

      else
          -- Aaargh.

          raise Constraint_Error;

      end if;

   end Drop;


   procedure Drop_Slice (
      First_Drop : in     Index_Type;
      Last_Drop  : in     Natural;
      From       : in out Unbounded_Vector)
   is

      Last_Index : constant Natural := Last (From);
      Num_Drop   : constant Integer := Last_Drop - First_Drop + 1;

   begin

      if First_Drop > Last_Drop then
         -- Null slice. Nothing to do.

         null;

      elsif    From.Store /= null
      and then First_Drop in First (From) .. Last_Index
      and then Last_Drop  in First (From) .. Last_Index
      then
         -- Valid drop.

         From.Store(First_Drop .. Last_Index - Num_Drop) :=
            From.Store(Last_Drop + 1 .. Last_Index);

         From.Length := From.Length - Num_Drop;

      else
          -- Aaargh.

          raise Constraint_Error;

      end if;

   end Drop_Slice;


   function Is_Element (
      Source : Unbounded_Vector;
      Value  : Element_Type)
   return Boolean
   is
   begin

      if Source.Length = 0 then
         -- The vector is empty and cannot contain the Value.

         return False;

      else
         -- The vector is not empty, so we search it.

         for I in First(Source) .. Last(Source) loop

            if Source.Store(I) = Value then

               return True;

            end if;

         end loop;

         return False;

      end if;

   end Is_Element;


   procedure Find_Or_Add (
      Value  : in     Element_Type;
      Vector : in out Unbounded_Vector;
      Index  :    out Index_Type)
   is
   begin

      Index := Unbounded_Vectors.Index (
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


   Null_Vector : Vector_Type(Index_Type'Succ(First_Index) .. First_Index);
   --
   -- A vector of zero length.
   --
   pragma Warnings (Off, Null_Vector);
   --
   -- To suppress the Gnat warning that Null_Vector is not initialized
   -- and is never assigned a value.


   function To_Vector (V : Unbounded_Vector) return Vector_Type
   is
   begin

      if V.Length = 0 then

         return Null_Vector;

      else

         return V.Store (First(V) .. Last(V));

      end if;

   end To_Vector;


end Unbounded_Vectors;
