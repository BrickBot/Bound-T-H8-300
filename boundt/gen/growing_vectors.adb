-- Growing_Vectors (body)
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
-- $Date: 2015/10/24 20:05:49 $
--
-- $Log: growing_vectors.adb,v $
-- Revision 1.5  2015/10/24 20:05:49  niklas
-- Moved to free licence.
--
-- Revision 1.4  2007-10-26 12:44:36  niklas
-- BT-CH-0091: Reanalyse a boundable edge only if its domain grows.
--
-- Revision 1.3  2007/03/29 15:18:03  niklas
-- BT-CH-0056.
--
-- Revision 1.2  2007/03/28 13:54:39  niklas
-- Corrected to use Natural where Length can be zero.
--
-- Revision 1.1  2007/03/28 13:38:09  niklas
-- First version.
--


with Faults;
with Unchecked_Deallocation;


package body Growing_Vectors is


   procedure Free is new Unchecked_Deallocation (
      Piece_Type,
      Piece_Ref);


   function Length (V : Unbounded_Vector) return Natural
   is
   begin

      return V.Length;

   end Length;


   function First (V : Unbounded_Vector) return Positive
   is
   begin

      return 1;

   end First;


   function Last (V : Unbounded_Vector) return Natural
   is
   begin

      return Positive'Pred (First(V) + V.Length);

   end Last;


   function Next (V : Unbounded_Vector) return Positive
   is
   begin

      if V.Length = 0 then

         return First (V);

      else

         return Positive'Succ (Last (V));

      end if;

   end Next;


   procedure Grow (
      Vector     : in out Unbounded_Vector;
      To_Include : in     Positive)
   --
   -- Increases the storage allocated to the Vector so that it
   -- includes at least the index To_Include.
   --
   is

      Capacity : Positive;
      -- The current capacity (maximum index without more allocation)
      -- of the Vector.

      Last_Piece : Piece_Ref := Vector.First.Prev;
      -- The last dynamically allocated piece in the Vector.

      Increment : Positive;
      -- The size of the next piece to allocate.

      First_New_Index : Positive;
      -- The first index of the new piece.

      New_Piece : Piece_Ref;
      -- The new piece, if allocated.

   begin

      if Last_Piece = null then
         -- There are no dynamically allocated pieces.

         Capacity := Vector.First.Last;

      else
         -- There are some dynamically allocated pieces.

         Capacity := Last_Piece.Last;

      end if;

      -- Allocate more capacity if necessary:

      while Capacity < To_Include loop

         -- Compute the size Increment to be allocated:

         if Last_Piece = null then
            -- No dynamic pieces yet allocated.

            Increment := First_Increment;

            First_New_Index := Vector.First.Last + 1;

         else

           if Last_Piece.Prev = null then
            -- Only one dynamic piece allocated.

              Increment := Initial_Size + Last_Piece.Store'Length;

            else
               -- Two or more dynamic pieces allocated.

               Increment := Last_Piece.Store'Length
                          + Last_Piece.Prev.Store'Length;

            end if;

            First_New_Index := Last_Piece.Last + 1;

         end if;

         Increment := Positive'Min (Increment, Max_Increment);

         -- Allocate and link the new piece:

         New_Piece := new Piece_Type (
            First => First_New_Index,
            Last  => First_New_Index + Increment - 1);

         New_Piece.Prev := Last_Piece;

         Last_Piece := New_Piece;

         Vector.First.Prev := Last_Piece;

         Capacity := Last_Piece.Last;

      end loop;

   end Grow;


   procedure Set (
      Vector : in out Unbounded_Vector;
      Index  : in     Positive;
      To     : in     Element_Type)
   is

      Piece : Piece_Ref;
      -- The piece in which the Index lies, if not Vector.First.

   begin

      if Index <= Vector.First.Last then
         -- Fits in the first piece.

         Vector.First.Store(Index) := To;

      else
         -- Must look at the dynamic pieces and perhaps allocate more.

         Piece := Vector.First.Prev;
         -- The last allocate piece, with the highest index range.

         if      Piece = null
         or else Index > Piece.Last
         then
            -- Must add capacity.

            Grow (Vector => Vector, To_Include => Index);

            Piece := Vector.First.Prev;

         end if;

         -- Now Piece /= null and Index <= Piece.Last.

         -- Find the piece that contains the Index:

         while Index < Piece.First loop

            Piece := Piece.Prev;

         end loop;

         Piece.Store(Index) := To;

      end if;

      -- Update Length:

      if Index > Vector.Length then

         Vector.Length := Index;

      end if;

   end Set;


   procedure Append (
      To    : in out Unbounded_Vector;
      Value : in     Element_Type)
   is
   begin

      Set (
         Vector => To,
         Index  => Next (To),
         To     => Value);

   end Append;


   procedure Erase (Item : in out Unbounded_Vector)
   is

      Piece : Piece_Ref := Item.First.Prev;
      -- The first remaining dynamically allocated piece.

      Kill : Piece_Ref;
      -- A piece to be deallocated.

   begin

      Item.Length := 0;

      while Piece /= null loop

         Kill  := Piece;
         Piece := Piece.Prev;

         if Deallocate then

            Free (Kill);

         end if;

      end loop;

      Item.First.Prev := null;
      -- It was the first Piece that we deallocated.

   exception when others => Faults.Deallocation;

   end Erase;


   function Element (
      Vector : Unbounded_Vector;
      Index  : Positive)
   return Element_Type
   is

      Piece : Piece_Ref;
      -- The dynamically allocated piece that contains the Index.

   begin

      if Index > Vector.Length then
         -- There ain't no such animal.

         raise Constraint_Error;

      end if;

      if Index <= Vector.First.Last then
         -- The first piece is satisfactory.

         return Vector.First.Store(Index);

      else
         -- Must look for the right dynamic piece.

         Piece := Vector.First.Prev;

         while Index < Piece.First loop

            Piece := Piece.Prev;

         end loop;

         return Piece.Store(Index);

      end if;

   end Element;


   function Index (
      Source : Unbounded_Vector;
      Value  : Element_Type)
   return Positive
   is

      Piece : Piece_Ref;
      -- One of the dynamic pieces.

   begin

      -- Look in the dynamic pieces:

      Piece := Source.First.Prev;

      while Piece /= null loop

         -- Look in this piece:

         for I in Piece.First .. Natural'Min (Piece.Last, Source.Length) loop

            if Piece.Store(I) = Value then

               return I;

            end if;

         end loop;

         -- It was not in this piece.
         -- Look in earlier pieces:

         Piece := Piece.Prev;

      end loop;

      -- It was not in the dynamic pieces.
      -- Look in the first piece:

      for I in 1 .. Natural'Min (Source.First.Last, Source.Length) loop

         if Source.First.Store(I) = Value then

            return I;

         end if;

      end loop;

      -- The Value is nowhere in the Source.

      return Source.Length + 1;

   end Index;


   procedure Drop (
      Index : in     Positive;
      From  : in out Unbounded_Vector)
   is

      Piece : Piece_Ref := From.First.Prev;
      -- A dynamically allocated piece of the vector.

      Drop_Out : Element_Type;
      -- The element that is shifted out from a higher piece.

      Drop_In : Element_Type;
      -- The element that is shifted into a lower piece.

      Have_Drop_In : Boolean := False;
      -- Whether we have a Drop_In element.

      First, Last : Positive;
      -- The valid index range in the Piece.

   begin

      -- Check for valid Index:

      if Index > From.Length then
         -- Out of range.

         raise Constraint_Error;

      end if;

      -- Skip unused dynamic pieces:

      while    Piece /= null
      and then Piece.First > From.Length
      loop
         -- This piece is completely > From.Length and thus unused.

         Piece := Piece.Prev;

      end loop;

      -- Process the dynamic pieces from high indices to low:

      loop

         exit when Piece = null;
         -- All dynamic pieces processed.

         Last := Positive'Min (Piece.Last, From.Length);
         -- The last currently valid index in this Piece.

         if Index >= Piece.First then
            -- The drop is in this dynamic piece.
            -- This is the last piece that needs modification.

            Piece.Store(Index .. Last - 1) := Piece.Store(Index + 1 .. Last);

            if Have_Drop_In then

               Piece.Store(Last) := Drop_In;

            end if;

            Have_Drop_In := False;

            exit;

         else
            -- The drop is in an inner piece. This Piece is only shifted.

            First := Piece.First;

            Drop_Out := Piece.Store(First);

            Piece.Store(First .. Last - 1) := Piece.Store(First + 1 .. Last);

            if Have_Drop_In then

               Piece.Store(Last) := Drop_In;

            end if;

            Drop_In      := Drop_Out;
            Have_Drop_In := True;

            Piece := Piece.Prev;

         end if;

      end loop;

      -- Process the first piece if necessary:

      if Index <= From.First.Last then
         -- The drop is in the first piece.

         Last := Positive'Min (From.First.Last, From.Length);

         From.First.Store(Index .. Last - 1) :=
            From.First.Store(Index + 1 .. Last);

         if Have_Drop_In then

            From.First.Store(Last) := Drop_In;

         end if;

      end if;

      -- And we are done:

      From.Length := From.Length - 1;

   end Drop;


   function Is_Element (
      Source : Unbounded_Vector;
      Value  : Element_Type)
   return Boolean
   is
   begin

      return Index (Source, Value) <= Last (Source);

   end Is_Element;


   procedure Find_Or_Add (
      Value  : in     Element_Type;
      Vector : in out Unbounded_Vector;
      Index  :    out Positive)
   is
   begin

      Index := Growing_Vectors.Index (
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
      Ignore : Positive;
   begin

      Find_Or_Add (Value => Value, Vector => To, Index => Ignore);

   end Add;


   function To_Vector (Item : Unbounded_Vector) return Vector_Type
   is

      Piece : Piece_Ref := Item.First.Prev;
      -- A dynamic piece of the vector.

      Last : Natural;
      -- The last valid index in the Piece.

      Vec : Vector_Type (1 .. Item.Length);
      -- The result.

   begin

      -- Concatenate the dynamic pieces:

      while Piece /= null loop

         Last := Natural'Min (Piece.Last, Item.Length);

         if Last >= Piece.First then
            -- There is some data in this Piece.

            Vec(Piece.First .. Last) := Piece.Store(Piece.First .. Last);

         end if;

         Piece := Piece.Prev;

      end loop;

      -- And the first piece:

      Last := Natural'Min (Item.First.Last, Item.Length);

      if Last >= 1 then

         Vec(1 .. Last) := Item.First.Store(1 .. Last);

      end if;

      return Vec;

   end To_Vector;


end Growing_Vectors;
