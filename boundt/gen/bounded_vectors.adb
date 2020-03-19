-- Bounded_Vectors (body)
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
-- $Date: 2015/10/24 20:05:46 $
--
-- $Log: bounded_vectors.adb,v $
-- Revision 1.3  2015/10/24 20:05:46  niklas
-- Moved to free licence.
--
-- Revision 1.2  2008-02-18 12:47:03  niklas
-- Added Drop_First and Add, for Programs.Call_Sets.
--
-- Revision 1.1  2000/06/28 08:36:33  holsti
-- First version.
--



package body Bounded_Vectors is



   function "+" (Left : Index_Type; Offset : in Integer)
   return Index_Type
   is
   begin
      return Index_Type'Val(Index_Type'Pos(Left) + Offset);
   end "+";


   function "-" (Left, Right : Index_Type) return Integer
   is
   begin
      return Index_Type'Pos(Left) - Index_Type'Pos(Right);
   end "-";


   function Length (V : Bounded_Vector) return Natural
   is
   begin
      return V.Length;
   end Length;


   function First (V : Bounded_Vector) return Index_Type
   is
   begin
      return V.First;
   end First;


   function Last (V : Bounded_Vector) return Index_Type
   is
   begin
      return
         Index_Type'Pred (First(V) + V.Length);
   end Last;


   function Next (V : Bounded_Vector) return Index_Type
   is
   begin
       if V.Length = 0 then
          return V.First;
       else
          return Index_Type'succ (Last(V));
       end if;
   end Next;


   procedure Set (
      Vector : in out Bounded_Vector;
      Index  : in     Index_Type;
      To     : in     Element_Type)
   is
      Offset : constant Natural := Index - First(Vector);
   begin

      Vector.Store(Index) := To;

      if Offset >= Vector.Length then
         Vector.Length := Offset + 1;
      end if;

   end Set;


   procedure Append (
      To    : in out Bounded_Vector;
      Value : in     Element_Type)
   is
   begin
      Set (
         Vector => To,
         Index  => Next(To),
         To     => Value);
   end Append;


   procedure Erase (Item : in out Bounded_Vector)
   is
   begin
      Item.Length := 0;
   end Erase;


   function Element (
      Vector : Bounded_Vector;
      Index  : Index_Type)
   return Element_Type
   is
   begin
      return Vector.Store(Index);
   end Element;


   function Index (
      Source : Bounded_Vector;
      Value  : Element_Type)
   return Index_Type
   is
   begin
      if Source.Length = 0 then
         return Source.First;
      else
         for I in First(Source) .. Last(Source) loop
            if Source.Store(I) = Value then
               return I;
            end if;
         end loop;
         return Index_Type'Succ (Last(Source));
      end if;
   end Index;
      

   procedure Drop_First (
      Value : in     Element_Type;
      From  : in out Bounded_Vector)
   is

      Here : Index_Type;
      -- The index of the first element of this Value.

      Lazt : Index_Type;
      -- The index of the last element.

   begin

      if From.Length > 0 then
         -- We may find the Value.

         Here := Index (From, Value);

         Lazt := Last (From);

         if Here <= Lazt then
            -- An element of this Value found, at index Here.

            -- Shift the remaining elements:

            for K in Here .. Index_Type'Pred (Lazt) loop

               From.Store(K) := From.Store(Index_Type'Succ (K));

            end loop;

            -- And then there is one less:

            From.Length := From.Length - 1;

         end if;

      end if;

   end Drop_First;


   function Is_Element (
      Source : Bounded_Vector;
      Value  : Element_Type)
   return Boolean
   is
   begin
      if Source.Length = 0 then
         return False;
      else
         for I in First(Source) .. Last(Source) loop
            if Source.Store(I) = Value then
               return True;
               end if;
         end loop;
         return False;
      end if;
   end Is_Element;

 
   procedure Add (
      Value : in     Element_Type;
      To    : in out Bounded_Vector)
   is
   begin

      if not Is_Element (Source => To, Value => Value) then

         Append (To => To, Value => Value);

      end if;

   end Add;


   function To_Vector (V : Bounded_Vector) return Vector_Type
   is
   begin
      if V.Length = 0 then
         return V.Store (Index_Type'Succ(V.First) .. V.First);
      else
         return V.Store (First(V) .. Last(V));
      end if;
   end To_Vector;


end Bounded_Vectors;

