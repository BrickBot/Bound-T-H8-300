-- Formats.In_Memory (body)
--
-- Author: Niklas Holsti.
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
-- $Date: 2015/10/24 20:53:53 $
--
-- $Log: formats-in_memory.adb,v $
-- Revision 1.5  2015/10/24 20:53:53  niklas
-- Moved to free licence.
--
-- Revision 1.4  2014/06/20 19:48:19  niklas
-- Added Load from and Dump to another stream.
--
-- Revision 1.3  2007-06-14 11:16:41  niklas
-- BT-CH-0060.
--
-- Revision 1.2  2007/01/25 21:25:36  niklas
-- BT-CH-0043.
--
-- Revision 1.1  2004/04/24 18:01:40  niklas
-- First version.
--


with Ada.Unchecked_Deallocation;
with Formats.In_Memory.Opt;
with Formats.Output;


package body Formats.In_Memory  is


   use type Offset_T;
   use type Count_T ;


   procedure Read (
      Stream : in out Stream_T;
      Item   :    out Ada.Streams.Stream_Element_Array;
      Last   :    out Ada.Streams.Stream_Element_Offset)
   is

      Amount : constant Count_T :=
        Count_T'Min (Stream.Length - Stream.Index + 1, Item'Length);
      -- The amount of data to be read.

      New_Index : constant Index_T := Stream.Index + Amount;
      -- The updated Index.

   begin

      Last := Item'First + Amount - 1;

      Item(Item'First .. Last) := Stream.Data(Stream.Index .. New_Index - 1);

      Stream.Index := New_Index;

   end Read;


   procedure Write (
      Stream : in out Stream_T;
      Item   : in     Ada.Streams.Stream_Element_Array)
   is

      New_Index : constant Index_T := Stream.Index + Item'Length;
      -- The updated Index.

   begin

      Stream.Data(Stream.Index .. New_Index - 1) := Item;

      Stream.Index := New_Index;

   end Write;


   function Index (Stream : Stream_T) return Index_T
   is
   begin

      return Stream.Index;

   end Index;


   function End_Of_Data (Stream : Stream_T) return Boolean
   is
   begin

      return Stream.Index > Stream.Length;

   end End_Of_Data;


   procedure Set_Index (
      Stream : in out Stream_T;
      To     : in     Index_T)
   is
   begin

      if To in Stream.Data'First .. Stream.Data'Last + 1 then

         Stream.Index := To;

      else

         Output.Fault (
            Location => "Formats.From_Memory.Set_Index",
            Text =>
                 "New index"
               & Index_T'Image (To)
               & " not in Stream.Data range"
               & Index_T'Image (Stream.Data'First)
               & " .."
               & Index_T'Image (Stream.Data'Last));

      end if;

   end Set_Index;


   procedure Set_Offset (
      Stream : in out Stream_T;
      To     : in     Count_T)
   is
   begin

      Set_Index (Stream => Stream, To => Index_T'First + To);

   end Set_Offset;


   procedure Skip (
      Stream : in out Stream_T;
      Count  : in     Count_T)
   is
   begin

      Stream.Index := Stream.Index + Count;

   end Skip;


   procedure Load (
      From   : access Ada.Streams.Root_Stream_Type'Class;
      Length : in     Count_T;
      Giving :    out Stream_Ref)
   is

      Last : Index_T;
      -- The last filled index of Giving.Data.

   begin

      Giving := new Stream_T (Length => Length);

      Ada.Streams.Read (
         Stream => From.all,
         Item   => Giving.Data,
         Last   => Last);

      if Last /= Giving.Data'Last then

         Output.Fault (
            Location => "Formats.From_Memory.Load",
            Text =>
                 "Last loaded index"
               & Index_T'Image (Last)
               & " /= Data'Last ="
               & Index_T'Image (Giving.Data'Last));

      end if;

      Giving.Index := 1;

   end Load;


   procedure Load (
      From   : access Ada.Streams.Root_Stream_Type'Class;
      Giving :    out Stream_T)
   is
   begin

      Ada.Streams.Stream_Element_Array'Read (From, Giving.Data);

      Giving.Index := 1;

   end Load;


   procedure Load (
      From   : in     IO.File_Type;
      Part   : in     Segment_Desc_T;
      Giving :    out Stream_Ref)
   is

      Stream : IO.Stream_Access := IO.Stream (From);
      -- The file as a stream.

   begin

      IO.Set_Index (
         File => From,
         To   => Part.Start);

      Load (
         From   => Stream,
         Length => Count_T (Part.Octets),
         Giving => Giving);

   end Load;


   function To_String (Item : Ada.Streams.Stream_Element_Array)
   return String
   is

      Result : String (1 .. Item'Length);
      -- The result.

      R : Positive := Result'First;
      -- The place in the Result for the next character/element.

   begin

      for I in Item'Range loop

         Result(R) := Character'Val (Item(I));

         R := R + 1;

      end loop;

      return Result;

   end To_String;


   function String_To_Null (
      From  : Stream_Ref;
      Index : Index_T)
   return String
   is
      use type Ada.Streams.Stream_Element;

      Point : Index_T := Index;
      -- The scanning point.

   begin

      loop

         exit when Point > From.Data'Last;
         -- No terminating null found.

         exit when From.Data(Point) = 0;
         -- The null terminator.

         Point := Point + 1;

      end loop;

      if Point > From.Data'Last then

         Output.Fault (
            Location => "Formats.In_Memory.String_To_Null",
            Text     =>
                 "No null after index"
               & Index_T'Image (Index));

      end if;

      return To_String (From.Data(Index .. Point - 1));

   end String_To_Null;


   procedure Dump (
      From   : in     Stream_T;
      To     : access Ada.Streams.Root_Stream_Type'Class)
   is
   begin

      Ada.Streams.Stream_Element_Array'Write (
         To,
         From.Data(1 .. From.Index - 1));

   end Dump;


   procedure Unchecked_Discard
   is new Ada.Unchecked_Deallocation (
      Name   => Stream_Ref,
      Object => Stream_T);


   procedure Discard (Item : in out Stream_Ref)
   is
   begin

      if Opt.Deallocate then

         Unchecked_Discard (Item);

      else

         Item := null;

      end if;

   end Discard;


end Formats.In_Memory;
