-- Formats.In_Memory (decl)
--
-- Reading and writing octet streams from or to memory buffers.
--
-- This package defines a Stream type that takes its data from an octet
-- array in memory. The octet array would typically be filled from a program
-- executable to hold e.g. debugging information. This information can then
-- be read and parsed using the Stream approach.
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
-- $Revision: 1.6 $
-- $Date: 2015/10/24 20:53:53 $
--
-- $Log: formats-in_memory.ads,v $
-- Revision 1.6  2015/10/24 20:53:53  niklas
-- Moved to free licence.
--
-- Revision 1.5  2014/06/20 19:48:09  niklas
-- Added Load from and Dump to another stream.
--
-- Revision 1.4  2007-06-14 11:16:41  niklas
-- BT-CH-0060.
--
-- Revision 1.3  2007/04/18 18:35:49  niklas
-- BT-CH-0057.
--
-- Revision 1.2  2007/03/29 12:51:48  niklas
-- BT-CH-0056.
--
-- Revision 1.1  2004/04/24 18:01:40  niklas
-- First version.
--

--:dbpool with GNAT.Debug_Pools;

with Ada.Streams;


package Formats.In_Memory  is


   subtype Offset_T is Ada.Streams.Stream_Element_Offset;
   --
   -- A positive or negative offset (difference) between two stream
   -- positions, measured in stream elements.


   subtype Count_T  is Ada.Streams.Stream_Element_Count;
   --
   -- A non-negative number of stream elements.


   subtype Index_T is Offset_T range 1 .. Offset_T'Last;
   --
   -- The index of a stream element.


   type Stream_T (Length : Count_T) is new Ada.Streams.Root_Stream_Type
   with private;
   --
   -- A stream of data elements of type Ada.Streams.Stream_Element,
   -- stored in a memory array. The data can be read or written.
   -- There is a current stream position (index) that can be set
   -- and queried.


   type Stream_Ref is access Stream_T;
   --
   -- Refers to a heap-allocated from-memory stream.

   --:dbpool Stream_Pool : GNAT.Debug_Pools.Debug_Pool;

   --:dbpool for Stream_Ref'Storage_Pool use Stream_Pool;


   procedure Read (
      Stream : in out Stream_T;
      Item   :    out Ada.Streams.Stream_Element_Array;
      Last   :    out Ada.Streams.Stream_Element_Offset);
   --
   -- Overrides Ada.Streams.Read, which see.


   procedure Write (
      Stream : in out Stream_T;
      Item   : in     Ada.Streams.Stream_Element_Array);
   --
   -- Overrides Ada.Streams.Write, which see.


   function Index (Stream : Stream_T) return Index_T;
   --
   -- The current Stream position.


   function End_Of_Data (Stream : Stream_T) return Boolean;
   --
   -- Whether the Stream is ended, meaning that the index is placed
   -- after the last element in the Stream and no more can be read.


   procedure Set_Index (
      Stream : in out Stream_T;
      To     : in     Index_T);
   --
   -- Sets the current Stream position To the given index.


   procedure Set_Offset (
      Stream : in out Stream_T;
      To     : in     Count_T);
   --
   -- Sets the current Stream position To the given Offset relative
   -- to the start of the data. A zero Offset refers to the first
   -- element in the data.


   procedure Skip (
      Stream : in out Stream_T;
      Count  : in     Count_T);
   --
   -- Skips a given number of elements in the stream.


   procedure Load (
      From   : access Ada.Streams.Root_Stream_Type'Class;
      Length : in     Count_T;
      Giving :    out Stream_Ref);
   --
   -- Loads Length stream elements From the given stream, stores them
   -- in (heap) memory and returns in Giving a stream access to them.
   -- Initializes Index(Giving) to 1, the start of the stream.


   procedure Load (
      From   : access Ada.Streams.Root_Stream_Type'Class;
      Giving :    out Stream_T);
   --
   -- Loads Giving.Length elements From the current position of the
   -- given stream and stores them in the Giving in-memory stream.
   -- Initializes Index(Giving) to 1, the start of the stream.


   procedure Load (
      From   : in     IO.File_Type;
      Part   : in     Segment_Desc_T;
      Giving :    out Stream_Ref);
   --
   -- Loads the defined Part of the data From an open file into
   -- (heap) memory and returns in Giving a stream access to the data.
   -- Initializes Index(Giving) to 1, the start of the stream.


   function String_To_Null (
      From  : Stream_Ref;
      Index : Index_T)
   return String;
   --
   -- The data From the given stream, starting at the given
   -- Index and ending at a null element. The null element is
   -- not included in the resulting string.
   --
   -- The current index of the stream is not changed.


   procedure Dump (
      From   : in     Stream_T;
      To     : access Ada.Streams.Root_Stream_Type'Class);
   --
   -- Dumps (writes) all elements From the start of the given in-memory
   -- stream, from its start up to but not including its current position,
   -- to the given stream. If the From stream has just been written, its
   -- current position is after the last-written element, therefore the
   -- dump moves all the written data From the in-memory stream To the
   -- other stream.


   procedure Discard (Item : in out Stream_Ref);
   --
   -- Discards (deallocates, destroys) the memory array accessed by
   -- the Item, which is returned as a null reference. Later use of
   -- any earlier copy of this Item is erroneous.


private

   type Stream_T (Length : Count_T) is new Ada.Streams.Root_Stream_Type
   with record
      Data  : Ada.Streams.Stream_Element_Array (1 .. Length);
      Index : Offset_T := 1;
   end record;
   --
   -- Data
   --    The data array.
   -- Index
   --    The index of the current position in Data, 1 .. Length + 1.
   --    The element Data(Index) is the next one to be read or written.


end Formats.In_Memory;
