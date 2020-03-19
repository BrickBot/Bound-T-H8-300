-- Memories (body)
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
-- $Revision: 1.4 $
-- $Date: 2015/10/24 20:53:55 $
--
-- $Log: memories.adb,v $
-- Revision 1.4  2015/10/24 20:53:55  niklas
-- Moved to free licence.
--
-- Revision 1.3  2008-11-08 17:46:29  niklas
-- Extended procedure Allocate to allow part or all of the address range
-- to be already allocated, and to allocate storage for those parts that
-- are not yet allocated. Removed exception Allocation_Overlap as no
-- longer necessary. Added function Allocated to help unit tests.
--
-- Revision 1.2  2004/06/13 11:38:47  niklas
-- Corrected the Fetch operation to check that the right amount of
-- data is Loaded (off by one).
--
-- Revision 1.1  2004/04/24 17:18:00  niklas
-- First version.
--


package body Memories is


   function "-" (Left, Right : Address_Type) return Offset_T
   --
   -- Offset between two addresses, assuming Left >= Right.
   --
   is
   begin

      return Offset_T (Address_Type'(Left - Right));

   end "-";


   function "+" (Left : Address_Type; Right : Offset_T) return Address_Type
   --
   -- The address with a given forward offset from a given address.
   --
   is
   begin

      return Left + Address_Type (Right);

   end "+";


   procedure Initialize (Item : in out Content_T)
   is
   begin

      Initialize (Item.Data_Map);

      Initialize (Item.Load_Map);

   end Initialize;


   function Image (Item : Segment_T) return String
   is
      Addr : constant String := Address_Type'Image (Item.Address);
   begin

      return
           "[ Segment"
         & Addr & " +" & Offset_T'Image (Item.Data'First)
         & " .."
         & Addr & " +" & Offset_T'Image (Item.Data'Last)
         & " ]";

   end Image;


   function Image (Item : Load_Status_T) return String
   is
   begin
      return "Loaded";
   end Image;


   function Loaded (
      Address : Address_Type;
      Within  : Content_T)
   return Boolean
   is
   begin

      return Defined (Within.Load_Map, Address);

   end Loaded;


   function Loaded (
      Address : Address_Type;
      Span    : Offset_T;
      Within  : Content_T)
   return Boolean
   is
   begin

      return
         Fully_Defined (
            Map  => Within.Load_Map,
            Over => (First => Address, Last => Address + Span));

   end Loaded;


   procedure Allocate (
      Address : in     Address_Type;
      Span    : in     Offset_T;
      Within  : in out Content_T)
   is

      Interval : Data_Maps.Interval_T := (
         First => Address,
         Last  => Address + Span);
      -- The address interval (left) to be allocated.

      Last_Void : Address_Type;
       -- The last non-allocated address after Interval.First.

   begin

      -- The following iteration reduces Interval until it is
      -- (logically) empty by iteratively increasing Interval.First
      -- using one of two methods:
      --
      -- > If Interval.First is an allocated address, increase
      --   Iteration.First to the next larger unallocated address.
      --
      -- > If Interval.First is an unallocated address, allocate
      --   a new interval that fills the unallocated stretch
      --   from Interval.First up to the next allocated address (or
      --   Interval.Last, whichever comes first) and increase
      --   Iteration.First to the next address after this allocation.

      while Interval.First <= Interval.Last loop

         if Defined (Map => Within.Data_Map, Index => Interval.First) then
            -- The first part of the Interval is allocated already.
            -- We skip ahead to the first unallocated address.

            begin

               Interval.First := Next_Undefined (
                  After => Interval.First,
                  Map   => Within.Data_Map);
               -- Raises Undefined_Value if all addresses > Interval.First
               -- are allocated, in which case we are done.

               -- Now Interval.First is unallocated, but may
               -- be > Interval.Last, so we return to loop.

            exception

            when Data_Maps.Undefined_Value =>
               -- All addresses >= Interval.First are allocated.
               -- Nothing more needs to be done.

               exit;

            end;

         else
            -- Interval.First is an unallocated point.
            -- We allocate as much as necessary and possible,
            -- starting here.

            begin

               Last_Void := Next (
                  After => Interval.First,
                  Map   => Within.Data_Map) - 1;
               -- Raises Undefined_Value if there are no allocations
               -- after Interval.First.

               -- The range Interval.First .. Last_Void is unallocated
               -- and we shall thus allocate as much as we need of it.

               if Last_Void > Interval.Last then

                  Last_Void := Interval.Last;

               end if;

               Set (
                  Map  => Within.Data_Map,
                  From => (
                     First => Interval.First,
                     Last  => Last_Void),
                  To   => (
                     Address => Interval.First,
                     Data    =>
                        new Data_Type(0 .. Last_Void - Interval.First)));

               -- Continue to find the next unallocated bit:

               Interval.First := Last_Void + Offset_T'(1);

               -- Interval.First is now an allocated point, or it
               -- is > Interval.Last, so we return to loop again.

            exception

            when Data_Maps.Undefined_Value =>
               -- There is no allocated area after Interval.First.
               -- We can allocate the whole Interval and are then
               -- finished.

               Set (
                  Map  => Within.Data_Map,
                  From => Interval,
                  To   => (
                     Address => Interval.First,
                     Data    =>
                        new Data_Type(0 .. Interval.Last - Interval.First)));

               exit;

            end;

         end if;

      end loop;

   end Allocate;


   function Allocated (Address : Address_Type; Within : Content_T)
   return Boolean
   is
   begin

      return Defined (Within.Data_Map, Address);

   end Allocated;


   procedure Load (
      Into   : in     Address_Type;
      Datum  : in     Datum_Type;
      Within : in out Content_T)
   is
   begin

      Load (Into => Into, Data => (0 => Datum), Within => Within);

   end Load;


   procedure Load (
      Into   : in     Address_Type;
      Data   : in     Data_Type;
      Within : in out Content_T)
   is

      Offset : Offset_T := Data'First;
      -- The next loaded datum is Data(Offset).

      Next : Address_Type;
      -- The next address to be loaded.

      Segment : Segment_T;
      -- The segment that holds the next location to be loaded.

      Target : Offset_T;
      -- The index in Segment.Data of the next location to be
      -- loaded.

      Span : Offset_T;
      -- The last source for the current segment is Data(Offset + Span).

   begin

      -- Check that storage has been allocated:

      if not Fully_Defined (
         Map  => Within.Data_Map,
         Over => (Into + Data'First, Into + Data'Last))
      then
         -- Some location in the load range is not allocated.

         raise Memory_Not_Allocated;

      end if;

      while Offset <= Data'Last loop

         Next := Into + Offset;

         Segment := Value (Within.Data_Map, Next);

         Target := Next - Segment.Address;

         Span := Offset_T'Min (
            Data        'Last - Offset,
            Segment.Data'Last - Target);

         Segment.Data (Target .. Target + Span) :=
            Data (Offset .. Offset + Span);

         Offset := Offset + Span + 1;

      end loop;

      Set (
         Map  => Within.Load_Map,
         From => (
            First => Into + Data'First,
            Last  => Into + Data'Last),
         To   => Loaded);

   end Load;


   function Datum (From : Address_Type; Within : Content_T)
   return Datum_Type
   is

      Segment : Segment_T;

   begin

      if not Loaded (From, Within) then

         raise Undefined_Value;

      end if;

      Segment := Value (Within.Data_Map, From);

      return Segment.Data(From - Segment.Address);

   exception

      when Data_Maps.Undefined_Value =>

         raise Undefined_Value;

   end Datum;


   procedure Fetch (
      From   : in     Address_Type;
      Within : in     Content_T;
      Into   :    out Data_Type)
   is

      Offset : Offset_T := Into'First;
      -- The next destination is Into(Offset).

      Next : Address_Type;
      -- The next address to be fetched.

      Segment : Segment_T;
      -- The segment that holds the next data to be fetched.

      Source : Offset_T;
      -- The index in Segment.Data of the next datum to be
      -- fetched.

      Span : Offset_T;
      -- The last destination from the current segment
      -- is Into(Offset + Span).

   begin

      if not
         Loaded (
            Address => From + Into'First,
            Span    => Into'Length - Offset_T'(1),
            Within  => Within)
      then
         -- Some location in the fetch range is not loaded.

         raise Undefined_Value;

      end if;

      while Offset <= Into'Last loop

         Next := From + Offset;

         Segment := Value (Within.Data_Map, Next);

         Source := Next - Segment.Address;

         Span := Offset_T'Min (
            Into        'Last - Offset,
            Segment.Data'Last - Source);

         Into (Offset .. Offset + Span) :=
            Segment.Data (Source .. Source + Span);

         Offset := Offset + Span + 1;

      end loop;

   end Fetch;


   procedure Show_Maps (Item : in Content_T)
   is
   begin

      Show (Item.Data_Map);

      Show (Item.Load_Map);

   end Show_Maps;


end Memories;
