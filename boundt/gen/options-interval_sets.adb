-- Options.Interval_Sets (body)
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
-- $Date: 2015/10/24 20:05:50 $
--
-- $Log: options-interval_sets.adb,v $
-- Revision 1.3  2015/10/24 20:05:50  niklas
-- Moved to free licence.
--
-- Revision 1.2  2014/06/01 10:35:35  niklas
-- Added the query function Is_Empty.
--
-- Revision 1.1  2013-02-03 12:32:36  niklas
-- First version.
--


with Storage.Bounds.Get;


package body Options.Interval_Sets is


   procedure Add (
      Item : in     Storage.Bounds.Interval_T;
      To   : in out Interval_Set_T)
   is
      use Storage.Bounds;
      use type Storage.Value_T;

      Before : Natural := 0;
      -- The index of the last interval in the set that lies entirely
      -- before the new interval, with some gap in between, or zero
      -- if there is no such interval in the set.

      After : Positive := Length (To) + 1;
      -- The index of the first interval in the set that lies entirely
      -- after the new interval, or Length (To) + 1 if there is no
      -- such interval in the set.

      Merged : Storage.Bounds.Interval_T := Item;
      -- The new interval (Item) merged with any intersecting
      -- or contiguous intervals already in the set.

      Int : Storage.Bounds.Interval_T;
      -- An interval from the set.

   begin

      if not Void (Item) then

         -- Find Before and After:

         for T in First (To) .. Last (To) loop

            Int := Element (To, T);

            if       (Known (Int.Max) and Known (Item.Min))
            and then (Value (Int.Max)  <  Value (Item.Min) - 1)
            then
               -- This Int lies entirely before the new Item, with
               -- some gap in between.

               Before := T;

            end if;

            if       (Known (Int.Min) and Known (Item.Max))
            and then (Value (Int.Min)  >  Value (Item.Max) + 1)
            then
               -- This Int lies entirely after the new Item, with
               -- some gap in between.

               After := T;

               exit;
               -- No point in looking further -- all the later
               -- intervals will also lie after Item, since the
               -- intervals are listed in increasing order.

            end if;

         end loop;

         -- The intervals between Before and After (exclusive), if
         -- any, either intersect the new interval or are contiguous
         -- with it. These intervals shall be included in the Merged
         -- interval and then replaced with the Merged interval:

         for T in Before + 1 .. After - 1 loop

            Merged := Merged or Element (To, T);

         end loop;

         -- The following is a bit wasteful in that it drops all
         -- the old intervals and then inserts the new one. It would
         -- be a bit faster to drop all but one of the old intervals
         -- (when there are some old intervals) and then replace the
         -- remaining old interval with the new one.

         Drop_Slice (
            First_Drop => Before + 1,
            Last_Drop  => After  - 1,
            From       => To);

         Insert (
            Vector => To,
            Index  => Before + 1,
            Value  => Merged);

      end if;

   end Add;


   function Is_Empty (Set : Interval_Set_T) return Boolean
   is
   begin

      return Length (Set) > 0;

   end Is_Empty;


   function Max (
      Left  : Storage.Value_T;
      Right : Interval_Set_T)
   return Storage.Value_T
   is
   begin

      if Length (Right) = 0 then
         -- There are no intervals in the Right set.

         return Left;

      else

         return Storage.Value_T'Max (
            Left,
            Storage.Bounds.Value (Element (Right, Last (Right)).Max));

      end if;

   end Max;


   overriding
   function Type_And_Default (Option : access Option_T)
   return String
   is
   begin

      return "Set of integer intervals, empty by default";

   end Type_And_Default;


   overriding
   procedure Reset (Option : access Option_T)
   is
   begin

      Truncate_Length (Vector => Option.Value, To => 0);

   end Reset;


   overriding
   procedure Set (
      Option : access Option_T;
      Value  : in     String)
   is
   begin

      Add (
         Item => Storage.Bounds.Get.Interval_Value (From => Value),
         To   => Option.Value);

   end Set;


   function To_List (Option : Option_T)
   return Storage.Bounds.Interval_List_T
   is
   begin

      return To_Vector (Option.Value);

   end To_List;


end Options.Interval_Sets;
