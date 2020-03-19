-- License (body)
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
-- $Revision: 1.9 $
-- $Date: 2015/10/24 20:05:49 $
--
-- $Log: license.adb,v $
-- Revision 1.9  2015/10/24 20:05:49  niklas
-- Moved to free licence.
--
-- Revision 1.8  2012-01-19 19:43:29  niklas
-- BT-CH-0223: Package License does not depend on package Flow.
--
-- Revision 1.7  2011-03-24 20:08:07  niklas
-- Changed the meaning of the "size" limit so that it applies only after
-- the date limit has expired. If the date limit is in effect, the size
-- is always unlimited. This also corrects an error in which the Zero
-- size limit disallowed all analysis at all times.
--
-- Revision 1.6  2010-01-18 14:52:13  niklas
-- Corrected Valid_Date to set Valid when Now <= Expired.
--
-- Revision 1.5  2009-12-30 10:18:25  niklas
-- BT-CH-0208: Isolate licence options within package License.
--
-- Revision 1.4  2009-12-27 22:34:31  niklas
-- BT-CH-0205: Licensing based on code size and time/space dimensions.
--
-- Revision 1.3  2004-11-16 08:55:25  niklas
-- Added Image function.
--
-- Revision 1.2  2004/04/25 13:49:32  niklas
-- First Tidorum version. Refer to Tidorum instead of SSF.
--
-- Revision 1.1  2001/12/18 19:07:19  holsti
-- First, date-based version.
--


with License.Date;
with License.Dimension;
with License.Size;

with Ada.Calendar;
with Ada.Text_IO;
with Ada.Strings.Fixed;


package body License is

   use Ada.Calendar;


   Origin : constant Time := Time_Of (Year => 2003, Month => 12, Day => 23);
   --
   -- There is no "now" before this.


   function Image (Item : Time) return String
   is
      use Ada.Strings, Ada.Strings.Fixed;
   begin
      return
         Trim (Year_Number'Image(Year(Item)), Left)
       & '-'
       &  Trim (Month_Number'Image(Month(Item)), Left)
       & '-'
       & Trim (Day_Number'Image(Day(Item)), Left);
   end Image;


   type Size_Limit_T is (Zero, Finite, Infinite);
   --
   -- The possible kinds of size limit.


   function Size_Limit return Size_Limit_T
   --
   -- The kind of size limit that is in effect.
   --
   is
   begin

      if Size.Max_Measure = 0 then

         return Zero;

      elsif Size.Max_Measure < Size_Measure_T'Last then

         return Finite;

      else

         return Infinite;

      end if;

   end Size_Limit;



   function Present return Time
   --
   -- The current time, but not before Origin.
   --
   is

      Now : Time := Clock;
      -- The current time.

   begin

      if Now < Origin then
         -- Naughty, someone has set the clock back...

         return Origin;

         -- The point of not accepting any "now" before Origin is
         -- that if Date.Expiry < Origin then no fiddling with the
         -- system date can remove a size limit.

      else

         return Now;

      end if;

   end Present;


   function Valid_Date (Measure : Size_Measure_T) return Boolean
   is
      use Ada.Text_IO;

      Now : constant Time := Present;
      -- The present time.

      Valid : Boolean;
      -- Approved or not.

   begin

      if Now > Date.Expiry then

         case Size_Limit is

         when Zero | Infinite =>
            -- No use after the expiry date.

            Put_Line (
               "Sorry, your licence expired on "
             & Image(Date.Expiry) & '.');

            Put_Line (
               "Contact Tidorum Ltd to continue using the program.");

            Valid := False;

         when Finite =>
            -- Size-limited use after the expiry date.

            Put_Line (
                 "Licence limited to at most"
               & Size_Measure_T'Image (Size.Max_Measure)
               & " instructions (steps) per analysis.");

            Valid := Valid_Size (Measure);

         end case;

      else
         -- Now <= Date.Expiry: licence valid.

         Valid := True;

         if Now >= Date.Warning then

            case Size_Limit is

            when Zero | Infinite =>
               -- Dire warning.

               Put_Line (
                   "Your licence ends on "
                  & Image(Date.Expiry) & '.');

               Put_Line (
                  "Contact Tidorum Ltd to continue using the program.");

            when Finite =>
               -- Less dire.

               Put_Line (
                    "After "
                  & Image(Date.Expiry)
                  & " your licence will allow at most"
                  & Size_Measure_T'Image (Size.Max_Measure)
                  & " instructions (steps) per analysis.");

            end case;

         end if;

      end if;

      return Valid;

   end Valid_Date;


   function Valid_Size (Measure : Size_Measure_T) return Boolean
   is
   begin

      if Present <= Date.Expiry then
         -- Valid date, so no size limit.

         return True;

      else
         -- Expired date, so size limit is effective.

         return Measure <= Size.Max_Measure;

      end if;

   end Valid_Size;


   function Allows_Time_Analysis return Boolean
   is
   begin

      return License.Dimension.Time;

   end Allows_Time_Analysis;


   function Allows_Stack_Analysis return Boolean
   is
   begin

      return License.Dimension.Space;

   end Allows_Stack_Analysis;


   function Image return String
   is
   begin

      case Size_Limit is

      when Zero | Infinite =>

         return
              "Licensed for "
            & Dimension.Image
            & " analysis until "
            & Image (Date.Expiry)
            & '.';

      when Finite =>

         if Date.Expiry < Origin then
            -- Only Dimension and Size matter.

            return
                 "Licensed for "
               & Dimension.Image
               & " analysis with at most"
               & Size_Measure_T'Image (Size.Max_Measure)
               & " instructions (steps) per analysis.";

         else
            -- Date, Dimension, and Size may all matter.

            return
                 "Licensed for "
               & Dimension.Image
               & " analysis. After "
               & Image (Date.Expiry)
               & " licence allows at most"
               & Size_Measure_T'Image (Size.Max_Measure)
               & " instructions (steps) per analysis.";

         end if;

      end case;

   end Image;


   procedure Set_Size_Limit (
      To    : in     String;
      Valid :    out Boolean)
   is

      New_Value : Size_Measure_T;
      -- The new value for Max_Measure.

   begin

      New_Value := Size_Measure_T'Value (To);

      if New_Value <= License.Size.Max_Measure then

         License.Size.Max_Measure := New_Value;

         Valid := True;

      else

         Valid := False;

      end if;

   exception

   when Constraint_Error =>

      Valid := False;

   end Set_Size_Limit;


end License;
