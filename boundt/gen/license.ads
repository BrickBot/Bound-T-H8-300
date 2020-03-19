-- License (decl)
--
-- Checking the user's right to this software.
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
-- $Revision: 1.7 $
-- $Date: 2015/10/24 19:36:50 $
--
-- $Log: license.ads,v $
-- Revision 1.7  2015/10/24 19:36:50  niklas
-- Moved to free licence.
--
-- Revision 1.6  2012-01-19 19:43:29  niklas
-- BT-CH-0223: Package License does not depend on package Flow.
--
-- Revision 1.5  2011-03-24 20:06:27  niklas
-- Added End_Of_Time.
--
-- Revision 1.4  2009-12-30 10:18:25  niklas
-- BT-CH-0208: Isolate licence options within package License.
--
-- Revision 1.3  2009-12-27 22:34:31  niklas
-- BT-CH-0205: Licensing based on code size and time/space dimensions.
--
-- Revision 1.2  2004-11-16 08:55:25  niklas
-- Added Image function.
--
-- Revision 1.1  2001/12/18 19:07:19  holsti
-- First, date-based version.
--


with Ada.Calendar;


package License is


   End_Of_Time : constant Ada.Calendar.Time :=
     Ada.Calendar.Time_Of (
        Year  => 2099,
        Month => 12,
        Day   => 31);
   --
   -- A date that indicates an eternally valid licence.


   type Size_Measure_T is new Natural;
   --
   -- A measure of the size of the code under analysis,
   -- for sze-limited licences.


   function Valid_Date (Measure : Size_Measure_T) return Boolean;
   --
   -- Whether the user is still licensed to run this software,
   -- on the current date. If the date has passed, analysis with
   -- a small program size-Measure may still be allowed.


   function Valid_Size (Measure : Size_Measure_T) return Boolean;
   --
   -- Whether the user is licensed to analyse program part of
   -- this current Measure of size.


   function Allows_Time_Analysis return Boolean;
   --
   -- Whether the licence allows time (WCET) analysis.


   function Allows_Stack_Analysis return Boolean;
   --
   -- Whether the licence allows stack-usage analysis.


   function Image return String;
   --
   -- A user-readable description of the current license.


   procedure Set_Size_Limit (
      To    : in     String;
      Valid :    out Boolean);
   --
   -- Changes the size limit To some value.
   -- This change may be Valid (allowed by the licence)
   -- or not.


end License;
