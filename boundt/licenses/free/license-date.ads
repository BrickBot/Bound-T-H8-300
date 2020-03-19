-- License.Date (decl)
--
-- Warning and expiry dates for the user license.
-- This version defines an eternal license (well, as
-- far in the future as Ada.Calendar allows).
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
-- $Date: 2015/10/24 18:42:39 $
--
-- $Log: license-date.ads,v $
-- Revision 1.2  2015/10/24 18:42:39  niklas
-- Moved to free licence.
--
-- Revision 1.1  2015/10/24 18:14:54  niklas
-- First version.
--
-- Revision 1.1  2006-04-20 06:41:14  niklas
-- First version.
--


with Ada.Calendar;


package License.Date is

   use Ada.Calendar;


   End_Of_Time : constant Time := Time_Of (
      Year  => Year_Number'Last,
      Month => Month_Number'Last,
      Day   => Day_Number'Last);
   --
   -- The end of all time, far, far in the future.


   Warning : constant Time := End_Of_Time;
   --
   -- Expiry warnings will be issued starting on this date.
   

   Expiry : constant Time := End_Of_Time;
   --
   -- The license expires at start of this day (midnight).


end License.Date;
