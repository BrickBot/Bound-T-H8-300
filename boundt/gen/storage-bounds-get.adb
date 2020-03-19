-- Storage.Bounds.Get (body)
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
-- $Date: 2015/10/24 20:05:51 $
--
-- $Log: storage-bounds-get.adb,v $
-- Revision 1.2  2015/10/24 20:05:51  niklas
-- Moved to free licence.
--
-- Revision 1.1  2013-02-03 12:37:03  niklas
-- First version.
--


with Arithmetic;


package body Storage.Bounds.Get is


   function Interval_Value (From : String)
   return Interval_T
   is
      use Arithmetic;

      Min, Max : Limit_T;
      -- The limits of the interval, by default unlimited.

      Dot_Dot : Positive := From'Last + 1;
      -- The index in From of the first "." in the "..", if
      -- there is a "..", else this initial value.

   begin

      if From'Length = 0 then

         raise Constraint_Error;

      end if;

      -- Find the ".." if there is one:

      for D in From'First .. From'Last - 1 loop
         if From(D .. D + 1) = ".." then
            Dot_Dot := D;
            exit;
         end if;
      end loop;

      -- Take the Min value before the "..", or the single value
      -- if there is no "..":

      if Dot_Dot > From'First then
         -- We have something before the ".." (or there is no "..").

         Min := Limit (Value_T'Value (From(From'First .. Dot_Dot - 1)));

      end if;

      -- Take the Max value after the "..", or use the single (Min)
      -- value if there is no "..":

      if Dot_Dot + 2 <= From'Last then
         -- We have something after the "..".

         Max := Limit (Value_T'Value (From(Dot_Dot + 2 .. From'Last)));

      elsif Dot_Dot > From'Last then
         -- No "..", singular value = Min.

         Max := Min;

      end if;

      return (Min, Max);

   end Interval_Value;


end Storage.Bounds.Get;
