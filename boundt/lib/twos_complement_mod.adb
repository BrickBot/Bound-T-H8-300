-- Twos_Complement_Mod (body)
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
-- $Date: 2015/10/24 20:53:55 $
--
-- $Log: twos_complement_mod.adb,v $
-- Revision 1.3  2015/10/24 20:53:55  niklas
-- Moved to free licence.
--
-- Revision 1.2  2005-12-01 10:30:44  niklas
-- Changed Sign_Bit to depend only on Unsigned_Type, not on
-- Signed_Type. Now Signed_Type can be wider than the minimum.
-- Corrected function Negative.
--
-- Revision 1.1  2004/04/26 09:37:42  niklas
-- First version.
--


package body Twos_Complement_Mod is


   Sign_Bit : constant Unsigned_Type := Unsigned_Type'Last / 2 + 1;
   --
   -- An unsigned value with only the sign bit on.


   function Negative (Item : Unsigned_Type) return Boolean
   is
   begin

      return (Item and Sign_Bit) /= 0;

   end Negative;


   function Signed (Item : Unsigned_Type) return Signed_Type
   is
   begin

      if (Item and Sign_Bit) = 0 then
         -- Zero or positive value.

         return Signed_Type (Item);

      else
         -- Negative value.

         return
              Signed_Type (Item xor Sign_Bit)
            + Signed_Type'First;

      end if;

   end Signed;


   function "abs" (Item : Unsigned_Type) return Abs_Type
   is
   begin

      return abs Abs_Type'Base (Signed (Item));

   end "abs";


end Twos_Complement_Mod;
