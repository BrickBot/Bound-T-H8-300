-- Bits (body)
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
-- $Date: 2015/10/24 20:53:51 $
--
-- $Log: bits.adb,v $
-- Revision 1.3  2015/10/24 20:53:51  niklas
-- Moved to free licence.
--
-- Revision 1.2  2005-10-24 08:43:42  niklas
-- Using package Compiler instead of package Interfaces, to follow
-- the limitations of the platform compiler.
--
-- Revision 1.1  2004/04/24 16:36:02  niklas
-- First version.
--


with Compiler;


function Bits (
   Low, High : in Natural;
   From      : in Value_T)
return Value_T
is

   type Base_T is new Compiler.Unsigned_Max;
   -- This is the largest bit-string we handle.

   Verify : constant Base_T := Base_T (Value_T'Last);
   -- Verify that Base_T can handle all values.

   Num : constant Natural := High - Low + 1;
   -- The number of bits in the field.

   Mask : constant Base_T := 2 ** Num - 1;
   -- Mask to select the lowest Num bits.

begin

   return Value_T (
      Shift_Right (Value => Base_T (From), Amount => Low)
      and Mask);

end Bits;
