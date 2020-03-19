-- Twos_Complement_Mod (decl)
--
-- Two's complement conversions for a modular type.
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
-- $Date: 2015/10/24 20:53:55 $
--
-- $Log: twos_complement_mod.ads,v $
-- Revision 1.2  2015/10/24 20:53:55  niklas
-- Moved to free licence.
--
-- Revision 1.1  2004-04-26 09:37:42  niklas
-- First version.
--


generic

   type Unsigned_Type is mod <>;
   --
   -- This type should be mod 2**N for some positive N.

   type Signed_Type is range <>;
   --
   -- This type should be range -2**(N-1) .. 2**(N-1) - 1.

   type Abs_Type is range <>;
   --
   -- This type should include range 0 .. 2**(N-1).
   -- Note that it must one at least one bit wider than
   -- Signed_Type, to include -(Signed_Type'First).


package Twos_Complement_Mod is


   function Negative (Item : Unsigned_Type) return Boolean;
   --
   -- Whether the Item represents a negative value (after
   -- conversion from two's complement).


   function Signed (Item : Unsigned_Type) return Signed_Type;
   --
   -- The signed value represented by Item.


   function "abs" (Item : Unsigned_Type) return Abs_Type;
   --
   -- The absolute value represented by Item.


end Twos_Complement_Mod;
