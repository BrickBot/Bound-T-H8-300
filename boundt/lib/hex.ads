-- Hex (decl)
--
-- Hexadecimal string images of unsigned numbers.
-- Author: Mikko Ala-Fossi, Space Systems Finland, 1999
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
-- $Revision: 1.5 $
-- $Date: 2015/10/24 20:53:54 $
--
-- $Log: hex.ads,v $
-- Revision 1.5  2015/10/24 20:53:54  niklas
-- Moved to free licence.
--
-- Revision 1.4  2007-03-11 13:23:27  niklas
-- Added flexible-width Image for Word32_T.
--
-- Revision 1.3  2005/10/24 08:50:29  niklas
-- Using Compiler.Unsigned_64 instead of Interfaces.Unsigned_64,
-- to follow the limitations of the platform compiler.
--
-- Revision 1.2  2004/04/24 17:15:27  niklas
-- First Tidorum version. Types from Interfaces, add 64-bit.
--
-- Revision 1.1  2000/12/03 10:41:01  holsti
-- Moved from gen to lib.
--
-- Revision 1.2  2000/07/02 08:13:27  holsti
-- Renamed Nibble to Nybble.
--
-- Revision 1.1  2000/04/23 21:41:16  holsti
-- Added Flow package.
--


with Interfaces;

with Compiler;


package Hex is


   type Nybble_T is mod 2**4;
   type Byte_T   is new Interfaces.Unsigned_8;
   type Word16_T is new Interfaces.Unsigned_16;
   type Word24_T is mod 2**24;
   type Word32_T is new Interfaces.Unsigned_32;
   type Word64_T is new Compiler.Unsigned_64;


   function Image (Value : Nybble_T) return String;
   function Image (Value : Byte_T  ) return String;
   function Image (Value : Word16_T) return String;
   function Image (Value : Word24_T) return String;
   function Image (Value : Word32_T) return String;
   function Image (Value : Word64_T) return String;
   --
   -- Each of the Image functions returns a string of hexadecimal
   -- digits (0 .. 9, A .. F), of a length appropriate to the
   -- type of the value. For example, Nybble_T produces one
   -- hexadecimal digit, and Word32_T produces 8 digits.


   function Image (Value : Word32_T; Octets : in Positive)
   return String;
   --
   -- The Value in hexadecimal form, padding with leading zeros to
   -- give at least 2*Octets hex digits, which corresponds to a natural
   -- size of Octets octets for the Value.
   --
   -- The single-argument Image (Value) corresponds to Image (Value, 8).


   function Image (Value : Word64_T; Octets : in Positive)
   return String;
   --
   -- The Value in hexadecimal form, padding with leading zeros to
   -- give at least 2*Octets hex digits, which corresponds to a natural
   -- size of Octets octets for the Value.
   --
   -- The single-argument Image (Value) corresponds to Image (Value, 8).


end Hex;
