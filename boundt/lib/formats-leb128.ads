-- Formats.LEB128 (decl)
--
-- Numbers represented in base-128, variable-length form.
--
-- For space savings, many numbers in DWARF are represented in a form
-- called LEB128 for little-endian, base-128.
--
-- This representation can be used to represent either signed or
-- unsigned numbers; the consumer must known which type of number
-- is stored.
--
-- The stored number is represented by a sequence of octets in
-- increasing order of significance (little-endian order). In each
-- octet, the low 7 bits are data and the highest bit is a marker for
-- the last octet.
--
-- The represented number consists of the 7 data bits from each octet
-- concatenated in little-endian order. For the signed form, the
-- resulting bit sequence is interpreted as a two's complement number.
--
-- The highest bit in each octet is 1 in all except the last octet where
-- it is zero. This means that a single octet can represent unsigned
-- numbers between 0 and 127 and signed numbers between -64 and +63.
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
-- $Revision: 1.3 $
-- $Date: 2015/10/24 20:53:53 $
--
-- $Log: formats-leb128.ads,v $
-- Revision 1.3  2015/10/24 20:53:53  niklas
-- Moved to free licence.
--
-- Revision 1.2  2007-02-27 22:10:36  Niklas
-- Corrected 'Read procedures for scalar types to use the 'Base
-- subtype of the scalar type and not to use Read operations inherited
-- from parent types (and so having "intrinsic" convention).
--
-- Revision 1.1  2004/04/24 17:28:17  niklas
-- First version.
--


package Formats.LEB128 is


   --
   --    Generic Read operations for signed and unsigned numbers:
   --


   generic

      type Number_Type is range <>;

   procedure Read_Signed (
      Stream : access Ada.Streams.Root_Stream_Type'Class;
      Item   : out    Number_Type'Base);
   --
   -- Reads a signed number in LEB128 form.


   generic

      type Number_Type is mod <>;

   procedure Read_Unsigned(
      Stream : access Ada.Streams.Root_Stream_Type'Class;
      Item   : out    Number_Type'Base);
   --
   -- Reads an unsigned (modular) number in LEB128 form.


   --
   --    The longest numbers supported in this implementation:
   --


   type Longest_Signed_T is new Signed_64_T;
   --
   -- The longest signed type supported in this LEB128 implementation.


   type Longest_Unsigned_T is new Unsigned_64_T;
   --
   -- The longest unsigned type supported in this LEB128 implementation.


   procedure Read_Longest_Signed (
      Stream : access Ada.Streams.Root_Stream_Type'Class;
      Item   : out    Longest_Signed_T'Base);
   --
   -- Reads a longest-possible signed number in LEB128 form.
   --
   for Longest_Signed_T'Read use Read_Longest_Signed;


   procedure Read_Longest_Unsigned (
      Stream : access Ada.Streams.Root_Stream_Type'Class;
      Item   : out    Longest_Unsigned_T'Base);
   --
   -- Reads a longest-possible unsigned number in LEB128 form.
   --
   for Longest_Unsigned_T'Read use Read_Longest_Unsigned;


end Formats.LEB128;
