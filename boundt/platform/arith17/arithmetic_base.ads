-- Arithmetic_Base (decl)
--
-- The type of arithmetic values for use in Bound-T as the "generic"
-- type of numbers modelling the target program's computation.
-- This version models 16-bit computations by using an integer type
-- with at least 17 bits, thus able to model both the signed and
-- unsigned views of any 16-bit number.
---
-- Author: Niklas Holsti, Tidorum Ltd
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
-- $Date: 2015/10/25 07:13:13 $
--
-- $Log: arithmetic_base.ads,v $
-- Revision 1.2  2015/10/25 07:13:13  niklas
-- Moved to free licence.
--
-- Revision 1.1  2009-11-27 11:42:51  niklas
-- First version.
--


with Interfaces;


package Arithmetic_Base is


   --
   ---  Integer (signed) values
   --


   Value_Width : constant := 16;
   --
   -- The number of bits in the basic numbers we model.
   -- We will use (at least) one more bit to allow for signed
   -- views of unsigned values.


   Min_Value : constant := - 2 ** Value_Width;
   Max_Value : constant :=   2 ** Value_Width - 1;
   --
   -- The range of (Width+1)-bit numbers,


   type Basic_Value_T is range Min_Value .. Max_Value;
   --
   -- The basic range of arithmetic values, comprising both the
   -- signed and the unsigned views.


   type Value_T is new Basic_Value_T'Base;
   --
   -- Defines the arithmetic values of the processor, or a superset
   -- of these values. Covers both signed and unsigned views.


   function Image (Item : Value_T) return String;
   --
   -- The number as a decimal digit string with no spaces whether
   -- leading, trailing or embedded.


   --
   ---  Binary (unsigned) bit-string values
   --


   Max_Width : constant := 16;
   --
   -- The maximum bit-width supported, that is, the maximum number
   -- of bits in any expressible unsigned value.


   type Width_T is range 0 .. Max_Width;
   --
   -- The number of bits in (the value of) an expression.


   type Word_T is new Interfaces.Unsigned_32;
   --
   -- Unsigned base-2 integers for computing bit-wise operations.
   --
   -- We would like to declare this as "mod 2 ** Max_Width but
   -- then it would not have the shift and rotate operations.
   --
   -- NOTE! We user Unsigned_32, not Unsigned_16, to work around a
   -- bug in GNAT 4.3.2 which otherwise causes a bug-box while
   -- compiling gen/arithmetic-evaluation.adb.
   -- TBM when this GNAT bug (gnat_001 in our numbering) is fixed.


   function Image (Item : Word_T) return String;
   --
   -- The number as a decimal digit string with no spaces whether
   -- leading, trailing or embedded.


end Arithmetic_Base;
