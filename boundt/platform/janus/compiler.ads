-- Compiler (decl)
--
-- Compiler-specific definitions for the host platform compiler that
-- we use to compile Bound-T.
--
-- This version is for the Janus/Ada 95 compiler.
--
-- This package exists mainly to hide the size of the predefined
-- Integer type and the maximum size of the signed and unsigned modular
-- types defined in Interfaces.
--
-- Types defined in this package correspond to the predefined types and
-- we therefore omit the "_T" suffix that we use for other types.
--
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
-- $Revision: 1.3 $
-- $Date: 2015/10/25 07:21:59 $
--
-- $Log: compiler.ads,v $
-- Revision 1.3  2015/10/25 07:21:59  niklas
-- Moved to free licence.
--
-- Revision 1.2  2005-10-24 17:27:47  niklas
-- Added Integer_64 (which is 32 bits).
--
-- Revision 1.1  2005/10/24 08:37:06  niklas
-- First version.
--


with Interfaces;


package Compiler is


   type Native_Integer is new Long_Integer;
   --
   -- The longest native integer type that works efficiently on the
   -- platform and compiler in question. To make Bound-T reasonably
   -- useful, this should be more than 16 bits and hopefully is at
   -- least 32 bits.


   subtype Native_Natural is Native_Integer
      range 0 .. Native_Integer'Last;
   --
   -- The non-negative range of native integers.


   subtype Native_Positive is Native_Integer
      range 1 .. Native_Integer'Last;
   --
   -- The positive range of native integers.


   type Integer_64 is new Interfaces.Integer_32;
   --
   -- A 64-bit (signed) integer, if the compiler supports such a type,
   -- otherwise some shorter integer.


   type Unsigned_64 is new Interfaces.Unsigned_32;
   --
   -- A 64-bit modular (unsigned) integer, if the compiler supports
   -- such a type, otherwise some shorter modular integer.


   type Unsigned_Max is new Interfaces.Unsigned_32;
   --
   -- The longest available modular (unsigned) integer type defined
   -- in package Interfaces and thus provided with shift and rotate
   -- functions.


end Compiler;
