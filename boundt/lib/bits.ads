-- Bits (decl)
--
-- Extraction of a bit-field from a bit string.
--
-- Author: Niklas Holsti
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
-- $Date: 2015/10/24 20:53:51 $
--
-- $Log: bits.ads,v $
-- Revision 1.2  2015/10/24 20:53:51  niklas
-- Moved to free licence.
--
-- Revision 1.1  2004-04-24 16:36:03  niklas
-- First version.
--


generic

   type Value_T is mod <>;

function Bits (
   Low, High : in Natural;
   From      : in Value_T)
return Value_T;
--
-- Extracts and returns a contiguous bit-field from a given bit string.
-- The bits are numbered Low .. High, with Low <= High, in increasing order
-- of significance, thus zero means the least significant bit in From.
--
-- Value_T should be a binary (base-2) modular type. The maximum value that
-- can be processed depends on the implementation.
