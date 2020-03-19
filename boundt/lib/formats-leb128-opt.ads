-- Formats.LEB128.Opt (decl)
--
-- Options for numbers represented in base-128, variable-length form.
--
-- Some sources of LEB128 encodings may have peculiarities that must be
-- taken into account in the decoding. Such options, and perhaps others,
-- are defined here.
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
-- $Revision: 1.2 $
-- $Date: 2015/10/24 20:53:53 $
--
-- $Log: formats-leb128-opt.ads,v $
-- Revision 1.2  2015/10/24 20:53:53  niklas
-- Moved to free licence.
--
-- Revision 1.1  2005-03-03 17:50:10  niklas
-- First version.
--


package Formats.LEB128.Opt is


   Max_Value_Bits : Positive := Positive'Last;
   --
   -- The maximum number of bits in the original value that are
   -- encoded in the LEB128 form.
   --
   -- This is relevant only for SLEB128 encodings of negative numbers.
   -- Some encoders (eg. IAR XLINK) do not fill the last code septet
   -- with sign-extension bits, but pad bits after Max_Value_Bits (which
   -- for IAR XLINK seems to be 32) with zero bits, making the decoder
   -- see a zero sign bit and believe that the number is non-negative
   -- while the "real" sign bit (bit number Max_Value_Bits - 1) is
   -- ignored.


end Formats.LEB128.Opt;
