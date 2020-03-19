-- H8_300.Verify (decl)
--
-- Verifying the decoding and disassembly of Renesas H8/300 instructions.
--
-- Author: Niklas Holsti
--
-- This file is a component of the Bound-T Worst-Case Execution Time Tool.
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
-- $Date: 2015/10/26 22:19:14 $
--
-- $Log: h8_300-verify.ads,v $
-- Revision 1.3  2015/10/26 22:19:14  niklas
-- Version 4b, and moved to free open-source licence.
--
-- Revision 1.2  2005/10/24 11:33:00  niklas
-- Changed Check_All_Words from a global option to a parameter of
-- Try_All so that it can be controlled eg by command-line options.
--
-- Revision 1.1  2004/10/22 07:53:43  niklas
-- First version.
--


package H8_300.Verify is


   procedure Try (
      Source : in String;
      Word   : in Unsigned_Word_T;
      Instr  : in Instruction_T);
   --
   -- Decodes a given instruction Word that represents a 16-bit instruction
   -- and compares against an expected decoded Instr and an expected Source
   -- disassembly.


   procedure Try (
      Source : in String;
      Word   : in Unsigned_Word_T;
      More   : in Unsigned_Word_T;
      Instr  : in Instruction_T);
   --
   -- Decodes a given instruction Word that represents a 32-bit instruction
   -- and thus needs one More word, and compares against an expected decoded
   -- Instr and an expected Source disassembly.


   procedure Try_All (Check_All_Words : in Boolean);
   --
   -- Decodes one or a couple of examples of all instruction types and
   -- compares against expected results.
   --
   -- Check_All_Words
   --    Whether to try to decode all possible instruction words (one-
   --    and two-word sequences), to verify that the decoder can handle
   --    any input without exceptions, even invalid input.


end H8_300.Verify;
