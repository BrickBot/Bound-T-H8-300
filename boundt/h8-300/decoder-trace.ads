-- Decoder.Trace (decl)
--
-- Tracing the decoding process by displaying each instruction to
-- be decoded and the result of the decoding: the disassembled
-- instruction, the arithmetic effect (optional) and the resulting
-- steps (by number) in the control-flow graph.
--
-- This file supports the Renesas H8/300 target processor.
--
-- Authors:
--    Samuel Petersson, Mälardalen University
--    Niklas Holsti, Tidorum Ltd
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
-- $Date: 2015/10/26 22:19:13 $
--
-- $Log: decoder-trace.ads,v $
-- Revision 1.5  2015/10/26 22:19:13  niklas
-- Version 4b, and moved to free open-source licence.
--
-- Revision 1.4  2005/09/03 11:59:17  niklas
-- BT-CH-0006.
--
-- Revision 1.3  2005/03/22 20:42:05  niklas
-- Moved "with Arithmetic" here from parent.
--
-- Revision 1.2  2004/06/27 15:49:11  niklas
-- Added Start and Stop procedures.
--
-- Revision 1.1  2004/06/16 07:41:35  niklas
-- First version.
--


with Arithmetic;
with Flow;
with H8_300;


package Decoder.Trace is

-- For each instruction, the decoding is traced by generating
-- a "trace line" on standard output.
-- The trace lines contains a number of fields or columns.
-- The type and order of the fields is generally the same for
-- all instructions.

   
   procedure Start_Decoding (
      Name     : in String;
      Resuming : in Boolean);
   --
   -- Reports that decoding starts or resumes for the subprogram of
   -- the given Name. Prints the column headings.


   -- The following procedures generate the trace lines field by field.
   --
   -- The procedures Instruction_Address, Instruction and Result
   -- should be called in this (otherwise, the trace line will be
   -- broken into many lines, to "go back" to earlier positions).
   -- However, it is allowed to skip any of the procedures, and
   -- the corresponding field will then be blank.


   procedure Step_Tag (Item : in Flow.Step_Tag_T);
   --
   -- Traces the location and context of the instruction to be decoded.


   procedure Code_Words (
      Address : in Processor.Code_Address_T;
      Code    : in H8_300.Word_List_T);
   --
   -- Traces the words of the H8_300 instruction to be decoded, and its
   -- address. The Code can contain one or two words.


   procedure Instruction (Item : in H8_300.Instruction_T);
   --
   -- Traces the decoded H8/300 instruction in disassembled form.


   procedure Result (
      Effect : in Arithmetic.Effect_Ref;
      Effort : in Processor.Effort_T);
   --
   -- Traces the results of decoding.


   procedure Step (Item : in Flow.Step_T);
   --
   -- Traces the creation of new steps as the instruction is
   -- decoded. This procedure can be called at any time,
   -- in any order with respect to the other procedures.
   -- It can called several times per instruction, and the
   -- steps are accumulated.


   procedure Remark (Text : in String);
   --
   -- Any remark to explain or characterise the instruction or
   -- its decoding. This procedure can be called at any time,
   -- in any order with respect to the above procedures.
   -- It can called several times per instruction, and the
   -- remarks are accumulated and separated by commas.


   procedure Done;
   --
   -- Finish the trace for this instruction (as New_Line).
   -- Outputs the accumulated steps (if any) and clears the
   -- step-store the next instruction.
   -- Outputs the accumulated remarks (if any) and clears
   -- the remark-store for the next instruction.


   procedure Stop_Decoding (
      Name      : in String;
      Graph     : in Flow.Graph_T;
      Suspended : in Boolean);
   --
   -- Reports that decoding stops for the subprogram of the given
   -- Name, having constructed a given flow Graph. This may be
   -- the final stop, or decoding may only be Suspended in order
   -- to resolve some dynamic flow. 


end Decoder.Trace;
