-- Processor.Timing (decl)
--
-- Modelling the timing (number of clock states) of H8/300 instructions and
-- in particular the time required for operand access.
--
-- The timing depends on the specific device (Processor.Opt.Device), the
-- instruction and its operands, and perhaps on other options.
--
-- Author: Niklas Holsti, Tidorum Ltd.
--
-- A component of the Bound-T Timing Analysis Tool.
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
-- $Date: 2015/10/26 22:19:15 $
--
-- $Log: processor-timing.ads,v $
-- Revision 1.5  2015/10/26 22:19:15  niklas
-- Version 4b, and moved to free open-source licence.
--
-- Revision 1.4  2005/09/03 12:00:00  niklas
-- BT-CH-0006.
--
-- Revision 1.3  2005/04/23 07:49:19  niklas
-- Added Time_To_Auto_Mod.
--
-- Revision 1.2  2005/03/22 20:39:48  niklas
-- Updated for the removal of Memory_Indirect from Operand_T.
-- Added function Time_To_Vector to handle Memory_Indirect.
--
-- Revision 1.1  2005/01/26 20:04:14  niklas
-- First version.
--


with Flow;


package Processor.Timing is


   function Time_To_Fetch (
      From   : Flow.Step_Tag_T;
      Length : H8_300.Instruction_Length_T)
   return Effort_T;
   --
   -- The time required to fetch an instruction of the given Length
   -- From a step with the given tag (context and state).


   Time_To_Auto_Mod : constant Effort_T := 2;
   --
   -- The time required to auto-modify a pointer register when
   -- it is used in a Register_Indirect operand. Two Internal
   -- cycles are needed.


   function Time_To_Read (
      Operand : H8_300.Operand_T;
      Width   : H8_300.Width_T)
   return Effort_T;
   --
   -- The time required to read the Operand of the given Width.
   -- Internal cycles for auto-modifying the Pointer of a
   -- Register_Indirect Operand are included.


   function Time_To_Write (
      Operand : H8_300.Operand_T;
      Width   : H8_300.Width_T)
   return Effort_T;
   --
   -- The time required to write the Operand of the given Width.
   -- Internal cycles for auto-modifying the Pointer of a
   -- Register_Indirect Operand are included.


   function Time_To_Vector (Through : H8_300.Vector_T)
   return Effort_T;
   --
   -- The time to read the vector address from memory, for a
   -- Jump/Jump_Subroutine with memory-indirect addressing.


   function Time_To_Push return Effort_T;
   --
   -- The time required to push a Word on the stack.
   -- Internal cycles for auto-modifying the Stack Pointer are included.


   function Time_To_Pop return Effort_T;
   --
   -- The time required to pop a Word from the stack.
   -- Internal cycles for auto-modifying the Stack Pointer are included.


   --
   --    Notes on internal cycles
   --

   -- The number of internal cycles for auto-modifying Register_Indirect
   -- operand pointers (including stack push/pop) are set assuming only
   -- one such access per instruction. The number may need to be adjusted
   -- for special instructions that perform more than one register-indirect
   -- memory access with auto-modification, for example EEPMOV.
   --
   -- Moreover, we assume that one internal cycle needs one execution
   -- state (clock state), in other words, the number of states per
   -- internal cycle is one (1).
   --
   -- The internal cycles for JMP/JSR with indirect operands are not
   -- included in the times computed here, because these operands do
   -- not auto-modify.


end Processor.Timing;
