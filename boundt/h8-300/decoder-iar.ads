-- Decoder.IAR (decl)
--
-- Modelling code generated by the IAR compiler for the Renesas H8/300,
-- including:
--
-- > calling protocols
--
-- > switch/case support functions
--
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
-- $Log: decoder-iar.ads,v $
-- Revision 1.5  2015/10/26 22:19:13  niklas
-- Version 4b, and moved to free open-source licence.
--
-- Revision 1.4  2009/12/02 14:20:01  niklas
-- BT-CH-0188: H8/300 updates for bit-widths and Word_T.
--
-- Revision 1.3  2007-01-21 22:07:59  niklas
-- Updated for BT-CH-0041.
--
-- Revision 1.2  2005/10/20 17:05:55  niklas
-- Added function Unresolved_Call_Effect to support dynamic calls
-- as implemented in BT-CH-0014. For the same reason, moved
-- Register_Set_T to Processor.Cells.
--
-- Revision 1.1  2005/05/09 15:56:15  niklas
-- First version.
--


with Arithmetic;
with Calling;
with Calling.Stacked;
with Processor.Cells;
with Programs;
with Storage;


package Decoder.IAR is


   --
   --    Calling protocols
   --
   -- Ref: H8/300 IAR C Compiler Reference Guide
   -- IAR Part number CH8300-1, first edition, February 2000.


   type Protocol_T is new Calling.Stacked.Protocol_T
   with record
      Param_Regs : Processor.Cells.Register_Set_T;
      Trash_Regs : Processor.Cells.Register_Set_T;
   end record;
   --
   -- The calling protocol used by the IAR C Compiler, version TBD.
   --
   -- This protocol uses the HW (SP = R7) stack for return addresses and
   -- parameters.
   --
   -- Param_Regs
   --    The set of registers used for passing parameters from the
   --    caller to the callee (and perhaps back, too).
   -- Trash_Regs
   --    The set of registers that can be trashed (clobbered, changed)
   --    by the callee.
   --
   -- By default all parameters except the first are passed on the stack.
   -- The first parameter is passed in R6L, R6 or R5:R6 depending on its
   -- size, but TBC has a location on the stack, too.
   --
   -- The compiler option -ur can assign additional registers for passing
   -- parameters as follows:
   --    -ur0    The default method described above.
   --    -ur1    R5 and R6 are used for parameters
   --    -ur2    R4, R5, R6 are used for parameters
   --    -ur3    R3, R4, R5, R6 are used for parameters.
   -- Up to 8 parameters can be passed in registers, TBC by passing 8
   -- one-octet parameters in R(3..6)(L..H).
   --
   -- Function results are passed back TBC in R6L, R6 or R5:R6, depending
   -- on the size of the result.
   --
   -- By default a subprogram must preserve (save/restore) all registers
   -- except registers used to pass parameters or results.
   --
   -- The compiler option -uu can designate certain registers as "trash"
   -- registers that can be clobbered (changed) by a subprogram and need
   -- not be restored to their original values. The possibilities are:
   --    -uu0    No registers destroyed (TBC if = default, or special)
   --    -uu1    R6 is trash
   --    -uu2    R5, R6 are trash
   --    -uu3    R4, R5, R6 are trash
   --    -uu4    R3, R4, R5, R6 are trash.
   --
   -- TBD interactions between -ur and -uu.
   --
   -- The effective -ur and -uu are held in Decoder.IAR.Opt.
   --
   -- The actual parameter registers for a given subprogram are defined
   -- primarily by the symbol-table for the subprogram, and by the -ur
   -- option only if there is no symbol table.
   --
   -- The actual trash registers for a given subprogram are defined by
   -- the -uu option. However, for flexibility the trash register set is
   -- also a component of each Protocol object.


   type Protocol_Ref is access all Protocol_T'Class;
   --
   -- A reference to an IAR protocol object.


   function Map_Kind (
      Callee : Storage.Cell_T;
      Under  : Protocol_T)
   return Calling.Map_Kind_T;
   --
   -- Overrides (implements) Calling.Stacked.Map_Kind.


   function Caller_Cell (
      Callee : Storage.Cell_T;
      Under  : Protocol_T)
   return Storage.Cell_T;
   --
   -- Overrides (implements) Calling.Stacked.Caller_Cell.


   -- overriding
   function Sure_Invariant (
      Caller : Storage.Cell_T;
      Under  : Protocol_T;
      Stub   : Calling.Stub_Level_T)
   return Boolean;


   function Protocol (Program : Programs.Program_T)
   return Calling.Protocol_Ref;
   --
   -- Creates a new instance of the IAR protocol for a call in
   -- the given Program.


   function Unresolved_Call_Effect (Program : Programs.Program_T)
   return Arithmetic.Effect_Ref;
   --
   -- The arithmetic effect to be assumed for an unresolved dynamic
   -- call (that is, a call where the callee is unknown).


end Decoder.IAR;
