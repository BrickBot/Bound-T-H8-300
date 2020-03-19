-- Decoder.IAR (body)
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
-- $Revision: 1.7 $
-- $Date: 2015/10/26 22:19:13 $
--
-- $Log: decoder-iar.adb,v $
-- Revision 1.7  2015/10/26 22:19:13  niklas
-- Version 4b, and moved to free open-source licence.
--
-- Revision 1.6  2009/12/02 14:20:01  niklas
-- BT-CH-0188: H8/300 updates for bit-widths and Word_T.
--
-- Revision 1.5  2007-03-22 18:51:34  niklas
-- BT-CH-0053.
--
-- Revision 1.4  2007/01/21 22:07:59  niklas
-- Updated for BT-CH-0041.
--
-- Revision 1.3  2005/10/20 17:05:55  niklas
-- Added function Unresolved_Call_Effect to support dynamic calls
-- as implemented in BT-CH-0014. For the same reason, moved
-- Register_Set_T to Processor.Cells.
--
-- Revision 1.2  2005/05/09 16:23:32  niklas
-- Removed erroneous fault message for Jump_Index.
--
-- Revision 1.1  2005/05/09 15:56:15  niklas
-- First version.
--


with Decoder.IAR.Opt;
with Output;
with Processor;
with Processor.Cells;
with Storage;
with Storage.Bounds;


package body Decoder.IAR is


   --
   --    Protocol options
   --


   Param_Regs :
      constant array (Opt.Param_Regs_T) of Processor.Cells.Register_Set_T := (
         0 => (5 .. 6 => True, others => False),
         1 => (5 .. 6 => True, others => False),
         2 => (4 .. 6 => True, others => False),
         3 => (3 .. 6 => True, others => False));
   --
   -- The set of registers that can be used for passing parameters,
   -- as a function of the -ur option.
   --
   -- Register R7 = SP is never considered a parameter register.
   --
   -- The difference between -ur0 and -ur1 is that -ur0 passes at most one
   -- parameter in registers, usually in R6L or R6, and uses R5:R6 only
   -- for a 32-bit parameter, while -ur1 puts as many parameters as will
   -- fit in registers R5 and R6. This is TBC, though.


   Trash_Regs :
      constant array (Opt.Trash_Regs_T) of Processor.Cells.Register_Set_T := (
         0 => (                others => False),
         1 => (     6 => True, others => False),
         2 => (5 .. 6 => True, others => False),
         3 => (4 .. 6 => True, others => False),
         4 => (3 .. 6 => True, others => False));
   --
   -- The set of registers that can be trashed (clobbered, modified)
   -- by a subprogram call, as a function of the -uu option.
   --
   -- Register R7 = SP is never a trash register.


   --
   --    Calling protocols
   --


   function Map_Kind (
      Callee : Storage.Cell_T;
      Under  : Protocol_T)
   return Calling.Map_Kind_T
   is
      use Calling;
      use Processor;

      Spec : constant Cell_Spec_T := Storage.Spec_Of (Callee);

   begin

      case Spec.Kind is

      when Flag_Kind_T => return Fixed;

      when Reg_Kind_T =>

         if Under.Param_Regs(Spec.Num) then
            -- The register is used (may be used) for parameters.
            -- If the register is not used for parameters, the value-origin
            -- analysis may show that its value is invariant (saved and
            -- restored).

            return Fixed;

         elsif Under.Trash_Regs(Spec.Num) then
            -- This register may be trashed (clobbered) so it cannot
            -- be considered Privy. Again, the value-origin analysis may
            -- show that the register's value is not trashed.

            return Fixed;

         else
            -- The register is not used for parameters and is saved
            -- and restored by the Callee. It thus acts as a private
            -- cell for the Callee.

            return Privy;

          end if;

      when Local_Kind_T => return Privy;

      when Param_Kind_T => return Dynamic;

      when Mem_Kind_T   => return Static;

      when Stack_Height => return Privy;

      when Jump_Index   => return Privy;

      end case;

   end Map_Kind;


   function Caller_Cell (
      Callee : Storage.Cell_T;
      Under  : Protocol_T)
   return Storage.Cell_T
   is
      use Processor;
      use type Arithmetic.Value_T;

      Spec : constant Cell_Spec_T := Storage.Spec_Of (Callee);
      -- The spec for the callee cell.

      Take_Off : Arithmetic.Value_T;
      -- The take-off stack-height in the caller.
      -- There is only one stack.

      Caller_Offset : Local_Offset_T;
      -- The offset for the Caller Local cell that corresponds to
      -- the Callee cell.

   begin

      case Spec.Kind is

      when Param_Kind_T =>

         if not Calling.Static (Calling.Protocol_T'Class(Under)) then
            -- Take-off height not known.

            return Storage.No_Cell;

         else
            -- The take-off stack height is known, so we can map.

            Take_Off := Storage.Bounds.Single_Value (
               Under.Intervals(Under.Intervals'First));

            begin

               Caller_Offset := Local_Offset_T (
                  Take_Off - Arithmetic.Value_T (Spec.Param));

               return Cells.Local (
                  Offset => Caller_Offset,
                  Width  => Cell_Width(Spec.Kind));

            exception

            when Constraint_Error =>
               -- Problem in Caller_Offset.

               Output.Error (
                    "The callee cell "
                  & Storage.Image (Callee)
                  & " points beyond the caller's frame which has "
                  & Arithmetic.Image (Take_Off)
                  & " octets.");

               return Storage.No_Cell;

            end;

         end if;

      when others =>
         -- No mapping needed.

         return Callee;

      end case;

   end Caller_Cell;


   function Sure_Invariant (
      Caller : Storage.Cell_T;
      Under  : Protocol_T;
      Stub   : Calling.Stub_Level_T)
   return Boolean
   is
      use Processor;

      Spec : constant Cell_Spec_T := Storage.Spec_Of (Caller);
      -- The spec for the caller cell.

   begin

      case Spec.Kind is

      when Flag_Kind_T =>
         -- The CCR condition flags can be modified by any call.

         return False;

      when Reg_Kind_T =>
         -- A register can be modified by any call if it is a
         -- trash register or a parameter-passing register.

         return not (   Under.Param_Regs(Spec.Num)
                     or Under.Trash_Regs(Spec.Num));

      when Param_Kind_T =>
         -- A cell that the caller sees as a Parameter should
         -- normally not be modified by a call. TBM if the
         -- address of the cell is made available to the callee.

         return True;

      when Local_Kind_T =>
         -- A cell that the caller sees as a Local cell could
         -- be modified by a call if its address is made available
         -- to the callee. TBA address-taken analysis.

         return False;

      when Mem_Kind_T =>
         -- Statically allocated memory cells are available to
         -- any subprogram and so could be modified by any call.

         return False;

      when Stack_Height =>
         -- The stack-height is usually not preserved across a
         -- call because the callee pops the return address.

         return False;

      when Jump_Index =>
         -- The jump-index is a kind of global cell, although it
         -- should never be significant on entry to a subprogram
         -- or return from a subprogram.

         return False;

      end case;

   end Sure_Invariant;


   function Protocol (Program : Programs.Program_T)
   return Calling.Protocol_Ref
   is

      Ref : constant Protocol_Ref := new Protocol_T (
         Num_Stacks => Programs.Number_Of_Stacks (Program));
      -- The result.

   begin

      Ref.Program    := Program;
      Ref.Param_Regs := Param_Regs(Opt.Param_Regs);
      Ref.Trash_Regs := Trash_Regs(Opt.Trash_Regs);

      return Calling.Protocol_Ref (Ref);

   end Protocol;


   function Unresolved_Call_Effect (Program : Programs.Program_T)
   return Arithmetic.Effect_Ref
   is
      use type Processor.Cells.Register_Set_T;
   begin

      -- The call can alter any register used for parameter-passing
      -- and any trash register:

      return Processor.Cells.Veiling (
         Registers => Param_Regs(Opt.Param_Regs)
                   or Trash_Regs(Opt.Trash_Regs));

   end Unresolved_Call_Effect;


end Decoder.IAR;
