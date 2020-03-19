-- Decoder (body)
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
-- $Revision: 1.39 $
-- $Date: 2015/10/26 22:19:13 $
--
-- $Log: decoder.adb,v $
-- Revision 1.39  2015/10/26 22:19:13  niklas
-- Version 4b, and moved to free open-source licence.
--
-- Revision 1.38  2015/10/26 21:57:56  niklas
-- Updated to use current Options services, etc, and to omit Extra_Symbols.
--
-- Revision 1.37  2010/02/25 12:22:05  niklas
-- Changed the name of the stack from "SP-stack" to just "SP".
-- Updated Model_Jump_Subroutine_Instruction for the changes in
-- Decoder.Dynamics (Call_Via_Pointer_T) and to model JSR @@aa:8,
-- with a dynamic vector value, as a dynamic call.
--
-- Revision 1.36  2009-12-02 14:20:01  niklas
-- BT-CH-0188: H8/300 updates for bit-widths and Word_T.
--
-- Revision 1.35.2.1  2009-10-20 16:55:01  niklas
-- Version 3a2a, correcting BT-NC-0201.
--
-- Revision 1.35  2009-07-15 12:01:01  niklas
-- Changed Stack_Datum_Octets to use H8_300.Stack_Operand_Octets.
--
-- Revision 1.34  2009-07-15 11:48:26  niklas
-- Updated stack definitions in Initialize (Program) per BT-CH-0171.
-- Improved Value_Range, Max_Pos_Imm, Auto_Mod_Octets to use Operand_Octets
-- instead of literals. Added Datum_Octets and Stack_Datum_Octets and
-- modified New_Stack_Height and Pop_Return_Exception for same reason.
-- Updated Decoded.Decoded_Instruction to take a Program_T
-- instead of a Model_Ref, per BT-CH-0158.
--
-- Revision 1.33  2008-10-29 14:48:37  niklas
-- Updated for BT-CH-0142: Integrate_Call.Cond.
--
-- Revision 1.32  2008/04/29 20:12:47  niklas
-- Updated for BT-CH-0121 by removing param Decode.Decoded_Step.
--
-- Revision 1.31  2008/02/02 14:53:15  niklas
-- Extended Initialize (Program) to silently forward Argument_Error.
--
-- Revision 1.30  2007/12/27 23:12:32  niklas
-- BT-CH-0104: S-record program format for Bound-T/H8-300.
--
-- Revision 1.29  2007/03/22 18:51:34  niklas
-- BT-CH-0053.
--
-- Revision 1.28  2007/01/21 22:07:58  niklas
-- Updated for BT-CH-0041.
--
-- Revision 1.27  2006/11/28 19:23:44  niklas
-- Updated for BT-CH-0037.
--
-- Revision 1.26  2006/11/05 21:19:37  niklas
-- BT-CH-0036: Property BCC_Signed.
--
-- Revision 1.25  2006/08/23 19:32:42  niklas
-- Updates for BT-CH-0025.
--
-- Revision 1.24  2006/08/22 11:15:36  niklas
-- Updated for recent changes in generic/specific interface by
-- renaming Loading_Error to Decoder_Error and adding Patch_Code.
-- Updated per BT-CH-0019 for changes in Flow.Calls interfaces.
--
-- Revision 1.23  2005/10/20 17:16:13  niklas
-- Modified Model_Jump_Subroutine_Instruction to model dynamic calls
-- ("jsr @Rn") as in BT-CH-0014, by Flow.Calls.Add_Boundable_Call.
-- For support, added the function Unresolved_Call_Effect.
-- Updated Trace_Main_Step and Trace_Extra_Step to use the new
-- convenience function Flow.Effort.
--
-- Revision 1.22  2005/09/06 12:27:56  niklas
-- Changed Model_Two_Operand_Instruction to add, to the arithmetic
-- effect of a dummy move instruction (mov Rd,Rd), an equality
-- constraint that shows the relationship between the values of the
-- whole 16-bit register and its 8-bit parts. While this addition is
-- consistent with the current arithmetic model, it was chosen
-- specifically to support the analysis of the switch/case structures
-- in some routines (set_lcd_segment et al.) in the Lego ROM.
--
-- Revision 1.21  2005/09/05 11:33:23  niklas
-- Updated for BT-CH-0007.
-- Extended Model_Jump_Subroutine_Instruction to handle "jsr @Rn"
-- by creating a boundable edge of type Call_Via_Register_T.
--
-- Revision 1.20  2005/09/03 11:58:35  niklas
-- BT-CH-0006 and consequences.
-- Corrected Model_Branch_Subroutine_Instruction  and
-- Model_Jump_Subroutine_Instruction to use Trace_Main_Step
-- instead of Trace.Step, thus showing also the effort and effect
-- of the "bsr" or "jsr" step.
--
-- Revision 1.19  2005/08/25 21:09:27  niklas
-- Extended Initialize to load the optional S-record file (option
-- -srec) and the optional extra-symbols file (option -sym).
--
-- Revision 1.18  2005/07/25 19:37:06  niklas
-- Added tracing of Bcc condition.
--
-- Revision 1.17  2005/05/11 18:33:04  niklas
-- Changed the arithmetic model of overlapping octet and word cells
-- so that an assignment to a part of a word makes the word unknown,
-- instead of recomputing the word (register) from its octet parts.
-- As before, an assignment to a word makes its parts unknown unless
-- the assigned word value can be divided into parts which can be assigned
-- separately to the parts of the target word; this goes for immediate
-- values, cell values and now also for reference values. This model
-- supports stacked octet parameters for IAR and also supports the
-- value-origin analysis of octet registers. "Compose" steps are no
-- longer used.
-- Simplified the arithmetic model of the Increment and Decrement
-- instructions to just increment or decrement the register cell without
-- conditions. This is necessary to analyse loops that count with these
-- instructions.
-- Corrected Model_Call to trace the Call step and consequently changed
-- Model_Branch_Subroutine and Model_Jump_Subroutine to trace the main
-- step (instead of tracing it in the Model procedure) so that the steps
-- are traced in order of creation.
--
-- Revision 1.16  2005/05/09 16:02:42  niklas
-- Added support for the IAR compiler. The calling protocol is chosen
-- according to the executable format (GCC => COFF, UBROF => IAR).
-- Immediate word operands can now be taken as signed. Added the
-- function Unsigned_Word to help find the low and high octets of
-- a negative word. The new option Opt.Move_Signed_Words controls the
-- signedness of immediate operands in Move.
--
-- Revision 1.15  2005/05/01 20:28:30  niklas
-- Extended Update_Touched_Cells to assign the low and high
-- octets of an immediate word operand to the low and high parts
-- of the target cell, respectively.
--
-- Revision 1.14  2005/04/23 07:56:38  niklas
-- Corrected Model_Two_Operand_Instruction to include the operand
-- access time only once for the Move_From/To_Peri instructions.
-- Corrected Model_Jump_Instruction to add two Internal cycles
-- when the operand is Memory_Indirect.
-- Corrected Model_Jump_Subroutine_Instruction to always count two
-- Fetch cycles as if the instruction were two words long.
-- Corrected Model_Move_Block_Instruction to exclude the Auto_Mod
-- time from the Effort_Per_Octet.
--
-- Revision 1.13  2005/04/20 12:31:00  niklas
-- Extended Model_Move_Block_Instruction to trace the synthetic steps.
-- Added procedures Trace_Main_Step and Trace_Extra_Step to help.
-- Removed unused variable Last_Step from procedure Model.
--
-- Revision 1.12  2005/04/01 17:21:13  niklas
-- Added decoding of Memory_Indirect jumps and calls via constant
-- vector slots. Improved the error messages for unimplemented
-- dynamic jumps and calls.
-- Added modeling of Load_CCR and Store_CCR.
-- Corrected Model.Add_Normal_Step to use the Effect parameter
-- instead of No_Effect (oops).
-- Added a variant Model.Add_Normal_Step that takes an Effect_Ref
-- and no Effort; useful for CCR Transfer and CCR Update.
--
-- Revision 1.11  2005/04/01 12:40:52  niklas
-- Added combination of the instruction pairs "cmp.b;subx" and
-- "sub.b;subx", giving steps of the kind Cmp_Subx and Sub_Subx
-- respectively.
--
-- Revision 1.10  2005/03/25 16:59:18  niklas
-- Added modelling of dynamic jumps via address tables as follows.
-- Extended Model_Two_Operand_Instruction to check for the jump-
-- via-table instruction sequence (mov.w from table to register
-- followed by jmp @register), to model the table-index in the new
-- Jump_Index cell and to create a Jump_Via_Table loose edge to the
-- jump instruction, thus providing the table base address.
-- Extended Model_Jump_Instruction to detect the Jump_Via_Table case
-- and create a dynamic edge accordingly.
-- Also modified Model_Two_Operand_Instruction to construct the
-- Source operand expression only when needed. This avoids the
-- useless construction of dynamic memory references in some cases.
-- Also modified Model_Two_Operand_Instruction to avoid creating
-- trivial z-conditions of the form "constant = zero" and to create
-- instead assignments of the form z = 0 or z = 1.
--
-- Revision 1.9  2005/03/24 18:22:23  niklas
-- Changed Update_Stack_Height to use the new function Arithmetic.-
-- Algebra.To_Expr (Terms) which simplifies the expression by combining
-- terms with the same variable. This means that some unknown terms
-- can cancel and lets Flow.Const compute stack usage in more cases.
--
-- Revision 1.8  2005/03/23 20:09:37  niklas
-- Extended Model_Two_Operand_Instruction to combine an Add operation
-- with the following Add_With_Carry if the two together make up a
-- 16-bit addition. This also means that the Decode operation may be
-- given loose edges with targets of the Add_Addx kind.
--
-- Revision 1.7  2005/03/22 21:03:17  niklas
-- First implementation of arithmetic analysis as follows.
-- The 8/16-bit computation is interpreted as unsigned arithmetic,
-- with some immediate literal operands considered signed values.
-- The condition flags are replaced by two Boolean cells, Equal
-- and Less, which are defined only to model the relationship of
-- unsigned integer values. However, the option Opt.Unsigned_Cond
-- can make the analysis understand some signed arithmetic, too.
-- Bit operations are considered opaque.
-- Added the use of Compose steps (for two-operand instructions)
-- and Clobber steps (for Move_Block instructions).
-- Using Processor.Cells, Decoder.Dynamics and Decoder.GCC.
--
-- Other changes as follows:
-- Corrected Model_Bit_Instruction to include Time_To_Read for
-- all bit instructions (not just for non-Write ones).
-- Updated Model_Jump_Instruction to use Target_T.
-- Updated Model_Jump_Subroutine_Instruction to use Target_T
-- which corrects its use of Target.Kind = Register instead of
-- the correct Register_Indirect.
-- Changed Model_Jump(_Subroutine)_Instruction to use
-- Time_To_Vector instead of Time_To_Read.
--
-- Revision 1.6  2005/03/17 07:32:37  niklas
-- Updated for changes in the general part as follows.
-- Parameters of a reference type use "in" mode.
-- Removed all Resolve_Xxx operations.
-- Removed all operations for calling protocols.
-- Moved some stuff to the new package Processor.Properties.
-- Added the Post_Analysis procedure.
-- Adapted all uses of Symbol_Table and Processor_Info to the
-- privatization of Program_T attributes.
-- Extended Initialize to define the program's stacks (not a
-- complete implementation yet).
-- Adapted Model_Call to changes in Flow.Calls.Add_Call.
--
-- Revision 1.5  2005/03/16 15:40:09  niklas
-- As implemented by Samuel Petersson and delivered to Tidorum from
-- Mälardalen on 2005-03-15. Fully models the H8/300 instructions
-- in the flow-graph with the exception of arithmetic effects and
-- dynamic references. Samuel also corrected procedure Decode by
-- changing the if-statement after the call of Format.Fetch_Code
-- from "Last_Word < Words'Last" to "Last_Word < Words'First".
-- Tidorum standardized the formatting and corrected the Effort of
-- the Loop_Step in the Move_Block model (omit the fetch effort).
--
-- Revision 1.4  2005/01/26 20:18:13  niklas
-- Updated the setting of Effort_T values to use execution state counts
-- and functions from Processor.Timing.
-- Updated the Power function to use a null Power_T type.
-- The Initialize procedure now calls Decoder.Opt.Finish to cross-check
-- and adjust the command-line option values.
--
-- Revision 1.3  2004/09/27 08:52:10  niklas
-- Updated the assignment targets to use Arithmetic.Variable_T
-- instead of Storage.Cell_T.
--
-- Revision 1.2  2004/06/27 15:49:11  niklas
-- Added Start and Stop procedures.
--
-- Revision 1.1  2004/06/16 07:41:36  niklas
-- First version.
--


with Arithmetic.Algebra;
with Arithmetic.Opt;
with Decoder.Dynamics;
with Decoder.GCC;
with Decoder.IAR;
with Decoder.Opt;
with Decoder.Trace;
with Format;
with H8_300;
with H8_300.Text;
with Options;
with Options.String_Sets;
with Output;
with Processor.Cells;
with Processor.Program;
with Processor.Timing;
with Storage;
with Storage.Bounds;
with String_Pool;
with Symbols;
with Symbols.Show;
with Flow.Calls;

pragma Elaborate_All (Processor.Cells);


package body Decoder is


   use H8_300;
   use Processor.Cells;

   use type Arithmetic.Effect_Ref;
   use type Arithmetic.Expr_Ref;
   use type Arithmetic.Value_T;
   use type Arithmetic.Word_T;


   function EqZero (Expr : Arithmetic.Expr_Ref)
   return Arithmetic.Expr_Ref
   renames Arithmetic.EqZero;

   function EqOne (Expr : Arithmetic.Expr_Ref)
   return Arithmetic.Expr_Ref
   renames Arithmetic.EqOne;


   --
   ---   Loading the target program for analysis
   --


   procedure Initialize (
      File_Name : in     String;
      Program   : in out Programs.Program_T)
   is

      Info : Processor.Program.Info_T;

   begin

      Decoder.Opt.Finish;

      Info := new Format.Object_T;

      -- Load the main executable file:

      Format.Load_File (
         File_Name    => File_Name,
         Object       => Info,
         Symbol_Table => Programs.Symbol_Table (Program));

      -- Load the optional, additional S-record file if any:

      declare
         use Options.String_Sets, String_Pool;

         S_Record_Files : constant String_List_T :=
            To_List (Opt.S_Record_File_Opt);

      begin

         for S in S_Record_Files'Range loop

            Format.Load_S_Record_File (
               File_Name => To_String (S_Record_Files(S)),
               Object    => Info);

         end loop;

      end;

      -- Set up the Program:

      Programs.Set_Processor_Info (
         To     => Info,
         Within => Program);

      -- Define the stacks:

      -- TBA to make the set of stacks depend on the compiler.

      Programs.Add_Unstable_Stack (
         Name       => "SP",
         Pointer    => SP_Cell,
         Height     => SH_Cell,
         Unit       => 8,
         Coupling   => Arithmetic.Algebra.Opposite,
         Net_Change => Storage.Bounds.Not_Limited,
         To         => Program);

   exception

   when Format.Loading_Error =>
      -- Already reported.

      raise Decoder_Error;

   when Decoder_Error =>
      -- Already reported.

      raise;

   when X : others =>
      -- Oops.

      Output.Exception_Info (
         Text       => "Decoder.Initialize",
         Occurrence => X);

      raise Decoder_Error;

   end Initialize;


   procedure Initialize (
      Subprogram : in Programs.Subprogram_T;
      Within     : in Programs.Program_T)
   is
   begin

      null;
      -- We have nothing to do so far, for the H8/300.

   end Initialize;


   procedure Patch_Code (
      Program : in     Programs.Program_T;
      Address : in     Processor.Code_Address_T;
      Data    : in     String;
      Params  : in     Programs.Code_Address_List_T;
      Valid   :    out Boolean)
   is
   begin

      Output.Error ("Patching is not implemented for this target.");

      Valid := False;

   end Patch_Code;


   procedure Dump_Program (File_Name : in String)
   is
   begin

      Format.Dump_File (File_Name);

   exception

   when X : others =>
      -- Oops.

      Output.Exception_Info (
         Text       => "Decoder.Dump_Program",
         Occurrence => X);

   end Dump_Program;


   procedure Pre_Analysis (
      Bounds : in Programs.Execution.Bounds_Set_T)
   is
   begin

      null;
      -- We have nothing to do for the H8/300.

   end Pre_Analysis;


   --
   ---   Arithmetic operands and effects:
   --


   Condition :
      constant array (Condition_T) of Arithmetic.Condition_T := (
      Always           => Arithmetic.Always,
      Never            => Arithmetic.Never,
      High             => EqZero (Equal_Flag) and EqZero (Less_Flag),
      Low_Or_Same      => EqOne  (Equal_Flag) or  EqOne  (Less_Flag),
      Carry_Clear      => EqZero (Less_Flag ),
      Carry_Set        => EqOne  (Less_Flag ),
      Not_Equal        => EqZero (Equal_Flag),
      Equal            => EqOne  (Equal_Flag),
      No_Overflow      => Arithmetic.Unknown,
      Overflow         => Arithmetic.Unknown,
      Plus             => Arithmetic.Unknown,
      Minus            => Arithmetic.Unknown,
      Greater_Or_Equal => Arithmetic.Unknown,
      Less             => Arithmetic.Unknown,
      Greater          => Arithmetic.Unknown,
      Less_Or_Equal    => Arithmetic.Unknown);
   --
   -- The arithmetic condition corresponding to a branch condition-code.
   --
   -- We assume that the Equal and Less flags result from the comparison
   -- of two unsigned values, the subtraction of two unsigned values, or
   -- the addition of an unsigned value and a negative literal. Thus,
   -- we model the unsigned comparison conditions but not the signed
   -- ones.
   --
   -- This is the edge precondition for taking a branch from a Bcc
   -- instruction.


   Similar_Unsigned_Cond :
      constant array (Condition_T range Greater_Or_Equal .. Less_Or_Equal)
      of Condition_T := (
      Greater_Or_Equal => Carry_Clear,
      Less             => Carry_Set,
      Greater          => High,
      Less_Or_Equal    => Low_Or_Same);
   --
   -- The branch condition for unsigned values that is equivalent to
   -- a given branch condition for signed values, assuming that the
   -- signed values are in the non-negative range.


   function Bit (
      Number : Word_Bit_T;
      Within : Unsigned_Word_T)
   return Bit_Value_T
   --
   -- The value of the bit with the given Number, Within a given word.
   --
   is
   begin

      return Field (Low => Number, High => Number, From => Within);

   end Bit;


   function Target (
      Operand : Operand_T;
      Width   : Width_T)
   return Arithmetic.Variable_T
   --
   -- The variable (cell or memory reference) that models the given
   -- destination Operand in an operation (instruction) with the given
   -- bit-Width.
   --
   is
   begin

      case Operand.Kind is

      when Immediate =>

         Output.Fault (
            Location => "Decoder.Target",
            Text     => "An Immediate operand cannot be a Target.");

         raise Program_Error;

      when Absolute =>

         -- TBA check for read-only, constant memory and emit an
         -- error message (cannot write read-only memory).

         return Arithmetic.Expr (Memory (
            Address => Processor.Address_T (Operand.Address),
            Width   => Width));

      when Register =>

         return Register_Var(Operand.Number, Operand.Part);

      when Register_Indirect =>

         return Arithmetic.Reference (Dynamics.Reference (
            Operand => Operand,
            Width   => Width));

      end case;

   end Target;


   function Width (Var : Arithmetic.Variable_T)
   return H8_300.Width_T
   --
   -- The width (octet or word) of the Variable, which must be an
   -- octet or a word, not a bit or a flag.
   --
   is

      Cell_Kind : Processor.Cell_Kind_T;
      -- The kind of the cell.

   begin

      case Arithmetic.Variable_Kind_T (Var.Kind) is

      when Arithmetic.Cell =>

         Cell_Kind := Storage.Spec_Of (Var.Cell).Kind;

         if Cell_Kind in Processor.Cell_Width'Range then

            return Processor.Cell_Width(Cell_Kind);

         else

            Output.Fault (
               Location => "Decoder.Width (Var)",
               Text     =>
                    "The variable cell is not an octet or a word"
                  & Output.Field_Separator
                  & Arithmetic.Image (Var));

            return Octet;

         end if;

      when Arithmetic.Ref =>

         return Dynamics.Width (Var.Ref);

      end case;

   end Width;


   Value_Range : constant array (Width_T) of Arithmetic.Value_T := (
      Octet => 2**(Octet_Bits * Natural (Operand_Octets(Octet))),
      Word  => 2**(Octet_Bits * Natural (Operand_Octets(Word ))));
   --
   -- The overall range of an Immediate operand of a given Width.
   -- For use in the calculation of a negative 2's complement value.


   function Low_Octet (Value : Arithmetic.Word_T)
   return Arithmetic.Expr_Ref
   --
   -- The unsigned low octet of the immediate 16-bit Value.
   --
   is
   begin

      return Arithmetic.Const (
         Value  => Value and Arithmetic.Max_Word (Octet_Bits),
         Width  => Octet_Bits,
         Signed => False);

   end Low_Octet;


   function High_Octet (Value : Arithmetic.Word_T)
   return Arithmetic.Expr_Ref
   --
   -- The unsigned high octet of the immediate 16-bit Value.
   --
   is
   begin

      return Low_Octet (Arithmetic.Shift_Right (
          Value  => Value,
          Amount => Octet_Bits));

   end High_Octet;


   function Low_Octet (Word : Arithmetic.Expr_Ref)
   return Arithmetic.Expr_Ref
   --
   -- The low octet of the given Word expression.
   --
   is
   begin

      case Word.Kind is

      when Arithmetic.Opaque =>

         return Arithmetic.Unknown;

      when Arithmetic.Const =>

         return Low_Octet (Word.Value);

      when Arithmetic.Cell =>

         return Processor.Cells.Low_Octet (Word.Cell);

      when Arithmetic.Ref =>

         return Arithmetic.Reference (To => Dynamics.Low_Octet (Word.Ref));

      when others =>
         -- A more complex expression.

         return Arithmetic.Unknown;

      end case;

   end Low_Octet;


   function High_Octet (Word : Arithmetic.Expr_Ref)
   return Arithmetic.Expr_Ref
   --
   -- The high octet of the given Word expression.
   --
   is
   begin

      case Word.Kind is

      when Arithmetic.Opaque =>

         return Arithmetic.Unknown;

      when Arithmetic.Const =>

         return High_Octet (Word.Value);

      when Arithmetic.Cell =>

         return Processor.Cells.High_Octet (Word.Cell);

      when Arithmetic.Ref =>

         return Arithmetic.Reference (To => Dynamics.High_Octet (Word.Ref));

      when others =>
         -- A more complex expression.

         return Arithmetic.Unknown;

      end case;

   end High_Octet;


   function Whole_Word (Octet : Arithmetic.Variable_T)
   return Arithmetic.Variable_T
   --
   -- The Word variable that contains the given Octet variable.
   --
   is
   begin

      case Arithmetic.Variable_Kind_T (Octet.Kind) is

      when Arithmetic.Cell =>

         return Processor.Cells.Whole_Word (Octet.Cell);

      when Arithmetic.Ref =>

         return Arithmetic.Reference (To => Dynamics.Whole_Word (Octet.Ref));

      end case;

   end Whole_Word;


   function Source (
      Operand : Operand_T;
      Width   : Width_T)
   return Arithmetic.Expr_Ref
   --
   -- The expression that represents the value of the Source Operand
   -- in an operation (instruction) with the given bit-Width.
   --
   is
   begin

      case Operand.Kind is

      when Immediate =>

         return Arithmetic.Const (
            Value  => Arithmetic.Word_T (Operand.Value),
            Width  => Width_In_Bits (Width),
            Signed => False);

      when Absolute =>

         -- TBA check for read-only, constant memory and return a
         -- constant value for such an Operand.

         return Arithmetic.Expr (Memory (
            Address => Processor.Address_T (Operand.Address),
            Width   => Width));

      when Register =>

         return Register_Var(Operand.Number, Operand.Part);

      when Register_Indirect =>

         return Arithmetic.Reference (Dynamics.Reference (
            Operand => Operand,
            Width   => Width));

      end case;

   end Source;


   procedure Update_Stack_Height (
      New_SP : in     Arithmetic.Expr_Ref;
      Within : in out Arithmetic.Assignment_Set_T)
   --
   -- Given an assignment SP := New_SP, we add an assignment that updates
   -- the local Stack Height (SH) accordingly.
   --
   is
      use type Arithmetic.Algebra.Term_T;
   begin

      Arithmetic.Add (
         To   => Within,
         More => Arithmetic.Set (
            Target => SH,
            Value  => Arithmetic.Algebra.To_Expr (
               Terms => (
                    1  * SH,
                    1  * Processor.Cells.SP,
                  (-1) * New_SP),
               Const => 0,
               Width => Arithmetic.Width_Of (SH))));
      --
      -- Note that as the stack grows downward, SP decreases in absolute
      -- value but the stack height increases, and therefore the stack
      -- height changes with the same amount but the opposite sign.

   end Update_Stack_Height;


   procedure Update_Touched_Cells (
      Target  : in     Arithmetic.Variable_T;
      Value   : in     Arithmetic.Expr_Ref;
      Within  : in out Arithmetic.Assignment_Set_T)
   --
   -- Given the assignment Target := Value, Within a set of assignments,
   -- this operation adds suitable assignments to any sub-cell or
   -- super-cell of the Target cell.
   --
   -- The Target must be an octet or a word, not a bit or flag.
   --
   -- Most new assignments are added
   -- Within the given assignment set, but when the Target is an
   -- octet register the assignment to the super-cell (the word
   -- register) is returned in Compose, which is otherwise returned
   -- as null.
   --
   is
      use Arithmetic;
      use Processor;
      use type Storage.Cell_T;


      procedure Veil (Touched : in Variable_T)
      --
      -- Adds an opaque assignment to the Touched variable.
      --
      is
      begin

         Add (Within, Set (Touched));

      end Veil;


   begin  -- Update_Touched_Cells

      case Width (Target) is

      when H8_300.Octet =>
         -- An octet cell touches the containing word cell and so
         -- we consider the word cell to get an unknown value.

         Veil (Whole_Word (Target));

         -- Is the Target a part of the Stack Pointer?

         if       Target.Kind = Arithmetic.Cell
         and then Cells.Whole_Word (Target.Cell).Cell = Cells.SP_Cell
         then
            -- Expletive deleted: the Stack Pointer is munged.

            Output.Warning (
                 "Assignment to "
               & Arithmetic.Image (Target)
               & " makes stack height unknown.");

            Veil (Cells.SH);

         end if;

      when H8_300.Word =>
         -- A word cell touches its octet parts.
         -- We try to assign the low/high octet of the Value
         -- to the low/high octet of the Target; if the Value is
         -- a complex expression the Target parts will be Unknown.

         Arithmetic.Add (
            To   => Within,
            More => Arithmetic.Effect_T'(
               Arithmetic.Set (
                  Target => Low_Octet (Target),
                  Value  => Low_Octet (Value )),
               Arithmetic.Set (
                  Target => High_Octet (Target),
                  Value  => High_Octet (Value ))));

      end case;

   end Update_Touched_Cells;


   procedure Assign (
      Target  : in     Arithmetic.Variable_T;
      Value   : in     Arithmetic.Expr_Ref;
      Within  : in out Arithmetic.Assignment_Set_T)
   --
   -- Adds the assignment Target := Value, Within a set of assignments,
   -- and also assignments to any sub-cell or super-cell of the Target.
   -- The Target must be an octet or a word, not a bit or flag.
   --
   is
      use type Arithmetic.Expr_Kind_T;
      use type Storage.Cell_T;
   begin

      Arithmetic.Add (
         To   => Within,
         More => Arithmetic.Set (Target, Value));

      Update_Touched_Cells (
         Target  => Target,
         Value   => Value,
         Within  => Within);

      if       Target.Kind = Arithmetic.Cell
      and then Target.Cell = Processor.Cells.SP_Cell
      then
         -- The SP is changed so the stack height should also change.

         Update_Stack_Height (
            New_SP => Value,
            Within => Within);

      end if;

   end Assign;


   Datum_Octets : constant array (Width_T) of Arithmetic.Expr_Ref := (
      Octet => Arithmetic.Const (
         Value  => Arithmetic.Word_T (Operand_Octets(Octet)),
         Width  => Address_Bits,
         Signed => False),
      Word  => Arithmetic.Const (
         Value  => Arithmetic.Word_T (Operand_Octets(Word )),
         Width  => Address_Bits,
         Signed => False));
   --
   -- The number of octets in an operand datum, as a function of the
   -- width of the operand. For use in modifying the value of a
   -- pointer to the datum.


   Auto_Mod_Octets : constant array (Width_T) of Arithmetic.Expr_Ref := (
      Octet => Datum_Octets(Octet),
      Word  => Datum_Octets(Word ));
   --
   -- The increase or decrease in a pointer (register indirect)
   -- register for an auto-modification, as a function of the
   -- width of the operand.


   procedure Auto_Modify (
      Operand : in     Operand_T;
      Width   : in     Width_T;
      Within  : in out Arithmetic.Assignment_Set_T)
   --
   -- If the Operand involves auto-modification of a register,
   -- adds the corresponding assignment Within the set, including
   -- also updates to the sub-cells (octet registers).
   --
   is
      use type Arithmetic.Effect_Ref;

      Pointer : Arithmetic.Variable_T;
      -- The pointer register, if Operand is Register_Indirect.

      New_Value : Arithmetic.Expr_Ref;
      -- The new value of the Pointer.

   begin

      if       Operand.Kind = Register_Indirect
      and then Operand.Auto_Mod /= None
      then

         Pointer := Processor.Cells.Register_Var(Operand.Pointer, Word);

         case Real_Mod_T (Operand.Auto_Mod) is

         when Post_Increment =>
            -- The Pointer is increased/advanced by one operand.

            New_Value := Pointer + Auto_Mod_Octets(Width);

         when Pre_Decrement =>
            -- The Pointer is decreased/retreated by one operand.

            New_Value := Pointer - Auto_Mod_Octets(Width);

         end case;

         Arithmetic.Add (
            To   => Within,
            More => Arithmetic.Set (
               Target => Pointer,
               Value  => New_Value));

         Update_Touched_Cells (
            Target  => Pointer,
            Value   => New_Value,
            Within  => Within);

      end if;

   end Auto_Modify;


   --
   ---   Decoding target instructions and building control-flow graphs
   --


   function "&" (
      Place : Flow.Step_Tag_T;
      Kind  : Processor.Step_Kind_T)
   return Flow.Step_Tag_T
   --
   -- The step-tag at the same Place but with a new Kind.
   --
   is
   begin

      return Flow.Transit (
         From => Place,
         To   => (
            Address => Place.State.Address,
            Kind    => Kind));

   end "&";


   function "+" (
      After  : Flow.Step_Tag_T;
      Length : Instruction_Length_T)
   return Flow.Step_Tag_T
   --
   -- The tag (address) of the next instruction After a given
   -- instruction with a given Length.
   --
   is
      use Processor;
   begin

      return Flow.Transit (
         From   => After,
         To     => (
            Kind    => Normal,
            Address => Shift (
               Base   => After.State.Address,
               Offset => Address_Offset_T (Length))));

   end "+";


   procedure Trace_Main_Step (Step : in Flow.Step_T)
   --
   -- Traces the new Step, if tracing chosen.
   -- This displays the Effect and Effort of the Step and also
   -- the sequential number (index) of the Step.
   --
   -- This operation is intended for tracing the principal (real)
   -- decoded steps.
   --
   is
   begin

      if Opt.Trace then

         Trace.Result (
            Effect => Flow.Effect (Step),
            Effort => Flow.Effort (Step));

         Trace.Step (Step);

      end if;

   end Trace_Main_Step;


   procedure Trace_Extra_Step (Step : in Flow.Step_T)
   --
   -- Traces the new Step, if tracing chosen.
   -- If tracing includes the effect, the Step is given its own trace
   -- line, including the step-address, the effect and  the effort,
   -- otherwise only the sequential number (index) of the Step is traced.
   --
   -- This operation is intended for tracing synthetic steps that
   -- are added after the principal decoded step.
   --
   is
   begin

      if Opt.Trace then

         if Opt.Trace_Effect then

            Trace.Done;
            -- Terminates the trace line for the preceding step.

            Trace.Step_Tag (Flow.Tag (Step));

            Trace.Result (
               Effect => Flow.Effect (Step),
               Effort => Flow.Effort (Step));

         end if;

         Trace.Step (Step);

      end if;

   end Trace_Extra_Step;


   function Calling_Protocol (Program : Programs.Program_T)
   return Calling.Protocol_Ref
   is
   begin

      case Format.Form (Programs.Processor_Info (Program).all) is

      when Format.COFF =>
         -- Assume it's the GCC compiler.

         return GCC.Protocol (Program);

      when Format.UBROF =>
         -- Assume its the IAR compiler.

         return IAR.Protocol (Program);

      when Format.SRec =>
         -- Assume its the GCC compiler.
         -- This is TBM, really.

         return GCC.Protocol (Program);

      end case;

   end Calling_Protocol;


   function Unresolved_Call_Effect (Program : Programs.Program_T)
   return Arithmetic.Effect_Ref
   --
   -- The arithmetic effect to be assumed for an unresolved dynamic
   -- call (that is, a call where the callee is unknown).
   --
   is
   begin

      case Format.Form (Programs.Processor_Info (Program).all) is

      when Format.COFF =>
         -- Assume it's the GCC compiler.

         return GCC.Unresolved_Call_Effect (Program);

      when Format.UBROF =>
         -- Assume its the IAR compiler.

         return IAR.Unresolved_Call_Effect (Program);

      when Format.SRec =>
         -- Assume it's the GCC compiler.
         -- This is TBM, really.

         return GCC.Unresolved_Call_Effect (Program);

      end case;

   end Unresolved_Call_Effect;


   procedure Veil_Flags (Within : in out Arithmetic.Assignment_Set_T)
   --
   -- Makes the Equal and Less flags unknown Within the effect.
   --
   is
   begin

      Arithmetic.Veil (
         Variables => (
            Processor.Cells.Equal_Flag,
            Processor.Cells.Less_Flag),
         Within    => Within);

   end Veil_Flags;


   Stack_Datum_Octets : constant Arithmetic.Expr_Ref :=
      Arithmetic.Const (
         Value  => Arithmetic.Word_T (Stack_Operand_Octets),
         Width  => Stack_Pointer_Bits,
         Signed => False);
   --
   Stack_Datum_Bits : constant := Stack_Operand_Bits;
   --
   -- Every stack datum (pusp/pop item) is a Word.


   New_Stack_Height :
      constant array (Stack_Kind_T) of Arithmetic.Expr_Ref := (
      Push => Processor.Cells.SH + Stack_Datum_Octets,
      Pop  => Processor.Cells.SH - Stack_Datum_Octets);
   --
   -- The expression for the new Stack Height after Push or Pop.


   Stack_Top_Operand : constant array (Stack_Kind_T) of Operand_T := (

      Push => (Kind         => Register_Indirect,
               Pointer      => H8_300.SP,
               Auto_Mod     => Pre_Decrement,
               Displacement => 0),

      Pop =>  (Kind         => Register_Indirect,
               Pointer      => H8_300.SP,
               Auto_Mod     => Post_Increment,
               Displacement => 0) );
   --
   -- The stack operands for Push or Pop.


   function Push_Return (To : Flow.Step_Tag_T)
   return Arithmetic.Effect_T
   --
   -- The effect of pushing the return address for a subprogram
   -- call (Jump_Subroutine or Branch_Subroutine), when the call
   -- returns To a given step-tag.
   --
   -- The effect includes a new Stack Pointer value and a new
   -- Stack Height value and also the actual return address on
   -- top of the stack.
   --
   is
      use Processor;

      Stack_Op : constant Operand_T := Stack_Top_Operand(Push);
      -- The place in the stack for the return address.

      Effect : Arithmetic.Assignment_Set_T (Max_Size => 7);
      -- The result.
      --
      --    Effect part                     Assignments
      --    -----------                     -----------
      --    return address into stack word    1
      --    octet parts of stack word         2
      --    new stack pointer                 1
      --    octet parts of stack pointer      2
      --    new stack height                  1
      --    -----------------------------------
      --    total                             7

   begin

      -- Store the return address into the stack:

      Assign (
         Target => Target (Stack_Op, Word),
         Value  => Arithmetic.Const (
            Value  => Arithmetic.Word_T (Flow.Prime_Address (To)),
            Width  => Stack_Datum_Bits,
            Signed => False),
         Within => Effect);

      -- Update Stack Pointer and its parts:

      Auto_Modify (
         Operand => Stack_Op,
         Width   => Word,
         Within  => Effect);

      -- Update Stack Height:

      Arithmetic.Add (
         To   => Effect,
         More => Arithmetic.Set (
            Target => Cells.SH,
            Value  => New_Stack_Height(Push)));

      return Arithmetic.To_Effect (Effect);

   end Push_Return;


   function Pop_Return return Arithmetic.Effect_Ref
   --
   -- The effect of popping the return address from the stack, as
   -- part of a Return_From_Subprogram.
   --
   -- The effect includes a new Stack Pointer value and a new
   -- Stack Height value. The popped value is not stored anywhere.
   --
   is
      use Processor;

      Effect : Arithmetic.Assignment_Set_T (Max_Size => 4);
      -- The result.
      --
      --    Effect part                     Assignments
      --    -----------                     -----------
      --    new stack pointer                 1
      --    octet parts of stack pointer      2
      --    new stack height                  1
      --    -----------------------------------
      --    total                             4

   begin

      -- Update Stack Pointer and its parts:

      Auto_Modify (
         Operand => Stack_Top_Operand(Pop),
         Width   => Word,
         Within  => Effect);

      -- Update Stack Height:

      Arithmetic.Add (
         To   => Effect,
         More => Arithmetic.Set (
            Target => Cells.SH,
            Value  => New_Stack_Height(Pop)));

      return Arithmetic.To_Effect_Ref (Effect);

   end Pop_Return;


   function Pop_Return_Exception return Arithmetic.Effect_Ref
   --
   -- The effect of popping the CCR and the return address from
   -- the stack, as part of a Return_From_Exception.
   --
   -- The effect includes a new Stack Pointer value, a new
   -- Stack Height value, and all flags veiled. The popped
   -- return address is not stored anywhere.
   --
   is
      use Processor;

      Effect : Arithmetic.Assignment_Set_T (Max_Size => 6);
      -- The result.
      --
      --    Effect part                     Assignments
      --    -----------                     -----------
      --    new stack pointer                 1
      --    octet parts of stack pointer      2
      --    new stack height                  1
      --    all flags veiled                  2
      --    -----------------------------------
      --    total                             6

   begin

      -- Update Stack Pointer and its parts and the stack height:

      Assign (
         Target => Cells.SP,
         Value  => Cells.SP + Stack_Datum_Octets + Stack_Datum_Octets,
         Within => Effect);

      -- Clobber the flags:

      Veil_Flags (Within => Effect);

      return Arithmetic.To_Effect_Ref (Effect);

   end Pop_Return_Exception;


   procedure Model_Call (
      Source : in Flow.Step_T;
      Target : in Processor.Code_Address_T;
      Length : in Instruction_Length_T;
      Caller : in Programs.Subprogram_T;
      Graph  : in Flow.Graph_T)
   --
   -- Models a subprogram call (Jump_Subroutine or Branch_Subroutine)
   -- to a Target subprogram by creating a call step after the (already
   -- created) Source step that models the call instruction itself.
   --
   -- Source
   --    The step created for the call instruction.
   -- Target
   --    The entry address of the callee.
   -- Length
   --    The length of the call instruction (at Source).
   -- Caller
   --    The caller subprogram, which contains the call instruction.
   -- Graph
   --    The flow-graph of the Caller, to be extended with the model
   --    of the call.
   --
   is
      use type Programs.Call_T;

      Call : Programs.Call_T;
      -- The new call.

   begin -- Model_Call

      Flow.Calls.Add_Call (
         To        => Graph,
         From      => Source,
         Caller    => Caller,
         Target    => Target,
         Time      => 0,
         Protocol  => Calling_Protocol (Programs.Program (Caller)),
         Call_Info => Processor.Program.Initial_Sub_Info,
         Step_Info => (Effort => Processor.No_Effort),
         Retur     => (Flow.Calls.To_Caller, Flow.Tag (Source) + Length),
         Giving    => Call);

      if Opt.Trace then

         if Call /= Programs.No_Call then
            -- A real (not integrated) call was created.

            Trace.Step (Programs.Step (Call));

            Trace.Remark ("call to " & Processor.Image (Target));

         else
            -- This call will be integrated into the Caller.

            Trace.Remark ("integrated call");

         end if;

      end if;

   end Model_Call;


   procedure Model_Two_Operand_Instruction (
      Operation : in     Two_Operand_Kind_T;
      Operands  : in     Two_Op_T;
      Length    : in     Instruction_Length_T;
      Tag       : in     Flow.Step_Tag_T;
      Program   : in     Programs.Program_T;
      Graph     : in     Flow.Graph_T;
      Giving    :    out Flow.Step_T)
   --
   -- Models a two-operand instruction with a given Operation and
   -- given Operands, by adding the corresponding step(s) at the given
   -- Tag to the Graph. The loose edge to the next instruction
   -- is also added here.
   --
   -- Operation
   --    The operation in question.
   -- Operands
   --    The two operands for the Operation.
   -- Length
   --    The Length of the instruction determines the number of
   --    Fetch cycles.
   -- Tag
   --    The step-tag for the instruction. Usually this is a
   --    Normal state, but it may also be of the Add_Addx kind to
   --    show that the effect of this Add_With_Carry Operation was
   --    already modeled as part of the preceding Add operation.
   -- Program
   --    Just for peeking at the next instruction (Add-Addx combo).
   -- Graph
   --    The flow-graph under construction for the subprogram that
   --    contains the instruction.
   -- Giving
   --    The new step that models the instruction, added to the Graph.
   --    This step has the given Tag and contains most of the effect
   --    and all of the effort of the instruction.
   --
   is
      use Processor;
      use type Arithmetic.Expr_Kind_T;

      Target : Arithmetic.Variable_T :=
         Decoder.Target (Operands.Destination, Operands.Width);
      -- The target operand (cell).
      -- The initial value is good except when
      --    o   an Add is combined with a following Add_With_Carry, or
      --    o   the operation is a multiplication.

      Source : Arithmetic.Variable_T;
      -- The source operand (cell or constant).

      Effect : Arithmetic.Assignment_Set_T (Max_Size => 10);
      -- The collected effect of the instruction.
      -- The Max_Size includes assignments to:
      --       1  one main target (destination) cell
      --       2  at most two cells "touched" by the main target
      --       2  at most two flag cells
      --       1  possibly an auto-modified pointer cell
      --       2  at most two sub-cells of the auto-modified pointer
      --       1  possibly the Stack Height cell
      --       1  possible range constraint.
      -- Total up to 10 assignments, although there is probably
      -- no instruction that is complex enough to reach this.

      Effort : Processor.Effort_T :=
         Timing.Time_To_Fetch (Tag, Length);
      -- The effort of the instruction. More effort added below
      -- for operand access and some special instructions.

      Result : Arithmetic.Expr_Ref;
      -- The result of the operation, in some cases where this is
      -- needed in multiple places.

      Mul_Target : Arithmetic.Variable_T;
      -- The real target of a Multiply operation = the word
      -- register that contains the octet register Target.

      Next_Step_Tag : Flow.Step_Tag_T := (Tag + Length) & Normal;
      -- The step-tag of the next step (instruction) to follow this
      -- two-operand instruction. The initial value is usually good,
      -- but there are some special cases:
      --
      -- > When this Move Operation is followed by a jump-via-register
      --   and the two are found to be a jump-via-table combination,
      --   then Next_Step_Tag.State.Kind is set to Jump_Via_Table to show
      --   that the later decoding of the jump-via-register should
      --   create the corresponding kind of dynamic edge.
      --
      -- > When this Add Operation is followed by an Add_With_Carry
      --   and the two can be combined into a 16-bit addition,
      --   Next_Step_Tag.State.Kind is set to Add_Addx to show that
      --   the later decoding of the Add_With_Carry should not generate
      --   any arithmetic effect.
      --
      -- > When this Compare Byte Operation is followed by a Subtract
      --   With Carry and the two can be combined into a 16-bit comparison,
      --   Next_Step_Tag.State.Kind is set to Cmd_Subx to show that
      --   the later decoding of the Subtract_With_Carry should not
      --   generate any arithmetic effect on the flags.
      --
      -- > When this Subtract Byte Operation is followed by a Subtract
      --   With Carry and the two can be combined into a 16-bit
      --   subtraction, Next_Step_Tag.State.Kind is set to Sub_Subx to
      --   show that the later decoding of the Subtract_With_Carry should
      --   not generate any arithmetic effect.

      Next_Edge_Info : Processor.Loose_Edge_Info_T :=
         Processor.No_Loose_Edge_Info;
      -- The context information for the loose edge to be created
      -- from this step to the next step. The initial value is usually
      -- good and is replaced only for the Jump_Via_Table combination.


      procedure Set_Normal_Source
      --
      -- Defines the Source expression in the normal way.
      -- The abnormal ways hold for Jump_Via_Table and Add_Addx.
      --
      is
      begin

         Source := Decoder.Source (
            Operand => Operands.Source,
            Width   => Operands.Width);

      end Set_Normal_Source;


      function Next_Instruction return Instruction_T
      --
      -- Fetches and decodes the next one-word instruction.
      -- Raises Invalid_Instruction if the next word could not be
      -- fetched or if it is not a valid one-word instruction.
      --
      is

         Next_Word : Word_List_T (0 .. 0);
         Last_Word : Integer;
         -- The next instruction word.

         Next_Instr  : Instruction_T;
         Next_Length : Instruction_Length_T;
         -- The next instruction and its length.

      begin

         -- Fetch the next instruction word:

         Format.Fetch_Code (
            From    => Programs.Processor_Info (Program).all,
            Address => Next_Step_Tag.State.Address,
            Into    => Next_Word,
            Last    => Last_Word);

         if Last_Word /= Next_Word'Last then
            -- The next word could not be fetched.

            raise Invalid_Instruction;

         end if;

         -- Decode the instruction into abstract form:

         H8_300.Decode (
            From        => Next_Word,
            Instruction => Next_Instr,
            Length      => Next_Length);
         --
         -- This can also raise Invalid_Instruction.

         return Next_Instr;

      end Next_Instruction;


      procedure Store (Value : in Arithmetic.Expr_Ref)
      --
      -- Adds the assignment Target := Value to the Effect, plus
      -- the assignments to update sub-cells or super-cells, some
      -- of which may become the Compose effect.
      --
      is
      begin

         Assign (
            Target  => Target,
            Value   => Value,
            Within  => Effect);

      end Store;


      procedure Set (
         Flag  : in Processor.Flag_Kind_T;
         Value : in Arithmetic.Condition_T)
      --
      -- Adds the assignment Flag := Value, Within the effect.
      --
      is
      begin

         Arithmetic.Add (
            To   => Effect,
            More => Arithmetic.Set (
               Target => Cells.Flag(Flag),
               Value  => Value));

      end Set;


      procedure Set_Equal_If_Zero (Value : Arithmetic.Expr_Ref)
      --
      -- Sets the Equal flag to Value = Zero, with simplification if
      -- the Value is a static constant.
      --
      is
      begin

         Set (Flag => Equal, Value => EqZero (Value));

      end Set_Equal_If_Zero;


      procedure Store_And_Equal (Value : in Arithmetic.Expr_Ref)
      --
      -- Stores the Value and sets the Equal flag to Value = Zero.
      --
      is
      begin

         Store (Value => Value);

         Set_Equal_If_Zero (Value => Value);

      end Store_And_Equal;


      procedure Whole_Vs_Parts (Register : in Register_Number_T)
      --
      -- Adds a range constraint (an equality, in fact) that defines
      -- the relationship between the whole 16-bit Register and its
      -- 8-bit parts, to the Effect.
      --
      is
      begin

         Arithmetic.Add (
            To   => Effect,
            More => Arithmetic.Range_Pre (
               Target => Cells.Register_Var    (Register, Word),
               Min    => Cells.Word_From_Octets(Register),
               Max    => Cells.Word_From_Octets(Register)));

      end Whole_Vs_Parts;


      procedure Try_Jump_Via_Table
      --
      -- Tries to match the Move Word from Register_Indirect Operation
      -- with a matching Jump Register_Indirect in the next instruction
      -- so that the combination is a jump-via-address-table. On success,
      -- adds the necessary assignments of Jump_Index etc. to the Effect,
      -- sets Next_Step_Tag.State.Kind to Jump_Via_Table and stores the
      -- base address of the address-table in Next_Edge_Info.
      --
      -- Precondition:
      --    Operation                 = Move               and
      --    Operands.Width            = Word               and
      --    Operands.Source.Kind      = Register_Indirect  and
      --    Operands.Destination.Kind = Register.
      --
      -- A Jump matches this Move if the Jump is Register_Indirect and
      -- its Target.Pointer is the Destination of the Move, as in:
      --
      --      mov.w @(base,Ri), Rn
      --      jmp @Rn
      --
      -- We can have i = n, that is, Ri can be the same register as Rn.
      -- The Move instruction can have auto-increment or auto-decrement.
      --
      -- A failed match is shown by Next_Step_Tag.State.Kind = Normal.
      --
      is

         Next_Instr : Instruction_T;
         -- The next instruction and its length.

         Jump_To : H8_300.Target_T;
         -- The operand of Next_Instr when it is Jump.

         Table_Base : Processor.Code_Address_T;
         -- The base address of the address-table.

         Index_Reg : Arithmetic.Variable_T;
         -- The register used to index the address table (Operands.Source).

         Index : Arithmetic.Expr_Ref;
         -- The table index which is Index_Reg possibly modified
         -- by pre-decrement.

      begin

         Next_Instr := Next_Instruction;

         if Next_Instr.Kind = Jump then
            -- Hey, we have a chance!

            Jump_To := Next_Instr.Target;

            if       Jump_To.Kind    = Register_Indirect
            and then Jump_To.Pointer = Operands.Destination.Number
            then
               -- Match!

               Table_Base := Code_Address_T (Operands.Source.Displacement);

               Output.Warning (
                    "Assuming constant jump-address table at "
                  & Processor.Image (Table_Base));

               if Opt.Trace then

                  Trace.Remark ("reads jump table");

               end if;

               -- Assign the table index to Jump_Index:

               Index_Reg := Cells.Register_Var(Operands.Source.Pointer, Word);

               if Operands.Source.Auto_Mod = Pre_Decrement then
                  -- The real index is a little less.

                  Index := Index_Reg - Auto_Mod_Octets(Word);

               else
                  -- The real index is just the register value.

                  Index := Index_Reg;

               end if;

               Arithmetic.Add (
                  To   => Effect,
                  More => Arithmetic.Set (
                     Target => Cells.Jump_Index_Var,
                     Value  => Index));

               -- Declare the index register to be non-negative:

               Arithmetic.Add (
                  To   => Effect,
                  More => Arithmetic.Range_Pre (
                     Target => Index_Reg,
                     Min    => Zero_Word,
                     Max    => Arithmetic.Unknown));

               -- Veil the register holding the jump address, to show
               -- that it is assigned, and also veil the Equal flag for
               -- the same reason:

               Store_And_Equal (Value => Arithmetic.Unknown);

               -- Define the edge to the next instruction (the jump-
               -- via-register) as part of a Jump_Via_Table:

               Next_Step_Tag.State.Kind := Jump_Via_Table;

               Next_Edge_Info := (
                  Kind => Jump_Via_Table,
                  Base => Table_Base);

            end if;

         end if;

      exception

      when Invalid_Instruction =>
         -- The next instruction is not a valid one-word instruction.
         -- We do nothing; if the instruction really is invalid, it
         -- will be reported as such when it is decoded later.

         null;

      end Try_Jump_Via_Table;


      function Imm_Op_RnL return Boolean
      --
      -- Whether the Operands are of Octet width, with an Immediate
      -- source operand and the low octet of a register as the
      -- destination operand. Such an operation may perhaps be combined
      -- with the next instruction if the next instruction extends the
      -- same or similar operation to the high octets.
      --
      is
      begin

         return (    Operands.Width            = Octet
            and      Operands.Source.Kind      = Immediate
            and      Operands.Destination.Kind = Register)
            and then Operands.Destination.Part = Low_Byte;

      end Imm_Op_RnL;


      procedure Try_Pairing (
         High_Kind : in Two_Operand_Kind_T;
         Comb_Kind : in Processor.Step_Kind_T)
      --
      -- Tries to match the current Operation on the low octet of a
      -- register with a matching operation on the high octet of this
      -- register in the next instruction. On success, defines the combined
      -- Target and Source and sets Next_Step_Tag.State.Kind to Comb_Kind.
      --
      -- Precondition: the Operation is a pairable operation and the
      -- Operands are of type Imm_Op_RnL.
      --
      -- The next instruction matches the current Operation if it is
      -- of the High_Kind, its Width is also Octet, its Source is also
      -- Immediate and its Destination is the High_Byte of the same
      -- Register as Operands.Destination.
      --
      -- A failed match is shown by Next_Step_Tag.State.Kind = Normal.
      --
      is

         Next_Instr : Instruction_T;
         -- The next instruction and its length.

         High_Op : Two_Op_T;
         -- The operands of Next_Instr when it is High_Kind.

      begin

         Next_Instr := Next_Instruction;

         if Next_Instr.Kind = High_Kind then
            -- Hey, we have a chance!

            High_Op :=  Next_Instr.Two;

            if  High_Op.Source.Kind = Immediate
            and High_Op.Width       = Octet
            and High_Op.Destination = (
                  Kind   => Register,
                  Number => Operands.Destination.Number,
                  Part   => High_Byte)
            then
               -- Match!

               Target := Cells.Register_Var(
                  Operands.Destination.Number,
                  Word);

               Source := Decoder.Source (
                  Operand => (
                     Kind  => Immediate,
                     Value =>
                        Shift_Left (
                           Value  => High_Op.Source.Value,
                           Amount => Octet_Bits)
                        or Operands.Source.Value),
                  Width  => Word);

               Next_Step_Tag.State.Kind := Comb_Kind;

               if Opt.Trace then

                  Trace.Remark ("combined with next");

               end if;

            end if;

         end if;

      exception

      when Invalid_Instruction =>
         -- The next instruction is not a valid one-word instruction.
         -- We do nothing; if the instruction really is invalid, it
         -- will be reported as such when it is decoded later.

         null;

      end Try_Pairing;


   begin  -- Model_Two_Operand_Instruction

      -- Construct the Effect:

      case Operation is

      when Move
         | Move_From_Peri
         | Move_To_Peri   =>

         if  Operation                 = Move
         and Operands.Width            = Word
         and Operands.Source.Kind      = Register_Indirect
         and Operands.Destination.Kind = Register
         then
            -- Perhaps this Move reads an address table in preparation
            -- for a jump-via-register.

            Try_Jump_Via_Table;

         end if;

         if Next_Step_Tag.State.Kind = Normal then
            -- Not part of a Jump_Via_Table.

            Set_Normal_Source;

            if Operands.Destination /= Operands.Source then
               -- A real Move (or a Move_From/To_Peri).

               Store_And_Equal (Value => Source);

            else
               -- A dummy move (Rd := Rd), to test the operand and set flags.
               -- (Note that there is no real Move instruction where both
               -- operands would be register-indirect with sequential
               -- auto-modification -- such an instruction could actually
               -- move data from one cell to another although the Source and
               -- Destination components would be equal.)

               Set_Equal_If_Zero (Value => Source);

               if Operands.Source.Kind = Register then
                  -- This may be a good point to remind the analysis
                  -- about the relationship between the whole register
                  -- and its parts.

                  Whole_Vs_Parts (Register => Operands.Source.Number);

               end if;

            end if;

         end if;

      when Add =>

         if Imm_Op_RnL then
            -- An "add.b #xx,RnL" that may perhaps be combined with
            -- a following "addx #yy,RnH".

            Try_Pairing (
               High_Kind => Add_With_Carry,
               Comb_Kind => Processor.Add_Addx);

         end if;

         if Next_Step_Tag.State.Kind = Normal then
            -- Not combined with a following Add_With_Carry.

            Set_Normal_Source;

         end if;

         if Arithmetic.Is_Negative_Constant (Source) then
            -- Help analysis by changing addition to subtraction.

            if Arithmetic.Opt.Warn_Signing_Literal then

               Output.Warning (
                    "Immediate operand "
                  & Arithmetic.Image (Source)
                  & " taken as signed = "
                  & Arithmetic.Image (Arithmetic.Signed_Value (Source)));

            end if;

            Result := Target - Arithmetic.Const (
               Value  => - Arithmetic.Signed_Value (Source),
               Width  => Arithmetic.Width_Of (Source),
               Signed => True);

            Store (Value => Result);

            -- Adding a negative number can yield a zero or
            -- negative value.

            Set_Equal_If_Zero (Result);

            Set (Flag => Less, Value => Arithmetic.Signw (Result));

         else
            -- Addition remains addition. If the Source operand
            -- is negative, too bad.

            Result := Target + Source;

            Store (Value => Result);

            -- Adding two unsigned values can never, in our view,
            -- yield zero or a negative value. Therefore, the CCR
            -- flags no longer match the Equal and Less values.

            Veil_Flags (Within => Effect);

         end if;

      when Add_With_Carry =>

         case Tag.State.Kind is

         when Normal =>
            -- This is a "stand-alone" addition with carry, so we
            -- will not model the arithmetic effect.

            Store (Value => Arithmetic.Unknown);

            Veil_Flags (Within => Effect);

         when Add_Addx =>
            -- The effect of this instruction was already modelled in
            -- the preceding step, which is an Add instruction.

            if Opt.Trace then

               Trace.Remark ("combined with preceding add");

            end if;

         when others =>

            Output.Fault (
               Location => "Decoder.Model_Two_Operand_Instruction",
               Text     =>
                    "Tag.State.Kind ="
                  & Step_Kind_T'Image (Tag.State.Kind));

         end case;

      when Add_With_Sign_Ext =>

         Set_Normal_Source;

         Store (Value => Target + Source);

      when Subtract =>

         if Operands.Destination = Operands.Source then
            -- Target - Target = zero.
            --
            -- Note that there is no real Subtract instruction where
            -- both operands would be register-indirect with sequential
            -- auto-modification -- such an instruction could actually
            -- subtract two different cells although the Source and
            -- Destination components would be equal in Operands.

            Result := Zero (Operands.Width);

            Store (Value => Result);

            -- The Equal flag is on, Less is off:

            Set (Flag => Equal, Value => Arithmetic.One_Bit);
            Set (Flag => Less , Value => Arithmetic.Zero_Bit);

         else
            -- A real subtraction

            if Imm_Op_RnL then
               -- An "sub.b #xx,RnL" that may perhaps be combined with
               -- a following "subx #yy,RnH".

               Try_Pairing (
                  High_Kind => Subtract_With_Carry,
                  Comb_Kind => Processor.Sub_Subx);

            end if;

            if Next_Step_Tag.State.Kind = Normal then
               -- Not combined with a following Subtract_With_Carry.

               Set_Normal_Source;

            end if;

            Store (Value => Target - Source);

            Set (Flag => Equal, Value => Target = Source);

            Set (Flag => Less , Value  => Arithmetic.Lts (Target, Source));
            --
            -- "Lts" TBC when modelling flags better.

         end if;

      when Subtract_With_Carry =>

         case Tag.State.Kind is

         when Normal =>
            -- This is a "stand-alone" subtraction with carry, so we
            -- will not model the result values.

            Store (Value => Arithmetic.Unknown);

            Veil_Flags (Within => Effect);

         when Cmp_Subx =>
            -- The flag values were already modelled in the preceding
            -- step, which is a Compare instruction. The effect on
            -- the high octet of the 16-bit register is modelled here.

            Store (Value => Arithmetic.Unknown);

            if Opt.Trace then

               Trace.Remark ("combined with preceding cmp");

            end if;

         when Sub_Subx =>
            -- The arithmetic effect was already modelled in the preceding
            -- step, which is a Subtract instruction.

            if Opt.Trace then

               Trace.Remark ("combined with preceding sub");

            end if;

         when others =>

            Output.Fault (
               Location => "Decoder.Model_Two_Operand_Instruction",
               Text     =>
                    "Tag.State.Kind ="
                  & Step_Kind_T'Image (Tag.State.Kind));

         end case;

      when Subtract_With_Sign_Ext =>

         Set_Normal_Source;

         Store (Value => Target - Source);

      when Compare =>
         -- The Target operand is not assigned, but the flags
         -- are defined as for a subtraction Target - Source.

         if Imm_Op_RnL then
            -- A "cmp.b #xx,RnL" that may perhaps be combined with
            -- a following "subx #yy,RnH".

            Try_Pairing (
               High_Kind => Subtract_With_Carry,
               Comb_Kind => Processor.Cmp_Subx);

         end if;

         if Next_Step_Tag.State.Kind = Normal then
            -- Not combined with a following Subtract_With_Carry.

            Set_Normal_Source;

         end if;

         Set (Flag => Equal, Value => Target = Source);

         Set (Flag => Less , Value  => Arithmetic.Lts (Target, Source));
         --
         -- "Lts" TBC when modelling flags better.

      when Multiply =>
         -- The instruction is represented with Width = Octet, but
         -- the result is a 16-bit value and is stored in the Whole
         -- Target register.

         Set_Normal_Source;

         Mul_Target := Cells.Register_Var(
            Operands.Destination.Number,
            H8_300.Word);

         Assign (
            Target  => Mul_Target,
            Value   => Arithmetic.Mulu (
               L => Arithmetic.Extz (
                  Expr => Cells.Register_Var(
                     Operands.Destination.Number,
                     H8_300.Low_Byte),
                  Width => H8_300.Word_Bits),
               R => Arithmetic.Extz (
                  Expr  => Source,
                  Width => H8_300.Word_Bits)),
            Within  => Effect);
         --
         -- The expression Target * Source uses 8-bit registers.

      when Divide =>
         -- Division is not modelled; the result is unknown.

         Store (Value => Arithmetic.Unknown);

         Veil_Flags (Within => Effect);
         -- The Less flag (Carry flag) is really unchanged but is
         -- unlikely to be significant for loop counting.

      when Bitwise_And =>
         -- This instruction is available only for octet data.

         Set_Normal_Source;

         Store_And_Equal (Value => Arithmetic.Andw (Target, Source));

      when Bitwise_Or =>
         -- This instruction is available only for octet data.

         Set_Normal_Source;

         Store_And_Equal (Value => Arithmetic.Orw (Target, Source));

         -- TBD if better: Equal := Target = Zero and Source = Zero.

      when Bitwise_Xor =>
         -- This instruction is available only for octet data.

         Set_Normal_Source;

         Store (Value => Arithmetic.Xorw (Target, Source));

         Set (Flag => Equal, Value => Target = Source);

      end case;

      -- Add pointer auto-modification to the Effect:

      Auto_Modify (
         Operand => Operands.Destination,
         Width   => Operands.Width,
         Within  => Effect);

      Auto_Modify (
         Operand => Operands.Source,
         Width   => Operands.Width,
         Within  => Effect);

      -- Add operand access to the Effort:

      if Operation = Move_From_Peri
      or Operation = Move_To_Peri
      then
         -- The data access time is 9 .. 16 cycles depending on
         -- E-clock phase. We assume the worst case.

         Effort := Effort + 16;

      else
         -- The operand access time is fixed.

         Effort := Effort
            + Timing.Time_To_Read  (Operands.Source     , Operands.Width)
            + Timing.Time_To_Write (Operands.Destination, Operands.Width);

      end if;

      -- Add extra cycles for special instructions:

      if Operation = Multiply
      or Operation = Divide
      then
         -- Lots of Internal cycles needed.

         Effort := Effort + 12;

      end if;

      -- Create the steps:

      Flow.Add_Step (
         To     => Graph,
         Tag    => Tag,
         Effect => Arithmetic.To_Effect_Ref (Effect),
         Info   => (Effort => Effort),
         Giving => Giving);

      Trace_Main_Step (Giving);

      -- Add loose edge to next instruction:

      Flow.Add_Edge (
         To      => Graph,
         Source  => Giving,
         Time    => 0,
         Target  => Next_Step_Tag,
         Info    => Next_Edge_Info);

   end Model_Two_Operand_Instruction;


   procedure Model_One_Operand_Instruction (
      Operation : in     One_Operand_Kind_T;
      Operands  : in     One_Op_T;
      Length    : in     Instruction_Length_T;
      Tag       : in     Flow.Step_Tag_T;
      Graph     : in     Flow.Graph_T;
      Giving    :    out Flow.Step_T)
   --
   -- Models a one-operand instruction with a given Operation and
   -- given Operand, by adding the corresponding step at the given
   -- Tag to the Graph.
   --
   -- Operation
   --    The operation in question.
   -- Operands
   --    The single operand for the Operation.
   -- Length
   --    The Length of the instruction determines the number of
   --    Fetch cycles.
   -- Tag
   --    The step-tag for the instruction (a Normal state).
   -- Graph
   --    The flow-graph under construction for the subprogram that
   --    contains the instruction.
   -- Giving
   --    The new step that models the instruction, added to the Graph.
   --    This step has the given Tag.
   --
   is
      use Processor;

      Operand : constant Arithmetic.Variable_T :=
         Processor.Cells.Register_Var(Operands.Register, Operands.Part);
      -- The single operand register, which may act both as source
      -- operand and destination operand.

      Effect : Arithmetic.Assignment_Set_T (Max_Size => 5);
      -- The collected effect of the instruction.
      -- The Max_Size includes assignments to:
      --       1  one main target (destination) cell
      --       1  at most one super-cell "touched" by the main target
      --       2  at most two flag cells
      --       0  no auto-modified pointer cell
      --       0  no sub-cells of the auto-modified pointer
      --       1  possibly the Stack Height cell.
      -- Total up to 5 assignments.

      Effort : constant Processor.Effort_T :=
         Timing.Time_To_Fetch (Tag, Length);
      -- The effort of the instruction. For One-operand instructions, the
      -- number of cycles only depends on the Time_To_Fetch.


      procedure Set (
         Flag  : in Processor.Flag_Kind_T;
         Value : in Arithmetic.Condition_T)
      --
      -- Adds the assignment Flag := Value, Within the effect.
      --
      is
      begin

         Arithmetic.Add (
            To   => Effect,
            More => Arithmetic.Set (
               Target => Cells.Flag(Flag),
               Value  => Value));

      end Set;


   begin  -- Model_One_Operand_Instruction

      -- Construct the Effect:

      case Operation is

      when Increment =>

         Arithmetic.Add (
            To   => Effect,
            More => Arithmetic.Set (
               Target => Operand,
               Value  => Operand + One_Octet));

         -- The Equal flag is set properly, assuming at most one
         -- wrap-around from 255 to 0:

         Set (Flag => Equal, Value => Operand = Max_Octet);

      when Decrement =>

         Arithmetic.Add (
            To   => Effect,
            More => Arithmetic.Set (
               Target => Operand,
               Value  => Operand - One_Octet));

         -- The Equal flag is set properly, assuming at most one
         -- transition from 1 to 0:

         Set (Flag => Equal, Value => Operand = One_Octet);

      when Negate =>
         -- The result is opaque (since we interpret all operands
         -- as unsigned) but some flags are set.

         Arithmetic.Veil (
            Variables => (1 => Operand),
            Within    => Effect);

         Set (Flag => Equal, Value => EqZero (Operand));

         Set (Flag => Less , Value => Arithmetic.Gts (Operand, Zero_Octet));

      when Bitwise_Not =>
         -- The result and the Equal flag are unknown.

         Arithmetic.Veil (
            Variables => (Operand, Cells.Equal_Flag),
            Within    => Effect);

      when Shift_Arith_Left
         | Shift_Arith_Right
         | Shift_Logical_Left
         | Shift_Logical_Right
         | Rotate_Left
         | Rotate_Left_Via_Carry
         | Rotate_Right
         | Rotate_Right_Via_Carry
         | Decimal_Adjust =>
         -- Result and all flags are unknown.

         Arithmetic.Veil (
            Variables => (Operand, Cells.Equal_Flag, Cells.Less_Flag),
            Within    => Effect);

      when Decimal_Adjust_Subtract =>
         -- The result and the Equal flag are unknown.

         Arithmetic.Veil (
            Variables => (Operand, Cells.Equal_Flag),
            Within    => Effect);

      end case;

      Update_Touched_Cells (
         Target  => Operand,
         Value   => Arithmetic.Unknown,
         Within  => Effect);

      -- The Operand is always an internal processor register, so:
      -- > there is no pointer auto-modification for the Effect, and
      -- > there is no operand-access time for the Effort.

      -- Create the step:

      Flow.Add_Step (
         To     => Graph,
         Tag    => Tag,
         Effect => Arithmetic.To_Effect_Ref (Effect),
         Info   => (Effort => Effort),
         Giving => Giving);

   end Model_One_Operand_Instruction;


   procedure Model_Stack_Instruction (
      Operation : in     Stack_Kind_T;
      Operands  : in     Stack_Op_T;
      Length    : in     Instruction_Length_T;
      Tag       : in     Flow.Step_Tag_T;
      Graph     : in     Flow.Graph_T;
      Giving    :    out Flow.Step_T)
   --
   -- Models a stack-kind instruction with a given Operation and
   -- given Operand, by adding the corresponding step at the given
   -- Tag to the Graph and returning this step in Giving.
   --
   -- The Length of the instruction is also given because it determines
   -- the number of Fetch cycles.
   --
   is
      use Processor;

      Effect : Arithmetic.Assignment_Set_T (Max_Size => 8);
      -- The collected effect of the instruction.
      -- The Max_Size includes assignments to:
      --       1  one main target (destination) cell
      --       2  at most two sub-cells "touched" by the main target
      --       1  one flag cell
      --       1  one auto-modified pointer cell (SP)
      --       2  two sub-cells of the auto-modified pointer
      --       1  the Stack Height cell.
      -- Total up to 8 assignments.

      Effort : Processor.Effort_T := (
         Timing.Time_To_Fetch (Tag, Length));
      -- The effort for the instruction. The initial value
      -- accounts for instruction fetch; more may be added below.

      Stack_Operand : constant Operand_T := Stack_Top_Operand(Operation);
      -- The stack operand (target for Push, source for Pop).

      Register : constant Arithmetic.Variable_T :=
         Processor.Cells.Register_Var(Operands.Register, Word);
      -- The register operand.

      Target : Arithmetic.Variable_T;
      -- The destination variable.

      Value : Arithmetic.Variable_T;
      -- The value that is pushed or popped and thus defines the
      -- Equal (Z) flag.

   begin  -- Model_Stack_Instruction

      -- Construct the main Effect:

      case Operation is

      when Push =>

         Target := Decoder.Target (Stack_Operand, Word);
         Value  := Register;
         Effort := Effort + Timing.Time_To_Push;

      when Pop =>

         Target := Register;
         Value  := Source (Stack_Operand, Word);
         Effort := Effort + Timing.Time_To_Pop;

      end case;

      -- Assign Target := Value:

      Assign (
         Target  => Target,
         Value   => Value,
         Within  => Effect);

      -- Update the Equal flag (Z):

      Arithmetic.Add (
         To   => Effect,
         More => Arithmetic.Set (
            Target => Cells.Equal_Flag,
            Value  => EqZero (Value)));

      -- Update the stack pointer and local stack height:

      Auto_Modify (
         Operand => Stack_Operand,
         Width   => Word,
         Within  => Effect);

      Arithmetic.Add (
         To   => Effect,
         More => Arithmetic.Set (
            Target => Processor.Cells.SH,
            Value  => New_Stack_Height(Operation)));

      -- Create the step:

      Flow.Add_Step (
         To     => Graph,
         Tag    => Tag,
         Effect => Arithmetic.To_Effect_Ref (Effect),
         Info   => (Effort => Effort),
         Giving => Giving);

   end Model_Stack_Instruction;


   function CCR_Transfer_Effect (
      Kind    : in CCR_Transfer_Kind_T;
      Operand : in CCR_Transfer_Op_T)
   return Arithmetic.Effect_T
   --
   -- The arithmetic effect of an instruction that transfers the CCR
   -- value to a register or vice versa.
   --
   is
      use Processor;

      Effect : Arithmetic.Assignment_Set_T (Max_Size => 2);
      -- For register-to-CCR we can affect two flags.
      -- For CCR-to-register we can affect one octet register and
      -- its containing word register.

   begin

      case Kind is

      when Load_CCR =>
         -- The flags are clobbered.

         Arithmetic.Veil (
            Variables => (Cells.Equal_Flag, Cells.Less_Flag),
            Within    => Effect);

      when Store_CCR =>
         -- The destination register is clobbered, as is its
         -- containing word register.

         Assign (
            Target  => Cells.Register_Var(
               Operand.Register,
               Operand.Part),
            Value   => Arithmetic.Unknown,
            Within  => Effect);

     end case;

     return Arithmetic.To_Effect (Effect);

   end CCR_Transfer_Effect;


   procedure Update_Flag (
      Flag      : in     Arithmetic.Variable_T;
      Operation : in     CCR_Update_Kind_T;
      Imm_Value : in     Bit_Value_T;
      Effect    : in out Arithmetic.Assignment_Set_T)
   --
   -- Models the effect on one CCR flag of an instruction that updates
   -- the CCR by applying a logical Operation on the CCR and an immediate
   -- value. If this Operation changes the flag, we add a corresponding
   -- assignment to the Effect.
   --
   -- Flag
   --    The flag cell as a variable, to be one of the operands of
   --    the Operation and the Target of the assignment.
   -- Operation
   --    The operation itself. The update is
   --      Flag := Operation (Flag, Imm_Value)
   -- Imm_Value
   --    The bit, from the immediate operand, that acts on this Flag.
   --
   -- If the combination of Operation and Imm_Value means that there
   -- is no change in the value of the Flag, no assignment is added
   -- to the Effect.
   --
   is
      use Arithmetic;

      Imm : Expr_Ref;
      -- The Imm_Value as an expression.

   begin

      -- Pick Zero or One for the Imm expression:

      case Imm_Value is
      when 0 => Imm := Zero_Bit;
      when 1 => Imm := One_Bit ;
      end case;

      -- An alternative way to define Imm would be
      --    Imm := Const (Arithmetic.Value_T (Imm_Value));
      -- but this would construct a new Expr_T element on the
      -- heap.

      -- Handle the Operations:

      case Operation is

      when Set_CCR =>
         -- The new value comes from the immediate operand. The old
         -- value of the flag has no effect.

         Add (
            To   => Effect,
            More => Set (Target => Flag, Value => Imm));

      when And_CCR =>
         -- Logical conjunction.

         if Imm_Value = 0 then
            -- The flag becomes zero, whatever its old value.

            Add (Effect, Set (Flag, Zero_Bit));

         -- else
         --    The flag is not changed, so we add no assignment.

         end if;

      when Or_CCR =>
         -- Logical disjunction.

         if Imm_Value = 1 then
            -- The flag becomes one, whatever its old value.

            Add (Effect, Set (Flag, One_Bit));

         -- else
         --    The flag is not changed, so we add no assignment.

         end if;

      when Xor_CCR =>
         -- Logical exclusive or.

         if Imm_Value = 1 then
            -- The flag is inverted.

            Add (Effect, Set (Flag, Notw (Flag)));

         -- else
         --    The flag is not changed, so we add no assignment.

         end if;

      end case;

   end Update_Flag;


   function CCR_Update_Effect (
      Kind  : CCR_Update_Kind_T;
      Value : Octet_T)
   return Arithmetic.Effect_T
   --
   -- The arithmetic effect, on the CCR, of an instruction that updates
   -- the CCR using a logical bit-wise operation with an immediate 8-bit
   -- value.
   --
   is

      Effect : Arithmetic.Assignment_Set_T (Max_Size => 8);
      -- To build the effect on the flags in the CCR.
      -- Although the CCR is 8 bits, the arithmetic model only tracks
      -- three flags (C, N, Z) so there are at most three assignments.
      -- However, we set Max_Size to 8 for robustness against future
      -- extension of the flag tracking.

   begin

      -- Update C = Less and Z = Equal flags:

      Update_Flag (
         Flag      => Less_Flag,
         Operation => Kind,
         Imm_Value => Bit (Number => Flag_T'Pos (H8_300.C), Within => Value),
         Effect    => Effect);

      Update_Flag (
         Flag      => Equal_Flag,
         Operation => Kind,
         Imm_Value => Bit (Number => Flag_T'Pos (H8_300.Z), Within => Value),
         Effect    => Effect);

      -- Return the collected effect:

      return Arithmetic.To_Effect (Effect);

   end CCR_Update_Effect;


   procedure Model_Bit_Instruction (
      Operation : in     Bit_Kind_T;
      Operands  : in     Bit_Op_T;
      Length    : in     Instruction_Length_T;
      Tag       : in     Flow.Step_Tag_T;
      Graph     : in     Flow.Graph_T;
      Giving    :    out Flow.Step_T)
   --
   -- Models a bit-instruction with a given Operation and
   -- given Operands, by adding the corresponding step at the given
   -- Tag to the Graph and returning step in Giving.
   --
   -- The Length of the instruction is also given because it determines
   -- the number of Fetch cycles.
   --
   is
      use Processor;

      Host : constant Arithmetic.Variable_T := Target (Operands.Host, Octet);
      -- The host octet as a target (destination) for a bit assignment.

      Effect : Arithmetic.Assignment_Set_T (Max_Size => 4);
      -- The collected effect of the instruction.
      -- The Max_Size includes assignments to:
      --       1  one main target (destination) cell
      --       1  at most one super-cell "touched" by the main target
      --       1  at most one flag cell
      --       0  no auto-modified pointer cell
      --       0  no sub-cells of the auto-modified pointer
      --       1  possibly the Stack Height cell.
      -- Total up to 4 assignments, but no instruction reaches this
      -- limit (main target is exclusive with flag target).

      Effort : Processor.Effort_T := (
           Timing.Time_To_Fetch (Tag, Length))
         + Timing.Time_To_Read  (Operands.Host, Octet);
      --
      -- The effort of the instruction.
      -- The initial value corresponds to an instruction that stores
      -- the result in a condition flag. For instructions that store
      -- the result in a register or memory, the time-to-write is
      -- added below as necessary.

   begin  -- Model_Bit_Instruction

      -- There are three groups of bit instructions, from the point
      -- of view of the arithmetic effect:
      --
      -- 1. Instructions that store the result in the C flag.
      --    These are Bit_Load, Bit_And, Bit_Or and Bit_Xor.
      --
      -- 2. Instructions that store the result in the Z flag.
      --    Only Bit_Test is in this group.
      --
      -- 3. Instructions that store the result in the selected
      --    bit of the Host operand. These are Bit_Clear, Bit_Not,
      --    Bit_Set and Bit_Store. No flags are changed.
      --
      -- We model the first and second groups (target in C or Z) by
      -- assigning an unknown value to the Less or Equal flag cell
      -- (which correspond to the C and Z flags respectively).
      --
      -- We model the third group (target in the Host) by assigning
      -- an unknown value to the whole Host octet and to the word of
      -- which the Host is a part.
      --
      -- The boolean operation is not modelled for either group.

      if Operation in Bit_Kind_Write_T then
         -- The result goes in the Host operand.

         Assign (
            Target  => Host,
            Value   => Arithmetic.Unknown,
            Within  => Effect);

         Effort := Effort + Timing.Time_To_Write (Operands.Host, Octet);

      elsif Operation = Bit_Test then
         -- The result goes in the Z/Equal flag.

         Arithmetic.Add (
            To   => Effect,
            More => Arithmetic.Set (Cells.Equal_Flag));

      else
         -- The result goes in the C/Less flag.

         Arithmetic.Add (
            To   => Effect,
            More => Arithmetic.Set (Cells.Less_Flag));

      end if;

      -- Create the step:

      Flow.Add_Step (
         To     => Graph,
         Tag    => Tag,
         Effect => Arithmetic.To_Effect_Ref (Effect),
         Info   => (Effort => Effort),
         Giving => Giving);

   end Model_Bit_Instruction;


   procedure Model_Branch_Instruction (
      Operands      : in     Cond_Branch_Op_T;
      Length        : in     Instruction_Length_T;
      Tag           : in     Flow.Step_Tag_T;
      Unsigned_Cond : in     Boolean;
      Graph         : in     Flow.Graph_T;
      Giving        :    out Flow.Step_T)
   --
   -- Models branch instructions.
   --
   -- All branch-instructions have the same Effort, so the core of this
   -- procedure is in the case Operands.Condition, where the arithmetics is
   -- set, and where the Flow.Add_Edge that performs a "jump" in the code if
   -- its condition is true, is added. An "Branch_Not_Taken" -Edge is also
   -- added.
   --
   -- Unsigned_Cond
   --    Tells us to approximate a signed condition by the corresponding
   --    unsigned one. This can be unsafe.
   --

   is
      use Processor;

      Branch_Cond : Condition_T := Operands.Condition;
      -- The branch condition.
      -- The initial value may be modified under some options.

      Effort : Processor.Effort_T := (
         Timing.Time_To_Fetch (Tag, Length => 4));
      -- The parameter Length is set to 4, because all Bcc-instructions
      -- have 2 fetch-cycles although they are only two octets long.


      procedure Add_Branch_False_Condition
      --
      -- Adds the "branch not taken" edge from the Bcc step to the
      -- next consecutive instruction.
      --
      is
      begin

         Flow.Add_Edge (
            To      => Graph,
            Source  => Giving,
            Cond    => Condition(Negation(Branch_Cond)),
            Time    => 0,
            Target  => Tag + Length);

      end Add_Branch_False_Condition;


      procedure Add_Branch_True_Condition
      --
      -- Adds the "branch taken" edge from the Bcc step to the branch address.
      --
      is

         Target : constant Processor.Code_Address_T :=
            Shift (
               Base   => Tag.State.Address,
               Offset => Address_Offset_T (Operands.Offset));
         -- The target address where we go.

         Cond : constant Arithmetic.Condition_T := Condition(Branch_Cond);
         -- The precondition.

      begin

         Flow.Add_Edge (
            To     => Graph,
            Source => Giving,
            Cond   => Condition(Branch_Cond),
            Time   => 0,
            Target => Flow.Transit (
               From => Tag,
               To   => (Kind => Normal, Address => Target)));

         if Opt.Trace then

            Trace.Remark ("branch to " & Processor.Image (Target));

            if Opt.Trace_Effect and Cond /= Arithmetic.Always then

               Trace.Remark ("when " & Arithmetic.Image (Cond));

            end if;

         end if;

      end Add_Branch_True_Condition;


   begin -- Model_Branch_Instruction

      -- Perhaps translate signed condition to unsigned?

      if       Unsigned_Cond
      and then Branch_Cond in Similar_Unsigned_Cond'Range
      then

         Branch_Cond := Similar_Unsigned_Cond(Branch_Cond);

         if Opt.Trace then

            Trace.Remark ("using unsigned cond");

         end if;

      end if;

      -- Create the step:

      Flow.Add_Step (
         To     => Graph,
         Tag    => Tag,
         Effect => Arithmetic.No_Effect,
         Info   => (Effort => Effort),
         Giving => Giving);

      -- Create the edges and arithmetics:

      case Operands.Condition is

      when Always =>

         Add_Branch_True_Condition;

         -- BRA needs no "Branch not taken"-edge, because it will
         -- under no circumstances use that edge.

      when Never =>

         Output.Note ("Why use a ""branch never"" (BRN) instruction?");

         -- Add Edge:

         Add_Branch_False_Condition;

         -- BRN needs no "Branch not taken"-edge, because it will
         -- under no circumstances use that edge.

      when others =>

         Add_Branch_True_Condition;

         Add_Branch_False_Condition;

      end case;

   end Model_Branch_Instruction;


   procedure Model_Branch_Subroutine_Instruction (
      Tag        : in     Flow.Step_Tag_T;
      Offset     : in     Branch_Offset_T;
      Length     : in     Instruction_Length_T;
      Graph      : in     Flow.Graph_T;
      Subprogram : in     Programs.Subprogram_T;
      Giving     :    out Flow.Step_T)
   --
   -- Models a Branch_Subroutine instruction.
   --
   is
      use Processor;

      -- Construct the Effort:

      Effort : Processor.Effort_T :=
           Timing.Time_To_Fetch (Tag, Length => 4)
         + Timing.Time_To_Push
         - 2;
      -- The BSR-instruction is only 2 octets = 1 word long, but it still
      -- needs 2 fetch-cycles according to literature. Therefore, the
      -- parameter Length is set to 4 octets.

   begin  -- Model_Branch_Subroutine_Instruction

      -- Add the decoded step:

      Flow.Add_Step (
         To     => Graph,
         Tag    => Tag,
         Effect => Push_Return (To => Tag + Length),
         Info   => (Effort => Effort),
         Giving => Giving);

      if Opt.Trace then
         -- Trace the step here so that it is listed before
         -- the call-step created in Model_Call.

         Trace_Main_Step (Giving);

      end if;

      Model_Call (
         Source => Giving,
         Target => Shift (
            Base   => Tag.State.Address,
            Offset => Address_Offset_T (Offset)),
         Length   => Length,
         Caller   => Subprogram,
         Graph    => Graph);

   end Model_Branch_Subroutine_Instruction;


   procedure Get_Target (
      Vector  : in     H8_300.Address_T;
      Program : in     Programs.Program_T;
      Defined :    out Boolean;
      Target  :    out Processor.Code_Address_T)
   --
   -- As part of modelling a Memory_Indirect jump or jump-subroutine,
   -- we get the Target address in the given Vector slot (in the range
   -- 0 to FF hex) from the memory image of the Program.
   --
   -- The Defined parameter shows if the Vector slot is defined (loaded)
   -- by the program. If so, we assume that the content is static,
   -- otherwise it must be dynamic.
   --
   is

      Slot : constant Processor.Address_T := Processor.Address_T (Vector);
      -- The Vector address in Processor terms.

   begin

      Target := Processor.Code_Address_T (
         Format.Word (
            Address => Slot,
            From    => Programs.Processor_Info (Program).all));

      Defined := True;

      Output.Warning (
           "Assuming constant vector to "
         & Processor.Image (Target)
         & " at "
         & Processor.Image (Slot));

   exception

   when Format.Blank_Address =>
      -- The slot is not loaded.

      Output.Warning (
           "Target of vector at "
         & Processor.Image (Slot)
         & " is unknown.");

      Defined := False;
      Target  := Processor.Code_Address_T'First;

   end Get_Target;


   procedure Model_Jump_Instruction (
      Edge_Info : in     Processor.Loose_Edge_Info_T;
      Operation : in     Jump_Kind_T;
      Target    : in     Target_T;
      Length    : in     Instruction_Length_T;
      Tag       : in     Flow.Step_Tag_T;
      Program   : in     Programs.Program_T;
      Graph     : in     Flow.Graph_T;
      Giving    :    out Flow.Step_T)
   --
   -- Models a jump instruction with a given Operation and
   -- given Operand, by adding the corresponding step at the given
   -- Tag to the Graph and returning this step in Giving.
   --
   -- Processor-specific Edge_info for the loose edge leading to this
   -- jump instruction is also given, for use in Jump_Via_Table. The
   -- Program is given for the same reason.
   --
   -- The Length of the instruction is also given because it determines
   -- the number of Fetch cycles.
   --
   is
      use Processor;

      Effort : Processor.Effort_T :=
         Timing.Time_To_Fetch (Tag, Length => 4);
      --
      -- The effort of the instruction.
      -- More cycles are added below as necessary.
      -- The parameter Length is set to 4, because @R and @@aa:8 is only two
      -- octets long (which gives 1 fetch-cycle), while they acually have
      -- 2 fetch-cycles. @aa:16 is not affected, because it also has 2 cycles.

      Vector_Defined : Boolean;
      -- Whether the vector slot is defined (loaded) for a memory-indirect
      -- jump.

      Vector_Target : Processor.Code_Address_T;
      -- The target of a memory-indirect jump.

   begin  -- Model_Jump_Instruction

      -- Contruct the effort:

      case Target.Kind is

      when Absolute =>
         -- Also uses two Internal cycles.

         Effort := Effort + 2;

      when Register_Indirect =>

         null;

      when Memory_Indirect =>
         -- Also uses one Branch Address Read cycle and two Internal cycles.

         Effort := Effort + Timing.Time_To_Vector (Through => Target) + 2;

      end case;

      -- Create the step:

      Flow.Add_Step (
         To     => Graph,
         Tag    => Tag,
         Effect => Arithmetic.No_Effect,
         Info   => (Effort => Effort),
         Giving => Giving);

      -- Add the Edges:

      case Target.Kind is

      when Absolute =>
         -- Jump to a statically known address.

         Flow.Add_Edge (
            To     => Graph,
            Source => Giving,
            Time   => 0,
            Target => Flow.Transit (
               From => Tag,
               To   => (
                  Kind    => Normal,
                  Address => Code_Address_T (Target.Address))));

      when Register_Indirect =>
         -- Jump to the address given in a register.

         case Tag.State.Kind is

         when Normal =>
            -- A "jmp @Rn" that is not a part of a jump-via-table.

            Output.Error ("Jump via register taken as return.");

            Flow.Calls.Return_After (
               Step   => Giving,
               Within => Graph);

            -- TBA implementation?

         when Jump_Via_Table =>
            -- The preceding instruction was "mov.w @(base,Ri), Rn"
            -- and this one is "jmp @Rn". This seems to be a jump via
            -- a table of addresses.

            Flow.Add_Dynamic_Edge (
               To     => Graph,
               Source => Giving,
               Edge   => Dynamics.Jump_Via_Table (
                  Base  => Edge_Info.Base,
                  Index => Cells.Jump_Index_Var,
                  Code  => Programs.Processor_Info (Program)),
               Warn => Flow.Never_Warn);
            --
            -- A warning was emitted earlier, in Try_Jump_Via_Table.

            if Opt.Trace then

               Trace.Remark ("jump via table");

            end if;

         when others =>

            Output.Fault (
               Location => "Decoder.Model_Jump_Instruction",
               Text     =>
                    "Tag.State.Kind is "
                  & Step_Kind_T'Image (Tag.State.Kind));

         end case;

      when Memory_Indirect =>
         -- Jump to the address given in a memory (vector) slot.

         Get_Target (
            Vector  => Target.Target_Pointer,
            Program => Program,
            Defined => Vector_Defined,
            Target  => Vector_Target);

         if Vector_Defined then
            -- The vector is loaded. We assume it is constant.

            Flow.Add_Edge (
               To     => Graph,
               Source => Giving,
               Time   => 0,
               Target => Flow.Transit (
                  From => Tag,
                  To   => (
                     Kind    => Normal,
                     Address => Vector_Target)));

         else
            -- The vector is not loaded, so it must be set dynamically.
            -- We give up.

            Output.Error ("Jump via dynamic vector taken as return.");

            Flow.Calls.Return_After (
               Step   => Giving,
               Within => Graph);

         end if;

      end case;

   end Model_Jump_Instruction;


   procedure Model_Jump_Subroutine_Instruction (
      Target     : in     Target_T;
      Tag        : in     Flow.Step_Tag_T;
      Length     : in     Instruction_Length_T;
      Subprogram : in     Programs.Subprogram_T;
      Graph      : in     Flow.Graph_T;
      Giving     :    out Flow.Step_T)
   --
   -- Models a jump subroutine instruction with a given Operation and
   -- given Operand, by adding the corresponding step at the given
   -- Tag to the Graph and returning this step in Giving.
   --
   -- The Length of the instruction is also given because it determines
   -- the number of Fetch cycles.
   --
   is
      use Processor;

      Effort : Processor.Effort_T :=
           Timing.Time_To_Fetch (Tag, Length => 4)
         + Timing.Time_To_Push;
      --
      -- The initial effort of the instruction.
      -- Some cycles are added or subtracted below as necessary.
      -- The parameter Length is set to 4, because @R and @@aa:8 is only two
      -- octets long (which gives 1 fetch-cycle), while they acually have
      -- 2 fetch-cycles. @aa:16 is not affected, because it also has 2 cycles.

      Defined : Boolean;
      -- Whether the callee address is statically defined (when doubtful).

      Callee : Processor.Code_Address_T;
      -- The entry address of the callee, if Defined.

      Return_Point : constant Flow.Step_Tag_T := Tag + Length;
      -- The return point.

      Call_Step : Flow.Step_T;
      -- The call-step.

   begin  -- Model_Jump_Subroutine_Instruction

      -- Contruct the effort:

      case Target.Kind is

      when Absolute =>
         -- No further effort required.

         null;

      when Register_Indirect =>
         -- The auto-modification of the Stack Pointer does not
         -- increase the number of internal cycles.

         Effort := Effort
                 - Timing.Time_To_Auto_Mod;

      when Memory_Indirect =>
         -- Also needs a Branch Address Read cycle, but again the
         -- auto-modification of the Stack Pointer does not add
         -- Internal cycles.

         Effort := Effort
                 + Timing.Time_To_Vector (Through => Target)
                 - Timing.Time_To_Auto_Mod;

      end case;

      -- Create the main step for the JSR:

      Flow.Add_Step (
         To     => Graph,
         Tag    => Tag,
         Effect => Push_Return (To => Return_Point),
         Info   => (Effort => Effort),
         Giving => Giving);

      if Opt.Trace then
         -- Trace the step here so that it is listed before
         -- the call-step created in Model_Call.

         Trace_Main_Step (Giving);

      end if;

      -- Find the Callee:

      case Target.Kind is

      when Absolute =>
         -- Call to a statically known (immediate) address.

         Callee := Code_Address_T (Target.Address);

         Model_Call (
            Source => Giving,
            Target => Callee,
            Length => Length,
            Caller => Subprogram,
            Graph  => Graph);

      when Register_Indirect =>
         -- Call to the address given in a register.

         Flow.Calls.Add_Boundable_Call (
            To     => Graph,
            From   => Giving,
            Caller => Subprogram,
            Target => Decoder.Dynamics.Call_Via_Pointer (
               Index  => Register_Var (Target.Pointer, Word),
               Caller => Subprogram),
            Effect => Unresolved_Call_Effect (Programs.Program (Subprogram)),
            Info   => (Effort => Processor.No_Effort),
            Retur  => (Flow.Calls.To_Caller, Return_Point),
            Giving => Call_Step);

         if Opt.Trace then

            Trace.Remark ("dynamic call");

            Trace_Extra_Step (Call_Step);

         end if;

      when Memory_Indirect =>
         -- Call to the address given in a memory (vector) slot.

         Get_Target (
            Vector  => Target.Target_Pointer,
            Program => Programs.Program (Subprogram),
            Defined => Defined,
            Target  => Callee);

         if Defined then
            -- The vector is statically loaded. We assume it is constant.

            if Opt.Trace then

               Trace.Remark ("static vector");

            end if;

            Model_Call (
               Source => Giving,
               Target => Callee,
               Length => Length,
               Caller => Subprogram,
               Graph  => Graph);

         else
            -- The vector is not loaded, so it must be set dynamically.

            Flow.Calls.Add_Boundable_Call (
               To     => Graph,
               From   => Giving,
               Caller => Subprogram,
               Target => Decoder.Dynamics.Call_Via_Pointer (
                  Index  => Memory (
                     Address => Processor.Address_T (Target.Target_Pointer),
                     Width   => Word),
                  Caller => Subprogram),
               Effect => Unresolved_Call_Effect (Programs.Program (Subprogram)),
               Info   => (Effort => Processor.No_Effort),
               Retur  => (Flow.Calls.To_Caller, Return_Point),
               Giving => Call_Step);

            if Opt.Trace then

               Trace.Remark ("dynamic call via vector");

               Trace_Extra_Step (Call_Step);

            end if;

         end if;

      end case;

   end Model_Jump_Subroutine_Instruction;


   procedure Model_Move_Block_Instruction (
      Tag    : in     Flow.Step_Tag_T;
      Length : in     Instruction_Length_T;
      Graph  : in     Flow.Graph_T;
      Giving :    out Flow.Step_T)
   --
   -- The Move Block instruction is truly a special case in H8/300 decoding,
   -- it is used to move a block of data from the memory location specified in
   -- general register R5 to its sibling in R6. General register R4L gives
   -- the byte length of the block. This is done in a kind of loop. One byte
   -- at a time is transferred.
   --
   -- This operation performs its own tracing of the added steps.
   --
   is
      use Processor;

      R4L : constant Arithmetic.Variable_T := Register_Var(4, Low_Byte);
      -- The counter register.

      R5 : constant Arithmetic.Variable_T := Register_Var(5, Word);
      -- The source pointer.

      R6 : constant Arithmetic.Variable_T := Register_Var(6, Word);
      -- The destination pointer.

      Source_Operand : constant Operand_T := (
         Kind         => Register_Indirect,
         Pointer      => 5,
         Auto_Mod     => Post_Increment,
         Displacement => 0);
      -- The source operand (one octet).

      Target_Operand : constant Operand_T := (
         Kind         => Register_Indirect,
         Pointer      => 6,
         Auto_Mod     => Post_Increment,
         Displacement => 0);
      -- The target operand (one octet).

      Loop_Effect : constant Arithmetic.Effect_T := (
         Arithmetic.Set (
            Target => Target (Target_Operand, Octet),
            Value  => Source (Source_Operand, Octet)),
         Arithmetic.Set (R4L, R4L - One_Octet),
         Arithmetic.Set (R5 , R5  + One_Word),
         Arithmetic.Set (R6 , R6  + One_Word));
      -- The effect that moves one octet in Move_Block and updates
      -- the counter and the pointers. One iteration of the loop.

      Clobber_Effect : constant Arithmetic.Effect_T := (
         Arithmetic.Set (Register_Var(4, Word     )),
         Arithmetic.Set (Register_Var(5, Low_Byte )),
         Arithmetic.Set (Register_Var(5, High_Byte)),
         Arithmetic.Set (Register_Var(6, Low_Byte )),
         Arithmetic.Set (Register_Var(6, High_Byte)));
      -- The effect that veils (makes unknown) those sub/super-cells
      -- of the counter and pointers that are clobbered by the
      -- execution of a Move_Block with a non-null block.
      --
      -- We keep this effect separate from the Loop_Effect to simplify
      -- the arithmetic of the loop and to reduce the maximum number of
      -- assignments per step.

      Exit_Move_Cond : constant Arithmetic.Condition_T := EqZero (R4L);
      -- The condition for exiting (terminating) or not entering the loop.

      Repeat_Move_Cond : constant Arithmetic.Condition_T :=
         Arithmetic.Gtu (R4L, Zero_Octet);
      -- The condition for repeating (iterating) the loop.

      Loop_Step : Flow.Step_T;
      -- The synthetic step that represents the loop.

      Clobber_Step : Flow.Step_T;
      -- The synthetic step that clobbers the sub/super-cells of the
      -- counter and pointers, on exit from the loop.

      Next_Step : constant Flow.Step_Tag_T := Tag + Length;
      -- The address of the next (consecutive) instruction, to
      -- follow Move_Block.
      --
      -- The flow-graph for the Move Block instruction consists of 3 steps
      -- and five edges as in the following pseudo-code:
      --
      -- [step Giving, models the EEPMOV instruction itself]
      --    if R4L  = 0 then go to Next_Step (null block)
      --    if R4L /= 0 then go to Loop_Step (non-null block)
      --
      -- [step Loop_Step]
      --    move octet from @R5+ to @R6+
      --    decrement R4L
      --    if R4L /= 0 then go to Loop_Step    (repeat loop)
      --    if R4L  = 0 then go to Clobber_Step (exit loop)
      --
      -- [step Clobber_Step]
      --    veil R4, R5L, R5H, R6L, R6H
      --    go to Next_Step.

      Fetch_Effort : Processor.Effort_T :=
         Timing.Time_To_Fetch (Tag, Length);
      -- The effort to fetch the Move_Block instruction.

      Effort_Per_Octet : constant Processor.Effort_T :=
           Timing.Time_To_Read  (Source_Operand, Octet)
         + Timing.Time_To_Write (Target_Operand, Octet)
         - Timing.Time_To_Auto_Mod * 2;
      --
      -- The effort for moving one octet: reading an octet from
      -- the address in R5 and writing it to the address in R6.
      -- However, the time to auto-modify the pointers is somehow
      -- absorbed, so we subtract it here.

   begin  -- Model_Move_Block_Instruction

      -- Construct the first, normal step:

      Flow.Add_Step (
         To     => Graph,
         Tag    => Tag & Normal,
         Effect => Arithmetic.No_Effect,
         Info   => (Effort => Fetch_Effort + Effort_Per_Octet + 1),
         Giving => Giving);
      --
      -- Note that some effort is expended in addition to the fetch
      -- even if R4L = 0 and no octets are really moved.

      Trace_Main_Step (Giving);

      -- Construct the second, synthetic Loop Step:

      Flow.Add_Step (
         To     => Graph,
         Tag    => Tag & EEPMOV,
         Effect => Loop_Effect,
         Info   => (Effort => Effort_Per_Octet),
         Giving => Loop_Step);

      Trace_Extra_Step (Loop_Step);

      -- Construct the third, also synthetic Clobber Step:

      Flow.Add_Step (
         To     => Graph,
         Tag    => Tag & Clobber,
         Effect => Clobber_Effect,
         Info   => (Effort => No_Effort),
         Giving => Clobber_Step);

      Trace_Extra_Step (Clobber_Step);

      -- Construct the Edge from the first step to next instruction,
      -- executed when R4L = 0 (null block):

      Flow.Add_Edge (
         To     => Graph,
         Source => Giving,
         Cond   => Exit_Move_Cond,
         Time   => 0,
         Target => Next_Step);

      -- Construct the Edge from the first step to the Loop Step,
      -- executed when R4L > 0 (non-null block):

      Flow.Add_Edge (
         To     => Graph,
         Source => Giving,
         Cond   => Repeat_Move_Cond,
         Time   => 0,
         Target => Loop_Step);

      -- Construct the Edge that models the loop in the Loop Step,
      -- executed when R4L > 0 (more octets to move):

      Flow.Add_Edge (
         To     => Graph,
         Source => Loop_Step,
         Cond   => Repeat_Move_Cond,
         Time   => 0,
         Target => Loop_Step);

      -- Construct the Edge from the Loop_Step to the Clobber Step,
      -- executed when R4L = 0 (all octets moved):

      Flow.Add_Edge (
         To     => Graph,
         Source => Loop_Step,
         Cond   => Exit_Move_Cond,
         Time   => 0,
         Target => Clobber_Step);

      -- Construct the Edge from the Clobber Step to the next instruction:

      Flow.Add_Edge (
         To     => Graph,
         Source => Clobber_Step,
         Cond   => Arithmetic.Always,
         Time   => 0,
         Target => Next_Step);

   end Model_Move_Block_Instruction;


   function Signed_Cond_Unknown (Subprogram : Programs.Subprogram_T)
   return Boolean
   --
   -- Whether branch instructions with signed conditions should be
   -- modelled as unknown conditions, in this Subprogram, whatever
   -- value was given on the command line for the -bcc=xxx option.
   --
   is
   begin

      return Programs.Processor_Info (Subprogram).Signed_Cond_Unknown;

   end Signed_Cond_Unknown;


   procedure Model (
      Instruction : in Instruction_T;
      Length      : in Instruction_Length_T;
      Tag         : in Flow.Step_Tag_T;
      Edge_Info   : in Processor.Loose_Edge_Info_T;
      Program     : in Programs.Program_T;
      Subprogram  : in Programs.Subprogram_T;
      Graph       : in Flow.Graph_T)
   --
   -- Models the given Instruction by adding the corresponding steps
   -- and edges to the flow Graph for the Subprogram in which the
   -- Instruction occurs.
   --
   -- The Length parameter shows the length, in octets, of the concrete
   -- (binary) form of the Instruction.
   --
   -- The Tag parameter is the step-tag (context and state) for the
   -- (first) step that will be created to model this instruction.
   --
   -- The Edge_Info parameter contains the processor-specific context
   -- information from the loose edge leading to the Tag.
   --
   -- The other parameters are as in the Decode operation.
   --
   is
      use Processor;

      Decoded_Step : Flow.Step_T;
      -- The first step that models the Instruction.


      procedure Trace_Step
      --
      -- Traces the result (effect and effort) and the number of
      -- the Decoded_Step, if tracing is chosen.
      --
      is
      begin

         Trace_Main_Step (Decoded_Step);

      end Trace_Step;


      procedure Normal_Succession
      --
      -- Adds a normal, zero-time, unconditional edge from Decoded
      -- Step to the next consecutive instruction.
      --
      is
      begin

         Flow.Add_Edge (
            To      => Graph,
            Source  => Decoded_Step,
            Time    => 0,
            Target  => Tag + Length);

      end Normal_Succession;


      procedure Add_Normal_Step (
         Effect : in Arithmetic.Effect_Ref;
         Effort : in Processor.Effort_T)
      --
      -- Adds a "normal" step with given Effect and Effect.
      --
      is

      begin

         Flow.Add_Step (
            To     => Graph,
            Tag    => Tag,
            Effect => Effect,
            Info   => (Effort => Effort),
            Giving => Decoded_Step);

         Trace_Step;

      end Add_Normal_Step;


      procedure Add_Normal_Step (Effect : in Arithmetic.Effect_T)
      --
      -- Adds a "normal" step with a given Effect and an effort that
      -- is defined just by the instruction fetch.
      --
      is
      begin

         Flow.Add_Step (
            To     => Graph,
            Tag    => Tag,
            Effect => Effect,
            Info   => (
               Effort => Timing.Time_To_Fetch (Tag, Length)),
            Giving => Decoded_Step);

         Trace_Step;

      end Add_Normal_Step;


   begin  -- Model (Instruction)

      -- In this case-when, the instructions is added to the flow graph,
      -- and they are divided after their Instruction-kind. In most cases,
      -- a procedure is used for encapsulation.

      case Instruction.Kind is

      when Two_Operand_Kind_T =>

         Model_Two_Operand_Instruction (
            Operation => Instruction.Kind,
            Operands  => Instruction.Two,
            Length    => Length,
            Tag       => Tag,
            Program   => Program,
            Graph     => Graph,
            Giving    => Decoded_Step);

      when One_Operand_Kind_T =>

         Model_One_Operand_Instruction (
            Operation => Instruction.Kind,
            Operands  => Instruction.One,
            Length    => Length,
            Tag       => Tag,
            Graph     => Graph,
            Giving    => Decoded_Step);

         Trace_Step;

         Normal_Succession;

      when Stack_Kind_T =>

         Model_Stack_Instruction (
            Operation => Instruction.Kind,
            Operands  => Instruction.Stack,
            Length    => Length,
            Tag       => Tag,
            Graph     => Graph,
            Giving    => Decoded_Step);

         Trace_Step;

         Normal_Succession;

      when CCR_Transfer_Kind_T =>

         Add_Normal_Step (
            Effect  =>
               CCR_Transfer_Effect (
                  Kind    => Instruction.Kind,
                  Operand => Instruction.Transfer));

         Normal_Succession;

      when CCR_Update_Kind_T =>

         Add_Normal_Step (
            Effect  =>
               CCR_Update_Effect (
                  Kind  => Instruction.Kind,
                  Value => Instruction.Update.Value));

         Normal_Succession;

      when Bit_Kind_T =>

         Model_Bit_Instruction (
            Operation => Instruction.Kind,
            Operands  => Instruction.Bit,
            Length    => Length,
            Tag       => Tag,
            Graph     => Graph,
            Giving    => Decoded_Step);

         Trace_Step;

         Normal_Succession;

      when Branch =>

         Model_Branch_Instruction (
            Operands      => Instruction.Branch_Op,
            Length        => Length,
            Tag           => Tag,
            Unsigned_Cond =>
               Opt.Unsigned_Cond
               and then not Signed_Cond_Unknown (Subprogram),
            Graph         => Graph,
            Giving        => Decoded_Step);

         Trace_Step;

      when Branch_Subroutine =>

         Model_Branch_Subroutine_Instruction (
            Tag        => Tag,
            Offset     => Instruction.Subroutine_Offset,
            Length     => Length,
            Graph      => Graph,
            Subprogram => Subprogram,
            Giving     => Decoded_Step);

      when Jump =>

         Model_Jump_Instruction (
            Edge_Info => Edge_Info,
            Operation => Instruction.Kind,
            Target    => Instruction.Target,
            Length    => Length,
            Tag       => Tag,
            Program   => Program,
            Graph     => Graph,
            Giving    => Decoded_Step);

         Trace_Step;

      when Jump_Subroutine =>

         Model_Jump_Subroutine_Instruction (
            Target     => Instruction.Target,
            Tag        => Tag,
            Length     => Length,
            Subprogram => Subprogram,
            Graph      => Graph,
            Giving     => Decoded_Step);

      when Move_Block =>

         Model_Move_Block_Instruction(
            Tag    => Tag,
            Length => Length,
            Graph  => Graph,
            Giving => Decoded_Step);

      when No_Op =>

         Add_Normal_Step (
            Effect => Arithmetic.No_Effect,
            Effort => Timing.Time_To_Fetch (Tag, Length));

         Normal_Succession;

      when Return_From_Subroutine =>

         Add_Normal_Step (
            Effect => Pop_Return,
            Effort =>
                 Timing.Time_To_Fetch (Tag, Length => 4)
               + Timing.Time_To_Pop);
         -- The parameter Length is set to 4, because RTS is only two octets
         -- in length, altough it has 2 fetch-cycles.

         -- We add no new loose edge since the subprogram stops here.

         Flow.Calls.Return_After (
            Step   => Decoded_Step,
            Within => Graph);

      when Return_From_Exception =>

         Add_Normal_Step (
            Effect => Pop_Return_Exception,
            Effort => (
                 Timing.Time_To_Fetch (Tag, Length => 4)
               + Timing.Time_To_Pop * 2
               - 2));
         -- The parameter Length is set to 4, because RTE is only two octets
         -- in length, altough it has 2 fetch-cycles.

         -- We add no new loose edge since the subprogram stops here.

         Flow.Calls.Return_After (
            Step   => Decoded_Step,
            Within => Graph);

      when Sleep =>

         Output.Warning ("WCET omits sleeping time for SLEEP instruction.");

         Add_Normal_Step (
            Effect => Arithmetic.No_Effect,
            Effort => Timing.Time_To_Fetch (Tag, Length));

         Normal_Succession;

      when Undefined =>

         Output.Error ("Undefined instruction code.");

         Flow.Add_Step (
            To     => Graph,
            Tag    => Tag,
            Effect => Arithmetic.No_Effect,
            Info   => (
               Effort => Timing.Time_To_Fetch (Tag, Length)),
            Giving => Decoded_Step);

         Trace_Step;

         if Opt.Trace then

            Trace.Remark ("not modelled");

         end if;

         Normal_Succession;

      end case;

   end Model;


   procedure Start (
      Program    : in Programs.Program_T;
      Subprogram : in Programs.Subprogram_T;
      Resuming   : in Boolean)
   is
   begin

      if Opt.Trace then

         Trace.Start_Decoding (
            Name     => Programs.Name (Subprogram),
            Resuming => Resuming);

      end if;

   end Start;


   Valid_Loose_Edge : constant array (Processor.Step_Kind_T) of Boolean := (
      Processor.Normal         => True,
      Processor.Add_Addx       => True,
      Processor.Cmp_Subx       => True,
      Processor.Sub_Subx       => True,
      Processor.Jump_Via_Table => True,
      Processor.EEPMOV         => False,
      Processor.Clobber        => False);
   --
   -- Whether the Kind of step address in a loose edge is valid for
   -- presentation to the Decode procedure. The invalid Kinds are used
   -- for steps that are generated within one Decoder invocation and
   -- are (or should) never be the target of a loose edge.


   procedure Decode (
      Loose_Edge : in Flow.Loose_Edge_T;
      Program    : in Programs.Program_T;
      Subprogram : in Programs.Subprogram_T;
      Graph      : in Flow.Graph_T)
   is
      use type Processor.Step_Kind_T;

      Tag : constant Flow.Step_Tag_T := Loose_Edge.Target;
      -- The context and state of the step (instruction) to be decoded.

      PC : Processor.Code_Address_T := Tag.State.Address;
      -- The address of the instruction to be decoded.

      Words : Word_List_T (0 .. 1);
      -- The H8/300 instruction words fetched from PC.

      Last_Word : Integer;
      -- The index of the last fetched word, in Words.

      Instruction : Instruction_T;
      -- The H8/300 instruction, as decoded from the Words.

      Instruction_Length : Instruction_Length_T;
      -- The length of the Instruction.

      Last_Used_Word : Integer;
      -- The index of the last word in Words that was used to
      -- create the Instruction. This depends on Instruction_Length.

      Step_Mark : Output.Nest_Mark_T;
      -- Marks the default output locus describing the location
      -- of this step (address) in the program.

   begin

      -- Keep the user informed:

      Step_Mark := Output.Nest (Output.Locus (
         Statements =>
            Symbols.Show.Statements (
               Address => PC,
               Source  => Programs.Symbol_Table (Program))));

      if Opt.Trace then

         Trace.Step_Tag (Tag);

      end if;

      -- Check kind of address for internal consistency:

      if not Valid_Loose_Edge(Tag.State.Kind) then

         Output.Fault (
            Location => "Decoder.Decode",
            Text     =>
                 "Wrong Kind of address"
               & Output.Field_Separator
               & Processor.Step_Kind_T'Image (Tag.State.Kind));

         raise Program_Error;

      end if;

      -- Fetch the instruction and decode it:

      Format.Fetch_Code (
         From    => Programs.Processor_Info (Program).all,
         Address => PC,
         Into    => Words,
         Last    => Last_Word);

      if Last_Word < Words'First then
         -- Nothing could be fetched at this address.

         if Opt.Trace then

            Trace.Remark ("no instruction loaded at this address");

         end if;

         Output.Error ("No instruction loaded at this address.");

      else
         -- Something could be fetched.

         -- Decode the instruction into abstract form:

         H8_300.Decode (
            From        => Words(Words'First .. Last_Word),
            Instruction => Instruction,
            Length      => Instruction_Length);

         if Instruction_Length = 2 then
            -- A one-word instruction.

            Last_Used_Word := Words'First;

         else
            -- A two-word instruction (assume length = 4 octets).

            Last_Used_Word := Words'First + 1;

         end if;

         -- Perhaps trace the binary and disassembled instruction:

         if Opt.Trace then

            Trace.Code_Words (
               Address => PC,
               Code    => Words(Words'First .. Last_Used_Word));

            Trace.Instruction (Instruction);

         end if;

         -- Add the instruction to the flow graph:

         Model (
            Instruction => Instruction,
            Length      => Instruction_Length,
            Tag         => Tag,
            Edge_Info   => Loose_Edge.Info,
            Program     => Program,
            Subprogram  => Subprogram,
            Graph       => Graph);

      end if;

      -- Finish:

      if Opt.Trace then

         Trace.Done;

      end if;

      Output.Unnest (Step_Mark);

   exception

   when Invalid_Instruction =>

      if Opt.Trace then

         Trace.Code_Words (
            Address => PC,
            Code    => Words(Words'First .. Last_Word));

         Trace.Remark ("invalid instruction");

         Trace.Done;

      end if;

      Output.Error ("Invalid instruction.");

      Output.Unnest (Step_Mark);

   when others =>

      if Opt.Trace then

         Trace.Remark ("decoding failed");

         Trace.Done;

      end if;

      Output.Unnest (Step_Mark);

      raise;

   end Decode;


   procedure Integrate_Call (
      Host   : in Programs.Subprogram_T;
      Graph  : in Flow.Graph_T;
      Callee : in Programs.Subprogram_T;
      Source : in Flow.Step_T;
      Cond   : in Arithmetic.Condition_T;
      Time   : in Processor.Time_T;
      Target : in Flow.Step_Tag_T)
   is
   begin

      Flow.Add_Edge (
         To     => Graph,
         Source => Source,
         Cond   => Cond,
         Time   => Time,
         Target => Target);

   end Integrate_Call;


   procedure Integrate_Return (
      Host   : in Programs.Subprogram_T;
      Graph  : in Flow.Graph_T;
      Source : in Flow.Step_T;
      Time   : in Processor.Time_T;
      Target : in Flow.Step_Tag_T)
   is
   begin

      Flow.Add_Edge (
         To     => Graph,
         Source => Source,
         Time   => Time,
         Target => Target);

   end Integrate_Return;


   procedure Stop (
      Program    : in Programs.Program_T;
      Subprogram : in Programs.Subprogram_T;
      Graph      : in Flow.Graph_T;
      Suspended  : in Boolean)
   is
   begin

      if Opt.Trace then

         Trace.Stop_Decoding (
            Name      => Programs.Name (Subprogram),
            Graph     => Graph,
            Suspended => Suspended);

      end if;

   end Stop;


   procedure Stub (
      Entry_Tag  : in Flow.Step_Tag_T;
      Graph      : in Flow.Graph_T;
      Program    : in Programs.Program_T;
      Subprogram : in Programs.Subprogram_T)
   is

      Step : Flow.Step_T;
      -- The single step for the stub.

   begin

      Flow.Add_Step (
         To  => Graph,
         Tag => Flow.Transit (
            From => Entry_Tag,
            To   => (
               Kind    => Processor.Normal,
               Address => Entry_Tag.State.Address)),
         Effect  => Arithmetic.No_Effect,
         Info    => (Effort => Processor.No_Effort),
         Giving  => Step);

   end Stub;


   procedure Finish_Arithmetic (
      Subprogram  : in Programs.Subprogram_T;
      Graph       : in Flow.Graph_T;
      Call_Bounds : in Programs.Execution.Call_Bounds_List_T)
   is
   begin

      null;
      --
      -- No finishing actions required at present.
      -- The arithmetic effects built in Decode are complete.

   end Finish_Arithmetic;


   function Value_Origins_Applicable (
      To    : Programs.Execution.Bounds_Ref;
      Along : Programs.Call_Path_T)
   return Boolean
   is
   begin

      return False;
      --
      -- We not not use value-origin analysis for any purposes
      -- specific to the H8/300.

   end Value_Origins_Applicable;


   procedure Apply_Value_Origins (
      Origins : in Flow.Origins.Map_Ref;
      Bounds  : in Programs.Execution.Bounds_Ref)
   is
   begin

      Output.Fault (
         Location => "Decoder.Apply_Value_Origins",
         Text     => "Should never be called.");

   end Apply_Value_Origins;


   procedure Finish (
      Program    : in Programs.Program_T;
      Subprogram : in Programs.Subprogram_T;
      Graph      : in Flow.Graph_T;
      Assert_Map : in Assertions.Assertion_Map_T)
   is
   begin

      null;

   end Finish;


   --
   ---   Target-specific additional analysis
   --


   procedure Additional_Analysis (
      Program    : in Programs.Program_T;
      Asserts    : in Assertions.Assertion_Set_T;
      Bounds_Set : in Programs.Execution.Bounds_Set_T)
   is
   begin

      null;

   end Additional_Analysis;


   function Additional_Variables (
      Subprogram : Programs.Subprogram_T;
      Bounds     : Programs.Execution.Bounds_Ref)
   return ILP.Var_List_T
   is
   begin

      return ILP.No_Variables;

   end Additional_Variables;


   function Additional_Time (
      Subprogram : Programs.Subprogram_T;
      Bounds     : Programs.Execution.Bounds_Ref)
   return ILP.Expression_T
   is
   begin

      return ILP.Zero_Expression;

   end Additional_Time;


   procedure Additional_Bounds (
      Subprogram : in     Programs.Subprogram_T;
      Bounds     : in     Programs.Execution.Bounds_Ref;
      Solver     : in out ILP.Solver_T)
   is
   begin

      null;

   end Additional_Bounds;


   procedure Take_Additional_Values (
      Values     : in ILP.Valuation_T;
      Subprogram : in Programs.Subprogram_T;
      Bounds     : in Programs.Execution.Bounds_Ref)
   is
   begin

      null;

   end Take_Additional_Values;


   procedure Post_Analysis (
      Bounds : in Programs.Execution.Bounds_Set_T)
   is
   begin

      null;

   end Post_Analysis;


   --
   ---   Graphic decorations
   --


   function Decoded_Instruction (
      Step    : Flow.Step_T;
      Program : Programs.Program_T)
   return String
   is
      use Processor;

      Prog_Info : Processor.Program.Info_T
         renames Programs.Processor_Info (Program);
      -- Info for the program.

      State : constant Flow_State_T := Flow.State (Step);
      -- The control-flow state at this step.

      Words : Word_List_T (0 .. 1);
      -- The H8/300 instruction words fetched from PC.

      Last_Word : Integer;
      -- The index of the last fetched word, in Words.

      Instruction : Instruction_T;
      -- The H8/300 instruction, as decoded from the Words.

      Instruction_Length : Instruction_Length_T;
      -- The length of the Instruction.

   begin

      case State.Kind is

      when Normal | Add_Addx | Cmp_Subx | Sub_Subx | Jump_Via_Table =>
         -- This step represents a real instruction, although
         -- the instruction may have a special interpretation or
         -- model in the context of this step.
         --
         -- We will show the decoded instruction.

	 Format.Fetch_Code (
            From    => Prog_Info.all,
            Address => Prime_Address (State),
            Into    => Words,
            Last    => Last_Word);

	 if Last_Word < Words'First then
            -- Nothing could be fetched at this address.

            return "[none]";

	 else
            -- Something could be fetched.

            H8_300.Decode (
               From        => Words(Words'First .. Last_Word),
               Instruction => Instruction,
               Length      => Instruction_Length);

           return H8_300.Text.Image (Instruction);

         end if;

      when EEPMOV =>
         -- Synthetic step for EEPMOV modelling.

         return "[eepmov1]";

      when Clobber =>
         -- Synthetic step for clobbering touched sub/super-cells.

         return "[eepmov2]";

      end case;

   end Decoded_Instruction;


end Decoder;
