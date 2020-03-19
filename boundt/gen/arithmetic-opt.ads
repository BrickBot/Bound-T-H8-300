-- Arithmetic.Opt (decl)
--
-- Options related to arithmetic analysis.
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
-- $Revision: 1.13 $
-- $Date: 2015/10/24 19:36:46 $
--
-- $Log: arithmetic-opt.ads,v $
-- Revision 1.13  2015/10/24 19:36:46  niklas
-- Moved to free licence.
--
-- Revision 1.12  2013-02-12 08:47:19  niklas
-- BT-CH-0245: Global volatile cells and "volatile" assertions.
--
-- Revision 1.11  2011-08-31 04:23:33  niklas
-- BT-CH-0222: Option registry. Option -dump. External help files.
--
-- Revision 1.10  2009-11-27 11:28:06  niklas
-- BT-CH-0184: Bit-widths, Word_T, failed modular analysis.
--
-- Revision 1.9  2008/07/23 09:07:14  niklas
-- BT-CH-0139: Fix recursion in Programs.Execution.Paths.
--
-- Revision 1.8  2007/10/31 12:15:59  niklas
-- BT-CH-0095: Arithmetic analysis of "live" dynamic data refs.
--
-- Revision 1.7  2007/08/02 11:06:34  niklas
-- Added option Check_Dup_Target.
--
-- Revision 1.6  2006/04/10 08:15:09  niklas
-- Moved the options for maximum calculator value from
-- Arithmetic.Opt to Calculator.Opt.
--
-- Revision 1.5  2004/04/25 14:08:33  niklas
-- First Tidorum version. Added Bound_Bitwise_Ops.
--
-- Revision 1.4  2003/03/13 07:35:42  holsti
-- Increased Default_Calc_Max to include the addresses of
-- typical trap-handlers in ERC32 programs.
--
-- Revision 1.3  2003/02/27 14:35:41  holsti
-- Added options Warn_Large_Literal and Warn_Signing_Literal.
--
-- Revision 1.2  2003/02/17 14:13:03  holsti
-- Added option -calc_max, replacing the constants Min/Max_Int_Omega,
-- and reduced the default limit on literals passed to the calculator.
-- Added version identification to the "usage" print-out.
--
-- Revision 1.1  2001/03/10 00:26:21  holsti
-- First version.
--


with Options.Bool;
with Options.Nat;


package Arithmetic.Opt is


   pragma Elaborate_Body;
   --
   -- To register the options.


   type Possible_Choice_T is (
      Undefined,
      Disabled,
      Automatic,
      Enforced);
   --
   -- Controls whether arithmetic analysis is applied to
   -- subprograms:
   --
   -- Undefined
   --    The choice is not made at this level.
   --
   -- Disabled
   --    Arithmetic analysis is not applied, and error messages
   --    are emitted if analysis would be needed.
   --
   -- Automatic
   --    Arithmetic analysis is applied when needed to bound
   --    the execution of a subprogram.
   --
   -- Enforced
   --    Arithmetic analysis is applied always, even if not
   --    necessary to bound the execution. This option can be
   --    used to compare asserted bounds (on e.g. loops) with
   --    computed bounds.


   subtype Choice_T is Possible_Choice_T range Disabled .. Enforced;
   --
   -- A defined choice of arithmetic analysis, or not.


   Analysis : Choice_T := Automatic;
   --
   -- The general option to control arithmetic analysis.
   -- This is a command-line option. Through assertions,
   -- the option can be overridden for specific subprograms.


   Analysis_Opt : aliased Options.Bool.Option_T (Default => True);
   --
   -- A Boolean command-line option that (partially) controls the
   -- arithmetic analysis. Setting this option to True sets the
   -- option Analysis to Enforced. Setting this option to False
   -- set the option Analysis to Disabled. Resetting the option
   -- sets Analysis to Automatic.


   type Ref_Choice_T is (
      None,
      Relevant,
      All_Item);
   --
   -- Controls how arithmetic analysis is applied to boundable
   -- memory references.
   --
   -- None
   --    Arithmetic analysis is not applied to any boundable memory
   --    references. However, such references may still be resolved
   --    by other forms of analysis such as constant propagation.
   -- Relevant
   --    Arithmetic analysis is applied only to boundable memory
   --    references that appear in expressions that define values that
   --    are relevant for the analysis. This includes expressions that
   --    appear in relevant assignments and expressions in the logical
   --    preconditions of flow-graph edges.
   -- All_Item
   --    Arithmetic analysis is applied to all boundable memory
   --    references, whereever they appear.


   package Ref_Choice_Valued is new Options.Discrete_Valued (
      Value_Type  => Ref_Choice_T,
      Value_Image => Ref_Choice_T'Image);
   --
   -- Options with values of type Ref_Choice_T.


   Ref_Choice_Opt : aliased Ref_Choice_Valued.Option_T (
      Default => Relevant);
   --
   -- The general option to control arithmetic analysis of
   -- boundable memory references.
   --
   Ref_Choice : Ref_Choice_T renames Ref_Choice_Opt.Value;


   Swap_Small_Negatives_Opt : aliased Options.Bool.Option_T (Default => False);
   --
   -- Whether to convert an addition of a large unsigned constant
   -- that represents a small negative number, to a subtraction of
   -- that small number, and vice versa for converting a subtraction
   -- to an addition.
   --
   -- For example, with 32-bit computations this will convert the
   -- expression x + 4294967295 into x - 1.
   --
   -- The main benefit of such conversions is to make the arithmetic
   -- effects easier to read and understand. Furthermore, using smaller
   -- numbers may lead to less (apparent) overflows, making the analysis
   -- easier. On the other hand, it may harm some interval bounds, bu
   -- making the upper bound negative while the lower bound is non-negative,
   -- thus making the interval void.
   --
   Swap_Small_Negatives : Boolean renames Swap_Small_Negatives_Opt.Value;


   Warn_Signing_Literal_Opt : aliased Options.Bool.Option_T (Default => True);
   --
   -- Whether to emit a warning when an instruction contains a literal
   -- value that can be interpreted as signed or unsigned, but we
   -- choose a specific interpretation because it makes the absolute
   -- value smaller. For example, 16#FFFF# is interpreted as "-1".
   --
   -- The use of this option is currently target-specific; it is
   -- not used in the target-independent modules.
   --
   Warn_Signing_Literal : Boolean renames Warn_Signing_Literal_Opt.Value;


   Show_Plus_Sign_Opt : aliased Options.Bool.Option_T (Default => False);
   --
   -- Whether to add a trailing "[+]" to the unsigned images of
   -- constant-valued arithmetic expressions when the expression is
   -- defined as Signed, although the value is non-negative and thus
   -- the signed and unsigned views (and images) are equal.
   --
   Show_Plus_Sign : Boolean renames Show_Plus_Sign_Opt.Value;


   Bound_Bitwise_Ops_Opt : aliased Options.Bool.Option_T (Default => True);
   --
   -- Whether to translate bit-wise logical "and" and "or" operations
   -- on values to bounds on the arithmetic value of the result, for
   -- calculation (Omega) purposes. The alternative is to translate
   -- such expressions into unknown arithmetic values.
   --
   -- If chosen, the translations are as follows:
   --
   --   Assignment          Translation
   --   ----------          -----------
   --   T := (A and B)      (0 <= T) and (T <= A) and (T <= B).
   --   T := (A or  B)      (0 <= T) and (T <= (A + B)).
   --
   -- The translation occurs only for the outermost bit-wise operation
   -- in an expression. For example, the expression "T := A and (C or D)"
   -- would be translated to the bounds (0 <= T) and (T <= A) and
   -- (T <= unknown). However, it seems unlikely that an instruction
   -- decoder would generate such nested bit-wise expressions.
   --
   -- The "xor" and "not" operations are never translated into integer
   -- constraints. For these operations, the target cell is always
   -- marked as unknown, except for special cases such as "A xor A"
   -- where the result can be evaluated without knowledge of the
   -- operands ("A xor A" is always zero).
   --
   -- If this option is disabled, the bit-wise "and" and "or" operations
   -- are treated in the same was as "xor" and "not".
   --
   Bound_Bitwise_Ops : Boolean renames Bound_Bitwise_Ops_Opt.Value;


   Max_Shift_Mul_Bits_Opt : aliased Options.Nat.Option_T (Default => 10);
   --
   -- The maximum value of "n" for which the value of
   -- "shift X left by n bits" is modelled as X * 2**n.
   -- For negative values of "n", or variable values of "n",
   -- the shift is considered opaque.
   --
   Max_Shift_Mul_Bits : Natural renames Max_Shift_Mul_Bits_Opt.Value;


   Warn_Large_Shift_Opt : aliased Options.Bool.Option_T (Default => False);
   --
   -- Whether to emit a warning when the amount of shift in an Slz
   -- operation exceeds the range 0 .. Max_Shift_Mul_Bits, which
   -- causes this operation to be translated as an unknown value
   -- for the calculator.
   --
   Warn_Large_Shift : Boolean renames Warn_Large_Shift_Opt.Value;


   Check_Dup_Target_Opt : aliased Options.Bool.Option_T (Default => False);
   --
   -- Whether to check, when adding an assignment to a set of assignments,
   -- that the set does not already contain an assignment to the same
   -- target cell.
   --
   Check_Dup_Target : Boolean renames Check_Dup_Target_Opt.Value;


   Trace_Alias_Opt : aliased Options.Bool.Option_T (Default => False);
   --
   -- Whether to emit a trace message when some possible aliasing is
   -- detected between cells such that assigning a value to one cell
   -- may alter the values of other cells.
   --
   Trace_Alias : Boolean renames Trace_Alias_Opt.Value;


end Arithmetic.Opt;
