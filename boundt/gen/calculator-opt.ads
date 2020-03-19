-- Calculator.Opt (decl)
--
-- Command-line options that control the calculator use
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
-- $Revision: 1.14 $
-- $Date: 2015/10/24 19:36:47 $
--
-- $Log: calculator-opt.ads,v $
-- Revision 1.14  2015/10/24 19:36:47  niklas
-- Moved to free licence.
--
-- Revision 1.13  2011-09-08 09:04:40  niklas
-- Increased Max_Int_Calc_Opt.Default from 40_000_000 to 5_000_000_000,
-- to cover all 32-bit values.
--
-- Revision 1.12  2011-08-31 04:23:34  niklas
-- BT-CH-0222: Option registry. Option -dump. External help files.
--
-- Revision 1.11  2009-11-27 11:28:07  niklas
-- BT-CH-0184: Bit-widths, Word_T, failed modular analysis.
--
-- Revision 1.10  2008/04/26 19:19:44  niklas
-- BT-CH-0124: Joint loop counters and induction variables.
--
-- Revision 1.9  2008/04/22 12:43:52  niklas
-- Added option Find_Null_Flow.
--
-- Revision 1.8  2006/04/10 08:15:09  niklas
-- Moved the options for maximum calculator value from
-- Arithmetic.Opt to Calculator.Opt.
--
-- Revision 1.7  2005/09/12 19:02:58  niklas
-- BT-CH-0008.
--
-- Revision 1.6  2004/05/01 20:14:00  niklas
-- First Tidorum version.
-- Added option Keep_Files.
--
-- Revision 1.5  2001/09/28 09:15:05  holsti
-- Platform-dependent parts moved to Calculator.Platform.
--
-- Revision 1.4  2001/01/07 22:00:22  holsti
-- Trace_Comments added.
--
-- Revision 1.3  2000/08/20 21:00:31  holsti
-- Trace_IO added.
--
-- Revision 1.2  2000/06/16 09:23:53  saarinen
-- Added calculator input and output filenames for debugging
--
-- Revision 1.1  2000/05/17 14:32:30  saarinen
-- Initial implementation
--


with Arithmetic;
with Calculator.Platform;
with Options;
with Options.Bool;
with Options.Strings;
with Options.Width;


package Calculator.Opt is


   Prog_Calculator : aliased Options.Strings.Option_T :=
      Options.Strings.Set (Calculator.Platform.Program);
   --
   -- The name of (path to) the calculator program.
   -- It can be an absolute path, or just a name. In the latter
   -- case, the user's command-lookup path is used to find the
   -- program.


   package Numeric is new Options.Integer_Valued (Arithmetic.Value_T);
   --
   -- Options with values of type Arithmetic.Value_T.
   
   
   Max_Int_Calc_Opt : aliased Numeric.Option_T (Default => 5_000_000_000);
   --
   -- The range of literal values that is generated in the arithmetic
   -- model for resolution by the Calculator. The Calculator has certain
   -- limits on its internal computations. Since the internal computations
   -- may transform input literals to larger values, the input bounds may
   -- have to be smaller than the internal limits, in a way that depends
   -- on the calculations performed by the subprogram.
   --
   Max_Int_Calc : Arithmetic.Value_T renames Max_Int_Calc_Opt.Value;


   function Min_Int_Calc return Arithmetic.Value_T;
   --
   -- The lower bound of the calculable value range.
   -- Currently equals -Max_Int_Calc.


   Warn_Large_Literal_Opt : aliased Options.Bool.Option_T (Default => False);
   --
   -- Whether to emit a warning when a literal value exceeds the range
   -- Min_Int_Calc .. Max_Int_Calc and is therefore translated to an
   -- unknown value for the calculator.
   --
   Warn_Large_Literal : Boolean renames Warn_Large_Literal_Opt.Value;


   Max_Modular_Width_Opt : aliased Options.Width.Option_T (Default => 0);
   --
   -- The maximum number width (number of bits) for which comparison
   -- relations (<, <=, =, >=, >) are modelled with modular arithmetic.
   -- For wider numbers comparisons are modelled with the unbounded
   -- (un-modular) integer comparisons.
   --
   -- If the value is zero, modular arithmetic is not used at all.
   --
   Max_Modular_Width : Arithmetic.Width_T renames Max_Modular_Width_Opt.Value;


   Warn_Large_Width_Opt : aliased Options.Bool.Option_T (Default => False);
   --
   -- Whether to emit a warning when a comparison relation is applied
   -- to operands of width larger than Max_Modular_Width, and is
   -- therefore modelled as a comparison of unbounded integers rather
   -- than modular integers bounded by this width.
   --
   Warn_Large_Width : Boolean renames Warn_Large_Width_Opt.Value;


   Use_Convex_Hull_Opt : aliased Options.Bool.Option_T (Default => False);
   --
   -- Whether to use the (true) ConvexHull function to compute
   -- the hull of a one-dimensional set (Hull_Bound).
   --
   -- The alternative is to use the rough-but-quick Hull function,
   -- which may give an over-approximation (a loose hull).
   --
   Use_Convex_Hull : Boolean renames Use_Convex_Hull_Opt.Value;


   type Limit_Method_T is (Hull, Search);
   --
   -- The available methods for computing the limits of a one-dimensional
   -- pool of values.
   --
   -- Hull
   --    Use the Hull_Bound of the pool.
   -- Search
   --    Use a binary search and null-set test.
   --    This is useful if the current Omega Calculator has bugs
   --    in the Hull function.


   package Limit_Method_Valued is new Options.Discrete_Valued (
      Value_Type  => Limit_Method_T,
      Value_Image => Limit_Method_T'Image);
   --
   -- Options with values of type Limit_Method_T.


   Limit_Method : aliased Limit_Method_Valued.Option_T (Default => Search);
   --
   -- How to compute the Smallest_Value/Largest_Value of a
   -- one-dimensional pool.


   Initial_Limit_Sill_Opt : aliased Numeric.Option_T (Default => 5_000);
   --
   -- The initial value for the upper (lower) bound in the binary search
   -- for the Smallest (Largest) Value that is a member of a given
   -- one-dimensional pool. The search starts by a halving/doubling
   -- phase, to avoid using very large numbers in the calculator when
   -- not necessary. The specified Min (Max) is added to this Sill
   -- to give the actual upper (lower) bound.
   --
   Initial_Limit_Sill : Arithmetic.Value_T := Initial_Limit_Sill_Opt.Value;


   Check_Hull_Limit_Opt : aliased Options.Bool.Option_T (Default => True);
   --
   -- Whether to check that a limit (upper or lower bound) on a set,
   -- computed using the "hull" or "convex hull" functions, is really
   -- a member of the set. The check may well fail for "hull", which is
   -- defined as a possibly over-estimated hull, while the check may
   -- fail for "convex hull" if the function meets with numerical
   -- overflow. If the check fails, the true limit is found by the
   -- binary search method.
   --
   Check_Hull_Limit : Boolean renames Check_Hull_Limit_Opt.Value;


   Warn_Hull_Loose_Opt : aliased Options.Bool.Option_T (Default => True);
   --
   -- Whether to warn if the hull or convex-hull of a set if found to
   -- be "loose" in the sense that a limit of the hull is not a member
   -- of the set.
   --
   Warn_Hull_Loose : Boolean renames Warn_Hull_Loose_Opt.Value;


   Trace_IO_Opt : aliased Options.Bool.Option_T (Default => False);
   --
   -- Whether to trace input commands to the calculator engine
   -- and outputs from the engine in real time, interactively.
   -- The inputs and outputs are always logged (see below),
   -- but the real-time trace is optional.
   --
   Trace_IO : Boolean renames Trace_IO_Opt.Value;


   Trace_Comments_Opt : aliased Options.Bool.Option_T (Default => False);
   --
   -- Whether to trace, on standard output, comments supplied to annotate
   -- the calculation. These comments are always included in the input
   -- and output, which are always logged (see below), but the real-time
   -- trace on standard output is optional.
   --
   Trace_Comments : Boolean renames Trace_Comments_Opt.Value;


   Keep_Files_Opt : aliased Options.Bool.Option_T (Default => False);
   --
   -- Whether the calculator-engine input and output files should
   -- be kept as permanent disk files at the end of the run.
   --
   Keep_Files : Boolean renames Keep_Files_Opt.Value;


   Calc_Input_File : aliased Options.Strings.Option_T :=
      Options.Strings.Set ("omega_in");
   --
   -- The prefix of the name for the file that will be created to
   -- hold a log of all inputs to the calculator-engine.
   -- Significant if Keep_Files = True.


   Calc_Output_File : aliased Options.Strings.Option_T :=
      Options.Strings.Set ("omega_out");
   --
   -- The prefix of the name for the file that will be created to
   -- hold a log of all outputs from the calculator-engine.
   -- Significant if Keep_Files = True.

   -- The actual log files consist of the above file-name prefixes
   -- extended with a sequential numbering of the calculator-engine
   -- instances started during the Bound-T run.


   Join_Steps_Opt : aliased Options.Bool.Option_T (Default => True);
   --
   -- Whether to join consecutive steps in a node, into a joint
   -- data-flow-relation (joint arithmetic effect), covering as
   -- many steps as possible. The aim is to simplify the Omega
   -- expression of the data-flow by reducing the number of "copy"
   -- constraints and the number of joined relations.
   --
   Join_Steps : Boolean renames Join_Steps_Opt.Value;


   Find_Null_Flow_Opt : aliased Options.Bool.Option_T (Default => False);
   --
   -- Whether to inspect each "flow" set, for each flow-graph edge,
   -- to see if it is the null set, and if so to mark that edge
   -- as infeasible.
   --
   Find_Null_Flow : Boolean renames Find_Null_Flow_Opt.Value;


end Calculator.Opt;
