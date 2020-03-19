-- Bounds (body)
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
-- $Revision: 1.115 $
-- $Date: 2015/10/24 20:05:46 $
--
-- $Log: bounds.adb,v $
-- Revision 1.115  2015/10/24 20:05:46  niklas
-- Moved to free licence.
--
-- Revision 1.114  2014/07/01 22:07:49  niklas
-- Added some trace output to Bound_Time_And_Space.
--
-- Revision 1.113  2013/12/23 21:19:04  niklas
-- Provide assertions to SWEET.Resolve_Dynamic_Jumps.
--
-- Revision 1.112  2013/12/20 21:20:55  niklas
-- Modified Avoid_Arithmetic_Analysis not to report dynamic flow as
-- needing arithmetic analysis, if SWEET is included.
--
-- Revision 1.111  2013/12/12 22:26:04  niklas
-- BT-CH-0262: Corrections to new value-origin analysis.
--
-- Revision 1.110  2013/12/08 22:05:57  niklas
-- BT-CH-0259: Storing value-origin analysis results in execution bounds.
--
-- Revision 1.109  2013-12-01 20:14:33  niklas
-- Updated for changes to opt/sweet, for i8051_4b3.
--
-- Revision 1.108  2013-02-03 21:06:48  niklas
-- BT-CH-0239: SWEET for dynamic flow analysis - step 1.
--
-- Revision 1.107  2011-10-18 20:19:49  niklas
-- Updated to provide the Subprogram as a parameter to
-- Processor.Properties.Entry_Bounds, an update required by
-- the ALF export function.
--
-- Revision 1.106  2011-08-31 04:23:33  niklas
-- BT-CH-0222: Option registry. Option -dump. External help files.
--
-- Revision 1.105  2009-11-27 11:28:06  niklas
-- BT-CH-0184: Bit-widths, Word_T, failed modular analysis.
--
-- Revision 1.104  2009-10-07 19:26:09  niklas
-- BT-CH-0183: Cell-sets are a tagged-type class.
--
-- Revision 1.103  2009-04-16 19:46:01  niklas
-- Expanded "others" Goals in calls of Flow.Origins.Propagate.
--
-- Revision 1.102  2008/12/25 08:59:33  niklas
-- Removed unused context clauses and local variables.
--
-- Revision 1.101  2008/11/09 21:41:57  niklas
-- BT-CH-0158: Option "-trace instr".
--
-- Revision 1.100  2008/11/02 08:46:52  niklas
-- BT-CH-0154: Assertions on mobile variables correctly used.
--
-- Revision 1.99  2008/10/19 10:12:05  niklas
-- Improved a trace heading in Bound_Execution.
--
-- Revision 1.98  2008/09/24 08:38:52  niklas
-- BT-CH-0146: Assertions on "loop starts <bound> times".
-- BT-CH-0146: Loop-repeat assertions set both lower and upper bound.
-- BT-CH-0146: Report locations of contradictory "count" assertions.
-- BT-CH-0146: Contradictory "count" assertions imply infeasibility.
--
-- Revision 1.97  2008/09/20 12:41:50  niklas
-- BT-CH-0145: No error re too few assertion matches if graph is growing.
--
-- Revision 1.96  2008/07/28 19:23:45  niklas
-- BT-CH-0140: Detect contradictory execution-count bounds.
--
-- Revision 1.95  2008/07/23 09:07:15  niklas
-- BT-CH-0139: Fix recursion in Programs.Execution.Paths.
--
-- Revision 1.94  2008/07/14 19:16:55  niklas
-- BT-CH-0135: Assertions on "instructions".
--
-- Revision 1.93  2008/04/26 19:19:43  niklas
-- BT-CH-0124: Joint loop counters and induction variables.
--
-- Revision 1.92  2008/03/05 18:45:30  niklas
-- BT-CH-0120: Allow several dynamically conditional edges from a step.
--
-- Revision 1.91  2008/02/27 14:58:48  niklas
-- BT-CH-0116: Call-specific time and stack assertions.
--
-- Revision 1.90  2008/01/31 21:57:44  niklas
-- BT-CH-0108: Fixes to BT-CH-0098.
--
-- Revision 1.89  2007/12/22 15:23:46  niklas
-- BT-CH-0101: Option "-trace graph".
--
-- Revision 1.88  2007/12/21 13:32:53  niklas
-- Extended Bound_Execution to use (new) Bound_As_Infeasible to define
-- some dummy bounds (eg. empty cell-sets) for an infeasible subprogram.
--
-- Revision 1.87  2007/12/17 13:54:35  niklas
-- BT-CH-0098: Assertions on stack usage and final stack height, etc.
--
-- Revision 1.86  2007/11/12 21:37:27  niklas
-- BT-CH-0097: Only arithmetic analysis marks boundable edge domain.
--
-- Revision 1.85  2007/10/31 12:16:00  niklas
-- BT-CH-0095: Arithmetic analysis of "live" dynamic data refs.
--
-- Revision 1.84  2007/10/28 09:32:45  niklas
-- BT-CH-0092: Arithmetic analysis of dynamic data refs is optional.
--
-- Revision 1.83  2007/10/26 12:44:34  niklas
-- BT-CH-0091: Reanalyse a boundable edge only if its domain grows.
--
-- Revision 1.82  2007/10/11 12:00:22  niklas
-- Use Flow.Computation.Show.Report_Unresolved_Flow instead of the
-- like-named operation of Flow.Show. This suppresses unnecessary
-- error messages for unresolved but infeasible dynamic edges.
--
-- Revision 1.81  2007/10/02 20:47:10  niklas
-- Added list of basis cells to Trace_Arith output.
--
-- Revision 1.80  2007/08/20 07:56:38  niklas
-- Removed Add_Inputs_For_Unbounded_Calls (added in rev 1.75) because
-- they add unnecessary input cells (cells that are given values in the
-- subprogram, although not sufficient to bound the call).
--
-- Revision 1.79  2007/08/17 14:44:00  niklas
-- BT-CH-0074: Stable and Unstable stacks.
--
-- Revision 1.78  2007/08/10 06:55:09  niklas
-- Added Trace line as heading to Trace_Context.
--
-- Revision 1.77  2007/08/06 09:20:43  niklas
-- Added option Trace_Phase.
--
-- Revision 1.76  2007/08/03 19:10:48  niklas
-- Updated to use Programs.Show.
--
-- Revision 1.75  2007/07/09 13:55:12  niklas
-- Extended Bound_With_Arithmetic to Add_Inputs_For_Unbounded_Calls
-- when it stores the set of input cells in the execution bounds.
--
-- Revision 1.74  2007/02/13 20:14:20  Niklas
-- BT-CH-0044.
--
-- Revision 1.73  2007/01/13 13:51:02  niklas
-- BT-CH-0041.
--
-- Revision 1.72  2006/11/26 22:07:25  niklas
-- BT-CH-0039.
--
-- Revision 1.71  2006/11/20 20:20:18  niklas
-- BT-CH-0037.
--
-- Revision 1.70  2006/10/24 21:41:05  niklas
-- BT-CH-0030.
--
-- Revision 1.69  2006/10/24 08:44:29  niklas
-- BT-CH-0028.
--
-- Revision 1.68  2006/09/04 15:07:03  niklas
-- Added the option "-trace arith", as Bounds.Opt.Trace_Arith.
--
-- Revision 1.67  2006/08/22 13:16:56  niklas
-- Extended Finish_Bounds to call Decoder.Stop when decoding was
-- suspended for dynamic flow, but no further decoding was necessary.
-- This avoids a dangling "Decoding suspended" message.
-- Removed the unused Subprogram parameter from Show_IO_Cells, and
-- also the first Trace line (uninformative title).
--
-- Revision 1.66  2006/05/27 21:56:08  niklas
-- Updated for BT-CH-0020.
--
-- Revision 1.65  2006/05/26 15:15:34  niklas
-- Corrected Bound_With_Arithmetic to use the Call_Off index-shift
-- only for Into_Interest, not for Call_Steps. Calls_Steps'Range
-- is the same as Unbounded_Calls'Range, no shift there.
--
-- Revision 1.64  2005/10/20 19:34:00  niklas
-- BT-CH-0016.
--
-- Revision 1.63  2005/10/20 11:28:28  niklas
-- BT-CH-0015.
--
-- Revision 1.62  2005/09/20 10:03:15  niklas
-- Changed Bound_Execution.Apply_Assertions to apply loop-assertions
-- only to loops that are still feasible and feasibly repeatable and
-- not yet bounded. This avoids repeated application of the same
-- assertions to the same loop (after flow-graph pruning) and also
-- avoids repeated warnings about unreachable or unrepeatable loops.
--
-- Revision 1.61  2005/09/17 14:42:04  niklas
-- BT-CH-0009.
--
-- Revision 1.60  2005/09/14 12:00:38  niklas
-- Corrected Bound_With_Arithmetic to deal with the case of an empty
-- Basis cell-set, for which no arithmetic analysis is possible or
-- useful.
-- Extended Bound_Execution to display all the asserted and inherited
-- bounds on variable values if Bounds.Opt.Trace_Context is set.
--
-- Revision 1.59  2005/09/12 19:02:57  niklas
-- BT-CH-0008.
--
-- Revision 1.58  2005/09/03 11:50:27  niklas
-- BT-CH-0006.
--
-- Revision 1.57  2005/07/01 11:06:58  niklas
-- Added the exception Flow.False_Path to the exception handlers
-- for Calculator.Empty_Flux, since some analysis routines now use
-- the former exception to signal infeasibility. In Resolve_Protocol
-- removed Empty_Flux since it cannot occur now due to changes in
-- Bounds.Calling.Bound_Protocol.
-- Corrected the Note regarding iteration in Bound_With_Arithmetic.
--
-- Revision 1.56  2005/06/29 13:02:41  niklas
-- Added optional (-warn reach) warnings about unreachable
-- instructions and calls.
--
-- Revision 1.55  2005/06/29 09:37:05  niklas
-- Check and report conflicting asserted or computed cell-bounds
-- on entry to the subprogram under analysis.
--
-- Revision 1.54  2005/06/28 08:36:44  niklas
-- Updated for changes in Bounds.Opt.
-- Changed Show_IO_Cells to use the same order as in detailed
-- output: input cells, basis cells, output cells.
-- Removed Exec_Bounds parameter from Analyze_Irreducible_Subprogram
-- because the caller (Bound_Executions) does not need it.
-- Added procedure Find_Execution_Bounds to find existing (asserted)
-- execution bounds and avoid analysis of "stub" flow-graphs.
--
-- Revision 1.53  2005/06/14 17:05:48  niklas
-- Corrected Bound_With_Arithmetic, in its local procedures
-- Resolve_Data, Resolve_Edge_Cond, Resolve_Protocol and
-- Resolve_Flow, to constrain the flux into the relevant step
-- by all applicable assertions and by the Range_Pre elements
-- in the step's effect. This is done by applying the function
-- Assertion_Constrained to the flux from Calculator.Flux_To_Steps.
--
-- Revision 1.52  2005/05/09 15:34:48  niklas
-- Added value-origin analysis.
--
-- Revision 1.51  2005/04/18 10:51:29  niklas
-- Added call of Flow.Computation.Show.Report_Unresolved_Data.
--
-- Revision 1.50  2005/04/18 09:31:16  niklas
-- Added call to Flow.Show.Report_Unresolved_Flow.
--
-- Revision 1.49  2005/02/23 09:05:14  niklas
-- BT-CH-0005.
--
-- Revision 1.48  2005/02/20 15:15:35  niklas
-- BT-CH-0004.
--
-- Revision 1.47  2005/02/16 21:11:40  niklas
-- BT-CH-0002.
--
-- Revision 1.46  2004/05/02 06:04:53  niklas
-- First Tidorum version.
-- Added support for optional time analysis and optional space analysis, in
-- particular in Avoid_Arithmetic_Analysis.
-- Added support for variables that are held in different cells depending
-- on code address (Storage.Location).
-- Tolerating irreducible subprograms with Analyze_Irreducible_Subprogram.
-- Added constant propagation using Flow.Const in Launch_And_Propagate, but
-- so far only on the universal level (no call path dependency) and after
-- all dynamic flow is resolved.
-- Using dynamism resolution operations from Flow.Dynamic.
-- Added support for dynamic flow edges (new form of dynamic flow).
-- Extended Nesting_Bounded and Assertion_Constrained to include Range_Pre
-- constraints.
-- Updated for changes in the Programs family. In particular, extended and
-- improved the stack usage analysis, separating Bound_Local_Stack_Height,
-- Bound_Take_Off_Height and Bound_Total_Stack_Usage.
-- Added support for assertions on subprogram input parameters.
-- Taking Cell_T stuff from Storage, not from Arithmetic.
-- Using Output.Trace for trace output.
-- Bound_Jumps_And_Execution (Subprogram_Set_T) now places a locus nesting
-- mark for the subprogram to be analyzed.
-- Added several Calculator.Comment calls.
--
-- Revision 1.45  2003/03/11 08:31:34  holsti
-- Using execution-bounds types from Programs.Execution.
-- Added calculator comments for loop-counter checks.
--
-- Revision 1.44  2003/02/27 14:37:15  holsti
-- Some warnings are conditional on Opt.Warn_Unresolved_Data.
--
-- Revision 1.43  2002/11/30 10:47:13  holsti
-- Changed Bound_Jumps_And_Execution (for subprogram sets) to delay
-- bounding of any caller of a "resolved" subprogram, even if the caller
-- has unresolved dynamic flow. This fixes NC_0147.
-- Moved Report_Recursion_Cycle to the package level for readability.
--
-- Revision 1.42  2002/11/29 11:02:37  holsti
-- Using Programs.Sort instead of local code.
--
-- Revision 1.41  2002/03/21 09:13:49  saarinen
-- Fixed NC_116.
--
-- Revision 1.40  2001/12/14 10:49:49  saarinen
-- Call dependent analysis limited by option 'max_dependency_depth'.
--
-- Revision 1.39  2001/12/10 13:00:07  holsti
-- When a call provides no new context, its execution bounds refer
-- to the existing (not fully bounded) execution bounds.
--
-- Bound_Asserted_Calls has Program parameter because some operations
-- in Programs now require it for indexing of execution bound objects.
--
-- Calculator.Comments added in Resolve_Flow and Resolve_Data.
--
-- Revision 1.38  2001/11/19 10:57:08  saarinen
-- Modified for Bound-T/ERC32:
-- Removed Compute_Node_Times.
--
-- Revision 1.37  2001/06/23 11:44:37  holsti
-- Call_Count assertion applied for all calls (NC_135).
--
-- Revision 1.36  2001/05/20 13:36:43  holsti
-- Finishes flow-graphs with  Decoder.Finish (NC_117).
-- Computes node execution times with Decoder.Power, Flow.Work,
-- and Processor.Time_To_Finish (NC_120, NC_121, NC_122, NC_128).
--
-- Revision 1.35  2001/04/14 17:17:12  holsti
-- Warn here about unresolved dynamic accesses.
-- Warning re asserted bounds overriding computed bounds changed.
--
-- Revision 1.34  2001/03/21 20:18:22  holsti
-- Output with Locus_T. Some reorganization, too.
--
-- Revision 1.33  2001/03/16 09:37:06  ville
-- Effort function provided by Flows package used
--
-- Revision 1.32  2001/03/15 20:48:32  holsti
-- Exit bounding-loop when a resolved subprogram is called.
--
-- Revision 1.31  2001/03/15 07:32:41  holsti
-- Loop approximation restricted to Basis cells.
--
-- Revision 1.30  2001/03/10 22:39:02  holsti
-- Option Show_Cell_Sets (-trace io) added. Notes modified.
--
-- Revision 1.29  2001/03/10 00:54:10  holsti
-- Extensive changes to handle effects of calls (input and ouptut cells)
-- and to reduce the size of the basis cell-set.
-- Adaptations to the new dynamic parameter-mapping for calls.
-- Adaptations to compute and store input and output cell-sets
-- and initial bounds for execution bounds.
-- NC_098 corrected at least partially.
--
-- Revision 1.28  2001/02/19 15:16:50  holsti
-- Avoid arithmetic analysis, if possible or required.
--
-- Revision 1.27  2001/02/19 09:44:44  holsti
-- Adapted to changes in Programs.Execution_Bounds_Ref.
-- Execution bounds are never attached to a call, but always to
-- a call-path (null for universal bounds) and stored with the
-- subprogram.
-- All derived execution bounds are stored, whether complete or not.
-- Some editorial changes in output formats.
--
-- Revision 1.26  2001/02/14 06:49:17  holsti
-- Option to trace counters added.
--
-- Revision 1.25  2001/01/19 08:54:47  saarinen
-- Call to Flux_To_Step modied according to changes in calculator.
-- Omega is not used for analysis for subprograms without loops,
-- call or dynamics.
--
-- Revision 1.24  2001/01/13 11:11:31  holsti
-- Adapted to changes in Calculator of this date.
--
-- Revision 1.23  2001/01/07 22:18:43  holsti
-- Live-cell analysis added.
-- Comments on calculation-steps added.
-- Trace of param framing and values added (Show_Parameters).
-- Deleted "to-be" remarks in favour of non-conformances.
--
-- Revision 1.22  2001/01/04 09:57:48  saarinen
-- Fixed NC_074.
--
-- Revision 1.21  2000/12/28 19:05:14  holsti
-- Medium-important improvements as follows.
-- General description updated.
-- Editorial improvements in comments and formatting.
-- Bound_Call uses both Min and Max of asserted call-count (NC_030).
-- Call_Level parameters moved to suit their "in" mode.
-- Check_Step sets Const_Step in a simpler way.
-- Unused parameters of Bound_Loops (Graph, Root_Flux) removed.
-- Bound_Loops clarified with new local variables and subprograms.
-- Bound_Call_With_Context clarified with new local subprograms.
-- Bound_Call_With_Context displays Call_Level in note.
-- Bound_Call_With_Context checks that callee is static.
-- Resolve_Dynamic_Flow uses Arithmetic.Void_Bound for empty flux.
-- Resolve_Dynamic_Data uses Arithmetic.Void_Bound for empty flux.
-- Use Loop_Cell_Set_T and Loop_Flux_T from Calculator (NC_040).
-- Use Programs.Some_Calls and Flow.Unresolved_Flow.
--
-- Revision 1.20  2000/12/28 14:05:37  saarinen
-- Fixed NC_046: Negative loop-bounds cause Constraint Error.
-- Fixed NC_051: Loop termination on counter equal to limit.
-- Does not call audition_for_counter with non-counter cells.
--
-- Revision 1.19  2000/12/21 14:37:53  sihvo
-- Minor changes in layout etc.
--
-- Revision 1.18  2000/12/05 15:48:01  holsti
-- Adapted to new names of Decoder framing functions.
-- The term "loop neck" replaces the overloaded "loop entry".
-- Decoder.Stack_Height_Cell replaces deleted Arithmetic function.
-- Absence of stack-height cell tolerated (then no stack bounds).
--
-- Revision 1.17  2000/11/29 14:58:51  holsti
-- Using Decoder.Duration instead of Processor.To_Time.
--
-- Revision 1.16  2000/11/29 13:50:24  saarinen
-- Cleaned procedure Update_Var_Cell_Set.
--
-- Revision 1.15  2000/11/24 12:06:00  sihvo
-- Added stack height analysis.
--
-- Revision 1.14  2000/11/24 10:13:08  saarinen
-- Calls to Flux_To_Steps modified.
-- Variable cells in subprograms are updated.
--
-- Revision 1.13  2000/11/22 22:42:13  holsti
-- Added function Time to compute the time of one node, using
-- the new function Processor.Effort (Step_Info_T).
--
-- Renamed Bound_Node_Times to Compute_Node_Times, and changed
-- it to use an Assertion_Map_T instead of an Assertion_Set_T,
-- in preparation for the use of a "property map".
--
-- Moved the two calls of Compute_Node_Times into one call in
-- Bound_Jumps_And_Execution (for one subprogram), so that the
-- assertion map is available for use as a parameter.
--
-- Report_Recursion_Cycle modified for new Programs functions,
-- robustness, layout, comments.
--
-- Minor commenting and layout improvements.
--
-- Revision 1.12  2000/11/14 10:39:32  sihvo
-- Added recursion reporting.
--
-- Revision 1.11  2000/11/09 14:48:17  saarinen
-- Fixed call dependent bounding.
-- Handles empty fluxes.
-- NC_020 fixed.
--
-- Revision 1.10  2000/10/26 10:01:23  saarinen
-- Fixed call-dependent bounding.
--
-- Revision 1.9  2000/10/19 11:17:08  saarinen
-- Using parameters for call-specific bounding.
-- Resolving indirect data accesses.
-- Delaying bounding of subprograms with dynamic-resolved callees.
--
-- Revision 1.8  2000/10/17 11:35:31  langback
-- Changed Bounds_Calls so that it works correctly with the return type
-- of Assertion.Call_Count
--
-- Revision 1.7  2000/10/06 14:10:47  saarinen
-- Modified loop bounding.
--
-- Revision 1.6  2000/09/20 18:54:43  saarinen
-- Added function Bound_Node_Times.
--
-- Revision 1.5  2000/08/18 18:19:30  holsti
-- Bound_Jumps_And_Execution includes To_Bound in sorting.
-- Programs.Output used to trace analysis.
--
-- Revision 1.4  2000/08/04 14:47:50  saarinen
-- Corrected some typos.
--
-- Revision 1.3  2000/07/25 03:13:59  holsti
-- First implementation (incomplete).
--
-- Revision 1.2  2000/07/12 12:24:31  holsti
-- Normalised indentation.
--


with Arithmetic;
with Arithmetic.Opt;
with Bounds.Calling;
with Bounds.Looping;
with Bounds.Opt;
with Bounds.Recursing;
with Bounds.Stacking;
with Bounds.Timing;
with Calculator;
with Decoder;
with Flow;
with Flow.Computation;
with Flow.Computation.Show;
with Flow.Const;
with Flow.Life;
with Flow.Origins;
with Flow.Origins.For_Flow;
with Flow.Origins.Overall_Invariants;
with Flow.Pruning;
with Flow.Pruning.Opt;
with Flow.Show;
with Loops;
with Loops.Show;
with Loops.Slim;
with Output;
with Processor;
with Processor.Properties;
with Programs.Execution;
with Programs.Show;
with Programs.Sort;
with Storage.Bounds.Show;
with Storage.List_Cell_Sets;
with SWEET;
with Symbols;


package body Bounds is
--
-- This package contains a number of operations with similar and
-- complex functionality, which may be difficult to understand and
-- organise in the reader's mind. The following summary of the calling
-- sequences and activities may help.
--
-- There are two public operations: Bound_Executions bounds a set of
-- subprograms, and Bound_Execution bounds one subprogram in a given
-- calling context.
--
-- When Bound_Executions is first used, the full set of subprograms has
-- perhaps not yet been discovered, if some subprograms contain unresolved
-- dynamic jumps.

-- Bound_Executions works bottom-up in the (currently known) call-graph
-- and computes execution bounds for subprograms that do not have open
-- (unresolved) dynamic jumps. If it encounters a subprogram with such
-- jumps, it uses the data-flow analysis to resolve the jumps and extend
-- the control-flow graph of the subprogram, and terminates there; the
-- caller is expected to trace out the extended control-flow graph as far
-- as it can, update the call-graph using the new flow-graph, and
-- continue the analysis by again calling Bound_Executions, for the set
-- of subprograms that were not yet bounded (thus including the one for
-- which the control-flow graph was extended).
--
-- When the data-flow analysis of a subprogram does not produce
-- execution bounds without knowledge about parameter values, the
-- subprogram is left without (generic) execution bounds, and is
-- considered "call-dependent". Later, when some caller subprogram is
-- bounded, the calling context is used to sharpen a new data-flow
-- analysis of the call-dependent subprogram, to give call-specific
-- execution bounds. This explains why Bound_Execution is self-recursive
-- via the procedure Bounds.Calling.Bound_Call_With_Context.
--
-- The algorithm in Bound_Executions is summarised as follows, including
-- all the operations called from this package but only the most important
-- operations called from other packages:
--
-- Bound_Executions (set of subprograms):
--
--    for each subprogram in bottom-up calling order:
--
--       Bound_Execution (one subprogram):
--
--          Programs.Execution.Bound_Initial_Values to record
--             the context- and assertion-derived bounds on the
--             values of cells on entry to the subprogram;
--
--          Programs.Execution.Bound_Call_Inputs to record the
--             the context- and assertion-derived bounds on the
--             inputs to calls to lower-level subprograms;
--
--          loop while the computation model improves but
--             dynamic edges are not resolved:
--
--             Flow.Origins.Propagate provides cell-value origins
--                for target-specific uses, for example to chain
--                single-word instructions thru carry bits into
--                multi-word arithmetic operations.
--
--             Flow.Const.Propagate propagates constant values around
--                the flow-graph to refine the computation model and
--                perhaps prune infeasible parts of the graph;
--
--             Flow.Origins.Propagate propagates value copies around
--                the flow-graph, here used to resolve boundable
--                jumps, especially jumps to the return address;
--
--             Assertions.Identify_Loops_And_Calls to map the user's
--                assertions to the subprogram's structure;
--
--             Bounds.Looping.Bound_Asserted_Starts to bound the loops
--                for which the user has asserted loop-start bounds;
--
--             Bounds.Looping.Bound_Asserted_Repeats to bound the loops
--                for which the user has asserted loop-repeat bounds;
--
--             Bounds.Calling.Bound_Asserted_Calls to bound the calls
--                for which the user has asserted a worst-case time;
--
--             Abstract Execution using SWEET to resolve boundable jumps,
--                if the SWEET option is included and options are set
--                to use it for resolving dynamic jumps;
--
--             Avoid_Arithmetic_Analysis to see if arithmetic
--                analysis is still required;
--
--             if arithmetic analysis is still required and enabled:
--
--                Bound_With_Arithmetic (see below)
--
--                exit loop when the computation model is stable
--                  or dynamic edges were resolved
--
--             else:
--
--                Bound_Without_Arithmetic (see below)
--
--                exit loop
--
--          end loop
--
--
-- Bound_With_Arithmetic:
--
--    Flow.Life.Propagate identifies the "live" assignments;
--
--    Loops.Approximate_Loops and
--    Calculator.Pool_To_Steps for data-flow analysis;
--
--    for all steps with dynamic data references:
--       Arithmetic.Bound_References to resolve them;
--
--    for all edges with dynamic data references:
--       Arithmetic.Reference_Bounded to resolve them;
--
--    for all calls with a dynamic calling protocol:
--       Bounds.Calling.Bound_Protocol to resolve it, using
--          the arithmetic flux into the call.
--
--    if no new dynamic data references were resolved:
--
--       for each dynamic edge in the flow-graph:
--          Flow.Apply to resolve the edge;
--
--       if no extension of flow-graph:
--
--          Flow.Remove_All_Dynamic_Edges;
--
--          for each unbounded, feasible loop:
--             Nesting_Bounded for assertions on "into" flux;
--             Nesting_Bounded for assertions on repeat-edge flux;
--             Bounds.Looping.Bound_Loop
--
--          for each unbounded, feasible call:
--             Nesting_Bounded for assertions on "into" flux;
--             Bounds.Calling.Bound_Call
--
--          if stack bounds desired:
--             Bounds.Stacking.Bound_Local_Stack_Height
--             Bounds.Stacking.Bound_Take_Off_Height
--
--
-- Bound_Without_Arithmetic:
--
--    for all calls with dynamic calling protocol:
--       Bounds.Calling.Bound_Protocol to resolve it,
--          using all assertions applicable to this call.
--
--    for each unbounded, feasible call:
--       Bounds.Calling.Bound_Call to bound the callee,
--          using all assertions applicable to this call.
--
-- This completes the call-sequence summary.


   use type Programs.Call_Path_T;


   function Fully_Bounded (Item : Programs.Execution.Bounds_Ref)
   return Boolean
   --
   -- Whether the bounds are fully bounded with respect to the
   -- desired execution measures as defined in Bounds.Opt.
   --
   renames Programs.Execution.Bounded;


   function Nesting_Bounded_Flux (
      Step    : Flow.Step_T;
      Flux    : Calculator.Flux_T;
      Living  : Flow.Life.Living_T;
      Luups   : Loops.Loop_List_T;
      Asserts : Assertions.Assertion_Map_T)
   return Calculator.Flux_T
   --
   -- The given Flux that enters the given Step, range-constrained with all
   -- Asserted cell-bounds for all Luups that contain the Step (including
   -- the assertions for the Step itself, if it is a loop-head), plus
   -- any Range_Pre assignment constraints in the effect of the Step
   -- itself, under the given Living computation model.
   --
   is

      Point : constant Processor.Code_Address_T := Flow.Prime_Address (Step);
      -- The code address of the step, for mapping variables to cells.

      Node : constant Flow.Node_T :=
         Flow.Node_Containing (Step, Flow.Life.Graph (Living));
      -- The node that contains the given step.

      Loop_Bounds : constant Storage.Bounds.Cell_Interval_List_T :=
         Assertions.Loop_Nest_Values (
            Luups   => Loops.Containing_Loops (Luups, Node),
            Point   => Point,
            Asserts => Asserts);
      -- The asserted variable (cell) bounds for those Luups that
      -- contain this Node, mapped to cells at this Point.

      Result : Calculator.Flux_T := Flux;
      -- The result, initialised to the given (raw) flux.

   begin

      -- Apply assertions from loops:

      if Loop_Bounds'Length > 0 then
         -- Some assertions given for containing loops.

         Calculator.Comment (
            Text =>"Using assertions from containing loops",
            Calc => Calculator.Owner_Of (Flux));

         Result := Calculator.Range_Bounded_Flux (
            Flux   => Result,
            Bounds => Loop_Bounds);

      end if;

      -- Apply Range_Pre constraints in the effect of the step:

      Result := Calculator.Range_Bounded_Flux (
         Flux => Result,
         Pre  => Flow.Life.Live_Effect (Step, Living));

      return Result;

   end Nesting_Bounded_Flux;


   function Nesting_Bounded_Pool (
      Step    : Flow.Step_T;
      Pool    : Calculator.Pool_T;
      Living  : Flow.Life.Living_T;
      Luups   : Loops.Loop_List_T;
      Asserts : Assertions.Assertion_Map_T)
   return Calculator.Pool_T
   --
   -- The given Pool that enters the given Step, constrained with all
   -- Asserted cell-bounds for all Luups that contain the Step (including
   -- the assertions for the Step itself, if it is a loop-head), plus
   -- any Range_Pre assignment constraints in the effect of the Step
   -- itself, under the given Living computation model.
   --
   is

      Point : constant Processor.Code_Address_T := Flow.Prime_Address (Step);
      -- The code address of the step, for mapping variables to cells.

      Node : constant Flow.Node_T :=
         Flow.Node_Containing (Step, Flow.Life.Graph (Living));
      -- The node that contains the given step.

      Loop_Bounds : constant Storage.Bounds.Cell_Interval_List_T :=
         Assertions.Loop_Nest_Values (
            Luups   => Loops.Containing_Loops (Luups, Node),
            Point   => Point,
            Asserts => Asserts);
      -- The asserted variable (cell) bounds for those Luups that
      -- contain this Node, mapped to cells at this Point.

      Result : Calculator.Pool_T := Pool;
      -- The result, initialised to the given (raw) pool.

   begin

      -- Apply assertions from loops:

      if Loop_Bounds'Length > 0 then
         -- Some assertions given for containing loops.

         Calculator.Comment (
            Text => "Using assertions from containing loop.",
            Calc => Calculator.Owner_Of (Pool));

         Result := Calculator.Bounded_Pool (
            Pool   => Result,
            Bounds => Loop_Bounds);

      end if;

      -- Apply Range_Pre constraints in the effect of the step:

      Result := Calculator.Bounded_Pool (
         Pool => Result,
         Pre  => Flow.Life.Live_Effect (Step, Living));

      return Result;

   end Nesting_Bounded_Pool;


   --
   -- Auxiliary functions for gaining subprogram data:
   --


   procedure Avoid_Arithmetic_Analysis (
      Exec_Bounds : in     Programs.Execution.Bounds_Ref;
      Choice      : in     Arithmetic.Opt.Choice_T;
      Chosen      :    out Boolean)
   --
   -- Decides whether it is necessary to apply arithmetic analysis
   -- for bounding this subprogram further.
   --
   -- If arithmetic analysis is not enabled for this subprogram (by
   -- option or assertion), but the subprogram contains some feature
   -- that would require such analysis, these features are reported
   -- as errors.
   --
   -- If arithmetic analysis is not needed for this subprogran, but
   -- is enforced (by option or assertion), a note is issued.
   --
   -- We assume that we have already applied all assertions on
   -- execution time, stack usage, loop bounds, zero execution
   -- count (infeasibility), and variable values on this subprogram,
   -- its loops, and its calls. Moreover, we assume that less costly
   -- forms of analysis (constant propagation, value-origin analysis)
   -- have been applied, too, so that the only "unbounded" parts left
   -- are those that need arithmetic analysis.
   --
   -- For time-bounds, arithmetic analysis is needed if the subprogram
   -- is not yet time-bounded (by an assertion or because it inherits
   -- time-bounds from a shallower context) and one or more of the
   -- following occur in the subprogram (as feasible):
   --
   -- > An unresolved boundable jump or call. (Note that this can
   --   occur only in a context-free, universal analysis.)
   --
   -- > An unbounded but finite (non-eternal) loop. (Also means that
   --   the flow-graph is reducible.)
   --
   -- > A call where the execution time Depends on context and
   --   TBA the present (caller) subprogram provides some relevant
   --   context (contains origins of input cells for the call).
   --
   -- For stack-bounds, arithmetic analysis is needed if the subprogram
   -- is not yet stack-bounded (by an assertion or because it inherits
   -- stack-bounds from a shallower context) and if one or more of the
   -- following occur in the subprogram (as feasible); these conditions
   -- are checked separately for each stack that is not yet bounded:
   --
   -- > An unresolved boundable jump or call. (Note that this can
   --   occur only in a context-free, universal analysis.)
   --
   -- > A call where the stack usage Depends on context and
   --   TBA the present (caller) subprogram provides some relevant
   --   context (contains origins of input cells for the call).
   --
   -- > TBA: A call with an unbounded take-off height where the callee
   --   is not stack-vague.
   --
   -- The presence of dynamic calling protocols in the calls from the
   -- subprogram is not, in itself, a reason for arithmetic analysis.
   --
   -- The presence of unresolved dynamic memory references in the
   -- subprogram is not, in itself, a reason for arithmetic analysis.
   --
   is
      use Arithmetic.Opt;
      use Programs.Execution;
      use type Flow.Edge_Resolution_T;
      use type Storage.Cell_T;

      Unresolved_Flow : constant Boolean :=
         Unstable_Dynamic_Edges (Exec_Bounds)'Length > 0;
      --
      -- Dynamic flow requires arithmetic analysis to be resolved
      -- for any sort of analysis.

      Loose_Loops : constant Boolean :=
         Opt.Bound_Time
         and then Unbounded_Loops (
            Within  => Exec_Bounds,
            Eternal => False)'Length > 0;
      --
      -- Any loop that is not yet bounded, but is finite and thus
      -- potentially boundable by anaysis, requires arithmetic
      -- analysis if the execution time should be bounded.

      Stack_Mods : constant Boolean :=
         Opt.Bound_Stack
         and then Unbounded_Stack_Steps (Exec_Bounds)'Length > 0;
      --
      -- Any step that modifies the stack pointer requires arithmetic
      -- analysis, if the stack usage should be bounded and has not
      -- already been bounded (by constant propagation).

      Input_Dep_Calls : constant Boolean :=
         Input_Dependent_Calls (Exec_Bounds)'Length > 0;
      --
      -- Some (unbounded) calls may benefit from context-dependent
      -- analysis that provides bounds on inputs (parameters).

      Needed : constant Boolean :=
            Unresolved_Flow
         or Loose_Loops
         or Stack_Mods
         or Input_Dep_Calls;
      --
      -- Whether arithmetic analysis is needed.


      procedure Why (What : in String)
      --
      -- Explain why arithmetic analysis needed.
      --
      is
         What_Needs : constant String := What & " arithmetic analysis.";
      begin

         if Choice = Disabled then

            Output.Error (What_Needs);

         else

            Output.Trace (What_Needs);

         end if;

      end Why;


   begin  -- Avoid_Arithmetic_Analysis

      if Needed and (Choice = Disabled or Opt.Trace_Arith) then

         -- Report why arithmetic analysis is required:

         if Unresolved_Flow and not SWEET.Included then

            Why ("Dynamic flow needs");

         end if;

         if Loose_Loops then

            Why ("Loops need");

         end if;

         if Stack_Mods then

            Why ("Stack usage needs");

         end if;

         if Input_Dep_Calls then

            Why ("Calls need");

         end if;

      end if;

      if not Needed then

         if Choice = Enforced then

            Output.Note ("Arithmetic analysis is enforced.");

         else

            Output.Note ("Arithmetic analysis is not needed.");

         end if;

      end if;

      Chosen :=  Choice = Enforced
             or (Choice = Automatic and Needed);

   end Avoid_Arithmetic_Analysis;


   procedure Show_IO_Cells (
      Basis   : in Storage.Cell_Set_T;
      Inputs  : in Storage.Cell_Set_T;
      Outputs : in Storage.Cell_Set_T)
   --
   -- Displays the sets of basis cells, input cells, and output
   -- to be used for bounding the subprogram on the current
   -- call path (assumed to be defined as the current output locus).
   --
   is
   begin

      Output.Trace (Text =>
           "Input cells"
         & Output.Field_Separator
         & Storage.Image (Inputs));

      Output.Trace (Text =>
           "Basis cells"
         & Output.Field_Separator
         & Storage.Image (Basis));

      Output.Trace (Text =>
           "Output cells"
         & Output.Field_Separator
         & Storage.Image (Outputs));

   end Show_IO_Cells;


   procedure Set_IO_Cells (
      Inputs  : in Storage.Cell_Set_T;
      Outputs : in Storage.Cell_Set_T;
      Basis   : in Storage.Cell_Set_T;
      Within  : in Programs.Execution.Bounds_Ref)
   --
   -- Stores the sets of input and output cells as well as the
   -- calculation-basis set Within the given execution bounds, and
   -- optionally displays the cell sets.
   --
   is
   begin

      Programs.Execution.Set_Input_Cells (
         To     => Inputs,
         Within => Within);

      Programs.Execution.Set_Output_Cells (
         To     => Outputs,
         Within => Within);

      Programs.Execution.Set_Basis_Cells (
         To     => Basis,
         Within => Within);

      if Opt.Trace_Cell_Sets then

         Show_IO_Cells (
            Basis   => Basis,
            Inputs  => Inputs,
            Outputs => Outputs);

      end if;

   end Set_IO_Cells;


   procedure Mark_Unreachable (
      Step  : in Flow.Step_T;
      Model : in Flow.Computation.Model_Handle_T;
      Kind  : in String := "instruction")
   --
   -- Marks the Step as unreachable (infeasible) under the
   -- given Model and prunes the Model.
   --
   is
   begin

      if Flow.Pruning.Opt.Warn_Unreachable then

         Output.Warning ("Unreachable " & Kind);

      end if;

      Flow.Computation.Mark_Infeasible (Step, Model.all);

      Flow.Computation.Prune (Model.all);

   end Mark_Unreachable;


   procedure Trace_Nubs (
      Kind    : in String;
      Nubs    : in Flow.Step_List_T;
      Program : in Programs.Program_T)
   --
   -- Shows that the Nubs need arithmetic analysis.
   --
   is

      Source : Symbols.Symbol_Table_T := Programs.Symbol_Table (Program);
      -- For output locus.

   begin

      for N in Nubs'Range loop

         Output.Trace (
            Locus => Flow.Show.Locus (Step => Nubs(N), Source => Source),
            Text  =>
                 Kind
               & " nub in step"
               & Flow.Step_Index_T'Image (Flow.Index (Nubs(N))));

      end loop;

   end Trace_Nubs;


   function Opt_Steps_With_Dynamic_Effect (
      Living : Flow.Life.Living_T)
   return Flow.Step_List_T
   --
   -- The feasible steps that have effects that contain "live"
   -- assignments with value expressions or condition expressions
   -- that contain dynamic data references that shall be subjected
   -- to arithmetic analysis.
   --
   is
      use Arithmetic.Opt;
   begin

      case Ref_Choice is

      when None =>

         return Flow.No_Steps;

      when Relevant =>

         return Flow.Life.Steps_With_Dynamic_Effect (Living);

      when All_Item =>

         return Flow.Computation.Steps_With_Dynamic_Effect (
                   Under => Flow.Life.Model (Living).all);

      end case;

   end Opt_Steps_With_Dynamic_Effect;


   function Opt_Edges_With_Dynamic_Condition (
      Model : Flow.Computation.Model_Ref)
   return Flow.Step_Edge_List_T
   --
   -- The step-edges that have preconditions that contain dynamic data
   -- references that shall be subjected to arithmetic analysis.
   --
   is
      use Arithmetic.Opt;
   begin

      case Ref_Choice is

      when None =>

         return Flow.No_Step_Edges;

      when Relevant
         | All_Item =>

         return Flow.Computation.Edges_With_Dynamic_Condition (Model);

      end case;

   end Opt_Edges_With_Dynamic_Condition;


   procedure Bound_With_Arithmetic (
      Subprogram  : in Programs.Subprogram_T;
      Call_Path   : in Programs.Call_Path_T;
      Initial     : in Storage.Bounds.Cell_Interval_List_T;
      Asserted    : in Storage.Bounds.Var_Interval_List_T;
      Inherit_Inv : in Storage.Cell_Set_T;
      Asserts     : in Assertions.Assertion_Set_T;
      Assert_Map  : in Assertions.Assertion_Map_T;
      Bounds_Set  : in Programs.Execution.Bounds_Set_T;
      Exec_Bounds : in Programs.Execution.Bounds_Ref)
   --
   -- Bounds the memory accesses and the execution of a subprogram
   -- using arithmetic analysis of the effects of its instructions.
   --
   -- It is assumed that all bounds that can be derived without
   -- arithmetic analysis are already entered in the execution
   -- bounds on entry; this includes asserted loop-bounds and
   -- asserted worst-case times for specific calls as well as
   -- bounds derived by other automatic analyses such as constant
   -- propagation. Only the remaining unresolved or unbounded parts
   -- of the subprogram are analysed here.
   --
   -- Input parameters:
   --
   -- Subprogram
   --    The subprogram to be subjected to arithmetic analysis.
   -- Call_Path
   --    A call path ending at the subprogram (for context-dependent
   --    analysis) or null (for universal analysis). If the call-path
   --    is not null, the Callee of the last element is Subprogram.
   -- Initial
   --    Initial bounds for some cells, valid on entry to the
   --    Subprogram, expressed in the subprogram's own frame (the
   --    callee frame when considering the last element on the
   --    Call_Path).
   -- Asserted
   --    Asserted bounds for some variables, valid throughout the
   --    execution of the Subprogram.
   -- Inherit_Inv
   --    Inherited set of invariant cells.
   -- Asserts
   --    Set of user-given assertions.
   -- Assert_Map
   --    Assertion map for the Subprogram and the Asserts set.
   -- Bounds_Set
   --    Set of execution bounds computer earlier.
   -- Exec_Bounds
   --    Execution bounds under construction for the Subprogram in
   --    this context.
   --
   -- Output parameters:
   --
   -- Subprogram
   --    The flow-graph may be extended with new loose edges, if
   --    the analysis was able to resolve some dynamic edges.
   -- Exec_Bounds
   --    Execution bounds for the Subprogram in this context, updated
   --    with new input/output/basis cell-sets from liveness analysis
   --    and new bounds from arithmetic analysis.
   -- Bounds_Set
   --    Set of execution bounds perhaps updated with context-specific
   --    bounds for the calls from the Subprogram to lower-level
   --    subprograms.
   --
   -- The overall result can be classified as follows:
   --
   -- New flow, when Dynamic_Flow (Exec_Bounds) = Growing
   --
   --    The control-flow graph for Subprogram was extended by bounding
   --    dynamic edges. No attempt was made to bound loops or calls.
   --    The new flow should be traced to complete the flow-graph and
   --    then Bound_Execution should be called again. The present
   --    Exec_Bounds are out of date and should be discarded.
   --
   -- New computation, when Computation_Changed (Exec_Bounds)
   --
   --    The computation model in Exec_Bounds was updated / improved by
   --    bounding dynamic data references in "live" assignments or
   --    by bounding dynamic calling protocols in "live" calls to update
   --    the effect of the call-step. Dead assignments are retained
   --    unchanged (because they may "come alive" when more dynamic data
   --    references are resolved to cells). No attempt was made to bound
   --    loops or calls. Bound_With_Arithmetic should be called again,
   --    after constant propagation is applied to the new model.
   --
   -- Finished (neither of the above)
   --
   --    The computation model and flow-graph appear stable. An attempt
   --    was made to bound loops and calls; the results if any are in
   --    Exec_Bounds.
   --
   is
      use type Storage.Cell_Set_T;
      use type Storage.Bounds.Cell_Interval_List_T;
      use type Flow.Step_List_T;


      --    Principles Of Operation
      --
      -- There may be several boundable (dynamic) aspects in the
      -- subprogram and its callees that might become partly or fully
      -- bounded by arithmetic analysis. The first question is to
      -- decide in which order these aspects are tackled; the problem
      -- is that the analysis of one aspect may be hindered by the
      -- unboundedness of another aspect. For example, while a dynamic
      -- memory reference is unbounded it may have a wide alias-range;
      -- if the reference is the target of an assignment, the aliasing
      -- effect may hide the value of a loop-counter cell and thus
      -- hinder the bounding of a loop.
      --
      -- No fixed order may be the best in all cases, and there may be
      -- a trade-off between orders that give a fast analysis and orders
      -- that give accurate bounds.
      --
      -- To get the most accurate bounds we may have to resort to
      -- iteration in which each dynamic feature is repeatedly analysed
      -- in different data contexts that are more and more constrained
      -- by the analysis of the other dynamic features. However, the
      -- iteration loop is external to Bound_With_Arithmetic because
      -- each iteration round begins with a constant-propagation phase.
      --
      -- At present, the following order is applied in each round of
      -- iteration. This order favours precision over speed:
      --
      -- 1. Refine the computation model by trying to bound the
      --    dynamic data references and dynamic calling protocols.
      --    When a dynamic calling protocol is bounded, the effect
      --    of the call is also refined.
      --
      -- If the computation model changes, the next steps are skipped
      -- and a new iteration round starts (with constant propagation).
      -- If the computation model is stable we go on to step 2:
      --
      -- 2. Try to bound dynamic control-flow edges (if any).
      --
      -- If som dynamic edge is successfully bounded and the control-flow
      -- graph grows, the iteration (on the original flow-graph) is stopped
      -- and we return to tracing the new flow to complete the flow-graph.
      --
      -- Otherwise (dynamic flow fully resolved or cannot be resolved
      -- further) we go on to steps 3 and 4:
      --
      -- 3. Bound the loops and calls.
      --
      -- 4. Bound the stack heights and stack usage.
      --
      -- A future redesign of the arithmetic analysis to use slicing
      -- instead of Pool_To_Steps will have to reconsider the order;
      -- perhaps a flow order would be better than an order based on
      -- the type of the dynamic feature.
      --
      --
      -- The present analysis uses Calculator.Pool_To_Steps in a global
      -- way which means that we first collect a list of all the steps
      -- that contain some boundable features, use Pool_To_Steps to
      -- find the data flux into each of these steps, and then analyse
      -- these fluxes in the order described above.


      -- Get subprogram data:

      Program : constant Programs.Program_T :=
         Programs.Execution.Program (Exec_Bounds);
      -- The program under analysis.

      Graph : Flow.Graph_T := Programs.Flow_Graph (Subprogram);
      -- The flow-graph of this subprogram.

      Model : constant Flow.Computation.Model_Handle_T :=
         Programs.Execution.Computation (Exec_Bounds);
      -- The given (input) computation model.

      Feasible_Loops : constant Loops.Loop_List_T :=
         Flow.Computation.Loops_Of (Model.all);
      -- The feasible loop-structure of this subprogram.
      -- Some or all of these loops may already have bounds.


      -- Get assertion data:

      Asserted_Inv : constant Storage.Cell_Set_T :=
         Assertions.Subprogram_Invariants (Subprogram, Asserts);
      -- Subprogram-specific assertions on cell invariance.

      Local_Inv : constant Storage.Cell_Set_T :=
         Storage.Mixed.Union (Inherit_Inv, Asserted_Inv);
      -- All invariant cells (inherited + specifically asserted).


      -- Get the unbounded features:

      Dyn_Edges : constant Flow.Dynamic_Edge_List_T :=
         Flow.Computation.Unstable_Dynamic_Edges (Model.all);
      --
      -- The edges with dynamically computed target address (indexed
      -- jumps) that are feasible under the given Model and have not
      -- been resolved into Growing or Stable states by earlier analyses
      -- of this Graph (in its present state). We will try to resolve
      -- these edges with arithmetic analysis.

      Unbounded_Calls : constant Programs.Call_List_T :=
         Programs.Execution.Input_Dependent_Calls (Exec_Bounds);
      --
      -- Those calls from this subprogram, to other subprograms, where
      -- the callee has some unbounded but desired aspects that may
      -- benefit from context-specific analysis (giving input parameter
      -- bounds) and where the call is feasible under the computation model.

      Take_Off_Calls : constant Programs.Call_List_T :=
         Programs.Execution.Calls_With_Unbounded_Take_Off (Exec_Bounds);
      -- The calls from this subprogram, to other subprograms, where
      -- the take-off stack-height (in this subprogram) is not yet bounded
      -- and the total stack usage is not yet bounded.

      Dyn_Cond_Edges : constant Flow.Step_Edge_List_T :=
         Opt_Edges_With_Dynamic_Condition (Model.all);
      --
      -- The step-edges with preconditions that contain some unresolved
      -- dynamic data references, or a null list if arithmetic analysis
      -- is not applied to resolve such references.

      Living : Flow.Life.Living_T :=
         Flow.Life.Live_Computation (
            Model     => Model,
            Calls     => Programs.Execution.Call_Bounds (Exec_Bounds),
            Heights   =>
               Programs.Execution.Loose_Stack_Heights (Exec_Bounds),
            Finals    =>
               Programs.Execution.Loose_Final_Stack_Heights (Exec_Bounds),
            Asserts   => Asserts,
            For_Time  => Opt.Bound_Time,
            For_Space => Opt.Bound_Stack,
            For_Data  => Arithmetic.Opt.Ref_Choice);
      --
      -- The result of liveness analysis of the given Model, showing
      -- which assignments are "live" in the Model, that is, which
      -- assignments influence interesting cells and expressions.
      -- Also shows the input, basis, and output cells.
      --
      -- This may be redundant work, if the liveness is not affected by
      -- the call-dependent analysis or the (call-dependent) analysis
      -- of the callees.

      Inputs : constant Calculator.Cell_Set_T :=
         Calculator.Copy (Flow.Life.Inputs  (Living));
      -- The Inputs may not contain all input cells for (unbounded)
      -- calls, because Flow.Life considers such cells relevant only if
      -- they are assigned in this subprogram or are outputs from calls
      -- in this subprogram. We will add them TBC, but later on, because we
      -- hope to bound some of these unbounded calls and this can reduce
      -- the input-cell set.

      Basis : constant Calculator.Cell_Set_T :=
         Calculator.Copy (Flow.Life.Basis (Living));

      Outputs : constant Calculator.Cell_Set_T:=
         Calculator.Copy (Flow.Life.Outputs (Living));

      -- Gather the "interesting" steps:

      Jump_Steps : constant Flow.Step_List_T (Dyn_Edges'Range) :=
         Flow.Sources (Dyn_Edges);
      -- The source-steps of the dynamic edges.
      -- If a step is the source of more than one dynamic edge, the
      -- step occurs as many times in this list.

      Call_Steps : constant Flow.Step_List_T (Unbounded_Calls'Range) :=
         Programs.Steps (Unbounded_Calls);
      -- The steps that contain the unbounded calls.

      Stack_Steps : constant Flow.Step_List_T :=
         Programs.Execution.Unbounded_Stack_Steps (Exec_Bounds);
      -- The steps that change some stack pointer, in case the
      -- total stack usage has not already been bounded and space
      -- analysis is requested. Also all final (return) steps, if
      -- some final stack height is not yet known.

      Take_Off_Steps : constant Flow.Step_List_T (Take_Off_Calls'Range) :=
         Programs.Steps (Take_Off_Calls);
      -- The steps that contain the calls with unbounded take-off height,
      -- for stacks where the total usage is not yet bounded.

      Effect_Steps : constant Flow.Step_List_T :=
         Opt_Steps_With_Dynamic_Effect (Living);
      -- The steps that have effects that contain dynamic data references,
      -- perhaps selected depending on where they occur, or a null list
      -- if arithmetic analysis is not applied to resolve such references.

      Cond_Steps : constant Flow.Step_List_T :=
         Flow.Sources (Edges => Dyn_Cond_Edges, Unique => True);
      -- The source-steps of the edges that have preconditions that
      -- contain dynamic data references, with each source-step listed
      -- only once even if it is the source of several such edges.


      Interest : constant Flow.Step_List_T :=
           Jump_Steps
         & Call_Steps
         & Stack_Steps
         & Take_Off_Steps
         & Effect_Steps
         & Cond_Steps;
      --
      -- The set of "interesting" steps contains those steps of the
      -- (original) flow-graph. Loop-head steps are not included because
      -- they are handled separately.

      Jump_First : constant Positive := Interest'First;
      -- Interest(Jump_First) is the first of the Jump_Steps.

      Call_First : constant Positive := Jump_First + Jump_Steps'Length;
      -- Interest(Call_First) is the first of the Call_Steps.

      Stack_First : constant Positive := Call_First + Call_Steps'Length;
      -- Interest(Stack_First) is the first of the Stack_Steps.

      Take_Off_First : constant Positive := Stack_First + Stack_Steps'Length;
      -- Interest(Take_Off_First) is the first of the Take_Off_Steps.

      Effect_First : constant Positive :=
         Take_Off_First + Take_Off_Steps'Length;
      -- Interest(Effect_First) is the first of the Effect_Steps.

      Cond_First : constant Positive := Effect_First + Effect_Steps'Length;
      -- Interest(Cond_First) is the first of the Cond_Steps.


      Jump_Off : constant Integer := Jump_First - Jump_Steps'First;
      -- Jump_Steps(J) corresponds to Interest(J + Jump_Off).

      Call_Off : constant Integer := Call_First - Call_Steps'First;
      -- Call_Steps(C) corresponds to Interest(C + Call_Off).

      Stack_Off : constant Integer := Stack_First - Stack_Steps'First;
      -- Stack_Steps(S) corresponds to Interest(S + Stack_Off).

      Take_Off_Off : constant Integer := Take_Off_First - Take_Off_Steps'First;
      -- Take_Off_Steps(T) corresponds to Interest(T + Take_Off_Off).

      Effect_Off : constant Integer := Effect_First - Effect_Steps'First;
      -- Effect_Steps(E) corresponds to Interest(E + Effect_Off).

      Cond_Off : constant Integer := Cond_First - Cond_Steps'First;
      -- Cond_Steps(C) corresponds to Interest(C + Cond_Off).


      -- Arithmetic analysis stuff:

      Calc : Calculator.Calc_Handle_T;
      -- The calculator we use (and start, and stop).

      Root_Pool : Calculator.Pool_T;
      -- The root data-pool into the subprogram as the combination of
      -- the given Initial parameter bounds and the Asserted bounds.

      Exit_Pool : Calculator.Pool_T;
      -- The exit data-pool of the subprogram.

      Summary : Calculator.Loop_Summary_List_T (Feasible_Loops'Range);
      -- The summary effect of each loop body (flux on the repeat
      -- edges, set of invariant cells).

      Loop_Init : Calculator.Pool_List_T (Feasible_Loops'Range);
      -- The initializing data-pool into each loop, without the assertions.

      Loop_Repeat : Calculator.Flux_List_T (Feasible_Loops'Range);
      -- The repeat flux of each loop, without the assertions but with
      -- knowledge of the range of Loop_Init and Summary.Repeat, ie. the
      -- improved repeat flux from Calculator.Pool_To_Steps.

      Into_Interest : Calculator.Pool_List_T (Interest'Range);
      -- The data-pool into each interesting step.


      function Assertion_Constrained_Flux (
         Flux : Calculator.Flux_T;
         Step : Flow.Step_T)
      return Calculator.Flux_T
      --
      -- Constrains the flux by the Asserted bounds, by all the
      -- assertions on the nested loops that contain the step, and
      -- by any Range_Pre assignment constraints in the step itself.
      --
      is
      begin

         return
            Nesting_Bounded_Flux (
               Step    => Step,
               Flux    =>
                  Calculator.Range_Bounded_Flux (
                     Flux   => Flux,
                     Bounds => Storage.Bounds.Cell_Intervals (
                        From  => Asserted,
                        Point => Flow.Prime_Address (Step))),
               Living  => Living,
               Luups   => Feasible_Loops,
               Asserts => Assert_Map);

      end Assertion_Constrained_Flux;


      function Assertion_Constrained_Pool (
         Pool : Calculator.Pool_T;
         Step : Flow.Step_T)
      return Calculator.Pool_T
      --
      -- Constrains the Pool by the assertion-pool, by all the
      -- assertions on the nested loops that contain the step, and
      -- by any Range_Pre assignment constraints in the step itself.
      --
      is
      begin

         return
            Nesting_Bounded_Pool (
               Step    => Step,
               Pool    => Calculator.Bounded_Pool (
                  Pool   => Pool,
                  Bounds => Storage.Bounds.Cell_Intervals (
                     From  => Asserted,
                     Point => Flow.Prime_Address (Step))),
               Living  => Living,
               Luups   => Feasible_Loops,
               Asserts => Assert_Map);

      end Assertion_Constrained_Pool;


      procedure Resolve_Data (
         Step : in Flow.Step_T;
         Pool : in Calculator.Pool_T)
      --
      -- Tries to resolve the dynamic data references in the effect of
      -- the given Step using, as bounds, the calculated data-Pool into
      -- the Step.
      --
      -- A dynamic data reference is analysed here if the reference
      -- occurs in an assignment where
      --
      -- > the assignment is "live", or
      --
      -- > the target is a dynamic data reference.
      --
      -- TBM in the last case to consider only assignments where the
      -- target may alias with some live/desired cells, including inputs
      -- to calls.
      --
      -- However, under the option "-arith_ref all" we analyse all
      -- dynamic data references.
      --
      is
         use type Arithmetic.Effect_Ref;
         use type Arithmetic.Expr_Kind_T;
         use type Arithmetic.Opt.Ref_Choice_T;

         All_Refs : constant Boolean :=
            Arithmetic.Opt.Ref_Choice = Arithmetic.Opt.All_Item;
         -- Whether all dynamic refs should be analysed.

         Step_Mark : Output.Nest_Mark_T;
         -- For the output locus of the step.

         Effect : Arithmetic.Effect_T :=
            Flow.Computation.Effect (Step, Model.all);
         -- The given effect of the Step, under the given model.
         -- Contains both live and dead assignments.

         Refs_Bounded : Boolean := False;
         -- Whether some of the dynamic references in Effect were bounded.

         One_Ref_Bounded : Boolean;
         -- Whether one or more dynamic references in the current assignment
         -- in Effect was bounded.

         Valid_Pool : Calculator.Pool_T;
         -- The given Pool constrained with assertions and with
         -- the Range_Pre assignment constraints in the effect
         -- of the Step.

      begin

         Step_Mark := Output.Nest (
            Flow.Show.Locus (Step, Programs.Symbol_Table (Program)));

         Calculator.Comment (
            Text =>
               "Resolving data in step"
               & Flow.Step_Index_T'Image (Flow.Index (Step))
               & " at "
               & Flow.Image (Flow.Tag (Step)),
            Calc => Calc);

         Valid_Pool := Assertion_Constrained_Pool (Pool, Step);

         -- Try to bound the references in "live" assignments:

         for E in Effect'Range loop

            if All_Refs
            or else (
               Effect(E).Target.Kind = Arithmetic.Ref
            or Flow.Life.Is_Live (
                  Item   => E,
                  Effect => Effect,
                  Step   => Step,
                  Living => Living))
            then
               -- Effect(E) is a "live" assignment or has a dynamic
               -- reference as the target, or we shall analyse all
               -- dynamic references.

               Arithmetic.Bound_References (
                  Within  => Effect(E),
                  Bounds  => Valid_Pool,
                  Bounded => One_Ref_Bounded);

               Refs_Bounded := Refs_Bounded or One_Ref_Bounded;

           end if;

         end loop;

         -- Was something bounded?

         if Refs_Bounded then
            -- The effect was resolved / refined.

            if Opt.Trace_Data_Resolution then

               Output.Trace (
                   "Effect resolved to "
                  & Arithmetic.Image (Effect));

            end if;

            Flow.Computation.Set_Effect (
               Step  => Step,
               To    => new Arithmetic.Effect_T'(Effect),
               Under => Model.all);

         end if;

         Output.Unnest (Step_Mark);

      exception

      when Flow.False_Path | Calculator.Null_Set_Error =>
         -- Cannot reach this Step in this Model.

         Output.Note (
              "Step #"
            & Flow.Step_Index_T'Image (Flow.Index (Step))
            & " is infeasible (for data refs).");

         Mark_Unreachable (Step, Model);

         Output.Unnest (Step_Mark);

      when others =>

         Output.Fault (
            Location => "Bounds.Bound_With_Arithmetic.Resolve_Data",
            Text     => "Exception propagated.");

         Output.Unnest (Step_Mark);

         raise;

      end Resolve_Data;


      procedure Resolve_Edge_Cond (
         Step : in Flow.Step_T;
         Pool : in Calculator.Pool_T)
      --
      -- Tries to resolve the dynamic data references in the preconditions
      -- of edges leaving the given Step using, as bounds, the calculated
      -- data-Pool into the Step transformed by the effect of the step.
      --
      -- The given Pool parameter is the data-pool into the Step. The effect
      -- of the Step is here applied to Pool to give the pool (bounds) for
      -- the edges from the Step. In principle, the precondition of each edge
      -- also bounds the dynamic references in the precondition itself;
      -- for example, the precondition could have the form "v = 5 and
      -- Memory[v] > 22", resolving to "v = 5 and Memory[5] > 22". This
      -- principle is not used yet.
      --
      is
         use type Arithmetic.Condition_T;
         use type Flow.Step_T;

         Step_Mark : Output.Nest_Mark_T;
         -- For the output locus of the step.

         Cond_Pool : Calculator.Pool_T;
         -- The data-pool after the Step, into the edge preconditions,
         -- and constrained by all assertions.

         Edge : Flow.Step_Edge_T;
         -- One of the Dyn_Cond_Edges.

         Old_Cond : Arithmetic.Condition_T;
         -- The given (old) precondition of the Edge.

         New_Cond : Arithmetic.Condition_T;
         -- The new precondition, or perhaps the old one unchanged.

      begin

         Step_Mark := Output.Nest (
            Flow.Show.Locus (Step, Programs.Symbol_Table (Program)));

         Calculator.Comment (
            Text =>
               "Computing flux after step"
               & Flow.Step_Index_T'Image (Flow.Index (Step))
               & " at "
               & Flow.Image (Flow.Tag (Step)),
            Calc => Calc);

         Cond_Pool := Calculator.Pool_After_Effect (
            Pool   => Assertion_Constrained_Pool (Pool, Step),
            Effect => Flow.Computation.Effect (Step, Model.all));
         --
         -- The above computation applies the Range_Pre assignment
         -- constraints twice, once in Assertion_Constrained_Pool and once
         -- in Pool_After_Effect. TBM if this becomes a problem.

         for D in Dyn_Cond_Edges'Range loop

            Edge := Dyn_Cond_Edges(D);

            if Flow.Source (Edge) = Step then
               -- This is an Edge from the Step, with some dynamic
               -- data references in its precondition.

               Old_Cond := Flow.Computation.Condition (Edge, Model.all);

               Calculator.Comment (
                  Text =>
                       "Resolving data in precondition of edge"
                     & Flow.Step_Edge_Index_T'Image (Flow.Index (Edge)),
                  Calc => Calc);

               New_Cond := Arithmetic.Reference_Bounded (
                  Expr   => Old_Cond,
                  Bounds => Cond_Pool);

               if New_Cond /= Old_Cond then
                  -- The precondition was resolved / refined.

                  Flow.Computation.Set_Condition (
                     On    => Edge,
                     To    => New_Cond,
                     Under => Model.all);

                  if Opt.Trace_Data_Resolution then

                     Output.Trace (
                         "Edge condition resolved from "
                        & Arithmetic.Image (Old_Cond)
                        & " to "
                        & Arithmetic.Image (New_Cond));

                  end if;

               end if;

            end if;

         end loop;

         Output.Unnest (Step_Mark);

      exception

      when Flow.False_Path | Calculator.Null_Set_Error =>
         -- Cannot reach this Step in this Model.

         Output.Note (
              "Step #"
            & Flow.Step_Index_T'Image (Flow.Index (Step))
            & " is infeasible (for data refs in conds).");

         Mark_Unreachable (Step, Model);

         Output.Unnest (Step_Mark);

      when others =>

         Output.Fault (
            Location => "Bounds.Bound_With_Arithmetic.Resolve_Edge_Cond",
            Text     => "Exception propagated.");

         Output.Unnest (Step_Mark);

         raise;

      end Resolve_Edge_Cond;


      procedure Resolve_Protocol (
         Call : in Programs.Call_T;
         Step : in Flow.Step_T;
         Pool : in Calculator.Pool_T)
      --
      -- Tries to resolve (bound) the calling protocol in the given
      -- Call by using as bounds the calculated data-Pool into the
      -- call-Step. Does nothing if the protocol is already Static.
      --
      is

         Valid_Pool : Calculator.Pool_T;
         -- The given Pool constrained with assertions and with
         -- the Range_Pre assignment constraints in the effect of
         -- the Step.

         Bounded : Boolean;
         -- Whether the protocol was bounded.

      begin

         if not Flow.Computation.Calling_Protocol_Is_Static (Call, Model.all)
         then
            -- The calling protocol is dynamic.

            Calculator.Comment (
               Text =>
                    "Resolving protocol in call "
                  & Programs.Image (Call)
                  & " at "
                  & Flow.Image (Flow.Tag (Step)),
               Calc => Calc);

            Valid_Pool := Assertion_Constrained_Pool (Pool, Step);

            -- Try to bound the protocol:

            Bounds.Calling.Bound_Protocol (
               Call    => Call,
               Data    => Valid_Pool,
               Model   => Model.all,
               Bounded => Bounded);
            --
            -- May propagate Flow.False_Path.

            -- if Bounded then:
            --    The protocol was refined or constrained in some way.
            --    This may change the effect of the call may change, so
            --    we have a (possibly) new computation model.
            --    This is seen as Computation_Changed (Exec_Bounds).

         end if;

      exception

      when Flow.False_Path =>
         -- Cannot reach this Call in this Model.

         Output.Note (
            Locus => Programs.Locus (Call),
            Text  =>
                 "Call  in step #"
               & Flow.Step_Index_T'Image (Flow.Index (Step))
               & " is infeasible (for protocol).");

         Mark_Unreachable (Step, Model, "call");

      end Resolve_Protocol;


      procedure Resolve_Flow (
         Edge : in out Flow.Boundable_Edge_T'Class;
         Pool : in     Calculator.Pool_T)
      --
      -- Tries to resolve the given dynamic Edge using, as bounds, the
      -- calculated data-Pool into the source-step of the Edge.
      --
      is

         Source : constant Flow.Step_T := Flow.Source (Edge);
         -- The source step of Edge.

         Step_Mark : Output.Nest_Mark_T;
         -- For the output locus of the Source step.

         Valid_Pool : Calculator.Pool_T;
         -- The given Pool constrained with assertions and with
         -- the Range_Pre assignment constraints in the effect of
         -- the Source step.

      begin

         Step_Mark := Output.Nest (
            Flow.Show.Locus (
               Step   => Source,
               Source => Programs.Symbol_Table (Program)));

         Calculator.Comment (
            Text =>
                 "Resolving dynamic edge from step"
               & Flow.Step_Index_T'Image (Flow.Index (Source))
               & " at "
               & Flow.Image (Flow.Tag (Source)),
            Calc => Calc);

         Valid_Pool := Assertion_Constrained_Pool (Pool, Source);

         Flow.Mark_Domain (Edge);

         Flow.Apply (
            Bounds => Valid_Pool,
            Upon   => Edge,
            Graph  => Graph);
         --
         -- The results are possible new edges in the Graph and
         -- a possible change in state of the Dyn_Edge, to be
         -- checked later with Flow.Computation.Dynamic_Flow or
         -- Programs.Execution.Dynamic_Flow.

         Output.Unnest (Step_Mark);

      exception

      when Flow.False_Path | Calculator.Null_Set_Error =>

         Output.Note (
               "Step #"
            & Flow.Step_Index_T'Image (Flow.Index (Source))
            & " is infeasible (as source of dynamic edges).");

         Mark_Unreachable (Step => Source, Model => Model);

         Output.Unnest (Step_Mark);

      when others =>

         Output.Fault (
            Location => "Bounds.Bound_With_Arithmetic.Resolve_Flow",
            Text     => "Exception propagated.");
         Output.Unnest (Step_Mark);

         raise;

      end Resolve_Flow;


      procedure Bound_Loop (
         Luup      : in Loops.Loop_T;
         Init      : in Calculator.Pool_T;
         Invariant : in Calculator.Cell_Set_T;
         Repeat    : in Calculator.Flux_T)
      --
      -- Bounds the Luup using the Initializing data-pool into the Luup,
      -- the Invariant cells of the Luup, the Repeat flux of the Luup,
      -- and assertions.
      --
      -- Luup
      --    A feasible, unbounded loop.
      -- Init
      --    The data-pool into the Luup from outside the loop (initialisation
      --    pool).
      -- Invariant
      --    The cells that are invariant in the Luup.
      -- Repeat
      --    The (improved) repeat-flux of the Luup.
      --
      is

         Loop_Mark : Output.Nest_Mark_T;
         -- Marks the default Output locus for the Luup.

         Head_Step : Flow.Step_T;
         -- The head step of the loop.

         Into_Loop : Calculator.Pool_T;
         -- The Init pool constrained with the assertion pool and the
         -- assertions on outer loops and the loop itself.

         Repeat_Loop : Calculator.Flux_T;
         -- The Repeat flux constrained with the assertion pool and the
         -- assertions on outer loops and the Luup itself.

      begin

         Loop_Mark := Output.Nest (
            Loops.Show.Locus (
               Luup   => Luup,
               Within => Graph,
               Source => Programs.Symbol_Table (Program)));

         Head_Step := Loops.Head_Step (Luup);

         Calculator.Comment (
            Text =>
                 "Bounding loop"
               & Loops.Loop_Index_T'Image (Loops.Loop_Index (Luup)),
            Calc => Calc);

         Calculator.Comment (
            Text => "Assertion-constrain Into",
            Calc => Calc);

         Into_Loop :=
            Assertion_Constrained_Pool (
               Pool => Init,
               Step => Head_Step);

         Calculator.Comment (
            Text => "Assertion-constrain Repeat",
            Calc => Calc);

         Repeat_Loop :=
            Assertion_Constrained_Flux (
               Flux => Repeat,
               Step => Head_Step);

         Bounds.Looping.Bound_Loop (
            Luup        => Luup,
            Initial     => Into_Loop,
            Repeat      => Repeat_Loop,
            Repeat_Inv  => Invariant,
            Inherit_Inv => Local_Inv,
            Exec_Bounds => Exec_Bounds);

         Output.Unnest (Loop_Mark);

      end Bound_Loop;


      procedure Bound_Call (
         Call : in Programs.Call_T;
         Step : in Flow.Step_T;
         Pool : in Calculator.Pool_T)
      --
      -- Bounds the Call using the Pool into the call-Step and assertions.
      --
      is

         Into_Call : Calculator.Pool_T;
         -- The pool into the Call, constrained with the assertion
         -- pool and the assertions on loops that contain the call step.

         Call_Mark : Output.Nest_Mark_T;
         -- Marks the default Output locus for the Call.

      begin

         Call_Mark := Output.Nest (Programs.Locus (Call_Path & Call));

         Calculator.Comment (
             Text =>
                 "Assertion-constrain Into for "
               & Programs.Image (Call),
            Calc => Calc);

         Into_Call := Assertion_Constrained_Pool (Pool, Step);

         Bounds.Calling.Bound_Call (
            Call          => Call,
            Caller_Bounds => Exec_Bounds,
            Data          => Into_Call,
            Inherit_Inv   => Local_Inv,
            Asserts       => Asserts,
            Assert_Map    => Assert_Map,
            Bounds_Set    => Bounds_Set);

         Output.Unnest (Call_Mark);

      end Bound_Call;


      function Unbounded_Loops return Natural
      --
      -- The number of unbounded Feasible_Loops.
      --
      is

         Num : Natural := 0;

      begin

         for L in Feasible_Loops'Range loop

            if not Programs.Execution.Bounded (
               Luup   => Feasible_Loops(L),
               Within => Exec_Bounds)
            then

               Num := Num + 1;

            end if;

         end loop;

         return Num;

      end Unbounded_Loops;


      procedure Bound_Time_And_Space
      --
      -- Tries to bound all unbounded loops and Unbounded_Calls, assuming that
      -- dynamic flow and data-access has been stably resolved (or failed to
      -- be resolved). Then tries to bound local stack height and total stack
      -- usage.
      --
      -- As a side effect, some parts of the computation model may be marked
      -- infeasible (unreachable) and the model may then be pruned. This is
      -- not reported in any output parameter.
      --
      is

         Into_Stack_Steps : Calculator.Pool_List_T (Stack_Steps'Range);
         -- The pool into each step that changes the stack pointer.

         Into_Take_Off : Calculator.Pool_List_T (Take_Off_Steps'Range);
         -- The pool into each call-step that has an unbounded take-off.

      begin

         -- Bound the loops if time-bounds are desired:

         if Opt.Bound_Time then

            for L in Feasible_Loops'Range loop

               if not Programs.Execution.Bounded (
                  Luup   => Feasible_Loops(L),
                  Within => Exec_Bounds)
               then

                  Bound_Loop (
                     Luup      => Feasible_Loops(L),
                     Init      => Loop_Init(L),
                     Invariant => Summary(L).Invariant,
                     Repeat    => Loop_Repeat(L));

               end if;

            end loop;

         end if;


         -- Bound the calls:

         for C in Unbounded_Calls'Range loop

            Bound_Call (
               Call => Unbounded_Calls(C),
               Step => Call_Steps(C),
               Pool => Into_Interest(C + Call_Off));

         end loop;


         -- Bound the stack usage if desired:

         if Opt.Bound_Stack then

            for S in Stack_Steps'Range loop

               Into_Stack_Steps(S) := Into_Interest(S + Stack_Off);

            end loop;

            for T in Take_Off_Steps'Range loop

               Into_Take_Off(T) := Into_Interest(T + Take_Off_Off);

            end loop;

            Bounds.Stacking.Bound_Local_Stack_Height (
               Stack_Steps      => Stack_Steps,
               Into_Stack_Steps => Into_Stack_Steps,
               Exec_Bounds      => Exec_Bounds);

            Bounds.Stacking.Bound_Take_Off_Height (
               Calls       => Take_Off_Calls,
               Into_Calls  => Into_Take_Off,
               Exec_Bounds => Exec_Bounds);

         end if;

      end Bound_Time_And_Space;


   begin  -- Bound_With_Arithmetic

      if Opt.Trace_Arith then

         Output.Trace (
              "Arithmetic analysis starts for"
            & Natural'Image (Interest'Length)
            & " nubs,"
            & Natural'Image (Unbounded_Loops)
            & " unbounded loops, and"
            & Flow.Step_Index_T'Image (Flow.Max_Step (Graph))
            & " steps.");

         Output.Trace (
              "Initial bounds"
            & Output.Field_Separator
            & Storage.Bounds.Image (Initial));

         Output.Trace (
              "Asserted bounds"
            & Output.Field_Separator
            & Storage.Bounds.Image (Asserted));

         Output.Trace (
              "Inherited invariant cells"
            & Output.Field_Separator
            & Storage.Image (Inherit_Inv));

      end if;

      -- First show the "interesting" steps if desired:

      if Opt.Trace_Nubs then

         Trace_Nubs ("Jump"    , Jump_Steps    , Program);
         Trace_Nubs ("Call"    , Call_Steps    , Program);
         Trace_Nubs ("Stack"   , Stack_Steps   , Program);
         Trace_Nubs ("Take_Off", Take_Off_Steps, Program);
         Trace_Nubs ("Effect"  , Effect_Steps  , Program);
         Trace_Nubs ("Cond"    , Cond_Steps    , Program);

      end if;

      -- Then analyse:

      if Opt.Trace_Arith then

         Output.Trace (
              "Arithmetic basis contains"
            & Natural'Image (Flow.Life.Basis_Size (Living))
            & " storage cells"
            & Output.Field_Separator
            & Calculator.Image (Basis));

      end if;

      if Flow.Life.Basis_Size (Living) = 0 then
         -- No arithmetic cells seem relevant to the unbounded
         -- parts of this subprogram. We have no arithmetic to
         -- analyse.

         Output.Warning ("No relevant arithmetic to be analysed.");

         -- These bounds are as good as they get, for this context.

         Set_IO_Cells (
            Inputs  => Inputs,
            Outputs => Outputs,
            Basis   => Basis,
            Within  => Exec_Bounds);

         -- No change to flow-graph or computation model, so
         -- we are finished.

      else
         -- Some arithmetic cells seem relevant to the unbounded
         -- parts of this subprogram.

         -- Start a calculator for the arithmetic analysis:

         Calc :=
            Calculator.Start (Comment_Text =>
                 Programs.Name (Subprogram)
                  & ", on path "
                  & Programs.Image (Call_Path));


         -- Define the root data-pool into the subprogram:

         Root_Pool :=
            Calculator.Bounded_Pool (
               Cells  => Basis,
               Bounds => Initial
                     and Storage.Bounds.Cell_Intervals (
                            From  => Asserted,
                            Point => Programs.Entry_Address (Subprogram)),
               Calc => Calc);

         -- Approximate the loops using a context-free classification
         -- of cells into loop-invariants and loop-variants, for
         -- each loop:

         Loops.Slim.Approximate_Loops (
            Living      => Living,
            Loops       => Feasible_Loops,
            Inherit_Inv => Local_Inv,
            Asserts     => Assert_Map,
            Basis       => Basis,
            Calc        => Calc,
            Summaries   => Summary);

         if Opt.Trace_Arith then

            Output.Trace ("Arithmetic loop-approximation done.");

         end if;

         -- Calculate all the fluxes that arrive at the interesting steps,
         -- and also the initial loop values for each unbounded loop:

         -- Note that the decoding process is required to place dynamic
         -- accesses and subprogram calls in steps so that the value of
         -- the dynamic indices, or the parameter values of the call, can
         -- be read from the flux that arrives at the step before executing
         -- the effect of the step.
         --
         -- An exception are the dynamic data accesses in edge preconditions.
         -- These depend on the flux after the step, including the effect of
         -- the step. This modification is implemented in Resolve_Edge_Cond.
         --
         -- For the loop-heads, the values of interest here are the
         -- initial values of the counter-candidates and invariants,
         -- which can be read from the flux that arrives at the head
         -- step (not including repeat edges).

         Calculator.Pool_To_Steps (
            Nodes       => (1 => Flow.Entry_Node (Graph)),
            Edges       =>
               Flow.Computation.Feasible (
                  Edges => Loops.Forward_Edges (
                     Within   => Graph,
                     Avoiding => Programs.Loops_Of (Subprogram)),
                  Under => Model.all),
            Living      => Living,
            Root        => Root_Pool,
            Luups       => Feasible_Loops,
            Summaries   => Summary,
            Steps       => Interest,
            Into_Luups  => Loop_Init,
            Repeats     => Loop_Repeat,
            Into_Steps  => Into_Interest,
            Exit_Pool   => Exit_Pool);

         if Opt.Trace_Arith then

            Output.Trace ("Arithmetic flow propagated to steps.");

         end if;

         -- Try to resolve the dynamic data accesses (step 1):

         case Arithmetic.Opt.Ref_Choice is

         when Arithmetic.Opt.None =>

            if Opt.Trace_Arith then

               Output.Trace (
                  "Arithmetic not applied to dynamic data refs or edge conds.");

            end if;

         when Arithmetic.Opt.Relevant
            | Arithmetic.Opt.All_Item =>

            for E in Effect_Steps'Range loop

               Resolve_Data (
                  Step => Effect_Steps(E),
                  Pool => Into_Interest(E + Effect_Off));

            end loop;

            if Opt.Trace_Arith then

               Output.Trace ("Arithmetic applied to dynamic data refs.");

            end if;

            for C in Cond_Steps'Range loop

               Resolve_Edge_Cond (
                  Step => Cond_Steps(C),
                  Pool => Into_Interest(C + Cond_Off));

            end loop;

            if Opt.Trace_Arith then

               Output.Trace ("Arithmetic applied to dynamic edge conds.");

            end if;

         end case;

         -- Try to resolve the dynamic calling protocols:

         for C in Unbounded_Calls'Range loop

            Resolve_Protocol (
               Call => Unbounded_Calls(C),
               Step => Call_Steps(C),
               Pool => Into_Interest(C + Call_Off));

         end loop;

         if Opt.Trace_Arith then

            Output.Trace ("Arithmetic applied to dynamic protocols.");

         end if;

         if Flow.Computation.Changed (Model.all) then
            -- Some data references in the computation model were
            -- successfully bounded and changed, or some parts of the
            -- subprogram were found to be unreachable (infeasible),
            -- so we abandon this arithmetic analysis and start over
            -- with the new computation model.

            Output.Note ("Arithmetic analysis changed computation model.");

         else
            -- The computation model appears stable for this flow-graph.

            -- Try to resolve the dynamic flow (step 2):

            for J in Jump_Steps'Range loop

               Resolve_Flow (
                  Edge => Dyn_Edges(J).all,
                  Pool => Into_Interest(J + Jump_Off));

            end loop;

            if Opt.Trace_Arith then

               Output.Trace ("Arithmetic applied to dynamic jumps.");

            end if;

            -- Is the flow graph still growing?

            case Flow.Computation.Dynamic_Flow (Model.all) is

            when Flow.Growing =>
               -- Some dynamic edge(s) were resolved and the flow-graph is
               -- still growing. It is not useful to try to bound loops or
               -- calls yet, since the flow-graph will be changed and so will
               -- the computed data flow.

               null;

            when Flow.Unresolved | Flow.Stable =>
               -- All dynamic edges resolved stably, or left unresolved.
               -- The flow-graph is as complete as we can make it.

               -- The real work of bounding time and space:

               Bound_Time_And_Space;
               --
               -- This may find that some loops or calls are unreachable
               -- (infeasible), which could be a reason for a new iteration
               -- of the analysis. However, we stop here.

               -- These bounds are as good as they get, for this context.

               Set_IO_Cells (
                  Inputs  => Inputs,
                  Outputs => Outputs,
                  Basis   => Basis,
                  Within  => Exec_Bounds);

            end case;

         end if;

         -- This concludes the data-flow calculations for this iteration.
         -- If the flow-graph was extended when dynamic jumps were resolved,
         -- or the arithmetic effects were updated when dynamic data accesses
         -- were resolved, this subprogram will be reanalysed using a new
         -- calculator instance on the new flow-graph and/or new computation
         -- model.
         --
         -- If the bounding of some loop failed, even after resolving data
         -- accesses, the bounding will be attempted again in the context
         -- of a more specific call-path.
         --
         -- If the bounding of some call-specific callee failed, even after
         -- resolving data accesses, the bounding of this subprogram also
         -- becomes call-specific and will be attempted again in the context
         -- of calls from higher levels.

         if Opt.Trace_Arith then

            Output.Trace ("Arithmetic analysis done.");

         end if;

         Calculator.Stop (Calc);

      end if;

      -- We no longer need the Living object:

      Flow.Life.Discard (Living);

   end Bound_With_Arithmetic;


   procedure Bound_Without_Arithmetic (
      Subprogram  : in Programs.Subprogram_T;
      Call_Path   : in Programs.Call_Path_T;
      Inherit_Inv : in Storage.Cell_Set_T;
      Asserts     : in Assertions.Assertion_Set_T;
      Assert_Map  : in Assertions.Assertion_Map_T;
      Bounds_Set  : in Programs.Execution.Bounds_Set_T;
      Exec_Bounds : in Programs.Execution.Bounds_Ref)
   --
   -- Bound a subprogram without arithmetic analysis.
   -- This essentially means setting the input and output
   -- cell-sets.
   --
   is

      Model : constant Flow.Computation.Model_Handle_T :=
         Programs.Execution.Computation (Exec_Bounds);
      -- The given (input) computation model.

      Unbounded_Calls : constant Programs.Call_List_T :=
         Programs.Execution.Context_Dependent_Calls (Exec_Bounds);
      --
      -- Those calls from this subprogram, to other subprograms, where
      -- the callee has some unbounded but desired aspects that may
      -- benefit from context-specific analysis and where the call is
      -- feasible under the computation model.

      -- TBA unbounded call protocols.

      Call : Programs.Call_T;
      -- One of the Unbounded_Calls.

      Call_Mark : Output.Nest_Mark_T;
      -- Locus for the Call.

   begin

      if Opt.Trace_Phase then

         Output.Trace ("Phase avoids arithmetic analysis.");

      end if;

      -- TBA unbounded protocols.

      for U in Unbounded_Calls'Range loop

         Call := Unbounded_Calls(U);

         Call_Mark := Output.Nest (Programs.Locus (Call_Path & Call));

         Calling.Bound_Call (
            Call          => Call,
            Caller_Bounds => Exec_Bounds,
            Data          => Assertions.Local_Call_Values (Call, Assert_Map),
            Inherit_Inv   => Inherit_Inv,
            Asserts       => Asserts,
            Assert_Map    => Assert_Map,
            Bounds_Set    => Bounds_Set);

         Output.Unnest (Call_Mark);

      end loop;

      Set_IO_Cells (
         Inputs  => Programs.Execution.Inputs_For_Unbounded_Calls (Exec_Bounds),
         Outputs => Flow.Computation.Cells_Defined (By => Model.all),
         Basis   => Storage.List_Cell_Sets.Empty,
         Within  => Exec_Bounds);

      -- The cells needed as inputs for bounding lower-level calls
      -- are probably not assigned in this subprogram, since this
      -- would require arithmetic analysis of this subprogram.
      -- Instead, values for these cells are (will be) passed across
      -- this subprogram in cell-bounds.

   end Bound_Without_Arithmetic;


   procedure Bound_As_Infeasible (
      Exec_Bounds : in Programs.Execution.Bounds_Ref)
   --
   -- Provide some (dummy) execution bounds for a subprogram that has
   -- been found to be infeasible (to have no feasible execution path)
   -- in the present context.
   --
   -- The main purpose is to avoid irrelevant alarms from functions
   -- that access some attributes of the execution bounds and (without
   -- this operation) would find that undefined attributes.
   --
   is
      use Storage.List_Cell_Sets;
   begin

      Set_IO_Cells (
         Inputs  => Empty,
         Outputs => Empty,
         Basis   => Empty,
         Within  => Exec_Bounds);

   end Bound_As_Infeasible;


   procedure Finish_Bounds (
      Exec_Bounds : in     Programs.Execution.Bounds_Ref;
      Assert_Map  : in     Assertions.Assertion_Map_T;
      Bounds_Set  : in     Programs.Execution.Bounds_Set_T;
      Flow_Frozen :    out Boolean)
   --
   -- Finishes the analysis of the Execution Bounds of a subprogram,
   -- where the subprogram is possibly still under construction by
   -- resolving dynamic control-flow constructs. There are two cases:
   --
   -- > If the subprogram's flow-graph is still growing, because some
   --   dynamic edges were resolved by recent analysis giving and using
   --   these Execution Bounds, the bounds are discarded (because they
   --   will not apply to the extended flow-graph) and Flow_Frozen is
   --   returned as False.
   --
   -- > If the subprogram's flow-graph is now stable, or could not be
   --   resolved further by the analysis giving and using these
   --   Execution Bounds, Flow_Frozen is returned as True and several
   --   finishing actions are applied to these bounds:
   --
   --   o   Report the end of decoding for this subprogram.
   --   o   Remove all dynamic edges and report the unresolved ones.
   --   o   Report unresolved data references (optional).
   --   o   Invoke Decoder.Finish for universal execution bounds.
   --   o   Evaluate the time-state and compute the stack usage.
   --   o   Use value-origin analysis to find invariant cells etc.
   --   o   Store these Execution Bounds in the Bounds_Set.
   --
   -- Exec_Bounds
   --    The execution bounds to be finished and stored, if the
   --    flow-graph is stable, or discarded otherwise.
   -- Assert_Map
   --    The assertion map for these Execution Bounds.
   --    May be No_Map in some cases, for example when there is
   --    no feasible execution path in the subprogram.
   -- Bounds_Set
   --    The Execution Bounds are stored here, if Flow_Frozen.
   -- Flow_Frozen
   --    Shows if the flow-graph of this subprogram is completed,
   --    and whether the Execution Bounds were stored in Bounds_Set.
   --
   is
      use Programs, Programs.Execution;
      use type Assertions.Assertion_Map_T;

      Graph : Flow.Graph_T := Flow_Graph (Exec_Bounds);
      -- The flow-graph of this subprogram.

      Feasible : Boolean;
      -- Whether the subprogram is feasible (has a feasible execution
      -- path) under these Exec_Bounds.

   begin

      -- Some ad-hoc internal checks:

      if  Assert_Map /= Assertions.No_Map
      and then Assertions.Subprogram (Assert_Map) /= Subprogram (Exec_Bounds)
      then
         -- Oops.

         Output.Fault (
            Location => "Bounds.Finish_Bounds",
            Text     => "Subprograms differ for bounds and map");

         -- The rest will be a mess.

      end if;

      case Dynamic_Flow (Exec_Bounds) is

      when Flow.Growing =>
         -- Some dynamic edges were resolved into new real edges.
         -- The flow-graph is still growing; all execution bounds
         -- computed so far are incomplete and insufficient for the
         -- extended flow-graph. Therefore the bounds are not yet
         -- stored in the Bounds_Set.

         Output.Note (
              "Dynamic flow not yet stable in bounds #"
            & Bounds_Index_T'Image (Index (Exec_Bounds)));

         Flow_Frozen := False;

         -- TBA discard Exec_Bounds as useless.

      when Flow.Unresolved | Flow.Stable =>
         -- All dynamic flow is resolved as far as possible.
         -- This means, among other things, that the call-tree is
         -- now stable (but may be incomplete) and the execution
         -- bounds are valid for this context, although perhaps not
         -- fully bounded.

         -- Report and handle remaining unresolved dynamic flow and data:

         if Flow.Some_Dynamic_Edges (Graph) then
            -- The decoding was suspended to handle dynamic flow
            -- but is now finished, either with a stable resolution
            -- or with some unresolved dynamic flow.

            Decoder.Stop (
               Program    => Program    (Exec_Bounds),
               Subprogram => Subprogram (Exec_Bounds),
               Graph      => Graph,
               Suspended  => False);

         end if;

         Flow.Computation.Show.Report_Unresolved_Flow (
            Model  => Computation (Exec_Bounds).all,
            Source => Symbol_Table (Exec_Bounds));

         Flow.Remove_All_Dynamic_Edges (Graph);

         if Opt.Warn_Unresolved_Data then

            Flow.Computation.Show.Report_Unresolved_Data (
               Model  => Computation  (Exec_Bounds).all,
               Source => Symbol_Table (Exec_Bounds));

         end if;

         Feasible := Is_Feasible (Exec_Bounds);

         -- Perhaps "finish" and trace the flow-graph:

         if Level (Exec_Bounds) = Universal then
            -- The flow-graph has been completed on the universal level.

            -- We can let the Decoder apply any finishing touches it
            -- deems suitable:

            if Feasible then

               Decoder.Finish (
                  Program    => Program    (Exec_Bounds),
                  Subprogram => Subprogram (Exec_Bounds),
                  Graph      => Graph,
                  Assert_Map => Assert_Map);

            end if;

            -- Moreover, we may want to show the graph in detail:

            if Opt.Trace_Graphs then

               Flow.Show.Trace_Graph (
                  Graph  => Graph,
                  Source => Symbol_Table (Exec_Bounds));

            end if;

            if Opt.Trace_Instructions then

               Programs.Show.Trace_Instructions_And_Branches (
                  Subprogram => Subprogram (Exec_Bounds));

            end if;

         end if;

         Flow_Frozen := True;

         if Opt.Bound_Time then

            Timing.Evaluate_Time_State (Exec_Bounds);

         end if;

         if Opt.Bound_Stack then

            Stacking.Compute_Stack_Usage (Exec_Bounds);

         end if;

         -- Apply value-origin analysis to find obviously invariant
         -- cells etc.

         if not Feasible then

            Output.Note ("Infeasible subprogram, skip value-origin analysis.");

         else

            Programs.Execution.Bound_Value_Origins (Exec_Bounds);

            if Programs.Execution.Value_Origins_Defined (Exec_Bounds) then

               Programs.Execution.Remove_From_Output (
                  Cells  => Flow.Origins.Overall_Invariants (
                     Map => Programs.Execution.Value_Origins (Exec_Bounds)),
                  Within => Exec_Bounds);

               -- TBC do not apply Decoder Additional actions on value-origin map
               -- TBA update Subprogram_Effect
               -- TBA update Loop_Effect

            else

               Output.Note ("No value-origins for invariance analysis.");

            end if;

         end if;

         -- The execution bounds are stored with the subprogram,
         -- whether fully bounded or not:

         Store_Bounds (
            Bounds => Exec_Bounds,
            Within => Bounds_Set);

      end case;

   end Finish_Bounds;


   procedure Report_Conflicting_Bounds (
      Intervals : in     Storage.Bounds.Cell_Interval_List_T;
      Message   : in     String;
      Conflict  : in out Boolean)
   --
   -- Checks the Intervals for void (empty) bounds on some cell, and
   -- reports them as Warnings under the Message heading. If some
   -- voids are found, sets Conflict to True.
   --
   is
   begin

      for I in Intervals'Range loop

         if Storage.Bounds.Void (Intervals(I).Interval) then

            Conflict := True;

            Output.Warning (
                 Message
               & Output.Field_Separator
               & Storage.Bounds.Image (Intervals(I)));

         end if;

      end loop;

   end Report_Conflicting_Bounds;


   procedure Bound_Execution (
      Exec_Bounds : in     Programs.Execution.Bounds_Ref;
      Params      : in     Storage.Bounds.Cell_Interval_List_T;
      Inherit_Inv : in     Storage.Cell_Set_T;
      Asserts     : in     Assertions.Assertion_Set_T;
      Bounds_Set  : in     Programs.Execution.Bounds_Set_T;
      Flow_Frozen :    out Boolean)
   is
      use type Assertions.Assertion_Map_T;
      use type Flow.Edge_Resolution_T;
      use type Storage.Bounds.Cell_Interval_List_T;
      use type Storage.Bounds.Var_Interval_List_T;

      Subprogram : constant Programs.Subprogram_T :=
         Programs.Execution.Subprogram (Exec_Bounds);
      -- The subprogram to be analysed.

      Entry_Address : constant Processor.Code_Address_T :=
         Programs.Entry_Address (Subprogram);
      -- The code address of the subprogram entry point.

      Call_Path : constant Programs.Call_Path_T :=
         Programs.Execution.Call_Path (Exec_Bounds);
      -- The context for the analysis.


      -- Collect asserted and inherited bounds on cells:


      Assumed_Initials : constant Storage.Bounds.Cell_Interval_List_T :=
         Programs.Unstable_Stack_Heights_Zero (Programs.Program (Subprogram));
      -- The initial local stack height for any Unstable stack is
      -- assumed (axiomatically) to be zero.

      Global_Bounds : constant Storage.Bounds.Var_Interval_List_T :=
         Assertions.Global_Values (Asserts);
      -- Globally asserted bounds on cells, valid throughout
      -- the subprogram.

      Sub_Bounds : constant Storage.Bounds.Var_Interval_List_T :=
         Assertions.Subprogram_Values (Subprogram, Asserts);
      -- Subprogram-specific asserted bounds on cells, valid
      -- throughout the subprogram.

      Global_Inputs : constant Storage.Bounds.Cell_Interval_List_T :=
          Storage.Bounds.Cell_Intervals (Global_Bounds, Entry_Address);
      -- Globally asserted bounds as they apply to cells on entry.

      Sub_Inputs : constant Storage.Bounds.Cell_Interval_List_T :=
         Storage.Bounds.Cell_Intervals (Sub_Bounds, Entry_Address);
      -- Whole-subprogram asserted bounds as they apply to cells on entry.

      Dec_Bounds : constant Storage.Bounds.Cell_Interval_List_T :=
         Processor.Properties.Entry_Bounds (
            Subprogram => Subprogram,
            Sub_Info   => Programs.Processor_Info (Subprogram));
      -- Bounds that the decoder knows hold on entry.
      -- This defines the initial local stack height for Stable stacks.

      Input_Bounds : constant Storage.Bounds.Cell_Interval_List_T :=
         Assertions.Subprogram_Inputs (Subprogram, Asserts);
      -- Subprogram-specific bounds that hold on entry, for any
      -- call, and can apply to parameters or global input cells.

      Generic_Bounds : constant Storage.Bounds.Cell_Interval_List_T :=
             Assumed_Initials
         and Global_Inputs
         and Sub_Inputs
         and Dec_Bounds
         and Input_Bounds;
      --
      -- The generic (not call-path dependent) cell-bounds on
      -- entry to the subprogram.

      Initial_Bounds : constant Storage.Bounds.Cell_Interval_List_T :=
             Params
         and Generic_Bounds;
      --
      -- All initial cell-bounds on entry to the subprogram,
      -- including the call-path dependent parameter bounds.


      -- Other locals:

      Bounds_Void : Boolean := False;
      -- Whether some input bounds / assertions conflict, giving
      -- a void value interval for some cells.

      Model : Flow.Computation.Model_Handle_T;
      -- The computation model used and updated.

      Assert_Map : Assertions.Assertion_Map_T;
      -- The assertions mapped to this subprogram.
      -- May be null, when the subprogram is found to be infeasible
      -- before assertions are mapped onto it.

      Valid_Map : Boolean;
      -- Whether the Assert_Map is valid, in terms of actual match
      -- tallies conforming to expected populations, etc.

      Arithmetic_Chosen : Boolean;
      -- Whether arithmetic analysis is chosen, either because it is
      -- required by the structure of the subprogram and shortcomings
      -- in the bounds from lower levels and from assertions, or
      -- because it is enforced by option or assertion.


      procedure Check_Feasibility
      --
      -- Checks if the execution-count bounds are still (possibly)
      -- feasible, and marks the exec-bounds as infeasible otherwise.
      --
      is
      begin

         if not Programs.Execution.Flow_Bounds_Feasible (Exec_Bounds) then

            Output.Warning (
               "Contradictory assertions make execution infeasible.");

            Programs.Execution.Mark_Infeasible (Exec_Bounds);

         end if;

      end Check_Feasibility;


      procedure Apply_Instruction_Count_Assertions
      --
      -- Applies the assertions on the number of repetitions of
      -- specific instructions. This may make some parts of the
      -- underlying flow-graph infeasible.
      --
      is
      begin

         Programs.Execution.Bound_Node_Counts (
            By => Assertions.Instruction_Counts (
               Subprogram => Subprogram,
               Model      => Model,
               Asserts    => Asserts),
            Within => Exec_Bounds);

         Check_Feasibility;

         Programs.Execution.Prune_Flow (Exec_Bounds);

      end Apply_Instruction_Count_Assertions;


      procedure Apply_Assertions
      --
      -- Locates and uses the assertions on this subprogram to bound
      -- as much as possible.
      --
      is
      begin

         Assertions.Identify_Assertions (
            Model         => Model,
            Assertion_Set => Asserts,
            Assertion_Map => Assert_Map);
         --
         -- The assertion mapping can depend on the call-path
         -- through different dynamic memory accesses (uses/defines
         -- characteristics) and TBA path-specific assertions.
         -- TBA: include Model parameter to show feasible parts and
         -- sharpen uses/defines for dynamic references.

         Programs.Execution.Bound_Assertions (
            Map    => Assert_Map,
            Within => Exec_Bounds);

         -- Use the asserted loop-start bounds (on potentially all loops):

         Bounds.Looping.Bound_Asserted_Starts (
            Luups       => Loops.All_Loops (Programs.Loops_Of (Subprogram)),
            Asserts     => Assert_Map,
            Exec_Bounds => Exec_Bounds);

         -- Use the asserted repetition bounds on loops (only for loops
         -- that are feasible, feasibly repeatable, and not yet bounded):

         Bounds.Looping.Bound_Asserted_Repeats (
            Luups => Programs.Execution.Unbounded_Loops (
               Within  => Exec_Bounds,
               Eternal => True),
            Asserts     => Assert_Map,
            Exec_Bounds => Exec_Bounds);

         -- Use the asserted bounds and other assertions on calls:

         Bounds.Calling.Bound_Asserted_Calls (
            Calls         => Flow.Computation.Calls_From (Model.all),
            Assert_Map    => Assert_Map,
            Caller_Bounds => Exec_Bounds,
            Bounds_Set    => Bounds_Set);

         Check_Feasibility;

      end Apply_Assertions;


      procedure Mark_Abnormal_Calls
      --
      -- If a call within the given Exec_Bounds has execution bounds
      -- under which the callee is infeasible, this procedure marks
      -- the call as infeasible in the computation model of the caller.
      -- Likewise, if a call within the given Exec_Bounds has execution
      -- bounds under which the callee never returns to the caller, this
      -- procedure marks the call as "non-returning" in the computation
      -- model.
      --
      -- Both actions can change the (in)feasibility of some parts of
      -- the flow-graph. The flow-graph is then pruned.
      --
      is

         Calls : constant Programs.Execution.Call_Bounds_List_T :=
            Programs.Execution.Call_Bounds (Exec_Bounds);
         -- All calls and their possibly context-dependent execution
         -- bounds. Includes only calls that are (still) considered
         -- feasible under these Exec_Bounds; some of them may be
         -- marked as infeasible below.

         Call : Programs.Execution.Call_Bounds_T;
         -- One of the Calls.

      begin

         for C in Calls'Range loop

            Call := Calls(C);

            if not Programs.Execution.Is_Feasible (Call.Bounds) then
               -- The callee has no feasible execution path, thus
               -- the call must be considered infeasible.

               if Flow.Pruning.Opt.Warn_Unreachable then

                  Output.Warning (
                     Locus => Programs.Locus (Call_Path & Call.Call),
                     Text  => "Callee has no feasible execution path.");

               end if;

               Flow.Computation.Mark_Infeasible (
                  Step  => Programs.Step (Call.Call),
                  Under => Model.all);

            elsif not Programs.Execution.Returns (Call.Bounds) then
               -- A call that does not return.

               Flow.Computation.Mark_No_Return (
                  From => Call.Call,
                  To   => Model.all);

            end if;

         end loop;

         Flow.Computation.Prune (Model.all);

      end Mark_Abnormal_Calls;


      type Phase_T is (
         Value_Origins_For_Target,
         Constant_Propagation,
         Value_Origins_For_Flow,
         Applying_Assertions,
         Abstract_Execution,
         Arithmetic_Analysis);
      --
      -- The successive phases of analysis that we apply.
      --
      -- Value_Origins_For_Target
      --    Value-origin analysis for target-specific uses, for
      --    example to chain narrow operations into wider ones.
      --    This may modify the arithmetic effects and conditions,
      --    and introdude new cells, for example register pairs.
      --
      -- Constant_Propagation
      --    Constant propagation for refining arithmetic expressions,
      --    resolving dynamic memory references and dynamic jumps,
      --    finding some infeasible paths, and bounding stack heights.
      --
      -- Value_Origins_For_Flow
      --    Value-origin analysis for resolving boundable edges.
      --    This is useful for some targets, for example to
      --    separate between ordinary calls and tail-calls.
      --
      -- Applying_Assertions
      --    Identifying and applying assertions on call and loop
      --    repetition bounds, including loop-start bounds.
      --
      -- Abstract_Execution
      --    Abstract execution of the subprogram and its callees, to
      --    resolve boundable edges, and perhaps other things TBA.
      --    This is an optional phase that uses the SWEET analyser
      --    from Mälardalen University.
      --
      -- Arithmetic_Analysis
      --    Arithmetic analysis for loop bounds and for resolving
      --    all other dynamic objects.
      --
      -- The result of a phase can be:
      --
      -- > Growth of the flow-graph (if dynamic edges were resolved).
      --   We then return from Bound_Execution and let the Decoder
      --   continue to trace out the flow-graph.
      --
      -- > Changes to the computation model. We then recompute the
      --   effects of calls and start over from the first phase.
      --
      -- > None of the above. We can go on to the next phase.


      Phase : Phase_T := Phase_T'First;
      -- The current phase of analysis.

      Restarts : Natural := 0;
      -- The number of times we have restarted an earlier phase
      -- because a later phase has updated the computation model.

   begin  -- Bound_Execution

      -- Check the contextual bounds:

      Report_Conflicting_Bounds (
         Intervals => Generic_Bounds,
         Message   => "Conflicting assertions on entry",
         Conflict  => Bounds_Void);

      Report_Conflicting_Bounds (
         Intervals => Initial_Bounds,
         Message   => "Conflicting assertions or context on entry",
         Conflict  => Bounds_Void);

      if Bounds_Void
      or Opt.Trace_Context
      then
         -- Show the input bounds in detail.

         Output.Trace ("Input bounds from context and assertions:");

         Output.Heading ("Assumed initial stack heights:");
         Storage.Bounds.Show.Show (Assumed_Initials);

         Output.Heading ("Global assertions:");
         Storage.Bounds.Show.Show (Global_Inputs);

         Output.Heading ("Subprogram entry assertions:");
         Storage.Bounds.Show.Show (Input_Bounds);

         Output.Heading ("Subprogram general assertions at entry:");
         Storage.Bounds.Show.Show (Sub_Inputs);

         Output.Heading ("Call assertions and computed bounds:");
         Storage.Bounds.Show.Show (Params);

         Output.Heading ("Target-dependent implicit bounds:");
         Storage.Bounds.Show.Show (Dec_Bounds);

         -- TBA skip this subprogram/context, marking it "infeasible"?

      end if;

      -- Define the contextual bounds on cell values:

      Programs.Execution.Bound_Initial_Values (
         To     => Initial_Bounds,
         Within => Exec_Bounds);

      Programs.Execution.Bound_Call_Inputs (
         To   => Global_Bounds and Sub_Bounds,
         From => Exec_Bounds);

      -- Possible asserted bounds in instruction execution counts:

      Apply_Instruction_Count_Assertions;

      -- Possible asserted bounds on stacks:

      Bounds.Stacking.Apply_Assertions (Asserts, Exec_Bounds);

      -- Iterate through the analysis phases:

      Model := Programs.Execution.Computation (Exec_Bounds);

      Iteration :
      loop
         -- Iterative improvement of the computation model and
         -- the bounds on loops and calls.

         -- At this point the computation model is know to be "clean",
         -- that is, the effects of calls are consistent with the
         -- other parts of the model.

         if not Programs.Execution.Is_Feasible (Exec_Bounds) then

            Output.Warning ("All execution paths are infeasible.");

            Bound_As_Infeasible (Exec_Bounds);

            exit Iteration;

         end if;

         -- Perform this phase:

         if Opt.Trace_Phase then

            Output.Trace ("Phase " & Phase_T'Image (Phase));

         end if;

         case Phase is

         when Value_Origins_For_Target =>

            -- Possibly refine or extend the arithmetic model by using
            -- cell-value origins in a target-specific way:

            if Decoder.Value_Origins_Applicable (
               To    => Exec_Bounds,
               Along => Call_Path)
            then

               Programs.Execution.Bound_Value_Origins (Exec_Bounds);

               if Programs.Execution.Value_Origins_Defined (Exec_Bounds) then

                  Decoder.Apply_Value_Origins (
                     Origins => Programs.Execution.Value_Origins (Exec_Bounds),
                     Bounds  => Exec_Bounds);
                  --
                  -- TBC replace "Bounds" parameter with computation model?

                  -- This may change the computation model, in which
                  -- case we should clean it up and try again.

               else

                  Output.Note ("No value-origins for target-specific analysis.");

               end if;

            end if;

         when Constant_Propagation =>

            -- Refine the arithmetic model by propagating constant values
            -- around the control-flow graph, prune infeasible flow, try
            -- to resolve boundable (dynamic) things, and try to find stack
            -- bounds if constant:

            Flow.Const.Propagate (
               Subprogram   => Subprogram,
               Asserted     => Global_Bounds and Sub_Bounds,
               Bounds       => Exec_Bounds);

         when Value_Origins_For_Flow =>

            -- Apply value-origin analysis to bound some boundable things
            -- (those that depend on values of specific cells on entry,
            -- for example on the return-address register on the SPARC):

            Programs.Execution.Bound_Value_Origins (Exec_Bounds);

            if Programs.Execution.Value_Origins_Defined (Exec_Bounds) then

               Flow.Origins.For_Flow.Resolve_Boundable_Edges (
                  Map => Programs.Execution.Value_Origins (Exec_Bounds));

            else

               Output.Note ("No value-origins for resolving boundable edges.");

            end if;

         when Applying_Assertions =>

            -- Locate the assertions for this subprogram and try to use
            -- them to bound loops and calls:

            Apply_Assertions;

            if not Programs.Execution.Is_Feasible (Exec_Bounds) then
               -- Some execution-count assertions on loops or calls
               -- make execution infeasible, or are contradictory.

               Output.Warning ("All execution paths are infeasible.");

               Bound_As_Infeasible (Exec_Bounds);

               exit Iteration;

            end if;

         when Abstract_Execution =>

            if SWEET.Included then
               -- We may use the SWEET tool.

               if Programs.Execution.Dynamic_Flow (Exec_Bounds) = Flow.Unresolved
               then
                  -- There are some dynamic edges, not yet stably resolved,
                  -- and the SWEET analyser is available. So we use it.

                  SWEET.Resolve_Dynamic_Jumps (
                     Subprogram => Subprogram,
                     Bounds     => Exec_Bounds,
                     Num_Bounds => Programs.Execution.Number_Of_Bounds (
                        Within => Bounds_Set),
                     Asserts    => Asserts);

               end if;

            elsif Opt.Trace_Phase then

               Output.Trace (Phase_T'Image (Phase) & " not available.");

            end if;

         when Arithmetic_Analysis =>

            -- Arithmetic analysis tries to bound all remaining boundable
            -- things.

            -- Can we avoid arithmetic analysis?

            Avoid_Arithmetic_Analysis (
               Exec_Bounds => Exec_Bounds,
               Choice      => Programs.Arithmetic_Analysis (Subprogram),
               Chosen      => Arithmetic_Chosen);

            if not Arithmetic_Chosen then

               Bound_Without_Arithmetic (
                  Subprogram  => Subprogram,
                  Call_Path   => Call_Path,
                  Inherit_Inv => Inherit_Inv,
                  Asserts     => Asserts,
                  Assert_Map  => Assert_Map,
                  Bounds_Set  => Bounds_Set,
                  Exec_Bounds => Exec_Bounds);

               exit Iteration;
               -- Bound_Without_Arithmetic cannot change the model
               -- in a way that would need iteration.
               -- TBC after addition of call-bounding without arithmetic.

            elsif not Programs.Reducible (Subprogram) then
               -- We cannot apply arithmetic analysis to
               -- an irreducible flow-graph.

               Output.Error (
                  "Irreducible flow-graph prevents arithmetic analysis.");

               Bound_Without_Arithmetic (
                  Subprogram  => Subprogram,
                  Call_Path   => Call_Path,
                  Inherit_Inv => Inherit_Inv,
                  Asserts     => Asserts,
                  Assert_Map  => Assert_Map,
                  Bounds_Set  => Bounds_Set,
                  Exec_Bounds => Exec_Bounds);
               --
               -- This will appear to succeed, but the bounds will not be
               -- considered "fully bounded" because the subprogram is
               -- not reducible.

               exit Iteration;
               -- Bound_Without_Arithmetic cannot change the model
               -- in a way that would need iteration.
               -- TBC after addition of call-bounding without arithmetic.

            elsif Call_Path'Length > Opt.Max_Dependency_Depth  then

               Output.Error (Text =>
                  "Maximum call-dependent analysis depth reached.");

               Bound_Without_Arithmetic (
                  Subprogram  => Subprogram,
                  Call_Path   => Call_Path,
                  Inherit_Inv => Inherit_Inv,
                  Asserts     => Asserts,
                  Assert_Map  => Assert_Map,
                  Bounds_Set  => Bounds_Set,
                  Exec_Bounds => Exec_Bounds);

               exit Iteration;
               -- Bound_Without_Arithmetic cannot change the model
               -- in a way that would need iteration.
               -- TBC after addition of call-bounding without arithmetic.

            else
               -- We can and must analyse the arithmetic.

               Bound_With_Arithmetic (
                  Subprogram  => Subprogram,
                  Call_Path   => Call_Path,
                  Initial     => Initial_Bounds,
                  Asserted    => Global_Bounds and Sub_Bounds,
                  Inherit_Inv => Inherit_Inv,
                  Asserts     => Asserts,
                  Assert_Map  => Assert_Map,
                  Bounds_Set  => Bounds_Set,
                  Exec_Bounds => Exec_Bounds);

            end if;

         end case;

         -- Some calls may now be revealed as infeasible or non-returning:

         Mark_Abnormal_Calls;

         -- Check the result of this phase:

         if not Programs.Execution.Is_Feasible (Exec_Bounds) then
            -- Some abnormal calls made the whole subprogram infeasible.

            Output.Warning ("All execution paths are infeasible.");

            Bound_As_Infeasible (Exec_Bounds);

            exit Iteration;

         elsif Flow.Computation.Dynamic_Flow (Model.all) = Flow.Growing then
            -- If dynamic edges were resolved, the flow-graph is growing
            -- and these Exec_Bounds are out of date.

            if Opt.Trace_Phase then

               Output.Trace ("Resuming decoding because the flow-graph grew.");

            end if;

            exit Iteration;

         elsif Programs.Execution.Computation_Changed (Exec_Bounds)
         and   Restarts < Opt.Max_Restarts
         then
            -- The computation model was updated, so we must also update
            -- the effects of calls etc. and then restart the analysis.

            if Opt.Trace_Phase then

               Output.Trace ("Repeating phases for changed computation model.");

            end if;

            Programs.Execution.Note_Updated_Computation (Exec_Bounds);

            Programs.Execution.Mark_Computation_Clean (Exec_Bounds);

            Restarts := Restarts + 1;

            Phase := Constant_Propagation;
            -- The phase Value_Origins_For_Target is not repeated TBC.

         else
            -- We will go on to the next Phase if there is one.

            if Programs.Execution.Computation_Changed (Exec_Bounds) then
               -- The computation model was updated but this has already
               -- happened so many times that we are bored with it. We
               -- leave it possibly inconsistent.

               Output.Error (
                    "Computation model did not converge in"
                  & Natural'Image (Restarts)
                  & " iterations and may be unsafe.");

            end if;

            if Phase < Phase_T'Last then
               -- We can go on to the next phase.

               Phase := Phase_T'Succ (Phase);

            else
               -- Last phase was satisfactory, to borrow Nero Wolfe.

               exit Iteration;

            end if;

         end if;

      end loop Iteration;

      -- That's (nearly) all folks!

      Finish_Bounds (
         Exec_Bounds => Exec_Bounds,
         Assert_Map  => Assert_Map,
         Bounds_Set  => Bounds_Set,
         Flow_Frozen => Flow_Frozen);

      -- Check that the assertions were mapped properly:

      if Assert_Map = Assertions.No_Map then

         Output.Note ("No assertion map.");

      elsif Flow_Frozen then

         Assertions.Verify_Map (
            Map   => Assert_Map,
            Valid => Valid_Map);

      end if;

   end Bound_Execution;


   procedure Find_Execution_Bounds (
      Subprogram  : in     Programs.Subprogram_T;
      Asserts     : in     Assertions.Assertion_Set_T;
      Bounds_Set  : in out Programs.Execution.Bounds_Set_T;
      Flow_Frozen :    out Boolean)
   --
   -- Finds bounds on the execution of the given Subprogram either
   -- in the Bounds_Set (if already bounded) or by analysis otherwise.
   -- In the latter case, adds the new bounds to the Bounds_Set.
   --
   -- The Flow_Frozen parameter shows whether the control-flow graph is
   -- completed (all dynamic control-flow resolved as far as we can).
   --
   is
      use type Flow.Edge_Resolution_T;
      use type Programs.Execution.Bounds_Ref;

      Exec_Bounds : Programs.Execution.Bounds_Ref :=
         Programs.Execution.Bounds_For (
            Subprogram => Subprogram,
            Within     => Bounds_Set,
            Along      => Programs.Null_Call_Path);
      -- The execution bounds, initialised to the existing bounds if any.

   begin

      if Exec_Bounds = Programs.Execution.No_Bounds then
         -- No existing bounds, find them by analysis.

         Output.Note ("No execution bounds known yet.");

         -- Finish the arithmetic effects in the flow-graph,
         -- if the decoding left them somehow incomplete:

         Decoder.Finish_Arithmetic (
            Subprogram  => Subprogram,
            Graph       => Programs.Flow_Graph (Subprogram),
            Call_Bounds => Programs.Execution.Bounds_For_Calls (
               From   => Subprogram,
               Within => Bounds_Set));

         -- Create the execution bounds object:

         Programs.Execution.Initialize_Universal_Bounds (
            Subprogram => Subprogram,
            Within     => Bounds_Set,
            Bounds     => Exec_Bounds);

         -- Analyse to find execution bounds:

         Bound_Execution (
            Exec_Bounds => Exec_Bounds,
            Params      => Storage.Bounds.Empty,
            Inherit_Inv => Storage.List_Cell_Sets.Empty,
            Asserts     => Asserts,
            Bounds_Set  => Bounds_Set,
            Flow_Frozen => Flow_Frozen);

      else
         -- Some execution bounds already known (from assertions),
         -- and are sufficiently bounded so that we do not need
         -- to analyse the subprogram.

         Output.Note ("Execution bounds already known.");

         Flow_Frozen :=
            Programs.Execution.Dynamic_Flow (Exec_Bounds) = Flow.Stable;

      end if;

   end Find_Execution_Bounds;


   procedure Bound_Executions (
      To_Bound   : in out Programs.Subprogram_Set_T;
      Asserts    : in     Assertions.Assertion_Set_T;
      Bounds_Set : in out Programs.Execution.Bounds_Set_T;
      Growing    :    out Programs.Subprogram_Set_T)
   is

      Program : constant Programs.Program_T :=
         Programs.Execution.Program (Bounds_Set);
      -- The target program in which this analysis occurs.

      All_Calls : constant Programs.Call_List_T :=
         Programs.Calls_Between (
            Subprograms => To_Bound,
            Program     => Program);
      -- All calls between the subprograms to be bounded.
      -- These calls will define the bottom-up order.

      Subs : Programs.Subprogram_List_T :=
         Programs.Sort.Bottom_Up (
            Elements => Programs.To_List (To_Bound),
            Pairs    => All_Calls);
      -- The subprograms in bottom-up calling order.

      Subprogram_Mark : Output.Nest_Mark_T;
      -- Marks the default output locus for the current subprogram.

      Frozen : Boolean;
      -- Whether the control-flow graph of the current subprogram is
      -- completed (all dynamic control flow resolved as far as we can).

   begin -- Bound_Executions

      -- Optional tracing of analysis:

      if Opt.Trace_Subprograms then

         Output.Trace ("Subprograms to be bounded:");

         Programs.Show.Show (To_Bound);

         Output.Trace ("All calls between the subprograms:");

         Programs.Show.Show (All_Calls);

         Output.Trace ("Subprograms in bottom-up order:");

         Programs.Show.Show (Subs);

      end if;

      -- Start from an empty set of growing subprograms:

      Programs.Erase (Growing);

      -- Check for recursion:

      if Subs'Length < Programs.Cardinality (To_Bound) then

         -- There is a recursion cycle, which includes the
         -- To_Bound subprograms that are missing from Subs.

         Bounds.Recursing.Report_Cycle (
            Recursive     => To_Bound,
            Non_Recursive => Subs);

         raise Recursion;

      end if;

      -- Bound the subprograms in order:

      Bounding_Loop:

      for S in Subs'Range loop

         Subprogram_Mark := Output.Nest (Programs.Locus (Subs(S)));

         if Programs.Some_Calls (From => Subs(S), Into => Growing)
         then

            -- This subprogram cannot yet be fully bounded, because it calls
            -- some subprogram in Growing, for which additional callees can
            -- still be found.

            Output.Note ("Calls some growing subprogram, bounding delayed");

            -- Moreover, some of the higher-level subprograms
            -- in Subs(S+1 ..) may call this one, which means that
            -- _their_ arithmetic analysis must be delayed. It is
            -- simpler to stop here, and start over from the bottom.

            Output.Unnest (Subprogram_Mark);

            exit Bounding_Loop;

         else
            -- The subprogram can be analysed. However, in some cases
            -- the Bounds_Set may already hold execution bounds for
            -- the subprogram. Currently this happens only if the
            -- bounds were asserted.

            Find_Execution_Bounds (
               Subprogram  => Subs(S),
               Asserts     => Asserts,
               Bounds_Set  => Bounds_Set,
               Flow_Frozen => Frozen);

            if Frozen then

               -- All dynamic control-flow is resolved, execution bounds
               -- perhaps derived.

               Programs.Remove (From => To_Bound, Removing => Subs(S));

               -- If the execution bounds are complete (fully bounded), they
               -- are universal (call-independent) bounds, which completes
               -- the bounding for this subprogram.
               -- If the execution bounds are incomplete, the analysis
               -- will be continued in a call- or path-specific way, for
               -- each call of this subprogram.
               -- Whether the bounds are complete or not, they are stored
               -- with the subprogram for later use (if complete) or display.
               -- (Storage is implicit, since all the operations that create
               -- execution bounds also store them.)

            else

               -- Some dynamic control-flow remains, the control-flow graph
               -- is still growing and re-analysis is needed.

               Programs.Add (To => Growing, Adding => Subs(S));

            end if;

         end if;

         Output.Unnest (Subprogram_Mark);

      end loop Bounding_Loop;

   end Bound_Executions;


end Bounds;
