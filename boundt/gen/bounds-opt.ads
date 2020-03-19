-- Bounds.Opt (decl)
--
-- Command-line options for the dynamic bounding functions.
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
-- $Revision: 1.26 $
-- $Date: 2015/10/24 19:36:47 $
--
-- $Log: bounds-opt.ads,v $
-- Revision 1.26  2015/10/24 19:36:47  niklas
-- Moved to free licence.
--
-- Revision 1.25  2013-02-05 20:09:11  niklas
-- Using Options.General.Trace_Resolution for Trace_Data_Resolution.
--
-- Revision 1.24  2011-08-31 04:23:33  niklas
-- BT-CH-0222: Option registry. Option -dump. External help files.
--
-- Revision 1.23  2009-12-27 22:34:31  niklas
-- BT-CH-0205: Licensing based on code size and time/space dimensions.
--
-- Revision 1.22  2008-11-09 21:41:57  niklas
-- BT-CH-0158: Option "-trace instr".
--
-- Revision 1.21  2008/04/26 19:19:43  niklas
-- BT-CH-0124: Joint loop counters and induction variables.
--
-- Revision 1.20  2007/12/22 15:23:46  niklas
-- BT-CH-0101: Option "-trace graph".
--
-- Revision 1.19  2007/10/31 12:16:00  niklas
-- BT-CH-0095: Arithmetic analysis of "live" dynamic data refs.
--
-- Revision 1.18  2007/10/28 09:32:45  niklas
-- BT-CH-0092: Arithmetic analysis of dynamic data refs is optional.
--
-- Revision 1.17  2007/08/06 09:20:42  niklas
-- Added option Trace_Phase.
--
-- Revision 1.16  2007/01/13 13:51:02  niklas
-- BT-CH-0041.
--
-- Revision 1.15  2006/09/04 15:07:03  niklas
-- Added the option "-trace arith", as Bounds.Opt.Trace_Arith.
--
-- Revision 1.14  2005/09/14 11:50:30  niklas
-- Extended the meaning of Trace_Context to display the asserted and
-- derived bounds when starting any arithmetic analysis of a
-- subprogram, whether universal or context-dependent.
--
-- Revision 1.13  2005/09/12 19:02:57  niklas
-- BT-CH-0008.
--
-- Revision 1.12  2005/06/28 08:36:10  niklas
-- Amended the names of -trace options to Trace_Xxx.
-- Added option Trace_Context.
--
-- Revision 1.11  2005/02/23 09:05:14  niklas
-- BT-CH-0005.
--
-- Revision 1.10  2005/02/16 21:11:39  niklas
-- BT-CH-0002.
--
-- Revision 1.9  2004/05/01 20:36:33  niklas
-- First Tidorum version.
-- Added option Bound_Time.
-- Moved option Warn_Unresolved_Data to the new package Flow.Dynamic.Opt.
--
-- Revision 1.8  2003/02/27 14:36:44  holsti
-- Added option Warn_Unresolved_Data.
--
-- Revision 1.7  2001/12/18 20:34:44  holsti
-- Default_Max_Dependency_Depth added.
--
-- Revision 1.6  2001/12/14 10:48:34  saarinen
-- Added option 'max_dependency_depth'.
--
-- Revision 1.5  2001/03/10 22:35:47  holsti
-- Show_Cell_Sets (-trace io) added.
--
-- Revision 1.4  2001/02/19 14:48:06  holsti
-- Option Bound_Stack added.
--
-- Revision 1.3  2001/02/14 06:49:12  holsti
-- Option to trace counters added.
--
-- Revision 1.2  2001/01/07 21:59:49  holsti
-- Show_Parameters added. Default values := False.
--
-- Revision 1.1  2000/08/18 18:15:40  holsti
-- First version.
--


with License.Dimension;
with Options.Bool;
with Options.Nat;
with Programs.Opt;


package Bounds.Opt is


   Bound_Time_Opt : aliased Options.Bool.Option_T (
      Default => License.Dimension.Time);
   --
   -- Whether to compute bounds on the execution time.
   --
   Bound_Time : Boolean renames Bound_Time_Opt.Value;


   Bound_Stack_Opt : aliased Options.Bool.Option_T (
      Default => License.Dimension.Space
         and not License.Dimension.Time);
   --
   -- Whether to compute bounds on the stack usage.
   -- The default is True when space analysis is the only
   -- licensed dimension of analysis.
   --
   Bound_Stack : Boolean renames Bound_Stack_Opt.Value;


   procedure Set_Stack_Analysis (To : in Boolean);
   --
   -- Sets the value of Bound_Stack.


   Trace_Subprograms : Boolean renames Programs.Opt.Trace_Subprograms;
   --
   -- Whether to list (trace) the subprograms to which bounding is
   -- being applied.


   Trace_Phase_Opt : aliased Options.Bool.Option_T (Default => False);
   --
   -- Whether to trace the progress through the various phases of
   -- analysis: constant propagation, ..., arithmetic analysis.
   --
   Trace_Phase : Boolean renames Trace_Phase_Opt.Value;


   Trace_Arith_Opt : aliased Options.Bool.Option_T (Default => False);
   --
   -- Whether the trace the starting and stopping of each arithmetic
   -- analysis (Calculator execution).
   --
   Trace_Arith : Boolean renames Trace_Arith_Opt.Value;


   Trace_Context_Opt : aliased Options.Bool.Option_T (Default => False);
   --
   -- Whether to list (trace) the asserted and derived bounds on
   -- input parameters, global variables, and local variables, when
   -- starting to bound the execution of a subprogram.
   --
   Trace_Context : Boolean renames Trace_Context_Opt.Value;


   Trace_Nubs_Opt : aliased Options.Bool.Option_T (Default => False);
   --
   -- Whether to list (trace) those steps in each flow-graph that are
   -- interesting for the arithmetic analysis, that is, the steps that
   -- contain some boundable things that were not bounded by simpler
   -- analyses such as constant propagation.
   --
   Trace_Nubs : Boolean renames Trace_Nubs_Opt.Value;


   Trace_Cell_Sets_Opt : aliased Options.Bool.Option_T (Default => False);
   --
   -- Whether to list (trace) the sets of input cells, output cells,
   -- and basis cells for each arithmetic analysis, as soon as they
   -- are defined.
   --
   Trace_Cell_Sets : Boolean renames Trace_Cell_Sets_Opt.Value;


   function Trace_Data_Resolution return Boolean;
   --
   -- Whether to trace the process of resolving dynamic data access.
   -- This option tracks the value of Options.General.Trace_Resolution.


   Warn_Unresolved_Data_Opt : aliased Options.Bool.Option_T (Default => False);
   --
   -- Whether to issue a warning message when a dynamic data access
   -- is left unresolved (with an unknown source or target cell).
   --
   Warn_Unresolved_Data : Boolean renames Warn_Unresolved_Data_Opt.Value;


   Trace_Graphs_Opt : aliased Options.Bool.Option_T (Default => False);
   --
   -- Whether to trace the steps and edges and their properties in
   -- the finished flow-graphs for all subprograms.
   --
   Trace_Graphs : Boolean renames Trace_Graphs_Opt.Value;


   Trace_Instructions_Opt : aliased Options.Bool.Option_T (Default => False);
   --
   -- Whether to trace, in each completed flow-graph, the symbolic
   -- disassembly and the arithmetic effect and computational effort
   -- of each step, and the necessary arithmetic condition and
   -- execution time for each edge.
   --
   -- This option is meant to help test the arithmetic modelling of
   -- instructions while being insensitive to the source-level and
   -- machine-level locations (source file, line number, code address),
   -- as far as possible. The trace line for a step does show the locus
   -- of the step, but this can easily be ignored when the trace is
   -- compared to a reference file. The trace line for an edge shows
   -- the source instruction disassembled and the offset to the
   -- target instruction.
   --
   Trace_Instructions : Boolean renames Trace_Instructions_Opt.Value;


   Max_Dependency_Depth_Opt : aliased Options.Nat.Option_T (Default => 3);
   --
   -- The maximum analysis depth for call-dependent call paths, in
   -- terms of the number of calls involved.
   -- Call-dependendent bounding will not be attempted for longer
   -- call-paths (so they will be unbounded in the result).
   --
   Max_Dependency_Depth : Natural renames Max_Dependency_Depth_Opt.Value;


   Max_Restarts_Opt : aliased Options.Nat.Option_T (Default => 5);
   --
   -- The maximum number of times the analysis process will be restarted
   -- from an earlier phase (see Bound_Execution.Phase_T) because a later
   -- phase has changed the model (flow-graph or computation model) in a
   -- way that requires repeating the earlier phases.
   --
   Max_Restarts : Natural renames Max_Restarts_Opt.Value;


end Bounds.Opt;
