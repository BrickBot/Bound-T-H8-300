-- Analyser (decl)
--
-- Master analysis algorithms, driving the analysis of control-flow,
-- loop structure, data-flow and WCET.
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
-- $Revision: 1.8 $
-- $Date: 2015/10/24 19:36:46 $
--
-- $Log: analyser.ads,v $
-- Revision 1.8  2015/10/24 19:36:46  niklas
-- Moved to free licence.
--
-- Revision 1.7  2007-06-06 19:49:28  niklas
-- Analyse traps Bounds.Recursion and calls Draw_Recursion_Graph.
--
-- Revision 1.6  2004/05/01 11:17:50  niklas
-- First Tidorum version.
-- Renamed the procedure Estimate_WCET to Analyse, to give equal support
-- to time and space analysis. Extended the implementation to obey options
-- on analysis type.
-- Added toleration of irreducible graphs and eternal loops.
-- Changed handling of time assertions so that Trace_Flow creates and
-- stores the asserted bounds and Trace_Flow_And_Calls stops tracing
-- the (stub) subprogram (does not place it in the Traced set).
-- Changed Trace_Flow to dump the flow-graph on exception only if the
-- options Opt.Show_Flow_Steps and/or Opt.Show_Flow_Nodes are set.
-- Added support for the new options Cease_Unless_Bounded, Tabulate_Time
-- and Show_Space.
--
-- Revision 1.5  2003/03/11 08:32:04  holsti
-- Using execution-bounds types from Programs.Execution.
--
-- Revision 1.4  2000/07/25 01:38:03  holsti
-- Trace_Flow_And_Calls_And_Bound_Loops revised; uses Bounds.
--
-- Revision 1.3  2000/07/13 11:26:33  saarinen
-- Added creation of Call_Graph.
--
-- Revision 1.2  2000/04/27 10:46:58  holsti
-- First implementation.
--


with Assertions;
with Programs;
with Programs.Execution;


package Analyser is


   -- PROVIDED OPERATIONS:


   procedure Analyse (
      Program    : in out Programs.Program_T;
      Asserts    : in out Assertions.Assertion_Set_T;
      Bounds_Set :    out Programs.Execution.Bounds_Set_T);
   --
   -- Analyses the given program, under the given assertions and
   -- options (not an explicit parameter), giving a set of execution
   -- bounds.
   --
   -- Using :
   --    a target program, with a set of root-calls to be analysed,
   --    a set of assertions provided by the user,
   --    analysis options (as defined by the Options package and
   --    stored here and there).
   -- Giving:
   --    control-flow information (flow graphs, loops, call graphs),
   --    execution time bounds (if requested),
   --    memory usage bounds (if requested),
   --    diagnostic messages re the use made of the assertions,
   --    set of execution bounds for the root subprograms and their
   --    callees.
   --
   -- The results include the flow-graphs of all subprograms (roots
   -- and their direct and indirect callees), with loop-structures
   -- identified, and a call graph.
   --
   -- The quantitative analysis results are stored in the set of
   -- execution bounds. When the bounding of a subprogram needs
   -- actual parameter values or other calling context, the execution
   -- bounds for the caller contain (references to) the call-path-
   -- dependent execution bounds for the call.
   --
   -- The mapping of the assertions to the program structures is
   -- also stored in the set of execution bounds. TBA.
   --
   -- The assertion set is updated only to mark the way the
   -- assertions were used in the analysis, including whether
   -- the mapping of an assertion to the target program was
   -- ambiguous or void (impossible).
   --
   -- If recursion is found in the call-graph of the roots, this is
   -- reported in error messages and the Bounds_Set will be incomplete.


end Analyser;
