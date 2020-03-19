-- Analyser (body)
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
-- $Revision: 1.60 $
-- $Date: 2015/10/24 20:05:44 $
--
-- $Log: analyser.adb,v $
-- Revision 1.60  2015/10/24 20:05:44  niklas
-- Moved to free licence.
--
-- Revision 1.59  2012-01-19 19:43:29  niklas
-- BT-CH-0223: Package License does not depend on package Flow.
--
-- Revision 1.58  2009-12-27 22:34:31  niklas
-- BT-CH-0205: Licensing based on code size and time/space dimensions.
--
-- Revision 1.57  2009-12-17 14:05:53  niklas
-- BT-CH-0197: Assertions on instruction roles.
--
-- Revision 1.56  2009-06-10 12:44:30  niklas
-- BT-CH-0179: Assumed properties of omitted subprograms.
--
-- Revision 1.55  2009/05/21 08:08:27  niklas
-- BT-CH-0175: Limits on the number of Warnings, Errors, Faults.
--
-- Revision 1.54  2008/03/11 22:08:05  niklas
-- BT-CH-0121: Delayed calls and other SHARC support.
--
-- Revision 1.53  2008/02/28 10:33:44  niklas
-- BT-CH-0116: Call-specific time and stack assertions.
--
-- Revision 1.52  2007/12/17 13:54:33  niklas
-- BT-CH-0098: Assertions on stack usage and final stack height, etc.
--
-- Revision 1.51  2007/10/26 12:44:33  niklas
-- BT-CH-0091: Reanalyse a boundable edge only if its domain grows.
--
-- Revision 1.50  2007/10/18 13:10:02  niklas
-- Extended procedure Trace_Flow to obey Analyser.Opt.Root_Alone.
--
-- Revision 1.49  2007/06/24 16:30:25  niklas
-- Using Opt.Warn_Eternal_Loop.
--
-- Revision 1.48  2007/06/06 19:49:42  niklas
-- Build_Nodes_And_Loops re-raises Loops.Irreducible silently.
-- Analyse traps Bounds.Recursion and calls Draw_Recursion_Graph.
--
-- Revision 1.47  2007/03/29 15:18:00  niklas
-- BT-CH-0056.
--
-- Revision 1.46  2007/01/13 13:51:01  niklas
-- BT-CH-0041.
--
-- Revision 1.45  2006/11/26 22:07:23  niklas
-- BT-CH-0039.
--
-- Revision 1.44  2006/10/24 21:41:05  niklas
-- BT-CH-0030.
--
-- Revision 1.43  2006/10/24 08:44:28  niklas
-- BT-CH-0028.
--
-- Revision 1.42  2006/08/22 13:50:47  niklas
-- BT-CH-0025.
--
-- Revision 1.41  2006/05/29 14:17:36  niklas
-- Trace_Flow now silently passes on Decoder_Error exceptions.
--
-- Revision 1.40  2006/05/29 11:22:31  niklas
-- BT-CH-0023.
--
-- Revision 1.39  2006/05/06 06:59:18  niklas
-- BT-CH-0021.
--
-- Revision 1.38  2006/02/27 10:07:13  niklas
-- Added Trace lines before the display of flow-graphs (on step
-- or node level) to identify the subprogram concerned.
--
-- Revision 1.37  2005/09/23 10:51:41  niklas
-- BT-CH-0012.
--
-- Revision 1.36  2005/09/05 11:23:37  niklas
-- BT-CH-0007.
--
-- Revision 1.35  2005/09/03 11:50:25  niklas
-- BT-CH-0006.
--
-- Revision 1.34  2005/08/08 14:01:31  niklas
-- Added trace to mark the start of loop identification.
--
-- Revision 1.33  2005/06/28 08:34:22  niklas
-- Fixed procedure Analyse to make the -bold option work and to omit
-- detailed output when there are no root calls.
-- Updated for changes in Programs.Execution.Show. Also added
-- heading for detailed output from Programs.Execution.Show.
--
-- Revision 1.32  2005/05/18 07:54:50  niklas
-- Improved Trace_Flow to assert internally a zero execution time
-- for an undecodable subprogram. This avoids a spurious error from
-- the IPET analysis of the null stub.
--
-- Revision 1.31  2005/03/22 18:19:02  niklas
-- Extended Trace_Flow to use Decoder.Stub if Decoder.Decode is
-- unable to create a flow-graph for the subprogram. This can
-- happen for example if the subprogram's code is not loaded.
--
-- Revision 1.30  2005/02/23 09:05:12  niklas
-- BT-CH-0005.
--
-- Revision 1.29  2005/02/20 15:15:35  niklas
-- BT-CH-0004.
--
-- Revision 1.28  2005/02/16 21:11:36  niklas
-- BT-CH-0002.
--
-- Revision 1.27  2004/06/26 09:43:20  niklas
-- Added Decoder.Start and Decoder.Stop.
--
-- Revision 1.26  2004/05/01 11:17:50  niklas
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
-- Revision 1.25  2003/03/11 08:32:04  holsti
-- Using execution-bounds types from Programs.Execution.
--
-- Revision 1.24  2001/12/14 10:35:40  saarinen
-- Shows only unbounded bounds if a root subprogram cannot be bounded.
--
-- Revision 1.23  2001/11/19 10:51:57  saarinen
-- Modified for Bound-T/ERC32:
-- Call to Decoder.Additional_Analysis added.
-- Call to Paths.Find_Worst_Case includes assertion set as a parameter.
--
-- Revision 1.22  2001/05/27 10:49:10  holsti
-- Using "show" options for summary output.
--
-- Revision 1.21  2001/05/20 13:31:30  holsti
-- Updated for changes in Flow and Decoder.
--
-- Revision 1.20  2001/03/21 20:32:15  holsti
-- Output with Locus values.
--
-- Revision 1.19  2001/03/16 14:22:26  holsti
-- Eternal loops detected and reported (NC_053).
--
-- Revision 1.18  2001/03/10 22:37:10  holsti
-- Added notes on result of control-flow analysis.
--
-- Revision 1.17  2001/03/10 00:25:35  holsti
-- New callees in a subprogram-set, not in a call-step set.
--
-- Revision 1.16  2001/02/19 09:38:09  holsti
-- Adapted to changes in Programs.Execution_Bounds_Ref.
--
-- Revision 1.15  2000/12/29 13:19:53  holsti
-- Removed tbas etc. in favour of NCs.
--
-- Revision 1.14  2000/12/28 17:52:27  holsti
-- Trace_Flow_And_Calls description corrected (NC_037).
-- Removed unnecessary to-be-added comments and fixed formatting.
--
-- Revision 1.13  2000/12/28 12:21:44  holsti
-- Erase Calls parameters of Decoder.Decode before call (NC_065).
-- Message re unbounded root subprogram includes subprogram name.
--
-- Revision 1.12  2000/12/13 11:11:03  sihvo
-- Changed TABs to spaces.
--
-- Revision 1.11  2000/11/24 12:05:59  sihvo
-- Added stack height analysis.
--
-- Revision 1.10  2000/11/22 14:29:18  holsti
-- Also Stub graphs are stored with Set_Flow_Graph.
--
-- Revision 1.9  2000/10/26 09:54:14  saarinen
-- Modified Estimate_WCET to check only that Root_Calls are
-- fully bounded; others are handled recursively inside
-- Programs.Fully_Bounded.
--
-- Revision 1.8  2000/09/20 18:31:21  saarinen
-- Added call to paths and processor.sub_info into decode -calls.
--
-- Revision 1.7  2000/07/25 01:38:02  holsti
-- Trace_Flow_And_Calls_And_Bound_Loops revised; uses Bounds.
--
-- Revision 1.6  2000/07/13 11:26:32  saarinen
-- Added creation of Call_Graph.
--
-- Revision 1.5  2000/07/02 18:46:01  holsti
-- Added output of flow graph on exception.
--
-- Revision 1.4  2000/06/11 21:40:27  holsti
-- Flow-graph display options added.
--
-- Revision 1.3  2000/06/11 19:05:40  holsti
-- Using Decoder.Stub for asserted subprogram.
--
-- Revision 1.2  2000/04/27 10:46:58  holsti
-- First implementation.
--


with Ada.Text_IO;
with Analyser.Opt;
with Arithmetic;
with Processor;
with Bounds;
with Bounds.Opt;
with Bounds.Stacking;
with Bounds.Timing;
with Decoder;
with Flow;
with Flow.Calls;
with Flow.Show;
with Loops;
with Loops.Show;
with Output;
with Processor.Properties;
with Programs;
with Programs.Execution.Draw;
with Programs.Execution.Draw.Opt;
with Programs.Execution.Paths;
with Programs.Execution.Show;
with Programs.Execution.Tables;
with Storage.Bounds;


package body Analyser is
--
-- The following summarises the structure of this package, in
-- terms of the call-graph of the main operation, Analyse,
-- and its chief part, Trace_Flow_And_Calls_And_Bound_Loops:
--
-- Analyse:
--
--    Trace_And_Bound for root-calls;
--    Decoder.Additional_Analysis;
--    if execution path bounds found then
--       for each root call:
--          Programs.Execution.Paths.Find_Worst_Case for this call
--
-- Trace_And_Bound:
--
--    initialise set of untraced subs to the set of root subprograms;
--    (all the untraced subs will, in fact, be Raw or Flow_Resolved)
--
--    while there remain untraced subs:
--
--       Trace_Flow_And_Calls for the untraced subs:
--
--          initialise set of raw-or-resolved subs to the untraced subs;
--
--          while there remain raw-or-resolved subs:
--             for each raw-or-resolved sub:
--                Trace_Flow for the subprogram:
--                   Decoder.Decode;
--                   move the sub to the set of traced-or-dynamic subs;
--                   add new, Raw subs to the raw-or-resolved subs;
--
--       Bounds.Bound_Executions for the traced-or-dynamic subs,
--          moving some subs to higher levels, and some subs
--          from Flow_Dynamic to Flow_Resolved;
--
--       update the set of untraced subs by:
--
--       - removing the subs at Bounded and Call_Dependent levels;
--
--       - adding (or keeping) subs for which the control-flow
--         graph was extended by resolved dynamic jumps (moving them
--         from Flow_Dynamic to Flow_Resolved).
--


   -- INTERNAL OPERATIONS:


   procedure Trace_Loops (
      Graph      : in Flow.Graph_T;
      Subprogram : in Programs.Subprogram_T;
      Program    : in Programs.Program_T)
   --
   -- Using :
   --   Graph, a flow-graph with basic blocks collected,
   --   Subprogram that owns the given graph
   -- Giving:
   --    loop structure of graph, stored with the subprogram,
   --    warnings about eternal loops, if any;
   --    or propagates Loops.Irreducible if the flow-graph
   --    is irreducible.
   --
   -- The program's symbol-table is needed for displaying
   -- source-line locations.
   --
   is

      Luups : constant Loops.Loops_T := Loops.Loops (Graph);
      -- The loops in the graph.
      -- May raise Loops.Irreducible.

   begin

      -- Store the loop structure:

      Programs.Set_Loops (
         Within => Subprogram,
         To     => Luups);

      -- Optional display:

      if Analyser.Opt.Show_Loops then

         Loops.Show.Show_Loops (Programs.Loops_Of (Subprogram));

      end if;

      -- Check for eternal loops:

      for L in Luups'Range loop

         if  Loops.Eternal (Luups(L))
         and Opt.Warn_Eternal_Loop
         then

            Output.Warning (
               Locus => Loops.Show.Locus (
                  Luup   => Luups(L),
                  Within => Graph,
                  Source => Programs.Symbol_Table (Program)),
               Text => "Eternal loop (no exit edges).");

         end if;

      end loop;

   end Trace_Loops;


   procedure Build_Nodes_And_Loops (
      Graph      : in   Flow.Graph_T;
      Subprogram : in   Programs.Subprogram_T;
      Program    : in   Programs.Program_T)
   --
   -- Collects the steps in the Graph into nodes and then finds
   -- the loops in the Graph. The Graph is the flow-graph of the
   -- given Subprogram, within the given Program.
   --
   -- May propagate Loops.Irreducible.
   --
   is
   begin

      if Analyser.Opt.Show_Flow_Steps then

         Output.Trace ("Flow-graph steps follow.");

         Flow.Show.Show_Steps (
            Graph  => Graph,
            Source => Programs.Symbol_Table (Program));

      end if;

      -- Collect the basic blocks:

      Flow.Collect_Blocks (Graph);

      if Analyser.Opt.Show_Flow_Nodes then

         Output.Trace ("Flow-graph nodes (basic blocks) follow.");

         Flow.Show.Show_Nodes (
            Graph  => Graph,
            Source => Programs.Symbol_Table (Program));

      end if;

      -- Identify the loops, or detect that the flow-graph
      -- is irreducible:

      if Analyser.Opt.Show_Loops then

         Output.Trace ("Identifying the loops.");

      end if;

      Trace_Loops (
         Graph      => Graph,
         Subprogram => Subprogram,
         Program    => Program);
      --
      -- May raise Loops.Irreducible.


   exception

   when Loops.Irreducible =>
      -- The flow-graph structure is not reducible, so we
      -- were unable to find the loops.

      raise;

   when Output.Too_Many_Problems =>
      -- Not our problem, pass it on.

      raise;

   when X : others =>

      Output.Exception_Info (
         Text       => "Analyser.Build_Nodes_And_Loops",
         Occurrence => X);

      raise;

   end Build_Nodes_And_Loops;


   procedure Expand_Asserted_Dynamic_Calls (
      Subprogram : in     Programs.Subprogram_T;
      Graph      : in     Flow.Graph_T;
      Asserts    : in     Assertions.Assertion_Set_T;
      Expanded   :    out Boolean)
   --
   -- If the Subprogram contains unresolved dynamic calls, and
   -- the Assertion contains assertions that define the callees
   -- of these unresolved calls, this procedure expands the dynamic
   -- calls to use the asserted callees. This may add steps to the
   -- flow-graph which thus becomes Expanded.
   --
   -- Note that resolving a dynamic call to a single possible callee
   -- does not add any steps to the flow-graph, so this does not
   -- expand the graph. The graph is Expanded only if some dynamic
   -- call has two or more possible callees.
   --
   is
      use type Flow.Step_Index_T;

      Initial_Max_Step : constant Flow.Step_Index_T :=  Flow.Max_Step (Graph);
      -- The number of steps in the Graph, before possible expansions.

      Unresolved : Flow.Dynamic_Edge_List_T :=
         Flow.Calls.Unresolved_Calls (Graph);
      -- The unresolved dynamic calls.

      Resolved : Natural := 0;
      -- The number of Unresolved calls that were resolved by
      -- assertions.


      procedure Expand (Call : in out Flow.Dynamic_Edge_T)
      --
      -- Tries to expand this Call using assertions that list
      -- the possible callees.
      --
      is

          Callees : constant Programs.Subprogram_List_T :=
             Assertions.Callees (
                Call    => Call,
                From    => Subprogram,
                Model   => null,
                Asserts => Asserts);
          -- Asserted callees, if any.

          Targets : Flow.Target_List_T (Callees'Range);
          -- The entry addresses of the Callees.

      begin

         if Callees'Length > 0 then
            -- Some callees are asserted.

            for T in Targets'Range loop

               Targets(T) := Programs.Entry_Address (Callees(T));

            end loop;

            Flow.Take_Asserted_Targets (
               Edge    => Call.all,
               Targets => Targets,
               Graph   => Graph);

            Flow.Mark_Stable (Call.all);

            Flow.Remove_Dynamic_Edge (
               Edge => Call,
               From => Graph);

            Resolved := Resolved + 1;

         else

            Output.Note (
               Locus => Flow.Show.Locus (
                  Edge   => Call.all,
                  Source => Programs.Symbol_Table (Subprogram)),
               Text  => "No callees asserted for dynamic call.");

         end if;

      end Expand;


   begin  -- Expand_Asserted_Dynamic_Calls;

      for U in Unresolved'Range loop

         Expand (Call => Unresolved(U));

      end loop;

      Expanded := Flow.Max_Step (Graph) > Initial_Max_Step;

      Output.Note (
           "Assertions resolved"
         & Natural'Image (Resolved)
         & " of"
         & Natural'Image (Unresolved'Length)
         & " dynamic calls.");

      if Expanded then

         Output.Note (
              "This increased the flow-graph from"
            & Flow.Step_Index_T'Image (Initial_Max_Step)
            & " to"
            & Flow.Step_Index_T'Image (Flow.Max_Step (Graph))
            & " steps.");

      end if;

   end Expand_Asserted_Dynamic_Calls;


   procedure Set_Assumed_Properties (
      Exec_Bounds : in Programs.Execution.Bounds_Ref)
   --
   -- Sets some assumed properties of the execution bounds of a
   -- subprogram that is omitted from the analysis (stubbed), either
   -- by a specific "omit" assertion or because we are analysing
   -- only root subprograms "alone" and this subprogram is not a
   -- root.
   --
   is
      use Programs.Execution;

      Stacks : constant Programs.Stacks_T :=
         Programs.Stacks (Within => Program (Exec_Bounds));
      -- All the stacks in the program.

      Limit : Storage.Bounds.Limit_T;
      -- An assumed limit on the net change in local stack height.

   begin

      -- Assumed net change in stack height:

      for S in Stacks'Range loop

         if not Final_Stack_Height_Known (Stacks(S), Exec_Bounds) then
            -- The final stack height has not been asserted to
            -- a single value. Perhaps a value should be assumed:

            Limit := Processor.Properties.Assumed_Net_Change (
               Stack => Stacks(S),
               Stub  => Subprogram (Exec_Bounds));

            if Storage.Bounds.Known (Limit) then
               -- A single net-change value is to be assumed.

               Output.Note (
                    "Assuming net change "
                  & Storage.Bounds.Image (Limit)
                  & " for "
                  & Programs.Name (Stacks(S)));

               Bound_Final_Stack_Height (
                  Stack  => Stacks(S),
                  To     => (Min => Limit, Max => Limit),
                  Within => Exec_Bounds,
                  Silent => True);

            end if;

         end if;

      end loop;

   end Set_Assumed_Properties;


   procedure Trace_Flow (
      Subprogram  : in     Programs.Subprogram_T;
      Program     : in out Programs.Program_T;
      Asserts     : in     Assertions.Assertion_Set_T;
      Bounds_Set  : in out Programs.Execution.Bounds_Set_T;
      Bounded     :    out Boolean)
   --
   -- Starts or continues tracing the execution and control flow
   -- within the Subprogram in the Program.
   --
   -- Subprogram
   --    The subprogram to be analysed. We will extend the flow-graph
   --    of this subprogram and (re-) build the basic-block and loop
   --    structures for it.
   -- Program
   --    The program that contains the Subprogram. If new calls
   --    and new callee subprograms are found in the Subprogram,
   --    they are added to the Program. Some of these may be new
   --    "shoots" that will need to be traced and analysed too,
   --    in a later call of this procedure.
   -- Asserts
   --    Assertions that apply to this Program. In particular,
   --    if this call of Trace_Flow is the first one for this
   --    Subprogram, we check for assertions on this Subprogram
   --    that make its tracing and analysis unnecessary.
   -- Bounds_Set
   --    The set of execution bounds for this Program. If the Asserts
   --    completely bound the Subprogram the corresponding execution
   --    bounds are here added to Bounds_Set.
   -- Bounded
   --    Whether the Subprogram was completely bounded by the
   --    Assertions, or was omitted from analysis for some reason.
   --
   is
      use type Processor.Time_T;

      Subprogram_Mark : constant Output.Nest_Mark_T :=
         Output.Nest (Programs.Locus (Subprogram));
      --
      -- Marks the default locus for the subprogram.

      Graph : Flow.Graph_T := Programs.Flow_Graph (Subprogram);
      --
      -- The flow-graph of the subprogram.
      -- Note that this is a reference type, so the flow-graph
      -- can be changed although this Graph variable is not changed.

      Entry_Step_Tag : constant Flow.Step_Tag_T := (
         Context => Flow.No_Context,
         Data    => Flow.Any_Data,
         State   => Processor.Properties.Entry_State (Subprogram));
      --
      -- The step-tag (context and program-sequencing state) on entry
      -- to the subprogram.

      Started_Decoding : Boolean := False;
      --
      -- Wether Decoder.Start was called and needs a matching Decoder.Stop.

      Omitted_Or_Alone : Boolean := False;
      --
      -- Whether the Subprogram is stubbed because it is asserted to
      -- be omitted, or because we are analysing only root subprograms
      -- "alone" and the Subprogram is not a root.

      Asserted_Bounds : Programs.Execution.Bounds_Ref;
      --
      -- The execution bounds for the possibly asserted time
      -- and/or space when a stub flow-graph is used.
      -- They are stored in the Bounds_Set but not used here.

      Loose_Edge : Flow.Loose_Edge_T;
      --
      -- One of the loose edges from the graph.

      Calls_Expanded : Boolean;
      -- Whether the graph was expanded by assertions that
      -- resolve dynamic calls.

   begin

      Bounded := True;
      -- Unless we find differently below.

      if Flow.Is_Empty (Graph) then
         -- This is the first try at flow-analysis for this subprogram.

         if Programs.Stub (Subprogram) then
            -- An assertion tells us to omit the subprogram from the
            -- analysis and replace it by a stub.

            Output.Note ("Subprogram is omitted.");

            if Opt.Trace_Omitted_Subprograms then

               Output.Result (
                  Key  => "Omitted",
                  Text => "Omitted subprogram.");

            end if;

            -- Bounded remains True, so the subprogram will be
            -- represented by a stub flow-graph.

            Omitted_Or_Alone := True;

         elsif Programs.Unused (Subprogram) then
            -- The subprogram will never be called (there may be calls
            -- in the program, but the calls are never executed).
            -- There is no need to analyse this subprogram, so we make
            -- a stub (surrogate), minimal flow-graph without looking
            -- at the instructions in the subprogram.

            Output.Note ("Subprogram is unused.");

            if Opt.Trace_Unused_Subprograms then

               Output.Result (
                  Key  => "Unused",
                  Text => "Unused subprogram.");

            end if;

            -- Bounded remains True, so the subprogram will be
            -- represented by a stub flow-graph.

         else
            -- The subprogram is not explicitly omitted from the analysis
            -- and may be called so it must be analysed unless its relevant
            -- space/time consumption has been fully asserted or it is
            -- omitted for some other reason (eg. "-alone").

            -- Check for asserted bounds on the desired consumption:

            if Bounds.Opt.Bound_Time
            and then
               not Assertions.Subprogram_Time_Bounded (
                      Subprogram => Subprogram,
                      Asserts    => Asserts)
            then

               Output.Note ("Analysis for time is needed.");

               Bounded := False;

            end if;

            if Bounds.Opt.Bound_Stack
            and then
               not Assertions.Stacks_Bounded (
                      Subprogram => Subprogram,
                      Asserts    => Asserts)
            then

               Output.Note ("Analysis for stacks is needed.");

               Bounded := False;

            end if;

            if not (Bounds.Opt.Bound_Time or Bounds.Opt.Bound_Stack) then
               -- The purposes of this analysis is only to explore and
               -- build the call-graphs and control-flow graphs.

               Output.Note ("Analysis for flow-graphs only.");

               Bounded := False;

            end if;

         end if;

         -- Stub or analyse?

         if Bounded then
            -- There is no need to analyse this subprogram.
            -- We construct a stub.

            Output.Note ("Using stub for flow-graph.");

            Decoder.Stub (
               Entry_Tag  => Entry_Step_Tag,
               Graph      => Graph,
               Program    => Program,
               Subprogram => Subprogram);
            --
            -- The stub graph will not have any loose edges, so
            -- flow analysis will not add anything.

            Programs.Set_Stub (Subprogram => Subprogram, To => True);

         elsif Opt.Root_Alone and (not Programs.Root (Subprogram))
         then
            -- This is a non-root, non-stub subprogram but we should
            -- analyse only root subprograms. Ergo we pretend that
            -- the subprogram is asserted into a null stub.

            Output.Note ("Not a root. Using stub for flow-graph.");

            Decoder.Stub (
               Entry_Tag  => Entry_Step_Tag,
               Graph      => Graph,
               Program    => Program,
               Subprogram => Subprogram);
            --
            -- The stub graph will not have any loose edges, so
            -- flow analysis will not add anything.

            Programs.Set_Stub (Subprogram => Subprogram, To => True);

            Bounded := True;
            --
            -- The stub shall not be analysed.

            Omitted_Or_Alone := True;
            --
            -- The stub may get some assumed properties to help
            -- the analysis of its callers.

         else
            -- Must analyse subprogram, as some desired resource
            -- consumption was not sufficiently bounded by assertions.
            -- The Graph is still empty.

            -- Start decoding from the entry point:

            Output.Note ("Control-flow analysis starts.");

            Decoder.Start (
               Program    => Program,
               Subprogram => Subprogram,
               Resuming   => False);

            Started_Decoding := True;

            Decoder.Decode (
               Loose_Edge   => (
                  Source => Flow.No_Step,
                  Cond   => Arithmetic.Always,
                  Time   => Processor.No_Time,
                  Info   => Processor.No_Loose_Edge_Info,
                  Target => Entry_Step_Tag),
               Program      => Program,
               Subprogram   => Subprogram,
               Graph        => Graph);

            -- Subprogram is marked as "not a stub" by default.

            if Flow.Is_Empty (Graph) then
               -- Some problem in creating the flow-graph.
               -- Perhaps the code for this subprogram is not
               -- present in the executable file under analysis.

               Output.Error ("Cannot decode subprogram; using null stub.");

               Decoder.Stub (
                  Entry_Tag  => Entry_Step_Tag,
                  Graph      => Graph,
                  Program    => Program,
                  Subprogram => Subprogram);
               --
               -- The stub graph will not have any loose edges, so
               -- flow analysis will not add anything.

               -- Assert the execution time as zero to avoid IPET analysis
               -- of the stub graph:

               Bounded := True;
               -- Well, the stub will be bounded.

               Programs.Set_Stub (Subprogram => Subprogram, To => True);

            end if;

         end if;

      else
         -- The flow-graph is not empty so this is not the first
         -- time we are tracing this subprogram. However, it may
         -- happen that the flow-graph needs no more decoding (yet)
         -- because there are only new dynamic edges (resolved from
         -- other dynamic edges) and no loose edges to new code.

         if Flow.Loose_Edges (Graph) then
            -- There are some actual loose edges.

            Output.Note (Text => "Control-flow analysis resumes.");

            Decoder.Start (
               Program    => Program,
               Subprogram => Subprogram,
               Resuming   => True);

            Started_Decoding := True;

         end if;

         Bounded := False;

      end if;


      -- Follow loose edges to new steps, as long as they last:

      while Flow.Loose_Edges (Graph)
      loop

         -- When there are loose edges, we take them one by one in
         -- in increasing order, and decode the target step. The
         -- Decoder is responsible for adding the steps and edges
         -- to the graph:

         Loose_Edge := Flow.First_Loose_Edge (From => Graph);

         Decoder.Decode (
            Loose_Edge => Loose_Edge,
            Program    => Program,
            Subprogram => Subprogram,
            Graph      => Graph);

      end loop;


      -- Report why we stopped, if needed:

      if not Started_Decoding then
         -- No need to report that we stopped.

         null;

      elsif Flow.Some_Dynamic_Edges (Within => Graph) then

         Output.Note ("Control-flow analysis suspended due to dynamic flow.");

         Decoder.Stop (
            Program    => Program,
            Subprogram => Subprogram,
            Graph      => Graph,
            Suspended  => True);

      elsif not Bounded then
         -- Completed tracing the flow of a non-stub graph.

         Output.Note ("Control-flow analysis finished.");

         Decoder.Stop (
            Program    => Program,
            Subprogram => Subprogram,
            Graph      => Graph,
            Suspended  => False);

      end if;

      -- The step-graph is complete (apart from dynamic edges)
      -- so we can build the node graph and the loops.

      begin
         -- The exception Loops.Irreducible may arise here.

         Build_Nodes_And_Loops (Graph, Subprogram, Program);

         -- Possibly asserted execution bounds:

         if Bounded then
            -- If the resource consumption is asserted we know (or
            -- can assume) that the stub flow-graph is reducible.
            -- We create execution bounds to hold the asserted bounds
            -- on time and/or space:

            Programs.Execution.Initialize_Asserted_Bounds (
               Subprogram => Subprogram,
               Within     => Bounds_Set,
               Bounds     => Asserted_Bounds);

            Bounds.Timing.Apply_Assertions (
               Asserts     => Asserts,
               Exec_Bounds => Asserted_Bounds);

            Bounds.Stacking.Apply_Assertions (
               Asserts     => Asserts,
               Exec_Bounds => Asserted_Bounds);

            if  Omitted_Or_Alone
            and Opt.Assume_Omit_Alone
            then

               Set_Assumed_Properties (Exec_Bounds => Asserted_Bounds);

            end if;

            Programs.Execution.Store_Bounds (
               Bounds => Asserted_Bounds,
               Within => Bounds_Set);

         else
            -- Possibly asserted callees of dynamic calls:

            Expand_Asserted_Dynamic_Calls (
               Subprogram => Subprogram,
               Graph      => Graph,
               Asserts    => Asserts,
               Expanded   => Calls_Expanded);

            if Calls_Expanded then
               -- New steps were added to the Graph, so we must
               -- rebuild the nodes and loops:

               Build_Nodes_And_Loops (Graph, Subprogram, Program);

            end if;

         end if;

      exception

      when Loops.Irreducible =>
         -- As noted in Build_Nodes_And_Loops.

         Programs.Set_Irreducible (Subprogram);

      end;

      -- The Graph was (most likely) expanded, so some of the old
      -- boundable edges may have to be re-analysed and there may
      -- be new boundable edges, too:

      Flow.Find_Unstable_Dynamic_Edges (Graph);

      -- And then the Graph is ready for analysis, if any.

      Output.Unnest (Subprogram_Mark);


   exception

   when Decoder.Decoder_Error =>
      -- Already reported, we can assume.

      Output.Unnest (Subprogram_Mark);

      raise;

   when Output.Too_Many_Problems =>
      -- Not our problem, pass it on.

      raise;

   when X : others =>

      Output.Exception_Info (
         Text       => "Analyser.Trace_Flow",
         Occurrence => X);

      if Opt.Show_Flow_Steps then

         Output.Note (Text => "Current flow graph follows");

         Flow.Show.Show_Steps (
            Graph  => Graph,
            Source => Programs.Symbol_Table (Program));

      end if;

      if Opt.Show_Flow_Nodes then

         Output.Note (Text => "Current flow graph follows");

         Flow.Show.Show_Nodes (
            Graph  => Graph,
            Source => Programs.Symbol_Table (Program));

      end if;

      Output.Unnest (Subprogram_Mark);

      raise;

   end Trace_Flow;


   procedure Trace_Flow_And_Calls (
      To_Trace   : in     Programs.Subprogram_Set_T;
      Program    : in out Programs.Program_T;
      Asserts    : in out Assertions.Assertion_Set_T;
      Traced     : in out Programs.Subprogram_Set_T;
      Bounds_Set : in out Programs.Execution.Bounds_Set_T)
   --
   -- Starts or continues to trace the flow of execution within a
   -- set of subprogram To_Trace, including calls to other subprograms
   -- in this set or not in this set.
   --
   -- To_Trace
   --    The initial set of subprograms to be analysed. These are either
   --    new, Raw subprograms, or subprograms in which dynamic jumps or
   --    calls have been resolved and so flow-tracing can continue.
   --    We will extend the flow-graphs of these subprograms and (re-)
   --    build their basic-block and loop structures, and the same for
   --    all direct and indirect callees of the To_Trace subprograms.
   -- Program
   --    The program that contains the subprograms To_Trace.
   --    If new calls and new callee subprograms are found in these
   --    subprograms, they are added to the Program. Some of these may
   --    be new "shoots" that must traced and analysed too. We continue
   --    digging into the call-graph until we can go no further, either
   --    because all flow-graphs and calls are completed or because some
   --    subprograms contain dynamic jumps or dynamic calls that must be
   --    analysed and bounded before flow-tracing can continue.
   -- Asserts
   --    Assertions that apply to this Program. In particular, before we
   --    first try to trace the execution flow in a subprogram we check
   --    for assertions on this subprogram that make its tracing and
   --    analysis unnecessary (eg. the WCET is asserted and we are
   --    analysing for time-bounds only).
   --    Possibly updated with assertion-usage information.
   -- Traced
   --    The set of subprograms for which we have traced the execution
   --    flow and the calls. We will add all subprograms from To_Trace
   --    and all their direct and indirect callees to the Traced set,
   --    except for subprograms for which Assertions are given that make
   --    further analysis unnecessary and subprograms that are decoded
   --    by integrating their code into the caller's flow-graph.
   -- Bounds_Set
   --    The set of execution bounds for this Program. If the Asserts
   --    completely bound some subprogram the corresponding execution
   --    bounds are here added to Bounds_Set (and the subprogram is not
   --    added to the Traced set).
   --
   is

      Work_Set : Programs.Subprogram_Set_T;
      -- Subprograms that are left to be flow-traced.

      Assertion_Bounded : Boolean;
      -- Whether a subprogram was bounded by assertions, so
      -- that it need not be bounded by analysis. In this case
      -- the subprogram is not placed in the Traced set.

   begin

      -- Create a "work-set" holding the subprograms to be traced:

      Programs.Erase (Work_Set);

      Programs.Add (To => Work_Set, Adding => To_Trace);

      Work_Set_Loop:
      while not Programs.Is_Empty (Work_Set)
      loop
         -- Trace the flow of all subprograms in the work-set.
         -- Record new callees and newly analysed subprograms.

         declare

            Work_List : Programs.Subprogram_List_T :=
                        Programs.To_List (Work_Set);
            -- The work-set as a list.

         begin

            for W in Work_List'Range loop

               Trace_Flow (
                  Subprogram => Work_List(W),
                  Program    => Program,
                  Asserts    => Asserts,
                  Bounds_Set => Bounds_Set,
                  Bounded    => Assertion_Bounded);

               if not Assertion_Bounded then
                  -- The execution of the subprogram was not
                  -- bounded by assertions, so we must try to
                  -- bound it by analysis.

                  Programs.Add (To => Traced, Adding => Work_List(W));
                  -- The subprogram was already present in Traced,
                  -- if it was re-traced for resolved dynamic jumps.
                  -- No harm is done by adding it once again.

               end if;

               if not Flow.Size_Licence_Valid then

                  Output.Note ("Exceeded licensed program size.");

                  exit Work_Set_Loop;

               end if;

            end loop;

         end;

         -- Continue working on any new callees that were found:

         Programs.Erase (Work_Set);

         Programs.Move_Shoots (From => Program, To => Work_Set);

      end loop Work_Set_Loop;

   end Trace_Flow_And_Calls;


   procedure Trace_And_Bound (
      Root_Calls : in     Programs.Call_List_T;
      Program    : in out Programs.Program_T;
      Asserts    : in out Assertions.Assertion_Set_T;
      Bounds_Set : in out Programs.Execution.Bounds_Set_T)
   --
   -- Analyses and bounds the execution of a set of root subprograms
   -- and their direct and indirect callees.
   --
   -- Root_Calls
   --    The root calls that define the set of root subprograms.
   --    We will trace the flow of execution in each root subprogram
   --    and each direct or indirect callee. We will build the control-flow
   --    graphs, basic-block structures and loop structures. When needed,
   --    we will apply various forms of analysis (Bounds.Bound_Executions)
   --    to bound dynamic jumps and calls, bound loop iterations, bound
   --    stack usage, and find bounds on the execution time and/or
   --    stack usage of the root subprograms including their callees.
   -- Program
   --    The program that contains the root subprograms.
   --    We will add all calls and all direct and indirect callees to
   --    the Program.
   -- Asserts
   --    Assertions that apply to this Program and to all our analyses.
   --    Possibly updated with assertion-usage information.
   -- Bounds_Set
   --    All execution bounds that we find will be stored here.
   --
   is

      Untraced : Programs.Subprogram_Set_T;
      -- The set of subprograms for which some flow-tracing is
      -- needed. These are either "Raw" subprograms, with no flow
      -- yet traced, or "Flow_Resolved" subprograms, where some
      -- dynamic jumps have been resolved and the flow-graphs need
      -- to be extended.

      Unbounded : Programs.Subprogram_Set_T;
      -- The set of subprograms for which execution bounding is
      -- needed, but for which flow-tracing is complete (or suspended
      -- because the subprogram contains unresolved dynamic jumps).
      --
      -- This set will be non-empty only if Untraced is also non-empty,
      -- because the only reason for these subprograms being Unbounded
      -- is that some (probably) lower subrogram had its dynamic jumps
      -- resolved and must be flow-traced before these (probably) higher
      -- subprograms can be bounded. Here, "higher" and "lower" are used
      -- in the call-tree sense, with caller "higher" than callee.
      --
      -- Note that when the resolved control-flow of the (probably) lower
      -- subprogram is traced, it may turn out to contain a call of the
      -- (probably) higher subprogram, which shows that the relationship
      -- between these subprograms was in fact the other way around (or
      -- that there is recursion).

      Iterations : Natural := 0;
      -- Counts the number of iterations of alternating control-flow
      -- analysis and data-flow analysis with resolution of dynamic jumps.

      Root : Programs.Subprogram_T;
      -- One of the roots.

   begin

      -- Initially, the untraced subs are the root subprograms:

      Programs.Erase (Untraced);

      for R in Root_Calls'Range loop

         Root := Programs.Callee (Root_Calls(R));

         if Programs.Stub (Root) then

            Output.Warning (
               Locus => Programs.Locus (Root),
               Text  => "Root subprogram is a stub.");

         end if;

         Programs.Add (
            To     => Untraced,
            Adding => Root);

      end loop;

      -- Initially, the set of unbounded subprograms is empty,
      -- because the only ones we know of, the root subprograms,
      -- are in the Untraced state:

      Programs.Erase (Unbounded);

      -- The following loop iterates control-flow analysis and
      -- data-flow analysis until all dynamic branches are resolved.

      Flow_Analysis:
      while not Programs.Is_Empty (Untraced)
      loop

         if Iterations >= Analyser.Opt.Max_Iterations then

            Output.Error (Text =>
               "Dynamism bounding did not converge in"
               & Natural'Image (Iterations)
               & " iterations.");

            -- Abort analysis:
            exit Flow_Analysis;

         end if;

         Iterations := Iterations + 1;

         Output.Note (Text =>
            "Dynamism bounding iteration number"
            & Natural'Image (Iterations));

         -- Decode and flow-analyse the subprograms and callees:
         --
         -- On the first iteration of the flow_analysis loop, the
         -- following call traces the flow of the whole call tree
         -- of the root subprograms, except when dynamic branches
         -- hide some flow. On each later flow_analysis iteration,
         -- some dynamic branches have been resolved, and this
         -- call extends the flow-graphs of all affected subprograms.
         -- This may find new callees, which are also traced.

         Trace_Flow_And_Calls (
            To_Trace   => Untraced,
            Program    => Program,
            Asserts    => Asserts,
            Traced     => Unbounded,
            Bounds_Set => Bounds_Set);

         exit Flow_Analysis when not Flow.Size_Licence_Valid;

         -- Bound the data-dependent control flow:

         Bounds.Bound_Executions (
            To_Bound   => Unbounded,
            Asserts    => Asserts,
            Bounds_Set => Bounds_Set,
            Growing    => Untraced);

         -- If there were no dynamic jumps, the sets Untraced and Unbounded
         -- are empty here after the first execution of the flow_analysis loop,
         -- which thus terminates.
         --
         -- If there were dynamic jumps or calls, the Untraced set contains
         -- the subprograms with resolved dynamic jumps or calls, and
         -- Unbounded contains the (probably) higher-level subprograms that
         -- were not yet bounded. Flow_analysis is repeated until the
         -- dynamic jumps and calls stabilise or for the maximum number
         -- of iterations.
         --
         -- If there were dynamic calls and some of them were resolved
         -- then new callees (shoots) may have been found. These must be
         -- added to the Untraced set:

         Programs.Move_Shoots (From => Program, To => Untraced);

      end loop Flow_Analysis;

   end Trace_And_Bound;


   procedure Tabulate_Time (
      Root   : in Programs.Subprogram_T;
      Bounds : in Programs.Execution.Bounds_Ref)
   --
   -- Outputs a table of the execution-time Bounds, showing how they
   -- are made up of bounds on lower-level callees.
   --
   is
      use Programs.Execution;
   begin

      if       Bounds /= No_Bounds
      and then Time_Bounded (Bounds)
      then

         Tables.Tabulate_Subprogram_Bounds (
            Root      => Bounds,
            Qualified => False);

      else

         Output.Note (
            Locus => Programs.Locus (Root),
            Text  => "Time not bounded, so times cannot be tabulated.");

      end if;

   end Tabulate_Time;



   -- OPERATION IMPLEMENTATIONS:


   procedure Analyse (
      Program    : in out Programs.Program_T;
      Asserts    : in out Assertions.Assertion_Set_T;
      Bounds_Set :    out Programs.Execution.Bounds_Set_T)
   is

      All_Roots_Bounded : Boolean := True;
      -- Whether all root subprograms are fully bounded.

      Need_Time : constant Boolean := Bounds.Opt.Bound_Time;
      -- Whether we should show worst-case execution time bounds.

      Need_Space : constant Boolean := Bounds.Opt.Bound_Stack;
      -- Whether we should show worst-case memory usage bounds.


      procedure Check_Bounding (Call : in Programs.Call_T)
      --
      -- Check that the given root call is fully bounded.
      --
      is
         use Programs.Execution;

         Call_Bounds : constant Bounds_Ref :=
            Bounds_For (
               Root   => Call,
               Within => Bounds_Set);
        -- The bounds for the call, such as they are.

      begin

         if      Call_Bounds = No_Bounds
         or else not Bounded (Call_Bounds)
         then

            Output.Error (
               Locus => Programs.Locus (Programs.Callee (Call)),
               Text  => "Could not be fully bounded.");

            Show.Show_Unbounded (
               Bounds  => Call_Bounds,
               Within  => Bounds_Set,
               Path    => (1 => Call),
               Callers => Analyser.Opt.Show_View (Analyser.Opt.Callers));

            All_Roots_Bounded := False;

         end if;

      end Check_Bounding;


      procedure Analyse_Roots
      --
      -- Analyse all the root calls.
      --
      is

         Root_Calls : Programs.Call_List_T :=
            Programs.Root_Calls (Program);
         -- The root-subprogram calls, from which the analysis starts.

      begin

         Trace_And_Bound (
            Root_Calls => Root_Calls,
            Program    => Program,
            Asserts    => Asserts,
            Bounds_Set => Bounds_Set);

         if not Flow.Size_Licence_Valid then

            Output.Error ("Exceeded licensed limit on program size.");

            return;

         end if;

         -- Additional target-specific analysis:

         Output.Note ("Target-specific additional analysis starts.");

         Decoder.Additional_Analysis (
            Program    => Program,
            Asserts    => Asserts,
            Bounds_Set => Bounds_Set);

         -- Display worst-case call-paths for stacks:

         if  Bounds.Opt.Bound_Stack
         and Analyser.Opt.Show_Stack_Path
         then

            for R in Root_Calls'Range loop

               Programs.Execution.Show.Show_Stack_Path (
                  Root_Call => Root_Calls(R),
                  Within    => Bounds_Set);

            end loop;

         end if;

         -- Perhaps compute worst-case execution times:

         if Need_Time then

            -- Find the worst-case paths and execution times:

            Output.Note ("Generic worst-case path and time analysis starts.");

            for R in Root_Calls'Range loop

               Programs.Execution.Paths.Find_Worst_Case (
                  Call       => Root_Calls(R),
                  Bounds_Set => Bounds_Set);

            end loop;

         end if;

         -- Check that all is bounded, and display the unbounded
         -- parts if any:

         for C in Root_Calls'Range loop

            Check_Bounding (Call => Root_Calls(C));

         end loop;

         -- Perhaps tabulate the bounds:

         if Analyser.Opt.Tabulate_Time then

            for R in Root_Calls'Range loop

               Tabulate_Time (
                  Root   => Programs.Callee (Root_Calls(R)),
                  Bounds => Programs.Execution.Bounds_For (
                     Root   => Root_Calls(R),
                     Within => Bounds_Set));

            end loop;

         end if;

         -- Perhaps show the detailed bounds themselves:

         if  Programs.Execution.Show.Some_Content (Opt.Show_View)
         and Root_Calls'Length > 0
         then
            -- Something to show that should be shown.

            Ada.Text_IO.New_Line;
            Ada.Text_IO.Put_Line ("Details as requested by -show:");
            Ada.Text_IO.New_Line;

            Programs.Execution.Show.Show (
               Calls  => Root_Calls,
               Bounds => Bounds_Set,
               View   => Opt.Show_View);

         end if;

      end Analyse_Roots;


   begin  -- Analyse

      -- Initialize the set of bounds:

      Programs.Execution.Initialize_Bounds_Set (
         Program   => Program,
         For_Time  => Need_Time,
         For_Space => Need_Space,
         Set       => Bounds_Set);

      -- Processor-specific pre-analysis:

      Decoder.Pre_Analysis (Bounds_Set);

      -- Analyse control flow, calls, loops and loop-bounds:

      Analyse_Roots;

      Assertions.Warn_About_Unused_Options (
         Assertion_Set => Asserts,
         Program       => Program);

      -- Perhaps draw some flow and call graphs:

      if Programs.Execution.Draw.Opt.Draw_Graphs then

         Programs.Execution.Draw.Draw_Graphs (Bounds_Set);

      end if;

      -- Processor-specific post-analysis:

      Decoder.Post_Analysis (Bounds_Set);

   exception

   when Bounds.Recursion =>
      -- The call-graph of the root subprogram(s) is recursive.

      if Programs.Execution.Draw.Opt.Draw_Graphs then

         Programs.Execution.Draw.Draw_Recursion_Graph (Program);

      end if;

   end Analyse;


end Analyser;
