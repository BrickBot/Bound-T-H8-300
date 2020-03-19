-- Programs.Execution (decl)
--
-- Models and bounds of program and subprogram execution, as derived
-- from the program analysis.
--
-- Note that this package was originally a part of the Programs package,
-- and much of the version history is embedded in the history of the
-- Programs package.
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
-- $Revision: 1.41 $
-- $Date: 2015/10/24 19:36:51 $
--
-- $Log: programs-execution.ads,v $
-- Revision 1.41  2015/10/24 19:36:51  niklas
-- Moved to free licence.
--
-- Revision 1.40  2013/12/12 22:26:04  niklas
-- BT-CH-0262: Corrections to new value-origin analysis.
--
-- Revision 1.39  2013/12/08 22:05:57  niklas
-- BT-CH-0259: Storing value-origin analysis results in execution bounds.
--
-- Revision 1.38  2009-11-27 11:28:07  niklas
-- BT-CH-0184: Bit-widths, Word_T, failed modular analysis.
--
-- Revision 1.37  2009-10-07 19:26:10  niklas
-- BT-CH-0183: Cell-sets are a tagged-type class.
--
-- Revision 1.36  2009-04-16 16:29:24  niklas
-- BT-CH-0171: Stack pointers in call effects.
--
-- Revision 1.35  2008/11/09 21:43:05  niklas
-- BT-CH-0158: Output.Image (Time_T) replaces Programs.Execution.Image.
--
-- Revision 1.34  2008/09/24 08:38:53  niklas
-- BT-CH-0146: Assertions on "loop starts <bound> times".
-- BT-CH-0146: Loop-repeat assertions set both lower and upper bound.
-- BT-CH-0146: Report locations of contradictory "count" assertions.
-- BT-CH-0146: Contradictory "count" assertions imply infeasibility.
--
-- Revision 1.33  2008/07/28 19:23:45  niklas
-- BT-CH-0140: Detect contradictory execution-count bounds.
--
-- Revision 1.32  2008/07/23 09:07:16  niklas
-- BT-CH-0139: Fix recursion in Programs.Execution.Paths.
--
-- Revision 1.31  2008/07/14 19:16:58  niklas
-- BT-CH-0135: Assertions on "instructions".
--
-- Revision 1.30  2008/02/27 14:58:49  niklas
-- BT-CH-0116: Call-specific time and stack assertions.
--
-- Revision 1.29  2008/02/23 13:34:04  niklas
-- BT-CH-0115: Wcet_Loop output and option -loop_time.
--
-- Revision 1.28  2008/02/18 13:24:10  niklas
-- BT-CH-0111: Processor-specific info in execution bounds.
--
-- Revision 1.27  2007/12/17 13:54:40  niklas
-- BT-CH-0098: Assertions on stack usage and final stack height, etc.
--
-- Revision 1.26  2007/11/12 21:37:28  niklas
-- BT-CH-0097: Only arithmetic analysis marks boundable edge domain.
--
-- Revision 1.25  2007/08/17 14:44:01  niklas
-- BT-CH-0074: Stable and Unstable stacks.
--
-- Revision 1.24  2007/07/09 13:46:25  niklas
-- Added a variant of Add_Inputs_For_Unbounded_Calls that updates
-- a Cell_Set_T, instead of a Small_Cell_Set_T.
--
-- Revision 1.23  2007/03/18 12:50:40  niklas
-- BT-CH-0050.
--
-- Revision 1.22  2007/02/13 20:24:50  Niklas
-- BT-CH-0044.
--
-- Revision 1.21  2007/01/21 19:31:48  niklas
-- BT-CH-0042.
--
-- Revision 1.20  2007/01/13 13:51:06  niklas
-- BT-CH-0041.
--
-- Revision 1.19  2006/12/05 18:48:37  niklas
-- BT-CH-0040.
--
-- Revision 1.18  2006/11/20 18:59:02  niklas
-- Added the function Bounds_For_Calls to give the universal bounds
-- for all calls from a given Subprogram, itself perhaps not bounded.
--
-- Revision 1.17  2006/05/27 21:39:53  niklas
-- BT-CH-0020.
--
-- Revision 1.16  2006/05/26 13:55:11  niklas
-- Added function Calling_Protocol, for RapiTime export.
--
-- Revision 1.15  2006/03/25 13:34:09  niklas
-- Added query functions Executed_Edges and Executed_Call_Bounds.
-- Added functions to compute the Total_Time of a set of nodes
-- or edges, or of all edges within some execution bounds (based
-- on the earlier private function Total_Time). Modified the
-- Total_Time functions for loops and calls accordingly.
--
-- Revision 1.14  2005/10/09 08:10:23  niklas
-- BT-CH-0013.
--
-- Revision 1.13  2005/09/20 09:57:47  niklas
-- Added function Flow_Graph (Bounds_Ref).
--
-- Revision 1.12  2005/08/24 10:28:59  niklas
-- Added stuff to support the Analysis Workbench, including
-- the functions Link (From Bounds Via Call), No_Links,
-- Counts_Set (Bounds), Count (Node), Count (Edge),
-- Start_Count (Loop), Head_Count (Loop), Neck_Count (Loop),
-- Repeat_Count (Loop), Call_Count (Call), Call_Count (Link),
-- Total_Time (Loop), Total_Time (Call) and Total_Time (Link).
--
-- Revision 1.11  2005/08/08 17:55:15  niklas
-- Made Bounds_Set_T non-limited to let AW store it in widgets.
-- Added the function Bound_At to access execution bounds using the
-- program-wide unique index Bounds_Index_T.
-- Added the functions Loop_Neck_Bound and Loop_Repeat_Bound to
-- query loop-bounds for a given loop (not all loops at once).
--
-- Revision 1.10  2005/06/29 19:35:31  niklas
-- Added Prune_Flow.
--
-- Revision 1.9  2005/06/29 12:54:34  niklas
-- Added the Locus function for loops.
--
-- Revision 1.8  2005/06/12 07:30:24  niklas
-- Changed the display of calls from A->B to A=>B, to avoid
-- confusion with '-' used in approximate line-numbers.
--
-- Revision 1.7  2005/05/09 15:34:07  niklas
-- Added initial support for value-origin analysis, by adding the
-- operation Remove_From_Output.
--
-- Revision 1.6  2005/04/17 12:37:00  niklas
-- Removed the constant Unknown_Stack_Height, now unused.
--
-- Revision 1.5  2005/02/23 09:05:20  niklas
-- BT-CH-0005.
--
-- Revision 1.4  2005/02/20 15:15:37  niklas
-- BT-CH-0004.
--
-- Revision 1.3  2005/02/16 21:11:48  niklas
-- BT-CH-0002.
--
-- Revision 1.2  2004/05/01 09:51:07  niklas
-- First Tidorum version.
-- Taking Cell_T stuff from Storage, not Arithmetic.
-- Added Analysis_Level_T item Universally_Bounded.
-- Added description of analysis level: WCET assertion, constant
-- propagation, sublevels Paths_Bounded, Time_Bounded, Failed.
-- Added operations Assert_Universal_Time and Assert_Path_Specific_Time
-- to indicate that a time-bound is asserted and not computed.
-- Tolerate irreducible subprograms as one kind of unbounded paths.
-- Add concepts of links (Link_T) between execution bounds.
-- Add attributes for Program_T and Assertion_Map_T to Execution Bounds.
-- This reduces the number of parameters that must be passed around.
-- Add edge and node times as Execution Bounds attributes so that they
-- can depend on call path.
-- Major extensions to stack-usage analysis, including definition and
-- safety levels and the concept of take-off stack height. Separate
-- between analysis for time bounds and analysis for space bounds.
-- Warn of an attempt to set a negative stack limit and substitute a
-- zero limit.
-- Cache "boundedness" state for Execution Bounds to avoid repeated
-- traversal of a bounds graph.
-- Removed the function Lowest_WCET. It was used only in ERC32 floating
-- point blocking analysis and is no longer needed there.
-- Added optional Trace output to some operations.
-- Added a variant of the function Bounds_For to get the execution
-- bounds on a root call.
-- Corrected Initialize_Bounds to always create the Call_Bounds and
-- the new Take_Off_Limits even if there are no calls. Thus these
-- components will be valid accesses to null vectors, rather than
-- null accesses.
--
-- Revision 1.1  2003/03/11 08:28:28  holsti
-- First version split off from the parent package Programs.
--


with Arithmetic;
with Assertions;
with Calling;
with Flow;
with Flow.Computation;
with Flow.Execution;
with Flow.Execution.Times;
with Flow.Origins;
with Loops;
with Output;
with Processor.Execution;
with Storage;
with Storage.Bounds;


package Programs.Execution is
--
-- Defines the types that model the execution of the target program under
-- analysis, both in terms of the constraints (bounds) on the execution
-- and in terms of the computed extremal (worst-case) path and time, which
-- are the final results of the analysis.


   type Analysis_Level_T is (
      Raw,
      Flow_Dynamic,
      Flow_Resolved,
      Flow_Traced,
      Universally_Bounded,
      Call_Dependent);
   --
   -- The levels of progression of the analysis of a subprogram,
   -- with some taint of analysis results, too.
   --
   -- Raw
   --    The subprogram has been found through a call (or calls)
   --    to it, but has not yet been analysed in any other way.
   --    It has no flow-graph or higher-level results.
   --
   -- Flow_Dynamic
   --    The control-flow of the subprogram has been analysed (traced)
   --    but some dynamic jumps were found and have not yet been
   --    resolved, so the tracing is incomplete. However, an incomplete
   --    basic-block graph and loop-structure have been created using the
   --    resolved part of the flow graph. Data-flow analysis will be
   --    used to try to resolve the dynamic jumps.
   --
   -- Flow_Resolved
   --    The control-flow of the subprogram has been analysed (traced),
   --    some dynamic jumps were found, and some of them have been
   --    resolved to their targets, and so the control-flow tracing
   --    can be continued to extend the control-flow graph.
   --    Basic-blocks and loops have be recomputed after tracing.
   --
   -- Flow_Traced
   --    The control-flow of the subprogram has been analysed (traced)
   --    and is either fully static, or all its dynamic jumps were
   --    resolved (using data-flow analysis). The control-flow graph
   --    is thus complete, and the basic-block graph and loop-
   --    structure also. It may also happen that some dynamic jumps
   --    could not be resolved and were simply deleted, and then the
   --    flow-graph remains incomplete.
   --
   -- Universally_Bounded
   --    Execution bounds for the subprogram have been derived and do
   --    not depend on the parameters. Of course, the control-flow was
   --    resolved as a prerequisite (subsuming the Flow_Traced level).
   --
   -- Call_Dependent
   --    Execution bounds for the subprogram must be computed per call,
   --    since we failed to derive parameter-independent bounds.
   --    The control-flow was resolved as a prerequisite (subsuming
   --    the Flow_Traced level).
   --
   -- A given subprogram progresses thru these levels as follows:
   --
   -- Initially, the subprogram is created at the Raw level, either
   -- through being listed as a root-subprogram, or by being found
   -- for the first time as the callee of a call in another
   -- subprogram. As a special case, if a sufficient time/space bounds
   -- are asserted for the subprogram, it is not analysed at all, but
   -- kicked directly from Raw to Universally_Bounded. Otherwise,
   -- analysis of a Raw subprogram starts by instruction decoding and
   -- flow analysis. It is also possible to assert that a subprogram
   -- is omitted from the analysis, even without sufficient assertions
   -- on time/space bounds; this also means that the subprogram is not
   -- analysed. Such a subprogram is said to have "vague" bounds on
   -- the non-asserted time/space dimensions.
   --
   -- When control-flow analysis (tracing) is applied to the Raw
   -- subprogram, the subprogram either goes directly to the
   -- Flow_Traced level if it has no dynamic jumps, or first to
   -- Flow_Dynamic, from which it will later move to Flow_Resolved
   -- when the dynamic jumps are resolved. Further tracing on the
   -- Flow_Resolved level will either discover more dynamic jumps
   -- and cause a return to Flow_Dynamic, or move on to Flow_Traced.
   --
   --
   -- Data-flow anaysis is used to resolve dynamic jumps and other
   -- dynamic elements of a subprogram at the Flow_Dynamic and
   -- Flow_Traced levels. However, when dynamic jumps are resolved
   -- on the Flow_Dynamic level, extending the control-flow graph,
   -- the data-flow models based on the earlier, incomplete flow
   -- graph are discarded because they do not cover all executions
   -- of the extended flow-graph.
   --
   -- The data-flow analyses used include:
   --
   --    o   liveness (use-def)
   --    o   constant propagation
   --    o   value origin (copy propagation, value numbering, SSA)
   --    o   Presburger modeling.
   --
   -- The first time data-flow analysis is applied to a subprogram on
   -- the Flow_Traced level (or a subprogram that rises from
   -- Flow_Resolved to Flow_Traced due to this data-flow analysis)
   -- shows if the execution can be bounded without knowledge of
   -- the parameters. If so, the subprogram rises to the Universally
   -- Bounded level where it also remains. Otherwise, the subprogram
   -- enters the Call_Dependent level, and also remains there. Thus,
   -- Universally_Bounded and Call_Dependent are the two mutually
   -- exclusive final levels.
   --
   -- With respect to execution-time bounds, the Universally_Bounded
   -- and Call_Dependent levels are divided into three sub-levels:
   --
   --    o   Time Computable, where bounds on the execution paths
   --        (loops, branches, calls) are known, and only the (IPET)
   --        computation to find the worst-case execution path is
   --        not yet done;
   --    o   Time Computed, where bounds on the execution time have
   --        been calculated from the execution paths;
   --    o   Failed, when the calculcation of the execution-time
   --        bounds failed for some reason. This has three forms:
   --        infeasible (IPET problem is infeasible), unbounded (IPET
   --        problem is not bounded), and failed for some other reason.
   --
   -- When the execution bounds are call-dependent, it is not really
   -- the subprogram itself that resides at a certain sub-level, but
   -- the (more or less complete) bounds derived for the subprogram
   -- for a certain call-path. Thus, for one call-path we may not
   -- find any execution-path bounds, so for this path the level is
   -- only Flow_Traced, while for other paths we may find bounds
   -- and reach the Time_Bounded level.
   --
   -- The following can be call-path (context-) dependent:
   --
   --    o   computation model (arithmetic effect of each flow step,
   --        logical precondition of each flow edge)
   --    o   feasibility or infeasibility of flow-graph parts
   --    o   loop-invariant cells TBA
   --    o   loop bounds
   --    o   summary arithmetic effect of loops TBA
   --    o   stack usage, including "take-off" height for calls
   --    o   calling protocol (parameter mapping) for calls
   --    o   whether a call can (feasibly) return to caller
   --    o   execution bounds for calls (ie. for callees in the
   --        context of these calls).
   --    o   input/output cell-sets for the subprogram itself
   --    o   summary arithmetic effect of the whole subprogram (this
   --        is currently derived from the set of output cells but TBA
   --        may include more detailed info).
   --
   -- The following cannot be call-path (context-) dependent:
   --
   --    o   the control-flow graph (thus, dynamic jumps must be
   --        resolved in a universal, context-independent way).
   --
   -- However, note that while the basic structure of the control-
   -- flow graph cannot be context-dependent, some parts of the graph
   -- may be known as infeasible in some contexts.



   --
   ---   Execution Bounds
   --


   type Bounds_Set_T is private;
   --
   -- The set of execution bounds derived for a given target program.
   -- This is a set of "execution bounds" objects, each of which applies
   -- to a subprogram (universally, for any call) or to the call at
   -- the end of a specific call-path.
   --
   -- Before use, every Bounds_Set_T object must be initialized with
   -- the operation Initialize_Bounds_Set (see below).
   --
   -- This type has reference semantics.
   --
   -- The attributes of a Bound_Set_T object include:
   --
   -- > Whether bounds are sought for execution time or stack space
   --   or both time and space.
   --
   -- > Execution bounds keyed on call-paths.
   --
   -- > The total number of call "links" between these execution bounds.
   --   A "link" represents a call  between subprograms as it is
   --   reflected in dependencies between execution bounds. A link
   --   shows that specific execution bounds for the caller depend on
   --   specific execution bounds for the callee because the caller
   --   contains a call of this callee.
   --
   -- Once some execution bounds are stored in a Bounds_Set_T certain
   -- attributes of the bounds are considered frozen and cannot be
   -- modified. The frozen attributes are those that affect the
   -- computation model and the feasibility of executing elements of
   -- the control-flow graph. Other attributes of the bounds may still
   -- be set or updated (although the need for this is an artifact of
   -- the currently used order of analysis phases).


   No_Bounds_Set : constant Bounds_Set_T;
   --
   -- A special value of Bounds_Set_T that indicates an empty or missing
   -- set of bounds. This is the default value of any Bounds_Set_T
   -- variable.


   type Bounds_Ref is private;
   --
   -- Execution bounds apply to a subprogram either universally,
   -- without reference to calling context, or specifically to
   -- a certain call-path. Context-dependent bounds can also be
   -- though of as bounds on the execution of the call that is the
   -- last call in the call-path.
   --
   -- An execution bounds object has the following attributes that
   -- are always present and defined:
   --
   -- > Whether bounds are sought for execution time or stack space
   --   or both time and space.
   --
   -- > The subprogram to which the bounds apply.
   --
   -- > The call-path (perhaps empty) to which the bounds apply.
   --
   -- > A reference to the execution bounds for the caller, when
   --   this execution-bounds object is created specifically for a
   --   given call in the context of the referenced execution
   --   bounds for the caller.
   --
   -- > The program that contains the subprogram and the other
   --   subprograms in the call-path.
   --
   -- > A unique sequential index (1, 2, ...). The index is unique
   --   over all execution bounds within one Bounds_Set_T.
   --   The index can be used to get the execution bounds with a
   --   known index from a given Bounds_Set_T. The index can also
   --   be used to keep track of which bounds have been displayed
   --   or otherwise processed.
   --
   -- > A sequential index (1, 2, ...) that identifies the bounds
   --   within all bounds on the same subprogram. Note that this
   --   index is not visible (there is no query function); it is
   --   just a means for iterating over all bounds for the same
   --   subprogram.
   --
   -- The program-attribute is included to shorten the parameter
   -- lists of the operations on execution bounds.
   --
   -- Depending on the progress and success of the analysis, an
   -- execution bounds object may have some subset of the attributes
   -- that hold analysis results as follows:
   --
   -- > Processor-specific information on the execution, as defined
   --   in Processor.Execution.Info_T.
   --
   -- > The computation model (arithmetic effects and conditions
   --   ascribed to the control-flow graph of this subprogram) to
   --   which the bounds apply. The decoder defines a "primitive"
   --   computation model by assigning arithmetic effects and
   --   edge conditions as it builds the control-flow graph. Later
   --   analysis stages create refined computation models which
   --   are stored in execution bounds objects. The computation
   --   model includes the effects of any calls on the computation
   --   in this (caller) subprogram. When the callee is a stub, or
   --   calls stubs, the effect of the call may have to be updated
   --   when the computation model is refined to refer to new cells.
   --
   -- > A value-origin (reaching definitions) analysis of the
   --   computation model. This may or may not be present, depending
   --   on the options chosen for the analysis.
   --
   -- > The set of all cells that are referenced in the computation
   --   model or are input cells to unbounded calls.
   --
   -- > The "stub level" of the subprogram, which is the length of
   --   the shortest feasible call-path from the subprogram to some
   --   stub subprogram, or to an unresolved dynamic call. The stub
   --   level is zero when the subprogram is itself a stub and is
   --   "infinite" ('Last) when the subprogram is not itself a stub
   --   and never calls a stub, directly or indirectly.
   --
   -- > The mapping of assertions to subprogram parts (flow-graph
   --   parts) as used for deriving these bounds.
   --
   -- > The input cells that were, or are, necessary for bounding
   --   the execution. The cells are viewed from the subprogram's
   --   own frame. (Thus, for execution bounds on a call the
   --   cells are in the callee frame.)
   --
   -- > The initial bounds on cells at subprogram entry, including
   --   input-cell values from the call site, all types of assertions,
   --   processor-specific Entry_Bounds, and the initial local stack
   --   heights.
   --
   -- > The basis cells for Presburger arithmetic analysis (undefined
   --   if arithmetic analysis was not used).
   --
   -- > The output cells that are assigned (altered) by the execution.
   --   The cells are viewed from the subprogram's own frame. The set
   --   of output cells may depend on the context (call-path) and
   --   the related bounds on the input cells. The set of output cells
   --   does not contain any cells detected or asserted as invariant
   --   in the subprogram, nor any cells that are "private" to the
   --   subprogram and cease to exist on return from the subprogram,
   --   nor the cells that model local stack height (see the final
   --   stack height attribute below).
   --
   -- > The summarised arithmetic effect of an execution of the
   --   subprogram, viewed from the subprogram's own frame.
   --   The precision of the summarised effect depends on which
   --   analyses are applied, and of course on the structure of the
   --   subprogram; it may be a gross overapproximation of the real
   --   effect. (The summarised effect is currently derived from the
   --   set of output cells but TBA may be more detailed.)
   --
   -- > The net effect of an execution of the subprogram on the
   --   height of the program's Unstable stacks, that is whether the
   --   subprogram pushes more than it pops or vice versa. Since the
   --   local stack height for an Unstable stack is zero on entry to
   --   the subprogram, the net number of pushes minus pops is equal
   --   to the final local stack height on return from the subprogram.
   --   This attribute is a part of the summarised arithmetic effect
   --   but is expressed separately. The value (pushes - pops) may not
   --   be known exactly and is then bounded to an interval.
   --   For Stable stacks there is no such net effect and the caller's
   --   local stack height is stable (invariant) over any call.
   --
   -- > The worst-case execution time of each flow-edge between
   --   two steps in the control-flow graph. The decoder initially
   --   places such times in the control-flow graph itself. Later,
   --   target-specific additional analysis phases can modify the
   --   times (usually increasing them) in a universal or a call-
   --   path-dependent way. Call-path-specific edge-times can be
   --   placed in execution-bounds objects.
   --
   -- > The worst-case execution time of each basic block (node)
   --   in the subprogram's flow-graph, not including the time
   --   spent in lower-level callees. The total time (including
   --   callees) can be queried if desired. The time of each node
   --   is usually computed from the "effort" that the Decoder
   --   assigns to each step, some execution time assigned to
   --   edges between steps, and the local processor "power" that
   --   is available in the node. The power can depend on the
   --   call-path and therefore so can the per-node times.
   --
   -- > Derived or asserted bounds on the loops (entry edges,
   --   neck-edges, and repeat-edges).
   --
   -- > Any asserted (or derived) bounds on the number of times
   --   other edges or nodes can be executed, per call of the
   --   subprogram.
   --
   -- > A flag that shows if some of these execution-count and
   --   loop-repetition bounds are themselves contradictory (void
   --   intervals).
   --
   -- > Whether we should assume (by assertion) that there are
   --   enough execution-count assertions on parts of the
   --   subprogram to define a worst-case execution path, even if
   --   some loops are unbounded or the flow-graph is irreducible.
   --
   -- > The derived worst-case path, in the form of execution
   --   counts of nodes and edges in the subprogram's flow graph.
   --
   -- > The total worst-case execution time, either computed from
   --   the worst-case path, or asserted by the user.
   --   The time can also be marked as "failed", meaning that
   --   an attempt to compute the time failed, and should not be
   --   attempted again.
   --
   -- > A boolean flag showing that the final time-analysis step,
   --   that of finding the worst-case path and calculating the
   --   worst-case execution time, has been done. This guides the
   --   bottom-up traversal of the (call) graph of execution
   --   bounds during this final phase. It is implemented as an
   --   attribute of the execution bounds, rather than as a local
   --   variable in the traversal algorithm, because the algorithm
   --   is not always centralised; versions of Bound-T for some
   --   target processors need to implement this traversal in a
   --   dispersed, on-the-fly manner.
   --
   -- > Bounds on the stack usage, separately for each stack as
   --   follows:
   --
   --   o  Interval bounds on local stack-height reached in the
   --      subprogram, excluding stack used in lower-level callees.
   --      The lower bound can be negative.
   --
   --   o  For each call in the subprogram, interval bounds on the
   --      local stack-height in this subprogram (caller) when
   --      control transfers to the callee. This is called the
   --      "take-off stack-height" of the call. The lower bound
   --      can be negative.
   --
   --   o  An upper bound on the total stack-usage of this sub-
   --      program, including stack usage of lower-level callees.
   --      If the maximum usage occurs during a call, the specific
   --      call is also indicated.
   --
   --      The bound is associated with a state indication showing
   --      if a bound is known and whether it is asserted or computed.
   --
   --   o  See also the final stack height attribute above, which shows
   --      the local stack height on return from the subprogram.
   --      Clearly the bounds on local stack usage must include the
   --      bounds on the final height, however the final height for
   --      an Unstable stack can be negative (more pops than pushes
   --      while the local stack usage is always non-negative.
   --
   -- > For each call from this subprogram to another subprogram:
   --
   --   o  A reference to the execution bounds used for the callee.
   --      The callee execution bounds can be reused from another
   --      context (for example, if the callee is universally bounded)
   --      or they can be specific to this context.
   --
   --   o  Bounds on the call input cells and protocol basis cells,
   --      on entry to the call step, expressed in the caller frame.
   --
   -- The execution bounds can be complete (fully bounded) or
   -- incomplete (not fully bounded) separately for time bounds and
   -- space bounds on each stack.
   --
   -- Incomplete (partially bounded) execution bounds can report
   -- which loops and calls could not be bounded yet. However, these
   -- loops and calls may still be successfully bounded for longer
   -- call-paths where more context and parameter information may
   -- be known, except when the unboundedness stems from calls to
   -- "vague" subprograms, that is, stubbed (omitted) subprograms
   -- for which the time/space bound is not asserted.
   --
   -- If the final stack height for some Unstable stack is not fully
   -- bounded the analyses of callers may have problems because the
   -- effect of the call on the local stack height is not exactly known
   -- and thus the references to locals or parameters in the caller may
   -- not be resolved.
   --
   -- Execution bounds contain many attributes that are updated
   -- and set piecemeal as the analysis processed. Moreover,
   -- a given execution-bounds object is often referred to from
   -- multiple places. Therefore, execution bounds are usually
   -- accessed via references, so that updates apply at all places
   -- that share these bounds. The type Bounds_Ref is implemented
   -- as a reference (access) to an execution-bounds object. However,
   -- since the object-type is declared in the body of this package,
   -- we cannot make Bounds_Ref visible as an access type.
   --
   -- When an execution-bounds object is stored in a Bounds_Set_T
   -- and thus becomes accessible for reuse in other contexts, some
   -- attributes of the bounds are frozen and must not be changed any
   -- more. These attributes are:
   --
   -- > The computation model.
   -- > The value-origin map.
   -- > The assertion map.
   -- > The set of input cells.
   -- > The set of basis cells.
   -- > The set of output cells.
   -- > The bounds on inputs for calls to other subprograms.
   -- > The (nested) execution bounds on calls to other subprograms.
   -- > Consequently, the stub level is also frozen.
   --
   -- Note that although these attributes are frozen in an execution-bounds
   -- object, one can still create new execution-bounds objects for the
   -- same subprogram (in more specific contexts, for example) where these
   -- attributes can be changed.


   No_Bounds : constant Bounds_Ref;
   --
   -- The "null", or completely undefined, bounds.


   type Bounds_List_T is array (Positive range <>) of Bounds_Ref;
   --
   -- A list (set, bag) of (references to) execution bounds.
   -- Meaning depends on context.


   subtype Bounding_Level_T is Natural;
   --
   -- Execution bounds can apply at several levels, of three kinds
   -- as explained below. The level defines the length of the
   -- call-path that is needed to give actual parameter values
   -- for the bounds.


   Universal : constant Bounding_Level_T := 0;
   --
   -- If a subprogram can be bounded without knowledge of the
   -- calling context (parameter values, context-specific global
   -- values, etc.), the execution bounds are called "universal".
   --
   -- Universal bounds are shared by all the executions (calls) of
   -- this subprogram. Timing analysis is simplest if most subprograms
   -- have universal bounds.
   --
   -- If the user asserts the worst-case execution time of the
   -- subprogram, this is also considered universal bounding.
   --
   -- The goal of the analysis is to compute universal bounds for
   -- each "root" subprogram named by the user (on the command line,
   -- or as part of the HRT structure). If universal bounds are not
   -- found, context-dependent (call-path-specific) bounds are sought.


   subtype Path_Specific_T is
      Bounding_Level_T range 1 .. Bounding_Level_T'Last - 1;
   --
   -- If a subprogram does not have universal bounds, but can
   -- be bounded in the context of a specific call-path, the
   -- execution bounds are said to be "path-specific".
   --
   -- If the user asserts the worst-case execution time of a specific
   -- call, this results in path-specific bounds of level 1.


   Indefinite : constant Bounding_Level_T := Bounding_Level_T'Last;
   --
   -- If a subprogram or call could not (so far) be bounded at any
   -- level, its bounding level is said to indefinite (unbounded).


   -- A call-path is used to identify the context in which the final
   -- subprogram (the callee of the last call) is being analysed. If
   -- the call-path is empty, the subprogram is being analysed without
   -- benefit of calling context, and then the subprogram under analysis
   -- cannot be found from the call-path but must be known implicitly.
   --
   -- When a subprogram's execution bounds are derived in a path-specific
   -- way, there will be one set of execution bounds for each specific
   -- call-path.
   --
   -- When execution bounds for a subprogram S have been computed
   -- or asserted for a call-path P, these same bounds will be reused
   -- for every call of S with a call-path Q that contains P as a suffix.
   -- This means that once the call-path is long enough to allow
   -- complete bounds to be derived, we assume that even longer
   -- call-paths do not add significant new constraints.
   --
   -- For example, if the bounds are known for the call-path P = E=>F=>S,
   -- and we are analysing the subprogram A, which involves the call-path
   -- Q = A=>E=>F=>S, the bounds for P are reused for Q (by reference).
   --
   -- The only exception to this rule is the case where S has universal
   -- bounds (null call-path) but the user asserts the WCET of S
   -- for a specific call (call-path of length 1). For this reason,
   -- the rule is actually implemented as follows: if there are
   -- bounds for several paths that are suffixes of the current path,
   -- the bounds for the longest suffix are reused.
   --
   -- Some natural relationships between the bounding levels or
   -- bounds that reuse other bounds can be noted:
   --
   -- > Universal execution bounds for a subprogram refer to
   --   execution bounds for each call in this subprogram. These
   --   nested bounds can be universal or call-specific (level 1).
   --   They cannot be path-specific at higher levels since there
   --   is no higher-level caller in the context.
   --
   -- > Path-specific bounds for a subprogram also refer to execution
   --   bounds for each deeper call in this subprogram. These nested
   --   bounds can be universal or path-specific at any level from 1
   --   up to one lever higher than the containing bounds.
   --
   -- In summary, execution bounds at level L for a subprogram
   -- can refer to bounds at levels 0 .. L + 1.
   --
   --
   -- The following types represent the components of the
   -- execution bounds:


   subtype Node_Bounds_T is Flow.Execution.Bounds_T;
   --
   -- Bounds on the execution count of nodes in the flow-graph.
   -- Usually, only a few nodes will have such a bound.


   subtype Loop_Bounds_T is Flow.Execution.Loop_Bounds_T;
   --
   -- Bounds on the number of starts or repetitions of the loops in
   -- a subprogram. A Max value Flow.Execution.Infinite indicates a
   -- loop for which an upper bound has not been found. A Min value
   -- of zero may indicate that no lower bound has been found.
   --
   -- Note that a repetition bound can be interpreted in two different
   -- ways in, terms of constraints on the execution of flow-graph
   -- nodes and edges: it can mean either a bound on the number of
   -- executions of edges from the loop head to the body of the loop,
   -- or a bound on the number of executions of the repeat edges.
   -- There are in fact two different execution-bound attributes, one
   -- for each interpretation. (The former interpretation applies
   -- to user-asserted loop-bounds, the latter to automatically
   -- derived loop-bounds.)


   subtype Node_Times_T      is Flow.Execution.Times.Node_Times_T;
   subtype Edge_Times_T      is Flow.Execution.Times.Edge_Times_T;
   subtype Step_Edge_Times_T is Flow.Execution.Times.Step_Edge_Times_T;
   --
   -- The worst-case execution time of each node and edge in the
   -- subprogram's flow-graph. This must, of course, be known before
   -- a worst-case path can be computed.
   --
   -- The node-times may or may not include time spent in callee
   -- subprograms; this depends on how the times were queried.
   --
   -- Note that even when callee time is excluded, the time per node or
   -- edge can be call-specific for some target processors, such as the
   -- SPARC, where the times for call/return depend on the number of
   -- active register windows, and thus on the position in the call graph.


   subtype Node_Times_Ref is Flow.Execution.Times.Node_Times_Ref;
   subtype Step_Edge_Times_Ref is Flow.Execution.Times.Step_Edge_Times_Ref;


   --
   ---   Path bounds
   --


   subtype Flow_Counts_T is Flow.Execution.Counts_T;
   --
   -- Represents an execution of a flow-graph by giving the number
   -- of times each node and edge is executed.


   subtype Flow_Counts_Ref is Flow.Execution.Counts_Ref;


   --
   ---   Time bounds
   --


   type Time_State_T is (
      Undefined,
      Vague,
      Depends,
      Computable,
      Computed,
      Asserted,
      Infeasible,
      Unbounded,
      Failed);
   --
   -- The state of analysis of the worst-case execution-time for
   -- given execution bounds of a given subprogram.
   --
   -- Undefined
   --    Not yet known.
   --    This is the initial state when a new execution-bounds object
   --    is created by the operation Initialize_Bounds.
   -- Vague
   --    The subprogram is a stub with no asserted execution time bounds,
   --    or some aspects of the subprogram's execution are not bounded
   --    well enough to let us compute execution-time bounds, even if
   --    further parameter context were known. No amount of further
   --    analysis can give bounds on execution time.
   -- Depends
   --    Some aspects of the subprogram's execution (or the context-
   --    dependent execution of its callees) are not yet bounded well
   --    enough to let us compute execution-time bounds, nor are time
   --    bounds asserted, but there is hope that more parameter context
   --    may give bounds on execution time in a context-dependent re-
   --    analysis of the subprogram, perhaps including context-dependent
   --    re-analyses of callees where time Depends on parameters.
   --    There can be no feasible calls to Vague callees, because more
   --    analysis does not help for such calls. The subprogram may have
   --    an irreducible flow-graph if there are "enough" assertions to
   --    bound the execution paths.
   -- Computable
   --    The subprogram's execution is sufficiently bounded to let us
   --    compute a bound on the execution time. This means that all
   --    execution paths in the subprogram and its callees are bounded
   --    (or asserted "enough" for time to be bounded) and any other
   --    sources of variable execution time are also bounded.
   -- Computed
   --    Computed from the worst-case execution path defined by the
   --    related execution bounds on the subprogram in question.
   -- Asserted
   --    The user asserted the time for a subprogram, call or call-path.
   --    The related execution bounds may not contain any information
   --    about loop-bounds or other information that would be needed to
   --    compute a time.
   -- Infeasible
   --    The time was Computable, but the computation of the worst-case
   --    path found that the constraints were conflicting and that no
   --    execution of the subprogram is feasible, under these constraints.
   -- Unbounded
   --    The time was Computable, but the computation of the worst-case
   --    path found that the constraints were not sufficient to bound
   --    the execution time.
   -- Failed
   --    The time was Computable, but the computation of the worst-case
   --    path failed in some other way (neither Infeasible nor Unbounded).
   --
   -- The literal order is important, in particular the ordering
   -- Vague < Depends < Computable.


   subtype Time_Boundable_T is Time_State_T range Computable .. Asserted;
   --
   -- The time-states in which a value for the worst-case execution
   -- time bound is available, or will be available after some
   -- computation that is very likely to succeed.


   subtype Time_Bounded_T is Time_State_T range Computed .. Asserted;
   --
   -- The time-states in which a value for the worst-case execution
   -- time bound is available.


   --
   ---   Stack height bounds
   --


   type Stack_Limit_T is new Storage.Bounds.Interval_T;
   --
   -- Bounds on a local stack height, in processor-specific units,
   -- eg. octets or words.


   function To_Stack_Limit (Item : Storage.Bounds.Interval_T)
   return Stack_Limit_T;
   --
   -- Conversion from (Min, Max) bounds to stack limit.
   -- With the current definition of Stack_Limit_T this is
   -- an identity function (type transformation).


   function Unbounded return Stack_Limit_T;
   --
   -- An unbounded stack-height (-inf .. +inf).


   -- function "+" (Left, Right : Stack_Limit_T)
   -- return Stack_Limit_T;
   --
   -- The interval sum of Left and Right.
   -- Inherited from Storage.Bounds.


   function Max (Left, Right : Stack_Limit_T)
   return Stack_Limit_T;
   --
   -- The interval union of Left and Right.


   function Max (Left : Stack_Limit_T; Right : Arithmetic.Value_T)
   return Stack_Limit_T;
   --
   -- Same as Max (Left, Singleton (Right)).


   type Stack_Limits_T is array (Stack_Index_T range <>) of Stack_Limit_T;
   --
   -- A limit for each stack in the processor.


   --
   ---   Final stack height bounds
   --


   subtype Final_Stack_Height_T is Stack_Limit_T;
   --
   -- Bounds on the net change in the height of some stack, from
   -- entry to a subprogram to return (exit) from the subprogram.
   -- If the subprogram pushes more than it pops, the net change
   -- is positive and the stack-height increases; if the subprogram
   -- pops more than it pushes the net change is negative and the
   -- stack-height decreases. The net change in stack height is the
   -- same as the final stack height on return from the subprogram
   -- after the execution of the return instruction itself.
   --
   -- The main role of the final stack height of a subprogram is
   -- in the analysis of stacked data references in the caller.
   -- The final stack height of a subprogram is important for the
   -- stack-usage analysis of the subprogram itself only when the
   -- final stack height represents the largest local stack height
   -- ever reached in the subprogram.


   --
   ---   Stack usage bounds
   --


   type Space_State_T is (
      Undefined,
      Vague,
      Depends,
      Computed,
      Asserted,
      Infeasible,
      Unbounded);
   --
   -- The state of analysis of the stack usage for one stack in some
   -- given execution bounds for a given subprogram.
   --
   -- Undefined
   --    Not yet known.
   --    This is the initial state when a new execution-bounds object
   --    is created by the operation Initialize_Bounds.
   -- Vague
   --    The subprogram is a stub with no asserted stack-usage bounds,
   --    or some aspects of the subprogram's execution are not bounded
   --    well enough to let us compute stack-usage bounds, even if
   --    further parameter context were known. No amount of further
   --    analysis can give bounds on stack usage.
   -- Depends
   --    Some aspects of the subprogram's execution (or the context-
   --    dependent execution of its callees) are not yet bounded well
   --    enough to let us compute stack-usage bounds, nor are such
   --    bounds asserted, but there is hope that more parameter context
   --    may give bounds on stack usage in a context-dependent re-
   --    analysis of the subprogram, perhaps including context-dependent
   --    re-analyses of callees where stack usage Depends on parameters.
   --    There can be no feasible calls to Vague callees, because more
   --    analysis does not help for such calls.
   -- Computed
   --    The subprogram's execution is sufficiently bounded to let us
   --    compute bounds on the stack usage. This implies an upper bound
   --    on the final stack height, but not a lower bound.
   -- Asserted
   --    The user asserted the stack usage for a subprogram, call or
   --    call-path. The related execution bounds may not contain any
   --    information about take-off heights or stack usage of callees
   --    or other information that would be needed to compute stack
   --    usage bounds.
   -- Infeasible
   --    The computation of the stack usage bounds (including the
   --    computation of the take-off heights) found that the constraints
   --    were conflicting and that no execution of the subprogram is
   --    feasible, under these constraints.
   -- Unbounded
   --    Computation of stack usage bounds did not produce an upper
   --    bound on the usage.
   --
   -- The literal order is important, in particular the ordering
   -- Vague < Depends < Computed.


   subtype Space_Bounded_T is Space_State_T range Computed .. Asserted;
   --
   -- The space-states in which a Safe upper bound on the stack usage
   -- is available.


   type Stack_Usage_T is record
      State  : Space_State_T := Undefined;
      Height : Arithmetic.Value_T;
      Call   : Call_T := No_Call;
   end record;
   --
   -- Bounds on the stack usage, for one stack, in the call tree of
   -- some subprogram.
   --
   -- State
   --    The state of the analysis of stack usage bounds for this stack.
   --    The other components are relevant (defined) only if State is
   --    in Space_Bounded_T.
   -- Height
   --    An upper bound on stack usage, in processor-specific units, for
   --    any execution of this subprogram, including the execution of
   --    the callees of this subprogram. It is the sum of (the upper
   --    bounds on) the local stack heights in the call-path that uses
   --    the most stack.
   -- Call
   --    If the maximum stack usage occurs when the top subprogram
   --    calls some other subprogram, this is the call that leads
   --    to the maximum stack usage (the next link in the worst-case
   --    call-path).
   --    If the maximum stack usage occurs in the top subprogram
   --    itself, Call is No_Call. Note that this can happen even when
   --    the top subprogram calls other subprograms.


   function Value_Image (Item : Stack_Usage_T) return String;
   --
   -- Describes the State and Height, but not the Call.


   function Brief_Image (Item : Stack_Usage_T) return String;
   --
   -- Describes the State and Height, but not the Call.
   -- Undefined, vague, etc. states are indicated very briefly.


   function Total_Usage (
      Call     : Call_T;
      Take_Off : Stack_Limit_T;
      Callee   : Stack_Usage_T)
   return Stack_Usage_T;
   --
   -- The total stack usage of a call, given the take-off height
   -- and the stack usage of the callee, for one stack (not named
   -- here).


   function Max (Left, Right : Stack_Usage_T)
   return Stack_Usage_T;
   --
   -- The overall (bound on) stack usage, taken as the maximum of
   -- two stack usage bounds reflecting two different call paths
   -- rooted at the same caller subprogram, or two different places
   -- in the same (caller) subprogram with different local stack
   -- height, or a mixture of these.


   --
   ---   Stubs and stub-level
   --


   function Stub_Level (Bounds : Bounds_Ref) return Calling.Stub_Level_T;
   --
   -- The stub level of the Bounds, computed by traversing the
   -- feasible calls from these Bounds and collecting the stub-levels
   -- of their execution bounds.


   function Unknown_Effect (Bounds : Bounds_Ref) return Boolean;
   --
   -- Whether the arithmetic effect of executing the subprogram with
   -- the given execution Bounds is wholly or partially unknown.
   --
   -- This is equivalent to Stub_Level (Bounds) < Calls_No_Stub.
   -- Currently only stub subprograms have an unknown effect.


   --
   ---   Call bounds
   --


   type Call_Bounds_T is record
      Call   : Call_T;
      Bounds : Bounds_Ref;
   end record;
   --
   -- A call with some execution bounds that apply to it.
   -- The bounds need _not_ be complete (fully bounded).
   -- The bounds need not be the final bounds computed for this call;
   -- they can be a first approximation (eg. incomplete but universal
   -- bounds) used in iteration towards the final bounds.


   function Stub_Level (Call : Call_Bounds_T) return Calling.Stub_Level_T;
   --
   -- The stub level of the call, which is one more than the stub
   -- level of the callee bounds, or Calls_No_Stub if the callee's
   -- level is Calls_No_Stub.


   type Call_Bounds_List_T is array (Positive range <>) of Call_Bounds_T;
   --
   -- A list of calls with bounds.
   -- See description of Call_Bounds_T for the meaning of the bounds.


   function Calls_Of (List : Call_Bounds_List_T) return Call_List_T;
   --
   -- The Call components of the listed call-bounds.


   function Bounds_Of (List : Call_Bounds_List_T)
   return Bounds_List_T;
   --
   -- The Bounds components of the listed call-bounds.


   function Stub_Level (List : Call_Bounds_List_T)
   return Calling.Stub_Level_T;
   --
   -- The minimum stub level of the listed call-bounds.


   --
   ---   Links between bounds, implied by calls.
   --


   type Link_T is private;
   --
   -- A link between bounds for the caller and bounds for the callee.


   function Link (
      From : Bounds_Ref;
      Via  : Call_T)
   return Link_T;
   --
   -- The link that connects From the given execution bounds, Via
   -- a call in these bounds, to the bounds for this call.


   function Caller_Bounds (Item : Link_T) return Bounds_Ref;
   --
   -- The execution bounds of the caller, involved in the link.


   function Callee_Bounds (Item : Link_T) return Bounds_Ref;
   --
   -- The execution bounds of the callee, involved in the link.


   function Call (Item : Link_T) return Call_T;
   --
   -- The call that creates the link.
   --
   -- Some identities that relate these functions are:
   --
   --    Caller (Call (Link)) = Subprogram (Caller_Bounds (Link)).
   --
   --    Callee (Call (Link)) = Subprogram (Callee_Bounds (Link)).
   --
   --    Call_Path (Callee_Bounds (Link))
   --       is a suffix of
   --    Call_Path (Caller_Bounds (Link)) & Call (Link).


   type Link_List_T is array (Positive range <>) of Link_T;
   --
   -- A list (set, bag) of links, with a context-dependent meaning.


   function No_Links return Link_List_T;
   --
   -- An empty (null) list of links.



   --
   ---   Bounds indexing and subsetting
   --


   type Bounds_Count_T is new Natural;
   --
   -- The number of execution bounds generated for one program.


   subtype Bounds_Index_T is
      Bounds_Count_T range 1 .. Bounds_Count_T'Last;
   --
   -- Identifies a specific instance of Execution Bounds within all
   -- bounds within one Bounds_Set_T.
   --
   -- Such indices are used in client software to keep track of
   -- which execution bounds have been processed, for example
   -- displayed.


   type Bounds_Subset_T (Max_Size : Bounds_Count_T) is private;
   --
   -- A subset of the execution bounds in one Bounds_Set_T.
   -- Members are marked by True values.
   -- The default initial value is the empty subset.


   function Locus (Item : Bounds_Ref)
   return Output.Locus_T;
   --
   -- Describes the part of the target program to which the bounds
   -- apply, in terms of the bounded subprogram and the call-path
   -- if the bounds are path-dependent.


   function Locus (Luup : Loops.Loop_T; Within : Bounds_Ref)
   return Output.Locus_T;
   --
   -- Describes the location of the Luup, Within the subprogram and
   -- program to which the bounds apply.


   function Locus (Call : Call_T; Within : Bounds_Ref)
   return Output.Locus_T;
   --
   -- Describes the location of the Call, Within the subprogram and
   -- program to which the bounds apply. The call-path in the locus
   -- is that of the bounds, extended with the Call.


   --
   ---   Constructing and querying execution bounds sets
   --


   procedure Initialize_Bounds_Set (
      Program   : in     Program_T;
      For_Time  : in     Boolean;
      For_Space : in     Boolean;
      Set       : in out Bounds_Set_T);
   --
   -- Creates and initializes a set of execution bounds to the
   -- empty set. This operation must be used before any other
   -- operation on the set except for comparison to No_Bounds_Set.
   --
   -- Program
   --    The program under analysis. The set will contain execution
   --    bounds for subprograms within this program.
   -- For_Time, For_Space
   --    Whether bounds are sought for execution time, (stack) memory
   --    space or both. For_Space is silently overridden to False if
   --    the Program has no stacks.
   -- Set
   --    The new set.
   --
   -- When new execution bounds are added to the Set, they must have
   -- at least the same set of For_Time and/or For_Space goals.


   function Program (Item : Bounds_Set_T) return Program_T;
   --
   -- The program to which the bounds in the given set apply.


   function Number_Of_Bounds (Within : Bounds_Set_T)
   return Bounds_Count_T;
   --
   -- Returns the number of execution bounds stored for subprograms
   -- in the given set of bounds. The indices of these execution bounds
   -- run from 1 to Number_Of_Bounds.
   -- Note that new execution bounds may still be stored, and will
   -- then be assigned larger indices. So this function returns a
   -- snapshot.


   function Number_Of_Bounds (
      Sub    : Subprogram_T;
      Within : Bounds_Set_T)
   return Natural;
   --
   -- Returns the number of execution bounds created so far for the
   -- given subprogram in the given set of bounds. These bounds (for
   -- this subprogram) can be accessed with subprogram-specific indices
   -- in the range 1 .. Number_Of_Bounds.


   function Bound_At (
      Index  : Bounds_Index_T;
      Within : Bounds_Set_T)
   return Bounds_Ref;
   --
   -- The execution bounds with the given Index, unique Within the
   -- given set of execution bounds.


   procedure Store_Bounds (
      Bounds : in Bounds_Ref;
      Within : in Bounds_Set_T);
   --
   -- Store a new set of execution Bounds for a subprogram.
   --
   -- This operation must be done for bounds created by Initialize_Bounds
   -- before the bounds can be reused in other contexts (by other calls,
   -- for example). Certain attributes of the stored bounds are then
   -- considered frozen and shall not be modified.
   --
   -- The Bounds to be stored Within the set must have at least the
   -- same goals (for Time, for Space) as the set itself, otherwise a
   -- fault is signalled.


   function Bound_At (
      Index  : Positive;
      Sub    : Subprogram_T;
      Within : Bounds_Set_T)
   return Bounds_Ref;
   --
   -- Returns the execution bounds of Sub at Index, where Index is
   -- in the range 1 .. Number_Of_Bounds (Sub, Within).


   function Bounds_For (
      Root   : Call_T;
      Within : Bounds_Set_T)
   return Bounds_Ref;
   --
   -- The bounds computed or asserted for the root subprogram
   -- that is the callee of the Root call. If multiple bounds
   -- exist (as can happen if a root subprogram is also called
   -- by another subprogram in the analysed set) the universal
   -- (not call-dependent) bounds are returned.
   --
   -- The Root parameter may be an actual "root call" (null
   -- caller) or an ordinary call.


   function Bounds_For (
      Subprogram : Subprogram_T;
      Within     : Bounds_Set_T;
      Along      : Call_Path_T)
   return Bounds_Ref;
   --
   -- Returns the bounds computed or asserted for the given
   -- Subprogram Along the given call-path Within the given set
   -- of execution bounds.
   --
   -- Such bounds exist if, and only if, they have been stored in
   -- Within using Store_Bounds for this subprogram, and for a
   -- call-path that is a suffix of Along (or the whole of Along).
   --
   -- If multiple such execution bounds exist, the best one is chosen
   -- and returned. Firstly, complete (fully bounded) bounds are preferred
   -- to incomplete ones. Secondly, longer call-paths (more specific)
   -- are preferred to short call-paths (more general).
   --
   -- If no applicable bounds exist, No_Bounds is returned.


   function Bounds_For_Calls (
      From   : Subprogram_T;
      Within : Bounds_Set_T)
   return Call_Bounds_List_T;
   --
   -- Execution bounds for all calls From a given subprogram,
   -- Within the given set of execution bounds and with the Caller
   -- as the only context, if any.
   --
   -- The caller (From) may be at a very early stage of analysis;
   -- it may not yet have any execution-bounds Within the set.


   function Root_Bounds (Within : Bounds_Set_T)
   return Call_Bounds_List_T;
   --
   -- The universal execution bounds for all the root calls in the
   -- target program Within the given bounds set.
   --
   -- If a root call has no universal execution bounds, No_Bounds is
   -- returned in that list position.


   function Number_Of_Links (Within : Bounds_Set_T)
   return Natural;
   --
   -- The total number of links between bounds in the set.


   function All_Links (
      From   : Call_List_T;
      Within : Bounds_Set_T)
   return Link_List_T;
   --
   -- All the links, Within the given set of bounds, originating
   -- From the universal execution bounds attached to the callees
   -- of the of the given calls.



   --
   ---   Constructing execution bounds
   --
   --
   -- Execution bounds are usually constructed incrementally,
   -- adding a bound here, a bound there.


   procedure Initialize_Bounds (
      For_Time   : in     Boolean;
      For_Space  : in     Boolean;
      Subprogram : in     Subprogram_T;
      Along      : in     Call_Path_T;
      Caller     : in     Bounds_Ref;
      Within     : in     Bounds_Set_T;
      Bounds     :    out Bounds_Ref);
   --
   -- Creates and initialises execution bounds for a subprogram
   -- along a given call-path.
   --
   -- For_Time, For_Space
   --    Whether we seek bounds on execution time, (stack) memory space
   --    or both. For_Space is silently overridden to False if the
   --    program under analysis has no stacks.
   -- Subprogram
   --    The subprogram to be bounded.
   -- Along
   --    The call path (context for the bounds). It is either null
   --    or ends with a call of the Subprogram.
   -- Caller
   --    The execution bounds of the caller, when the new execution
   --    bounds are meant to be specific to this call from these
   --    bounds on the caller. Null otherwise.
   -- Within
   --    The set of bounds to which the new bounds will (later) be
   --    added. Also provides initial bounds for the calls in the
   --    Subprogram.
   -- Bounds
   --    The new execution bounds here created.
   --
   -- If the Along path is null, this is the first attempt to bound
   -- the execution of this Subprogram, and there are no earlier bounds
   -- on Subprogram that might be relevant.
   --
   -- If the Along path is not null, there should be earlier bounds
   -- on Subprogram Within the bounds-set that are relevant to this
   -- context, that is, apply to a call-path that is a suffix of Along.
   -- These earlier bounds are not fully bounded (otherwise we would
   -- not try to find new bounds).
   --
   -- The new execution bounds are formed as follows:
   --
   -- > The subprogram, call-path and program are as given.
   --
   -- > For each nested call:
   --
   --   o  The nested execution bounds refer to the bounds of the
   --      callee, as currently known Within the bounds-set for this
   --      call-path (i.e. Along & call). These bounds _must not_
   --      be modified any more as they may be shared with other
   --      contexts where the modifications do not apply. Instead,
   --      new bounds for the call can be constructed and a reference
   --      to the new bounds inserted here.
   --
   --   o  Bounds on the input cells and protocol basis cells are
   --      derived from the Asserted bounds, or inherited from the
   --      earlier relevant execution bounds for Subprogram when such
   --      earlier bounds exist (that is, when Along is not null).
   --
   -- > The computation model is formed as follows:
   --
   --   o  If there are no earlier relevant bounds on Subprogram
   --      (that is, if Along is null) the "primitive model" built
   --      into the flow-graph is taken and is extended with the
   --      effects of the calls from Subprogram to lower-level
   --      callees, computed under the Asserted bounds on inputs
   --      and under the nested execution bounds on the callees.
   --      The model is marked "clean".
   --
   --   o  If there are earlier relevant bounds on the Subprogram
   --      (that is, if Along is not null) the computation model is
   --      initialized from those bounds, including the effects of
   --      the calls as computed for the earlier bounds. It is
   --      assumed (and checked elsewhere) that the model is "clean".
   --
   -- > No assertion map is associated with the bounds.
   --
   -- > There are no bounds on the initial values of cells on
   --   entry to the subprogram.
   --
   -- > The calls from the subprogram all have null bounds on
   --   initial cell values.
   --
   -- > The worst-case time of nodes and edges is undefined.
   --
   -- > All loops are unbounded.
   --
   -- > No bounds on other nodes or edges are set.
   --
   -- > The worst-case path is undefined.
   --
   -- > The total worst-case time is Undefined.
   --
   -- > The final stack height for each stack is set to the Net_Change
   --   defined for that stack, if known, else to the universal interval.
   --   For a Stable stack the net change is known to be zero.
   --
   -- > All stack-usage bounds are undefined.
   --
   -- The new Bounds are _not_ stored Within the bounds-set yet,
   -- and so are not available for reuse in other contexts
   -- until they are stored with Store_Bounds (which see). No
   -- attributes of the new Bounds are frozen.
   --
   -- A new index is allocated to the new Bounds and is an attribute
   -- of the new Bounds. Note that the indexing sequence increases
   -- _before_ Store_Bounds; this is why the Within parameter is
   -- "in out" instead of "in".
   --
   -- The computation model is treated in a special way because of
   -- its "copy-on-change" semantics: the model for the new Bounds
   -- is directly associated with the new Bounds. However, the query
   -- function for the model returns a Model_Handle_T through which
   -- the model can be updated and refined in-place.


   procedure Initialize_Universal_Bounds (
      Subprogram : in     Subprogram_T;
      Within     : in     Bounds_Set_T;
      Bounds     :    out Bounds_Ref);
   --
   -- Creates and initialises execution bounds for a subprogram in
   -- the universal (null) context, making the bounds, such as they
   -- are or will be, applicable to all calls of this Subprogram.
   -- Equivalent to Initialize_Bounds with Along => Null_Call_Path
   -- and Caller => null. The For_Time and For_Space options are
   -- taken from Within the bounds-set.


   procedure Adopt_To_Context (
      Call   : in     Call_T;
      Caller : in     Bounds_Ref;
      Within : in     Bounds_Set_T;
      Giving :    out Bounds_Ref);
   --
   -- Ensures that the execution bounds on the Call, within the
   -- given Caller bounds, are specialized to the context of this
   -- call from these Caller bounds, and are not a reference to
   -- bounds inherited from some other (shallower) context.
   --
   -- If the bounds on the Call within the Caller are already
   -- dedicated to this Caller (as shown by the Caller_Bounds link)
   -- no action is taken. The bounds on the Call are returned
   -- in Giving.
   --
   -- Otherwise, the operation creates a new execution-bounds object
   -- that is a copy of the inherited bounds on the Call, but
   -- dedicated to these Caller bounds. The copy is not stored
   -- Within the bounds-set, so it is not yet frozen. The copy can
   -- be modified independently, without effect on the inherited bounds
   -- for this Call. The copy is of course returned in the Giving
   -- parameter, and is also defined as the new bounds on this Call,
   -- within the Caller bounds.
   --
   -- The copy has the following new properties:
   --
   -- > The index is new, of course.
   -- > The context is the context of the Caller, plus the Call.
   --
   -- The following properties are the same in the copy as in the
   -- original, if they are bounded or defined in the original:
   --
   -- > The state of the execution-time bounds (eg. Computed, Asserted)
   --   (but the flag showing if the time-calculation phase has been
   --   applied is not copied).
   -- > The path bounds on nodes and loops, if Time_Boundable_T.
   -- > The execution-time bounds, if Time_Bounded_T.
   -- > The stack bounds.
   -- > The take-off stack-heights for deeper calls.
   -- > The processor-specific info.
   -- > The sets of input and output cells.
   -- > The assertion map.
   --
   -- All other properties of the copy are set as in Initialize_Bounds.
   -- In particular, the copy bounds are flagged to show that the
   -- worst-case path and time have not yet been calculated.
   --
   -- This operation is meant to support call-specific assertions
   -- on time and space usage. "Deeper" context-specific assertions
   -- are not yet supported.
   --
   -- Call
   --    The call for which execution bounds are handled.
   -- Caller
   --    Execution bounds for Caller (Call), to be the context for
   --    the execution bounds on the Call.
   -- Within
   --    The set of execution bounds that contains the Caller bounds.
   --    Provides the new index for the new execution-bounds object.
   -- Giving
   --    The execution bounds for the Call, within and specialized
   --    to the context of the given Caller bounds.


   procedure Set_Processor_Info (
      To     : in Processor.Execution.Info_T;
      Within : in Bounds_Ref);
   --
   -- Sets the processor-specific information Within the given
   -- execution bounds To a given value.


   function All_Cells (Within : Bounds_Ref) return Storage.Cell_List_T;
   --
   -- All cells that are statically referenced Within the computation
   -- modelled by the given execution bounds on a subprogram, including:
   --
   -- > Cells referenced in the arithmetic effects and edge preconditions
   --   in all feasible parts of the computation model.
   --
   -- > Basis cells for dynamic memory references, dynamic edges and
   --   dynamic protocols in the feasible parts of the model.
   --
   -- > All cells that may be assigned (veiled) as the effect of calls
   --   in the feasible parts of the model.
   --
   -- > All input cells for feasible unbounded calls Within the given
   --   execution bounds.


   function Computation_Changed (Within : Bounds_Ref)
   return Boolean;
   --
   -- Same as Flow.Computation.Changed (Computation (Within)).


   procedure Note_Updated_Computation (Within : in Bounds_Ref);
   --
   -- Notifies us that the computation model Within the given bounds
   -- has been updated (modified) and so we should update properties
   -- of the given bounds that depend on the computation model. This
   -- means that:
   --
   -- > The set of cells referenced in the model must be updated
   --   because:
   --   o  the arithmetic effects of steps may have changed;
   --   o  the preconditions of edges may have changed;
   --   o  some calling protocols may have changed, changing the
   --      set of input cells (in our frame) for these calls.
   --
   -- > If the model now references new cells, the effects of stub
   --   calls (calls with stub level /= Calls_No_Stub) must be updated
   --   to veil those new cells that may be assigned in the stubs.
   --
   -- > The value-origin map must be updated. In fact, it is discarded
   --   here, for later recomputation if needed.
   --
   -- > TBA update the computation (esp. call effects) for aliasing
   --   and part/whole cells.


   procedure Mark_Computation_Clean (Within : in Bounds_Ref);
   --
   -- Same as Flow.Computation.Mark_Clean (Computation (Within)).


   procedure Bound_Computation (
      Model  : in Flow.Computation.Model_Ref;
      Within : in Bounds_Ref);
   --
   -- Attaches a computation model to the bounds (replacing any
   -- earlier model). A computation model associates an arithmetic
   -- effect with each step in the control-flow graph, an arithmetic
   -- condition with each edge in the control-flow graph, and a
   -- calling protocol (including parameter map) with each call
   -- from the subprogram.
   --
   -- There is usually no need to use this operation because the
   -- model created in Initialize_Bounds can be updated in-place and
   -- does not have to be (re-) attached to the bounds.
   --
   -- Note: The caller is responsible for making sure that the new
   -- Model is consistent with the other attributes of Within.
   -- This procedure applies no check on this. However, it does call
   -- Note_Updated_Computation.
   --
   -- If the caller has no further need for the given Model, the
   -- caller should Discard it. The underlying model remains attached
   -- Within the bounds and can be retrieved from there.
   --
   -- The computation model is one of the "frozen" attributes so this
   -- operation is only possible before the bounds are frozen. Moreover,
   -- even if a client has retained a handle to the computation model
   -- the client shall not change the model via this handle after the
   -- bounds are frozen. There is no run-time check for this error,
   -- however.


   procedure Bound_Value_Origins (Within : in Bounds_Ref);
   --
   -- Computes or recomputes the value-origin map for the computation
   -- model Within the given bounds. If a value-origin map already exists,
   -- it is discarded before the new map is computed.


   procedure Bound_Assertions (
      Map    : in Assertions.Assertion_Map_T;
      Within : in Bounds_Ref);
   --
   -- Attaches an assertion map to the bounds. An assertion map
   -- connects some of the assertions with the corresponding program
   -- structures, for example an assertion on loop repetition count
   -- with the Loop_T in the Subprogram.
   --
   -- Also sets the Enough_For_Time attribute of the execution bounds
   -- according to the assertion set connected to the Map.
   --
   -- The assertion map is one of the "frozen" attributes so this
   -- operation is only possible before the bounds are frozen.


   procedure Initialize_Asserted_Bounds (
      Subprogram : in     Subprogram_T;
      Within     : in     Bounds_Set_T;
      Bounds     :    out Bounds_Ref);
   --
   -- Creates and defines execution bounds that represent user-asserted
   -- bounds on the time/space consumption of a subprogram universally,
   -- independent of call-path, as follows:
   --
   -- > The subprogram is the given one.
   --
   -- > The program is taken from the bounds-set Within which
   --   the new bounds are stored.
   --
   -- > The call-path is null.
   --
   -- > The computation model is null.
   --
   -- > No assertion map is associated with the bounds (the asserted
   --   time and space bounds apply to the whole subprogram; the
   --   internal structure of loops etc. is not relevant).
   --
   -- > The set of input cells is Storage.Forever_Empty.
   --
   -- > The set of output cells is Storage.Forever_Empty.
   --   TBM reasonable definition of output cells or aliases.
   --
   -- > Time_State is Vague.
   --
   -- > Space_State is Vague for all stacks.
   --
   -- All other attributes of the returned bounds are either
   -- "unbounded" or "undefined".
   --
   -- The new bounds are not yet stored Within the given set of bounds.
   -- The next steps are to enter the asserted bounds using the proper
   -- procedures Bound_Xxx and then call Store_Bounds.


   procedure Set_Input_Cells (
      To     : in Storage.Cell_Set_T;
      Within : in Bounds_Ref);
   --
   -- Defines the input cells for the execution bounds.
   -- The cells are viewed in the bounded subprogram's own frame.
   -- Thus, in a call of this subprogram, the cells are relative
   -- to the callee frame.


   procedure Set_Basis_Cells (
      To     : in Storage.Cell_Set_T;
      Within : in Bounds_Ref);
   --
   -- Defines the basis cells that were used for the arithmetic
   -- analysis that generated the execution bounds.
   -- The cells are viewed in the bounded subprogram's own frame.
   -- Thus, in a call of this subprogram, the cells are relative
   -- to the callee frame.


   procedure Set_Output_Cells (
      To     : in Storage.Cell_Set_T;
      Within : in Bounds_Ref);
   --
   -- Defines the output cells for the execution bounds.
   -- The cells are viewed in the bounded subprogram's own frame.
   -- Thus, in a call of this subprogram, the cells are relative
   -- to the callee frame.
   --
   -- The summarised arithmetic effect of the subprogram assigns an
   -- unknown (opaque) value to each output cell, except those cells
   -- that are asserted as invariant in the subprogram or for a
   -- specific call of the subprogram.
   --
   -- The procedure Remove_From_Output can remove cells from the
   -- output set, see below.


   procedure Remove_From_Output (
      Cells  : in Storage.Cell_List_T;
      Within : in Bounds_Ref);
   --
   -- Removes the Cells from the set of output cells Within the
   -- execution bounds. Before this can be done, the set of output
   -- cells must be be defined with Set_Output_Cells. Removing a cell
   -- from the output set means that the summary arithmetic effect of
   -- the subprogram leaves the cell unchanged (invariant).


   procedure Bound_Initial_Values (
      To     : in Storage.Bounds.Cell_Interval_List_T;
      Within : in Bounds_Ref);
   --
   -- Sets the bounds on the input cell values at entry to the
   -- subprogram Within the given execution bounds. The bounded
   -- cells are expressed in the subprogram's frame (not in the
   -- caller's frame).


   procedure Bound_Call_Inputs (
      To   : in Storage.Bounds.Var_Interval_List_T;
      From : in Bounds_Ref);
   --
   -- Sets bounds on the inputs for all the calls From the given
   -- execution bounds, To the given variable bounds, asserted or
   -- derived to hold at all points (at least all calls) within
   -- the caller's execution.


   procedure Bound_Edge_Times (
      Times  : in Step_Edge_Times_T;
      Within : in Bounds_Ref);
   --
   -- Sets the worst-case execution time of each step-edge.
   --
   -- The edge-times will be used later to copmute the worst-case
   -- execution-time of the nodes, and then to find the worst-case
   -- path and the WCET.
   --
   -- The times are added within the given bounds, replacing
   -- any earlier such times.


   procedure Bound_Node_Times (
      Times  : in Node_Times_T;
      Within : in Bounds_Ref);
   --
   -- Sets the worst-case execution time of each node, without
   -- including the time of calls to lower-level subprograms.
   --
   -- The node (and edge) execution times will be used later to
   -- find the worst-case path and then the WCET.
   --
   -- The times are added within the given bounds, replacing
   -- any earlier such times.


   procedure Compute_Node_Times (Bounds : in Bounds_Ref);
   --
   -- Computes the per-node execution times for the subprogram
   -- under the given Bounds, and stores the times in the Bounds.
   --
   -- The computation uses the per-edge times set by a preceding
   -- Bound_Edge_Times. If no per-edge times have been set, a
   -- fault is signalled.


   procedure Compute_Node_Times (
      Calls     : in Call_Bounds_List_T;
      Max_Index : in Bounds_Index_T);
   --
   -- Computes the per-node execution times for all the call-bounds
   -- in the listed calls, and their lower-level call-bounds, and
   -- stores the times in the respective execution bounds.
   --
   -- At each point in the recursion, the computation uses the
   -- per-edge times from Bound_Edge_Times. If no per-edge times
   -- have been set, a fault is signalled and new per-node times
   -- are not computed for the relevant execution bounds.
   --
   -- Max_Index must be given as an upper bound on the Index of
   -- the Bounds to be computed. For example, Max_Index can
   -- be the Number_Of_Bounds in the bounds-set.


   procedure Bound_Flow_Counts (
      Counts : in Flow.Execution.Counts_Ref;
      Within : in Bounds_Ref);
   --
   -- Sets the execution count of each node and edge. The execution
   -- counts for infeasible nodes or edges should be zero in Counts.
   --
   -- The execution counts are added within the given bounds, replacing
   -- any earlier such counts.
   --
   -- The function Counts_Set reports whether the execution counts
   -- have been set in this way.


   procedure Bound_Node_Count (
      Node   : in Flow.Node_T;
      Count  : in Flow.Execution.Bound_T;
      Within : in Bounds_Ref);
   --
   -- Bounds the number of executions of the given node to
   -- be in the given Count range, but also including (conjoining)
   -- the earlier bounds on the execution count, if any.
   --
   -- The bound is added within the given bounds. If the result is
   -- an empty interval (no valid number of executions), the
   -- bounds are flagged as having infeasible flow bounds. If the
   -- result is the singleton {0} the Node is marked as infeasible
   -- Within these execution bounds, but the computation model is
   -- not (yet) pruned.


   procedure Bound_Node_Counts (
      By     : in Flow.Execution.Node_Count_Bounds_T;
      Within : in Bounds_Ref);
   --
   -- Bounds the number of executions of some nodes, per node, By
   -- the given bounds, Within the given execution bounds, but also
   -- including the earlier bounds, if any, on the execution counts.
   --
   -- The bounds are added within the given bounds. If the result is
   -- an empty interval (no valid number of executions) for some node,
   -- the bounds are flagged as having infeasible flow bounds. If the
   -- result is the singleton {0} for some node, that node is marked
   -- as infeasible Within these execution bounds, but the computation
   -- model is not (yet) pruned.


   procedure Bound_Call_Count (
      Call   : in Call_T;
      Count  : in Flow.Execution.Bound_T;
      Within : in Bounds_Ref);
   --
   -- Bounds the number of executions of the given call to be
   -- be in the given Count bound, Within the bounds of the caller.
   --
   -- The bound is added within the given bounds as a bound on the
   -- execution count of the flow-graph node that contains the call.
   -- Thus, there is no specific query function to return the
   -- execution-count bounds on a call. If the bound is void, the
   -- execution bounds are marked as having infeasible flow bounds.
   -- If the result is the singleton {0} executions, the call-node
   -- is marked as infeasible Within these execution bounds, but the
   -- computation model is not (yet) pruned.


   procedure Bound_Loop_Starts (
      Luup   : in Loops.Loop_T;
      Starts : in Flow.Execution.Bound_T;
      Within : in Bounds_Ref);
   --
   -- Bounds the number of times the loop starts, that is the number of
   -- times the loop-head is entered from outside the loop, within one
   -- execution of the subprogram that contains the loop.
   -- The bound is added within the given bounds. If the bound is void,
   -- the execution bounds are marked as having infeasible flow bounds.


   procedure Bound_Loop_Neck (
      Luup   : in Loops.Loop_T;
      Count  : in Flow.Execution.Bound_T;
      Within : in Bounds_Ref);
   --
   -- Bounds the number of times execution passes from the loop-head
   -- into the loop's body (or directly back to the head, using a
   -- repeat-edge head->head). The constraint is given per the number
   -- of times the loop-head is entered from outside the loop.
   -- The bound is added within the given bounds. If the bound is void,
   -- the execution bounds are marked as having infeasible flow bounds.


   procedure Bound_Loop_Repeats (
      Luup    : in Loops.Loop_T;
      Repeats : in Flow.Execution.Bound_T;
      Within  : in Bounds_Ref);
   --
   -- Bounds the number of times the loop is repeated by executing
   -- one of the repeat edges. The constraint is given per the number
   -- of times the loop-head is entered from outside the loop.
   -- The bound is added within the given bounds. If the bound is void,
   -- the execution bounds are marked as having infeasible flow bounds.


   procedure Bound_Call_Input (
      Call   : in Call_T;
      Bounds : in Storage.Bounds.Bounds_Ref;
      Within : in Bounds_Ref);
   --
   -- Sets the input-cell Bounds for the given Call, Within the bounds
   -- for the caller. The Bounds must be expressed in the caller frame.
   --
   -- The call-input bounds are one of the "frozen" attributes so this
   -- operation is only possible before the execution bounds are frozen.


   procedure Bound_Call (
      Call   : in Call_T;
      Bounds : in Bounds_Ref;
      Within : in Bounds_Ref);
   --
   -- Sets the execution-bounds for the given call within the bounds
   -- for the caller.
   --
   -- The callee's bounds (in the Bounds parameter) can be partly
   -- or fully complete; the right thing will be done.
   --
   -- The execution bounds for calls are one of the "frozen" attributes
   -- so this operation is only possible before the bounds on the
   -- caller (Within) are frozen.
   --
   -- Moreover, the Bounds for the call must either be derived Within
   -- the context of these bounds on the caller (that is, Within must
   -- equal the Caller_Bounds of the Bounds on the call), or the Bounds
   -- on the call must _be_ frozen at this time, that is, when they are
   -- linked Within the caller's bounds by this operation.


   procedure Bound_Final_Stack_Height (
      Stack  : in Stack_T;
      To     : in Final_Stack_Height_T;
      Within : in Bounds_Ref;
      Silent : in Boolean := False);
   --
   -- Bounds the final stack height (net change in stack height) for
   -- a given Stack, Within the execution bounds of a given subprogram.
   --
   -- It is a fault to bound the final height of a Stable stack to
   -- any value other than the singleton interval containing zero.
   --
   -- The optional trace output can be suppressed with Silent => True.


   procedure Bound_Stack_Height (
      Stack  : in Stack_T;
      To     : in Stack_Limit_T;
      Within : in Bounds_Ref;
      Silent : in Boolean := False);
   --
   -- Bounds the maximum local stack height for a given Stack, Within
   -- the execution bounds of a given subprogram. This is the same or
   -- less than Max_Stack_Usage, which bounds the stack in the call
   -- tree starting from a subprogram.
   --
   -- The optional trace output can be suppressed with Silent => True.


   procedure Bound_Take_Off_Height (
      Stack  : in Stack_T;
      Before : in Call_T;
      To     : in Stack_Limit_T;
      Within : in Bounds_Ref;
      Silent : in Boolean := False);
   --
   -- Bounds the local stack-height for a given Stack that obtains in
   -- the caller just Before control transfers to the callee, Within
   -- the bounds of the caller. Thus, the Within bounds apply to
   -- Caller (Before).
   --
   -- The optional trace output can be suppressed with Silent => True.


   procedure Bound_Stack_Usage (
      Stack  : in Stack_T;
      To     : in Stack_Usage_T;
      Within : in Bounds_Ref;
      Silent : in Boolean := False);
   --
   -- Bounds the maximum stack usage, for a given Stack, Within the
   -- execution bounds for the call tree starting from a subprogram.
   -- Also emits a "Stack" output line to show this result (unless
   -- suppressed with Silent, see below).
   --
   -- The result output and the optional trace output can be
   -- suppressed with Silent => True.


   procedure Bound_Time (
      Time   : in Processor.Time_T;
      State  : in Time_Bounded_T;
      Within : in Bounds_Ref);
   --
   -- Sets the worst-case execution Time for the given bounds.
   -- The Time is Computed or Asserted according to the State.
   -- Preserves all other attributes of the bounds.
   --
   -- Reaching this operation with a good, tight, Computed value of
   -- Time is the main objective and Holy Grail of the worst-case
   -- execution-time analysis.


   procedure Set_Time_State (
      To     : in Time_State_T;
      Within : in Bounds_Ref);
   --
   -- Sets the time-state To the given value, Within the given bounds.
   -- If the new state implies the existence of a time bound, you
   -- should use the operation Bound_Time, above.


   procedure Set_Time_Calculated (Within : in Bounds_Ref);
   --
   -- Marks the execution bounds to show that the final phase of
   -- the execution-time analysis, that of finding the worst-case
   -- path and calculating the worst-case execution time (if possible),
   -- has been applied to these bounds (and, it is implied, also to
   -- all execution bounds on callees linked to these bounds).


   --
   ---   Execution bound query operations
   --

   -- Queries usually deliver all the accumulated bounds
   -- of a specific types, in one batch.


   function Defined (Item : Bounds_Ref) return Boolean;
   --
   -- Whether the bounds are defined at all (not null).
   --
   -- The initial value of an execution-bounds variable is "undefined"
   -- in this sense. Same as Item /= No_Bounds.


   function Index (Item : Bounds_Ref)
   return Bounds_Index_T;
   --
   -- The unique, sequential index of the bounds within its set, in
   -- the range 1 .. Number_Of_Bounds (set), without regard to the
   -- subprogram to which the bounds apply.
   --
   -- Note that this Index is different from the subprogram-specific index
   -- which is not a visible attribute. Within one set of bounds, all the
   -- bounds for subprogram of P are indexed 1 .. Number_Of_Bounds (P, set)
   -- using the subprogram-specific bounds index.


   function Subprogram (Item : Bounds_Ref)
   return Subprogram_T;
   --
   -- The subprogram to which the bounds apply.


   function Flow_Graph (Item : Bounds_Ref)
   return Flow.Graph_T;
   --
   -- Short for Flow_Graph (Subprogram (Item)).


   function Call_Path (Item : Bounds_Ref)
   return Call_Path_T;
   --
   -- The call-path to which the bounds apply.
   -- If the result is the null array (Length = 0), the bounds
   -- are universal.


   function Level (Item : Bounds_Ref) return Bounding_Level_T;
   --
   -- The call-path level (length) at which the given bounds apply.
   -- Same as Call_Path(Item)'Length.


   function Program (Item : Bounds_Ref) return Program_T;
   --
   -- The program within which the bounds are defined.


   function Symbol_Table (Item : Bounds_Ref) return Symbols.Symbol_Table_T;
   --
   -- The symbol-table for the program within which the bounds are defined.
   -- This is equivalent to Symbol_Table (Program (Item)).


   function For_Time (Bounds : Bounds_Ref) return Boolean;
   --
   -- Whether we seek Bounds on execution time.


   function For_Space (Bounds : Bounds_Ref) return Boolean;
   --
   -- Whether we seek Bounds on (stack) memory space.


   function Processor_Info (From : Bounds_Ref)
   return Processor.Execution.Info_T;
   --
   -- The processor-specific information From the given bounds.


   function Computation (Item : Bounds_Ref)
   return Flow.Computation.Model_Handle_T;
   --
   -- The computation model that underlies the bounds, to be used or
   -- changed by the caller. Since a handle is returned, copy-on-change
   -- will change the model attached to the bounds. However, the caller
   -- must not change the model after the bounds are frozen.


   function Value_Origins_Defined (Item : Bounds_Ref)
   return Boolean;
   --
   -- Whether a value-origin analysis has been performed and recorded
   -- for these bounds.


   function Value_Origins (Item : Bounds_Ref)
   return Flow.Origins.Map_Ref;
   --
   -- The value-origin map of the computation model that underlies
   -- the bounds, if one already exists. Thus, the result may be a
   -- "null" map (not Flow.Origins.Is_Valid), but in that case the
   -- function Value_Origins_Defined returns False.
   --
   -- A reference to the map object is returned, not a copy. This
   -- means that the returned value is valid only for as long as the
   -- the given execution bounds exist and retain the same map object.
   -- If the map object in the bounds is discarded (e.g. through a
   -- call of Note_Updated_Computation) the returned reference becomes
   -- a dangling pointer and must not be used.


   function Is_Feasible (Item : Bounds_Ref) return Boolean;
   --
   -- Whether the subprogram has (or may have, as far as we know) a
   -- feasible execution path under these bounds.
   --
   -- If the subprogram itself is "unused" (see Programs.Unused) then
   -- we consider that it has no feasible execution path under any
   -- execution bounds. For other subprograms, if the computation
   -- model is infeasible, or if bounds have an Infeasible time-state,
   -- then the subprogram is infeasible under these bounds; otherwise
   -- the subprogram is (or seems to be) feasible under these bounds.


   function Returns (Item : Bounds_Ref) return Boolean;
   --
   -- Whether the subprogram, when executed under these bounds, ever
   -- returns to its caller.
   --
   -- If the subprogram itself is non-returning (see Programs.Returns)
   -- then it is non-returning under all execution bounds. For other
   -- subprograms it depends on the feasibility of the paths to the
   -- return points, which depends on the context and the execution
   -- bounds.


   function Calling_Protocol (
      Call   : Call_T;
      Within : Bounds_Ref)
   return Calling.Protocol_Ref;
   --
   -- The calling protocol used for the given Call, Within the
   -- computation model that underlies the given execution bounds.


   function Dynamic_Flow (Item : Bounds_Ref)
   return Flow.Edge_Resolution_T;
   --
   -- The overall state of resolution of the dynamic edges (computed
   -- control flow) in the flow-graph of the subprogram that Item
   -- bounds, where we include only dynamic edges that are feasible
   -- under the computation model in the execution bounds.
   --
   -- See Flow.Computation.Dynamic_Flow for more description.


   function Unstable_Dynamic_Edges (Item : Bounds_Ref)
   return Flow.Dynamic_Edge_List_T;
   --
   -- All those dynamic edges, in the flow-graph of the subprogram that
   -- item bounds, that are not yet stably resolved (or stably unresolved)
   -- but are feasible under the computation model in the execution bounds.
   -- A dynamic edge is considered feasible iff the source step is
   --
   -- See Flow.Computation.Unstable_Dynamic_Edges for more description.


   function Assertion_Map (Item : Bounds_Ref)
   return Assertions.Assertion_Map_T;
   --
   -- The assertion map attached to the bounds, or No_Map
   -- if no map has been attached.


   function Inputs_Defined (Item : Bounds_Ref)
   return Boolean;
   --
   -- Whether the input cells have been defined for these bounds.


   function Input_Cells (Item : Bounds_Ref)
   return Storage.Cell_List_T;
   --
   -- The input cells for these bounds, when defined.
   -- This set is null until defined by Set_Input_Cells.


   function Number_Of_Input_Cells (Item : Bounds_Ref)
   return Natural;
   --
   -- The number of input cells for these bounds, when defined.
   -- Same as Input_Cells'Length.


   function Input_Cells (
      Call   : Call_T;
      Within : Bounds_Ref)
   return Storage.Cell_List_T;
   --
   -- The input cells for the Call, Within the bounds of the caller.
   -- Taken from the current execution bounds on the Call, Within
   -- the bounds of the caller.


   function Basis (Item : Bounds_Ref)
   return Storage.Cell_List_T;
   --
   -- The Presburger arithmetic basis cells for these bounds, or the
   -- empty list if arithmetic analysis was not done (yet).


   function Initial (Item : Bounds_Ref)
   return Storage.Bounds.Cell_Interval_List_T;
   --
   -- The initial bounds on cells, upon entry to the subprogram,
   -- from which these bounds were derived.


   function Outputs_Defined (Item : Bounds_Ref)
   return Boolean;
   --
   -- Whether the output cells have been defined for these bounds.


   function Output_Cells (Item : Bounds_Ref)
   return Storage.Cell_List_T;
   --
   -- The output cells for these bounds, when defined.
   -- This set is null until defined by Set_Output_Cells.


   function Enough_For_Time (Item : Bounds_Ref) return Boolean;
   --
   -- Whether we shall assume that there are enough assertions on the
   -- execution count of parts of the subprogram under these Bounds to
   -- bound the execution path and thus give a bound on execution time,
   -- even if there are unbounded loops or if the flow-graph is
   -- irreducible.


   function Time_State (Item : Bounds_Ref) return Time_State_T;
   --
   -- The state of the execution-time bound for the given
   -- execution bounds.


   function Time_Calculated (Within : Bounds_Ref) return Boolean;
   --
   -- Whether the path- and time-calculation phase has been applied
   -- to these execution bounds. See Set_Time_Calculated.


   function Time_Bounded (Item : Bounds_Ref) return Boolean;
   --
   -- Whether the execution time bound is known.
   -- Defined as Time_State (Item) in Time_Bounded_T.


   function Time_Asserted (Item : Bounds_Ref) return Boolean;
   --
   -- Whether the WCET defined by the bounds was asserted by the
   -- user, rather than computed by analysis of the program.
   -- Defined as Time_State = Asserted.


   function Time (Item : Bounds_Ref) return Processor.Time_T;
   --
   -- The bound on the worst-case execution time, assuming that
   -- Time_Bounded (Item). Note that it is not enough for the
   -- Time_State to be Computable.


   procedure Prune_Flow (Item : in Bounds_Ref);
   --
   -- Prunes the infeasible parts from the flow-graph (computation
   -- model) associated with the execution bounds. Internally calls
   -- Mark_Flow_Pruned.
   --
   -- This is not allowed if the bounds are "frozen".


   procedure Mark_Flow_Pruned (Item : in Bounds_Ref);
   --
   -- Signals that the flow-graph (computation model) has been changed
   -- by making some parts infeasible.
   --
   -- This is not allowed if the bounds are "frozen".


   procedure Mark_Infeasible (
      Step   : in Flow.Step_T;
      Within : in Bounds_Ref);
   --
   -- Marks a Step as infeasible Within the execution of a subprogram.
   -- The step is marked as infeasible in the computation model and the
   -- model is then pruned.
   --
   -- This is not allowed if the bounds are "frozen".


   procedure Mark_Infeasible (
      Edges  : in Flow.Edge_List_T;
      Within : in Bounds_Ref);
   --
   -- Marks each of the Edges as infeasible Within the execution of a
   -- subprogram. The edges are marked as infeasible in the computation
   -- model and the model is then pruned.
   --
   -- This is not allowed if the bounds are "frozen".


   procedure Mark_Infeasible (Bounds : in Bounds_Ref);
   --
   -- Marks the whole execution of a subprogram as infeasible under
   -- the given Bounds. This means that the entry step is marked as
   -- infeasible in the computation model and the model is then pruned.
   --
   -- This is not allowed if the bounds are "frozen".


   function Bounded (Bounds : Bounds_Ref)
   return Boolean;
   --
   -- Whether these execution bounds are complete in those respects
   -- for which the bounds were created (execution time and/or stack
   -- memory space). For time we accept that Time_State may be only
   -- Computable, not yet Computed (Asserted is also good, of course).


   function Bounded (
      Luup   : Loops.Loop_T;
      Within : Bounds_Ref)
   return Boolean;
   --
   -- Whether the given Luup has iteration bounds Within the execution
   -- bounds, either neck-bounds or repeat-bounds or both. The Luup's
   -- feasibility has no effect.


   function Loop_Start_Bound (
      Luup   : Loops.Loop_T;
      Within : Bounds_Ref)
   return Flow.Execution.Bound_T;
   --
   -- The bound, if any, on the number of times the given Luup is
   -- started (entered from outside the loop), Within the given
   -- bounds and per one execution of the subprogram that contains
   -- the Luup.


   function Loop_Neck_Bound (
      Luup   : Loops.Loop_T;
      Within : Bounds_Ref)
   return Flow.Execution.Bound_T;
   --
   -- The bound, if any, on the execution count of the neck
   -- of the given Luup, Within the given bounds.


   function Loop_Repeat_Bound (
      Luup   : Loops.Loop_T;
      Within : Bounds_Ref)
   return Flow.Execution.Bound_T;
   --
   -- The bound, if any, on the execution count of the repeat
   -- edges of the given Luup, Within the given bounds.


   function Loop_Start_Bounds (Item : Bounds_Ref)
   return Loop_Bounds_T;
   --
   -- The loop-start bounds. See Bound_Loop_Starts.
   -- Information is returned for all loops, whether feasible or not.


   function Loop_Neck_Bounds (Item : Bounds_Ref)
   return Loop_Bounds_T;
   --
   -- The loop-neck-bounds. See Bound_Loop_Neck.
   -- Information is returned for all loops, whether feasible or not.


   function Loop_Repeat_Bounds (Item : Bounds_Ref)
   return Loop_Bounds_T;
   --
   -- The loop-repeat-bounds. See Bound_Loop_Repeats.
   -- Information is returned for all loops, whether feasible or not.


   function Call_Input_Bounds (
      On     : Call_T;
      Within : Bounds_Ref)
   return Storage.Bounds.Bounds_Ref;
   --
   -- The bounds, if any, recorded for the input cells and protocol basis
   -- cells On the given call Within the given bounds on the caller,
   -- Information is returned whether the call is feasible or not.


   function Call_Bounds (
      On     : Call_T;
      Within : Bounds_Ref)
   return Bounds_Ref;
   --
   -- Returns the execution bounds On the given call from Within
   -- the given bounds on the caller.
   -- Information is returned whether the call is feasible or not.


   function Call_Bounds (Within : Bounds_Ref)
   return Call_Bounds_List_T;
   --
   -- The execution bounds (complete or not) of those lower-level
   -- calls Within the given bounds that are feasible under the
   -- computation model.


   function Unbounded_Loops (
      Within  : Bounds_Ref;
      Eternal : Boolean)
   return Loops.Loop_List_T;
   --
   -- The loops that are not yet bounded but are feasible and feasibly
   -- repeatable under the computation model Within these bounds.
   -- The parameter Eternal selects whether eternal loops are included:
   -- if False, eternal loops are omitted; if True, eternal loops are
   -- included if they have the other required properties: not yet
   -- bounded, etc.
   --
   -- These loops may be successfully bounded later, by additional
   -- assertions or further analysis (when not eternal), perhaps in
   -- a more specific context when more parameter values are known.


   function Context_Dependent_Calls (Within : Bounds_Ref)
   return Call_List_T;
   --
   -- The calls Within some (caller) subprogram's bounds that are not
   -- yet bounded with respect to the desired execution measures (time
   -- and/or stack memory space) but are feasible under the computation
   -- model and may allow context-dependent bounds to be found. This
   -- includes calls that have no input cells; context-dependent bounds
   -- for such calls then require some kind of call-specific or
   -- context-specific assertions.


   function Input_Dependent_Calls (Within : Bounds_Ref)
   return Call_List_T;
   --
   -- The calls Within some (caller) subprogram's bounds that are not
   -- yet bounded with respect to the desired execution measures (time
   -- and/or stack memory space) but are feasible under the computation
   -- model and may allow context-dependent bounds to be found when
   -- the context constrains the value of the inputs for the call and
   -- thus bounds the execution of the callee. At present, the only
   -- criterion (in addition to Context_Dependent_Calls) is that the
   -- call should have some input cells.


   function Unbounded_Calls (
      Within      : Bounds_Ref;
      Irreducible : Boolean)
   return Call_List_T;
   --
   -- The calls Within some (caller) subprogram's bounds that are not
   -- yet bounded with respect to the desired execution measures (time
   -- and/or stack memory space) but are feasible under the computation
   -- model and are not stub calls (that is, the callee is not a stub).
   --
   -- A call is considered unbounded if it is not Bounded.
   --
   -- If space bounds are desired, a call is moreover considered
   -- unbounded if the call's take-off height is unbounded Within the
   -- caller for some stack.
   --
   -- If Irreducible is True, we include calls of irreducible callees,
   -- otherwise only calls with reducible callees are returned. Calls
   -- to stubs are never included.


   procedure Add_Inputs_For_Unbounded_Calls (
      Within : in     Bounds_Ref;
      To     : in out Storage.Cell_Set_T);
   --
   -- Adds To the given cell-set the input cells for the not-fully-bounded
   -- lower-level calls Within the given bounds, recursively to all levels,
   -- as far as these cells have been resolved and mapped to the caller's
   -- frame.
   --
   -- The conditions under which a call is considered "unbounded" are
   -- explained above, in connection with the function Unbounded_Calls,
   -- and both reducible and irreducible callees are included. Calls
   -- to stubs are never included.
   --
   -- The returned cells are viewed in the caller's frame.


   function Inputs_For_Unbounded_Calls (Within : Bounds_Ref)
   return Storage.Cell_Set_T;
   --
   -- The input cells for the not-fully-bounded lower-level calls
   -- Within the given bounds, recursively to all levels, as far as
   -- they have been resolved and mapped to the caller's frame.
   -- Calls to stubs are not considered to have any input cells.
   --
   -- The returned cells are viewed in the caller's frame.


   function Unbounded_Stack_Steps (Within : Bounds_Ref)
   return Flow.Step_List_T;
   --
   -- The feasible steps that are relevant for the further analysis of
   -- local and final stack height, under the computation model Within
   -- the given execution bounds. These steps are the following:
   --
   -- > All final (return) steps, if there is an Unstable stack that
   --   is still without bounds on the final stack height. These steps
   --   are included whether or not space bounds are desired, because
   --   the final stack height is important for any kind of data-flow
   --   analysis.
   --
   -- > All steps that change the stack-height of some stack that is
   --   still without bounds on the stack usage and without bounds on
   --   the local stack height. These steps are included only if space
   --   bounds are desired.
   --
   -- The result will not contain duplicated steps even if the same
   -- step changes the height of several unbounded stacks or if
   -- a return step changes the stack height of some unbounded stack.


   function Calls_With_Unbounded_Take_Off (Item : Bounds_Ref)
   return Call_List_T;
   --
   -- The calls that do not have a bounded take-off stack-height
   -- within the given execution bounds for any stack that does not
   -- yet have stack-usage bounds within these same execution bounds.
   -- Includes only calls that are feasible under the computation model.
   --
   -- The result is a null list if the program under analysis has no
   -- stacks.


   function Flow_Bounds_Feasible (Item : in Bounds_Ref)
   return Boolean;
   --
   -- Whether the execution-count bounds on the loops, calls, and nodes
   -- within the given execution bounds are feasible (no element has
   -- contradictory bounds = an empty interval of allowed execution
   -- counts).


   function Node_Bounds (Item : in Bounds_Ref)
   return Node_Bounds_T;
   --
   -- Returns the execution-count bounds of nodes that are given by
   -- these bounds. Information is returned for all nodes, whether
   -- feasible or not.


   function Edge_Times_Bounded (Item : in Bounds_Ref)
   return Boolean;
   --
   -- Whether the per-edge execution times have been bounded.
   -- The same value holds for per-step-edge times and per-node-edge times.


   function Step_Edge_Times (Item : in Bounds_Ref)
   return Step_Edge_Times_T;
   --
   -- The per-step-edge execution times, if bounded.
   -- Information is returned for all edges, whether feasible or not.


   function Time (Edge : Flow.Step_Edge_T; From : Bounds_Ref)
   return Processor.Time_T;
   --
   -- The execution time for this step-edge, if bounded, else zero.
   -- Information is returned whether the edge is feasible or not.


   function Edge_Times (Item : in Bounds_Ref)
   return Edge_Times_T;
   --
   -- The per-node-edge execution times, if bounded.
   -- Information is returned for all edges, whether feasible or not.


   function Time (Edge : Flow.Edge_T; From : Bounds_Ref)
   return Processor.Time_T;
   --
   -- The execution time for this node-edge, if bounded, else zero.
   -- Information is returned whether the edge is feasible or not.


   function Node_Times_Bounded (Item : in Bounds_Ref)
   return Boolean;
   --
   -- Whether the per-node execution times have been bounded.


   function Node_Times (
      From       : Bounds_Ref;
      With_Calls : Boolean)
   return Node_Times_T;
   --
   -- Returns the worst-case execution times of nodes and edges
   -- that are given by these bounds.
   --
   -- The execution time of calls to lower-level subprograms is
   -- included only if desired (With_Calls = True).
   --
   -- Information is returned for all nodes, whether feasible or not.


   function Time (
      Node       : Flow.Node_T;
      From       : Bounds_Ref;
      With_Calls : Boolean)
   return Processor.Time_T;
   --
   -- The execution time for this node, if bounded, else zero.
   -- Information is returned whether the node is feasible or not.


   --
   ---   Execution counts for nodes, loops, calls, ...
   --


   function "*" (
      Left  : Flow.Execution.Count_T;
      Right : Processor.Time_T)
   return Processor.Time_T
   renames Flow.Execution."*";


   function Counts_Set (Item : in Bounds_Ref) return Boolean;
   --
   -- Whether the worst-case execution counts for the nodes and
   -- edges have been defined (set) in these bounds.


   function Counts (Item : in Bounds_Ref)
   return Flow_Counts_Ref;
   --
   -- Returns the worst-case execution counts for the nodes and
   -- edges, according to these bounds and if Counts_Set is True.
   --
   -- Information is returned for all nodes and edges, whether
   -- feasible or not. The execution count for infeasible graph
   -- elements will be zero. Null is returned if Counts:Set is
   -- False (no execution counts set).


   function Count (Node : Flow.Node_T; Within : Bounds_Ref)
   return Flow.Execution.Count_T;
   --
   -- The execution count of the Node, in the worst-case execution path
   -- Within the given bounds. Zero if the Counts_Set is False.


   function Count (Edge : Flow.Edge_T; Within : Bounds_Ref)
   return Flow.Execution.Count_T;
   --
   -- The execution count of the Edge, in the worst-case execution path
   -- Within the given bounds. Zero if the Counts_Set is False.


   function Start_Count (
      Luup   : Loops.Loop_T;
      Within : Bounds_Ref)
   return Flow.Execution.Count_T;
   --
   -- The total number of times the given Luup is started in
   -- the worst-case execution path Within the given bounds,
   -- assuming that Counts_Set is True (otherwise zero is
   -- returned).
   --
   -- The number of "starts" is the sum of the execution count
   -- of all edges leading from outside the loop to the loop head.


   function Head_Count (
      Luup   : Loops.Loop_T;
      Within : Bounds_Ref)
   return Flow.Execution.Count_T;
   --
   -- The total execution count of the head of the given Luup,
   -- in the worst-case execution path Within the given bounds,
   -- assuming that Counts_Set is True (otherwise zero is
   -- returned).


   function Neck_Count (
      Luup   : Loops.Loop_T;
      Within : Bounds_Ref)
   return Flow.Execution.Count_T;
   --
   -- The total execution count of the neck of the given Luup,
   -- in the worst-case execution path Within the given bounds,
   -- assuming that Counts_Set is True (otherwise zero is
   -- returned).
   --
   -- The neck count is the total execution count of the edges
   -- leading from the loop-head into the loop-body or back to
   -- the loop-head.


   function Repeat_Count (
      Luup   : Loops.Loop_T;
      Within : Bounds_Ref)
   return Flow.Execution.Count_T;
   --
   -- The total repetition count of the given Luup, in the
   -- worst-case execution path Within the given bounds, assuming
   -- that Counts_Set is True (otherwise zero is returned).
   --
   -- The repetition count is the total execution count of the edges
   -- leading from the loop-body (which includes the loop-head)
   -- back to the loop-head.


   function Call_Count (
      Call   : Call_T;
      Within : Bounds_Ref)
   return Flow.Execution.Count_T;
   --
   -- The total execution count of the given Call, in the
   -- worst-case execution path Within the given bounds, assuming
   -- that Counts_Set is True (otherwise zero is returned).


   function Call_Count (Link : Link_T)
   return Flow.Execution.Count_T;
   --
   -- The total execution count of the call in the given Link,
   -- in the worst-case execution path within the Caller_Bounds
   -- of the Link, assuming that Counts_Set is True for these bounds
   -- (otherwise zero is returned).


   --
   ---   The nodes, edges etc that are executed on the worst-case path
   --


   function Executed_Edges (Within : Bounds_Ref)
   return Flow.Edge_List_T;
   --
   -- The list of edges that are executed (positive execution count)
   -- on the worst-case path Within the given bounds, in no particular
   -- order. If Counts_Set is False, a null list is returned.


   function Executed_Call_Bounds (Within : Bounds_Ref)
   return Call_Bounds_List_T;
   --
   -- The execution bounds of those lower-level calls Within the give
   -- bounds that have a positive execution count in the worst-case
   -- execution path Within the given bounds. If Counts_Set is
   -- is False, a null list is returned.


   --
   ---   Total execution time for (sets of) nodes, edges, loops, calls
   --


   function Total_Time (
      Nodes      : Flow.Node_List_T;
      Within     : Bounds_Ref;
      With_Calls : Boolean)
   return Processor.Time_T;
   --
   -- The total execution time of the given Nodes, in the worst-case
   -- execution Within the bounds. This is the sum of execution-time times
   -- execution-count, over all Nodes. We include the execution time of
   -- callees if With_Calls is True.
   --
   -- The result is meaningful if the bounds contain the per-node execution
   -- times and execution counts per node and edge (the worst-case
   -- path). Otherwise the result is zero.


   function Total_Time (
      Edges  : Flow.Edge_List_T;
      Within : Bounds_Ref)
   return Processor.Time_T;
   --
   -- The total execution time of the given Edges, in the worst-case
   -- execution Within the given bounds.
   --
   -- The result is meaningful if the bounds contain the per-edge execution
   -- times and execution counts per node and edge (the worst-case
   -- path). Otherwise the result is zero.


   type Time_Sum_T is record
      Num : Natural;
      Sum : Processor.Time_T;
      Min : Processor.Time_T;
      Max : Processor.Time_T;
   end record;
   --
   -- The sum and range of a set of execution times.


   function Total_Edge_Time (Within : Bounds_Ref)
   return Time_Sum_T;
   --
   -- The total execution time of the flow-graph edges (step edges), in
   -- the worst-case execution Within the given bounds, and the range
   -- of the execution times of those edges.
   --
   -- The result includes the internal edges of the executed nodes and
   -- the executed inter-node edges. The Num component counts only
   -- the executed edges with a positive (nonzero) execution time.
   --
   -- The execution time of the flow-graph steps is excluded, which
   -- means that also the execution time of the callees is excluded.
   --
   -- If the worst-case execution contains no edges, the result is
   -- (others => 0).
   --
   -- Precondition: The edge times and execution counts are known.


   function Total_Time (
      Luup       : Loops.Loop_T;
      Within     : Bounds_Ref;
      With_Calls : Boolean)
   return Processor.Time_T;
   --
   -- The total execution time for the given Luup, in the worst-case
   -- execution Within the given bounds. This is meaningful if, Within
   -- the bounds, the execution time of each node and edge is defined
   -- and the worst-case execution path (execution counts of each node
   -- and edge) is defined (otherwise the result is zero).
   --
   -- The total execution time for the Luup is the sum, over all
   -- nodes and edges in the Luup, of the execution time of the node or
   -- edge times the execution count of the node or edge. Edges that
   -- have only one end in the loop (entry edges or exit edges) are
   -- not included.
   --
   -- The execution time of lower-level calls can optionally be included,
   -- as controlled by With_Calls.


   function Total_Time (
      Call   : Call_T;
      Within : Bounds_Ref)
   return Processor.Time_T;
   --
   -- The total execution time for the given Call, in the worst-case
   -- execution Within the given bounds. This is meaningful if, Within
   -- the bounds, the execution time of each node and edge is defined
   -- and the worst-case execution path (execution counts of each node
   -- and edge) is defined (otherwise the result is zero). (Well, to be
   -- precise, it would be enough for the execution time and execution
   -- count to be defined for just for the call step.)
   --
   -- The total execution time for the Call is the product of the
   -- execution time of the call step and the execution count of this step.


   function Total_Time (Link : Link_T)
   return Processor.Time_T;
   --
   -- The total execution time of the call in the given Link, within
   -- the execution bounds of the caller of the Link. More info in
   -- Total_Time (Call, Bounds_Ref).


   function Total_Time (
      Calls  : Call_Bounds_List_T;
      Within : Bounds_Ref)
   return Processor.Time_T;
   --
   -- The total execution time of the given Calls (each with their
   -- execution bounds), in the worst-case execution Within the given
   -- caller bounds. This is meaningful if, Within the caller bounds,
   -- the execution time of each node and edge is defined and the
   -- worst-case execution path (execution counts of each node
   -- and edge) is defined (otherwise the result is zero).
   --
   -- The total execution time for a call is the product of the
   -- execution-time bound of the callee bounds (from Calls) and
   -- the execution count of the call (Within the caller's bounds)
   -- The result includes only those Calls where the execution time
   -- of the callee is bounded.


   function Callee_Time (Within : Bounds_Ref)
   return Processor.Time_T;
   --
   -- The total execution time (bound) of the callees, Within the
   -- given execution bounds for a caller subprogram. For more
   -- description of the preconditions and the meaning of the result,
   -- see function Total_Time (Calls, Within) above.


   --
   ---  Final stack height bounds
   --


   function Final_Stack_Height_Known (
      Stack  : Stack_T;
      Within : Bounds_Ref)
   return Boolean;
   --
   -- Whether the final (local) stack height of the given Stack, Within
   -- the given execution bounds, is bounded to a single, known value.
   --
   -- For a Stable stack the final height (zero) is always known.
   -- For an Unstable stack it depends on the Net_Change attribute and
   -- on earlier calls of Bound_Final_Stack_Height, if any.
   --
   -- If the subprogram under these bounds never returns, we consider
   -- the final stack height to be known for all stacks.


   function Final_Stack_Heights_Known (Within : Bounds_Ref)
   return Boolean;
   --
   -- Whether the final (local) stack height is bounded to a single,
   -- known value, Within the given execution bounds, for all stacks
   -- used in the program under analysis.
   --
   -- If the subprogram under these bounds never returns, we consider
   -- the final stack height to be known for all stacks.


   function Final_Stack_Height (
      Stack  : Stack_T;
      Within : Bounds_Ref)
   return Final_Stack_Height_T;
   --
   -- The bounds, as far as known, on the final (local) stack height
   -- of the given Stack, Within the given execution bounds. These are
   -- bounds on the net change in the height of the stack for one
   -- execution of the subprogram to which these bounds apply.
   --
   -- For a Stable stack the final height (zero) is always known.
   -- For an Unstable stack it depends on the Net_Change attribute and
   -- on earlier calls of Bound_Final_Stack_Height, if any.
   -- If no bounds are known (set) the result is Universal_Interval.


   function Loose_Final_Stack_Heights (Within : Bounds_Ref)
   return Storage.Cell_List_T;
   --
   -- The stack-height cells of those (unstable) stacks for which
   -- good (singular) bounds on the final stack-height are not yet
   -- known Within the given execution bounds.


   function Final_Stack_Effect (From : Bounds_Ref)
   return Arithmetic.Effect_T;
   --
   -- The effect (changes) to the local heights and pointers of all
   -- stacks defined in the program, From one execution of the
   -- subprogram under the given execution bounds.
   --
   -- The result is a set of assignments where each local-stack-height
   -- cell is given either a known increment or decrement (assignment
   -- of the form sh := sh + const or sh := sh - const) or is given
   -- an unknown value (sh := ?). Likewise, each stack pointer cell
   -- is given the same or the opposite increment or decrement,
   -- depending on the coupling between the pointer and the height.
   --
   -- PLAN TBA: If the subprogram has no net effect on a stack then there
   -- is no assignment to the stack-height cell or stack pointer cell of
   -- that stack. This is the case (at least) for all Stable stacks,
   -- so the result never contains an assignment to the stack-height
   -- or stack pointer of a Stable stack.
   -- ACTUALLY NOW: If there is no net effect on a stack then there
   -- are identity assignments of the form "sh := sh" and "sp := sp"
   -- for the stack height and stack pointer. This is a technical hack
   -- and should be removed TBM.
   --
   -- TBA possible use of range-post constraints when the change is
   -- bounded but not to a single value.


   --
   ---   Stack bounds
   --


   function Bounded (Item : Stack_Limit_T) return Boolean;
   --
   -- Whether the stack limit has a finite upper bound (Max).


   function Exact (Item : Stack_Limit_T) return Boolean;
   --
   -- Whether the stack limit is exact (defines a single finite value).


   function Bounded (Item : Stack_Usage_T) return Boolean;
   --
   -- Whether the stack usage is bounded (computed or asserted).


   function Space_Bounded (Item : in Bounds_Ref)
   return Boolean;
   --
   -- Whether the memory space usage is bounded.
   -- Currently only the stack usage (for all stacks) is considered,
   -- not any other dynamic memory usage.


   function Stack_Height_Bounded (
      Stack  : Stack_T;
      Within : Bounds_Ref)
   return Boolean;
   --
   -- Whether the local stack-height of the given Stack is bounded Within
   -- some execution bounds, ignoring the stack-usage of the lower-level
   -- callees. This means that the Bound_Stack_Height operation has
   -- supplied a finite limit on stack height for this Stack.
   --
   -- Note that the Stack_Height function will always return some height
   -- limit, by default Storage.Bounds.Universal_Interval and otherwise
   -- the limit supplied by Bound_Stack_Height.


   function Stack_Height (
      Stack  : Stack_T;
      Within : Bounds_Ref)
   return Stack_Limit_T;
   --
   -- The upper limit for local stack-height of the given Stack, Within
   -- the bounds for a subprogram. This considers only the subprogram
   -- to which the given bounds apply and ignores all lower-level callees.
   --
   -- If Bound_Stack_Height has not been applied to these execution
   -- bounds and this Stack, the default limit Universal_Internal
   -- is returned.


   function Stack_Usage_Bounded (
      Stack  : Stack_T;
      Within : Bounds_Ref)
   return Boolean;
   --
   -- Whether the total stack-usage is bounded, including the stack-usage
   -- of the lower-level callees, for the given Stack, Within the given
   -- execution bounds. This means that the Bound_Stack_Usage operation
   -- has supplied a finite limit on the usage of this Stack. The local
   -- stack height and final stack height are not considered.


   function Stack_Usage_Bounded (Item : in Bounds_Ref)
   return Boolean;
   --
   -- Whether the total stack-usage is bounded, including the stack-usage
   -- of the lower-level callees, for all stacks. This means that the
   -- Bound_Stack_Usage operation has supplied a finite limit on the
   -- usage of all stacks. The local stack height and final stack
   -- height are not considered.


   function Stack_Usage (
      Stack  : Stack_T;
      Within : Bounds_Ref)
   return Stack_Usage_T;
   --
   -- The upper limit for stack usage for the given Stack, Within the
   -- execution bounds for the call-tree starting from the subprogram
   -- to which these bounds apply. Thus, the stack usage of lower-level
   -- callees is included.
   --
   -- If Bound_Stack_Usage has not been applied to these execution
   -- bounds and this Stack, an Undefined stack usage is returned.


   function Loose_Stack_Heights (Within : Bounds_Ref)
   return Storage.Cell_List_T;
   --
   -- The stack-height cells of those stacks for which bounds on the
   -- total stack usage are not yet known Within the given execution bounds.


   function Space_State (
      Stack  : Stack_T;
      Within : Bounds_Ref)
   return Space_State_T;
   --
   -- The state of the (current) bounds on stack usage for the
   -- given Stack, Within the given execution bounds.


   function Space_State (Within : Bounds_Ref)
   return Space_State_T;
   --
   -- The state of the (current) bounds on stack usage, Within the
   -- given execution bounds, over all stacks, as follows:
   --
   -- > If any stack is Infeasible the result is Infeasible, otherwise
   -- > if any stack is Depends the result is Depends, otherwise
   -- > if any stack is Vague the result is Vague, otherwise
   -- > if any stack is Undefined the result is Undefined, otherwise
   -- > if any stack is Unbounded the result is Unbounded, otherwise
   -- > if any stack is Computed the result is Computed, otherwise
   -- > all stacks are  Asserted and the result is Asserted.


   function Take_Off_Height_Bounded (
      Stack  : Stack_T;
      Before : Call_T;
      Within : Bounds_Ref)
   return Boolean;
   --
   -- Whether the caller's local stack-height, for a given Stack and
   -- just Before a call goes to the callee, is bounded. This means that
   -- the operation Bound_Take_Off_Height has supplied a finite limit
   -- for the local stack-height in this Stack before the call. Note
   -- that the function Take_Off_Height will always return some height
   -- limit, by default a Universal_Interval limit and otherwise the
   -- limit supplied by Bound_Take_Off_Height.
   --
   -- Information is returned whether the call is feasible or not.


   function Take_Off_Height (
      Stack  : Stack_T;
      Before : Call_T;
      Within : Bounds_Ref)
   return Stack_Limit_T;
   --
   -- The upper limit for the caller's stack-height in the given Stack
   -- just Before control transfer to the callee, Within the bounds of
   -- the caller.
   --
   -- If Bound_Take_Off_Height has not been applied to this call and
   -- this Stack in these execution bounds, the default limit
   -- Universal_Interval is returned.
   --
   -- Information is returned whether the call is feasible or not.


   -- function Image (Item : Stack_Limit_T) return String;
   --
   -- A description of the limit as a single value or an interval
   -- with finite or infinite ends.
   --
   -- This function is inherited from Storage.Bounds.


   function Max_Image (Item : Stack_Limit_T) return String;
   --
   -- A description of the upper bound of the limit. The lower bound
   -- is ignored (not shown).


   --
   ---   Bounds_Subset_T operations
   --


   procedure Erase (Subset : in out Bounds_Subset_T);
   --
   -- Makes the subset empty.


   function Is_Member (
      Index  : Bounds_Index_T;
      Of_Set : Bounds_Subset_T)
   return Boolean;
   --
   -- Whether the bounds object with the given Index is a member
   -- of the subset.


   function Is_Member (
      Item   : Bounds_Ref;
      Of_Set : Bounds_Subset_T)
   return Boolean;
   --
   -- Whether the Item is a member of the subset.


   procedure Add (
      Item : in     Bounds_Ref;
      To   : in out Bounds_Subset_T);
   --
   -- Adds (makes a member) an Item To a subset.


private

   -- Most of the type implementations are deferred to the body:


   type Bounds_Set_Object_T;

   type Bounds_Set_T is access Bounds_Set_Object_T;

   No_Bounds_Set : constant Bounds_Set_T := null;


   type Bounds_Object_T;

   type Bounds_Ref is access Bounds_Object_T;

   No_Bounds : constant Bounds_Ref := null;


   type Bounds_Bitmap_T is array (Bounds_Index_T range <>) of Boolean;
   --
   -- The basis for a Bounds_Subset_T.


   type Bounds_Subset_T (Max_Size : Bounds_Count_T)
   is record

      Members : Bounds_Bitmap_T (1 .. Max_Size) := (others => False);

   end record;


   type Link_T is record
      Call   : Call_T;
      Caller : Bounds_Ref;
      Callee : Bounds_Ref;
   end record;


end Programs.Execution;
