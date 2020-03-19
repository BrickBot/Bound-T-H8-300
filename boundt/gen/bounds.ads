-- Bounds (decl)
--
-- Bounding the execution of the subprograms under analysis.
--
-- Given: a set of subprograms, represented by control-flow graphs
-- and related by subprogram calls.
--
-- Goal: bounds on the dynamic control flow and stack usage of each
-- "root" subprogram in the set and all its callees.
--
-- To reach the goal, we must compute bounds on the dynamic behaviour of
-- subprograms. Dynamic behaviour arises when the execution flow depends
-- on data variables. The given subprogram set may have several sources
-- of such dynamic behaviour:
--
-- > Some of the control-flow graphs may be incomplete, that is, they
--   may contain unresolved dynamic branches. We try to bound the possible
--   targets of such branches, to help complete the control-flow graphs.
--
-- > Some of the calls may be incompletely resolved, that is, the
--   callee subprogram(s) may be dynamically determined and not yet
--   (fully) known. We try to bound the set of possible callees, to help
--   complete the call-graph of the subprogram set.
--
-- > Loops may (and usually do) have a dynamically defined number of
--   iterations. We try to bound the iterations of counter-based loops.
--
-- > TBA Different conditional branches may be correlated so that
--   certain paths are feasible while others are infeasible. We try
--   to detect the most important infeasible paths and bound the flow
--   model to exclude these paths.
--
-- > Pushing or popping a dynamically determined amount of data onto
--   or off the stack creates dynamic stack consumption. We try to
--   bound the amount of data that is pushed or popped (or the
--   equivalent changes in the stack pointer caused by arithmetic
--   instructions).
--
-- To bound such dynamic behaviour, we create models of the arithmetic
-- computation (data processing) that the subprograms do, and use these
-- models to bound the values of the variables that the dynamic items
-- use. This introduces more forms of dynamic behaviour that concern
-- the computation model itself:
--
-- > There may be dynamically addressed data references (indexed
--   addressing, indirect addressing). We try to bound the set of
--   possible referents, to make the computation model more accurate.
--
-- > The data-flow across a call, from the caller to the callee or vice
--   versa, may use a dynamically defined mapping from the caller's
--   variables (actual parameters) to the callee's variables (formal
--   parameters). This may depend, for example, on the local "stack
--   height" in the caller. We try to bound these mappings to make
--   the computation model more accurate across calls.
--
-- The data-flow across calls is important for two reasons:
--
-- > The computation model of the caller must include the possible
--   effects of the callee on the computation in the caller.
--
-- > In the arithmetic analysis of the callee, it may be essential
--   to use bounds on the values of the input parameters (and global
--   variables) that flow from the caller to the callee.
--
-- Many of the bounds can be "context dependent", that is, different
-- bounds may be computed for different call-paths to a given subprogram.
-- See Programs.Execution.Bounds_T for detail.
--
-- The bounds computed for a given control-flow graph are conservative
-- bounds in the sense that they include all possible executions of the
-- control-flow graph. However, if the control-flow graph is incomplete
-- (ie. if it contains an unresolved dynamic branch), the bounds may
-- _not_ include all executions of the complete flow-graph. Therefore,
-- when a dynamic branch is resolved and the control-flow graph is
-- extended all bounds computed for the incomplete flow-graph should be
-- discarded. 
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
-- $Revision: 1.11 $
-- $Date: 2015/10/24 19:36:47 $
--
-- $Log: bounds.ads,v $
-- Revision 1.11  2015/10/24 19:36:47  niklas
-- Moved to free licence.
--
-- Revision 1.10  2008-02-27 14:58:48  niklas
-- BT-CH-0116: Call-specific time and stack assertions.
--
-- Revision 1.9  2006/10/24 21:41:05  niklas
-- BT-CH-0030.
--
-- Revision 1.8  2006/05/27 21:56:08  niklas
-- Updated for BT-CH-0020.
--
-- Revision 1.7  2005/02/16 21:11:40  niklas
-- BT-CH-0002.
--
-- Revision 1.6  2003/03/11 08:31:06  holsti
-- Using execution-bounds types from Programs.Execution.
--
-- Revision 1.5  2001/03/10 00:48:49  holsti
-- Unused with-clauses deleted.
--
-- Revision 1.4  2000/08/04 08:16:25  saarinen
-- Changed program to be in/out parameter for Bound_Jumps_And_Execution.
--
-- Revision 1.3  2000/07/25 03:14:01  holsti
-- First implementation (incomplete).
--
-- Revision 1.2  2000/07/04 12:07:07  holsti
-- Real Ada text. Added Loop_Bounds_T.
--


with Assertions;
with Programs;
with Programs.Execution;
with Storage;
with Storage.Bounds;


package Bounds is


   Recursion : exception;
   --
   -- Raised when a recursive cycle between target subprograms is
   -- detected.


   function Fully_Bounded (Item : Programs.Execution.Bounds_Ref)
   return Boolean;
   --
   -- Whether these bounds are complete with respect to the
   -- aspects (time, space) we are analysing.


   procedure Bound_Execution (
      Exec_Bounds : in     Programs.Execution.Bounds_Ref;
      Params      : in     Storage.Bounds.Cell_Interval_List_T;
      Inherit_Inv : in     Storage.Cell_Set_T;
      Asserts     : in     Assertions.Assertion_Set_T;
      Bounds_Set  : in     Programs.Execution.Bounds_Set_T;
      Flow_Frozen :    out Boolean);
   --
   -- Tries to bound the execution time and/or stack usage within
   -- the given Exec_Bounds, by analysis of the subprogram under
   -- these bounds and of its callees, which are recursively analysed
   -- if not yet bounded.
   --
   -- Input parameters:
   --
   -- Exec_Bounds
   --    The execution bounds created for the subprogram to be
   --    analysed, in the context to be analysed. The subprogram
   --    and the context (call-path) are defined by Exec_Bounds.
   --
   --    Some quantitative bounds may already be present, for example
   --    an asserted execution time or an asserted stack usage. Our
   --    task is then to analyse and bound only the as yet unbounded
   --    dimensions of the execution.
   --
   --    The context may be null (universal bounds sought). If the
   --    context is not null, the Exec_Bounds refer to the bounds
   --    of the caller, that constitute the context for this analysis
   --    of the callee subprogram.
   --
   --    We assume that these Exec_Bounds are not yet stored in the
   --    Bounds_Set, thus they are not yet frozen and can be extended
   --    with more bounds on eg. the execution time.
   --
   -- Params
   --    Bounds on the input cells for this call, expressed in the
   --    subprogram's (the callee's) frame. The bounds may be asserted
   --    or (if the call-path is non-empty) derived from the actual
   --    parameters or actual data context at any call in the path.
   --
   -- Inherit_Inv
   --    The inherited set of invariant cells. Usage TBA.
   --
   -- Asserts
   --    A set of assertions to constrain the execution of the
   --    Subprogram and its callees.
   -- 
   -- Bounds_Set
   --    A set of execution bounds computed so far.
   --
   -- Algorithm:
   --
   -- This procedure uses various forms of static analysis to bound
   -- the dynamic execution of the given Subprogram with regard to
   -- the following dynamic aspects:
   --
   -- > Dynamic (indirect, computed) control flow, as represented by
   --   elements of type Flow.Boundable_Edge_T (Dynamic_Edge_T),
   --   with Role = Boundable_Jump, in the control-flow graph.
   --   If a control-flow graph has such elements then the flow-graph
   --   is incomplete.
   --
   -- > Dynamic (indirect, computed) calls, as represented by
   --   elements of type Flow.Boundable_Edge_T (Dynamic_Edge_T),
   --   with Role = Boundable_Call, in the control-flow graph.
   --   If a control-flow graph has such elements then the flow-graph
   --   is incomplete (missing call-steps) as is the call-graph
   --   rooted at this subprogram.
   --
   -- > Dynamic (indirect, computed) data variable references, as
   --   represented by Arithmetic.Reference elements in the arithmetic
   --   effects attached to flow-graph steps.
   --
   -- > Loop iteration bounds.
   --
   -- > Parameter-passing mappings at call sites where this mapping
   --   depends on dynamic aspects, as represented by dynamic forms
   --   of Calling.Protocol_T attached to calls in the flow-graph.
   --
   -- > Maximum local stack height, as determined by the maximum value
   --   assigned to the local stack-height cell at any point in the
   --   flow-graph.
   --
   -- > Maximum total stack usage, as determined by the largest of the
   --   maximum value of the local stack height and the maximum, over
   --   all calls to other subprograms, of the local stack height at
   --   the call (a.k.a. the call's take-off height) and the total stack
   --   usage of the callee.
   --
   -- The static analysis methods used include:
   --
   -- > Constant propagation.
   --
   -- > Value-origin analysis.
   --
   -- > Arithmetic analysis using Presburger Arithmetic.
   --
   -- We first apply the assertions from the Asserts set, then constant
   -- propagation and value-origin analysis if needed, and finally
   -- Presburger analysis if some dynamic aspects are still not bounded
   -- and the flow-graph is reducible.
   --
   -- If the subprogram has no unresolved dynamic control flow, its flow-
   -- and call-analysis are complete and the execution bounds computed
   -- here are valid (but possibly too loose; context-dependent bounds
   -- may be added later, as callers are analysed).
   --
   -- If some dynamic control flow was resolved, new edges were added to
   -- the control-flow graph and/or call-graph, so the flow-graph must
   -- be extended by repeating the control-flow tracing process.
   -- The bounds computed here are not valid for the extended flow-graph,
   -- so new bounds must be computed.
   --
   -- Output parameters:
   --
   -- Exec_Bounds
   --    Updated with the computed bounds.
   --    If the analysis has extended the flow-graph, these bounds
   --    are now of historic interest only. They apply to the
   --    incomplete flow-graph before the extensions, and have no
   --    significance for the extended flow-graph (TBC).
   --
   -- Asserts
   --    Updated with usage information.
   --
   -- Bounds_Set
   --    Updated with the computed bounds (unless the control-flow
   --    graph was extended).
   --
   -- Flow_Frozen
   --    Whether the control-flow graph of the Subprogram is now
   --    completed, that is, dynamic control-flow has been resolved
   --    as far as possible and no dynamic edges remain in the graph.


   procedure Bound_Executions (
      To_Bound   : in out Programs.Subprogram_Set_T;
      Asserts    : in     Assertions.Assertion_Set_T;
      Bounds_Set : in out Programs.Execution.Bounds_Set_T;
      Growing    :    out Programs.Subprogram_Set_T);
   --
   -- Analyses a set of subprograms to bound the dynamic aspects of
   -- their executions.
   --
   -- Input parameters:
   --
   -- To_Bound
   --    A set of subprograms provided with control-flow graphs and
   --    call graphs, such that for any call from a subprogram in
   --    the To_Bound set either the callee is also in To_Bound or
   --    the callee has already been bounded and has bounds in the
   --    Bound_Set.
   --
   -- Asserts
   --    A set of assertions that constrain the executions of the
   --    Program and the subprograms.
   --
   -- Bounds_Set
   --    A set of execution bounds computed so far.
   --
   -- Algorithm:
   --
   -- This procedure uses Bound_Execution to bound the dynamic execution
   -- of the given subprograms.
   --
   -- The procedure works bottom-up in the call-graph of the subprograms
   -- in the To_Bound set, calling Bound_Execution (Subprogram, ...)
   -- for each subprogram.
   --
   -- The procedure stops at the first subprogram (in the bottom-up
   -- call order) for which dynamic control-flow was resolved and the
   -- control-flow graph is thus still growing. The subprogram is placed
   -- in the Growing set. For this subprogram, flow-analysis (flow tracing)
   -- should be continued in order to complete the control-flow graph or,
   -- perhaps, discover new dynamic control-flow to be resolved again.
   --
   -- The procedure propagates the Recursion exception if recursion is
   -- detected between the subprograms to be bounded.
   --
   -- Output parameters:
   --
   -- Control-flow graphs
   --    For the first (if any) subprogram, in bottom-up call order, for
   --    which dynamic control-flow was resolved, the control flow-graph
   --    is extended with new edges (resolved from the dynamic branches).
   --    The new edge may be "bound" (target step exists) or "loose"
   --    (target step does not yet exist, only target step-address is
   --    specified).
   --
   -- To_Bound
   --    The subset of subprograms, from the initial To_Bound set, for
   --    which bounds on the dynamic execution were not yet computed.
   --    May or may not include the Resolved set.
   --
   -- Program
   --    TBD?
   --
   -- Asserts
   --    Updated with usage information (TBC).
   --
   -- Bounds_Set
   --    Execution bounds are added for those subprograms To_Bound that
   --    had complete control-flow graphs on entry (ie. no unresolved
   --    dynamic jumps), or that had dynamic control-flow but for which
   --    the dynamic control-flow was resolved away (no new edges, no
   --    dynamism left). However, see above for when the algorithm stops;
   --    later subprograms (in bottom-up call order) are not processed
   --    and remain in To_Bound.
   --
   -- Growing
   --    Returns the subset of subprograms (from To_Bound) for which
   --    dynamic control flow was resolved, which means that their
   --    control-flow graphs are still growing and can be traced further,
   --    after which these subprograms can again be analysed for dynamic
   --    bounds.


private


   procedure Finish_Bounds (
      Exec_Bounds : in     Programs.Execution.Bounds_Ref;
      Assert_Map  : in     Assertions.Assertion_Map_T;
      Bounds_Set  : in     Programs.Execution.Bounds_Set_T;
      Flow_Frozen :    out Boolean);
   --
   -- Finishing touches for the new Exec_Bounds on a subprogram.
   -- The main thing is to check if the dynamic control-flow is
   -- sufficiently resolved to declare the Flow_Frozen, and if so
   -- to let the Decoder Finish the flow-graph if the bounds
   -- are universal and to store the bounds in the Bounds_Set.
   --
   -- Unresolved dynamic flow is reported as Errors.
   -- Unresolved dynamic data is reported as Warnings (optional).
   --
   -- The Assert_Map is provided for Decoder.Finish. If the subprogram
   -- is infeasible, the Assert_Map may be null (uninitialized), but
   -- then Decoder.Finish is not called.


end Bounds;
