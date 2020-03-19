-- Flow.Life (decl)
--
-- Cell-liveness analysis in a flow-graph containing uses and
-- definitions of cells.
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
-- $Revision: 1.19 $
-- $Date: 2015/10/24 19:36:49 $
--
-- $Log: flow-life.ads,v $
-- Revision 1.19  2015/10/24 19:36:49  niklas
-- Moved to free licence.
--
-- Revision 1.18  2013-02-12 08:47:19  niklas
-- BT-CH-0245: Global volatile cells and "volatile" assertions.
--
-- Revision 1.17  2009-10-07 19:26:10  niklas
-- BT-CH-0183: Cell-sets are a tagged-type class.
--
-- Revision 1.16  2008-04-22 12:40:08  niklas
-- Added function Program, for convenience.
--
-- Revision 1.15  2007/12/17 13:54:36  niklas
-- BT-CH-0098: Assertions on stack usage and final stack height, etc.
--
-- Revision 1.14  2007/10/31 12:16:01  niklas
-- BT-CH-0095: Arithmetic analysis of "live" dynamic data refs.
--
-- Revision 1.13  2007/10/28 09:32:46  niklas
-- BT-CH-0092: Arithmetic analysis of dynamic data refs is optional.
--
-- Revision 1.12  2007/10/03 07:26:34  niklas
-- BT-CH-0081: Fix: Size of joint effect from Flow.Life.Join_Effects.
-- BT-CH-0081: Fix: Use given computation model in Local_Demand.
--
-- Revision 1.11  2007/04/18 18:34:38  niklas
-- BT-CH-0057.
--
-- Revision 1.10  2007/03/29 15:18:02  niklas
-- BT-CH-0056.
--
-- Revision 1.9  2007/02/13 20:18:17  Niklas
-- BT-CH-0044.
--
-- Revision 1.8  2005/10/20 19:34:01  niklas
-- BT-CH-0016.
--
-- Revision 1.7  2005/09/12 19:03:00  niklas
-- BT-CH-0008.
--
-- Revision 1.6  2005/02/23 09:05:19  niklas
-- BT-CH-0005.
--
-- Revision 1.5  2005/02/16 21:11:45  niklas
-- BT-CH-0002.
--
-- Revision 1.4  2004/04/27 20:05:51  niklas
-- First Tidorum version.
-- Take Cell_T stuff from Storage instead of Arithmetic.
-- Support Range constraint assignments. The target of a Range assignment
-- depends on all cells used in the range constraint expressions. A Range
-- assignment is live if the constrained target cell is live. Live_Effect
-- deletes dead Range assignments from its result.
-- Allow options For_Time and For_Space in Find_Live_Cells.
-- Take into account feasibility of flow-graph edges.
-- Collect live-assignment map on the fly during iteration.
--
-- Revision 1.3  2003/03/11 08:23:57  holsti
-- Split execution-bounds stuff from the Programs package to
-- make the child package Programs.Execution.
--
-- Revision 1.2  2001/03/10 00:48:08  holsti
-- Computes also input, output and relevant cells.
--
-- Revision 1.1  2001/01/07 21:54:21  holsti
-- First version.
--

--:dbpool with GNAT.Debug_Pools;


with Arithmetic.Opt;
with Assertions;
with Flow.Computation;
with Programs;
with Programs.Execution;
with Storage;


package Flow.Life is
--
--
-- This package provides operations that determine the "relevance"
-- and the "liveness" of data-cells in a flow-graph computation,
-- and simplify the computation model to omit irrelevant and "dead"
-- assignments.
--
--    RELEVANT CELLS
--
-- The cells that are "relevant" for the bounding of a subprogram are:
--
-- > The cells used by edge conditions.
--
-- > The cells on which any unstable dynamic edges (jumps) depend, that
--   is, the basis cells of boundable edges not yet stably resolved or
--   stably unresolved.
--
-- > Optionally, the cells on which any unresolved dynamic data
--   references depend, that is, their basis cells.
--
--   TBA: include basis cells for dynamic memory references only if the
--   assignment in which the reference appears has a target that is a
--   relevant cell or may alias with a relevant cell.
--
-- > The cells on which any unresolved dynamic calling protocol depends,
--   that is, the basis cells of the protocol.
--
-- > The input cells for all unbounded calls, if the cell is assigned
--   in the subprogram or is an output cell of some call (the same
--   call or another call). (These cells are actually needed only after
--   all dynamism is resolved, but that refinenement is not applied yet.)
--   TBA aliasing conditions here.
--
-- > All stack-height cells, if stack analysis is chosen and if the
--   total stack usage is not already bounded (by an assertion, for
--   example. (These cells may be relevant also as a basis cell for a
--   calling protocol or other boundable object, but that is decided
--   separately.)
--
-- > The stack-height cells of unstable stacks, after all return steps,
--   if the final height of the stack is not already bounded to a single
--   value (and whether or not stack analysis is chosen, because the
--   final height is important for all data-flow analysis in the callers).
--
-- > All cells on which these cells depend, transitively.
--
-- > A cell T depends directly on a cell C (at a given point in the
--   flow-graph) if the value of T (at this point) comes from (or may
--   come from) a Defining assignment where C appears in an expression,
--   or if the value of T (at this point) is (or may be) constrained
--   by a Range assignment where C appears in an expression.
--   TBA aliasing conditions here.
--   Note that "control dependence" is not included here because
--   all cells used by edge conditions are considered relevant.
--
-- > Volatile cells are ignored in this analysis. A volatile cell
--   is never a relevant cell. However, the value resulting from
--   an expression that depends on volatile cells, as well as non-
--   volatile cells, can be relevant, and can make its non-volatile
--   operand cells relevant. This is because the value of a volatile
--   cell can be bounded by assertions, and therefore the expression
--   can truly depend on the values of its non-volatile operands.
--
-- Those input cells for unbounded calls that are not assigned in
-- the subprogram, nor output from other calls, are of course
-- required to finish the bounding of the calls. However, arithmetic
-- analysis of these cells in this subprogram is not needed. Other,
-- cheaper means can transport bounds on the values of these cells
-- from higher levels to the analysis of the calls.
--
-- Note that these cells may depend on the effects of calls in the
-- subprogram. Thus, the output cells of some called lower-level
-- subprogram may be included, if they influence edge conditions or
-- the inputs to lower-level calls, directly or indirectly.
--
-- Note also that the output cells of the subprogram itself are
-- not specifically included. Of course, some output cells may be
-- relevant for other reasons, for example because they are used in
-- an edge condition.
--
-- Since the set of relevant cells depends on which calls are bounded,
-- it depends on the call-path, and must be recomputed for each
-- arithmetic analysis.
--
--    LIVE CELLS
--
-- A relevant cell is "live" at some point in the flow-graph, if
-- there is a path from this point to a use of the cell, such that
-- the path does not contain a (new) definition of the cell.
--
-- Each flow-graph step "uses" some cells, and "defines" some cells.
-- If the same cell is both used and defined, the "use" is held to
-- precede the "define".
--
-- The cells "used" by a step are those relevant cells that also
-- fulfil one of the following conditions:
--
-- > The cell is used in a live assignment in the effect of the
--   step. The liveness notion for assignments is defined below;
--   it is mutually recursive with the liveness notion for cells.
--
-- > The cell is a basis cell for an unresolved dynamic boundable
--   object. The boundable objects are dynamic edges, dynamic memory
--   references in the arithmetic effect (optional), and dynamic
--   calling protocols.
--
-- > The cell is a relevant input for an unbounded call step.
--
-- > The cell is a stack-height cell and stack analysis is chosen.
--
-- Cell liveness propagates in the flow-graph as follows:
--
-- > If a cell is used by the step, it is always live before the
--   step, whether or not the step defines it, and whether or
--   not it is live after the step.
--
-- > If the step defines a cell, but does not use it, the cell is
--   dead (not live) before the step.
--
-- > A cell that is live after the step, and is not defined in
--   the step, is also live before the step.
--
-- > A cell that is used in the condition of an edge is live after
--   the source-step of the edge.
--
-- Cell liveness relates to assignment liveness as follows:
--
-- > A Defining assignment to a cell is "live" if the target-cell
--   is live after the assigning step.
--
-- > A Range_Pre assignment (constraint) on a cell is "live" if the
--   target-cell is live before the step.
--
-- > A Range_Rel or Range_Post assignment (constraint) on a cell is
--   "live" if the target-cell is live after the step.
--
-- > If a Range_Post assignment is live, then all the cells used
--   in the Min and Max expressions are live after the step that
--   contains the Range_Post assignment.
--
-- Since irrelevant cells are never live, an assignment to an
-- irrelevant cell is always "dead".
--
-- It is now evident that the set of relevant cells is exactly
-- the set of cells that are live at some point in the flow-graph.
--
-- Volatile cells can be live and/or relevant, since their values
-- can be important for the computation. However, they are never
-- input cells, since their assigned values on entry to the subprogram
-- do not determine the values read form the cells later.
--
-- Since assignments to volatile cells cannot be used to predict
-- the values later read from the cells, they are never live, which
-- means that a volatile cell appears to be unchanged from its
-- input value (value on entry to the subprogram), which is not
-- generally true, of course. But, since a volatile cell can have a
-- bounded value only through assertions, the only problem that can
-- occur is with assertions that define non-intersecting (contradictory)
-- bounds at different points in the subprogram, since no single value
-- can satisfy those bounds. This not a problem for liveness analysis,
-- because this analysis cannot detect such contradictions. Other
-- analyses applied to the live computation must ward against such
-- contradictions.
--
-- Since an assignment to a volatile cell is dead, it does not make
-- any of its operand cells live either.
--
-- The main function of this package is to detect which cells are
-- relevant and which assignments are live, so that the rest of the
-- cells and assignments can be ignored (or simplified) by the
-- arithmetic analysis.


   type Living_T is private;
   --
   -- A data structure that shows which assignments in a computation
   -- model are "live" in the sense that the target-cell is live after
   -- the assigning step.
   --
   -- The liveness analysis also labels each cell that occurs in the
   -- computation model as "live" or "dead". Any cell that occurs in
   -- a "live" assignment, whether in the target or in the assigned
   -- expression, is a "live" cell.
   --
   -- Moreover, the liveness analysis also finds all the "input" and
   -- "output" cells of the computation model. The input cells are
   -- those "live" cells that have relevant values on entry to the
   -- subprogram. The output cells are simply all cells that are
   -- assigned in the computation in any assignment (live or dead)
   --
   -- Volatile cells are never "input" or "output" cells, but can
   -- occur as operands (but not targets) in live assignments.
   --
   -- The initial value of a Living_T object is undefined, and must
   -- be constructed by Find_Live_Cells before it is used.
   --
   -- A Living_T object has the following (hidden) attributes:
   --
   -- > A handle (Flow.Computation.Model_Handle_T) to the computation
   --   model on which this object is based.
   --
   -- > A table that shows which assignments are "live" in the effect
   --   of each step in the underlying flow-graph.
   --
   -- > The total number of "live" cells, called the "basis cells".
   --
   -- > The sets of input, output, and basis cells.
   --
   -- Living_T has reference semantics. A value of type Living_T
   -- should be thought of as a reference to an underlying object.
   -- Since Living_T is not "limited", it can be assigned freely and
   -- several references to the underlying object can exist. These
   -- are not counted or tracked in any way, so the client must take
   -- care not to use dangling references.


   function Model (Item : Living_T)
   return Flow.Computation.Model_Handle_T;
   --
   -- The computation model to which the given Living object applies.
   -- The result is meant only for temporary use eg. in a parameter
   -- association.


   function Graph (Item : Living_T) return Graph_T;
   --
   -- The flow-graph underlying the given Living object.


   function Program (Item : Living_T) return Programs.Program_T;
   --
   -- The program under analysis.


   function Inputs (Item : Living_T) return Storage.Cell_Set_T;
   --
   -- The set of input cells: those "live" cells that have relevant
   -- values on entry to the subprogram. That is, the subprogram uses
   -- the given (input) values of these cells in some important way.
   --
   -- The cells are relative to the subrogram's own frame.


   function Basis (Item : Living_T) return Storage.Cell_Set_T;
   --
   -- The set of "live" cells, that is, the cells that are relevant
   -- at some point in the computation.
   --
   -- The cells are relative to the subrogram's own frame.


   function Outputs (Item : Living_T) return Storage.Cell_Set_T;
   --
   -- The set of output cells, that is, all cells that are assigned
   -- somewhere in the computation (by any kind of feasible assignment,
   -- live or dead). Includes cells assigned by callees (if visible to
   -- this subprogram, the caller). Does not include cells that may
   -- be referenced by unresolved dynamic memory references.
   --
   -- The cells are relative to the subrogram's own frame.


   function Live_Computation (
      Model     : Computation.Model_Handle_T;
      Calls     : Programs.Execution.Call_Bounds_List_T;
      Heights   : Storage.Cell_List_T;
      Finals    : Storage.Cell_List_T;
      Asserts   : Assertions.Assertion_Set_T;
      For_Time  : Boolean;
      For_Space : Boolean;
      For_Data  : Arithmetic.Opt.Ref_Choice_T)
   return Living_T;
   --
   -- Propagates relevance and liveness in a control-flow graph, based
   -- on an original computation Model, and returns a Living_T object
   -- that defines the live assignments and the set of input, output
   -- and live cells.
   --
   -- Model
   --    The given computation model that may contain irrelevant cells
   --    and dead (irrelevant) assignments.
   --    The effect of any calls from this subprogram is assumed to be
   --    represented in the Model effect of the call-steps.
   --    Only feasible steps and feasible edges propagate relevance
   --    and liveness.
   -- Calls
   --    The calls from this subprogram to lower-level subprograms and
   --    bounds on these calls, as far as is currently known. The input
   --    cells for a call are relevant only if the call is not yet
   --    bounded.
   -- Heights
   --    The stack-height cells of those stacks for which the total
   --    stack usage it not yet bounded. These cells are therefore
   --    relevant everywhere, for computing an upper bound on the
   --    local stack height as well as the take-off heights for calls.
   -- Finals
   --    The stack-height cells of those (unstable) stacks for which
   --    the final stack-height is not yet known (fully). These cells
   --    are therefore relevant at every return point.
   -- Asserts
   --    Assertions on the program under analysis.
   -- For_Time
   -- For_Space
   --    Options for time and/or space analysis.
   --    For_Space should be False if Processor.Stacked is False.
   -- For_Data
   --    Whether to include the basis cells of unresolved dynamic
   --    data references as "demanded" (relevant) cells, depending
   --    on where the reference occurs. There are three choices:
   --    include basis cells for no dynamic data references; or for
   --    references that occur in relevant expressions; or for all
   --    references.
   --
   -- The function returns an object that extends the given computation
   -- Model with information about "liveness" of assignments. This
   -- object represents a new computation model that is a subset of
   -- the given Model in the sense that the effect of a step, under
   -- the new model, contains a subset of the assignments in the effect
   -- of the step under the original Model, as described in connection
   -- with the function Live_Effect below. All assignments in infeasible
   -- steps are dead.
   --
   -- This operation can be applied to any subprogram with a flow-graph
   -- that has no loose edges. It does not need the node-level graph
   -- (the basic-block structure). The flow-graph may contain dynamic
   -- edges; if so, their basis cells are considered "relevant", but the
   -- returned model does not apply to any extended flow-graph in which
   -- these dynamic edges are resolved.


   function Is_Live (
      Item   : Positive;
      Effect : Arithmetic.Effect_T;
      Step   : Step_T;
      Living : Living_T)
   return Boolean;
   --
   -- Whether the assignment Effect(Item) is "live" under the given
   -- Living set, when Effect is the (full) effect of the Step under
   -- the computation model on which the Living set is based.


   function Basis_Size (Living : Living_T) return Natural;
   --
   -- The total number of "live" cells. Same as Card (Basis) where
   -- Basis is the Basis set from Propagate.


   function Live_Assignments (
      Step   : Step_Index_T;
      Living : Living_T)
   return Arithmetic.Assignment_List_T;
   --
   -- The "live" assignments of the step with the given index, which is
   -- its original effect under the computation model on which the Living
   -- object is based, but leaving out all dead assignments of all kinds.
   -- Note the difference to the function Live_Effect, below: here, the
   -- dead assignments are just left out, not converted to assignments
   -- of opaque values.


   function Dead_Definitions (
      Step   : Step_Index_T;
      Living : Living_T)
   return Arithmetic.Assignment_List_T;
   --
   -- The "dead" Defining assignments of the step with the given index.


   function Live_Effect (
      Step   : Step_T;
      Living : Living_T)
   return Arithmetic.Effect_T;
   --
   -- The "live" effect of the given step, which is its original effect
   -- under the computation model on which the Living object is based,
   -- but with all dead Defining assignments modified to assign an opaque
   -- (unconstrained) value to the target cell, and all dead Range constraint
   -- assignments deleted.


   function Max_Joint_Effect_Length (
      Steps  : Step_List_T;
      Living : Living_T)
   return Natural;
   --
   -- An upper bound on the number of assignments in the joined effect
   -- of the Steps, using the Living assignments in these Steps and
   -- the procedure Join_Effects, below.


   procedure Join_Effects (
      Steps  : in     Step_List_T;
      Living : in     Living_T;
      Basis  : in     Storage.Cell_Set_T;
      Into   :    out Arithmetic.Assignment_Set_T;
      Last   :    out Natural);
   --
   -- Sequentially joins the "live" effects of the Steps Into a
   -- single set of assignments, starting from Steps'First and
   -- continuing as far as possible, which means up to and including
   -- Steps'Last or the Last step that can be included in the same
   -- assignment set without creating a double def or a def-use
   -- dependency within the set.
   --
   -- Only assignments with targets in the Basis set are included
   -- in the joint effect.
   --
   -- Steps
   --    The steps for which the effects will be combined.
   --    The combined effect only makes sense if these Steps are
   --    a linear chain in a flow-graph, or in other words a
   --    subsequence of a basic block (a Node).
   --    Precondition: Steps'Length > 0.
   -- Living
   --    Defines the "live" assignments.
   -- Basis
   --    Defines the interesting target cells. Assignments to
   --    other cells are not joined to the Into set, even if
   --    the assignments are classed as "live".
   -- Into
   --    Returns the joint set of assignments. The maximum size of
   --    this set (maximum number of assignments) can be bounded
   --    with the function Max_Joint_Effect_Length, above, but
   --    the actual size is usually less.
   -- Last
   --    The index of the last step to be joined. The assignments
   --    in the Into set model the joint effect of sequentially
   --    executing Steps(Steps'First .. Last).
   --    Last is always in Steps'Range.
   --
   -- The algorithm simply adds the "live" assignments for the Steps,
   -- sequentially from Steps'First, Into the assignment set, up to and
   -- including Steps'Last or up to but *not* including the first step
   -- that cannot be so joined because it has a live assignment to a
   -- Basis cell with one or both of the following properties:
   --
   -- > The assignment is a Defining one and the target is a cell
   --   that is already defined in the Into set.
   --
   -- > The target reference (if not a cell), the condition (if any) or
   --   the assigned value(s) uses a cell that is already defined in
   --   the Into set.
   --
   -- Here a cell is "defined" in the Into set if the set contains
   -- a Defining assignment with this cell as the target.
   --
   -- Finally, for each Basis target cell of a "dead" Defining assignment
   -- in the list of joined steps (Steps'First .. Last), we add Into
   -- the set a Defining assignment of an opaque value to this cell,
   -- to show that the steps do change the value of this cell,
   -- although the value is "dead" in these steps.


   procedure Add_Cells_Accessed (
      By     : in     Node_T;
      Living : in     Living_T;
      To     : in out Storage.Cell_Set_T);
   --
   -- Adds all the cells accessed (used or defined) By the computations
   -- in a given node, using only the Living assignments, To a set of
   -- cells. The basis cells for all dynamic edges leaving this node
   -- are included. No cells used in the preconditions of edges are
   -- included. (It is more or less assumed that edges _within_ a node
   -- have the precondition Arithmetic.Always because these edges must
   -- be unconditional transfers of control.)


   function Steps_With_Dynamic_Effect (Living : Living_T)
   return Step_List_T;
   --
   -- All the feasible flow-graph steps where the effect of the step
   -- contains some Living assignments where the assigned value
   -- expressions or the assignment condition contain some elements
   -- of the class Storage.References.Boundable_T.


   procedure Discard (Living : in out Living_T);
   --
   -- Discards the Living information, releasing memory.
   -- The associated cell-sets (inputs, basis, outputs) are not
   -- explicitly discarded at this time, so other references to
   -- these cell-sets remain valid.


private

   type Living_Object_T;

   type Living_T is access Living_Object_T;

   --:dbpool Living_Pool : GNAT.Debug_Pools.Debug_Pool;

   --:dbpool for Living_T'Storage_Pool use Living_Pool;

end Flow.Life;
