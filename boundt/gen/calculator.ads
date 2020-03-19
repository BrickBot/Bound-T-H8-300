-- Calculator (decl)
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
-- $Revision: 1.43 $
-- $Date: 2015/10/24 19:36:47 $
--
-- $Log: calculator.ads,v $
-- Revision 1.43  2015/10/24 19:36:47  niklas
-- Moved to free licence.
--
-- Revision 1.42  2013/12/22 20:14:56  niklas
-- Value enumerators obey Storage.Bounds.Opt.Max_Listed_Values and raise
-- Unbounded_Set or Storage.Bounds.Unbounded if more values are offered;
-- they do not return a truncated list.
--
-- Revision 1.41  2009-11-27 11:28:07  niklas
-- BT-CH-0184: Bit-widths, Word_T, failed modular analysis.
--
-- Revision 1.40  2009-10-07 19:26:10  niklas
-- BT-CH-0183: Cell-sets are a tagged-type class.
--
-- Revision 1.39  2009-01-18 08:03:18  niklas
-- Moved context clause to body.
--
-- Revision 1.38  2008/06/20 10:11:53  niklas
-- BT-CH-0132: Data pointers using Difference (Expr, Cell, Bounds).
--
-- Revision 1.37  2008/06/18 20:52:55  niklas
-- BT-CH-0130: Data pointers relative to initial-value cells.
--
-- Revision 1.36  2008/05/03 09:22:06  niklas
-- BT-CH-0126: Combining joint-counter and each-counter analysis.
--
-- Revision 1.35  2008/04/28 08:40:11  niklas
-- BT-CH-0125: Fix loop-until-equal analysis.
--
-- Revision 1.34  2008/04/26 19:19:44  niklas
-- BT-CH-0124: Joint loop counters and induction variables.
--
-- Revision 1.33  2008/04/22 12:51:27  niklas
-- Extended Pool_To_Steps to optionally find edges that have a null
-- (empty) data pool, and to mark those edges infeasible. Added the
-- nested procedures Mark_Infeasible and Check_Null_Flow.
--
-- Revision 1.32  2007/08/27 16:22:08  niklas
-- Added nicer Image for Pool_T.
--
-- Revision 1.31  2006/04/28 09:46:14  niklas
-- Added functions Values (Expr, Pool or Flux) to override the
-- inherited functions Values (Expr, Arithmetic.Bounds_T).
--
-- Revision 1.30  2005/10/20 19:34:01  niklas
-- BT-CH-0016.
--
-- Revision 1.29  2005/10/20 11:28:30  niklas
-- BT-CH-0015.
--
-- Revision 1.28  2005/09/17 14:42:06  niklas
-- BT-CH-0009.
--
-- Revision 1.27  2005/09/16 14:19:11  niklas
-- Added Calc_Image functions for Pool_T and Flux_T, for easier
-- cross-referencing to Omega files. Note that Flux_T already
-- inherits an Image function from Arithmetic.Bounds_T but this has
-- a different purpose.
--
-- Revision 1.26  2005/07/01 10:58:02  niklas
-- Changed the Interval functions for Flux_T to propagate
-- Flow.False_Path instead of Empty_Flux, since these functions
-- may be called through dynamic dispatch in contexts that are not
-- aware of the actual (derived) type of the bounds.
--
-- Revision 1.25  2005/02/16 21:11:41  niklas
-- BT-CH-0002.
--
-- Revision 1.24  2004/05/01 20:23:01  niklas
-- First Tidorum version.
-- Taking Cell_T stuff from Storage, not from Arithmetic. Removed "use" for
-- Arithmetic, added some "use type" for Storage types.
-- Added variant of Range_Bounded_Flux that takes the bounds from Range_Pre
-- assignment constraints.
-- Changed Constrain_Range and Restrict_Domain_Of_Cells to use the new
-- functions Adapted and Warp and the modified Apply function from
-- Calculator.Formulas, for simpler code and calculator expressions.
-- Corrected Propagate.Handle_Loop_Head to make variant cells unknown.
-- Changed Bounds_After_Step to use the new function Transformation from
-- Calculator.Formulas.
--
-- Revision 1.23  2001/01/19 08:50:20  saarinen
-- NC_041 Special handling of loop-exit edges.
-- NC_084 Flux_To_Steps review issues.
--
-- Revision 1.22  2001/01/13 11:10:37  holsti
-- Removed Limits_From_Flux in favour of Bounds_From_Flux (with
-- no "Inv" parameter, which was unused anyway).
-- Renamed Limits_From_Complement to Bounds_From_Complement (and
-- removed the unused "Inv" parameter).
-- Removed to-be's in favour of NC's.
--
-- Revision 1.21  2001/01/07 22:05:12  holsti
-- Comment parameter added to Start.
-- Comments on calculation steps added.
-- Parameters for live-cell analysis added.
-- Owner_Of operations added to support commenting.
--
-- Revision 1.20  2000/12/28 13:29:48  saarinen
-- Added function Limits_From_Complement.
--
-- Revision 1.19  2000/11/24 12:06:01  sihvo
-- Added stack height analysis.
--
-- Revision 1.18  2000/11/23 12:46:58  saarinen
-- Deleted Flux_To_Steps, Flux_Of_Region, Flux_To_Nodes.
-- Fixed joining of fluxes with different bases (NC_029).
--
-- Revision 1.17  2000/11/06 11:52:28  saarinen
-- Added exception Empty_Flux.
--
-- Revision 1.16  2000/10/06 14:02:09  saarinen
-- Added function Restrict_Domain_Of_Cells, and
-- commented out unused functions Flux_To_Steps and Flux_To_Nodes.
--
-- Revision 1.15  2000/09/20 18:58:56  saarinen
-- Modified Calc_Handle_T.
-- Fixed some problems with different Cell_Sets in some procedures.
-- Changed Flux_To_Step not to use transitive closure.
--
-- Revision 1.14  2000/08/17 13:00:54  holsti
-- Bounds_From_Flux for expression added.
--
-- Revision 1.13  2000/07/24 22:42:37  holsti
-- Flux_To_Steps over acyclic region added.
--
-- Revision 1.12  2000/07/17 21:00:11  holsti
-- Cell_Set_T instead of Cell_List_T for domains and ranges.
--
-- Revision 1.11  2000/07/16 18:42:33  holsti
-- Flux_To_Nodes uses loop-repeat fluxes as designed.
--
-- Revision 1.10  2000/07/16 13:10:56  holsti
-- Cells_Of (Flux) added.
--
-- Revision 1.9  2000/07/14 20:35:51  holsti
-- Using Arithmetic.Cell_List_Ref instead of own.
--
-- Revision 1.8  2000/07/12 20:46:01  holsti
-- Data-flow in regions uses only region edges.
--
-- Revision 1.7  2000/07/12 12:26:15  holsti
-- Calculator.Formulas added and used.
--
-- Revision 1.6  2000/06/16 10:42:03  saarinen
-- Added function Flux_To_Steps and removed Function Cells_In_Graph
--
-- Revision 1.5  2000/06/11 19:03:14  holsti
-- Arithmetic package separated from Calculator.
--
-- Revision 1.4  2000/06/02 08:38:31  saarinen
-- Added many local procedures and implemented some of the visible ones
--
-- Revision 1.3  2000/05/18 10:27:02  saarinen
-- Added expression and cell_set types
--
-- Revision 1.2  2000/05/12 11:06:37  saarinen
-- first implementations
--


with Arithmetic;
with Exec_Cmd;
with Flow;
with Flow.Life;
with Loops;
with Storage;
with Storage.Bounds;
with Storage.List_Cell_Sets;


package Calculator is
   --
   -- Data-flow analysis through calculation of data-flow relations,
   -- invariants and data ranges.
   --
   -- Attempts to hide the choice of the calculation engine (e.g.
   -- whether the Omega Calculator or the Omega Library is used, or
   -- some quite different mechanism).
   --


   --
   ---   Calculator instances
   --


   type Calc_Handle_T is private;
   --
   -- An access to an instance of the calculation engine,
   -- able to compute fluxes and other things.
   --
   -- In the present implementation, a calculator handle
   -- refers to a child process running the Omega Calculator.


   --
   ---   Cell sets, as used here
   --


   package Cell_Sets renames Storage.List_Cell_Sets;
   --
   -- The implementation of cell sets that we use.


   subtype Cell_Set_T is Cell_Sets.Set_T;
   --
   -- A set of cells somehow involved with a calculation.


   procedure Add (
      Cells : in     Storage.Cell_List_T;
      To    : in out Cell_Set_T)
   renames Cell_Sets.Add;

   function Intersection (Left, Right : Cell_Set_T)
   return Storage.Cell_Set_T
   renames Cell_Sets.Intersection;

   function Copy (Item : Storage.Cell_Set_T) return Cell_Set_T
   renames Cell_Sets.Copy;

   function Image (Item : Cell_Set_T) return String
   renames Cell_Sets.Image;


   --
   ---   Common properties of data pools and data fluxes
   --


   type Bounds_T is abstract new Arithmetic.Bounds_T with private;
   --
   -- A root type for data pools and data fluxes.


   -- overriding
   function Basis (Item : Bounds_T) return Storage.Cell_List_T;
   --
   -- The cells that the Item bounds are just the cells of the pool
   -- or the flux.


   -- overriding
   function Difference (To, From : Storage.Cell_T; Under : Bounds_T)
   return Storage.Bounds.Interval_T;
   --
   -- The interval (range) of values of the difference To - From,
   -- permitted Under the given bounds.
   --
   -- If both To and From are in the Basis of the bounds, the
   -- default implementation uses Interval (Expr, Under) with
   -- an expression that computes the difference To - From, and
   -- redispatching on Under. Otherwise, that is if one or both
   -- of the cells is not in the Basis, the default implementation
   -- returns Universal_Interval.


   -- overriding
   function Difference (
      To    : Arithmetic.Expr_Ref;
      From  : Storage.Cell_T;
      Under : Bounds_T)
   return Storage.Bounds.Interval_T;
   --
   -- The interval (range) of values of the difference To - From,
   -- permitted Under the given bounds.
   --
   -- If From is in the Basis of the bounds, the default implementation
   -- uses Interval (Expr, Under) with an expression that computes the
   -- difference To - From, and redispatching on Under. Otherwise, that
   -- is if From is not in the Basis, the default implementation returns
   -- Universal_Interval.


   function Cells_Of (Item : Bounds_T) return Storage.Cell_Set_T;
   --
   -- The cells that form the basis of the pool or the flux.


   function Owner_Of (Item : Bounds_T) return Calc_Handle_T;
   --
   -- The calculator in which the pool or the flux was calculated,
   -- and which thus "owns" the pool or the flux.


   --
   ---   Data pools
   --


   type Pool_T is new Bounds_T with private;
   --
   -- A reference to a constrained set of data-states, where
   -- a data-state is the association of an integer value
   -- with each cell in a set of cells (the basis of the pool).
   -- The set is computed by a calculator and contains within
   -- itself a calculator-handle and the basis-set of cells.
   --
   -- The possible data-states at a certain location in the
   -- program (e.g. on entry to a loop) are often represented
   -- as a pool.
   --
   -- The term "pool" is meant to suggest, firstly, that there
   -- are many items (data states), but limited by a boundary
   -- (the constraints as a "shoreline), secondly that the
   -- set of data is (often) located at a specific point
   -- in the subprogram), and thirdly that two pools can be
   -- joined by a stream (a flux, see below) that transports
   -- items from one pool to the other, perhaps with some
   -- transformation on the way.
   --
   -- The pool type is derived from Arithmetic.Bounds_T since one of
   -- the major roles of a pool object is to place bounds on the values
   -- of arithmetic cells and expressions in the data-state.
   --
   -- The bounds in a pool may be contradictory which means that the
   -- pool is "empty" (no data states). This usually means that the
   -- program path or point to which the pool applies is unreachable
   -- (infeasible).
   --
   -- In the present implementation, a pool contains the
   -- identifier of the Omega Calculator variable that
   -- contains the set of data-states.


   -- overriding
   function Image (Item : Pool_T) return String;
   --
   -- A brief identification of the pool.


   -- overriding
   function Interval (Cell : Storage.Cell_T; Under : Pool_T)
   return Storage.Bounds.Interval_T;
   --
   -- The interval (range) of values of the Cell, permitted Under
   -- the given pool.
   --
   -- Propagates Flow.False_Path if the pool is empty.


   -- overriding
   function Interval (Expr : Arithmetic.Expr_Ref; Under : Pool_T)
   return Storage.Bounds.Interval_T;
   --
   -- The interval (range) of values of the Expr, permitted Under the
   -- given pool. Note that the Expr does not necessarily take on
   -- every value in this interval. The function does not propagate
   -- Storage.Bounds.Unbounded but may return an interval where either
   -- or both Min and Max are unlimited.
   --
   -- Propagates Flow.False_Path if the pool is empty.


   -- overriding
   function Values (Expr : Arithmetic.Expr_Ref; Under : Pool_T)
   return Storage.Bounds.Value_List_T;
   --
   -- The list of possible values of the Expr, permitted Under the
   -- given pool. This list contains all and only the possible
   -- values of the Expr. If such a list cannot be derived from the
   -- bounds, the function propagates Storage.Bounds.Unbounded.
   --
   -- The length of the returned list is bounded by
   -- Storage.Bounds.Opt.Max_Listed_Values. If there are more
   -- values, the function propagates Storage.Bounds.Unbounded.


   function Calc_Image (Item : Pool_T) return String;
   --
   -- A readable description (as far as we can) of the pool,
   -- possibly referring to some calculator-specific identifier.


   type Pool_List_T is array (Positive range <>) of Pool_T;
   --
   -- A list of pools.


   --
   ---   Data fluxes
   --


   type Flux_T is new Bounds_T with private;
   --
   -- A reference to a "data flux", or the relation between the
   -- input-data-state and output-data-state of a program
   -- region, computed by a calculator. It contains within itself
   -- a calculator-handle and a set of cells (the basis of the
   -- data state).
   --
   -- The term "flux" is intended to connote both "change", as
   -- in "changes to variables", and "flow", as in "data-state
   -- that flows from this region to its successors".
   --
   -- The flux type is derived from Arithmetic.Bounds_T since one of
   -- the major roles of a flux object is to place bounds on the values
   -- of arithmetic cells and expressions in the output-data-state.
   --
   -- The bounds in a flux may be contradictory which means that the
   -- flux is "empty" (no data states). This usually means that the
   -- program path or point to which the flux applies is unreachable
   -- (infeasible).
   --
   -- In the present implementation, a flux contains the
   -- identifier of the Omega Calculator variable that
   -- contains the computed relation.


   function Interval (Cell : Storage.Cell_T; Under : Flux_T)
   return Storage.Bounds.Interval_T;
   --
   -- The interval (range) of values of the Cell, permitted Under
   -- the given flux.
   --
   -- Propagates Flow.False_Path if the flux is empty.
   --
   -- Overrides Arithmetic.Interval.


   function Interval (Expr : Arithmetic.Expr_Ref; Under : Flux_T)
   return Storage.Bounds.Interval_T;
   --
   -- The interval (range) of values of the Expr, permitted Under the
   -- given flux. Note that the Expr does not necessarily take on
   -- every value in this interval. The function does not propagate
   -- Storage.Bounds.Unbounded but may return an interval where either
   -- or both Min and Max are unlimited.
   --
   -- Propagates Flow.False_Path if the flux is empty.
   --
   -- Overrides (implements) Arithmetic.Interval.


   function Values (Expr : Arithmetic.Expr_Ref; Under : Flux_T)
   return Storage.Bounds.Value_List_T;
   --
   -- The list of possible values of the Expr, permitted Under the range
   -- of the given flux. This list contains all and only the possible
   -- values of the Expr. If such a list cannot be derived from the
   -- bounds, the function propagates Storage.Bounds.Unbounded.
   --
   -- The length of the returned list is bounded by
   -- Storage.Bounds.Opt.Max_Listed_Values. If there are more
   -- values, the function propagates Storage.Bounds.Unbounded.
   --
   -- Overrides Arithmetic.Values.


   function Calc_Image (Item : Flux_T) return String;
   --
   -- A readable description (as far as we can) of the flux,
   -- possibly referring to some calculator-specific identifier.
   --
   -- Note that this does *not* override the inherited Image function
   -- from Arithmetic.Bounds_T.


   type Flux_List_T is array (Positive range <>) of Flux_T;
   --
   -- A list of fluxes.


   --
   ---   Loop summaries
   --


   type Loop_Summary_T is record
      Repeat    : Flux_T;
      Variant   : Cell_Set_T;
      Invariant : Cell_Set_T;
   end record;
   --
   -- A summary of the effect of a loop body.
   --
   -- Repeat
   --    The flux on the repeat edges, from and including the loop-head
   --    to and including the precondition of the repeat edges.
   -- Variant
   --    The set of cells that, as far as we know, can vary (change their
   --    value) in an execution of the loop-body up to a repeat edge.
   -- Invariant
   --    The set of cells that are invariant in an execution of the
   --    loop-body up to a repeat edge.
   --
   -- The Repeat flux shall include the effect of the loop-head node and
   -- the paths within the loop to any repeat edge, including the
   -- precondition on the repeat edge. If there are several repeat
   -- edges, the Repeat flux shall express the union of their fluxes
   -- computed in this way. Note that the Repeat flux shall _not_
   -- include anything from the path _to_ the loop-head from outside
   -- the loop.
   --
   -- The Variant and Invariant cells shall be disjoint and together
   -- form the Cells_Of (Repeat).


   type Loop_Summary_List_T is array (Positive range <>) of Loop_Summary_T;
   --
   -- A list of loop-body summaries, for some list of loops (known by
   -- other means).


   --
   -- Exceptions
   --

   Calculator_Error : exception;
   --
   -- To be raised when some error occurs in the communication with a
   -- calculator instance. This is usually due to a fault in the
   -- emitted calculator formulae, or an error detected in the
   -- calculator itself.


   Null_Set_Error : exception;
   --
   -- To be raised when queries are made on an empty set (an empty
   -- pool or flux). This is usually due to an unreachable (infeasible)
   -- execution path, which may be intrinsic to the program under
   -- analysis or made infeasible by assertions.


   -- PROVIDED OPERATIONS:


   function Start (Comment_Text : in String)
   return Calc_Handle_T;
   --
   -- Starts a calculator instance.
   --
   -- Using  : comment text, purely descriptive, may be empty.
   -- Giving : calculator handle.


   procedure Stop (Calc : in out Calc_Handle_T);
   --
   -- Stops a calculator instance.
   --
   -- Using  : calculator-handle
   -- Giving : updated (now invalid) calculator handle.
   --
   -- No pools or fluxes associated with this calculator
   -- instance can be used after the calculator is stopped.


   procedure Comment (
      Text : in String;
      Calc : in Calc_Handle_T);
   --
   -- Provides a textual comment or description of the calculation about to
   -- be performed, or of the result(s) just calculated.
   -- This has no effect on the calculation, but may help the user understand
   -- problems and correct them.


   function Bounded_Pool (
      Cells  : Cell_Set_T'Class;
      Bounds : Storage.Bounds.Cell_Interval_List_T;
      Calc   : Calc_Handle_T)
   return Pool_T;
   --
   -- The pool based on the given Cells and constrained with
   -- the given Bounds.
   --
   -- The Cells parameter is class-wide to make only the result type
   -- controlling.


   function Bounded_Pool (
      Pool   : Pool_T;
      Bounds : Storage.Bounds.Cell_Interval_List_T)
   return Pool_T;
   --
   -- The given Pool, further constrained by the given Bounds on
   -- cells in the pool.
   --
   -- If the Bounds list is empty or places no bounds on the cells
   -- in the Pool, the Pool is returned unchanged.


   function Bounded_Pool (
      Pool : Pool_T;
      Pre  : Arithmetic.Effect_T)
   return Pool_T;
   --
   -- The given Pool, further constrained by the all the Range_Pre
   -- assignment constraints in Pre.
   --
   -- If there are no Range_Pre assignment constraints, the Pool
   -- is returned unchanged. Note that all assignments of any other
   -- type in Pre are ignored.
   --
   -- This function can be used for example in the following case:
   -- Pool is the data-pool into a step and Pre is the effect of the step.
   -- The Range_Pre constraints in Pre act to constrain the values
   -- that are actually used in the step, so the Pool is first passed
   -- through these constraints before the step is executed.


   function Intersection (Left, Right : Pool_T) return Pool_T;
   --
   -- The intersection of the two Pools, which are assumed to have
   -- the same set of cells (and exist in the same calculator instance).


   function Complement (Pool : Pool_T) return Pool_T;
   --
   -- The complement set of the given Pool.


   function Range_Bounded_Flux (
      Flux   : Flux_T;
      Bounds : Storage.Bounds.Cell_Interval_List_T)
   return Flux_T;
   --
   -- Using :
   --    flux,
   --    set of bounds on (some of) the basis cells
   -- Giving:
   --    the flux, range-constrained with the pool
   --    defined by the given bounds.
   --
   -- If the Bounds list is empty or places no bounds on the cells
   -- in the Flux, the Flux is returned unchanged.


   function Range_Bounded_Flux (
      Flux : Flux_T;
      Pre  : Arithmetic.Effect_T)
   return Flux_T;
   --
   -- Using :
   --    flux,
   --    set of assignments where only the Range_Pre assignment
   --    constraints are significant.
   -- Giving:
   --    the flux, range-constrained with the pool defined by
   --    all the Range_Pre assignment constraints in Pre.
   --
   -- If there are no Range_Pre assignment constraints, the Flux
   -- is returned unchanged. Note again that all assignments of
   -- any other type in Pre are ignored.
   --
   -- This function can be used for example in the following case:
   -- Flux is the flux into a step and Pre is the effect of the step.
   -- The Range_Pre constraints in Pre act to constrain the values
   -- that are actually used in the step, so the Flux is first passed
   -- through these constraints before the step is executed.


   function Identity_Flux (Pool : Pool_T'Class) return Flux_T;
   --
   -- Using :
   --    pool
   -- Giving:
   --    a flux with the same basis-set of cells as
   --    the pool, domain-constrained to the pool,
   --    and implementing the identity mapping (no
   --    change in cell values).


   function Domain_Of (Flux : Flux_T'Class)
   return Pool_T;
   --
   -- The domain pool of the given flux.


   function Constrain_Range (
      Flux : Flux_T;
      Pool : Pool_T'Class)
   return Flux_T;
   --
   -- Using :
   --    flux,
   --    pool
   -- Giving:
   --    the same flux, with the constraint that
   --    the output-data-state is in the pool.
   --
   -- NOTE: The Flux and the Pool can have different cell_sets.


   procedure Flux_To_Steps (
      Nodes      : in     Flow.Node_List_T;
      Edges      : in     Flow.Edge_List_T;
      Living     : in     Flow.Life.Living_T;
      Root       : in     Flux_T;
      Luups      : in     Loops.Loop_List_T;
      Summaries  : in     Loop_Summary_List_T;
      Steps      : in     Flow.Step_List_T;
      Into_Luups :    out Flux_List_T;
      Repeats    :    out Flux_List_T;
      Into_Steps :    out Flux_List_T;
      Exit_Flux  :    out Flux_T);
   --
   -- Propages data-flow flux within an acyclic region of a flow-graph
   -- to compute the flux into certain interesting steps within the region.
   -- The region can contain collapsed loops with associated repeat-fluxes.
   --
   -- The interesting steps for which "into" fluxes are computed are the
   -- loop-head steps and some explicitly listed Steps. Improved repeat-
   -- fluxes are computed for the loops. The exit flux from the whole
   -- region is also computed.
   --
   -- Nodes, Edges
   --    Define the acyclic flow-graph region to be processed.
   --    The set of nodes in the region is the union of the sources
   --    and targets of the given Edges and the given Nodes. Thus, the
   --    trivial case of a one-node region would have an empty Edges
   --    list and the single node in the Nodes list. The Nodes parameter
   --    has no other significance.
   --
   --    The data-flow computation for a given node in the region considers
   --    only incoming and outgoing edges from the Edges set. Other edges
   --    connected to this node may exist in the flow-graph that contains
   --    the region, but are not included in the computation. Thus, the body
   --    of a loop can be considered an acyclic region by leaving out the
   --    repeat edges from the Edges set. Moreover, infeasible edges can
   --    be left out.
   --
   -- Living
   --    A computation model and live-assignment map for the flow-graph.
   --    It defines the (live) effect of each step and the precondition
   --    of each edge. The underlying flow-graph shows how many steps and
   --    nodes must be handled which sets the size of some local data.
   --
   -- Root
   --    The root-flux assumed to enter the region's root(s).
   --
   -- Luups
   --    Loops to be considered in the calculation. This list should
   --    contain exactly the collapsed loops within the region and must
   --    be sorted in bottom-up nesting order (inner loop before outer).
   --
   -- Summaries
   --    The summary effects of the Luups, for each loop giving the flux
   --    on the repeat-edges and the set of loop-invariant cells, for
   --    example as generated by Loops.Slim.Approximate_Loops.
   --
   --    The indexing equals that of the Luups list: the summary for
   --    Luups(I) is Summaries(I).
   --
   -- Steps
   --    Lists the "interesting" steps in the region, that is, those
   --    for which we will compute the flux into the step. The same
   --    step may occur several times in the list, but the flux will
   --    anyway be computed once for each listed step.
   --
   -- Into_Luups
   --    The flux into the loop-head steps of the Luups from outside
   --    the loop, propagated along the region from the assumed Root flux
   --    at the root nodes and including the transitive closure of the
   --    collapsed loops but _excluding_ the repeat flux from the body
   --    of this loop back to the loop-head. Thus, this flux shows the
   --    initial values for the loop on entry to the loop.
   --
   --    The indexing equals that of the Luups list: the flux into the
   --    head of Luups(L) is Into_Luups(L).
   --
   -- Repeats
   --    An improved version of the repeat-flux for the Luups. This is
   --    Summaries.Repeat but domain-constrained to a better model of the
   --    data-pool at the start of the loop-head. Note that this repeat-
   --    flux, unlike Summaries.Repeat, depends on the computation that
   --    leads to the loop (Into_Luups).
   --
   --    The indexing equals that of the Luups list: the repeat-flux for
   --    Luups(L) is Repeats(L).
   --
   -- Into_Steps
   --    The flux into each of the given Steps, propagated along the
   --    region from the assumed Root flux at the root nodes, including
   --    the transitive closure of the collapsed loops.
   --
   --    The indexing equals that of the Steps list: the flux into Steps(I)
   --    is Into_Steps(I).
   --
   --    If Steps(I) is a loop-head step, Into_Steps(I) contains the flux
   --    from all edges to the step, including the repeat edges. Thus,
   --    Into_Steps(I) is probably different from Into_Luups() for this
   --    loop.
   --
   -- Exit_Flux
   --    The flux that exits the region, which is the combined flux on
   --    all edges from the region to outside the region.
   --
   -- The total data-flow into a loop-head is taken as the flow along the
   -- entry edges (from outside the loop, computed in the normal way and
   -- returned as Into_Luups), united with the flow along the repeat edges,
   -- which is computed as Into_Luups . Z . Repeats where Z is a "fuzz"
   -- relation that keeps loop-invariant cells unchanged and makes
   -- loop-variant cells unknown (opaque).
   --
   -- All explorations of the flow-graph use only the given Edges. Thus,
   -- to omit infeasible edges (and nodes) the Edges list should include
   -- only feasible edges.
   --
   -- If some of the given steps do not belong to the region, a fault
   -- message is emitted and Constraint_Error is raised.
   --
   -- Into_Luups and Repeats will contain only the fluxes for those Luups
   -- where the loop-head is included in the region.


   procedure Pool_To_Steps (
      Nodes      : in     Flow.Node_List_T;
      Edges      : in     Flow.Edge_List_T;
      Living     : in     Flow.Life.Living_T;
      Root       : in     Pool_T;
      Luups      : in     Loops.Loop_List_T;
      Summaries  : in     Loop_Summary_List_T;
      Steps      : in     Flow.Step_List_T;
      Into_Luups :    out Pool_List_T;
      Repeats    :    out Flux_List_T;
      Into_Steps :    out Pool_List_T;
      Exit_Pool  :    out Pool_T);
   --
   -- Propages data-flow flux within an acyclic region of a flow-graph
   -- to compute the data-pools into certain interesting steps within the
   -- region. The region can contain collapsed loops with associated
   -- repeat-fluxes.
   --
   -- The interesting steps for which "into" pools are computed are the
   -- loop-head steps and some explicitly listed Steps. Improved repeat-
   -- fluxes are computed for the loops. The exit pool from the whole
   -- region is also computed.
   --
   -- Nodes, Edges
   --    Define the acyclic flow-graph region to be processed.
   --    The set of nodes in the region is the union of the sources
   --    and targets of the given Edges and the given Nodes. Thus, the
   --    trivial case of a one-node region would have an empty Edges
   --    list and the single node in the Nodes list. The Nodes parameter
   --    has no other significance.
   --
   --    The data-flow computation for a given node in the region considers
   --    only incoming and outgoing edges from the Edges set. Other edges
   --    connected to this node may exist in the flow-graph that contains
   --    the region, but are not included in the computation. Thus, the body
   --    of a loop can be considered an acyclic region by leaving out the
   --    repeat edges from the Edges set. Moreover, infeasible edges can
   --    be left out.
   --
   -- Living
   --    A computation model and live-assignment map for the flow-graph.
   --    It defines the (live) effect of each step and the precondition
   --    of each edge. The underlying flow-graph shows how many steps and
   --    nodes must be handled which sets the size of some local data.
   --    If the data-flux propagation finds some empty pools, the
   --    corresponding flow-graph elements are marked infeasible, and
   --    the computation model is pruned. However, checking for null pools
   --    is optional, per Calculator.Opt.Find_Null_Flow.
   --
   -- Root
   --    The root-pool assumed to enter the region's root(s).
   --
   -- Luups
   --    Loops to be considered in the calculation. This list should
   --    contain exactly the collapsed loops within the region and must
   --    be sorted in bottom-up nesting order (inner loop before outer).
   --
   -- Summaries
   --    The summary effects of the Luups, for each loop giving the flux
   --    on the repeat-edges and the set of loop-invariant cells, for
   --    example as generated by Loops.Slim.Approximate_Loops.
   --
   --    The indexing equals that of the Luups list: the summary for
   --    Luups(I) is Summaries(I).
   --
   -- Steps
   --    Lists the "interesting" steps in the region, that is, those
   --    for which we will compute the pool into the step. The same
   --    step may occur several times in the list, but the pool will
   --    anyway be computed once for each listed step.
   --
   -- Into_Luups
   --    The pool into the loop-head steps of the Luups from outside
   --    the loop, propagated along the region from the assumed Root pool
   --    at the root nodes and including the transitive closure of the
   --    collapsed loops but _excluding_ the repeat flux from the body
   --    of this loop back to the loop-head. Thus, this pool shows the
   --    initial values for the loop on entry to the loop.
   --
   --    The indexing equals that of the Luups list: the pool into the
   --    head of Luups(L) is Into_Luups(L).
   --
   -- Repeats
   --    An improved version of the repeat-flux for the Luups. This is
   --    Summaries.Repeat but domain-constrained to a better model of the
   --    data-pool at the start of the loop-head. Note that this repeat-
   --    flux, unlike Summaries.Repeat, depends on the computation that
   --    leads to the loop (Into_Luups).
   --
   --    The indexing equals that of the Luups list: the repeat-flux for
   --    Luups(L) is Repeats(L).
   --
   -- Into_Steps
   --    The pool into each of the given Steps, propagated along the
   --    region from the assumed Root pool at the root nodes, including
   --    the approximated transitive closure of the collapsed loops.
   --
   --    The indexing equals that of the Steps list: the pool into Steps(I)
   --    is Into_Steps(I).
   --
   --    If Steps(I) is a loop-head step, Into_Steps(I) contains the data
   --    from all edges to the step, including the repeat edges. Thus,
   --    Into_Steps(I) is probably different from Into_Luups() for this
   --    loop.
   --
   -- Exit_Pool
   --    The pool that exits the region, which is the combined data-pool
   --    on all edges from the region to outside the region.
   --
   -- The total data-flow into a loop-head is taken as the flow along the
   -- entry edges (from outside the loop, computed in the normal way and
   -- returned as Into_Luups), united with the flow along the repeat edges,
   -- which is computed as Into_Luups . Z . Repeats where Z is a "fuzz"
   -- relation that keeps loop-invariant cells unchanged and makes
   -- loop-variant cells unknown (opaque).
   --
   -- All explorations of the flow-graph use only the given Edges. Thus,
   -- to omit infeasible edges (and nodes) the Edges list should include
   -- only feasible edges.
   --
   -- If some of the given steps do not belong to the region, a fault
   -- message is emitted and Constraint_Error is raised.
   --
   -- Into_Luups and Repeats will contain only the pools for those Luups
   -- where the loop-head is included in the region.


   function Union (Fluxes : Flux_List_T) return Flux_T;
   --
   -- Using :
   --    a set of fluxes (all from the same calculator)
   -- Giving:
   --    the united flux being the "union" of the given fluxes.


   function Invariants_In_Flux (Flux : Flux_T)
   return Storage.Cell_List_T;
   --
   -- The set of variables that the given Flux does not modify.


   function Flux_To_Vary (
      Basis : Cell_Set_T'Class;
      Var   : Cell_Set_T'Class;
      Calc  : Calc_Handle_T)
   return Flux_T;
   --
   -- A flux, with the given Basis, that varies the Varying
   -- variables in an undefined (hidden) manner and keeps
   -- all other basis variables invariant.
   --
   -- The Basis and Var parameters are class-wide to make only
   -- the result type controlling.


   function Repeat_With_Induction (
      Initial   : Pool_T'Class;
      Invariant : Cell_Set_T'Class;
      Induction : Storage.Cell_List_T;
      Step      : Storage.Bounds.Interval_List_T;
      Repeat    : Flux_T'Class)
   return Flux_T;
   --
   -- A flux that models the Repeat flux of a loop constrained by
   -- the dependence of the values of Induction variables on their
   -- Initial values, their Steps, and a joint (synthetic) counter
   -- variable. We assume that the Repeat flux is the "improved"
   -- repeat flux from Pool_To_Steps, with a domain constrained to
   -- the full pool at the start of the loop body (including the
   -- raw repeat flux from the repeat edges). The Invariant cells
   -- are all the loop-invariant cells. No Induction cell shall
   -- be an Invariant.
   --
   -- The domain space and range space of the Initial flux may be
   -- larger than those of the Repeat flux. The Repeat flux concerns
   -- only the cells involved in the loop, but includes all Induction
   -- variables.
   --
   -- The resulting flux is the composition of two fluxes: the
   -- induction model and the given Repeat flux. The composition
   -- is then projected on the domain side to have only the joint
   -- iteration counter in its domain.
   --
   -- The domain space of the induction-model flux is that of the
   -- Repeat flux, extended with the joint iteration counter C.
   -- The domain of the induction-model flux constrains the values
   -- of the Induction variables to their Initial values.
   --
   -- The range space of the induction-model flux is the domain space
   -- of the Repeat flux; the joint iteration counter is left out.
   -- The range of the induction-model flux constrains each Induction
   -- variable to its Initial value (from the domain) plus C times
   -- its Step. Tthe other non-Invariant variables are unconstrained.
   --
   -- The domain of the resulting flux contains those values of
   -- the joint iteration counter, at the start of the loop body,
   -- that allow the loop to repeat. If the domain is empty, the
   -- loop is unrepeatable. If the domain has an upper bound M,
   -- M+1 is an upper bound on loop repetitions.
   --
   -- Note that the resulting flux is unusual in that its domain has
   -- no cells, only the iteration counter.
   --
   -- The Invariant parameter is class-wide to make only the
   -- return type controlling.


   function Is_In (Value : Arithmetic.Value_T; Pool : Pool_T)
   return Boolean;
   --
   -- Whether the given Value is in the given one-dimensional Pool.


   function Smallest_Value (
      Pool : Pool_T;
      Min  : Arithmetic.Value_T)
   return Arithmetic.Value_T;
   --
   -- The smallest value in the given one-dimensional Pool, no less
   -- than Min. Propagates Null_Set_Error if such a value cannot be found
   -- (perhaps because it is larger than Opt.Max_Int_Calc).


   function Largest_Value (
      Pool : Pool_T;
      Max  : Arithmetic.Value_T)
   return Arithmetic.Value_T;
   --
   -- The largest value in the given one-dimensional Pool, no greater
   -- than Max. Propagates Null_Set_Error if such a value cannot be found
   -- (perhaps because it is less than Opt.Min_Int_Calc).


   function Smallest_Hole (
      Pool : Pool_T;
      Min  : Arithmetic.Value_T)
   return Arithmetic.Value_T;
   --
   -- The smallest value that is not in the given one-dimensional Pool,
   -- and is no less than Min. Propagates Null_Set_Error if such a value
   -- cannot be found (perhaps because it is larger than Opt.Max_Int_Calc).
   --
   -- This should be the same as Smallest_Value (Complement (Pool), Min).
   -- However, this operation always uses a binary-search method, not
   -- a method based on hull/convexhull.


   function Largest_Hole (
      Pool : Pool_T;
      Max  : Arithmetic.Value_T)
   return Arithmetic.Value_T;
   --
   -- The largest value that is not in the given one-dimensional Pool,
   -- and is no greater than Max. Propagates Null_Set_Error if such a value
   -- cannot be found (perhaps because it is less than Opt.Min_Int_Calc).
   --
   -- This should be the same as Largest_Value (Complement (Pool), Max).
   -- However, this operation always uses a binary-search method, not
   -- a method based on hull/convexhull.


   function Bounds_Of (Pool : Pool_T)
   return Storage.Bounds.Interval_T;
   --
   -- The bounds, if any, on the values in the given one-dimensional Pool.
   --
   -- Raises exception Null_Set_Error if the Pool is empty.


   function Bounds_Of_Complement (
      Pool   : Pool_T;
      Within : Storage.Bounds.Interval_T)
   return Storage.Bounds.Interval_T;
   --
   -- The bounds, if any, on the complement of the given one-dimensional
   -- Pool, including only the values Within the given interval.
   --
   -- Raises exception Null_Set_Error if the complement of the Pool
   -- has no elements Within the desired interval.


   function Bounds_Of_Domain (Flux : Flux_T)
   return Storage.Bounds.Interval_T;
   --
   -- The bounds, if any, on the domain of the Flux, which is
   -- assumed to be one-dimensional (possibly containing only
   -- a synthetic variable, not necessarily a cell).
   --
   -- Raises exception Null_Set_Error if the Flux is empty (infeasible).


   function Is_In_Domain (Value : Arithmetic.Value_T; Flux : Flux_T)
   return Boolean;
   --
   -- Whether the given Value is a member of the one-dimensional
   -- domain of the given Flux.


   function Bounds_From_Pool (
      Pool : Pool_T;
      Cell : Storage.Cell_T)
   return Storage.Bounds.Interval_T;
   --
   -- Using :
   --    a pool,
   --    a cell which is in the basis of the pool.
   -- Giving:
   --    the range of the variable cell, implied by the pool, with a
   --    lower-bound and upper-bound of known (literal) value,
   --    or "unlimited".
   --
   -- Raises exception Null_Set_Error if Pool is empty (unreachable).


   function Bounds_From_Pool (
      Pool : Pool_T;
      Expr : Arithmetic.Expr_Ref)
   return Storage.Bounds.Interval_T;
   --
   -- Using :
   --    a pool,
   --    an expression of cells in the pool
   -- Giving:
   --    the range of the expression, implied by the pool, with a
   --    lower-bound and upper-bound of known (literal) value,
   --    or "unlimited".
   --
   -- Raises exception Null_Set_Error if Pool is empty (unreachable).


   function Bounds_From_Flux (
      Flux : Flux_T;
      Cell : Storage.Cell_T)
   return Storage.Bounds.Interval_T;
   --
   -- Using :
   --    a flux,
   --    a cell which is a variable in the flux
   -- Giving:
   --    the range of the variable cell, implied by the flux, with a
   --    lower-bound and upper-bound of known (literal) value,
   --    or "unlimited".
   --
   -- Raises exception Null_Set_Error if Flux is empty (unreachable).


   function Bounds_From_Complement (
      Flux : Flux_T;
      Cell : Storage.Cell_T)
   return Storage.Bounds.Interval_T;
   --
   -- Using :
   --    a flux,
   --    a cell which is a variable in the flux
   -- Giving:
   --    limits (lower bound and upper bound) for the variable cell,
   --    implied by the complement of the flux.
   --
   -- Raises exception Null_Set_Error if the complemenet of the
   -- Flux is empty (unreachable).


   function Bounds_From_Flux (
      Flux : Flux_T;
      Expr : Arithmetic.Expr_Ref)
   return Storage.Bounds.Interval_T;
   --
   -- Using :
   --    a flux,
   --    an expression of cells in the flux
   -- Giving:
   --    the range of the expression, implied by the flux, with a
   --    lower-bound and upper-bound of known (literal) value,
   --    or "unlimited".
   --
   -- Raises exception Null_Set_Error if Flux is empty (unreachable).


   function Is_In_Range (
      Value : Arithmetic.Value_T;
      Flux  : Flux_T;
      Cell  : Storage.Cell_T)
   return Boolean;
   --
   -- Whether the given Cell can have the given Value, in the
   -- range of the given Flux.


   function Step_From_Flux (
      Flux : Flux_T;
      Cell : Storage.Cell_T)
   return Storage.Bounds.Interval_T;
   --
   -- Using :
   --    a flux,
   --    a cell which is a variable in the flux
   -- Giving:
   --    the range of the change in the value of the cell, caused
   --    by the flux, with a lower-bound and upper-bound of known
   --    literal) value, or "unlimited".
   --
   -- Raises exception Null_Set_Error if Flux is empty (unreachable).


   function Pool_After_Effect (
      Pool   : Pool_T;
      Effect : Arithmetic.Effect_T)
   return Pool_T;
   --
   -- The pool that result from a given Pool after mapping (transforming)
   -- the Pool with an Effect. For example, if Pool is the data-pool into
   -- a step, and Effect is the effect of the step, then the result is the
   -- data-pool out from the step.


   function Flux_After_Effect (
      Flux   : Flux_T;
      Effect : Arithmetic.Effect_T)
   return Flux_T;
   --
   -- The flux that result from a given Flux joined (on the Range side)
   -- to an Effect that transforms the data state. For example, if
   -- Flux is the flux into a step, and Effect is the effect of the
   -- step, then the result is the flux out from the step.


   function Bounds_After_Step (
      Into_Step : Pool_T;
      Effect    : Arithmetic.Effect_T;
      Cell      : Storage.Cell_T)
   return Storage.Bounds.Interval_T;
   --
   -- The bounds (interval), if any, on the given Cell, implied by the
   -- data-pool that results when the given Effect is applied to the
   -- given Into_Step data-pool.


   function Bounds_After_Step (
      Into_Step : Flux_T;
      Effect    : Arithmetic.Effect_T;
      Cell      : Storage.Cell_T)
   return Storage.Bounds.Interval_T;
   --
   -- Using :
   --    a flux into a step,
   --    the effect of the step,
   --    a cell which is a variable in the flux
   -- Giving:
   --    lower bound and upper bound, or "unlimited", for the value
   --    of the cell caused by the flux and the execution of the step.
   --
   -- Raises exception Null_Set_Error if Flux is empty (unreachable).


   function Values_In_Domain (
      Flux : Flux_T'Class;
      Cell : Storage.Cell_T)
   return Pool_T;
   --
   -- The pool of Cell values in the domain of the Flux.


   function Value_Pool (
      Flux : Flux_T'Class;
      Expr : Arithmetic.Expr_Ref;
      Var  : Storage.Cell_T)
   return Pool_T;
   --
   -- The pool of values of the Expr, in the range of the Flux, using
   -- Var cell to represent the value of the Expr in the pool. In
   -- other words, the basis of the pool is {Var}. The Var cell can
   -- occur in the expression, but this has no effect on the result,
   -- where Var is used only as a place-holder.


   function Values (Within : Pool_T) return Storage.Bounds.Value_List_T;
   --
   -- The list of all values Within a given one-dimensional pool.
   --
   -- The length of the returned list is bounded by
   -- Storage.Bounds.Opt.Max_Listed_Values. If there are more
   -- values, or if the pool is not bounded, the function propagates
   -- Storage.Bounds.Unbounded.


private


   type Calc_Id_T is new Positive;
   --
   -- A unique sequential number to identify a calculator instance.


   type Pool_Id_T is new Positive;
   --
   -- A unique sequential number to identify a pool
   -- (within a calculator).


   type Flux_Id_T is new Positive;
   --
   -- A unique sequential number to identify a flux
   -- (within a calculator).


   type Calc_Object_T is record

      Exec : Exec_Cmd.Execution_T;

      Calc_ID : Calc_Id_T;

      Next_Pool_Id : Pool_Id_T := 1;
      -- The next pool identifier to be used by New_Pool.

      Next_Flux_Id : Flux_Id_T := 1;
      -- The next flux identifier to be used by New_Flux.

   end record;

   type Calc_Handle_T is access Calc_Object_T;
   --
   -- By using an access type, we can use functions with
   -- Calc_Handle parameters, yet update the calculator status
   -- accessible to all users of that calculator.


   type Bounds_T is new Arithmetic.Bounds_T with record
      Cells : Cell_Set_T;
      Calc  : Calc_Handle_T;
   end record;


   type Pool_T is new Bounds_T with record
      Id : Pool_Id_T;
   end record;


   type Flux_T is new Bounds_T with record
      Id : Flux_Id_T;
   end record;


end Calculator;
