-- Flow.Origins (decl)
--
-- Propagation of value origins (definitions) in a a flow-graph associated
-- with a computation model that attaches definitions and uses of cells to
-- flow steps and arithmetic preconditions to flow edges. The result is
-- information about the overall transfer relation of the computation, in
-- particular about the invariance of certain cells across the whole
-- computation (entry to exit) and over loop bodies (separately for repeat
-- iterations and overall).
--
-- This analysis concentrates on "copy" assignments of the form x := y
-- where the right-hand-side (y) is a single cell. The target cell (x)
-- is then considered to have the same origin as the right-hand-side (y).
--
-- Any assignment where the right-hand-side is a more complex expression is
-- considered to yield an unknown value and becomes the origin for the
-- target cell at this point.
--
-- When two or more different origins for the same cell flow into the
-- same point in the flow-graph, a "merge" origin is defined for this cell
-- at this point. (This corresponds to "phi" functions in "static single
-- assignment" form.)
--
-- This analysis has four main purposes:
--
-- > To detect cells that are preserved (invariant) in any execution of
--   the flow-graph (any call of the subprogram), even though the
--   computation uses and assigns these cells. Such cells are often
--   "callee-save" registers. The analysis of the caller often depends
--   critically on knowing which caller cells are preserved across the
--   call.
--
-- > To detect cells that are preserved (invariant) in a loop, so that
--   the loop can be analysed using the initial values of these cells.
--
-- > To bound certain boundable elements of the subprogram based on
--   knowing that some relevant cells hold certain values from the
--   entry to the subprogram. For example, a dynamic jump to a register
--   that holds the return address can be resolved into a return from
--   the subprogram.
--
-- > Finally, the analysis results can be passed to a target-specific
--   procedure that may use them for any purpose.
--
-- The analysis can also report that the input value of a cell ends up
-- as the output value of another cell (invariance of value but change
-- of storage location). For example, the effect of a function that swaps
-- the values of its two input parameters can be reported exactly.
--
-- The result of this analysis is valid only for the given control-
-- flow graph. If the flow-graph contains unresolved dynamic flow,
-- which is later resolved to add edges that create more paths from
-- some (new or existing) value assignments to existing value uses,
-- these new paths may invalidate the results of this analysis. To
-- be precise, the origins in the old analysis should still be possible
-- origins in the new flow-graph, but new origins may appear, too.
--
-- This analysis is path-sensitive but not context-sensitive. It does
-- not use any information about the numerical values of cells on
-- entry to the flow-graph or within the flow-graph, nor does it use
-- the edge preconditions but assumes that all feasible edges can be
-- taken (that is, only edges with "Never" preconditions are omitted
-- from the analysed paths).
--
-- Volatile cells are almost completely omitted from this analysis.
-- The origins of volatile cells are not analysed. If a non-volatile
-- cell is assigned the value of a volatile cell, this assignment
-- becomes the origin of the value of the target cell, and the value
-- assigned is considered unknown (that is, the volatile cell is
-- considered a "complex expression", not a simple copy).
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
-- $Date: 2015/10/24 19:36:49 $
--
-- $Log: flow-origins.ads,v $
-- Revision 1.11  2015/10/24 19:36:49  niklas
-- Moved to free licence.
--
-- Revision 1.10  2013/12/08 22:05:57  niklas
-- BT-CH-0259: Storing value-origin analysis results in execution bounds.
--
-- Revision 1.9  2013-02-19 09:17:26  niklas
-- BT-CH-0245 clean-up. Only descriptions changed.
--
-- Revision 1.8  2013-02-12 08:47:19  niklas
-- BT-CH-0245: Global volatile cells and "volatile" assertions.
--
-- Revision 1.7  2008-12-25 09:00:34  niklas
-- Moved context clause for Storage.Cell_Numbering to body.
--
-- Revision 1.6  2007/04/18 18:34:39  niklas
-- BT-CH-0057.
--
-- Revision 1.5  2007/03/29 15:18:03  niklas
-- BT-CH-0056.
--
-- Revision 1.4  2007/01/13 13:51:04  niklas
-- BT-CH-0041.
--
-- Revision 1.3  2006/11/20 20:20:19  niklas
-- BT-CH-0037.
--
-- Revision 1.2  2006/10/24 08:44:31  niklas
-- BT-CH-0028.
--
-- Revision 1.1  2005/05/09 15:24:23  niklas
-- First version.
--

--:dbpool with GNAT.Debug_Pools;


with Flow.Computation;
with Programs;
with Symbols;


package Flow.Origins is


   --
   ---   Results of the analysis
   --


   type Origin_Kind_T is (
      Initial,
      Assigned,
      Merged);
   --
   -- We consider three kinds of origins (sources) for the values
   -- of cells.
   --
   -- Initial
   --    The value is the initial value of the cell on entry to the
   --    subprogram under analysis.
   -- Assigned
   --    The value is computed in a step and assigned to the cell in
   --    the step's effect.
   -- Merged
   --    The value flows into a step from several predecessor steps
   --    such that not all predecessors have the same origin for
   --    the value. In other words, the value has multiple origins
   --    from the multiple paths into this step.


   type Origin_T is record
      Kind : Origin_Kind_T;
      Cell : Storage.Cell_T;
      Step : Step_T;
   end record;
   --
   -- The origin of a value.
   --
   -- Kind
   --    The kind of origin (how the value is defined).
   -- Cell
   --    The original cell that holds the value, at the point of origin.
   -- Step
   --    The point of origin.
   --    When Kind = Initial this is the entry step of the flow-graph.
   --    When Kind = Assigned this is the step that assigns the value
   --    to the Cell.
   --    When Kind = Merged this is the step at which the multiple
   --    origins converge and are merged.


   function Image (Item : Origin_T) return String;
   --
   -- Displaying the origin for tracing purposes.


   type Map_Ref is private;
   --
   -- The results of the value-origin analysis: a mapping of cells to the
   -- origins of their values.
   --
   -- This type has reference semantics. It represents several
   -- dynamically (heap-) allocated objects and therefore needs
   -- to be discarded when no longer needed.
   --
   -- The analysis results can be null, that is, absent.


   function Is_Valid (Item : Map_Ref) return Boolean;
   --
   -- Whether the given analysis results are valid and not "null".


   procedure Analyse (
      Computation : in     Flow.Computation.Model_Ref;
      Giving      :    out Map_Ref);
   --
   -- Finds the origin of each cell at each step in the subprogram under
   -- the given Computation model. The analysis is controlled by the options
   -- in Flow.Origins.Opt and can be entirely disabled by those options.
   --
   -- The effect of any calls from the subprogram is assumed to be represented
   -- in the effect of the call-steps under the model.
   --
   -- Only those parts of the flow-graph that are feasible in this Computation
   -- model are used and represented in the analysis result.
   --
   -- Responsibility for the dynamically allocated memory holding the
   -- analysis results is given to the caller, who should Discard the
   -- analysis when the caller no longer needs it.
   --
   -- The result can be non-valid, in particular if the options entirely
   -- disable value-origin analysis. In that case, a Discard is not
   -- necessary (but does no harm).


   procedure Discard (Item : in out Map_Ref);
   --
   -- Discards (deallocates) all the dynamically allocated memory forming
   -- the given value-origin map. The map becomes non-valid and must not
   -- be used thereafter, until the object is reconstructed with the
   -- Analyse procedure.


   function Computation (Under : Map_Ref)
   return Flow.Computation.Model_Handle_T;
   --
   -- The computation model Underlying the given value-origin results.
   -- Note that if the computation model is changed, throught the handle
   -- here returned, the results of the value-origin analysis may become
   -- invalid for the new computation model.


   function Symbol_Table (Under : Map_Ref)
   return Symbols.Symbol_Table_T;
   --
   -- The symbol-table for the program which contains the subprogram on
   -- which the given value-origin was applied.


   function All_Cells (Under : Map_Ref)
   return Storage.Cell_List_T;
   --
   -- All the cells for which Origin_Is_Known, Under the given
   -- value-origin map, listed in some unspecified order.


   function Origin_Is_Known (
      Cell : Storage.Cell_T;
      From : Map_Ref)
   return Boolean;
   --
   -- Whether the origin of the given Cell is known From the given
   -- value-origin map (at each and every step in the flow-graph).


   function Origin_After (
      Step : Step_T;
      Cell : Storage.Cell_T;
      From : Map_Ref)
   return Origin_T;
   --
   -- The origin of the value of the given Cell, after the execution
   -- of the given Step (on exit from the Step), as derived From the
   -- value-origin map.
   --
   -- Precondition: Origin_Is_Known (Cell, From).


   function Origin_Before (
      Step  : Step_T;
      Cell  : Storage.Cell_T;
      From  : Map_Ref)
   return Origin_T;
   --
   -- The origin of the value of the given Cell, before the execution
   -- of the given Step (on entry to the Step), as derived From the
   -- value-origin map.
   --
   -- Precondition: Origin_Is_Known (Cell, From).
   --
   -- Note that the returned Origin_T is not necessarily one of the
   -- origins listed in From. If the Step has more than one feasible
   -- predecessor, and different origins of the cell flow from some
   -- predecessors, a new Merged origin is returned.


   function Has_Same_Value (
      After  : Step_T;
      Before : Step_T;
      Expr   : Arithmetic.Expr_Ref;
      From   : Map_Ref)
   return Boolean;
   --
   -- Whether the given Expr is sure to have the same value when
   -- evaluated After a given step as when evaluated Before another
   -- step. This is the case iff every cell used in the Expr has the
   -- same Origin_After After as its Origin_Before (Before), and
   -- moreover the Expr has no unresolved dynamic references (we
   -- could not check if the unknown target of the unresolved
   -- reference has the same value after After and before Before).


private


   type Map_Object_T (Steps : Positive);
   --
   -- The results of value-origin analysis.
   --
   -- Steps
   --    The number of steps in the flow-graph that was analysed.


   type Map_Ref is access Map_Object_T;

   --:dbpool Map_Pool : GNAT.Debug_Pools.Debug_Pool;

   --:dbpool for Map_Ref'Storage_Pool use Map_Pool;


end Flow.Origins;
