-- Flow.Const (body)
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
-- $Revision: 1.38 $
-- $Date: 2015/10/24 20:05:48 $
--
-- $Log: flow-const.adb,v $
-- Revision 1.38  2015/10/24 20:05:48  niklas
-- Moved to free licence.
--
-- Revision 1.37  2013/12/20 21:11:07  niklas
-- Using Opt.Resolve_Opt (Resolve_Flow, Resolve_Stack) to control
-- the use of constant propagation for resolving dynamic flow and
-- stack usage.
--
-- Revision 1.36  2013-02-19 09:15:24  niklas
-- BT-CH-0245 clean-up. Only descriptions and tracing changed.
--
-- Revision 1.35  2013-02-12 08:47:19  niklas
-- BT-CH-0245: Global volatile cells and "volatile" assertions.
--
-- Revision 1.34  2012-02-13 17:52:20  niklas
-- BT-CH-0230: Options -max_loop and -max_stack for spurious bounds.
--
-- Revision 1.33  2009-11-27 11:28:07  niklas
-- BT-CH-0184: Bit-widths, Word_T, failed modular analysis.
--
-- Revision 1.32  2009/02/27 11:53:13  niklas
-- Suppressed the Fault messages on unprocessed feasible steps
-- when Opt.Refine_Edge_Conditions is disabled.
--
-- Revision 1.31  2009/01/18 08:06:50  niklas
-- Removed unused context clauses and locals.
--
-- Revision 1.30  2008/06/20 10:11:53  niklas
-- BT-CH-0132: Data pointers using Difference (Expr, Cell, Bounds).
--
-- Revision 1.29  2008/06/18 20:52:56  niklas
-- BT-CH-0130: Data pointers relative to initial-value cells.
--
-- Revision 1.28  2008/01/31 21:57:45  niklas
-- BT-CH-0108: Fixes to BT-CH-0098.
--
-- Revision 1.27  2007/12/17 13:54:36  niklas
-- BT-CH-0098: Assertions on stack usage and final stack height, etc.
--
-- Revision 1.26  2007/11/12 21:37:27  niklas
-- BT-CH-0097: Only arithmetic analysis marks boundable edge domain.
--
-- Revision 1.25  2007/10/26 12:44:35  niklas
-- BT-CH-0091: Reanalyse a boundable edge only if its domain grows.
--
-- Revision 1.24  2007/08/25 18:56:11  niklas
-- Added a nicer Image for Eval_Domain_T.
--
-- Revision 1.23  2007/08/17 14:44:00  niklas
-- BT-CH-0074: Stable and Unstable stacks.
--
-- Revision 1.22  2007/08/14 20:52:32  niklas
-- Changed Known_Values to give bounds only for given Cells.
-- Changed Bound_Calls to bound only the call's input cells.
--
-- Revision 1.21  2007/07/21 18:18:41  niklas
-- BT-CH-0064. Support for AVR/IAR switch-handler analysis.
--
-- Revision 1.20  2007/07/10 19:13:22  niklas
-- Improved Refine (Edge) to show Initial before Refined.
--
-- Revision 1.19  2007/04/18 18:34:38  niklas
-- BT-CH-0057.
--
-- Revision 1.18  2007/03/29 15:18:01  niklas
-- BT-CH-0056.
--
-- Revision 1.17  2007/03/18 12:50:38  niklas
-- BT-CH-0050.
--
-- Revision 1.16  2007/01/25 21:25:14  niklas
-- BT-CH-0043.
--
-- Revision 1.15  2007/01/13 13:51:04  niklas
-- BT-CH-0041.
--
-- Revision 1.14  2006/10/24 08:44:31  niklas
-- BT-CH-0028.
--
-- Revision 1.13  2006/09/05 18:40:56  niklas
-- Added Note on number of cells.
--
-- Revision 1.12  2006/05/06 06:59:20  niklas
-- BT-CH-0021.
--
-- Revision 1.11  2005/10/12 17:31:44  niklas
-- Extended Compute_And_Assign to show the locus of the
-- step that causes an assertion violation.
--
-- Revision 1.10  2005/08/24 10:01:30  niklas
-- Force display of the code address in warnings about unreachable
-- (infeasible) flow (as if the -address option were in effect).
--
-- Revision 1.9  2005/06/29 13:01:10  niklas
-- Added optional (-warn reach) warning about infeasible edges,
-- called "unreachable flow" in the warning.
--
-- Revision 1.8  2005/06/14 17:10:16  niklas
-- Added step context for Output in Resolve_Dynamic_Edges.
--
-- Revision 1.7  2005/05/10 13:46:50  niklas
-- Corrected Find_Stack_Heights to call P.E.Bound_Stack_Height.
--
-- Revision 1.6  2005/04/20 12:15:33  niklas
-- Corrected Warn_If_Node_Splits to skip the warning when the edge
-- loops back from the last to the first step of the node, because
-- this is not an internal edge. Also, added the edge and node
-- indices to the warning message when it occurs.
--
-- Revision 1.5  2005/02/23 09:05:18  niklas
-- BT-CH-0005.
--
-- Revision 1.4  2005/02/20 15:15:36  niklas
-- BT-CH-0004.
--
-- Revision 1.3  2005/02/16 21:11:43  niklas
-- BT-CH-0002.
--
-- Revision 1.2  2004/08/09 19:51:32  niklas
-- BT-CH-0001.
--
-- Revision 1.1  2004/04/28 18:54:41  niklas
-- First version.
--

--:dbpool with GNAT.Debug_Pools;

with Ada.Text_IO;
with Ada.Unchecked_Deallocation;

with Arithmetic.Evaluation;
with Bounds.Calling;
with Bounds.Stacking.Opt;
with Calling;
with Faults;
with Flow.Computation;
with Flow.Const.Opt;
with Flow.Pruning.Opt;
with Flow.Show;
with Least_Fixpoint;
with Output;
with Storage.Cell_Numbering;


package body Flow.Const is


   use type Computation.Model_Ref;

   package Stacking_Opt renames Bounds.Stacking.Opt;


   -- Principles of Operation
   --
   -- The analysis consists of six phases:
   --
   -- (1) Propagate the constant values for all cells through the
   --     flow-graph by a least-fixpoint data-flow solution over the
   --     domain of mappings from cell to the domain (unknown, constant
   --     value = C, variable).
   --
   -- (2) Construct a simplified computation model by partial
   --     evaluation of the given model on the computed cell values,
   --     where a cell has only a single possible value. This phase
   --     also applies such single possible values as bounds to
   --     resolve dynamic references to cells.
   --
   -- (3) Prune the simplified computation model by identifying
   --     the infeasible edges, if any, and propagating infeasibility
   --     over the flow-graph.
   --
   -- (4) Bound the protocols and inputs for the (remaining) feasible
   --     calls by applying the propagated constant values as bounds.
   --
   -- (5) Again prune the computation model in case some more calls
   --     were found to be infeasible in phase 4.
   --
   -- If phase 2 or 4 resolves dynamic cell references to static cell
   -- names, we can optionally repeat phases 1 .. 4 to include
   -- these (perhaps new) cells in the results and to improve the
   -- precision of the results. Otherwise, we go on to phase 6:
   --
   -- (6) Extract the maximum local stack height and the take-off
   --     heights for the calls from the computed stack-height
   --     values (if constant), for all stacks.
   --
   -- (7) Apply the propagated constant cell values as bounds to
   --     resolve dynamic control-flow edges.
   --
   -- If phase 7 actually resolves some dynamic flow and extends the
   -- flow-graph, the results of phases 1 .. 6 are out of date and
   -- can be discarded (but they are still computed).


   --
   --    The basic domain of abstracted values:
   --


   subtype Level_T is Arithmetic.Evaluation.Level_T;
   --
   -- The three levels in the value domain, expressing the
   -- level of knowledge we have about the possible values that
   -- a cell can take at a given point in the flow-graph.
   --
   -- The levels correspond to the evaluation levels from the
   -- Arithmetic.Evaluation package, but note carefully that their
   -- lattice order here, for constant propagation purposes, is
   -- different from their order in Arithmetic.Evaluation.
   --
   -- The meaning of the three levels here is:
   --
   -- Unknown
   --    We know nothing yet. Bottom element of value domain.
   -- Known
   --    We have seen a single value so far.
   --    Middle level of value domain (domain elements at this
   --    level are distinguished by the values, see Value_T).
   -- Relative
   --    We have so far seen only a value of the form Base + Offset,
   --    where Base is a known initial-value cell (evaluated on
   --    entry to the subprogram under analysis, and constant
   --    thereafter) and Offset is a known constant.
   --    Middle level of value domain (domain elements at this
   --    level are distinguished by the Base and Offset).
   -- Variable
   --    We have seen at least two different values, so
   --    we consider the cell to be variable (not constant)
   --    at this point. Top element of domain.

   use type Level_T;

   Unknown  : constant Level_T := Arithmetic.Evaluation.Unknown;
   Known    : constant Level_T := Arithmetic.Evaluation.Known;
   Relative : constant Level_T := Arithmetic.Evaluation.Relative;
   Variable : constant Level_T := Arithmetic.Evaluation.Variable;


   subtype Value_T is Arithmetic.Evaluation.Cell_Value_T;
   --
   -- The possible values of a cell at some point in the
   -- flow-graph, from the point of view of constant propagation.
   -- The type has a discriminant, Level, that shows the level
   -- of knowledge about the cell's value. If Level = Known,
   -- the type has a component Value : Processor.Value_T that
   -- is the known unique value of the cell.


   Unknown_Value : constant Value_T := (Level => Unknown);
   --
   -- Represents a lack of knowledge about the value of a
   -- cell at some point.
   -- The single element in the domain at the Unknown level; the
   -- bottom element in the domain.


   Variable_Value : constant Value_T := (Level => Variable);
   --
   -- Represents a cell that is not known to have a constant
   -- value (appears to have many possible values).
   -- The single element in the domain at the Variable level;
   -- the top element in the domain.


   function Image (Item : Value_T) return String
   renames  Arithmetic.Evaluation.Image;


   subtype Target_Level_T is Arithmetic.Evaluation.Target_Level_T;
   --
   -- Just an abbreviation.

   use type Target_Level_T;

   Dynamic_Ref : constant Target_Level_T := Arithmetic.Evaluation.Dynamic_Ref;
   --
   -- Just an abbreviation.


   procedure Merge (
      More : in     Value_T;
      Into : in out Value_T;
      Grew : in out Boolean)
   --
   -- Updates the Into value to define the union set of the
   -- Into value and the More value. Since we only record one
   -- possible constant value for each cell, or one possible
   -- Base cell and constant Offset, if More and Into provide
   -- different values Into becomes Variable_Value.
   --
   -- Returns Grew as True if Into was changed, otherwise
   -- does not change Grew.
   --
   is
      use type Value_T;
      use type Arithmetic.Word_T;
      use type Arithmetic.Width_T;

   begin

      if More.Level /= Unknown then
         -- We may learn something.

         case Into.Level is

         when Unknown =>
            -- We now know a little bit, whereas we
            -- knew nothing before.

            Into := More;

            Grew := True;

         when Relative =>

            if More /= Into then
               -- We knew something, but we are now told
               -- something different, so who knows...

               Into := Variable_Value;

               Grew := True;

            end if;

         when Known =>

            if        More.Level = Known
            and then (More.Value = Into.Value
            and       More.Width = Into.Width)
            then
               -- More and Into are the same value and width, so
               -- there is no change in those aspects of the value.

               if More.Signed and not Into.Signed then
                  -- More brings a hint that this value should
                  -- be considered signed. This hint is merged Into
                  -- the united value.

                  Into.Signed := True;

                  Grew := True;

               end if;

            else
               -- We knew something, but we are now told
               -- something different, so who knows...

               Into := Variable_Value;

               Grew := True;

            end if;

         when Variable =>
            -- We have already lost whatever illusions
            -- we may have had.

            null;

         end case;

      end if;

   end Merge;


   procedure Set_Single_Value (
      Bound : in     Storage.Bounds.Interval_T;
      Width : in     Arithmetic.Width_T;
      Value : in out Value_T)
   --
   -- Sets the Value to the single constant value that may be
   -- implied by the given Bound, otherwise leaves the Value
   -- unchanged.
   --
   is
      use type Arithmetic.Value_T;

      Single_Value : Arithmetic.Value_T;
      -- The single value in the Bound, if Bound is singular.

   begin

      if Storage.Bounds.Singular (Bound) then
         -- Unique value.

         Single_Value := Storage.Bounds.Single_Value (Bound);

         Value := (
            Level => Known,
            Value => Arithmetic.Unsigned_Word (
               Value => Single_Value,
               Width => Width),
            Width  => Width,
            Signed => Single_Value < 0);

      end if;

   end Set_Single_Value;


   --
   --    Assigning an abstract value to each cell:
   --


   type Numbered_Cell_Values_T is
      array (Storage.Cell_Numbering.Number_T range <>)
      of Value_T;
   --
   -- The possible constant value for each cell used in the
   -- flow-graph under analysis.
   -- This is the domain for phase 1, constant propagation,
   -- when constrained to the cell-numbers in use.
   --
   -- If the flow-graph contains dynamic data (cell) references,
   -- the possible referent cells are not included in the cell
   -- numbering. If constant propagation manages to resolve such
   -- a reference resolves to a unique cell, that cell is considered
   -- to have an undefined (Variable) value in this round of constant
   -- propagation. However, this will trigger a new round of constant
   -- propagation in which this referent cell is included in the same
   -- way as statically addressed cells.


   type Numbered_Cell_Values_Ref is access Numbered_Cell_Values_T;
   --
   -- The vectors of cell values are stored in the heap for
   -- two reasons: firstly, to make it easier to refer to the
   -- correct vector when using Arithmetic.Evaluation to evalute
   -- the expressions in a step, and secondly because such a
   -- vector is needed for each step in the flow-graph and the
   -- whole set of vectors would use a lot of stack space, which
   -- is limited on some platforms.

   --:dbpool Numbered_Cell_Values_Pool : GNAT.Debug_Pools.Debug_Pool;

   --:dbpool for Numbered_Cell_Values_Ref'Storage_Pool use Numbered_Cell_Values_Pool;


   function Image (Item : Numbered_Cell_Values_T) return String
   --
   -- Describes the cell values.
   --
   is
      use type Storage.Cell_Numbering.Number_T;
   begin

      if Item'Length = 0 then

         return "";

      else

         return
              Storage.Cell_Numbering.Number_T'Image (Item'First)
            & " =>"
            & Image (Item(Item'First))
            & ','
            & Image (Item(Item'First + 1 .. Item'Last));

      end if;

   end Image;


   procedure Unchecked_Discard
   is new Ada.Unchecked_Deallocation (
      Name   => Numbered_Cell_Values_Ref,
      Object => Numbered_Cell_Values_T);


   procedure Discard (Item : in out Numbered_Cell_Values_Ref)
   --
   -- Unchecked deallocation depending on Opt.Deallocate.
   --
   is
   begin

      if Opt.Deallocate then

         Unchecked_Discard (Item);

      end if;

   exception when others => Faults.Deallocation;

   end Discard;


   --
   --    Evaluation domain for constant propagation:
   --


   type Eval_Domain_T is new Arithmetic.Evaluation.Domain_T
   with record
      Numbers : Storage.Cell_Numbering.Map_T;
      Values  : Numbered_Cell_Values_Ref;
   end record;
   --
   -- The domain, defining some cell values, within which
   -- the expressions in a step are evaluated to propagate
   -- constant cell values to constant (or partially evaluated)
   -- expression results.


   -- overriding
   function Image (Item : Eval_Domain_T) return String;


   -- overriding
   function Basis (Item : Eval_Domain_T) return Storage.Cell_List_T;
   --
   -- The basis of an Eval Domain is the set of cells that are
   -- involved, ie. the cells with Numbers and Values.


   -- overriding
   function Value_Of (Cell : Storage.Cell_T; Within : Eval_Domain_T)
   return Value_T;
   --
   -- The value that this domain defines for the cell.


   --    Eval_Domain_T operation bodies


   function Image (Item : Eval_Domain_T) return String
   is
   begin

      pragma Warnings (Off, Item);
      -- Yes, GNAT, we know that Item is not referenced here.

      return "constant propagation";

   end Image;


   function Basis (Item : Eval_Domain_T) return Storage.Cell_List_T
   is
      use Storage;
   begin

      return Cell_List_T (Cell_Numbering.Inverse (Item.Numbers));

   end Basis;


   function Value_Of (Cell : Storage.Cell_T; Within : Eval_Domain_T)
   return Value_T
   is
      use Storage.Cell_Numbering;
   begin

      if Numbered (Cell, Within.Numbers) then
         -- This Cell's value is included in this round of constant
         -- propagation.

         return Within.Values(Number (Cell, Within.Numbers));

      else
         -- This Cell is a new one for this round, discovered as the
         -- unique referent of a dynamic pointer. Its value is not
         -- considered in this round of constant propagation but is
         -- taken as variable (in effect "not known").

         return (Level => Variable);

      end if;

   end Value_Of;


   function Numbered_Cell (
      Target : Arithmetic.Variable_T;
      Within : Eval_Domain_T)
   return Boolean
   --
   -- Whether the Target is a basis cell Within the domain.
   --
   is
      use type Arithmetic.Expr_Kind_T;
   begin

      return   Target.Kind = Arithmetic.Cell
      and then Storage.Cell_Numbering.Numbered (Target.Cell, Within.Numbers);

   end Numbered_Cell;


   function Known_Values (
      Cells  : Storage.Cell_List_T;
      Domain : Eval_Domain_T)
   return Storage.Bounds.Bounds_Ref
   --
   -- The Domain bounds on the given Cells, converted to Singular_Bounds_T
   -- and stored on the heap. If the Domain contains no Known values for
   -- these Cells the result is Storage.Bounds.No_Bounds and nothing new
   -- is stored on the heap. It may be that some Cells are not in the
   -- Domain numbering; for such cells no bounds apply.
   --
   is

      Values : Storage.Cell_Value_List_T (1 .. Cells'Length);
      Last : Natural := 0;
      -- The cells and their values will be in Values(1 .. Last).

      Cell_Value : Value_T;
      -- The Domain value for one of the Cells.

   begin

      for C in Cells'Range loop

         Cell_Value := Value_Of (Cell => Cells(C), Within => Domain);

         if Cell_Value.Level = Known then

            Last := Last + 1;

            Values(Last) := (
               Cell  => Cells(C),
               Value => Cell_Value.Value);

         end if;

      end loop;

      if Last > 0 then
         -- We have some known cell values.

         return new Storage.Bounds.Singular_Bounds_T'(
            Length => Last,
            List   => Values(1 .. Last));

      else
         -- No known cell values.

         return Storage.Bounds.No_Bounds;

      end if;

   end Known_Values;


   --
   --    Creating a new computation model:
   --


   procedure Refine (
      Assignment : in     Arithmetic.Assignment_T;
      Within     : in     Eval_Domain_T;
      Into       : in out Arithmetic.Assignment_Set_T;
      Refined    : in out Boolean;
      Bounded    : in out Boolean;
      Values     : in out Numbered_Cell_Values_T)
   --
   -- Refines a Defining Assignment, Within the domain of cell-values
   -- that flow into the assignment, to make a new assignment in which
   -- the expressions (including the condition if any) are refined by
   -- partial evaluation on the domain, as is also the target variable
   -- if it is a dynamic memory reference. The new assignment is added
   -- Into a set of (refined) assignments, unless it has become just
   -- an identity assignment that can be dropped.
   --
   -- If the assignment was changed or dropped, Refined is returned
   -- as True, otherwise Refined is returned unchanged.
   --
   -- If a dynamic data pointer was resolved or bounded, Bounded is returned
   -- as True, otherwise Bounded is returned unchanged.
   --
   -- If the evaluated Target variable is one of the cells currently under
   -- analysis, the value of the assigned expression is also stored in
   -- Values under the index number that Within assigns to this cell.
   --
   -- If Opt.Refine_Effects is False, we just evaluate and store the
   -- value in Values and do not try to refine the expressions.
   --
   is
      use Arithmetic.Evaluation;

      Target_Result : constant Target_Result_T :=
         Eval (
            Target => Assignment.Target,
            Within => Within,
            Partly => Opt.Refine_Effects);
      --
      -- The result of evaluating the target.

      Target : constant Arithmetic.Variable_T := Residual (Target_Result);
      --
      -- The partially evaluated (dereferenced) target variable.

      Result : constant Assignment_Result_T :=
         Eval (
            Assignment => Assignment,
            Within     => Within,
            Target     => Target_Result,
            Partly     => Opt.Refine_Effects);
      --
      -- The partially evaluated assignment.

      Target_Number : Storage.Cell_Numbering.Number_T;
      --
      -- The (number of) the target cell of an assignment where the
      -- target is a statically named or resolved cell and this cell
      -- belongs to the nomenclatura (range of Within).

   begin

      -- Perhaps add the assignment Into the new effect:

      if Identity (Result) then
         -- This assignment can be ignored.

         Refined := True;

      else
         -- The assignment must be kept.

         if not Same (Result, Assignment) then
            -- Some real refinement.

            Refined := True;

         end if;

         Arithmetic.Add (To => Into, More => Residual (Result));

      end if;

      -- Note bounded pointers:

      if Result.Ref_Bounded then

         Output.Note ("Refine (Assignment) bounded a pointer.");

         Bounded := True;

      end if;

      -- Perhaps store the assigned value in Values(Target):

      if Numbered_Cell (Target, Within) then
         -- The (possibly resolved) target is a cell, and moreover one
         -- of the cells involved in the constant propagation, so we
         -- will return the assigned value in Values.

         Target_Number := Storage.Cell_Numbering.Number (
             Target.Cell, Within.Numbers);

         Values(Target_Number) := Value_Of (Result.Value);

      end if;

   end Refine;


   procedure Warn_If_Node_Splits (
      Edge    : in Step_Edge_T;
      Graph   : in Graph_T;
      Program : in Programs.Program_T)
   --
   -- Warns if the infeasible Edge splits a node in the Graph.
   -- Such a split is unexpected and suspicious, because edges within
   -- a node should usually have a True precondition.
   --
   is

      Source_Node : constant Node_T := Node_Containing (Source (Edge), Graph);
      Target_Node : constant Node_T := Node_Containing (Target (Edge), Graph);
      -- The source and target nodes of the Edge.

   begin

      if       Source_Node = Target_Node
      and then Target (Edge) /= First_Step (Target_Node)
      then
         -- The Edge is internal to a node.

         Output.Warning (
            Locus => Flow.Show.Locus (
               Node   => Source_Node,
               Source => Programs.Symbol_Table (Program)),
            Text =>
                 "Infeasible edge"
               & Step_Edge_Index_T'Image (Index (Edge))
               & " splits node"
               & Node_Index_T'Image (Index (Source_Node)));

      -- else
      --    If Source_Node /= Target_Node there is no problem.
      --    Otherwise, the Edge goes from the last step of the node back
      --    to the first step of the node, so the node is the body of a
      --    loop. The infeasibility of the edge simply means that the loop
      --    is unrepeatable. No problem, in fact rather nice.
      --

      end if;

   end Warn_If_Node_Splits;


   procedure Refine (
      Edge    : in     Step_Edge_T;
      Within  : in     Eval_Domain_T;
      Program : in     Programs.Program_T;
      Model   : in out Computation.Model_Ref;
      Bounded : in out Boolean)
   --
   -- Refines the precondition of an edge, under a given computation
   -- Model and Within the domain of cell-values that flow out from the
   -- source step, to make a new precondition expression that is refined
   -- by partial evaluation on the domain.
   --
   -- The new precondition is stored in the refined computation Model.
   --
   -- If a dynamic data pointer is resolved or bounded, Bounded is returned
   -- as True, otherwise Bounded is returned unchanged.
   --
   -- A warning is issued if the Edge is internal to a node and becomes
   -- infeasible.
   --
   -- The Program parameter is only for showing loci.
   --
   is
      use Arithmetic.Evaluation;
      use type Arithmetic.Expr_Ref;

      Old_Cond : constant Arithmetic.Condition_T :=
         Computation.Condition (Edge => Edge, Under => Model);
      -- The old precondition.

      Result : Result_T;
      -- The (partially) evaluated old precondition.

      New_Cond : Arithmetic.Condition_T;
      -- The new condition, if refined.

   begin

      if  Old_Cond /= Arithmetic.Always
      and Old_Cond /= Arithmetic.Never
      and Old_Cond /= Arithmetic.Unknown
      then
         -- Partial evaluation may help.

         Result := Eval (
            Expr   => Old_Cond,
            Within => Within,
            Partly => True);

         if Result.Ref_Bounded then

            Output.Note ("Refine (Edge) bounded a pointer.");

            Bounded := True;

         end if;

         if not Same (Result, Old_Cond) then
            -- The condition changed.

            New_Cond := To_Condition (Result);

            if Opt.Trace_Refinements then

               Output.Trace (
                    "Initial condition"
                  & Output.Field_Separator
                  & "Edge"
                  & Step_Edge_Index_T'Image (Index (Edge))
                  & Output.Field_Separator
                  & Arithmetic.Image (Old_Cond));

               Output.Trace (
                    "Refined condition"
                  & Output.Field_Separator
                  & "Edge"
                  & Step_Edge_Index_T'Image (Index (Edge))
                  & Output.Field_Separator
                  & Arithmetic.Image (New_Cond));

            end if;

            Computation.Set_Condition (
               On    => Edge,
               To    => New_Cond,
               Under => Model);

            if New_Cond = Arithmetic.Never then
               -- The edge became infeasible. Hmm.

               Output.Note (
                    "Infeasible flow to "
                  & Output.Image (Flow.Show.All_Statements (
                       Step    => Target (Edge),
                       Source  => Programs.Symbol_Table (Program)),
                       Address => True));

               if Pruning.Opt.Warn_Unreachable then

                  Output.Warning (
                       "Unreachable flow to instruction at "
                     & Output.Image (
                          Item   => Flow.Show.All_Statements (
                             Step    => Target (Edge),
                             Source  => Programs.Symbol_Table (Program)),
                             Address => True));

               end if;

               Warn_If_Node_Splits (
                  Edge    => Edge,
                  Graph   => Computation.Graph (Model),
                  Program => Program);

            end if;

         end if;

      end if;

   end Refine;


   procedure Trace_Refined_Effect (
      Step    : in Step_T;
      Locus   : in Output.Locus_T;
      Initial : in Arithmetic.Effect_T;
      Refined : in Arithmetic.Effect_T)
   --
   -- Traces the refinement of the effect of the Step.
   --
   is
   begin

      Output.Trace (
         Locus => Locus,
         Text  =>
               "Initial effect"
            & Output.Field_Separator
            & "Step"
            & Step_Index_T'Image (Index (Step))
            & Output.Field_Separator
            & Arithmetic.Image (Initial));

      Output.Trace (
         Locus => Locus,
         Text  =>
              "Refined effect"
            & Output.Field_Separator
            & "Step"
            & Step_Index_T'Image (Index (Step))
            & Output.Field_Separator
            & Arithmetic.Image (Refined));

   end Trace_Refined_Effect;


   procedure Refine (
      Step    : in     Step_T;
      Domain  : in out Eval_Domain_T;
      Post    : in     Numbered_Cell_Values_Ref;
      Program : in     Programs.Program_T;
      Model   : in out Computation.Model_Ref;
      Bounded : in out Boolean)
   --
   -- Refines (simplifies, partially evaluates) the effect of a Step,
   -- including dynamic data accesses, and the preconditions on the
   -- edges that leave the Step, under a given computation Model and
   -- a given Domain of cell-values that flow into the step.
   --
   -- All the refinements are optional depending on Opt.Refine_Effects
   -- and Opt.Refine_Edge_Conditions.
   --
   -- The Post parameter is a working area with the same Range
   -- as Domain.Values. Its contents on entry are irrelevant,
   -- and it will be returned as the Post values holding after
   -- the Step. For reasons of convenience, Domain.Values will
   -- be set to Post on return; this is why Domain is "in out".
   --
   -- The new effect etc. are stored in the refined computation Model.
   --
   -- If some dynamic data access pointer is resolved to a unique cell,
   -- or is bounded to a narrower range, the Bounded parameter is set
   -- True, otherwise it is returned unchanged.
   --
   is
      use Arithmetic;

      Old_Effect : constant Effect_Ref :=
         Computation.Effect (Step => Step, Under => Model);
      -- The old effect of the step.

      New_Effect : Assignment_Set_T (Positive_Length (Old_Effect));
      -- The new, refined effect of the step.

      Effect_Refined : Boolean := False;
      -- Whether the effect was really changed.

      Edges : constant Step_Edge_List_T :=
         Computation.Edges_From (Step => Step, Under => Model);
      -- The edges leaving the Step, feasible under the Model.
      -- We will refine the preconditions on these edges.

      Step_Mark : Output.Nest_Mark_T;
      -- Marks the default Output locus for the step.

   begin

      Step_Mark := Output.Nest (
         Flow.Show.Locus (
            Step   => Step,
            Source => Programs.Symbol_Table (Program)));

      -- Refine the effect and compute the Post values:

      Post.all := Domain.Values.all;

      if Opt.Refine_Effects or Opt.Refine_Edge_Conditions
      then
         -- If the edge conditions will be refined, we must
         -- compute the Post values even if the effect is not
         -- to be refined.

         for E in Old_Effect'Range loop

            if Old_Effect(E).Kind in Defining_Kind_T then
               -- Currently we refine only Defining assignments.

               Refine (
                  Assignment => Old_Effect(E),
                  Within     => Domain,
                  Into       => New_Effect,
                  Refined    => Effect_Refined,
                  Bounded    => Bounded,
                  Values     => Post.all);

            else
               -- Range constraint assignments are currently kept
               -- in their original form.

               -- TBA refine the Min and Max expressions in
               -- the range constraint Old_Effect(E).

               -- TBA refine (resolve) the Target of Old_Effect(E).

               Add (New_Effect, Old_Effect(E));

            end if;

         end loop;

         if Effect_Refined and Opt.Refine_Effects then
            -- The effect was refined and refinement is enabled.
            -- Therefore we shall record the new, refined effect
            -- for later use.

            if Opt.Trace_Refinements then

               Trace_Refined_Effect (
                  Step  => Step,
                  Locus => Flow.Show.Locus (
                     Step   => Step,
                     Source => Programs.Symbol_Table (Program)),
                  Initial => Old_Effect.all,
                  Refined => To_Effect (New_Effect));

            end if;

            Computation.Set_Effect (
               Step  => Step,
               To    => To_Effect_Ref (New_Effect),
               Under => Model);

         end if;

      end if;

      -- Refine the edge preconditions:

      Domain.Values := Post;

      if Opt.Refine_Edge_Conditions then

         for E in Edges'Range loop

            Refine (
               Edge    => Edges(E),
               Within  => Domain,
               Program => Program,
               Model   => Model,
               Bounded => Bounded);

         end loop;

      end if;

      Output.Unnest (Step_Mark);

   exception

   when others =>
       -- Clean up the output locus and punt:

      Output.Unnest (Step_Mark);

      raise;

   end Refine;


   --
   --    Components for Least_Fixpoint for constant propagation:
   --


   procedure Trace (
      Action  : in String;
      Step    : in Step_T;
      Source  : in Step_T;
      Input   : in Numbered_Cell_Values_T;
      Output  : in Numbered_Cell_Values_T;
      Grew    : in Boolean)
   --
   -- Report an "initialisation", "transform", or "merging" action.
   --
   is
      use Ada.Text_IO;
   begin

      Put (
          "Constant Propagation:"
        & Action
        & ":Step:" & Step_Index_T'Image (Index (Step)));

      if Source /= null then
         Put (":Source_Step:" & Step_Index_T'Image (Index (Source)));
      end if;

      New_Line;

      Put_Line ("- Input  : " & Image (Input));
      Put_Line ("- Output : " & Image (Output));
      Put_Line ("- Grew   : " & Boolean'Image (Grew));

   end Trace;


   subtype Step_Number_T is Positive;
   --
   -- The number of a step in the flow-graph, as required for
   -- Least_Fixpoint. The value is the same as the Step_Index,
   -- only the type is different.


   function Number (Step : Step_T) return Step_Number_T
   --
   -- The index of a step, as a Step_Number_T for Least_Fixpoint.
   --
   is
   begin

      return Step_Number_T (Index (Step));

   end Number;


   procedure Propagate_Constants (
      Subprogram : in     Programs.Subprogram_T;
      Asserted   : in     Storage.Bounds.Var_Interval_List_T;
      Initial    : in     Storage.Bounds.Cell_Interval_List_T;
      Round      : in     Positive;
      Last_Round : in     Boolean;
      Bounded    :    out Boolean;
      Bounds     : in     Programs.Execution.Bounds_Ref)
   --
   -- Implements the provided operation Propagate, when
   -- Opt.Propagate is True. However, this operation executes phases 1
   -- to 4 once; the iteration over these phases is implemented in the
   -- caller, because the types and local variables defined here are
   -- sized for a certain set of cells and must be elaborated anew if
   -- new (resolved) cells are added.
   --
   -- Asserted
   --    Bounds on variable (cell) values, throughout the subprogram.
   --
   -- Initial
   --    The bounds on the initial values of cells, on entry to the
   --    Subprogram. We assume that the Asserted bounds are included
   --    (conjoined) with the specific entry-point bounds, and both
   --    are represented here.
   --
   -- Round
   --    The number of the iteration round.
   --
   -- Last_Round
   --    Whether this is the last round of iteration of phases 1 .. 4.
   --    In other words, even if Bounded is returned as True, the caller
   --    will not repeat this Propagate_Constants operation because of
   --    Bounded. Consequently, when Last_Round is True this operation
   --    will perform phases 5 and 6 even if Bounded is returned as True.
   --
   -- Bounded
   --    Whether some data pointers (dynamic data references) were
   --    resolved to unique cell referents, or bounded (constrained)
   --    to narrower pointers, in the refined model. Also true if some
   --    calling protocol was refined and thus the effect of a call was
   --    updated.
   --
   -- If Bounded is True it means, first, that the refined model is
   -- different from the input model, and second, that a new round of
   -- Propagate_Constants may be useful because there are new cells that
   -- may have constant values, or new assignments or uses of existing
   -- cells, or because reduced aliasing may give a better result.
   --
   is

      -- Constant propagation is implemented by least-fixpoint
      -- iteration of data-flow equations along the flow (forward
      -- direction). Thus, the results are the constant cell values
      -- (if any) before each step.

      use type Storage.Cell_T;
      use type Storage.Bounds.Cell_Interval_List_T;
      use type Storage.Bounds.Var_Interval_List_T;


      Program : constant Programs.Program_T :=
         Programs.Execution.Program (Bounds);
      --
      -- The target program under analysis, for show.

      Model : Flow.Computation.Model_Ref renames
         Programs.Execution.Computation (Bounds).all;
      --
      -- The computation model to be used and updated.

      Graph : constant Graph_T := Computation.Graph (Model);
      --
      -- The flow-graph underlying the computation model.
      -- Only the step-level graph will be used.

      Steps : constant Step_List_T := All_Steps (Graph);
      --
      -- All the steps of the graph (including infeasible steps).
      -- Note that the indices in this vector are equal to the
      -- Step_Index_T values of the steps, although of Positive type.
      -- In other words, this array can be indexed by Step_Number_T.
      -- Infeasible steps are not processed, but they are included in
      -- the list to have this indexing property.

      Code_Range : constant Storage.Code_Address_Range_T :=
         Flow.Code_Range (Graph);
      --
      -- A code-address interval that covers the Graph.
      -- For checking which bounded variables are located in the same
      -- cells throughout the subprogram. This is a weak way of using
      -- the Asserted variable bounds; TBM to a localized one.

      Named_Cells : Storage.Cell_Set_T := Computation.Cells_In (
         Model => Model,
         Calls => True);
      --
      -- All the cells (statically) named in the given computation model,
      -- including the output cells of calls as far as they are encoded
      -- in the effects of the call steps.
      --
      -- The input cells of calls are not included, but if such a cell
      -- is not named in the computation model itself then we have no
      -- reason to propagate values for it.
      --
      -- TBA: include those input cells for calls that are bounded in
      -- the initial bounds or asserted for this subprogram.

      Domain : Eval_Domain_T := (
         Numbers => Storage.Cell_Numbering.Map (Named_Cells),
         Values  => null);
      --
      -- The Numbers component defines a (fixed) consecutive numbering
      -- of all the cells (statically) named in the computation. This
      -- provides each such cell with an index position in the vectors
      -- of abstract cell values.
      --
      -- This Domain variable is used as a global work-space in most
      -- of the least-fixpoint iteration operations. The Numbers
      -- component is held fixed; the Values component is set as
      -- needed.
      --
      -- The Values component will be set to the Cell_Values flowing
      -- into a step, when we propagate these values through the
      -- assignments in the step.
      --
      -- The Values component will be set to the Cell_Values flowing
      -- from a step, when we compute the preconditions on edges
      -- that leave this step.

      Cell_Num : constant Storage.Cell_Numbering.List_T :=
         Storage.Cell_Numbering.Inverse (Domain.Numbers);
      --
      -- The cell for each cell-number.

      subtype Cell_Number_T is
         Storage.Cell_Numbering.Number_T range Cell_Num'Range;
      --
      -- The number of a cell in the consecutive numbering of
      -- of all the (statically) named cells in the flow-graph.

      subtype Cell_Values_T is Numbered_Cell_Values_T (Cell_Number_T);
      --
      -- The abstract value for each cell, indexed by the consecutive
      -- numbers assigned to cells (statically) named in the computation.

      subtype Cell_Values_Ref is Numbered_Cell_Values_Ref;
      --
      -- The accessed object will always be of subtype Cell_Values_T.

      type Cell_Values_List_T is
         array (Step_Number_T range <>) of Cell_Values_Ref;
      --
      -- Abstract cell values for each step in the flow-graph.
      -- This is the result of the least-fixpoint iteration.

      Values : Cell_Values_List_T (Steps'Range);
      --
      -- For each step, the propagated abstract cell values
      -- on reaching the step through any kind of edge. For
      -- the entry step, the values hold on entering the
      -- subprogram.

      Work_Post : Cell_Values_Ref := new Cell_Values_T;
      --
      -- A working array of cell values, to be used in phase 2
      -- where the new computation model is constructed.

      subtype Default_T is Level_T range Unknown .. Relative;
      --
      -- Specifies a default level of knowledge for a cell.


      function Default_Value (
         Cell    : Storage.Cell_T;
         Default : Default_T)
      return Value_T
      --
      -- The Default value for the given Cell.
      --
      is
         use type Storage.Cell_T;

         Init : constant Storage.Cell_T := Storage.Initial_Cell (Cell);
         -- The initial-value cell for the given Cell, or No_Cell
         -- if there is none.

      begin

         case Default is

         when Unknown =>

            return Unknown_Value;

         when Variable =>

            return Variable_Value;

         when Relative =>

            if Opt.Relative_Values and Init /= Storage.No_Cell then
               -- Use the Relative model.

               return (
                  Level  => Relative,
                  Base   => Init,
                  Offset => 0);

            else
               -- Relative values are denied, or the Cell has no
               -- initial-value cell, so we use a Variable model
               -- instead.

               return Variable_Value;

            end if;

         end case;

      end Default_Value;


      function To_Values (
         Given   : Storage.Bounds.Cell_Interval_List_T;
         Default : Default_T)
      return Cell_Values_T
      --
      -- Returns a set of abstract cell values that contains those
      -- single values that are defined by the Given bounds and
      -- marks the other cells with a given default value.
      --
      -- If the same cell is listed twice in the Given list,
      -- the last occurrence applies.
      --
      is
         use Arithmetic;

         Num : Cell_Number_T;
         -- The number of a cell that is listed in the Given
         -- bounds and is also named in the computation.

         Values : Cell_Values_T;
         -- The result.

      begin

         -- First make everything default:

         for B in Values'Range loop

            Values(B) := Default_Value (Cell_Num(B), Default);

         end loop;

         -- Then add the given bounds:

         for G in Given'Range loop

            if Storage.Is_Member (Given(G).Cell, Named_Cells) then
               -- This cell is named in the computation.

               Num := Storage.Cell_Numbering.Number (
                  Cell  => Given(G).Cell,
                  Under => Domain.Numbers);

               Set_Single_Value (
                  Bound => Given(G).Interval,
                  Width => Storage.Width_Of (Given(G).Cell),
                  Value => Values(Num));

            end if;

         end loop;

         return Values;

      end To_Values;


      function To_Values (
         Given   : Storage.Bounds.Var_Interval_List_T;
         Default : Value_T)
      return Cell_Values_T
      --
      -- Returns a set of abstract cell values that contains those
      -- single values that are defined by the Given variable bounds
      -- for the _whole_ subprogram (fixed-location variables) and
      -- marks the other cells with a given default value.
      --
      -- This is a weak way of using the Asserted variable bounds; TBM
      -- to a localized one.
      --
      -- If the same cell is listed twice in the Given list,
      -- the last occurrence applies.
      --
      is
         use Arithmetic;
         use type Storage.Code_Address_Range_T;

         Loc : Storage.Location_Ref;
         -- The location of one of the bounded variables.

         Num : Cell_Number_T;
         -- The number of a cell that is listed in the Given
         -- bounds and is also named in the computation.

         Values : Cell_Values_T;
         -- The result.

      begin

         -- First make everything default:

         for B in Values'Range loop

            Values(B) := Default;

         end loop;

         -- Then add the given bounds:

         for G in Given'Range loop

            Loc := Given(G).Location;

            for L in Loc'Range loop

               if       Code_Range <= Loc(L).Address
               and then Storage.Is_Member (Loc(L).Cell, Named_Cells)
               then
                  -- The bounded variable is located in Loc(L).Cell through
                  -- the whole subprogram, and this cell is named in the
                  -- computation.

                  Num := Storage.Cell_Numbering.Number (
                     Cell  => Loc(L).Cell,
                     Under => Domain.Numbers);

                  Set_Single_Value (
                     Bound => Given(G).Interval,
                     Width => Storage.Width_Of (Loc(L).Cell),
                     Value => Values(Num));

               end if;

            end loop;

         end loop;

         return Values;

      end To_Values;


      Invariant_Values : constant Cell_Values_T :=
         To_Values (
            Given   => Asserted,
            Default => Unknown_Value);
      --
      -- The cell values that are asserted to hold at all points
      -- in the subprogram. For other cells, we know nothing but
      -- are prepared to know more.
      --
      -- This is the actual "bottom" element in the least-fixpoint
      -- domain, representing our least possible knowledge of the
      -- values of the relevant cells.


      Pass_Cell : array (Cell_Number_T) of Boolean := (others => True);
      --
      -- Workspace for marking the cells that are to be passed through
      -- a step, from Pre to Post.
      -- Used in Compute_And_Assign, but kept in a global variable to
      -- retain the initial value (all True) which is also restored on
      -- exit from Compute_And_Assign.


      function Feasible_Successors (
         After  : Step_T;
         Post   : Cell_Values_Ref;
         Within : Computation.Model_Ref)
      return Step_List_T
      --
      -- The "successors" operation for Least_Fixpoint used for constant
      -- value propagation.
      --
      -- Returns the successor steps After the given step, omitting those
      -- successors for which the edge precondition evaluates to False
      -- under the currently known cell values (Post) flowing from the
      -- After step. Only the successors that are feasible Within the
      -- given computation model are considered.
      --
      is
         use Arithmetic.Evaluation;
         use type Arithmetic.Expr_Ref;

         Edges : constant Step_Edge_List_T :=
            Computation.Edges_From (After, Within);
         -- The edges leaving the step and feasible Within this model.

         Targets : Step_List_T (1 .. Edges'Length);
         Last : Natural := 0;
         -- The accepted successor steps are Targets(1 .. Last).

         Cond : Arithmetic.Condition_T;
         -- The precondition of the edge under consideration.

         Result : Result_T;
         -- The result of evaluating Cond under the Post values.

         Feasible : Boolean;
         -- Whether the edge is feasible under the Post values.

      begin

         Domain.Values := Post;

         for E in Edges'Range loop

            Cond := Computation.Condition (
               Edge  => Edges(E),
               Under => Model);

            if Cond = Arithmetic.Always then
               -- The most common and also easy case.

               Feasible := True;

            else

               Result := Eval (
                  Expr   => Cond,
                  Within => Domain,
                  Partly => False);

               Feasible := To_Condition (Result) /= Arithmetic.Never;

            end if;

            if Feasible then
               -- Accept this edge as leading to a successor.

               Last := Last + 1;

               Targets(Last) := Target (Edges(E));

            elsif Opt.Trace_Iteration then

               Output.Trace (
                    "Constant propgation ignores infeasible edge"
                  & Step_Edge_Index_T'Image (Index (Edges(E))));

            end if;

         end loop;

         return Targets(1 .. Last);

      end Feasible_Successors;


      procedure Compute_And_Assign (
         Pre  : in     Cell_Values_Ref;
         Via  : in     Step_T;
         Post : in out Cell_Values_T;
         Grew :    out Boolean)
      --
      -- The "transform" operation for Least_Fixpoint used for constant
      -- value propagation.
      --
      -- Interprets the arithmetic expressions and Defining assignments
      -- in the step, as constrained by the values (Pre) flowing into
      -- the step, and updates the values (Post) flowing out from the
      -- step to include the possible assigned values and the flow-
      -- through values of the other cells.
      --
      -- Ignores assignments to volatile cells and instead lets the Pre
      -- value flow through to the Post value.
      --
      -- Uses the (global) array Pass_Cell as workspace, with the
      -- pre- and postcondition that it is filled with True values.
      --
      is
         use type Arithmetic.Word_T;
         use type Arithmetic.Expr_Kind_T;

         Effect : constant Arithmetic.Effect_Ref :=
            Computation.Effect (Step => Via, Under => Model);
         -- The effect of the step under the given computation model.

         Target_Result : Arithmetic.Evaluation.Target_Result_T;
         -- The result of evaluating the target of an assignment.

         Target : Cell_Number_T;
         -- The (number of) the target cell of an assignment,
         -- assuming that the target variable is a unique cell.

         Result : Arithmetic.Evaluation.Assignment_Result_T;
         -- The result of evaluating an assignment.

         Value : Value_T;
         -- The value of the result, as constrained by the
         -- invariant assertions.

         Invariant : Value_T;
         -- The invariant value (if any) asserted for the target.

      begin

         -- Evaluate each assignment expression in the step within
         -- the domain of cell values defined by Pre, and propagate
         -- the assigned abstract value to Post.

         Domain.Values := Pre;

         Grew := False;

         -- Apply the effect:

         for E in Effect'Range loop

            if Effect(E).Kind in Arithmetic.Defining_Kind_T then
               -- A defining assignment.

               Target_Result := Arithmetic.Evaluation.Eval (
                  Target => Effect(E).Target,
                  Within => Domain,
                  Partly => False);

               if Target_Result.Level = Dynamic_Ref then
                  -- The target is a dynamic reference and not resolved.

                  if Opt.Trace_Iteration then

                     Output.Trace (
                          "Constant propagation ignores assignment "
                        & "to dynamic target"
                        & Output.Field_Separator
                        & Arithmetic.Image (Effect(E)));

                  end if;

                  null;  -- TBA aliasing effects.

               elsif not Storage.Is_Member (Target_Result.Cell, Named_Cells)
               then
                  -- Target pointer resolved to a new cell, not in Named_Cells.

                  if Opt.Trace_Iteration then

                     Output.Trace (
                          "Constant propagation ignores assignment "
                        & "to novel cell"
                        & Output.Field_Separator
                        & Arithmetic.Image (Effect(E)));

                  end if;

                  null;  -- TBA something?

               elsif Storage.Is_Volatile (Target_Result.Cell) then
                  -- An assignment to a volatile cell, to be ignored.
                  -- The value of the cell (if known, i.e. if asserted) is
                  -- passed through to the Post values.

                  if Opt.Trace_Iteration then

                     Output.Trace (
                          "Constant propagation ignores assignment "
                        & "to volatile cell"
                        & Output.Field_Separator
                        & Arithmetic.Image (Effect(E)));

                  end if;

               else
                  -- The target is a known non-volatile cell and one of
                  -- the cells for which we propagate values.

                  Target :=
                     Storage.Cell_Numbering.Number (
                        Cell  => Target_Result.Cell,
                        Under => Domain.Numbers);

                  if Post(Target).Level /= Variable then
                     -- We can perhaps add information to Post(Target).

                     Result := Arithmetic.Evaluation.Eval (
                        Assignment => Effect(E),
                        Within     => Domain,
                        Target     => Target_Result,
                        Partly     => False);

                     Value := Arithmetic.Evaluation.Value_Of (Result.Value);

                     -- Constrain the result with the asserted invariant:

                     Invariant := Invariant_Values(Target);

                     if Invariant.Level = Known then
                        -- This cell has an asserted invariant value.

                        if Value.Level = Variable then
                           -- Appears variable, but really invariant.
                           -- TBD if Value.Level = Relative.

                           Value := Invariant;

                        elsif    Value.Level  = Known
                        and then Value.Value /= Invariant.Value
                        then
                           -- Invariant violation!

                           Output.Warning (
                              Locus => Show.Locus (
                                 Step   => Via,
                                 Source => Programs.Symbol_Table (Subprogram)),
                              Text =>
                                   "Assertion violated in step"
                                 & Step_Index_T'Image (Index (Via))
                                 & Output.Field_Separator
                                 & Arithmetic.Image (Effect(E).Target)
                                 & " := "
                                 & Arithmetic.Image (Value.Value)
                                 & " /= "
                                 & Arithmetic.Image (Invariant.Value));

                           Value := Variable_Value;

                        end if;

                     end if;

                     -- Merge the result into the Post values:

                     Merge (
                        More => Value,
                        Into => Post(Target),
                        Grew => Grew);

                  end if;

                  Pass_Cell(Target) := False;
                  --
                  -- Whether or not this defining assignment is evaluated,
                  -- the cell value is not to be passed from Pre to Post,
                  -- because either it was assigned by Evaluate to Post, or
                  -- Post is already Variable.

               end if;

            else
               -- A range constraint assignment.

               null;  -- TBA evaluate and use Range_Kind assignments.

            end if;

         end loop;

         -- Pass the cells that are not assigned by Effect, unless
         -- the cell may be aliased by an assigned cell in which case
         -- pass a Variable value:

         for C in Cell_Number_T loop

            if not Pass_Cell(C) then
               -- The cell was assigned in this step, and the assigned
               -- value was merged into Post(C).
               -- Restore Pass_Cell to all True:

               Pass_Cell(C) := True;

            elsif   Invariant_Values(C).Level = Known
            or else Storage.Is_Volatile (Cell_Num(C))
            then
               -- The cell has an asserted invariant value, or is
               -- volatile. For a volatile cell, we want to propagate
               -- a possible asserted initial value to all program
               -- points. Thus, in both cases, the value of the cell
               -- passes from Pre to Post:

               if Opt.Trace_Iteration then

                  Output.Trace (
                       "Constant propagation merging invariant or "
                     & "volatile cell "
                     & Storage.Image (Cell_Num(C)));

               end if;

               Merge (
                  More => Pre(C),
                  Into => Post(C),
                  Grew => Grew);

            elsif Arithmetic.May_Alias (
                     Cell   => Cell_Num (C),
                     Effect => Effect.all)
                -- TBA alias-range for calls if not in Effect call-step.
            then
               -- The cell is not assigned in this step, and does not
               -- have an invariant asserted value, and is at risk from
               -- aliasing with the assignment targets. This means that
               -- its value becomes unknown (variable) in this step.

               if Opt.Trace_Iteration then

                  Output.Trace (
                       "In constant propagation, aliasing in step"
                     & Step_Index_T'Image (Index (Via))
                     & " hides cell"
                     & Output.Field_Separator
                     & Storage.Image (Cell_Num (C)));

               end if;

               Merge (
                  More => Variable_Value,
                  Into => Post(C),
                  Grew => Grew);

            else
               -- The cell is not assigned in this step, does not have
               -- an invariant asserted value, and is neither volatile
               -- nor at risk from aliasing with the assignment targets.
               -- This means that its value passes through, unchanged,
               -- from Pre to Post.

               Merge (
                  More => Pre(C),
                  Into => Post(C),
                  Grew => Grew);

            end if;

         end loop;

      end Compute_And_Assign;


      procedure Compute_And_Assign_Ref (
         Pre  : in     Cell_Values_Ref;
         Via  : in     Step_T;
         Post : in out Cell_Values_Ref;
         Grew :    out Boolean)
      --
      -- The "transform" operation for Least_Fixpoint used for constant
      -- value propagation. Same as Compute_And_Assign but the Post
      -- parameter is a reference (access) as Least_Fixpoint requires
      -- in our instance.
      --
      is
      begin

         Compute_And_Assign (
            Pre  => Pre,
            Via  => Via,
            Post => Post.all,
            Grew => Grew);

         if Opt.Trace_Iteration then

             Trace (
                Action  => "Trans",
                Step    => Via,
                Source  => null,
                Input   => Pre.all,
                Output  => Post.all,
                Grew    => Grew);

         end if;

      end Compute_And_Assign_Ref;


      procedure Initialize_Values (
         Via   : in     Step_T;
         Pre   :    out Cell_Values_Ref;
         Post  :    out Cell_Values_Ref;
         Grew  :    out Boolean)
      --
      -- The "initialize" operation for Least_Fixpoint for constant
      -- value propagation.
      --
      -- Initializes the cell values that flow into the step (Pre) to
      -- be bounded only by the global assertions, and the cell
      -- values after the step (Post) by interpreting the assignments
      -- in the step on the initialized values that flow into the
      -- step.
      --
      -- As a special case, for the entry step the Initial bounds
      -- (entry to subprogram) and also taken into account in the
      -- flow into the step.
      --
      is
      begin

         -- Initialize the values flowing into the step:

         if Via = Entry_Step (Graph) then
            -- The initial bounds apply (only) to the entry step.
            -- The globally asserted bounds also apply.

            Pre := new Cell_Values_T'(
               To_Values (
                  Given   => Initial,
                  Default => Relative));
            --
            -- For those cells that are not hereby constrained to
            -- single values, a variable or relative value is fed
            -- into the flow-graph from the caller.

         else
            -- General case: not entry step.
            -- Only the globally asserted bounds apply.

            Pre := new Cell_Values_T'(Invariant_Values);

         end if;

         -- TBA/TBC other assertions: calls, loops.

         -- Effect of step on values after the step:

         Post := new Cell_Values_T'(Invariant_Values);

         Compute_And_Assign (
            Pre  => Pre,
            Via  => Via,
            Post => Post.all,
            Grew => Grew);
         --
         -- TBC if Grew is correct if Via is the entry step
         -- and there is an edge from Via to itself, meeting
         -- the Pre value taken from Initial and Asserted and
         -- not just Invariant_Values which is the reference
         -- value of Post visavi Grew.

         if Opt.Trace_Iteration then

             Trace (
                Action  => "Init",
                Step    => Via,
                Source  => null,
                Input   => Pre.all,
                Output  => Post.all,
                Grew    => Grew);

         end if;

      end Initialize_Values;


      -- TBA verification of computed constant values against
      -- Asserted bounds (not just against Invariant_Values).


      procedure Merge_Values (
         Source : in     Step_T;
         Post   : in     Cell_Values_Ref;
         Target : in     Step_T;
         Pre    : in out Cell_Values_Ref;
         Grew   :    out Boolean)
      --
      -- The "merge" operation for Least_Fixpoint for constant
      -- propagation.
      --
      -- Updates the cell values that flow into the Target step (Pre),
      -- using a new set of cell values (Post) generated from the
      -- Source step, which is a predecessor of Target.
      --
      is
      begin

         Grew := False;
         -- Default value unless changes noted below.

         for N in Cell_Number_T loop

            Merge (
               More => Post(N),
               Into => Pre (N),
               Grew => Grew);

         end loop;

         if Opt.Trace_Iteration then

            Trace (
               Action  => "Merge",
               Step    => Target,
               Source  => Source,
               Input   => Post.all,
               Output  => Pre.all,
               Grew    => Grew);

         end if;

      end Merge_Values;


      procedure Finalize (Item : in out Cell_Values_Ref)
      --
      -- The "Finalize" operation for Least_Fixpoint.
      --
      is
      begin

         Discard (Item);
         -- Deallocate the vector of cell values.

      end Finalize;



      function Constant_Cell_Values
      is new Least_Fixpoint (
         Graph       => Computation.Model_Ref,
         Node        => Step_T,
         Node_List   => Step_List_T,
         Index_Of    => Number,
         Successors  => Feasible_Successors,
         Value       => Cell_Values_Ref,
         Value_List  => Cell_Values_List_T,
         Initialize  => Initialize_Values,
         Transform   => Compute_And_Assign_Ref,
         Merge       => Merge_Values,
         Finalize    => Finalize);
      --
      -- Instance of Least_Fixpoint for constant propagation.



      procedure Show_Cell_Values
      --
      -- Displays the computed value information.
      --
      is
         use Ada.Text_IO;

         Cell_Value_Col : constant := 10;
         -- Column for the first cell value, in the cell value table.

         Cell_Value_Width : constant := 8;
         -- The width of each column in the cell value table.

         Value_Col : Positive_Count;
         -- Current column in cell value table.

      begin

         New_Line;

         Put_Line (
              "Constant propagation results for "
            & Programs.Name (Subprogram));

         New_Line;
         Put_Line (
              Arithmetic.Evaluation.Unknown_Image
            & " means unknown (not reached)");
         Put_Line (
              Arithmetic.Evaluation.Variable_Image
            & " means variable (not constant)");
         New_Line;

         Set_Col (Cell_Value_Col);
         Put_Line ("Cell values flowing into the step, by Cell #");

         Put ("Step#");

         Value_Col := Cell_Value_Col;

         for C in Cell_Number_T loop

            Set_Col (Value_Col - 1);
            Put (Cell_Number_T'Image (C));

            Value_Col := Value_Col + Cell_Value_Width;

         end loop;

         New_Line;

         for S in Steps'Range loop

            Put (Step_Index_T'Image (Index(Steps(S))));

            Value_Col := Cell_Value_Col;

            for C in Cell_Number_T loop

               Set_Col (Value_Col - 1); Put (' ');
               Put (Image (Values(S)(C)));

               Value_Col := Value_Col + Cell_Value_Width;

            end loop;

            New_Line;

         end loop;

         New_Line;

         Put_Line (
              "End of constant propagation results for "
            & Programs.Name (Subprogram));

         New_Line (2);

      end Show_Cell_Values;


      procedure Bound_Calls
      --
      -- Bounds the dynamic calling protocols for all (feasible) calls
      -- from the Subprogram using the computed Values. Sets Bounded to
      -- True if some dynamic protocol(s)are successfully bounded, and
      -- for such calls also updates the effect of the call-step to use
      -- the new protocol.
      --
      -- If this is the final round of Propagate_Constants (that is, if
      -- Last_Round or not Bounded) this operation also extracts input
      -- bounds from Values for all feasible calls and stores them in
      -- Bounds.
      --
      is
         use type Calling.Protocol_Ref;

         Calls : constant Programs.Execution.Call_Bounds_List_T :=
            Programs.Execution.Call_Bounds (Bounds);
         -- The calls from the Subprogram that are (still) feasible
         -- under the present computation model, together with execution
         -- bounds on the callees (from earlier analyses of the callees).

         Call : Programs.Call_T;
         -- One of the Calls.

         Call_Mark : Output.Nest_Mark_T;
         -- For the locus of the Call.

         Protocol_Bounded : Boolean;
         -- Whether the calling protocol for the Call was bounded or
         -- constrained to a tighter protocol.

      begin

         -- Bound the dynamic protocols if any:

         for C in Calls'Range loop

            Call := Calls(C).Call;

            if not Computation.Calling_Protocol_Is_Static (Call, Model)
            then
               -- The protocol is dynamic and can perhaps be bounded.

               Call_Mark := Output.Nest (Programs.Locus (Call));

               begin

                  Domain.Values := Values(Number (Programs.Step (Call)));
                  -- The cell-bounds on entry to the Call, as computed
                  -- by constant propagation.
                  -- TBM to include the Asserted bounds.
                  -- TBM? to include (existing) input bounds from Bounds.

                  Standard.Bounds.Calling.Bound_Protocol (
                     Call    => Call,
                     Data    => Domain,
                     Model   => Model,
                     Bounded => Protocol_Bounded);

                  if Protocol_Bounded then

                     Bounded := True;
                     -- A new computation model is formed, so a new
                     -- round of Propagate_Constants is desirable.

                  end if;

               exception

               when Flow.False_Path =>
                  -- This call seems to be infeasible.

                  Computation.Mark_Infeasible (Call, Model);

               end;

               Output.Unnest (Call_Mark);

            end if;

         end loop;

         -- Bound the inputs for the calls:

         if Last_Round or not Bounded then
            -- The Values and Model are as good as it gets, so now it makes
            -- sense to compute and store the input bounds for the calls.

            for C in Calls'Range loop

               Call := Calls(C).Call;

               Call_Mark := Output.Nest (Programs.Locus (Call));

               Domain.Values := Values(Number (Programs.Step (Call)));

               Programs.Execution.Bound_Call_Input (
                  Call   => Call,
                  Bounds => Known_Values (
                     Cells =>
                        Programs.Execution.Input_Cells (Calls(C).Bounds),
                     Domain  => Domain),
                  Within => Bounds);

               Output.Unnest (Call_Mark);

            end loop;

         end if;

      exception

      when others =>
          -- Clean up the output locus and punt:

         Output.Unnest (Call_Mark);

         raise;

      end Bound_Calls;


      procedure Find_Final_Stack_Height (
         Stack   : in     Programs.Stack_T;
         Returns : in     Step_List_T;
         SH      : in     Cell_Number_T;
         Giving  :    out Programs.Execution.Final_Stack_Height_T)
      --
      -- Finds the final local stack height in the given Stack for the
      -- Return steps. Assumes that the stack-height cell for the given
      -- Stack is a member of Named_Cells with number SH.
      --
      -- This procedure is called only for an Unstable Stack because the
      -- final local stack height for Stable stacks is known (assumed).
      --
      -- Enters the result in the Bounds and also returns it in Giving.
      --
      is

         Step : Step_T;
         -- The return step under inspection.

         Post : Cell_Values_T;
         -- The cell values after the Step.

         Grew : Boolean;
         -- Unused output from Compute_And_Assign.

         Height : Value_T;
         -- The stack-height after the Step, from Post.

         Known_Returns : Natural := 0;
         -- The number of Return steps for which a constant final
         -- stack-height was found.

         Autograph : constant String :=
            "Flow.Const.Propagate_Constants.Find_Final_Stack_Height";
         -- For Fault messages.

         Final : Storage.Bounds.Interval_T := Storage.Bounds.Void_Interval;
         -- Collecting the final stack-height values.

      begin

         -- Find the bounds on the stack height after all Return steps:

         for R in Returns'Range loop

            Step := Returns(R);

            if Computation.Is_Feasible (Step, Model) then

               -- Compute the state after the return:

               Post := Invariant_Values;

               Compute_And_Assign (
                  Pre  => Values(Number (Step)),
                  Via  => Step,
                  Post => Post,
                  Grew => Grew);

               Height := Post(SH);

               case Height.Level is

               when Unknown =>

                  if Opt.Refine_Edge_Conditions then
                     -- This should not happen.

                     Output.Fault (
                        Location => Autograph,
                        Text     =>
                            "Feasible return step"
                           & Positive'Image (Number (Step))
                           & " has unknown final stack-height for stack "
                           & Programs.Name (Stack));

                  -- else
                  --    This step was omitted from the analysis because it
                  --    is infeasible, but it was not marked as infeasible
                  --    because edge-conditions are not refined.

                  end if;

               when Known =>

                  Storage.Bounds.Widen (
                     Interval   => Final,
                     To_Contain => Arithmetic.Signed_Value (
                        Word  => Height.Value,
                        Width => Programs.Width_Of (Stack)));

                  Known_Returns := Known_Returns + 1;

               when Variable | Relative =>

                  Final := Storage.Bounds.Universal_Interval;

               end case;

            end if;

         end loop;

         if Returns'Length > 0 then

            Output.Note (
        	 "Final stack-height in stack "
               & Programs.Name (Stack)
               & " is constant for"
               & Natural'Image (Known_Returns)
               & " of"
               & Natural'Image (Returns'Length)
               & " return points.");

         else
            -- No return points? Never returns?

            Output.Note (
                 "No return steps for final stack-height in stack "
               & Programs.Name (Stack));

            Final := Storage.Bounds.Singleton (0);
            -- Just to bound it, although the bound should never
            -- be relevant.

         end if;

         Giving := Programs.Execution.To_Stack_Limit (Final);

         Output.Note (
              "Final stack-height from constant propagation for "
            & Programs.Name (Stack)
            & Output.Field_Separator
            & Programs.Execution.Image (
                 Item => Giving,
                 Name => Storage.Image (Cell_Num (SH))));

         Stacking_Opt.Ignore_Huge_Bounds (Giving);

         Programs.Execution.Bound_Final_Stack_Height (
            Stack  => Stack,
            To     => Giving,
            Within => Bounds);

      end Find_Final_Stack_Height;


      procedure Find_Stack_Heights (
         Stack : in Programs.Stack_T;
         Calls : in Programs.Call_List_T;
         SH    : in Cell_Number_T;
         Final : in Programs.Execution.Final_Stack_Height_T)
      --
      -- Finds the maximum local stack height and the take-off heights
      -- in the given Stack for the given Calls, when these heights have
      -- been found to be constant. Assumes that the stack-height cell
      -- for the given Stack is a member of Named_Cells with number SH.
      -- The (bounds on the) Final local stack height have already been
      -- computed and take part in the maximum.
      --
      is

         Max_Stack : Programs.Execution.Stack_Limit_T :=
            Programs.Execution.To_Stack_Limit (
               Storage.Bounds.Singleton (0));
         -- Collects the maximum stack height over all steps.

         Step : Step_T;
         -- The step under inspection.

         Height : Value_T;
         -- The stack-height at Step.

         Known_Calls : Natural := 0;
         -- The number of Calls for which a constant take-off
         -- stack-height was found.

         Autograph : constant String :=
            "Flow.Const.Propagate_Constants.Find_Stack_Heights";
         -- For Fault messages.

         Take_Off : Programs.Execution.Stack_Limit_T;
         -- The take-off height for a call.

      begin

         -- Consider the final stack height:

         Max_Stack := Programs.Execution.Max (Max_Stack, Final);

         -- Find the maximum stack height over all steps, using the
         -- stack height on entry to the step:

         for S in Steps'Range loop

            Step := Steps(S);

            if Computation.Is_Feasible (Step, Model) then

               Height := Values(S)(SH);

               case Height.Level is

               when Unknown =>

                  if Opt.Refine_Edge_Conditions then
                     -- This should not happen.

                     Output.Fault (
                        Location => Autograph,
                        Text     =>
                            "Feasible step"
                           & Positive'Image (S)
                           & " has unknown stack-height for stack "
                           & Programs.Name (Stack));

                  -- else
                  --    This step was omitted from the analysis because it
                  --    is infeasible, but it was not marked as infeasible
                  --    because edge-conditions are not refined.

                  end if;

               when Known =>

                  Max_Stack := Programs.Execution.Max (
                     Max_Stack,
                     Arithmetic.Signed_Value (
                        Word  => Height.Value,
                        Width => Programs.Width_Of (Stack)));

               when Variable | Relative =>

                  Max_Stack := Programs.Execution.Unbounded;

               end case;

            end if;

         end loop;

         Output.Note (
              "Maximum stack-height from constant propagation for "
            & Programs.Name (Stack)
            & Output.Field_Separator
            & Programs.Execution.Max_Image (Max_Stack));

         Stacking_Opt.Ignore_Huge_Bounds (Max_Stack);

         Programs.Execution.Bound_Stack_Height (
            Stack  => Stack,
            To     => Max_Stack,
            Within => Bounds);


         -- Take-off height for all calls:

         for C in Calls'Range loop

            Step := Programs.Step (Calls(C));

            if Computation.Is_Feasible (Step, Model) then

               Height := Values(Number(Step))(SH);

               case Height.Level is

               when Unknown =>

                  if Opt.Refine_Edge_Conditions then
                     -- This should not happen.

                     Output.Fault (
                        Location => Autograph,
                        Text     =>
                            "Feasible call-step"
                           & Positive'Image (Number (Step))
                           & " has unknown stack-height for stack "
                           & Programs.Name (Stack));

                  -- else
                  --    This step was omitted from the analysis because it
                  --    is infeasible, but it was not marked as infeasible
                  --    because edge-conditions are not refined.

                  end if;

                  Take_Off := Programs.Execution.Unbounded;

               when Known =>

                  Take_Off := Programs.Execution.To_Stack_Limit (
                     Storage.Bounds.Singleton (
                       Arithmetic.Signed_Value (
                          Word  => Height.Value,
                          Width => Programs.Width_Of (Stack))));

                  Known_Calls := Known_Calls + 1;

               when Variable | Relative =>

                  Take_Off := Programs.Execution.Unbounded;

               end case;

               Stacking_Opt.Ignore_Huge_Bounds (Take_Off);

               Programs.Execution.Bound_Take_Off_Height (
                  Stack  => Stack,
                  Before => Calls(C),
                  To     => Take_Off,
                  Within => Bounds);

            end if;

         end loop;

         Output.Note (
              "Take-off stack-height in stack "
            & Programs.Name (Stack)
            & " is constant for"
            & Natural'Image (Known_Calls)
            & " of"
            & Natural'Image (Calls'Length)
            & " calls.");

      end Find_Stack_Heights;


      function Height_For_Unused (Stack : Programs.Stack_T)
      return Programs.Execution.Stack_Limit_T
      --
      -- The height (limit) to be used for a Stack when the
      -- subprogram does not change the stack-height cell.
      --
      is

         Height : constant Storage.Cell_T := Programs.Height (Stack);
         -- The stack-height cell.

      begin

         -- Use the bounds on the stack-height from the Initial bounds:

         return Programs.Execution.To_Stack_Limit (
            Storage.Bounds.Interval (
               Cell => Height,
               From => Initial));

      end Height_For_Unused;


      procedure Bound_Stack_Heights
      --
      -- Bounds the local stack-height and the take-off height for
      -- all (feasible) calls from the Subprogram and all stacks.
      --
      is

         Stacks : constant Programs.Stacks_T := Programs.Stacks (Program);
	      -- All the stacks in the program.

         Calls : constant Programs.Call_List_T :=
            Computation.Calls_From (Model);
         -- The calls from the Subprogram that are (still) feasible
         -- under the present Model.

         Stack_Height : Storage.Cell_T;
         -- The stack-height cell for the current stack.

         SH : Cell_Number_T;
         -- The number of the Stack_Height cell, in our numbering.

         Unused_Height : Programs.Execution.Stack_Limit_T;
         -- The height limit for a stack where the subprogram
         -- does not access the stack-height cell.

         Net_Change : Storage.Bounds.Limit_T;
         -- The possibly known net change in stack height = final stack
         -- height at return.

	      Final_Height : Programs.Execution.Final_Stack_Height_T;
	      -- The final (local) stack height for the stack.

      begin

         for S in Stacks'Range loop

            Stack_Height := Programs.Height (Stacks(S));

            if not Storage.Is_Member (Stack_Height, Named_Cells) then
               -- This stack-height cell is not accessed in this flow-graph.
               -- Use the initial stack-height limit for this stack and
               -- subprogram.

               Unused_Height := Height_For_Unused (Stack => Stacks(S));

               Output.Note (
                    "No change in initial stack-height for stack "
                  & Programs.Name (Stacks(S))
                  & Output.Field_Separator
                  & Programs.Execution.Max_Image (Unused_Height));

               Programs.Execution.Bound_Stack_Height (
                  Stack  => Stacks(S),
                  To     => Unused_Height,
                  Within => Bounds);

               for C in Calls'Range loop

                  Programs.Execution.Bound_Take_Off_Height (
                     Stack  => Stacks(S),
                     Before => Calls(C),
                     To     => Unused_Height,
                     Within => Bounds);

               end loop;

               -- The final stack height is zero. For a Stable stack
               -- this is the defined (but irrelevant) value; for an
               -- Unstable stack this is the result of making no changes
               -- to an initial zero stack height.

               Programs.Execution.Bound_Final_Stack_Height (
                  Stack  => Stacks(S),
                  To     => Programs.Execution.To_Stack_Limit (
                               Storage.Bounds.Exactly_Zero),
                  Within => Bounds);

            else
               -- This stack-height cell is used or manipulated in the
               -- subprogram, and we have tried to compute constant values
               -- for it at each step. We scan these values to find the
               -- maximum local stack height and the take-off heights.

               SH := Storage.Cell_Numbering.Number (
                  Cell  => Stack_Height,
                  Under => Domain.Numbers);

               -- First we find the final stack height, because it too is
               -- a contributor to the upper bound on the local stack height
               -- when a return step can increase the height:

               Net_Change := Programs.Net_Change (Stacks(S));

               Final_Height := Programs.Execution.Final_Stack_Height (
                  Stack  => Stacks(S),
                  Within => Bounds);

               if Programs.Execution.Singular (Final_Height) then
                  -- The final height is already known precisely.

                  null;

               elsif Storage.Bounds.Known (Net_Change) then
                  -- The net change = final height of this stack is known to
                  -- be the same for all calls of all subprograms. No need to
                  -- do any work to find it.

                  Output.Note (
                       "Fixed final height of stack "
                     & Programs.Name (Stacks(S))
                     & Output.Field_Separator
                     & Storage.Bounds.Image (Net_Change));

                  Final_Height := Programs.Execution.One_Or_All (Net_Change);

                  Programs.Execution.Bound_Final_Stack_Height (
                     Stack  => Stacks(S),
                     To     => Final_Height,
                     Within => Bounds);

               else
                  -- The net change = final height of this stack may vary
                  -- from subprogram to subprogram. Working, working...

        	         Find_Final_Stack_Height (
                     Stack   => Stacks(S),
                     Returns => Computation.Final_Steps (Model),
                     SH      => SH,
                     Giving  => Final_Height);

               end if;

               Find_Stack_Heights (
                  Stack => Stacks(S),
                  Calls => Calls,
                  SH    => SH,
                  Final => Final_Height);

            end if;

         end loop;

      end Bound_Stack_Heights;


      procedure Resolve_Dynamic_Edges
      --
      -- Applies the computed Values as bounds on the (remaining
      -- feasible and Unresolved) dynamic edges in the Graph, trying
      -- to resolve them into statically known edges.
      --
      is

         Dyn_Edges : constant Dynamic_Edge_List_T :=
            Computation.Unstable_Dynamic_Edges (Model);
         -- The dynamic (boundable) edges in the Graph that are
         -- still feasible under the (refined) Model and are
         -- still not stably resolved (or stably unresolved).

         Edge : Dynamic_Edge_T;
         -- One of the dynamic edges.

         Step : Step_T;
         -- The source of the Edge.

         Step_Mark : Output.Nest_Mark_T;
         -- The locus of the Step.

      begin

         for D in Dyn_Edges'Range loop

            Edge := Dyn_Edges(D);

            Step := Source (Edge.all);

            Step_Mark := Output.Nest (Show.Locus (
               Step   => Step,
               Source => Programs.Symbol_Table (Program)));

            Domain.Values := Values(Number (Step));

            Apply (
               Bounds => Domain,
               Upon   => Edge.all,
               Graph  => Graph);

            Output.Unnest (Step_Mark);

         end loop;

      end Resolve_Dynamic_Edges;


   begin  -- Propagate_Constants

      if Opt.Trace_Iteration
      or Opt.Show_Results
      then

         Output.Trace (
              "Constant propagation round"
            & Positive'Image (Round)
            & " starts for"
            & Natural'Image (Cell_Num'Length)
            & " cells.");

         Ada.Text_IO.New_Line;
         Ada.Text_IO.Put_Line ("Cell numbering for constant propagation:");

         Storage.Cell_Numbering.Show (Domain.Numbers);

      end if;

      Bounded := False;
      -- Initially. Will be changed to True if some dynamic reference
      -- or calling protocol is bounded.


      -- Compute cell values by constant propagation (phase 1):

      Values := Constant_Cell_Values (
         Nodes  => Steps,
         Edges  => Natural (Max_Step_Edge (Graph)),
         Within => Model);

      if Opt.Show_Results then

         Show_Cell_Values;

      end if;


      -- Create the new computation model (phase 2):

      for S in Steps'Range loop

         if Computation.Is_Feasible (Steps(S), Model) then

            Domain.Values := Values(S);

            Refine (
               Step    => Steps(S),
               Domain  => Domain,
               Post    => Work_Post,
               Program => Program,
               Model   => Model,
               Bounded => Bounded);

         end if;

      end loop;


      -- Prune the graph (phase 3):

      Computation.Prune (Model);


      -- Bound calling protocols and inputs for calls (phase 4):

      Bound_Calls;


      -- And again prune the graph (phase 5):

      Computation.Prune (Model);


      -- Are we done?

      if Last_Round
      or not Programs.Execution.Computation_Changed (Bounds)
      then
         -- The computation model is stable, or we will not iterate
         -- further on it.

         -- Determine local stack heights and take-off heights (phase 6):

         if  Programs.Number_Of_Stacks (Program) > 0
         and Opt.Resolve_Stack
         then
            -- There are some stack-heights that we can bound.

            Bound_Stack_Heights;

         end if;

         -- Apply the constant values to bound dynamic flow (phase 7):

         if Opt.Resolve_Flow then

            Resolve_Dynamic_Edges;

         end if;

      end if;


      -- Some working data are no longer needed:

      Storage.Discard (Named_Cells);

      Storage.Cell_Numbering.Discard (Domain.Numbers);

      for V in Values'Range loop

         Discard (Values(V));

      end loop;

      Discard (Work_Post);

   end Propagate_Constants;


   --
   --    Provided operations:
   --


   procedure Propagate (
      Subprogram : in Programs.Subprogram_T;
      Asserted   : in Storage.Bounds.Var_Interval_List_T;
      Bounds     : in Programs.Execution.Bounds_Ref)
   is
      use type Storage.Bounds.Cell_Interval_List_T;

      Initial : constant Storage.Bounds.Cell_Interval_List_T :=
             Programs.Execution.Initial (Bounds)
         and Storage.Bounds.Cell_Intervals (
                From  => Asserted,
                Point => Programs.Entry_Address (Subprogram));
      --
      -- The "and" conjunction of the Initial and Asserted bound lists
      -- computes the intersection of the bounds, if the same cell appears
      -- in both lists. For those cells that are not hereby constrained to
      -- single values, a variable or relative value is fed into the
      -- flow-graph from the caller.
      --
      -- The Asserted bounds are probably already included in the Initial
      -- bounds (vide package Bounds) but including them again does no
      -- harm and makes this package more general.

      Pointers_Bounded : Boolean;
      -- Whether Propagate_Constants was able to resolve or bound some
      -- pointers for dynamically addressed data.

      Rounds : Natural := 0;
      -- Counts the number of iterations of Propagate_Constants to make
      -- use of resolved or bounded data pointers.

   begin

      if Opt.Propagate then
         -- Constant propagation is enabled.
         -- We repeat it until the computation model is stable, or
         -- the flow-graph grows through resolved dynamic edges, or
         -- the iteration limit is reached.

         -- Optionally show the initial and invariant bounds:

         if Opt.Show_Results
         or Opt.Trace_Iteration
         then

            Output.Trace (
                 "Constant propagation initial bounds"
               & Output.Field_Separator
               & Storage.Bounds.Image (Initial));

            if Asserted'Length > 0 then

               Output.Trace (
                  "Constant propagation invariant bounds:");

               for A in Asserted'Range loop

                  Output.Trace (
                       "Location"
                     & Output.Field_Separator
                     & Storage.Image (Asserted(A).Location.all));

                  Output.Trace (
                       "Interval"
                     & Output.Field_Separator
                     & Storage.Bounds.Image (Asserted(A).Interval));

               end loop;

            end if;

         end if;

         -- Propagate constants, repeatedly:

         loop

            Rounds := Rounds + 1;

            Propagate_Constants (
               Subprogram => Subprogram,
               Asserted   => Asserted,
               Initial    => Initial,
               Round      => Rounds,
               Last_Round => Rounds >= Opt.Max_Pointer_Iterations,
               Bounded    => Pointers_Bounded,
               Bounds     => Bounds);

            if not Programs.Execution.Computation_Changed (Bounds) then
               -- Constant propagation did not change the computation
               -- model in any way. We have nothing more to do here.

               Output.Note ("Constant propagation changed nothing.");

               exit;

            end if;

            if Programs.Execution.Dynamic_Flow (Bounds) = Growing then
               -- Constant propagation resolved some dynamic edges and
               -- the flow-graph is thus growing. Even if the computation
               -- model changed we have no reason to do any more work on
               -- this model, because we will anyway build a new model
               -- for the extended flow-graph.

               if Opt.Show_Results then

                  Output.Trace (
                     "Constant propagation resolved dynamic edges.");

                  end if;

               exit;

            end if;

            -- Constant propagation changed the computation model in
            -- some ways, by simplifying the effects or conditions or
            -- by resolving dynamic memory references or calling
            -- protocols, but the flow-graph is not (yet) growing, so
            -- we will continue with this computation model.

            if Opt.Show_Results then

               Output.Trace (
                  "Constant propagation refined the computation.");

            end if;

            Programs.Execution.Note_Updated_Computation (Bounds);

            Programs.Execution.Mark_Computation_Clean (Bounds);

            if Rounds >= Opt.Max_Pointer_Iterations then

               Output.Warning (
                  "Constant propagation stops at iteration limit.");

               exit;

            end if;

            Output.Note ("Repeating constant propagation.");

         end loop;

      end if;

   end Propagate;


end Flow.Const;
