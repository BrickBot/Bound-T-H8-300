-- Bounds.Looping (body)
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
-- $Revision: 1.23 $
-- $Date: 2015/10/24 20:05:46 $
--
-- $Log: bounds-looping.adb,v $
-- Revision 1.23  2015/10/24 20:05:46  niklas
-- Moved to free licence.
--
-- Revision 1.22  2012-02-13 17:52:19  niklas
-- BT-CH-0230: Options -max_loop and -max_stack for spurious bounds.
--
-- Revision 1.21  2011-09-08 08:56:06  niklas
-- Extended function Step_Bounds to swap small negative steps, under
-- control of the new options "sw_neg_step" and "warn sw_neg_step".
--
-- Revision 1.20  2009-11-27 11:28:06  niklas
-- BT-CH-0184: Bit-widths, Word_T, failed modular analysis.
--
-- Revision 1.19  2009-10-07 19:26:09  niklas
-- BT-CH-0183: Cell-sets are a tagged-type class.
--
-- Revision 1.18  2009-08-16 13:34:57  niklas
-- Corrected description of exceptions from General_Procedure.
--
-- Revision 1.17  2009-01-18 08:05:15  niklas
-- Removed unused context clauses.
--
-- Revision 1.16  2008/09/24 08:38:51  niklas
-- BT-CH-0146: Assertions on "loop starts <bound> times".
-- BT-CH-0146: Loop-repeat assertions set both lower and upper bound.
-- BT-CH-0146: Report locations of contradictory "count" assertions.
-- BT-CH-0146: Contradictory "count" assertions imply infeasibility.
--
-- Revision 1.15  2008/05/03 09:22:06  niklas
-- BT-CH-0126: Combining joint-counter and each-counter analysis.
--
-- Revision 1.14  2008/04/28 08:40:10  niklas
-- BT-CH-0125: Fix loop-until-equal analysis.
--
-- Revision 1.13  2008/04/26 19:19:43  niklas
-- BT-CH-0124: Joint loop counters and induction variables.
--
-- Revision 1.12  2007/12/17 13:54:34  niklas
-- BT-CH-0098: Assertions on stack usage and final stack height, etc.
--
-- Revision 1.11  2006/10/30 19:13:55  niklas
-- BT-CH-0032.
--
-- Revision 1.10  2006/02/06 21:14:40  niklas
-- Removed dead warning from Assert_Repeat_Zer_Times.
--
-- Revision 1.9  2006/02/06 14:23:09  niklas
-- Corrected Bound_Asserted_Loop and subprocedures to handle
-- eternal loops separately. It is now possible to assert one
-- repetition for an eternal loop and keep the loop feasible.
--
-- Revision 1.8  2005/10/20 11:28:28  niklas
-- BT-CH-0015.
--
-- Revision 1.7  2005/09/20 11:55:46  niklas
-- BT-CH-0010.
--
-- Revision 1.6  2005/09/17 14:42:04  niklas
-- BT-CH-0009.
--
-- Revision 1.5  2005/06/29 20:01:31  niklas
-- Added Repeat_Zero_Times to extend the checks and warnings
-- for unreachable parts of zero-repeat loops.
--
-- Revision 1.4  2005/06/29 13:00:01  niklas
-- Added optional (-warn reach) warnings about unreachable and
-- unrepeatable loops.
--
-- Revision 1.3  2005/06/28 08:35:24  niklas
-- Updated for changes in Bounds.Opt.
--
-- Revision 1.2  2005/02/19 20:34:55  niklas
-- BT-CH-0003.
--
-- Revision 1.1  2005/02/16 21:11:38  niklas
-- BT-CH-0002.
--


with Arithmetic;
with Bounds.Looping.Opt;
with Flow.Computation;
with Flow.Execution;
with Flow.Pruning;
with Flow.Pruning.Opt;
with Output;
with Storage.Bounds;


package body Bounds.Looping is


  procedure Assert_Infeasible_Loop (
      Luup  : in Loops.Loop_T;
      Model : in Flow.Computation.Model_Handle_T)
   --
   -- Special actions for a loop that is found to be unreachable.
   --
   is
   begin

      Flow.Computation.Mark_Infeasible (
         Luup  => Luup,
         Under => Model.all);

   end Assert_Infeasible_Loop;


   procedure Bound_Asserted_Start (
      Luup        : in     Loops.Loop_T;
      Count       : in     Flow.Execution.Bound_T;
      Exec_Bounds : in     Programs.Execution.Bounds_Ref;
      Infeasible  : in out Boolean)
   --
   -- Bounds the number of starts of the given Luup with an asserted
   -- start Count, within the given Execution Bounds. If the number of
   -- starts is zero, the loop-head is marked infeasible and Infeasible
   -- is returned as True, otherwise it is returned unchanged. If the
   -- loop starts at the entry point of the subprogram, but the Count
   -- does not allow one start, a void "start" bound is put in the
   -- execution bounds to mark this contradiction, and an error is
   -- issued.
   --
   is
      use type Flow.Execution.Count_T;
      use type Flow.Node_T;

      Loop_Mark : Output.Nest_Mark_T;
      -- For the locus of the loop.

   begin

      Loop_Mark := Output.Nest (Programs.Execution.Locus (Luup, Exec_Bounds));

      if Count.Max < 1 then
         -- The loop becomes infeasible.

         Output.Warning ("Unreachable loop (asserted to start zero times).");

         Assert_Infeasible_Loop (
            Luup  => Luup,
            Model => Programs.Execution.Computation (Exec_Bounds));

         Infeasible := True;

      elsif Loops.Head_Node (Luup)
            = Flow.Entry_Node (Programs.Execution.Flow_Graph (Exec_Bounds))
      and then not Flow.Execution.Is_In (1, Count)
      then
         -- The loop starts at the entry point of the subprogram,
         -- which means that it starts exactly once, but the
         -- assertion forbids one start. This is contradictory.

         Output.Error (
              "Loop at entry point starts once, not "
            & Flow.Execution.Image (Count)
            & " times.");

         Programs.Execution.Bound_Loop_Starts (
            Luup   => Luup,
            Starts => Flow.Execution.Impossible,
            Within => Exec_Bounds);

      else
         -- Nothing special, use Count as is.

         Programs.Execution.Bound_Loop_Starts (
            Luup   => Luup,
            Starts => Count,
            Within => Exec_Bounds);

      end if;

      Output.Unnest (Loop_Mark);

   end Bound_Asserted_Start;


   procedure Bound_Asserted_Starts (
      Luups       : in Loops.Loop_List_T;
      Asserts     : in Assertions.Assertion_Map_T;
      Exec_Bounds : in Programs.Execution.Bounds_Ref)
   is

      Some_Infeasible_Parts : Boolean := False;
      -- Whether some flow-graph parts became infeasible because of
      -- the loop-start assertions.

      Count : Flow.Execution.Bound_T;
      -- Asserted bounds (if any) on the starts of a loop.

   begin

      for L in Luups'Range loop

         Count := Assertions.Loop_Start (
            Luup    => Luups(L),
            Asserts => Asserts);

         if Flow.Execution.Bounded (Count) then
            -- There is a start-assertion for this loop.

            Bound_Asserted_Start (
               Luup        => Luups(L),
               Count       => Count,
               Exec_Bounds => Exec_Bounds,
               Infeasible  => Some_Infeasible_Parts);

         end if;

      end loop;

      if Some_Infeasible_Parts then
         -- Let the infeasibility spread.

         Programs.Execution.Prune_Flow (Exec_Bounds);

      end if;

   end Bound_Asserted_Starts;


   procedure Assert_Repeat_Zero_Times (
      Luup  : in Loops.Loop_T;
      Model : in Flow.Computation.Model_Handle_T)
   --
   -- Special actions for a loop that is asserted to repeat zero times
   -- through the repeat-edges. The repeat edges are marked infeasible.
   --
   is
   begin

      Flow.Computation.Mark_Infeasible (
         Edges => Flow.Computation.Repeat_Edges (Luup, Model.all),
         Under => Model.all);

   end Assert_Repeat_Zero_Times;


   procedure Assert_Repeat_Neck_Zero_Times (
      Luup  : in Loops.Loop_T;
      Model : in Flow.Computation.Model_Handle_T)
   --
   -- Special actions for a loop that is asserted to repeat zero times
   -- through the loop-neck.
   --
   -- If no exit-edges leave the loop-head, asserting zero repetitions
   -- makes the loop-head unreachable. This always evokes a warning.
   --
   -- If all repeat-edges start from the loop-head, the entire loop
   -- consists of the loop-head node. This means that the loop body is
   -- executed once even when zero repetitions are asserted. This always
   -- evokes a warning.
   --
   -- Otherwise, a warning is optionally issued that the loop body
   -- is unreachable.
   --
   -- In any case, the neck edges are marked infeasible.
   --
   is
      use type Flow.Node_T;

      Head : constant Flow.Node_T := Loops.Head_Node (Luup);
      -- The loop-head node.

      Exits : constant Flow.Edge_List_T :=
         Flow.Computation.Exit_Edges (Luup, Model.all);
      -- The feasible exit edges.

      Repeats : constant Flow.Edge_List_T :=
         Flow.Computation.Repeat_Edges (Luup, Model.all);
      -- The feasible repeat edges.

      Head_Exits : Boolean := False;
      -- Whether some exit edge starts from the Head.

      Only_Head_Repeats : Boolean := True;
      -- Whether all repeat edges start from the Head.

   begin

      -- Check the sources of the exit edges:

      for E in Exits'Range loop

         if Flow.Source (Exits(E)) = Head then
            -- This edge exits the loop from the head node.
            -- The edge will not be marked infeasible, so the head node
            -- may also remain feasible.

            Head_Exits := True;

          end if;

      end loop;

      -- Check the sources of the repeat edges:

      for R in Repeats'Range loop

         if Flow.Source (Repeats(R)) /= Head then
            -- This edge repeats the loop from a non-head node.
            -- This means that there is a non-null "real" loop body
            -- in addition to the head node.

            Only_Head_Repeats := False;

         end if;

      end loop;

      -- Warnings if indicated:

      if not Head_Exits then

         Output.Warning (
            "Unreachable loop (asserted to repeat zero times).");

      elsif Only_Head_Repeats then

         Output.Warning (
            "Loop body executes once (asserted to repeat zero times).");

      elsif Flow.Pruning.Opt.Warn_Unreachable then

         Output.Warning (
            "Unreachable loop body (asserted to repeat zero times).");

      end if;

      Flow.Computation.Mark_Infeasible (
         Edges => Flow.Computation.Neck_Edges (Luup, Model.all),
         Under => Model.all);

   end Assert_Repeat_Neck_Zero_Times;


   procedure Bound_Asserted_Repeat (
      Luup        : in     Loops.Loop_T;
      Count       : in     Flow.Execution.Bound_T;
      Exec_Bounds : in     Programs.Execution.Bounds_Ref;
      Infeasible  : in out Boolean)
   --
   -- Bounds the given Luup with an asserted repetition Count, within
   -- the given Execution Bounds. If this causes some flow-graph parts
   -- to be marked infeasible (for example, for zero repetitions) then
   -- Infeasible is returned as True, otherwise it is returned unchanged.
   --
   is
      use type Flow.Execution.Count_T;

      Model : constant Flow.Computation.Model_Handle_T :=
         Programs.Execution.Computation (Exec_Bounds);
      -- The computation model in use.
      -- We may mark some edges as infeasible in this model.

      Loop_Mark : Output.Nest_Mark_T;
      -- For the locus of the loop.

   begin

      -- There are three cases:
      --
      -- > Eternal loops: for Count <= 0 the loop becomes infeasible,
      --   otherwise the repeat edge(s) are bounded to Count. In the
      --   later IPET stage one execution of the repeat edge(s) can
      --   escape (exit the program) without entering the loop head.
      --
      -- > Exit-at-end loops: for Count <= 0 the loop becomes infeasible,
      --   otherwise the repeat edge(s) are bounded to Count - 1 and
      --   become infeasible if Count = 1.
      --
      -- > Exit-in-the middle loops: the neck edges are bounded to Count
      --   and become infeasible if Count <= 0. The loop-head remains
      --   feasible for all Count values.

      Loop_Mark := Output.Nest (Programs.Execution.Locus (Luup, Exec_Bounds));

      if Flow.Computation.Is_Eternal (Luup, Model.all) then
         -- Assertions on eternal loops are interpreted differently.

         Output.Note (
              "Loop"
            & Loops.Loop_Index_T'Image (Loops.Loop_Index (Luup))
            & " is eternal.");

         if Count.Max <= 0 then
            -- The loop becomes infeasible.

            Output.Warning (
               "Unreachable eternal loop (asserted to repeat zero times).");

            Assert_Infeasible_Loop (
               Luup  => Luup,
               Model => Model);

            Infeasible := True;

         else
            -- The loop remains feasible and we can place a repetition
            -- bound on the repeat-edges. Note that this bound is Count,
            -- not Count - 1, but in the IPET stage we will let one
            -- execution escape from the repeat edges, to terminate
            -- the measured execution.

            Programs.Execution.Bound_Loop_Repeats (
               Luup    => Luup,
               Repeats => Count,
               Within  => Exec_Bounds);

         end if;

      elsif Loops.Exits_At_End (
         Luup   => Luup,
         Within => Programs.Execution.Flow_Graph (Exec_Bounds))
      then
         -- For a loop that exits only at the end (bottom), the
         -- bound can safely be reduced by one and applied to
         -- the repeat edges instead of to the neck.

         Output.Note (
              "Loop"
            & Loops.Loop_Index_T'Image (Loops.Loop_Index (Luup))
            & " exits only at its end.");

         if Count.Max <= 0 then
            -- The loop becomes infeasible.

            Output.Warning (
                 "Unreachable exit-at-end loop "
               & "(asserted to repeat zero times).");

            Assert_Infeasible_Loop (
               Luup  => Luup,
               Model => Model);

            Infeasible := True;

         else
            -- The loop remains feasible (unless it is eternal) and we
            -- can place a reduced repetition bound on the repeat-edges.

            Programs.Execution.Bound_Loop_Repeats (
               Luup    => Luup,
               Repeats => (
                  Min => Flow.Execution.Count_T'Max (0, Count.Min - 1),
                  Max => Count.Max - 1),
               Within  => Exec_Bounds);

            if Count.Max = 1 then
               -- This loop-body is never repated; the repeat edges
               -- are infeasible.

               Assert_Repeat_Zero_Times (
                  Luup  => Luup,
                  Model => Model);

               Infeasible := True;

            end if;

         end if;

      else
         -- For a loop that does not exit only at the end (bottom),
         -- the bound is applied, as such, to the neck edges.

         Programs.Execution.Bound_Loop_Neck (
            Luup   => Luup,
            Count  => Count,
            Within => Exec_Bounds);

         if Count.Max <= 0 then
            -- This loop is never entered; the neck edges are infeasible.

            Assert_Repeat_Neck_Zero_Times (
               Luup  => Luup,
               Model => Model);

            Infeasible := True;

         end if;

      end if;

      Output.Unnest (Loop_Mark);

   end Bound_Asserted_Repeat;


   procedure Bound_Asserted_Repeats (
      Luups       : in Loops.Loop_List_T;
      Asserts     : in Assertions.Assertion_Map_T;
      Exec_Bounds : in Programs.Execution.Bounds_Ref)
   is

      Some_Infeasible_Parts : Boolean := False;
      -- Whether some flow-graph parts became infeasible because of
      -- the loop-repeat assertions.

      Count : Flow.Execution.Bound_T;
      -- Asserted bounds (if any) on the repetitions of a loop.

   begin

      for L in Luups'Range loop

         Count := Assertions.Loop_Count (
            Luup    => Luups(L),
            Asserts => Asserts);

         if Flow.Execution.Bounded (Count) then
            -- There is a repetition assertion for this loop.

            Bound_Asserted_Repeat (
               Luup        => Luups(L),
               Count       => Count,
               Exec_Bounds => Exec_Bounds,
               Infeasible  => Some_Infeasible_Parts);

         end if;

      end loop;

      if Some_Infeasible_Parts then
         -- Let the infeasibility spread.

         Programs.Execution.Prune_Flow (Exec_Bounds);

      end if;

   end Bound_Asserted_Repeats;


   --
   --    Loop-bounds analysis
   --


   Infeasible_Loop : exception;
   --
   -- Signals that the loop under analysis is infeasible (cannot be
   -- entere) because the bounds on the initial values are void (the
   -- empty set).


   Unrepeatable_Loop : exception;
   --
   -- Signals that the loop under analysis is unrepeatable (not really
   -- a loop) because the bounds on the values along the repeat edges
   -- are void (the empty set).


   type Loop_Vars_T (Num_Var : Natural) is record
      Num_Proc : Natural := 0;
      Num_Ind  : Natural := 0;
      Eff_Ind  : Boolean := False;
      Find     : Natural := 0;
      Var      : Storage.Cell_List_T(1 .. Num_Var);
      Ind_Var  : Storage.Cell_List_T            (1 .. Num_Var);
      Step     : Storage.Bounds.Interval_List_T (1 .. Num_Var);
   end record;
   --
   -- The loop-bounds analysis is based on a set of variables
   -- and their properties, as represented in this type.
   --
   -- Num_Var
   --    The number of variables that are candidates for induction
   --    variables in the loop.
   -- Num_Proc
   --    The number of variables analysed for inductions so far.
   -- Num_Ind
   --    The number of induction variables found so far.
   --    An induction variable is a variable for which some Step
   --    bounds are known (but not necessarily both lower and
   --    upper bounds, and a zero step may be allowed).
   --    If Num_Proc = Num_Var, there are no more to be found.
   -- Eff_Ind
   --    Whether an effective induction variable has been found.
   --    An induction variable is effective if its Step bounds
   --    exclude a zero step (unchanged variable value).
   -- Find
   --    A cursor for finding counters in Ind_Var.
   --    The index of the last counter found and returned, or
   --    zero if none have been sought yet. Greater than Num_Ind
   --    if all counters have been found.
   -- Var
   --    The candidate variables, 1 .. Num_Var.
   --    These are the loop-variant "counter" cells.
   --    The cells Var(1 .. Num_Proc) have been analysed to see
   --    if they are induction variables.
   -- Ind_Var
   --    The induction variables found so far, 1 .. Num_Ind.
   -- Step
   --    The bounds on the step (change) for each each Ind_Var.
   --    Step(v) bounds the step for Ind_Var(v).


   function Initial_Counter_Bounds (
      Initial : Calculator.Pool_T;
      Counter : Storage.Cell_T)
   return Storage.Bounds.Interval_T
   --
   -- Bounds on the Counter cell, in the pool of Initial values
   -- for a loop (of all variables).
   --
   -- May propagate Infeasible_Loop (if the Initial pool is null).
   --
   is

      Result : Storage.Bounds.Interval_T;
      -- Guess what.

   begin

      Calculator.Comment (
         Text =>
              "Check initial value for counter candidate "
            & Storage.Image (Counter),
         Calc => Calculator.Owner_Of (Initial));

      Result := Calculator.Bounds_From_Pool (
         Pool => Initial,
         Cell => Counter);

      if Opt.Trace_Counters then

         Output.Trace (Text =>
              "Loop-init bounds: "
            & Storage.Bounds.Image (
                 Item => Result,
                 Name => Storage.Image (Counter)) );

      end if;

      return Result;

   exception

   when Calculator.Null_Set_Error =>
      -- The init pool is void: the loop seems infeasible.

      Output.Note ("Loop is infeasible (checking initial data)");

      raise Infeasible_Loop;

   end Initial_Counter_Bounds;


   function Step_Bounds (
      Var    : Storage.Cell_T;
      Repeat : Calculator.Flux_T)
   return Storage.Bounds.Interval_T
   --
   -- Bounds on the Step of the variant Variable, in the Repeat flux
   -- of a loop.
   --
   -- May propagate Unrepeatable_Loop if the Repeat flux is void.
   --
   is

      Step : Storage.Bounds.Interval_T;
      -- The result.

   begin

      Calculator.Comment (
         Text =>
              "Check step for induction candidate "
            & Storage.Image (Var),
         Calc => Calculator.Owner_Of (Repeat));

      Step := Calculator.Step_From_Flux (Flux => Repeat, Cell => Var);

      if Opt.Trace_Counters then

         Output.Trace (Text =>
              "Loop-step bounds: "
            & Storage.Bounds.Image (
                 Item => Step,
                 Name => "step(" & Storage.Image (Var) & ')'));

      end if;

      if Opt.Swap_Small_Negative_Step
      and then
         Arithmetic.Is_Small_Negative (
            Interval => Step,
            Width    => Storage.Width_of (Var))
      then

         if Opt.Warn_Swap_Small_Negative_Step then

            Output.Warning (
                 "Reversing sign of step for induction candidate"
               & Output.Field_Separator
               & Storage.Image (Var));

         end if;

         Step := Arithmetic.Opposite_Sign (
            Interval => Step,
            Width    => Storage.Width_Of (Var));

         if Opt.Trace_Counters then

            Output.Trace (Text =>
                 "Loop-step bounds, sign reversed: "
               & Storage.Bounds.Image (
                    Item => Step,
                    Name => "step(" & Storage.Image (Var) & ')'));

         end if;

      end if;

      return Step;

   exception

   when Calculator.Null_Set_Error =>
      -- The Repeat flux seems to be void.

      Output.Note ("Loop is unrepeatable (checking variable step).");

      raise Unrepeatable_Loop;

   end Step_Bounds;


   function Repeat_Counter_Bounds (
      Repeat  : Calculator.Pool_T;
      Counter : Storage.Cell_T)
   return Storage.Bounds.Interval_T
   --
   -- Bounds on the value of the Counter, in the domain of the Repeat
   -- flux of a loop.
   --
   -- The Repeat parameter is the single-dimensional pool of the
   -- values of the Counter in the domain of the loop-repeat relation.
   --
   -- May propagate Unrepeatable_Loop if the Repeat pool is void.
   --
   is

      Result : Storage.Bounds.Interval_T;
      -- The result.

   begin

      Calculator.Comment (
         Text =>
              "Check repeat bounds for counter candidate "
            & Storage.Image (Counter),
         Calc => Calculator.Owner_Of (Repeat));

      Result := Calculator.Bounds_Of (Repeat);

      if Opt.Trace_Counters then

         Output.Trace (
              "Loop-repeat bounds: "
            & Storage.Bounds.Image (Result, Storage.Image (Counter)));

      end if;

      return Result;

   exception

   when Calculator.Null_Set_Error =>
      -- The repeat flux is void: the loop seems unrepeatable.

      Output.Note ("Loop is unrepeatable (checking limit data).");

      raise Unrepeatable_Loop;

   end Repeat_Counter_Bounds;


   procedure Find_Counters_Again (Vars : in out Loop_Vars_T)
   --
   -- Restarts the scan for counter variables from the first variable.
   --
   is
   begin

      Vars.Find := 0;

   end Find_Counters_Again;


   procedure Find_Next_Counter (
      Repeat  : in     Calculator.Flux_T;
      Vars    : in out Loop_Vars_T;
      Counter :    out Storage.Cell_T;
      Step    :    out Storage.Bounds.Interval_T)
   --
   -- Finds the next counter variable among the Vars, first looking
   -- among the known induction variables. If there are no more counters
   -- there, we analyse the unprocessed variables in the Vars one by
   -- one to see which are induction variables, and stop when we find
   -- an effective induction variable: a counter.
   --
   -- If a counter if found, it is returned in Counter, with its Step
   -- bounds; otherwise Counter is returned as No_Cell and the Step
   -- bounds are undefined.
   --
   -- All induction variables and their step bounds are also stored
   -- in the Vars.
   --
   -- May propagate Unrepeatable_Loop if the Repeat flux is void.
   --
   -- This rather complicated method is devised to support the "first"
   -- option, in which we accept the first loop-bound found. By finding
   -- counter variables one by one, we avoid the analysis of the steps
   -- of the remaining variables, after the first loop-bound is found.
   --
   is
      use Storage.Bounds;

      Cand : Storage.Cell_T;
      -- A candidate variable.

      Found : Boolean := False;
      -- Whether we have found a counter.

   begin

      Found := False;

      -- First look at the induction variables known so far:

      while (Vars.Find < Vars.Num_Ind)
      and   (not Found)
      loop

         Vars.Find := Vars.Find + 1;

         Cand := Vars.Ind_Var(Vars.Find);
         Step := Vars.Step   (Vars.Find);

         Found := not Is_In (Value => 0, Interval => Step);

      end loop;

      -- If a counter was not found, analyse unprocessed variables:

      while Vars.Num_Proc < Vars.Num_Var
      and  (not Found)
      loop

         Vars.Num_Proc := Vars.Num_Proc + 1;

         Cand := Vars.Var(Vars.Num_Proc);

         Step := Step_Bounds (Cand, Repeat);
         --
         -- May propagate Unrepeatable_Loop, in which case
         -- we are done for this loop.

         if Known (Step) then
            -- This is an induction variable, because its Step
            -- has a lower bound, or an upper bound, or both.

            Vars.Num_Ind := Vars.Num_Ind + 1;

            Vars.Ind_Var(Vars.Num_Ind) := Cand;
            Vars.Step   (Vars.Num_Ind) := Step;

            Found := not Is_In (Value => 0, Interval => Step);

            if Found then
               -- This is an effective induction variable : a counter.

               Vars.Eff_Ind := True;

               Vars.Find := Vars.Num_Ind;
               -- This counter was found now.

            end if;

         end if;

      end loop;

      if Found then

         Counter := Cand;

      else

         Counter := Storage.No_Cell;

         Vars.Find := Vars.Num_Ind + 1;
         -- No more counters to be found.

      end if;

   end Find_Next_Counter;


   procedure Find_Induction_Variables (
      Repeat  : in     Calculator.Flux_T;
      Vars    : in out Loop_Vars_T)
   --
   -- Finds all the (remaining) indunction variables in the
   -- loop with the given Repeat flux (already restricted to
   -- the domain of initial values).
   --
   -- May propagate Unrepeatable_Loop.
   --
   is
      use type Storage.Cell_T;

      Counter : Storage.Cell_T;
      Step    : Storage.Bounds.Interval_T;
      -- A counter variable and its step bounds.

   begin

      loop

         Find_Next_Counter (
            Repeat  => Repeat,
            Vars    => Vars,
            Counter => Counter,
            Step    => Step);

         exit when Counter = Storage.No_Cell;
         -- All variables processed.

      end loop;

      if Opt.Trace_Counters then

         Output.Trace (
              "Induction variables: "
            & Storage.Image (Vars.Ind_Var(1 .. Vars.Num_Ind)));

      end if;

   end Find_Induction_Variables;


   Joint_Counter_Name : constant String := "#";
   --
   -- The name for the joint iteration counter, in Trace lines.


   function Joint_Induction_Bound (
      Count_Repeat : Calculator.Pool_T)
   return Storage.Bounds.Limit_T
   --
   -- Finds a repetition limit on a loop by analysing the domain
   -- of values of the joint iteration Count in the inductive
   -- Repeat relation, to find bounds on the Count.
   --
   -- Count_Repeat is the domain of the inductive loop-repeat
   -- relation, projected to joint-counter variable.
   --
   -- This is the "positive" form of the joint-counter analysis.
   --
   -- Propagates Unrepeatable_Loop if loop-repetition turns out
   -- to be impossible.
   --
   is
      use Storage.Bounds;
      use type Arithmetic.Value_T;

      Count_Bound : Interval_T;
      -- The bounds, if any, on the joint iteration counter, from
      -- Count_Repeat.

      Count : Arithmetic.Value_T;
      -- The limiting value of the joint iteration counter.

   begin

      -- Find the bounds on the joint iteration counter implied
      -- by the inductive repeat relation:

      Calculator.Comment (
         Text => "Bounds on joint iteration counter",
         Calc => Calculator.Owner_Of (Count_Repeat));

      Count_Bound := Calculator.Bounds_Of (Count_Repeat);
      --
      -- May propagate Null_Set_Error.

      -- So, the loop seems to be repeatable, at least.

      if Opt.Trace_Counters then

         Output.Trace (
              "Joint counter bounds for repeat: "
            & Storage.Bounds.Image (
                 Item => Count_Bound,
                 Name => Joint_Counter_Name));

      end if;

      if Known (Count_Bound.Max) then
         -- This is an upper bound on the iteration counter for
         -- the last iteration that can take a repeat edge.

         Count := Value (Count_Bound.Max) + 1;
         -- The "+1" because iterations are counted from zero.

         return Limit (Count);

      else
         -- No upper bound known here.

         return Not_Limited;

      end if;

   exception

   when Calculator.Null_Set_Error =>
      -- The Count_Repeat flux seems to be void.

      Output.Note ("Loop is unrepeatable (checking joint counter).");

      raise Unrepeatable_Loop;

   end Joint_Induction_Bound;


   function Joint_Induction_Limit (
      Count_Repeat : Calculator.Pool_T)
   return Storage.Bounds.Limit_T
   --
   -- Finds a repetition limit on a loop by analysing the complement
   -- of the domain of values of the joint iteration Count in the
   -- inductive Repeat relation, to find a limiting Count value that
   -- makes the loop terminate.
   --
   -- Count_Repeat is the domain of the inductive loop-repeat
   -- relation, projected to joint-counter variable.
   --
   -- This is the "negative" form of the joint-counter analysis.
   --
   is

      First_Exit : Arithmetic.Value_T;
      -- The smallest non-negative value of the joint iteration
      -- counter that terminates the loop.

   begin

      Calculator.Comment (
         Text => "Joint induction limit search",
         Calc => Calculator.Owner_Of (Count_Repeat));

      First_Exit := Calculator.Smallest_Hole (
         Pool => Count_Repeat,
         Min  => 0);
      --
      -- Since the joint iteration counter always starts from zero
      -- and then grows, we must omit all negative values. They
      -- represent exits that occur "before" the initial values.

      if Opt.Trace_Counters then

         Output.Trace (
              "Loop exits for "
            & Joint_Counter_Name
            & '='
            & Arithmetic.Image (First_Exit));

      end if;

      return Storage.Bounds.Limit (First_Exit);

   exception

   when Calculator.Null_Set_Error =>
      -- The complement of the Ind_Repeat flux seems to be void.
      -- The loop may be eternal, or the values of the induction
      -- variables variables and their steps may be so variable
      -- (eg. context-dependent) that the loop can potentially
      -- terminate after any number of iterations.

      Output.Note ("Loop unbounded (checking joint counter).");

      return Storage.Bounds.Not_Limited;

   end Joint_Induction_Limit;


   procedure Find_Per_Counter_Bound (
      Counter     : in     Storage.Cell_T;
      Initial     : in     Calculator.Pool_T;
      Step        : in     Storage.Bounds.Interval_T;
      Repeat      : in     Calculator.Pool_T;
      Init_Bounds : in out Storage.Bounds.Interval_T;
      Result      :    out Storage.Bounds.Limit_T)
   --
   -- Finds a repetition limit on a loop by analysing the Initial
   -- values, the Step, and the Repeat relation for a given Counter
   -- cell, when this Counter cell is considered an unbounded
   -- integer variable (not a modular one).
   --
   -- This is the "positive" form of the "each counter" loop analysis,
   -- for unbounded variables.
   --
   -- The loop repeat relation is the relation R(t,t') that contains
   -- those pairs of tuples t,t' for which execution of the loop
   -- starting with the variable values in t can reach a repeat
   -- edge with the variable values in t'.
   --
   -- For this analysis to work, the bounds on the Initial value,
   -- the Step, and the values that let the loop Repeat must have
   -- matching properties as follows:
   --
   -- > The Step bounds must exclude zero, that is be all negative
   --   or all positive.
   --
   -- > If the Step is positive:
   --    o  Initial must have a lower bound.
   --    o  Repeat must have an upper bound
   --
   -- > If the Step is negative:
   --    o  Initial must have an upper bound.
   --    o  Repeat must have a lower bound.
   --
   -- In each case, it follows that the number of Counter steps is
   -- limited from above by the distance between the "earliest"
   -- Initial value and the "latest" Repeat value. On the next step,
   -- the Counter passes beyond the Repeat pool and the loop must
   -- terminate.
   --
   -- Counter
   --    A loop induction variable.
   -- Initial
   --    The initial values for the loop, of all variables.
   -- Step
   --    The (single possible) induction step of the Counter.
   --    Currently restricted to +1 or -1 for this analysis.
   -- Repeat
   --    The values of the Counter in the domain of the repeat
   --    relation of the loop. Other variables are not included
   --    so this is a one-dimensional pool.
   -- Init_Bounds
   --    Bounds on the Initial value of the Counter, computed
   --    here if (and only if) Step and Repeat are consistent
   --    with a possible loop repetition bound, so that the
   --    initial Counter values are relevant. Otherwise not
   --    changed.
   -- Result
   --    The loop repetition limit.
   --
   -- May propagate Unrepeatable_Loop if the Repeat pool turns
   -- out to be empty.
   --
   -- May propagate Infeasible_Loop if the Initial pool turns
   -- out to be empty.
   --
   is
      use Storage.Bounds;
      use type Arithmetic.Value_T;

      type Role_T is (Up_Counter, Down_Counter, None);
      -- The types of counters we know of.

      subtype Counter_Role_T is Role_T range Up_Counter .. Down_Counter;
      -- The promising counter types.

      Role : Role_T;
      -- The possible role of the Counter, from Step and Repeat.

      Counter_Name : constant String := Storage.Image (Counter);
      -- The name of the counter cell.

      Repeat_Bounds : Interval_T;
      -- Bounds on the counter, implied by repetition of the loop.

      Rep : Arithmetic.Value_T;
      -- The upper bound on loop repetitions.

   begin

      Result := Not_Limited;
      -- Initially not known.

      -- Check the Step bounds:

      if       Known (Step.Min)
      and then Value (Step.Min) > 0
      then
         -- Counting up:

         Role := Up_Counter;

      elsif    Known (Step.Max)
      and then Value (Step.Max) < 0
      then
         -- Counting down:

         Role := Down_Counter;

      else
         -- Not good.

         Role := None;

      end if;

      -- Check the Repeat bounds if still relevant:

      if Role in Counter_Role_T then

         Repeat_Bounds := Repeat_Counter_Bounds (Repeat, Counter);
         --
         -- May propagate Unrepeatable_Loop.

         case Counter_Role_T (Role) is

         when Up_Counter =>

            if not Known (Repeat_Bounds.Max) then
               -- Counting up, but not bounded from above.

               Role := None;

            end if;

         when Down_Counter =>

            if not Known (Repeat_Bounds.Min) then
               -- Counting down, but not bounded from below.

               Role := None;

            end if;

         end case;

      end if;

      -- Check the Initial bounds if still relevant:

      if Role in Counter_Role_T  then

         Init_Bounds := Initial_Counter_Bounds (Initial, Counter);
         --
         -- May propagate Infeasible_Loop.

         case Counter_Role_T (Role) is

         when Up_Counter =>

            if Known (Init_Bounds.Min) then
               -- Longest from Init.Min by Step.Min to Repeat.Max.

               Rep := Ceil (
                  Left  => Value (Repeat_Bounds.Max)
                         - Value (  Init_Bounds.Min) + 1,
                  Right => Value (Step.Min));

               if Opt.Trace_Counters then

                  Output.Trace (
                       Counter_Name
                     & Output.Field_Separator
                     & "Up_Counter"
                     & Output.Field_Separator
                     & Arithmetic.Image (Rep));

               end if;

               Result := Limit (Rep);

            end if;

         when Down_Counter =>

            if Known (Init_Bounds.Max) then
               -- Longest from Init.Max by Step.Max to Repeat.Min.

               Rep := Ceil (
                  Left  => Value (Repeat_Bounds.Min)
                         - Value (  Init_Bounds.Max) - 1,
                  Right => Value (Step.Max));

               if Opt.Trace_Counters then

                  Output.Trace (
                       Counter_Name
                     & Output.Field_Separator
                     & "Down_Counter"
                     & Output.Field_Separator
                     & Arithmetic.Image (Rep));

               end if;

               Result := Limit (Rep);

            end if;

         end case;

      end if;

   end Find_Per_Counter_Bound;


   procedure Find_Per_Modular_Counter_Bound (
      Counter     : in     Storage.Cell_T;
      Initial     : in     Calculator.Pool_T;
      Step        : in     Storage.Bounds.Interval_T;
      Repeat      : in     Calculator.Pool_T;
      Init_Bounds : in out Storage.Bounds.Interval_T;
      Result      :    out Storage.Bounds.Limit_T)
   --
   -- Finds a repetition limit on a loop by analysing the Initial
   -- values, the Step, and the Repeat relation for a given Counter
   -- cell, when this Counter is considered a bounded integer
   -- variable, modulus its width in bits.
   --
   -- This is the "positive" form of the "each counter" loop analysis,
   -- for modular variables.
   --
   -- All parameters have the same meaning as in Find_Per_Counter_Bound,
   -- above, for unbounded variables.
   --
   -- The Step interval is assumed to be expressed in terms of signed
   -- values modulo the width of the Counter.
   --
   -- If the Init_Bounds are computed here, they are bounds on the
   -- initial value of the Counter, unsigned-modulo its width.
   --
   is
      use Storage.Bounds;
      use type Arithmetic.Value_T;

      type Role_T is (Up_Counter, Down_Counter, None);
      -- The types of counters we know of.

      subtype Counter_Role_T is Role_T range Up_Counter .. Down_Counter;
      -- The promising counter types.

      Role : Role_T;
      -- The possible role of the Counter, from Step and Repeat.

      Counter_Name : constant String := Storage.Image (Counter);
      -- The name of the counter cell.

      Repeat_Bounds : Interval_T;
      -- Bounds on the unsigned counter, modulo its width, implied by
      -- repetition of the loop.

      Rep : Arithmetic.Value_T;
      -- The upper bound on loop repetitions.

   begin

      Result := Not_Limited;
      -- Initially not known.

      -- Check the Step bounds:

      if       Known (Step.Min)
      and then Value (Step.Min) > 0
      then
         -- Counting up:

         Role := Up_Counter;

      elsif    Known (Step.Max)
      and then Value (Step.Max) < 0
      then
         -- Counting down:

         Role := Down_Counter;

      else
         -- Not good.

         Role := None;

      end if;

      -- Check the Repeat bounds if still relevant:

      -- TBA/TBD signed and/or unsigned modular bounds

      if Role in Counter_Role_T then

         Repeat_Bounds := Repeat_Counter_Bounds (Repeat, Counter);
         --
         -- May propagate Unrepeatable_Loop.

         case Counter_Role_T (Role) is

         when Up_Counter =>

            if not Known (Repeat_Bounds.Max) then
               -- Counting up, but not bounded from above.

               Role := None;

            end if;

         when Down_Counter =>

            if not Known (Repeat_Bounds.Min) then
               -- Counting down, but not bounded from below.

               Role := None;

            end if;

         end case;

      end if;

      -- Check the Initial bounds if still relevant:

      if Role in Counter_Role_T  then

         Init_Bounds := Initial_Counter_Bounds (Initial, Counter);
         --
         -- May propagate Infeasible_Loop.

         case Counter_Role_T (Role) is

         when Up_Counter =>

            if Known (Init_Bounds.Min) then
               -- Longest from Init.Min by Step.Min to Repeat.Max.

               Rep := Ceil (
                  Left  => Value (Repeat_Bounds.Max)
                         - Value (  Init_Bounds.Min) + 1,
                  Right => Value (Step.Min));

               if Opt.Trace_Counters then

                  Output.Trace (
                       Counter_Name
                     & Output.Field_Separator
                     & "Up_Counter"
                     & Output.Field_Separator
                     & Arithmetic.Image (Rep));

               end if;

               Result := Limit (Rep);

            end if;

         when Down_Counter =>

            if Known (Init_Bounds.Max) then
               -- Longest from Init.Max by Step.Max to Repeat.Min.

               Rep := Ceil (
                  Left  => Value (Repeat_Bounds.Min)
                         - Value (  Init_Bounds.Max) - 1,
                  Right => Value (Step.Max));

               if Opt.Trace_Counters then

                  Output.Trace (
                       Counter_Name
                     & Output.Field_Separator
                     & "Down_Counter"
                     & Output.Field_Separator
                     & Arithmetic.Image (Rep));

               end if;

               Result := Limit (Rep);

            end if;

         end case;

      end if;

   end Find_Per_Modular_Counter_Bound;


   function Per_Counter_Limit (
      Counter    : Storage.Cell_T;
      Initial    : Storage.Bounds.Interval_T;
      Step       : Arithmetic.Value_T;
      Not_Repeat : Calculator.Pool_T)
   return Storage.Bounds.Limit_T
   --
   -- Finds a repetition limit on a loop by analysing the Initial values,
   -- the (constant) Step, and the complement of the Repeat relation for
   -- a given Counter, using the complement of the Counter value domain
   -- to find a limiting Counter value that makes the loop terminate.

   -- This is the "negative" form of the "each counter" loop analysis.
   --
   -- The loop repeat relation is the relation R(t,t') that contains
   -- those pairs of tuples t,t' for which execution of the loop
   -- starting with the variable values in t can reach a repeat
   -- edge with the variable values in t'.
   --
   -- For this analysis to work, the Initial value of the Counter
   -- must have both a lower and an upper bound, say Imin..Imax.
   --
   -- Assume that the Step is positive (+1); the negative case is
   -- symmetric. The analysis tries to find a "final" Counter value,
   -- larger or equal to Imax, such that the loop must terminate
   -- at this final value (if not earlier). In this way:
   --
   -- > Compute the domain of R:
   --
   --   domain := { t | exists t': R(t,t') }
   --
   -- > Find the values of Counter in the domain of R:
   --
   --   Repeat := { v | exists t,t': Counter(t) = v and R(t,t') }
   --
   --   where Counter(t) means the value of the Counter variable
   --   in the tuple t.
   --
   -- > Compute the complement, including only values >= Imax:
   --
   --   cpl := (complement (Repeat)) intersect Imax..inf
   --        = { v >= Imax | for all t,t' with Counter(t) = v: not R(t,t') }
   --
   -- > Find the least value in cpl:
   --
   --   final := min (cpl).
   --
   -- If this computation succeeds, the loop is sure to terminate
   -- when Counter reaches the final value. This happens after at
   -- most "final - Imin" repetitions of the loop.
   --
   -- Counter
   --    A loop induction variable.
   -- Initial
   --    Bounds on the initial value of the Counter.
   --    Must be a finite interval.
   -- Step
   --    The (single possible) induction step of the Counter.
   --    Currently restricted to +1 or -1 for this analysis.
   -- Not_Repeat
   --    The values of the Counter not in the domain of the repeat
   --    relation of the loop. Other variables are not included
   --    so this is a one-dimensional pool.
   --
   -- Preconditions:
   --    Step = +1 or Step = -1.
   --    Initial is a finite interval (Storage.Bounds.Known).
   --
   is
      use Storage.Bounds;
      use type Arithmetic.Value_T;

      Counter_Name : constant String := Storage.Image (Counter);
      -- The name of the counter cell.

      First_Exit : Arithmetic.Value_T;
      -- The first (smallest for +ve step, largest for -ve step) value
      -- of the Counter for which the loop exits.

      Final : Limit_T;
      -- The actual final Counter value.

      Result : Limit_T := Not_Limited;
      -- The computed bound on loop repetitions.

   begin

      -- Check precondition on Step:

      if abs Step /= 1 then

         Output.Fault (
            Location => "Bounds.Looping.Limit_By_Complement",
            Text     => "Step is " & Arithmetic.Image (Step));

         return Not_Limited;

      end if;

      -- Find a final Counter value, from the complement
      -- of the Counter values in the domain of Repeat:

      Calculator.Comment (
         Text =>
              "Check complement-limit for counter candidate "
            & Counter_Name,
         Calc => Calculator.Owner_Of (Not_Repeat));

      -- The following may propagate Calculator.Null_Set_Error
      -- if the Smallest/Largest_Value is not found:

      if Step > 0 then
         -- Going up, so we must find a final value
         -- no less than the largest Initial value.

         First_Exit := Calculator.Smallest_Value (
            Pool => Not_Repeat,
            Min  => Value (Initial.Max));

      else
         -- Going down, so we must find a final value
         -- no greater than the smallest Initial value.

         First_Exit := Calculator.Largest_Value (
            Pool => Not_Repeat,
            Max  => Value (Initial.Min));

      end if;

      -- We have a First_Exit value.

      if Opt.Trace_Counters then

         Output.Trace (
              "Loop exits for "
            & Counter_Name
            & '='
            & Arithmetic.Image (First_Exit));

      end if;

      if Step > 0 then

         Result := Limit (First_Exit - Value (Initial.Min));

      else

         Result := Limit (Value (Initial.Max) - First_Exit);

      end if;

      -- Report the repetition limit, if found:

      if  Known (Result)
      and Opt.Trace_Counters
      then

         Output.Trace (
              "Count from "
            & Image (Initial, Counter_Name)
            & " by "
            & Arithmetic.Image (Step)
            & " to "
            & Arithmetic.Image (First_Exit)
            & Output.Field_Separator
            & Arithmetic.Image (Value (Result)));

      end if;

      return Result;

   exception

   when Calculator.Null_Set_Error =>
      -- A Final_Bound was not found; the set was empty.
      -- This means that we have not found a Counter
      -- value that is impossible for the Repeat flux.

      return Not_Limited;

   end Per_Counter_Limit;


   procedure General_Procedure (
      Initial      : in     Calculator.Pool_T;
      Repeat       : in     Calculator.Flux_T;
      Repeat_Inv   : in     Calculator.Cell_Set_T;
      Candidates   : in     Storage.Cell_List_T;
      Param        : in     Opt.Proc_Param_T;
      Repeat_Limit :    out Storage.Bounds.Limit_T)
   --
   -- The "general" procedure for applying various forms of
   -- loop-bounds analysis to a given loop. This general
   -- procedure is controlled by the Params to implement a
   -- specific procedure.
   --
   -- May propagate Infeasible_Loop or Unrepeatable_Loop.
   --
   is
      use Storage;
      use Storage.Bounds;
      use type Arithmetic.Value_T;

      Calc : constant Calculator.Calc_Handle_T :=
         Calculator.Owner_Of (Initial);
      -- The calculator instance we use.

      Vars : Loop_Vars_T (Num_Var => Candidates'Length);
      -- Information about the induction variables.

      Ind_Repeat : Calculator.Flux_T;
      -- The Repeat flux, constrained by the joint inductive
      -- properties of the induction variables.

      Ind_Count_Repeat : Calculator.Pool_T;
      -- The set of joint iteration counts in the domain
      -- of Ind_Repeat.

      Counter : Cell_T;
      -- A counter, for the per-counter analysis.

      Initial_Bounds : Interval_T := Void_Interval;
      -- Bounds on the Initial value of the Counter.
      -- Void_Interval means that the bounds are not yet computed.

      Step : Interval_T;
      -- Bounds on the step of the Counter.

      Counter_Repeat : Calculator.Pool_T;
      -- The Counter values in the domain of the Repeat relation.
      -- Other variables are not included.

      Counter_Not_Repeat : Calculator.Pool_T;
      -- The complement of Counter_Repeat, if needed.

      Counter_Limit : Limit_T;
      -- Loop repetition limit from this Counter.

      Counter_Bounds_Loop : Boolean;
      -- Whether this Counter bounds the loop repetitions by the
      -- positive form of the per-counter analysis.

      Done : Boolean := False;
      -- Whether the analysis is finished.


      procedure Set_Repeat_Limit (Limit : in Limit_T)
      --
      -- Sets a Limit on the number of loop repetitions.
      --
      is
      begin

         Repeat_Limit := And_Max (Repeat_Limit, Limit);

         if Param.First and Known (Repeat_Limit) then
            -- We are content with this limit.

            Done := True;

         end if;

      end Set_Repeat_Limit;


   begin  -- General_Procedure

      Repeat_Limit := Not_Limited;

      -- Initialize Vars:

      Vars.Var := Candidates;

      -- Vars.Num_Proc = 0    : No variables processed yet.
      -- Vars.Num_Var  = 0    : No induction variables known yet.
      -- Vars.Eff_Ind  = False: No effective induction variables known.

      -- Possible joint induction counter bounds:

      if Param.Joint then
         -- Analyse all induction variables together, using a joint
         -- counter and an induction model, and try to find positive
         -- bounds on the joint counter in the repeat flux.

         Find_Induction_Variables (
            Repeat => Repeat,
            Vars   => Vars);

         if Vars.Eff_Ind then
            -- There is at least one effective induction variable.

            -- Model the induction variables, at the start of the loop,
            -- as functions of their initial values and of the synthetic
            -- iteration counter:

            Calculator.Comment (
               Text => "Compute inductive repeat relation",
               Calc => Calc);

            Ind_Repeat := Calculator.Repeat_With_Induction (
               Initial   => Initial,
               Invariant => Repeat_Inv,
               Induction => Vars.Ind_Var(1 .. Vars.Num_Ind),
               Step      => Vars.Step   (1 .. Vars.Num_Ind),
               Repeat    => Repeat);

            if Opt.Use_Positive_Form then
               -- Try the "positive" joint-counter analysis: see if
               -- the inductive repeat relation implies an upper
               -- bound on the joint counter.

               Ind_Count_Repeat := Calculator.Domain_Of (Ind_Repeat);

               Set_Repeat_Limit (Joint_Induction_Bound (Ind_Count_Repeat));

            end if;

         end if;

      end if;

      -- Possible bounds or limits from each counter:

      if (not Done) and Param.Each then
         -- Analyse each counter separately.

         Find_Counters_Again (Vars);

         Scan_Counters :
         loop

            Find_Next_Counter (
               Repeat  => Repeat,
               Vars    => Vars,
               Counter => Counter,
               Step    => Step);

            exit Scan_Counters when Counter = Storage.No_Cell;

            Calculator.Comment (
               Text => "Analysing counter cell " & Storage.Image (Counter),
               Calc => Calculator.Owner_Of (Repeat));

            -- Possible bounds on this Counter:

            if Param.Joint
            or (not Opt.Use_Positive_Form)
            then
               -- The positive form of the joint-counter analysis
               -- subsumes the positive form of the per-counter
               -- analysis, so the latter can be omitted. Of course
               -- it is omitted also if Use_Positive_Form is False.

               Counter_Bounds_Loop := False;

            else
               -- Try the positive form of the per-counter analysis.

               Counter_Repeat := Calculator.Values_In_Domain (
                  Flux => Repeat,
                  Cell => Counter);

               Find_Per_Counter_Bound (
                  Counter     => Counter,
                  Initial     => Initial,
                  Step        => Step,
                  Repeat      => Counter_Repeat,
                  Init_Bounds => Initial_Bounds,
                  Result      => Counter_Limit);

               Counter_Bounds_Loop := Known (Counter_Limit);

               Set_Repeat_Limit (Counter_Limit);

            end if;

            exit Scan_Counters when Done;

            -- Possible limit on this Counter:

            if  ((not Counter_Bounds_Loop) or Param.Neg)
            and (Singular (Step) and then abs Single_Value (Step) = 1)
            then
               -- The positive form of the per-counter analysis failed,
               -- or was omitted, or we are asked to do the negative
               -- form in any case, and the negative form may be applicable
               -- because the step is +1 or -1.

               if Void (Initial_Bounds) then
                  -- Not yet computed above, and needed now.

                  Initial_Bounds := Initial_Counter_Bounds (Initial, Counter);

               end if;

               if Bounded (Initial_Bounds) then
                  -- The negative per-counter analysis is applicable.

                  if Param.Joint then
                     -- This was not done yet, and we need it now:

                     Counter_Repeat := Calculator.Values_In_Domain (
                        Flux => Repeat,
                        Cell => Counter);

                  end if;

                  Counter_Not_Repeat := Calculator.Complement (Counter_Repeat);

                  Counter_Limit := Per_Counter_Limit (
                     Counter    => Counter,
                     Initial    => Initial_Bounds,
                     Step       => Single_Value (Step),
                     Not_Repeat => Counter_Not_Repeat);

                  Set_Repeat_Limit (Counter_Limit);

               end if;

            end if;

            exit Scan_Counters when Done;

         end loop Scan_Counters;

      end if;

      -- Possible joint induction counter limits:

      if (not Done) and Param.Joint and Vars.Eff_Ind then
         -- Try the negative form of the joint counter analysis.

         if not Opt.Use_Positive_Form then
            -- Compute the domain of the inductive repeat relation,
            -- as we skipped it above.

            Ind_Count_Repeat := Calculator.Domain_Of (Ind_Repeat);

         end if;

         Set_Repeat_Limit (Joint_Induction_Limit (Ind_Count_Repeat));

      end if;

   end General_Procedure;


   procedure Bound_Loop (
      Luup        : in Loops.Loop_T;
      Initial     : in Calculator.Pool_T;
      Repeat      : in Calculator.Flux_T;
      Repeat_Inv  : in Calculator.Cell_Set_T;
      Inherit_Inv : in Storage.Cell_Set_T;
      Exec_Bounds : in Programs.Execution.Bounds_Ref)
   is
      use type Arithmetic.Value_T;

      Loop_Index : constant Loops.Loop_Index_T := Loops.Loop_Index (Luup);
      -- The index number of the loop, for display only.

      Var_List : constant Storage.Cell_List_T :=
         Storage.To_List (
            From   => Calculator.Cells_Of (Repeat),
            Except => Repeat_Inv);
      -- The list of loop-variant cells.

      Candidates : constant Storage.Cell_List_T :=
         Storage.Counter_Cells (Var_List);
      -- The list of counter candidate cells.

      Model : constant Flow.Computation.Model_Handle_T :=
         Programs.Execution.Computation (Exec_Bounds);
      -- The computation model for this analysis.

      Repeat_Limit : Storage.Bounds.Limit_T;
      -- Computed bounds on the repetitions of the loop.

      Repeat_Value : Arithmetic.Value_T;
      -- The computed maximum number of loop-repetitions.
      -- Possibly negative, although unlikely.

      Repeat_Count : Flow.Execution.Count_T;
      -- The computed maximum number of loop-repetitions,
      -- non-negative.


      procedure Trace_Finish
      is
      begin

         if Opt.Trace_Counters then

            Output.Trace (
                 "Finished loop-counter analysis for loop"
               & Loops.Loop_Index_T'Image (Loop_Index));

         end if;

      end Trace_Finish;


   begin  -- Bound_Loop

      -- Show time:

      if Opt.Trace_Counters then

         Output.Trace (
              "Starting loop-counter analysis for loop"
            & Loops.Loop_Index_T'Image (Loop_Index));

         Output.Trace (
              "Invariant cells: "
            & Calculator.Image (Repeat_Inv));

         Output.Trace (
              "Variant cells: "
            & Storage.Image (Var_List));

         Output.Trace (
              "Candidates for counters: "
            & Storage.Image (Candidates));

      end if;

      -- Do it in your own way:

      General_Procedure (
         Initial      => Initial,
         Repeat       => Repeat,
         Repeat_Inv   => Repeat_Inv,
         Candidates   => Candidates,
         Param        => Opt.Param(Opt.Proc),
         Repeat_Limit => Repeat_Limit);

      -- Check if a credible bound was computed:

      if Storage.Bounds.Known (Repeat_Limit) then
         -- A bound was computed.

         Repeat_Value := Storage.Bounds.Value (Repeat_Limit);

         if Storage.Bounds.Over (Repeat_Value, Opt.Max_Loop_Bound) then
            -- The bound is too large to be credible (useful).

            Output.Warning (
                 "Large loop-bound ignored as spurious"
               & Output.Field_Separator
               & Arithmetic.Image (Repeat_Value));

         else
            -- The bound is small enough to be credible.

            if Repeat_Value < 0 then

               Output.Warning (
                    "Negative loop-bound "
                  & Arithmetic.Image (Repeat_Value)
                  & " taken as zero (loop not repeatable).");

               Repeat_Count := 0;

            else

               Repeat_Count := Flow.Execution.Count_T (Repeat_Value);

            end if;

            Programs.Execution.Bound_Loop_Repeats (
               Luup    => Luup,
               Repeats => (Min => 0, Max => Repeat_Count),
               Within  => Exec_Bounds);

            Output.Result (
               Key   => "Loop_Bound",
               Text  => Flow.Execution.Image (Repeat_Count));

            if Repeat_Count = 0 then
               -- We found that the loop cannot repeat.

               Output.Note ("Loop is unrepeatable (zero bound).");

               raise Unrepeatable_Loop;

            end if;

         end if;

      end if;

      -- Closing time:

      Trace_Finish;

   exception

   when Infeasible_Loop =>

      if Flow.Pruning.Opt.Warn_Unreachable then

         Output.Warning ("Unreachable loop.");

      end if;

      Flow.Computation.Mark_Infeasible (
         Luup  => Luup,
         Under => Model.all);

      Trace_Finish;

   when Unrepeatable_Loop =>

      if Flow.Pruning.Opt.Warn_Unreachable then

         Output.Warning ("Unrepeatable loop.");

      end if;

      Flow.Computation.Mark_Infeasible (
         Edges => Flow.Computation.Repeat_Edges (Luup, Model.all),
         Under => Model.all);

      Trace_Finish;

   end Bound_Loop;


end Bounds.Looping;
