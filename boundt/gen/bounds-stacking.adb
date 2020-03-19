-- Bounds.Stacking (body)
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
-- $Revision: 1.13 $
-- $Date: 2015/10/24 20:05:46 $
--
-- $Log: bounds-stacking.adb,v $
-- Revision 1.13  2015/10/24 20:05:46  niklas
-- Moved to free licence.
--
-- Revision 1.12  2012-02-13 17:52:19  niklas
-- BT-CH-0230: Options -max_loop and -max_stack for spurious bounds.
--
-- Revision 1.11  2009-11-27 11:28:06  niklas
-- BT-CH-0184: Bit-widths, Word_T, failed modular analysis.
--
-- Revision 1.10  2009/01/18 07:54:34  niklas
-- Removed unused context clauses and locals.
--
-- Revision 1.9  2008/07/23 09:07:15  niklas
-- BT-CH-0139: Fix recursion in Programs.Execution.Paths.
--
-- Revision 1.8  2007/12/17 13:54:34  niklas
-- BT-CH-0098: Assertions on stack usage and final stack height, etc.
--
-- Revision 1.7  2007/08/17 14:43:59  niklas
-- BT-CH-0074: Stable and Unstable stacks.
--
-- Revision 1.6  2007/03/18 12:50:37  niklas
-- BT-CH-0050.
--
-- Revision 1.5  2005/10/20 11:28:28  niklas
-- BT-CH-0015.
--
-- Revision 1.4  2005/04/17 09:15:42  niklas
-- Changed all Output.Unknown messages to Output.Error.
--
-- Revision 1.3  2005/02/23 09:05:14  niklas
-- BT-CH-0005.
--
-- Revision 1.2  2005/02/20 15:15:35  niklas
-- BT-CH-0004.
--
-- Revision 1.1  2005/02/16 21:11:39  niklas
-- BT-CH-0002.
--


with Arithmetic;
with Bounds.Stacking.Opt;
with Flow.Computation;
with Flow.Pruning.Opt;
with Flow.Show;
with Output;
with Programs.Execution.Opt;


package body Bounds.Stacking is


   procedure Apply_Assertions (
      Asserts     : in Assertions.Assertion_Set_T;
      Exec_Bounds : in Programs.Execution.Bounds_Ref)
   is
      use Storage.Bounds;

      Subprogram : constant Programs.Subprogram_T :=
         Programs.Execution.Subprogram (Exec_Bounds);
      -- The subprogram in question.

      Program : constant Programs.Program_T := Programs.Program (Subprogram);
      -- The program under analysis.

      Usage : constant Assertions.Stack_Bounds_T :=
         Assertions.Stack_Usage (Subprogram, Asserts);
      -- The asserted bounds on stack usage.

      Final : constant Assertions.Stack_Bounds_T :=
         Assertions.Final_Stack_Height (Subprogram, Asserts);
      -- The asserted bounds on final stack height.

      Stack : Programs.Stack_T;
      -- On the the stacks in the program.

      Usage_Bounded, Final_Bounded : Boolean;
      -- Whether a bound is asserted on the usage or final height.

      Max_Usage : Arithmetic.Value_T;
      -- The asserted (maximum) value of stack usage.

   begin

      for S in 1 .. Programs.Number_Of_Stacks (Program) loop

         Stack := Programs.Stack_By (Index => S, Within => Program);

         -- What is asserted for this stack?

         Usage_Bounded := S in Usage'Range
            and then not Unlimited (Usage(S).Max);

         Final_Bounded := S in Final'Range
            and then not Universal (Final(S));

         -- Apply stack-usage assertion, if any:

         if Usage_Bounded then
            -- An upper bound is asserted on the usage of this stack.

            Max_Usage := Value (Usage(S).Max);

            Programs.Execution.Bound_Stack_Usage (
               Stack => Stack,
               To    => (
                  State  => Programs.Execution.Asserted,
                  Height => Max_Usage,
                  Call   => Programs.No_Call),
               Within => Exec_Bounds);

         end if;

         -- Apply final-height assertion, if any:

         if Final_Bounded then
            -- Some bounds asserted for the final stack height.

            Programs.Execution.Bound_Final_Stack_Height (
               Stack  => Stack,
               To     => Programs.Execution.To_Stack_Limit (Final(S)),
               Within => Exec_Bounds);

         end if;

      end loop;

   end Apply_Assertions;


   procedure Bound_Local_Stack_Height (
      Stack_Steps      : in Flow.Step_List_T;
      Into_Stack_Steps : in Calculator.Pool_List_T;
      Exec_Bounds      : in Programs.Execution.Bounds_Ref)
   is
      use type Storage.Cell_T;
      use type Programs.Execution.Stack_Limit_T;

      Model : constant Flow.Computation.Model_Handle_T :=
         Programs.Execution.Computation (Exec_Bounds);
      -- A handle on the computation model, for using it and updating it.

      Subprogram : constant Programs.Subprogram_T :=
         Programs.Execution.Subprogram (Exec_Bounds);
      -- The subprogram under analysis.

      Program : constant Programs.Program_T :=
         Programs.Execution.Program (Exec_Bounds);
      -- The program under analysis. We need its Symbol_Table for show.


      procedure Bound_Step (
         Step  : in     Flow.Step_T;
         Pool  : in     Calculator.Pool_T;
         Stack : in     Programs.Stack_T;
         Cell  : in     Storage.Cell_T;
         Limit :    out Programs.Execution.Stack_Limit_T)
      --
      -- Bounds the height of the Stack represented by a given stack-height
      -- Cell at one stack-changing Step, with a given data-Pool into the
      -- Step, giving the Limit for this Step.
      -- If the Step is found to be infeasible, the Step is so marked
      -- in the Model and the Model is pruned.
      --
      is

         Index : constant Flow.Step_Index_T := Flow.Index (Step);
         -- The index of the Step.

         Step_Mark : Output.Nest_Mark_T;
         -- Marks the locus of the Step.

      begin

         Step_Mark := Output.Nest (
            Flow.Show.Locus (Step, Programs.Symbol_Table (Program)));

         Calculator.Comment (
            Text =>
                 "Local stack height for "
               & Programs.Name (Stack)
               & " at step"
               & Flow.Step_Index_T'Image (Index),
            Calc => Calculator.Owner_Of (Pool));

         Limit := Programs.Execution.To_Stack_Limit (
            Calculator.Bounds_After_Step (
               Into_Step => Pool,
               Effect    => Flow.Computation.Effect (Step, Model.all),
               Cell      => Cell));

         Opt.Ignore_Huge_Bounds (Limit);

         if Programs.Execution.Opt.Trace_Stack_Bounds then

            Output.Trace (
                 "Local stack height"
               & Output.Field_Separator
               & Programs.Name (Stack)
               & Output.Field_Separator
               & Programs.Execution.Image (Limit));

         end if;

         Output.Unnest (Step_Mark);

      exception

      when Calculator.Null_Set_Error =>
         -- Cannot reach this Step in this Model.

         if Flow.Pruning.Opt.Warn_Unreachable then

            Output.Warning (
                 "Step #"
               & Flow.Step_Index_T'Image (Index)
               & " is infeasible (for local stack-height of "
               & Programs.Name (Stack)
               & ").");

         end if;

         Programs.Execution.Mark_Infeasible (
            Step   => Step,
            Within => Exec_Bounds);

         Output.Unnest (Step_Mark);

      when others =>
          -- Clean up the output locus and punt:

         Output.Unnest (Step_Mark);

         raise;

      end Bound_Step;


      Stacks : constant Programs.Stacks_T := Programs.Stacks (Program);
      -- All the stacks in the program.
      -- We will try to bound the local stack-height for those stacks
      -- that do not yet have bounds on the local stack height.
      -- We will try to bound the final stack-height for those stacks
      -- that do not yet have a known final stack-height.

      Final_Known : array (Stacks'Range) of Boolean;
      -- Whether the final stack-height is (was) already known
      -- for a given stack.

      Stack_Height : Storage.Cell_T;
      -- The stack height cell for a stack.

      Earlier_Limit : Programs.Execution.Stack_Limit_T;
      -- The limit on local stack-height, as known before this.

      Earlier_Bounded : array (Stacks'Range) of Boolean;
      -- Whether a stack has bounded Earlier_Limit and known
      -- final stack height.

      Num_Limits : Natural := 0;
      -- The actual number of limits that we compute for
      -- stack-height cells at the Stack_Steps.

      Limit_Defined : array (Stack_Steps'Range, Stacks'Range) of Boolean;
      -- Whether we have computed limits at a given Stack_Step
      -- for the height cell of a given Stack.

      Limits :
         array (Stack_Steps'Range, Stacks'Range)
         of Programs.Execution.Stack_Limit_T;
      -- The stack-height limits for each Stack_Step, here computed
      -- and applying to the stack height after the step is executed.
      -- Limits(step,stack) is valid only if Limit_Defined(step,stack)
      -- is true and Earlier_Bounded(stack) is false.

      Step : Flow.Step_T;
      -- One of the Stack_Steps.

      Final_Step : Boolean;
      -- Whether a step is a final (return) step.

      Defines_Height : Boolean;
      -- Whether a step defines the stack height of a given stack.

      Limit : Programs.Execution.Stack_Limit_T;
      -- The maximum of the Limits computed here for one stack.

      Final : Programs.Execution.Final_Stack_Height_T;
      -- The union of the Limits computed here for the final height
      -- of one stack.


   begin  -- Bound_Local_Stack_Height

      -- Perhaps we can avoid doing work:

      if Stacks'Length = 0 then
         -- There are no stacks in this program.

         Output.Error (Text => "This program has no stacks.");

         return;

      end if;

      for T in Stacks'Range loop

         Earlier_Limit := Programs.Execution.Stack_Height (
            Stack  => Stacks(T),
            Within => Exec_Bounds);

         Earlier_Bounded(T) := Programs.Execution.Bounded (Earlier_Limit);

         Final_Known(T) := Programs.Execution.Final_Stack_Height_Known (
            Stack  => Stacks(T),
            Within => Exec_Bounds);

         if Earlier_Bounded(T) and Final_Known(T) then
            -- The local and final stack-height is already fully bounded.

            Output.Note (
                 "Stack height for "
               & Programs.Name (Stacks(T))
               & " bounded without arithmetic analysis.");

         else
            -- Find Limits on local stack height for all feasible
            -- Stack_Steps that modify this Stack_Height or are
            -- final (return) steps:

            Stack_Height := Programs.Height (Stacks(T));

            for S in Stack_Steps'Range loop

               Step := Stack_Steps(S);

               -- Is this step a final step, or a height-defining step?

               Final_Step := Flow.Computation.Is_Final (
                  Step  => Step,
                  Under => Model.all);

               Defines_Height := Flow.Computation.Is_Defined (
                  Cell  => Stack_Height,
                  By    => Step,
                  Under => Model.all);

               -- Are we interested in this stack at this step?

               Limit_Defined(S,T) :=
                  (Final_Step and not Final_Known(T))
                  or
                  (Defines_Height and not Earlier_Bounded(T));

               if Limit_Defined(S,T) then
                  -- A feasible step that changes the height of a
                  -- stack without earlier stack-height bounds, or
                  -- returns the final height of a stack without
                  -- earlier knowledge of the final height.

                  Bound_Step (
                     Step  => Step,
                     Pool  => Into_Stack_Steps(S),
                     Stack => Stacks(T),
                     Cell  => Stack_Height,
                     Limit => Limits(S,T));

                  Num_Limits := Num_Limits + 1;

               -- else
               --    Limits(S,T) is not defined.

               end if;

             end loop;  -- over Stack_Steps.

         end if;

      end loop;  -- over Stack_Number_T.

      if Num_Limits = 0 and Stack_Steps'Length > 0 then

         Output.Fault (
            Location => "Bounds.Bound_Local_Stack_Height",
            Text =>
                 "No stack-height changes, but still "
               & Output.Image (Natural'(Stack_Steps'Length))
               & " unbounded stack-manipulating steps.");

      end if;

      -- Find the maximum value of the Limits on local stack height
      -- at all Stack_Steps that are still feasible, for all stacks
      -- that were not bounded earlier:

      for T in Stacks'Range loop

         if not Earlier_Bounded(T) then

            Limit := Programs.Execution.To_Stack_Limit (
               Storage.Bounds.Singleton (
                  Programs.Initial_Stack_Height (
                     Stack    => Stacks(T),
                     Entering => Subprogram)));
            -- Start from the initial stack height, because it would not
            -- be included as the result of a stack-changing step.

            for S in Stack_Steps'Range loop

               Step := Stack_Steps(S);

               if  Limit_Defined(S,T)
               and Flow.Computation.Is_Feasible (Step, Model.all)
               then

                  Limit := Programs.Execution.Max (Limit, Limits(S,T));

               end if;

            end loop;

            Programs.Execution.Bound_Stack_Height (
               Stack  => Stacks(T),
               To     => Limit,
               Within => Exec_Bounds);

         end if;

      end loop;

      -- Find the Limits on the final stack height at all final
      -- Stack_Steps that are still feasible, for all stacks where
      -- the final stack height was unknown:

      for T in Stacks'Range loop

         if not Final_Known(T) then

            Final := Programs.Execution.To_Stack_Limit (
               Storage.Bounds.Void_Interval);
            -- Start from a void interval, to which we then add
            -- (unite) the limits from all return steps.

            for S in Stack_Steps'Range loop

               Step := Stack_Steps(S);

               if       Flow.Computation.Is_Feasible (Step, Model.all)
               and then Flow.Computation.Is_Final    (Step, Model.all)
               then
                  -- A final return step.

                  if Limit_Defined(S,T) then

                     Final := Programs.Execution.Max (Final, Limits(S,T));

                  else

                     Output.Fault (
                        Location => "Bounds.Stacking.Bound_Local_Stack_Height",
                        Text     =>
                             "Limits undefined in step"
                           & Flow.Step_Index_T'Image (Flow.Index (Step))
                           & Output.Field_Separator
                           & Programs.Stack_Name (T, Program));

                  end if;

               end if;

            end loop;

            if Programs.Execution.Singular (Final) then

               Programs.Execution.Bound_Final_Stack_Height (
                  Stack  => Stacks(T),
                  To     => Final,
                  Within => Exec_Bounds);

            end if;

         end if;

      end loop;

   end Bound_Local_Stack_Height;


   procedure Bound_Take_Off_Height (
      Calls       : in Programs.Call_List_T;
      Into_Calls  : in Calculator.Pool_List_T;
      Exec_Bounds : in Programs.Execution.Bounds_Ref)
   is
      use type Storage.Cell_T;

      Model : constant Flow.Computation.Model_Handle_T :=
         Programs.Execution.Computation (Exec_Bounds);
      -- A handle on the computation model, for using it and updating it.

      Stacks : constant Programs.Stacks_T :=
         Programs.Stacks (Flow.Computation.Program (Model.all));
      -- All the stacks in the program.


      procedure Bound_Call (
         Call  : in Programs.Call_T;
         Pool  : in Calculator.Pool_T;
         Stack : in Programs.Stack_T)
      --
      -- Bounds the take-off height of a given Stack, at one Call, with a
      -- given data-Pool into the Calll. If the Call is found to be infeasible,
      -- the call-step is so marked in the Model and the Model is pruned.
      --
      is

         Call_Mark : Output.Nest_Mark_T;
         -- Marks the locus of the Call.

         Step : constant Flow.Step_T := Programs.Step (Call);
         -- The call-step.

         Step_Index : constant Flow.Step_Index_T := Flow.Index (Step);
         -- The index of the call-step, for display only.

         Limit : Programs.Execution.Stack_Limit_T;
         -- The limit here computed for the take-off height of the Call.

      begin

         Call_Mark := Output.Nest (Programs.Locus (Call));

         Calculator.Comment (
            Text =>
                 "Local stack height for "
               & Programs.Name (Stack)
               & " at call step"
               & Flow.Step_Index_T'Image (Step_Index),
            Calc => Calculator.Owner_Of (Pool));

         Limit := Programs.Execution.To_Stack_Limit (
            Calculator.Bounds_From_Pool (
               Pool => Pool,
               Cell => Programs.Height (Stack)));

         Opt.Ignore_Huge_Bounds (Limit);

         Programs.Execution.Bound_Take_Off_Height (
            Stack  => Stack,
            Before => Call,
            To     => Limit,
            Within => Exec_Bounds);

         Output.Unnest (Call_Mark);

      exception

      when Calculator.Null_Set_Error =>
         -- Cannot reach this Call in this Model.

         if Flow.Pruning.Opt.Warn_Unreachable then

            Output.Warning (
               Locus => Programs.Locus (Call),
               Text  =>
                    "Call is infeasible (for take-off height in "
                  & Programs.Name (Stack)
                  & ").");

         end if;

         Programs.Execution.Mark_Infeasible (
            Step   => Step,
            Within => Exec_Bounds);

         Output.Unnest (Call_Mark);

      when others =>
          -- Clean up the output locus and punt:

         Output.Unnest (Call_Mark);

         raise;

      end Bound_Call;


   begin  -- Bound_Take_Off_Height

      if Stacks'Length = 0 then
         -- There are no stacks in this program.

         Output.Error (Text => "This program has no stacks.");

         return;

      end if;

      -- Find the "take-off" stack height for each stack at each
      -- feasible call step:

      for T in Stacks'Range loop

         for C in Calls'Range loop

            if Flow.Computation.Is_Feasible (Calls(C), Model.all) then

               Bound_Call (
                  Call  => Calls(C),
                  Pool  => Into_Calls(C),
                  Stack => Stacks(T));

            end if;

         end loop;

      end loop;

   end Bound_Take_Off_Height;


   procedure Compute_Usage (
      Stack  : in Programs.Stack_T;
      Within : in Programs.Execution.Bounds_Ref)
   --
   -- Performs Compute_Stack_Usage (Within) for a given Stack.
   --
   is
      use Programs;
      use Programs.Execution;

      Local_Max : constant Storage.Bounds.Limit_T :=
         Stack_Height (Stack, Within).Max;
      -- The upper bound on the local stack height in the subprogram.

      Calls : constant Call_Bounds_List_T := Call_Bounds (Within);
      -- Bounds on the calls within this subprogram, as far as
      -- known in the given bounds and including only calls that are
      -- feasible under the computation model.

      Call_Mark : Output.Nest_Mark_T;
      -- Marks the locus of the current call in the subprogram
      -- at which the stack height & usage are investigated.

      Take_Off : Stack_Limit_T;
      -- The take-off stack-height for a call.

      Callee_Usage : Stack_Usage_T;
      -- Upper limit for stack usage in a callee of this subprogram
      -- as extracted from the call bounds in Within.

      Max_Call_Usage : Stack_Usage_T := (
         State  => Computed,
         Height => 0,
         Call   => No_Call);
      --
      -- Maximum stack usage at a call, including local take-off
      -- height and callee usage. Initial value represents perfect
      -- knowledge before we have looked at any calls (neutral
      -- value for the Max operation).

      Local_Usage : Stack_Usage_T;
      -- The Local_Max as a stack usage, if bounded.

   begin

      if not Flow.Computation.Is_Feasible (Computation (Within).all) then
         -- The whole computation is infeasible.

         Bound_Stack_Usage (
            Stack  => Stack,
            To     => (
               State  => Infeasible,
               Height => 0,
               Call   => No_Call),
            Within => Within);

      else
         -- There is some feasible execution path.

         -- Get the total stack usage at each call step, adding the
         -- local height at the call to the stack usage of the callee:

         for C in Calls'Range loop

            Call_Mark := Output.Nest (Locus (Calls(C).Call));

            -- Get the take-off height:

            Take_Off := Take_Off_Height (
               Stack  => Stack,
               Before => Calls(C).Call,
               Within => Within);

            if (not Bounded (Take_Off))
            and Programs.Execution.Opt.Warn_Unbounded_Call
            then
               -- Local stack height at call is not bounded.

               Output.Warning (
                    "Take-off stack-height not bounded"
                  & Output.Field_Separator
                  & Name (Stack));

            end if;

            -- Get the callee usage:

            Callee_Usage := Stack_Usage (
               Stack  => Stack,
               Within => Calls(C).Bounds);

            if (not Bounded (Callee_Usage))
            and Programs.Execution.Opt.Warn_Unbounded_Call
            then

               Output.Warning (
                    "Callee stack-usage not bounded"
                  & Output.Field_Separator
                  & Name (Stack));

            end if;

            -- Cumulate the maximum usage at calls:

            Max_Call_Usage := Max (
               Max_Call_Usage,
               Total_Usage (
                  Call     => Calls(C).Call,
                  Take_Off => Take_Off,
                  Callee   => Callee_Usage));

            Output.Unnest (Call_Mark);

         end loop;

         -- Translate Local_Max to a stack-usage:

         if Storage.Bounds.Known (Local_Max) then
            -- The local stack height has an upper bound.

            Local_Usage := (
               State  => Computed,
               Height => Storage.Bounds.Value (Local_Max),
               Call   => No_Call);

         elsif Number_Of_Input_Cells (Within) > 0 then
            -- If the local stack height has no upper bound, nor
            -- does the stack usage, but we hope that more
            -- context can bound it.

            Local_Usage := (
               State  => Depends,
               Height => 0,
               Call   => No_Call);

         else
            -- Local stack height and stack usage not bounded, and
            -- there are no input cells, so no help from context.

            if Programs.Execution.Opt.Warn_Unbounded_Call then

               Output.Warning (
                    "No inputs for context-dependent stack bounds"
                  & Output.Field_Separator
                  & Name (Stack));

            end if;

            Local_Usage := (
               State  => Vague,
               Height => 0,
               Call   => No_Call);

         end if;

         -- The total stack usage bounds is the maximum of the
         -- local height (associated with no call) and the maximum
         -- callee usage:

         Bound_Stack_Usage (
            Stack  => Stack,
            To     => Max (Local_Usage, Max_Call_Usage),
            Within => Within);

      end if;

   end Compute_Usage;


   procedure Compute_Stack_Usage (Within : in Programs.Execution.Bounds_Ref)
   is
      use Programs.Execution;

      Stacks : constant Programs.Stacks_T :=
         Programs.Stacks (Program (Within));
      -- All the stacks in the program.

   begin

      for S in Stacks'Range loop

         case Space_State (Stacks(S), Within) is

         when Undefined
            | Vague
            | Depends =>

            Compute_Usage (
               Stack  => Stacks(S),
               Within => Within);

         when Computed
            | Asserted
            | Infeasible
            | Unbounded =>
            -- Nothing to do.

            null;

         end case;

      end loop;

   end Compute_Stack_Usage;


end Bounds.Stacking;
