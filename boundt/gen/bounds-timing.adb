-- Bounds.Timing (body)
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
-- $Revision: 1.6 $
-- $Date: 2015/10/24 20:05:46 $
--
-- $Log: bounds-timing.adb,v $
-- Revision 1.6  2015/10/24 20:05:46  niklas
-- Moved to free licence.
--
-- Revision 1.5  2008-07-23 09:07:15  niklas
-- BT-CH-0139: Fix recursion in Programs.Execution.Paths.
--
-- Revision 1.4  2008/07/15 06:38:39  niklas
-- BT-CH-0136: "No feasible execution path" demoted to Warning.
--
-- Revision 1.3  2008/02/27 14:58:48  niklas
-- BT-CH-0116: Call-specific time and stack assertions.
--
-- Revision 1.2  2008/01/31 21:57:44  niklas
-- BT-CH-0108: Fixes to BT-CH-0098.
--
-- Revision 1.1  2007/12/17 13:54:34  niklas
-- BT-CH-0098: Assertions on stack usage and final stack height, etc.
--


with Arithmetic.Opt;
with Flow.Computation;
with Loops;
with Output;
with Processor;
with Programs.Execution.Opt;


package body Bounds.Timing is


   procedure Apply_Assertions (
      Asserts     : in Assertions.Assertion_Set_T;
      Exec_Bounds : in Programs.Execution.Bounds_Ref)
   is
      use type Processor.Time_T;

      Time : constant Assertions.Time_Bound_T :=
         Assertions.Subprogram_Time (
            Subprogram => Programs.Execution.Subprogram (Exec_Bounds),
            Asserts    => Asserts);
      -- The execution time possibly asserted for the subprogram.

   begin

      if Time.Max < Processor.Time_T'Last then
         -- An upper time bound is asserted.

         Programs.Execution.Bound_Time (
            Time   => Time.Max,
            State  => Programs.Execution.Asserted,
            Within => Exec_Bounds);

      end if;

   end Apply_Assertions;


   procedure Define_Time_State (Within : in Programs.Execution.Bounds_Ref)
   --
   -- Implements Evaluate_Time_State for bounds in which the time-state
   -- is initially Undefined.
   --
   is
      use type Arithmetic.Opt.Choice_T;
      use Programs;
      use Programs.Execution;

      Arith_Choice : constant Arithmetic.Opt.Choice_T :=
         Programs.Arithmetic_Analysis (Subprogram (Within));
      -- Whether "arithmetic analysis" is allowed or denied.

      Num_Inputs : constant Natural := Number_Of_Input_Cells (Within);
      -- The number of input cells on which these execution bounds
      -- may depend, if not already fully bounded.

      Loose_Loops : constant Loops.Loop_List_T :=
         Unbounded_Loops (Within => Within, Eternal => True);
      -- Feasible but still unbounded loops, including eternal loops
      -- that are not bounded by loop-repeat assertions.

      Calls : constant Call_Bounds_List_T := Call_Bounds (Within);
      -- Feasible calls and their current, possibly context-dependent bounds.

      State : Time_State_T := Computable;
      -- The final time-state. Optimistically initialized.

      Call_State : Time_State_T;
      -- The time-state of one of the Calls.

   begin

      -- Check the properties of this subprogram itself:

      if not Flow.Computation.Is_Feasible (Computation (Within).all) then
         -- There is no feasible execution path in the subprogram.
         -- The Loose_Loops and Calls are necessarily null.

         Output.Warning ("No feasible execution path.");

         State := Infeasible;

      elsif Enough_For_Time (Within) then
         -- We are assured that enough execution-count assertions exist
         -- to bound the execution paths for a Computable time-bound,
         -- even if there are unbounded loops and even if the flow-graph
         -- is irreducible.

         if Loose_Loops'Length > 0 then

            Output.Warning (
                 "Ignoring"
               & Natural'Image (Loose_Loops'Length)
               & " unbounded loop(s) because ""enough"" is asserted.");

         elsif not Reducible (Subprogram (Within)) then

            Output.Warning (
               "Ignoring irreducibility because ""enough"" is asserted.");

         end if;

      elsif Loose_Loops'Length > 0 then
         -- Some loops are still not bounded.
         -- Perhaps we need more parameter context, or context-specific
         -- assertions on relevant locally used variables.

         -- The following logic will need TBA extensions if:
         -- > loops can be bounded by other than arithmetic analysis, or
         -- > context-specific assertions are allowed.

         if  Num_Inputs = 0
         and Opt.Warn_Unbounded_Call
         then

            Output.Warning ("No inputs for context-dependent time bounds.");

         end if;

         if  Arith_Choice /= Arithmetic.Opt.Disabled
         and Num_Inputs > 0
         then
            -- Arithmetic analysis is allowed, and the bounds seem to
            -- depend on some inputs, so we can make use of context.

            State := Depends;

         else
            -- The execution bounds are either without input cells, which
            -- means that the present analysis is as good as we can make
            -- it, or arithmetic analysis is denied, which means that we
            -- cannot make any use of context even if there are input cells.

            State := Vague;

         end if;

      elsif not Reducible (Subprogram (Within)) then

         Output.Error ("Irreducible flow-graph.");

         State := Vague;

      end if;

      -- Check the time-state of the calls:

      for C in Calls'Range loop

         Call_State := Time_State (Calls(C).Bounds);

         case Call_State is

         when Undefined =>
            -- Whoops, how did this happen?

            Output.Fault (
               Location => "Bounds.Timing.Define_Time_State",
               Locus    => Locus (Calls(C).Call, Within),
               Text     =>
                    "Callee Time_State is "
                  & Time_State_T'Image (Call_State));

         when Vague =>

            State := Time_State_T'Min (State, Call_State);

         when Depends =>

            -- The following logic will need TBA extensions if:
            -- > caller input context can be transmitted to callees
            --   by other means than arithmetic analysis.

            if Arith_Choice /= Arithmetic.Opt.Disabled then
               -- Arithmetic analysis of the caller is allowed, which
               -- means that caller-context can be transmitted to
               -- callees that depend on inputs.

               State := Time_State_T'Min (State, Call_State);

            else
               -- Arithmetic analysis of the caller is denied, so
               -- analysis of the caller cannot provide context for
               -- bounding the calls.

               State := Time_State_T'Min (State, Vague);

            end if;

         when Computable
            | Computed
            | Asserted =>
            -- Good.

            null;

         when Infeasible
            | Unbounded  =>
            -- The constraints on the callee make its execution
            -- infeasible or unbounded. We mark the caller as Vague,
            -- not infeasible or unbounded, to help the display of
            -- "unbounded" parts.

            State := Time_State_T'Min (State, Vague);

         when Failed =>
            -- This obviously prevents us from computing a time-bound
            -- on this caller subprogram. But the failure was already
            -- reported, and another error will be reported when we
            -- try to compute the time-bounde for this caller. So we
            -- can ignore it here.

            null;

         end case;

      end loop;

      Output.Note (
           "Time_State for bounds #"
         & Bounds_Index_T'Image (Index (Within))
         & " defined as "
         & Time_State_T'Image (State));

      Set_Time_State (
         To     => State,
         Within => Within);

   end Define_Time_State;


   procedure Evaluate_Time_State (Within : in Programs.Execution.Bounds_Ref)
   is
      use Programs;
      use Programs.Execution;
   begin

      case Time_State (Within) is

      when Undefined =>
         -- The subprogram is not a stub and does not have an asserted
         -- execution time (in this context) and does not inherit a
         -- computed execution time (in this context).

         Define_Time_State (Within);

      when Vague =>
         -- The subprogram is a stub but does not have an asserted
         -- execution time (in this context).
         -- Nothing we can do.

         null;

      when Asserted =>
         -- Nothing to do.

         null;

      when others =>
         -- The execution bounds were Adopted_To some _Context and
         -- inherited the Time_State of the original bounds for a
         -- shallower context.

         Output.Note (
              "Adopted Time_State left as "
            & Time_State_T'Image (Time_State (Within)));

      end case;

   end Evaluate_Time_State;


end Bounds.Timing;
