-- Bounds.Calling (body)
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
-- $Revision: 1.18 $
-- $Date: 2015/10/24 20:05:46 $
--
-- $Log: bounds-calling.adb,v $
-- Revision 1.18  2015/10/24 20:05:46  niklas
-- Moved to free licence.
--
-- Revision 1.17  2009-10-07 19:26:09  niklas
-- BT-CH-0183: Cell-sets are a tagged-type class.
--
-- Revision 1.16  2009-01-18 08:10:33  niklas
-- Removed unused context clauses and locals.
--
-- Revision 1.15  2008/11/09 21:43:03  niklas
-- BT-CH-0158: Output.Image (Time_T) replaces Programs.Execution.Image.
--
-- Revision 1.14  2008/09/24 08:38:51  niklas
-- BT-CH-0146: Assertions on "loop starts <bound> times".
-- BT-CH-0146: Loop-repeat assertions set both lower and upper bound.
-- BT-CH-0146: Report locations of contradictory "count" assertions.
-- BT-CH-0146: Contradictory "count" assertions imply infeasibility.
--
-- Revision 1.13  2008/09/20 12:41:50  niklas
-- BT-CH-0145: No error re too few assertion matches if graph is growing.
--
-- Revision 1.12  2008/02/27 14:58:47  niklas
-- BT-CH-0116: Call-specific time and stack assertions.
--
-- Revision 1.11  2007/12/17 13:54:34  niklas
-- BT-CH-0098: Assertions on stack usage and final stack height, etc.
--
-- Revision 1.10  2007/01/13 13:51:01  niklas
-- BT-CH-0041.
--
-- Revision 1.9  2006/11/26 22:07:24  niklas
-- BT-CH-0039.
--
-- Revision 1.8  2006/05/28 07:10:58  niklas
-- Extended the trace output from Bound_Protocol.
--
-- Revision 1.7  2006/05/27 21:46:38  niklas
-- Updated for BT-CH-0020.
--
-- Revision 1.6  2005/10/20 11:28:27  niklas
-- BT-CH-0015.
--
-- Revision 1.5  2005/07/01 11:02:25  niklas
-- Extended Bound_Protocol to note when the exception Flow.False_Path
-- occurs and to propagate it to the caller. Only other exceptions
-- give rise to a Fault message.
-- Changed Parameter_Bounds to propagate Flow.False_Path instead of
-- Calculator.Empty_Flux, and changed Bound_Call accordingly to catch
-- this exception as a sign that the call is unreachable.
--
-- Revision 1.4  2005/06/29 12:56:01  niklas
-- Made the warning re "infeasible" call conditional on the
-- option "-warn reach" and changed it to "unreachable".
--
-- Revision 1.3  2005/06/28 08:34:49  niklas
-- Added output for Bounds.Opt.Trace_Context.
--
-- Revision 1.2  2005/02/23 09:05:13  niklas
-- BT-CH-0005.
--
-- Revision 1.1  2005/02/16 21:11:38  niklas
-- BT-CH-0002.
--


with Bounds.Opt;
with Calling;
with Calling.Opt;
with Flow.Calls;
with Flow.Execution;
with Flow.Pruning.Opt;
with Output;
with Processor;


package body Bounds.Calling is


   use type Programs.Call_Path_T;


   procedure Bound_Protocol (
      Call    : in     Programs.Call_T;
      Data    : in     Storage.Bounds.Bounds_T'Class;
      Model   : in out Flow.Computation.Model_Ref;
      Bounded :    out Boolean)
   is
      use type Standard.Calling.Protocol_Ref;

      Call_Mark : Output.Nest_Mark_T;
      -- For the output locus of the Call.

      Protocol : constant Standard.Calling.Protocol_Ref :=
         Flow.Computation.Calling_Protocol (Call, Model);
      -- The old calling protocol for Call.

      Opt_Trace : constant Boolean := Standard.Calling.Opt.Trace_Protocols;
      -- Whether the bounding should be traced.

      New_Protocol : Standard.Calling.Protocol_Ref;
      -- The bounded new protocol, or null if not better than the old.

   begin

      Call_Mark := Output.Nest (Programs.Locus (Call));

      if Standard.Calling.Static (Protocol.all) then
         -- The protocol is static so we need do nothing.

         Bounded := False;

         if Standard.Calling.Opt.Trace_Protocols then

            Output.Trace (
                 "Protocol is static"
               & Output.Field_Separator
               & Standard.Calling.Image (Protocol.all));

         end if;

      else
         -- The protocol is dynamic and needs bounding.

         if Opt_Trace then

            Output.Trace (
                 "Applying "
               & Storage.Bounds.Image (Data)
               & " upon "
               & Standard.Calling.Image (Protocol.all));

         end if;

         Standard.Calling.Apply (
            Bounds => Data,
            Upon   => Protocol.all,
            Giving => New_Protocol);

         Bounded := New_Protocol /= null;

         if Bounded then
            -- The protocol was constrained, and so the effect of
            -- the Call may need update. The set of input cells and
            -- the basis cells for the protocol may also have changed.

            if Opt_Trace then

               Output.Trace (
                    "Bounded calling protocol"
                  & Output.Field_Separator
                  & Standard.Calling.Image (New_Protocol.all));

            end if;

            Flow.Computation.Set_Calling_Protocol (
               Call  => Call,
               To    => New_Protocol,
               Under => Model);

         elsif Opt_Trace then

            Output.Trace ("Protocol was not bounded.");

         end if;

      end if;

      Output.Unnest (Call_Mark);

   exception

   when Flow.False_Path =>
      -- The Call is probably infeasible.

      Output.Note ("Bounding protocol raised False_Path.");

      Output.Unnest (Call_Mark);

      raise;

   when others =>

      Output.Fault (
         Location => "Bounds.Calling.Bound_Protocol",
         Text     => "Exception propagated.");

      Output.Unnest (Call_Mark);

      raise;

   end Bound_Protocol;


   procedure Apply_Final_Stack_Height_Assertions (
      Call          : in Programs.Call_T;
      Assert_Map    : in Assertions.Assertion_Map_T;
      Callee_Bounds : in Programs.Execution.Bounds_Ref)
   --
   -- Applies any assertions on final stack-height on this Call,
   -- within the Assert_Map, to the Callee_Bounds.
   --
   is
      use Programs.Execution;

      Program : constant Programs.Program_T := Programs.Program (Call);
      -- The program under analysis.

      Final : constant Assertions.Stack_Bounds_T :=
         Assertions.Call_Final_Stack_Height (Call, Assert_Map);
      -- Assertions on the final stack-height for this call.

      Stack : Programs.Stack_T;
      -- One of the stacks in the Program.

      Limit : Programs.Execution.Stack_Limit_T;
      -- The Final bound for the Stack, as a stack-limit.

   begin

      for F in Final'Range loop

         case Programs.Stack_Kind (F, Program) is

         when Programs.Stable =>
            -- Ignore any assertions. The final height is zero.

            null;

         when Programs.Unstable =>
            -- Check for assertion:

            if Storage.Bounds.Known (Final(F)) then
               -- An upper and/or lower bound is asserted.

               Limit := To_Stack_Limit (Final(F));

               Stack := Programs.Stack_By (F, Program);

               Output.Note (
                    "Asserted final stack height for call"
                  & Output.Field_Separator
                  & Programs.Name (Stack)
                  & Output.Field_Separator
                  & Programs.Execution.Image (Limit));

               Bound_Final_Stack_Height (
                  Stack  => Stack,
                  To     => Limit,
                  Within => Callee_Bounds);

            end if;

         end case;

      end loop;

   end Apply_Final_Stack_Height_Assertions;


   procedure Apply_Stack_Usage_Assertions (
      Call          : in Programs.Call_T;
      Assert_Map    : in Assertions.Assertion_Map_T;
      Callee_Bounds : in Programs.Execution.Bounds_Ref)
   --
   -- Applies any assertions on stack usage on this Call, within
   -- the Assert_Map, to the Callee_Bounds, assuming that we are
   -- analysing for space bounds.
   --
   is
      use Programs.Execution;

      Program : constant Programs.Program_T := Programs.Program (Call);
      -- The program under analysis.

      Usage : constant Assertions.Stack_Bounds_T :=
         Assertions.Call_Stack_Usage (Call, Assert_Map);
      -- Assertions on the total stack usage of the Call.

      Stack : Programs.Stack_T;
      -- One of the stacks in the Program.

      Bound : Storage.Bounds.Interval_T;
      -- The asserted bounds on the Stack.

   begin

      for U in Usage'Range loop

         if Storage.Bounds.Known (Usage(U).Max) then
            -- An upper bound is asserted on stack usage.

            Bound := Usage(U);

            Stack := Programs.Stack_By (U, Program);

            Output.Note (
                 "Asserted stack usage for call"
               & Output.Field_Separator
               & Programs.Name (Stack)
               & Output.Field_Separator
               & Storage.Bounds.Image (Bound));

            Bound_Stack_Usage (
               Stack  => Stack,
               To     => (
                  State  => Asserted,
                  Height => Storage.Bounds.Value (Bound.Max),
                  Call   => Programs.No_Call),
               Within => Callee_Bounds);

         end if;

      end loop;

   end Apply_Stack_Usage_Assertions;


   procedure Bound_Asserted_Calls (
      Calls         : in Programs.Call_List_T;
      Assert_Map    : in Assertions.Assertion_Map_T;
      Caller_Bounds : in Programs.Execution.Bounds_Ref;
      Bounds_Set    : in Programs.Execution.Bounds_Set_T)
   is
      use type Processor.Time_T;

      Model : constant Flow.Computation.Model_Handle_T :=
         Programs.Execution.Computation (Caller_Bounds);
      -- The computation model of the caller. We may mark some
      -- call-steps as infeasible and then prune the model.


      procedure Bound_Call_Time_Space (Call : in Programs.Call_T)
      --
      -- Applies time and space assertions to the given Call.
      --
      is

         Time_Bound : constant Assertions.Time_Bound_T :=
            Assertions.Call_Time (
               Call    => Call,
               Asserts => Assert_Map);
         -- The time bounds asserted for this Call, if any.

         Time_Asserted : constant Boolean :=
            Programs.Execution.For_Time (Caller_Bounds)
            and then
            Time_Bound.Max < Processor.Time_T'Last;
         -- Whether an upper time bound is asserted for this Call.

         Stack_Final_Asserted : constant Boolean :=
            Assertions.Call_Final_Stack_Height_Asserted (
               Call    => Call,
               Asserts => Assert_Map);
         -- Whether there are some effective assertions on the
         -- final stack height for this Call.

         Stack_Usage_Asserted : constant Boolean :=
            Programs.Execution.For_Space (Caller_Bounds)
            and then
            Assertions.Call_Stack_Usage_Asserted (
               Call    => Call,
               Asserts => Assert_Map);
         -- Whether there are some effective assertions on the
         -- stack usage for this Call, assuming that stack usage
         -- analysis is requested.

         Callee_Bounds : Programs.Execution.Bounds_Ref;
         -- Execution bounds for the callee.

         Flow_Frozen : Boolean;
         -- Whether the Callee_Bounds, when fully bounded by
         -- assertions (and previous analysis), have only static
         -- flow of control.

      begin

         if Time_Asserted
         or Stack_Final_Asserted
         or Stack_Usage_Asserted
         then
            -- Time or space or both are asserted for this call.

            -- Ensure that the bounds on this Call are specific
            -- to this context:

            Programs.Execution.Adopt_To_Context (
               Call   => Call,
               Caller => Caller_Bounds,
               Within => Bounds_Set,
               Giving => Callee_Bounds);

            if Time_Asserted then

               Output.Note (
                    "Asserted execution time for call"
                  & Output.Field_Separator
                  & Output.Image (Time_Bound.Max));

               Programs.Execution.Bound_Time (
                  Time   => Time_Bound.Max,
                  State  => Programs.Execution.Asserted,
                  Within => Callee_Bounds);

            end if;

            if Stack_Final_Asserted then

               Apply_Final_Stack_Height_Assertions (
                  Call          => Call,
                  Assert_Map    => Assert_Map,
                  Callee_Bounds => Callee_Bounds);

            end if;

            if Stack_Usage_Asserted then

               Apply_Stack_Usage_Assertions (
                  Call          => Call,
                  Assert_Map    => Assert_Map,
                  Callee_Bounds => Callee_Bounds);

            end if;

            if Fully_Bounded (Callee_Bounds) then
               -- These bounds are done and can be finished.

               Finish_Bounds (
                  Exec_Bounds => Callee_Bounds,
                  Assert_Map  =>
                     Programs.Execution.Assertion_Map (Callee_Bounds),
                  Bounds_Set  => Bounds_Set,
                  Flow_Frozen => Flow_Frozen);

               if not Flow_Frozen then

                  Output.Fault (
                     Location => "Bounds.Calling.Bound_Asserted_Calls",
                     Text     => "Callee has unbounded dynamic flow.");

               end if;

            end if;

         end if;

      end Bound_Call_Time_Space;


      procedure Bound_Call_Count (Call : in Programs.Call_T)
      --
      -- Applies execution-count assertions to the given Call.
      -- This has no effect on the execution-bounds for the callee
      -- at the Call, only on the Caller_Bounds.
      --
      is

         Exec_Bound : constant Flow.Execution.Bound_T :=
            Assertions.Call_Count (
               Call    => Call,
               Asserts => Assert_Map);
         -- The execution-count bound asserted for this Call, if any.

      begin

         -- Possible assertion on execution count:

         if Flow.Execution.Bounded (Exec_Bound) then

            Programs.Execution.Bound_Call_Count (
               Call   => Call,
               Count  => Exec_Bound,
               Within => Caller_Bounds);

            -- Perhaps the call even becomes infeasible?

            if Exec_Bound.Max = 0 then
               -- A call that can only be executed zero times is infeasible.

               Programs.Execution.Mark_Infeasible (
                  Step   => Programs.Step (Call),
                  Within => Caller_Bounds);

            end if;

         end if;

      end Bound_Call_Count;


      procedure Bound_Call (Call : in Programs.Call_T)
      --
      -- Apply all assertions for this Call.
      --
      is

         Call_Mark : Output.Nest_Mark_T;
         -- The locus of this Call within the Caller_Bounds.

      begin

         Call_Mark := Output.Nest (Programs.Execution.Locus (
            Call   => Call,
            Within => Caller_Bounds));

         -- Possible assertions on time or space:

         Bound_Call_Time_Space (Call);

         -- Possible assertions on execution count:

         Bound_Call_Count (Call);

         -- Possible assertions on call-invariant cells:

         Flow.Calls.Spare_Call_Invariants (
            Call      => Call,
            Invariant => Assertions.Call_Invariants (Call, Assert_Map),
            Within    => Model.all);

         Output.Unnest (Call_Mark);

      end Bound_Call;


   begin  -- Bound_Asserted_Calls

      for C in Calls'Range loop

         Bound_Call (Calls(C));

      end loop;

   end Bound_Asserted_Calls;


   --
   --    Context-specific bounding of calls
   --


   function Input_Parameters (
      Call   : Programs.Call_T;
      Within : Programs.Execution.Bounds_Ref)
   return Flow.Calls.Parameter_Map_T
   --
   -- The input parameters for the Call, Within the caller's
   -- execution bounds, defined by the input cells for the
   -- execution bounds on the Call, mapped through the protocol
   -- for the Call.
   --
   is
   begin

      return Flow.Calls.Input_Parameters (
         Inputs   => Programs.Execution.Input_Cells (Call, Within),
         Protocol => Programs.Execution.Calling_Protocol (Call, Within).all);

   end Input_Parameters;


   procedure Bound_Call_With_Parameters (
      Call          : in Programs.Call_T;
      Caller_Bounds : in Programs.Execution.Bounds_Ref;
      Param_Bounds  : in Storage.Bounds.Cell_Interval_List_T;
      Inherit_Inv   : in Storage.Cell_Set_T;
      Asserts       : in Assertions.Assertion_Set_T;
      Assert_Map    : in Assertions.Assertion_Map_T;
      Bounds_Set    : in Programs.Execution.Bounds_Set_T)
   --
   -- Analyse the Call, in the context of the given Caller_Bounds,
   -- Param_Bounds, and Inherited Invariants, for time and/or space bounds.
   -- The assertion set, the assertion map for this Call, and the set of
   -- execution bounds are further parameters.
   --
   is
      use type Programs.Subprogram_T;

      Caller : constant Programs.Subprogram_T :=
         Programs.Execution.Subprogram (Caller_Bounds);
      -- The caller subprogram, according to Caller_Bounds.

      Call_Inv : constant Storage.Cell_Set_T :=
         Assertions.Call_Invariants (
            Call    => Call,
            Asserts => Assert_Map);
      -- Cells asserted as invariant for the call.

      Call_Bounds : Programs.Execution.Bounds_Ref;
      -- The execution bounds for the Call in the Caller_Bounds context.

      Flow_Frozen : Boolean;
      -- Whether the dynamic control flow in the callee is now frozen.
      -- It damn well should be true, since the analysis of the callee
      -- on the universal (call-independent) level should make it so.

   begin   -- Bound_Call_With_Parameters

      -- Check consistency:

      if Programs.Caller (Call) /= Caller then

         Output.Fault (
            Location => "Bounds.Calling.Bound_Call_With_Parameters",
            Text     =>
                 "Caller_Bounds for "
               & Programs.Name (Caller)
               & ", call "
               & Programs.Image (Call));

      end if;

      -- Optionally trace the context:

      if Opt.Trace_Context then

         Output.Trace (
              "Call values"
            & Output.Field_Separator
            & Storage.Bounds.Image (Param_Bounds));

         Output.Trace (
              "Call invariants"
            & Output.Field_Separator
            & Storage.Image (Call_Inv));

      end if;

      -- Call-specific analysis is done if some input parameter
      -- is bounded, or there are call-specific assertions:

      if Param_Bounds'Length > 0
      or not Storage.Is_Empty (Call_Inv)
      then

         Output.Note (Text => "Start bounding call.");

         -- Ensure that the bounds on this call are specific
         -- to this context:

         Programs.Execution.Adopt_To_Context (
            Call   => Call,
            Caller => Caller_Bounds,
            Within => Bounds_Set,
            Giving => Call_Bounds);

         Bound_Execution (
            Exec_Bounds => Call_Bounds,
            Params      => Param_Bounds,
            Inherit_Inv => Storage.Mixed.Union (Call_Inv, Inherit_Inv),
            Asserts     => Asserts,
            Bounds_Set  => Bounds_Set,
            Flow_Frozen => Flow_Frozen);

         if not Flow_Frozen then

            Output.Fault (
               Location => "Bounds.Calling.Bound_Call_With_Parameters",
               Text     => "Callee has unresolved dynamic flow");

         end if;

         -- The returned execution bounds may be incomplete
         -- in that some loops/calls are not bounded.
         -- This will lead to a new attempt at bounding the
         -- call, in a "deeper" context (one more call level
         -- up in the call-graph).
         -- Nevertheless, the bounds that resulted from this
         -- attempt (call-specific but incomplete bounds) were
         -- stored, for possible inspection by the user. The
         -- bounds were stored in Bound_Execution.

         if Bounds.Fully_Bounded (Call_Bounds) then

            Output.Note (Text => "Fully bounded");

         else

            Output.Note (Text => "Not yet fully bounded");

         end if;

      else
         -- Leave Call as "unbounded" (refer to the old bounds),
         -- This will lead to a new attempt at bounding.

         Output.Note (Text => "No new context");

      end if;

   end Bound_Call_With_Parameters;


   procedure Mark_Infeasible (
      Call   : in Programs.Call_T;
      Within : in Programs.Execution.Bounds_Ref)
   --
   -- Marks the Call as infeasible (unreachable) Within the given
   -- bounds on the execution of the caller.
   --
   is
   begin

      if Flow.Pruning.Opt.Warn_Unreachable then

         Output.Warning ("Unreachable call.");

      end if;

      Programs.Execution.Mark_Infeasible (
         Step   => Programs.Step (Call),
         Within => Within);

   end Mark_Infeasible;


   --
   --    Context-specific bounding of calls by Calculator pools
   --


   function Parameter_Bounds (
      Call   : Programs.Call_T;
      Params : Flow.Calls.Parameter_Map_T;
      Data   : Calculator.Pool_T)
   return Storage.Bounds.Cell_Interval_List_T
   --
   -- Bounds on Parameter cells (input cells) for a given Call.
   --
   -- Call
   --    The call for which input values will be bounded.
   -- Params
   --    The parameter mapping for the input parameters of the Call,
   --    pairing value-providing cells from the caller's frame with
   --    value-receiving cell's from the callee's frame.
   -- Data
   --    Bounds on the caller's cells at the call-step.
   --
   -- The cells in the result are in the callee frame.
   --
   -- If the Data flux turns out to be empty, this function propagates
   -- the exception Flow.False_Path.
   --
   is

      Param_Bounds : Storage.Bounds.Cell_Interval_List_T (1 .. Params'Length);
      Num_Bounds   : Natural := 0;
      --
      -- Real bounds on each input parameter from the call-context
      -- are collected in Param_Bounds(1 .. Num_Bounds).

      Caller_Cell : Storage.Cell_T;
      Callee_Cell : Storage.Cell_T;
      -- One mapped pair of caller/callee cells.

      Bound : Storage.Bounds.Interval_T;
      -- Bounds on a parameter (input) cell.

   begin

      -- Perhaps show the parameter associations:

      if Standard.Calling.Opt.Trace_Parameters then

         for F in Params'Range loop

            Output.Trace (Text =>
                 "Parameter association "
               & Storage.Name_Of (Params(F).Caller)
               & " -> "
               & Storage.Name_Of (Params(F).Callee));
         
         end loop;

      end if;

      -- Determine the bounds on actual parameter values,
      -- implied by the assertions and the caller's computation:

      for P in Params'Range loop

         Caller_Cell := Params(P).Caller;
         Callee_Cell := Params(P).Callee;

         Calculator.Comment (
            Text => "Parameter bounds for "
               & Storage.Name_Of (Caller_Cell)
               & " -> "
               & Storage.Name_Of (Callee_Cell),
            Calc => Calculator.Owner_Of (Data));

         Bound := Calculator.Bounds_From_Pool (
            Pool => Data,
            Cell => Caller_Cell);

         if Storage.Bounds.Known (Bound) then
            -- A real bound, collect it:

            Num_Bounds := Num_Bounds + 1;

            Param_Bounds(Num_Bounds) := (
               Cell     => Callee_Cell,
               Interval => Bound);

         end if;

         if Standard.Calling.Opt.Trace_Parameters then

            Output.Result (
               Key   => "Param_Bounds",
               Locus => Output.Current_Locus,
               Text  =>
                    Storage.Name_Of (Caller_Cell)
                  & Output.Field_Separator
                  & Storage.Bounds.Image (
                       Item => Bound,
                       Name => Storage.Name_Of (Callee_Cell)));
         end if;

      end loop;

      return Param_Bounds(1 .. Num_Bounds);

   exception

   when Calculator.Null_Set_Error =>
      -- The call is probably infeasible.

      Output.Note ("Parameter flux is empty.");

      raise Flow.False_Path;

   end Parameter_Bounds;


   procedure Bound_Call (
      Call          : in Programs.Call_T;
      Caller_Bounds : in Programs.Execution.Bounds_Ref;
      Data          : in Calculator.Pool_T;
      Inherit_Inv   : in Storage.Cell_Set_T;
      Asserts       : in Assertions.Assertion_Set_T;
      Assert_Map    : in Assertions.Assertion_Map_T;
      Bounds_Set    : in Programs.Execution.Bounds_Set_T)
   is
   begin

      Bound_Call_With_Parameters (
         Call          => Call,
         Caller_Bounds => Caller_Bounds,
         Param_Bounds  => Parameter_Bounds (
            Call   => Call,
            Params => Input_Parameters (Call, Caller_Bounds),
            Data   => Calculator.Bounded_Pool (
               Pool   => Data,
               Bounds => Assertions.Call_Values (Call, Assert_Map))),
         Inherit_Inv => Inherit_Inv,
         Asserts     => Asserts,
         Assert_Map  => Assert_Map,
         Bounds_Set  => Bounds_Set);

   exception

   when Flow.False_Path =>
      -- This call seems infeasible because the input flux is empty.
      -- This may be due to an infeasible branch or a conflict between
      -- derived input bounds and asserted input bounds for this call.

      Mark_Infeasible (Call, Caller_Bounds);

   end Bound_Call;


   --
   --    Context-specific bounding of calls by intervals
   --


   function Parameter_Bounds (
      Call   : Programs.Call_T;
      Params : Flow.Calls.Parameter_Map_T;
      Data   : Storage.Bounds.Cell_Interval_List_T)
   return Storage.Bounds.Cell_Interval_List_T
   --
   -- Bounds on Parameter cells (input cells) for a given Call.
   --
   -- Call
   --    The call for which input values will be bounded.
   -- Params
   --    The parameter mapping for the input parameters of the Call,
   --    pairing value-providing cells from the caller's frame with
   --    value-receiving cell's from the callee's frame.
   -- Data
   --    Bounds on the caller's cells at the call-step.
   --
   -- The cells in the result are in the callee frame.
   --
   -- If the Data flux turns out to be empty, this function propagates
   -- the exception Flow.False_Path.
   --
   is

      Param_Bounds : Storage.Bounds.Cell_Interval_List_T (1 .. Params'Length);
      Num_Bounds   : Natural := 0;
      --
      -- Real bounds on each input parameter from the call-context
      -- are collected in Param_Bounds(1 .. Num_Bounds).

      Caller_Cell : Storage.Cell_T;
      Callee_Cell : Storage.Cell_T;
      -- One mapped pair of caller/callee cells.

      Bound : Storage.Bounds.Interval_T;
      -- Bounds on a parameter (input) cell.

      Some_Void : Boolean := False;
      -- Whether some Bound is void (null set).

   begin

      -- Perhaps show the parameter associations:

      if Standard.Calling.Opt.Trace_Parameters then

         for F in Params'Range loop

            Output.Trace (Text =>
                 "Parameter association "
               & Storage.Name_Of (Params(F).Caller)
               & " -> "
               & Storage.Name_Of (Params(F).Callee));
         
         end loop;

      end if;

      -- Determine the bounds on actual parameter values,
      -- implied by the assertions and the caller's computation:

      for P in Params'Range loop

         Caller_Cell := Params(P).Caller;
         Callee_Cell := Params(P).Callee;

         Bound := Storage.Bounds.Interval (
            Cell => Caller_Cell,
            From => Data);

         if Storage.Bounds.Known (Bound) then
            -- A real bound, collect it:

            Num_Bounds := Num_Bounds + 1;

            Param_Bounds(Num_Bounds) := (
               Cell     => Callee_Cell,
               Interval => Bound);

            Some_Void := Some_Void or Storage.Bounds.Void (Bound);

         end if;

         if Standard.Calling.Opt.Trace_Parameters then

            Output.Result (
               Key   => "Param_Bounds",
               Locus => Output.Current_Locus,
               Text  =>
                    Storage.Name_Of (Caller_Cell)
                  & Output.Field_Separator
                  & Storage.Bounds.Image (
                       Item => Bound,
                       Name => Storage.Name_Of (Callee_Cell)));
         end if;

      end loop;

      -- Check for void bounds as signs of an infeasible call:

      if Some_Void then
         -- The call is probably infeasible.

         Output.Note ("Parameter flux is empty.");

         raise Flow.False_Path;

      end if;

      return Param_Bounds(1 .. Num_Bounds);

   end Parameter_Bounds;


   procedure Bound_Call (
      Call          : in Programs.Call_T;
      Caller_Bounds : in Programs.Execution.Bounds_Ref;
      Data          : in Storage.Bounds.Cell_Interval_List_T;
      Inherit_Inv   : in Storage.Cell_Set_T;
      Asserts       : in Assertions.Assertion_Set_T;
      Assert_Map    : in Assertions.Assertion_Map_T;
      Bounds_Set    : in Programs.Execution.Bounds_Set_T)
   is
   begin

      Bound_Call_With_Parameters (
         Call          => Call,
         Caller_Bounds => Caller_Bounds,
         Param_Bounds  => Parameter_Bounds (
            Call   => Call,
            Params => Input_Parameters (Call, Caller_Bounds),
            Data   => Data),
         Inherit_Inv => Inherit_Inv,
         Asserts     => Asserts,
         Assert_Map  => Assert_Map,
         Bounds_Set  => Bounds_Set);

   exception

   when Flow.False_Path =>
      -- This call seems infeasible because the input flux is empty.
      -- This may be due to an infeasible branch or a conflict between
      -- derived input bounds and asserted input bounds for this call.

      Mark_Infeasible (Call, Caller_Bounds);

   end Bound_Call;


end Bounds.Calling;
