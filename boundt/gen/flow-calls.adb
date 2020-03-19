-- Flow.Calls (body)
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
-- $Revision: 1.40 $
-- $Date: 2015/10/24 20:05:47 $
--
-- $Log: flow-calls.adb,v $
-- Revision 1.40  2015/10/24 20:05:47  niklas
-- Moved to free licence.
--
-- Revision 1.39  2014/06/11 12:56:29  niklas
-- Updated Add_Boundable_Call for new location of Code_Address_T.
--
-- Revision 1.38  2010-02-04 14:57:07  niklas
-- BT-CH-0219: Return_After tail-call steps.
--
-- Revision 1.37  2010-01-30 21:13:29  niklas
-- BT-CH-0216: Subprograms have a Return_Method_T attribute.
--
-- Revision 1.36  2008-09-07 07:29:08  niklas
-- BT-CH-0142: Conditional calls and option -[no_]tail_calls.
--
-- Revision 1.35  2008/03/23 10:38:02  niklas
-- Spelling and other corrections in descriptions.
--
-- Revision 1.34  2008/03/11 22:08:05  niklas
-- BT-CH-0121: Delayed calls and other SHARC support.
--
-- Revision 1.33  2007/07/22 14:40:19  niklas
-- Added Integration_Level and used it in a note.
--
-- Revision 1.32  2007/07/21 18:18:40  niklas
-- BT-CH-0064. Support for AVR/IAR switch-handler analysis.
--
-- Revision 1.31  2007/05/05 08:42:02  niklas
-- Changed error message for recursive integrated calls.
--
-- Revision 1.30  2007/05/05 08:10:41  niklas
-- Added check and error for recursive integration of the same callee.
--
-- Revision 1.29  2007/03/09 13:50:22  niklas
-- Using Output.Trace for construction tracing.
--
-- Revision 1.28  2007/03/04 19:56:03  niklas
-- Extended Input_Parameters to Note when a callee input cell
-- is not visible in the caller.
--
-- Revision 1.27  2007/01/13 13:51:03  niklas
-- BT-CH-0041.
--
-- Revision 1.26  2006/11/26 22:07:25  niklas
-- BT-CH-0039.
--
-- Revision 1.25  2006/10/24 08:44:30  niklas
-- BT-CH-0028.
--
-- Revision 1.24  2006/08/22 13:50:48  niklas
-- BT-CH-0025.
--
-- Revision 1.23  2006/08/22 12:28:34  niklas
-- Making use of the option Warn_No_Return for warnings about
-- calls to non-returning subprograms.
--
-- Revision 1.22  2006/05/29 15:09:13  niklas
-- Added functions Is_Resolved_Call (Step) and ditto (Node) for
-- use in RapiTime.Export.
--
-- Revision 1.21  2006/05/29 11:22:34  niklas
-- BT-CH-0023.
--
-- Revision 1.20  2006/05/17 20:09:08  niklas
-- Added variants of the functions Is_Call, Is_Unresolved_Call
-- and Call_In that take a Node, instead of a Step.
--
-- Revision 1.19  2006/05/06 06:59:20  niklas
-- BT-CH-0021.
--
-- Revision 1.18  2006/04/14 18:00:39  niklas
-- BT-CH-0019.
--
-- Revision 1.17  2006/02/27 09:27:15  niklas
-- Corrected the description of Add_Call to allow delayed calls
-- as in the SPARC, where the From step is a delay step and is
-- not the step for the call instruction.
-- Extended Add_Call (actually Add_Reference_Call) to omit the
-- return edge if the callee is known not to return.
--
-- Revision 1.16  2005/10/20 15:28:24  niklas
-- Extended Add_Boundable_Call to return the call-step.
--
-- Revision 1.15  2005/10/13 19:27:38  niklas
-- BT-CH-0014.
--
-- Revision 1.14  2005/09/23 10:50:45  niklas
-- Added function Call_In (Step).
--
-- Revision 1.13  2005/09/05 11:23:37  niklas
-- BT-CH-0007.
--
-- Revision 1.12  2005/09/03 11:50:27  niklas
-- BT-CH-0006.
--
-- Revision 1.11  2005/02/16 21:11:42  niklas
-- BT-CH-0002.
--
-- Revision 1.10  2005/02/04 20:47:49  niklas
-- In Add_Call, changed the output parameter Giving from Step_T to
-- Call_T to ease access to the caller subprogram.
--
-- Revision 1.9  2004/08/09 19:51:31  niklas
-- BT-CH-0001.
--
-- Revision 1.8  2004/04/25 20:52:08  niklas
-- First Tidorum version.
-- Using parameter mode "in" for Graph_T and Step_T (reference semantics).
-- Taking the Cell_T stuff from the Storage package, not from Arithmetic.
-- Added support for alias ranges and constant propagation.
-- Added Frame expression list as component to Param_Dynamics_T.
-- Added Map_T for quickly finding the call, if any, in a given step.
--
-- Revision 1.7  2003/03/11 08:23:57  holsti
-- Split execution-bounds stuff from the Programs package to
-- make the child package Programs.Execution.
--
-- Revision 1.6  2003/02/17 16:16:18  holsti
-- Added option Trace_Calls (-trace calls) to display calls as they are found.
--
-- Revision 1.5  2003/01/03 11:46:48  holsti
-- Using Flow.Opt.Trace_Construction.
--
-- Revision 1.4  2001/11/19 11:02:24  saarinen
-- Processor.Sub_Info_T -> Processor.Program.Sub_Info_T.
--
-- Revision 1.3  2001/03/21 20:32:26  holsti
-- Output with Locus values.
--
-- Revision 1.2  2001/03/19 08:10:29  ville
-- Add_Calls requires call step and denies effort
--
-- Revision 1.1  2001/03/10 00:26:15  holsti
-- First version.
--


with Decoder;
with Output;
with Processor.Properties;

with Flow.Calls.Opt;
with Flow.Opt;


package body Flow.Calls is


   --
   ---   Step contexts for calls, by reference and integrated.
   --


   type Call_Context_T is new Step_Context_T with record
      Target    : Processor.Code_Address_T;
      Call_Step : Step_T;
      Call      : Programs.Call_T;
      Retur     : Return_T;
   end record;
   --
   -- A context for a step (-tag) that identifies the step as a
   -- synthetic call-step that is a proxy for the execution of a callee
   -- as part of the execution of the caller. For delayed calls the
   -- context is also used for steps on the path from the call
   -- instruction to the point where control transfers to the callee.
   -- There can be multiple delay paths for the same call (for example,
   -- in the SHARC the delay instructions may be conditional, which
   -- is modelled as branching delay paths in the flow-graph).
   --
   -- Target
   --    The entry address of the callee, when statically known.
   --    Important for delayed calls. TBD for dynamic calls.
   --    Target = Entry_Address (Callee (Call)) when Call /= No_Call.
   -- Call_Step
   --    The call-step, or No_Step is there is no call-step yet.
   --    For delayed calls, this component distinguishes between the
   --    call-step itself and the steps on the delay path; the latter
   --    are tagged with the same call-context as the call-step but
   --    are /= Call_Step.
   -- Call
   --    The call in question. No_Call if there is no call yet, either
   --    because the call is unresolved or because the call is delayed
   --    and we are still tracing out the delay path(s).
   -- Retur
   --    The return point of the call, as specified when the Call
   --    was created. This is needed chiefly for computed returns from
   --    unresolved calls because the flow-graph edges for the return
   --    (from the call-step to the return point) are created only after
   --    the callees are resolved.
   --
   -- Each call (site) from a caller subprogram to a known callee subprogram
   -- (except those calls that are "in line" decoded) will be represented
   -- by:
   --
   -- > a Call_T,
   -- > a Call_Context_T which refers to that Call_T, and
   -- > a Step_T = Call_Context_T.Call_Step with a Tag.Context that
   --   refers to that Call_Context_T.
   --
   -- If the call is delayed, the Step_Ts on the delay path(s) also refer
   -- to the same Call_Context_T, but are different from the Call_Step.
   --
   -- The Call_T (when it exists) refers back to the Call_Context_T.Call_Step
   --
   -- When the callee subprogram is not known, we have an unresolved call
   -- and then Call = Programs.No_Call. Thus, an unresolved call is
   -- represented only by:
   --
   -- > a Call_Context_T with Call = No_Call, and
   -- > a Step_T = Call_Context_T.Call_Step with a Tag.Context that
   --   refers to that Call_Context_T.
   --
   -- TBD/TBA delayed, unresolved calls.
   --
   -- There is no Call_T for an unresolved call-step.


   type Call_Context_Ref is access all Call_Context_T'Class;
   --
   -- A reference to a Call Context.


   -- overriding
   function Image (Item : Call_Context_T) return String;
   --
   -- Overrides Flow.Image.


   function Image (Item : Call_Context_T) return String
   is
   begin

      return "@";

   end Image;


   type Integrated_Context_T is new Step_Context_T with record
      Host          : Programs.Subprogram_T;
      Callee        : Processor.Code_Address_T;
      Return_Method : Calling.Return_Method_T;
      Retur         : Return_T;
   end record;
   --
   -- A context that tags all the steps and edges that represent an
   -- integrated decoding of a Callee subprogram, as part of the
   -- flow-graph of a Host subprogram.
   --
   -- The Host may not be the direct caller of the callee; there may
   -- be a chain of integrated calls that originate at the Host and
   -- integrate a whole call-graph into the Host's flow-graph.
   --
   -- The context also identifies the Return point in the caller's
   -- flow-graph or in a higher level or via a computed return. When
   -- the integrated decoding reaches a return step in the callee, the
   -- Return is applied to make return edge(s) from this step and to
   -- terminate the integrated decoding path at this step.
   --
   -- The context also gives the Return_Method used by the callee,
   -- again for use when the integrated decoding reaches a return step
   -- in the callee. The Return_Method may modify the Return.
   --
   -- The Callee is identified by its entry address because we do not
   -- always want to make integrated subprograms into Subprogram_Ts.
   --
   -- An integrated call has neither a call-step nor a Call_T.


   type Integrated_Context_Ref is access all Integrated_Context_T'Class;
   --
   -- A reference to an Integrated Context.


   -- overriding
   function Image (Item : Integrated_Context_T) return String;
   --
   -- Overrides Flow.Image.


   function Image (Item : Integrated_Context_T) return String
   is
   begin

      return "+";

   end Image;


   procedure Trace_Call (
      Key    : in String;
      Callee : in Programs.Subprogram_T)
   --
   -- Traces the call to Callee, with the given Key.
   --
   is

      Callee_Locus : constant Output.Locus_T := Programs.Locus (Callee);
      -- The locus of the callee.

   begin

      Output.Result (
         Key   => Key,
         Text  =>
              Output.Source_File (Callee_Locus)
            & Output.Field_Separator
            & Output.Call_Path (Callee_Locus)
            & Output.Field_Separator
            & Output.Image (Output.Statements (Callee_Locus)));

   end Trace_Call;


   function Call_Tag (
      From   : Step_T;
      Target : Processor.Code_Address_T;
      Retur  : Return_T)
   return Step_Tag_T
   --
   -- The tag of a call-step, when the call issues From a given step,
   -- has the given Target, and Returns in the given way (possibly
   -- undefined).
   --
   -- The result refers to a new Call_Context_T, but this context is
   -- only partially defined: the Call_Step and Call are null.
   --
   is
   begin

      if Flow.Opt.Trace_Construction then

         Output.Trace (
              "Create context for immediate call from "
            & Image (Tag (From))
            & " to "
            & Processor.Image (Target));

      end if;

      return (
         Context => new Call_Context_T'(
            Outer     => Context (From),
            Call_Step => No_Step,
            Target    => Target,
            Call      => Programs.No_Call,
            Retur     => Retur),
         Data  => Post_Data (From),
         State => State (From));

   end Call_Tag;


   procedure Add_Reference_Call (
      To         : in     Graph_T;
      Call_Tag   : in     Step_Tag_T;
      Caller     : in     Programs.Subprogram_T;
      Callee     : in     Programs.Subprogram_T;
      Protocol   : in     Calling.Protocol_Ref;
      Step_Info  : in     Processor.Step_Info_T;
      Call_Step  :    out Step_T;
      Call       :    out Programs.Call_T)
   --
   -- Add a call that is decoded in the normal way, by reference,
   -- using a synthetic call-step and a Call_T.
   --
   -- This procedure encapsulates a common part of the public
   -- operations Add_Call, Add_Delayed_Call, and Add_Resolved_Call.
   -- The parameters with the same names as in those two operations
   -- have the same roles here.
   --
   -- Call_Tag is the complete tag for the Call_Step, including
   -- the proper Context, Data, and State. The Context initially
   -- has null Call_Step and Call. Context.Retur defines the return
   -- from the call and must not be Undefined. If return is To_Caller,
   -- and the context of the return tag is this (call) Context, that
   -- context is removed from the return tag for the return edge.
   -- If return is To_Higher, the call-step is marked as a return
   -- point by calling Return_After.
   --
   -- The new call-step is returned in Call_Step and the new call
   -- object is returned in Call. They are also stored in the Context
   -- of the Call_Tag.
   --
   -- This procedure does *not* create the edge From the call-instruction
   -- step to the Call_Step, because this is done differently in Add_Call,
   -- Add_Delayed_Call, and Add_Resolved_Call.
   --
   is
      use type Calling.Return_Way_T;

      Unused : constant Boolean := Programs.Unused (Callee);
      -- Whether the callee is an unused subprogram.

      Call_Context : constant Call_Context_Ref :=
         Call_Context_Ref (Call_Tag.Context);
      -- The context object for the call.

      Return_Method : constant Calling.Return_Method_T :=
         Programs.Return_Method (Callee);
      -- How and if the callee returns, from the callee's point of view.

      Return_Tag : Step_Tag_T;
      -- The effective tag for return To_Caller.

   begin

      -- Create the call-step:

      Add_Step (
         To      => To,
         Tag     => Call_Tag,
         Effect  => Arithmetic.No_Effect,
         Info    => Step_Info,
         Giving  => Call_Step);

      -- Create and add the call to the program:

      Programs.Add_Call (
         Caller   => Caller,
         Callee   => Callee,
         Protocol => Protocol,
         Step     => Call_Step,
         Giving   => Call);

      -- Complete the Call Context:

      Call_Context.Call_Step := Call_Step;
      Call_Context.Call      := Call;

      -- Optional trace:

      if Unused and Opt.Trace_Unused_Calls then

         Trace_Call (Key => "Unused_Call", Callee => Callee);

      elsif Opt.Trace_Calls then

         Trace_Call (Key => "Call", Callee => Callee);

      end if;

      -- Now for the return:

      if Return_Method.Way = Calling.No_Return then
         -- The callee never returns.
         -- Off into the wild blue yonder...

         if Opt.Warn_No_Return and not Unused then
            -- The callee may be used (the call may be executed)
            -- but the callee never returns and we should warn of
            -- such things. (Unused subprograms are considered not
            -- to return.)

            Output.Warning (
               Locus => Programs.Locus (Call),
               Text  => "Callee never returns.");

         end if;

         if Call_Context.Retur.Kind = To_Higher then

            Output.Warning (
               Locus => Programs.Locus (Call),
               Text  => "Tail call to callee that never returns.");
            --
            -- The problem is that in the analysis of the caller the
            -- tail call seems to return to a higher level, but actually
            -- it never returns because the callee never returns.

         end if;

      else
         -- The callee returns in some way.

         case Call_Context.Retur.Kind is

         when Undefined =>

            Output.Fault (
               Location => "Flow.Calls.Add_Reference_Call",
               Text     => "Return is Undefined.");

         when To_Caller =>
            -- Connect the Call_Step to the return point:

            Return_Tag := Call_Context.Retur.Tag;

            if Return_Tag.Context = Step_Context_Ref (Call_Context) then
               -- This call stops here.

               Return_Tag.Context := Call_Context.Outer;

            end if;

            -- Perhaps modify the return point by callee actions:

            case Calling.Return_Some_Way_T (Return_Method.Way) is

            when Calling.Normal_Return =>

               null;

            when Calling.Offset_Return =>

               Return_Tag := Flow.Transit (
                  From => Return_Tag,
                  To   => Processor.Properties.Offset_Return (
                     From => Flow.State (Return_Tag),
                     By   => Return_Method.Offset));

            end case;

            -- From the call step to the return point:

            Add_Edge (
               To     => To,
               Source => Call_Step,
               Time   => 0,
               Target => Return_Tag);

         when To_Higher =>
            -- The Call_Step also returns from the Caller.

            Output.Note (
               Locus => Programs.Locus (Call),
               Text  => "Tail call returns to higher level.");

            if Return_Method.Way /= Calling.Normal_Return then

               Output.Warning (
                    "Tail call returns to higher level by "
                  & Calling.Image (Return_Method));
               --
               -- The problem is that the special return (eg. offset)
               -- should be applied to the higher-level call, but
               -- we have no mechanism for that at present.

            end if;

            Return_After (
               Step   => Call_Step,
               Within => To);

         when To_Computed =>
            -- The return point is computed in some dynamic way.

            if Opt.Warn_Computed_Return then

               Output.Warning (
                  Locus => Programs.Locus (Call),
                  Text  => "Return point is dynamically computed.");

            end if;

            if Return_Method.Way /= Calling.Normal_Return then

               Output.Warning (
                    "Computed return uses "
                  & Calling.Image (Return_Method));
               --
               -- The problem is that the special return (eg. offset)
               -- should be applied to the computed return address. TBA.

            end if;

            Add_Return (
               From  => Call,
               Via   => Call_Context.Retur.Computed.all,
               After => Call_Step,
               To    => To);

         end case;

      end if;

   end Add_Reference_Call;


   function Already_Integrating (
      Callee : Processor.Code_Address_T;
      Tag    : Step_Tag_T)
   return Boolean
   --
   -- Whether the context of the given (calling) Tag is already one
   -- in which the given Callee is being integrated.
   --
   is
      use type Processor.Code_Address_T;

      Ref : Step_Context_Ref := Tag.Context;
      -- A context of the Tag.

      Int_Ref : Integrated_Context_Ref;
      -- An integration context of the Tag.

   begin

      while Ref /= null loop

         if Ref.all in Integrated_Context_T'Class then
            -- Already integrating some routine.

            Int_Ref := Integrated_Context_Ref (Ref);

            if Callee = Int_Ref.Callee then
               -- Already integrating same callee routine.

               return True;

            end if;

         end if;

         -- No match in this context level.
         -- Look at outer ones:

         Ref := Ref.Outer;

      end loop;

      -- No match in any context level.

      return False;

   end Already_Integrating;


   function Already_Integrating (
      Callee : Processor.Code_Address_T;
      Step   : Step_T)
   return Boolean
   --
   -- Whether the context of the given (calling) Step is already one
   -- in which the given Callee is being integrated.
   --
   is
   begin

      return Already_Integrating (Callee, Tag (Step));

   end Already_Integrating;


   function Integration_Entry (
      Host   : Programs.Subprogram_T;
      From   : Step_T;
      Callee : Programs.Subprogram_T;
      Retur  : Return_T)
   return Step_Tag_T
   is

      Integrated_Context : Integrated_Context_Ref;
      -- The integrated call context.

   begin

      Integrated_Context := new Integrated_Context_T'(
         Outer         => Context (From),
         Host          => Host,
         Callee        => Programs.Entry_Address (Callee),
         Return_Method => Programs.Return_Method (Callee),
         Retur         => Retur);

      return Step_Tag_T'(
         Context => Step_Context_Ref (Integrated_Context),
         Data    => Post_Data (From),
         State   => Processor.Properties.Entry_State (Callee));

   end Integration_Entry;


   function Integration_Level (Tag : Step_Tag_T) return Natural
   is

      Context : Step_Context_Ref := Tag.Context;
      -- For scanning the context stack.

      Level : Natural := 0;
      -- To be the result.

   begin

      while Context /= null loop

         if Context.all in Integrated_Context_T'Class then

            Level := Level + 1;

         end if;

         Context := Context.Outer;

      end loop;

      return Level;

   end Integration_Level;


   --
   ---   Detecting tail calls, optimized into jumps or other things
   --


   function Maybe_Tail_Call (
      From : Programs.Subprogram_T;
      To   : Processor.Code_Address_T)
   return Boolean
   is
      use type Processor.Code_Address_T;
   begin

      return   Opt.Detect_Tail_Calls
      and then To /= Programs.Entry_Address (From)
      and then Programs.Enters_Subprogram (
                  Address => To,
                  Program => Programs.Program (From));

   end Maybe_Tail_Call;


   --
   ---   Creating calls
   --


   procedure Add_Call (
      To        : in     Graph_T;
      From      : in     Step_T;
      Caller    : in     Programs.Subprogram_T;
      Target    : in     Processor.Code_Address_T;
      Cond      : in     Arithmetic.Condition_T := Arithmetic.Always;
      Time      : in     Processor.Time_T;
      Protocol  : in     Calling.Protocol_Ref;
      Call_Info : in     Processor.Program.Sub_Info_T;
      Step_Info : in     Processor.Step_Info_T;
      Form      : in     Call_Form_T := Any_Form;
      Retur     : in     Return_T;
      Giving    :    out Programs.Call_T)
   is
      use type Processor.Effort_T;

      Integrate : Boolean;
      -- Whether the call should be decoded by integration.

      Call_Step : Step_T;
      -- The new call-step, for a call decoded by reference.

      Callee : Programs.Subprogram_T;
      -- The callee subprogram.

   begin

      if Processor.Effort (Step_Info) /= Processor.No_Effort then

         Output.Fault (
            Location => "Flow.Calls.Add_Call",
            Text     => "Step_Info has effort");

      end if;

      if Flow.Opt.Trace_Construction then

         Output.Trace (
              "Add call after "
            & Image (Tag (From))
            & " from "
            & Programs.Name (Caller)
            & " to "
            & Processor.Image (Target)
            & " when "
            & Arithmetic.Image (Cond)
            & ", protocol "
            & Calling.Image (Protocol.all));

      end if;

      -- Identify the Callee:

      Programs.Identify_Callee (
         Caller => Caller,
         Target => Target,
         Info   => Call_Info,
         Callee => Callee);

      -- Choose the calling form:

      case Form is

      when Any_Form =>

         Integrate := Programs.Integrate_Calls (Callee);

      when Reference_Form =>

         Integrate := False;

      when Integrated_Form =>

         Integrate := True;

      end case;

      if Integrate then
         -- This Callee must be decoded "integrated", without creating
         -- a Call_T or a call-step. The Callee does not need to be
         -- decoded or analysed on its own.

         if Already_Integrating (Callee => Target, Step => From) then
            -- Attempted recursive integration of the same callee routine.
            -- Too risky for us.

            Output.Error (
                 "Recursive integrated call to "
               & Programs.Name (Callee)
               & " at "
               & Processor.Image (Target)
               & " changed to normal (recursive) call.");

            Integrate := False;

         end if;

      end if;

      if Integrate then
         -- The call will be decoded by integration.

         -- A loose edge to enter an integrated context and thus
         -- decode the Callee as an integrated part of the Caller:

         Output.Note (
              "Integrating call to "
            & Programs.Name (Callee)
            & " from integration level"
            & Natural'Image (Integration_Level (Tag (From))));

         if Opt.Trace_Calls then

            Trace_Call (Key => "Integrated_Call", Callee => Callee);

         end if;

         Decoder.Integrate_Call (
            Host   => Caller,
            Graph  => To,
            Callee => Callee,
            Source => From,
            Cond   => Cond,
            Time   => Time,
            Target => Integration_Entry (
               Host   => Caller,
               From   => From,
               Callee => Callee,
               Retur  => Retur));

      else
         -- This Callee should be decoded by reference in the
         -- normal way, with a call-step that refers to a Call_T.

         -- Create and add the call-step to the graph:

         Add_Reference_Call (
            To        => To,
            Call_Tag  => Call_Tag (From, Target, Retur),
            Caller    => Caller,
            Callee    => Callee,
            Protocol  => Protocol,
            Step_Info => Step_Info,
            Call_Step => Call_Step,
            Call      => Giving);

         -- Add the edge to the Call_Step:

         Add_Edge (
            To     => To,
            Source => From,
            Cond   => Cond,
            Time   => Time,
            Target => Call_Step);

      end if;

   end Add_Call;


   function Delayed_Call_Root (
      After  : Step_Tag_T;
      Target : Processor.Code_Address_T)
   return Step_Tag_T
   is

      Call_Context : Call_Context_Ref;
      -- The new call context.

   begin

      if Flow.Opt.Trace_Construction then

         Output.Trace (
              "Create context for delayed call after "
            & Image (After)
            & " to "
            & Processor.Image (Target));

      end if;

     -- A new call context:

      Call_Context := new Call_Context_T'(
         Outer     => After.Context,
         Target    => Target,
         Call_Step => No_Step,
         Call      => Programs.No_Call,
         Retur     => (Kind => Undefined));
      --
      -- The Call_Step, Call, and Retur components will be set later,
      -- in Add_Delayed_Call.

      return (
         Context => Step_Context_Ref (Call_Context),
         Data    => After.Data,
         State   => After.State);

   end Delayed_Call_Root;


   function Abandon_Delayed_Call (Tag : Step_Tag_T) return Step_Tag_T
   is
   begin

      if       Tag.Context /= null
      and then Tag.Context.all in Call_Context_T'Class
      then
         -- It might be a context for a delayed call (but could
         -- also be a context for a non-delayed call-step).

         return (
            Context => Tag.Context.Outer,
            Data    => Tag.Data,
            State   => Tag.State);

      else
         -- Nah.

         Output.Fault (
            Location => "Flow.Calls.Abandon_Delayed_Call",
            Text     =>
                 "Not a delayed-call tag "
               & Image (Tag));

         return Tag;

      end if;

   end Abandon_Delayed_Call;


   procedure Add_Delayed_Call (
      To        : in     Graph_T;
      After     : in     Loose_Edge_T;
      Caller    : in     Programs.Subprogram_T;
      Protocol  : in     Calling.Protocol_Ref;
      Call_Info : in     Processor.Program.Sub_Info_T;
      Step_Info : in     Processor.Step_Info_T;
      Form      : in     Call_Form_T := Any_Form;
      Retur     : in     Return_T;
      Giving    :    out Programs.Call_T)
   is
      use type Programs.Call_T;

      Tag : constant Step_Tag_T := After.Target;
      -- The tag for the call-step.

      Context : Call_Context_Ref;
      -- Tag.Context, as a call-context reference.

      Callee : Programs.Subprogram_T;
      -- The callee subprogram.

      Integrate : Boolean;
      -- Whether the call should be decoded by integration.

      Call_Step : Step_T;
      -- The created call-step.

   begin

      if      Tag.Context = null
      or else Tag.Context.all not in Call_Context_T'Class
      or else Call_Context_Ref (Tag.Context).Call /= Programs.No_Call
      then

         Output.Fault (
            Location => "Flow.Calls.Add_Delayed_Call",
            Text     =>
                 "After.Target is not a delay-path tag "
               & Image (Tag));

         raise Program_Error;

      end if;

      Context := Call_Context_Ref (Tag.Context);

      if Flow.Opt.Trace_Construction then

         Output.Trace (
              "Add delayed call at"
            & Image (Tag)
            & " to "
            & Processor.Image (Context.Target)
            & " when "
            & Arithmetic.Image (After.Cond));

      end if;

      -- Identify the Callee:

      Programs.Identify_Callee (
         Caller => Caller,
         Target => Context.Target,
         Info   => Call_Info,
         Callee => Callee);

      -- Choose the calling form:

      case Form is

      when Any_Form =>

         Integrate := Programs.Integrate_Calls (Callee);

      when Reference_Form =>

         Integrate := False;

      when Integrated_Form =>

         Integrate := True;

      end case;

      if Integrate then
         -- This Callee must be decoded "integrated", without creating
         -- a Call_T or a call-step. The Callee does not need to be
         -- decoded or analysed on its own.

         if Already_Integrating (Callee => Context.Target, Tag => Tag) then
            -- Attempted recursive integration of the same callee routine.
            -- Too risky for us.

            Output.Error (
                 "Recursive integrated call to "
               & Programs.Name (Callee)
               & " at "
               & Image (Tag)
               & " changed to normal (recursive) call.");

            Integrate := False;

         end if;

      end if;

      if Integrate then
         -- The call will be decoded by integration.
         --
         -- At present, we cannot do this, because the context is
         -- not compatible with Decoder.Integrate_Call; we have a
         -- Loose_Edge, so we must create a step with this Tag,
         -- but Decoder.Integrate_Call creates an edge, not a step.

         Output.Fault (
            Location => "Flow.Calls.Add_Delayed_Call",
            Text     => "Integration of delayed calls not implemented.");

         raise Program_Error;

      else
         -- This Callee should be decoded by reference in the
         -- normal way, with a call-step that refers to a Call_T.

         -- We now know how the call returns:

         Context.Retur := Retur;

         -- And can create a reference call:

         Add_Reference_Call (
            To        => To,
            Call_Tag  => Tag,
            Caller    => Caller,
            Callee    => Callee,
            Protocol  => Protocol,
            Step_Info => Step_Info,
            Call_Step => Call_Step,
            Call      => Giving);

         -- The given loose edge (After) is now automatically bound
         -- to the newly created Call_Step, because their tags match.

      end if;

   end Add_Delayed_Call;


   procedure Add_Boundable_Call (
      To     : in     Graph_T;
      From   : in     Step_T;
      Caller : in     Programs.Subprogram_T;
      Target : in     Dynamic_Edge_T;
      Effect : in     Arithmetic.Effect_Ref;
      Info   : in     Processor.Step_Info_T;
      Retur  : in     Return_T;
      Giving :    out Step_T)
   is

      Call_Step : Step_T;
      -- The new but unresolved call-step.

      Call_Context : Call_Context_Ref;
      -- The new but unresolved call context,

   begin

      if Flow.Opt.Warn_Dynamic_Flow then

         Output.Warning ("Dynamic call.");

      end if;

      if Flow.Opt.Trace_Construction then

         Output.Trace (
              "Add boundable call at "
            & Image (Tag (From))
            & " from "
            & Programs.Name (Caller)
            & " to "
            & Flow.Image (Target.all));

      end if;

      -- Add the boundable edge:

      if Target.Role /= Undefined then

         Output.Fault (
            Location => "Flow.Calls.Add_Boundable_Call",
            Text     =>
                 "Target role is "
               & Boundable_Edge_Role_T'Image (Target.Role));

      end if;

      Target.Role := Boundable_Call;

      Add_Dynamic_Edge (
         To     => To,
         Source => From,
         Edge   => Target);

      -- Make the unresolved call-step:

      Call_Context := new Call_Context_T'(
         Outer     => Context (From),
         Target    => Processor.Code_Address_Default,
         Call_Step => No_Step,
         Call      => Programs.No_Call,
         Retur     => Retur);

      Add_Step (
         To     => To,
         Tag    => (
            Context => Step_Context_Ref (Call_Context),
            Data    => Post_Data (From),
            State   => State (From)),
         Effect => Effect,
         Info   => Info,
         Giving => Call_Step);

      Call_Context.Call_Step := Call_Step;

      -- Connect the call-step:

      Add_Edge (
         To     => To,
         Source => From,
         Time   => 0,
         Target => Call_Step);

      case Return_Defined_T (Retur.Kind) is

      when To_Caller =>

         Add_Edge (
            To     => To,
            Source => Call_Step,
            Time   => 0,
            Target => Retur.Tag);

      when To_Higher =>

         Output.Note ("Dynamic call returns to higher level.");

         Return_After (
            Step   => Call_Step,
            Within => To);

      when To_Computed =>

         Output.Note ("Dynamic call with computed return.");

      end case;

      Giving := Call_Step;

   end Add_Boundable_Call;


   procedure Add_Resolved_Call (
      To        : in     Graph_T;
      Source    : in out Boundable_Edge_T'Class;
      Caller    : in     Programs.Subprogram_T;
      Target    : in     Processor.Code_Address_T;
      Cond      : in     Arithmetic.Condition_T := Arithmetic.Always;
      Time      : in     Processor.Time_T;
      Protocol  : in     Calling.Protocol_Ref;
      Call_Info : in     Processor.Program.Sub_Info_T;
      Step_Info : in     Processor.Step_Info_T;
      Giving    :    out Programs.Call_T)
   is
      use type Calling.Return_Way_T;
      use type Processor.Effort_T;

      Self : constant String := "Flow.Calls.Add_Resolved_Call";
      -- For Fault messages.

      From : constant Step_T := Flow.Source (Source);
      -- The source-step from which the dynamic call originates.

      Succ : constant Step_List_T := Successors (Step => From, Within => To);
      -- The successor steps. As a precondition, there must be at least
      -- one successor; all successors must be call-steps; and at most
      -- one successor can be an unresolved call-step.

      Return_Method : Calling.Return_Method_T;
      -- Whether and how the callee returns.

      Call_Step : Step_T := No_Step;
      -- The call-step, which is either the existing unresolved call-step
      -- or a new one. Initially we have no unresolved call-step.

      Call_Edge : Step_Edge_T;
      -- The edge from the call instruction (From) to the Call_Step,
      -- when there is an unresolved Call_Step.

      Callee : Programs.Subprogram_T;
      -- The callee subprogram.

      Outer_Context : Step_Context_Ref;
      -- The context of the unresolved call step that is resolved here
      -- (with the Call_Context popped off).

      Retur : Return_T;
      -- The return point, as taken from the call-context of one
      -- of the Succ steps.

   begin

      if Flow.Opt.Trace_Construction
      or Flow.Opt.Trace_Flow_Resolution
      then

         Output.Trace (
              "Add resolved call at "
            & Image (Tag (From))
            & " from "
            & Programs.Name (Caller)
            & " along "
            & Flow.Image (Source)
            & " to "
            & Processor.Image (Target)
            & ", protocol "
            & Calling.Image (Protocol.all));

      end if;

      if Processor.Effort (Step_Info) /= Processor.No_Effort then

         Output.Fault (
            Location => Self,
            Text     => "Step_Info has effort");

      end if;

      -- Identify the Callee:

      Programs.Identify_Callee (
         Caller => Caller,
         Target => Target,
         Info   => Call_Info,
         Callee => Callee);

      if Programs.Integrate_Calls (Callee) then
         -- This Callee should be decoded "integrated", without creating
         -- a Call_T or a call-step. However, this is not possible for
         -- a dynamically resolved call.

         Output.Error (
              "Cannot integrate dynamic call to "
            & Programs.Name (Callee)
            & ". Calling by reference.");

      end if;

      Return_Method := Programs.Return_Method (Callee);

      -- Find the return point:

      if Succ'Length = 0 then

         Output.Fault (
            Location => Self,
            Text     => "No (un)resolved call steps.");

         Retur := (Kind => To_Higher);

      else

         declare

            Context_Ref : constant Step_Context_Ref :=
               Context (Succ(Succ'First));
            -- This should be in Call_Context_T'Class.

         begin

            if       Context_Ref /= No_Context
            and then Context_Ref.all in Call_Context_T'Class
            then

               Retur := Call_Context_T (Context_Ref.all).Retur;

            else

               Output.Fault (
                  Location => Self,
                  Text     => "First Succ is not in Call_Context_T.");

               Retur := (Kind => To_Higher);

             end if;

          end;

      end if;

      -- Find or create the call-step:

      for S in Succ'Range loop

         if Is_Unresolved_Call (Succ(S)) then
            -- We can resolve this unresolved call-step.

            Call_Step := Succ(S);

            exit;

         end if;

      end loop;

      if Call_Step = No_Step then
         -- There is no unresolved call-step, so we must add
         -- a new (resolved) call-step.

         Add_Reference_Call (
            To        => To,
            Call_Tag  => Call_Tag (From, Target, Retur),
            Caller    => Caller,
            Callee    => Callee,
            Protocol  => Protocol,
            Step_Info => Step_Info,
            Call_Step => Call_Step,
            Call      => Giving);

         -- Add the edge to the Call_Step:

         Add_Resolved_Edge (
            To     => To,
            Source => Source,
            Cond   => Cond,
            Time   => Time,
            Target => Call_Step);

      else
         -- There is an unresolved call-step, so we can resolve
         -- it to this Target instead of making a new call-step.

         -- Create and add the call to the program:

         Programs.Add_Call (
            Caller   => Caller,
            Callee   => Callee,
            Protocol => Protocol,
            Step     => Call_Step,
            Giving   => Giving);

         if Opt.Trace_Calls then

            Trace_Call (Key => "Call", Callee => Callee);

         end if;

         -- Resolve the Call_Step by associating with the call:

         Call_Context_Ref (Context (Call_Step)).Call   := Giving;
         Call_Context_Ref (Context (Call_Step)).Target := Target;

         -- Set the other attributes of the call-step and the
         -- edge to the call-step:

         Call_Edge := Only_Edge_Into (Call_Step);

         Set_Time      (Along => Call_Edge, To => Time);
         Set_Condition (On    => Call_Edge, To => Cond);

         -- Count the resolvent and mark the Growing state in Source:

         Add_Resolvent (To => Source);

         -- Finish the return from this resolved call:

         case Return_Method.Way is

         when Calling.No_Return =>

            if Opt.Warn_No_Return then

               Output.Warning (
                  Locus => Programs.Locus (Giving),
                  Text  => "Resolved callee never returns.");

            end if;

         when Calling.Normal_Return =>

            null;

         when others =>

            Output.Warning (
                 "Resolved callee returns by "
               & Calling.Image (Return_Method));

         end case;

         case Return_Defined_T (Retur.Kind) is

         when To_Caller =>
            -- An unconditional return edge already was created,
            -- tentatively, in Add_Boundable_Call.

            if Return_Method.Way = Calling.No_Return then
               -- The return edge is now seen to be infeasible.

               Set_Condition (
                  On => Only_Edge_From (Call_Step),
                  To => Arithmetic.Never);

            end if;

         when To_Higher =>
            -- There should be and are (we hope) no return edges, unless
            -- the call is an integrated one (in which case there is
            -- a return edge that stops the integration).

            Outer_Context := Context (Call_Step).Outer;

            if (        Outer_Context = No_Context
                or else Outer_Context.all not in Integrated_Context_T'Class)
            and then
                Number_From (Call_Step, To) > 0
            then

               Output.Fault (
                  Location => Self,
                  Locus    => Programs.Locus (Giving),
                  Text     =>
                       "Tail-call step"
                     & Step_Index_T'Image (Index (Call_Step))
                     & " has some return edges.");

            end if;

            Output.Note (
               Locus => Programs.Locus (Giving),
               Text  => "Resolved callee returns to higher level.");

         when To_Computed =>

            if Programs.Returns (Callee) then
               -- No return edge was yet added in Add_Boundable_Call
               -- so we do it now.

               Add_Return (
                  From  => Giving,
                  Via   => Retur.Computed.all,
                  After => Call_Step,
                  To    => To);

             end if;

         end case;

      end if;

   end Add_Resolved_Call;


   function Is_Call (Step : Step_T) return Boolean
   is

      Context_Ref : constant Step_Context_Ref := Context (Step);
      -- The context of the step.

   begin

      -- The Step is a call step if its (immediate) Context is
      -- a Call_Context object and Step = Context.Call_Step.

      return      Context_Ref /= No_Context
         and then Context_Ref.all in Call_Context_T'Class
         and then Call_Context_Ref (Context_Ref).Call_Step = Step;

   end Is_Call;


   function Is_Call_Delay (Step : Step_T) return Boolean
   is

      Context_Ref : constant Step_Context_Ref := Context (Step);
      -- The context of the step.

   begin

      -- The Step is a delay step if its (immediate) Context is
      -- a Call_Context object and Step /= Context.Call_Step.

      return      Context_Ref /= No_Context
         and then Context_Ref.all in Call_Context_T'Class
         and then Call_Context_Ref (Context_Ref).Call_Step /= Step;

   end Is_Call_Delay;


   function Is_Resolved_Call (Step : Step_T) return Boolean
   is
      use type Programs.Call_T;

      Context_Ref : constant Step_Context_Ref := Context (Step);
      -- The context of the step.

   begin

      -- The Step is a call step if its (immediate) Context is
      -- a Call_Context object and Step = Context.Call_Step.
      -- It is a resolved call Context.Call /= No_Call.

      return      Context_Ref /= No_Context
         and then Context_Ref.all in Call_Context_T'Class
         and then Call_Context_Ref (Context_Ref).Call_Step = Step
         and then Call_Context_Ref (Context_Ref).Call /= Programs.No_Call;

   end Is_Resolved_Call;


   function Is_Call (Node : Node_T) return Boolean
   is
   begin

      return Is_Call (First_Step (Node));

   end Is_Call;


   function Is_Resolved_Call (Node : Node_T) return Boolean
   is
   begin

      return Is_Resolved_Call (First_Step (Node));

   end Is_Resolved_Call;


   function Is_Unresolved_Call (Step : Step_T) return Boolean
   is
      use type Programs.Call_T;

      Context_Ref : constant Step_Context_Ref := Context (Step);
      -- The context of the step.

   begin

      -- The Step is a call step if its (immediate) Context is
      -- a Call_Context object and Context.Call_Step = Step.
      -- It is unresolved if Context.Call = No_Call.

      return      Context_Ref /= No_Context
         and then Context_Ref.all in Call_Context_T'Class
         and then Call_Context_Ref (Context_Ref).Call_Step = Step
         and then Call_Context_Ref (Context_Ref).Call = Programs.No_Call;

   end Is_Unresolved_Call;


   function Is_Unresolved_Call (Node : Node_T) return Boolean
   is
   begin

      return Is_Unresolved_Call (First_Step (Node));

   end Is_Unresolved_Call;


   function Unresolved_Calls (Within : Dynamic_Edge_List_T)
   return Dynamic_Edge_List_T
   --
   -- Those dynamic edges from Within that represent dynamic calls.
   --
   is

      Calls : Dynamic_Edge_List_T (1 .. Within'Length);
      Last  : Natural := 0;
      -- The result will be Calls(1 .. Last).

   begin

      for W in Within'Range loop

         if Within(W).Role = Boundable_Call then
            -- Here is an unresolved call.

            Last := Last + 1;

            Calls(Last) := Within(W);

         end if;

      end loop;

      return Calls(1 .. Last);

   end Unresolved_Calls;


   function Unresolved_Calls (Within : Graph_T)
   return Dynamic_Edge_List_T
   is
   begin

      return Unresolved_Calls (Within => Dynamic_Edges (Within));

   end Unresolved_Calls;


   function Unresolved_Calls (Under : Computation.Model_Ref)
   return Dynamic_Edge_List_T
   is
   begin

      return Unresolved_Calls (Within => Computation.Dynamic_Edges (Under));

   end Unresolved_Calls;


   function Call_In (Step : Step_T) return Programs.Call_T
   is

      Context_Ref : constant Step_Context_Ref := Context (Step);
      -- The context of the step.

   begin

      if       Context_Ref /= No_Context
      and then Context_Ref.all in Call_Context_T'Class
      and then Call_Context_Ref (Context_Ref).Call_Step = Step
      then

         return Call_Context_T (Context_Ref.all).Call;

      else

         return Programs.No_Call;

      end if;

   end Call_In;


   function Call_In (Node : Node_T) return Programs.Call_T
   is
   begin

      return Call_In (First_Step (Node));

   end Call_In;


   function Callee_Address (Tag : Step_Tag_T)
   return Processor.Code_Address_T
   is
   begin

      return Call_Context_Ref (Tag.Context).Target;

   end Callee_Address;


   procedure Return_After (
      Step   : in Step_T;
      Within : in Graph_T)
   is
      use type Calling.Return_Way_T;

      Context_Ref : Step_Context_Ref := Context (Step);
      -- A context, if any, of the return step.
      -- We may have to ascend in the context nest if integrated
      -- calls are nested.

      Integration : Integrated_Context_Ref;
      -- Such a view of Context_Ref, when appropriate.

      Retur : Return_T;
      -- A return point from the integrated call.
      -- We may have to ascend through nested To_Higher returns.

      Return_Method : Calling.Return_Method_T;
      -- The return method of the integrated callee.

   begin

      if Number_From (Step, Within) > 0 then

         Output.Fault (
            Location => "Flow.Calls.Return_After",
            Text     => "The step is not a return point.");

      end if;

      if       Context_Ref /= No_Context
      and then Context_Ref.all in Call_Context_T'Class
      then
         -- The step is itself a call-step, which means that this
         -- call returns to a higher level (optimized tail call.
         -- We can ignore this context layer for our purposes.

         Context_Ref := Context_Ref.Outer;

      end if;

      while    Context_Ref /= No_Context
      and then Context_Ref.all in Integrated_Context_T'Class
      loop
         -- A return from an integrated call.

         Integration := Integrated_Context_Ref (Context_Ref);

         Return_Method := Integration.Return_Method;
         Retur         := Integration.Retur;

         Output.Note (
              "Returning "
            & Return_Kind_T'Image (Retur.Kind)
            & " from integrated call to "
            & Processor.Image (Integration.Callee)
            & " by "
            & Calling.Image (Return_Method));

         if Return_Method.Way = Calling.No_Return then
            -- Whoops, a contradiction.

            Output.Warning (
               "Returning from integrated call that should never return.");

            -- TBA TBC raise False_Path?

         end if;

         case Return_Defined_T (Retur.Kind) is

         when To_Caller =>
            -- Returning to the caller, so we connect the return
            -- step to the return point in the caller's flow-graph.

            case Return_Method.Way is

            when Calling.No_Return
               | Calling.Normal_Return =>

               null;

            when Calling.Offset_Return =>

               Retur.Tag := Flow.Transit (
                  From => Retur.Tag,
                  To   => Processor.Properties.Offset_Return (
                     From => Flow.State (Retur.Tag),
                     By   => Return_Method.Offset));

            end case;

            Decoder.Integrate_Return (
               Host   => Integration.Host,
               Graph  => Within,
               Source => Step,
               Time   => 0,
               Target => Retur.Tag);

            -- The return chain stops here, for now:

            Context_Ref := No_Context;

         when To_Higher =>
            -- A tail-call that returns to a higher level, so
            -- no return edge in the caller's flow-graph is apt
            -- for this level of integration, but an edge may be
            -- needed in an outer level.

            if  Return_Method.Way /= Calling.No_Return
            and Return_Method.Way /= Calling.Normal_Return
            then

               Output.Warning (
                    "Integrated call returns to higher level by "
                  & Calling.Image (Return_Method));

            end if;

            Context_Ref := Context_Ref.Outer;

         when To_Computed =>
            -- An integrated call with a computed return.
            -- Each return step in the integrated subprogram gets
            -- its own return edges from the Computed return.

            if  Return_Method.Way /= Calling.No_Return
            and Return_Method.Way /= Calling.Normal_Return
            then

               Output.Warning (
                    "Computed return from integrated call uses "
                  & Calling.Image (Return_Method));

            end if;

            Add_Return (
               From  => Programs.No_Call,
               Via   => Retur.Computed.all,
               After => Step,
               To    => Within);

         end case;

      end loop;

   end Return_After;


   --
   ---   Arithmetic effects of a call on the caller's computation
   --


   function Output_Cells (
      Call      : Programs.Call_T;
      Protocol  : Calling.Protocol_T'Class;
      Outputs   : Storage.Cell_List_T;
      Invariant : Storage.Cell_List_T)
   return Storage.Cell_List_T
   is
      use type Calling.Map_Kind_T;
      use type Storage.Cell_T;

      List : Storage.Cell_List_T (1 .. Outputs'Length);
      Last : Natural := 0;
      -- The result will be List(1 .. Last).

      Callee_Cell, Caller_Cell : Storage.Cell_T;
      -- An output cell in the callee frame and the corresponding
      -- cell (if any) in the caller frame.

      Call_Invariant : Boolean;
      -- Whether the Protocol considers Callee_Cell invariant.

      Map : Calling.Map_Kind_T;
      -- The mapping for Callee_Cell from the Protocol.

   begin

      for C in Outputs'Range loop

         Callee_Cell := Outputs(C);

         Call_Invariant := Calling.Invariant (Callee_Cell, Protocol);

         Map := Calling.Map_Kind (Callee_Cell, Protocol);

         if Call_Invariant or Map = Calling.Privy then
            -- No effect in the caller.

            null;

         else
            -- Possibly an effect on a caller cell.

            if Map = Calling.Fixed then
               -- Cell identity same for caller and callee.

               Caller_Cell := Callee_Cell;

            else
               -- Static or dynamic transformation of cell identity.

               Caller_Cell := Calling.Caller_Cell (Callee_Cell, Protocol);

            end if;

            if Caller_Cell = Storage.No_Cell then
               -- The protocol could not map the cell (yet).

               if Map = Calling.Static then
                  -- The protocol should have mapped the cell.

                  Output.Fault (
                     Location => "Flow.Calls.Effect",
                     Text     =>
                          "No caller cell from "
                        & Calling.Image (Protocol)
                        & " for callee cell "
                        & Storage.Image (Callee_Cell)
                        & " with "
                        & Calling.Map_Kind_T'Image (Map)
                        & " mapping.");

               else
                  -- Dynamic map may have failed due to loose bounds
                  -- on the protocol, for example unknown stack height.

                  Output.Note (
                       "Could not map callee cell "
                     & Storage.Image (Callee_Cell)
                     & " with "
                     & Calling.Map_Kind_T'Image (Map)
                     & " mapping.");

               end if;

            elsif not Storage.Is_Member (Caller_Cell, Invariant) then
               -- The change is visible in the caller.

               Last       := Last + 1;
               List(Last) := Caller_Cell;

            end if;

         end if;

      end loop;

      return List(1 .. Last);

   end Output_Cells;


   procedure Add_Call_Effect (
      Call      : in     Programs.Call_T;
      Protocol  : in     Calling.Protocol_T'Class;
      Outputs   : in     Storage.Cell_List_T;
      Invariant : in     Storage.Cell_List_T;
      To        : in out Arithmetic.Assignment_Set_T)
   is

      Caller_Cells : constant Storage.Cell_List_T :=
         Output_Cells (Call, Protocol, Outputs, Invariant);
      -- The call's output cells, in the caller frame.

   begin

      if Caller_Cells'Length > 0 then
         -- The call has some effect.

	 Arithmetic.Veil (
            Variables => Arithmetic.To_Variables (Caller_Cells),
            Within    => To);

	 if Opt.Trace_Call_Effects then

            Output.Trace (
        	 "Call veils "
               & Storage.Image (Caller_Cells));

         end if;

      end if;

   end Add_Call_Effect;


   procedure Add_Unknown_Call_Effect (
      Call      : in     Programs.Call_T;
      Protocol  : in     Calling.Protocol_T'Class;
      Stub      : in     Calling.Stub_Level_T;
      Upon      : in     Storage.Cell_List_T;
      To        : in out Arithmetic.Assignment_Set_T)
   is

      Cell : Storage.Cell_T;
      -- One of the cells Upon an effect may happen.

      Clobbered : Storage.Cell_List_T (1 .. Upon'Length);
      Last      : Natural := 0;
      -- The clobbered cells, for tracing.

   begin

      for U in Upon'Range loop

         Cell := Upon(U);

         if not Calling.Sure_Invariant (
            Caller => Cell,
            Under  => Protocol,
            Stub   => Stub)
         and then not Arithmetic.Is_Defined (Cell => Cell, By => To)
         then
            -- This cell may be clobbered and is not already
            -- assigned in the assignment set.

            Arithmetic.Veil (
               Variable => Arithmetic.Cell (Cell),
               Within   => To);

            Last            := Last + 1;
            Clobbered(Last) := Cell;

         end if;

      end loop;

      if Opt.Trace_Call_Effects and Last > 0 then

         Output.Trace (
              "Stub call (level "
            & Calling.Stub_Level_T'Image (Stub)
            & ") clobbers "
            & Storage.Image (Clobbered(1 .. Last)));

      end if;

   end Add_Unknown_Call_Effect;


   procedure Spare_Call_Invariants (
      Call      : in     Programs.Call_T;
      Invariant : in     Storage.Cell_Set_T;
      Within    : in out Computation.Model_Ref)
   is
      use type Arithmetic.Expr_Kind_T;

      Step : Flow.Step_T := Programs.Step (Call);
      -- The step that contains the call.

      Effect : Arithmetic.Effect_T := Computation.Effect (Step, Within);
      -- The existing effect of the step, to be updated
      -- by removing the assignments to invariant cells.

      Last : Natural := Effect'First - 1;
      -- The last validated assignment in Effect.

   begin

      for E in Effect'Range loop

         if Effect(E).Target.Kind = Arithmetic.Cell
         and then
            Storage.Is_Member (
               Cell   => Effect(E).Target.Cell,
               Of_Set => Invariant)
         then

            Output.Note (
               Locus => Programs.Locus (Call),
               Text  =>
                    "Asserted call invariant cell"
                  & Output.Field_Separator
                  & Storage.Image (Effect(E).Target.Cell));

         else
            -- This target is not asserted invariant, so it
            -- should be retained in the effect.

            Last := Last + 1;

            if Last < E then

               Effect(Last) := Effect(E);

            end if;

         end if;

      end loop;

      if Last < Effect'Last then
         -- Some assignments were deleted.

         Computation.Set_Effect (
            Step  => Step,
            To    => Arithmetic.To_Effect_Ref (Effect(Effect'First .. Last)),
            Under => Within);

      end if;

   end Spare_Call_Invariants;


   function Input_Parameters (
      Inputs   : Storage.Cell_List_T;
      Protocol : Calling.Protocol_T'Class)
   return Parameter_Map_T
   is
      use type Calling.Map_Kind_T;
      use type Storage.Cell_T;

      Kind : Calling.Map_Kind_T;
      -- The kind of mapping between caller/callee.

      Callee, Caller : Storage.Cell_T;
      -- An input cell in the callee frame and the corresponding
      -- mapped cell in the caller frame, if any, else No_Cell.

      Map  : Parameter_Map_T (1 .. Inputs'Length);
      Last : Natural := 0;
      --
      -- The result will be Map(1 .. Last).
      -- Callee-private cells are not included.

   begin

      for C in Inputs'Range loop

         Callee := Inputs(C);

         -- Find the corresponding caller-side cell, if any:

         Kind :=
            Calling.Map_Kind (
               Callee => Callee,
               Under  => Protocol);

         case Kind is

         when Calling.Fixed =>

            Caller := Callee;

         when Calling.Static
            | Calling.Dynamic =>

            -- This cell is visible to the caller.

            Caller := Calling.Caller_Cell (Callee, Protocol);

         when Calling.Privy =>

            -- This cell is private to the callee.
            -- Any knowledge of its value on entry to the callee
            -- comes from processor-specific bounds (as given by
            -- Processor.Properties.Entry_Bounds) or assertions.

            Caller := Storage.No_Cell;

         end case;

         -- Perhaps add the Caller-Callee pair to the Map:

         if Caller /= Storage.No_Cell then
            -- An input cell visible to the caller.

            Last := Last + 1;

            Map(Last) := (
               Caller => Caller,
               Callee => Callee);

         else

            Output.Note (
                 "Callee input cell "
               & Storage.Image (Callee)
               & " is not visible in the caller.");

            if Kind = Calling.Static then
            -- The protocol is at fault.

               Output.Fault (
        	  Location => "Flow.Calls.Input_Parameters",
        	  Text     =>
                      "No caller cell from "
                     & Calling.Image (Protocol)
                     & " for callee cell "
                     & Storage.Image (Callee)
                     & " with "
                     & Calling.Map_Kind_T'Image (Kind)
                     & " mapping.");

            end if;

         end if;

      end loop;

      return Map(1 .. Last);

   end Input_Parameters;


end Flow.Calls;
