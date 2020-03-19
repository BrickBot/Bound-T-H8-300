-- Flow.Calls (decl)
--
-- Call steps in control-flow graphs.
--
-- A call step is a step that represents a call from the current (caller)
-- subprogram to a lower-level (callee) subprogram. To find the effect
-- and execution time of call steps needs some type of inter-procedural
-- analysis. This package implements operations to create call steps,
-- resolve the input and output parameter cells, and find the effect of
-- a call on the caller's computation.
--
-- As a special case, some callee subprograms are decoded "integrated" in
-- the caller's control-flow graph. In such a case, a call-step is not
-- created, but instead the callee's instructions are decoded (again) in
-- the context of this call, and steps representing these instructions
-- are added to the caller's flow-graph. A normal edge links the call-
-- instruction step to the new step for the entry point of the callee,
-- and other normal or loose edges link each "integrated" step that
-- models a return instructions in the callee, to the return point in
-- the caller.
--
-- Whether a given call is decoded in the usual fashion (synthetic
-- call-step and a Call_T reference to the callee's flow-graph) or
-- integrated in the caller is decided by the properties of the callee
-- subprogram, specifically by Programs.Integrate_Calls.
--
-- A callee that is "integrated" in the caller's flow-graph may itself
-- contain calls, and these calls may be decoded as references or by
-- integration. Thus, an entire chain or graph of calls can be integrated
-- in the flow-graph of the original caller, which is then known as the
-- "host" subprogram for these integrated calls.
--
-- Another special case is presented by "dynamic" calls where the
-- address of the callee is dynamically computed rather than statically
-- set. A call-step with an unknown callee is called "unresolved".
-- An unresolved call-step is not associated with a Call_T.
-- An unresolved call-step can be resolved thru analysis of the
-- arithmetic computation of the callee's address, and is then connected
-- to a Call_T and thus to a callee and a calling protocol.
--
-- Finally, the return from a call may take various forms. In the
-- simplest case, the call returns to a statically known address (a
-- flow tag) that identifies a step in the caller's flow graph; then
-- a "return edge" is added from the call-step to this return point
-- at the same time as the call-step is created. There are two other
-- forms of return: tail calls and computed returns. When a call is
-- the last action taken by the caller before it, itself, returns, some
-- compilers generate optimized tail-call code in which the return
-- from the callee in fact returns also from the caller, to some higher
-- level of the call stack. For such a tail-call, no return edge from
-- the call-step is created, so the call-step is a return step in the
-- caller's flow-graph. Lastly, the return address for a call may
-- be computed in some dynamic way, which means that the computation must
-- be analysed to resolve the return. Such computed returns are modelled
-- as boundable edges (dynamic edges) and can be resolved by various
-- analyses; the result is either a normal return to a step in the
-- caller's flow-graph or a return to a higher level (a tail-call
-- return).
--
-- When a dynamic call has a computed return we first resolve the call
-- and then introduce the computed return as a boundable edge to be
-- resolved after the call is resolved. Thus, the analysis and resolution
-- of the computed return can take into account the effect of the actual
-- (resolved) callee. When the dynamic call resolves into several possible
-- callees the computed return is duplicated into as many instances
-- (copies) of the boundable return edge and each copy is resolved
-- separately and perhaps differently.
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
-- $Revision: 1.29 $
-- $Date: 2015/10/24 19:36:48 $
--
-- $Log: flow-calls.ads,v $
-- Revision 1.29  2015/10/24 19:36:48  niklas
-- Moved to free licence.
--
-- Revision 1.28  2010-02-04 14:57:07  niklas
-- BT-CH-0219: Return_After tail-call steps.
--
-- Revision 1.27  2008-09-07 07:29:08  niklas
-- BT-CH-0142: Conditional calls and option -[no_]tail_calls.
--
-- Revision 1.26  2008/03/23 10:38:02  niklas
-- Spelling and other corrections in descriptions.
--
-- Revision 1.25  2008/03/11 22:08:05  niklas
-- BT-CH-0121: Delayed calls and other SHARC support.
--
-- Revision 1.24  2007/07/22 14:40:19  niklas
-- Added Integration_Level and used it in a note.
--
-- Revision 1.23  2007/07/21 18:18:41  niklas
-- BT-CH-0064. Support for AVR/IAR switch-handler analysis.
--
-- Revision 1.22  2007/03/18 12:50:37  niklas
-- BT-CH-0050.
--
-- Revision 1.21  2007/01/13 13:51:03  niklas
-- BT-CH-0041.
--
-- Revision 1.20  2006/10/24 08:44:30  niklas
-- BT-CH-0028.
--
-- Revision 1.19  2006/08/22 13:50:48  niklas
-- BT-CH-0025.
--
-- Revision 1.18  2006/05/29 15:09:13  niklas
-- Added functions Is_Resolved_Call (Step) and ditto (Node) for
-- use in RapiTime.Export.
--
-- Revision 1.17  2006/05/29 11:22:34  niklas
-- BT-CH-0023.
--
-- Revision 1.16  2006/05/17 20:09:08  niklas
-- Added variants of the functions Is_Call, Is_Unresolved_Call
-- and Call_In that take a Node, instead of a Step.
--
-- Revision 1.15  2006/05/06 06:59:20  niklas
-- BT-CH-0021.
--
-- Revision 1.14  2006/04/14 18:00:40  niklas
-- BT-CH-0019.
--
-- Revision 1.13  2006/02/27 09:27:16  niklas
-- Corrected the description of Add_Call to allow delayed calls
-- as in the SPARC, where the From step is a delay step and is
-- not the step for the call instruction.
-- Extended Add_Call (actually Add_Reference_Call) to omit the
-- return edge if the callee is known not to return.
--
-- Revision 1.12  2005/10/20 15:28:24  niklas
-- Extended Add_Boundable_Call to return the call-step.
--
-- Revision 1.11  2005/10/13 19:27:39  niklas
-- BT-CH-0014.
--
-- Revision 1.10  2005/09/23 10:50:45  niklas
-- Added function Call_In (Step).
--
-- Revision 1.9  2005/09/05 11:23:38  niklas
-- BT-CH-0007.
--
-- Revision 1.8  2005/09/03 11:50:28  niklas
-- BT-CH-0006.
--
-- Revision 1.7  2005/02/16 21:11:42  niklas
-- BT-CH-0002.
--
-- Revision 1.6  2005/02/04 20:47:49  niklas
-- In Add_Call, changed the output parameter Giving from Step_T to
-- Call_T to ease access to the caller subprogram.
--
-- Revision 1.5  2004/04/25 20:52:08  niklas
-- First Tidorum version.
-- Using parameter mode "in" for Graph_T and Step_T (reference semantics).
-- Taking the Cell_T stuff from the Storage package, not from Arithmetic.
-- Added support for alias ranges and constant propagation.
-- Added Frame expression list as component to Param_Dynamics_T.
-- Added Map_T for quickly finding the call, if any, in a given step.
--
-- Revision 1.4  2003/03/11 08:23:57  holsti
-- Split execution-bounds stuff from the Programs package to
-- make the child package Programs.Execution.
--
-- Revision 1.3  2001/11/19 11:02:24  saarinen
-- Processor.Sub_Info_T -> Processor.Program.Sub_Info_T.
--
-- Revision 1.2  2001/03/21 20:32:29  holsti
-- Output with Locus values.
--
-- Revision 1.1  2001/03/10 00:26:19  holsti
-- First version.
--


with Calling;
with Flow.Computation;
with Processor.Program;
with Programs;
with Storage;


package Flow.Calls is


-- Calls and call-steps pass through the following phases, when they
-- are normal calls that are not decoded in the "integrated" way and
-- when the callee is statically known:
--
-- > Creation: The call is added as a call-step to the caller's flow-graph.
--
--   At this time, the callee's entry address and the primitive calling
--   protocol are known, but the callee subprogram may be new (not found
--   before) or may be known and already analysed to any level.
--
--   The primitive effect of the call-step is null in any case.
--
-- > Modeling: The effect of the call is added to a computation model
--   for the caller, as the effect of the call-step.
--
--   At this time, the control-flow graph of the callee is complete
--   and execution bounds exist for the callee, but may not be fully
--   bounded. The execution bounds define the input and output cells
--   of the call in the callee frame. The caller's computation model
--   defines the calling protocol which maps the input cells and the
--   effect from the callee frame to the caller frame.
--
-- > Update of unknown effect: If the callee has a wholly or partially
--   unknown arithmetic effect, which happens when the callee is a
--   stub or directly or indirectly calls stubs, the effect of the call
--   is open-ended in the sense that it can change any of the caller's
--   cells that are potentially accessible (writable) from the actual
--   subprograms that were replaced by stubs in the analysis. The
--   effect of the call step then contains veiling assignments (of
--   the Unknown value) to all such caller cells. If the set of cells
--   used in the caller is expanded, for example after resolving some
--   dynamic jumps, or the mapping between caller and callee cells is
--   changed by resolving a dynamic calling protocol, the effect of the
--   call step is updated to veil those added caller cells that may be
--   affected by the call. This may happen several times during the
--   analysis of the caller.
--
-- Calls that are decoded "integrated" are not handled in the above way.
-- No call-step is created; instead, the decoder is guided to decode the
-- callee in the context of the call, creating a subgraph of the caller's
-- flow-graph (in place of the call-step). The calling protocol is
-- ignored (currently) and each instruction has exactly the same effect
-- as it would have if the compiler had inlined the code of the callee
-- into the caller (in place of the call instruction). The call is not
-- represented as a Programs.Call_T.
--
-- Calls with a dynamically computed callee are handled as follows.
-- First, an unresolved call-step is created and added to the caller's
-- flow-graph, together with a boundable edge that represents the
-- computation of the callee address. If later analysis resolves this
-- boundable edge to one or more possible callees, the unresolved
-- call-step is resolved to represent the first possible callee. For
-- the other possible callees (if any) new (resolved) call-steps are
-- created as for a static call. The effect of each call to each
-- possible callee is added to the computation model as for static calls.
-- If a dynamic (boundable) call is not resolved at all, the unresolved
-- call-step remains unresolved and should model the possible effect of
-- any callee subprogram.
--
-- When a dynamic call has a computed return, the boundable return
-- edge is not entered in the flow-graph until the call is resolved.
-- Each callee and call-step resolved from the dynamic call gets its
-- own copy (instance) of the boundable return edge.
--
-- In addition to the above, a call may be "delayed". This means that
-- the target program prepares (starts) the call some time before
-- the call actually takes place; some number of instructions in the
-- calling subprogram are executed (or at least traversed) before
-- control transfers to the called subprogram. To cater for delayed
-- calls, this package allows the preparation of the call "context"
-- separately from and before the creation of the call-step. The
-- function Delayed_Call_Root prepares the context, and the procedure
-- Add_Delayed_Call completes the call by creating the call-step.
-- TBA/TBD integrated delayed calls and unresolved delayed calls.
--
-- When a processor used delayed calls, Processor.Flow_State_T for
-- a call-step must be different from its value for any other step,
-- so that the tag of the call-step is unique and different from the
-- tags of the steps on the delay path(s), although they have the
-- same call-context (from Delayed_Call_Root).


   type Call_Form_T is (Any_Form, Reference_Form, Integrated_Form);
   --
   -- Specifies the form of decoding a given call:
   --
   -- Any_Form
   --    Use either "reference" or "integrated" decoding, depending
   --    on other factors.
   -- Reference_Form
   --    Decode the call as a "reference" call.
   -- Integrated_Form
   --    Decode the call and the callee as an "integrated" call.


   --
   ---   Returns
   --


   -- Calls fall in four different classes depending on if and how they
   -- return:
   --
   -- > Normally, a call returns to the caller subprogram, which is
   --   modelled by an edge from the call-step to the return-point step
   --   in the caller's flow-graph.
   --
   -- > Tail calls use an optimized calling sequence in which the return
   --   from the callee also returns from the caller. The call-step is
   --   then a return step in the caller's flow-graph.
   --
   -- > The return point can be computed dynamically, which means that
   --   the return edge is a boundable edge. Analysis (bounding) of this
   --   edge may show that the call returns to the caller subprogram or
   --   that the call is a tail call (also returns from the caller).
   --
   -- > Some subprograms never return (in our model). Such a subprogram
   --   can end with an eternal loop or take some abnormal action, such
   --   as shutting down the program. The call-step for a non-returning
   --   call is not the source of any edge, and so appears as a return
   --   step for the caller, although it really is not a return step.


   type Return_Kind_T is (
      Undefined,
      To_Caller,
      To_Higher,
      To_Computed);
   --
   -- The way a call returns.
   --
   -- Undefined
   --    To be defined later. This is used for the context of a
   --    delayed call that is not yet realized as a call-step.
   -- To_Caller (normal non-tail call)
   --    When the callee returns, control passes to a return point in
   --    the caller. The return point is usually the next instruction
   --    in the caller's code after the calling sequence.
   -- To_Higher (tail call)
   --    When the callee returns, the caller also returns, using
   --    whatever kind of return the caller's caller has set up.
   --    This happens without executing any more instructions in
   --    the caller's code.
   -- To_Computed
   --    The address of the return point is computed in some dynamic
   --    way, and must be analysed (bounded) to show if it returns
   --    To_Caller or To_Higher.


   subtype Return_Defined_T is Return_Kind_T range To_Caller .. To_Computed;
   --
   -- Omitting the Undefined return.


   type Computed_Return_T is abstract tagged null record;
   --
   -- Some way of computing the return point of a call when the
   -- call-step and the callee are known, possibly using other
   -- properties of the call as known in derived types.
   --
   -- A computed return can be useful even if the return address
   -- is statically known; the computed return can associate the
   -- return edge with a non-zero execution time, or create several
   -- return edges with different conditions, targets and times.


   procedure Add_Return (
      From  : in Programs.Call_T;
      Via   : in Computed_Return_T;
      After : in Step_T;
      To    : in Graph_T)
   is abstract;
   --
   -- Adds edges (static or boundable) To the given flow-graph to
   -- model the return From the given call, Via the given computed
   -- return address. If the call turns out to be a tail call the
   -- operation perhaps adds no edges at all.
   --
   -- From
   --    The call that returns, with two cases:
   --
   --    > If From /= No_Call then this is a normal "reference"
   --      call and After = Step (From).
   --
   --    > If From = No_Call then the call is an "integrated" call
   --      and After is a return step within the integrated callee.
   -- 
   -- Via
   --    Defines the computation of the return point.
   --
   -- After
   --    The step in the caller's flow-graph that is the source
   --    of the return. For a "reference" call it is the synthetic
   --    call-step; for an "integrated" call it is a return step in
   --    the callee which is, however, integrated in the caller's
   --    flow-graph.
   --
   -- To
   --    The caller's flow-graph. After is in this graph.
   --
   -- Implementations should take care to terminate integrated calls
   -- properly when From = No_Call and some return edges are created.
   -- The integrated context should be popped from the Tags of the
   -- targets of the return edges.


   type Computed_Return_Ref is access all Computed_Return_T'Class;
   --
   -- Refers to some way of computing the return point of a call.


   type Return_T (Kind : Return_Kind_T := Undefined) is record
      case Kind is
      when Undefined   => null;
      when To_Caller   => Tag : Step_Tag_T;
      when To_Higher   => null;
      when To_Computed => Computed : Computed_Return_Ref;
      end case;
   end record;
   --
   -- The way a call returns, and the return point (Tag) in the
   -- caller's flow-graph, if control returns to the caller. 
   -- A computed return address is represented by a Computed_Return
   -- object that will Add the Return points in some way. If a
   -- dynamic call has a computed return then the return will be
   -- computed separately for each resolved callee.


   --
   ---   Integrated call tags
   --


   function Integration_Entry (
      Host   : Programs.Subprogram_T;
      From   : Step_T;
      Callee : Programs.Subprogram_T;
      Retur  : Return_T)
   return Step_Tag_T;
   --
   -- A step tag that represents the start of an integrated call
   -- From a given step within the flow-graph of the Host, to a
   -- Callee subprogram, with a specified Return point (which must
   -- not be Undefined).
   --
   -- The Host is not always the immediate caller of the Callee.
   -- There may be a chain of integrated calls starting in the Host
   -- and ending with an integrated call to the Callee so that all
   -- the subprograms in this chain are integrated in the Host's
   -- flow-graph.
   --
   -- If a loose edge is placed From the given step to this tag,
   -- it will activate integrated decoding of the Callee.


   function Integration_Level (Tag : Step_Tag_T) return Natural;
   --
   -- The integration level (number of nested integrated calls)
   -- in the context of the given Tag. Zero means that no call is
   -- being integrated; one means one calls, and so on.


   --
   ---   Detecting tail calls, optimized into jumps or other things
   --


   function Maybe_Tail_Call (
      From : Programs.Subprogram_T;
      To   : Processor.Code_Address_T)
   return Boolean;
   --
   -- Whether a non-call transfer of control (jump, branch, ...) From
   -- the given subprogram, To the given address, might be an optimized
   -- form of a tail call, that is, a call From this subprogram To the
   -- subprogram at the given address, encoded in such a way that return
   -- from the callee (at To) also returns From the caller, without
   -- executing any other part of the caller (From).
   --
   -- This is true if all the following conditions hold:
   --
   -- > The given address, To, is the entry address of some subprogram,
   --   as decided by Programs.Enters_Subprogram.
   --
   -- > The subprogram at To is not the same as the subprogram at From
   --   (otherwise, the tail-call would be directly recursive).
   --
   -- > The tail-call detection option (Opt.Detect_Tail_Calls) is
   --   enabled.


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
      Giving    :    out Programs.Call_T);
   --
   -- Creates a call From a step that represents the last instruction
   -- executed within the Caller subprogram before control transfers to
   -- a Target address that is the entry point of the callee subprogram.
   -- Adds the call To the caller's flow-graph.
   --
   -- Other inputs are the Condition and the execution Time for the edge
   -- From the last callee instruction to the call-step (or the entry
   -- step of the integrated callee), the Protocol for the call, the
   -- Call_Info and the Step_Info for the call-step (if one is created),
   -- and the Return point, which can be in the Caller's flow-graph
   -- (To_Caller) or in a higher-level caller (To_Higher) or defined by
   -- a dynamic computation (To_Computed). It must not be Undefined.
   --
   -- The Form parameter can force a certain form of analysis or
   -- decoding of the call. The default value (Any_Form) is neutral;
   -- the choice then depends on other factors (properties of the
   -- callee subprogram).
   --
   -- Normally the call is analysed as a reference to the callee. This
   -- means that the only new step is a "call step" that is added To the
   -- Caller's flow graph. This call-step is a "proxy" for the execution
   -- of the callee that is invoked by this call. A Call_T is also created
   -- to record this link between the Caller and the callee subprograms.
   -- After the callee has been analysed and bounded (perhaps using
   -- information from this calling context) the arithmetic effect,
   -- execution time and stack usage of the callee become properties
   -- of the call-step (or of the execution bounds associated with the
   -- call-step).
   --
   -- For such a normal call, decoded as a reference to the callee:
   --
   -- To
   --    Is extended with a new call-step and an edge From the
   --    call-instruction step to the call-step, with the given
   --    Condition and Time.
   --    If the Return is To_Caller an unconditional, zero-time edge from
   --    the call-step to the Return point is created, unless the callee
   --    is known not to return.
   --    If the Return is To_Higher no such return edge is created whether
   --    or not the callee returns. Instead, the call-step is marked as
   --    a return point by calling Return_After.(Thus, the call appears
   --    to return even if the callee does not -- TBD remedy.) 
   --    If the Return is To_Computed we Add the Return via Return.Computed
   --    which usually creates a boundable return edge.
   -- Cond
   --    The precondition for the edge From the call-instruction step
   --    to the call-step.
   -- Time
   --    The execution time for the edge From the call-instruction step
   --    to the call-step. This time can model some additional delays
   --    due to the transfer of control, for example pipeline reloading.
   -- Protocol
   --    Defines the protocol for the call.
   -- Call_Info
   --    Gives the processor-specific information derived from this
   --    call, to initialize the callee subprogram info if this is
   --    a new subprogram, or update the callee subprogram info if
   --    this subprogram already exists.
   -- Step_Info
   --    Gives the processor-specific information for the call-step.
   -- Giving
   --    Is returned as the new Call_T. The new call-step is
   --    available via Programs.Step (Giving). The Giving call is
   --    also visible as one of the calls from the Caller.
   --
   -- If the callee subprogram is marked to be decoded "integrated" in
   -- the calleer's flow-graph, and Form is not Reference_Form, the call
   -- is analysed in a different way: the entire callee subprogram is
   -- decoded (again), in the context of this call, and the resulting
   -- steps and edges are added To the Caller's flow-graph (with a
   -- context-tag that separates them from other integrated decodings of
   -- the same callee in the same Caller).
   --
   -- The integrated decoding is initiated by adding a loose edge To the
   -- caller's flow-graph, From the call-instruction to the Target address
   -- which is also the entry address of the callee, but in a new
   -- "integrated" step-tag context. The decoder will follow this loose
   -- edge and (re-) decode the callee. A call-step is not created; its
   -- place is taken by the steps and edges that model the instructions
   -- in the callee. These steps and edges are analysed as a part of the
   -- Caller, just as if the compiler had in-lined the code of the callee
   -- in the code of the Caller. No Call_T is created.
   --
   -- For a call decoded in this "integrated" way:
   --
   -- To
   --    Is extended with a loose edge From the call instruction step
   --    to the Target, tagged with an "integrated" context, and taking
   --    the given Time. The "integrated" context records the Return point
   --    for later use in Return_After. In fact, the operation
   --    Decoder.Integrate_Call is invoked to add this loose edge and/or
   --    perform some processor-specific actions. For example, this
   --    operation may decide to add a boundable edge, instead of a
   --    static loose edge, to resolve some aspect of the call.
   -- Cond
   --    The precondition for the loose edge From the call-instruction
   --    step to the Target state.
   -- Time
   --    The execution time for the loose edge From the call-instruction
   --    step to Target state. This time can model some additional delays
   --    due to the transfer of control, for example pipeline reloading.
   -- Protocol
   --    Is irrelevant.
   -- Call_Info
   --    Is irrelevant.
   -- Step_Info
   --    Is irrelevant.
   -- Giving
   --    Is returned as No_Call.


   function Delayed_Call_Root (
      After  : Step_Tag_T;
      Target : Processor.Code_Address_T)
   return Step_Tag_T;
   --
   -- Creates the context for a delayed call, to a given Target address
   -- (the callee entry point), that occurs at some point After a given
   -- state. Returns a tag that has the same flow-state and data-state
   -- as After, but a new context defining the delayed call.
   --
   -- To create the delayed paths, first apply Transit to the returned
   -- tag, to advance the flow-state, then Add_Edges from the After step.
   -- To end the delayed paths, use Add_Delayed_Call.
   --
   -- Each use of Delayed_Call_Root creates a new call context and
   -- therefore (later, in Add_Delayed_Call) a new call-step and a
   -- new Call_T.


   function Abandon_Delayed_Call (Tag : Step_Tag_T) return Step_Tag_T;
   --
   -- Given a Tag on the delay path to a delayed call, this function
   -- returns the Tag without the context of the delayed call. The
   -- returned Tag, or other tags derived from it, cannot be used in
   -- Add_Delayed_Call. This function is meant for use when the
   -- processor can decide, at some point in the delay path, not to
   -- execute the delayed call, after all, so that a (branch from a)
   -- delay path becomes a normal path, not a delay path. It may be
   -- necessary to change the flow-state, too, by applying Transit
   -- to the result from this function.


   procedure Add_Delayed_Call (
      To        : in     Graph_T;
      After     : in     Loose_Edge_T;
      Caller    : in     Programs.Subprogram_T;
      Protocol  : in     Calling.Protocol_Ref;
      Call_Info : in     Processor.Program.Sub_Info_T;
      Step_Info : in     Processor.Step_Info_T;
      Form      : in     Call_Form_T := Any_Form;
      Retur     : in     Return_T;
      Giving    :    out Programs.Call_T);
   --
   -- Ends a delay path for a delayed call (initiated by an earlier
   -- construction of the Delayed_Call_Root) by creating the call-step
   -- and the call. The call step is created After the given loose edge,
   -- which is thus bound to the call-step. The other parameters have
   -- the same roles as in the Add_Call operation.
   --
   -- When Return is To_Caller, the Return.Tag may have the context
   -- for the delayed call (be derived from the Delayed_Call_Root).
   -- If so, the context is removed when the return edge is created.
   -- When Return is To_Higher, the call-step is marked as a return
   -- point by calling Return_After, as in Add_Call.
   --
   -- When there are several delayed paths to the same delayed call,
   -- the first path that ends (the first use of Add_Delayed_Call)
   -- creates the call-step and the call. If other delayed paths
   -- already end with loose edges with the same target (After.Target)
   -- they are automatically bound to the same call-step and thus the
   -- same Protocol, Call_info, etc. Likewise, if other delayed paths
   -- are later created and/or extended to the same target, they also
   -- are bound automatically to the same call-step and call.
   --
   -- TBA integrated delayed calls, boundable delayed calls.


   procedure Add_Boundable_Call (
      To     : in     Graph_T;
      From   : in     Step_T;
      Caller : in     Programs.Subprogram_T;
      Target : in     Dynamic_Edge_T;
      Effect : in     Arithmetic.Effect_Ref;
      Info   : in     Processor.Step_Info_T;
      Retur  : in     Return_T;
      Giving :    out Step_T);
   --
   -- Creates a call From a step that represents the call instruction
   -- within the Caller subprogram to a Target address that is computed
   -- dynamically and represented as a boundable edge.
   --
   -- Adds an unresolved call-step To the caller's flow-graph, with an
   -- edge From the call instruction to the unresolved call-step and an
   -- edge from the unresolved call-step to the Return point (for a
   -- call that returns To_Caller or To_Computed). These edges are
   -- unconditional and take zero time. The Return must not be Undefined.
   -- If the Return is To_Higher, the unresolved call-step is marked as
   -- a return point by means of Return_After.
   --
   -- Adds the Target as a boundable edge From the call instruction step.
   -- Resolving the Target edge may later resolve the unresolved call-step,
   -- place a time and a condition on the edge to the call-step, and add
   -- more call-steps between From and Return, through calls of
   -- Add_Resolved_Call.
   --
   -- The role of the Target edge must initially be Undefined; the role
   -- is here changed to Boundable_Call.
   --
   -- The Effect parameter defines the arithmetic effect of the unresolved
   -- call-step. This should be an upper bound on the effect of any
   -- possible callee. The Info parameter defines the processor-specific
   -- information for the unresolved call-step.
   --
   -- Returns the unresolved call-step in Giving.


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
      Giving    :    out Programs.Call_T);
   --
   -- Analogous to the procedure Add_Call, but here the Source is a
   -- boundable edge (representing a dynamically computed callee) and
   -- the Target address has been resolved from the Source by bounding
   -- the possible callees. All the other parameters with the same names
   -- as in Add_Call have the same roles as in Add_Call. However, unlike
   -- Add_Call, the call must be decoded in the normal way by reference
   -- (creating a Call_T that is returned in the Giving parameter). It
   -- is not possible to integrate the callee To the Caller's flow graph.
   --
   -- Assumes that the Source edge was added To the graph by means of
   -- the operation Add_Boundable_Call.
   --
   -- The Cond parameter (which does not occur in Add_Call) gives the
   -- precondition for the edge from the Source (or its source step) to
   -- the call-step.
   --
   -- The call instruction (or, to be precise, the last instruction executed
   -- in the caller before control transfers to the callee) is represented
   -- by the source-step of the Source edge. It is a precondition that all
   -- the immediate successor steps of this source-step are call-steps;
   -- that there must be at least one successor; and that at most one of
   -- the successors is an unresolved call-step (as created by
   -- Add_Boundable_Call). The successor call-steps may be connected to
   -- the same return point or to different return points (if the
   -- return is To_Computed).
   --
   -- If there is an unresolved call-step, Add_Resolved_Call updates
   -- (resolves) it to represent the call to this Target, with this
   -- Step_Info, and updates the edge from the source-step to this call-step
   -- to have the given Cond and Time. Otherwise (no unresolved call-step),
   -- Add_Resolved_Call creates a new call-step and new edges for this
   -- Target, Step_Info, Cond and Time.
   --
   -- If the resolved call returns To_Computed, or if a new call-step
   -- is created, Add_Resolved_Call also creates the return edges,
   -- using the Add_Return operation for return To_Computed.
   -- If a new call-step is created and the resolved call returns
   -- To_Higher, the new call-step is marked as a return point by
   -- means of Return_After.
   --
   -- The number of resolvents of the Source edge is always incremented
   -- and the Source edge is always marked as Growing.
   --
   -- The precondition stated above also holds as a postcondition, but
   -- there will be no unresolved call-steps among the successors of the
   -- source-step of the Source edge.


   function Is_Call (Step : Step_T) return Boolean;
   --
   -- Whether this step is a (synthetic) call-step that represents
   -- a normal (not integrated) call to another (callee) subprogram.
   -- Both resolved and unresolved call-steps are included.
   -- Steps on the delay paths leading to delayed calls are not
   -- considered call-steps.


   function Is_Call_Delay (Step : Step_T) return Boolean;
   --
   -- Whether this step is a normal step (not a call-step) on the
   -- delay path leading to a delayed call (for which the call-step
   -- may or may not exist yet).


   function Is_Resolved_Call (Step : Step_T) return Boolean;
   --
   -- Same as Is_Call but excludes unresolved calls. Thus the call
   -- in the Step is not No_Call and the callee is statically known. 


   function Is_Call (Node : Node_T) return Boolean;
   --
   -- Whether this node holds (only) a (synthetic) call-step.
   -- Both resolved and unresolved call-steps are included.


   function Is_Resolved_Call (Node : Node_T) return Boolean;
   --
   -- Same as Is_Call but excludes unresolved calls. Thus the call
   -- in the Node is not No_Call and the callee is statically known. 


   function Is_Unresolved_Call (Step : Step_T) return Boolean;
   --
   -- Whether this step is a (synthetic) call-step that represents
   -- a normal (not integrated) call to an as yet unknown callee
   -- subprogram, that is, an unresolved call-step.


   function Is_Unresolved_Call (Node : Node_T) return Boolean;
   --
   -- Whether this node holds only a (synthetic) call-step that
   -- represents a normal (not integrated) call to an as yet
   -- unknown callee subprogram, that is, an unresolved call-step.


   function Unresolved_Calls (Within : Graph_T)
   return Dynamic_Edge_List_T;
   --
   -- All the unresolved calls Within the graph.


   function Unresolved_Calls (Under : Computation.Model_Ref)
   return Dynamic_Edge_List_T;
   --
   -- All the unresolved calls within a flow-graph that are
   -- feasible Under a given computation model.


   function Call_In (Step : Step_T) return Programs.Call_T;
   --
   -- The call in this Step, if Is_Call (Step), or No_Call otherwise.
   -- No_Call is returned also for an unresolved call-step.


   function Call_In (Node : Node_T) return Programs.Call_T;
   --
   -- The call in this Node, if Is_Call (Node), or No_Call otherwise.
   -- No_Call is returned also for an unresolved call-step.


   function Callee_Address (Tag : Step_Tag_T)
   return Processor.Code_Address_T;
   --
   -- The entry address of the callee, assuming that the Tag is
   -- the tag of a call-step or a tag on the delay path for a
   -- delayed call (this includes the tag from Delayed_Call_Root).
   -- Otherwise the function propagates Constraint_Error.


   procedure Return_After (
      Step   : in Step_T;
      Within : in Graph_T);
   --
   -- Marks the Step as a return step, that is, the execution of the
   -- current subprogram ends here and control returns to the caller.
   -- The Step must not have any out-edges; this is checked.
   --
   -- The effect of this operation depends on whether the Step is a
   -- part of an "integrated" call, as shown by Tag(Step).Context.
   --
   -- If the Step is not an integrated step, this operation has no
   -- effect (except for checks).
   --
   -- If the Step is an integrated step, this operation applies the
   -- return definition of the integrated call, taking the return
   -- definition from Tag(Step).Context, as follows:
   --
   -- > For a return To_Caller the operation adds a zero-time loose
   --   edge from the Step to the return point of the integrated call,
   --   Within the given graph (which is the caller's flow graph).
   --   In fact, the operation Decoder.Integrate_Return is invoked to
   --   add this loose edge and/or perform some processor-specific
   --   actions.
   --
   -- > For a return To_Higher no such edge is added, which means that
   --   the Step becomes (or should become) a return step Within the
   --   caller's flow graph.
   --
   -- > For a return To_Computed the operation uses Add_Return on
   --   the Computed_Return_Ref.
   --
   -- > In any case, this should terminate the integrated decoding
   --   because the return point is not (or should not be) tagged
   --   with this integrated context.


   --
   ---   Arithmetic effects of a call on the caller's computation
   --


   function Output_Cells (
      Call      : Programs.Call_T;
      Protocol  : Calling.Protocol_T'Class;
      Outputs   : Storage.Cell_List_T;
      Invariant : Storage.Cell_List_T)
   return Storage.Cell_List_T;
   --
   -- The caller cells that are assigned by a Call, under a given
   -- calling Protocol, when the callee's Outputs are known, possibly
   -- constrained by assertions that some caller cells are Invariant
   -- and by some callee's Outputs being private (Privy) to the callee
   -- and invisible in the caller.
   --
   -- Call
   --    The call for which the output cells are computed.
   --
   -- Protocol
   --    The calling protocol that provides a map from caller cells
   --    (as used in the Outputs) to callee cells (as used in the
   --    resulting effect).
   --
   -- Outputs
   --    The output cells from the callee, in the callee's frame.
   --    If the callee is not a stub and calls no stubs, these are all
   --    the cells that may be assigned in the callee's execution (except
   --    for aliases TBA and unresolved dynamic references).
   --
   --    Some of these cells may be private (Privy) to the callee, thus
   --    the callee's assignments to those cells have no effect on any
   --    cells in the caller and they contribute nothing to the result.
   --    The case of net change in local stack height is not considered
   --    here (see Programs.Execution.Final_Stack_Effect).
   --
   --    If the callee is a stub or calls some stub, other cells may
   --    also be assigned but this is not considered here (see the
   --    procedure Add_Unknown_Call_Effect, below).
   --
   -- Invariant
   --    Caller cells that are known (asserted) to be invariant (not
   --    affected) by the Call, even if the callee assigns values to
   --    these cells. The result will not contain any of these cells.


   procedure Add_Call_Effect (
      Call      : in     Programs.Call_T;
      Protocol  : in     Calling.Protocol_T'Class;
      Outputs   : in     Storage.Cell_List_T;
      Invariant : in     Storage.Cell_List_T;
      To        : in out Arithmetic.Assignment_Set_T);
   --
   -- Adds the effect of a Call, as derived from the known Outputs of the
   -- Call but constrained by asserting some caller cells as Invariant
   -- across the Call, To a set of assignments, which will form part of
   -- the total effect of the Call in the caller's computation model.
   -- The effect simply veils (makes unknown, opaque) all the Output_Cells
   -- of the call, except for cells that are private (Privy) to the
   -- callee (for example, cells that model local stack height).
   --
   -- The parameters have the same meaning as in the function Output_Cells.
   --
   -- The returned effect (in To) is stated in the caller's frame. In
   -- other words, the calling Protocol is used to map the effect of the
   -- callee from the callee's frame to the caller's frame before the
   -- assignments are added To the effect.
   --
   -- The effect may include assignments to cells that are asserted
   -- as invariant for the specific Call (but not to cells marked as
   -- invariant in the callee). These assignments can be removed later
   -- with Spare_Call_Invariants (below). The reason for this two-step
   -- process is that the Effect of the Call may be necessary for
   -- mapping the invariance assertions to the Call.


   procedure Add_Unknown_Call_Effect (
      Call      : in     Programs.Call_T;
      Protocol  : in     Calling.Protocol_T'Class;
      Stub      : in     Calling.Stub_Level_T;
      Upon      : in     Storage.Cell_List_T;
      To        : in out Arithmetic.Assignment_Set_T);
   --
   -- Adds the effect, Upon some caller cells, of a Call with a completely
   -- or partially unknown effect (as shown by the Stub level which is not
   -- Calls_No_Stub), To a set of assignments, which will form part of the
   -- total effect of the Call in the caller's computation model. The
   -- effect simply veils (makes unknown, opaque) those Upon cells that
   -- are not known to be invariant under this Protocol and Stub level
   -- and are not already assigned in the assignment set.


   procedure Spare_Call_Invariants (
      Call      : in     Programs.Call_T;
      Invariant : in     Storage.Cell_Set_T;
      Within    : in out Computation.Model_Ref);
   --
   -- Reduces the Call's effect, Within a given computation model,
   -- by removing the assignments to those caller cells that are
   -- asserted as Invariant in the call.


   type Parameter_Association_T is record
      Caller : Storage.Cell_T;
      Callee : Storage.Cell_T;
   end record;
   --
   -- Represents the association (mapping) of a cell in the caller's
   -- frame (Caller) that supplies an actual parameter value, and
   -- the input parameter cell in the callee's frame (Callee) that
   -- receives this value on entry to the callee, or vice versa for
   -- output parameters where the Callee cell supplies a value for
   -- the Caller cell on return from the callee.


   type Parameter_Map_T is
      array (Positive range <>) of Parameter_Association_T;
   --
   -- A list of parameter associations. The call to which this list
   -- applies must be known from context. Whether the list holds all
   -- the parameters, or just the inputs or just the outputs, for
   -- example, depends on context and usage.


   function Input_Parameters (
      Inputs   : Storage.Cell_List_T;
      Protocol : Calling.Protocol_T'Class)
   return Parameter_Map_T;
   --
   -- The input parameters of a call with given Input cells in the
   -- callee's frame, as far as they have been resolved and mapped
   -- to the caller's frame via the given Protocol.
   --
   -- When possible, only cells still relevant to bounding the callee
   -- are returned (the remaining input cells are suppressed).


end Flow.Calls;
