-- Bounds.Calling (decl)
--
-- Bounding subprogram calls possibly using parameter bounds collected
-- along specific call-paths. Calls can also be bounded by assertions.
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
-- $Revision: 1.9 $
-- $Date: 2015/10/24 19:36:47 $
--
-- $Log: bounds-calling.ads,v $
-- Revision 1.9  2015/10/24 19:36:47  niklas
-- Moved to free licence.
--
-- Revision 1.8  2008-02-27 14:58:48  niklas
-- BT-CH-0116: Call-specific time and stack assertions.
--
-- Revision 1.7  2007/01/13 13:51:02  niklas
-- BT-CH-0041.
--
-- Revision 1.6  2006/11/26 22:07:24  niklas
-- BT-CH-0039.
--
-- Revision 1.5  2006/05/27 21:46:38  niklas
-- Updated for BT-CH-0020.
--
-- Revision 1.4  2005/10/20 11:28:28  niklas
-- BT-CH-0015.
--
-- Revision 1.3  2005/07/01 10:59:31  niklas
-- Noted that Bound_Protocol may propagate Flow.False_Path.
--
-- Revision 1.2  2005/02/23 09:05:14  niklas
-- BT-CH-0005.
--
-- Revision 1.1  2005/02/16 21:11:38  niklas
-- BT-CH-0002.
--


with Assertions;
with Calculator;
with Flow.Computation;
with Programs;
with Programs.Execution;
with Storage;
with Storage.Bounds;


package Bounds.Calling is


   procedure Bound_Protocol (
      Call    : in     Programs.Call_T;
      Data    : in     Storage.Bounds.Bounds_T'Class;
      Model   : in out Flow.Computation.Model_Ref;
      Bounded :    out Boolean);
   --
   -- Tries to bound (resolve) the calling protocol in the given Call
   -- by using the given Data bounds on the cell values at the call-step.
   --
   -- If the protocol is constrained or bounded, the computation Model is
   -- updated with the new protocol for the Call and Bounded is returned
   -- as True. Note that under the new protocol the effect of the Call
   -- may be different and thus should also be updated in the Model but
   -- this procedure does not do so; the caller should observe Bounded
   -- and should arrange for the Call's effect to be updated.
   --
   -- If the protocol was already Static, or if the Data bounds did not
   -- lead to a more constrained protocol, the computation model is not
   -- modified and Bounded is returned as False.
   --
   -- May propagate the False_Path exception.
   --
   -- TBD if this could be strengthened by including the bounds on inputs
   -- for the call, as recorded within the caller's execution bounds.


   procedure Bound_Asserted_Calls (
      Calls         : in Programs.Call_List_T;
      Assert_Map    : in Assertions.Assertion_Map_T;
      Caller_Bounds : in Programs.Execution.Bounds_Ref;
      Bounds_Set    : in Programs.Execution.Bounds_Set_T);
   --
   -- Applies call-specific assertions on execution time, stack usage,
   -- and execution count, to the Calls within given Caller_Bounds.
   --
   -- Also applies execution-count assertions on calls, whether or
   -- not the execution time is asserted or computed for the call.
   -- If the asserted execution-count is zero, the call (step) is
   -- marked as infeasible in the computation model for Caller_Bounds
   -- and the model is Pruned.
   --
   -- Also applies assertions on call-invariant cells to the effect
   -- of the call (the effect of the call-step) in the caller's
   -- computation model in Caller_Bounds.
   --
   -- Calls
   --    The calls to which assertions will be applied.
   -- Assert_Map
   --    Assertion map for the caller, connecting assertions with
   --    some of the Calls.
   -- Caller_Bounds
   --    Execution bounds for the caller, to be extended.
   -- Bounds_Set
   --    Set of execution bounds computed earlier, to be extended.
   --    Does not yet include Caller_Bounds.
   --
   -- The Caller_Bounds and Bounds_Set are updated as follows,
   -- treating each of the Calls separately:
   --
   -- > If the worst-case execution time, stack usage, or final stack
   --   height for a call is asserted, a copy of the inherited execution
   --   bounds for this call is created, enhanced with the asserted
   --   information, and inserted in the Caller_Bounds to bound the
   --   time and/or stack for this call. However, assertions on time
   --   are ignored if time is not analysed, and assertions on stack
   --   usage are ignored if stack usage is not analysed. If the call
   --   is then fully bounded, the new execution bounds are given to
   --   Finish_Bounds for inclusion in the Bounds_Set. If the call is
   --   not yet fully bounded, the new execution bounds are not stored
   --   in Bounds_Set and therefore remain unfrozen, to be completed
   --   by analysis (see Bound_Call, below).
   --
   -- > If the execution count for one of the calls is asserted,
   --   this bound on the execution count of the call-node is inserted
   --   in the Caller_Bounds.
   --
   -- > If some invariant cells for one of the calls are asserted,
   --   the effect of the call is correspondingly limited in the
   --   computation model within Caller_Bounds.
   --
   -- The Caller_Bounds are not changed in any other way; bounds
   -- for the non-asserted calls are left as they are.


   procedure Bound_Call (
      Call          : in Programs.Call_T;
      Caller_Bounds : in Programs.Execution.Bounds_Ref;
      Data          : in Calculator.Pool_T;
      Inherit_Inv   : in Storage.Cell_Set_T;
      Asserts       : in Assertions.Assertion_Set_T;
      Assert_Map    : in Assertions.Assertion_Map_T;
      Bounds_Set    : in Programs.Execution.Bounds_Set_T);
   --
   -- Tries to bound a call, that has not been fully bounded before,
   -- using Data context from the arithmetic analysis of the caller.
   --
   -- It is assumed that Bound_Asserted_Calls has been used before
   -- this procedure, but the assertions did not bound the call.
   --
   -- Call
   --    The call to be bounded.
   -- Caller_Bounds
   --    The context in which the Call should be analysed, represented
   --    as the execution bounds on the caller.
   -- Data
   --    Data-pool into the call, in terms of caller cells, including
   --    all assertions for the caller, but not including call-specific
   --    assertions on input values in terms of callee cells.
   -- Inherit_Inv
   --    Inherited invariant cells in the caller's view.
   -- Asserts
   --    Assertion set. Relevant assertions may apply to this Call or
   --    to Callee(Call) or deeper callees.
   -- Assert_Map
   --    Assertion map for Asserts and the caller, Caller(Call).
   --    Used to locate Call-specific assertions.
   -- Program
   --    The target program under analysis.
   -- Bounds_Set
   --    The set of execution bounds under construction.
   --
   -- If the Data or assertions define values for inputs to the Call,
   -- the callee is reanalysed under these new constraints (new context)
   -- by calling Bounds.Bound_Execution. Otherwise, the existing (not
   -- fully bounded) bounds are left in place in Caller_Bounds.
   --
   -- If the Call appears to be infeasible (because the calculation of
   -- parameter bounds returns a contradiction), the call-step is marked
   -- infeasible in the caller's computation model and the model is
   -- pruned.
   --
   -- Precondition: The output locus should refer to the Call under
   -- this Caller_Path.


   procedure Bound_Call (
      Call          : in Programs.Call_T;
      Caller_Bounds : in Programs.Execution.Bounds_Ref;
      Data          : in Storage.Bounds.Cell_Interval_List_T;
      Inherit_Inv   : in Storage.Cell_Set_T;
      Asserts       : in Assertions.Assertion_Set_T;
      Assert_Map    : in Assertions.Assertion_Map_T;
      Bounds_Set    : in Programs.Execution.Bounds_Set_T);
   --
   -- Tries to bound a call, that has not been fully bounded before,
   -- using only asserted variable bounds at the call site (no
   -- arithmetic analysis of the caller).
   --
   -- It is assumed that Bound_Asserted_Calls has been used before
   -- this procedure, but the assertions did not bound the call.
   --
   -- Call
   --    The call to be bounded.
   -- Caller_Bounds
   --    The context in which the Call should be analysed, represented
   --    as the execution bounds on the caller.
   -- Data
   --    All variable-value assertions applicable to the Call, including
   --    assertions for the subprogram (caller) that contains the Call,
   --    for the loops that contain the Call and the Call itself.
   --    Global assertions need not be included.
   -- Inherit_Inv
   --    Inherited invariant cells in the caller's view.
   -- Asserts
   --    Assertion set. Relevant assertions may apply to this Call or
   --    to Callee(Call) or deeper callees.
   -- Assert_Map
   --    Assertion map for Asserts and the caller, Caller(Call).
   --    Used to locate Call-specific assertions.
   -- Bounds_Set
   --    The set of execution bounds under construction.
   --
   -- If the Data or assertions define values for inputs to the Call,
   -- the callee is reanalysed under these new constraints (new context)
   -- by calling Bounds.Bound_Execution. Otherwise, the existing (not
   -- fully bounded) bounds are left in place in Caller_Bounds.
   --
   -- If the Call appears to be infeasible (because the calculation of
   -- parameter bounds returns a contradiction), the call-step is marked
   -- infeasible in the caller's computation model and the model is
   -- pruned.
   --
   -- Precondition: The output locus should refer to the Call under
   -- this Caller_Path.


end Bounds.Calling;
