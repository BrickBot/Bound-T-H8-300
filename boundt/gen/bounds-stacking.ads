-- Bounds.Stacking (decl)
--
-- Bounding the stack usage.
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
-- $Revision: 1.5 $
-- $Date: 2015/10/24 19:36:47 $
--
-- $Log: bounds-stacking.ads,v $
-- Revision 1.5  2015/10/24 19:36:47  niklas
-- Moved to free licence.
--
-- Revision 1.4  2007-12-17 13:54:34  niklas
-- BT-CH-0098: Assertions on stack usage and final stack height, etc.
--
-- Revision 1.3  2005/10/20 11:28:28  niklas
-- BT-CH-0015.
--
-- Revision 1.2  2005/02/23 09:05:14  niklas
-- BT-CH-0005.
--
-- Revision 1.1  2005/02/16 21:11:39  niklas
-- BT-CH-0002.
--


with Assertions;
with Calculator;
with Flow;
with Programs;
with Programs.Execution;


package Bounds.Stacking is


   procedure Apply_Assertions (
      Asserts     : in Assertions.Assertion_Set_T;
      Exec_Bounds : in Programs.Execution.Bounds_Ref);
   --
   -- Enters asserted bounds from on the stack behaviour from the
   -- given set of Asserts into the given Execution Bounds.


   procedure Bound_Local_Stack_Height (
      Stack_Steps      : in Flow.Step_List_T;
      Into_Stack_Steps : in Calculator.Pool_List_T;
      Exec_Bounds      : in Programs.Execution.Bounds_Ref);
   --
   -- Bounds the local stack-heights, for all stacks, using the computed
   -- data-pool into each step that changes a stack-height cell. However,
   -- the local stack-height for some stack(s) may already be bounded (from
   -- constant propagation), in which case we do nothing for that stack.
   -- If the local stack-height is only partly (unsafely) bounded already,
   -- we try to improve the bounding using arithmetic analysis.
   -- Moreover, if the final stack-height is not yet known for some stack,
   -- we assume that all final (return) steps are included in Stack_Steps
   -- and we try to bound the final stack-height, too.
   --
   -- Stack_Steps
   --    The steps that change some stack-height cell. Note that a given
   --    step may not change all stack-height cells. Also includes all
   --    final (return) steps if some stack is yet without bounds on
   --    the final stack height.
   -- Into_Stack_Steps
   --    Calculcated data-pool into each Stack_Step.
   -- Exec_Bounds
   --    Execution bounds derived so far for this subprogram.
   --    Will be updated with execution bounds for local stack height
   --    that we compute here.
   --
   -- As a side effect, we may discover that some of the Stack_Steps
   -- are infeasible under the computation model of Exec_Bounds. If so,
   -- they are marked as infeasible in the model and the model is pruned.
   --
   -- The index ranges of Stack_Steps and Into_Stack_Steps must be the
   -- same. It is assumed that Into_Stack_Steps(I) is the into-flux of
   -- Stack_Steps(I).


   procedure Bound_Take_Off_Height (
      Calls       : in Programs.Call_List_T;
      Into_Calls  : in Calculator.Pool_List_T;
      Exec_Bounds : in Programs.Execution.Bounds_Ref);
   --
   -- Bounds the take-off stack-height of the listed calls, for all
   -- stacks, using the computed data-pool into the call steps.
   --
   -- Calls
   --    Calls with unbounded take-off height (for some stacks).
   --    We plan to bound these.
   -- Into_Calls
   --    Data-pool into each of the Calls.
   -- Exec_Bounds
   --    The execution bounds derived and under construction for
   --    the caller subprogram, to be updated with new or better
   --    take-off-height bounds for the Calls.
   --
   -- As a side effect, we may discover that some of the Calls (call steps)
   -- are infeasible under the computation model of Exec_Bounds. If so,
   -- they are marked as infeasible in the model and the model is pruned.
   --
   -- The index ranges of Calls and Into_Calls must be the same.
   -- It is assumed that Into_Calls(I) is the data-pool into Calls(I).


   procedure Compute_Stack_Usage (Within : in Programs.Execution.Bounds_Ref);
   --
   -- Computes the maximum stack usage Within the given bounds, for
   -- all stacks in the processor (model), by joining the maximum local
   -- stack height inside the subprogram covered by the bounds, the
   -- take-off heights for all the calls from this subprogram, and the
   -- maximum stack usage of the callees.
   --
   -- Bounds for maximum local stack height and take-off heights must
   -- already be available Within the bounds for the whole call-tree.
   -- The result is used to Bound_Stack_Usage (Within).


end Bounds.Stacking;
