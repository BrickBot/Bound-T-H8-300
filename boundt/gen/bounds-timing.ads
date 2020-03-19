-- Bounds.Timing (decl)
--
-- Bounding the execution time.
--
-- This package provides operations specific to bounds on the
-- execution time of parts of the target program.
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
-- $Revision: 1.2 $
-- $Date: 2015/10/24 19:36:47 $
--
-- $Log: bounds-timing.ads,v $
-- Revision 1.2  2015/10/24 19:36:47  niklas
-- Moved to free licence.
--
-- Revision 1.1  2007-12-17 13:54:35  niklas
-- BT-CH-0098: Assertions on stack usage and final stack height, etc.
--


with Assertions;
with Programs.Execution;


package Bounds.Timing is


   procedure Apply_Assertions (
      Asserts     : in Assertions.Assertion_Set_T;
      Exec_Bounds : in Programs.Execution.Bounds_Ref);
   --
   -- Enters asserted bounds from on the execution time from the
   -- given set of Asserts into the given Execution Bounds.


   procedure Evaluate_Time_State (Within : in Programs.Execution.Bounds_Ref);
   --
   -- Evaluates the state of bounding of the maximum execution time
   -- Within the given bounds, making use of all the available bounds
   -- on loops, callees, and other features of the subprogram Within
   -- these bounds, assuming all such bounds to be already represented
   -- Within the given execution bounds on the subprogram.
   --
   -- Various cases:
   --
   -- > If time-bounds are asserted, does nothing.
   --
   -- > If there is no feasible execution path in the subprogram under
   --   this computation model, sets the time-state to Infeasible.
   --
   -- > If there are feasible calls with Vague time-state, sets the
   --   time-state to Vague here (for the caller), too.
   --
   -- > If the flow-graph is irreducible and there are not "enough"
   --   other assertions on execution counts to bound the execution
   --   paths, sets the time-state to Vague.
   --
   -- > If there are unbounded loops or calls with unbounded
   --   context-dependent execution time, sets the time-state
   --   to Depends.
   --
   -- > Otherwise sets the time-state to Computable. But note that
   --   the worst-case path and upper time bound are not yet
   --   computed here.


end Bounds.Timing;
