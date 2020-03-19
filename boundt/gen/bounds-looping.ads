-- Bounds.Looping (decl)
--
-- Bounding the number of loop repetitions by finding loop-counter cells
-- and bounding their initial value, final value and step. Loops can
-- also be bounded by assertions.
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
-- $Date: 2015/10/24 19:36:47 $
--
-- $Log: bounds-looping.ads,v $
-- Revision 1.6  2015/10/24 19:36:47  niklas
-- Moved to free licence.
--
-- Revision 1.5  2009-10-07 19:26:09  niklas
-- BT-CH-0183: Cell-sets are a tagged-type class.
--
-- Revision 1.4  2008-09-24 08:38:51  niklas
-- BT-CH-0146: Assertions on "loop starts <bound> times".
-- BT-CH-0146: Loop-repeat assertions set both lower and upper bound.
-- BT-CH-0146: Report locations of contradictory "count" assertions.
-- BT-CH-0146: Contradictory "count" assertions imply infeasibility.
--
-- Revision 1.3  2005/10/20 11:28:28  niklas
-- BT-CH-0015.
--
-- Revision 1.2  2005/09/20 11:03:47  niklas
-- Changed the type of the Luups parameter for Bound_Asserted_Loops
-- from Loops_T to Loop_List_T so that the operation can be applied
-- to a subset of all the loops in a subprogram. This is required
-- for compatibility with rev 1.62 of bounds.adb.
--
-- Revision 1.1  2005/02/16 21:11:38  niklas
-- BT-CH-0002.
--


with Assertions;
with Calculator;
with Flow;
with Loops;
with Programs.Execution;
with Storage;


package Bounds.Looping is


   procedure Bound_Asserted_Starts (
      Luups       : in Loops.Loop_List_T;
      Asserts     : in Assertions.Assertion_Map_T;
      Exec_Bounds : in Programs.Execution.Bounds_Ref);
   --
   -- Bounds the number of starts for Luups for which the user has
   -- Asserted start bounds.
   --
   -- Luups
   --    A set of loops in a subprogram.
   --    This does not have to contain all the loops in the subprogram.
   -- Asserts
   --    The assertions, mapped to this subprogram.
   -- Exec_Bounds
   --    The execution bounds under construction for this subprogram.
   --    Among other things, these bounds define the computation model.
   --
   -- The asserted loop-start bounds are placed in the execution bounds.
   -- Normally, no other part of the execution bounds is updated.
   -- However, if a loop is asserted to start zero times then the
   -- loop-head is marked as infeasible in the computation model for
   -- the execution bounds. The model is then pruned.


   procedure Bound_Asserted_Repeats (
      Luups       : in Loops.Loop_List_T;
      Asserts     : in Assertions.Assertion_Map_T;
      Exec_Bounds : in Programs.Execution.Bounds_Ref);
   --
   -- Bounds the number of repetitions of the Luups for which the user
   -- has Asserted repetition bounds.
   --
   -- Luups
   --    A set of loops in a subprogram.
   --    This does not have to contain all the loops in the subprogram.
   --    Typically it will contain all the feasible, repeatable and still
   --    unbounded loops.
   -- Asserts
   --    The assertions, mapped to this subprogram.
   -- Exec_Bounds
   --    The execution bounds under construction for this subprogram.
   --    Among other things, these bounds define the computation model.
   --
   -- The asserted loop-repetition bounds are placed in the execution
   -- bounds. Normally, no other part of the execution bounds is updated.
   -- However, if a loop is asserted to repeat zero times then the
   -- corresponding flow-graph edges or steps are marked as infeasible
   -- in the computation model for the execution bounds. The model
   -- is then pruned.


   procedure Bound_Loop (
      Luup        : in Loops.Loop_T;
      Initial     : in Calculator.Pool_T;
      Repeat      : in Calculator.Flux_T;
      Repeat_Inv  : in Calculator.Cell_Set_T;
      Inherit_Inv : in Storage.Cell_Set_T;
      Exec_Bounds : in Programs.Execution.Bounds_Ref);
   --
   -- Bounds a loop based on an analysis of the loop counters, if any.
   -- It is assumed that Bound_Asserted_Loops has been used before
   -- this procedure, but the assertions did not bound the loop.
   --
   -- Luup
   --    The loop to be analysed.
   -- Initial
   --    The data-pool giving initial cell values on entry to the Luup
   --    from outside the Luup.
   -- Repeat
   --    The flux along the (union of the) repeat edges of the Luup.
   -- Repeat_Inv
   --    Cells computed to be invariant in the Repeat flux.
   -- Inherit_Inv
   --    Inherited invariant cells.
   -- Exec_Bounds
   --    Execution bounds for the subprogram that contains the loop.
   --    Updated with new loop-bounds if successful.
   --
   -- Two special cases can occur as follows.
   --
   -- > If the loop is found to be infeasible (the Initial pool is void),
   --   the loop is marked infeasible in the computation model for the
   --   execution bounds.
   --
   -- > If the loop is found to be unrepeatable (the Repeat flux is void),
   --   the repeat edges are marked infeasible in the computation model
   --   for the execution bounds.
   --
   -- In both of the above special cases, the computation model is also
   -- pruned to propagate infeasibility by implication. Note, however,
   -- that the Luup parameter itself is not updated. To find out if
   -- some such special case occurred, the properties of the Luup must
   -- be queried from the computation model.
   --
   -- Precondition: The output locus should refer to the loop.


end Bounds.Looping;
