-- Loops.Cells (decl)
--
-- Cells used or defined by a loop.
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
-- $Revision: 1.4 $
-- $Date: 2015/10/24 19:36:50 $
--
-- $Log: loops-cells.ads,v $
-- Revision 1.4  2015/10/24 19:36:50  niklas
-- Moved to free licence.
--
-- Revision 1.3  2007-10-05 20:33:43  niklas
-- BT-CH-0086: Use also primitive model for loop "uses/defines".
-- BT-CH-0086: Use optional "no_" prefix for more options.
--
-- Revision 1.2  2005/02/16 21:11:46  niklas
-- BT-CH-0002.
--
-- Revision 1.1  2004/04/28 19:35:35  niklas
-- First version.
--


with Flow.Computation;
with Storage;


package Loops.Cells is


   function Used_By (
      Luup  : Loop_T;
      Under : Flow.Computation.Model_Ref)
   return Storage.Cell_List_T;
   --
   -- All the storage cells used (read) by the steps in the Luup
   -- Under the given computation model.
   --
   -- A cell is included if the cell is 
   -- > used in a Defining assignment, or
   -- > used or constrained by a Range_Pre constraint, or
   -- > used in a Range_Rel constraint, or
   -- > used in the precondition of an edge that leaves a loop-step, or
   -- > in the basis of a dynamic data reference in any of the above, or
   -- > in the basis of a dynamic edge that leaves a loop-step.
   --
   -- A cell is not included if it only
   -- > is the target of a Defining assignment, or
   -- > is the target (constrained cell) of a Range_Rel constraint, or
   -- > occurs in a Range_Post constraint, or
   -- > is used in the precondition of an edge that enters a loop step
   --   from outside the loop, or
   -- > is used by the callee of a call-step in the loop, or
   -- > is in the basis of a dynamic calling protocol.
   --
   -- Note that although the model may hold some steps/edges to be
   -- infeasible, the cells used in these steps/edges are still included.
   -- This function is used to identify loops in assertions, and the
   -- person who writes the assertions may not be aware of which steps
   -- or edges are infeasible.
   --
   -- TBA option to omit infeasible parts.
   --
   -- Only the given computation model is used. The primitive model
   -- in the underlying flow-graph is not used.


   function Defined_By (
      Luup  : Loop_T;
      Under : Flow.Computation.Model_Ref)
   return Storage.Cell_List_T;
   --
   -- All the storage cells defined (assigned) by the steps in the Luup
   -- Under the given computation model.
   --
   -- A cell is included if the cell is
   -- > defined by a Defining assignment, or
   -- > is defined by the callee of a call in the loop where this effect
   --   of the call is represented in the effect of the call-step.
   --
   -- A cell is not included if it only
   -- > is constrained by a range constraint, or
   -- > is defined by the callee of a call where this effect of the
   --   call is not represented in the effect of the call step.
   --
   -- Note that although the model may hold some steps/edges to be
   -- infeasible, the cells used in these steps/edges are still included.
   -- This function is used to identify loops in assertions, and the
   -- person who writes the assertions may not be aware of which steps
   -- or edges are infeasible.
   -- TBA option to omit infeasible parts.
   --
   -- Only the given computation model is used. The primitive model
   -- in the underlying flow-graph is not used.


   function Is_Used (
      Location : Storage.Location_T;
      By       : Loop_T;
      Under    : Flow.Computation.Model_Ref)
   return Boolean;
   --
   -- Whether the location is used (read) by some step in the loop
   -- Under the given computation model, or (optionally) in the
   -- primitive computation model in the underlying flow-graph.
   --
   -- The location is considered to be used by a loop-step if the
   -- location maps to a cell at the address of this step such that
   -- this cell is
   -- > used in a Defining assignment in the step, or
   -- > used in the precondition of an edge that leaves the step, or
   -- > in the basis of a dynamic data reference in any of the above, or
   -- > in the basis of a dynamic edge that leaves the step.
   --
   -- If the location maps to several cells at the address of a step,
   -- it is enough for one of these cells to be used in the above ways.
   --
   -- The range constraints in the loop-steps are not significant for
   -- this function, nor are the cells used (read) by callees.
   --
   -- Note that although the model may hold some steps/edges to be
   -- infeasible, the cells used in these steps/edges are still included.
   -- This function is used to identify loops in assertions, and the
   -- person who writes the assertions may not be aware of which steps
   -- or edges are infeasible.
   -- TBA option to omit infeasible parts.


   function Is_Defined (
      Location : Storage.Location_T;
      By       : Loop_T;
      Under    : Flow.Computation.Model_Ref)
   return Boolean;
   --
   -- Whether the location is defined (assigned) by some step in the
   -- loop Under the given computation model, or (optionally) in the
   -- primitive computation model in the underlying flow-graph.
   --
   -- The location is considered to be defined by a loop-step if the
   -- location maps to a cell at the address of this step such that
   -- this cell is
   -- > defined in a Defining assignment in the step, or
   -- > defined in the callee of a call-step where this effect of the
   --   call is represented in the effect of the call-step.
   --
   -- If the location maps to several cells at the address of a step,
   -- it is enough for one of these cells to be defined in the above ways.
   --
   -- The range constraints in the loop-steps are not significant for
   -- this function, nor are unlaunched calls.
   --
   -- Note that although the model may hold some steps/edges to be
   -- infeasible, the cells used in these steps/edges are still included.
   -- This function is used to identify loops in assertions, and the
   -- person who writes the assertions may not be aware of which steps
   -- or edges are infeasible.
   -- TBA option to omit infeasible parts.


end Loops.Cells;
