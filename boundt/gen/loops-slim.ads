-- Loops.Slim (decl)
--
-- Approximations and reductions (slimmings) of loops in
-- a flow graph, with and for data-flow calculations.
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
-- $Revision: 1.8 $
-- $Date: 2015/10/24 19:36:50 $
--
-- $Log: loops-slim.ads,v $
-- Revision 1.8  2015/10/24 19:36:50  niklas
-- Moved to free licence.
--
-- Revision 1.7  2009-10-07 19:26:10  niklas
-- BT-CH-0183: Cell-sets are a tagged-type class.
--
-- Revision 1.6  2005/02/16 21:11:46  niklas
-- BT-CH-0002.
--
-- Revision 1.5  2004/04/28 19:39:45  niklas
-- First Tidorum version.
-- Taking Cell_T stuff from Storage, not Arithmetic.
-- Added Calculator comments to calculations.
--
-- Revision 1.4  2001/03/15 07:32:12  holsti
-- Calculations restricted to Basis cells.
--
-- Revision 1.3  2001/01/07 22:06:08  holsti
-- Parameters for live-cell analysis added.
--
-- Revision 1.2  2000/12/28 17:45:46  holsti
-- Use Loop_Cell_Set_T and Loop_Flux_T from Calculator (NC_040).
--
-- Revision 1.1  2000/07/14 20:34:51  holsti
-- First (incomplete) version.
--


with Assertions;
with Calculator;
with Flow.Life;
with Storage;


package Loops.Slim is


   procedure Approximate_Loops (
      Living      : in     Flow.Life.Living_T;
      Loops       : in     Loop_List_T;
      Inherit_Inv : in     Storage.Cell_Set_T;
      Asserts     : in     Assertions.Assertion_Map_T;
      Basis       : in     Calculator.Cell_Set_T;
      Calc        : in     Calculator.Calc_Handle_T;
      Summaries   :    out Calculator.Loop_Summary_List_T);
   --
   -- Approximates each loop in the list as preserving the values of
   -- a set of loop-invariant cells, and modifying the other cells in
   -- an opaque (unknown) manner. This analysis is performed within
   -- a given set of "live" assignments over a given computation model
   -- for a given control-flow graph and supported by a set of assertions.
   --
   -- Living
   --    Defines the "live" assignments in a computation model for
   --    a flow-graph. Provides access to the computation model and
   --    the flow-graph.
   -- Loops
   --    A list of loops in the flow-graph, which should contain all
   --    the loops that are feasible under the computation model, in
   --    bottom-up nesting order (inner loops before outer loops).
   -- Inherit_Inv
   --    Inherited set of (asserted) invariant cells.
   -- Asserts
   --    Assertions mapped to this flow-graph.
   -- Basis
   --    The set of basis cells that should be included in the analysis.
   --    Assignments to other cells are ignored. The Basis set should
   --    be consistent in the sense that an assignment to a Basis cell
   --    uses only Basis cells.
   -- Calc
   --    The Calculator instance to be used for the analysis.
   -- Summaries
   --    The resulting summary of the effect of each of the Loops,
   --    indexed in the same way as the Loops. Thus, the summary for
   --    Loops(I) is returned in Summaries(I).
   --
   -- A loop summary contains two components: the Repeat flux and
   -- the Invariant cell-set.
   --
   -- The Repeat-flux for a loop is a _local_ relation that includes
   -- the computation from and including the loop-head node to any
   -- repeat edge, including the precondition of the edge. If there
   -- are several repeat edges, the union flux is taken. Note that
   -- the Repeat-flux does _not_ contain the flux into the loop-head,
   -- so it does not in any way reflect the computation that leads to
   -- an entry to the loop head from outside the loop.
   --
   -- The Invariant set contains those cells that are invariant in any
   -- execution of the loop body, from and including the loop-head node
   -- up to any edge that repeats or exits the loop.
   --
   -- The assertions are used in a limited way; only the assertions
   -- regarding loop-invariant cells are used, to increase the
   -- Invariant sets. Assertions on variable values or loop repetitions
   -- are not used, although they could improve the Repeat-fluxes.


end Loops.Slim;
