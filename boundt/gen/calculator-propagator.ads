-- Calculator.Propagator (decl)
--
-- Elementary operations to propagate data-flow in an acyclic region
-- of a flow-graph. These operations are used in higher-level data-flow
-- calculations.
--
-- These operations may rely on a systematic assignment of identifiers
-- to the pools and fluxes associated with the nodes and edges of a
-- flow-graph.
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
-- $Revision: 1.7 $
-- $Date: 2015/10/24 19:36:47 $
--
-- $Log: calculator-propagator.ads,v $
-- Revision 1.7  2015/10/24 19:36:47  niklas
-- Moved to free licence.
--
-- Revision 1.6  2009-10-07 19:26:09  niklas
-- BT-CH-0183: Cell-sets are a tagged-type class.
--
-- Revision 1.5  2005/10/20 11:28:30  niklas
-- BT-CH-0015.
--
-- Revision 1.4  2005/02/16 21:11:41  niklas
-- BT-CH-0002.
--
-- Revision 1.3  2001/01/07 22:06:02  holsti
-- Parameters for live-cell analysis added.
--
-- Revision 1.2  2000/07/17 21:00:08  holsti
-- Cell_Set_T instead of Cell_List_T for domains and ranges.
--
-- Revision 1.1  2000/07/14 20:34:11  holsti
-- First version.
--


with Calculator.Formulas;


private package Calculator.Propagator is
--
-- The package is private to prevent other Bound-T parts from
-- becoming dependent on these operations, which are considered
-- internal to Calculator and prone to change.


   type Edge_Index_List_T is
      array (Positive range <>) of Flow.Edge_Index_T;
   --
   -- Represents a list of edges in a way that applies both to
   -- ordinary flow-graphs (Flow.Graph_T) and to slimmed flow-graphs
   -- (Flow.Slim.Graph_T).


   function Flow_Into (
      Node  : Flow.Node_Index_T;
      Along : Edge_Index_List_T;
      Root  : Flux_T;
      Calc  : Calc_Handle_T)
   return Formulas.Identifier_T;
   --
   -- Computes the data-flow "into" a node along some chosen edges
   -- coming to the node, or by the Root flux if there are no edges.
   -- Returns the calculator identifier for the computed "into"
   -- relation.


   function Pool_Into (
      Node  : Flow.Node_Index_T;
      Along : Edge_Index_List_T;
      Root  : Pool_T;
      Calc  : Calc_Handle_T)
   return Formulas.Identifier_T;
   --
   -- Computes the data-pool "into" a node along some chosen edges
   -- coming to the node, or by the Root pool if there are no edges.
   -- Returns the calculator identifier for the computed "into" set.


   function Effect (
      Node   : Flow.Node_T;
      Domain : Cell_Set_T;
      Living : Flow.Life.Living_T;
      Calc   : Calc_Handle_T)
   return Formulas.Identifier_T;
   --
   -- Computes the effect of a node by joining the effects of
   -- the steps in the Node, using only the "live" assignments.
   -- Returns the calculator identifier for the computed effect
   -- relation, which is based on the Domain cells.


   function Flow_Out (
      From  : Formulas.Identifier_T;
      Cond  : Arithmetic.Condition_T;
      Basis : Formulas.Tuple_T)
   return Formulas.Relation_T;
   --
   -- The formula for the "flow" relation along an edge that
   -- leaves a node with a given "from" relation under a given
   -- precondition (necessary but perhaps not sufficient).
   -- The Basis tuple is used for the range-restriction
   -- implied by the condition (if any).


   function Set_Out (
      From  : Formulas.Identifier_T;
      Cond  : Arithmetic.Condition_T;
      Basis : Formulas.Tuple_T)
   return Formulas.Set_T;
   --
   -- The formula for the "flow" pool along an edge that
   -- leaves a node with a given "from" pool under a given
   -- precondition (necessary but perhaps not sufficient).
   -- The Basis tuple is used for the constraint implied by
   -- the condition (if any).


end Calculator.Propagator;
