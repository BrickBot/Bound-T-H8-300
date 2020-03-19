-- Calculator.Slim (decl)
--
-- Data-flow analysis of acyclic regions in slimmed flow-graphs.
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
-- $Date: 2015/10/24 19:36:47 $
--
-- $Log: calculator-slim.ads,v $
-- Revision 1.8  2015/10/24 19:36:47  niklas
-- Moved to free licence.
--
-- Revision 1.7  2009-10-07 19:26:10  niklas
-- BT-CH-0183: Cell-sets are a tagged-type class.
--
-- Revision 1.6  2005/02/16 21:11:41  niklas
-- BT-CH-0002.
--
-- Revision 1.5  2004/05/02 05:34:27  niklas
-- First Tidorum version.
-- Taking Cell_T stuff from Storage, not from Arithmetic.
-- Using the new function Calculator.Formulas.Adapted to match bases.
--
-- Revision 1.4  2001/01/07 22:06:06  holsti
-- Parameters for live-cell analysis added.
--
-- Revision 1.3  2000/12/29 14:40:16  holsti
-- Symbolic-constant usage deleted (part of NC_078).
--
-- Revision 1.2  2000/08/17 13:01:47  holsti
-- Changed cell-lists to cell-sets.
--
-- Revision 1.1  2000/07/14 20:34:12  holsti
-- First version.
--


with Flow.Slim;


package Calculator.Slim is


   function Flux_Of_Region (
      Within : Flow.Slim.Graph_T;
      Nodes  : Flow.Slim.Node_List_T;
      Edges  : Flow.Slim.Edge_List_T;
      Final  : Flow.Slim.Edge_List_T;
      Var    : Cell_Set_T;
      Calc   : Calc_Handle_T)
   return Flux_List_T;
   --
   -- Using :
   --    a set of nodes and edges forming an acyclic region within
   --    a slimmed flow-graph,
   --    a set of "final" edges from nodes in the region,
   --    a set of cells to be treated as variables,
   --    a calculator handle
   -- Giving:
   --    for each "final" edge, the flux from the region's root
   --    nodes to the edge (including edge success predicates));
   --
   -- The region is usually determined entirely by the Edges and then
   -- contains all the nodes that are the source or target of some
   -- edge in Edges.
   --
   -- The Nodes parameter can usually be empty. In general, it can 
   -- contain any subset of the nodes in the region. It can contain
   -- nodes that are not connected to the Edges, although this would
   -- be abnormal and probably not useful for Bound-T.
   --
   -- However, if the region contains exactly one node, then it cannot
   -- be defined by edges (because there are no internal Edges) and so
   -- the single node must be given in Nodes and Edges is empty.
   --
   -- The set of Final edges is usually disjoint from the set of Edges
   -- that define the region, but they can overlap, too.
   --
   -- Note that the data-flow computation for a given node in the region
   -- considers only incoming edges from the Edges set and outgoing
   -- edges from the Edges and Final sets. Other edges connected to this
   -- node may exist in the flow-graph that contains the region, but are
   -- not included in the computation. Thus, the body of a loop can be
   -- considered an acyclic region by leaving out the repeat edges from
   -- the Edges set and if any inner loops are already fused into single
   -- fusion nodes.


end Calculator.Slim;
