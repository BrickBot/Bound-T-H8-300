-- Flow.Slim (decl)
--
-- Slimming (reducing) a flow graph by fusing subgraphs
-- into single nodes called fusions.
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
-- $Revision: 1.11 $
-- $Date: 2015/10/24 19:36:49 $
--
-- $Log: flow-slim.ads,v $
-- Revision 1.11  2015/10/24 19:36:49  niklas
-- Moved to free licence.
--
-- Revision 1.10  2009-01-18 08:08:13  niklas
-- Moved context clause to body.
--
-- Revision 1.9  2007/04/18 18:34:39  niklas
-- BT-CH-0057.
--
-- Revision 1.8  2007/03/29 15:18:03  niklas
-- BT-CH-0056.
--
-- Revision 1.7  2007/02/13 20:21:57  Niklas
-- BT-CH-0044.
--
-- Revision 1.6  2006/02/27 09:30:00  niklas
-- Added functions Edges_Into and Edges_From.
--
-- Revision 1.5  2005/02/16 21:11:45  niklas
-- BT-CH-0002.
--
-- Revision 1.4  2001/03/16 11:14:08  holsti
-- Cells_Accessed includes edge conditions (NC_045).
--
-- Revision 1.3  2000/08/21 13:07:29  holsti
-- Edgeless graphs allowed (Max_Edge return Edge_Count_T).
--
-- Revision 1.2  2000/07/16 13:12:48  holsti
-- Fused nodes (and edges) are deleted from the slimmed graph.
--
-- Revision 1.1  2000/07/14 20:35:08  holsti
-- First version.
--

--:dbpool with GNAT.Debug_Pools;

with Arithmetic;
with Calculator;
with Flow.Life;
with Storage;


package Flow.Slim is
--
-- A slim(med) graph is a flow graph, and a computation model for this
-- flow graph, which have been subjected to zero or more fusing operations.
-- Each fusing operation takes a nonempty subset of the graph nodes and
-- replaces it with a single node which is called the fusion of the
-- subgraph.
--
-- Any edges between the fused subgraph and the rest of the
-- graph are also changed into the corresponding edges in which
-- the end that was in the subgraph is replaced by the fusion.
--
-- Any edges internal to the fused subgraph (that is, edges where
-- both source and target are in the fused subgraph) are deleted
-- as part of the fusing operation.


   -- GRAPHS --


   type Graph_T is private;
   --
   -- A slimmed flow-graph.
   -- The attributes of a flow-graph include:
   --
   -- > The entry node.
   -- > The set of nodes.
   -- > The set of edges between nodes
   -- > The computation model and set of "live" assignments.
   --
   -- A slimmed flow-graph is always based on a normal flow-graph
   -- (Flow.Graph_T) using the "node" view. The "step" view is not
   -- available through the slimmed graph.
   --
   -- Nodes and edges in the slimmed flow-graph are numbered
   -- (indexed) in the same way as in the original flow-graph
   -- on which the slimmed graph is based. However, the numbering
   -- is no longer a simple bijection between an index interval
   -- and the set of nodes or edges, because some indices in the
   -- range that applies to the original graph have no corresponding
   -- nodes or edges in the slimmed graph, if the nodes were fused
   -- or the edges were internal to some fused subgraph and thus
   -- deleted from the slimmed graph.
   --
   -- The computation model attached to the original flow-graph and
   -- the slimmed form is not modified by slimming. The model is
   -- included in the slimmed graph to define the edge conditions and
   -- the set of used cells (vide the corresponding functions below).
   -- The other attributes of the computation model are not relevant.
   --
   -- The reference from a slimmed graph to the computation model and
   -- "live" assignments is not counted separately in the reference count
   -- of the computation model, because the reference goes via an object
   -- of type Flow.Life.Living_T and references to such objects are not
   -- counted.
   --
   -- Graph_T has reference semantics. The values of type Graph_T returned
   -- by functions in this package are to be understood as references to
   -- some underlying flow-graph objects; any update applied to such a
   -- reference is visible via all references to the same object.


   function Max_Node (Graph : Graph_T) return Node_Index_T;
   function Max_Edge (Graph : Graph_T) return Edge_Count_T;
   --
   -- These functions return the highest sequential number (index)
   -- of any node or edge in the graph.
   -- All the number sequences start at 1 and run independently.
   --
   -- Note that some node numbers in the interval 1 .. Max_Node
   -- and some edge numbers in the interval 1 .. Max_Edge
   -- may not be in use. Attempting to use such an index to
   -- access the edge will raise No_Such_Node or No_Such_Edge,
   -- respectively.
   -- Also note that a very simple graph (one node) may have no
   -- edges, in which case Max_Edge returns zero.


   No_Such_Node : exception;
   No_Such_Edge : exception;
   --
   -- Raised upon an attempt to find a node or edge by its index,
   -- when the slimmed graph no longer contains a node or edge with
   -- this index.


   function Living (Graph : Graph_T) return Life.Living_T;
   --
   -- The set of "live" assignments associated with the slimmed Graph.


   -- NODES --


   type Node_Kind_T is (Basic, Fusion);

   type Node_T (Kind : Node_Kind_T := Basic) is private;
   --
   -- A node in a slimmed flow-graph is either a basic node
   -- (Flow.Node_T) or a fusion.
   --
   -- The attributes of a node in a slimmed flow-graph include:
   --
   -- > Its index number (1..).
   -- > Its arithmetic effect, which can take two forms:
   --   >> A calculated flux (if the node is a fusion), or
   --   >> The same as the effect of the basic node, if
   --      the node is not a fusion.
   -- > The sets of cells defined and used, which for a basic
   --   node is computed from the arithmetic effect, but for
   --   a fusion node is defined by the fusion operation.
   --
   -- A very notable property of a slimmed graph is that the
   -- numbering of nodes (and edges) follows the numbering of the
   -- Flow.Graph_T on which the slimmed graph is based. Thus, as
   -- long as a node (or edge) has not been involved in a fusion,
   -- its number and attributes are the same in the slimmed graph
   -- as in the basic graph. For a fusion node, the index is taken
   -- from one of the fused nodes as specified by the client.
   --
   -- The node and edge sets are also filtered by the feasibility of
   -- nodes and edges under the computation model associated with the
   -- slimmed graph.


   subtype Basic_Node_T  is Node_T (Basic);
   subtype Fusion_Node_T is Node_T (Fusion);


   function Index (Node : Node_T) return Node_Index_T;
   --
   -- The index number of a node.
   -- These numbers are in the range 1 .. Max_Node.
   -- However, not all numbers in this range will correspond
   -- to nodes in the slimmed graph, because some nodes may be
   -- infeasible and some nodes may have gone into a fusion.


   function Node_At (
      Index  : Node_Index_T;
      Within : Graph_T)
   return Node_T;
   --
   -- The node identified by the given Index, Within the given graph.
   -- If the index is out of range, Constraint_Error is raised.
   -- If the node has been deleted as the result of a fusion, or is
   -- infeasible under the computation model, No_Such_Node is raised.


   function Basic (Node : Basic_Node_T) return Flow.Node_T;
   --
   -- The basic node that corresponds to this slimmed node.


   function Flux (Node : Fusion_Node_T) return Calculator.Flux_T;
   --
   -- The flux of the node. The node is a fusion of a subgraph
   -- of basic nodes; this flux represents that part of the
   -- computational effect of the subgraph which does not
   -- depend on the particular exit taken from the subgraph.
   -- The rest of the computational effect is represented in
   -- the fluxes of the edges leaving this fusion node.


   type Node_List_T is array (Positive range <>) of Node_T;
   --
   -- A list (set) of nodes.



   -- EDGES --


   type Edge_Kind_T is (Bare, Fluxed);

   type Edge_T (Kind : Edge_Kind_T := Bare) is private;
   --
   -- There are two kinds of edges in a slimmed flow-graph.
   -- For a "Bare" edge, the flux on the edge has not yet
   -- been computed.
   -- A "Fluxed" edge is the result of a fusion operation
   -- in which a flux has already been computed for the edge.
   --
   -- The attributes of an edge include:
   --
   -- > Its index number (1..)
   -- > The source node from which the edge starts.
   -- > The target node at which the edge ends.
   -- > For a Bare edge, the logical precondition for executing
   --   the edge.
   -- > For a Fluxed edge, the calculated flux on the edge.
   --
   -- In practice, there is a correlation between the kinds
   -- of nodes and edges: all edges leaving a Fusion node
   -- will be Fluxed, and all other edges will be Bare.
   -- Note that an edge entering a Fusion can be Bare or Fluxed,
   -- depending on the kind of the edge's source node.


   subtype Bare_Edge_T   is Edge_T (Bare);
   subtype Fluxed_Edge_T is Edge_T (Fluxed);


   function Index (Edge : in Edge_T) return Edge_Index_T;
   --
   -- The index number of an edge.
   -- These numbers run from 1 to Max_Edge.
   -- However, not all numbers in this range will correspond
   -- to edges in the slimmed graph.


   function Edge_At (
      Index  : Edge_Index_T;
      Within : Graph_T)
   return Edge_T;
   --
   -- The edge identified by the given index, within the
   -- given graph.
   -- If the index is out of range, Constraint_Error is raised.
   -- If the edge has been deleted as the result of a fusion, or is
   -- infeasible under the computation model, No_Such_Edge is raised.


   function Source (Edge : Edge_T) return Node_T;
   function Target (Edge : Edge_T) return Node_T;
   --
   -- The source and target nodes of the edge.
   -- Note that a fusion changes the target of all edges that
   -- enter the fused subgraph, and changes the source of all edges
   -- that leave the fused subgraph.


   function Condition (Edge : Bare_Edge_T)
   return Arithmetic.Condition_T;
   --
   -- See Flow.Condition and Flow.Computation.Condition.


   function Flux (Edge : Fluxed_Edge_T)
   return Calculator.Flux_T;
   --
   -- The flux along the edge. The source node of this edge
   -- will be a fusion of a subgraph of basic nodes from which
   -- the basic counterpart of this edge was an exit.
   -- The flux along the edge represents part of the computation
   -- and conditions on the path(s) in the fused subgraph that
   -- lead to this edge. Another part is represented in the
   -- flux of the fusion node.


   type Edge_List_T is array (Positive range <>) of Edge_T;
   --
   -- A set (or list) of edges, for example, all edges that
   -- leave a given step.


   function Edges_Into (Node : Node_T; Within : Graph_T)
   return Edge_List_T;
   --
   -- All the edges that enter the node (Target(Edge) = Node).


   function Edges_From (Node : Node_T; Within : Graph_T)
   return Edge_List_T;
   --
   -- All the edges that leave the node (Source(Edge) = Node).



   -- GRAPH CONSTRUCTION AND SLIMMING --


   function Entire_Graph (
      Living : Flow.Life.Living_T)
   return Graph_T;
   --
   -- Creates a slimmed graph as a logical copy of a basic graph under
   -- a computation model and "live" assignment set for the basic graph.
   --
   -- The slimmed graph is then "based on" the source graph.
   -- This involves several references from the slimmed graph
   -- to the source graph; if the source graph is modified in
   -- some way, the modification may or may not be visible in
   -- the slimmed graph. It is not advisable to modify the source
   -- graph while a slimmed graph based on it exists.
   --
   -- Likewise, the slimmed graph is "based on" the given computation
   -- model and "live" assignment set and contain references to these.


   procedure Destroy (Graph : in out Graph_T);
   --
   -- Erases the graph and frees the memory used by it.
   -- The parameter graph must not be used after this operation;
   -- trying to use it may raise Constraint_Error.
   -- This operation has no effect on the basic graph and the
   -- computation model from which the destroyed slimmed graph
   -- was created.


   procedure Fuse (
      Nodes   : in     Node_Set_T;
      Proxy   : in     Node_Index_T;
      Flux    : in     Calculator.Flux_T;
      Within  : in out Graph_T);
   --
   -- Modifies the given graph by fusing all the given Nodes into
   -- one node called the fusion. The index of the fusion node will
   -- be the Proxy index, which must belong to one of the fused nodes.
   -- The Flux parameter defines the flux through the fusion
   --
   -- All the fused nodes are deleted, and a fusion node is added.
   -- Thus, all the indices of the fused nodes become invalid,
   -- except for the Proxy index which identifies the fusion.
   --
   -- Edges with both ends in the given set of nodes are deleted.
   --
   -- Edges with one end in the given set of nodes are modified to
   -- have the fusion node at that end, instead of the original
   -- node.
   --
   -- Note that the Nodes parameter, although typed as Node_Set_T,
   -- is actually a set of node indices (see Flow.Node_Set_T).
   -- It is allowed to include indices of deleted nodes in the
   -- set, and they have no effect on the fusion operation.


   procedure Set_Flux (
      Edge   : in     Edge_Index_T;
      Flux   : in     Calculator.Flux_T;
      Within : in out Graph_T);
   --
   -- Sets the flux attribute of the given edge.
   -- The edge will change kind from Bare to Fluxed if necessary.
   -- In practice, a flux will be assigned only to edges that
   -- have a fusion node as source node.


   function Edges (
      From   : Node_Set_T;
      Into   : Node_Set_T;
      Within : Graph_T)
   return Edge_List_T;
   --
   -- Given a set From of possible source nodes and another set
   -- Into of possible target nodes, returns all edges that
   -- go from some source to some target, but only edges that are
   -- feasible under the computation model associated with the
   -- slimmed graph.


   function Cells_Accessed (
      By     : Node_Set_T;
      Within : Graph_T)
   return Storage.Cell_Set_T;
   --
   -- The cells that are accessed (read and/or written) by the
   -- nodes in the given node-set or by the conditions on edges
   -- from these nodes, under the computation model and "live"
   -- assignments associated with the slimmed graph.



private

   type Graph_Object_T (
      Max_Node : Node_Index_T;
      Max_Edge : Edge_Count_T);
   --
   -- A slimmed graph starting with the given number of nodes and edges.
   -- There will always be at least one node, but perhaps no edges.

   type Graph_T is access Graph_Object_T;
   --
   -- A slimmed graph.

   --:dbpool Graph_Pool : GNAT.Debug_Pools.Debug_Pool;

   --:dbpool for Graph_T'Storage_Pool use Graph_Pool;


   type Fusion_T (
      Number_Into : Natural;
      Number_From : Natural);
   --
   -- Represents a fusion node, with the given number of entering
   -- and leaving edges.

   type Fusion_Ref is access Fusion_T;

   --:dbpool Fusion_Pool : GNAT.Debug_Pools.Debug_Pool;

   --:dbpool for Fusion_Ref'Storage_Pool use Fusion_Pool;


   type Node_T (Kind : Node_Kind_T := Basic) is record

      Exists : Boolean;

      case Kind is
      when Basic  => Basic  : Flow.Node_T;
      when Fusion => Fusion : Fusion_Ref;
      end case;

   end record;
   --
   -- A node in a slimmed graph (including the "ghost" of a
   -- deleted node).
   --
   -- The Exists component is False when this node has been deleted
   -- by a fusion operation or if the node is infeasible under the
   -- computation model. In this case, none of the other components
   -- should be considered meaningful.


   type Edge_T (Kind : Edge_Kind_T := Bare) is record

      Exists  : Boolean;
      Graph   : Graph_T;
      Source  : Node_Index_T;
      Target  : Node_Index_T;

      case Kind is

      when Bare =>
         Bare : Flow.Edge_T;

      when Fluxed =>
         Index  : Edge_Index_T;
         Flux   : Calculator.Flux_T;

      end case;

   end record;
   --
   -- An edge in a slimmed graph (including the "ghost" of
   -- a deleted edge).
   --
   -- The Exists component is False when this edge has been deleted
   -- by a fusion operation or if the edge is infeasible under the
   -- computation model. In this case, none of the other components
   -- should be considered meaningful.
   --
   -- The edge contains a reference to its containing graph,
   -- so that the Source and Target indices can be mapped to
   -- their currently corresponding nodes. (The alternative
   -- to use Source/Target components of type "access all Node_T"
   -- doesn't work if the nodes are held in an array, because
   -- aliased array components must be constrained, while Node_T
   -- is not.) Moreover, this gives access to the computation Model
   -- that defines the precondition on the edge.


end Flow.Slim;
