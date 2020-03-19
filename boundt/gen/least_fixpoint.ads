-- Least_Fixpoint (decl)
--
-- Finding the least fixpoint of a set of data-flow equations
-- over a directed, connected graph.
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
-- $Date: 2015/10/24 19:36:50 $
--
-- $Log: least_fixpoint.ads,v $
-- Revision 1.5  2015/10/24 19:36:50  niklas
-- Moved to free licence.
--
-- Revision 1.4  2005-05-09 15:31:01  niklas
-- Added the parameter Least_Fixpoint.Afters, to choose between the
-- pre-values and the post-values of the solution. The value-origin
-- analysis needs post-values.
--
-- Revision 1.3  2005/02/16 21:11:46  niklas
-- BT-CH-0002.
--
-- Revision 1.2  2004/04/28 19:19:41  niklas
-- First Tidorum version.
-- Added a Value parameter to the generic formal Successors function so
-- that the successor node-set can depend on the current value, for
-- example to elide infeasible edges.
-- Removed the built-in iteration-tracing feature (the generic formals
-- Node_Image and Value_Image and the internal operation Show_Values
-- and its calls). If tracing is desired, it should be implemented in
-- the actual operations bound to the generic formals.
--
-- Revision 1.1  2001/01/07 21:54:23  holsti
-- First version.
--


generic


   type Graph is limited private;
   --
   -- The graph type. This is used only as an opaque parameter,
   -- passed on to calls of the node-level operations.


   type Node is private;
   type Node_List is array (Positive range <>) of Node;
   --
   -- The graph-node type, and a list of such nodes.


   with function Index_Of (Item : Node) return Positive;
   --
   -- Each node is assumed to have a unique index, which also gives
   -- the index of the node in the "Nodes" parameter of Least_Fixpoint,
   -- see below.


   type Value is private;
   type Value_List is array (Positive range <>) of Value;
   --
   -- The type of the values for data-flow analysis, as explained
   -- below in more detail.


   with function Successors (
      After  : Node;
      Post   : Value;
      Within : Graph)
   return Node_List;
   --
   -- The successor nodes of the given node. This defines the
   -- edges of the graph. The successor set can depend on the
   -- current value (Post) considered to flow from the node to
   -- its successors.
   --
   -- For example, if the graph is the control-flow graph of a
   -- subprogram, and the Post value represents bounds on the values
   -- of program variables after the node, these bounds may imply
   -- that the logical precondition (conditional branch condition)
   -- of an edge is false, and thus this Post value cannot flow
   -- along this edge, so the edge should be ignored when the
   -- successors are listed.
   --
   -- Such changes in the successor list do not have to be
   -- monotonic with respect to the Post value.
   --
   -- The Successors list can contain duplicates. In other words,
   -- there can be more than one edge from a given source node to a
   -- given target node.


   with procedure Initialize (
      Via  : in     Node;
      Pre  :    out Value;
      Post :    out Value;
      Grew :    out Boolean);
   --
   -- The data-flow function that initializes the Pre and Post
   -- values for each node.
   -- The procedure should set Pre to the "bottom" or "smallest" possible
   -- value, and Post to the value that results from transforming this
   -- Pre-value via the "execution" of the node.
   -- The procedure should return Grew as True if the resulting Post
   -- value is "larger" than the "bottom" value, which means that the
   -- successors of this node must be updated in the iteration.


   with procedure Transform (
      Pre  : in     Value;
      Via  : in     Node;
      Post : in out Value;
      Grew :    out Boolean);
   --
   -- The data-flow function that transforms the value (Pre) holding
   -- before a node, via the "execution" of the node, to the value (Post)
   -- holding after the node.
   --
   -- After the initialisation with Initialize, this procedure is
   -- applied iteratively, and is given the value of Post from the preceding
   -- iteration as the "in" value of Post. It shall update Post to its new
   -- value (a function of Pre and Via) and indicate if Post "grew", or did
   -- not change, in Grew.
   --
   -- If Transfrom returns Grew as True, the Successor function is invoked
   -- to find the nodes to which this new value of Post can flow, and the
   -- new Post value is later transported to the successors by means of
   -- the Merge procedure as described below.


   with procedure Merge (
      Source : in     Node;
      Post   : in     Value;
      Target : in     Node;
      Pre    : in out Value;
      Grew   :    out Boolean);
   --
   -- The data-flow function that merges data-values along confluent
   -- flow edges into the merged value holding before the target node
   -- of the edges. Source is one of the predecessors of Target (i.e.
   -- Target is one of the Successors of Source), and Post is the value
   -- holding after Source is executed. This procedure is applied
   -- iteratively and incrementally. Whenever one of the Post values
   -- flowing into the Target node changes, Merge is invoked with
   -- the new Post value and the old Pre value of the Target node.
   -- It shall update Pre by merging in the new Post value, and
   -- indicate if Prev "grew", or did not change, in Grew.
   --
   -- If there are several edges from Source to Target, Merge is still
   -- called only once to carry the changed Post data from Source to
   -- Target, until the Post value of Source changes again.


   with procedure Finalize (Item : in out Value);
   --
   -- Discards a data value. This procedure is called when a local
   -- variable of type Value is no longer needed.
   -- This occurs for those values of each node which are not returned
   -- from the Least_Fixpoint function and therefore have the status
   -- of working variables. The values that are returned from
   -- Least_Fixpoint are _not_ finalized in this way. See the
   -- parameter Afters of Least_Fixpoint.


function Least_Fixpoint (
   Nodes  : Node_List;
   Edges  : Natural;
   Within : Graph;
   Afters : Boolean := False)
return Value_List;
--
-- Finds the least fixpoint of a set of data-flow equations
-- over a directed graph.
--
-- The graph consists of nodes, as listed in the Nodes parameter,
-- and edges. The edges are not explicitly represented here; only
-- their total number (or an upper bound) is needed as parameter.
--
-- The function Index_Of must return the index, in the Nodes
-- list, of the given node.
--
-- Each node has a list of successor nodes (perhaps empty).
-- The predecessors of a given node N are defined as the set of nodes
-- M for which N is a successor of M.
--
-- A variable of type Value is associated with each node, and
-- is called the "pre-value" of the node. Conceptually, the
-- pre-value is associated with the start of the node (entering
-- the node). The pre-value is the data that "flows into" the node.
--
-- Each node "transforms" its pre-value in some way (depending on the
-- particular node). The transformed value is conceptually associated
-- with the end of the node (leaving the node), and is called the
-- "post-value" of the node. The post-value is the data the "flows
-- out from" the node, along the edges to its successors.
--
-- The data-flow equations to be solved consist of the "transform"
-- equation, plus the constraint that the pre-value of a node is
-- equal to the value computed by a function that "merges" the
-- post-values of all predecessors of the node.
--
-- The Value type is assumed to be (partially) ordered (in some way
-- not explicitly represented here), with a "bottom" value that is
-- "less or equal" to any other value. The transform function and the
-- merge function are assumed to be monotonic with respect to their
-- Value parameters, that is, if the parameter is made "larger", the
-- function's value also becomes "larger" or is unchanged.
-- Moreover, the "bottom" value is assumed to be a neutral (zero)
-- element for the "merge" function, that is, a post-value equal
-- to "bottom" has no effect on the result of "merge". Note that
-- no such assumption is made regarding the "transform" function.
--
-- The "merge" function is assumed to be insensitive to the order
-- of the post-values (associative and commutative), so that its
-- value can be computed incrementally and iteratively, as shown
-- below in the algorithm.
--
-- The "least fixpoint" of the data-flow equations is defined to
-- be an assignment of pre-values to nodes such that the data-flow
-- equations are satisfied, and making any pre-values "smaller"
-- violates some data-flow equation.
--
-- The function operates iteratively in the following way.
-- There is a work-list that contains all the edges for which the
-- pre-value of the target node must be recomputed, because the
-- post-value of the source-node has changed. The work-list is
-- initially empty.
--
-- First, the pre-value of each node is initialized to the "bottom"
-- value with Initialize, which also computes the corresponding
-- post-value. For each node in which the post-value "grew" (is no
-- longer Bottom), the edges to the successors are added to the
-- work-list for iterative processing.
--
-- As long as the work-list contains edges, one of their target nodes
-- is chosen, all edges with this target are removed from the work-list,
-- and Merge is called with the Post parameter taking, in turn,
-- the post-value of each predecessor of the target node, and with
-- the target node's pre-value as the Pre parameter. Merge is assumed
-- to merge the Post value into the Pre value. It should return the
-- Grew parameter as True if the value of Pre changed (it must in fact
-- "grow") and False if Pre did not change.
--
-- If Merge indicates that the pre-value grew, the post-value is updated
-- with Transform. Transform is given the post-value as an "in out"
-- parameter, with the old value as "in" and the new value as "out".
-- It should return the Grew parameter as True if the post-value
-- changed (it must in fact "grow") and False otherwise. If the
-- post-value grew, all the edges from this node are are added to
-- the work-list (if not already there).
--
-- The Source and Target parameters for Merge indicate the edge
-- which is under consideration. They are intended chiefly for tracing
-- purposes, but they could also influence the Merge result.
--
-- The function returns the final values of all the nodes, with the
-- same indices as in the Nodes parameter. If the parameter Afters is
-- False, the pre-values are returned; if Afters is True, the post-values
-- are returned. The other values (post-values if Afters is False,
-- pre-values if Afters is True) are given to Finalize, which usually
-- simply discards them.
