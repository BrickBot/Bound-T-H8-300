-- Loops (decl)
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
-- $Revision: 1.31 $
-- $Date: 2015/10/24 19:36:50 $
--
-- $Log: loops.ads,v $
-- Revision 1.31  2015/10/24 19:36:50  niklas
-- Moved to free licence.
--
-- Revision 1.30  2009-03-20 18:19:30  niklas
-- BT-CH-0164: Assertion context identified by source-line number.
--
-- Revision 1.29  2008/01/14 20:27:28  niklas
-- BT-CH-0106: Loops.Max_Depth and other changes in Loops.
--
-- Revision 1.28  2006/10/30 23:09:06  niklas
-- BT-CH-0033.
--
-- Revision 1.27  2006/05/27 21:34:20  niklas
-- Renamed function Loop_Index to Index, for brevity, as
-- part of BT-CH-0020.
--
-- Revision 1.26  2005/09/20 09:56:36  niklas
-- Added function Exits_At_End.
--
-- Revision 1.25  2005/09/03 11:50:30  niklas
-- BT-CH-0006.
--
-- Revision 1.24  2005/08/08 20:15:48  niklas
-- Added function Sorted_By_Address.
--
-- Revision 1.23  2005/08/08 17:39:00  niklas
-- Added the operator "<" to test loop containment.
-- Added the function Root_Loops.
--
-- Revision 1.22  2005/02/16 21:11:47  niklas
-- BT-CH-0002.
--
-- Revision 1.21  2004/04/28 19:34:04  niklas
-- Made the Irreducible exception public, for higher-level handling.
-- Added support for Eternal loops.
-- Added function Forward_Edges returning step-edge list.
-- Reimplemented the Loop_Members function to return a reference (access)
-- to a heap-allocated node-set rather than a node-set as such.
-- Clarified the structure of the Loops function.
--
-- Revision 1.20  2001/03/21 20:24:13  holsti
-- Removed Loop_Ref_T. Renamed Loops_Ref_T to Loops_Ref.
--
-- Revision 1.19  2001/03/09 13:40:05  holsti
-- Edges_From added.
--
-- Revision 1.18  2001/03/06 09:15:35  holsti
-- Exit_Edges and Loop_Headed added.
--
-- Revision 1.17  2000/12/28 12:23:52  holsti
-- Add_Loops added.
--
-- Revision 1.16  2000/12/22 13:34:20  sihvo
-- Added Steps_In.
--
-- Revision 1.15  2000/12/05 15:44:02  holsti
-- The term "loop neck" replaces the overloaded "loop entry".
-- Decoder.Stack_Height_Cell replaces deleted Arithmetic function.
--
-- Revision 1.14  2000/09/19 12:08:40  langback
-- Added the function Loop_Index
--
-- Revision 1.13  2000/09/05 12:25:40  langback
-- Updated Loops_Contained_In. Renamed the second instance of
-- Containing_Loops to Containing_Loop, since it returns maximally
-- one loop after latest update.
--
-- Revision 1.12  2000/08/04 13:35:46  langback
-- Interface to Containing_Loops (one instance) and Loops_Contained_In
-- slightly modified
--
-- Revision 1.11  2000/08/04 11:18:00  langback
-- Added Loops_Ref_T type. Added Is_Loop_Head function.
--
-- Revision 1.10  2000/07/17 21:00:55  holsti
-- Internal_Edges and Repeat_Edges added.
--
-- Revision 1.9  2000/07/16 18:39:54  holsti
-- Forward_Edges added.
--
-- Revision 1.8  2000/07/14 20:37:49  holsti
-- Moved Calculator-dependent parts to Loops.Slim.
--
-- Revision 1.7  2000/07/05 10:04:47  langback
-- Change of the definition of the Loops_T type.
--
-- Revision 1.6  2000/07/04 12:06:26  holsti
-- Renamed several types.
--
-- Revision 1.5  2000/06/28 15:07:20  langback
-- Added Loop_Index_T.
-- Added functions returning the Head node of a loop and Loop and Entry
-- edges of a given loop head.
--
-- Revision 1.4  2000/06/13 12:34:48  langback
-- First version with CVS headers included.
-- Still a preliminary version.
--


with Flow;
with Processor;


package Loops is

-- In this package, the following terminology is used:
--
-- "Flow-graph" always means "basic-block flow-graph".
--
-- "Node" means a "basic block".
--
-- The "entry node" is the first node in the flow graph (the
-- basic block that contains the entry step of the subprogram).


   type Loop_T is private;
   --
   -- One loop within a subprogram.
   -- Because "loop" is a reserved word, an object of this
   -- type will often be called "Luup" instead of "Loop".


   type Loop_List_T is array (Positive range <>) of Loop_T;
   --
   -- Array of loops in unspecified order as compared to the type
   -- Loops_T defined below.


   type Loop_Count_T is new Natural;
   --
   -- A number of loops, for example the number of loops in a subprogram.


   subtype Loop_Index_T is Loop_Count_T range 1 .. Loop_Count_T'Last;
   --
   -- Identifies one loop within a subprogram.
   -- The loops are numbered so that the number of an inner loop
   -- is always smaller than the number of the outer loops that contain it.


   subtype Poss_Loop_Index_T is Loop_Count_T;
   --
   -- A loop index, or zero to mean "no loop".


   No_Loop_Index : constant Poss_Loop_Index_T :=
      Poss_Loop_Index_T'Pred (Loop_Index_T'First);
   --
   -- An "index" that means "no loop".


   type Loops_T is array (Loop_Index_T range <>) of Loop_T;
   --
   -- The loops of a flow graph.
   -- Ordered from inner loops to outer loops.


   type Loops_Ref is access Loops_T;


   Irreducible : exception;
   --
   -- Raised if the loop-finder detects that the structure of the
   -- given control-flow graph is not reducible, which means that
   -- a loop structure cannot be found (in this implementation).
   --
   -- In a reducible flow-graph, each loop has a single "head node"
   -- that is the only point of entry into the loop body. Moreover,
   -- two loops are either completely separate (no nodes in common)
   -- or one is completely nested within the other.



   -- PROVIDED OPERATIONS:


   function Loops (Flow_Graph : in Flow.Graph_T)
   return Loops_T;
   --
   -- Returns the loops in the given flow graph, if the graph is
   -- reducible. Otherwise, raises Irreducible


   function Head_Node (Item : Loop_T) return Flow.Node_T;
   --
   -- The head node of the loop.


   function Head_Step (Item : Loop_T) return Flow.Step_T;
   --
   -- The head step of the loop, which is the first step in the
   -- head node.


   function Head_Address (Item : Loop_T) return Processor.Code_Address_T;
   --
   -- The prime address of the Head_Step of the loop.


   function Members (Item : Loop_T) return Flow.Node_Set_Ref;
   --
   -- The members of the loop (including the head node).


   function Contains (
      Luup : Loop_T;
      Node : Flow.Node_T)
   return Boolean;
   --
   -- Whether the loop contains the node (including nested loops).
   -- A loop is considered to contain its loop head as well as the
   -- other nodes in the loop.


   function Max_Depth (Luups : Loops_T) return Natural;
   --
   -- The maximum depth of containment in the given set of Luups.
   -- If there are no loops, the depth is zero.
   -- If there are no nested loops, the depth is one.
   -- When there are loops within loops, the depth is greater than one.
   -- The depth does not depend on the total number of loops, only
   -- on the number of containment levels.


   function Entry_Edges (
      Into   : Loop_T;
      Within : Flow.Graph_T)
   return Flow.Edge_List_T;
   --
   -- The edges that enter the loop (head) from outside the loop.


   function Pre_Head_Steps (
      Before : Loop_T;
      Within : Flow.Graph_T)
   return Flow.Step_List_T;
   --
   -- The steps from which the loop can be entered, from outside
   -- the loop. That is, the source steps of the Entry_Edges.


   function Neck_Edges (
      Into   : Loop_T;
      Within : Flow.Graph_T)
   return Flow.Edge_List_T;
   --
   -- The edges from the loop head into the loop's body.
   -- Note that the head node is also considered part of the
   -- loop body, so the result may contain edges from the head
   -- to itself.


   function Internal_Edges (
      Inside : Loop_T;
      Within : Flow.Graph_T)
   return Flow.Edge_List_T;
   --
   -- The forward edges that are internal to the loop, which
   -- means all edges between nodes contained in the loop
   -- except for the repeat edges (edges from a node in the
   -- loop to the loop-head).


   function Edges_From (
      Node   : Flow.Node_T;
      Into   : Loop_T;
      Within : Flow.Graph_T)
   return Flow.Edge_List_T;
   --
   -- The edges from the given node to some node within the
   -- given loop. The given node may or may not also be
   -- in the given loop.


   function Repeat_Edges (
      Repeating : Loop_T;
      Within    : Flow.Graph_T)
   return Flow.Edge_List_T;
   --
   -- The repeat-edges of the loop, which are the edges from
   -- a node within the loop (including the loop-head) to the
   -- loop-head.


   function Exit_Edges (
      Exiting : Loop_T;
      Within  : Flow.Graph_T)
   return Flow.Edge_List_T;
   --
   -- The exit-edges of the loop, which are the edges from
   -- a node within the loop (including the loop-head) to some
   -- node outside the loop.


   function Eternal (Luup : Loop_T) return Boolean;
   --
   -- Whether the loop is eternal, in other words it has no
   -- exit edges.


   procedure Mark_As_Eternal (Luup : in out Loop_T);
   --
   -- Marks the Luup as eternal, overriding whatever property
   -- the Luup had when created. This operation is used when we
   -- discover that all the Luup's exit edges are in fact infeasible
   -- under some computation model.


   function Exits_At_End (
      Luup   : Loop_T;
      Within : Flow.Graph_T)
   return Boolean;
   --
   -- Whether all exits from the Luup occur at the end of the Luup.
   -- The condition here is that for any exit edge, all the other
   -- edges with the same source node as the exit edge are either
   -- exit edges or repeat edges in this Luup. This is (more or less)
   -- the same as the common term "bottom-test loop".


   function Forward_Edges (
      Within   : Flow.Graph_T;
      Avoiding : Loops_T)
   return Flow.Edge_List_T;
   --
   -- The forward edges in the graph, defined as any edge that
   -- is not a loop-repeat edge.


   function Forward_Edges (
      Within   : Flow.Graph_T;
      Avoiding : Loops_T)
   return Flow.Step_Edge_List_T;
   --
   -- The forward step-edges in the graph, defined as any step-edge
   -- that is not a loop-repeat edge.


   function All_Loops (From : Loops_T) return Loop_List_T;
   --
   -- Returns all the loops in the loop-structure, as a list.
   -- The list order is undefined in principle, but is from
   -- inner to outer in practice.


   function Head_Steps (Loops : Loop_List_T) return Flow.Step_List_T;
   --
   -- The head steps of the given Loops. The head-step of a loop is
   -- the first step in the Head_Node of the loop.


   function "<" (Left : Loop_T; Right : Loop_T) return Boolean;
   --
   -- Whether the Left loop is contained in the Right loop (and they
   -- are not the same loop).


   function Containing_Loops (
      Loops : Loop_List_T;
      Node  : Flow.Node_T)
   return Loop_List_T;
   --
   -- Return references to all the Loops that contain the given Node.
   -- The result is listed in top-down containment order, assuming
   -- that the given Loops list is in bottom-up containment order.
   -- The given Loops list need not contain all the loops in the
   -- relevant flow-graph.


   function Containing_Loop (
      Loops : Loops_T;
      Luup  : Loop_T)
   return Loop_List_T;
   --
   -- Return references to the loop on the next level higher
   -- that contains the parameter loop, if such a loop exists.
   -- ("next higher level" means that the returned loop contains
   -- the loop "Luup", but does not contain any other loop that
   -- contains "Luup".)
   -- Return empty if such a loop does not exist.


   function Root_Loops (Loops : Loops_T)
   return Loop_List_T;
   --
   -- The root loops, that is, the outermost loops that are not
   -- contained in any other loops and thus form the roots of
   -- the loop forest.


   function Loops_Contained_In (
      Loops : Loops_T;
      Luup  : Loop_T)
   return Loop_List_T;
   --
   -- Return references to the loops on the next level lower
   -- that are contained in the parameter loop, if such loops exists.
   -- ("next lower level" means that the returned loops are contained in
   -- the loop "Luup", but are not contained in any other loops that are
   -- contained in "Luup".)
   --
   -- Return empty if such loops do not exist.


   function Is_Loop_Head (
      Loops : Loops_T;
      Node  : Flow.Node_T)
   return Boolean;
   --
   -- Whether the Node is a loop head (for some loop of all
   -- the loops in Loops).


   function Loop_Headed (
      By    : Flow.Node_T;
      Among : Loops_T)
   return Loop_T;
   --
   -- The loop headed by the given node, which is assumed to be
   -- a loop-head of one of the given loops.
   -- If this assumption is false, Constraint_Error is raised.


   function Index (Luup : Loop_T)
   return Loop_Index_T;
   --
   -- Return the index value of a given loop.


   function Loop_Index (Luup : Loop_T)
   return Loop_Index_T
   renames Index;
   --
   -- Deprecated synonym for the Index function.


   function Steps_In (
      Luup   : Loop_T;
      Within : Flow.Graph_T)
   return Flow.Step_List_T;
   --
   -- All steps within the given loop.


   function Sorted_By_Address (List : Loop_List_T)
   return Loop_List_T;
   --
   -- The given List, sorted into ascending order by the step-tag
   -- of the loop-head step. This usually means sorting into increasing
   -- code address, but it depends on Processor."<" for Flow_State_T,
   -- which is used in Flow."<" for Step_Tag_T.


private


   type Loop_T is record
      Index   : Loop_Index_T;
      Head    : Flow.Node_T;
      Members : Flow.Node_Set_Ref;
      Card    : Flow.Node_Count_T;
      Eternal : Boolean;
      Depth   : Positive;
      Outer   : Poss_Loop_Index_T;
   end record;
   --
   -- One loop in the flow graph.
   --
   -- Index
   --    The identifying index of the loop (its index in a Loops_T).
   --    Increasing index order corresponds to the inner-to-outer
   --    containment order.
   -- Head
   --    The head node.
   -- Members
   --    The set of nodes that form the loop.
   --    The Head node is included, as are the nodes of inner loops
   --    if any.
   -- Card
   --    The number of Members (cardinality).
   -- Eternal
   --    Whether the loop is (structurally) eternal, meaning that it
   --    has no exit edges at all.
   -- Depth
   --    The nesting depth of the loop. One for an outermost loop,
   --    greater than one for inner loops.
   -- Outer
   --    The identifying index of the (next, containing) outer loop
   --    if Depth > 1. No_Loop_Index for an outermost loop.


end Loops;
