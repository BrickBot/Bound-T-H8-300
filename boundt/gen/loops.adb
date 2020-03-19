-- Loops (body)
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
-- $Revision: 1.39 $
-- $Date: 2015/10/24 20:05:49 $
--
-- $Log: loops.adb,v $
-- Revision 1.39  2015/10/24 20:05:49  niklas
-- Moved to free licence.
--
-- Revision 1.38  2009-03-20 18:19:29  niklas
-- BT-CH-0164: Assertion context identified by source-line number.
--
-- Revision 1.37  2008/01/14 20:27:29  niklas
-- BT-CH-0106: Loops.Max_Depth and other changes in Loops.
--
-- Revision 1.36  2007/12/17 13:54:38  niklas
-- BT-CH-0098: Assertions on stack usage and final stack height, etc.
--
-- Revision 1.35  2006/10/30 23:09:06  niklas
-- BT-CH-0033.
--
-- Revision 1.34  2006/10/24 21:41:05  niklas
-- BT-CH-0030.
--
-- Revision 1.33  2006/05/27 21:34:20  niklas
-- Renamed function Loop_Index to Index, for brevity, as
-- part of BT-CH-0020.
--
-- Revision 1.32  2006/02/04 09:22:11  niklas
-- Removed unused type Bit_Set_T.
--
-- Revision 1.31  2005/09/20 09:56:36  niklas
-- Added function Exits_At_End.
--
-- Revision 1.30  2005/09/03 11:50:29  niklas
-- BT-CH-0006.
--
-- Revision 1.29  2005/08/08 20:15:48  niklas
-- Added function Sorted_By_Address.
--
-- Revision 1.28  2005/08/08 17:40:08  niklas
-- Added the operator "<" to test loop containment.
-- Added the function Root_Loops.
-- Modified function Loops to apply a maximum iteration number
-- and optionally to trace the iterations.
--
-- Revision 1.27  2005/02/16 21:11:47  niklas
-- BT-CH-0002.
--
-- Revision 1.26  2004/04/28 19:34:04  niklas
-- Made the Irreducible exception public, for higher-level handling.
-- Added support for Eternal loops.
-- Added function Forward_Edges returning step-edge list.
-- Reimplemented the Loop_Members function to return a reference (access)
-- to a heap-allocated node-set rather than a node-set as such.
-- Clarified the structure of the Loops function.
--
-- Revision 1.25  2001/04/18 08:42:11  ville
-- Reducibility check implemented (NC_050)
--
-- Revision 1.24  2001/03/21 20:25:13  holsti
-- Steps_In corrected to include head node once.
--
-- Revision 1.23  2001/03/09 13:39:58  holsti
-- Edges_From added.
--
-- Revision 1.22  2001/03/06 09:15:27  holsti
-- Exit_Edges and Loop_Headed added.
--
-- Revision 1.21  2000/12/28 17:43:52  holsti
-- Is_Parent_Loop simplified and parameter names clarified.
-- Loops_Contained_In simplified.
--
-- Revision 1.20  2000/12/28 12:26:09  holsti
-- All_Loops added.
-- Unroll_Simple_Middle_Exits and some dead comments removed.
-- Minor formatting changes.
--
-- Revision 1.19  2000/12/22 13:34:02  sihvo
-- Added Steps_In.
--
-- Revision 1.18  2000/12/08 08:21:36  sihvo
-- Fixed NC 035, modified Containing_Loop and Is_Parent_Loop.
--
-- Revision 1.17  2000/12/05 15:43:56  holsti
-- The term "loop neck" replaces the overloaded "loop entry".
-- Decoder.Stack_Height_Cell replaces deleted Arithmetic function.
--
-- Revision 1.16  2000/09/19 12:09:15  langback
-- Added the function Loop_Index
--
-- Revision 1.15  2000/09/05 12:26:50  langback
-- Updated Loops_Contained_In. Renamed the second instance of
-- Containing_Loops to Containing_Loop, since it returns maximally
-- one loop after latest update.
--
-- Revision 1.14  2000/08/21 13:06:25  holsti
-- Edgeless graphs allowed (Edge_Count_T etc.)
--
-- Revision 1.13  2000/08/04 13:37:22  langback
-- Interface to Containing_Loops (one instance) and Loops_Contained_In
-- slightly modified
--
-- Revision 1.12  2000/08/04 11:18:39  langback
-- Added Is_Loop_Head function.
--
-- Revision 1.11  2000/07/17 21:00:54  holsti
-- Internal_Edges and Repeat_Edges added.
--
-- Revision 1.10  2000/07/16 18:39:53  holsti
-- Forward_Edges added.
--
-- Revision 1.9  2000/07/14 20:37:48  holsti
-- Moved Calculator-dependent parts to Loops.Slim.
--
-- Revision 1.8  2000/07/05 10:05:35  langback
-- Minor correction + Changes due to change of definition of Loops_T type
--
-- Revision 1.7  2000/07/04 12:06:25  holsti
-- Renamed several types.
--
-- Revision 1.6  2000/06/29 10:54:54  langback
-- Implemented the functions Loop_Edges and Entry_Edges
--
-- Revision 1.5  2000/06/28 15:08:22  langback
-- Added Loop_Index_T.
-- Added functions returning the Head node of a loop and Loop and Entry
-- edges of a given loop head.
--
-- Revision 1.4  2000/06/13 12:36:43  langback
-- First version with CVS headers included.
-- Still a preliminary version.
--


with Flow.Sort;
with Loops.Opt;
with Output;
with Processor;
with Quick_Sort_Function;   -- MW_Components


package body Loops is


   use type Flow.Node_Index_T;
   use type Flow.Node_Set_T;
   use type Flow.Node_Set_Ref;
   use type Flow.Node_T;


   function Head_Node (Item : Loop_T) return Flow.Node_T
   is
   begin

      return Item.Head;

   end Head_Node;


   function Head_Step (Item : Loop_T) return Flow.Step_T
   is
   begin

      return Flow.First_Step (Item.Head);

   end Head_Step;


   function Head_Address (Item : Loop_T) return Processor.Code_Address_T
   is
   begin

      return Flow.Prime_Address (Head_Step (Item));

   end Head_Address;


   function Members (Item : Loop_T) return Flow.Node_Set_Ref
   is
   begin
      return Item.Members;
   end Members;


   function Contains (
      Luup : Loop_T;
      Node : Flow.Node_T)
   return Boolean
   is
   begin
      return Luup.Members(Flow.Index(Node));
   end Contains;


   function Max_Depth (Luups : Loops_T) return Natural
   is

      Max : Natural := 0;
      -- The largest Depth seen so far.

   begin

      for L in Luups'Range loop

         Max := Natural'Max (Max, Luups(L).Depth);

      end loop;

      return Max;

   end Max_Depth;


   function Source_In (
      Luup : Loop_T;
      Edge : Flow.Edge_T)
   return Boolean
   --
   -- Whether the source of the edge is contained in the loop.
   --
   is
   begin
      return Contains (Luup, Flow.Source (Edge));
   end Source_In;


   function Target_In (
      Luup : Loop_T;
      Edge : Flow.Edge_T)
   return Boolean
   --
   -- Whether the target of the edge is contained in the loop.
   --
   is
   begin
      return Contains (Luup, Flow.Target (Edge));
   end Target_In;


   function Entry_Edges (
      Into   : Loop_T;
      Within : Flow.Graph_T)
   return Flow.Edge_List_T
   is

      Edges_Into : Flow.Edge_List_T :=
         Flow.Edges_Into (Node => Into.Head, Within => Within);
      -- All the edges entering the head node. We will return
      -- a subset of these edges.

      Entry_Edges : Flow.Edge_List_T (Edges_Into'Range);
      Counter : Natural := 0;
      --
      -- The edge subset will be accumulated into Entry_Edges.

   begin

      -- Loop through all the edges coming into the head node:

      for I in Edges_Into'Range loop
         -- Check if the source of the edge is in the loop:

         if not Source_In (Luup => Into, Edge => Edges_Into(I))
         then
            -- The edge comes from outside the loop, so it is an entry edge.
            -- Add it to the set of edges to be returned:
            Counter := Counter + 1;
            Entry_Edges(Counter) := Edges_Into(I);
         end if;
      end loop;

      return Entry_Edges(1 .. Counter);

   end Entry_Edges;


   function Pre_Head_Steps (
      Before : Loop_T;
      Within : Flow.Graph_T)
   return Flow.Step_List_T
   is
   begin

      return Flow.Sources (
         Edges  => Flow.Step_Edges (Entry_Edges (Before, Within)),
         Unique => True);

   end Pre_Head_Steps;


   function Neck_Edges (
      Into   : Loop_T;
      Within : Flow.Graph_T)
   return Flow.Edge_List_T
   is

      Edges_From : constant Flow.Edge_List_T :=
         Flow.Edges_From (Node => Into.Head, Within => Within);
      --
      -- All the edges leaving the head node. We will return
      -- a subset of these edges.

      Necks : Flow.Edge_List_T (Edges_From'Range);
      Counter : Natural := 0;
      --
      -- The edge subset will be accumulated into Necks.

   begin

      -- Loop through all the edges leaving the head node.

      for I in Edges_From'Range loop
         -- Check if the target of the edge is in the loop:

         if Target_In (Luup => Into, Edge => Edges_From(I))
         then
            -- The edge enters the loop, so it is a neck edge.
            -- Add it to the set of edges to be returned:
            Counter := Counter + 1;
            Necks(Counter) := Edges_From(I);
         end if;
      end loop;

      return Necks(1 .. Counter);

   end Neck_Edges;


   function Internal_Edges (
      Inside : Loop_T;
      Within : Flow.Graph_T)
   return Flow.Edge_List_T
   is
      use type Flow.Node_T;

      Last_Index : constant Flow.Edge_Count_T := Flow.Max_Edge (Within);
      -- The maximum edge-index value.

      Edge : Flow.Edge_T;
      -- An edge being considered.

      Internal : Flow.Edge_List_T (1 .. Natural(Last_Index));
      Last : Natural := 0;
      -- The edges to be returned are Internal(1 .. Last).

   begin

      for E in 1 .. Last_Index loop

         Edge := Flow.Edge_At (Index => E, Within => Within);

         if       Source_In (Luup => Inside, Edge => Edge)
         and then Target_In (Luup => Inside, Edge => Edge)
         and then Flow.Target (Edge) /= Inside.Head
         then
            -- The edge goes loop-node -> loop-node. and
            -- yet is not a repeat-edge. Ergo, an internal edge.

            Last := Last + 1;
            Internal(Last) := Edge;

         end if;

      end loop;

      return Internal(1 .. Last);

   end Internal_Edges;


   function Edges_From (
      Node   : Flow.Node_T;
      Into   : Loop_T;
      Within : Flow.Graph_T)
   return Flow.Edge_List_T
   is

      From : Flow.Edge_List_T := Flow.Edges_From (Node, Within);
      -- All the edges from the given node.

      Last : Natural := From'First - 1;
      -- The accepted edges are From(From'First .. Last).

   begin

      for F in From'Range loop

         if Target_In (Luup => Into, Edge => From(F)) then

            Last := Last + 1;
            From(Last) := From(F);

         end if;

      end loop;

      return From(From'First .. Last);

   end Edges_From;


   function Repeat_Edges (
      Repeating : Loop_T;
      Within    : Flow.Graph_T)
   return Flow.Edge_List_T
   is

      Into_Head : constant Flow.Edge_List_T :=
         Flow.Edges_Into (Node => Repeating.Head, Within => Within);
      -- All the edges into the head node.
      -- We will return a subset of these edges.

      Repeats : Flow.Edge_List_T (Into_Head'Range);
      Last : Natural := 0;
      -- The edges to be returned are Repeats(1 .. Last).

   begin

      for I in Into_Head'Range loop
         if Source_In (Luup => Repeating, Edge => Into_Head(I)) then
            -- The edge goes loop-node -> head, ergo a repeat edge.
            Last := Last + 1;
            Repeats(Last) := Into_Head(I);
         end if;
      end loop;

      return Repeats(1 .. Last);

   end Repeat_Edges;


   function Exit_Edges (
      Exiting : Loop_T;
      Within  : Flow.Graph_T)
   return Flow.Edge_List_T
   is
   begin

      if Exiting.Eternal then
         -- We know that there are no exit edges.

         return Flow.No_Edges;

      else
         -- There should be some exit edges.

         return Flow.Edges (
            From   => Exiting.Members.all,
            Into   => Exiting.Members.all, Not_Into => True,
            Within => Within);

      end if;
 
   end Exit_Edges;


   function Eternal (Luup : Loop_T) return Boolean
   is
   begin

      return Luup.Eternal;

   end Eternal;


   procedure Mark_As_Eternal (Luup : in out Loop_T)
   is
   begin

      Luup.Eternal := True;

   end Mark_As_Eternal;


   function Is_Repeat_Edge (
     Edge   : Flow.Edge_T;
     Repeats: Loop_T)
   return Boolean
   --
   -- Whether the given Edge is a repeat edge that Repeats the
   -- given loop.
   --
   is
   begin

      return Flow.Target (Edge) = Head_Node (Repeats);

   end Is_Repeat_Edge;


   function Is_Exit_Edge (
      Edge : Flow.Edge_T;
      From : Loop_T)
   return Boolean
   --
   -- Whether the given Edge is an exit edge that exits From the
   -- given loop.
   --
   is
   begin

      return  Contains (Luup => From, Node => Flow.Source (Edge))
      and not Contains (Luup => From, Node => Flow.Target (Edge));

   end Is_Exit_Edge;


   function Is_End_Exit (
      Exit_Edge : Flow.Edge_T;
      Luup      : Loop_T;
      Within    : Flow.Graph_T)
   return Boolean
   --
   -- Whether the given Exit_Edge is an "end exit" for the given Luup,
   -- that is, whether all the edges leaving the source-node of Exit_Edge
   -- are either exit edges of the Luup or repeat edges of the Luup.
   --
   is

      Siblings : constant Flow.Edge_List_T :=
         Flow.Edges_From (
            Node   => Flow.Source (Exit_Edge),
            Within => Within);
      -- All the edges with the same source as the Exit_Edge.
      -- This includes the Exit_Edge itself.

   begin

      for S in Siblings'Range loop

         if not (
               Is_Repeat_Edge (Siblings(S), Luup)
            or Is_Exit_Edge   (Siblings(S), Luup))
         then
            -- This edge is neither a repeat nor an exit.

            return False;

         end if;

      end loop;

      -- All Siblings (including the Exit_Edge itself) are
      -- either repeat edges or exit edges.

      return True;

   end Is_End_Exit;


   function Exits_At_End (
      Luup   : Loop_T;
      Within : Flow.Graph_T)
   return Boolean
   is

      Exits : constant Flow.Edge_List_T := Exit_Edges (Luup, Within);
      -- All the exit edges.

   begin

      for E in Exits'Range loop

         if not Is_End_Exit (Exits(E), Luup, Within) then
            -- This edge is not an end-exit.

            return False;

         end if;

      end loop;

      -- All exit edges are end-exits.

      return True;

   end Exits_At_End;


   function Forward_Edges (
      Within   : Flow.Graph_T;
      Avoiding : Loops_T)
   return Flow.Edge_List_T
   is

      Last_Index : constant Flow.Edge_Count_T := Flow.Max_Edge (Within);
      -- The maximum edge-index value.

      Include : array (1 .. Last_Index) of Boolean := (others => True);
      -- The indices of the edges to be included in the result.
      -- Initialised to "all edges"; the repeat-edges will be
      -- removed before returning.

      Forward : Flow.Edge_List_T (1 .. Natural(Last_Index));
      Last : Natural := 0;
      -- The edges to be returned are Forward(1 .. Last).


      procedure Avoid (Luup : in Loop_T)
      --
      -- Remove the repeat-edges of this loop from the
      -- included edge set.
      --
      is
         Repeats : constant Flow.Edge_List_T :=
            Repeat_Edges (Repeating => Luup, Within => Within);
         -- All the repeat-edges of this loop.
      begin
         -- Avoid (do not include) the repeat edges:
         for R in Repeats'Range loop
            Include(Flow.Index (Repeats(R))) := False;
         end loop;
      end Avoid;


   begin  -- Forward_Edges

      -- Scan the loops and black-ball the repeat edges:

      for A in Avoiding'Range loop
         Avoid (Luup => Avoiding(A));
      end loop;

      -- Pick the included edges:

      for I in Include'Range loop
         if Include(I) then
            Last := Last + 1;
            Forward(Last) := Flow.Edge_At (Index => I, Within => Within);
         end if;
      end loop;

      return Forward(1..Last);

   end Forward_Edges;


   function Forward_Edges (
      Within   : Flow.Graph_T;
      Avoiding : Loops_T)
   return Flow.Step_Edge_List_T
   is

      Last_Index : constant Flow.Step_Edge_Count_T :=
         Flow.Max_Step_Edge (Within);
      -- The maximum edge-index value.

      Include : array (1 .. Last_Index) of Boolean := (others => True);
      -- The indices of the edges to be included in the result.
      -- Initialised to "all edges"; the repeat-edges will be
      -- removed before returning.

      Forward : Flow.Step_Edge_List_T (1 .. Natural(Last_Index));
      Last : Natural := 0;
      -- The edges to be returned are Forward(1 .. Last).


      procedure Avoid (Luup : in Loop_T)
      --
      -- Remove the repeat-edges of this loop from the
      -- included edge set.
      --
      is

         Repeats : constant Flow.Edge_List_T :=
            Repeat_Edges (Repeating => Luup, Within => Within);
         -- All the (node-level) repeat-edges of this loop.

      begin

         -- Avoid (do not include) the step-edges that
         -- correspond to the node-level repeat edges:

         for R in Repeats'Range loop

            Include(Flow.Index (Flow.Step_Edge (Repeats(R)))) := False;

         end loop;

      end Avoid;


   begin  -- Forward_Edges

      -- Scan the loops and black-ball the repeat edges:

      for A in Avoiding'Range loop

         Avoid (Luup => Avoiding(A));

      end loop;

      -- Pick the included edges:

      for I in Include'Range loop

         if Include(I) then

            Last := Last + 1;

            Forward(Last) := Flow.Edge_At (Index => I, Within => Within);

         end if;

      end loop;

      return Forward(1..Last);

   end Forward_Edges;


   function All_Loops (From : Loops_T) return Loop_List_T
   is
      List : Loop_List_T (1 .. From'Length);
      -- The result.
   begin

      for F in From'Range loop
         List (Natural(F - From'First) + List'First) := From(F);
      end loop;

      return List;
   end All_Loops;


   function Loop_Members (
      Loop_Head    : Flow.Node_T;
      Repeat_Nodes : Flow.Node_List_T;
      Flow_Graph   : Flow.Graph_T)
   return Flow.Node_Set_Ref
   --
   -- Given a loop head node and the set of repeat nodes, this
   -- procedure collects and returns the set of nodes that constitute
   -- the loop. The set includes the loop head, too.
   --
   -- The input set, Repeat_Nodes, can contain the loop head, even if
   -- the head is not a repeat node - this has no effect on the result.
   --
   -- In practice this is an implementation based on the loop-body
   -- algorithm which is defined on page 604 in the Compiler book
   -- by Aho-Sethi-Ullman (1986).
   --
   is

      Members : Flow.Node_Set_Ref :=
         new Flow.Node_Set_T (1 .. Flow.Max_Node (Flow_Graph));
      -- Set in which to collect the nodes to be returned.


      procedure Insert (Node : in Flow.Node_T);
      --
      -- Inserts the Node as a loop member, if not already there,
      -- and recursively inserts its predecessors.


      procedure Insert (Nodes : in Flow.Node_List_T)
      --
      -- Calls Insert for each of the Nodes.
      --
      is
      begin

         for N in Nodes'Range loop

            Insert (Nodes(N));

         end loop;

      end Insert;


      procedure Insert (Node : Flow.Node_T)
      is

         Index : constant Flow.Node_Index_T := Flow.Index (Node);
         -- The index of this node.

      begin

         if not Members (Index) then
            -- A new member.

            Members(Index) := True;

            Insert (Flow.Predecessors (Node, Flow_Graph));

         end if;

      end Insert;


   begin  -- Loop_Members

      Members.all := (others => False);

      -- The loop-head is a member (but its predecessors are
      -- not necessarily members, so we cannot use the Insert
      -- operation here):

      Members(Flow.Index(Loop_Head)) := True;

      -- The repeat nodes are members, and so are their predecessors
      -- up to the loop-head, where the recursion stops because the
      -- loop-head is already marked as a member:

      Insert (Repeat_Nodes);

      -- And those are all the members:

      return Members;

   end Loop_Members;


   procedure Check_Reducibility (
      Graph         : in Flow.Graph_T;
      Forward_Edges : in Flow.Edge_List_T)
   --
   -- Checks the reducibility of the given graph using the
   -- given edge list.
   --
   -- The check includes two verifications: all nodes have to
   -- be reachable from the entry node via the given edges and
   -- the given edges must not form any cycles.
   --
   -- Raises Irreducible if the graph is irreducible.
   --
   is

      Reducible : Boolean := True;
      -- Whether the graph is reducible.
      -- So far no evidence to the contrary.

      Nodes : constant Flow.Node_List_T :=
         Flow.Sort.By_Flow(
            Elements => (1 => Flow.Entry_Node(Graph)),
            Pairs    => Forward_Edges);
      --
      -- Topologically sorted list of nodes contained in the graph,
      -- ordered by forward edges. If there are any cycles in the
      -- forward-edge graph the list does not contain all nodes
      -- which means that the graph was not reduced.

      Forward : array (
         Flow.Edge_Index_T range 1 .. Flow.Max_Edge(Graph)) of 
         Boolean := (others => False);
      -- Array of edges where the forward edges are true and
      -- others false.
      
      Reached : array (
         Flow.Node_Index_T range 1 .. Flow.Max_Node(Graph)) of
         Boolean := (others => False);
      -- Array of nodes where reachable nodes are true and
      -- others false.
         
         
      procedure Mark_Reached_Node (Node : in Flow.Node_T) is
      -- Marks the given node as reachable and calls itself
      -- recursively for any unmarked successor nodes that
      -- are targets of forward edges leaving from the given
      -- node.
      
         Leaving : constant Flow.Edge_List_T := 
            Flow.Edges_From (Node => Node, Within => Graph);
         -- List of edges leaving from the node.
            
      begin
      
         Reached(Flow.Index(Node)) := True;
         -- The node is reachable.
         
         for E in Leaving'range loop
         
            if (not Reached(Flow.Index(Flow.Target(Leaving(E)))))
            and Forward(Flow.Index(Leaving(E)))
            then
               Mark_Reached_Node(Flow.Target(Leaving(E)));
               -- Mark the target node.
            end if;
            
         end loop;
         
      end Mark_Reached_Node;
      
      
      function All_Nodes_Reached return Boolean is
      -- Returns true if all nodes are reachable and false
      -- otherwise.
      --
      begin
      
         for N in Reached'range loop         
            if not Reached(N) then
               return False;
            end if;            
         end loop;
         
         return True;
         
      end All_Nodes_Reached;
      
      
   begin  -- Check_Reducibility
   
      if Nodes'Length /= Flow.Max_Node(Graph) then
         -- The forward edges form one or more cycles in the
         -- graph and that means that the graph is irreducible.
         
         Reducible := False;
      
      else
      
         for E in Forward_Edges'range loop         
            Forward(Flow.Index(Forward_Edges(E))) := True;
         end loop;
         
         Mark_Reached_Node(Flow.Entry_Node(Graph));
         -- Mark recursevicely all nodes that are reachable via
         -- the forward edges.
         
         if not All_Nodes_Reached then
            -- All nodes was not reached via the forward edges and
            -- that means that the graph is irreducible.

            Reducible := False;
            
         end if;
         
      end if;

      if not Reducible then

         raise Irreducible;

      end if;

   end Check_Reducibility;


   function Sorted_Loops (Loops : Loop_List_T; Graph : Flow.Graph_T) 
   return Loops_T
   --
   -- The sorted loop-structure for the given loop-list.
   --
   -- Sorts the loops in order from inner loops to outer loops and
   -- sets the Index, Depth and Outer index of each loop accordingly.
   -- All other components of the Loops must already be set.
   --
   is
      use type Flow.Node_Count_T;

      --    Principles of Operation
      --
      -- An inner loop has a smaller cardinality than an outer loop
      -- that contains it. Thus, if we order loops by non-decreasing
      -- cardinality, we get an inner-to-outer order. But not always
      -- a very natural order.

      Sorted : Loops_T (1 .. Loops'Length);
      -- The given Loops in order of non-decreasing Cardinality.

      Last : Loop_Count_T := 0;
      -- The last Sorted element at present.

      Card : Flow.Node_Count_T;
      -- The cardinality of one of the Loops, for insertion in Sorted.

      Place : Loop_Index_T;
      -- Scanning through the Sorted vector.

      Head : Flow.Node_Index_T;
      -- The index of the head node of a loop.

   begin

      -- Sort Loops into Sorted by the insertion method:

      for L in Loops'Range loop

         Card := Loops(L).Card;

         Place := Last + 1;

         while    Place > Sorted'First
         and then Card  < Sorted(Place - 1).Card
         loop
            -- Sorted(Place) is a vacant place, but we cannot put
            -- this loop in Sorted(Place) because it has a smaller
            -- cardinality than the loop in Sorted(Place - 1).

            Sorted(Place) := Sorted(Place - 1);

            Place := Place - 1;

         end loop;

         -- Sorted(Place) is the place for this loop.

         Sorted(Place) := Loops(L);

         Last := Last + 1;

      end loop;

      -- Set up the Index, Depth and Outer for all loops:

      for S in Sorted'Range loop

         Sorted(S).Index := S;
         -- Final value.

         Sorted(S).Depth := 1;
         Sorted(S).Outer := No_Loop_Index;
         -- Initial values, updated below.

         Head := Flow.Index (Sorted(S).Head);

         for R in reverse S + 1 .. Sorted'Last loop

            if Sorted(R).Members(Head) then
               -- Sorted(R) is an outer loop for Sorted(S).

               Sorted(S).Depth := Sorted(S).Depth + 1;
               Sorted(S).Outer := R;

            end if;

         end loop;

      end loop;

      -- Are we happy?

      Check_Reducibility (
         Graph         => Graph, 
         Forward_Edges =>
            Forward_Edges (Within => Graph, Avoiding => Sorted)); 
      --
      -- May raise Irreducible.
      
      return Sorted;

   end Sorted_Loops;


   function Loops (Flow_Graph : in Flow.Graph_T)
   return Loops_T
   is

      Max_Node : constant Flow.Node_Index_T := Flow.Max_Node (Flow_Graph);
      -- The maximum node index in the graph.
      -- The nodes are indexed 1.. Max_Node.

      Num_Nodes : constant Positive := Positive (Max_Node);
      -- The number of nodes in the graph.

      subtype Node_Index_T is Flow.Node_Index_T range 1 .. Max_Node;
      -- Index of a graph node.

      Entry_Index : constant Node_Index_T :=
         Flow.Index (Flow.Entry_Node (Flow_Graph));
      -- The index of the entry node.

      subtype Node_Set_T is Flow.Node_Set_T (Node_Index_T);
      -- A set of graph nodes.

      type Node_Set_Map_T is array (Node_Index_T) of Node_Set_T;
      -- For each node (index), a set of graph nodes in some relation
      -- to the node,

      Dominators: Node_Set_Map_T := (others => (others => True));
      -- The dominator sets for all nodes in the graph.
      --
      -- For a node with index I, the set Dominators(I) will be
      -- the set of dominator nodes, that is those nodes that
      -- dominate node I.
      --
      -- In other words, the node with index J dominates the node
      -- with index I when Dominators(I)(J) is True.
      --
      -- Initially we consider that all nodes dominate one another.

      Repeat_Nodes : Node_Set_Map_T := (others => (others => False));
      -- For each (head) node the set of associated repeat nodes.
      --
      -- Thus, the node R is a repeat node for the loop-head node H if
      -- Repeat_Nodes(H)(R) is True.
      --
      -- If there are repeat nodes, the target node itself is also
      -- included in this set to make it easy to check if a node
      -- is a head.
      --
      -- Thus, node H is a loop-head if Repeat_Nodes(H)(H) is True.

      Luups : Loop_List_T (1 .. Num_Nodes);
      -- Collects the loops  in the order they are found.
      -- Potentially we have as many loops as there are nodes in
      -- the graph. Note that the Index component of the loops in
      -- Luups is not valid, because the loops are not yet sorted.

      Loop_Count : Natural := 0;
      -- Number of loops found. They are Luups(1 .. Loop_Count).


      procedure Winnow (
         Node    : in     Node_Index_T;
         Changed : in out Boolean)
      --
      -- Winnows (reduces) the dominator set for the given Node
      -- to include only the intersection of the dominator sets
      -- of the node's predecessors (plus the node itself, always).
      -- Sets Changed to True if the dominator set for Node was
      -- changed, otherwise returns Changed as it came.
      --
      is

         Preds : constant Flow.Node_List_T := 
            Flow.Predecessors (
               Node   => Flow.Node_At (Node, Flow_Graph),
               Within => Flow_Graph);
         -- The set of predecessors of this node.

         Pred : Node_Index_T;
         -- The index of a predecessor node.

         Section : Node_Set_T := (others => True);
         -- The intersection of the dominator sets of the
         -- predecessor nodes. Initialized to the neutral
         -- element for intersection.

      begin

         if Opt.Trace_Iteration then

            Output.Trace (
                 "Loops: winnowing dominators for node"
               & Flow.Node_Index_T'Image (Node));

         end if;

         -- Calculate the intersection of the predecessor dominators:

         for P in Preds'Range loop

            Pred := Flow.Index (Preds(P));

            Section := Section and Dominators(Pred);

        end loop;

        -- Ensure that the Node itself is always included:

        Section(Node) := True;

        -- Check for changes and update the dominator sets:

        if Section /= Dominators(Node) then

           if Opt.Trace_Iteration then

              Output.Trace (
                   "Loops: dominator set changed for node"
                 & Flow.Node_Index_T'Image (Node));

           end if;

           Changed := True;

           Dominators(Node) := Section;

        end if;

      end Winnow;


      procedure Find_Dominators
      --
      -- The dominator algorithm as defined (on page 671)
      -- in the Compiler book by Aho-Sethi-Ullman (1986).
      --
      -- We assume that the Dominators structure has been initialized
      -- to (others => (others => True)), and return the dominator
      -- sets in Dominators.
      --
      is

         Changed : Boolean;
         -- Whether some dominator elements changed in the last
         -- iteration. We iterate until the situation is stable.

         Max_Iterations : constant Natural := Num_Nodes * Num_Nodes;
         -- Each iteration will remove at least one node from some
         -- dominator set, until the last one where there is no change
         -- but which is not counted as an iteration. This number is
         -- thus an upper limit on the number of iterations.

         Iterations : Natural := 0;
         -- Counts the number of iterations.

      begin

         if Opt.Trace_Iteration then

            Output.Trace ("Loops: finding dominators.");

         end if;

         -- For the entry node E, dominators(E) := {E};

         Dominators(Entry_Index) := (others => False);

         Dominators(Entry_Index)(Entry_Index) := True;

         -- For every other node N, dominators(N) := all nodes in graph.
         -- This is the assumed initial value of the Dominators structure.

         -- Iteratively winnow (reduce) the dominator set for each
         -- node to include only the intersection of the dominator sets
         -- of the node's predecessors (plus the node itself, always).
         --
         -- The entry node's predecessor set is already final, so
         -- it is not modified.
         --
         -- The loop ends when there are no more changes in the
         -- dominator sets.

         loop

            Changed := False;

            for N in Node_Index_T loop

               if N /= Entry_Index then

                  Winnow (Node => N, Changed => Changed);

               end if;

            end loop;

            exit when not Changed;

            Iterations := Iterations + 1;

            if Iterations > Max_Iterations then

               Output.Fault (
                  Location => "Loops.Loops.Find_Dominators",
                  Text     =>
                       "Over"
                     & Natural'Image (Max_Iterations)
                     & " iterations for a graph with"
                     & Natural'Image (Num_Nodes)
                     & " nodes. Quitting.");

               exit;

            end if;

         end loop;

         Output.Note (
              "Dominator iterations"
            & Natural'Image (Iterations)
            & " for"
            & Natural'Image (Num_Nodes)
            & " graph nodes.");

      end Find_Dominators;


      procedure Check_Edge (Edge : Flow.Edge_T)
      --
      -- Checks whether an edge is a repeat edge using the
      -- dominator relationship (assumed complete).
      -- If so, adds the Source and Target to the Repeat_Nodes
      -- structure, showing that the Target is a loop-head and
      -- the Source is one of its repeat-nodes.
      --
      is
         Source : Node_Index_T := Flow.Index (Flow.Source (Edge));
         Target : Node_Index_T := Flow.Index (Flow.Target (Edge));
      begin

         if Dominators(Source)(Target) then
            -- Target dominates Source, so the edge is a repeat edge
            -- from the repeat-node Source to the loop-head Target.

            Repeat_Nodes(Target)(Target) := True;
            Repeat_Nodes(Target)(Source) := True;

         end if;

      end Check_Edge;


      function New_Loop (
         Head         : in Flow.Node_T;
         Repeat_Nodes : in Flow.Node_List_T)
      return Loop_T
      --
      -- A new loop with the given head-node, built from
      -- the Repeat_Nodes structure.
      --
      -- The Index component will not yet be valid, because the
      -- order in which loops are discovered here is not the final
      -- index order. The Index will be added in the Sorted_Loops
      -- function. Ditto for Depth and Outer.
      --
      is

         Members : constant Flow.Node_Set_Ref :=
            Loop_Members (
               Loop_Head    => Head,
               Repeat_Nodes => Repeat_Nodes,
               Flow_Graph   => Flow_Graph);
         --
         -- The nodes that belong to the loop, including the
         -- head node and nodes in inner, nested loops if any.

         Exits : constant Flow.Edge_List_T :=
            Flow.Edges (
               From   => Members.all,
               Into   => Members.all, Not_Into => True,
               Within => Flow_Graph);
         --
         -- The edges that exit the loop, just to know if
         -- the loop is eternal.

      begin

         return (
            Index   => Loop_Index_T'Last,
            Head    => Head,
            Members => Members,
            Card    => Flow.Cardinality (Members.all),
            Eternal => Exits'Length = 0,
            Depth   => 1,
            Outer   => No_Loop_Index);

      end New_Loop;


   begin  -- Loops

      -- First find the dominator sets:

      Find_Dominators;

      -- Then find the repeat edges:

      for E in 1 .. Flow.Max_Edge (Flow_Graph) loop

         Check_Edge (Flow.Edge_At (E, Flow_Graph));

      end loop;

      -- Now find the loop-head nodes and create the loops:

      for N in Node_Index_T loop

         if Repeat_Nodes(N)(N) then
            -- Node N is a loop head. We have found a new loop.

            Loop_Count := Loop_Count + 1;

            Luups(Loop_Count) :=
               New_Loop (
                  Head         => Flow.Node_At (N, Flow_Graph),
                  Repeat_Nodes =>
                     Flow.To_List (
                        Set  => Repeat_Nodes(N),
                        From => Flow_Graph));

         end if;

      end loop;

      -- Return the loops sorted into hierarchical order:

      return Sorted_Loops (Luups(1 .. Loop_Count), Flow_Graph);

   end Loops;


   function Head_Steps (Loops : Loop_List_T) return Flow.Step_List_T
   is

      Heads : Flow.Step_List_T (Loops'Range);
      -- The result.

   begin

      for L in Loops'Range loop

         Heads(L) := Flow.First_Step (Node => Loops(L).Head);

      end loop;

      return Heads;

   end Head_Steps;


   function "<" (Left : Loop_T; Right : Loop_T) return Boolean
   is
      use type Flow.Node_T;
   begin

      return Left.Head /= Right.Head
         and Contains (Right, Left.Head);

   end "<";


   function Containing_Loops(
      Loops : Loop_List_T;
      Node  : Flow.Node_T)
   return Loop_List_T
   is

      Loop_List : Loop_List_T (1 .. Loops'Length);
      -- The list of loops to be returned.

      Num : Natural := 0;
      -- The number of nodes that have been stored
      -- in the list of nodes to be returned.

   begin

      for I in reverse Loops'Range loop

         -- Since the loops are sorted bottom up in the Loops structure,
         -- reversal of the order will give us the desired top-down order.

         if Contains (Loops(I), Node) then

            Num := Num + 1;

            Loop_List(Num) := Loops(I);

         end if;

      end loop;

      return Loop_List(1 .. Num);

   end Containing_Loops;


   function Containing_Loop (
      Loops : Loops_T;
      Luup  : Loop_T)
   return Loop_List_T
   is
   begin

      if Luup.Depth = 1 then
         -- An outermost loop.

         return (1 .. 0 => Luup);

      else
         -- An inner loop.

         return (1 .. 1 => Loops(Luup.Outer));

      end if;

   end Containing_Loop;


   function Root_Loops (Loops : Loops_T)
   return Loop_List_T
   is

      Result : Loop_List_T (1 .. Loops'Length);
      Last   : Natural := 0;
      -- The result will be Result(1 .. Last).

   begin

      for L in Loops'Range loop

         if Loops(L).Depth = 1 then
            -- This is an outermost, root loop.

            Last := Last + 1;

            Result(Last) := Loops(L);

         end if;

      end loop;

      return Result(1 .. Last);

   end Root_Loops;


   function Is_Parent_Loop (
      Parent : Loop_T;
      Child  : Loop_T)
   return Boolean
   --
   -- Whether "Parent" is the "next level" parent of "Child", that is
   -- the next outer loop that contains Child.
   --
   is
   begin

      return   Child.Depth > 1
      and then Child.Outer = Parent.Index;

   end Is_Parent_Loop;


   function Loops_Contained_In (
      Loops : Loops_T;
      Luup  : Loop_T)
   return Loop_List_T
   is

      Loop_List : Loop_List_T (1 .. Loops'Length);
      -- The list of loops to be returned.

      Num : Natural := 0;
      -- The number of loops that have been stored in Loop_List.

   begin

      -- All the inner loops (if any) have smaller loop-index than
      -- the outer loop, so we scan the loops with smaller index:

      for I in Loops'First .. Luup.Index - 1 loop

         if Is_Parent_Loop (
            Parent => Luup,
            Child  => Loops(I))
         then
            Num := Num + 1;
            Loop_List(Num) := Loops(I);
         end if;

      end loop;

      return Loop_List(1 .. Num);

   end Loops_Contained_In;


   function Is_Loop_Head (
      Loops : Loops_T;
      Node  : Flow.Node_T)
   return Boolean
   is
   begin

      for I in Loops'Range loop

         if Loops(I).Head = Node then

            return True;

         end if;

      end loop;

      return False;

   end Is_Loop_Head;


   function Loop_Headed (
      By    : Flow.Node_T;
      Among : Loops_T)
   return Loop_T
   is
   begin

      for I in Among'Range loop

         if Among(I).Head = By then

            return Among(I);

         end if;

      end loop;

      -- Oops - the node is not a head.

      raise Constraint_Error;

   end Loop_Headed;


   function Index (Luup : Loop_T)
   return Loop_Index_T
   is
   begin

      return Luup.Index;

   end Index;


   function Steps_In ( 
      Luup   : Loop_T;
      Within : Flow.Graph_T)
   return Flow.Step_List_T 
   is

      Store : Flow.Step_List_T (1 .. Natural(Flow.Max_Step(Within)));
      Num : Natural := 0;
      --
      -- The list of steps to be returned. Indices 1 .. Num are used.

      Nodes : constant Flow.Node_List_T := 
         Flow.To_List (Set  => Luup.Members.all, 
                       From => Within);
      --
      -- All nodes in the loop (including head).

   begin

      -- Add steps of the member nodes to the list.

      for L in Nodes'Range loop

         declare
            Steps : Flow.Step_List_T := Flow.Steps_In (Nodes(L));
         begin
            Store (Num+1 .. Num+Steps'Length) := Steps;
            Num := Num + Steps'Length;
         end;

      end loop;
      
      return Store (1 .. Num);
      
   end Steps_In;


   function Earlier_Tag (X, Y : Loop_T) return Boolean
   --
   -- Whether the step-tag of the head-step of X is less than
   -- that of the head-step of Y. For ordering loops by address.
   --
   is
   begin

      return Flow.Tag_Less (Head_Step (X), Head_Step (Y));

   end Earlier_Tag;


   function Quick_Sort_By_Tag
   is new Quick_Sort_Function (
      Index  => Positive,
      Item   => Loop_T,
      Vector => Loop_List_T,
      "<"    => Earlier_Tag);


   function Sorted_By_Address (List : Loop_List_T)
   return Loop_List_T
   renames Quick_Sort_By_Tag;


end Loops;
