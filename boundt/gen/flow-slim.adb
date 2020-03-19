-- Flow.Slim (body)
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
-- $Revision: 1.15 $
-- $Date: 2015/10/24 20:05:48 $
--
-- $Log: flow-slim.adb,v $
-- Revision 1.15  2015/10/24 20:05:48  niklas
-- Moved to free licence.
--
-- Revision 1.14  2009-10-07 19:26:10  niklas
-- BT-CH-0183: Cell-sets are a tagged-type class.
--
-- Revision 1.13  2009-01-18 08:08:27  niklas
-- Moved context clause to body. Removed unused locals.
--
-- Revision 1.12  2007/10/28 09:32:46  niklas
-- BT-CH-0092: Arithmetic analysis of dynamic data refs is optional.
--
-- Revision 1.11  2007/03/29 15:18:03  niklas
-- BT-CH-0056.
--
-- Revision 1.10  2007/02/13 20:22:07  Niklas
-- BT-CH-0044.
--
-- Revision 1.9  2007/01/25 21:25:16  niklas
-- BT-CH-0043.
--
-- Revision 1.8  2006/02/27 09:30:00  niklas
-- Added functions Edges_Into and Edges_From.
--
-- Revision 1.7  2005/02/16 21:11:45  niklas
-- BT-CH-0002.
--
-- Revision 1.6  2004/04/26 20:20:00  niklas
-- First Tidorum version.
-- Taking Cell_T stuff from Storage instead of Arithmetic.
--
-- Revision 1.5  2001/03/16 14:56:53  holsti
-- Cells_Accessed uses only Bare edges.
--
-- Revision 1.4  2001/03/16 11:14:07  holsti
-- Cells_Accessed includes edge conditions (NC_045).
--
-- Revision 1.3  2000/08/21 13:07:27  holsti
-- Edgeless graphs allowed (Max_Edge return Edge_Count_T).
--
-- Revision 1.2  2000/07/16 13:12:47  holsti
-- Fused nodes (and edges) are deleted from the slimmed graph.
--
-- Revision 1.1  2000/07/14 20:35:07  holsti
-- First version.
--


with Faults;
with Flow.Computation;
with Flow.Slim.Opt;
with Output;
with Storage.Bitvec_Cell_Sets;
with Unchecked_Deallocation;


package body Flow.Slim is

   --
   -- Graph object type:
   --

   type Nodes_T is array (Node_Index_T range <>) of Node_T;
   --
   -- All the nodes of slimmed graph.


   type Edges_T is array (Edge_Index_T range <>) of Edge_T;
   --
   -- All the edges of a slimmed graph.


   type Graph_Object_T (
      Max_Node : Node_Index_T;
      Max_Edge : Edge_Count_T)
   is record
      Original : Flow.Graph_T;
      Living   : Life.Living_T;
      Model    : Computation.Model_Ref;
      Nodes    : Nodes_T (1 .. Max_Node);
      Edges    : Edges_T (1 .. Max_Edge);
   end record;
   --
   -- The main structure of a slimmed graph.
   --
   -- Original
   --    The original graph on which this slimmed graph is based.
   --    Equals Flow.Computation.Graph (Model).
   --
   -- Living
   --    The set of "live" assignments and the computation model
   --    associated with the Original graph and the slimmed graph.
   --    Note that Living_T is a reference type.
   --
   -- Model
   --    The computation model that defines the feasible part of
   --    the Original graph and the arithmetic effects and conditions
   --    in the graph. Model equals Flow.Life.Model (Living).
   --
   -- Nodes
   --    All the nodes in this graph. Even nodes that have been
   --    fused are represented, but their Fusion components point
   --    to the same Fusion_T object. Nodes that are infeasible
   --    under the computation model or that have been fused are
   --    not considered to exist in the slimmed graph. The "proxy"
   --    node that stands for a set of fused nodes does exist.
   --
   -- Edges
   --    All the edges in this graph, however infeasible edges or
   --    those of the Deleted kind shall not be considered as
   --    existing edges.


   procedure Unchecked_Discard is new Unchecked_Deallocation (
      Object => Graph_Object_T,
      Name   => Graph_T);


   procedure Free (Item : in out Graph_T)
   --
   -- Discards the Item with optional Deallocate.
   --
   is
   begin

      if Opt.Deallocate then

         Unchecked_Discard (Item);

      else

         Item := null;

      end if;

   exception when others => Faults.Deallocation;

   end Free;


   function Max_Node (Graph : Graph_T) return Node_Index_T
   is
   begin

      return Graph.Max_Node;

   end Max_Node;


   function Max_Edge (Graph : Graph_T) return Edge_Count_T
   is
   begin

      return Graph.Max_Edge;

   end Max_Edge;


   function Living (Graph : Graph_T) return Life.Living_T
   is
   begin

      return Graph.Living;

   end Living;


   --
   -- Fusion object:
   --


   type Edge_Index_List_T is array (Positive range <>) of Edge_Index_T;
   --
   -- For the lists of incoming and outgoing edges of a fusion.


   type Fusion_T (
      Number_Into : Natural;
      Number_From : Natural)
   is record
      Index   : Node_Index_T;
      Flux    : Calculator.Flux_T;
      Into    : Edge_Index_List_T (1 .. Number_Into);
      From    : Edge_Index_List_T (1 .. Number_From);
   end record;
   --
   -- A fusion node.
   --
   -- Index
   --    The index assigned to this fusion. It will be one of
   --    indices of the nodes that were fused.
   --
   -- Flux
   --    The flux of the fusion (see function Flux).
   --
   -- Into, From
   --    The (indices of) edges entering and leaving the fusion node.
   --    All the From edges will normally be Fluxed (when the fusion is
   --    completed by Set_Flux operations).
   --    The Into edged may be Bare or Fluxed, depending on their
   --    source node.


   procedure Unchecked_Discard is new Unchecked_Deallocation (
      Object => Fusion_T,
      Name   => Fusion_Ref);


   procedure Free (Item : in out Fusion_Ref)
   --
   -- Discards the Item with optional Deallocate.
   --
   is
   begin

      if Opt.Deallocate then

         Unchecked_Discard (Item);

      else

         Item := null;

      end if;

   exception when others => Faults.Deallocation;

   end Free;


   procedure Free (Node : in out Node_T)
   --
   -- Frees any dynamic storage allocated for this node.
   --
   is
   begin

      if Node.Exists and then Node.Kind = Fusion then

         Free (Node.Fusion);

      end if;

   end Free;


   procedure Check_That (Exists : in Boolean)
   --
   -- Ensure that we access only existing data.
   --
   is
   begin

      if not Exists then

         Output.Fault (
            Location => "Flow.Slim.Check_That",
            Text     => "Access deleted graph element");

         raise No_Such_Node;
         -- Could also be No_Such_Edge.

      end if;

   end Check_That;


   function Index (Node : Node_T) return Node_Index_T
   is
   begin

      Check_That (Node.Exists);

      case Node.Kind is
      when Basic  => return Flow.Index (Node.Basic);
      when Fusion => return Node.Fusion.Index;
      end case;

   end Index;


   function Node_At (
      Index  : Node_Index_T;
      Within : Graph_T)
   return Node_T
   is
      Node : Node_T;
      -- The result.
   begin

      Node := Within.Nodes(Index);

      if not Node.Exists then

         raise No_Such_Node;

      end if;

      return Node;

   end Node_At;


   function Basic (Node : Basic_Node_T) return Flow.Node_T
   is
   begin

      Check_That (Node.Exists);

      return Node.Basic;

   end Basic;


   function Flux (Node : Fusion_Node_T) return Calculator.Flux_T
   is
   begin

      Check_That (Node.Exists);

      return Node.Fusion.Flux;

   end Flux;


   --
   -- Edge operations:
   --


   function Index (Edge : Edge_T) return Edge_Index_T
   is
   begin

      Check_That (Edge.Exists);

      case Edge.Kind is
      when Bare    => return Flow.Index (Edge.Bare);
      when Fluxed  => return Edge.Index;
      end case;

   end Index;


   function Edge_At (
      Index  : Edge_Index_T;
      Within : Graph_T)
   return Edge_T
   is
      Edge : Edge_T;
      -- The result.
   begin

      Edge := Within.Edges(Index);

      if not Edge.Exists then

         raise No_Such_Edge;

      end if;

      return Edge;

   end Edge_At;


   function Source (Edge : Edge_T) return Node_T
   is
   begin

      Check_That (Edge.Exists);

      return Edge.Graph.Nodes(Edge.Source);

   end Source;


   function Target (Edge : Edge_T) return Node_T
   is
   begin

      Check_That (Edge.Exists);

      return Edge.Graph.Nodes(Edge.Target);

   end Target;


   function Condition (Edge : Bare_Edge_T)
   return Arithmetic.Condition_T
   is
   begin

      Check_That (Edge.Exists);

      return Flow.Computation.Condition (
         Edge  => Step_Edge (Edge.Bare),
         Under => Edge.Graph.Model);

   end Condition;


   function Flux (Edge : Fluxed_Edge_T)
   return Calculator.Flux_T
   is
   begin

      Check_That (Edge.Exists);

      return Edge.Flux;

   end Flux;


   function Edges_At (
      Indices : Edge_Index_List_T;
      Within  : Graph_T)
   return Edge_List_T
   --
   -- The edges at the given indices within the given graph.
   --
   is

      Edges : Edge_List_T (Indices'Range);
      -- The result.

   begin

      for I in Indices'Range loop

         Edges(I) := Edge_At (Indices(I), Within);

      end loop;

      return Edges;

   end Edges_At;


   function Slim_Edges ( 
      Edges  : Flow.Edge_List_T;
      Within : Graph_T)
   return Edge_List_T
   --
   -- The edges in the slimmed graph that correspond to (have the
   -- same indices as) the given edges in the original graph.
   --
   is

      Slims : Edge_List_T (Edges'Range);
      -- The result.

   begin

      for E in Edges'Range loop

         Slims(E) := Edge_At (Flow.Index (Edges(E)), Within);

      end loop;

      return Slims;

   end Slim_Edges;


   function Edges_Into (Node : Node_T; Within : Graph_T)
   return Edge_List_T
   is
   begin

      Check_That (Node.Exists);

      case Node.Kind is

      when Basic =>

         return Slim_Edges (
            Edges  => Flow.Edges_Into (Node.Basic, Within.Original),
            Within => Within);

      when Fusion =>

         return Edges_At (Node.Fusion.Into, Within);

      end case;

   end Edges_Into;


   function Edges_From (Node : Node_T; Within : Graph_T)
   return Edge_List_T
   is
   begin

      Check_That (Node.Exists);

      case Node.Kind is

      when Basic =>

         return Slim_Edges (
            Edges  => Flow.Edges_From (Node.Basic, Within.Original),
            Within => Within);

      when Fusion =>

         return Edges_At (Node.Fusion.From, Within);

      end case;

   end Edges_From;


   --
   -- GRAPH CONSTRUCTION AND SLIMMING
   --


   procedure Make_Graph (
      Model  : in     Flow.Computation.Model_Ref;
      Graph  :    out Graph_T)
   --
   -- Creates a slimmed graph based on a computation Model and fills
   -- the Node and Edge lists of the Graph with the nodes and edges
   -- that are feasible under the Model.
   --
   is

      Source : constant Flow.Graph_T := Computation.Graph (Model);
      -- The underlying ordinary flow-graph.

      Node : Flow.Node_T;
      -- A node in the Source graph.

      Edge : Flow.Edge_T;
      -- An edge in the Source graph.

   begin

      Graph := new Graph_Object_T (
         Max_Node => Flow.Max_Node (Source),
         Max_Edge => Flow.Max_Edge (Source));

      Graph.Original := Source;

      -- Graph.Living is not yet assigned.

      Computation.Refer (
         From => Graph.Model,
         To   => Model);

      for N in Graph.Nodes'Range loop

         Node := Flow.Node_At (N, Source);

         Graph.Nodes(N) := (
            Kind   => Basic,
            Exists => Computation.Is_Feasible (Node, Model),
            Basic  => Node);

      end loop;

      for E in Graph.Edges'Range loop

         Edge := Flow.Edge_At (E, Source);

         Graph.Edges(E) := (
            Kind   => Bare,
            Exists => Computation.Is_Feasible (Edge, Model),
            Graph  => Graph,
            Source => Flow.Index (Flow.Source (Edge)),
            Target => Flow.Index (Flow.Target (Edge)),
            Bare   => Edge);

      end loop;

   end Make_Graph;


   function Entire_Graph (
      Living : Flow.Life.Living_T)
   return Graph_T
   is

      Graph : Graph_T;
      -- The new graph.

   begin

      Make_Graph (
         Model => Life.Model (Living).all,
         Graph => Graph);

      Graph.Living := Living;

      return Graph;

   end Entire_Graph;


   procedure Destroy (Graph : in out Graph_T)
   is
   begin

      -- Discard the reference to the computation model:

      Computation.Discard (Graph.Model);

      -- Release all the Fusions:

      for N in Graph.Nodes'Range loop

         Free (Graph.Nodes(N));

      end loop;

      -- Release the slimmed graph object:

      Free (Graph);

      -- The reference to the associated Flow.Life.Living_T is
      -- not a counted one, so it silently disappears.

   end Destroy;


   procedure Fuse (
      Nodes   : in     Node_Set_T;
      Proxy   : in     Node_Index_T;
      Flux    : in     Calculator.Flux_T;
      Within  : in out Graph_T)
   is

      Indices : constant Flow.Node_Index_List_T := To_Index_List (Nodes);
      -- The indices of the nodes to be fused.

      Num_Into : Natural := 0;
      Num_From : Natural := 0;
      -- The number of edges entering and leaving the fusion.

      Edges_Into : Edge_Index_List_T (1 .. Within.Edges'Length);
      -- The edges (indices) entering and leaving the fusion
      -- are Edges_Into (1 .. Num_Into).

      Edges_From : Edge_Index_List_T (1 .. Within.Edges'Length);
      -- The edges (indices) entering and leaving the fusion
      -- are Edges_Into (1 .. Num_From).

      Fusion : Fusion_Ref;
      -- The result.


      procedure Fuse_Edge (Edge : in out Edge_T)
      --
      -- Check if the edge is involved in the fusion.
      -- Delete edges internal to the fusion.
      -- Accumulate edges entering and leaving the fusion and
      -- update their target or source index, respectively.
      --
      is
      begin

         if Nodes(Edge.Source) and Nodes(Edge.Target) then

            -- The edge is internal to the fusion.

            Edge.Exists := False;

         elsif Nodes(Edge.Source) then

            -- The edge leaves the fusion.

            Num_From := Num_From + 1;
            Edges_From (Num_From) := Index (Edge);

            Edge.Source := Proxy;

         elsif Nodes(Edge.Target) then

            -- The edge enters the fusion.

            Num_Into := Num_Into + 1;
            Edges_Into (Num_Into) := Index (Edge);

            Edge.Target := Proxy;

         end if;

      end Fuse_Edge;


      procedure Fuse_Node (Index : in Node_Index_T)
      --
      -- Given the Index of one of the fused nodes, update the
      -- node to mark it as part of this fusion.
      --
      is

         Node : Node_T renames Within.Nodes(Index);
         -- The node to be updated.

      begin

         if Node.Exists then

            Free (Node);
            -- Free the earlier Fusion object, if any.

            if Index = Proxy then
               -- This will be the new fusion node:

               Node := (
                  Kind   => Flow.Slim.Fusion,
                  Exists => True,
                  Fusion => Fusion);

            else
               -- Delete fused node:

               Node.Exists := False;

            end if;

         end if;

      end Fuse_Node;
         

   begin  -- Fuse

      -- Check Proxy validity:

      if not Nodes(Proxy) then

         Output.Fault (
            Location => "Flow.Slim.Fuse",
            Text     => "Proxy is not in fused set");

         raise Constraint_Error;

      end if;

      -- Scan and update the edges:

      for E in Within.Edges'Range loop

         if Within.Edges(E).Exists then

            Fuse_Edge (Edge => Within.Edges(E));

         end if;

      end loop;

      -- Create the fusion object:

      Fusion := new Fusion_T'(
         Number_Into => Num_Into,
         Number_From => Num_From,
         Index       => Proxy,
         Flux        => Flux,
         Into        => Edges_Into (1 .. Num_Into),
         From        => Edges_From (1 .. Num_From));

      -- Scan the fused nodes and delete them all, except
      -- for the Proxy which will become the fusion:

      for F in Indices'Range loop

         Fuse_Node (Index => Indices(F));

      end loop;

   end Fuse;


   procedure Set_Flux (
      Edge   : in     Edge_Index_T;
      Flux   : in     Calculator.Flux_T;
      Within : in out Graph_T)
   is

      E : Edge_T renames Within.Edges(Edge);
      -- The edge to be updated.

   begin

      Check_That (E.Exists);

      -- Check that the edge source is a fusion (this is
      -- a feature of the way Bound-T uses fusion, and is
      -- thus to be deleted if this package is used in other
      -- ways):

      Check_That (Within.Nodes(E.Source).Exists);

      if Within.Nodes(E.Source).Kind /= Fusion then

         Output.Fault (
            Location => "Flow.Slim.Set_Flux",
            Text     =>
                 "Edge" & Edge_Index_T'Image (Edge)
               & ", source (node"
               & Node_Index_T'Image(E.Source)
               & ") is not a fusion"); 

      end if;

      -- Update the edge:

      case E.Kind is

      when Bare =>

         -- Transform E from Bare to Fluxed:

         E := (
            Kind   => Fluxed,
            Exists => True,
            Graph  => E.Graph,
            Source => E.Source,
            Target => E.Target,
            Index  => Flow.Index (E.Bare),
            Flux   => Flux);

      when Fluxed =>

         -- Just update the flux:

         E.Flux := Flux;

      end case;

   end Set_Flux;


   function Edges (
      From   : Node_Set_T;
      Into   : Node_Set_T;
      Within : Graph_T)
   return Edge_List_T
   is

      List : Edge_List_T (1 .. Within.Edges'Length);
      Last : Natural := 0;
      -- The result will be List(1 .. Last).

   begin

      for E in Within.Edges'Range loop

         declare
            Edge : Edge_T renames Within.Edges(E);
         begin

            if Edge.Exists
            and then (From(Edge.Source) and Into(Edge.Target))
            then
               -- This edge is acceptable.

               Last := Last + 1;
               List(Last) := Edge;

            end if;

         end;

      end loop;

      return List(1 .. Last);

   end Edges;


   function Cells_Accessed (
      By     : Node_Set_T;
      Within : Graph_T)
   return Storage.Cell_Set_T
   is
      use Storage.Bitvec_Cell_Sets;

      Accessed : Set_T;
      -- The result under construction.
      -- Initially empty.


      procedure Add_From (Edge : in Edge_T)
      --
      -- Adds the cells accessed by the precondition of the Edge,
      -- if the Edge is bare. Basis cells for boundable memory
      -- references are included.
      --
      -- Precondition: The Edge exists (and Elvis is alive).
      --
      -- For Fluxed edges, it can be assumed that the cells in
      -- the flux also occur in the the source node, and are
      -- therefore included in the result of Cells_Accessed for
      -- that reason so the Fluxed edges can be ignored here.
      --
      is
      begin

         if Edge.Kind = Bare then

            Arithmetic.Add_Cells_Used (
               By => Flow.Computation.Condition (
                  Edge  => Step_Edge (Edge.Bare),
                  Under => Within.Model),
               Refs => True,
               To   => Accessed);

         end if;

      end Add_From;


      procedure Add_From (Node : in Node_T)
      --
      -- Adds the cells accessed by this node.
      --
      is
      begin

         case Node.Kind is

         when Basic =>

            Flow.Life.Add_Cells_Accessed (
               By     => Node.Basic,
               Living => Within.Living,
               To     => Accessed);

         when Fusion =>

            Storage.Mixed.Add (
               Cells => Calculator.Cells_Of (Node.Fusion.Flux),
               To    => Accessed);

         end case;

      end Add_From;


   begin  -- Cells_Accessed

      -- Scan the nodes in the set:

      for N in Within.Nodes'Range loop

         if Within.Nodes(N).Exists
         and then By(N)
         then

            Add_From (Node => Within.Nodes(N));

         end if;

      end loop;

      -- Scan the edges leaving the nodes in the set:

      for E in Within.Edges'Range loop

         if Within.Edges(E).Exists
         and then By(Within.Edges(E).Source)
         then

            Add_From (Edge => Within.Edges(E));

         end if;

      end loop;

      -- And that's all folks.

      return Accessed;

   end Cells_Accessed;


end Flow.Slim;

