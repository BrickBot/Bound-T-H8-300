-- Calculator.Slim (body)
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
-- $Date: 2015/10/24 20:05:47 $
--
-- $Log: calculator-slim.adb,v $
-- Revision 1.11  2015/10/24 20:05:47  niklas
-- Moved to free licence.
--
-- Revision 1.10  2009-10-07 19:26:10  niklas
-- BT-CH-0183: Cell-sets are a tagged-type class.
--
-- Revision 1.9  2009-01-18 08:13:24  niklas
-- Removed unused context clause.
--
-- Revision 1.8  2005/02/16 21:11:41  niklas
-- BT-CH-0002.
--
-- Revision 1.7  2004/05/02 05:34:27  niklas
-- First Tidorum version.
-- Taking Cell_T stuff from Storage, not from Arithmetic.
-- Using the new function Calculator.Formulas.Adapted to match bases.
--
-- Revision 1.6  2001/01/07 22:06:04  holsti
-- Parameters for live-cell analysis added.
--
-- Revision 1.5  2001/01/04 16:12:28  saarinen
-- Fixed NC_022.
--
-- Revision 1.4  2000/12/29 14:40:16  holsti
-- Symbolic-constant usage deleted (part of NC_078).
--
-- Revision 1.3  2000/08/17 13:01:47  holsti
-- Changed cell-lists to cell-sets.
--
-- Revision 1.2  2000/07/16 13:12:48  holsti
-- Fused nodes (and edges) are deleted from the slimmed graph.
--
-- Revision 1.1  2000/07/14 20:34:12  holsti
-- First version.
--


with Calculator.Formulas;
with Calculator.Propagator;
with List_Filters;
with Topo_Sort;


package body Calculator.Slim is


   use Calculator.Formulas;

   --
   -- Topo sort instance for ordering the nodes of an slimmed
   -- acyclic region into data-flow order.
   --

   function Flow_Sort is
      new Topo_Sort (
         Element      => Flow.Slim.Node_T,
         Pair         => Flow.Slim.Edge_T,
         Element_List => Flow.Slim.Node_List_T,
         Pair_List    => Flow.Slim.Edge_List_T,
         Lesser       => Flow.Slim.Source,
         Greater      => Flow.Slim.Target);


   --
   -- Filters to pick the edges relevant to a node:
   --

   package Edge_Filters is new List_Filters (
      Item_Type  => Flow.Slim.Edge_T,
      List_Type  => Flow.Slim.Edge_List_T,
      Value_Type => Flow.Slim.Node_T,
      "="        => Flow.Slim."=");

   function Edges_Into is new Edge_Filters.Chosen (Flow.Slim.Target);
   function Edges_From is new Edge_Filters.Chosen (Flow.Slim.Source);


   function Union (Left, Right : Flow.Slim.Edge_List_T)
   return Flow.Slim.Edge_List_T
   --
   -- The concatenation of the two edge lists but without duplication
   -- of elements due to the same edge appearing in Left and Right.
   -- Does not remove duplicates repeated within one of the given
   -- lists (for example, the same edge appearing twice in the Left
   -- list).
   --
   is
      use type Flow.Edge_Index_T;
      use type Flow.Slim.Edge_List_T;

      Unique: Flow.Slim.Edge_List_T (1 .. Right'Length);
      Last  : Natural := 0;
      -- Those Right edges that are not duplicates.
      -- The result will be Left & Unique(1 .. Last).

      Index : Flow.Edge_Index_T;
      -- The index of a Right edge.

      Duplicated : Boolean;
      -- Whether the Right edge at Index is a duplicate.

   begin

      -- Pick those edges from the Right list that are not in
      -- the Left list:

      for R in Right'Range loop

         Index := Flow.Slim.Index (Right(R));

         Duplicated := False;

         for L in Left'Range loop

            Duplicated := Flow.Slim.Index (Left(L)) = Index;

            exit when Duplicated;

         end loop;

         if not Duplicated then

            Last := Last + 1;

            Unique(Last) := Right(R);

         end if;

      end loop;

      -- Return Left and the non-duplicated part of Right:

      return Left & Unique(1 .. Last);

   end Union;


   function Indices (Edges : Flow.Slim.Edge_List_T)
   return Propagator.Edge_Index_List_T
   --
   -- The indices of the edges in the list.
   --
   is
      Ind : Propagator.Edge_Index_List_T (Edges'Range);
      -- The result.
   begin

      for E in Edges'Range loop
         Ind(E) := Flow.Slim.Index (Edges(E));
      end loop;

      return Ind;

   end Indices;


   procedure Encode_Node (
      Within     : in Flow.Slim.Graph_T;
      Node       : in Flow.Slim.Node_T;
      To_Edges   : in Flow.Slim.Edge_List_T;
      From_Edges : in Flow.Slim.Edge_List_T;
      Root       : in Flux_T;
      Basis      : in Tuple_T)
   --
   --
   -- Computes the "into", "effect", "from" and "flow" relations for
   -- the given Node, Within the given graph, and the given edges
   -- From it.
   --
   -- The calculation uses the "flow" relations on the given edges To
   -- the node, and these are assumed to have been calculated earlier.
   -- Root is the flux to nodes that have no To_Edges.
   --
   -- The Basis tuple is the list of all the "basis" Omega
   -- variables, which are also assumed to be the basis of the
   -- Root flux.
   --
   -- The results are stored in the calculator (Root.Calc) under
   -- systematic names (Into_Id, Eff_Id, From_Id, Flow_Id).
   --
   is
      use type Cell_Set_T;

      Nodex : constant Flow.Node_Index_T := Flow.Slim.Index (Node);
      -- The index of this node, for making relation Id's.

      Into_Id : Identifier_T;
      Eff_Id  : Identifier_T;
      -- The calculator identifiers for the "into" and "eff"
      -- relations of this node.

      From_Id : constant Identifier_T := Formulas.From_Id (Nodex);
      -- The calculator identifier of the "from" relation of the node.

      Calc : Calc_Handle_T := Root.Calc;
      -- The calculator we use, for short.


      procedure Compute_Flow (Edge : in Flow.Slim.Edge_T)
      --
      -- Computes and assigns the "flow" relation on the
      -- given edge leaving this node.
      --
      is

         Flow_Rel : Relation_T;
         -- The "flow" relation on the edge.

      begin

         case Edge.Kind is

         when Flow.Slim.Bare =>

            -- Flow is "From" restricted by the edge precondition:

            Flow_Rel := Propagator.Flow_Out (
               From  => From_Id,
               Cond  => Flow.Slim.Condition (Edge),
               Basis => Basis);

         when Flow.Slim.Fluxed =>

            -- Flow is "From" joined to the edge flux:

            Flow_Rel := Join (
               From_Id, 
               Adapted (
                  Flux => Flow.Slim.Flux (Edge),
                  To   => Root.Cells));

         end case;

         Assign (
            Target => Flow_Id (Flow.Slim.Index (Edge)),
            Value  => Flow_Rel,
            Calc   => Calc);

      end Compute_Flow;


   begin  -- Encode_Node

      -- "Into" relation by union of incoming flows.
      --
      -- Note that (even) for Fluxed incoming edges, we use not the
      -- flux of the edge but the "flow" relation that has been computed
      -- and stored in the calculator under the identifier (Flow_Id)
      -- derived from the index of the edge. This "flow" relation
      -- was computed earlier by using the flux of the edge (see
      -- Compute_Flow, above, and its call, below).

      Into_Id := Propagator.Flow_Into (
         Node  => Nodex,
         Along => Indices (To_Edges),
         Root  => Root,
         Calc  => Calc);

      -- The effect of the node depends on the node's kind:

      case Node.Kind is

      when Flow.Slim.Basic =>
         -- Join the effects of the steps:

         Eff_Id := Propagator.Effect (
            Node   => Flow.Slim.Basic (Node),
            Domain => Root.Cells,
            Living => Flow.Slim.Living (Within),
            Calc   => Calc);

      when Flow.Slim.Fusion =>

         if Flow.Slim.Flux (Node).Cells = Root.Cells
         then
            -- Use the existing fusion flux:

            Eff_Id := Id (Flow.Slim.Flux (Node));

         else
            -- A new adapted flux must be created.

            Calculator.Comment (
               Text => "Adapting basis of fused effect",
               Calc => Calc);

            Eff_Id := Id (New_Flux (
               Cells => Root.Cells,
               Value => Adapted (
                  Flux => Flow.Slim.Flux (Node),
                  To   => Root.Cells),
               Calc  => Calc));

         end if;

      end case;

      -- "From" relation as the input joined with the node effect:

      Assign (
         Target => From_Id,
         Value  => Join (Into_Id, Eff_Id),
         Calc   => Calc);


      -- "Flow" relations for each out-edge:

      for E in From_Edges'Range loop

         Compute_Flow (Edge => From_Edges(E));

      end loop;

   end Encode_Node;



   function Flux_Of_Region (
      Within : Flow.Slim.Graph_T;
      Nodes  : Flow.Slim.Node_List_T;
      Edges  : Flow.Slim.Edge_List_T;
      Final  : Flow.Slim.Edge_List_T;
      Var    : Cell_Set_T;
      Calc   : Calc_Handle_T)
   return Flux_List_T
   is

      use type Flow.Slim.Edge_List_T;

      Region : Flow.Slim.Node_List_T := Flow_Sort (Nodes, Edges);
      -- The nodes in precedence order using topological
      -- sorting of the edges in the acyclic region.

      Edges_Et_Final : constant Flow.Slim.Edge_List_T := Union (Edges, Final);
      -- The edges that are included as potential out-edges for data flow.

      Basis : constant Tuple_T := Tuple (Var);
      -- The "basis" tuple for the Omega relations is the set of
      -- cells to be treated as variables.

      Root : constant Flux_T := Identity_Flux (Domain => Var, Calc => Calc);
      -- The input flux for the roots of the region.

      Fluxes : Flux_List_T (Final'Range);
      -- The fluxes to be constructed, one for each final edge.

   begin

      -- For each node in precedence order, compute the
      -- relations "into", "effect", "from", and the "flow"
      -- relations on the out-edges:

      for I in Region'Range loop

         Encode_Node (
            Within     => Within,
            Node       => Region(I),
            To_Edges   => Edges_Into (
                             Pick => Region(I),
                             From => Edges),
            From_Edges => Edges_From (
                             Pick => Region(I),
                             From => Edges_Et_Final),
            Root       => Root,
            Basis      => Basis);

      end loop;


      -- For each "final" edge, return the flux:

      for I in Final'Range loop

         -- We copy (assign) the edge flux to the new "final"
         -- flux, since the edge flux might perhaps be recomputed
         -- (with a new value) while the "final" flux is still needed.

         Fluxes(I) := New_Flux (
            Cells => Root.Cells,
            Value => Flow_Id (Flow.Slim.Index (Final(I))),
            Calc  => Calc);

      end loop;

      return Fluxes;

   end Flux_Of_Region;


end Calculator.Slim;
