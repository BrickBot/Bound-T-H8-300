-- Loops.Slim (body)
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
-- $Revision: 1.14 $
-- $Date: 2015/10/24 20:05:49 $
--
-- $Log: loops-slim.adb,v $
-- Revision 1.14  2015/10/24 20:05:49  niklas
-- Moved to free licence.
--
-- Revision 1.13  2009-10-07 19:26:10  niklas
-- BT-CH-0183: Cell-sets are a tagged-type class.
--
-- Revision 1.12  2005/09/17 14:42:06  niklas
-- BT-CH-0009.
--
-- Revision 1.11  2005/09/16 14:20:50  niklas
-- Using Calculator.Calc_Image instead of Calculator.Image,
-- because here we want a reference to the Omega identifier.
--
-- Revision 1.10  2005/09/16 13:00:55  niklas
-- Added the procedure Trace_Summary and called it to
-- implement the option "-trace summary".
--
-- Revision 1.9  2005/02/16 21:11:46  niklas
-- BT-CH-0002.
--
-- Revision 1.8  2004/04/28 19:39:45  niklas
-- First Tidorum version.
-- Taking Cell_T stuff from Storage, not Arithmetic.
-- Added Calculator comments to calculations.
--
-- Revision 1.7  2001/03/15 07:32:09  holsti
-- Calculations restricted to Basis cells.
--
-- Revision 1.6  2001/01/07 22:06:06  holsti
-- Parameters for live-cell analysis added.
--
-- Revision 1.5  2000/12/29 14:42:38  holsti
-- Uses Assertions.Loop_Invariants.
-- Adapted to changes in Calculator.Slim (part of NC_078).
--
-- Revision 1.4  2000/12/28 17:45:44  holsti
-- Use Loop_Cell_Set_T and Loop_Flux_T from Calculator (NC_040).
--
-- Revision 1.3  2000/08/17 13:02:16  holsti
-- Changed cell-lists to cell-sets.
--
-- Revision 1.2  2000/07/16 13:13:29  holsti
-- First compilable version.
--
-- Revision 1.1  2000/07/14 20:34:50  holsti
-- First (incomplete) version.
--


with Flow.Slim;
with Calculator.Slim;
with Loops.Slim.Opt;
with Output;


package body Loops.Slim is


   procedure Collapse_Loop (
      Luup        : in     Loop_T;
      Body_Flux   : in     Calculator.Flux_T;
      Exits       : in     Flow.Slim.Edge_List_T;
      Exit_Fluxes : in     Calculator.Flux_List_T;
      Graph       : in out Flow.Slim.Graph_T)
   --
   -- Using:
   --    a loop with all inner loops (if any) already collapsed,
   --    the flow-graph containing the loop,
   --    a data-flow (flux) that approximates the loop,
   --    for each loop-exit edge, a data-flow (flux) that
   --       approximates the flow on the edge
   -- Giving:
   --    updated flow-graph with the loop collapsed to a single
   --    node, with the given approximation for the flux,
   --    and with approximate exit-fluxes attached to the exit
   --    edges.
   --
   is
   begin

      -- Fuse all the nodes in the loop:

      Flow.Slim.Fuse (
         Nodes  => Members (Luup).all,
         Proxy  => Flow.Index (Head_Node (Luup)),
         Flux   => Body_Flux,
         Within => Graph);

      -- For each exit edge, set the flux:

      for X in Exits'Range loop

         Flow.Slim.Set_Flux (
            Edge   => Flow.Slim.Index (Exits(X)),
            Flux   => Exit_Fluxes(X),
            Within => Graph);

      end loop;

   end Collapse_Loop;


   procedure Trace_Summary (
      Luup    : in Loop_T;
      Summary : in Calculator.Loop_Summary_T)
   --
   -- Displays the Summary for the Luup.
   --
   is

      Index_Image : constant String :=
         "Loop #" & Loop_Index_T'Image (Loop_Index (Luup));
      -- Identifies the loop by its index.

   begin

      Output.Trace (
           Index_Image
         & " summary repeat effect"
         & Output.Field_Separator
         & Calculator.Calc_Image (Summary.Repeat));

      Output.Trace (
           Index_Image
         & " summary variant cells"
         & Output.Field_Separator
         & Calculator.Image (Summary.Variant));

      Output.Trace (
           Index_Image
         & " summary invariant cells"
         & Output.Field_Separator
         & Calculator.Image (Summary.Invariant));

   end Trace_Summary;


   procedure Collapse_Loop_Variance (
      Luup        : in     Loop_T;
      Inherit_Inv : in     Storage.Cell_Set_T;
      Assert_Inv  : in     Storage.Cell_Set_T;
      Basis       : in     Calculator.Cell_Set_T;
      Calc        : in     Calculator.Calc_Handle_T;
      Graph       : in out Flow.Slim.Graph_T;
      Summary     :    out Calculator.Loop_Summary_T)
   --
   -- Summarises the computational effect of a Luup body and the
   -- repetition of the Luup body. Collapses the Luup into a fusion
   -- node that maintains the invariant cells and changes in an
   -- unspecified way all the other cells accessed by the Loop.
   --
   -- Luup
   --    The loop to be summarized and collapsed. We assume that all
   --    inner loops, if any, are already collapsed in the Graph.
   -- Inherit_Inv
   --    Cells that are invariant by inheritance.
   -- Assert_Inv
   --    Cells that are asserted to be invariant in this Luup.
   -- Basis
   --    The set of all cells relevant to this analysis.
   -- Calc
   --    The calculator instance to be used.
   -- Graph
   --    The (slimmed) flow-graph that contains the Luup and defines
   --    the computation model, including the set of "live" assignments.
   --    The Graph is modified by collapsing the Luup into a fusion node.
   -- Summary
   --    The summary of the Luup: the computed flux on the Luup's repeat
   --    edges, united from all repeat edges, and the computed sets of
   --    loop-variant and loop-invariant cells (where the latter includes
   --    the inherited and asserted invariant cells).
   --
   is
      use type Storage.Cell_Set_T;
      use type Flow.Node_Set_T;
      use type Flow.Slim.Edge_List_T;

      -- TBM: The following leaks a number of Cell_Set_T objects.

      Loop_Ident : constant String :=
         " for loop" & Loop_Index_T'Image (Loop_Index (Luup));
      -- Identifies the loop in Calculator comments.

      Head_Index : constant Flow.Node_Index_T :=
         Flow.Index (Head_Node (Luup));
      -- The index of the loop-head node.

      Members : Flow.Node_Set_T renames Luup.Members.all;
      -- The nodes in the loop, including the head.

      Accessed_Basis_Cells : constant Calculator.Cell_Set_T :=
         Calculator.Cell_Set_T (Calculator.Intersection (
            Calculator.Copy (Flow.Slim.Cells_Accessed (
               By     => Members,
               Within => Graph)),
            Basis));
      --
      -- The basis cells accessed (read and/or written) in the loop.

      Head_Set : constant Flow.Node_Set_T :=
         Flow.To_Set (
            Nodes => (1 => Head_Index),
            Last  => Members'Last);
      -- The loop-head node as a singleton node-set.

      Internal_Edges : constant Flow.Slim.Edge_List_T :=
         Flow.Slim.Edges (
            From   => Members,
            Into   => Members and (not Head_Set),
            Within => Graph);
      -- The (forward) internal edges in the loop.
      -- Repeat edges are excluded by not allowing the head as
      -- the edge target.

      Repeat_Edges : constant Flow.Slim.Edge_List_T :=
         Flow.Slim.Edges (
            From   => Members,
            Into   => Head_Set,
            Within => Graph);
      -- The loop's repeat edges.

      Exit_Edges : constant Flow.Slim.Edge_List_T :=
         Flow.Slim.Edges (
            From   => Members,
            Into   => not Members,
            Within => Graph);
      -- The loop's exit edges.

      Final_Edges : constant Flow.Slim.Edge_List_T :=
         Repeat_Edges & Exit_Edges;
      --
      -- The edges for which fluxes are required (repeat edges
      -- and exit edges).

      subtype Repeats_T is Positive
         range Final_Edges'First
            .. Final_Edges'First + Repeat_Edges'Length - 1;
      -- The slice of Final_Edges that contains repeat edges.

      subtype Exits_T is Positive
          range Repeats_T'Last + 1 .. Final_Edges'Last;
      -- The slice of Final_Edges that contains exit edges.

      Final_Fluxes : Calculator.Flux_List_T (Final_Edges'Range);
      -- The flux computed for each final edge.

   begin

      Calculator.Comment (
         Text => "Collapsing variance" & Loop_Ident,
         Calc => Calc);

      -- Compute the flux on each final edge:

      Final_Fluxes :=
         Calculator.Slim.Flux_Of_Region (
            Within => Graph,
            Nodes  => (1 => Flow.Slim.Node_At (Head_Index, Graph)),
            Edges  => Internal_Edges,
            Final  => Final_Edges,
            Var    => Accessed_Basis_Cells,
            Calc   => Calc);
      --
      -- The loop-head is included in the Nodes parameter to
      -- cater for the extreme case where the loop consists of
      -- just the head, since then there are no internal edges.
      -- The other nodes in the loop (if any) are discovered by
      -- tracing the internal edges.

      -- Unite the fluxes on the repeat edges:

      Calculator.Comment (
         Text => "United repeat flux" & Loop_Ident,
         Calc => Calc);

      Summary.Repeat := Calculator.Union (Final_Fluxes(Repeats_T));

      -- Find the invariant cells:

      Calculator.Comment (
         Text => "Invariant cells" & Loop_Ident,
         Calc => Calc);

      Summary.Invariant := Calculator.Copy (
         Storage.Mixed.Union (Inherit_Inv, Assert_Inv));

      Calculator.Add (
         Cells => Calculator.Invariants_In_Flux (Summary.Repeat),
         To    => Summary.Invariant);

      -- Note that a cell that is invariant in the repeat-flux
      -- is not necessarily invariant in all exit-fluxes, because
      -- an exit-edge can bring out from the loop a state in which
      -- the cell is modified, but it may happen that all repeat
      -- paths undo this modification and restore the original
      -- value of the cell.

      Summary.Variant := Calculator.Copy (Storage.Mixed."-" (
         Accessed_Basis_Cells, Summary.Invariant)); 

      -- Collapse the loop, using as variant cells all those
      -- cells accessed in the loop that are not invariants:

      Calculator.Comment (
         Text => "Collapse" & Loop_Ident,
         Calc => Calc);

      Collapse_Loop (
         Luup        => Luup,
         Body_Flux   =>
            Calculator.Flux_To_Vary (
               Basis => Accessed_Basis_Cells,
               Var   => Summary.Variant,
               Calc  => Calc),
         Exits       => Final_Edges (Exits_T),
         Exit_Fluxes => Final_Fluxes(Exits_T),
         Graph       => Graph);
      --
      -- This collapse represents the loop as an unknown number
      -- (one or more) of repetitions, each of which modifies the
      -- variant cells in an unspecified way (Body_Flux), followed
      -- by one execution of a path from the loop head to an exit
      -- (Exit_Fluxes). This approximation is simple to compute,
      -- since the transitive closure of Body_Flux is Body_Flux
      -- itself: Body_Flux+ = Body_Flux.
      --
      -- Note that for the Exits parameter we give a slice of
      -- Final_Edges, instead of Exit_Edges as such; this is to
      -- ensure that the Exits'Range equals Exit_Fluxes'Range.

      Calculator.Comment (
         Text => "End of collapsing variance" & Loop_Ident,
         Calc => Calc);

      if Opt.Trace_Summary then

         Trace_Summary (Luup, Summary);

      end if;

   end Collapse_Loop_Variance;


   procedure Approximate_Loops (
      Living      : in     Flow.Life.Living_T;
      Loops       : in     Loop_List_T;
      Inherit_Inv : in     Storage.Cell_Set_T;
      Asserts     : in     Assertions.Assertion_Map_T;
      Basis       : in     Calculator.Cell_Set_T;
      Calc        : in     Calculator.Calc_Handle_T;
      Summaries   :    out Calculator.Loop_Summary_List_T)
   is

      Slim_Graph : Flow.Slim.Graph_T := Flow.Slim.Entire_Graph (Living);
      --
      -- A working copy of the given graph, in which loops will be
      -- reduced (slimmed) to single nodes, one-by-one in bottom-up
      -- order (from innermost to outermost). The computation model
      -- and the Living assignment set are also attached to the
      -- slimmed graph.

   begin

      Calculator.Comment (
         Text => "Approximating loops:",
         Calc => Calc);

      -- The loops are already given in innermost-to-outermost order
      -- in Loops. Handle each loop in this order:

      for L in Loops'Range loop

         Collapse_Loop_Variance (
            Luup        => Loops(L),
            Inherit_Inv => Inherit_Inv,
            Assert_Inv  =>
               Assertions.Loop_Invariants (
                  Luup    => Loops(L),
                  Asserts => Asserts),
            Basis       => Basis,
            Calc        => Calc,
            Graph       => Slim_Graph,
            Summary     => Summaries(L));
            
      end loop;

      Flow.Slim.Destroy (Slim_Graph);
      -- The slimmed graph has done its work and is no longer needed.

      Calculator.Comment (
         Text => "End approximating loops",
         Calc => Calc);

   end Approximate_Loops;


end Loops.Slim;
