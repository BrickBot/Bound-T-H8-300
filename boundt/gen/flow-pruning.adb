-- Flow.Pruning (body)
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
-- $Revision: 1.6 $
-- $Date: 2015/10/24 20:05:48 $
--
-- $Log: flow-pruning.adb,v $
-- Revision 1.6  2015/10/24 20:05:48  niklas
-- Moved to free licence.
--
-- Revision 1.5  2007-12-17 13:54:37  niklas
-- BT-CH-0098: Assertions on stack usage and final stack height, etc.
--
-- Revision 1.4  2006/02/05 20:50:39  niklas
-- Added trace subheadings.
--
-- Revision 1.3  2005/02/20 15:15:36  niklas
-- BT-CH-0004.
--
-- Revision 1.2  2005/02/16 21:11:45  niklas
-- BT-CH-0002.
--
-- Revision 1.1  2004/04/28 19:07:49  niklas
-- First version.
--


with Flow.Pruning.Opt;
with Flow.Show;
with Loops;
with Output;
with Programs;


package body Flow.Pruning is


   procedure Make_Infeasible (
      Step    : in     Step_T;
      Under   : in out Computation.Model_Ref;
      Program : in     Programs.Program_T)
   --
   -- Marks the given step as infeasible.
   -- The Program parameter is only for locus.
   --
   is
   begin

      if Computation.Is_Feasible (Step, Under) then
         -- The step was feasible, so this will change.

         Computation.Mark_Infeasible (Step, Under);

         if Opt.Trace then

            Output.Trace (
               Locus => Flow.Show.Locus (
                  Step   => Step,
                  Source => Programs.Symbol_Table (Program)),
               Text =>
                    "Pruned infeasible step"
                  & Step_Index_T'Image (Index (Step)));

         end if;

      end if;

   end Make_Infeasible;


   procedure Make_Infeasible (
      Edge    : in     Step_Edge_T;
      Under   : in out Computation.Model_Ref;
      Program : in     Programs.Program_T)
   --
   -- Marks the given edge as infeasible.
   -- The Program parameter is only for locus.
   --
   is
   begin

      if Computation.Is_Feasible (Edge, Under) then
         -- The edge is not already marked as intrinsically
         -- infeasible. The condition will change.

         Computation.Mark_Infeasible (Edge, Under);

         if Opt.Trace then

            Output.Trace (
               Locus => Flow.Show.Locus (
                  Step   => Source (Edge),
                  Source => Programs.Symbol_Table (Program)),
               Text =>
                    "Pruned infeasible edge"
                  & Step_Edge_Index_T'Image (Index (Edge)));

         end if;

      end if;

   end Make_Infeasible;


   procedure Prune (Model : in out Computation.Model_Ref)
   is

      -- Principles of Operation:
      --
      -- Given some infeasible parts (steps, edges) in the flow-graph,
      -- any other part is feasible if and only there is a feasible
      -- path from the entry point to this part, and a feasible path
      -- from this part to one or several of the following:
      --
      -- > a feasible return point,
      --
      -- > a feasible call that does not return to the caller, or
      --
      -- > the feasible head of an eternal loop (the loop must
      --   also be feasibly repeatable).
      --
      -- This suggests depth-first feasible-reachability searches, one
      -- from the entry point to mark parts as "enterable", and another
      -- from the "enterable" return points, the "enterable" non-returning
      -- calls, and the heads of "enterable" eternal loops to mark
      -- parts as "returning" (leading to a return), "leaving" (leading
      -- to a non-returning call), or "looping" (leading to an eternal
      -- loop). A part that is both enterable and (returning or leaving
      -- or looping) is feasible.
      --
      -- We start off by making sure that all edges to or from an
      -- infeasible step are marked infeasible in the Model. This means
      -- that the search algorithm only has to inspect the feasibility of
      -- edges, not of steps. We assume that edges leaving a non-returning
      -- call are already marked infeasible.


      Graph : constant Graph_T := Computation.Graph (Under => Model);
      -- The flow-graph underlying the Model.

      Program : constant Programs.Program_T :=
         Programs.Program (Computation.Subprogram (Model));
      -- The program under analysis, for locus information.


      type Reach_T is (
         Unreachable,
         Enterable,
         Returning,
         Leaving,
         Looping);
      --
      -- How reachable a flow-graph part is and what can be
      -- reached from the part.
      --
      -- Unreachable
      --    Not reachable from the subprogram entry point along
      --    feasible paths. Consequently, infeasible.
      -- Enterable
      --    Reachable from the entry point along feasible paths.
      -- Returning
      --    Enterable and can lead to a return from the subprogram
      --    along a feasible path. Consequently, feasible.
      -- Leaving
      --    Enterable and can lead to a non-returning call along
      --    a feasible path. Consequently, feasible.
      -- Looping
      --    Enterable and can lead to an eternal loop along a
      --    feasible path. Consequently, feasible.


      subtype Feasible_T is Reach_T range Returning .. Looping;
      -- The Reach_T values that imply feasibility in the final
      -- marking phase.


      Step_Reach :
         array (Step_Index_T range 1 ..Max_Step (Graph))
         of Reach_T;
      -- Whether a step is reachable.

      Edge_Reach :
         array (Step_Edge_Index_T range 1 .. Max_Step_Edge (Graph))
         of Reach_T;
      -- Whether an edge is reachable.


      procedure Mark_Edges_Infeasible (Around : in Step_T)
      --
      -- Marks all edges Around the given step as infeasible.
      -- This means all edges entering or leaving the given step.
      --
      is

         Edges : constant Step_Edge_List_T :=
              Edges_Into (Around, Graph)
            & Edges_From (Around, Graph);
         -- All edges around the step.

      begin

         for E in Edges'Range loop

            Make_Infeasible (
               Edge    => Edges(E),
               Under   => Model,
               Program => Program);

         end loop;

      end Mark_Edges_Infeasible;


      procedure Reach_Forward (From : in Step_T)
      --
      -- Depth-first reachability in the forward direction, From
      -- a given step in the flow-graph to its direct or indirect
      -- successors along feasible edges.
      -- Precondition: the From step is not yet reached.
      --
      -- All the feasibly reached parts are changed from Unreachable
      -- to Enterable. The intrinsically infeasible edges will be left
      -- as Unreachable, as will those parts of the graph that can
      -- only be reached through such edges.
      --
      is

         Edges : constant Step_Edge_List_T := Edges_From (From, Graph);
         -- The edges leaving this step.

         Edge : Step_Edge_T;
         -- One of the edges.

         Edge_Index : Step_Edge_Index_T;
         -- The index of the edge.

         Target : Step_T;
         -- The target of the edge.

      begin

         Step_Reach(Index (From)) := Enterable;

         for E in Edges'Range loop

            Edge := Edges(E);

            Edge_Index := Index (Edge);

            if Edge_Reach (Edge_Index) = Unreachable
            and then Computation.Is_Feasible (Edge, Model)
            then
               -- This edge is not yet visited but should be.

               Edge_Reach(Edge_Index) := Enterable;

               Target := Flow.Target (Edge);

               if Step_Reach(Index (Target)) = Unreachable then
                  -- This step is not yet visited but should be.
                  -- It must be feasible under the Model because a
                  -- (still) feasible edge enters it.

                  Reach_Forward (From => Target);

               end if;

            end if;

         end loop;

      end Reach_Forward;


      procedure Reach_Backward (
         From : in Step_T;
         Mark : in Feasible_T)
      --
      -- Depth-first reachability in the backward direction.
      --
      -- The traversal uses (traverses) only graph parts that are marked
      -- Enterable. Any reached part is marked as Mark, starting with
      -- the From step itself.
      --
      is

         Edges : constant Step_Edge_List_T := Edges_Into (From, Graph);
         -- The edges entering this step.

         Edge : Step_Edge_T;
         -- One of the edges.

         Edge_Index : Step_Edge_Index_T;
         -- The index of the edge.

         Source : Step_T;
         -- The source of the edge.

      begin

         Step_Reach(Index (From)) := Mark;

         for E in Edges'Range loop

            Edge := Edges(E);

            Edge_Index := Index (Edge);

            if Edge_Reach (Edge_Index) = Enterable then

               Edge_Reach(Edge_Index) := Mark;

               Source := Flow.Source (Edge);

               if Step_Reach(Index (Source)) = Enterable then

                  Reach_Backward (From => Source, Mark => Mark);

               end if;

            end if;

         end loop;

      end Reach_Backward;


      procedure Reach_Backward (
         From : in Step_List_T;
         Mark : in Feasible_T)
      --
      -- Starts the backward reachability traversal from those of
      -- the given steps that are marked Enterable and are feasible
      -- under the Model. All traversed steps and edges are marked Mark.
      --
      is
      begin

         for F in From'Range loop

            if  Step_Reach(Index (From(F))) = Enterable
            and Computation.Is_Feasible (From(F), Model)
            then

               Reach_Backward (
                  From => From(F),
                  Mark => Mark);

            end if;

         end loop;

      end Reach_Backward;


   begin  -- Prune

      -- Mark as infeasible all edges entering or leaving an
      -- infeasible step:

      if Opt.Trace then

         Output.Trace ("Pruning edges around infeasible steps");

      end if;

      for S in Step_Reach'Range loop

         if not Computation.Is_Feasible (S, Model) then

            Mark_Edges_Infeasible (Around => Step_At (S, Graph));

         end if;

      end loop;

      -- Initialize all parts to "not yet reached":

      for S in Step_Reach'Range loop

         Step_Reach(S) := Unreachable;

      end loop;

      for E in Edge_Reach'Range loop

         Edge_Reach(E) := Unreachable;

      end loop;

      -- Mark the feasible parts:

      if Computation.Is_Feasible (Model) then
         -- At least we can enter the Graph.

         -- Forward depth-first traversal to mark some Unreachable
         -- parts as Enterable:

         Reach_Forward (From => Entry_Step (Graph));

         -- At this point the flow-graph parts are marked as Unreachable
         -- or Enterable. The Unreachable parts are infeasible but perhaps
         -- not yet so marked in the Model. An Enterable part is marked
         -- as feasible in the Model, but may yet turn out to be infeasible
         -- if it does not lead to a return, an eternal loop, or a
         -- non-returning call.

         -- Backward depth-first (or height-first ?-) traversal to
         -- mark some Enterable parts as Returning:

         Reach_Backward (
            From => Return_Steps (Graph),
            Mark => Returning);

         -- Backward depth-first (or height-first ?-) traversal to
         -- mark some Enterable parts as Leaving:

         Reach_Backward (
            From => Computation.Non_Returning_Call_Steps (Model),
            Mark => Leaving);

         -- At this point the flow-graph parts are marked as Unreachable,
         -- Enterable, Returning, or Leaving. The Unreachable parts are
         -- infeasible but perhaps not yet marked infeasible in the Model.
         -- The Returning and Leaving parts are feasible and so marked
         -- in the Model. An Enterable part cannot lead to a return or
         -- to a non-returning call, but may still be feasible if it leads
         -- to an eternal loop. Enterable parts are still considered
         -- feasible under the Model.

         -- Mark all Unreachable edges as infeasible in the Model, to
         -- improve the Model's knowledge of which loops are feasibly
         -- repeatable:

         if Opt.Trace then

            Output.Trace ("Pruning unenterable edges");

         end if;

         for E in Edge_Reach'Range loop

            if Edge_Reach(E) = Unreachable then
               -- This edge cannot be entered so it must be infeasible.

               Make_Infeasible (
                  Edge    => Edge_At (E, Graph),
                  Under   => Model,
                  Program => Program);

            end if;

         end loop;

         -- Backward depth-first (or height-first ?-) traversal to
         -- mark as Looping those Enterable parts that can lead
         -- to an Enterable loop head (that is, all Returning or
         -- Leaving parts are ignored):

         Reach_Backward (
            From => Loops.Head_Steps (Computation.Loops_Of (Model)),
            Mark => Looping);
         --
         -- It is not trivial to see that this Reach_Backward (call it
         -- RB) has the desired result, because the Model's knowledge
         -- of which loops are eternal and which loops are repeatable
         -- is incomplete at this time.
         --
         -- The desired result is that _after_ the final marking of
         -- infeasible parts (call it FMIP) below, the stated definition
         -- of feasibility holds: a graph part P is feasible if and only
         -- if there is a feasible path from the entry step to P and a
         -- feasible path from P to a return step, to a non-returning
         -- call, or to the head of an eternal and repeatable loop.
         --
         -- The difficulty is that RB uses all loops, not only those that
         -- are known to be eternal and repeatable. The set of eternal
         -- loops and the set of repeatable loops might change when the
         -- FMIP marks edges as infeasible.
         --
         -- Claim: If RB marks a graph part P Looping then after FMIP
         -- there is a feasible path from P to an eternal loop.
         --
         -- Proof. We will show that there is a path from P to a loop
         -- such that the path is feasible both before and after the FMIP
         -- and the loop is eternal after (but perhaps not before) the
         -- FMIP and remains repeatable.
         --
         -- First of all, note that no part reached from P can be marked
         -- Unreachable, Returning, or Leaving; only the marks Enterable
         -- or Looping can be reached from P.
         --
         -- Secondly, note that FMIP cannot make any loop unrepeatable
         -- because RB marks all repeat edges Looping so FMIP cannot
         -- make any repeat edge infeasible.
         --
         -- Assume RB is done but FMIP not. By the design of RB there
         -- is a feasible path from P to a loop-head L1 such that the
         -- whole path is marked Looping. This means that the path remains
         -- feasible after FMIP. Without loss of generality we can assume
         -- that L1 is the first loop-head on this path. Consider cases:
         --
         -- a) If L1 is eternal then it remains eternal after the
         --    FMIP, so the claim holds.
         --
         -- b) if L1 is not eternal it has some feasible exit edges.
         --    Consider subcases:
         --
         -- b1) If all exit edges from L1 are still marked Enterable
         --     then FMIP will mark them all infeasible. This makes L1
         --     eternal after FMIP, so the claim holds.
         --
         -- b2) If L1 has an exit edge X1 that is marked Looping we
         --     can apply the same argument to continue from X1 to a
         --     loop L2. If L2 is eternal the claim holds by case (a);
         --     if L2 has only Enterable exit edges the claim holds
         --     by case (b1); otherwise L2 has a Looping exit edge X2
         --     that reaches a loop L3 and so on. We now claim that any
         --     such sequence L1 -> X1 -> L2 -> X2 -> L3 -> ... must
         --     end at an eternal loop or a loop with only Enterable
         --     exit edges. If this claim holds, the proof is done.
         --
         -- To prove the claim in (b2), assume the contrary: that the
         -- sequence can be continued without end, always finding Looping
         -- exit edges. Since there is a finite number of loops in the
         -- graph, the sequence must end with a repeating cycle, in other
         -- words the same loop-head, L say, must occur repeatedly.
         -- Consider one occurrence of L in the cycle. The next element
         -- in the cycle is an exit edge X from L which goes to a node
         -- N outside L. Because L occurs again after X in the sequence,
         -- the sequence follows a Looping path from N to L. This means
         -- that there is an outer loop, L', that contains N and L.
         -- The sequence follows a Looping path from N through L and
         -- back to N. This path must contain the loop head of L' and
         -- so L' must also occur repeatedly in the sequence, which by
         -- the same argument implies an outer loop L" that contains L'
         -- and so on. There cannot be an infinite nest of loops so we
         -- have a contradiction and the claim is proved.

      end if;

      -- Final marking of the infeasible parts:

      if Opt.Trace then

         Output.Trace ("Final pruning of infeasible parts");

      end if;

      for S in Step_Reach'Range loop

         if Step_Reach(S) not in Feasible_T then

            Make_Infeasible (
               Step    => Step_At (S,Graph),
               Under   => Model,
               Program => Program);

         end if;

      end loop;

      for E in Edge_Reach'Range loop

         if Edge_Reach(E) not in Feasible_T then

            Make_Infeasible (
               Edge    => Edge_At (E, Graph),
               Under   => Model,
               Program => Program);

         end if;

      end loop;

   end Prune;


end Flow.Pruning;
