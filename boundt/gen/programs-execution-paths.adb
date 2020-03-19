-- Programs.Execution.Paths (body)
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
-- $Revision: 1.24 $
-- $Date: 2015/10/24 20:05:51 $
--
-- $Log: programs-execution-paths.adb,v $
-- Revision 1.24  2015/10/24 20:05:51  niklas
-- Moved to free licence.
--
-- Revision 1.23  2009-02-07 12:05:32  niklas
-- Corrected Get_Results to set out params on failure, too.
--
-- Revision 1.22  2008/12/25 08:59:51  niklas
-- Removed unused context clauses and constants.
--
-- Revision 1.21  2008/11/09 21:43:04  niklas
-- BT-CH-0158: Output.Image (Time_T) replaces Programs.Execution.Image.
--
-- Revision 1.20  2008/09/24 08:38:52  niklas
-- BT-CH-0146: Assertions on "loop starts <bound> times".
-- BT-CH-0146: Loop-repeat assertions set both lower and upper bound.
-- BT-CH-0146: Report locations of contradictory "count" assertions.
-- BT-CH-0146: Contradictory "count" assertions imply infeasibility.
--
-- Revision 1.19  2008/07/23 09:07:16  niklas
-- BT-CH-0139: Fix recursion in Programs.Execution.Paths.
--
-- Revision 1.18  2008/07/15 06:38:39  niklas
-- BT-CH-0136: "No feasible execution path" demoted to Warning.
--
-- Revision 1.17  2008/07/14 19:16:57  niklas
-- BT-CH-0135: Assertions on "instructions".
--
-- Revision 1.16  2008/02/28 07:53:16  niklas
-- BT-CH-0116: Call-specific time and stack assertions.
--
-- Revision 1.15  2008/02/23 13:34:04  niklas
-- BT-CH-0115: Wcet_Loop output and option -loop_time.
--
-- Revision 1.14  2007/12/17 13:54:39  niklas
-- BT-CH-0098: Assertions on stack usage and final stack height, etc.
--
-- Revision 1.13  2007/02/13 20:25:35  Niklas
-- BT-CH-0044.
--
-- Revision 1.12  2006/12/05 18:48:36  niklas
-- BT-CH-0040.
--
-- Revision 1.11  2006/11/01 21:27:26  niklas
-- BT-CH-0034.
--
-- Revision 1.10  2006/10/30 19:13:55  niklas
-- BT-CH-0032.
--
-- Revision 1.9  2006/08/22 12:55:47  niklas
-- Extended Find_Worst_Case (Call, Bounds_Set) to do nothing (except
-- a Note) if the given call has no execution bounds in the set.
--
-- Revision 1.8  2006/02/06 21:16:55  niklas
-- Corrected Constrain_Eternal_Loop to make the loop body
-- execute once more than the total executions of the repeat
-- edges, since the latter total is one less than the
-- asserted repetition count.
--
-- Revision 1.7  2005/06/29 13:44:21  niklas
-- Check that the model is feasible before ILP.
--
-- Revision 1.6  2005/04/17 09:22:31  niklas
-- Changed all Output.Unknown messages to Output.Error.
--
-- Revision 1.5  2005/02/20 15:15:36  niklas
-- BT-CH-0004.
--
-- Revision 1.4  2005/02/16 21:11:47  niklas
-- BT-CH-0002.
--
-- Revision 1.3  2004/05/01 10:19:23  niklas
-- First Tidorum version.
-- Updated the provided interface to make use of the fact that the target
-- program and assertion map for given Execution Bounds can now be reached
-- via the Bounds_Ref so separate Program_T and Assertion_Set_T parameters
-- are no longer needed. Likewise, updated the implementation to take node
-- and edge times from the Execution Bounds and to compute them only when
-- not yet done for these bounds.
-- Removed the procedure Find_All_Worst_Cases because it is no longer
-- needed in the ERC32 floating point blocking analysis.
-- Added Decoder-defined optional and additional ILP variables and terms
-- for the objective function.
-- Added support for eternal loops: if the loop bound is not asserted,
-- assume that the loop repeats zero times, which excludes it.
-- Added exclusion of parts of the flow-graph that are marked infeasible.
-- Added optional Trace output to some operations.
--
-- Revision 1.2  2003/03/13 07:44:53  holsti
-- Corrected Find_All_Worst_Cases to process only those bounds that
-- are fully bounded.
--
-- Revision 1.1  2003/03/11 08:30:51  holsti
-- First version renamed to be a child of Programs.Execution.
-- Added Find_All_Worst_Cases for use with the ERC32.
-- Added Find_Worst_Case for a call-path as a public operation.
-- Using new types and services of the ILP package.
--


with Decoder;
with Flow;
with Flow.Computation;
with Flow.Execution;
with Flow.Execution.Times;
with Flow.Pruning.Opt;
with ILP;
with Loops;
with Loops.Show;
with Output;
with Processor;
with Programs.Execution.Opt;
with Programs.Execution.Paths.Opt;


package body Programs.Execution.Paths is
--
-- The method for finding the worst-case path in a flow-graph is to
-- apply Integer Linear Programming as follows:
--
-- > The variables are the execution count (number of executions) of
--   each node and edge in the flow-graph. The target Decoder can
--   define additional variables, for target-specific purposes.
--
-- > The constraints are defined by the structure of the flow-graph
--   (sum of execution counts of edges into a node = execution count
--   of node = sum of executions of edges leaving the node) and
--   on the execution bounds derived for some of the edges, such
--   as loop-repeat edges. The target Decoder can defined additional
--   constraints, for target-specific purposes. The additional
--   constraints can involve node or edge variables or target-specific
--   additional variables.
--
-- > The objective function to be maximised is the estimated total
--   execution time, which is the sum of the total execution times of all
--   nodes. The total execution time of a node (or edge) is the product
--   of the execution count and execution time of the node (or edge).
--   The target Decoder can add an expression to the objective function.
--   The additional expression can involve node or edge variables or
--   target-specific variables.
--


   function One_If_Entry (
      Node  : Flow.Node_T;
      Graph : Flow.Graph_T)
   return ILP.Number_T
   --
   -- Returns one (1) if the node is the entry node of the graph,
   -- and zero (0) otherwise. This represents the additional one
   -- execution count into the entry node that is not represented
   -- in the incoming edges.
   --
   is
      use type Flow.Node_T;
   begin

      if Node = Flow.Entry_Node (Graph) then
         return 1;
      else
         return 0;
      end if;

   end One_If_Entry;


   procedure Define_Graph_Constraints (
      Model  : in     Flow.Computation.Model_Ref;
      Luups  : in     Loops.Loop_List_T;
      Solver : in out ILP.Solver_T)
   --
   -- Defines the input/output boundary conditions for all nodes.
   -- This is the "conservation of flow", which means that the
   -- execution count of a node equals the sum of the execution
   -- counts of the edges entering the node, and also equals the
   -- sum of the execution counts of the edges leaving the node.
   --
   -- Only the nodes and edges that are feasible under the given
   -- Model are considered.
   --
   -- Model
   --    The computation model and the underlying flow-graph.
   -- Luups
   --    The loops that are feasible and repeatable under Model.
   -- Solver
   --    The ILP solver.
   --
   -- There are three special cases:
   --
   -- > For the entry node, one is added to the sum of entering
   --   edges, to account for the initial entry.
   --
   -- > For the head node of an eternal loop, the execution count
   --   of the entry edges to the loop is limited to 0 .. 1. Thus,
   --   the execution can enter the eternal loop at most once.
   --   A "conservation of flow" condition is placed on the node to
   --   limit its execution count to one less than the sum of the
   --   execution counts of the edges into the node. This lets one
   --   execution of the repeat edges terminate the execution or,
   --   to be precise, terminate the measured part of the execution.
   --   Later, in the loop-bounding constraints, there may appear
   --   constraints that prevent the loop from circulating forever.
   --
   -- > For a return node, where there are no leaving edges, the
   --   constraint on leaving edges is omitted.
   --
   -- The natural "leaving" constraint on return nodes is that
   -- exactly one return node is executed and exactly once, but this
   -- constraint is implied by the conservation constraints and the
   -- constraint that the graph is entered once.
   --
   is
      use type Flow.Node_T;
      use type ILP.Var_Term_T;

      Graph : constant Flow.Graph_T := Flow.Computation.Graph (Model);
      -- The flow-graph we are analysing.

      Eternal : Flow.Node_Set_T (1 .. Flow.Max_Node (Graph)) :=
         (others => False);
      -- The nodes that head eternal loops.
      -- Initially we know none, but we will find them...


      procedure Constrain_Eternal_Loop (Luup : in Loops.Loop_T)
      --
      -- Defines the flow constraints to enter an eternal loop
      -- at most once, and repeat the head of this loop at most
      -- the number of (asserted) loop repetitions (which equals
      -- one more than the constraint on the total number of
      -- executions of the repeat edges).
      -- Adds the head node to the Eternal set.
      --
      is
         use type Flow.Edge_List_T;

         Head_Node : constant Flow.Node_T := Loops.Head_Node (Luup);
         -- The head of the loop.

         Entries : constant Flow.Edge_List_T :=
            Flow.Computation.Entry_Edges (Luup, Model);
         -- The edges that enter the luup (head) from outside the loop.
         -- Null list if the head is also the entry point of the
         -- subprogram.

         Repeats : constant Flow.Edge_List_T :=
            Flow.Computation.Repeat_Edges (Luup, Model);
         -- The edges that repeat the loop, that is, return to the
         -- loop head from within the loop (including from the loop head
         -- itself).

         Index : constant Flow.Node_Index_T := Flow.Index (Head_Node);
         -- The index of the head.

      begin

         Eternal(Index) := True;

         -- Constraint for entry edges to allow entering the
         -- loop at most once:

         if Head_Node /= Flow.Entry_Node (Graph) then

            ILP.Bound (
               Solver => Solver,
               Left   => 1,
               Rel    => ILP.Greater_Or_Equal,
               Right  => ILP.Sum_Of (Edges => Entries, Const => 0));

         end if;

         -- Constraint for the execution of the head node, made
         -- equal to the number of execution of the repeat edges
         -- by balancing (at most) one entry with (at most) one
         -- (implicit) exit:

         ILP.Bound (
            Solver => Solver,
            Left   => 1 * (ILP.Node, Index),
            Rel    => ILP.Equal,
            Right  =>
               ILP.Sum_Of (
                  Edges => Repeats,
                  Const => 0));

         -- Constraint on the number of edges leaving the head-node
         -- (some of these may be repeat edges, some not), necessary
         -- if there is no assertion on the number of repetitions:

         ILP.Bound (
            Solver => Solver,
            Left   => 1 * (ILP.Node, Index),
            Rel    => ILP.Equal,
            Right  =>
               ILP.Sum_Of (
                  Edges => Flow.Computation.Edges_From (Head_Node, Model),
                  Const => 0));

         -- A constraint on the total number of executions of the
         -- repeat edges is generated elsewhere (in Bound_Loop below)
         -- if there is an assertion on this loop.
         --
         -- If there is no assertion on the number of repetitions of
         -- this loop, the above constraints are not sufficient to
         -- bound the loop. Moreover, even if there are assertions on
         -- the (absolute, per call) execution count of parts of the
         -- loop body that do bound the number of executions of the
         -- loop, such loop executions may be included in the solution
         -- even if the loop-head is not entered at all (because there
         -- is no assertion that makes the execution count of the loop
         -- proportional to the loop-start count).

      end Constrain_Eternal_Loop;


      procedure Constrain_Node (
         Index : in Flow.Node_Index_T)
      --
      -- Defines the flow constraints for one node that is not
      -- the head of an eternal loop, but only if the node is
      -- feasible under the Model.
      --
      is
         use type ILP.Var_Term_T;

         Node : constant Flow.Node_T := Flow.Node_At (Index, Graph);
         -- The node in question.

         Node_Term : constant ILP.Var_Term_T := 1 * (ILP.Node, Index);
         -- The execution count variable of the node itself.

      begin

         if Flow.Computation.Is_Feasible (Node, Model) then

            -- Constraint for entering edges:

            ILP.Bound (
               Solver => Solver,
               Left   => Node_Term,
               Rel    => ILP.Equal,
               Right  =>
                  ILP.Sum_Of (
                     Edges => Flow.Computation.Edges_Into (Node, Model),
                     Const => One_If_Entry (Node, Graph)));

            -- Constraints for leaving edges:

            if Flow.Computation.Number_From (Node, Model) > 0 then
               -- Not a return node.

               ILP.Bound (
                  Solver => Solver,
                  Left   => Node_Term,
                  Rel    => ILP.Equal,
                  Right  =>
                     ILP.Sum_Of (
                        Edges => Flow.Computation.Edges_From (Node, Model),
                        Const => 0));

            end if;

         end if;

      end Constrain_Node;


   begin  -- Define_Graph_Constraints

      ILP.Comment (Solver, "Eternal loops if any");

      for L in Luups'Range loop

         if Flow.Computation.Is_Eternal (Luups(L), Model) then

            Constrain_Eternal_Loop (Luups(L));
 
         end if;

      end loop;               

      ILP.Comment (Solver, "Conservation of flow");

      for N in 1 .. Flow.Max_Node (Graph) loop

         if not Eternal(N) then

            Constrain_Node (Index => N);

         end if;

      end loop;

   end Define_Graph_Constraints;


   procedure Define_Execution_Time (
      Subprogram : in     Subprogram_T;
      Bounds     : in     Bounds_Ref;
      Model      : in     Flow.Computation.Model_Ref;
      Solver     : in out ILP.Solver_T)
   --
   -- Defines the execution time function for the Subprogram under the
   -- given Bounds and the computation Model for these Bounds.
   --
   -- This is the function which is to be maximised, and is defined as
   -- the sum of all execution counts multiplied by their execution times.
   --
   -- The execution times of the nodes and edges are defined by the
   -- given execution Bounds, including the execution times of the
   -- calls to lower-level subprograms.
   --
   is
      use type ILP.Var_Term_T;

      Graph : constant Flow.Graph_T := Flow.Computation.Graph (Model);
      -- The flow-graph under analysis.

      Per_Node : constant Node_Times_T :=
         Node_Times (
            From       => Bounds,
            With_Calls => True);
      --
      -- The (worst case) execution time of each node in the graph.
      -- including the execution times of the lower-level calls.

      Per_Edge : constant Flow.Execution.Times.Edge_Times_T :=
         Flow.Execution.Times.Edge_Times (
            Graph           => Graph,
            Step_Edge_Times => Step_Edge_Times (Bounds));
      --
      -- The (worst case) execution time of each edge between nodes
      -- in the graph, as copied from the corresponding step-edge
      -- times from the Bounds.

      Added_Time : constant ILP.Expression_T :=
         Decoder.Additional_Time (Subprogram, Bounds);
      --
      -- Additional time from target-specific analysis.

      Num_Vars : constant Positive :=
         Per_Node'Length + Per_Edge'Length + Added_Time.Max_Vars;
      --
      -- The maximum number of ILP variables in the expression.
      -- There is one variable for each node and one for each edge,
      -- and there may be some variables in the expression for the
      -- target-specific additional time. However, infeasible nodes
      -- and edges do not get any variables.

      Func : ILP.Expression_T (Num_Vars);
      -- The expression for the objective function.

   begin

      -- Include the node times:

      for N in Per_Node'range loop

         if Flow.Computation.Is_Feasible (N, Model) then

            ILP.Add (
               Term => ILP.Number_T (Per_Node(N)) * (ILP.Node, N),
               To   => Func);

         end if;

      end loop;

      -- Include the edge times:

      for E in Per_Edge'Range loop

         if Flow.Computation.Is_Feasible (E, Model) then

            ILP.Add (
               Term => ILP.Number_T (Per_Edge(E)) * (ILP.Edge, E),
               To   => Func);

         end if;

     end loop;

      -- Include the target-specific additional time:

      ILP.Add (
         Expr => Added_Time,
         To   => Func);

      -- Define the function.

      ILP.Objective (
         Solver => Solver,
         Func   => Func,
         Goal   => ILP.Maximize);

   end Define_Execution_Time;


   procedure Define_Node_Bounds (
      Limits : in     Flow.Execution.Bounds_T;
      Model  : in     Flow.Computation.Model_Ref;
      Solver : in out ILP.Solver_T)
   --
   -- Defines the node-specific execution-count bounds for
   -- the ILP analysis of the given call. Only limits for nodes
   -- that are feasible under the given Model are included.
   --
   is
      use type Flow.Execution.Count_T;

      Feasible : Boolean;
      -- Whether the node for some Limit is feasible under the Model.

   begin

      ILP.Comment (Solver, "Additional constraints");

      for L in Limits'Range loop
         -- L is the index of a flow-graph node.

         Feasible := Flow.Computation.Is_Feasible (L, Model);

         if Feasible and Limits(L).Min > 0 then
            -- A lower bound on the executions of this node.

            ILP.Bound (
               Solver => Solver,
               Left   => (ILP.Node, L),
               Rel    => ILP.Greater_Or_Equal,
               Right  => ILP.Number_T (Limits(L).Min));


         end if;

         if Feasible and Flow.Execution.Bounded (Limits(L).Max) then
            -- An upper bound on the executions of this node.

            ILP.Bound (
               Solver => Solver,
               Left   => (ILP.Node, L),
               Rel    => ILP.Less_Or_Equal,
               Right  => ILP.Number_T (Limits(L).Max));

         end if;

      end loop;

   end Define_Node_Bounds;


   procedure Define_Avoided_Steps (
      Steps  : in     Flow.Step_List_T;
      Graph  : in     Flow.Graph_T;
      Solver : in out ILP.Solver_T)
   --
   -- Defines the execution-count bounds that forbid the execution
   -- of the given Steps, in the ILP analysis of this Graph.
   --
   is

      Node : Flow.Node_T;
      -- The node for one of the Steps.

   begin

      ILP.Comment (Solver, "Avoided-step constraints");

      for S in Steps'Range loop

         Node := Flow.Node_Containing (Steps(S), Graph);

         ILP.Bound (
            Solver => Solver,
            Left   => (ILP.Node, Flow.Index (Node)),
            Rel    => ILP.Less_Or_Equal,
            Right  => 0);

      end loop;

   end Define_Avoided_Steps;


   procedure Bound_Total_Flow (
      Upon   : in     Flow.Edge_List_T;
      Bound  : in     Flow.Execution.Bound_T;
      Solver : in out ILP.Solver_T)
   --
   -- Defines constraints Upon the sum of execution counts of
   -- a set of edges. The constraint to be defined is
   --
   --    Sum(Upon) within Bound
   --
   -- The Bound can set a lower bound or an upper bound or both.
   --
   is
      use type ILP.Expression_T;

      Total : constant ILP.Expression_T :=
         ILP.Sum_Of (Edges => Upon, Const => 0);
      -- The left-hand side, the total execution count of the edges.

   begin

      if Bound.Min > 0 then

         ILP.Bound (
            Solver => Solver,
            Left   => Total,
            Rel    => ILP.Greater_Or_Equal,
            Right  => ILP.Number_T (Bound.Min));

      end if;

      if Flow.Execution.Bounded (Bound.Max) then

         ILP.Bound (
            Solver => Solver,
            Left   => Total,
            Rel    => ILP.Less_Or_Equal,
            Right  => ILP.Number_T (Bound.Max));

      end if;

   end Bound_Total_Flow;


   procedure Bound_Flow_Multiple (
      Product : in     Flow.Edge_List_T;
      Factor  : in     Flow.Execution.Bound_T;
      Source  : in     Flow.Edge_List_T;
      Const   : in     ILP.Number_T;
      Solver  : in out ILP.Solver_T)
   --
   -- Defines constraints that relate the sum of execution
   -- counts of two sets of edges with a constant multiple.
   -- The constraint to be defined is
   --
   --    Sum(Product) within Factor * (Sum(Source) + Const)
   --
   -- The Factor can set a lower bound or an upper bound or both.
   --
   -- The Const term is included only if not zero. It is used to
   -- simulate the implicit edge into the entry node of the graph.
   --
   is
      use type ILP.Expression_T;

      Products : constant ILP.Expression_T :=
         ILP.Sum_Of (Edges => Product, Const => 0);
      -- The left-hand side.

      Sources : constant ILP.Expression_T :=
         ILP.Sum_Of (Edges => Source, Const => Const);
      -- The right-hand side, except the Factor.

   begin

      if Factor.Min > 0 then

         ILP.Bound (
            Solver => Solver,
            Left   => Products,
            Rel    => ILP.Greater_Or_Equal,
            Right  => ILP.Number_T (Factor.Min) * Sources);

      end if;

      if Flow.Execution.Bounded (Factor.Max) then

         ILP.Bound (
            Solver => Solver,
            Left   => Products,
            Rel    => ILP.Less_Or_Equal,
            Right  => ILP.Number_T (Factor.Max) * Sources);

      end if;

   end Bound_Flow_Multiple;


   procedure Define_Loop_Bounds (
      Model  : in     Flow.Computation.Model_Ref;
      Luups  : in     Loops.Loop_List_T;
      Bounds : in     Bounds_Ref;
      Solver : in out ILP.Solver_T)
   --
   -- Defines the bounds on loop edges, including both
   -- neck and repeat bounds.
   --
   -- If there are no bounds on an eternal loop, the loop
   -- is constrained to repeat zero times.
   --
   is

      Graph : constant Flow.Graph_T := Flow.Computation.Graph (Model);
      -- The flow-graph under analysis.

      Start_Bounds : constant Loop_Bounds_T :=
         Loop_Start_Bounds (Bounds);
      -- Bounds on the starting edges (from outside the loop, into
      -- the loop head) for each loop (if known).

      Neck_Bounds : constant Loop_Bounds_T :=
         Loop_Neck_Bounds (Bounds);
      -- Bounds on the neck edge (from loop-head into the loop) for
      -- each loop (if known).

      Repeat_Bounds : constant Loop_Bounds_T :=
         Loop_Repeat_Bounds (Bounds);
      -- Bounds on the repeat edges (to loop-head from the loop) for
      -- each loop (if known).


      procedure Bound_Loop (
         Luup         : in Loops.Loop_T;
         Start_Bound  : in Flow.Execution.Bound_T;
         Neck_Bound   : in Flow.Execution.Bound_T;
         Repeat_Bound : in Flow.Execution.Bound_T)
      --
      -- Defines the bounds on one loop.
      --
      is
         use type Flow.Node_T;

         Head : constant Flow.Node_T := Loops.Head_Node (Luup);
         -- The head node of the loop.

         Bounded : Boolean := False;
         -- Whether some bounds (neck or repeat) are placed
         -- on this loop.

         -- The special edges of this loop:

         Entry_Edges : constant Flow.Edge_List_T :=
            Flow.Computation.Entry_Edges (Luup, Model);
         -- Edges from outside the loop, into the loop head.

         Neck_Edges  : constant Flow.Edge_List_T :=
            Flow.Computation.Neck_Edges (Luup, Model);
         -- Edges from the loop head into the loop (to another
         -- loop node, or back to head itself).

         Repeat_Edges : constant Flow.Edge_List_T :=
            Flow.Computation.Repeat_Edges (Luup, Model);
         -- Edges from some loop node (perhaps the head itself) to
         -- the loop head.

      begin

         -- Apply start bounds if any:

         if Flow.Execution.Bounded (Start_Bound) then

            if Head /= Flow.Entry_Node (Graph) then

               ILP.Comment (Solver, "Start bound");

               Bound_Total_Flow (
                  Upon   => Entry_Edges,
                  Bound  => Start_Bound,
                  Solver => Solver);

            -- else
            --    Earlier checks have verified that the Start_Bound
            --    allows one start, so we can ignore it.

            end if;

         end if;

         -- Apply neck bounds if any:

         if Flow.Execution.Bounded (Neck_Bound) then

            ILP.Comment (Solver, "Neck bound");

            Bound_Flow_Multiple (
               Product => Neck_Edges,
               Factor  => Neck_Bound,
               Source  => Entry_Edges,
               Const   => One_If_Entry (Head, Graph),
               Solver  => Solver);

            Bounded := True;

         end if;

         -- Apply repeat bounds if any:

         if Flow.Execution.Bounded (Repeat_Bound) then

            ILP.Comment (Solver, "Repeat bound");

            Bound_Flow_Multiple (
               Product => Repeat_Edges,
               Factor  => Repeat_Bound,
               Source  => Entry_Edges,
               Const   => One_If_Entry (Head, Graph),
               Solver  => Solver);

            Bounded := True;

         end if;

      end Bound_Loop;


      Luup : Loops.Loop_T;
      -- The current loop from Luups.

      Index : Loops.Loop_Index_T;
      -- The index of the Luup.


   begin  -- Define_Loop_Bounds

      ILP.Comment  (Solver, "Loop bounds");

      for L in Luups'Range loop

         Luup  := Luups(L);
         Index := Loops.Loop_Index (Luup);

         ILP.Comment (Solver, "Loop" & Loops.Loop_Index_T'Image(Index));

         Bound_Loop (
            Luup         => Luup,
            Start_Bound  => Start_Bounds (Index),
            Neck_Bound   => Neck_Bounds  (Index),
            Repeat_Bound => Repeat_Bounds(Index));

      end loop;

   end Define_Loop_Bounds;


   procedure Declare_Variables (
      Subprogram : in Subprogram_T;
      Bounds     : in Bounds_Ref;
      Model      : in Flow.Computation.Model_Ref;
      Counts     : in     Flow_Counts_Ref;
      Added_Vars : in     ILP.Var_List_T;
      Solver     : in out ILP.Solver_T;
      Num_Vars   :    out Natural)
   --
   -- Declare (define) all execution-count variables and target-
   -- specific additional variables as integer variables in the
   -- Solver.
   --
   -- Num_Vars
   --    The total number of variables declared. This may be less
   --    than normally expected if some nodes/edges are infeasible.
   --
   is

      Max_Vars : constant Positive :=
         Counts.Node'Length + Counts.Edge'Length + Added_Vars'Length;
      --
      -- The maximum number of ILP variables in the expression.
      -- There is one variable for each node and one for each edge,
      -- and there may be some variables in the expression for the
      -- target-specific additional time. However, no variables are
      -- defined for infeasible nodes or infeasible edges.

      Vars : ILP.Var_List_T (1 .. Max_Vars);
      -- The list of all variables.

   begin

      ILP.Comment  (Solver, "Integer declarations");

      Num_Vars := 0;

      -- Variables for nodes:

      for N in Counts.Node'Range loop

         if Flow.Computation.Is_Feasible (N, Model) then

            Num_Vars := Num_Vars + 1;

            Vars(Num_Vars) := (ILP.Node, N);

         end if;

      end loop;

      -- Variables for edges:

      for E in Counts.Edge'Range loop

         if Flow.Computation.Is_Feasible (E, Model) then

            Num_Vars := Num_Vars + 1;

            Vars(Num_Vars) := (ILP.Edge, E);

         end if;

      end loop;

      -- Additional target-specific variables:

      for A in Added_Vars'Range loop

         Num_Vars := Num_Vars + 1;

         Vars(Num_Vars) := Added_Vars(A);

      end loop;

      ILP.Declare_Integer_Variables (Solver, Vars(1 .. Num_Vars));

   end Declare_Variables;


   procedure Get_Results (
      Solver     : in out ILP.Solver_T;
      Num_Vars   : in     Natural;
      Bounds     : in     Bounds_Ref;
      Model      : in     Flow.Computation.Model_Ref;
      Added_Vars : in     ILP.Var_List_T;
      Status     :    out ILP.Status_T;
      Counts     :    out Flow.Execution.Counts_T;
      Time       :    out Processor.Time_T)
   --
   -- Solves the ILP problem and gets the results.
   --
   -- Solver
   --    The fully defined ILP problem.
   -- Num_Vars
   --    The total number of ILP variables.
   -- Bounds
   --    The execution bounds that control the solution.
   --    They are passed to Decoder.Take_Additional_Values, which
   --    may use or update them.
   -- Model
   --    The computation model for the Bounds. It is a separate
   --    parameter for technical reasons to do with the "limitedness"
   --    of the type and the "freezing" of the Bounds.
   -- Added_Vars
   --    The target-specific additional ILP variables as given by
   --    Decoder.Additional_Variables.
   -- Status
   --    Whether the ILP solution and the extraction of results succeed.
   -- Counts
   --    Returns the execution counts defined by the ILP solution.
   --    The execution count for an infeasible node or edge will be zero.
   -- Time
   --    Returns the execution time bound from the ILP solution.
   --
   -- The solved values for the Added_Vars are passed to the Decoder
   -- via Decoder.Take_Additional_Values. They are not stored anywhere
   -- else.
   --
   is
      use type ILP.Status_T;

      Self : constant String := "Programs.Execution.Paths.Get_Results";
      -- Location for fault messages.

      Node_Set : array (Counts.Node'Range) of Boolean := (others => False);
      Edge_Set : array (Counts.Edge'Range) of Boolean := (others => False);
      -- Whether the execution count for a given node/edge has been
      -- set by the Solver output.


      Added_Value : ILP.Valuation_T (1 .. Added_Vars'Length);
      Last_Added  : Natural := 0;
      -- The values of the target-specific additional variables.


      procedure Take (Solved : in ILP.Var_Value_T)
      is
      --
      -- Takes and uses the ILP solution for an ILP variable.
      --
      -- >  If the variable is a node or edge execution count, sets
      --    the Count for this node or edge.
      --
      -- >  If the variable is a target-specific additional variable,
      --    stores it in Added_Value.
      --
      begin

         case Solved.Var.Kind is

         when ILP.Node =>

            Counts.Node(Solved.Var.Node_Index) :=
               Flow.Execution.Count_T (Solved.Value);

            Node_Set(Solved.Var.Node_Index) := True;

         when ILP.Edge =>

            Counts.Edge(Solved.Var.Edge_Index) :=
               Flow.Execution.Count_T (Solved.Value);

            Edge_Set(Solved.Var.Edge_Index) := True;

         when ILP.Target =>

            Last_Added := Last_Added + 1;

            if Last_Added <= Added_Value'Last then

               Added_Value(Last_Added) := Solved;

            -- else a fault will be reported below.

            end if;

         when others =>

            Output.Fault (
               Text     => "ILP results unexpected.",
               Location => Self);

         end case;

      end Take;


      Results : ILP.Results_T (Num_Vars);
      -- Holds the result (variable values and function value)
      -- from the ILP solution.


   begin  -- Get_Results

      -- Get the results.

      ILP.Solve (
         Solver  => Solver,
         Results => Results);

      Status := Results.Status;

      if Status /= ILP.Solved then
         -- Failure.

         Counts.Node := (others => 0);
         Counts.Edge := (others => 0);
         Time := 0;

         return;

      end if;

      -- Extract the results.

      Time := Processor.Time_T (Results.Function_Value);

      for V in Results.Variable_Values'Range loop

         Take (Solved => Results.Variable_Values(V));

      end loop;

      -- Check that Counts has been set for all nodes/edges:

      for N in Node_Set'Range loop

         if not Node_Set(N) then

            Counts.Node(N) := 0;

            if Flow.Computation.Is_Feasible (N, Model) then

               Output.Fault (
                  Location => Self,
                  Text =>
                       "Execution count for node"
                     & Flow.Node_Index_T'Image (N)
                     & " is undefined.");

               Status := ILP.Failed;

            end if;

         end if;

      end loop;

      for E in Edge_Set'Range loop

         if not Edge_Set(E) then

            Counts.Edge(E) := 0;

            if Flow.Computation.Is_Feasible (E, Model) then

               Output.Fault (
                  Location => Self,
                  Text =>
                       "Execution count for edge"
                     & Flow.Edge_Index_T'Image (E)
                     & " is undefined.");

               Status := ILP.Failed;

            end if;

         end if;

      end loop;

      -- Check and use the target-specific additional values:

      if Last_Added < Added_Value'Last then

         Output.Fault (
            Location => Self,
            Text =>
                 "Only"
               & Natural'Image (Last_Added)
               & " target-specific variables defined, but"
               & Natural'Image (Added_Value'Length)
               & " were expected.");

      elsif Last_Added > Added_Value'Last then

         Output.Fault (
            Location => Self,
            Text =>
                 "Only"
               & Natural'Image (Added_Value'Length)
               & " target-specific variables expected, but"
               & Natural'Image (Last_Added)
               & " were defined.");

         Last_Added := Added_Value'Last;

      end if;

      Decoder.Take_Additional_Values (
         Values     => Added_Value(1 .. Last_Added),
         Subprogram => Subprogram (Bounds),
         Bounds     => Bounds);

   end Get_Results;


   procedure Warn_Ghost_Loops (
      Graph  : in Flow.Graph_T;
      Luups  : in Loops.Loop_List_T;
      Counts : in Flow.Execution.Counts_T;
      Source : in Symbols.Symbol_Table_T)
   --
   -- Checks if the IPET solution contains a loop that is never
   -- entered but nevertheless is repeated. This can happen if
   -- the loop does not have repetition bound and can be bypassed
   -- (execution can flow past the loop), and has some (absolute)
   -- execution-count assertions on elements of the loop body that
   -- bound the number of executions of the loop body (absolutely,
   -- not in proportion to the number of times the loop is started).
   -- This is "ghost" loop.
   --
   -- A repetition bound on the loop would, even if very large and
   -- thus not constraining, give a bound on the number of loop-body
   -- executions that is proportional to the number of times the
   -- loop starts. Thus, it would eliminate all executions of a
   -- non-started loop.
   --
   is
      use type Flow.Node_T;

      Head : Flow.Node_T;
      -- The head of a loop.

   begin

      for L in Luups'Range loop

         Head := Loops.Head_Node (Luups(L));

         if Head = Flow.Entry_Node (Graph) then
            -- The loop-head is also the entry point of the subprogram.
            -- This loop is necessarily started at least once, so it
            -- cannot be a ghost loop. We treat this case specially
            -- because here the number of loop-starts is not just the
            -- total execution count of the entry edges.

            null;

         elsif Flow.Execution.Count (Head, Counts) > 0
         and then
            Flow.Execution.Total_Count (
               Edges  => Loops.Entry_Edges (Luups(L), Graph),
               Within => Counts) = 0
         then
            -- The loop is never started, yet it repeats.
            -- This is a ghost loop.

            Output.Error (
               Locus => Loops.Show.Locus (Luups(L), Graph, Source),
               Text  => "Ghost loop needs asserted repetition bound.");

         end if;

      end loop;

   end Warn_Ghost_Loops;


   WCET_Key      : constant String := "Wcet";
   WCET_Call_Key : constant String := "Wcet_Call";
   --
   -- Key strings for Output.Result via Result_Key.


   WCET_Loop_Key : constant String := "Wcet_Loop";
   --
   -- Key string for loop time.


   function Result_Key (Level : Bounding_Level_T) return String
   --
   -- The result key appropriate to the bounding level.
   --
   is
   begin

      if Level = 0 then
         return WCET_Key;
      else
         return WCET_Call_Key;
      end if;

   end Result_Key;


   procedure Emit_Loop_Times (Bounds : Bounds_Ref)
   --
   -- Compute and output the WCET bounds for all loops within
   -- the given execution Bounds, including both feasible and
   -- infeasible loops and unrepeatable loops.
   --
   is
      use type Processor.Time_T;

      Luups : constant Loops.Loops_T := Loops_Of (Subprogram (Bounds));
      -- All the loops.

      Luup : Loops.Loop_T;
      -- One of the Luups.

      Count : Flow.Execution.Count_T;
      -- The execution count of the head of the loop.

      Total, Self, Callees : Processor.Time_T;
      -- The total time, self time, and callees time for the Luup.

      Mark : Output.Nest_Mark_T;
      -- Locus for the Luup.

   begin

      for L in Luups'Range loop

         Luup := Luups(L);

         begin

            Mark := Output.Nest (Loops.Show.Locus (
               Luup   => Luup,
               Within => Flow_Graph (Bounds),
               Source => Symbol_Table (Bounds)));

            Count := Head_Count (Luup, Bounds);

            Total := Total_Time (Luup, Bounds, With_Calls => True);

            if Opt.Split_Time then
               -- Show total, self, callees parts of the time bound.

               Self    := Total_Time (Luup, Bounds, With_Calls => False);
               Callees := Total - Self;

               Output.Result (
                  Key   => WCET_Loop_Key,
                  Text  =>
                       Output.Image (Natural (Count))
                     & Output.Field_Separator
                     & Output.Image (Total)
                     & Output.Field_Separator
                     & Output.Image (Self)
                     & Output.Field_Separator
                     & Output.Image (Callees));

            else
               -- Show only total time bound.

               Output.Result (
                  Key  => WCET_Loop_Key,
                  Text =>
                       Output.Image (Natural (Count))
                     & Output.Field_Separator
                     & Output.Image (Total));

            end if;

            Output.Unnest (Mark);

         exception

         when X : others =>

            Output.Exception_Info (
               Text       => "Programs.Execution.Paths.Emit_Loop_Times",
               Occurrence => X);

            Output.Unnest (Mark);

         end;

      end loop;

   end Emit_Loop_Times;


   procedure Find_Worst_Path_And_Time (
      Bounds   : in Bounds_Ref;
      Avoiding : in Flow.Step_List_T;
      Model    : in Flow.Computation.Model_Ref)
   --
   -- Finds the worst-case execution path and its execution time,
   -- using the IPET (ILP) method.
   --
   -- Otherwise identical to Find_Worst_Case, but does not check if
   -- the time is already bounded; computes a new path and time in
   -- any case if the execution paths are sufficiently bounded and if
   -- there is a feasible path.
   --
   -- Marks the Bounds to show that the time-calculation phase has
   -- been applied.
   --
   -- Bounds
   --    The execution bounds that determine the worst case.
   -- Avoiding
   --    Steps to be avoided in execution, in addition to the
   --    infeasible steps possibly shown in the Model.
   -- Model
   --    The computation model for the Bounds. It is a separate
   --    parameter for technical reasons to do with the "limitedness"
   --    of the type and the "freezing" of the Bounds.
   --
   is
      use type Output.Locus_T;
      use type Processor.Time_T;

      Program : constant Program_T :=
         Programs.Execution.Program (Bounds);
      -- The program we are analysing.

      Subprogram : constant Programs.Subprogram_T :=
         Programs.Execution.Subprogram (Bounds);
      -- The subprogram to which the Bounds apply.

      Graph : constant Flow.Graph_T := Flow.Computation.Graph (Model);
      -- The flow graph under analysis.

      Luups : constant Loops.Loop_List_T :=
         Flow.Computation.Loops_Of (Model);
      -- The loops in the flow-graph that are feasible and repeatable
      -- under the Model.

      Sufficient_Bounds : Boolean := True;
      -- Whether the Bounds are sufficiently bounded to support
      -- this calculation. Initially we know nothing to the
      -- contrary.

      Time_State : constant Time_State_T :=
         Programs.Execution.Time_State (Bounds);
      -- The state of bounding of the execution time.

      Counts : Flow_Counts_Ref :=
         new Flow.Execution.Counts_T (
            Nodes => Flow.Max_Node (Graph),
            Edges => Flow.Max_Edge (Graph));
      --
      -- The worst-case path to be computed, defined as the
      -- execution-count of each node and edge in the graph.

      Added_Vars : constant ILP.Var_List_T :=
         Decoder.Additional_Variables (Subprogram, Bounds);
      --
      -- Additional target-specific variables.

      Path_Time : Processor.Time_T;
      -- The computed execution time for the worst-case path.

      Self_Part, Callee_Part : Processor.Time_T;
      -- Splitting Path_Time into "self" and "callee" parts.

      Solver : ILP.Solver_T;
      -- The ILP solver, running as a child process.

      Num_Vars : Natural;
      -- The total number of ILP variables.

      Status : ILP.Status_T;
      -- Whether the computation succeeds.

      Autograph : constant String :=
         "Programs.Execution.Paths.Find_Worst_Path_And_Time";
      -- Signature for Fault messages.

   begin

      if Execution.Opt.Trace_Bounds then

         Output.Trace (
              "Finding the worst-case path using bounds #"
            & Bounds_Index_T'Image (Index (Bounds)));

      end if;

      -- Check that the subprogram is feasible:

      if not Flow.Computation.Is_Feasible (Model) then

         Output.Warning ("No feasible execution path.");

         Sufficient_Bounds := False;

      end if;

      -- Check that the bounds are sufficiently defined:

      case Time_State is

      when Computable =>
         -- This is the expected state.

         null;

      when Undefined
         | Vague
         | Depends
         | Computed
         | Asserted
         | Infeasible
         | Unbounded
         | Failed =>

         Output.Fault (
            Location => Autograph,
            Text =>
                 "Time_State of bounds #"
               & Bounds_Index_T'Image (Index (Bounds))
               & " is "
               & Time_State_T'Image (Time_State));

         Sufficient_Bounds := False;

      end case;

      -- Calculate if sufficient bounds:

      if Sufficient_Bounds then

         -- Ensure that per-edge times are available:

         if not Edge_Times_Bounded (Bounds) then

            Bound_Edge_Times (
               Times  => Flow.Execution.Times.Step_Edge_Times (Graph),
               Within => Bounds);

         end if;

         -- Ensure that per-node times are available:

         if not Node_Times_Bounded (Bounds) then

            Compute_Node_Times (Bounds);

         end if;

         -- Start an ILP solver:

         Solver := ILP.Start;

         ILP.Comment (Solver,
              "Maximise WCET for "
            & Output.Image (Output.Current_Locus));

         -- Define the ILP problem:

         Define_Execution_Time (
            Subprogram => Subprogram,
            Bounds     => Bounds,
            Model      => Model,
            Solver     => Solver);

         Define_Graph_Constraints (
            Model  => Model,
            Luups  => Luups,
            Solver => Solver);

         Define_Node_Bounds (
            Limits => Node_Bounds (Bounds),
            Model  => Model,
            Solver => Solver);

         Define_Loop_Bounds (
            Model   => Model,
            Luups   => Luups,
            Bounds  => Bounds,
            Solver  => Solver);

         Define_Avoided_Steps (
            Steps  => Avoiding,
            Graph  => Graph,
            Solver => Solver);

         Decoder.Additional_Bounds (
            Subprogram => Subprogram,
            Bounds     => Bounds,
            Solver     => Solver);

         Declare_Variables (
            Subprogram => Subprogram,
            Bounds     => Bounds,
            Model      => Model,
            Counts     => Counts,
            Added_Vars => Added_Vars,
            Solver     => Solver,
            Num_Vars   => Num_Vars);

         -- The ILP problem is complete.
         -- Solve it and get the results:

         Get_Results (
            Solver     => Solver,
            Num_Vars   => Num_Vars,
            Bounds     => Bounds,
            Model      => Model,
            Added_Vars => Added_Vars,
            Status     => Status,
            Counts     => Counts.all,
            Time       => Path_Time);

         -- Report success or failure:

         case Status is

         when ILP.Solved =>

            Warn_Ghost_Loops (
               Graph  => Graph,
               Luups  => Luups,
               Counts => Counts.all,
               Source => Programs.Symbol_Table (Program));

            Bound_Flow_Counts (
               Counts => Counts,
               Within => Bounds);

            Bound_Time (
               Time   => Path_Time,
               State  => Computed,
               Within => Bounds);

            if Opt.Split_Time then
               -- Show total, self, callees parts of the time bound.

               Callee_Part := Callee_Time (Bounds);
               Self_Part   := Path_Time - Callee_Part;

               Output.Result (
                  Key  => Result_Key (Level (Bounds)),
                  Text =>
                       Output.Image (Path_Time)
                     & Output.Field_Separator
                     & Output.Image (Self_Part)
                     & Output.Field_Separator
                     & Output.Image (Callee_Part));

            else
               -- Show only total time bound.

               Output.Result (
                  Key  => Result_Key (Level (Bounds)),
                  Text => Output.Image (Path_Time));

            end if;

            if Opt.Loop_Times then

               Emit_Loop_Times (Bounds);

            end if;

         when ILP.Infeasible =>

            -- An error message was already emitted in ILP.

            Set_Time_State (To => Infeasible, Within => Bounds);

         when ILP.Unbounded =>

            -- An error message was already emitted in ILP.

            Set_Time_State (To => Unbounded, Within => Bounds);

         when ILP.Failed =>

            Output.Error ("Worst-case path not found.");

            Set_Time_State (To => Failed, Within => Bounds);

         end case;

         -- Terminate the Solver.

         ILP.Stop (Solver);

      end if;

      Set_Time_Calculated (Bounds);

   end Find_Worst_Path_And_Time;


   procedure Find_Worst_Case (
      Bounds   : in Bounds_Ref;
      Avoiding : in Flow.Step_List_T)
   is

      Bounds_Mark : Output.Nest_Mark_T;
      -- Marks the location of these Bounds in the nest of
      -- default output location descriptions.

   begin

      if Time_State (Bounds) = Computable then

         begin

            Bounds_Mark := Output.Nest (Locus (Bounds));

            Find_Worst_Path_And_Time (
               Bounds   => Bounds,
               Avoiding => Avoiding,
               Model    => Computation (Bounds).all);

            Output.Unnest (Bounds_Mark);

         exception

         when X : others =>

            Output.Exception_Info (
               Text       => "Programs.Execution.Find_Worst_Case",
               Occurrence => X);

            Output.Unnest (Bounds_Mark);

            raise;

         end;

      end if;

   end Find_Worst_Case;


   procedure Compute_Worst_Case_Deeply (Bounds : in Bounds_Ref)
   --
   -- Finds the worst-case path for the Bounds, assuming that the
   -- time-state is Computable or Depends. Recursively analyses the
   -- bounds on deeper calls. If all deeper calls are bounded, and
   -- the time-state is Computable, computes the worst case path
   -- and execution-time bound for these Bounds. If the bounding of
   -- some deeper call fails, sets the time-state of these Bounds
   -- to Vague if it was Computable (otherwise leaving the state
   -- at Depends).
   --
   is

      Deeper_Calls : constant Call_Bounds_List_T := Call_Bounds (Bounds);
      -- The nested bounds on the deeper calls from the callee.

      Call_Times_Known : Boolean := True;
      -- Whether the execution-time bounds are known for all Deeper_Calls.
      -- Deeper_Calls that turn out to be infeasible are not included.

      Infeasible_Calls : Flow.Bounded_Step_List_T (Deeper_Calls'Length);
      -- The ILP analysis of the Deeper_Calls may show that some of
      -- them have no feasible execution path. The call-steps of these
      -- infeasible calls are collected here.

      Deeper_Call : Call_Bounds_T;
      -- One of the Deeper_Calls.

      Deeper_State : Time_State_T;
      -- The Time_State of the Deeper_Call.Bounds.


      procedure Time_Unknown (Reason : in String)
      --
      -- The execution-time bound for this Deeper_Call is unknown
      -- for the given Reason. Call_Times_Known becomes False and
      -- an optional warning gives the Reason.
      --
      is
      begin

         Call_Times_Known := False;

         if Execution.Opt.Warn_Unbounded_Call then

            Output.Warning (
               Locus => Locus (Deeper_Call.Call, Bounds),
               Text  => Reason);

         end if;

      end Time_Unknown;


   begin  -- Compute_Worst_Case_Deeply

      if Execution.Opt.Trace_Bounds then

         Output.Trace (
            Locus => Locus (Bounds),
            Text  =>
                 "Computing worst-case path for bounds #"
               & Bounds_Index_T'Image (Index (Bounds)));

      end if;

      -- Compute the time-bounds for the Deeper_Calls:

      for D in Deeper_Calls'Range loop

         Deeper_Call := Deeper_Calls(D);

         Find_Worst_Case_Deeply (Deeper_Call);

         Deeper_State := Time_State (Deeper_Call.Bounds);

         if Opt.Trace_Callee_Bounds then

            Output.Trace (
               Locus => Locus (Deeper_Call.Call, Bounds),
               Text  =>
                    "Time_State of callee bounds #"
                  & Bounds_Index_T'Image (Index (Deeper_Call.Bounds))
                  & " is "
                  & Time_State_T'Image (Deeper_State));

         end if;

         case Deeper_State is

         when Computed
            | Asserted =>
            -- Good.

            null;

         when Vague =>
            -- A problem has been reported earlier.

            Call_Times_Known := False;

         when Depends =>

            Time_Unknown ("Callee is not bounded by context.");

         when Infeasible =>

            if Flow.Pruning.Opt.Warn_Unreachable then

               Output.Warning (
                  Locus => Locus (Deeper_Call.Call, Bounds),
                  Text  => "Callee is infeasible.");

            end if;

            Flow.Add (Step (Deeper_Call.Call), Infeasible_Calls);

         when Unbounded =>

            Time_Unknown ("Callee is unbounded.");

         when Failed =>

            Time_Unknown ("Callee time-bound computation failed.");

         when Undefined
            | Computable =>

            Output.Fault (
               Location => "Programs.Execution.Paths.Compute_Worst_Case_Deeply",
               Locus    => Locus (Deeper_Call.Call),
               Text     =>
                    "Callee time-state is "
                  & Time_State_T'Image (Deeper_State));

            Time_Unknown ("Fault in callee state.");

         end case;

      end loop;

      -- Find the worst case at this level, if possible:

      if Time_State (Bounds) = Computable then

         if Call_Times_Known then
            -- Here is a job that can be done and needs doing.

            Find_Worst_Case (
               Bounds   => Bounds,
               Avoiding => Flow.To_List (Infeasible_Calls));

         else
            -- Some calls are not bounded for execution time, so
            -- we cannot compute execution-time bounds for this caller.

            Set_Time_State (To => Vague, Within => Bounds);

            Output.Warning (
               Locus => Locus (Bounds),
               Text  => "Time could not be bounded.");

         end if;

      end if;

   end Compute_Worst_Case_Deeply;


   procedure Find_Worst_Case_Deeply (Call : in Call_Bounds_T)
   is

      Bounds : constant Bounds_Ref := Call.Bounds;
      -- The bounds at this level. Just an abbreviation.

      State : constant Time_State_T := Time_State (Bounds);
      -- The initial time-state.

   begin

      if Time_Calculated (Bounds) then
         -- Already done. Won't do it again.

         null;

      else
         -- These bounds need calculation. And perhaps some
         -- callee bounds need it, too.

         if Execution.Opt.Trace_Bounds then

            Output.Trace (
               Locus => Locus (Bounds),
               Text  =>
                    "Starting worst-case path analysis for bounds #"
                  & Bounds_Index_T'Image (Index (Bounds)));

         end if;

         case State is

         when Undefined | Computed | Unbounded | Failed =>
            -- This State is not expected at this point.

            Output.Fault (
               Location => "Programs.Execution.Paths.Find_Worst_Case_Deeply",
               Locus    => Locus (Bounds),
               Text     =>
                    "Time_State is "
                  & Time_State_T'Image (State));

            Set_Time_Calculated (Bounds);
            -- To avoid another fault like the above.

         when Computable | Asserted | Depends | Vague | Infeasible =>
            -- Computable is the normal, desired case; we compute the
            -- execution-time bounds on the callees and then compute
            -- the bound for these Bounds.
            --
            -- For Asserted, Depends, Vague, and Infeasible, we want to
            -- compute the execution-time bounds on callees, just to
            -- inform the user, even if we cannot or need not compute
            -- the execution-time bound for these Bounds.

            Compute_Worst_Case_Deeply (Bounds);

         end case;

      end if;

   end Find_Worst_Case_Deeply;


   procedure Find_Worst_Case (
      Call       : in Call_T;
      Bounds_Set : in Bounds_Set_T)
   is

      Bounds : constant Bounds_Ref :=
         Bounds_For (Root => Call, Within => Bounds_Set);
      -- The execution bounds for this root Call, if any.

   begin

      if Bounds /= No_Bounds then
         -- We have some execution bounds.

         Find_Worst_Case_Deeply ((
            Call   => Call,
            Bounds => Bounds));

      else

         Output.Note (
            Locus => Locus (Call),
            Text  => "No execution bounds for worst-case path.");

      end if;

   end Find_Worst_Case;


end Programs.Execution.Paths;
