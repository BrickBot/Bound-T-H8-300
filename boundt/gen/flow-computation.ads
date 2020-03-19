-- Flow.Computation (decl)
--
-- The computation performed by a subprogram is modelled by arithmetic
-- effects and conditions attached to the elements of the flow-graph of
-- the subprogram. During analysis, the computation model may be
-- refined or modified in various ways, probably depending on context
-- and stage of analysis.
--
-- A "computation model" based on a flow-graph G is a data structure
-- that attaches
--
--  > an Arithmetic.Effect_T to each step in G,
--
--  > an Arithmetic.Condition_T to each (step) edge in G,
--
--  > a feasibility (true or false) to each step and step-edge in G,
--
--  > a Calling.Protocol_T to each call in G, and
--
--  > a flag showing which calls in G can return to G.
--
-- Thus, a computation model is similar to the functions Flow.Effect (Step)
-- and Flow.Condition (Edge) which return the "primitive" arithmetic
-- effects and edge preconditions as defined when the flow graph was built,
-- plus the function Programs.Protocol that returns the "primitive"
-- calling protocol of a call as defined when the call was added to the
-- flow graph.
--
-- However, a Flow.Computation.Model_T is separate from the flow-graph
-- and thus there can be many such models and they can be created and
-- destroyed without impact on the flow-graph.
--
-- For convenience and consistency, a computation model also contains
-- references to the subprogram and flow-graph to which the model applies.
--
-- Finally, a computation model may contain further processor-specific
-- information, for use in processor-specific analyses.
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
-- $Revision: 1.20 $
-- $Date: 2015/10/24 19:36:48 $
--
-- $Log: flow-computation.ads,v $
-- Revision 1.20  2015/10/24 19:36:48  niklas
-- Moved to free licence.
--
-- Revision 1.19  2015/05/22 06:31:53  niklas
-- Added Model_T.Proc_Info, processor-specific information.
--
-- Revision 1.18  2011-09-06 17:50:00  niklas
-- Added Is_Final (Node_T, ..) for help with ALF export.
--
-- Revision 1.17  2008-04-22 12:40:42  niklas
-- Added Mark_Infeasible for a node edge.
--
-- Revision 1.16  2008/02/18 12:58:27  niklas
-- Added functions Final_Nodes and Successors (return Node_List_T)
-- for use in SPARC RW-trap analysis.
--
-- Revision 1.15  2007/12/17 13:54:36  niklas
-- BT-CH-0098: Assertions on stack usage and final stack height, etc.
--
-- Revision 1.14  2007/10/26 12:44:35  niklas
-- BT-CH-0091: Reanalyse a boundable edge only if its domain grows.
--
-- Revision 1.13  2007/10/02 20:49:22  niklas
-- Added Unresolved_Dynamic_Edges_From (Step, Model).
--
-- Revision 1.12  2007/08/06 09:21:31  niklas
-- Added Model_T.Index : Model_Index_T.
--
-- Revision 1.11  2007/04/18 18:34:38  niklas
-- BT-CH-0057.
--
-- Revision 1.10  2007/03/29 15:18:01  niklas
-- BT-CH-0056.
--
-- Revision 1.9  2007/01/13 13:51:03  niklas
-- BT-CH-0041.
--
-- Revision 1.8  2006/11/26 22:07:26  niklas
-- BT-CH-0039.
--
-- Revision 1.7  2006/10/24 08:44:31  niklas
-- BT-CH-0028.
--
-- Revision 1.6  2005/09/20 09:50:45  niklas
-- Added Mark_Infeasible (Loop).
--
-- Revision 1.5  2005/09/12 19:02:59  niklas
-- BT-CH-0008.
--
-- Revision 1.4  2005/06/29 19:35:55  niklas
-- Added Exit_Edges.
--
-- Revision 1.3  2005/05/09 15:28:17  niklas
-- Added the functions Feasible (Steps), Number_Into (Step), Final_Steps,
-- Successors (Step) and Total_Defining_Assignments, all to support
-- value-origin analysis.
--
-- Revision 1.2  2005/02/23 09:05:18  niklas
-- BT-CH-0005.
--
-- Revision 1.1  2005/02/16 21:11:43  niklas
-- BT-CH-0002.
--

--:dbpool with GNAT.Debug_Pools;

with Calling;
with Loops;
with Processor.Computation;
with Programs;


package Flow.Computation is


   --
   ---   Models of computation over a flow-graph
   --


   subtype Model_Index_T is Positive;
   --
   -- A number that identifies a particular computation model (object).


   type Effects_T is
      array (Step_Index_T range <>) of Arithmetic.Effect_Ref;
   --
   -- An arithmetic effect for each step in a flow-graph.


   type Feasibility_T is
      array (Step_Index_T range <>) of Boolean;
   --
   -- A feasibility flag for each step in a flow-graph.


   type Conditions_T is
      array (Step_Edge_Index_T range <>) of Arithmetic.Condition_T;
   --
   -- An arithmetic precondition for each (step) edge in a flow-graph.
   -- The precondition for a node edge is implicitly taken as the
   -- precondition of the corresponding step edge.


   type Protocols_T is
      array (Programs.Call_Index_T range <>) of Calling.Protocol_Ref;
   --
   -- A calling protocol for each call in a flow-graph.


   type Call_Set_T is array (Programs.Call_Index_T range <>) of Boolean;
   --
   -- A subset of the calls in the model.
   -- Members are marked by True values.


   type Model_T (
      Steps : Step_Index_T;
      Edges : Step_Edge_Count_T;
      Calls : Natural)
   is record
      Index      : Model_Index_T;
      References : Natural := 0;
      Clean      : Boolean := True;
      Subprogram : Programs.Subprogram_T;
      Graph      : Graph_T;
      Pruned     : Boolean := True;
      Proc_Info  : Processor.Computation.Info_T;
      Effect     : Effects_T     (1 .. Steps);
      Feasible   : Feasibility_T (1 .. Steps);
      Condition  : Conditions_T  (1 .. Edges);
      Protocol   : Protocols_T   (1 .. Calls);
      New_Proto  : Call_Set_T    (1 .. Calls);
      Returns    : Call_Set_T    (1 .. Calls);
   end record;
   --
   -- A model of the arithmetic computations and conditions and the
   -- parameter-passing protocols in a flow-graph that has the given
   -- number of Steps, (step) Edges and Calls to callees.
   --
   -- Steps
   --    The number of steps in the graph (= the highest step index).
   -- Edges
   --    The number of step-edges in the graph.
   -- Calls
   --    The number of calls (call steps) in the graph.
   -- Index
   --    A number that identifies this Model_T object uniquely (for
   --    models allocated from the heap, that is).
   -- References
   --    The number of references (Model_Ref, see below) to this
   --    computation model object.
   -- Clean
   --    Whether the model is "clean", that is, it has no record
   --    or memory of changed model elements.
   -- Subprogram
   --    The subprogram on which this model is based.
   --    Note that Subprogram_T is a reference type.
   -- Graph
   --    The graph on which this model is based.
   --    Note that Graph_T is a reference type.
   -- Pruned
   --    Whether the model has been "pruned", that is, whether the
   --    feasibility or infeasibility has been propagated over the
   --    Graph since some part was last marked as infeasible.
   -- Proc_Info
   --    Processor-specific information, generated and used by
   --    processor-specific analyses of the computation model.
   --    TBD if and how changes to Proc_Info are considered in
   --    the copy-on-write management and reference counts.
   -- Effect
   --    The arithmetic effects attached to the steps in the model.
   -- Feasible
   --    Whether a step is feasible (True) or infeasible (False).
   -- Condition
   --    The arithmetic preconditions attached to the step-edges
   --    in the model.
   --    As usual, this is a necessary precondition for flow along
   --    the edge, but not necessarily a sufficient one.
   --    The edge is infeasible iff Condition is Arithmetic.Never.
   -- Protocol
   --    The calling protocol (parameter-passing map) used by the
   --    calls in the model, indexed by Programs.Call_Index_T.
   -- New_Proto
   --    The subset of calls for which a new Protocol has been set,
   --    since we last marked the model as "clean".
   -- Returns
   --    The subset of calls that can return to the caller.
   --
   -- A Model_T does not "own" the effects, assignments, expressions,
   -- conditions and protocols to which it refers via Effect, Condition
   -- Protocol; it may (and usually does) refer to effects etc. constructed
   -- for other purposes (eg. the primitive objects created when the
   -- flow-graph was built).


   type Model_Ref is limited private;
   --
   -- A reference to a heap-allocated computation model. We handle
   -- computation models by reference for two reasons:
   --
   -- > The same model may be reused in different contexts for the
   --   same subprogram. Reuse by reference saves memory and time.
   --
   -- > Heap allocation can be preferred to stack allocation for large
   --   graphs, where stack size could be a limiting factor.
   --
   -- Computation models are reference-counted to support copy-on-change
   -- and memory reclamation when references are discarded. However, we
   -- do not (yet) use controlled types for this.
   --
   -- Copy-on-change means that when we change a computation model
   -- through a Model_Ref, and there are other Model_Refs to the same
   -- model object, the model object is copied, the change is made in
   -- the copy, and the Model_Ref is changed to refer to the copy.
   -- The original (multiply referenced) computation model is not
   -- changed. Any computation model with more than one reference is
   -- immutable.
   --
   -- Memory reclamation means that when a Memory_Ref is Discarded,
   -- the referenced computation model object is deallocated if this
   -- was the last reference to the object.
   --
   -- Computation models record some of the changes that are made
   -- to them, which is useful when updates to the model or to some
   -- data derived from the model can be done incrementally, only
   -- for the changed parts of the model. Changes are recorded from
   -- the creation of the model. To forget old changes and start
   -- recording a new batch of changes a client calls the operation
   -- that marks the model as "clean".


   type Model_Handle_T is access all Model_Ref;
   --
   -- A reference to a reference to a heap-allocated computation model.
   -- This double-reference is useful when a data structure contains an
   -- attribute of type Model_Ref, but this attribute is not public and
   -- must be used via a query function, and yet we should be able to
   -- update the model using copy-on-change. In such cases, the query
   -- function returns a Model_Handle_T which means that its .all can
   -- be associated with in-out parameters.


   function Model (Ref : Model_Ref) return Model_T;
   --
   -- The computation model to which Ref refers.
   -- This function is provided for completeness, but it is usually
   -- more sensible to access parts of the model directly through the
   -- reference, rather than make a whole copy as here.


   function Is_Null (Model : Model_Ref) return Boolean;
   --
   -- Whether the Model reference is null, that is, refers to no
   -- computation model. The default initial value of any Model_Ref
   -- variable is the null reference.


   procedure Discard (Model : in out Model_Ref);
   --
   -- Discards the Model reference and returns a null reference in Model.
   -- If Model was the last active reference to the computation model
   -- object, the object is deallocated (removed from the heap).
   -- However, the parts of the computation model (effects, conditions,
   -- calling protocols) are not deallocated (they may be used by
   -- reference from other computation models).


   procedure Refer (
      From : in out Model_Ref;
      To   : in     Model_Ref);
   --
   -- Changes the From reference to refer to the same computation
   -- model as the To reference, as in "From := To". First, however,
   -- the procedure Discards the old value of From (if it is not the
   -- same as To already).


   function Primitive_Model (Subprogram : Programs.Subprogram_T)
   return Model_T;
   --
   -- The primitive computation model that is built into the flow-graph
   -- for the given Subprogram. In this model:
   --
   -- > The step effects are those assigned by the Decoder.
   -- > The edge conditions are those assigned by the Decoder
   --   except for edges from non-returning calls, see below.
   -- > All steps are marked "feasible" except the call-steps for
   --   callees that are marked "unused"; those call-steps are
   --   marked "infeasible".
   -- > The calling protocols are those assigned by the Decoder.
   -- > All calls are marked as returning or not returning,
   --   depending on Programs.Returns (callee). All edges leaving
   --   non-returning calls are marked "infeasible".
   --
   -- The fact that most steps are marked "feasible" will normally be
   -- consistent with the feasibility of the edges, because the
   -- Decoder should never add an explicitly infeasible edge to the
   -- flow-graph (that is, the Cond parameter in Add_Edge should not
   -- be Arithmetic.Never). However, the Decoder may (and normally
   -- should) add call-steps to "unused" subprograms, although these
   -- calls are also "unused".
   --
   -- The Subprogram also defines the set of calls for which the model
   -- defines calling protocols. The primitive model (usually) does not
   -- define the effects of call steps. To indicate that these effects are
   -- absent, the resulting Model considers all calls to have a changed
   -- calling protocol. Thus, the Model is "clean" only if there are
   -- no feasible calls from the Subprogram.
   --
   -- The result has References = 0.


   procedure Refer_To_Primitive_Model (
      Subprogram : in     Programs.Subprogram_T;
      Model      : in out Model_Ref);
   --
   -- Sets Model to refer to the primitive computation model that is
   -- built into the flow-graph for the given Subprogram, after Discarding
   -- the old (in) value of Model. See the function Primitive_Model for
   -- a description of this model.
   --
   -- The Subprogram also defines the set of calls for which the Model
   -- defines calling protocols. The primitive model (usually) does not
   -- define the effects of call steps. To indicate that these effects are
   -- absent, the resulting Model considers all calls to have a changed
   -- calling protocol. Thus, the Model is "clean" only if there are
   -- no feasible calls from the Subprogram.
   --
   -- The result may or may not be the only reference to the primitive
   -- computation model.


   function Changed (Model : in Model_Ref) return Boolean;
   --
   -- Whether the Model has as record or memory that some parts
   -- of it have been changed since it was last marked "clean".


   procedure Mark_Clean (Model : in Model_Ref);
   --
   -- Erases the record of changes to the Model and marks the
   -- Model as "clean".


   --
   ---   Properties of models
   --


   function Subprogram (Model : Model_Ref) return Programs.Subprogram_T;
   --
   -- The subprogram to which the Model applies.


   function Program (Model : Model_Ref) return Programs.Program_T;
   --
   -- The program that contains the subprogram to which the Model applies.
   -- Same as Programs.Program (Subprogram.Model).


   function Graph (Under : Model_Ref) return Graph_T;
   --
   -- The flow-graph that lies Under the given model.


   function Max_Step (Within : Model_Ref) return Step_Index_T;
   --
   -- Same as Max_Step (Graph (Within)).


   function Effect (Step : Step_Index_T; Under : Model_Ref)
   return Arithmetic.Effect_Ref;
   --
   -- The arithmetic effect of the Step with the given index,
   -- Under the given model.


   function Effect (Step : Step_T; Under : Model_Ref)
   return Arithmetic.Effect_Ref;
   --
   -- Short for Effect (Index (Step), Under).


   function Effect (Step : Step_T; Under : Model_Ref)
   return Arithmetic.Effect_T;
   --
   -- Short for Effect (Index (Step), Under).all.


   function Condition (Edge : Step_Edge_T; Under : Model_Ref)
   return Arithmetic.Condition_T;
   --
   -- The precondition (necessary but perhaps not sufficient) for
   -- executing the Edge, Under the given Model.


   function Condition (Edge : Edge_T; Under : Model_Ref)
   return Arithmetic.Condition_T;
   --
   -- The precondition (necessary but perhaps not sufficient) for
   -- executing the Edge, Under the given Model.


   function Is_Feasible (Model : Model_Ref) return Boolean;
   --
   -- Whether the whole Model is feasible, which is equivalent to
   -- the feasibility of the entry step of the underlying graph
   -- (assuming that feasibility has been propagated consistently).


   function Is_Feasible (Step : Step_Index_T; Under : Model_Ref)
   return Boolean;
   --
   -- Whether the Step with the given index is feasible (can be
   -- executed) Under this model.


   function Is_Feasible (Step : Step_T; Under : Model_Ref)
   return Boolean;
   --
   -- Whether the Step is feasible (can be executed) Under this model.


   function Is_Feasible (Edge : Step_Edge_Index_T; Under : Model_Ref)
   return Boolean;
   --
   -- Whether the Edge with the given index is feasible (can be
   -- executed) Under this model.


   function Is_Feasible (Edge : Step_Edge_T; Under : Model_Ref)
   return Boolean;
   --
   -- Whether the Edge is feasible (can be executed) Under this model.


   function Is_Feasible (Node : Node_Index_T; Under : Model_Ref)
   return Boolean;
   --
   -- Whether the Node of the given index is feasible under this Model.
   -- This is so if the first step of the Node is feasible.


   function Is_Feasible (Node : Node_T; Under : Model_Ref)
   return Boolean;
   --
   -- Whether the Node is feasible under this Model.
   -- This is so if the first step of the Node is feasible.


   function Is_Feasible (Edge : Edge_Index_T; Under : Model_Ref)
   return Boolean;
   --
   -- Whether the (node) Edge of the given index is feasible Under
   -- this model.
   -- This is equivalent to Is_Feasible (Step_Edge (Edge), Under).


   function Is_Feasible (Edge : Edge_T; Under : Model_Ref)
   return Boolean;
   --
   -- Whether the (node) Edge is feasible (can be executed) Under
   -- this model.
   -- This is equivalent to Is_Feasible (Step_Edge (Edge), Under).


   function Some_Feasible (Edges : Edge_List_T; Under : Model_Ref)
   return Boolean;
   --
   -- Whether at least one of the (node) Edges is feasible Under
   -- this model.
   --
   -- If the Edges list is empty, the function returns False.


   function Is_Feasible (Call : Programs.Call_T; Under : Model_Ref)
   return Boolean;
   --
   -- Whether the Call is feasible Under this model.
   -- This is so if the call-step is feasible Under the model and
   -- the Call itself is feasible on the universal level.
   --
   -- Precondition: Caller(Call) = Subprogram (Under).


   function Is_Feasible (Luup : Loops.Loop_T; Under : Model_Ref)
   return Boolean;
   --
   -- Whether the Luup is feasible Under this model.
   -- This is so if the head-step of the loop is feasible Under
   -- the model.


   function Is_Eternal (Luup : Loops.Loop_T; Under : Model_Ref)
   return Boolean;
   --
   -- Whether the Luup is eternal Under this model.
   -- This is so if the Luup is eternal (has no exit edges) in the
   -- underlying flow-graph or if all the exit edges are infeasible
   -- Under the model.


   function Is_Repeatable (Luup : Loops.Loop_T; Under : Model_Ref)
   return Boolean;
   --
   -- Whether the Luup can be repeated under this Model.
   -- This is so if the Luup has some repeat edges that are feasible
   -- Under the model. Otherwise (not repeatable) the Luup is not
   -- really a loop Under this model, but just an acyclic part of
   -- the flow-graph that is the ghost of a loop.


   function Target_Range (Step : Step_T; Under : Model_Ref)
   return Storage.Alias_Range_T;
   --
   -- The alias range that can be reached by the assignments in the
   -- step's effect Under the given computation model.


   function Calling_Protocol (
      Call_Index : Programs.Call_Index_T;
      Under      : Model_Ref)
   return Calling.Protocol_Ref;
   --
   -- The calling protocol (parameter-passing map) used by the call
   -- with the given Call_Indexndex, Under the given computation model.


   function Calling_Protocol (
      Call  : Programs.Call_T;
      Under : Model_Ref)
   return Calling.Protocol_Ref;
   --
   -- The calling protocol (parameter-passing map) used by the
   -- given Call, Under the given computation model.


   function Calling_Protocol_Is_Static (
      Call  : Programs.Call_T;
      Under : Model_Ref)
   return Boolean;
   --
   -- Whether the calling protocol (parameter-passing map) used by the
   -- given Call, Under the given computation model, is static in the
   -- sense that it does not depend on any cells with unknown or too
   -- weakly bounded values. For a static protocol, the map between
   -- actual (caller) and formal (callee) parameters is known.


   function Calling_Protocol_Changed (
      Call  : Programs.Call_T;
      Under : Model_Ref)
   return Boolean;
   --
   -- Whether the calling protocol (parameter-passing map) used by
   -- the given Call, Under the given computation model, has been
   -- changed (with Set_Calling_Protocol) since the model was last
   -- marked "clean".


   function Returns (
      Call  : Programs.Call_T;
      Under : Model_Ref)
   return Boolean;
   --
   -- Whether the given Call returns to the caller, Under the
   -- given computation model. (To find out if a call returns
   -- requires analysis of the callee, perhaps in a context derived
   -- from this model, but here we just return the information
   -- recorded in the model by Mark_No_Return.).


   --
   --    The feasible parts of a model, and their properties
   --
   -- The following functions are similar to like-named functions in the
   -- Flow, Loops and Programs packages, but consider only those parts of
   -- the underlying flow-graph or subprogram that are feasible under a
   -- given model.


   function Feasible (Steps : Step_List_T; Under : Model_Ref)
   return Step_List_T;
   --
   -- Those Steps that are feasible Under the given model.


   function Edges_Into (Step : Step_T; Under : Model_Ref)
   return Step_Edge_List_T;
   --
   -- All the feasible edges that enter the step (Edge.Target = Step).
   -- Loose edges are not included.
   -- Precondition: the Step itself is feasible.


   function Edges_From (Step : Step_T; Under : Model_Ref)
   return Step_Edge_List_T;
   --
   -- All the feasible edges that leave the step (Edge.Source = Step).
   -- Loose edges are not included.
   -- Precondition: the Step itself is feasible.


   function Number_Into (Step : Step_T; Under : Model_Ref)
   return Natural;
   --
   -- The number of feasible edges that enter the step (Edge.Target = Step).
   -- Loose edges are not included.
   -- Precondition: the Step itself is feasible.


   function Is_Final (Step : Step_T; Under : Model_Ref)
   return Boolean;
   --
   -- Whether the Step is a final step (a return from the subprogram)
   -- Under the given model. In other words, whether the step has no
   -- feasible leaving edges: Edges_From (Step, Under)'Length = 0.


   function Final_Steps (Under : Model_Ref)
   return Step_List_T;
   --
   -- The feasible final (return) steps of the graph, which are those
   -- feasible steps that have no leaving edges. Note that a step for
   -- which all leaving edges are infeasible does _not_ become a final
   -- step, it becomes an infeasible step.


   function Is_Final (Node : Node_T; Under : Model_Ref)
   return Boolean;
   --
   -- Whether the Node is a final node (the last step in the Node is
   -- a return from the subprogram) Under the given model. In other
   -- words, whether the Node has no feasible leaving edges:
   -- Edges_From (Node, Under)'Length = 0.


   function Final_Nodes (Under : Model_Ref)
   return Node_List_T;
   --
   -- The Final nodes, in other words, the nodes that contain
   -- the Final_Steps.


   function Returns (Under : Model_Ref) return Boolean;
   --
   -- Whether there are some feasible final (return) steps of the
   -- graph, Under the given model.


   function Non_Returning_Call_Steps (Under : Model_Ref)
   return Step_List_T;
   --
   -- The call-steps that are feasible Under the given model but
   -- do not return to the caller Under this model.


   function Predecessors (Step : Step_T; Under : Model_Ref)
   return Step_List_T;
   --
   -- All the feasible predecessors of the given Step, Under the
   -- given model. In other words, all the steps from which there is
   -- a feasible edge to the given Step.


   function Successors (Step : Step_T; Under : Model_Ref)
   return Step_List_T;
   --
   -- All the feasible successors of the given Step, Under the
   -- given model. In other words, all the steps to which there is
   -- a feasible edge from the given Step.


   function Edges_Into (Node : Node_T; Under : Model_Ref)
   return Edge_List_T;
   --
   -- All the feasible edges that enter the Node (Edge.Target = Node).
   -- Loose edges are not included.
   -- Precondition: the Node itself is feasible.


   function Number_From (Node : Node_T; Under : Model_Ref)
   return Natural;
   --
   -- The number of edges leaving the Node that are feasible Under the
   -- given model.


   function Edges_From (Node : Node_T; Under : Model_Ref)
   return Edge_List_T;
   --
   -- All the feasible edges that leave the Node (Edge.Source = Node).
   -- Loose edges are not included.
   -- Precondition: the Node itself is feasible.


   function Successors (Node : Node_T; Under : Model_Ref)
   return Node_List_T;
   --
   -- All the feasible successors of the given Node, Under the
   -- given model. In other words, all the nodes to which there is
   -- a feasible edge from the given Node.


   function Is_Feasible (Edge : Dynamic_Edge_T; Under : Model_Ref)
   return Boolean;
   --
   -- Whether the dynamic Edge is feasible Under the given computation
   -- model. This is defined by the feasibility of the Edge's source
   -- step Under the model.


   function Dynamic_Edges (Under : Model_Ref)
   return Dynamic_Edge_List_T;
   --
   -- All the feasible dynamic edges in the graph Under the given model.
   -- A dynamic edge is considered feasible iff the source step is
   -- feasible. This does not necessarily mean that the dynamic edge
   -- can spawn new feasible real edges.


   function Unstable_Dynamic_Edges (Under : Model_Ref)
   return Dynamic_Edge_List_T;
   --
   -- All those feasible dynamic edges in the graph, Under the given
   -- model, that are not yet stably resolved (or stably unresolved).
   -- A dynamic edge is considered feasible iff the source step is
   -- feasible. A dynamic edge is considered unstable if the domain of
   -- the edge has grown since the last analysis of the edge. (At this
   -- point, such an edge will be in the Unresolved state, thanks to
   -- Flow.Find_Unstable_Dynamic_Edges, but not all Unresolved edges
   -- are unstable.) This does not necessarily mean that the dynamic
   -- edge can spawn new feasible real edges.


   function Unstable_Dynamic_Edges_From (
      Source : Step_T;
      Under  : Model_Ref)
   return Dynamic_Edge_List_T;
   --
   -- All those feasible and dynamic edges from the Source step, Under
   -- the given model, that are not yet stably resolved (or stably
   -- unresolved). A dynamic edge is considered feasible iff the Source
   -- step is feasible. A dynamic edge is considered unstable if the domain
   -- of the edge has grown since the last analysis of the edge. (At this
   -- point, such an edge will be in the Unresolved state, thanks to
   -- Flow.Find_Unstable_Dynamic_Edges, but not all Unresolved edges
   -- are unstable.) This does not necessarily mean that the dynamic
   -- edge can spawn new feasible real edges.


   function Dynamic_Flow (Under : Model_Ref) return Edge_Resolution_T;
   --
   -- The overall state of resolution of the feasible dynamic edges
   -- Under the given model. The three possible values are:
   --
   -- Growing      if there is at least one feasible Growing edge.
   -- Unresolved   if there is at least one feasible Unresolved edge
   --              and no feasible Growing edge.
   -- Stable       if all feasible dynamic edges are Stable or there
   --              are no feasible dynamic edges at all.
   --
   -- A Growing result indicates that the flow-graph is still growing,
   -- as a result of resolving dynamic edges. The flow-graph should be
   -- elaborated by following its loose edges, decoding new instructions
   -- and adding steps and edges until there are no more loose edges.
   --
   -- A Stable result indicates that the dynamic control flow has been
   -- resolved to a consistent state that seems complete. The remaining
   -- dynamic edges can be removed to leave a completed, static flow-graph.
   --
   -- An Unresolved result indicates that the dynamic control flow has
   -- been resolved to a consistent state, although the bounds used
   -- so far have been too weak to resolve all dynamic edges. If better
   -- bounds are not available, the remaining dynamic edges can be
   -- removed to leave a static flow-graph but the graph is probably
   -- incomplete because of the Unresolved dynamic edges.


   function Loops_Of (Model : Model_Ref)
   return Loops.Loop_List_T;
   --
   -- All the loops within Subprogram (Model) that are feasible under
   -- this Model and still exist (are repeatable) under this Model.
   --
   -- Note that the function returns a Loop_List_T, not a Loops.Loops_T.
   -- Thus, some of the loops in the flow-graph may be omitted (if they
   -- are infeasible or unrepeatable under this Model) and the indices
   -- in the returned list are not the Loops.Loop_Index_T's of the loops.
   -- However, the list is still in order of increasing Loop_Index_T and
   -- thus has inner loops before the outer loops that contain them.
   --
   -- When all exit-edges of a loop are infeasible under the Model the
   -- loop will appear as an eternal loop in the result, even for the
   -- function Loops.Eternal.


   function Feasible (Edges : Edge_List_T; Under : Model_Ref)
   return Edge_List_T;
   --
   -- Those Edges that are feasible Under the given model.


   function Entry_Edges (
      Luup  : Loops.Loop_T;
      Under : Model_Ref)
   return Flow.Edge_List_T;
   --
   -- Those entry edges of the Luup that are feasible Under the given
   -- model.


   function Neck_Edges (
      Luup  : Loops.Loop_T;
      Under : Model_Ref)
   return Flow.Edge_List_T;
   --
   -- Those neck edges of the Luup that are feasible Under the given
   -- model.


   function Repeat_Edges (
      Luup  : Loops.Loop_T;
      Under : Model_Ref)
   return Flow.Edge_List_T;
   --
   -- Those repeat edges of the Luup that are feasible Under the given
   -- model.


   function Exit_Edges (
      Luup  : Loops.Loop_T;
      Under : Model_Ref)
   return Flow.Edge_List_T;
   --
   -- Those exit edges of the Luup that are feasible Under the given
   -- model.


   function Cells_In (
      Model : Model_Ref;
      Calls : Boolean)
   return Storage.Cell_Set_T;
   --
   -- All the cells (statically) named in the computation Model,
   -- including cells used in arithmetic expressions or edge
   -- conditions and target cells of assignments. However, only
   -- the feasible parts of the model are considered. The cells
   -- included are:
   --
   -- > cells used or defined in the effect of steps, including basis
   --   cells for dynamic memory references; however, the effect of
   --   call steps is included only if Calls is True,
   --
   -- > cells used in the preconditions of edges, including basis
   --   cells for dynamic memory references,
   --
   -- > basis cells for dynamic edges, and
   --
   -- > basis cells for dynamic calling protocols.
   --
   -- Output cells from calls are included only when Calls is True and
   -- only so far as they are given in the effects of the call steps.
   -- Input cells to calls are omitted.


   function Is_Defined (
      Cell  : Storage.Cell_T;
      By    : Step_T;
      Under : Model_Ref)
   return Boolean;
   --
   -- Whether the given Cell is defined (assigned) By the effect of
   -- the given step, Under the given computation model.
   --
   -- This is true iff the step is feasible and the Cell is the (static)
   -- target of an assignment in the effect of the step. Aliasing is
   -- not counted.


   function Cells_Defined (By : Model_Ref) return Storage.Cell_Set_T;
   --
   -- The set of storage cells that are written (assigned) By the
   -- feasible assignments in the given model.
   --
   -- Cells written by callees are included so far as they are
   -- given in the effects of the call steps.


   function Steps_Defining (Cell : Storage.Cell_T; Under : Model_Ref)
   return Step_List_T;
   --
   -- All the feasible steps in the flow-graph that define (assign)
   -- the given Cell, Under the given model.
   --
   -- A call that defines the Cell is included if this definition is
   -- given in the effect of the call step.


   function Max_Effect_Length (Under : Model_Ref)
   return Natural;
   --
   -- The maximum length (number of assignments) of any feasible
   -- effect in the computation model.


   function Total_Defining_Assignments (Under : Model_Ref)
   return Natural;
   --
   -- The total number of assignments (of Defining_Kind_T) in all
   -- the feasible effects in the computation model.


   --
   -- The following "used/defined" queries are meant for use in
   -- identifying loops and calls for assertions. Therefore, they
   -- include infeasible as well as feasible parts of the model.
   --


   function Cells_Used (
      Nodes : Node_List_T;
      Under : Model_Ref)
   return Storage.Cell_List_T;
   --
   -- The storage cells that are read in the Nodes listed or in the
   -- preconditions on edges within or leaving the nodes, Under the
   -- given computation model.
   --
   -- Basis cells for dynamic edges are included.
   -- Basis cells for dynamic calling protocols are not included.
   -- Cells read by callees are not included.
   --
   -- Note that cells used by infeasible steps/edges are included.
   -- TBA option to omit infeasible steps/edges.


   function Cells_Defined (
      Nodes : Node_List_T;
      Under : Model_Ref)
   return Storage.Cell_List_T;
   --
   -- The storage cells that are written in the Nodes listed, Under
   -- the given computation model.
   --
   -- Cells written by callees are included if they are listed in the
   -- effect of the call-step.
   --
   -- Note that cells written by infeasible steps are included.
   -- TBA option to omit infeasible steps.


   function Is_Used (
      Location : Storage.Location_T;
      By       : Node_List_T;
      Under    : Model_Ref)
   return Boolean;
   --
   -- Whether the given Location is used (read) By the steps in the
   -- given nodes. A step is considered to use the Location if the
   -- Location maps to a cell at the address of the step such that the
   -- cell is
   -- > used in a Defining assignment in the effect of the step, or
   -- > used in the precondition of an edge leaving the step, or
   -- > a member of the basis set for a dynamic edge leaving the step.
   --
   -- Basis cells for dynamic calling protocols are not considered.
   --
   -- Note that cells used by infeasible steps/edges are included.
   -- TBA option to omit infeasible steps/edges.


   function Is_Defined (
      Location : Storage.Location_T;
      By       : Node_List_T;
      Under    : Model_Ref)
   return Boolean;
   --
   -- Whether the given Location is defined (assigned) By the steps in
   -- the given nodes. A step is considered to define the Location if the
   -- Location maps to a cell at the address of the step such that the
   -- cell is the target of a Defining assignment in the effect of the step.
   --
   -- Note that cells defined by infeasible steps are included.
   -- TBA option to omit infeasible steps.


   --
   -- The following query functions again scan only feasible parts.
   -- They are meant to locate the parts of the model that need to
   -- be analysed and bounded.
   --


   function Steps_With_Dynamic_Effect (Under  : Model_Ref)
   return Step_List_T;
   --
   -- All the feasible flow-graph steps where the effect of the step,
   -- Under the given computation model, contains some elements of the
   -- class Storage.References.Boundable_T, either as assignments targets
   -- or as parts in the arithmetic expressions.


   function Edges_With_Dynamic_Condition (Under  : Model_Ref)
   return Step_Edge_List_T;
   --
   -- All the feasible step-edges in the flow-graph where the edge
   -- precondition, Under the given computation model, contains some
   -- parts of the class Storage.References.Boundable_T.


   function Calls_From (Model : Model_Ref)
   return Programs.Call_List_T;
   --
   -- All the calls from Subprogram (Model) to other subprograms,
   -- but only those that are feasible under the given Model.


   --
   --    Operations to modify a model
   --


   procedure Set_Effect (
      Step  : in     Step_T;
      To    : in     Arithmetic.Effect_Ref;
      Under : in out Model_Ref);
   --
   -- Sets (changes) the computational effect of the given Step,
   -- Under a given computation model, To the given effect.
   -- Applies copy-on-change to the Under object.
   -- Marks the Under object as changed (not "clean").
   -- Has no effect on the pruning status of the model.


   procedure Mark_Infeasible (
      Step  : in     Step_T;
      Under : in out Model_Ref);
   --
   -- Marks the given Step as infeasible (impossible to execute) Under
   -- the given computation model. Note that only this Step is marked,
   -- the logical implications are not propagated to other graph
   -- elements (for example, all edges entering or leaving the Step
   -- must also be infeasible). Use Flow.Pruning for the propagation.
   -- Applies copy-on-change to the Under object.
   -- Marks the Under object as changed (not "clean") and "not pruned".


   procedure Set_Condition (
      On    : in     Step_Edge_T;
      To    : in     Arithmetic.Condition_T;
      Under : in out Model_Ref);
   --
   -- Sets (changes) the precondition of the given Edge, Under a
   -- given computation model, To the given arithmetic condition.
   -- If the condition is Arithmetic.Never, the edge becomes infeasible.
   -- Applies copy-on-change to the Under object.
   -- Marks the Under object as changed (not "clean"). If the new
   -- condition is Arithmetic.Never, marks the object as "not pruned".


   procedure Mark_Infeasible (
      Edge  : in     Step_Edge_T;
      Under : in out Model_Ref);
   --
   -- Marks the given Edge as infeasible (impossible to execute) Under
   -- the given computation model. Note that only this Edge is marked,
   -- the logical implications are not propagated to other graph
   -- elements (for example, if this is the only edge that enters or
   -- leaves a step then the step must also be infeasible). Use
   -- Flow.Pruning for the propagation.
   --
   -- This operation is equivalent to Set_Condition (Edge, Never, Under).
   -- Applies copy-on-change to the Under object.
   -- Marks the Under object as changed (not "clean") and "not pruned".


   procedure Mark_Infeasible (
      Edges : in     Step_Edge_List_T;
      Under : in out Model_Ref);
   --
   -- Marks all the given Edges as infeasible Under the given computation
   -- model. See Mark_Infeasible for a single Step_Edge_T for more info.


   procedure Mark_Infeasible (
      Edge  : in     Edge_T;
      Under : in out Model_Ref);
   --
   -- Marks the (step) edge corresponding to the given (node) Edge as
   -- infeasible Under the given computation model. See Mark_Infeasible
   -- for a single Step_Edge_T for more info.


   procedure Mark_Infeasible (
      Edges : in     Edge_List_T;
      Under : in out Model_Ref);
   --
   -- Marks the (step) edges corresponding to the given (node) Edges
   -- as infeasible Under the given computation model. Refer to the
   -- Mark_Infeasible operation for Step_Edge_T for more info.
   -- Applies copy-on-change to the Under object.
   -- Marks the Under object as changed (not "clean") and "not pruned".


   procedure Mark_Infeasible (
      Call  : in     Programs.Call_T;
      Under : in out Model_Ref);
   --
   -- Marks the given Call as infeasible (impossible to execute) Under
   -- the given computation model by marking its head-step as infeasible.
   -- Marks the Under object as changed (not "clean") and "not pruned".


   procedure Mark_Infeasible (
      Luup  : in     Loops.Loop_T;
      Under : in out Model_Ref);
   --
   -- Marks the given Luup as infeasible (impossible to execute) Under
   -- the given computation model by marking its head-step as infeasible.
   -- Marks the Under object as changed (not "clean") and "not pruned".


   procedure Set_Calling_Protocol (
      Call  : in     Programs.Call_T;
      To    : in     Calling.Protocol_Ref;
      Under : in out Model_Ref);
   --
   -- Sets (changes) the calling protocol for the given Call, To the
   -- protocol, Under a given model structure.
   --
   -- The new calling protocol may mean that the effect of the Call
   -- in this model changes or should change. However, this operation
   -- does not itself update the effect.
   --
   -- Applies copy-on-change to the Under object.
   -- Marks the Under object as changed (not "clean").


   procedure Mark_No_Return (
      From : in     Programs.Call_T;
      To   : in out Model_Ref);
   --
   -- Marks the call as not returning From the callee To the caller,
   -- under the caller's computation model. This has two effects:
   --
   -- > All edges leaving the call-step are marked infeasible.
   --   A later pruning operation may propagate this infeasibility
   --   to other parts of the flow-graph.
   --
   -- > If the call-step is itself feasible, the call-step is accepted
   --   as the final point of a feasible path in the next pruning of
   --   the flow-graph. Otherwise the call-step would become infeasible
   --   since there is no path from the call-step to a return point or
   --   to an eternal loop.
   --
   -- Marks the caller's computation model To as changed (not "clean")
   -- and "not pruned".


   procedure Prune (Model : in out Model_Ref);
   --
   -- Prunes away all infeasible parts of the computation model by
   -- propagating the feasible/infeasible marks along the flow-graph
   -- edges and then marks the Model as "pruned". Does nothing if the
   -- model is already marked as "pruned".
   -- Applies copy-on-change if the Model is not initially "pruned".


private


   type Model_Access_T is access Model_T;
   --
   -- Refers to a computation model on the heap.

   --:dbpool Model_Access_Pool : GNAT.Debug_Pools.Debug_Pool;

   --:dbpool for Model_Access_T'Storage_Pool use Model_Access_Pool;


   type Model_Ref is limited record
      Ref : Model_Access_T;
   end record;
   --
   -- A reference to the computation model Ref.all.


end Flow.Computation;
