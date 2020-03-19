-- Flow (decl)
--
-- Flow graph structures.
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
-- $Revision: 1.81 $
-- $Date: 2015/10/24 19:36:49 $
--
-- $Log: flow.ads,v $
-- Revision 1.81  2015/10/24 19:36:49  niklas
-- Moved to free licence.
--
-- Revision 1.80  2013-02-19 09:17:41  niklas
-- BT-CH-0245 clean-up. Only descriptions changed.
--
-- Revision 1.79  2012-01-19 19:43:29  niklas
-- BT-CH-0223: Package License does not depend on package Flow.
--
-- Revision 1.78  2011-10-18 20:12:28  niklas
-- Added Prime_Address (Node_T), for ALF export.
--
-- Revision 1.77  2010-01-30 21:13:29  niklas
-- BT-CH-0216: Subprograms have a Return_Method_T attribute.
--
-- Revision 1.76  2009-12-27 22:34:31  niklas
-- BT-CH-0205: Licensing based on code size and time/space dimensions.
--
-- Revision 1.75  2009-11-27 11:28:07  niklas
-- BT-CH-0184: Bit-widths, Word_T, failed modular analysis.
--
-- Revision 1.74  2008/11/18 15:09:37  niklas
-- Added Reject_Resolved_Edge.
--
-- Revision 1.73  2008/07/14 19:16:57  niklas
-- BT-CH-0135: Assertions on "instructions".
--
-- Revision 1.72  2008/06/29 06:15:57  niklas
-- Extended Resolve_To_Return to mark the source step as a return
-- step with Calls.Return_After. This requires that the Graph also
-- be a parameter for Resolve_To_Return.
--
-- Revision 1.71  2008/03/11 22:08:06  niklas
-- BT-CH-0121: Delayed calls and other SHARC support.
--
-- Revision 1.70  2008/02/18 13:00:36  niklas
-- Added function Nodes_Containing (Steps) for use in Flow.Computation.
--
-- Revision 1.69  2008/01/14 20:36:20  niklas
-- BT-CH-0106: Loops.Max_Depth and other changes in Loops.
--
-- Revision 1.68  2007/12/27 12:44:43  niklas
-- Fixed typo in description of Warning_Opt_T.
--
-- Revision 1.67  2007/12/22 15:23:47  niklas
-- BT-CH-0101: Option "-trace graph".
--
-- Revision 1.66  2007/12/17 13:54:37  niklas
-- BT-CH-0098: Assertions on stack usage and final stack height, etc.
--
-- Revision 1.65  2007/11/12 21:37:27  niklas
-- BT-CH-0097: Only arithmetic analysis marks boundable edge domain.
--
-- Revision 1.64  2007/10/28 09:32:47  niklas
-- BT-CH-0092: Arithmetic analysis of dynamic data refs is optional.
--
-- Revision 1.63  2007/10/26 12:44:35  niklas
-- BT-CH-0091: Reanalyse a boundable edge only if its domain grows.
--
-- Revision 1.62  2007/08/31 08:44:50  niklas
-- Added Warning_Opt_T for better control over warnings from the
-- procedures that Add_Dynamic_Edges, in particular to suppress the
-- warning when the edge is directly resolved by the data-state.
--
-- Revision 1.61  2007/08/27 16:13:59  niklas
-- Added Add_Resolved_Dynamic_Edge, making it possible for a boundable
-- edge to resolve into another boundable edge.
--
-- Revision 1.60  2007/08/25 18:48:37  niklas
-- Added Image (Step_Data_Ref) suitable for tracing.
-- Added Full_Image (Step_Tag_T) suitable for tracing.
-- Made Image (Step_T) privately accessible for tracing purposes.
-- Added optional parameter Add_Dynamic_Edge.Warn by which the
-- warning can be disabled even under "-warn flow".
--
-- Revision 1.59  2007/08/20 12:16:25  niklas
-- Support for Storage.Data: Added preconditions for Transformed_Data
-- (the effect is not null) and Constrained_Data (the condition is
-- neither Never nor Always).
--
-- Revision 1.58  2007/07/22 14:39:43  niklas
-- Added types Step_Count_T and Node_Count_T and changed the types
-- Step_Index_T and Node_Index_T to subtypes. The functions Max_Step
-- and Max_Node now return Step/Node_Count_T, allowing a zero value.
--
-- Revision 1.57  2007/07/21 18:18:42  niklas
-- BT-CH-0064. Support for AVR/IAR switch-handler analysis.
--
-- Revision 1.56  2007/06/13 16:28:18  niklas
-- Improved description of edge conditions.
--
-- Revision 1.55  2007/03/22 12:53:25  niklas
-- Added function Prime_Address (Step_Tag_T).
--
-- Revision 1.54  2007/01/13 13:51:05  niklas
-- BT-CH-0041.
--
-- Revision 1.53  2006/11/20 20:20:19  niklas
-- BT-CH-0037.
--
-- Revision 1.52  2006/10/24 08:44:32  niklas
-- BT-CH-0028.
--
-- Revision 1.51  2006/05/29 11:22:35  niklas
-- BT-CH-0023.
--
-- Revision 1.50  2006/05/18 13:02:22  niklas
-- Added functions Only_Predecessor and Only_Successor.
--
-- Revision 1.49  2006/05/17 20:07:43  niklas
-- Added the No_Node constant.
--
-- Revision 1.48  2006/05/06 06:59:22  niklas
-- BT-CH-0021.
--
-- Revision 1.47  2006/04/28 09:16:46  niklas
-- Added function Is_Member (Tag, Step_List).
--
-- Revision 1.46  2006/02/24 15:55:54  niklas
-- Added an overriding "=" for Step_Tag_T, to make sure that we
-- use a possible overriding Processor."=" for the State.
-- Added function State (Step_Tag_T) for brevity.
-- Removed superfluous "in" modes from some function parameters.
--
-- Revision 1.45  2005/10/20 15:25:52  niklas
-- Added function Effort (Step).
--
-- Revision 1.44  2005/10/13 19:27:39  niklas
-- BT-CH-0014.
--
-- Revision 1.43  2005/09/12 19:03:00  niklas
-- BT-CH-0008.
--
-- Revision 1.42  2005/09/05 11:23:38  niklas
-- BT-CH-0007.
--
-- Revision 1.41  2005/09/03 11:50:29  niklas
-- BT-CH-0006.
--
-- Revision 1.40  2005/09/01 08:03:21  niklas
-- Removed unused function Steps_By_Address.
--
-- Revision 1.39  2005/08/08 13:59:49  niklas
-- Implemented an "=" operation for Node_Set_T to work around a
-- bug with packed boolean arrays in Gnat 3.15p.
--
-- Revision 1.38  2005/07/01 10:53:48  niklas
-- Added exception False_Path. Not yet used here.
--
-- Revision 1.37  2005/02/23 09:05:19  niklas
-- BT-CH-0005.
--
-- Revision 1.36  2005/02/16 21:11:45  niklas
-- BT-CH-0002.
--
-- Revision 1.35  2004/10/10 09:58:38  niklas
-- Add function Time for node edges.
--
-- Revision 1.34  2004/04/25 20:41:03  niklas
-- Corrected description of Collect_Blocks.
--
-- Revision 1.33  2004/04/25 17:44:06  niklas
-- First Tidorum version.
-- Adapted to use Cell_T stuff from Storage and not from Arithmetic.
-- Since Graph_T, Step_T and Step_Edge_T (and others) have reference
-- semantics, as parameters they have mode "in" even when the subprogram
-- updates the underlying graph, edge or step-edge object.
-- Added Dynamic_Edge concept. This will replace flow-dynamism in steps.
-- Added the feasible/infeasible concept for edges, including the
-- function Feasible_Predecessors (Step).
-- Removed the execution-time attribute of node edges (Edge_T). The
-- execution time of a node edge is now always taken from the execution
-- time of the corresponding step edge. Consequently, removed the
-- procedures Set_Time (Edge_T) and Update_Edge_Times.
-- Added the procedure Set_Pointers to support partial evaluation of
-- dynamism pointer expressions.
-- Added support for crude aliasing analysis using alias ranges.
-- Moved the operations Count_Memory_Traffic and Work to the child package
-- Flow.Execution, where they are more at home.
-- Added functions Is_Used and Is_Defined for Storage Location_T values,
-- where the interesting cell depends on the code address.
-- Modified Add_Cells_Used (By => Node_T) to include cells used in the
-- precondition of the edges that leave the node.
-- Added several trivial operations and constants for more access and
-- control over the data structures here defined.
--
-- Revision 1.32  2003/03/11 08:22:40  holsti
-- Added Update_Edge_Times.
--
-- Revision 1.31  2001/12/10 15:07:19  holsti
-- Return_Nodes description corrected.
-- Null_Edge added for use in Node_Edge.
--
-- Revision 1.30  2001/11/19 11:05:58  saarinen
-- Added functions Sources and Targets for Step_Edge_List_T.
-- Added function Return_Nodes.
-- Added function Node_Edge.
--
-- Revision 1.29  2001/05/21 13:42:02  holsti
-- The following changes for NC_117:
-- Added execution-time attribute to edges and loose edges.
-- Added the functions Time and Set_Time (for step-edges and node-edges).
-- Added a Time parameter to Add_Edge operations.
-- Added function Edge_At to get a step edge by its index.
-- Added function Step_Edge to get the step-edge that corresponds
-- to a node-edge.
-- It is now possible to have more than one edge between two given
-- steps or two given nodes. Consequently, removed the function Edge
-- (fortunately unused) to return the edge between two steps, and also
-- the exception Edge_Unknown.
-- Added function Return_Steps.
--
-- The following changes for NC_120, NC_121, NC_122, NC_128:
-- Replaced function Effort with function Work.
-- Added operation Count_Memory_Traffic.
--
-- Revision 1.28  2001/03/19 08:16:13  ville
-- Added function to add call step
--
-- Revision 1.27  2001/03/16 09:35:14  ville
-- Added function returning effort of step list
--
-- Revision 1.26  2001/03/08 10:38:29  holsti
-- Return_Edges and Cells_Defined (By: Graph) added.
--
-- Revision 1.25  2001/01/13 11:37:07  holsti
-- Removed to-be's in favour or NC's.
--
-- Revision 1.24  2001/01/07 21:56:42  holsti
-- Step operations added to suppport liveness analysis.
--
-- Revision 1.23  2000/12/28 19:43:44  holsti
-- Node_At (Step_Address_T) removed (not used).
--
-- Revision 1.22  2000/12/28 17:38:34  holsti
-- Unresolved_Flow added.
--
-- Revision 1.21  2000/12/22 13:33:20  sihvo
-- Added Steps_By_Address.
--
-- Revision 1.20  2000/08/18 18:14:33  holsti
-- Added edge-count types to allow for graph with no edges.
-- Unbounded_Vectors Index_Type removed.
--
-- Revision 1.19  2000/07/24 22:23:48  holsti
-- Resolution_T added.
--
-- Revision 1.18  2000/07/21 21:07:05  holsti
-- Unresolved_Steps replaces Steps_With_Unresolved_Flow.
--
-- Revision 1.17  2000/07/18 19:51:57  holsti
-- Node_Containing implemented.
--
-- Revision 1.16  2000/07/16 13:06:50  holsti
-- Node_Index_List_T and Add_Cells_Accessed added.
--
-- Revision 1.15  2000/07/13 14:17:56  langback
-- Changed return value type of Cells_Used and Cells_Defined.
--
-- Revision 1.14  2000/07/13 10:59:18  langback
-- Added function Cells_Used and Cells_Defined (not yet implemented).
--
-- Revision 1.13  2000/07/13 10:49:07  langback
-- Added function Node_Containg (however, not implemented yet).
--
-- Revision 1.12  2000/07/12 20:42:25  holsti
-- Added Graph parameter to To_List of Node_Set_T.
--
-- Revision 1.11  2000/07/06 13:28:57  langback
-- Correction to Edge_At function
--
-- Revision 1.10  2000/07/06 13:23:48  langback
-- Added Edge_At function over Node_Indexes
--
-- Revision 1.9  2000/07/06 12:51:56  holsti
-- Node sets added. Edge_At added.
--
-- Revision 1.8  2000/06/28 13:37:41  holsti
-- Dynamics and Bounds added.
--
-- Revision 1.7  2000/06/12 14:02:58  holsti
-- Added Cells_In (Graph) (not implemented yet).
--
-- Revision 1.6  2000/06/11 21:37:31  holsti
-- Added Condition for step-edges.
--
-- Revision 1.5  2000/06/11 19:03:53  holsti
-- First implementation.
--
-- Revision 1.4  2000/05/05 12:06:58  holsti
-- Null_Graph added.
--
-- Revision 1.3  2000/04/27 10:45:52  holsti
-- Using loose graph edges.
--
-- Revision 1.2  2000/04/26 14:03:58  holsti
-- Added Is_Empty and Take_First.
--
-- Revision 1.1  2000/04/23 21:41:14  holsti
-- Added Flow package.
--


with Arithmetic;
with Calling;
with License;
with Processor;
with Storage;
with Storage.Bounds;


package Flow is
--
-- A flow graph is equivalent to a finite-state automaton, with
-- an initial node, edges (arcs, transitions) between nodes, and
-- a subset of terminal nodes (returns).
--
-- A node in the flow graph corresponds to a specific state of
-- the target processor's program-sequencing (program flow control)
-- function. This is represented by the type Processor.Flow_State_T.
-- For a simple processor, this can be just the "Program Counter".
-- For a more complex processor, additional sequencer state
-- must be simulated, such as instruction pipelines, hardware
-- loops, etc.
--
-- In some cases, a given value of Processor.Flow_State_T needs to
-- be represented by more than one node in the flow graph, to separate
-- the several contexts in which this Flow_State_T can occur. Therefore
-- the nodes in the graph are identified by a composite type that has
-- the Flow_State_T as one component and a context reference as the
-- other component. This pair is called a "step tag" for reasons that
-- will become apparent below.
--
-- An edge in the flow graph corresponds to a state transition
-- in the target processor's program sequencer, most often to
-- fetch a new instruction.
--
-- For WCET analysis, the flow graph of each relevant subprogram
-- in the target program is constructed, by decoding the subprogram
-- starting from its entry address.
--
-- A flow graph can be viewed on two levels: the step level
-- and the basic-block level (or just block level, for short):
--
-- > A step is an atomic graph node. Decoding a target instruction
--   generates steps and edges between steps.
--
-- > A basic block is a maximal, linear chain of steps such that
--   it can be entered only through its first step and left only
--   through its last step.
--
-- Decoding a subprogram generates the steps, and edges between
-- steps. When these are complete, a specific algorithm is needed
-- to create the basic blocks. Edges between steps induce edges
-- between blocks, but the two levels of edges are distinguished,
-- just as the two levels of nodes are.
--
-- Most graph-analysing algorithms in Bound-T work on the block
-- level. Types and subprograms for the step level are named
-- with the word "Step" and "Step_Edge", while those on the block
-- level are named with "Node" and "Edge".
--
-- To simplify the incremental decoding and flow-graph building,
-- a flow-graph can contain "loose" edges on the step level.
-- A loose edge is an edge that starts at an existing step, and
-- has a known step-tag as target, but does not yet have a target
-- node because the instruction at the target step-tag has not yet
-- been decoded. There are separate operations to add bound
-- (non-loose) edges (giving both source and target steps) and to
-- add an edge that might be loose (giving the source step and the
-- step-tag of the target).
--
-- The result of WCET analysis is usually to add some information
-- to each node and/or edge in a flow-graph. To cater for such
-- additions, all nodes (whether steps or blocks) and all edges
-- (whether between steps or between blocks) are numbered (indexed)
-- sequentially. Thus, the additional data can be represented as
-- arrays indexed with the node or edge number.
--
-- Loose edges are not numbered and have no influence on the
-- numbering of bound edges.
--
-- When the decoded subprogram contains dynamic addressing, either
-- of data memory (indexed load/store) or of program memory (indexed
-- jump or call), some steps/nodes may be left in an unresolved
-- state when first decoded. For dynamic jumps or calls, the full
-- set of successors may not be known; for dynamic data accesses,
-- the precise effect may not be known. When data-flow analysis
-- has been applied to the incomplete flow graph, the dynamic
-- addresses may be resolved to complete the flow graph and the
-- effects. This package keeps track of the remaining unresolved
-- steps in the flow graph.


   --
   ---   Flow graphs
   --


   type Graph_T is private;
   --
   -- A flow-graph.
   -- The attributes of a flow-graph include:
   --
   -- > The entry step (and block).
   -- > The set of steps and blocks.
   -- > The set of edges (at step level and block level).
   -- > Possibly a set of "loose" edges where the source step is known
   --   and the target step-tag is known, but where the target step
   --   does not yet exist (or did not exist when the loose edge was
   --   created).
   -- > Possibly a set of dynamic edges where the source step is known
   --   but the target step-tag is dynamically computed.
   --
   -- Before use, a Graph_T object must be initialized with the Create
   -- procedure.
   --
   -- A flow-graph can be empty. A newly Created graph is empty. An
   -- empty graph has no nodes nor edges nor an entry step.
   --
   -- Graph_T has reference semantics. The values of type Graph_T
   -- returned by functions in this package are to be understood as
   -- references to some underlying flow-graph objects; any update
   -- applied to such a reference is visible via all references to
   -- the same object. Parameters of type Graph_T generally have
   -- mode "in" even when the subprogram updates the underlying
   -- graph object.


   procedure Create (Graph : in out Graph_T);
   --
   -- Creates a new, empty graph.


   function Is_Empty (Graph : Graph_T) return Boolean;
   --
   -- Whether the graph is empty.


   Step_Tag_Unknown : exception;
   --
   -- Raised when some element of a graph (step or node) is
   -- sought by its tag-value, but the graph does not contain
   -- an element with the given tag.


   Step_Tag_In_Use : exception;
   --
   -- Raised when a step with a given step-tag is to be added to a
   -- graph, but the graph already contains a step with this tag.


   False_Path : exception;
   --
   -- Raised when any analysis finds a contradiction in the target
   -- program state, in the current analysis context (target program
   -- state), to signal that the execution path to this state is false
   -- (infeasible).


   type Step_Count_T is new License.Size_Measure_T;
   type Node_Count_T is new Natural;
   --
   -- A number of steps, a number of nodes.
   -- For licensing purposes, the size of the analysed part of
   -- the program is measured by the number of steps in the
   -- control-flow graphs in the analysis.


   subtype Step_Index_T is Step_Count_T range 1 .. Step_Count_T'Last;
   subtype Node_Index_T is Node_Count_T range 1 .. Node_Count_T'Last;
   --
   -- Steps and nodes are numbered from 1 in each graph.


   type Step_Edge_Count_T is new Natural;
   type Edge_Count_T      is new Natural;
   --
   -- Edges (of both levels) are counted with these types.
   -- Note that a non-null graph may not have any edges (if it
   -- has only one step, or node). A graph with more than one step,
   -- and thus some step-edges, may have only one node, and thus no
   -- node edges.


   subtype Step_Edge_Index_T is Step_Edge_Count_T
      range 1 .. Step_Edge_Count_T'Last;

   subtype Edge_Index_T is Edge_Count_T
      range 1 .. Edge_Count_T'Last;
   --
   -- Edges (of both levels) are numbered from 1 in each graph.
   -- However, there may not be any edges.


   type Node_Index_List_T is array (Positive range <>) of Node_Index_T;
   --
   -- A list of node indices, with multiple uses.
   -- The main use for this type is as an alternative representation
   -- of a node-set (see Node_Set_T, below).


   function Max_Step      (Graph : in Graph_T) return Step_Count_T;
   function Max_Node      (Graph : in Graph_T) return Node_Count_T;
   function Max_Step_Edge (Graph : in Graph_T) return Step_Edge_Count_T;
   function Max_Edge      (Graph : in Graph_T) return Edge_Count_T;
   --
   -- These functions return the highest sequential number (index)
   -- of any step, block (node), or edge in the graph.
   -- All the number sequences start at 1 and run independently.
   -- If the graph contains no steps, Max_Step returns zero.
   -- If the graph contains no nodes, Max_Node returns zero.
   -- If the graph contains no edges at the indicated level, the
   -- edge number zero (for "no edge") is returned.


   function Code_Range (Graph : Graph_T) return Storage.Code_Address_Range_T;
   --
   -- The smallest interval of code addresses that contains all the
   -- prime addresses of the steps in the given Graph.
   --
   -- Note that the steps in the Graph do not necessarily cover every
   -- address in the interval, perhaps because the code of the subprogram
   -- is not contiguous in memory or because some step(s) represent more
   -- than one code memory element. Moreover, the subprogram may contain
   -- instructions with addresses outside this interval, because a step
   -- may correspond to a sequence of instructions and only the address of
   -- the first instruction becomes the prime address of the step.


   --
   ---   Step contexts, step data, and step tags
   --
   --
   -- We may need to analyse the same instruction, or processor
   -- flow-state, in different context, separated, for example, by
   -- the execution path that reaches this instruction, or by the
   -- values of some variables on which the instruction's effect
   -- depends, or when integrating a callee subprogram into the
   -- flow-graph of a caller subprogram ("integrated" decoding) as may
   -- be needed for the special "helper" routines that some compilers
   -- use to implement the preludes and postludes of subprograms.
   --
   -- When an instruction is analysed under different conditions in
   -- this way, we will create as many separate steps in the flow-graph
   -- to represent this instruction and its effect on the execution.
   -- The steps must be labeled with different labels or tags.
   --
   -- We group the different reasons for several analyses of the same
   -- instruction into two groups:
   --
   -- > Values of variables (storage cells) or "data state".
   --
   -- > All the other reasons, eg. integrated decoding of callees.
   --
   -- Accordingly, we define two data types to be components of the
   -- tag of a step, in addition to Processor.Flow_State_T:
   --
   -- > Step_Data_T    to represent the values of variables.
   -- > Step_Context_T for all the other reasons.


   type Step_Data_T is abstract tagged null record;
   --
   -- We use different Step_Data_T objects to separate the processor
   -- states depending on the content (values) of certain storage cells
   -- although the Processor.Flow_State_T is the same, so that the
   -- analysis can take these data values into account.
   --
   -- The set of cells that are tracked in this way depends on the
   -- situation and may be quite different for different Step_Data_T
   -- objects.
   --
   -- A Step_Data_T models or constrains the values of storage cells
   -- and is thus similar to Storage.Bounds.Bounds_T. We chose not to
   -- derive Step_Data_T from Storage.Bounds.Bounds_T because this lets
   -- each type derived from Step_Data_T use the kind of Bounds_T and
   -- its particular kind of Apply operation (the general Apply for
   -- Bounds_T is rather weak in its parameter profile).
   --
   -- The primitive operations of Step_Data_T need, as parameters,
   -- several data types that will be defined later, so the primitive
   -- operations are also defined later.


   type Step_Data_Ref is access all Step_Data_T'Class;
   --
   -- Step data states are separated by the *identity* of a Step_Data_T
   -- object, not by the *value* of such an object. Thus, we use an
   -- access to a Step_Data_T object to identify the data state.
   --
   -- The concrete data-state types derived from Step_Data_T should
   -- keep track of the data states that have been encountered in a
   -- simulation and the corresponding Step_Data_Ref values. When the
   -- simulation reaches a state that has occurred before, the same
   -- Step_Data_Ref should be used (otherwise the number of steps
   -- generated in the simulation may explode).
   --
   -- Another reason for using an access type is that this package (Flow)
   -- should not know of, nor depend on, the particular kinds of
   -- data-state objects that may be needed in the analysis. An access to
   -- a tagged class answers this need nicely.


   function Image (Item : Step_Data_Ref) return String;
   --
   -- Describes both the identity (location, address) and the content
   -- (data constraints).


   Any_Data : constant Step_Data_Ref := null;
   --
   -- Denotes the absence of constraints on cell values.
   --
   -- This value means that the flow-graph construction process (decoding
   -- and flow tracing) is currently not simulating any data states.
   --
   -- Data-state types derived from Step_Data_T shall *not* use Any_Data
   -- to mean that simulation continues but at this point cannot place
   -- any bounds on the data values. Instead, a non-null Step_Data_Ref
   -- shall be used, referring to a data state object that, through its
   -- content, shows that no bounds are known (eg. by using
   -- Storage.Bounds.Universal_Interval).


   type Step_Context_T;
   --
   -- We use different Step_Context_T objects to separate the processor
   -- states that have the same value of Processor.Flow_State_T but
   -- which should be distinct steps (nodes) in the flow-graph so
   -- that the analysis can take the context into account.
   --
   -- See below for the full declaration of this type.


   type Step_Context_Ref is access all Step_Context_T'Class;
   --
   -- Steps contexts are separated by the *identity* of a Step_Contex_T
   -- object, not by the *value* of such an object. Thus, we use an
   -- access to a Step_Context_T object to identify the context.
   --
   -- Another reason for using an access type is that this package (Flow)
   -- should not know of, nor depend on, the particular kinds of
   -- context objects that may be needed in the analysis. An access to
   -- a tagged class answers this need nicely.


   No_Context : constant Step_Context_Ref := null;
   --
   -- A null or empty context.


   type Step_Context_T is abstract tagged record
      Outer : Step_Context_Ref;
   end record;
   --
   -- See the earlier incomplete declaration for the general description
   -- and motivation of this type.
   --
   -- The contexts of steps in a flow graph may need to be nested.
   -- For example, when "integrated" decoding a callee into the flow-graph
   -- of the caller, in an "integrated context", the callee's code may
   -- contain steps that need additional context, for example a new
   -- level of integrated decoding. Nested contexts are implemented by
   -- letting each context object refer to its Outer context, forming
   -- a root-directed, linked, tree of context objects.
   --
   -- Outer
   --    Reference to the outer context level, if this is a nested
   --    context. Null for an unnested (outermost) context.


   function Image (Item : Step_Context_T) return String
   is abstract;
   --
   -- A brief mark to show the kind of context.


   function Nested_Image (Item : Step_Context_Ref) return String;
   --
   -- The Images of the context-nest for the given context.
   -- This is the concatenation of the Images of the nested contexts,
   -- from outer to inner.


   type Step_Tag_T is record
      Context : Step_Context_Ref;
      Data    : Step_Data_Ref;
      State   : Processor.Flow_State_T;
   end record;
   --
   -- Unique identifier (tag, key) of a steps (and atomic nodes) in a
   -- flow graph.
   --
   -- Context
   --    The generic (not processor-specific) part of the tag, apart
   --    from the Data state.
   --    Separates flow-graph steps that need to be analysed separately
   --    but have the same processor-dependent tag-value (the State)
   --    and the same Data state.
   --    May be No_Context.
   -- Data
   --    The data state of the tag. This may be generic or may be a
   --    data-state model defined for a specific processor and perhaps
   --    a specific compiler.
   --    Separates flow-graph steps that need to be analysed separately
   --    for different data states but have the same Context and State.
   --    Provides operations that can refine and filter the effects
   --    and conditions of steps and edges as they are being added to
   --    the flow graph, including resolving dynamic memory references
   --    and dynamic jumps, calls and calling protocols.
   --    May be Any_Data.
   -- State
   --    The processor-specific part of the tag. Provides all the
   --    information that the processor-specific instruction decoder
   --    needs to decode an instruction and insert the corresponding
   --    step(s) and edges into the flow graph.
   --
   -- Note that the Data state always models a superset of the data
   -- state that can hold on entry to the step, *even* if the flow-graph
   -- is extended with new steps and edges after this step is created.
   -- Any analysis based on the Data state is thus *definitive* for this
   -- step. For example, if the Data state resolves a boundable edge
   -- leaving this step, the boundable edge is thereby *fully* resolved
   -- and can be deleted.
   --
   -- Note, however, that this axiom (Data state is a superset) is
   -- not automatically maintained and ensured. The processor-specific
   -- types derived from Step_Data_T must take care to maintain the
   -- axiom by preventing new edges from targeting existing steps with
   -- Data components that do not include the data states from the edge.


   function Image (Item : Step_Tag_T) return String;
   --
   -- A readable but compact description of the Step Tag, built from
   -- Processor.Image (Item.State) and Nested_Image (Item.Context).


   function Full_Image (Item : Step_Tag_T) return String;
   --
   -- An image that is complete enough to correspond to the "=" function.


   function "=" (Left, Right : Step_Tag_T) return Boolean;
   --
   -- Equality is defined with predefined "=" for the Context and Data
   -- (comparing context and data references, not the underlying objects)
   -- and Processor."=" for the State.


   function "<" (Left, Right : Step_Tag_T) return Boolean;
   --
   -- The ordering is lexicographic with the State component
   -- most significant, the Context component next, and the Data
   -- component last.
   --
   -- The ordering of States is defined by Processor."<" and the
   -- ordering for Context and Data is arbitrary (by address).


   --
   ---   In_Transit and Transit
   --


   function In_Transit return Step_Data_Ref;
   --
   -- A special and unique Step_Data_Ref that occurs only in a
   -- Step_Tag_T defined by the Transit function below. This value
   -- is used to show that a Decoder has used Transit to create the
   -- target of an edge, delegating the task of data simulation to
   -- the Add_Edge functions in this package.
   --
   -- This value and the underlying Step_Data_T'Class should not
   -- be used in any other way.


   function Transit (
      From : Step_Tag_T;
      To   : Processor.Flow_State_T)
   return Step_Tag_T;
   --
   -- Models a flow-state transition From a state To a new state,
   -- by constructing the step-tag that has the same context as in
   -- From but the new flow state defined by To.
   --
   -- Note that the function does *not* model how the effect of
   -- the From step changes the data state. If From.Data is Any_Data
   -- then so is the Data in the resulting tag, otherwise the
   -- resulting tag has the special In_Transit Data component.


   --
   ---   Steps in flow graphs
   --


   type Step_T is private;
   --
   -- A step in a flow-graph.
   -- The attributes of a step include:
   --
   -- > Its index number (1..).
   -- > Its identifying tag (Step_Tag_T).
   -- > Its computational effect, in terms of cell updates. This is
   --   the "primitive" effect, as created by the instruction decoder.
   --   More refined (resolved, specialized, bounded) effects may
   --   later be associated with the step (see Flow.Computation).
   -- > Additional processor-dependent information generated
   --   and used by the instruction decoder.
   --
   -- The edges connected to the step are not considered attributes
   -- of the step, but of the graph.
   --
   -- After steps have been collected into basic blocks, the step
   -- gains a new attribute: the node that contains the step.
   --
   -- Step_T has reference semantics. The values of type Step_T
   -- returned by functions in this package are to be understood
   -- as references to some underlying step objects; any update
   -- applied to such a reference is visible via all references
   -- to the same object. Parameters of type Step_T generally
   -- have mode "in" even when the subprogram updates the
   -- underlying step object.


   No_Step : constant Step_T;
   --
   -- Signifies the absence of a step.
   -- The default initial value of any Step_T object.


   type Step_List_T is array (Positive range <>) of Step_T;
   --
   -- A set (or list) of steps, for example, all successors
   -- of a given step.


   function Index_Image (Item : Step_List_T) return String;
   --
   -- Displays the indices of the listed steps, in the order
   -- they are listed.


   function No_Steps return Step_List_T;
   --
   -- An empty (null) list of steps.


   function Is_Member (
      Tag   : Step_Tag_T;
      Steps : Step_List_T)
   return Boolean;
   --
   -- Whether some step in the list of Steps has the given Tag.


   function Index (Step : Step_T) return Step_Index_T;
   --
   -- The index number of a step.
   -- These numbers run from 1 to Max_Step.


   function Tag (Step : Step_T) return Step_Tag_T;
   --
   -- The identifying tag of a step.


   function State (Step : Step_T) return Processor.Flow_State_T;
   --
   -- Short for Tag (Step).State.


   function State (Tag : Step_Tag_T) return Processor.Flow_State_T;
   --
   -- Same as Tag.State.


   function Context (Step : Step_T) return Step_Context_Ref;
   --
   -- Short for Tag (Step).Context.


   function Data (Step : Step_T) return Step_Data_Ref;
   --
   -- Short for Tag (Step).Data.
   -- This is the data state on entry to the step.


   function Data (Tag : Step_Tag_T) return Step_Data_Ref;
   --
   -- Same as Tag.Data.
   -- This is the data state on entry to the step with this Tag.


   function Post_Data (Step : Step_T) return Step_Data_Ref;
   --
   -- The data state on exit from the Step, in other words Data (Step)
   -- transformed by Effect (Step).


   function Prime_Address (Step : in Step_T)
   return Processor.Code_Address_T;
   --
   -- Short for Processor.Prime_Address (State (Step)).


   function Prime_Address (Tag : in Step_Tag_T)
   return Processor.Code_Address_T;
   --
   -- Short for Processor.Prime_Address (State (Tag)).


   function Tag_Less (Left, Right : Step_T) return Boolean;
   --
   -- Short for Tag (Left) < Tag (Right).


   function Transit (
      From : Step_T;
      To   : Processor.Flow_State_T)
   return Step_Tag_T;
   --
   -- Short for Transit (From => Tag (From), To => To).
   --
   -- Note that the function does *not* model how the effect of
   -- the From step changes the data state.


   function Effect (Step : in Step_T) return Arithmetic.Effect_T;
   --
   -- The arithmetical effect of the step.


   function Effect (Step : in Step_T) return Arithmetic.Effect_Ref;
   --
   -- A reference to the effect of the step.
   -- If the user modifies the effect through the reference, the
   -- modification becomes permanently associated with the step.
   --
   -- NOTE that the user can cause several steps to share the
   -- same Effect_T object; if this object is modified via the
   -- reference, all steps see the modification.


   function Is_Feasible (Step : Step_T; Within : Graph_T)
   return Boolean;
   --
   -- Whether the step is really feasible (reachable) in some execution
   -- of the flow-graph. In some cases, compilers (or our decoders)
   -- can generate steps that are unreachable because logical conditions
   -- prevent any execution from entering the step.


   procedure Set_Effect (
      Step   : in Step_T;
      Effect : in Arithmetic.Effect_T);
   --
   -- Changes the effect of the step.
   --
   -- TBC precondition that the
   -- step has no successors (is not the source of any edge) or that
   -- the Data of the step-tag is Any_Data. Otherwise, the Data parts
   -- of the tags of the successors could become inconsistent because
   -- they depend on Post_Data (Step) which depends on Effect (Step).
   --
   -- The new effect (a copy of the Effect parameter) is not shared
   -- with any other step.


   procedure Set_Effect (
      Step   : in Step_T;
      Effect : in Arithmetic.Effect_Ref);
   --
   -- Changes the effect of the step, by reference.
   --
   -- TBC preconditions as in the Set_Effect above.
   --
   -- The step's effect will be defined by reference to Effect.all.
   -- The user must ensure that Effect.all exists as long as needed.
   -- If the same Effect reference is assigned to several steps,
   -- these steps will share the underlying Effect_T object, Effect.all.


   function Target_Range (Step : Step_T)
   return Storage.Alias_Range_T;
   --
   -- The alias range that can be reached by the assignments in the
   -- step's effect, under the given level of alias checking.


   function All_Steps (Within : Graph_T) return Step_List_T;
   --
   -- All the steps in the graph, listed so that the (Positive) index
   -- of a step in the list is the same as the (Step_Index_T) index of
   -- the step.


   function Step_At (
      Tag    : Step_Tag_T;
      Within : Graph_T)
   return Step_T;
   --
   -- Locates the step (if any) with the given Tag,  Within the
   -- given graph.
   -- If there is no such step, raises Step_Tag_Unknown.


   function Steps_Containing (
      Address   : Processor.Code_Address_T;
      Within    : Graph_T;
      Calls_Too : Boolean)
   return Step_List_T;
   --
   -- Locates the steps with Prime_Address equal to the given Address,
   -- optionally including synthetic call steps too.
   -- There may be zero, one, or several such steps.


   function Step_At (
      Index  : Step_Index_T;
      Within : Graph_T)
   return Step_T;
   --
   -- The step identified by the given Index, Within the given graph.
   -- If the index is out of range, Constraint_Error is raised.


   function Entry_Step (Graph : in Graph_T) return Step_T;
   --
   -- The entry step (entry point) of the graph, which is
   -- always the first step added to the graph.


   function Return_Steps (Graph : Graph_T) return Step_List_T;
   --
   -- The return steps of the graph, which are the steps that
   -- have no leaving edges (Number_From is zero).


   procedure Set_Info (
      Step : in Step_T;
      Info : in Processor.Step_Info_T);
   --
   -- Updates the processor-dependent information in the step.


   function Info (Step : Step_T) return Processor.Step_Info_T;
   --
   -- Gets the processor-dependent information in the step.


   function Effort (Step : Step_T) return Processor.Effort_T;
   --
   -- The computational effort of the step.
   -- Same as Processor.Effort (Info (Step)).


   function Some_Dynamic_Edges (Within : Graph_T)
   return Boolean;
   --
   -- Whether there are some boundable edges Within the
   -- given graph. The state of resolution of these edges
   -- does not matter.


   function Is_Member (Step : Step_T; Of_List : Step_List_T)
   return Boolean;
   --
   -- Whether the given Step is an element Of the given List.


   type Bounded_Step_List_T (Max_Length : Natural) is record
      List : Step_List_T (1 .. Max_Length);
      Last : Natural := 0;
   end record;
   --
   -- A dynamically extensible list of steps with a fixed maximum length.
   -- The list does not contain duplicated steps.
   -- The list is not necessarily ordered in any way.
   -- Each object of this type is initialized to the null list.
   --
   -- List, To
   --    The steps in the list are List(1 .. To).


   procedure Erase (List : in out Bounded_Step_List_T);
   --
   -- Clears the List to a null list.


   procedure Add (
      Step : in     Step_T;
      To   : in out Bounded_Step_List_T);
   --
   -- Adds (inserts) the Step To the given (bounded) step list,
   -- without adding duplicates. If the target list does not have
   -- space for the Step, the procedure propagates Constraint_Error.


   procedure Add (
      Steps : in     Step_List_T;
      To    : in out Bounded_Step_List_T);
   --
   -- Adds (inserts) all the Steps from the given list To the
   -- given (bounded) step list, without adding duplicates.
   -- If the target list does not have space for all steps, the
   -- procedure propagates Constraint_Error.


   function To_List (Item : Bounded_Step_List_T) return Step_List_T;
   --
   -- All the elements from the given bounded step-list, as an
   -- ordinary step-list.


   function Total_Number_Of_Steps
   return Step_Count_T;
   --
   -- The total number of steps created and added to any flow-graph.
   -- This is used for licensing based on the size of the program
   -- under analysis.


   function Size_Licence_Valid return Boolean;
   --
   -- Whether the program-size limit of the current licence allows
   -- analysis of code of the current size (measured by the number
   -- of steps in all flow-graphs).


   --
   ---   Step edges (edges between steps)
   --


   type Step_Edge_T is private;
   --
   -- An edge between steps in a flow-graph.
   -- The attributes of an edge include:
   --
   -- > Its index number (1..).
   -- > The source step from which the edge starts.
   -- > The target step at which the edge ends.
   -- > The logical precondition for executing the edge, a boolean
   --   expression to be evaluated after executing the effect of
   --   the source step of the edge.
   -- > The execution time of the edge, as some number of
   --   processor cycles.
   --
   -- Step_Edge_T has reference semantics. The values of type
   -- Step_Edge_T returned by functions in this package are to be
   -- understood as references to some underlying step-edge objects;
   -- any update applied to such a reference is visible via all
   -- references to the same object. Parameters of type Step_Edge_T
   -- generally have mode "in" even when the subprogram updates the
   -- underlying step object.
   --
   -- Within the logical preconditions of the edges emanating from
   -- a given step, all uses of a given cell represent the same value,
   -- even if the cell is a volatile cell. To be precise: if a volatile
   -- cell C is not assigned in the effect of the source step, then all
   -- uses of C in the effect of the source step, as well as in the
   -- preconditions of the edges leaving the source step, represent the
   -- same value of C, conceptually read from C when the step is entered.
   -- If the volatile cell C is assigned a new value in the effect of
   -- the source step, then all uses of C in the preconditions of the
   -- edges leaving the source step represent the value assigned. In
   -- other words, a volatile cell is not really volatile within a step
   -- nor during the evaluation of the preconditions of the edges that
   -- leave the step. The value of the volatile cell is lost (may change
   -- due to external input) only after evaluating those preconditions,
   -- before entering the next step.
   --
   -- The above rule or assumptions regarding volatile cells make it
   -- easier to analyse the preconditions, for example to detect
   -- mutually exclusive but complete pairs of edges, such as two
   -- edges where one edge has the precondition "x = 0" and the other
   -- edge has "x /= 0". (Note that, for such pairs, the preconditions
   -- are sufficient as well as necessary.)


   No_Step_Edge : constant Step_Edge_T;
   --
   -- Signifies the absence of a step-edge.
   -- The default initial value of any Step_Edge_T object.


   function Index (Edge : in Step_Edge_T) return Step_Edge_Index_T;
   --
   -- The index number of a step edge.
   -- These numbers run from 1 to Max_Step_Edge.


   function Source (Edge : in Step_Edge_T) return Step_T;
   function Target (Edge : in Step_Edge_T) return Step_T;
   --
   -- The source and target steps of the edge.



   function Condition (Edge : in Step_Edge_T)
   return Arithmetic.Condition_T;
   --
   -- The precondition (necessary but perhaps not sufficient) for
   -- executing the edge, when evaluated after the execution of the
   -- source step of the edge.


   function Is_Feasible (Edge : Step_Edge_T)
   return Boolean;
   --
   -- Whether the edge is really feasible (reachable) in some execution
   -- of the flow-graph. In some cases, compilers (or our decoders)
   -- can generate edges that are unreachable because logical conditions
   -- prevent any execution from entering the edge, for example because
   -- the edge's own precondition is constantly False.


   procedure Set_Condition (
      On : in Step_Edge_T;
      To : in Arithmetic.Condition_T);
   --
   -- Changes the precondition for executing the edge. The condition
   -- is expressed for evaluation in the state following the execution
   -- of the source step of the edge.


   function Time (Along : in Step_Edge_T) return Processor.Time_T;
   --
   -- The execution time of the edge.
   --
   -- For most processors, instructions can be decoded so that
   -- edges take no time to execute. In complex processors, a
   -- positive execution time for an edge can be used to model
   -- a blocking time or extra delay caused by some interaction
   -- between the processor resources used by the source step
   -- and those used by the target step.
   --
   -- For example, in the ADSP-21020, if the source step loads
   -- a DAG register and the target step uses the same DAG to
   -- address memory, a conflict on an internal bus means that
   -- one NOP cycle is executed between the source and target
   -- steps. This can be modelled by setting one cycle as the
   -- execution time of the edge.


   procedure Set_Time (
      Along : in Step_Edge_T;
      To    : in Processor.Time_T);
   --
   -- Sets the execution time of the edge.


   type Step_Edge_List_T is array (Positive range <>) of Step_Edge_T;
   --
   -- A set (or list) of edges, for example, all edges that
   -- leave a given step.


   function No_Step_Edges return Step_Edge_List_T;
   --
   -- An empty (null) list of step edges.


   function Sources (
      Edges  : Step_Edge_List_T;
      Unique : Boolean := False)
   return Step_List_T;
   --
   -- The source steps of the given Edges.
   --
   -- If Unique is False, the result has the same index range as
   -- the Edges and contains the source of each edge, even if several
   -- edges have the same source.
   --
   -- If Unique is True, the result contains the source steps of the
   -- Edges without duplication. The index range of the result is not
   -- defined.


   function Targets (Edges : Step_Edge_List_T) return Step_List_T;
   --
   -- The target steps of the given edges.


   function Edge_At (
      Index  : Step_Edge_Index_T;
      Within : Graph_T)
   return Step_Edge_T;
   --
   -- The step edge identified by the given index, within the
   -- given graph.
   -- If the index is out of range, Constraint_Error is raised.


   function Edges_Into (Step : Step_T; Within : Graph_T)
   return Step_Edge_List_T;
   --
   -- All the edges that enter the step (Edge.Target = Step).


   function Only_Edge_Into (Step : Step_T) return Step_Edge_T;
   --
   -- The only edge that enters the step (Edge.Target = Step).
   -- If there are several entering edges, a Fault is emitted
   -- and the first such edge is returned (Constraint_Error
   -- if none).


   function Edges_From (Step : Step_T; Within : Graph_T)
   return Step_Edge_List_T;
   --
   -- All the edges that leave the step (Edge.Source = Step).
   -- Loose edges are not included.


   function Only_Edge_From (Step : Step_T) return Step_Edge_T;
   --
   -- The only edge that leaves the step (Edge.Source = Step).
   -- If there are several leaving edges, a Fault is emitted
   -- and the first such edge is returned (Constraint_Error
   -- if none).


   function Number_Into (Step : Step_T; Within : Graph_T) return Natural;
   --
   -- The number of edges that enter the step.
   -- Same as Edges_Into()'Length.


   function Number_From (Step : Step_T; Within : Graph_T) return Natural;
   --
   -- The number of edges that leave the step.
   -- Same as Edges_From()'Length.
   -- Loose edges are not counted.


   function Predecessors (Step : Step_T; Within : Graph_T)
   return Step_List_T;
   --
   -- The immediate predecessors of the given step.
   -- The given step itself may be included, if there is a
   -- looping edge from this step to itself.


   function Feasible_Predecessors (Step : Step_T; Within : Graph_T)
   return Step_List_T;
   --
   -- The immediate predecessors of the given step except those
   -- where the edge to the given step is marked infeasible by
   -- a precondition equal to Arithmetic.Never.
   -- Otherwise the same as Predecessors.


   function Successors (Step : Step_T; Within : Graph_T)
   return Step_List_T;
   --
   -- The immediate successors of the given step.
   -- The given step itself may be included, if there is a
   -- looping edge from this step to itself.


   --
   ---   Nodes (basic blocks)
   --


   type Node_T is private;
   --
   -- A node (basic block) in a flow-graph.
   -- The attributes of a node include:
   --
   -- > Its index number (1..).
   -- > Its identifying tag (Step_Tag_T).
   --   This is the same as the tag of the first step in the node.
   -- > The list of steps that it contains.
   -- > Whether some of the contains steps have unresolved
   --   dynamic data access or control flow.
   --
   -- The edges connected to the node are not considered
   -- attributes of the node, but of the graph.


   No_Node : constant Node_T;
   --
   -- Signifies the absence of a node.
   -- The default initial value of any Node_T object.


   function Index (Node : Node_T) return Node_Index_T;
   --
   -- The index number of a node.
   -- These numbers run from 1 to Max_Node.


   function Tag (Node : Node_T) return Step_Tag_T;
   --
   -- The identifying tag of a node.


   function Steps_In (Node : Node_T) return Step_List_T;
   --
   -- The steps in the node, listed in order from the first step
   -- to the last step. The list contains at least one step.
   -- In the step-graph, these steps form a maximal linear
   -- chain in which each step is connected only to the next step,
   -- when the predecessors of the first step and the successors of
   -- the last step are not considered.


   function First_Step (Node : Node_T) return Step_T;
   --
   -- The first step in the node. All edges leading to the
   -- node actually lead to this step, which is the sole entry
   -- point to the node.


   function Prime_Address (Node : Node_T)
   return Processor.Code_Address_T;
   --
   -- The Prime_Address of the First_Step of the Node.


   function Node_Containing (
      Step  : Step_T;
      Graph : Graph_T)
   return Node_T;
   --
   -- Given a step, return the node (basic block) that contains the step.
   -- If this function is called before basic-blocks have been
   -- collected, Constraint_Error results.


   function Node_At (
      Index  : Node_Index_T;
      Within : Graph_T)
   return Node_T;
   --
   -- The node identified by the given index, within the
   -- given graph.
   -- If the index is out of range, Constraint_Error is raised.


   function Entry_Node (Graph : in Graph_T) return Node_T;
   --
   -- The entry node (entry point) of the graph.
   -- This is necessarily the basic block that contains the
   -- graph's entry step (as its first step).


   type Node_List_T is array (Positive range <>) of Node_T;
   --
   -- A set (or list) of nodes, for example, all successors
   -- of a given node.


   function All_Nodes (Within : Graph_T) return Node_List_T;
   --
   -- All the nodes in the graph, listed so that the (Positive) index
   -- of a node in the list is the same as the (Node_Index_T) index of
   -- the node.


   function Nodes_Containing (
      Steps : Step_List_T;
      Graph : Graph_T)
   return Node_List_T;
   --
   -- For each step in Steps, the node that contains the Step.
   -- The result may contain duplicate nodes, if some of the Steps
   -- belong to the same node.


   function Return_Nodes (Within : Graph_T)
   return Node_List_T;
   --
   -- All the return nodes.
   -- A return node is a node that has no successors.


   --
   ---   Node sets
   --


   type Node_Set_T is array (Node_Index_T range <>) of Boolean;
   --
   -- A set of flow-graph nodes, represented as a bit-map.

   pragma Pack(Node_Set_T);

   type Node_Set_Ref is access Node_Set_T;


   function Predefined_Equal (Left, Right : Node_Set_T) return Boolean
   renames "=";
   --
   -- This is the predefined equality operation. See "=" below.


   function "=" (Left, Right : Node_Set_T) return Boolean;
   --
   -- The predefined "=" has bugs for packed Boolean arrays in
   -- (some versions of?) Gnat 3.15p. It can return False although
   -- the arrays have identical components. Hypothesis: if the array
   -- does not entirely fill the last word, some garbage bits in the
   -- last word are compared, too.


   function No_Nodes (Within : Graph_T) return Node_Set_T;
   --
   -- The empty set of nodes, however with the ability to hold
   -- any node in the given Graph.


   function No_Nodes (Within : Graph_T) return Node_Set_Ref;
   --
   -- The empty set of nodes, however with the ability to hold
   -- any node in the given Graph.


   function "<=" (Left, Right : Node_Set_T) return Boolean;
   --
   -- Whether Left is a subset of Right.


   function Cardinality (Set : Node_Set_T) return Node_Count_T;
   --
   -- The number of nodes in the set.


   function To_List (Set : Node_Set_T; From : Graph_T)
   return Node_List_T;
   --
   -- Lists all the nodes in the set (in index order).


   function To_Index_List (Set : Node_Set_T)
   return Node_Index_List_T;
   --
   -- Lists the indices of all the nodes in the set (in index order).


   function To_Set (
      Nodes : Node_Index_List_T;
      Last  : Node_Index_T)
   return Node_Set_T;
   --
   -- The set containing the given indices, as a subset of
   -- the index range 1 .. Last.
   -- A Constraint_Error will result if any given index
   -- exceeds Last.


   --
   ---   Node edges (edges between nodes)
   --


   type Edge_T is private;
   --
   -- An edge between nodes in a flow-graph.
   -- The attributes of an edge include:
   --
   -- > Its index number (1..)
   -- > The source node from which the edge starts.
   -- > The target node at which the edge ends.
   -- > The corresponding step-edge.
   --
   -- The following attributes are inherited from the corresponding
   -- step edge, and are always accessed from that object:
   --
   -- > The logical precondition for taking the edge, a boolean
   --   expression evaluated after the execution of the source node.
   -- > The execution time of the edge.
   --
   -- For repeated uses of volatile cells in the logical precondition,
   -- the same remarks apply as for the precondition of the underlying
   -- step edge.


   Null_Edge : constant Edge_T;
   --
   -- Indicates the absence of an edge.


   function Index (Edge : in Edge_T) return Edge_Index_T;
   --
   -- The index number of an edge.
   -- These numbers run from 1 to Max_Edge_Index.


   function Source (Edge : in Edge_T) return Node_T;
   function Target (Edge : in Edge_T) return Node_T;
   --
   -- The source and target nodes of the edge.


   function Step_Edge (Edge : in Edge_T) return Step_Edge_T;
   --
   -- The step-edge that corresponds to the given node-edge.
   -- This is the step-edge that goes from the last step in
   -- the source node to the first step in the target node.


   function Node_Edge (Edge : in Step_Edge_T; Graph : Graph_T) return Edge_T;
   --
   -- The node-edge that corresponds to the given step-edge.
   -- This is the node-edge that goes from the source node
   -- to the target node.
   --
   -- Null_Edge is returned if the step-edge is within a node.


   function Condition (Edge : in Edge_T) return Arithmetic.Condition_T;
   --
   -- The boolean condition that is a necessary but not perhaps
   -- sufficient condition for the edge being executed.
   -- The value Arithmetic.Always indicates an unconditional
   -- edge.
   -- The value Arithmetic.Never indicates an infeasible (never
   -- taken) edge.
   --
   -- An edge between nodes does not really have a precondition
   -- attribute, because the precondition of the corresponding
   -- step-edge is used. As for step-edges, the precondition is
   -- evaluated after the execution of the source node. When
   -- volatile cells are used in the precondition, the rules
   -- stated for the preconditions of Step_Edge_T apply.


   function Time (Along : Edge_T) return Processor.Time_T;
   --
   -- The execution time of the edge. This is just the Time of
   -- the underlying Step_Edge.


   type Edge_List_T is array (Positive range <>) of Edge_T;
   --
   -- A set (or list) of edges, for example, all edges that
   -- leave a given step.


   function No_Edges return Edge_List_T;
   --
   -- An empty edge list.


   function Sources (Edges : in Edge_List_T) return Node_List_T;
   function Targets (Edges : in Edge_List_T) return Node_List_T;
   --
   -- The source and target nodes of the given edges.


   function Step_Edges (Edges : in Edge_List_T) return Step_Edge_List_T;
   --
   -- The step-edges that corresponds to the given node-edges.
   -- See the function Step_Edge, above.


   function Edge_At (
      Index  : Edge_Index_T;
      Within : Graph_T) return Edge_T;
   --
   -- The edge identified by the given index, within the
   -- given graph.
   -- If the index is out of range, Constraint_Error is raised.


   function Edges_Into (Node : Node_T; Within : Graph_T)
   return Edge_List_T;
   --
   -- All the edges that enter the node (Edge.Target = Node).


   function Edges_From (Node : Node_T; Within : Graph_T)
   return Edge_List_T;
   --
   -- All the edges that leave the node (Edge.Source = Node).


   function Number_Into (Node : Node_T; Within : Graph_T) return Natural;
   --
   -- The number of edges that enter the node.
   -- Same as Edges_Into()'Length.


   function Number_From (Node : Node_T; Within : Graph_T) return Natural;
   --
   -- The number of edges that leave the node.
   -- Same as Edges_From()'Length.


   function Predecessors (Node : Node_T; Within : Graph_T)
   return Node_List_T;
   --
   -- The immediate predecessors of the given node.
   -- Same as Sources(Edges_Into(Node,Within)).
   -- The given node itself may be included, if there is a
   -- looping edge from this node to itself.


   function Only_Predecessor (Node : Node_T; Within : Graph_T)
   return Flow.Node_T;
   --
   -- The only immediate predecessor of the given Node, Within
   -- the given graph. If the Node has no predecessors or more
   -- than one predecessor, a Fault is emitted and the first
   -- predecessor is returned (Constraint_Error if none).


   function Successors (Node : Node_T; Within : Graph_T)
   return Node_List_T;
   --
   -- The immediate successors of the given node.
   -- Same as Targets(Edges_From(Node,Within)).
   -- The given node itself may be included, if there is a
   -- looping edge from this node to itself.


   function Only_Successor (Node : Node_T; Within : Graph_T)
   return Flow.Node_T;
   --
   -- The only immediate successor of the given Node, Within
   -- the given graph. If the Node has no successors or more
   -- than one successor, a Fault is emitted and the first
   -- successor is returned (Constraint_Error if none).


   function Return_Edges (Within : Graph_T)
   return Edge_List_T;
   --
   -- All the edges that enter return nodes.
   -- A return node is a node that has no successors.
   -- If the graph has only one node (the entry node, which is
   -- also a return node in this case) the result is an
   -- empty edge list.
   --
   -- This may seem a peculiar function (why not produce the
   -- return nodes instead of the edges?) but it is adapted to
   -- the needs of the HRT Execution Skeleton generation.


   function Edges (
      From     : Node_Set_T;
      Into     : Node_Set_T;
      Not_From : Boolean := False;
      Not_Into : Boolean := False;
      Within   : Graph_T)
   return Edge_List_T;
   --
   -- The edges that go From a given node set Into another given
   -- node-set (which may be the same set). In other words, the
   -- edges Within the graph where the Source node is in From
   -- and the Target node is in Into.
   --
   -- The Boolean parameters Not_From and Not_Into can be used
   -- to complement the From set membership and/or the Into set
   -- membership. For example, if Not_From => True and
   -- Not_Into => False, the function gives all the edges where
   -- the source node is not in From and the target edge is
   -- in Into.


   --
   ---   Loose edges between steps
   --


   type Loose_Edge_T is record
      Source : Step_T;
      Cond   : Arithmetic.Condition_T;
      Time   : Processor.Time_T;
      Target : Step_Tag_T;
      Info   : Processor.Loose_Edge_Info_T;
   end record;
   --
   -- A new edge (state transition) discovered when incrementally
   -- exploring the control-flow of a subprogram.
   --
   -- The Source step already exists within the flow-graph.
   -- The Target state (step tag) may or may not already
   -- exist in the graph.
   --
   -- In addition to the standard edge attributes, processor-specific
   -- Info can be added.
   --
   -- The precondition Cond represents the precondition of the
   -- edge that will leave the given Source step for the step that
   -- represents the Target state. For volatile cells used in Cond,
   -- the same rules apply as for the preconditions of Step_Edge_T
   -- objects.


   function Loose_Edges (Item : Graph_T) return Boolean;
   --
   -- Whether the graph has some loose edges.


   function First_Loose_Edge (From : Graph_T)
   return Loose_Edge_T;
   --
   -- Returns that loose edge E which has the smallest step-address
   -- as defined by Processor."<", and returns it in Giving.
   -- This loose edge remains in the graph's set of loose edges.
   -- Raises Constraint_Error if the graph has no loose edges.


   --
   ---   Boundable (dynamically computed) edges between steps
   --


   type Boundable_Edge_T is abstract new Storage.Bounds.Boundable_T
   with private;
   --
   -- A flow-state transition in which the target flow-state is computed
   -- dynamically during program execution. The transition is represented
   -- as a kind of "boundable" target object that computes the target
   -- flow-state(s) dynamically, using bounds on cell values on entry to
   -- the source step (values that flow into the source step, before
   -- they are updated by the possible arithmetic effect of the source
   -- step).
   --
   -- The attributes of a boundable edge include:
   --
   -- > The source step in the flow-graph.
   --
   -- > Whether the edge represents a jump to another step in the same
   --   flow-graph, or a call to another subprogram (another flow-graph).
   --   A "jump" edge can also represent a return from the present
   --   subprogram (in which case no actual flow-graph edge results).
   --
   -- > The state of resolution of the boundable edge.
   --
   -- > The cells on which the dynamic target computation depends,
   --   inherited from Storage.Bounds.Boundable_T.
   --
   -- > The number of actual edges that have been created from this
   --   boundable edge so far, and whether the edge has been resolved
   --   into a return from the present subprogram.
   --
   -- > Information about the "domain" of the boundable edge, as it
   --   was when the boundable edge was last analysed to resolve it.
   --   The domain is the set of flow-graph steps from which flow can
   --   reach the boundable edge. Thus, the cell values that reach
   --   the boundable edge originate from these steps, or from the
   --   caller of the subprogram. We keep track of the domain so
   --   that we can avoid re-analysing the edge if its domain has
   --   not grown since the last analysis. The domain grows if the
   --   resolution of (this or other) boundable edges adds new paths
   --   to the steps in the domain. Only a full analysis (that is,
   --   all the way to arithmetic analysis if necessary) of the edge
   --   counts as the "last analysis" of the edge. A round of analysis
   --   that ends earlier, for example after constant propagation
   --   resolves some other boundable edge, does not count as the
   --   last analysis of an edge that is not yet resolved at this point.
   --
   --   Of course, if the arithmetic effects of the steps in the domain
   --   are changed it can be useful to re-analyse the boundable edge
   --   even if no new steps have been added to the domain.
   --
   -- > Specific attributes (in derived types) to control the resolution
   --   of the dynamic target and to detect when the result is stable.
   --
   -- The operation that applies bounds to a boundable edge can extend
   -- the flow-graph by adding actual edges (typically only edges that
   -- have the same source step as the boundable edge) or steps (typically
   -- only steps that are targets of the added edges). When a boundable
   -- edge has been fully resolved in this way, the boundable edge
   -- itself is deleted from the graph.
   --
   -- If a boundable edge is resolved into a return it cannot also be
   -- resolved into actual edges and vice versa. These are mutually
   -- exclusive resolutions of the boundable edge.
   --
   -- The decision to remove the boundable edges from a graph depends
   -- on the state of resolution of _all_ boundable edges in the graph.
   -- This is the reason for recording the state in the boundable
   -- edge object.
   --
   -- A boundable edge can also be resolved by assertions that list all
   -- the possible targets of the edge. The edge is then removed without
   -- waiting for the other boundable edges to be resolved.


   type Boundable_Edge_Role_T is (
      Undefined,
      Boundable_Jump,
      Boundable_Call);
   --
   -- The role of a boundable edge.
   --
   -- Undefined
   --    The role is not yet known. The role is set when the
   --    edge is added to the flow-graph.
   -- Boundable_Jump
   --    The edge represents a jump to another step, or to some
   --    other steps, in the same flow-graph. The edge will be
   --    resolved to normal edges between normal steps in the same
   --    flow-graph, or into a return from this flow-graph.
   -- Boundable_Call
   --    The edge represents a call to another subprogram, or to
   --    some other subprograms. The edge will be resolved to
   --    normal edges from the source step to special "call steps"
   --    that represent the execution of the callee.


   type Edge_Resolution_T is (
      Growing,
      Unresolved,
      Stable);
   --
   -- The state of resolution of a boundable edge.
   --
   -- Describes the result of trying to bound (resolve) a boundable edge
   -- by applying some cell-bounds to it.
   --
   -- Unresolved
   --    One of the following:
   --    a) the boundable edge has just been added to the flow-graph
   --       and has not been analysed to resolve it, or
   --    b) the domain of the edge has grown since this edge was last
   --       analysed and so the edge must be re-analysed, or
   --    c) one or more analysis methods have been applied to the
   --       boundable edge (since the edge was created, or the
   --       flow-graph was last extended), but all failed to make
   --       the edge Growing or Stable.
   --
   -- Growing
   --    Some analysis applied to this boundable edge has resolved some
   --    target addresses and added corresponding edges to the flow-graph,
   --    usually as loose edges. Some new steps may also have been added.
   --    This implies two things: firstly, the flow-graph decoding and
   --    tracing should be resumed and continued until all (new) loose
   --    edges have been processed; secondly, all boundable edges for
   --    which the domain of the edge has grown should be re-analysed to
   --    find more possible target addresses or to find that the set of
   --    targets is stable.
   --
   -- Stable
   --    One of the following:
   --    (a) Some (re-) analysis applied to this boundable edge has
   --        found that the set of possible targets is the same as
   --        in the last analysis (even if the domain of the edge
   --        has grown).
   --    (b) The last analysis of this edge placed it in the Growing
   --        state, and the domain of this edge has not changed since
   --        that analysis. A new analysis would thus not give any
   --        new possible targets.
   --    Thus, the analysis of this edge seems to be complete (stable)
   --    and needs to be repeated only if its domain grows again.
   --
   -- The meaning of the literals is further explained below in connection
   -- with the procedure Apply.
   --
   -- The order of the literals is significant; the 'Min function is
   -- applied in some places (in child packages).
   --
   -- The design of the boundable edges aims to the following overall
   -- iterative algorithm for building the flow-graphs of a subprogram
   -- when the entry address is known:
   --
   --    Create
   --       the entry step,
   --       and perhaps other steps,
   --       and any edges between and from these steps,
   --       whether loose, real or boundable;
   --
   --    Flow_Bounding : loop
   --       -- At this point the flow-graph may contain some loose
   --       -- edges and perhaps some boundable edges, too.
   --
   --       Flow_Tracing:
   --       while there are some loose edges loop
   --          Decode the targets of the loose edges
   --             perhaps creating new steps
   --             and new loose, real or boundable edges;
   --       end loop Flow_Tracing;
   --
   --       -- At this point the flow-graph has (or may have been)
   --       -- extended since the boundable edges were last analysed.
   --       -- If this extension has extended the domain of some
   --       -- boundable edge the edge should be re-analysed and
   --       -- so we mark such edges Unresolved again.
   --       -- Boundable edges that have an unchanged domain (since
   --       -- their last analysis) do not need re-analysed and their
   --       -- state is not changed.
   --
   --       Find Unstable Dynamic Edges (this operation marks as Unresolved
   --          and "unstable" those boundable edges for which the domain has
   --          grown, and marks as Stable those Growing edges that have the
   --          same domain as in their last analysis);
   --
   --       exit Flow_Bounding when all boundable edges are Stable;
   --
   --       Analysis:
   --       for each analysis method (constant propagation,
   --          value-origin analysis, arithmetic analysis)
   --       loop
   --
   --          for each unstable (and feasible) boundable edge
   --          loop
   --             if this analysis method is applicable to the edge
   --             then
   --
   --                if this is the most powerful analysis we have
   --                (that is, arithmetic analysis) then
   --                   Mark_Domain for the edge (this operation marks
   --                      the edge as having been last analysed in its
   --                      current domain);
   --                end if;
   --
   --                Analyse the edge and
   --                   if the analysis fails then
   --                      Add nothing to the flow-graph;
   --                      Leave the edge Unresolved;
   --                   elsif the analysis seems stable then
   --                      Add nothing to the flow-graph;
   --                      Mark the edge Stable;
   --                   else
   --                      Add new targets to the flow-graph;
   --                      Mark the edge Growing;
   --                   end if;
   --
   --             end if;
   --          end loop;
   --
   --          Optionally exit from the Analysis loop if some boundable
   --             edge was marked Growing. This can avoid applying
   --             an expensive analysis (eg. arithmetic analysis)
   --             to a flow-graph that could be resolved with
   --             cheaper analyses.
   --
   --       end loop Analysis;
   --
   --       -- At this point:
   --       --
   --       -- > If some boundable edge is marked Growing then the
   --       --   flow-graph was extended, compared to the last
   --       --   iteration of Flow_Bounding, so we should repeat
   --       --   loop Flow_Bounding to trace the new loose edges.
   --       --   If this increases the domain of some boundable edge
   --       --   then we should re-analyse that boundable edge.
   --       --   Furthermore, if some boundable edge is marked
   --       --   Unresolved then either this edge is a new one
   --       --   (added as a result of Growing some other boundable
   --       --   edge) or one of the two cases (A) or (B) below applies.
   --       --
   --       -- > Otherwise (no Growing edges) the flow-graph is the
   --       --   same as at the Find_Unstable_Dynamic_Edges, above. Thus,
   --       --   a re-analysis of the edges is futile (it would give the
   --       --   same result). Furthermore, if some boundable edge is
   --       --   marked Unresolved (ie. not Stable) then either
   --       --
   --       --   A) no (enabled) analysis was applicable and the
   --       --      edge remains unresolved, or
   --       --
   --       --   B) all (enabled) analyses of this edge failed to
   --       --      resolve it (into a Stable state).
   --       --
   --       --   The Unresolved edge may have been partially resolved,
   --       --   that is, some possible targets may have been found
   --       --   in earlier iterations of Flow_Bounding.
   --
   --       exit Flow_Bounding when no boundable edge is Growing;
   --
   --    end loop Flow_Bounding;
   --
   --    At this point all feasible boundable edges are either Stable
   --    or Unresolved (unless Flow_Bounding was aborted by a limit
   --    on the number of iterations).
   --
   --    Report error (unresolved dynamic flow) for the boundable
   --    edges that are Unresolved;
   --
   --    Close and remove all boundable edges, in whatever state.


   function Source (Edge : Boundable_Edge_T'Class) return Step_T;
   --
   -- The source step of the boundable edge.
   -- All static edges resolved from this boundable edge will have
   -- this step as the source.


   function Role (Edge : Boundable_Edge_T'Class) return Boundable_Edge_Role_T;
   --
   -- The role of the boundable edge.


   function State (Edge : Boundable_Edge_T'Class)
   return Edge_Resolution_T;
   --
   -- The current state of resolution of the boundable edge. When
   -- a boundable edge is created, the state is initially Unresolved.
   -- As bounds are applied to the edge, the state changes.


   function Full_Image (Item : Boundable_Edge_T'Class)
   return String;
   --
   -- All we know of the boundable edge.


   procedure Add_Resolvent (To : in out Boundable_Edge_T'Class);
   --
   -- Adds one To the count one resolved edges and marks the state
   -- as Growing. Notes that the edge is no longer "unstable".
   --
   -- A fault is reported if the edge has already been resolved
   -- into a return.


   procedure Resolve_To_Return (
      From : in     Graph_T;
      Edge : in out Boundable_Edge_T'Class);
   --
   -- Declares that the Edge has been resolved into a return from
   -- the subprogram (the Graph) that contains the source step, and
   -- marks the state as Stable. Notes that the Edge is no longer
   -- "unstable".
   --
   -- If this is the first call of this operation on this Edge, the
   -- operation declares the source step of the Edge as a return
   -- step by means of Flow.Calls.Return_After.
   --
   -- A fault is reported if the Edge has already been resolved
   -- into some actual edges.


   function Resolvents  (Edge : Boundable_Edge_T'Class)
   return Natural;
   --
   -- The number of actual edges that have been resolved from the
   -- given boundable Edge, so far. Initially zero.


   function Returns (Edge : Boundable_Edge_T'Class)
   return Boolean;
   --
   -- Whether the given boundable Edge has been resolved to a return
   -- from the subprogram that contains the source step.


   procedure Find_Unstable_Dynamic_Edges (Within : in Graph_T);
   --
   -- Evaluates the domains of all dynamic edges Within the graph
   -- with respect to the growth of the graph, since the last round
   -- of analysis and resolution of dynamic edges (that is, since the
   -- last call of this procedure). In particular, detects whether
   -- the new part of the graph "rejoins" the old part of the graph
   -- in a way that brings new (data) flow to the dynamic edges that
   -- were analysed in the last round.
   --
   -- Sets the resolution state of all boundable edges Within the
   -- given flow-graph as follows:
   --
   -- > If the domain of the edge, Within the current graph, has
   --   grown since the edge was last analysed, the edge is
   --   marked Unresolved (and the edge will be reanalysed unless
   --   it is classed as infeasible).
   --
   -- > If the domain of a Growing edge has not grown since the
   --   edge was last analysed the edge is marked Stable (and
   --   the edge will not be reanalysed in the next round).
   --
   -- > Marks as "unstable" all edges for which the domain has
   --   grown since their last analysis, and all other edges
   --   as "not unstable" (even if the state is Unresolved).
   --
   -- Also begins collecting information about edges added to the
   -- graph from this point on; this information is used in the next
   -- invocation to check if the domains have grown. This operation
   -- shall be called when the tracing of the graph is complete except
   -- for the remaining boundable edges, immediately before the analysis
   -- to resolve these edges and possible further tracing of the graph.


   procedure Mark_Domain (Edge : in out Boundable_Edge_T'Class);
   --
   -- Notes that the Edge was last fully analysed (or is about to
   -- be analysed) in its current domain. Thus, the domain of the
   -- Edge is now considered "not grown since the last analysis".
   --
   -- This operation *shall* be executed immediately before, during
   -- or after the full analysis and resolution of the Edge, and
   -- only then. By "full analysis" we mean the most powerful form
   -- of analysis in our repertoire (arithmetic analysis).


   procedure Mark_Stable (Edge : in out Boundable_Edge_T'Class);
   --
   -- Sets the resolution state of the Edge to Stable, and notes
   -- that the edge is no longer "unstable".


   -- not overriding
   procedure Apply (
      Bounds : in     Storage.Bounds.Bounds_T'Class;
      Upon   : in out Boundable_Edge_T;
      Graph  : in     Graph_T)
   is abstract;
   --
   -- Applies the Bounds Upon a boundable edge, hopefully resolving the
   -- dynamic target computation into actual (static) edges for the Graph.
   -- To add such edges to the Graph the Apply operation should use the
   -- dedicated operation Add_Resolved_Edge, declared later in this
   -- package (in two variants).
   --
   -- The Bounds constrain the values of the relevant cells on entry to
   -- the step Source (Upon), before these cells are updated by the
   -- possible arithmetic effect of this step. If the target address(es)
   -- in fact depend on the updated values, the operation must itself
   -- take the updates into account in some way.
   --
   -- This operation probably changes the state of resolution of the
   -- boundable edge, either indirectly by Add_Resolved_Edge or directly
   -- by the operation Mark_Stable. When Apply is called the initial
   -- state of the edge is always Unresolved.
   --
   -- The resulting state of the boundable edge describes the success or
   -- failure of this application. The meaning of the three possible
   -- states is as follows:
   --
   -- Growing
   --    The Bounds were strong enough to let us deduce one or more real
   --    target addresses and add the corresponding (static) edges to
   --    the Graph (usually as loose edges). It is also possible that
   --    one or more steps were added to the Graph, although new target
   --    steps are usually added in the Decoder as a result of following
   --    the new loose edges. These new edges may not have exhausted the
   --    possible targets of the boundable edge. It may be that the new
   --    parts of the Graph lead back to this boundable edge with more
   --    data states that generate more possible target addresses.
   -- Unresolved
   --    The Bounds were not strong enough to allow any resolution of
   --    the boundable edge; no real edges or other elements were added
   --    to the Graph. If sharper bounds cannot be found, the boundable
   --    edge cannot be resolved (failure).
   -- Stable
   --    The Bounds were strong enough to be used, but did not allow
   --    any new real target addresses for the boundable edge, so it
   --    seems that the edge has been fully resolved (stable state).
   --
   -- If the state is Growing, the tracing of the flow in Graph should
   -- be resumed (continued) by following all new loose edges until there
   -- are no more loose edges. Then, new Bounds should be computed using
   -- the extended Graph and should again be Applied Upon this boundable
   -- edge, to check if new real target addresses are possible in the
   -- extended Graph.
   --
   -- If the state is Stable, and all other boundable edges in the
   -- Graph are also Stable (or Unresolved even with the sharpest bounds
   -- we can apply), then it seems that the Graph is complete (or as
   -- complete as we can make it) and all boundable edges can be removed
   -- (but this is not done here, of course, since the state of all
   -- boundable edges must be known).
   --
   -- To decide when the edge should be marked Stable, and to avoid
   -- adding duplicate edges to the Graph, the boundable edge object must
   -- probably keep a record of the real edges that is has given rise to,
   -- or of the corresponding edge preconditions or cell values.
   --
   -- If the state is Unresolved, and sharper Bounds cannot be found,
   -- and all other boundable edges in the Graph are Stable or Unresolved,
   -- the Graph is as complete as we can make it and all boundable edges
   -- can be removed as above.
   --
   -- The conceptual algorithm for this operation should thus be:
   --
   --    if the Bounds are strong enough then
   --       if the Bounds allow new edges to be derived from Upon then
   --          call Add_Resolved_Edge to create the new edges
   --       else
   --          call Mark_Stable
   --       end if
   --    end if
   --
   -- The hardest decision in this operation is probably whether the
   -- Bounds are strong enough to give confidence in the real edges
   -- deduced from the Bounds. For example, if the boundable edge stands
   -- for a switch/case statement and the Bounds allow 500 branches,
   -- is this likely to be correct or are the Bounds overestimated?
   -- If we accept too wide Bounds, we may force the analysis to explore
   -- hundreds of false paths. If we reject Bounds mistakenly, the
   -- analysis may miss important paths.
   --
   -- Note that even if Apply marks the boundable edge as Stable, the
   -- edge can still be subjected to more Apply operations with new
   -- Bounds if some _other_ boundable edge in this Graph is resolved
   -- and thus extends the Graph. The new Bounds may let the new call
   -- of Apply further resolve the boundable edge that appeared Stable
   -- with the earlier Bounds, based on the incomplete Graph.
   --
   -- This operation does _not_ override Storage.Bounds.Apply, because
   -- of the different parameter profile.
   --
   -- Note that you shall perform Mark_Domain Upon the edge, usually
   -- immediately before Apply.


   -- not overriding
   procedure Apply (
      Transfer : in     Arithmetic.Transfer_Function_T'Class;
      Upon     : in out Boundable_Edge_T;
      Graph    : in     Graph_T);
   --
   -- Applies the Transfer function Upon a boundable edge, hopefully
   -- resolving the dynamic target computation into actual (static)
   -- edges for the Graph or (and more likely) into a return from the
   -- Graph, asssuming that the Transfer function models the computation
   -- from the entry point of the Graph up to but not including the
   -- Source (Upon). Note that the effect of Source (Upon) itself is not
   -- included in Transfer.
   --
   -- To add such edges to the Graph the Apply operation should use the
   -- dedicated operation Add_Resolved_Edge, declared later in this
   -- package (in two variants). If the edge is resolved into a return,
   -- the Apply operation should use operation Resolve_To_Return.
   --
   -- The Transfer constrains the values of the relevant cells on entry
   -- to the step Source (Upon), before these cells are updated by the
   -- possible arithmetic effect of this step.
   --
   -- This operation probably changes the state of resolution of the
   -- boundable edge, either indirectly by Add_Resolved_Edge or
   -- Resolve_To_Return or directly by the operation Mark_Stable.
   -- When Apply is called the initial state of the edge is always
   -- Unresolved.
   --
   -- The resulting state of the boundable edge describes the success or
   -- failure of this application as for Apply (Bounds), above.
   --
   -- This operation does _not_ override Storage.Bounds.Apply, because
   -- of the different parameter profile.
   --
   -- The default implementation does nothing (no resolution).
   -- This is a temporary solution for upwards compatibility; this
   -- operation will be made abstract later.
   --
   -- Note that you shall perform Mark_Domain Upon the edge, usually
   -- immediately before Apply.


   type Target_List_T is
      array (Positive range <>) of Processor.Code_Address_T;
   --
   -- A list of asserted target for a boundable edge.


   -- not overriding
   procedure Take_Asserted_Targets (
      Edge    : in out Boundable_Edge_T;
      Targets : in     Target_List_T;
      Graph   : in     Graph_T);
   --
   -- Resolves the Edge to have the asserted list of Targets.
   -- The Edge will then be Closed and removed from the Graph,
   -- but this should not be done in this operation (see
   -- Remove_Dynamic_Edge, below).
   --
   -- The default implementation calls the primitive operation
   -- Take_Asserted_Target (see below) for each of the Targets,
   -- redispatching on the Edge.


   -- not overriding
   procedure Take_Asserted_Target (
      Edge   : in out Boundable_Edge_T;
      Target : in     Processor.Code_Address_T;
      Graph  : in     Graph_T);
   --
   -- Resolves the Edge to have the asserted Target as one possible
   -- target address. However, there may be more asserted targets.
   --
   -- The default implementation reports a "not implemented" Fault.
   -- This is a temporary solution for upwards compatibility; this
   -- operation will be made abstract later.


   -- not overriding
   procedure Close (
      Edge  : in out Boundable_Edge_T;
      Graph : in     Graph_T);
   --
   -- Closes the Edge when all boundable edges in the Graph are stable
   -- or cannot be resolved further. This operation is called once, just
   -- before the Edge is removed from the Graph. This operation shall not
   -- add any steps or edges to the Graph.
   --
   -- The default implementation reports the number of Resolvents as
   -- a Note. If flow-resolution is being traced (option), the same is
   -- also reported as a Trace line.


   type Dynamic_Edge_T is access all Boundable_Edge_T'Class;
   --
   -- A dynamic edge, as a part of a flow-graph, is a reference to
   -- a boundable edge object of some derived type. The derived types
   -- are processor-specific and represent the various kinds of
   -- dynamically computed jumps or other transfers of control in
   -- the target architecture.


   type Dynamic_Edge_List_T is array (Positive range <>) of Dynamic_Edge_T;
   --
   -- A list or set of dynamic edges.


   function Dynamic_Edges_From (
      Source : Step_T;
      Within : Graph_T)
   return Dynamic_Edge_List_T;
   --
   -- All the (unresolved) dynamic edges leaving the Source step.
   -- The list is non-empty iff Unresolved_Successors (Source) is True.


   function Dynamic_Edges (Item : Graph_T)
   return Dynamic_Edge_List_T;
   --
   -- All the dynamic edges in the given graph.


   function Sources (Edges : in Dynamic_Edge_List_T)
   return Step_List_T;
   --
   -- The source steps of the given dynamic edges.
   -- The result has the same 'Range as the input, so
   -- result(I) = Source (Edges(I)).


   procedure Add_Basis_Cells (
      From : in     Dynamic_Edge_List_T;
      To   : in out Storage.Cell_Set_T);
   --
   -- Adds the basis cells From each listed dynamic edge, To a
   -- given set of cells.


   --
   ---   Step_Data_T primitive operations
   --
   --
   -- Three groups of operations are defined:
   --
   -- > Low-level operations that work on small pieces, eg.
   --   an Effect, an Expression, but which must be combined to
   --   implement the computations that we need.
   --
   -- > High-level operations that do in one call exactly what
   --   various flow-graph construction operations need to do
   --   with Step_Data_T.
   --
   -- > Abstract operations.
   --
   -- The default implementations for some low-level operations are
   -- dummy, null operations; the rest delegate the operation to
   -- anothe low-level operation which makes sense only if the latter
   -- operation is overridden with a non-null operation. The dummy
   -- operations issue Fault messages if called.
   --
   -- The default implementations for the high-level operations use
   -- the low-level operations to compute the results. However, a
   -- particular, concrete type derived from Step_Data_T may be able
   -- to make use of the additional information available in the
   -- high-level operations to produce a better result than can be
   -- achieved only by combining the low-level operations.
   --
   -- The idea is that a derived type can override operations on
   -- either the low or the high level, but usually not on both
   -- levels, although it is certainly permissible to do so.
   --
   -- Among all the operations in this Flow package, only the default
   -- implementations of the high-level Step_Data_T operations ever
   -- call the low-level Step_Data_T operations. Thus, if a derived
   -- type overrides the high-level operations without using the
   -- low-level operations, the derived type need not override the
   -- default implementations of the low-level operations.
   --
   -- Any concrete derived type must of course implement the abstract
   -- operations. If the implementations use some dummy, null low-level
   -- operations then the latter must also be overridden.


   -- First, the low-level operations:


   function Refined_Effect (
      From :        Arithmetic.Effect_Ref;
      On   : access Step_Data_T)
   return Arithmetic.Effect_Ref;
   --
   -- Derives a refined, residual effect From the given effect when
   -- applied On a given data state. That is, this function partially
   -- evaluates the given effect On constrained input data.
   --
   -- If some (useful) partial evaluation is possible, a new, refined
   -- effect is returned. Otherwise, the original Effect is returned.
   --
   -- The given effect, From.all, shall not be changed. Any refinement
   -- in any part of the effect shall lead to a new (heap-allocated)
   -- Effect_T. However, the new Effect_T may reuse (refer to) those
   -- parts of From.all that were not refined.
   --
   -- If the function finds a contradiction between the data state
   -- and some range-pre assignments in the effect the function
   -- shall raise False_Path.
   --
   -- The default implementation emits a Fault message, returns the
   -- given effect, From, and does not propagate False_Path.


   function Refined_Effect (
      From :        Arithmetic.Effect_T;
      On   : access Step_Data_T)
   return Arithmetic.Effect_Ref;
   --
   -- Just like the Effect above (with From an Effect_Ref), but
   -- by taking an Effect_T instead this function can avoid to
   -- allocate and discard heap space for the non-refined effect.
   --
   -- The default implementation makes a heap copy of the given
   -- effect, From, and passes it to the Effect function above,
   -- with redispatching On the data state.


   function Refined_Condition (
      From :        Arithmetic.Condition_T;
      On   : access Step_Data_T)
   return Arithmetic.Condition_T;
   --
   -- Derives a refined, residual condition From the given condition
   -- when applied On a given data state. That is, this function
   -- partially evaluates the given condition On constrained input data.
   --
   -- If some (useful) partial evaluation is possible, a new, refined
   -- condition is returned. Otherwise, the original From condition
   -- shall be returned.
   --
   -- If the function finds that the From condition is always false
   -- On the given data, it shall return Arithmetic.Never.
   --
   -- The default implementation emits a Fault message and returns
   -- the given condition, From.


   function Transformed_Data (
      From  : access Step_Data_T;
      After :        Arithmetic.Effect_T)
   return Step_Data_Ref;
   --
   -- The new data state that results From a given data state, After
   -- an effect is applied to it. The original data state shall not
   -- be modified; however, if the effect causes no change in the data
   -- state, the function can return a new reference to the original
   -- data state (which is why the data state is of access mode).
   --
   -- The function can assume that the given After effect is the
   -- result of the Refined_Effect function on the From state, so it
   -- is already partially evaluated on that state. Moreover, the
   -- function can assume that After'Length > 0, as the data-state
   -- cannot be changed by a null effect.
   --
   -- If the function finds a contradiction between the data state and
   -- the effect, for example a range-pre assignment that is false in
   -- this data, the function shall raise False_Path. However, this
   -- contradiction should have been discovered earlier when the
   -- effect was refined From this data state.
   --
   -- Note that an effect can *reduce* the information about data
   -- state, thus the result may place looser bounds or even no
   -- bounds on the data state. For example, the From state may imply
   -- that the cell C has the value 5, but After may assign an Unknown
   -- value to cell C so that the resulting data state has no
   -- information on C.
   --
   -- The default implementation emits a Fault message, returns a
   -- reference to the given data state, From, and never propagates
   -- False_Path.


   function Constrained_Data (
      From : access Step_Data_T;
      By   :        Arithmetic.Condition_T)
   return Step_Data_Ref;
   --
   -- The constrained data state that result From a given data state
   -- By assuming that a given constraint condition holds. The original
   -- data state shall not be modified; however, if the constraint has
   -- no effect on the data state, the function can return a new reference
   -- to the original data state (which is why the data state is of
   -- access mode).
   --
   -- The function can assume that the given constraint condition
   -- is the result of the Refined_Condition function On this data state,
   -- so it is already partially evaluated on this data state. Moreover,
   -- the function can assume that the condition is non-trivial, that is
   -- neither of the constants Never or Always.
   --
   -- If the function finds that the constraint is always false for
   -- the given data state it shall raise False_Path.
   --
   -- The default implementation emits a Fault message, returns the
   -- given data state, From, and never propagates False_Path.


   -- Then the high-level operations:


   procedure Transform_New_Step (
      Data   : access Step_Data_T;
      Step   : in     Step_T;
      Effect : in out Arithmetic.Effect_Ref;
      Post   :    out Step_Data_Ref);
   --
   -- This operation is used when a new Step is added to the flow-graph
   -- with a Tag that defines some input Data state that flows into
   -- the new Step, and using the Add_Step operation that takes an
   -- Effect_Ref parameter.
   --
   -- Precondition: Data = Data (Step) /= Any_Data. The Data parameter
   -- is needed only as a controlling parameter for dispatching. It
   -- is unspecified whether or not an effect (or the Effect) has been
   -- assigned to the Step.
   --
   -- The procedure should refine the Effect, using the Data, and apply
   -- it to the input Data, giving the Post data state after the Step
   -- (which shall not be Any_Data). The refined Effect shall be returned.
   --
   -- If the procedure finds a contradiction between the Data and the
   -- Effect it shall raise False_Path.
   --
   -- The default implementation uses the low-level functions
   -- Refined_Effect (From : Effect_T) and Transformed_Data,
   -- redispatching on the Data. These low-level operations may
   -- propagate False_Path if their default implementations are
   -- overridden.


   procedure Transform_New_Step (
      Data    : access Step_Data_T;
      Step    : in     Step_T;
      Effect  : in     Arithmetic.Effect_T;
      Refined :    out Arithmetic.Effect_Ref;
      Post    :    out Step_Data_Ref);
   --
   -- This operation is used when a new Step is added to the flow-graph
   -- with a Tag that defines some input Data state that flows into
   -- the new Step, and using the Add_Step operation that takes an
   -- Effect_T parameter.
   --
   -- Precondition: Data = Data (Step) /= Any_Data. The Data parameter
   -- is needed only as a controlling parameter for dispatching. Also,
   -- no effect is yet assigned to the Step.
   --
   -- The procedure should refine the Effect, using the Data, return
   -- the possibly Refined effect, and  apply the Refined effect to
   -- the input Data, giving the Post data state after the Step (which
   -- shall not be Any_Data).
   --
   -- If the procedure finds a contradiction between the Data and the
   -- Effect it shall raise False_Path.
   --
   -- The somewhat baroque profile at Effect and Refined is meant to
   -- avoid making a heap copy of the unrefined Effect_T.
   --
   -- The default implementation uses the low-level functions
   -- Transformed_Effect (From : Effect_Ref) and Transformed_Data,
   -- redispatching on the Data. These low-level operations may
   -- propagate False_Path if their default implementations are
   -- overridden.


   procedure Refine_New_Edge (
      Post   : access Step_Data_T;
      Source : in     Step_T;
      Target : in     Step_Tag_T;
      Cond   : in out Arithmetic.Condition_T;
      Giving :    out Step_Data_Ref);
   --
   -- This operation is used when a new step Edge is added to the
   -- flow-graph with a Source step and Target tag that indicate a
   -- refinement of the edge Condition and Target data state is
   -- possible and needed.
   --
   -- Preconditions:
   --    Data (Source) /= Any_Data.
   --    Target.Data   /= Any_Data.
   --    Post = Source.Post /= Any_Data.
   --    Cond /= Always.
   --
   -- The Post parameter is the data state on leaving the Source
   -- step (including the assignments in the effect of the Source).
   -- It is equal to Source.Post, so redundant, but is used as a
   -- controlling operand for dispatching.
   --
   -- The precondition Cond /= Always comes from the fact that if
   -- Cond = Always then it cannot be refined by the Post data
   -- and it cannot constrain the Post data for Giving, so we
   -- can only use Cond as such and assign Giving := Post, which
   -- is handled in the Add_Edge operation itself.
   --
   -- The procedure shall apply the Post data to refine the Cond
   -- to be assigned to the new edge, and then apply the refined Cond
   -- to the Post state, Giving the constrained data state for the
   -- actual (refined) target tag of the new edge.
   --
   -- The refined edge condition shall be returned in Cond. If the
   -- procedure finds that the condition is always False in the
   -- Post data state, it shall return Cond as Arithmetic.Never.
   --
   -- The default implementation uses the low-level functions
   -- Refined_Condition and Transformed_Data, redispatching on the
   -- data state (Post). If the Refined_Condition function returns
   -- Arithmetic.Never, the Transformed_Data function is not called
   -- and the given Post data state is returned in Giving.


   -- Finally the abstract operations:


   function Image (Item : Step_Data_T) return String
   is abstract;
   --
   -- A description of the data state for human understanding.
   -- The presentation depends on the derived type.


   function Image_All (Item : Step_Data_Ref) return String;
   --
   -- Image (Item.all), or "not constrained" if Item is null.


   procedure Apply (
      Pre   : access Step_Data_T;
      Post  : access Step_Data_T;
      Upon  : in out Boundable_Edge_T'Class;
      Graph : in     Graph_T)
   is abstract;
   --
   -- This operation is used when a new boundable edge is added to
   -- the flow-graph with a Source step that has a tag that defines
   -- some input Pre data state /= Any_Data. The Post parameter is the
   -- data state after transforming Pre by the effect of the Source
   -- step.

   -- The procedure shall apply the Pre and/or the Post states Upon
   -- the new boundable edge, hopefully resolving the dynamic target
   -- computation into actual (static) edges for the Graph.
   --
   -- We provide both the Pre and Post states for convenience. The
   -- Apply operation for Storage.Bounds.Bounds_T upon Boundable_Edge_T
   -- needs the bounds from the Pre state, but jump instructions often
   -- depend on the Post state.
   --
   -- Note that the Pre and Post parameters are both controlling and
   -- must therefore have the same tag (in the Ada sense).
   --
   -- This operation probably changes the state of resolution of the
   -- boundable edge, indirectly by Add_Resolved_Edge and/or directly
   -- by the operation Mark_Stable. When Apply is called the initial
   -- state of the edge is always Unresolved.
   --
   -- The resulting state of the boundable edge describes the success or
   -- failure of this application. The meaning of the three possible
   -- states is as follows:
   --
   -- Growing
   --    Should never occur and will cause a Fault report.
   -- Unresolved
   --    The Data bounds were not strong enough to allow any resolution of
   --    the boundable edge; no real edges or other elements were added
   --    to the Graph. If sharper bounds cannot be found, the boundable
   --    edge cannot be resolved (failure).
   -- Stable
   --    The Data were strong enough to resolve all the real target
   --    addresses for the boundable edge. Since the Data state is
   --    a superstate for any execution of this step, the edge is
   --    now fully resolved (stable state).
   --
   -- The ideal outcome of Apply is one or more calls of Add_Resolved_Edge
   -- followed by one call of Mark_Stable.


   procedure Apply (
      Data   : access Step_Data_T;
      Upon   : in     Calling.Protocol_T'Class;
      Giving :    out Calling.Protocol_Ref)
   is abstract;
   --
   -- Applies the Data state Upon a boundable calling protocol, perhaps
   -- Giving a more constrained / better defined protocol. The procedure
   -- shall return Giving as null if it cannot constrain the given
   -- protocol (Upon) by means of the Data bounds.


   --
   ---   Step and graph construction
   --


   procedure Add_Step (
      To     : in     Graph_T;
      Tag    : in     Step_Tag_T;
      Effect : in     Arithmetic.Effect_T;
      Info   : in     Processor.Step_Info_T;
      Giving :    out Step_T);
   --
   -- Adds a step with the given tag and other attributes
   -- to the graph. No edges to or from the step are yet added,
   -- except if there already are loose edges to this Tag,
   -- in which case these loose edges are converted to bound
   -- edges (and Edge_Duplicated may be raised, if more than
   -- one edge would be added between the same two nodes).
   --
   -- If Tag.Data is not Any_Data, the operation calls the primitive
   -- Step_Data_T operation Transform_New_Step to refine the given
   -- Effect by partial evaluation on Tag.Data and then assigns the
   -- refined effect to the new step. This may propagate False_Path
   -- if there is a contradiction between Tag.Data and range-pre
   -- assignments in the Effect.
   --
   -- The added step is also returned in Giving, besides being
   -- added to the graph.
   --
   -- If the graph was originally null (empty), the new
   -- step becomes its entry step.
   --
   -- If a step with this given address already existed in the
   -- graph, then Step_Tag_In_Use is raised.


   procedure Add_Step (
      To     : in     Graph_T;
      Tag    : in     Step_Tag_T;
      Effect : in     Arithmetic.Effect_Ref;
      Info   : in     Processor.Step_Info_T;
      Giving :    out Step_T);
   --
   -- Same as above, but with a reference to the effect, allowing
   -- sharing of effects across multiple copies of the same
   -- instruction (unless they are refined by Tag.Data).


   procedure Add_Edge (
      To      : in     Graph_T;
      Source  : in     Step_T;
      Cond    : in     Arithmetic.Condition_T := Arithmetic.Always;
      Time    : in     Processor.Time_T;
      Target  : in     Step_T;
      Giving  :    out Step_Edge_T);
   --
   -- Adds an edge from the Source step to the Target step in the graph.
   -- Both steps must already exist in the graph.
   --
   -- If the Cond parameter is given, it represents a necessary but
   -- perhaps not sufficient condition for the edge being executed.
   -- The Cond is evaluated after the effect of the Source step.
   --
   -- If the Cond refers to a volatile cell, every use of the cell in
   -- Cond is assumed to provide the same value; if the Source step
   -- assigns to the cell, the assigned value is provided (that is,
   -- the cell is not volatile between the effect of the Source step
   -- and the evalution of the Cond).
   --
   -- The default Cond value (Always) implies an unconditional edge.
   -- The Cond should not be Arithmetic.Never; a Fault is emitted
   -- in that case (but the edge is still created).
   --
   -- This operation uses the given Cond as such with no data-
   -- dependent refinment. It is the caller's responsibility to
   -- provide a good Cond, possibly depending on Tag (Source).Data
   -- and Tag (Target).Data.


   procedure Add_Edge (
      To      : in Graph_T;
      Source  : in Step_T;
      Cond    : in Arithmetic.Condition_T := Arithmetic.Always;
      Time    : in Processor.Time_T;
      Target  : in Step_T);
   --
   -- Exactly the same as Add_Edge above, but does not give a handle
   -- to the new edge -- quite often this handle is not needed --
   -- and therefore silently omits to create an edge if the
   -- Condition is Arithmetic.Never.


   procedure Add_Edge (
      To     : in Graph_T;
      Source : in Step_T;
      Cond   : in Arithmetic.Condition_T := Arithmetic.Always;
      Time   : in Processor.Time_T;
      Target : in Step_Tag_T;
      Info   : in Processor.Loose_Edge_Info_T :=
                     Processor.No_Loose_Edge_Info);
   --
   -- Adds an edge from the Source step to the Target state (tag)
   -- in the graph, possibly after transforming the data state of
   -- the Target tag.
   --
   -- The Source step must already exist in the graph.
   --
   -- If the Cond parameter is given, it represents a necessary but
   -- perhaps not sufficient condition for the edge being executed.
   -- The Cond is evaluated after the effect of the Source step.
   -- If the Cond uses volatile cells, the same rules apply as for
   -- the Cond parameter for an edge to an existing Target step.
   --
   -- The default Cond value (Always) implies an unconditional edge.
   --
   -- The Data components of the Source and Target define several
   -- cases as follows:
   --
   -- 1. Tag (Source).Data = Any_Data and Target.Data = Any_Data
   --
   --    This means normal decoding and flow-graph construction with
   --    no Data simulation. The Target and Cond are used as such
   --    with no refinement or transformation (because there is no
   --    data that could be used for refining or transforming).
   --
   -- 2. Tag (Source).Data = Any_Data and Target.Data /= Any_Data
   --
   --    This means the start of a data-dependent simulation. In
   --    this case, it is the caller's responsibility to include
   --    Effect (Source) in Target.Data and to give a useful Cond,
   --    perhaps refined by Target.Data. This operation uses the
   --    given Target and Cond with no refinement or transformation.
   --
   -- 3. Tag (Source).Data /= Any_Data and Target.Data = Any_Data.
   --
   --    This means that the data-dependent simulation stops and we
   --    return to normal decoding and flow construction. The Target
   --    and Cond are used as such with no refinement or transformation.
   --
   -- 4. Tag (Source).Data /= Any_Data and Target.Data /= Any.Data.
   --
   --    An edge of this kind means a transition in data-dependent
   --    flow-graph construction. There are two subcases:
   --
   -- 4a. Target.Data /= In_Transit.
   --
   --    An edge of this kind means a transition in data-dependent
   --    flow-graph construction where the caller has already computed
   --    the new data-state for the target. Accordingly, this operation
   --    uses the Target and Cond as such, with no refinement or
   --    transformation.
   --
   -- 4b. Target.Data = In_Transit
   --
   --    This means that the caller does not know about data simulation
   --    and has just used the Transit function to compute the Target.
   --    Accordingly, this operation will define the target data state
   --    and will refine the Cond by the data state; to do so, it calls
   --    the primitive Step_Data_T operation Refine_New_Edge.
   --
   -- If the (refined) Cond is Arithmetic.Never, an edge is not
   -- created. Otherwise:
   --
   -- > If a step with the target tag already exists in the graph,
   --   a real (bound) edge is added. In this case, the Info is not
   --   used (discarded).
   --
   -- > If a step with the new target tag does not already exist in
   --   the graph, a loose edge is added and records the Info.


   procedure Add_Edge (
      To     : in Graph_T;
      Source : in Step_T;
      Cond   : in Arithmetic.Condition_T := Arithmetic.Always;
      Time   : in Processor.Time_T;
      Target : in Processor.Flow_State_T;
      Info   : in Processor.Loose_Edge_Info_T :=
                     Processor.No_Loose_Edge_Info);
   --
   -- Equivalent to
   --   Add_Edge (.. Target => Transit (From => Source, To => Target)..).


   type Warning_Opt_T is (
      Never_Warn,
      Warn_If_Unresolved_By_Data,
      Always_Warn);
   --
   -- Defines when warnings are issued from Add_Dynamic_Edge, below.
   --
   -- Never_Warn
   --    A warning is never issued. A dynamic edge is silently added.
   -- Warn_If_Unresolved_By_Data
   --    A warning is not issued if the new dynamic edge is immediately
   --    resolved to a static edge by applying the data-state of the
   --    source step.
   -- Always_Warn
   --    A warning is always issued.
   --
   -- Note that there may also be command-line options that interact
   -- with these choices and may, for example, suppress warnings even
   -- when Always_Warn is chosen.


   procedure Add_Dynamic_Edge (
      To     : in Graph_T;
      Source : in Step_T;
      Edge   : in Dynamic_Edge_T;
      Warn   : in Warning_Opt_T := Always_Warn);
   --
   -- Adds a dynamic Edge from the Source step to a dynamically computed
   -- (set of) target addresses. The state of the Edge is initialized
   -- to Unresolved and its domain is considered to have grown. If the
   -- role of the Edge is Undefined, the role is set to Boundable_Jump.
   --
   -- If Tag (Source).Data is not Any_Data, the operation tries to
   -- resolve the new Edge by applying Tag (Source).Data. If this is
   -- successful, the Edge is closed and deleted.
   --
   -- The Warn parameter controls when a warning is issued. However,
   -- the Warn_Flow option must also be enabled for any warnings to
   -- result.


   procedure Add_Resolved_Edge (
      To     : in     Graph_T;
      Source : in out Boundable_Edge_T'Class;
      Cond   : in     Arithmetic.Condition_T := Arithmetic.Always;
      Time   : in     Processor.Time_T;
      Target : in     Step_Tag_T;
      Info   : in     Processor.Loose_Edge_Info_T :=
                         Processor.No_Loose_Edge_Info);
   --
   -- Adds a real edge, which is resolved from a dynamic Source edge
   -- and thus has the same source step, to a Target state (dynamically
   -- computed from the Source) and with given precondition and
   -- execution time.
   --
   -- The new edge is added using Add_Edge (Target : Step_Tag_T) and
   -- so it can be either a bound edge or a loose edge and the process
   -- can involve Data state transformations and can refine the Cond
   -- based on the Data states. See that Add_Edge for details.
   --
   -- Sets the state of the boundable Source edge to Growing to
   -- show that the flow-graph is growing. Increments the number of
   -- Resolvents of the Source. Notes that the Source is no longer
   -- "unstable".


   procedure Add_Resolved_Edge (
      To     : in     Graph_T;
      Source : in out Boundable_Edge_T'Class;
      Cond   : in     Arithmetic.Condition_T := Arithmetic.Always;
      Time   : in     Processor.Time_T;
      Target : in     Step_T);
   --
   -- Adds a real edge, which is resolved from a dynamic Source edge
   -- and thus has the same source step, to an existing Target step
   -- (with a tag dynamically computed from the Source) and with
   -- given precondition and execution time.
   --
   -- The new edge is added using Add_Edge (Target : Step_T) and
   -- so the Cond may be refined based on Tag (Target).Data. See
   -- that Add_Edge for details.
   --
   -- Sets the state of the boundable Source edge to Growing to
   -- show that the flow-graph is growing. Increments the number of
   -- Resolvents of the Source. Notes that the Source is no longer
   -- "unstable".


   procedure Add_Resolved_Dynamic_Edge (
      To     : in     Graph_T;
      Source : in out Boundable_Edge_T'Class;
      Edge   : in     Dynamic_Edge_T;
      Warn   : in     Warning_Opt_T := Always_Warn);
   --
   -- Adds a new dynamic Edge, which is resolved from a dynamic Source
   -- edge and thus has the same source step, but still needs some
   -- dynamic computation to resolve the (set of) target addresses.
   --
   -- The new dynamic Edge is added using Add_Dynamic_Edge and so it
   -- may be resolved immediately by Tag (Source (Source)).Data; see
   -- Add_Dynamic_Edge for details.
   --
   -- The Warn parameter is passed on to Add_Dynamic_Edge where it
   -- controls the possible warning for dynamic flow.
   --
   -- Sets the state of the boundable Source edge to Growing to
   -- show that the flow-graph is growing. Increments the number of
   -- Resolvents of the Source. Notes that the Source is no longer
   -- "unstable".


   procedure Reject_Resolved_Edge (
      Source : in Boundable_Edge_T'Class;
      Expr   : in Arithmetic.Expr_Ref;
      Value  : in Arithmetic.Word_T;
      Reason : in String);
   --
   -- Reports that the Source has rejected a putative edge that would
   -- correspond to the given Value, analysed as a possible value of
   -- the dynamic Expression of the Source, for the given Reason.
   -- The Reason can be, for example, that the address computed from
   -- the Value is not a valid code address (misaliged, out of range).


   procedure Remove_Dynamic_Edge (
      Edge : in out Dynamic_Edge_T;
      From : in     Graph_T);
   --
   -- Deletes the given dynamic Edge From the graph. This means that
   -- the dynamic control flow at _this_ Edge is considered fully
   -- resolved, or at least as fully resolved as possible. This
   -- happens mainly when the posible targets of the Edge have been
   -- asserted (see Take_Asserted_Targets).
   --
   -- Thus procedure applies to the Close operation to the Edge,
   -- after the Edge is deleted from the graph, but does not otherwise
   -- touch nor discard the underlying boundable-edge object.


   procedure Remove_All_Dynamic_Edges (From : in Graph_T);
   --
   -- Deletes all the dynamic edges From the graph. This means that
   -- the dynamic control flow is considered fully resolved or at least
   -- as fully resolved as possible.
   --
   -- The state of all dynamic edges must be Unresolved or Stable, else
   -- a fault message results.
   --
   -- This procedure applies the Close operation to each remaining
   -- dynamic edge in the graph, but  does not otherwise touch
   -- nor discard the underlying boundable-edge objects.


   procedure Collect_Blocks (Graph : in Graph_T);
   --
   -- Creates the basic blocks from the steps of the graph.
   -- Should be called once, after the graph has been constructed
   -- on the step level and before the graph is accessed on the
   -- block level.
   --
   -- Any value of type Node_T or Edge_T that was derived from this
   -- graph, before the call of Collect_Blocks, should no longer
   -- be used, because it may refer to deleted objects or to
   -- collections of steps that are no longer basic blocks.


   function Cells_In (Graph : Graph_T) return Storage.Cell_Set_T;
   --
   -- The set of storage cells that are accessed (read or
   -- written) in some step or edge precondition of the graph.
   -- Basis cells of boundable memory references and boundable
   -- edges are included.
   -- Cells accessed by callees are not included.


   function Cells_Used (
      Nodes : Node_List_T;
      Graph : Graph_T)
   return Storage.Cell_List_T;
   --
   -- The set of storage cells that are read in the nodes listed
   -- or in preconditions on edges within or leaving the nodes.
   -- Basis cells of boundable memory references and boundable
   -- edges are included.
   -- Cells accessed by callees are not included.


   function Cells_Defined (By : Graph_T)
   return Storage.Cell_Set_T;
   --
   -- The set of storage cells that are written in the graph.
   -- Cells written by callees are included if they are listed in the
   -- effect of the call step i.e. if the call has been "launched",
   -- see Flow.Calls.Launch.


   function Cells_Defined (
      Nodes : Node_List_T;
      Graph : Graph_T)
   return Storage.Cell_List_T;
   --
   -- The set of storage cells that are written in the nodes listed.
   -- Cells written by callees are included if they are listed in the
   -- effect of the call step.


   procedure Add_Dynamic_Edge_Basis_Cells (
      From  : in     Node_T;
      Graph : in     Graph_T;
      To    : in out Storage.Cell_Set_T);
   --
   -- Adds the Basis cells for all dynamic edges From the steps
   -- in the given Graph node To the given cell-set.


   procedure Add_Cells_Accessed (
      By    : in     Node_T;
      Graph : in    Graph_T;
      To    : in out Storage.Cell_Set_T);
   --
   -- Finds the cells accessed (read and/or written) by the given node
   -- (i.e. by the effects of the steps in the node or by the preconditions
   -- on edges leaving the node or by boundable objects as basis cells)
   -- and adds these cells to the given cell-set. Cells read by callees
   -- are not included. Cells written by callees are included if they are
   -- listed in the effect of the call step.


   function Is_Used (
      Location : Storage.Location_T;
      By       : Step_T)
   return Boolean;
   --
   -- Whether the given Location is used (read) By the given step.
   --
   -- The step is considered to use the location if the location maps
   -- to a cell at the address of the step such that the cell is
   -- > used in a Defining assignment in the effect of the step, or
   -- > used in the precondition of an edge leaving the step.
   --
   -- If the location maps to several cells at this step, it is enough
   -- for one of these cells to be used in the above ways.


   function Is_Used (
      Location : Storage.Location_T;
      By       : Node_List_T)
   return Boolean;
   --
   -- Whether the given Location is used (read) By the steps in the
   -- given nodes. For details see Is_Used (By : Step).


   function Is_Defined (
      Location : Storage.Location_T;
      By       : Step_T)
   return Boolean;
   --
   -- Whether the given Location is defined (assigned) By the given step.
   --
   -- The step is considered to define the location if the location maps
   -- to a cell at the address of the step such that the cell is
   -- > defined in a Defining assignment in the effect of the step, or
   -- > defined by the callee of a launched call-step.
   --
   -- If the location maps to several cells at this step, it is enough
   -- for one of these cells to be defined in the above ways.


   function Is_Defined (
      Location : Storage.Location_T;
      By       : Node_List_T)
   return Boolean;
   --
   -- Whether the given Location is defined (assigned) By the steps
   -- in the given nodes. For details see Is_Defined (By : Step).


private

   type Graph_Object_T;

   type Graph_T is access Graph_Object_T;

   Null_Graph : constant Graph_T := null;

   type Step_Object_T;

   type Step_T is access Step_Object_T;

   No_Step : constant Step_T := null;

   type Step_Edge_Object_T;

   type Step_Edge_T is access Step_Edge_Object_T;

   No_Step_Edge : constant Step_Edge_T := null;


   type Boundable_Edge_T is abstract new Storage.Bounds.Boundable_T
   with record
      Source       : Step_T                := No_Step;
      Role         : Boundable_Edge_Role_T := Undefined;
      State        : Edge_Resolution_T     := Unresolved;
      Resolvents   : Natural               := 0;
      Returns      : Boolean               := False;
      Domain_Grown : Boolean               := True;
      Unstable     : Boolean               := False;
   end record;
   --
   -- The common part of a boundable edge.
   --
   -- Source
   --    The source step, natch.
   -- Role
   --    The assumed or confirmed role of the edge.
   -- State
   --    The current state of resolution of the edge.
   -- Resolvents
   --    The number of static step-edges that this boundable
   --    edge has spawned.
   -- Returns
   --    Whether this boundable edge has been resolved to
   --    a return-from-subprogram.
   -- Domain_Grown
   --    Whether the domain of this boundable edge has grown since
   --    the last full (arithmetic) analysis of the edge. The domain
   --    grows if new edges enter the steps in the domain. A new
   --    boundable edge has Domain_Grown set by default so that
   --    it will always be analysed in the first round of boundable-
   --    edge analysis after the edge is created.
   -- Unstable
   --    Whether this boundable edge should be (re-)analysed
   --    in this round of boundable-edge analysis, assuming
   --    that the edge is feasible (reachable).


   type Node_Object_T (Number_Of_Steps : Positive);

   type Node_T is access Node_Object_T;

   No_Node : constant Node_T := null;

   type Edge_Object_T;

   type Edge_T is access Edge_Object_T;

   Null_Edge : constant Edge_T := null;


   function Image (Item : Step_T) return String;
   --
   -- A description of the step, for tracing purposes.


end Flow;
