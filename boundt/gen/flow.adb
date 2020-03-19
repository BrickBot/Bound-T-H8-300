-- Flow (body)
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
-- $Revision: 1.84 $
-- $Date: 2015/10/24 20:05:49 $
--
-- $Log: flow.adb,v $
-- Revision 1.84  2015/10/24 20:05:49  niklas
-- Moved to free licence.
--
-- Revision 1.83  2014/06/11 12:55:13  niklas
-- Updated Code_Range for new location of Code_Address_T.
--
-- Revision 1.82  2012-01-19 21:01:17  niklas
-- BT-CH-0223: Package License does not depend on package Flow.
--
-- Revision 1.81  2011-10-18 20:12:28  niklas
-- Added Prime_Address (Node_T), for ALF export.
--
-- Revision 1.80  2009-12-27 22:34:31  niklas
-- BT-CH-0205: Licensing based on code size and time/space dimensions.
--
-- Revision 1.79  2009-11-27 11:28:07  niklas
-- BT-CH-0184: Bit-widths, Word_T, failed modular analysis.
--
-- Revision 1.78  2009-10-07 19:26:10  niklas
-- BT-CH-0183: Cell-sets are a tagged-type class.
--
-- Revision 1.77  2009-05-23 19:04:49  niklas
-- Removed redundant "Flow: " prefixes from Trace messages.
--
-- Revision 1.76  2009/05/15 12:16:48  niklas
-- BT-CH-0173: Handling cells used by Range_Post constraints.
--
-- Revision 1.75  2008/11/18 15:09:37  niklas
-- Added Reject_Resolved_Edge.
--
-- Revision 1.74  2008/07/20 06:31:38  niklas
-- BT-CH-0137: Cached Locus for subprograms and execution bounds.
--
-- Revision 1.73  2008/07/14 19:16:56  niklas
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
-- Revision 1.68  2007/12/22 15:23:47  niklas
-- BT-CH-0101: Option "-trace graph".
--
-- Revision 1.67  2007/12/17 13:54:37  niklas
-- BT-CH-0098: Assertions on stack usage and final stack height, etc.
--
-- Revision 1.66  2007/11/14 10:41:40  niklas
-- Small fix in format of Full_Image for boundable edge.
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
-- Revision 1.60  2007/08/25 18:50:03  niklas
-- Added Image (Step_Data_Ref) suitable for tracing.
-- Added Full_Image (Step_Tag_T) suitable for tracing.
-- Made Image (Step_T) privately accessible for tracing purposes.
-- Added optional parameter Add_Dynamic_Edge.Warn by which the
-- warning can be disabled even under "-warn flow".
-- Added Trace_Also_Data for tracing data states and included calls
-- in Take_Loose_Edge, Bind_Loose_Edges, Insert (Step), and Add_Edge.
-- Extended Insert (Step) to optionally check graph consistency.
-- Removed the Note from Close (Boundable_Edge_T) and changed the
-- Warning to use Image (Edge) instead of the expanded tag name.
--
-- Revision 1.59  2007/07/22 14:39:43  niklas
-- Added types Step_Count_T and Node_Count_T and changed the types
-- Step_Index_T and Node_Index_T to subtypes. The functions Max_Step
-- and Max_Node now return Step/Node_Count_T, allowing a zero value.
--
-- Revision 1.58  2007/07/21 18:39:19  niklas
-- Fixed Add_Dynamic_Edge to avoid double warning for dynamic calls.
--
-- Revision 1.57  2007/07/21 18:18:42  niklas
-- BT-CH-0064. Support for AVR/IAR switch-handler analysis.
--
-- Revision 1.56  2007/04/18 18:34:39  niklas
-- BT-CH-0057.
--
-- Revision 1.55  2007/03/29 15:18:03  niklas
-- BT-CH-0056.
--
-- Revision 1.54  2007/03/28 14:00:42  niklas
-- Using Growing_Vectors instead of Unbounded_Vectors.
--
-- Revision 1.53  2007/03/22 12:53:25  niklas
-- Added function Prime_Address (Step_Tag_T).
--
-- Revision 1.52  2007/01/25 21:25:16  niklas
-- BT-CH-0043.
--
-- Revision 1.51  2006/10/24 21:41:19  niklas
-- BT-CH-0030.
--
-- Revision 1.50  2006/10/24 08:44:32  niklas
-- BT-CH-0028.
--
-- Revision 1.49  2006/09/05 18:40:02  niklas
-- Improved (corrected) Collect_Blocks to have a space complexity
-- linear in the number of step-edges, rather than quadratic in
-- the number of steps in the flow-graph. The new algorithm uses
-- a work-list instead of recursion. However, the problem was not
-- recursion as such, but using a local variable (New_Node.Steps)
-- of a worst-case size (number of steps) on all recursion levels.
--
-- Revision 1.48  2006/05/29 11:22:34  niklas
-- BT-CH-0023.
--
-- Revision 1.47  2006/05/18 13:02:22  niklas
-- Added functions Only_Predecessor and Only_Successor.
--
-- Revision 1.46  2006/05/06 06:59:21  niklas
-- BT-CH-0021.
--
-- Revision 1.45  2006/04/28 09:16:45  niklas
-- Added function Is_Member (Tag, Step_List).
--
-- Revision 1.44  2006/02/24 15:55:54  niklas
-- Added an overriding "=" for Step_Tag_T, to make sure that we
-- use a possible overriding Processor."=" for the State.
-- Added function State (Step_Tag_T) for brevity.
-- Removed superfluous "in" modes from some function parameters.
--
-- Revision 1.43  2005/10/20 15:25:52  niklas
-- Added function Effort (Step).
--
-- Revision 1.42  2005/10/13 19:27:39  niklas
-- BT-CH-0014.
--
-- Revision 1.41  2005/09/12 19:03:00  niklas
-- BT-CH-0008.
--
-- Revision 1.40  2005/09/05 11:23:38  niklas
-- BT-CH-0007.
--
-- Revision 1.39  2005/09/03 11:50:29  niklas
-- BT-CH-0006.
--
-- Revision 1.38  2005/09/01 08:03:21  niklas
-- Removed unused function Steps_By_Address.
--
-- Revision 1.37  2005/08/08 13:59:49  niklas
-- Implemented an "=" operation for Node_Set_T to work around a
-- bug with packed boolean arrays in Gnat 3.15p.
--
-- Revision 1.36  2005/06/14 17:25:16  niklas
-- Changed the two procedures Add_Resolved_Edge to use Output.Trace
-- and shortened the output a bit (the omitted information can be
-- traced in Add_Edge under "-trace flow").
--
-- Revision 1.35  2005/02/23 09:05:19  niklas
-- BT-CH-0005.
--
-- Revision 1.34  2005/02/16 21:11:45  niklas
-- BT-CH-0002.
--
-- Revision 1.33  2004/10/10 09:58:37  niklas
-- Add function Time for node edges.
--
-- Revision 1.32  2004/04/25 17:44:06  niklas
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
-- Revision 1.31  2003/03/11 08:22:40  holsti
-- Added Update_Edge_Times.
--
-- Revision 1.30  2003/01/03 11:46:48  holsti
-- Using Flow.Opt.Trace_Construction.
--
-- Revision 1.29  2001/12/10 15:10:46  holsti
-- Node_Edge returns Null_Edge when the given step-edge does not
-- cross blocks (it used to return an uninitialized edge).
--
-- Collect_Blocks verifies that all steps are placed in blocks.
--
-- Revision 1.28  2001/11/19 11:05:58  saarinen
-- Added functions Sources and Targets for Step_Edge_List_T.
-- Added function Return_Nodes.
-- Added function Node_Edge.
--
-- Revision 1.27  2001/06/18 20:54:18  holsti
-- Step_Address equality function obeyed (NC_133).
--
-- Revision 1.26  2001/05/21 13:45:50  holsti
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
-- Revision 1.25  2001/03/21 20:21:13  holsti
-- An Error changed to a Fault. Reformatting.
--
-- Revision 1.24  2001/03/19 08:15:07  ville
-- Add_Step denies call
--
-- Revision 1.23  2001/03/16 19:21:07  holsti
-- Branches and loops to the entry step allowed (NC_021).
--
-- Revision 1.22  2001/03/16 09:35:25  ville
-- Added function returning effort of step list
--
-- Revision 1.21  2001/03/08 10:38:26  holsti
-- Return_Edges and Cells_Defined (By: Graph) added.
--
-- Revision 1.20  2001/01/13 11:37:07  holsti
-- Removed to-be's in favour or NC's.
--
-- Revision 1.19  2001/01/07 21:56:37  holsti
-- Step operations added to suppport liveness analysis.
--
-- Revision 1.18  2000/12/28 19:43:41  holsti
-- Node_At (Step_Address_T) removed (not used).
--
-- Revision 1.17  2000/12/28 17:38:31  holsti
-- Unresolved_Flow added.
--
-- Revision 1.16  2000/12/22 13:33:03  sihvo
-- Added Steps_By_Address.
--
-- Revision 1.15  2000/12/21 15:57:52  saarinen
-- Corrected NC_023 and the same problem with Add_Cells_Accessed.
--
-- Revision 1.14  2000/08/20 21:01:36  holsti
-- Cells_In implemented.
--
-- Revision 1.13  2000/08/18 18:14:32  holsti
-- Added edge-count types to allow for graph with no edges.
-- Unbounded_Vectors Index_Type removed.
--
-- Revision 1.12  2000/07/21 21:07:04  holsti
-- Unresolved_Steps replaces Steps_With_Unresolved_Flow.
--
-- Revision 1.11  2000/07/18 19:51:56  holsti
-- Node_Containing implemented.
--
-- Revision 1.10  2000/07/16 13:07:56  holsti
-- Implemented operations for Node_Index_List_T and cell-sets.
--
-- Revision 1.9  2000/07/14 20:33:27  holsti
-- Changed return value type of Cells_Used and Cells_Defined.
--
-- Revision 1.8  2000/07/13 13:38:15  saarinen
-- Added empty bodies for Node_Containing, Cells_Used and Cells_Defined.
--
-- Revision 1.7  2000/07/12 20:43:06  holsti
-- Implemented Node_Set_T operations and Edge_At.
--
-- Revision 1.6  2000/06/28 13:37:40  holsti
-- Dynamics and Bounds added.
--
-- Revision 1.5  2000/06/12 14:02:57  holsti
-- Added Cells_In (Graph) (not implemented yet).
--
-- Revision 1.4  2000/06/11 21:38:36  holsti
-- Basic-block collection implemented.
--
-- Revision 1.3  2000/06/11 19:03:52  holsti
-- First implementation.
--
-- Revision 1.2  2000/04/27 10:45:52  holsti
-- Using loose graph edges.
--
-- Revision 1.1  2000/04/24 11:06:06  holsti
-- Program-dump ability added.
--


--:dbpool with GNAT.Debug_Pools;

with Ada.Tags;
with Ada.Text_IO;
with System;
with System.Address_To_Access_Conversions;
with System.Storage_Elements;
with Unchecked_Deallocation;

with Bags;      -- From MW Components.
with Bounded_Queues;
with Flow.Calls;
with Flow.Checks;
with Flow.Opt;
with Growing_Vectors;
with Output;
with Storage.Bitvec_Cell_Sets;

pragma Elaborate_All (Bags);
pragma Elaborate_All (Growing_Vectors);


package body Flow is


   procedure Trace (Text : in String)
   --
   -- Tracing the construction operations.
   --
   is
   begin

      Ada.Text_IO.Set_Col (1);

      Ada.Text_IO.Put ("Flow: ");

      Ada.Text_IO.Put_Line (Text);

   end Trace;


   procedure Trace_Also_Data (
      Kind : in String;
      Data : in Step_Data_Ref)
   --
   -- Adding some Data state information to a Trace.
   --
   is
   begin

      Ada.Text_IO.Put ("   ");
      Ada.Text_IO.Put (Kind);
      Ada.Text_IO.Put (": ");
      Ada.Text_IO.Put_Line (Image_All (Data));

   end Trace_Also_Data;


   --
   ---   Unbounded vectors for various collections.
   --


   package Step_Vectors is new Growing_Vectors (
      Element_Type    => Step_T,
      Vector_Type     => Step_List_T,
      Initial_Size    => 20,
      First_Increment => 30,
      Deallocate      => Opt.Deallocate);

   subtype Unbounded_Step_Vector_T is Step_Vectors.Unbounded_Vector;
   --
   -- The index is considered a Step_Index_T.


   package Step_Edge_Vectors is new Growing_Vectors (
      Element_Type => Step_Edge_T,
      Vector_Type  => Step_Edge_List_T,
      Initial_Size => 2,
      Deallocate   => Opt.Deallocate);

   subtype Unbounded_Step_Edge_Vector_T is Step_Edge_Vectors.Unbounded_Vector;
   --
   -- The index is considered a Step_Edge_Index_T.


   package Node_Vectors is new Growing_Vectors (
      Element_Type    => Node_T,
      Vector_Type     => Node_List_T,
      Initial_Size    => 20,
      First_Increment => 30,
      Deallocate      => Opt.Deallocate);

   subtype Unbounded_Node_Vector_T is Node_Vectors.Unbounded_Vector;
   --
   -- The index is considered a Node_Index_T.


   package Edge_Vectors is new Growing_Vectors (
      Element_Type => Edge_T,
      Vector_Type  => Edge_List_T,
      Initial_Size => 2,
      Deallocate   => Opt.Deallocate);

   subtype Unbounded_Edge_Vector_T is Edge_Vectors.Unbounded_Vector;
   --
   -- The index is considered an Edge_Index_T.


   package Dynamic_Edge_Vectors is new Growing_Vectors (
      Element_Type => Dynamic_Edge_T,
      Vector_Type  => Dynamic_Edge_List_T,
      Deallocate   => Opt.Deallocate);

   subtype Unbounded_Dynamic_Edge_Vector_T is
      Dynamic_Edge_Vectors.Unbounded_Vector;
   --
   -- The index has no logical meaning.


   use Step_Vectors;
   use Step_Edge_Vectors;
   use Node_Vectors;
   use Edge_Vectors;
   use Dynamic_Edge_Vectors;


   package Step_Bags is new Bags (
      Key_Type  => Step_Tag_T,
      Item_Type => Step_T,
      Key_Of    => Tag,
      "="       => Flow."=",
      "<"       => Flow."<",
      Count     => Natural);


   function Target_Of (Item : Loose_Edge_T) return Step_Tag_T
   --
   -- The step address which is the target of the loose step.
   -- For instantiating bags of loose edges.
   --
   is
   begin

      return Item.Target;

   end Target_Of;


   package Loose_Edge_Bags is new Bags (
      Key_Type  => Step_Tag_T,
      Item_Type => Loose_Edge_T,
      Key_Of    => Target_Of,
      "="       => Flow."=",
      "<"       => Flow."<",
      Count     => Natural);


   --
   ---   Mapping steps to nodes:
   --


   type Node_Of_Step_T is array (Step_Index_T range <>) of Node_T;
   --
   -- Maps a step (index) to the node that contains the step.

   type Node_Of_Step_Ref is access Node_Of_Step_T;

   --:dbpool Node_Of_Step_Pool : GNAT.Debug_Pools.Debug_Pool;

   --:dbpool for Node_Of_Step_Ref'Storage_Pool use Node_Of_Step_Pool;

   procedure Free is new Unchecked_Deallocation (
      Object => Node_Of_Step_T,
      Name   => Node_Of_Step_Ref);


   --
   ---   Object types
   --


   type Graph_Object_T is record
      Steps          : Unbounded_Step_Vector_T;
      Steps_By_Tag   : Step_Bags.Bag (Duplicate_Keys_Allowed => False);
      Step_Edges     : Unbounded_Step_Edge_Vector_T;
      Loose_Edges    : Loose_Edge_Bags.Bag (Duplicate_Keys_Allowed => True);
      Dynamic_Edges  : Unbounded_Dynamic_Edge_Vector_T;
      Nodes          : Unbounded_Node_Vector_T;
      Edges          : Unbounded_Edge_Vector_T;
      Node_Of_Step   : Node_Of_Step_Ref;
      Max_Old_Step   : Step_Count_T := 0;
      Rejoin_Steps   : Unbounded_Step_Vector_T;
   end record;
   --
   -- A control-flow graph.
   -- The unbounded-vector components are indexed by the respective
   -- "index" types, although the Ada index type is Positive.
   -- The node-level entities are created and valid only after
   -- the Collect_Basic_Blocks operation.
   --
   -- Steps
   --    The vector of all steps in the graph.
   --    Indexed by Step_Index_T.
   --    The entry-step is at index 1.
   --
   -- Steps_By_Tag
   --    A mapping from step-tag to step (ref), containing all the
   --    steps in the graph.
   --
   -- Step_Edges
   --    The vector of all step-edges in the graph.
   --    Indexed by Step_Edge_Index_T.
   --
   -- Loose_Edges
   --    The set of all loose edges in the graph.
   --    Treated as a priority queue (although the type
   --    is probably not as efficient as a real prio queue).
   --    Note that there may be several loose edges to the same
   --    target address; therefore this is a bag which allows
   --    duplicate keys.
   --
   -- Dynamic_Edges
   --    The set of all unresolved or partially resolved dynamic edges
   --    in the graph. Logically unordered. In principle several dynamic
   --    edges may have the same source step, although this is unlikely
   --    to arise in actual target architectures and programs.
   --
   -- Nodes
   --    The vector of all nodes (basic blocks) in the graph.
   --    Indexed by Node_Index_T.
   --    The entry node is at index 1.
   --
   -- Edges
   --    The vector of all block-level edges in the graph.
   --    Indexed by Edge_Index_T.
   --
   -- Node_Of_Step
   --    The vector that associates a step (index) with the node
   --    that contains the step. Created when basic-blocks are
   --    collected.
   --
   -- Max_Old_Step
   --    The maximum Step_Index_T that existed at the start of the
   --    present round of analysing and resolving dynamic edges and
   --    subsequent flow tracing. Such steps (with index less or
   --    equal to Max_Old_Step) are known as "old" steps. All steps
   --    with index > Max_Old_Step were created thru the new edges
   --    spawned from the dynamic edges, and are known as "new" steps.
   --
   -- Rejoin_Steps
   --    Those "old" steps -- steps that existed before the last round
   --    of analysing and resolving dynamic edges -- that received new
   --    incoming edges when the flow graph was extended thru the
   --    resolved dynamic edges. These new edges usually come from "new"
   --    steps but can also come from "old" steps. Thus, the Rejoin_Steps
   --    are the steps that may bring new data states into the "old" part
   --    of the flow-graph which can force us to re-analyse boundable
   --    parts in that "old" part, for example dynamic edges.


   type Step_Object_T is record
      Index  : Step_Index_T;
      Tag    : Step_Tag_T;
      Effect : Arithmetic.Effect_Ref;
      Info   : Processor.Step_Info_T;
      Post   : Step_Data_Ref := Any_Data;
      Into   : Unbounded_Step_Edge_Vector_T;
      From   : Unbounded_Step_Edge_Vector_T;
   end record;
   --
   -- A step.
   --
   -- Index
   --    The index of the step (thus, a given step can really only
   --    belong to one graph, or at least it must have the same index
   --    in all graphs that contain it).
   --
   -- Tag
   --    The identifying tag of the step. In any one graph, no two steps
   --    can have the same tag.
   --    This is they key-value in the graph's Steps_By_Tag table.
   --
   -- Effect
   --    The arithmetic, computational effect of the step.
   --    Possibly null.
   --
   -- Info
   --    The processor-dependent, decoder-generated information.
   --
   -- Post
   --    The data state at the end of the step, after applying the
   --    Effect to the data state defined by Tag.Data.
   --    Post shall be Any_Data if and only if Tag.Data is Any_Data.
   --
   -- Into
   --    List of references to incoming edges.
   --
   -- From
   --    List of references to outgoing edges.
   --
   -- A given edge E that goes from step S to step T will occur
   -- in S.From and also in T.Into.


   Total_Step_Count : Step_Count_T := 0;
   --
   -- The total number of steps created so far.


   type Step_Edge_Object_T is record
      Index  : Step_Edge_Index_T;
      Source : Step_T;
      Cond   : Arithmetic.Condition_T;
      Time   : Processor.Time_T;
      Target : Step_T;
   end record;


   type Node_Object_T (Number_Of_Steps : Positive) is record
      Index : Node_Index_T;
      Steps : Step_List_T (1 .. Number_Of_Steps);
      Into  : Unbounded_Edge_Vector_T;
      From  : Unbounded_Edge_Vector_T;
   end record;


   type Edge_Object_T is record
      Index     : Edge_Index_T;
      Source    : Node_T;
      Step_Edge : Step_Edge_T;
      Target    : Node_T;
   end record;



   --
   ---   Flow graphs
   --


   procedure Create (Graph : in out Graph_T)
   is
   begin

      -- TBA deallocate old Graph.

      Graph := new Graph_Object_T;

   end Create;


   function Is_Empty (Graph : Graph_T) return Boolean
   is
   begin

      return Length (Graph.Steps) = 0;

   end Is_Empty;


   function Max_Step (Graph : in Graph_T) return Step_Count_T
   is
   begin

      return Step_Count_T (Last (Graph.Steps));

   end Max_Step;


   function Max_Node (Graph : in Graph_T) return Node_Count_T
   is
   begin

      return Node_Count_T (Last (Graph.Nodes));

   end Max_Node;


   function Max_Step_Edge (Graph : in Graph_T) return Step_Edge_Count_T
   is
   begin

      return Step_Edge_Count_T (Last (Graph.Step_Edges));

   end Max_Step_Edge;


   function Max_Edge (Graph : in Graph_T) return Edge_Count_T
   is
   begin

      return Edge_Count_T (Last (Graph.Edges));

   end Max_Edge;


   function Code_Range (Graph : Graph_T) return Storage.Code_Address_Range_T
   is

      Code : Storage.Code_Address_Range_T :=
         Processor.Empty_Code_Address_Range;
      -- The result to be, initialized to the empty interval.

   begin

      for S in First (Graph.Steps) .. Last (Graph.Steps) loop

         Processor.Widen (
            Rainge     => Code,
            To_Include => Prime_Address (Element (Graph.Steps, S)));

      end loop;

      return Code;

   end Code_Range;


   --
   ---   Step contexts, step data, and step tags
   --



   function Nested_Image (Item : Step_Context_Ref) return String
   is
   begin

      if Item = No_Context then
         -- No context to speak of.

         return "";

      else
         -- Show the outer levels first, then this one:

         return Nested_Image (Item.Outer) & Image (Item.all);

      end if;

   end Nested_Image;


   function Image (Item : Step_Tag_T) return String
   is
   begin

      if Item.Data = Any_Data then

         return Processor.Image (Item.State)
              & Nested_Image    (Item.Context);

      else
         -- Just a little mark "=" to show the presence of data.

         return Processor.Image (Item.State)
              & Nested_Image    (Item.Context)
              & '=';

      end if;

   end Image;


   package Context_Address_Conversions
   is new System.Address_To_Access_Conversions (
      Object => Step_Context_T'Class);


   package Data_Address_Conversions
   is new System.Address_To_Access_Conversions (
      Object => Step_Data_T'Class);


   function To_Address (Item : Step_Context_Ref) return System.Address
   --
   -- For implementing an "<" on Context_Refs that works also when
   -- one or both of the comparands is null.
   --
   is
      use Context_Address_Conversions;
   begin

      return To_Address (Object_Pointer (Item));

   end To_Address;


   function To_Address (Item : Step_Data_Ref) return System.Address
   --
   -- For implementing an "<" on Data_Refs that works also when
   -- one or both of the comparands is null.
   --
   is
      use Data_Address_Conversions;
   begin

      return To_Address (Object_Pointer (Item));

   end To_Address;


   function Image (Item : System.Address) return String
   --
   -- The address as an integer.
   --
   is
      use System.Storage_Elements;
   begin

      return Integer_Address'Image (To_Integer (Item));

   end Image;


   function Image (Item : Step_Data_Ref) return String
   is
   begin

      return '/'
           & Image (To_Address (Item))
           & "/ "
           & Image_All (Item);

   end Image;


   function Full_Image (Item : Step_Tag_T) return String
   is
   begin

      return
           Processor.Image (Item.State)
         & '/'
         & Image (To_Address (Item.Context))
         & Image (Item.Data);

   end Full_Image;


   function "=" (Left, Right : Step_Tag_T) return Boolean
   is
   begin

      return   Left.Context = Right.Context
      and then Left.Data    = Right.Data
      and then Processor."=" (Left.State, Right.State);

   end "=";


   function "<" (Left, Right : Step_Tag_T) return Boolean
   is
      use type System.Address;
   begin

      -- Lexicographic ordering in the order State, Context, Data.

      if Processor."<" (Left.State, Right.State) then

         return True;

      elsif Processor."=" (Left.State, Right.State) then
         -- States are equal. Let Context and Data decide.

         if To_Address (Left.Context) < To_Address (Right.Context) then

            return True;

         elsif To_Address (Left.Context) = To_Address (Right.Context) then
            -- State and Context are equal. Let Data decide.

            return To_Address (Left.Data) < To_Address (Right.Data);

         else
            -- States are equal but Left Context > Right Context.

            return False;

         end if;

      else
         -- Left.State > Right.State.

         return False;

      end if;

   end "<";


   --
   ---   In_Transit and Transit
   --


   type In_Transit_T is new Step_Data_T with null record;

   type In_Transit_Ref is access In_Transit_T;


   --    Primitive operations for In_Transit_T


   -- overriding
   function Image (Item : In_Transit_T) return String;


   -- overriding
   procedure Apply (
      Pre   : access In_Transit_T;
      Post  : access In_Transit_T;
      Upon  : in out Boundable_Edge_T'Class;
      Graph : in     Graph_T);
   --
   -- Dummy implementation.


   -- overriding
   procedure Apply (
      Data   : access In_Transit_T;
      Upon   : in     Calling.Protocol_T'Class;
      Giving :    out Calling.Protocol_Ref);
   --
   -- Dummy implementation.


   --    Implementation of primitives for In_Transit_T


   function Image (Item : In_Transit_T) return String
   is
   begin

      return "[in transit]";

   end Image;


   procedure Apply (
      Pre   : access In_Transit_T;
      Post  : access In_Transit_T;
      Upon  : in out Boundable_Edge_T'Class;
      Graph : in     Graph_T)
   is
   begin

      Output.Fault (
         Location => "Apply (In_Transit_T, Boundable_Edge_T)",
         Text     => "Dummy operation called.");

   end Apply;


   procedure Apply (
      Data   : access In_Transit_T;
      Upon   : in     Calling.Protocol_T'Class;
      Giving :    out Calling.Protocol_Ref)
   is
   begin

      Output.Fault (
         Location => "Apply (In_Transit_T, Calling_Protocol_T)",
         Text     => "Dummy operation called.");

      Giving := null;

   end Apply;


   In_Transit_Solo : constant In_Transit_Ref := new In_Transit_T;
   --
   -- The single and solitary In_Transit_T object.


   In_Transit_Solo_Ref : constant Step_Data_Ref :=
      Step_Data_Ref (In_Transit_Solo);
   --
   -- Same with a convenient type.


   function In_Transit return Step_Data_Ref
   is
   begin

      return In_Transit_Solo_Ref;

   end In_Transit;


   function Transit (
      From : Step_Tag_T;
      To   : Processor.Flow_State_T)
   return Step_Tag_T
   is
   begin

      if From.Data = Any_Data then
         -- There will be no Data transformation or
         -- edge precondition refinement.

         return (
            Context => From.Context,
            Data    => Any_Data,
            State   => To);

      else
         -- Mark the Data "in transit" so that Add_Step
         -- will transform it, using the step's effect,
         -- to the step's Post Data.

         return (
            Context => From.Context,
            Data    => In_Transit_Solo_Ref,
            State   => To);

      end if;

   end Transit;


   --
   ---   Steps in flow graphs
   --


   function Index_Image (Item : Step_List_T) return String
   is
   begin

      if Item'Length = 0 then

         return "";

      else

         return
              Step_Index_T'Image (Index (Item(Item'First)))
            & Index_Image (Item(Item'First + 1 .. Item'Last));

      end if;

   end Index_Image;


   function No_Steps return Step_List_T
   is
   begin

      return (1 .. 0 => No_Step);

   end No_Steps;


   function Is_Member (
      Tag   : Step_Tag_T;
      Steps : Step_List_T)
   return Boolean
   is
   begin

      for S in Steps'Range loop

         if Steps(S).Tag = Tag then

            return True;

         end if;

      end loop;

      return False;

   end Is_Member;


   function Index (Step : Step_T) return Step_Index_T
   is
   begin
      return Step.Index;
   end Index;


   function Tag (Step : Step_T) return Step_Tag_T
   is
   begin

      return Step.Tag;

   end Tag;


   function State (Step : Step_T) return Processor.Flow_State_T
   is
   begin

      return Step.Tag.State;

   end State;


   function State (Tag : Step_Tag_T) return Processor.Flow_State_T
   is
   begin

      return Tag.State;

   end State;


   function Context (Step : Step_T) return Step_Context_Ref
   is
   begin

      return Step.Tag.Context;

   end Context;


   function Data (Step : Step_T) return Step_Data_Ref
   is
   begin

      return Step.Tag.Data;

   end Data;


   function Data (Tag : Step_Tag_T) return Step_Data_Ref
   is
   begin

      return Tag.Data;

   end Data;


   function Post_Data (Step : Step_T) return Step_Data_Ref
   is
   begin

      return Step.Post;

   end Post_Data;


   function Prime_Address (Step : in Step_T)
   return Processor.Code_Address_T
   is
   begin

      return Processor.Prime_Address (State (Step));

   end Prime_Address;


   function Prime_Address (Tag : in Step_Tag_T)
   return Processor.Code_Address_T
   is
   begin

      return Processor.Prime_Address (State (Tag));

   end Prime_Address;


   function Tag_Less (Left, Right : Step_T) return Boolean
   is
   begin

      return Left.Tag < Right.Tag;

   end Tag_Less;


   function Transit (
      From : Step_T;
      To   : Processor.Flow_State_T)
   return Step_Tag_T
   is
   begin

      return Transit (From => From.Tag, To => To);

   end Transit;


   function Effect (Step : in Step_T) return Arithmetic.Effect_T
   is
   begin
      return Step.Effect.all;
   end Effect;


   function Effect (Step : in Step_T) return Arithmetic.Effect_Ref
   is
   begin
      return Step.Effect;
   end Effect;


   function Is_Feasible (Step : Step_T; Within : Graph_T)
   return Boolean
   is

      Enterable : Boolean;
      -- Whether the step can be entered.

   begin

      -- A step is considered feasible if it is the entry step
      -- or at least one feasible edge leads to it.

      Enterable := Step = Entry_Step (Within);

      for I in 1 .. Length (Step.Into) loop

         if Is_Feasible (Edge => Element (Step.Into, I)) then

            Enterable := True;

            exit;

         end if;

      end loop;

      return Enterable;

   end Is_Feasible;


   function Image (Item : Step_T) return String
   --
   -- A description of the step, for tracing purposes.
   --
   is
   begin

      return
           "Step"
         & Step_Index_T'Image (Index (Item))
         & ' '
         & Image (Tag (Item));

   end Image;


   procedure Set_Effect (
      Step   : in Step_T;
      Effect : in Arithmetic.Effect_T)
   is
   begin

      Step.Effect := Arithmetic.To_Effect_Ref (Effect);

   end Set_Effect;


   procedure Set_Effect (
      Step   : in Step_T;
      Effect : in Arithmetic.Effect_Ref)
   is
   begin

      Step.Effect := Effect;

   end Set_Effect;


   function Target_Range (Step : Step_T)
   return Storage.Alias_Range_T
   is
   begin

      return Arithmetic.Target_Range (Step.Effect.all);

   end Target_Range;


   function All_Steps (Within : Graph_T) return Step_List_T
   is
   begin

      return To_Vector (Within.Steps);

   end All_Steps;


   function Step_At (
      Tag    : Step_Tag_T;
      Within : Graph_T)
   return Step_T
   is
   begin

      return Step_Bags.Search (
         Key    => Tag,
         Within => Within.Steps_By_Tag);

   exception

   when Step_Bags.Nonexistent_Key =>

      raise Step_Tag_Unknown;

   end Step_At;


   function Step_At (
      Index  : Step_Index_T;
      Within : Graph_T)
   return Step_T
   is
   begin

      return Element (Within.Steps, Positive (Index));

   end Step_At;


   function Steps_Containing (
      Address   : Processor.Code_Address_T;
      Within    : Graph_T;
      Calls_Too : Boolean)
   return Step_List_T
   is
      use type Processor.Code_Address_T;

      Steps : Step_List_T (1 .. Length (Within.Steps));
      Num   : Natural := 0;
      -- The result will be Steps(1 .. Num).

      Step : Step_T;
      -- One of the steps.

   begin

      -- Brute-force implementation by full traversal of all
      -- the steps in the graph. TBM/TBC to hashed access.

      for S in First (Within.Steps) .. Last (Within.Steps) loop

         Step := Element (Within.Steps, S);

         if Prime_Address (Step) = Address
         and then (Calls_Too or else not Calls.Is_Call (Step))
         then
            -- This is a match.

            Num        := Num + 1;
            Steps(Num) := Step;

         end if;

      end loop;

      return Steps(1 .. Num);

   end Steps_Containing;


   function Entry_Step (Graph : in Graph_T) return Step_T
   is
   begin

      return Step_At (Index => 1, Within => Graph);

   end Entry_Step;


   function Return_Steps (Graph : Graph_T) return Step_List_T
   is

      Step : Step_T;
      -- A candidate step.

      Ret  : Step_List_T (1 .. Natural (Max_Step (Graph)));
      Last : Natural := 0;
      -- The return steps are collected into Ret (1..Last).

   begin

      for S in 1 .. Max_Step (Graph) loop

         Step := Step_At (Index => S, Within => Graph);

         if Number_From (Step => Step, Within => Graph) = 0 then
            -- This is a return step.

            Last := Last + 1;
            Ret(Last) := Step;

         end if;

      end loop;

      return Ret(1..Last);

   end Return_Steps;


   function Is_Member (Step : Step_T; Of_List : Step_List_T)
   return Boolean
   is
   begin

      for L in Of_List'Range loop

         if Of_List(L) = Step then

            return True;

         end if;

      end loop;

      return False;

   end Is_Member;


   procedure Erase (List : in out Bounded_Step_List_T)
   is
   begin

      List.Last := 0;

   end Erase;


   procedure Add (
      Step : in     Step_T;
      To   : in out Bounded_Step_List_T)
   is
   begin

      if not Is_Member (Step, To.List(1 .. To.Last)) then

         To.Last := To.Last + 1;

         To.List(To.Last) := Step;

      end if;

   end Add;


   procedure Add (
      Steps : in     Step_List_T;
      To    : in out Bounded_Step_List_T)
   is
   begin

      for S in Steps'Range loop

         Add (Steps(S), To);

      end loop;

   end Add;


   function To_List (Item : Bounded_Step_List_T) return Step_List_T
   is
   begin

     return Item.List(1 .. Item.Last);

   end To_List;


   procedure Set_Info (
      Step : in Step_T;
      Info : in Processor.Step_Info_T)
   is
   begin

      Step.Info := Info;

   end Set_Info;


   function Info (Step : Step_T) return Processor.Step_Info_T
   is
   begin

      return Step.Info;

   end Info;


   function Effort (Step : Step_T) return Processor.Effort_T
   is
   begin

      return Processor.Effort (Step.Info);

   end Effort;


   function Some_Dynamic_Edges (Within : Graph_T) return Boolean
   is
   begin

      return Length (Within.Dynamic_Edges) > 0;

   end Some_Dynamic_Edges;


   function Total_Number_Of_Steps
   return Step_Count_T
   is
   begin

      return Total_Step_Count;

   end Total_Number_Of_Steps;


   function Size_Licence_Valid return Boolean
   is
   begin

      return Valid_Size (Measure => Total_Number_Of_Steps);

   end Size_Licence_Valid;


   --
   ---   Step edges (edges between steps)
   --


   function Index (Edge : in Step_Edge_T) return Step_Edge_Index_T
   is
   begin

      return Edge.Index;

   end Index;


   function Source (Edge : in Step_Edge_T) return Step_T
   is
   begin
      return Edge.Source;
   end Source;


   function Target (Edge : in Step_Edge_T) return Step_T
   is
   begin

      return Edge.Target;

   end Target;



   function Condition (Edge : in Step_Edge_T)
   return Arithmetic.Condition_T
   is
   begin
      return Edge.Cond;
   end Condition;


   function Is_Feasible (Edge : Step_Edge_T)
   return Boolean
   is
      use type Arithmetic.Expr_Ref;
   begin

      return Edge.Cond /= Arithmetic.Never;

   end Is_Feasible;


   procedure Set_Condition (
      On : in Step_Edge_T;
      To : in Arithmetic.Condition_T)
   is
   begin

      On.Cond := To;

   end Set_Condition;


   function Time (Along : in Step_Edge_T) return Processor.Time_T
   is
   begin

      return Along.Time;

   end Time;


   procedure Set_Time (
      Along : in Step_Edge_T;
      To    : in Processor.Time_T)
   is
   begin

      Along.Time := To;

   end Set_Time;


   function No_Step_Edges return Step_Edge_List_T
   is
   begin

      return (1 .. 0 => No_Step_Edge);

   end No_Step_Edges;


   function Sources (
      Edges  : Step_Edge_List_T;
      Unique : Boolean := False)
   return Step_List_T
   is

      Result : Step_List_T (Edges'Range);
      Last   : Natural := Result'First - 1;
      -- The result is Result(Result'First .. Last).

      Step : Step_T;
      -- The source of an edge.

      Novel : Boolean;
      -- Whether the Step is new and unique enough to be
      -- included in the Result.

   begin

      for E in Edges'Range loop

         Step := Source (Edges(E));

         Novel := True;

         if Unique then
            -- The Step is included only if it is a new one
            -- that is not already in the Result.

            for R in Result'First .. Last loop

               if Result(R) = Step then

                  Novel := False;

                  exit;

               end if;

            end loop;

         end if;

         if Novel then

            Last := Last + 1;

            Result(Last) := Step;

         end if;

      end loop;

      return Result(Result'First .. Last);

   end Sources;


   function Targets (Edges : Step_Edge_List_T) return Step_List_T
   is
      T : Step_List_T (Edges'Range);
   begin

      for I in T'Range loop

         T(I) := Target (Edges(I));

      end loop;

      return T;

   end Targets;


   function Edge_At (
      Index  : Step_Edge_Index_T;
      Within : Graph_T)
   return Step_Edge_T
   is
   begin

      return Element (Within.Step_Edges, Positive (Index));

   end Edge_At;


   function Edges_Into (Step : Step_T; Within : Graph_T)
   return Step_Edge_List_T
   is
   begin

      return To_Vector (Step.Into);

   end Edges_Into;


   function Only_Edge_Into (Step : Step_T) return Step_Edge_T
   is
   begin

      if Length (Step.Into) /= 1 then

         Output.Fault (
            Location => "Flow.Only_Edge_Into",
            Text =>
                 "Step #"
               & Step_Index_T'Image (Step.Index)
               & " has"
               & Natural'Image (Length (Step.Into))
               & " incoming edges.");

      end if;

      return Element (Step.Into, First (Step.Into));

   end Only_Edge_Into;


   function Edges_From (Step : Step_T; Within : Graph_T)
   return Step_Edge_List_T
   is
   begin

      return To_Vector (Step.From);

   end Edges_From;


   function Only_Edge_From (Step : Step_T) return Step_Edge_T
   is
   begin

      if Length (Step.From) /= 1 then

         Output.Fault (
            Location => "Flow.Only_Edge_From",
            Text =>
                 "Step #"
               & Step_Index_T'Image (Step.Index)
               & " has"
               & Natural'Image (Length (Step.From))
               & " leaving edges.");

      end if;

      return Element (Step.From, First (Step.From));

   end Only_Edge_From;


   function Number_Into (Step : Step_T; Within : Graph_T) return Natural
   is
   begin
      return Length (Step.Into);
   end Number_Into;


   function Number_From (Step : Step_T; Within : Graph_T) return Natural
   is
   begin
      return Length (Step.From);
   end Number_From;


   function Predecessors (Step : Step_T; Within : Graph_T)
   return Step_List_T
   is
      Pred : Step_List_T (1 .. Length (Step.Into));
      -- The result.
   begin

      for I in Pred'Range loop
         Pred(I) := Element (Step.Into, I) . Source;
      end loop;

      return Pred;

   end Predecessors;


   function Feasible_Predecessors (Step : Step_T; Within : Graph_T)
   return Step_List_T
   is
      use type Arithmetic.Expr_Ref;

      Pred : Step_List_T (1 .. Length (Step.Into));
      Last : Natural := 0;
      -- The result is Pred(1 .. Last).

      Edge : Step_Edge_T;
      -- One of the edges into the Step.

   begin

      for I in Pred'Range loop

         Edge := Element (Step.Into, I);

         if Edge.Cond /= Arithmetic.Never then

            Last := Last + 1;

            Pred(Last) := Edge.Source;

         end if;

      end loop;

      return Pred(1 .. Last);

   end Feasible_Predecessors;


   function Successors (Step : Step_T; Within : Graph_T)
   return Step_List_T
   is
      Succ : Step_List_T (1 .. Length (Step.From));
      -- The result.
   begin

      for I in Succ'Range loop
         Succ(I) := Element (Step.From, I) . Target;
      end loop;

      return Succ;

   end Successors;


   --
   ---   Nodes (basic blocks)
   --


   function Index (Node : in Node_T) return Node_Index_T
   is
   begin

      return Node.Index;

   end Index;


   function Tag (Node : Node_T) return Step_Tag_T
   is
   begin

      return Node.Steps (Node.Steps'First).Tag;

   end Tag;


   function Steps_In (Node : Node_T) return Step_List_T
   is
   begin

      return Node.Steps;

   end Steps_In;


   function First_Step (Node : Node_T) return Step_T
   is
   begin

      return Node.Steps(Node.Steps'First);

   end First_Step;


   function Prime_Address (Node : Node_T)
   return Processor.Code_Address_T
   is
   begin

      return Prime_Address (First_Step (Node));

   end Prime_Address;


   function Node_Containing (
      Step  : Step_T;
      Graph : Graph_T)
   return Node_T
   is
   begin
      return Graph.Node_Of_Step (Step.Index);
   end Node_Containing;


   function Node_At (
      Index  : Node_Index_T;
      Within : Graph_T)
   return Node_T
   is
   begin

      return Element (Within.Nodes, Positive (Index));

   end Node_At;


   function Entry_Node (Graph : in Graph_T) return Node_T
   is
   begin

      return Element (Graph.Nodes, 1);

   end Entry_Node;


   function All_Nodes (Within : Graph_T) return Node_List_T
   is
   begin

      return To_Vector (Within.Nodes);

   end All_Nodes;


   function Nodes_Containing (
      Steps : Step_List_T;
      Graph : Graph_T)
   return Node_List_T
   is

      Nodes : Node_List_T (Steps'Range);
      -- The result.

   begin

      for S in Steps'Range loop

         Nodes(S) := Node_Containing (Steps(S), Graph);

      end loop;

      return Nodes;

   end Nodes_Containing;


   function Return_Nodes (Within : Graph_T)
   return Node_List_T
   is

      Store : Node_List_T (1 .. Natural(Max_Node (Within)));
      Num   : Natural := 0;
      -- Store (1..Num) accumulates the nodes to be returned.

      Node  : Node_T;
      -- A node being considered.

   begin

      -- Trivial implementation by scanning all the nodes
      -- and collecting the ones with no successors.

      for I in 1 .. Max_Node (Within) loop

         Node := Node_At (Index => I, Within => Within);

         if Number_From (Node => Node, Within => Within) = 0 then
            -- This is a return Node.

            Num := Num + 1;
            Store(Num) := Node;

         end if;

      end loop;

      return Store(1..Num);

   end Return_Nodes;


   --
   ---   Node sets
   --


   procedure Equality_Bug
   is
   begin

      null;   -- Just to break on, within "=" below.

   end Equality_Bug;


   function "=" (Left, Right : Node_Set_T) return Boolean
   is

      Equal : Boolean := True;
      -- We have no evidence to the contrary, my Lord.

   begin

      if Left'Length /= Right'Length then

         Equal := False;

      else

         for I in Left'Range loop

            if Left(I) /= Right(I - Left'First + Right'First) then

               Equal := False;

               exit;

            end if;

         end loop;

      end if;

      -- if Equal and then not Predefined_Equal (Left, Right)
      -- then
         -- Ha ha ha... bug!

      --    Equality_Bug;

      -- end if;

      return Equal;

   end "=";


   function No_Nodes (Within : Graph_T) return Node_Set_T
   is
   begin

      return (1 .. Max_Node (Within) => False);

   end No_Nodes;


   function No_Nodes (Within : Graph_T) return Node_Set_Ref
   is
   begin

      return new Node_Set_T'(1 .. Max_Node (Within) => False);

   end No_Nodes;


   function "<=" (Left, Right : Node_Set_T) return Boolean
   is
   begin
      return (Left and Right) = Left;
   end "<=";


   function Cardinality (Set : Node_Set_T) return Node_Count_T
   is

      Count : Node_Count_T := 0;
      -- The count of members.

   begin

      for Nix in Set'Range loop

         if Set(Nix) then

            Count := Count + 1;

         end if;

      end loop;

      return Count;

   end Cardinality;


   function To_List (Set : Node_Set_T; From : Graph_T)
   return Node_List_T
   is
      List : Node_List_T (1 .. Set'Length);
      Last : Natural := 0;
      -- Accumulate the nodes in List(1 .. Last).

   begin

      for Nix in Set'Range loop
         if Set(Nix) then
            Last := Last + 1;
            List(Last) := Node_At (Nix, From);
         end if;
      end loop;

      return List(1 .. Last);

   end To_List;


   function To_Index_List (Set : Node_Set_T)
   return Node_Index_List_T
   is
      List : Node_Index_List_T (1 .. Set'Length);
      Last : Natural := 0;
      -- Accumulate the indices in List(1 .. Last).

   begin

      for Nix in Set'Range loop
         if Set(Nix) then
            Last := Last + 1;
            List(Last) := Nix;
         end if;
      end loop;

      return List(1 .. Last);

   end To_Index_List;


   function To_Set (
      Nodes : Node_Index_List_T;
      Last  : Node_Index_T)
   return Node_Set_T
   is
      Set : Node_Set_T := (1 .. Last => False);
      -- The result, initially empty.
   begin

      for N in Nodes'Range loop
         Set(Nodes(N)) := True;
      end loop;

      return Set;

   end To_Set;


   --
   ---   Node edges (edges between nodes)
   --


   function Index (Edge : in Edge_T) return Edge_Index_T
   is
   begin
      return Edge.Index;
   end Index;


   function Source (Edge : in Edge_T) return Node_T
   is
   begin
      return Edge.Source;
   end Source;


   function Target (Edge : in Edge_T) return Node_T
   is
   begin
      return Edge.Target;
   end Target;


   function Step_Edge (Edge : in Edge_T) return Step_Edge_T
   is
   begin
      return Edge.Step_Edge;
   end Step_Edge;



   function Node_Edge (Edge : in Step_Edge_T; Graph : Graph_T) return Edge_T
   is

      All_Edges : Edge_List_T := To_Vector (Graph.Edges);

   begin

      for E in All_Edges'Range loop

         if Step_Edge (All_Edges (E)) = Edge then

            return All_Edges (E);

         end if;

      end loop;

      -- Not found, return the null edge.

      return Null_Edge;

   end Node_Edge;



   function Condition (Edge : in Edge_T) return Arithmetic.Condition_T
   is
   begin

      return Edge.Step_Edge.Cond;

   end Condition;


   function Time (Along : Edge_T) return Processor.Time_T
   is
   begin

      return Along.Step_Edge.Time;

   end Time;


   function No_Edges return Edge_List_T
   is
   begin

      return (1 .. 0 => null);

   end No_Edges;


   function Sources (Edges : in Edge_List_T) return Node_List_T
   is
      S : Node_List_T (Edges'Range);
   begin
      for I in S'Range loop
         S(I) := Source (Edges(I));
      end loop;
      return S;
   end Sources;


   function Targets (Edges : in Edge_List_T) return Node_List_T
   is
      T : Node_List_T (Edges'Range);
   begin
      for I in T'Range loop
         T(I) := Target (Edges(I));
      end loop;
      return T;
   end Targets;


   function Edge_At (
      Index  : Edge_Index_T;
      Within : Graph_T)
   return Edge_T
   is
   begin
      return Element (Within.Edges, Positive (Index));
   end Edge_At;


   function Step_Edges (Edges : in Edge_List_T) return Step_Edge_List_T
   is

      Result : Step_Edge_List_T (Edges'Range);
      -- The step-edge for each given node-edge.

   begin

      for E in Edges'Range loop

         Result(E) := Edges(E).Step_Edge;

      end loop;

      return Result;

   end Step_Edges;


   function Edges_Into (Node : Node_T; Within : Graph_T)
   return Edge_List_T
   is
   begin
      return To_Vector (Node.Into);
   end Edges_Into;


   function Edges_From (Node : Node_T; Within : Graph_T)
   return Edge_List_T
   is
   begin
      return To_Vector (Node.From);
   end Edges_From;


   function Number_Into (Node : Node_T; Within : Graph_T) return Natural
   is
   begin
      return Length (Node.Into);
   end Number_Into;


   function Number_From (Node : Node_T; Within : Graph_T) return Natural
   is
   begin
      return Length (Node.From);
   end Number_From;


   function Predecessors (Node : Node_T; Within : Graph_T)
   return Node_List_T
   is
   begin
      return Sources (Edges_Into (Node, Within));
   end Predecessors;


   function Only_Predecessor (Node : Node_T; Within : Graph_T)
   return Flow.Node_T
   is

      Preds : constant Node_List_T := Predecessors (Node, Within);
      -- All the precessors.

   begin

      if Preds'Length /= 1 then

         Output.Fault (
            Location => "Flow.Only_Predecessor (Node)",
            Text     =>
                 "Node"
               & Node_Index_T'Image (Index (Node))
               & " has"
               & Natural'Image (Preds'Length)
               & " predecessor nodes.");

      end if;

      return Preds(Preds'First);

   end Only_Predecessor;


   function Successors (Node : Node_T; Within : Graph_T)
   return Node_List_T
   is
   begin
      return Targets (Edges_From (Node, Within));
   end Successors;


   function Only_Successor (Node : Node_T; Within : Graph_T)
   return Flow.Node_T
   is

      Succs : constant Node_List_T := Successors (Node, Within);
      -- All the successors.

   begin

      if Succs'Length /= 1 then

         Output.Fault (
            Location => "Flow.Only_Successor (Node)",
            Text     =>
                 "Node"
               & Node_Index_T'Image (Index (Node))
               & " has"
               & Natural'Image (Succs'Length)
               & " successor nodes.");

      end if;

      return Succs(Succs'First);

   end Only_Successor;


   function Return_Edges (Within : Graph_T)
   return Edge_List_T
   is

      Edges : Edge_List_T (1 .. Natural (Max_Edge (Within)));
      Last  : Natural := 0;
      -- The edges to be returned are collected in Edges (1 .. Last).

      Node : Node_T;
      -- A node under examination.

      Into : Natural;
      -- Number of edges into Node.

   begin

      for N in 1 .. Max_Node (Within) loop

         Node := Node_At (N, Within);

         Into := Number_Into (Node, Within);

         if  Number_From (Node, Within) = 0
         and Into > 0
         then
            -- This is a return node with some edges into it.
            -- Add these edges to the result:

            Edges(Last + 1 .. Last + Into) :=
               Edges_Into (Node, Within);

            Last := Last + Into;

         end if;

      end loop;

      return Edges(1 .. Last);

   end Return_Edges;


   function Edges (
      From     : Node_Set_T;
      Into     : Node_Set_T;
      Not_From : Boolean := False;
      Not_Into : Boolean := False;
      Within   : Graph_T)
   return Edge_List_T
   is

      Edges : Edge_List_T (1 .. Natural (Max_Edge (Within)));
      Last  : Natural := 0;
      -- The edges to be returned are collected in Edges (1 .. Last).

      Edge : Edge_T;
      -- An edge leaving Node.

   begin

      for E in 1 .. Length (Within.Edges) loop

         Edge := Element (Within.Edges, E);

         if  From(Edge.Source.Index) /= Not_From
         and Into(Edge.Target.Index) /= Not_Into
         then
            -- This edge goes from (not) From into (not) Into.

            Last        := Last + 1;
            Edges(Last) := Edge;

         end if;

      end loop;

      return Edges(1 .. Last);

   end Edges;


   --
   ---   Loose edges between steps
   --


   function Loose_Edges (Item : Graph_T) return Boolean
   is
   begin

      return not Loose_Edge_Bags.Empty (Item.Loose_Edges);

   end Loose_Edges;


   function Image (Item : Loose_Edge_T) return String
   --
   -- A description of the loose edge, for tracing purposes.
   --
   is
   begin

      return
           "Loose edge from "
         & Image (Item.Source)
         & " -> "
         & Image (Item.Target)
         & " when "
         & Arithmetic.Image (Item.Cond);

   end Image;


   function First_Loose_Edge (From : Graph_T)
   return Loose_Edge_T
   is

      Giving : Loose_Edge_T;
      -- The result.

   begin

      Giving := Loose_Edge_Bags.Min (Of_Bag => From.Loose_Edges);

      if Opt.Trace_Construction then

         Trace ("Taking " & Image (Giving));

         if Opt.Trace_Data_States then

            Trace_Also_Data ("Target", Giving.Target.Data);

         end if;

      end if;

      return Giving;

   exception

   when Loose_Edge_Bags.Bag_Empty =>
      -- There are no loose edges.

      raise Constraint_Error;

   end First_Loose_Edge;


   procedure Bind_Loose_Edges (
      To     : in Step_T;
      Within : in Graph_T)
   --
   -- Finds the loose edges that point to To (which is a newly
   -- inserted step) and converts them into bound edges, removing
   -- them from the loose-edge set.
   --
   is

      Loose_Edge : Loose_Edge_T;
      -- A loose edge to be converted.

      Unused_New_Edge : Step_Edge_T;
      -- The new step-edge (not returned or used).

   begin

      while Loose_Edge_Bags.Member (
         Key    => To.Tag,
         Of_Bag => Within.Loose_Edges)
      loop
         -- There is at least one loose edge -> To.
         -- Convert it to a bound edge.

         Loose_Edge_Bags.Remove (
            Key          => To.Tag,
            From         => Within.Loose_Edges,
            Removed_Item => Loose_Edge);

         if Opt.Trace_Construction then

            Trace (
                 "Binding "
               & Image (Loose_Edge)
               & ", to "
               & Image (To));

            if Opt.Trace_Data_States then

               Trace_Also_Data ("Target", To.Tag.Data);

            end if;

         end if;

         Add_Edge (
            To     => Within,
            Source => Loose_Edge.Source,
            Cond   => Loose_Edge.Cond,
            Time   => Loose_Edge.Time,
            Target => To,
            Giving => Unused_New_Edge);

      end loop;

   end Bind_Loose_Edges;


   --
   ---   Boundable (dynamically computed) edges between steps
   --


   function Source (Edge : Boundable_Edge_T'Class) return Step_T
   is
   begin

      return Edge.Source;

   end Source;


   function Role (Edge : Boundable_Edge_T'Class) return Boundable_Edge_Role_T
   is
   begin

      return Edge.Role;

   end Role;


   function State (Edge : Boundable_Edge_T'Class)
   return Edge_Resolution_T
   is
   begin

      return Edge.State;

   end State;


   function Maybe_Not (Item : Boolean) return String
   is
   begin

      if Item then return "";
              else return "not ";
      end if;

   end Maybe_Not;


   function Full_Image (Item : Boundable_Edge_T'Class)
   return String
   is
   begin

      return Image (Item)
           & " ("
           & Image (Source (Item))
           & ", "
           & Boundable_Edge_Role_T'Image (Role (Item))
           & ", "
           & Edge_Resolution_T'Image (State (Item))
           & ","
           & Natural'Image (Resolvents (Item))
           & " resolvents, "
           & Maybe_Not (Returns (Item))    & "return, "
           & Maybe_Not (Item.Domain_Grown) & "domain grown, "
           & Maybe_Not (Item.Unstable)     & "unstable)";

   end Full_Image;


   procedure Add_Resolvent (To : in out Boundable_Edge_T'Class)
   is
   begin

      if To.Returns then

         Output.Fault (
            Location => "Flow.Add_Resolvent",
            Text     => "The boundable edge returns.");

      end if;

      To.Resolvents := To.Resolvents + 1;
      To.State      := Growing;
      To.Unstable   := False;

   end Add_Resolvent;


   procedure Resolve_To_Return (
      From : in     Graph_T;
      Edge : in out Boundable_Edge_T'Class)
   is
   begin

      if Opt.Trace_Flow_Resolution then

         Output.Trace (
              "Resolving "
            & Image (Edge)
            & " to return.");

      end if;

      if Edge.Resolvents > 0 then

         Output.Fault (
            Location => "Flow.Resolve_To_Return",
            Text     => "The boundable edge jumps.");

      end if;

      Edge.Returns  := True;
      Edge.State    := Stable;
      Edge.Unstable := False;

      Calls.Return_After (Step => Source (Edge), Within => From);

   end Resolve_To_Return;


   function Resolvents  (Edge : Boundable_Edge_T'Class)
   return Natural
   is
   begin

      return Edge.Resolvents;

   end Resolvents;


   function Returns (Edge : Boundable_Edge_T'Class)
   return Boolean
   is
   begin

      return Edge.Returns;

   end Returns;


   package Step_Queues is new Bounded_Queues (Step_T);
   --
   -- Bounded queues of steps, for the work-list algorithms.


   procedure Mark_Grown_Domains (Within : in Graph_T)
   --
   -- Finds the boundable edges from "old" steps that may be reached
   -- from the Rejoin_Steps and marks them as Domain_Grown.
   -- Clears Rejoin_Steps and updates Max_Old_Step to prepare for
   -- collecting new "rejoin" information.
   --
   is
      use Step_Queues;

      Rejoin : Queue_Type (Max_Length => Natural (Within.Max_Old_Step));
      -- The work-list of "old" steps that may receive new data states from
      -- the "new" steps added in the last round of dynamic-edge analysis.

      Rejoined : array (Step_Index_T range 1 .. Within.Max_Old_Step)
                 of Boolean := (others => False);
      -- The set of "old" steps that are reachable from Rejoin_Steps,
      -- that is, the "old" steps known to receive new flow from the
      -- "new" parts of the flow-graph. Set members are marked by
      -- True values.


      procedure Visit (Step : in Step_T)
      --
      -- Visits this Step, which receives new flow, and continues
      -- to its successors, unless they are already visited.
      --
      is

         Afters : constant Step_List_T := Successors (Step, Within);
         -- The successor steps.

         Aft : Step_T;
         -- One of the Afters.

      begin

         Rejoined(Index (Step)) := True;

         for A in Afters'Range loop

            Aft := Afters(A);

            if Index (Aft) <= Rejoined'Last
            and then not Rejoined(Index (Aft))
            then
               -- This Aft is an "old" step that receives new flow
               -- but has not yet been visited.

               Put (Element => Aft, Into => Rejoin);

            end if;

         end loop;

      end Visit;


      Work : Step_T;
      -- One of the steps in the work-list.

      Edge : Dynamic_Edge_T;
      -- One of the dynamic edges in the graph.

      Source : Step_Index_T;
      -- The index of the source of the Edge.


   begin  -- Mark_Grown_Domains

      -- Initialize the work-list to Within.Rejoin_Steps:

      if Opt.Trace_Flow_Resolution then

         Output.Trace (
              "New flow rejoins"
            & Natural'Image (Length (Within.Rejoin_Steps))
            & " old steps.");

      end if;

      for R in First (Within.Rejoin_Steps)
            .. Last  (Within.Rejoin_Steps)
      loop

         Work := Element (Within.Rejoin_Steps, R);

         if Opt.Trace_Flow_Resolution then

            Output.Trace (
                 "New flow rejoins old step"
               & Output.Field_Separator
               & Image (Work));

         end if;

         Put (Element => Work, Into => Rejoin);

      end loop;

      -- Visit all the steps reachable from Rejoin_Steps:

      while Length (Rejoin) > 0 loop

         Get (From => Rejoin, Element => Work);

         Visit (Step => Work);

      end loop;

      -- Mark Domain Grown for all boundable edges that
      -- originate from Rejoined (visited) steps:

      for E in First (Within.Dynamic_Edges)
            .. Last  (Within.Dynamic_Edges)
      loop

         Edge := Element (Within.Dynamic_Edges, E);

         Source := Index (Flow.Source (Edge.all));

         if Source <= Rejoined'Last and then Rejoined(Source) then
            -- This Edge can be reached from the Rejoined_Steps
            -- and thus its domain has grown.

            Edge.Domain_Grown := True;

         end if;

      end loop;

      -- Start collecting new "rejoin" information:

      Within.Max_Old_Step := Max_Step (Within);

      Erase (Within.Rejoin_Steps);

   end Mark_Grown_Domains;


   procedure Find_Unstable_Dynamic_Edges (Within : in Graph_T)
   is

      Edge : Dynamic_Edge_T;
      -- One of the dynamic edges.

   begin

      Mark_Grown_Domains (Within);

      -- Check the boundable edges to see which of them should
      -- be (re-) analysed:

      for N in First (Within.Dynamic_Edges)
            .. Last  (Within.Dynamic_Edges)
      loop

         Edge := Element (Within.Dynamic_Edges, N);

         if Edge.Domain_Grown then
            -- The domain of this Edge has grown since the Edge
            -- was last analysed, so we must re-analyse the Edge.

            if Opt.Trace_Flow_Resolution then

               Output.Trace (
                    "Domain has grown"
                  & Output.Field_Separator
                  & Full_Image (Edge.all));

            end if;

            Edge.State := Unresolved;

         else
            -- The domain of this Edge is not affected by the new
            -- edges added (if any) since the last analysis of
            -- the Edge. The Edge does not need new analysis now,
            -- but may do so in the future if the domain grows
            -- when the flow-graph continues to be extended.

            if Opt.Trace_Flow_Resolution then

               Output.Trace (
                    "Domain unchanged"
                  & Output.Field_Separator
                  & Full_Image (Edge.all));

            end if;

            if Edge.State = Growing then
               -- Looks stable now.

               Edge.State := Stable;

            end if;

         end if;

         Edge.Unstable := Edge.Domain_Grown;
         -- These edges shall be analysed in the next (this) round
         -- of boundable-edge analysis, unless they are infeasible.

      end loop;

   end Find_Unstable_Dynamic_Edges;


   procedure Mark_Domain (Edge : in out Boundable_Edge_T'Class)
   is
   begin

      if Opt.Trace_Flow_Resolution then

         Output.Trace (
              "Mark_Domain"
            & Output.Field_Separator
            & Full_Image (Edge));

      end if;

      Edge.Domain_Grown := False;

   end Mark_Domain;


   procedure Mark_Stable (Edge : in out Boundable_Edge_T'Class)
   is
   begin

      if Opt.Trace_Flow_Resolution then

         Output.Trace (
              "Mark_Stable"
            & Output.Field_Separator
            & Full_Image (Edge));

      end if;

      Edge.State    := Stable;
      Edge.Unstable := False;

   end Mark_Stable;


   procedure Apply (
      Transfer : in     Arithmetic.Transfer_Function_T'Class;
      Upon     : in out Boundable_Edge_T;
      Graph    : in     Graph_T)
   is
   begin

      Output.Note ("Ignoring Apply (Transfer_Function_T) to " & Image (Upon));

   end Apply;


   procedure Take_Asserted_Targets (
      Edge    : in out Boundable_Edge_T;
      Targets : in     Target_List_T;
      Graph   : in     Graph_T)
   is
   begin

      for T in Targets'Range loop

         Take_Asserted_Target (
            Edge   => Boundable_Edge_T'Class (Edge),
            Target => Targets(T),
            Graph  => Graph);

      end loop;

   end Take_Asserted_Targets;


   procedure Take_Asserted_Target (
      Edge   : in out Boundable_Edge_T;
      Target : in     Processor.Code_Address_T;
      Graph  : in     Graph_T)
   is
   begin

      Output.Fault (
         Location => "Flow.Take_Asserted_Target",
         Text     =>
              "Not implemented for "
            & Image (Boundable_Edge_T'Class (Edge)));

      Output.Warning (
           "Ignoring asserted target "
         & Processor.Image (Target)
         & " for "
         & Image (Boundable_Edge_T'Class (Edge)));

   end Take_Asserted_Target;


   function Resolution (Edge : Boundable_Edge_T'Class)
   return String
   --
   -- To what the Edge is resolved: to a return or to some edges.
   --
   is
   begin

      if Returns (Edge) then

         return " return.";

      elsif Edge.Role = Boundable_Call then

         return Natural'Image (Edge.Resolvents)
              & " callees.";

      else

         return Natural'Image (Edge.Resolvents)
              & " edges.";

      end if;

   end Resolution;


   procedure Close (
      Edge  : in out Boundable_Edge_T;
      Graph : in     Graph_T)
   is
   begin

      if Opt.Trace_Flow_Resolution then

         Output.Trace (
              "Closing "
            & Image (Boundable_Edge_T'Class (Edge))
            & " from "
            & Processor.Image (Prime_Address (Source (Edge)))
            & " after resolving to"
            & Resolution (Edge));

      end if;

   end Close;


   function Dynamic_Edges_From (
      Source : Step_T;
      Within : Graph_T)
   return Dynamic_Edge_List_T
   is

      All_Edges : constant Dynamic_Edge_List_T :=
         To_Vector (Within.Dynamic_Edges);
      -- All the dynamic edges.

      From_Edges : Dynamic_Edge_List_T (1 .. All_Edges'Length);
      Last : Natural := 0;
      -- The result will be From_Edges(1 .. Last).

   begin

      for A in All_Edges'Range loop

         if All_Edges(A).Source = Source then

            Last := Last + 1;

            From_Edges(Last) := All_Edges(A);

         end if;

      end loop;

      return From_Edges(1 .. Last);

   end Dynamic_Edges_From;


   function Dynamic_Edges (Item : Graph_T)
   return Dynamic_Edge_List_T
   is
   begin

      return To_Vector (Item.Dynamic_Edges);

   end Dynamic_Edges;


   function Sources (Edges : in Dynamic_Edge_List_T)
   return Step_List_T
   is

      Steps : Step_List_T (Edges'Range);
      -- The source steps.

   begin

      for I in Steps'Range loop

         Steps(I) := Edges(I).Source;

      end loop;

      return Steps;

   end Sources;


   procedure Add_Basis_Cells (
      From : in     Dynamic_Edge_List_T;
      To   : in out Storage.Cell_Set_T)
   is
   begin

      for F in From'Range loop

         Add_Basis_Cells (From => From(F).all, To => To);

      end loop;

   end Add_Basis_Cells;


   --
   ---   Step_Data_T primitive operations
   --


   -- First, the low-level operations:


   Not_Overridden : constant String := "Null implementation not overridden.";
   --
   -- For some Fault messages below.


   function Refined_Effect (
      From :        Arithmetic.Effect_Ref;
      On   : access Step_Data_T)
   return Arithmetic.Effect_Ref
   is
   begin

      Output.Fault (
         Location => "Flow.Refined_Effect (Step_Data_T)",
         Text     => Not_Overridden);

      return From;

   end Refined_Effect;


   function Refined_Effect (
      From :        Arithmetic.Effect_T;
      On   : access Step_Data_T)
   return Arithmetic.Effect_Ref
   is
   begin

      return Refined_Effect (
         From => Arithmetic.To_Effect_Ref (From),
         On   => Step_Data_Ref (On));

   end Refined_Effect;


   function Refined_Condition (
      From :        Arithmetic.Condition_T;
      On   : access Step_Data_T)
   return Arithmetic.Condition_T
   is
   begin

      Output.Fault (
         Location => "Flow.Refined_Condition (Step_Data_T)",
         Text     => Not_Overridden);

      return From;

   end Refined_Condition;


   function Transformed_Data (
      From  : access Step_Data_T;
      After :        Arithmetic.Effect_T)
   return Step_Data_Ref
   is
   begin

      Output.Fault (
         Location => "Flow.Transformed_Data (Step_Data_T)",
         Text     => Not_Overridden);

      return Step_Data_Ref (From);

   end Transformed_Data;


   function Constrained_Data (
      From : access Step_Data_T;
      By   :        Arithmetic.Condition_T)
   return Step_Data_Ref
   is
   begin

      Output.Fault (
         Location => "Flow.Constrained_Data (Step_Data_T)",
         Text     => Not_Overridden);

      return Step_Data_Ref (From);

   end Constrained_Data;


   -- Then the high-level operations:


   procedure Transform_New_Step (
      Data   : access Step_Data_T;
      Step   : in     Step_T;
      Effect : in out Arithmetic.Effect_Ref;
      Post   :    out Step_Data_Ref)
   is
   begin

      Effect := Refined_Effect (
         From => Effect,
         On   => Step_Data_Ref (Data));

      Post := Transformed_Data (
         From  => Step_Data_Ref (Data),
         After => Effect.all);

   end Transform_New_Step;


   procedure Transform_New_Step (
      Data    : access Step_Data_T;
      Step    : in     Step_T;
      Effect  : in     Arithmetic.Effect_T;
      Refined :    out Arithmetic.Effect_Ref;
      Post    :    out Step_Data_Ref)
   is
   begin

      Refined := Refined_Effect (
         From => Effect,
         On   => Step_Data_Ref (Data));

      Post := Transformed_Data (
         From  => Step_Data_Ref (Data),
         After => Refined.all);

   end Transform_New_Step;


   procedure Refine_New_Edge (
      Post   : access Step_Data_T;
      Source : in     Step_T;
      Target : in     Step_Tag_T;
      Cond   : in out Arithmetic.Condition_T;
      Giving :    out Step_Data_Ref)
   is
      use type Arithmetic.Condition_T;
   begin

      Cond := Refined_Condition (
         From => Cond,
         On   => Step_Data_Ref (Post));

      if Cond = Arithmetic.Never then
         -- An infeasible edge: no point in transforming the data.

         Giving := Step_Data_Ref (Post);

      else

         Giving := Constrained_Data (
            From => Step_Data_Ref (Post),
            By   => Cond);

      end if;

   end Refine_New_Edge;


   --
   ---   Step and graph construction
   --


   procedure Create_Step (
      Graph  : in     Graph_T;
      Tag    : in     Step_Tag_T;
      Info   : in     Processor.Step_Info_T;
      Giving :    out Step_T)
   --
   -- Creates a new step for the given Graph with the given
   -- (partial) attributes. The step object is created and is
   -- allocated a Step_Index but is not yet inserted in the Graph
   -- (see Insert below). The Effect and Post attributes are
   -- undefined (null by default).
   --
   is
   begin

      Giving := new Step_Object_T;

      Giving.Index  := Step_Index_T (Next (Graph.Steps));
      Giving.Tag    := Tag;
      Giving.Info   := Info;

      -- The Into and From edge-lists are empty by default.

      Total_Step_Count := Total_Step_Count + 1;

   end Create_Step;


   procedure Insert (
      Step : in Step_T;
      Into : in Graph_T)
   --
   -- Inserts a new Step Into a graph.
   --
   -- The Step must already be fully constructed, but its Into
   -- and From list should still be empty (as they are by default
   -- intialization).
   --
   is
   begin

      if Opt.Trace_Construction then

         Trace (
              "Adding "
            & Image (Step)
            & " with effect "
            & Arithmetic.Image (Step.Effect.all));

         if Opt.Trace_Data_States then

            Trace_Also_Data ("Data", Step.Tag.Data);

         end if;

      end if;

      -- Insert the new step in the graph:

      Step_Bags.Insert (
         Item => Step,
         Into => Into.Steps_By_Tag);

      Set (
         Vector => Into.Steps,
         Index  => Positive (Step.Index),
         To     => Step);

      -- Connect any loose edges that point at the new step:

      Bind_Loose_Edges (
         To     => Step,
         Within => Into);

      if Opt.Check_Consistency then

         Checks.After_Inserting_Step (Step, Into);

      end if;

   exception

   when Step_Bags.Duplicate_Key =>

      -- A step with this Tag already exists.

      raise Step_Tag_In_Use;

   end Insert;


   procedure Trace_Data (
      Place : in String;
      Data  : in Step_Data_Ref)
   --
   -- Trace the Data state at a Place in the program.
   --
   is
   begin

      if Data /= Any_Data then

         Output.Trace (
              "Data "
            & Place
            & Output.Field_Separator
            & Image (Data.all));

      else

         Output.Trace (
              "Data "
            & Place
            & Output.Field_Separator
            & "Not constrained.");

      end if;

   end Trace_Data;


   function Image_All (Item : Step_Data_Ref) return String
   is
   begin

      if Item = Any_Data then

         return "Not constrained.";

      else

         return Image (Item.all);

      end if;

   end Image_All;


   procedure Add_Step (
      To     : in     Graph_T;
      Tag    : in     Step_Tag_T;
      Effect : in     Arithmetic.Effect_T;
      Info   : in     Processor.Step_Info_T;
      Giving :    out Step_T)
   is
   begin

      -- Create the step with null Effect and Post data:

      Create_Step (
         Graph  => To,
         Tag    => Tag,
         Info   => Info,
         Giving => Giving);

      if Tag.Data = Any_Data then
         -- The data state is unconstrained, so ditto is the
         -- Post data. Moreover, we cannot refine the Effect.

         Giving.Effect := Arithmetic.To_Effect_Ref (Effect);
         Giving.Post   := Any_Data;

      else
         -- We can try to refine the Effect on Tag.Data and
         -- compute the Post data state.

         if Opt.Trace_Data_States then

            Trace_Data ("pre ", Tag.Data);

         end if;

         Transform_New_Step (
            Data    => Tag.Data,
            Step    => Giving,
            Effect  => Effect,
            Refined => Giving.Effect,
            Post    => Giving.Post);

         if Opt.Trace_Data_States then

            Trace_Data ("post", Giving.Post);

         end if;

      end if;

      -- Insert the step in the graph structure:

      Insert (Step => Giving, Into => To);

   end Add_Step;


   procedure Add_Step (
      To     : in     Graph_T;
      Tag    : in     Step_Tag_T;
      Effect : in     Arithmetic.Effect_Ref;
      Info   : in     Processor.Step_Info_T;
      Giving :    out Step_T)
   is
   begin

      -- Create the step with null Effect and Post data:

      Create_Step (
         Graph  => To,
         Tag    => Tag,
         Info   => Info,
         Giving => Giving);

      Giving.Effect := Effect;

      if Tag.Data = Any_Data then
         -- The data state is unconstrained, so ditto is the
         -- Post data. Moreover, we cannot refine the Effect.

         Giving.Post := Any_Data;

      else
         -- We can try to refine the Effect on Tag.Data and
         -- compute the Post data state.

         if Opt.Trace_Data_States then

            Trace_Data ("pre ", Tag.Data);

         end if;

         Transform_New_Step (
            Data   => Tag.Data,
            Step   => Giving,
            Effect => Giving.Effect,
            Post   => Giving.Post);

         if Opt.Trace_Data_States then

            Trace_Data ("post", Giving.Post);

         end if;

      end if;

      -- Insert the step in the graph structure:

      Insert (Step => Giving, Into => To);

   end Add_Step;


   procedure Add_Edge (
      To      : in     Graph_T;
      Source  : in     Step_T;
      Cond    : in     Arithmetic.Condition_T := Arithmetic.Always;
      Time    : in     Processor.Time_T;
      Target  : in     Step_T;
      Giving  :    out Step_Edge_T)
   is
      use type Arithmetic.Condition_T;
   begin

      if Cond = Arithmetic.Never then

         Output.Fault (
            Location => "Flow.Add_Edge (Step to Step)",
            Text     => "Edge is infeasible (false condition)");

      end if;

      Giving := new Step_Edge_Object_T'(
         Index  => Step_Edge_Index_T (Next (To.Step_Edges)),
         Source => Source,
         Cond   => Cond,
         Time   => Time,
         Target => Target);

      if Opt.Trace_Construction
      or Cond = Arithmetic.Never
      then

         Trace (
              "Adding edge"
            & Step_Edge_Index_T'Image (Giving.Index)
            & " from "
            & Image (Source)
            & " -> "
            & Image (Target)
            & " when "
            & Arithmetic.Image (Giving.Cond));

         if Opt.Trace_Data_States then

            Trace_Also_Data ("From", Source.Tag.Data);
            Trace_Also_Data ("To  ", Target.Tag.Data);

         end if;

      end if;

      Append (To => To.Step_Edges, Value => Giving);
      Append (To => Source.From  , Value => Giving);
      Append (To => Target.Into  , Value => Giving);

      -- Update the "rejoin" information:

      if Index (Target) <= To.Max_Old_Step then
         -- The target is an "old" step, so this edge brings
         -- new flow into the "old" part of the graph.

         Step_Vectors.Add (Value => Target, To => To.Rejoin_Steps);

      end if;

   end Add_Edge;


   procedure Add_Edge (
      To      : in Graph_T;
      Source  : in Step_T;
      Cond    : in Arithmetic.Condition_T := Arithmetic.Always;
      Time    : in Processor.Time_T;
      Target  : in Step_T)
   is
      use type Arithmetic.Condition_T;

      Unused_Edge : Step_Edge_T;
      -- The new edge, not wanted by the caller.

   begin

      if Cond /= Arithmetic.Never then

         Add_Edge (
            To     => To,
            Source => Source,
            Cond   => Cond,
            Time   => Time,
            Target => Target,
            Giving => Unused_Edge);

      end if;

   end Add_Edge;


   procedure Add_Edge (
      To     : in Graph_T;
      Source : in Step_T;
      Cond   : in Arithmetic.Condition_T := Arithmetic.Always;
      Time   : in Processor.Time_T;
      Target : in Step_Tag_T;
      Info   : in Processor.Loose_Edge_Info_T :=
                     Processor.No_Loose_Edge_Info)
   is
      use type Arithmetic.Condition_T;

      Source_Data : constant Step_Data_Ref := Source.Tag.Data;
      -- The data state in the Source step, possibly Any_Data.

      New_Target : Step_Tag_T;
      -- The Target, possibly with a new Data state that comes from
      -- transforming Source_Data with Source.Effect.

      New_Cond : Arithmetic.Condition_T;
      -- The Cond, possibly refined by the data-state for the target.

      Target_Step : Step_T;
      -- The step at tag = New_Target, if it exists.

      Unused_New_Edge : Step_Edge_T;
      -- The new edge Source->Target_Step (not returned to caller).

      Loose_Edge : Loose_Edge_T;
      -- The new loose edge Source->Target, if the target step does not
      -- yet exist.

   begin

      -- Find the New_Target and New_Cond:

      New_Target := Target;
      New_Cond   := Cond;
      -- Possibly constrained and refined below.

      if Target.Data = In_Transit_Solo_Ref then
         -- We must compute the Data for the target.

         if Source_Data = Any_Data then
            -- Insufficient Data. Does not compute. Bzzz beep.

            New_Target.Data := Any_Data;

         elsif Cond = Arithmetic.Always then
            -- Simple: nothing to refine or constrain.

            New_Target.Data := Source.Post;

         else
            -- Argh, we may have to do some real work.

            if Opt.Trace_Data_States then

               Trace_Data (
                  Place => "from " & Image (Source.Tag),
                  Data  => Source.Post);

            end if;

            Refine_New_Edge (
               Post   => Source.Post,
               Source => Source,
               Target => Target,
               Cond   => New_Cond,
               Giving => New_Target.Data);

            if Opt.Trace_Data_States then

               Trace_Data (
                  Place => "into " & Image (Target),
                  Data  => New_Target.Data);

            end if;

            if New_Cond /= Cond and Opt.Trace_Data_Refinement then

               Output.Trace (
                    "Initial condition"
                  & Output.Field_Separator
                  & Arithmetic.Image (Cond));

               Output.Trace (
                    "Refined condition"
                  & Output.Field_Separator
                  & Arithmetic.Image (New_Cond));

            end if;

         end if;

      elsif Source.Post /= Target.Data then
         -- Target.Data explicitly changed by caller.

         if Opt.Trace_Data_States then

            Trace_Data (
               Place => "from " & Image (Source.Tag),
               Data  => Source.Post);

            Trace_Data (
               Place => "goal " & Image (Target),
               Data  => Target.Data);

         end if;

      end if;

      if New_Cond /= Arithmetic.Never then
         -- The edge (still) seems feasible.

         -- Look for an existing target step:

         Target_Step := Step_Bags.Search (
            Key    => New_Target,
            Within => To.Steps_By_Tag);
         -- Raises Nonexistent_Key if the target step (tag) is a
         -- new one, not already in the graph.

         -- Target step already exists, add bound edge:

         Add_Edge (
            To     => To,
            Source => Source,
            Cond   => New_Cond,
            Time   => Time,
            Target => Target_Step,
            Giving => Unused_New_Edge);

      else

         Output.Note (
              "Omitted infeasible edge from "
            & Image (Source.Tag)
            & " to "
            & Image (New_Target));

      end if;

   exception

   when Step_Bags.Nonexistent_Key =>

      -- Target step not yet in graph, add loose edge:

      Loose_Edge := (
         Source => Source,
         Cond   => New_Cond,
         Time   => Time,
         Target => New_Target,
         Info   => Info);

      if Opt.Trace_Construction
      or New_Cond = Arithmetic.Never
      then

         Trace (
              "Adding "
            & Image (Loose_Edge));

         if Opt.Trace_Data_States then

            Trace_Also_Data ("To", Loose_Edge.Target.Data);

         end if;

      end if;

      Loose_Edge_Bags.Insert (
         Item => Loose_Edge,
         Into => To.Loose_Edges);

   end Add_Edge;


   procedure Add_Edge (
      To     : in Graph_T;
      Source : in Step_T;
      Cond   : in Arithmetic.Condition_T := Arithmetic.Always;
      Time   : in Processor.Time_T;
      Target : in Processor.Flow_State_T;
      Info   : in Processor.Loose_Edge_Info_T :=
                     Processor.No_Loose_Edge_Info)
   is
   begin

      Add_Edge (
         To     => To,
         Source => Source,
         Cond   => Cond,
         Time   => Time,
         Target => Transit (From => Source, To => Target),
         Info   => Info);

   end Add_Edge;


   procedure Add_Dynamic_Edge (
      To     : in Graph_T;
      Source : in Step_T;
      Edge   : in Dynamic_Edge_T;
      Warn   : in Warning_Opt_T := Always_Warn)
   is

      Should_Warn : Boolean;
      -- Whether to issue a warning. If other conditions allow.

   begin

      if Opt.Trace_Construction then

         Trace (
              "Adding dynamic edge from "
            & Image (Source)
            & Output.Field_Separator
            & Image (Edge.all));

      end if;

      Edge.Source := Source;
      -- From default initialization:
      --    State           => Unresolved,
      --    Resolvents      => 0,
      --    Returns         => False,
      --    Domain_Grown    => True,
      --    Unstable        => False (but irrelevant).

      if Edge.Role = Undefined then
         -- Not added from Flow.Calls.Add_Boundable_Call.

         Edge.Role := Boundable_Jump;

      end if;

      if Source.Tag.Data /= Any_Data then
         -- We can try to bound the edge right away.

         if Opt.Trace_Data_States
         or Opt.Trace_Data_Refinement
         then

            Trace_Data (
               Place =>
                    "source for dynamic edge ("
                  & Ada.Tags.Expanded_Name (Source.Tag.Data.all'Tag)
                  & ')',
               Data  => Source.Tag.Data);

         end if;

         Apply (
            Pre   => Source.Tag.Data,
            Post  => Source.Post,
            Upon  => Edge.all,
            Graph => To);

         -- The result will be seen in Edge.State.

      end if;

      case Edge.State is

      when Growing =>
         -- This should never happen.

         Output.Fault (
            Location => "Flow.Add_Dynamic_Edge",
            Text     => "Edge is Growing.");

         Should_Warn := True;

      when Unresolved =>
         -- We could not bound the edge using Source.Tag.Data,
         -- so we must keep the Edge open for later analysis.

         Append (
            To    => To.Dynamic_Edges,
            Value => Edge);

         Should_Warn := Warn /= Never_Warn;

      when Stable =>
         -- We could bound the edge using Source.Tag.Data.
         -- Since the Data are comprehensive for this step, we
         -- can close and discard the Edge.

         Close (Edge => Edge.all, Graph => To);

         -- TBA/TBC deallocation of Edge.

         Should_Warn := Warn = Always_Warn;

      end case;

      if (Should_Warn and Opt.Warn_Dynamic_Flow)
      and then Role (Edge.all) /= Boundable_Call
      then

         Output.Warning ("Dynamic control flow.");
         --
         -- For a boundable call, a Warning was already emitted
         -- in Flow.Calls.Add_Boundable_Call.

      end if;

   end Add_Dynamic_Edge;


   procedure Add_Resolved_Edge (
      To     : in     Graph_T;
      Source : in out Boundable_Edge_T'Class;
      Cond   : in     Arithmetic.Condition_T := Arithmetic.Always;
      Time   : in     Processor.Time_T;
      Target : in     Step_Tag_T;
      Info   : in     Processor.Loose_Edge_Info_T :=
                         Processor.No_Loose_Edge_Info)
   is
   begin

      if Opt.Trace_Flow_Resolution then

         Output.Trace (
              "Resolving "
            & Image (Source)
            & " to "
            & Image (Target)
            & " when "
            & Arithmetic.Image (Cond));

      end if;

      Add_Edge (
         To     => To,
         Source => Source.Source,
         Cond   => Cond,
         Time   => Time,
         Target => Target,
         Info   => Info);

      Add_Resolvent (To => Source);

   end Add_Resolved_Edge;


   procedure Add_Resolved_Edge (
      To     : in     Graph_T;
      Source : in out Boundable_Edge_T'Class;
      Cond   : in     Arithmetic.Condition_T := Arithmetic.Always;
      Time   : in     Processor.Time_T;
      Target : in     Step_T)
   is
   begin

      if Opt.Trace_Flow_Resolution then

         Output.Trace (
              "Resolving "
            & Image (Source)
            & " to "
            & Image (Target)
            & " when "
            & Arithmetic.Image (Cond));

      end if;

      Add_Edge (
         To     => To,
         Source => Source.Source,
         Cond   => Cond,
         Time   => Time,
         Target => Target);

      Add_Resolvent (To => Source);

   end Add_Resolved_Edge;


   procedure Add_Resolved_Dynamic_Edge (
      To     : in     Graph_T;
      Source : in out Boundable_Edge_T'Class;
      Edge   : in     Dynamic_Edge_T;
      Warn   : in     Warning_Opt_T := Always_Warn)
   is
   begin

      if Opt.Trace_Flow_Resolution then

         Output.Trace (
              "Resolving "
            & Image (Source)
            & " to "
            & Image (Edge.all)
            & '.');

      end if;

      Add_Dynamic_Edge (
         To     => To,
         Source => Source.Source,
         Edge   => Edge,
         Warn   => Warn);

      Add_Resolvent (To => Source);

   end Add_Resolved_Dynamic_Edge;


   procedure Reject_Resolved_Edge (
      Source : in Boundable_Edge_T'Class;
      Expr   : in Arithmetic.Expr_Ref;
      Value  : in Arithmetic.Word_T;
      Reason : in String)
   is
   begin

      if Opt.Trace_Flow_Resolution then

         Output.Trace (
              "Rejected edge from "
            & Image (Source)
            & " when "
            & Arithmetic.Image (Expr)
            & '='
            & Arithmetic.Image (Value)
            & Output.Field_Separator
            & Reason);

      end if;

   end Reject_Resolved_Edge;


   procedure Remove_Dynamic_Edge (
      Edge : in out Dynamic_Edge_T;
      From : in     Graph_T)
   is

      Slot : constant Positive := Index (From.Dynamic_Edges, Edge);
      -- The index of the Edge, in the Dynamic_Edges of the graph.

   begin

      if Slot > Last (From.Dynamic_Edges) then

         Output.Fault (
            Location => "Flow.Remove_Dynamic_Edge",
            Text     =>
                 "Edge not in graph"
               & Output.Field_Separator
               & Image (Edge.all));

      else

         Drop (Index => Slot, From => From.Dynamic_Edges);

         Close (Edge => Edge.all, Graph => From);

      end if;

   end Remove_Dynamic_Edge;


   procedure Remove_All_Dynamic_Edges (From : in Graph_T)
   is

      Edge : Dynamic_Edge_T;
      -- One of the dynamic edges.

   begin

      for N in First (From.Dynamic_Edges)
            .. Last  (From.Dynamic_Edges)
      loop

         Edge := Element (From.Dynamic_Edges, N);

         if Edge.State not in Unresolved .. Stable then

            Output.Fault (
               Location => "Flow.Remove_All_Dynamic_Edges",
               Text     => Full_Image (Edge.all));

         end if;

         Close (
            Edge  => Edge.all,
            Graph => From);

      end loop;

      Erase (From.Dynamic_Edges);

   end Remove_All_Dynamic_Edges;


   procedure Link_Nodes (
      Source    : in Node_T;
      Step_Edge : in Step_Edge_T;
      Target    : in Node_T;
      Within    : in Graph_T)
   --
   -- Creates the edge Source -> Target.
   --
   is
      Edge : Edge_T;
      -- The new edge.
   begin

      Edge := new Edge_Object_T'(
         Index     => Edge_Index_T (Next (Within.Edges)),
         Source    => Source,
         Step_Edge => Step_Edge,
         Target    => Target);

      Append (To => Within.Edges, Value => Edge);
      Append (To => Source.From, Value => Edge);
      Append (To => Target.Into, Value => Edge);

   end Link_Nodes;


   procedure Collect_Blocks (Graph : in Graph_T)
   is

      --    Principles of operation
      --
      -- First, note the following:
      --
      -- > basic blocks are here called "nodes";
      --
      -- > our definition of basic block allows unconditional
      --   transfer of control (jump) within a basic block; and
      --
      -- > some "solitary" steps (in particular call steps) are
      --   always isolated into their own, one-step nodes, although
      --   by control-flow alone they should be combined with other
      --   steps into larger nodes.
      --
      -- The algorithm relies on the fact that we can decide whether
      -- a step-edge is internal to the node under construction, or
      -- passes between nodes (is a node-edge), by inspecting the
      -- properties of the edge as follows. The edge is a node edge
      -- if one or several of the following conditions hold:
      --
      -- > The source step is a solitary step.
      -- > The source step has more than one outgoing edge.
      -- > The target step is a solitary step.
      -- > The target step has more than one incoming edge.
      -- > The target step is the entry step of the flow-graph.
      --
      -- This means that given the starting step of a node, that
      -- node can be collected and created independently of all other
      -- nodes.
      --
      -- The algorithm uses a work-list of step-edges that have been
      -- found to lead to new nodes and, thus, will give rise to
      -- node-edges.


      subtype Real_Step_Index_T is Step_Index_T
         range 1 .. Max_Step (Graph);
      --
      -- The indices of the real (existing) steps in the graph.

      Node_Of : Node_Of_Step_Ref :=
         new Node_Of_Step_T'(Real_Step_Index_T => null);
      --
      -- The node (basic block) that each step belongs to, once this
      -- node is created. Null entries mean there is more work to do.
      -- This will become Graph.Node_Of_Step.

      New_Edge : Step_Edge_List_T (1 .. Natural (Max_Step_Edge (Graph)));
      -- The work-list of step-edges that will correspond to node-edges.
      --
      -- Note that the index is not Step_Edge_Index_T.
      -- For more on the indexing, see below after Num_Node_Edges.
      --
      -- All the edges in the work-list have the property that the
      -- source node is already created and all the steps in the
      -- source node are entered in Node_Of. However, the target node
      -- may or may not exist already.

      First_New_Edge : Positive := 1;
      -- The first new edge in the work-list is New_Edge(First_New_Edge),
      -- providing that First_New_Edge <= Last_New_Edge.

      Last_New_Edge : Natural := 0;
      -- The last new edge in the work-list is New_Edge(Last_New_Edge),
      -- providing that First_New_Edge <= Last_New_Edge.

      Intro : constant Step_T := Entry_Step (Graph);
      -- The entry step. This is considered to have an additional
      -- implicit incoming edge, the entry.


      function Solitary (Step : Step_T) return Boolean
      --
      -- Whether the step is of a kind that must be alone
      -- in its block.
      --
      is
      begin

         return Flow.Calls.Is_Call (Step);

      end Solitary;


      function Sole_Successor (Step : Step_T) return Step_T
      --
      -- The (unique) successor of the Step, assuming that
      -- there is exactly one edge leaving Step.
      --
      is
      begin

         return Element(Step.From, 1).Target;

      end Sole_Successor;


      procedure Mark_For_Work (Edges : in Step_Edge_List_T)
      --
      -- Add the Edges to the work-list.
      --
      is
      begin

         for E in Edges'Range loop

            Last_New_Edge := Last_New_Edge + 1;

            New_Edge(Last_New_Edge) := Edges(E);

         end loop;

      end Mark_For_Work;


      procedure Collect_New_Node (First_Step : in Step_T)
      --
      -- Collects and creates the node that starts with First_Step,
      -- marks all the member steps in Node_Of as members of
      -- the new node, and adds the step-edges leaving the last
      -- step to the work-list.
      --
      is

         Last_Step : Step_T := First_Step;
         -- The last step in the node, so far found.

         Steps : Step_List_T (1 .. Node_Of'Length);
         -- Accumulates all the steps in the block.

         Num_Steps : Natural := 1;
         -- The number of steps accepted so far.
         -- The First_Step is accepted, of course.

         Next_Step : Step_T;
         -- The next candidate step.

         Node_Index : constant Node_Index_T :=
            Node_Index_T (Next (Graph.Nodes));
         -- The index of the new node.

         Node : Node_T;
         -- The new node itself.

      begin

         -- Collect all the steps:

         Steps (1) := First_Step;

         if not Solitary (First_Step) then

            -- Collect maximal linear chain:

            loop

               exit when Number_From (Last_Step, Graph) /= 1;

               Next_Step := Sole_Successor (Last_Step);

               exit when Number_Into (Next_Step, Graph) /= 1
                      or Solitary (Next_Step)
                      or Next_Step = Intro;

               -- Yea, this step is for us:

               Last_Step := Next_Step;

               Num_Steps := Num_Steps + 1;

               Steps (Num_Steps) := Last_Step;

            end loop;

         end if;

         -- Create the new node:

         Node := new Node_Object_T (Number_Of_Steps => Num_Steps);

         Node.Index := Node_Index;
         Node.Steps := Steps (1 .. Num_Steps);

         Append (
            To    => Graph.Nodes,
            Value => Node);

         -- Mark the steps as belonging to this node:

         for S in 1 .. Num_Steps loop

            Node_Of(Index (Steps(S))) := Node;

         end loop;

         -- Mark the edges leaving the last step for later work:

         Mark_For_Work (Edges_From (Last_Step, Graph));

      exception

      when X : others =>

         Output.Exception_Info (
            Text       => "Flow.Collect_Blocks.Collect_New_Node",
            Occurrence => X);

         raise;

      end Collect_New_Node;


      procedure Work_On (Edge : in Step_Edge_T)
      --
      -- Takes an edge from the work-list and does what it must.
      --
      is

         Source_Node : constant Node_T := Node_Of(Index (Source (Edge)));
         -- The source node, which must exist (/= null).

         Target_Step : constant Step_T := Target (Edge);
         -- The target step, which is the first step in its node.

         Target_Node : Node_T := Node_Of(Index (Target_Step));
         -- The target node, or null if it does not yet exist.

      begin

         -- Ensure that the target node exists:

         if Target_Node = null then
            -- The target node is not yet collected, so we do it now.

            Collect_New_Node (First_Step => Target_Step);

            Target_Node := Node_Of(Index (Target_Step));

         end if;

         -- Create the corresponding node-edge:

         Link_Nodes (
            Source    => Source_Node,
            Step_Edge => Edge,
            Target    => Target_Node,
            Within    => Graph);

      end Work_On;


      procedure Verify
      --
      -- Verify that all steps have been assigned to nodes.
      --
      is

         Unassigned : Natural := 0;
         -- The number of unassigned steps.

      begin

         for S in Node_Of'Range loop

            if Node_Of(S) = null then

               Unassigned := Unassigned + 1;

            end if;

         end loop;

         if Unassigned > 0 then

            Output.Fault (
               Location => "Flow.Collect_Blocks",
               Text     =>
                    "Steps not assigned to nodes:"
                  & Output.Image(Unassigned));

         end if;

      end Verify;


   begin  -- Collect_Blocks

      Erase (Graph.Nodes);
      Erase (Graph.Edges);

      if Opt.Deallocate then

         Free (Graph.Node_Of_Step);

      else

         Graph.Node_Of_Step := null;

      end if;

      -- Collect the entry node and initialize the work-list:

      Collect_New_Node (First_Step => Intro);

      -- Work on the list until all is done:

      while First_New_Edge <= Last_New_Edge loop

         Work_On (Edge => New_Edge(First_New_Edge));

         First_New_Edge := First_New_Edge + 1;

      end loop;

      -- Check the result:

      Verify;

      -- Store the result:

      Graph.Node_Of_Step := Node_Of;

   exception

   when X : others =>

      Output.Exception_Info (
         Text       => "Flow.Collect_Blocks",
         Occurrence => X);

      raise;

   end Collect_Blocks;



   --
   -- Operations involving cell sets:
   --


   function Cells_In (Graph : Graph_T)
   return Storage.Cell_Set_T
   is
      use Storage.Bitvec_Cell_Sets;

      Cells : Set_T;
      -- Collects all the cells accessed in the graph.
      -- Default initialized to the empty set.

   begin

      -- Traverse the nodes and add accessed cells:

      for N in 1 .. Max_Node (Graph) loop

         Add_Cells_Accessed (
            By    => Node_At (Index => N, Within => Graph),
            Graph => Graph,
            To    => Cells);

      end loop;

      return Cells;

   end Cells_In;


   procedure Add_Dynamic_Edge_Basis_Cells (
      From  : in     Node_T;
      Graph : in     Graph_T;
      To    : in out Storage.Cell_Set_T)
   is

      Dyn_Edge : Dynamic_Edge_T;
      -- Possible dynamic edge leaving a step in the node.

   begin

      -- Add basis cells for dynamic edges from the steps in the node:

      for D in First (Graph.Dynamic_Edges) .. Last (Graph.Dynamic_Edges) loop

         Dyn_Edge := Element (Graph.Dynamic_Edges, D);

         if Node_Containing (Source (Dyn_Edge.all), Graph) = From then
            -- The source of Dyn_Edge is in this node.

            Add_Basis_Cells (From => Dyn_Edge.all, To => To);

         end if;

      end loop;

   end Add_Dynamic_Edge_Basis_Cells;


   procedure Add_Cells_Used (
      By    : in     Node_T;
      Graph : in     Graph_T;
      To    : in out Storage.Cell_Set_T)
   --
   -- Finds the cells used (read) by the given node (i.e. by the expressions
   -- in the effects of the steps in the node or by the preconditions
   -- on edges leaving the node) and adds these cells to the given cell-set.
   -- Basis cells for boundable memory references are included.
   -- Cells read by callees are not included.
   --
   is
   begin

      -- Add cells used in steps:

      for S in By.Steps'Range loop

         Arithmetic.Add_Cells_Used (
            By   => By.Steps(S).Effect.all,
            Refs => True,
            Post => False,
            To   => To);

      end loop;

      -- Add cells used in preconditions on leaving edges:

      for F in 1 .. Length (By.From) loop

         Arithmetic.Add_Cells_Used (
            By   => Condition (Element (By.From, F)),
            Refs => True,
            To   => To);

      end loop;

      Add_Dynamic_Edge_Basis_Cells (
         From  => By,
         Graph => Graph,
         To    => To);

   end Add_Cells_Used;


   procedure Add_Cells_Defined (
      By : in     Node_T;
      To : in out Storage.Cell_Set_T)
   is
   begin

      for S in By.Steps'Range loop

         Arithmetic.Add_Cells_Defined (
            By => By.Steps(S).Effect.all,
            To => To);

      end loop;

   end Add_Cells_Defined;


   function Cells_Used (
      Nodes : Node_List_T;
      Graph : Graph_T)
   return Storage.Cell_List_T
   is
      use Storage.Bitvec_Cell_Sets;

      Used : Set_T;
      -- The result, initially empty.

   begin

      for N in Nodes'Range loop

        Add_Cells_Used (
           By    => Nodes(N),
           Graph => Graph,
           To    => Used);

      end loop;

      return To_List (Used);

   end Cells_Used;


   function Cells_Defined (
      Nodes : Node_List_T;
      Graph : Graph_T)
   return Storage.Cell_List_T
   is
      use Storage.Bitvec_Cell_Sets;

      Defined : Set_T;
      -- The result, initially empty.

   begin

      for N in Nodes'Range loop

        Add_Cells_Defined (
           By => Nodes(N),
           To => Defined);

      end loop;

      return To_List (Defined);

   end Cells_Defined;


   function Cells_Defined (By : Graph_T)
   return Storage.Cell_Set_T
   is
      use Storage.Bitvec_Cell_Sets;

      Defined : Set_T;
      -- The result, initially empty.

   begin

      for N in 1 .. Max_Node (By) loop

        Add_Cells_Defined (
           By => Node_At (Index => N, Within => By),
           To => Defined);

      end loop;

      return Defined;

   end Cells_Defined;


   procedure Add_Cells_Accessed (
      By    : in     Node_T;
      Graph : in     Graph_T;
      To    : in out Storage.Cell_Set_T)
   is
   begin

      Add_Cells_Defined(
         By => By,
         To => To);

      Add_Cells_Used(
         By    => By,
         Graph => Graph,
         To    => To);

   end Add_Cells_Accessed;


   function Is_Used (
      Location : Storage.Location_T;
      By       : Step_T)
   return Boolean
   is

      Here : constant Processor.Code_Address_T := Prime_Address (By);
      -- The code address of the step, for mapping with the
      -- Location.

      Cell : Storage.Cell_T;
      -- One of the cells for the location.

   begin

      for L in Location'Range loop

         if Storage.In_Range (
            Address => Here,
            Rainge  => Location(L).Address)
         then
            -- The location maps Here to a cell.

            Cell := Location(L).Cell;

            -- Check the effect:

            if Arithmetic.Is_Used (Cell => Cell, By => Effect (By))
            then
               -- This cell is used in the effect.

               return True;

            end if;

            -- Check the leaving edges:

            for F in 1 .. Last (By.From) loop

               if Arithmetic.Is_Used (
                  Cell => Cell,
                  By   => Element (By.From, F).Cond)
               then
                  -- This cell is used in the precondition.

                  return True;

               end if;

            end loop;

         end if;

      end loop;

      -- Location is not used:

      return False;

   end Is_Used;


   function Is_Used (
      Location : Storage.Location_T;
      By       : Node_T)
   return Boolean
   --
   -- Whether the given Location is used (read) By the steps in the
   -- given node. For details see Is_Used (By : Step).
   --
   is
   begin

      for S in By.Steps'Range loop

         if Is_Used (Location, By.Steps(S)) then

            return True;

         end if;

      end loop;

      return False;

   end Is_Used;


   function Is_Used (
      Location : Storage.Location_T;
      By       : Node_List_T)
   return Boolean
   is
   begin

      for B in By'Range loop

         if Is_Used (Location, By(B)) then

            return True;

         end if;

      end loop;

      return False;

   end Is_Used;


   function Is_Defined (
      Location : Storage.Location_T;
      By       : Step_T)
   return Boolean
   is

      Here : constant Processor.Code_Address_T := Prime_Address (By);
      -- The code address of the step, for mapping with the
      -- Location.

   begin

      for L in Location'Range loop

         if Storage.In_Range (
               Address => Here,
               Rainge  => Location(L).Address)
         and then
            Arithmetic.Is_Defined (
               Cell => Location(L).Cell,
               By   => Effect (By))
         then

            return True;

         end if;

      end loop;

      -- Location is not defined:

      return False;

   end Is_Defined;


   function Is_Defined (
      Location : Storage.Location_T;
      By       : Node_T)
   return Boolean
   --
   -- Whether the given Location is defined (assigned) By the steps
   -- in the given node. For details see Is_Defined (By : Step).
   --
   is
   begin

      for S in By.Steps'Range loop

         if Is_Defined (Location, By.Steps(S)) then

            return True;

         end if;

      end loop;

      return False;

   end Is_Defined;


   function Is_Defined (
      Location : Storage.Location_T;
      By       : Node_List_T)
   return Boolean
   is
   begin

      for B in By'Range loop

         if Is_Defined (Location, By(B)) then

            return True;

         end if;

      end loop;

      return False;

   end Is_Defined;


end Flow;
