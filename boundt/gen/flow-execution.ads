-- Flow.Execution (decl)
--
-- Executions of flow graphs represented as execution counts
-- of nodes and edges. Computation of the execution time for
-- each node in a flow-graph, possibly depending on modified
-- times for step edges.
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
-- $Date: 2015/10/24 19:36:49 $
--
-- $Log: flow-execution.ads,v $
-- Revision 1.20  2015/10/24 19:36:49  niklas
-- Moved to free licence.
--
-- Revision 1.19  2009-01-18 08:01:21  niklas
-- Removed unused context clause.
--
-- Revision 1.18  2008/09/24 08:38:52  niklas
-- BT-CH-0146: Assertions on "loop starts <bound> times".
-- BT-CH-0146: Loop-repeat assertions set both lower and upper bound.
-- BT-CH-0146: Report locations of contradictory "count" assertions.
-- BT-CH-0146: Contradictory "count" assertions imply infeasibility.
--
-- Revision 1.17  2008/07/28 19:23:45  niklas
-- BT-CH-0140: Detect contradictory execution-count bounds.
--
-- Revision 1.16  2008/07/14 19:16:56  niklas
-- BT-CH-0135: Assertions on "instructions".
--
-- Revision 1.15  2007/12/17 13:54:36  niklas
-- BT-CH-0098: Assertions on stack usage and final stack height, etc.
--
-- Revision 1.14  2007/04/18 18:34:38  niklas
-- BT-CH-0057.
--
-- Revision 1.13  2007/03/29 15:18:02  niklas
-- BT-CH-0056.
--
-- Revision 1.12  2007/01/25 21:25:15  niklas
-- BT-CH-0043.
--
-- Revision 1.11  2005/10/09 08:10:21  niklas
-- BT-CH-0013.
--
-- Revision 1.10  2005/08/24 10:16:12  niklas
-- Added the inquiry functions Total_Count (Node_List) and
-- Total_Count (Edge_List) to support the Analysis Workbench.
--
-- Revision 1.9  2004/04/28 19:02:13  niklas
-- First Tidorum version.
-- Moved the operations Count_Memory_Traffic and Work from the parent
-- package to this child package, where they are more at home.
-- Added operations Node_Times and Edge_Times to compute the total
-- execution time per node and per (node-) edge.
--
-- Revision 1.8  2001/04/17 11:10:07  ville
-- Edge_Index_T used in Count_Per_Edge_T (NC_067)
--
-- Revision 1.7  2001/02/19 09:27:36  holsti
-- Infinite and Bounded added.
--
-- Revision 1.6  2001/01/13 11:08:36  holsti
-- Remove to-be's in favour of NC's.
--
-- Revision 1.5  2000/09/08 13:05:57  saarinen
-- Changed edges to be indexed by Edge_Count_T,
-- since the number of edges can be zero.
--
-- Revision 1.4  2000/07/13 10:50:40  parviain
-- Added Loop_Counts_T.
--
-- Revision 1.3  2000/07/04 10:58:09  parviain
-- added times type
--
-- Revision 1.2  2000/06/29 11:06:43  parviain
-- added Counts_Ref_T
--
-- Revision 1.1  2000/06/20 13:32:07  holsti
-- First version.
--


with Loops;
with Processor;


package Flow.Execution is


   subtype Count_T is Natural;
   --
   -- The number of times an element of a flow graph is (or can
   -- be) executed.


   function Image (Item : Count_T) return String;
   --
   -- Decimal image without leading blank.


   function "*" (
      Left  : Count_T;
      Right : Processor.Time_T)
   return Processor.Time_T;
   --
   -- The total execution time that results from repeating a given
   -- execution time (Right) a given number (Left) of times. And if
   -- that isn't belabouring the obvious...


   Infinite : constant Count_T := Count_T'Last;
   --
   -- Represents an unbounded count (an absent bound on some count).


   function Bounded (Count : Count_T) return Boolean;
   --
   -- Whether the Count, which represents some other execution-count
   -- bound, is really bounded (less than Infinite).


   type Bound_T is record
      Min : Count_T;
      Max : Count_T;
   end record;
   --
   -- Bounds on the number of times an element of a flow graph can
   -- be executed. The execution count must be in Min .. Max.
   -- The "unbounded" case is represented by Min = 0 and
   -- Max = Infinite, respectively.


   Unbounded : constant Bound_T := (Min => 0, Max => Infinite);
   --
   -- Represents an unbounded number of executions.


   Impossible : constant Bound_T := (Min => 1, Max => 0);
   --
   -- Represents an impossible number of executions.


   function Bounded (Bound : Bound_T) return Boolean;
   --
   -- Whether the Bound is really bounded at either end, that is,
   -- whether Min > 0 or Max < Infinite.


   function Image (Item : Bound_T) return String;
   --
   -- The bounds in the form min .. max, with max omitted if Infinite.


   function Void (Item : Bound_T) return Boolean;
   --
   -- Whether the bounds are void (contradictory), that is,
   -- whether Min > Max.


   function Is_In (Count : Count_T; Bound : Bound_T)
   return Boolean;
   --
   -- Whether the Count is within the Bound.


   function "and" (Left, Right : Bound_T) return Bound_T;
   --
   -- Intersection (conjunction) of bounds.


   type Bounds_T is array (Node_Index_T range <>) of Bound_T;
   --
   -- Bounds on the execution of a flow graph, given as bounds
   -- on the number of executions of each node.


   type Count_Per_Node_T is array (Node_Index_T range <>) of Count_T;
   type Count_Per_Edge_T is array (Edge_Index_T range <>) of Count_T;
   --
   -- A count for each node/edge in a flow graph.


   type Counts_T (
      Nodes : Node_Index_T;
      Edges : Edge_Count_T)
   --
   -- An execution of a flow graph, given as the number of times
   -- each node or edge is executed.
   -- The number of Nodes and Edges are given as discriminants.
   -- Note that not all values of this type are consistent with
   -- the structure of the flow graph.
   --
   is record
      Node : Count_Per_Node_T (1 .. Nodes);
      Edge : Count_Per_Edge_T (1 .. Edges);
   end record;


   type Counts_Ref is access Counts_T;
   --
   -- Reference, to avoid supplying the discriminants to
   -- Counts_T when not applicable or not yet known.


   function Count (
      Node   : Node_T;
      Within : Counts_T)
   return Count_T;
   --
   -- The execution count of this Node, Within the given execution.


   function Total_Count (
     Nodes  : Node_List_T;
     Within : Counts_T)
   return Count_T;
   --
   -- The total execution count of the listed Nodes, Within the
   -- given execution.


   function Total_Count (
     Edges  : Edge_List_T;
     Within : Counts_T)
   return Count_T;
   --
   -- The total execution count of the listed Edges, Within the
   -- given execution.


   type Loop_Bounds_T is array (Loops.Loop_Index_T range <>) of Bound_T;
   --
   -- For each loop in a subprogram, a bound on the number of
   -- times the loop starts or repeats.


   type Node_Count_Bound_T is record
      Node  : Node_T;
      Count : Bound_T;
   end record;
   --
   -- Bounds on the execution count of a single node.


   type Node_Count_Bounds_T is array (Positive range <>) of Node_Count_Bound_T;
   --
   -- A set of nodes with bounded execution counts.
   -- The order of nodes in the list is generally irrelevant, and
   -- nodes are generally not duplicated in the list.


   --
   --    Memory traffic counts
   --


   procedure Count_Memory_Traffic (
      From   : in     Node_T;
      Reads  :    out Natural;
      Writes :    out Natural);
   --
   -- Returns the total number of memory reads and memory writes
   -- executed by one node of a flow-graph.


end Flow.Execution;
