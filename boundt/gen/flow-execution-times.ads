-- Flow.Execution.Times (decl)
--
-- Computation of the execution time for each node in a flow-graph,
-- possibly depending on modified times for step edges.
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
-- $Revision: 1.2 $
-- $Date: 2015/10/24 19:36:48 $
--
-- $Log: flow-execution-times.ads,v $
-- Revision 1.2  2015/10/24 19:36:48  niklas
-- Moved to free licence.
--
-- Revision 1.1  2008-07-14 19:16:56  niklas
-- BT-CH-0135: Assertions on "instructions".
--


--:dbpool with GNAT.Debug_Pools;

with Assertions;
with Processor;
with Programs;


package Flow.Execution.Times is


   type Node_Times_T is array (Node_Index_T range <>) of Processor.Time_T;
   --
   -- Execution times of nodes of a graph.
   -- May depend on the call-path.


   type Node_Times_Ref is access Node_Times_T;

   --:dbpool Node_Times_Pool : GNAT.Debug_Pools.Debug_Pool;

   --:dbpool for Node_Times_Ref'Storage_Pool use Node_Times_Pool;


   procedure Discard (Item : in out Node_Times_Ref);
   --
   -- Discards the Item, releasing the allocated memory.


   type Step_Edge_Times_T is
      array (Step_Edge_Index_T range <>) of Processor.Time_T;
   --
   -- Execution times of the step-edges of a graph.
   -- May depend on the call-path.


   type Step_Edge_Times_Ref is access Step_Edge_Times_T;

   --:dbpool Step_Edge_Times_Pool : GNAT.Debug_Pools.Debug_Pool;

   --:dbpool for Step_Edge_Times_Ref'Storage_Pool use Step_Edge_Times_Pool;


   procedure Discard (Item : in out Step_Edge_Times_Ref);
   --
   -- Discards the Item, releasing the allocated memory.


   function Step_Edge_Times (Graph : Graph_T)
   return Step_Edge_Times_T;
   --
   -- The execution time of each step-edge in the flow-graph, copied
   -- from the execution-time attribute of the step-edges.


   function Work (
      Done_By : Node_T;
      Taking  : Step_Edge_Times_T;
      Using   : Processor.Power_T)
   return Processor.Work_T;
   --
   -- The total work required to execute one node in a flow-graph,
   -- when executing the step-edges takes a specified time, and
   -- using a specific amount of processor power. This work can
   -- then be converted into an execution time.
   --
   -- Done_By
   --    The node that is doing the work. The work includes the
   --    computational and other effort of the steps in the node,
   --    and the time taken by the edges between the steps.
   -- Taking
   --    Supplies the execution time of the step-edges.
   -- Using
   --    The processor power that is available for executing
   --    the node. It may depend on local properties of the node,
   --    for example in which type of memory the instructions lie.
   --
   -- The total work of the node is computed by summing the steps and
   -- edges between steps, in the execution order. The edges entering
   -- and leaving the node are not included, only internal edges.


   function Node_Times (
      Subprogram      : Programs.Subprogram_T;
      Step_Edge_Times : Step_Edge_Times_T;
      Asserts         : Assertions.Assertion_Map_T)
   return Node_Times_T;
   --
   -- The local execution time of all nodes of a Subprogram, that
   -- is the time for executing the instructions in the subprogram
   -- itself without including the time spent in callees as part of
   -- call nodes (call steps).
   --
   -- The time is computed from the effort per instruction (as
   -- given by the Decoder and embedded in the flow-graph), from
   -- the Step_Edge_Times and from assertions on processor power
   -- or memory wait states etc. (as given by the user). The power
   -- assertions can be localized to specific parts of the
   -- subprogram (e.g. loops) and are therefore given by an
   -- assertion-map.
   --
   -- The execution time can also depend on the resolved dynamic data
   -- accessed (thru e.g. address-dependent memory wait-states) and
   -- on other target-specific properties asserted for this subprogram.


   type Edge_Times_T is
      array (Edge_Index_T range <>) of Processor.Time_T;
   --
   -- Execution times of the node-edges of a graph.
   -- May depend on the call-path.


   function Edge_Times (
      Graph           : Graph_T;
      Step_Edge_Times : Step_Edge_Times_T)
   return Edge_Times_T;
   --
   -- The execution time of each node-edge in the flow-graph, copied
   -- from the corresponding and given step-edge times.


end Flow.Execution.Times;
