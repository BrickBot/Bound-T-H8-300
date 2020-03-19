-- Flow.Execution.Times (body)
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
-- $Date: 2015/10/24 20:05:48 $
--
-- $Log: flow-execution-times.adb,v $
-- Revision 1.2  2015/10/24 20:05:48  niklas
-- Moved to free licence.
--
-- Revision 1.1  2008-07-14 19:16:56  niklas
-- BT-CH-0135: Assertions on "instructions".
--


with Ada.Unchecked_Deallocation;
with Faults;
with Flow.Execution.Opt;
with Processor.Properties;


package body Flow.Execution.Times is


   procedure Unchecked_Discard
   is new Ada.Unchecked_Deallocation (
         Name   => Node_Times_Ref,
         Object => Node_Times_T);


   procedure Discard (Item : in out Node_Times_Ref)
   is
   begin

      if Opt.Deallocate then

         Unchecked_Discard (Item);

      else

         Item := null;

      end if;

   exception when others => Faults.Deallocation;

   end Discard;


   procedure Unchecked_Discard
   is new Ada.Unchecked_Deallocation (
         Name   => Step_Edge_Times_Ref,
         Object => Step_Edge_Times_T);


   procedure Discard (Item : in out Step_Edge_Times_Ref)
   is
   begin

      if Opt.Deallocate then

         Unchecked_Discard (Item);

      else

         Item := null;

      end if;

   exception when others => Faults.Deallocation;

   end Discard;


   function Step_Edge_Times (Graph : Graph_T)
   return Step_Edge_Times_T
   is

      Times : Step_Edge_Times_T (1 .. Max_Step_Edge (Graph));
      -- The result.

   begin

      for T in Times'Range loop

         Times(T) := Time (Along => Edge_At (T, Graph));

      end loop;

      return Times;

   end Step_Edge_Times;


   function Work (
      Done_By : Node_T;
      Taking  : Step_Edge_Times_T;
      Using   : Processor.Power_T)
   return Processor.Work_T
   is

      Steps : constant Step_List_T := Steps_In (Done_By);
      -- The steps in the node.

      Edge : Step_Edge_T;
      -- An edge between two steps in the node.

      Total_Work : Processor.Work_T := Processor.No_Work;
      -- The sum of the step and edge efforts.

   begin

      for S in Steps'Range loop

         if S /= Steps'First then
            -- Steps(S) is a step in the node, but not the first step.

            Edge := Only_Edge_Into (Steps(S));
            -- The first (and only) edge to Steps(S).

            Processor.Add_Edge (
               Taking => Taking(Index(Edge)),
               Using  => Using,
               To     => Total_Work);
            -- Include the time of the edge.

         end if;

         Processor.Add_Step (
            Taking => Effort (Steps(S)),
            Using  => Using,
            To     => Total_Work);
         -- Include the effort of the step.

      end loop;

      return Total_Work;

   end Work;


   function Node_Times (
      Subprogram      : Programs.Subprogram_T;
      Step_Edge_Times : Step_Edge_Times_T;
      Asserts         : Assertions.Assertion_Map_T)
   return Node_Times_T
   is

      Graph : Flow.Graph_T := Programs.Flow_Graph (Subprogram);
      -- The flow graph of this subprogram.

      Times : Node_Times_T (1 .. Flow.Max_Node (Graph));
      -- The execution time of each node in the graph, here computed.

      Node : Flow.Node_T;
      -- The node in the graph for which the time is being computed.

      Power : Processor.Power_T;
      -- The processing power in the Node.

      Work_Done : Processor.Work_T;
      -- The total work of the node.

   begin

      for N in Times'Range loop

         Node := Flow.Node_At (N, Graph);

         Power := Processor.Properties.Power (
            Within => Node,
            Assert => Asserts);

         Work_Done := Work (
            Done_By => Node,
            Taking  => Step_Edge_Times,
            Using   => Power);

         Times(N) := Processor.Time_To_Finish (
            Work  => Work_Done,
            Using => Power);

      end loop;

      return Times;

   end Node_Times;


   function Edge_Times (
      Graph           : Graph_T;
      Step_Edge_Times : Step_Edge_Times_T)
   return Edge_Times_T
   is

      Times : Edge_Times_T (1 .. Max_Edge (Graph));
      -- The result.

      Edge : Edge_T;
      -- One of the edges.

   begin

      for E in Times'Range loop

         Edge := Edge_At (Index => E, Within => Graph);

         Times(E) := Step_Edge_Times(Index (Step_Edge (Edge)));

      end loop;

      return Times;

   end Edge_Times;


end Flow.Execution.Times;
