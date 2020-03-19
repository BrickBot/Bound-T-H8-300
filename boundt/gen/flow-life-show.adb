-- Flow.Life.Show (body)
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
-- $Revision: 1.4 $
-- $Date: 2015/10/24 20:05:48 $
--
-- $Log: flow-life-show.adb,v $
-- Revision 1.4  2015/10/24 20:05:48  niklas
-- Moved to free licence.
--
-- Revision 1.3  2007-02-13 20:15:49  Niklas
-- BT-CH-0044.
--
-- Revision 1.2  2005/09/16 13:02:37  niklas
-- Extended Show_Per_Node to display the edge indices.
--
-- Revision 1.1  2005/09/12 19:02:59  niklas
-- BT-CH-0008.
--


with Ada.Text_IO;
with Flow.Computation;
with Flow.Computation.Show;


package body Flow.Life.Show is


   use Ada.Text_IO;


   procedure Show_Intro
   --
   -- Outputs the introductory text.
   --
   is
   begin

      Put_Line (
           "A '"
         & Computation.Show.Feasible_Mark(True)
         & "' means feasible, a '"
         & Computation.Show.Feasible_Mark(False)
         & "' means infeasible.");

      New_Line;

   end Show_Intro;


   procedure Show_Step (
      Step       : in Step_Index_T;
      Living     : in Living_T;
      Margin_Col : in Ada.Text_IO.Positive_Count;
      Effect_Col : in Ada.Text_IO.Positive_Count)
   --
   -- Displays the "live" effect of the given Step.
   --
   is

      Live : constant Arithmetic.Assignment_List_T :=
         Live_Assignments (Step, Living);
      -- The "live" assignments.

      Dead : constant Arithmetic.Assignment_List_T :=
         Dead_Definitions (Step, Living);
      -- The "dead" Defining assignments.

   begin

      Set_Col (Margin_Col);

      Put (Computation.Show.Feasible_Mark(Computation.Is_Feasible (
         Step, Model (Living).all)));

      Put (Step_Index_T'Image (Step));

      Set_Col (Effect_Col);

      Put (Arithmetic.Image (Live));

      if Dead'Length > 0 then

         Put ("; dead def");

         for D in Dead'Range loop

            Put (' ');

            Put (Arithmetic.Image (Dead(D).Target));

         end loop;

      end if;

      New_Line;

   end Show_Step;


   procedure Show_Per_Node (Living : in Living_T)
   --
   -- Displays the Living effects, taking the current Col position
   -- as the left margin for each line (some lines may be more indented).
   --
   -- The output is ordered by node index, with the steps in each node
   -- ordered in flow sequence.

   is

      Node_Margin : constant Positive_Count := Col;
      -- The current column is taken as the left margin.

      Step_Margin : constant Positive_Count := Node_Margin + 10;
      -- The column for starting the display of steps.

      Effect_Col : constant Positive_Count := Step_Margin + 8;
      -- The column for the Effect of each step.

      Edge_Margin : constant Positive_Count := Node_Margin + 5;
      -- The column for starting the display of an out-edge.

      Graph : constant Graph_T := Flow.Life.Graph (Living);
      -- The underlying flow-graph.

      Node : Node_T;
      -- The current node in the Graph.


      procedure Show (Steps : in Step_List_T)
      --
      -- Shows the "live" effect of all the steps in a node.
      --
      is
      begin

         for S in Steps'Range loop

            Show_Step (
               Step       => Index (Steps(S)),
               Living     => Living,
               Margin_Col => Step_Margin,
               Effect_Col => Effect_Col);

         end loop;

      end Show;


      procedure Show (Edges : in Edge_List_T)
      --
      -- Shows the edges that leave the node.
      --
      is

         Edge : Edge_T;
         -- One of the Edges.

      begin

         for E in Edges'Range loop

            Edge := Edges(E);

            Set_Col (Edge_Margin);

            Put (Computation.Show.Feasible_Mark(
               Computation.Is_Feasible (Edge, Model (Living).all)));

            Put (" ->");

            Put (Node_Index_T'Image (Index (Target (Edge))));

            Put_Line (
                 " when "
               & Arithmetic.Image (
                     Computation.Condition (Edge, Model (Living).all))
               & ", edge"
               & Edge_Index_T'Image (Index (Edge)));

          end loop;

          if Edges'Length = 0 then

             Set_Col (Edge_Margin);

             Put_Line ("stop");

          end if;

      end Show;


   begin  -- Show_Per_Node

      Put_Line ("Live arithmetic effects per node:");

      Show_Intro;

      Set_Col (Node_Margin); Put ("Node");
      Set_Col (Step_Margin); Put ("Step");
      Set_Col (Effect_Col ); Put ("Effect");
      New_Line;

      for N in 1 .. Max_Node (Graph) loop

         Node := Node_At (N, Graph);

         Set_Col (Node_Margin);

         Put (Computation.Show.Feasible_Mark(Computation.Is_Feasible (
            Node, Model (Living).all)));

         Put (Node_Index_T'Image (N));

         Show (Steps => Steps_In (Node));

         Show (Edges => Edges_From (Node, Graph));

      end loop;

      New_Line;

   end Show_Per_Node;


   procedure Show_Per_Step (Living : in Living_T)
   is

      Margin : constant Positive_Count := Col;
      -- The current column is taken as the left margin.

      Effect_Col : constant Positive_Count := Margin + 8;
      -- The column for the Effect of each step.

      Graph : constant Graph_T := Flow.Life.Graph (Living);
      -- The underlying flow-graph.

   begin

      Put_Line ("Live arithmetic effects per step:");

      Show_Intro;

      Set_Col (Margin);

      Put ("Step"); Set_Col (Effect_Col); Put_Line ("Effect");

      for S in 1 .. Max_Step (Graph) loop

         Show_Step (
            Step       => S,
            Living     => Living,
            Margin_Col => Margin,
            Effect_Col => Effect_Col);

      end loop;

      New_Line;

   end Show_Per_Step;


end Flow.Life.Show;
