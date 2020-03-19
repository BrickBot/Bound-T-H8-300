-- Flow.Show (body)
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
-- $Revision: 1.17 $
-- $Date: 2015/10/24 20:05:48 $
--
-- $Log: flow-show.adb,v $
-- Revision 1.17  2015/10/24 20:05:48  niklas
-- Moved to free licence.
--
-- Revision 1.16  2009-01-18 07:56:06  niklas
-- Removed unused context clause.
--
-- Revision 1.15  2008/11/09 21:43:03  niklas
-- BT-CH-0158: Output.Image (Time_T) replaces Programs.Execution.Image.
--
-- Revision 1.14  2008/01/31 21:57:45  niklas
-- BT-CH-0108: Fixes to BT-CH-0098.
--
-- Revision 1.13  2007/12/22 15:23:47  niklas
-- BT-CH-0101: Option "-trace graph".
--
-- Revision 1.12  2007/11/12 21:37:27  niklas
-- BT-CH-0097: Only arithmetic analysis marks boundable edge domain.
--
-- Revision 1.11  2007/08/27 16:21:40  niklas
-- Extended Report_Unresolved_Flow to show the Image of unresolved edges.
--
-- Revision 1.10  2006/05/29 11:22:34  niklas
-- BT-CH-0023.
--
-- Revision 1.9  2006/04/27 08:36:28  niklas
-- Added function Locus (Step_Tag_T).
--
-- Revision 1.8  2005/09/03 11:50:29  niklas
-- BT-CH-0006.
--
-- Revision 1.7  2005/04/18 09:30:46  niklas
-- Added Report_Unresolved_Flow.
--
-- Revision 1.6  2005/02/16 21:11:45  niklas
-- BT-CH-0002.
--
-- Revision 1.5  2004/10/10 09:59:30  niklas
-- Display also time for node edges.
--
-- Revision 1.4  2004/08/17 18:25:21  niklas
-- Show_Steps displays edge times.
--
-- Revision 1.3  2004/04/26 18:27:58  niklas
-- First Tidorum version.
-- Include Symbol Table parameter in Show_Steps and Show_Nodes.
-- Show also Dynamics of Step.
-- Catch exceptions in Show_Steps and Show_Nodes.
--
-- Revision 1.2  2003/02/27 14:37:41  holsti
-- Mark call steps as such.
--
-- Revision 1.1  2001/03/21 20:31:23  holsti
-- First version.
--


with Ada.Strings;
with Ada.Strings.Fixed;
with Ada.Text_IO;
with Ada.Exceptions;

with Flow.Calls;
with Flow.Opt;
with Symbols.Show;


package body Flow.Show is

   use Ada.Text_IO;


   function Statements (
      Step   : Step_T;
      Source : Symbols.Symbol_Table_T)
   return Output.Statement_Range_T
   is
   begin
      return
         Symbols.Show.Statements (
            Address => Prime_Address (Step),
            Source  => Source);
   end Statements;


   function All_Statements (
      Step   : Step_T;
      Source : Symbols.Symbol_Table_T)
   return Output.Statement_Range_T
   is
      use type Output.Statement_Range_T;

      Addresses : constant Processor.Code_Address_List_T :=
         Processor.All_Addresses (State (Step));
      -- The addresses of all instructions involved with
      -- the step.

      Stats : Output.Statement_Range_T := Output.No_Statements;
      -- Accumulates the loci of All_Addr.

   begin

      for A in Addresses'Range loop

         Stats := Stats
            + Symbols.Show.Statements (
                 Address => Addresses(A),
                 Source  => Source);

      end loop;

      return Stats;

   end All_Statements;


   function Statements (
      Steps  : Step_List_T;
      Source : Symbols.Symbol_Table_T)
   return Output.Statement_Range_T
   is
      use type Output.Statement_Range_T;

      Stats : Output.Statement_Range_T := Output.No_Statements;
      -- Accumulates the loci of the steps.

   begin

      for S in Steps'Range loop
         Stats := Stats + Statements (Steps(S), Source);
      end loop;

      return Stats;

   end Statements;


   function Locus (
      Graph  : Graph_T;
      Source : Symbols.Symbol_Table_T)
   return Output.Locus_T
   is
   begin
      return
         Output.Locus (
            Statements => Statements (All_Steps (Graph), Source));
   end Locus;


   function Locus (
      Tag    : Step_Tag_T;
      Source : Symbols.Symbol_Table_T)
   return Output.Locus_T
   is
   begin

      return
         Output.Locus (
            Statements => Symbols.Show.Statements (
               Address => Processor.Prime_Address (State (Tag)),
               Source  => Source));

   end Locus;

   function Locus (
      Step   : Step_T;
      Source : Symbols.Symbol_Table_T)
   return Output.Locus_T
   is
   begin
      return
         Output.Locus (Statements => Statements (Step, Source));
   end Locus;


   function Locus (
      Node   : Node_T;
      Source : Symbols.Symbol_Table_T)
   return Output.Locus_T
   is
   begin
      return
         Output.Locus (Statements => Statements (Steps_In (Node), Source));
   end Locus;


   function Locus (
      Edge   : Boundable_Edge_T'Class;
      Source : Symbols.Symbol_Table_T)
   return Output.Locus_T
   is
   begin

      return Locus (
         Step   => Flow.Source (Edge),
         Source => Source);

   end Locus;


   function Index (Step : Step_T) return String
   --
   -- The step-index as a decimal number, no leading space.
   --
   is
      use Ada.Strings, Ada.Strings.Fixed;
   begin

      return Trim (Step_Index_T'Image (Index(Step)), Left);

   end Index;


   function Index (Node : Node_T) return String
   --
   -- The node-index as a decimal number, no leading space.
   --
   is
      use Ada.Strings, Ada.Strings.Fixed;
   begin

      return Trim (Node_Index_T'Image (Index(Node)), Left);

   end Index;


   function Call_Gist (Step : Step_T) return String
   --
   -- Brief description of a call-step, or nothing if the step
   -- is not a call-step.
   --
   is
   begin

      if Flow.Calls.Is_Call (Step) then

         return ", call";

      else
 
         return "";

      end if;

   end Call_Gist;


   function Gist (Step : Step_T) return String
   is
   begin

      return "Step "
         & Index(Step)
         & " at "
         & Image (Tag (Step))
         & ", effect "
         & Arithmetic.Image (Effect (Step))
         & ", info "
         & Processor.Image (Info (Step))
         & Call_Gist (Step);

   end Gist;


   procedure Show_Steps (
      Graph  : in Graph_T;
      Source : in Symbols.Symbol_Table_T)
   is


      procedure Show_Edge (Edge : Step_Edge_T)
      is
      begin
         Put_Line ("      Edge"
            & Step_Edge_Index_T'Image (Index(Edge))
            & ": "
            & Index (Flow.Source(Edge))
            & " -> "
            & Index (Flow.Target(Edge))
            & " when "
            & Arithmetic.Image (Condition (Edge))
            & " time "
            & Output.Image (Time (Along => Edge)));
      end Show_Edge;


      procedure Show_Edges (Edges : Step_Edge_List_T)
      is
      begin
         for E in Edges'Range loop
            Show_Edge (Edges(E));
         end loop;
      end Show_Edges;


      procedure Show_Step (Index : in Step_Index_T)
      is

         Step : constant Step_T := Step_At (Index => Index, Within => Graph);
         -- The step to be shown.

         Dyn_Edges : constant Dynamic_Edge_List_T :=
            Dynamic_Edges_From (Source => Step, Within => Graph);
         -- The dynamic edges (boundable edges) leaving the step.

      begin

         Put_Line (Gist (Step));

         Put_Line ("   Edges in:"
            & Natural'Image (Number_Into (Step, Graph)));

         Show_Edges (Edges_Into (Step, Graph));

         Put_Line ("   Edges out:"
            & Natural'Image (Number_From (Step, Graph)));

         Show_Edges (Edges_From (Step, Graph));

         if Dyn_Edges'Length > 0 then

            Put_Line ("   Dynamic edges out:"
               & Natural'Image (Dyn_Edges'Length));

            for D in Dyn_Edges'Range loop

               Put_Line ("      "
                  & Image (Dyn_Edges(D).all));

            end loop;

         end if;

      end Show_Step;


   begin  -- Show_Steps

      Put_Line ("Flow graph steps and step-edges:");

      for S in 1 .. Max_Step (Graph)
      loop
         Show_Step (Index => S);
      end loop;

      Put_Line ("End of flow graph.");

   exception

   when X : others =>

      Put_Line (
           "Exception in Flow.Show.Show_Steps:"
         & Ada.Exceptions.Exception_Information (X));

   end Show_Steps;


   procedure Show_Nodes (
      Graph  : in Graph_T;
      Source : Symbols.Symbol_Table_T)
   is

      procedure Show_Node_Steps (Steps : in Step_List_T)
      is
      begin
         for S in Steps'Range loop
            declare
               Step : Step_T renames Steps(S);
            begin
               Put ("   " & Gist (Step));
               Put (" ("
                  & Natural'Image (Number_Into (Step, Graph))
                  & " >"
                  & Natural'Image (Number_From (Step, Graph))
                  & " )");
               New_Line;
            end;
         end loop;
      end Show_Node_Steps;


      procedure Show_Edge (Edge : Edge_T)
      is
      begin
         Put_Line ("      Edge"
            & Edge_Index_T'Image (Index(Edge))
            & " (step-edge"
            & Step_Edge_Index_T'Image (Index (Step_Edge (Edge)))
            & "): "
            & Index (Flow.Source(Edge))
            & " -> "
            & Index (Flow.Target(Edge))
            & " when "
            & Arithmetic.Image (Condition (Edge))
            & " time "
            & Output.Image (Time (Along => Edge)));
      end Show_Edge;


      procedure Show_Edges (Edges : Edge_List_T)
      is
      begin
         for E in Edges'Range loop
            Show_Edge (Edges(E));
         end loop;
      end Show_Edges;


      procedure Show_Node (Index : in Node_Index_T)
      is

         Node : constant Node_T := Node_At (Index => Index, Within => Graph);

      begin

         Put_Line ("Node"
            & Node_Index_T'Image (Index)
            & " at "
            & Output.Image (Locus (Node, Source)));

         Show_Node_Steps (Steps_In (Node));

         Put_Line ("   Edges in:"
            & Natural'Image (Number_Into (Node, Graph)));

         Show_Edges (Edges_Into (Node, Graph));

         Put_Line ("   Edges out:"
            & Natural'Image (Number_From (Node, Graph)));

         Show_Edges (Edges_From (Node, Graph));

      end Show_Node;


   begin  -- Show_Nodes

      Put_Line ("Flow graph nodes and node-edges:");

      for N in 1 .. Max_Node (Graph)
      loop
         Show_Node (Index => N);
      end loop;

      Put_Line ("End of flow graph.");

   exception

      when X : others =>

         Put_Line (
              "Exception in Flow.Show.Show_Nodes:"
            & Ada.Exceptions.Exception_Information (X));

   end Show_Nodes;


   procedure Trace_Graph (
      Graph : in Graph_T;
      Source : in Symbols.Symbol_Table_T)
   is

      Sep : constant Character := Output.Field_Separator;

      Step : Step_T;
      -- One of the steps.

      Edge : Step_Edge_T;
      -- One of the edges.

      Src, Trg : Step_T;
      -- The source and target steps of the Edge.

   begin

      -- Trace the steps:

      for S in 1 .. Max_Step (Graph) loop

         Step := Step_At (S, Graph);

         Output.Trace (
            Locus => Locus (Step, Source),
            Text  =>
                 "Step"
               & Sep
               & Image (Tag (Step))
               & Sep
               & Image_All (Data (Step))
               & Sep
               & Arithmetic.Image (Effect (Step))
               & Sep
               & Processor.Image (Info (Step))
               & Sep
               & Output.Image (Number_Into (Step, Graph))
               & Sep
               & Output.Image (Number_From (Step, Graph))
               & Sep
               & Output.Image (Positive (S)));

      end loop;

      -- Trace the edges:

      for E in 1 .. Max_Step_Edge (Graph) loop

         Edge := Edge_At (E, Graph);

         Src := Flow.Source (Edge);
         Trg := Flow.Target (Edge);

         Output.Trace (
            Locus => Locus (Src, Source),
            Text  =>
                 "Edge"
               & Sep
               & Image (Tag (Src))
               & Sep
               & Image (Tag (Trg))
               & Sep
               & Arithmetic.Image (Condition (Edge))
               & Sep
               & Output.Image (Time (Edge))
               & Sep
               & Output.Image (Positive (E))
               & Sep
               & Index (Src)
               & Sep
               & Index (Trg));

      end loop;

   end Trace_Graph;


   procedure Report_Unresolved_Flow (
      Graph  : in Graph_T;
      Source : in Symbols.Symbol_Table_T)
   is

      Edges : constant Dynamic_Edge_List_T := Dynamic_Edges (Graph);
      -- All the remaining dynamic edges.

      Edge : Dynamic_Edge_T;
      -- One of the Edges.

      Source_Locus : Output.Locus_T;
      -- Locates the source step of the Edge.

   begin

      for E in Edges'Range loop

         Edge := Edges(E);

         Source_Locus := Locus (Edge.Source, Source);

         case Edge.State is

         when Growing =>

            Output.Note (
               Locus => Source_Locus,
               Text  => "Dynamic control flow still growing.");

         when Unresolved =>

            Output.Error (
               Locus => Source_Locus,
               Text  =>
                    "Unresolved dynamic control flow"
                  & Output.Field_Separator
                  & Image (Edge.all));

         when Stable =>

            Output.Note (
               Locus => Source_Locus,
               Text  => "Dynamic control flow stably resolved.");

         end case;

         if Opt.Trace_Flow_Resolution then

            Output.Trace (Full_Image (Edge.all));

         end if;

      end loop;

   end Report_Unresolved_Flow;


end Flow.Show;
