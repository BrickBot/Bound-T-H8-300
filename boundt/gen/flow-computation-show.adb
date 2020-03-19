-- Flow.Computation.Show (body)
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
-- $Revision: 1.10 $
-- $Date: 2015/10/24 20:05:47 $
--
-- $Log: flow-computation-show.adb,v $
-- Revision 1.10  2015/10/24 20:05:47  niklas
-- Moved to free licence.
--
-- Revision 1.9  2008-04-26 14:36:51  niklas
-- Show also model index.
--
-- Revision 1.8  2008/02/15 20:27:43  niklas
-- BT-CH-0110: Better "&" for call-paths in Output loci.
--
-- Revision 1.7  2007/11/12 21:37:27  niklas
-- BT-CH-0097: Only arithmetic analysis marks boundable edge domain.
--
-- Revision 1.6  2007/10/11 11:58:47  niklas
-- Added Report_Unresolved_Flow, as in Flow.Show.
--
-- Revision 1.5  2005/06/28 07:06:48  niklas
-- Changed Show (Model) to omit the heading and the subprogram
-- name and to use Feasible_Mark in its legend.
--
-- Revision 1.4  2005/06/12 07:30:24  niklas
-- Changed the display of calls from A->B to A=>B, to avoid
-- confusion with '-' used in approximate line-numbers.
--
-- Revision 1.3  2005/04/18 12:58:25  niklas
-- Changed Report_Refs to issue at most one warning of each
-- kind (read memory, write memory) per inspected step.
--
-- Revision 1.2  2005/04/18 10:50:43  niklas
-- Added Report_Unresolved_Data.
--
-- Revision 1.1  2005/02/16 21:11:42  niklas
-- BT-CH-0002.
--


with Ada.Text_IO;
with Flow.Opt;
with Flow.Show;
with Output;
with Programs;


package body Flow.Computation.Show is


   procedure Show (Model : in Model_T)
   is
      use Ada.Text_IO;
      use type Arithmetic.Condition_T;
      use type Calling.Protocol_Ref;

      Margin  : constant Positive_Count := Col;
      -- The current column is taken as the left margin.

      -- For step effects:

      Effect_Col : constant Positive_Count := Margin + 6;
      -- The column for the Effect of each step.

      -- For edge conditions:

      Arrow_Col : constant Positive_Count := Effect_Col;
      -- The column for the Source->Target of each edge.

      Cond_Col : constant Positive_Count := Arrow_Col + 12;
      -- The column for the Cond of each edge.

      -- For dynamic edges:

      Dynamic_Edges : constant Dynamic_Edge_List_T :=
         Flow.Dynamic_Edges (Model.Graph);
      -- All the dynamic edges.

      Dyn_Edge : Dynamic_Edge_T;
      -- One of the Dynamic_Edges.

      Dyn_Source : Step_Index_T;
      -- The index of the source step of the Dyn_Edge.

      Dyn_State_Col : constant Positive_Count := Margin + 10;
      -- The column for the State of a dynamic edge.

      Dyn_Edge_Col : constant Positive_Count := Dyn_State_Col + 10;
      -- The column for the image of a dynamic edge.

      -- For calling protocols:

      Call_Step_Col : constant Positive_Count := Margin + 6;
      -- The column for the index of the call-step of a call.

      Call_Col : constant Positive_Count := Call_Step_Col + 6;
      -- The column for the image of a call.

      Proto_Col : constant Positive_Count := Call_Col + 25;
      -- The column for the Protocol of each call.

      -- Other variables:

      Edge : Step_Edge_T;
      -- One of the (step) edges.

      Call : Programs.Call_T;
      -- One of the calls.

      Protocol : Calling.Protocol_Ref;
      -- The protocol of the Call, or perhaps null.

   begin

      Set_Col (Margin);
      Put_Line (
           "Model #:"
         & Natural'Image (Model.Index)
         & ". References to this model:"
         & Natural'Image (Model.References));

      Set_Col (Margin);
      Put_Line (
           "A '"
         & Feasible_Mark(True)
         & "' means feasible, a '"
         & Feasible_Mark(False)
         & "' means infeasible.");

      New_Line;

      -- Show the effects:

      Set_Col (Margin);
      Put ("Step"); Set_Col (Effect_Col); Put_Line ("Effect");

      for S in Model.Effect'Range loop

         Set_Col (Margin);
         Put (Feasible_Mark(Model.Feasible(S)));
         Put (Step_Index_T'Image (S));

         Set_Col (Effect_Col);
         Put_Line (Arithmetic.Image (Model.Effect(S).all));

      end loop;

      New_Line;

      -- Show the edge preconditions:

      if Model.Condition'Length > 0 then
         -- There are some edges to show.

         Set_Col (Margin       ); Put ("Edge");
         Set_Col (Arrow_Col + 1); Put ("S -> T");
         Set_Col (Cond_Col     ); Put_Line ("Precondition");

         for E in Model.Condition'Range loop

            Set_Col (Margin);
            Put (Feasible_Mark(Model.Condition(E) /= Arithmetic.Never));
            Put (Step_Edge_Index_T'Image (E));

            Edge := Edge_At (E, Model.Graph);

            Set_Col (Arrow_Col);
            Put (Step_Index_T'Image (Index (Source (Edge))));
            Put (" ->");
            Put (Step_Index_T'Image (Index (Target (Edge))));

            Set_Col (Cond_Col);
            Put_Line (Arithmetic.Image (Model.Condition(E)));

         end loop;

         New_Line;

      end if;

      -- Show the dynamic edges:

      Set_Col (Margin);

      if Dynamic_Edges'Length > 0 then

         Put_Line ("Dynamic edges:");

         Set_Col (Margin       ); Put ("Source step");
         Set_Col (Dyn_State_Col); Put ("State");
         Set_Col (Dyn_Edge_Col ); Put_Line ("Dynamic edge");

         for D in Dynamic_Edges'Range loop

            Dyn_Edge := Dynamic_Edges(D);

            Dyn_Source := Index (Source (Dyn_Edge.all));

            Set_Col (Margin);
            Put (Feasible_Mark(Model.Feasible(Dyn_Source)));
            Put (Step_Index_T'Image (Dyn_Source));

            Set_Col (Dyn_State_Col);
            Put (Edge_Resolution_T'Image (State (Dyn_Edge.all)));

            Set_Col (Dyn_Edge_Col);
            Put_Line (Image (Dyn_Edge.all));

         end loop;

         New_Line;

      end if;

      -- Show the calling protocols:

      if Model.Protocol'Length = 0 then
         -- No calls to show.

         Set_Col (Margin);
         Put_Line ("No calls (leaf subprogram).");

      else
         -- There are some calls to show.

         Set_Col (Margin       ); Put ("Call");
         Set_Col (Call_Step_Col); Put ("Step");

         Set_Col (Call_Col     );
         Put ("Caller" & Output.Call_Mark & "Callee");

         Set_Col (Proto_Col    ); Put_Line ("Protocol");

         for C in Model.Protocol'Range loop

            Call := Programs.Call (
               From  => Model.Subprogram,
               Index => C);

            Protocol := Model.Protocol(C);

            Set_Col (Margin);
            Put (Feasible_Mark(Model.Feasible(Index (Programs.Step (Call)))));

            Put (Programs.Call_Index_T'Image (Programs.Index (Call)));

            Set_Col (Call_Step_Col);
            Put (Step_Index_T'Image (Index (Programs.Step (Call))));

            Set_Col (Call_Col);
            Put (Programs.Image (Call));

            Set_Col (Proto_Col);

            if Protocol = null then

               Put_Line ("(null)");

            else

               Put_Line (Calling.Image (Protocol.all));

            end if;

         end loop;

      end if;

      New_Line;

   end Show;


   procedure Show (Model : in Model_Ref)
   is
   begin

      if Model.Ref /= null then

         Show (Model => Model.Ref.all);

      else

         Ada.Text_IO.Put_Line ("No computation model.");

      end if;

   end Show;


   procedure Report_Refs (
      Step   : in Step_T;
      Effect : in Arithmetic.Effect_T;
      Source : in Symbols.Symbol_Table_T)
   --
   -- Reports, as Warnings, any remaining unresolved dynamic data
   -- references in the Effect of the Step.
   --
   is

      Targets : Boolean := False;
      Uses    : Boolean := False;
      -- Whether some assignment in the Effect targets a reference
      -- or uses a reference, respectively.

   begin

      -- Check each assignment in the Effect:

      for E in Effect'Range loop

         Targets := Targets or Arithmetic.Targets_Reference (Effect(E));
         Uses    := Uses    or Arithmetic.Uses_Reference    (Effect(E));

      end loop;

      -- Report:

      if Targets then

         Output.Warning (
            Locus => Flow.Show.Locus (Step, Source),
            Text  => "Unresolved dynamic memory write.");

      end if;

      if Uses then

         Output.Warning (
            Locus => Flow.Show.Locus (Step, Source),
            Text  => "Unresolved dynamic memory read.");

      end if;

   end Report_Refs;


   procedure Report_Unresolved_Data (
      Model  : in Model_Ref;
      Source : in Symbols.Symbol_Table_T)
   is

      Graph     : Graph_T       renames Model.Ref.Graph;
      Effect    : Effects_T     renames Model.Ref.Effect;
      Feasible  : Feasibility_T renames Model.Ref.Feasible;
      Condition : Conditions_T  renames Model.Ref.Condition;

   begin

      -- Dynamic memory refs in the arithmetic effect:

      for S in Effect'Range loop

         if Feasible(S) then

            Report_Refs (
               Step   => Step_At (Index => S, Within => Graph),
               Effect => Effect(S).all,
               Source => Source);

         end if;

      end loop;

      -- Dynamic memory refs in the edge preconditions:

      for E in Condition'Range loop

         if Arithmetic.Uses_Reference (Condition(E)) then
            -- The condition contains a Ref, which also means
            -- that the edge is still considered feasible.

            Output.Warning (
               Locus =>
                  Flow.Show.Locus (
                     Step => Flow.Source (
                        Edge => Edge_At (Index => E, Within => Graph)),
                     Source => Source),
               Text => "Unresolved dynamic memory read in condition.");

         end if;

      end loop;

      -- TBA (perhaps) unresolved dynamic calling protocols.

   end Report_Unresolved_Data;


   procedure Report_Unresolved_Flow (
      Model  : in Model_Ref;
      Source : in Symbols.Symbol_Table_T)
   is

      Edges : constant Dynamic_Edge_List_T := Dynamic_Edges (Model);
      -- All the remaining feasible dynamic edges.

      Edge : Dynamic_Edge_T;
      -- One of the Edges.

      Source_Locus : Output.Locus_T;
      -- Locates the source step of the Edge.

   begin

      for E in Edges'Range loop

         Edge := Edges(E);

         Source_Locus := Flow.Show.Locus (Edge.Source, Source);

         case Edge.State is

         when Growing =>

            Output.Note (
               Locus => Source_Locus,
               Text  => "Dynamic control flow still growing.");

         when Unresolved =>

            Output.Error (
               Locus => Source_Locus,
               Text  => "Unresolved dynamic control flow.");

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


end Flow.Computation.Show;
