-- Programs.Execution.Show (body)
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
-- $Revision: 1.14 $
-- $Date: 2015/10/24 20:05:51 $
--
-- $Log: programs-show.adb,v $
-- Revision 1.14  2015/10/24 20:05:51  niklas
-- Moved to free licence.
--
-- Revision 1.13  2009-03-10 17:10:36  niklas
-- Extended Trace_Instructions_And_Branches to show the name of the
-- callee for call steps. Modified the output to mark steps "Step"
-- or "Call", respectively (instead of "Instruction") and to mark
-- edges "Edge" (instead of "Condition").
--
-- Revision 1.12  2008/12/25 08:33:04  niklas
-- Removed unused local variable.
--
-- Revision 1.11  2008/11/09 21:41:57  niklas
-- BT-CH-0158: Option "-trace instr".
--
-- Revision 1.10  2007/08/03 17:45:14  niklas
-- Corrected context clauses.
--
-- Revision 1.9  2007/08/03 17:36:41  niklas
-- Recreated to extract the pure Subprogram/Call operations.
--


with Ada.Text_IO;
with Decoder;
with Flow.Calls;
with Flow.Show;
with Output;
with Processor.Properties;


package body Programs.Show is

   use Ada.Text_IO;


   procedure Show (Subprogram : in Subprogram_T)
   is

      Margin : constant Positive_Count := Col;
      -- The given left margin for the output.

      Calls : constant Call_List_T := Calls_From (Subprogram);
      -- All calls from this subprogram.

   begin

      Put_Line (Name (Subprogram, Qualified => True));

      Set_Col (Margin);

      Put (
           "Subprogram #"
         & Subprogram_Index_T'Image (Index (Subprogram))
         & ", entry address "
         & Processor.Image (Entry_Address (Subprogram)));

      if Reducible (Subprogram) then

         Put (", reducible flow-graph");

      else

         Put (", irreducible flow-graph");

      end if;

      if Returns (Subprogram) then

         Put (", can return to caller");

      else

         Put (", does not return to caller");

      end if;

      Put_Line (".");

      Set_Col (Margin);

      if Calls'Length = 0 then

         Put_Line ("No calls.");

      else

         Put_Line ("Calls:");

         Set_Col (Margin + 3);

         Show (Calls);

      end if;

   end Show;


   procedure Show (Subprograms : in Subprogram_Set_T)
   is

      Margin : constant Positive_Count := Col;
      -- The given left margin for the output.

      Subs : constant Subprogram_List_T := To_List (Subprograms);
      -- All the subprograms in the set.

   begin

      for S in Subs'Range loop

         Set_Col (Margin);

         Put ('[' & Positive'Image(S) & " ]  ");

         Show (Subprogram => Subs(S));

      end loop;

   end Show;


   procedure Show (Subprograms : in Subprogram_List_T)
   is

      Margin : constant Positive_Count := Col;
      -- The given left margin for the output.

   begin

      for S in Subprograms'Range loop

         Set_Col (Margin);

         Put ('[' & Positive'Image(S) & " ]  ");

         Put_Line (Index_And_Name (Subprograms(S)));

      end loop;

   end Show;


   procedure Show (Calls : in Call_List_T)
   is

      Margin : constant Positive_Count := Col;
      -- The given left margin for the output.

   begin

      for C in Calls'Range loop

         Set_Col (Margin);

         Put_Line (Image (Calls(C)));

      end loop;

   end Show;


   procedure Trace_Instructions_And_Branches (Subprogram : in Subprogram_T)
   is
      use Flow;

      Sep : constant Character := Output.Field_Separator;
      -- Just for brevity.

      Program : constant Program_T := Programs.Program (Subprogram);
      -- The program that contains this Subprogram.

      Syms : constant Symbols.Symbol_Table_T := Symbol_Table (Program);
      -- The program's symbol-table, for locus information.

      Graph : constant Flow.Graph_T := Flow_Graph (Subprogram);
      -- The flow-graph of this Subprogram, to be displayed.

      Edge : Step_Edge_T;
      -- An edge leaving the current step.

   begin

      for S in 1 .. Max_Step (Graph) loop

         declare

            Step     : constant Step_T := Step_At (S, Graph);
            Leaving  : constant Step_Edge_List_T := Edges_From (Step, Graph);
            Step_Loc : constant Output.Locus_T := Flow.Show.Locus (Step, Syms);
            Call     : constant Call_T := Flow.Calls.Call_In (Step);
            -- A step and its properties.

            Instr  : constant String :=
               Decoder.Decoded_Instruction (Step, Program);
            -- The decoded and disassembled instruction.

            Step_Text : constant String :=
                 Sep
               & Instr
               & Sep
               & Arithmetic.Image (Effect (Step))
               & Sep
               & Processor.Image (Effort (Step));
            -- The main output text for this Step.

         begin

            -- Trace the step, the instruction, and the effect:

            if Call = No_Call then
               -- An ordinary step, not a call.

               Output.Trace (
                  Locus => Step_Loc,
                  Text  => "Step" & Step_Text);

            else
               -- A call step: show the callee, too.

               Output.Trace (
                  Locus => Step_Loc,
                  Text  => "Call" & Step_Text & Sep & Name (Callee (Call)));

            end if;
               
            -- Trace the edges:

            for L in Leaving'Range loop

               Edge := Leaving(L);

               Output.Trace (
                  Locus => Step_Loc,
                  Text  =>
                       "Edge"
                     & Sep
                     & Instr
                     & Sep
                     & Processor.Properties.Offset_Image (
                          From => Prime_Address (Step),
                          To   => Prime_Address (Target (Edge)))
                     & Sep
                     & Arithmetic.Image (Condition (Edge))
                     & Sep
                     & Output.Image (Time (Edge)));

            end loop;

         end;

      end loop;

   end Trace_Instructions_And_Branches;


end Programs.Show;
