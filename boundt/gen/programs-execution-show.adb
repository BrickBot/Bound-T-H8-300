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
-- $Revision: 1.30 $
-- $Date: 2015/10/24 20:05:51 $
--
-- $Log: programs-execution-show.adb,v $
-- Revision 1.30  2015/10/24 20:05:51  niklas
-- Moved to free licence.
--
-- Revision 1.29  2011-08-31 04:23:34  niklas
-- BT-CH-0222: Option registry. Option -dump. External help files.
--
-- Revision 1.28  2009-11-27 11:28:07  niklas
-- BT-CH-0184: Bit-widths, Word_T, failed modular analysis.
--
-- Revision 1.27  2008/11/09 21:43:05  niklas
-- BT-CH-0158: Output.Image (Time_T) replaces Programs.Execution.Image.
--
-- Revision 1.26  2008/09/24 08:38:53  niklas
-- BT-CH-0146: Assertions on "loop starts <bound> times".
-- BT-CH-0146: Loop-repeat assertions set both lower and upper bound.
-- BT-CH-0146: Report locations of contradictory "count" assertions.
-- BT-CH-0146: Contradictory "count" assertions imply infeasibility.
--
-- Revision 1.25  2008/07/31 10:51:06  niklas
-- BT-CH-0141: Again show infeasible bounds as "unbounded".
--
-- Revision 1.24  2008/07/24 20:22:48  niklas
-- Added blank line after "stub" report.
--
-- Revision 1.23  2008/02/18 13:24:10  niklas
-- BT-CH-0111: Processor-specific info in execution bounds.
--
-- Revision 1.22  2007/12/21 06:58:48  niklas
-- Extended Show_Unbounded_Parts to show the unbounded loops also
-- for the Unbounded time-state, not just Vague and Depends.
--
-- Revision 1.21  2007/12/17 13:54:39  niklas
-- BT-CH-0098: Assertions on stack usage and final stack height, etc.
--
-- Revision 1.20  2007/08/03 17:51:55  niklas
-- Moved the Show procedures for Subprograms and Calls, with no use
-- of execution bounds, to the recreated package Programs.Show.
--
-- Revision 1.19  2007/06/24 18:46:56  niklas
-- Loop descriptions are exclusive so that a loop is either unrepeatable,
-- or else possibly eternal, or else possibly exit-at-end, but not more
-- than one of these.
--
-- Revision 1.18  2007/03/18 12:50:39  niklas
-- BT-CH-0050.
--
-- Revision 1.17  2007/02/13 20:28:39  Niklas
-- BT-CH-0044.
--
-- Revision 1.16  2007/01/13 13:51:05  niklas
-- BT-CH-0041.
--
-- Revision 1.15  2006/10/30 23:09:06  niklas
-- BT-CH-0033.
--
-- Revision 1.14  2006/08/22 12:53:08  niklas
-- Extended Show_Unbounded to accept and display No_Bounds.
--
-- Revision 1.13  2005/09/20 09:58:54  niklas
-- Added "exits at end" info for loops.
--
-- Revision 1.12  2005/06/29 09:34:32  niklas
-- Using Storage.Bounds.Show.
--
-- Revision 1.11  2005/06/28 08:38:24  niklas
-- Changes for User Manual version 3 as follows.
-- Added the Feature_T items General, Usage and Luups.
-- Added Full_View for use in "-show full".
-- Added function Some_Content to show if some real output is chosen.
-- Added type Sequence_T and related stuff to number the displayed
-- execution bounds sequentially and to keep track of which bounds
-- have already been shown and under which number (back reference).
-- Modified and described several Show procedures.
--
-- Revision 1.10  2005/06/19 08:16:20  niklas
-- Changed the format for reporting irreducible flow-graphs
-- and the absence of execution bounds, to be more like the
-- format for unbounded loops or unbounded local stack height.
--
-- Revision 1.9  2005/04/17 09:14:39  niklas
-- Changed Show_Stack to show Image (Max_Usage.Height) even when
-- it is Undefined and displays as '?'. This is more consistent
-- with the display of the stack path.
--
-- Revision 1.8  2005/04/14 21:17:58  niklas
-- Changed stack-path problem reports from Stack_Path lines to Warning
-- and Error lines.
--
-- Revision 1.7  2005/03/22 20:04:20  niklas
-- Fixed indentation in Show_Unbounded_Stack.
--
-- Revision 1.6  2005/02/23 09:05:19  niklas
-- BT-CH-0005.
--
-- Revision 1.5  2005/02/20 15:15:36  niklas
-- BT-CH-0004.
--
-- Revision 1.4  2005/02/16 21:11:47  niklas
-- BT-CH-0002.
--
-- Revision 1.3  2005/01/02 22:33:06  niklas
-- Corrected display of call with unbounded take-off.
--
-- Revision 1.2  2004/05/01 10:49:56  niklas
-- First Tidorum version.
-- Added options Spaces and Callers to various procedures.
-- Added procedures Show_Stack and Show_Stack_Path.
-- Added display of inverse call tree (Show_Paths_To) leading to unbounded
-- subprograms. This implementation uses linear search and is rather slow.
-- Added display of reducible/irreducible nature of subprogram.
-- Added display of eternal nature of loop.
-- Added detail on state of path bounds, time bounds, space bounds.
-- Added display of space (stack) bounds.
-- Removed Program_T parameters where the target program can now be
-- accessed via the Execution Bounds to be shown.
--
-- Revision 1.1  2003/03/11 08:28:57  holsti
-- First version renamed to be a child of Programs.Execution.
--


with Ada.Strings.Fixed;

with Flow;
with Flow.Computation.Show;
with Flow.Execution;
with Loops.Show;
with Output.Opt;
with Processor.Execution.Show;
with Processor.Properties;
with Storage.Bounds.Show;


package body Programs.Execution.Show is

   use Ada.Text_IO;


   --
   ---   Features to view
   --


   function Some_Content (View : View_T) return Boolean
   is
   begin

      for R in Real_Feature_T loop

         if View(R) then
            -- This real feature is selected.

            return True;

         end if;

      end loop;

      -- Nope:

      return False;

   end Some_Content;


   --
   ---   Indentation and column lay-out
   --


   First_Col : constant := 7;
   --
   -- The first column to be used for data.
   -- Earlier columns are used just for sequence numbers.


   Indent_Cols : constant := 3;
   --
   -- Indentation width in columns.


   Indent_Mark : constant String (1 .. Indent_Cols) := (
       1      => '.',
       others => ' ');
   --
   -- The "leader" string to be prefixed on indented lines.


   procedure Indent (To_Col : in Positive_Count)
   --
   -- Indent to a given column, using some multiple of Indent_Mark.
   --
   is
   begin

      while Col + Indent_Cols <= To_Col loop
         Put (Indent_Mark);
      end loop;

      Set_Col (To_Col);

   end Indent;


   function Image (Item : Integer) return String
   --
   -- Decimal digits, trimmed.
   --
   is
      use Ada.Strings, Ada.Strings.Fixed;
   begin

      return Trim (Integer'Image (Item), Left);

   end Image;


   procedure Show_Paths_To (
      Callee : in Subprogram_T;
      Margin : in Positive_Count)
   --
   -- Shows the inverse call tree of the given callee, listing all
   -- call paths to this callee from some root subprogram.
   --
   is

      procedure Ascend (
         From  : in Subprogram_T;
         Lower : in Call_Path_T)
      --
      -- Lists all the call paths from a root subprogram to the
      -- given From subprogram, including the Lower path (a path
      -- from the given From subprogram) in the output for each
      -- path.
      --
      is

         Up : constant Call_List_T := Calls_To (Callee => From);
         -- Any calls from above?

      begin

         if Up'Length = 0 then
            -- Yee-haw, we are at the summit! Gasp, gasp...

            Set_Col (Margin);
            Put_Line (Image (Lower));

         else
            -- We have not yet reached the top, but we hope that
            -- the oxygen will last and the abominable you-know-whats
            -- stay away.

            for U in Up'Range loop

               Ascend (
                  From  => Caller (Up(U)),
                  Lower => Up(U) & Lower);

            end loop;

        end if;

      end Ascend;


   begin  -- Show_Callers

      Ascend (
         From  => Callee,
         Lower => Null_Call_Path);

   end Show_Paths_To;


   procedure Show (
      Call_Path : in     Call_Path_T;
      Bounds    : in     Bounds_Set_T;
      Margin    : in     Positive_Count;
      View      : in     View_T;
      Sequence  : in out Sequence_T)
   is
   begin

      if Bounds /= No_Bounds_Set and Call_Path'Length > 0 then

         Show (
            Bounds  =>
               Bounds_For (
                  Subprogram => Callee (Call_Path (Call_Path'Last)),
                  Within     => Bounds,
                  Along      => Call_Path),
            Place    => Call_Path,
            Margin   => Margin,
            View     => View,
            Sequence => Sequence);

      end if;

   end Show;


   procedure Show (
      Calls  : in Call_List_T;
      Bounds : in Bounds_Set_T;
      View   : in View_T)
   is

      Margin : constant Positive_Count := Col;
      -- The given left margin for the output.

      Sequence : Sequence_T (Max_Size => Number_Of_Bounds (Bounds));
      -- The sequential numbering of the shown bounds.

   begin

      for S in Sequence.Index'Range loop

         Sequence.Index(S) := Not_Shown;

      end loop;

      for C in Calls'Range loop

         Set_Col (Margin);

         Show (
            Call_Path => (1 => Calls(C)),
            Bounds    => Bounds,
            Margin    => First_Col,
            View      => View,
            Sequence  => Sequence);

      end loop;

   end Show;


   procedure Show_General_Info (Bounds : in Bounds_Ref)
   --
   -- Show general information about bounds for a subprogram.
   --
   is

      Margin : constant Positive_Count := Col;
      -- The given left margin for the output.

      Sub_Locus : constant Output.Locus_T := Locus (Subprogram (Bounds));
      -- The location of the subprogram.

      Sub_Stats: constant Output.Statement_Range_T :=
         Output.Statements (Sub_Locus);
      -- The statement range of the subprogram.

      Context : constant Call_Path_T := Call_Path (Bounds);
      -- The context path.

      Stubs : constant Calling.Stub_Level_T := Stub_Level (Bounds);
      -- The stub level of these bounds.

   begin

      -- Identification of subprogram and bounds:

      Put_Line ("Full name      : "
         & Name (Subprogram (Bounds), Qualified => True));

      for F in 1 .. Output.Number_Of_Sources (Sub_Stats) loop

         Set_Col (Margin);

         Put_Line ("Source file    : "
            & Output.Source_File (Sub_Stats, F));

         Set_Col (Margin);

         Put_Line ("Source lines   : "
            & Output.Image (Sub_Stats, F));

      end loop;

      if Output.Number_Of_Sources (Sub_Stats) = 0
      or Output.Opt.Show_Code_Addresses
      then
         -- Show the code address range.

         Set_Col (Margin);

         Put_Line ("Code addresses : "
            & Output.Code_Image (Sub_Stats));

      end if;

      Set_Col (Margin);

      Put ("Call context   : ");

      if Context'Length = 0 then

         Put_Line ("none");

      else

         Put_Line (Image (Call_Path (Bounds)));

      end if;

      New_Line;

      Set_Col (Margin);

      Put_Line ("Execution bounds #"
         & Bounds_Index_T'Image (Index (Bounds)));

      -- General info on bounds:

      Set_Col (Margin);

      if Programs.Reducible (Subprogram (Bounds)) then

         Put ("Reducible");

      else

         Put ("Irreducible");

      end if;

      if Enough_For_Time (Bounds) then

         Put (", enough for time");

      end if;

      Put (", ");

      case Time_State (Bounds) is

      when Undefined  => Put ("time undefined");
      when Vague      => Put ("time vague");
      when Depends    => Put ("time context-dependent");
      when Asserted   => Put ("time asserted");
      when Computable => Put ("time computable");
      when Computed   => Put ("time computed");
      when Infeasible => Put ("execution infeasible");
      when Unbounded  => Put ("execution ubounded");
      when Failed     => Put ("time computation failed");

      end case;

      if Space_Bounded (Bounds) then

          Put (", space bounded");

      else

          Put (", space not bounded");

      end if;

      if Stubs = Calling.Calls_No_Stub then

         Put (", calls no stubs");

      else

         Put (", stub level" & Calling.Stub_Level_T'Image (Stubs));

      end if;

      Put_Line (".");

      New_Line;

   end Show_General_Info;


   procedure Show_Callers (
      To     : in Subprogram_T;
      Margin : in Positive_Count)
   --
   -- Show all the call paths from a root to this subprogram.
   --
   is
   begin

      Set_Col (Margin);
      Put_Line ( "All paths from a root to " & Name (To) & ':');

      Show_Paths_To (
         Callee => To,
         Margin => Margin + Indent_Cols);

      Set_Col (Margin + Indent_Cols);
      Put_Line ("---");

      New_Line;

   end Show_Callers;


   procedure Show (
      Bounds   : in     Bounds_Ref;
      Place    : in     Call_Path_T;
      View     : in     View_T;
      Margin   : in     Positive_Count;
      Sequence : in out Sequence_T)
   is

      Deeper_Margin : constant Positive_Count := Margin + Indent_Cols;
      -- The left margin for indented text.

      Not_A_Stub : Boolean;
      -- Whether the subprogram in question was analysed (is not
      -- modelled by a stub.


      procedure Show_Usage_Bounds
      --
      -- Show the requested resource-usage bounds.
      --
      is

         Stacks : constant Stacks_T := Programs.Stacks (Program (Bounds));
         -- All the stacks in the program.

         Usage : Stack_Usage_T;
         -- For one of the stacks.

      begin

         -- Time bounds:

         if For_Time (Bounds) then

            Set_Col (Margin);

            if Time_Bounded (Bounds) then

               Put ("WCET: " & Output.Image (Time (Bounds)));

               if Time_Asserted (Bounds) then

                  Put (" (asserted)");

               end if;

               New_Line;

            else

               Put_Line ("WCET is unknown.");

            end if;

            New_Line;

         end if;

         -- Space bounds:

         if For_Space (Bounds) then

            for S in Stacks'Range loop

               Set_Col (Margin);

               Put_Line (
                    "Local stack height for "
                  & Name (Stacks(S))
                  & ": "
                  & Max_Image (Stack_Height (Stacks(S), Bounds)));

               Set_Col (Margin);

               Usage := Stack_Usage (Stacks(S), Bounds);

               Put (
                    "Total stack usage  for "
                  & Name (Stacks(S))
                  & ": "
                  & Value_Image (Usage));

               if Usage.State = Asserted then

                  Put (" (asserted)");

               end if;

               New_Line;

            end loop;

            New_Line;

         end if;

      end Show_Usage_Bounds;


      procedure Show_Cells (
         What  : in String;
         Cells : in Storage.Cell_List_T)
      --
      -- Show a set of cells.
      --
      is
      begin

         Set_Col (Margin);
         Put (What & ':');

         if Cells'Length = 0 then

            Put_Line (" none");

         else

            New_Line;

            for C in Cells'Range loop
               Set_Col (Deeper_Margin);
               Put_Line (Storage.Image (Cells(C)));
            end loop;

         end if;

         New_Line;

      end Show_Cells;


      procedure Show_Inputs_And_Outputs
      --
      -- Show the input cells, arithmetic basis cells,
      -- initial cell bounds, and the output cells.
      --
      is

         Initial : constant Storage.Bounds.Cell_Interval_List_T :=
            Programs.Execution.Initial (Bounds);
         -- Initial bounds on cells (input cells and/or others).

      begin

         -- Input cells:

         if Inputs_Defined (Bounds) then

            Show_Cells (
               What  => "Input cells",
               Cells => Input_Cells (Bounds));

         else

            Set_Col (Margin);
            Put_Line ("Input cells undefined.");
            New_Line;

         end if;

         -- Basis cells:

         Show_Cells (
            What  => "Basis cells for arithmetic analysis",
            Cells => Basis (Bounds));

         -- Initial bounds:

         Set_Col (Margin);
         Put_Line ("Initial cell bounds on entry:");

         Set_Col (Deeper_Margin);
         Storage.Bounds.Show.Show (Initial);
         New_Line;

         -- Output cells:

         if Outputs_Defined (Bounds) then

            Show_Cells (
               What  => "Output cells",
               Cells => Output_Cells (Bounds));

         else

            Set_Col (Margin);
            Put_Line ("Output cells undefined.");
            New_Line;

         end if;

      end Show_Inputs_And_Outputs;


      procedure Show_Final_Stack_Heights
      --
      -- Show the final stack heights (net push-pop effect on
      -- stack height) for all stacks.
      --
      is

         Stacks : constant Stacks_T := Programs.Stacks (Program (Bounds));
         -- All the stacks in this program.

         Height_Col : constant Positive_Count := Deeper_Margin + 15;
         -- The column for the take-off height.

         Height : Final_Stack_Height_T;
         -- The bounds on the final stack height of a stack.

      begin

         if Stacks'Length = 0 then

            Set_Col (Margin);
            Put_Line ("No stacks in the program: no final stack heights.");

         else

            Set_Col (Margin);
            Put_Line ("Final stack height on return from subprogram:");
            New_Line;

            Set_Col (Deeper_Margin); Put ("Stack");
            Set_Col (Height_Col   ); Put ("Final height");
            New_Line;

            for S in Stacks'Range loop

               Height := Final_Stack_Height (
                  Stack  => Stacks(S),
                  Within => Bounds);

               Set_Col (Deeper_Margin);
               Put (Name (Stacks(S)));

               Set_Col (Height_Col);
               Put (Image (Height));

               New_Line;

            end loop;

         end if;

         New_Line;

      end Show_Final_Stack_Heights;


      procedure Show_Loop (
         Luup   : in Loops.Loop_T;
         Graph  : in Flow.Graph_T;
         Start  : in Flow.Execution.Bound_T;
         Neck   : in Flow.Execution.Bound_T;
         Repeat : in Flow.Execution.Bound_T)
      --
      -- Show the loop bounds.
      --
      is

         Loop_Locus : constant Output.Locus_T :=
            Loops.Show.Locus (
               Luup   => Luup,
               Within => Graph,
               Source => Symbol_Table (Bounds));
         -- The location of the loop.

      begin

         Set_Col (Margin);
         Put_Line (
              "Loop"
            & Loops.Loop_Index_T'Image (Loops.Loop_Index (Luup))
            & ' '
            & Output.Source_File (Loop_Locus)
            & ':'
            & Output.Image (Output.Statements (Loop_Locus)));

         if not Flow.Computation.Is_Feasible (
            Luup  => Luup,
            Under => Computation (Bounds).all)
         then

            Set_Col (Deeper_Margin);
            Put_Line ("Infeasible.");

         else
            -- The following are interesting only for a
            -- feasible loop:

            if not Flow.Computation.Is_Repeatable (
               Luup  => Luup,
               Under => Computation (Bounds).all)
            then
               Set_Col (Deeper_Margin);
               Put_Line ("Unrepeatable.");

            elsif Flow.Computation.Is_Eternal (
               Luup  => Luup,
               Under => Computation (Bounds).all)
            then
               Set_Col (Deeper_Margin);
               Put_Line ("Eternal.");

            elsif Loops.Exits_At_End (
               Luup   => Luup,
               Within => Graph)
            then
               Set_Col (Deeper_Margin);
               Put_Line ("Exits at end only (bottom-test).");
            end if;

         end if;

         if Flow.Execution.Bounded (Start) then
            Set_Col (Deeper_Margin);
            Put_Line ("Start " & Flow.Execution.Image (Start));
         end if;

         if Flow.Execution.Bounded (Neck) then
            Set_Col (Deeper_Margin);
            Put_Line ("Neck " & Flow.Execution.Image (Neck));
         end if;

         if Flow.Execution.Bounded (Repeat) then
            Set_Col (Deeper_Margin);
            Put_Line ("Repeat " & Flow.Execution.Image (Repeat));
         end if;

         if not (   Flow.Execution.Bounded (Neck)
                 or Flow.Execution.Bounded (Repeat))
         then
            Set_Col (Deeper_Margin);
            Put_Line ("Unbounded.");
         end if;

      end Show_Loop;


      procedure Show_Loops
      --
      -- Show the loop-bounds.
      --
      is

         Luups : constant Loops.Loops_T :=
            Loops_Of (Subprogram (Bounds));
         --
         -- All the loops in the subprogram.

         Graph : constant Flow.Graph_T :=
            Flow_Graph (Subprogram (Bounds));

         Start_Bounds : constant Loop_Bounds_T :=
            Loop_Start_Bounds (Bounds);
         -- The loop-start bounds.

         Neck_Bounds : constant Loop_Bounds_T :=
            Loop_Neck_Bounds (Bounds);
         -- The loop-neck bounds.

         Repeat_Bounds : constant Loop_Bounds_T :=
            Loop_Repeat_Bounds (Bounds);
         -- The loop-repeat bounds.

      begin

         if Luups'Length > 0 then

            if  Start_Bounds'Length  = Luups'Length
            and Neck_Bounds'Length   = Luups'Length
            and Repeat_Bounds'Length = Luups'Length
            then
               -- There seem to be loop-bounds defined.

               for L in Luups'Range loop

                  Show_Loop (
                     Luup   => Luups(L),
                     Graph  => Graph,
                     Start  => Start_Bounds(L),
                     Neck   => Neck_Bounds(L),
                     Repeat => Repeat_Bounds(L));

               end loop;

            else

               Set_Col (Margin);
               Put_Line ("Loop bounds not defined.");

            end if;

            New_Line;

         end if;

      end Show_Loops;


      procedure Show_Times
      --
      -- Show the execution time of each node, if known.
      --
      is
         use type Processor.Time_T;

         Local_Times : constant Node_Times_T :=
            Node_Times (From => Bounds, With_Calls => False);

         Full_Times : constant Node_Times_T :=
            Node_Times (From => Bounds, With_Calls => True);
         --
         -- The execution time of each node, or null array if not known.
         -- Full_Times includes the time of calls to lower-level
         -- subprograms; Local_Times does not.

         Full_Col  : constant Positive_Count := Deeper_Margin + 8;
         -- Column for Full_Times.

         Local_Col : constant Positive_Count := Full_Col + 10;
         -- Column for Local_Times.

         Calls_Col : constant Positive_Count := Local_Col + 10;
         -- Column for Full_Times - Local_Times = callee times.

      begin

         Set_Col (Margin);

         if Local_Times'Length = 0 then

            Put_Line ("Execution times of nodes not known.");

         else

            Put_Line ("Execution time of each node, in cycles:");
            New_Line;

            Set_Col (Deeper_Margin); Put ("Node");
            Set_Col (Full_Col     ); Put ("Total  =");
            Set_Col (Local_Col    ); Put ("Local  +");
            Set_Col (Calls_Col    ); Put ("Callees");
            New_Line;

            for N in Local_Times'Range loop

               Set_Col (Deeper_Margin);
               Put (Image (Integer(N)));

               Set_Col (Full_Col);
               Put (Output.Image (Full_Times(N)));

               Set_Col (Local_Col);
               Put (Output.Image (Local_Times(N)));

               Set_Col (Calls_Col);
               Put (Output.Image (Full_Times(N) - Local_Times(N)));

               New_Line;

            end loop;

         end if;

         New_Line;

      end Show_Times;


      function Flow_Arrow (Item : Flow.Edge_Index_T) return String
      --
      -- Describes the source and target of the edge in the form
      --
      --    <source node index> -> <target node index>.
      --
      is
         use Flow;

         Edge : constant Edge_T :=
            Edge_At (Item, Flow_Graph (Bounds));
         -- The edge itself.

      begin

         return
              Node_Index_T'Image (Index (Source (Edge)))
            & " ->"
            & Node_Index_T'Image (Index (Target (Edge)));

      end Flow_Arrow;


      procedure Show_Steps (List : in Flow.Step_List_T)
      --
      -- Shows the indices of the steps in the List.
      --
      is
      begin

         for L in List'Range loop

            Put (Flow.Step_Index_T'Image (Flow.Index (List(L))));

         end loop;

      end Show_Steps;


      procedure Show_Counts
      --
      -- Show the execution count of each node and edge, if known.
      --
      is
         use type Flow.Execution.Counts_Ref;

         Num_Col : constant Positive_Count := Deeper_Margin;
         -- The column for the number of the node or edge.

         Node_Count_Col : constant Positive_Count := Num_Col + 10;
         -- The column for the execution count of a node.

         Node_Steps_Col : constant Positive_Count := Node_Count_Col + 7;
         -- The column for the list of steps contained in a node.

         Edge_Count_Col : constant Positive_Count := Num_Col + 10;
         -- The column for the execution count of an edge.

         Arrow_Col : constant Positive_Count := Edge_Count_Col + 7;
         -- The column for the Source->Target of each edge.

         Counts : constant Flow.Execution.Counts_Ref :=
            Programs.Execution.Counts (Bounds);
         -- The execution counts of nodes and edges, or null
         -- if not known.

         Graph : constant Flow.Graph_T := Flow_Graph (Subprogram (Bounds));
         -- The control-flow graph.

         Feasible : Boolean;
         -- Whether a node or edge is feasible.

      begin

         Set_Col (Margin);

         if Counts = null then

            Put_Line ("Execution counts of nodes and edges not known.");

         else

            Put_Line ("Execution counts of nodes and edges:");
            New_Line;
            Set_Col (Deeper_Margin);
            Put_Line (
                "A '"
               & Flow.Computation.Show.Feasible_Mark(True)
               & "' means feasible, a '"
               & Flow.Computation.Show.Feasible_Mark(False)
               & "' means infeasible.");

            New_Line;

            Set_Col (Num_Col           ); Put ("Node");
            Set_Col (Node_Count_Col    ); Put ("Count");
            Set_Col (Node_Steps_Col + 1); Put ("Steps in node");
            New_Line;

            for N in Counts.Node'Range loop

               Feasible := Flow.Computation.Is_Feasible (
                  Node  => N,
                  Under => Computation (Bounds).all);

               Set_Col (Num_Col);
               Put (Flow.Computation.Show.Feasible_Mark(Feasible));

               Put (Flow.Node_Index_T'Image (N));

               Set_Col (Node_Count_Col);
               Put (Flow.Execution.Count_T'Image (Counts.Node(N)));

               Set_Col (Node_Steps_Col);
               Show_Steps (Flow.Steps_In (Flow.Node_At (N, Graph)));

               New_Line;

            end loop;

            New_Line;

            Set_Col (Num_Col       ); Put ("Edge");
            Set_Col (Edge_Count_Col); Put ("Count");
            Set_Col (Arrow_Col + 1 ); Put ("S -> T");
            New_Line;

            for E in Counts.Edge'Range loop

               Feasible := Flow.Computation.Is_Feasible (
                  Edge  => E,
                  Under => Computation (Bounds).all);

               Set_Col (Num_Col);
               Put (Flow.Computation.Show.Feasible_Mark(Feasible));

               Put (Flow.Edge_Index_T'Image (E));

               Set_Col (Edge_Count_Col);
               Put (Flow.Execution.Count_T'Image (Counts.Edge(E)));

               Set_Col (Arrow_Col);
               Put (Flow_Arrow (E));

               New_Line;

            end loop;

         end if;

         New_Line;

      end Show_Counts;


      procedure Show_Spaces
      --
      -- Show the space usage in this subprogram (those parts
      -- that were not shown in the general information).
      --
      is

         Stacks : constant Stacks_T := Programs.Stacks (Program (Bounds));
         -- All the stacks in this program.

         Calls : constant Call_List_T :=
            Programs.Calls_From (Subprogram (Bounds));
         -- All the calls from this subprogram, feasible or not.

         Model : Flow.Computation.Model_Handle_T := Computation (Bounds);
         -- The computation model, showing which calls are feasible.

         Stack : Stack_T;
         -- One of the stacks.

         Total : Stack_Usage_T;
         -- The total stack usage for these bounds for this Stack.

         Call : Call_T;
         -- One of the Calls.

         Callee_Bounds : Bounds_Ref;
         -- Bounds for the Call.

         Take_Off : Stack_Limit_T;
         -- The take-off height for the Call.

         Callee_Usage : Stack_Usage_T;
         -- The stack usage for the callee, in the Call.

         Call_Total : Stack_Usage_T;
         -- The total stack usage for the Call.

         Total_Col : constant Positive_Count := Deeper_Margin + 6;
         -- The column for the total stack usage of the call.

         Take_Off_Col : constant Positive_Count := Total_Col + 8;
         -- The column for the take-off height at the call.

         Callee_Col : constant Positive_Count := Take_Off_Col + 8;
         -- The column for the callee stack usage.

         Call_Col : constant Positive_Count := Callee_Col + 8;
         -- The column for the "caller=>callee" string.

      begin

         if Calls'Length = 0 then

            Set_Col (Margin);
            Put_Line ("No stack-bounds for calls.");

            New_Line;

         else

            for S in Stacks'Range loop

               Stack := Stacks(S);

               Set_Col (Margin);
               Put_Line (
                    "Bounds at calls for "
                  & Name (Stack)
                  & ":");

               if S = Stacks'First then

                  Set_Col (Deeper_Margin);
                  Put_Line ("A '+' means feasible, a '-' means infeasible.");
                  Set_Col (Deeper_Margin);
                  Put_Line ("A '*' marks the call giving maximum stack usage.");

               end if;

               New_Line;
               Set_Col (Deeper_Margin); Put ("Call");
               Set_Col (Total_Col    ); Put ("Total");
               Set_Col (Take_Off_Col ); Put ("Caller");
               Set_Col (Callee_Col   ); Put ("Callee");
               Set_Col (Call_Col     ); Put ("=> Callee");
               New_Line;

               Total := Stack_Usage (Stacks(S), Bounds);

               for C in Calls'Range loop

                  Call          := Calls(C);
                  Callee_Bounds := Call_Bounds (Call, Bounds);
                  Take_Off      := Take_Off_Height (Stack, Call, Bounds);
                  Callee_Usage  := Stack_Usage (Stack, Callee_Bounds);
                  Call_Total    := Total_Usage (Call, Take_Off, Callee_Usage);
 
                  -- The index and feasibility of the call:

                  Set_Col (Deeper_Margin);

                  if Flow.Computation.Is_Feasible (Call, Model.all) then
                     Put ("+ ");
                  else
                     Put ("- ");
                  end if;

                  Put (Image (C));

                  -- The total stack usage of the call:

                  Set_Col (Total_Col);

                  if Call = Total.Call then
                     Put ('*');
                  else
                     Put (' ');
                  end if;

                  Put (Brief_Image (Call_Total));

                  -- The take-off height:

                  Set_Col (Take_Off_Col);
                  Put (Image (Take_Off));

                  -- The callee stack usage:

                  Set_Col (Callee_Col);
                  Put (Brief_Image (Callee_Usage));

                  -- The "@line=>callee" description:

                  Set_Col (Call_Col);
                  Put (Call_Arrow (Call));

                  New_Line;

               end loop;

               New_Line;

            end loop;

         end if;

      end Show_Spaces;


      procedure Show_Processor_Info
      --
      -- Show the processor-specific execution-bounds information.
      --
      is
      begin

         Processor.Execution.Show (
            Info   => Processor_Info (Bounds),
            Bounds => Bounds,
            Margin => Margin);

      end Show_Processor_Info;


      procedure Show_Deeper_Calls
      --
      -- Show the bounds on deeper calls.
      --
      is

         Items : constant Call_Bounds_List_T := Call_Bounds (Bounds);
         -- All feasible calls within these execution bounds, together
         -- with the applicable execution bounds on the callee. 

      begin

         for C in Items'Range loop

            Show (
               Bounds   => Items(C).Bounds,
               Place    => Place & Items(C).Call,
               Margin   => Deeper_Margin,
               View     => View,
               Sequence => Sequence);

         end loop;

      end Show_Deeper_Calls;


   begin  -- Show

      -- Output the sequence number and call path:

      Put (Image (Integer (Sequence.Next)));

      Sequence.Next := Sequence.Next + 1;

      Set_Col (First_Col);

      Indent (To_Col => Margin);

      Put_Line (Image (Place));

      New_Line;

      -- Output the bounds:

      if not Defined (Bounds) then
         -- Huh?

         Set_Col (Margin);

         Put_Line ("Bounds not defined.");
         New_Line;

      elsif Sequence.Index(Index (Bounds)) /= Not_Shown then
         -- Already shown.

         Set_Col (Margin);

         Put_Line ("See above line"
            & Seq_Number_T'Image (Sequence.Index(Index (Bounds)))
            & '.');

         New_Line;

      else

         Sequence.Index(Index (Bounds)) := Sequence.Next - 1;

         Not_A_Stub := not Stub (Subprogram (Bounds));

         if not Not_A_Stub then

            Set_Col (Margin);

            Put_Line ("Stub subprogram (omitted from analysis).");

            New_Line;

         end if;

         -- General info on bounds:

         if View(General) and Not_A_Stub then

            Set_Col (Margin);

            Show_General_Info (Bounds);

         end if;

         if View(Bounds_Item) then

            Show_Usage_Bounds;

         end if;

         -- Computation model:

         if View(Model) and Not_A_Stub then

            Set_Col (Margin);

            Put_Line ("Computation model:");

            New_Line;

            Set_Col (Deeper_Margin);

            Flow.Computation.Show.Show (Computation (Bounds).all);

         end if;

         -- Input cells, basis cells, initial cell bounds:

         if View(Cells) and Not_A_Stub then

            Show_Inputs_And_Outputs;

         end if;

         if View(Stacks) then

            Show_Final_Stack_Heights;

         end if;

         -- Loop bounds:

         if View(Loops_Item) and Not_A_Stub then

            Show_Loops;

         end if;

         -- Node execution times:

         if View(Times) and Not_A_Stub then

            Show_Times;

         end if;

         -- Node and edge execution counts:

         if View(Counts) and Not_A_Stub then

            Show_Counts;

         end if;

         -- Space usage:

         if View(Spaces) and Not_A_Stub and For_Space (Bounds) then

            Show_Spaces;

         end if;

         -- Processor-specific information:

         if View(Proc) then

            Show_Processor_Info;

         end if;

         -- Inverse call tree:

         if View(Callers) then

            Show_Callers (
               To     => Subprogram (Bounds),
               Margin => Margin);

         end if;

         -- Call bounds:

         if View(Deeply) and Not_A_Stub then

            Show_Deeper_Calls;

         end if;

      end if;

   end Show;


   --
   ---   Show maximal  stack paths
   --


   Stack_Path_Key : constant String := "Stack_Path";
   --
   -- Key for the maximum stack-usage path in basic output, when
   -- the maximum usage is reached at a call (the path continues).


   Stack_Leaf_Key : constant String := "Stack_Leaf";
   --
   -- Key for the maximum stack-usage path in basic output, when
   -- the maximum usage is reached within a subprogram (not in a
   -- call) and the path ends.


   function Non_Stub_Height (
      Stack  : Stack_T;
      Within : Bounds_Ref)
   return String
   --
   -- The upper bound on the local Stack height, or the null string
   -- if the subprogram is a stub.
   --
   is
   begin

      if Stub_Level (Within) = 0 then

         return "";

      else 

         return Max_Image (Stack_Height (Stack, Within));

      end if;

   end Non_Stub_Height;


   procedure Show_Stack_Path (
      Stack  : in Stack_T;
      Within : in Bounds_Ref)
   --
   -- Shows the maximum stack-usage path for a given Stack, Within
   -- given execution bounds.
   --
   is
      use type Arithmetic.Value_T;

      Stack_Name : constant String := Name (Stack);
      -- The description of this stack.

      Bounds : Bounds_Ref := Within;
      -- Current bounds, going deeper and deeper.

      Max_Usage : Stack_Usage_T := Stack_Usage (Stack, Bounds);
      -- Stack used in the call tree starting from the current subprogram.

      Callee_Bounds : Bounds_Ref;
      -- Execution bounds on Max_Usage.Call, when not No_Call.

      Callee_Usage : Stack_Usage_T;
      -- Stack used in Callee_Bounds.

   begin

      -- Report on the overall result quality:

      if not Bounded (Max_Usage) then

         Output.Error (
            Locus => Locus (Bounds),
            Text  =>
                 Stack_Name
               & Output.Field_Separator
               & "Stack usage not bounded.");

      else

         -- Display the calls on the worst-case stack-usage path:

         while Max_Usage.Call /= No_Call loop

            Callee_Bounds := Call_Bounds (
               On     => Max_Usage.Call,
               Within => Bounds);

            Callee_Usage := Stack_Usage (Stack, Callee_Bounds);

            Output.Result (
               Key   => Stack_Path_Key,
               Locus => Locus (Max_Usage.Call),
               Text  =>
                    Stack_Name
                  & Output.Field_Separator
                  & Value_Image (Max_Usage)
                  & Output.Field_Separator
                  & Max_Image (Stack_Height (Stack, Bounds))
                  & Output.Field_Separator
                  & Max_Image (Take_Off_Height (Stack, Max_Usage.Call, Bounds))
                  & Output.Field_Separator
                  & Value_Image (Callee_Usage));

            -- Go to the next lower level in the path:

            Bounds    := Callee_Bounds;
            Max_Usage := Callee_Usage;

         end loop;

         -- At the end of the worst-case call path, the maximum usage
         -- is reached at a point within the present subprogram, with
         -- no relevant Call nor Take_Off:

         Output.Result (
            Key   => Stack_Leaf_Key,
            Locus => Locus (Subprogram (Bounds)),
            Text  =>
                 Stack_Name
               & Output.Field_Separator
               & Value_Image (Max_Usage)
               & Output.Field_Separator
               & Non_Stub_Height (Stack, Bounds)
               & Output.Field_Separator
               & Output.Field_Separator);

      end if;

   end Show_Stack_Path;


   procedure Show_Stack_Path (
      Root_Call : Call_T;
      Within    : Bounds_Set_T)
   is

      Bounds : constant Bounds_Ref :=
         Bounds_For (
            Subprogram => Callee (Root_Call),
            Within     => Within,
            Along      => Null_Call_Path);
      -- Execution bounds of the current subprogram in the path.

      Stacks : constant Stacks_T := Programs.Stacks (Program (Within));
      -- All the stacks in the program.

   begin

      for S in Stacks'Range loop

         Show_Stack_Path (
            Stack  => Stacks(S),
            Within => Bounds);

      end loop;

   end Show_Stack_Path;


   --
   ---   Show unbounded parts
   --


   procedure Show_Unbounded (
      Bounds  : in Bounds_Ref;
      Within  : in Bounds_Set_T;
      Path    : in Call_Path_T;
      Callers : in Boolean)
   is

      type Subset_T is array (Bounds_Index_T range <>) of Boolean;


      Bounds_Shown : Subset_T (1 .. Number_Of_Bounds(Within)) :=
         (others => False);
      --
      -- Whether a given execution-bounds object has already been shown.


      Callers_Shown : Subprogram_Set_T;
      --
      -- The subprograms for which the inverse call tree ("Callers")
      -- has been shown. It is shown at most once per subprogram.


      procedure Show_Unbounded_Parts (
         Call_Path : in Call_Path_T;
         Bounds    : in Bounds_Ref;
         Margin    : in Positive_Count);
      --
      -- Shows the unbounded parts of the call at the end of the
      -- given call-path, if these bounds have not been shown already.
      --
      -- Shows the unbounded parts in the called subprogram, and
      -- recursively shows the unbounded calls. Also remarks other
      -- bounding problems such as irreducible flow-graphs.
      --
      -- The Margin is the left margin for the output. The call-path
      -- is displayed starting from this margin, and then all other output
      -- is indented. However, if there are no unbounded parts in the
      -- called subprogram itself, the given call-path is not shown as
      -- such, instead we proceed immediately to the unbounded deeper
      -- calls.


      procedure Show_Unbounded_Calls (
         Call_Path : in Call_Path_T;
         Bounds    : in Bounds_Ref;
         Margin    : in Positive_Count)
      --
      -- Shows the unbounded parts of the unbounded calls within the 
      -- given Bounds, assumed to correspond to the given call-path.
      -- The Margin is the left margin for the output. This operation
      -- is mutually recursive with Show_Unbounded_Parts.
      -- 
      is

         Calls : Call_Bounds_List_T := Call_Bounds (Within => Bounds);
         -- The nested bounds, such as they are, for the calls.

      begin

         for C in Calls'Range loop

            if not Bounded (Calls(C).Bounds) then
               -- A call to a subprogram or a stub that is not
               -- sufficiently bounded by assertions or analysis.
               -- and thus has some unbounded parts (when a stub
               -- is considered a part of itself).

               Show_Unbounded_Parts (
                  Call_Path => Call_Path & Calls(C).Call,
                  Bounds    => Calls(C).Bounds,
                  Margin    => Margin);

            end if;

         end loop;

      end Show_Unbounded_Calls;


      procedure Show_Path (
         Call_Path   : in     Call_Path_T;
         Margin      : in     Positive_Count;
         Some_Shown  : in out Boolean;
         Next_Margin : in out Positive_Count)
      --
      -- Displays the heading for information about this Call_Path,
      -- if it has not already been displayed, and sets Next_Margin
      -- for showing deeper bounds.
      --
      is
      begin

         if not Some_Shown then

            Set_Col (Margin);
            Put_Line (Image (Call_Path));

            Next_Margin := Margin + Indent_Cols;
            Some_Shown  := True;

         end if;

      end Show_Path;


      procedure Show_Unbounded_Loops (
         Call_Path   : in     Call_Path_T;
         Sub         : in     Subprogram_T;
         Bounds      : in     Bounds_Ref;
         Margin      : in     Positive_Count;
         Some_Shown  : in out Boolean;
         Next_Margin : in out Positive_Count)
      --
      -- Shows the unbounded loops in the given Bounds, assumed
      -- to correspond to the given call-path. Nested bounds are
      -- not inspected here.
      --
      -- Margin
      --    The left margin for the output.
      -- Some_Shown
      --    in : Whether something has been shown for this Call_Path.
      --    out: Set to True if some unbounded loops in these Bounds
      --         were found and shown, otherwise not changed.
      -- Next_Margin
      --    The left margin for the deeper calls.
      --    Set to Margin + indentation if some unbounded loops in
      --    these Bounds were found and shown, otherwise not changed.
      --
      is

         Graph : Flow.Graph_T := Flow_Graph (Sub);
         -- The flow-graph of the subprogram.

         Luups : Loops.Loop_List_T :=
            Unbounded_Loops (Within => Bounds, Eternal => True);
         -- Unbounded loops in bounds, including unbounded eternal loops.

         Loc : Output.Locus_T;
         -- The locus of an unbounded loop.

      begin

         if Luups'Length > 0 then
            -- There are unbounded loops at this call level, which
            -- will be shown under this call-path.

            Show_Path (
               Call_Path   => Call_Path,
               Margin      => Margin,
               Some_Shown  => Some_Shown,
               Next_Margin => Next_Margin);

            for L in Luups'Range loop

               Set_Col (Next_Margin);

               Loc := Loops.Show.Locus (
                  Luup   => Luups(L),
                  Within => Graph,
                  Source => Symbol_Table (Bounds));

               Put ("Loop unbounded");

               if Flow.Computation.Is_Eternal (
                  Luup  => Luups(L),
                  Under => Computation (Bounds).all)
               then

                  Put (" (eternal)");

               elsif Loops.Exits_At_End (Luups(L), Graph)
               then

                  Put (" (exits at end only)");

               end if;

               Put_Line (
                    " at "
                  & Output.Source_File (Loc)
                  & ':'
                  & Output.Image (Output.Statements (Loc))
                  & ", offset "
                  & Processor.Properties.Offset_Image (
                     From => Entry_Address (Sub),
                     To   => Loops.Head_Address (Luups(L))));

            end loop;

         -- else
         --    Nothing to show at this level specifically.
         --    Go directly on to deeper levels.

         end if;

      end Show_Unbounded_Loops;


      procedure Show_Unbounded_Stack (
         Stack       : in     Stack_T;
         Call_Path   : in     Call_Path_T;
         Sub         : in     Subprogram_T;
         Bounds      : in     Bounds_Ref;
         Margin      : in     Positive_Count;
         Some_Shown  : in out Boolean;
         Next_Margin : in out Positive_Count)
      --
      -- Shows the unbounded usage of the given Stack in the given
      -- Bounds, assumed to correspond to the given call-path. Nested
      -- bounds are not inspected here.
      --
      -- Margin
      --    The left margin for the output.
      -- Some_Shown
      --    in : Whether something has been show for this Call_Path.
      --    out: Set to True if some unbounded stack in these Bounds
      --         was found and shown, otherwise not changed.
      -- Next_Margin
      --    The left margin for the deeper calls.
      --    Set to Margin + indentation if some unbounded stack in
      --    these Bounds was found and shown, otherwise not changed.
      --
      is

         Sub_Loc : constant Output.Locus_T := Locus (Sub);
         -- The subprogram locus.

         Height : constant Stack_Limit_T := Stack_Height (Stack, Bounds);
         -- The stack-height limit.

         Usage : constant Stack_Usage_T := Stack_Usage (Stack, Bounds);
         -- The stack-usage limit.

         Unbounded_Take_Off : constant Call_List_T :=
            Calls_With_Unbounded_Take_Off (Bounds);
         -- The calls with unbounded take-off limits for some stack.
         -- Some of these may have unbounded take-off for this Stack.

         Call : Call_T;
         -- One of the calls with possibly unbounded take-off limits.

         Loc : Output.Locus_T;
         -- The locus of a call with an unbounded take-off.

      begin

         if Stub (Sub) then
            -- For a stubbed subprogram only the total usage can be
            -- bounded or unbounded. The local stack height is
            -- irrelevant (or the same as the total usage) and there
            -- are no calls that could be unbounded.

            if not Bounded (Usage) then
               -- Unbounded total stack usage.

               Show_Path (
                  Call_Path   => Call_Path,
                  Margin      => Margin,
                  Some_Shown  => Some_Shown,
                  Next_Margin => Next_Margin);

               Set_Col (Next_Margin);

               Put_Line (
                    "Unbounded stub for "
                  & Name (Stack)
                  & " at "
                  & Output.Source_File (Sub_Loc)
                  & ':'
                  & Output.Image (Output.Statements (Sub_Loc)));

            end if;

         else
            -- For a real (non-stub) subprogram, the total stack usage
            -- is bounded iff the local stack height is bounded and
            -- all calls have bounded take-off height and stack usage.

            -- Unbounded local stack height?

            if not Bounded (Height) then
               -- Unbounded local stack height.

               Show_Path (
                  Call_Path   => Call_Path,
                  Margin      => Margin,
                  Some_Shown  => Some_Shown,
                  Next_Margin => Next_Margin);

               Set_Col (Next_Margin);

               Put_Line (
                    "Local stack-height unbounded for "
                  & Name (Stack)
                  & ':'
                  & Image (Height));

            end if;

            -- Unbounded take-off height for some call?

            for U in Unbounded_Take_Off'Range loop

               Call := Unbounded_Take_Off(U);

               if not Take_Off_Height_Bounded (Stack, Call, Bounds) then

                  Show_Path (
                     Call_Path   => Call_Path,
                     Margin      => Margin,
                     Some_Shown  => Some_Shown,
                     Next_Margin => Next_Margin);

                  Loc := Locus (Call);

                  Set_Col (Next_Margin);

                  Put_Line ("Local stack-height unbounded for "
                     & Name (Stack)
                     & " at call to "
                     & Name (Callee (Call))
                     & ':'
                     & Output.Source_File (Loc)
                     & ':'
                     & Output.Image (Output.Statements (Loc))
                     & ':'
                     & Image (Take_Off_Height (Stack, Call, Bounds)));

               end if;

            end loop;

         end if;

      end Show_Unbounded_Stack;


      procedure Show_Unbounded_Parts (
         Call_Path : in Call_Path_T;
         Bounds    : in Bounds_Ref;
         Margin    : in Positive_Count)
      is

         Sub : Subprogram_T := Subprogram (Bounds);
         -- The subprogram to which these bounds refer.

         Feasible : Boolean;
         -- Whether there is a feasible execution path.

         Some_Shown : Boolean := False;
         -- Whether some unbounded parts were found and shown for Sub.

         Next_Margin : Positive_Count := Margin;
         -- The margin for the next-level calls.
         -- Increased if something is shown at this level, otherwise
         -- maintained as is.


         procedure Report (Problem : in String)
         --
         -- Reports a problem with this call-path and subprogram.
         --
         is

            Loc : constant Output.Locus_T := Locus (Sub);
            -- The subprogram locus.

         begin

            Show_Path (
               Call_Path   => Call_Path,
               Margin      => Margin,
               Some_Shown  => Some_Shown,
               Next_Margin => Next_Margin);

            Set_Col (Next_Margin);

            Put_Line (
                 Problem
               & " at "
               & Output.Source_File (Loc)
               & ':'
               & Output.Image (Output.Statements (Loc)));

         end Report;


         procedure Show_Unbounded_Stacks
         --
         -- Show the unbounded stacks, assuming that 
         --
         is

            Stacks : constant Stacks_T := Programs.Stacks (Program (Bounds));
            -- All the stacks in the program.

         begin

            for S in Stacks'Range loop

               Show_Unbounded_Stack (
                  Stack       => Stacks(S),
                  Call_Path   => Call_Path,
                  Sub         => Sub,
                  Bounds      => Bounds,
                  Margin      => Margin,
                  Some_Shown  => Some_Shown,
                  Next_Margin => Next_Margin);

            end loop;

         end Show_Unbounded_Stacks;


      begin  -- Show_Unbounded_Parts

         if Bounds = No_Bounds then
            -- No bounds were found for this call path.
            -- This may occur if a failure of some kind aborts
            -- the bounding process.

            Report (Problem => "No bounds defined");

         elsif Bounds_Shown (Index (Bounds)) then
            -- Already handled.

            null;

         else
            -- These bounds not yet shown.

            Bounds_Shown (Index(Bounds)) := True;

            if not Reducible (Sub) then
               -- The callee is irreducible.

               Report (Problem => "Irreducible flow-graph");

            end if;

            -- Check if execution is feasible at all:

            Feasible :=
               Flow.Computation.Is_Feasible (Computation (Bounds).all)
               and ((not For_Time (Bounds))
                    or else Time_State (Bounds)  /= Infeasible)
               and ((not For_Space (Bounds))
                    or else Space_State (Bounds) /= Infeasible);

            if not Feasible then

               Report (Problem => "Execution infeasible");

            end if;

            -- Show the selected unbounded parts:

            if For_Time (Bounds) then

               case Time_State (Bounds) is

               when Vague | Depends =>

                  if Stub (Sub) then
                     -- Missing assertion on the execution time.

                     Report (Problem => "Unbounded stub");

                  else
                     -- Possibly some unbounded loops.

                     Show_Unbounded_Loops (
                        Call_Path   => Call_Path,
                        Sub         => Sub,
                        Bounds      => Bounds,
                        Margin      => Margin,
                        Some_Shown  => Some_Shown,
                        Next_Margin => Next_Margin);

                  end if;

               when Computed
                  | Asserted =>

                  null;

               when Infeasible =>
                  -- Already reported above.

                  null;

               when Unbounded =>

                  Show_Unbounded_Loops (
                     Call_Path   => Call_Path,
                     Sub         => Sub,
                     Bounds      => Bounds,
                     Margin      => Margin,
                     Some_Shown  => Some_Shown,
                     Next_Margin => Next_Margin);

                  Report (Problem => "Execution unbounded");

               when Failed =>

                  Report (Problem => "Time computation failed");

               when Undefined
                  | Computable =>

                  Report (Problem => "Fault");

                  Output.Fault (
                     Location => "Programs.Execution.Show_Unbounded_Parts",
                     Locus    => Locus (Bounds),
                     Text     =>
                          "Time_State is "
                        & Time_State_T'Image (Time_State (Bounds)));

               end case;

            end if;

            if For_Space (Bounds) and Feasible then

               Show_Unbounded_Stacks;

            end if;

            -- Possibly show the inverse call-tree:

            if (Callers and Some_Shown)
            and then not Is_Member (Sub, Callers_Shown)
            then
               -- This subprogram contains some unbounded parts, and
               -- the option to show all callers (call paths from a
               -- root to this subprogram) is chosen, and we have not
               -- yet shown the callers for this subprogram.

               Add (
                  To     => Callers_Shown,
                  Adding => Sub);

               Show_Callers (To => Sub, Margin => Next_Margin);

            end if;

            -- Dig deeper for gold (or sh*t):

            Show_Unbounded_Calls (
               Call_Path => Call_Path,
               Bounds    => Bounds,
               Margin    => Next_Margin);

         end if;

      end Show_Unbounded_Parts;


   begin  -- Show_Unbounded

      New_Line (2);

      if Bounds = No_Bounds then

         Put (Image (Path) & ": No execution bounds.");

      else

         Show_Unbounded_Parts (
            Call_Path => Path,
            Bounds    => Bounds,
            Margin    => Col);

      end if;

      New_Line (2);

      Erase (Callers_Shown);

   end Show_Unbounded;


end Programs.Execution.Show;
