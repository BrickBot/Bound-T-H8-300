-- Analyser.Opt
--
-- Command-line options for the main analysis functions.
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
-- $Revision: 1.19 $
-- $Date: 2015/10/24 19:36:46 $
--
-- $Log: analyser-opt.ads,v $
-- Revision 1.19  2015/10/24 19:36:46  niklas
-- Moved to free licence.
--
-- Revision 1.18  2011-08-31 04:23:33  niklas
-- BT-CH-0222: Option registry. Option -dump. External help files.
--
-- Revision 1.17  2009/06/10 12:44:30  niklas
-- BT-CH-0179: Assumed properties of omitted subprograms.
--
-- Revision 1.16  2007/12/17 13:54:32  niklas
-- BT-CH-0098: Assertions on stack usage and final stack height, etc.
--
-- Revision 1.15  2007/10/18 13:07:37  niklas
-- Added option Root_Alone.
--
-- Revision 1.14  2007/06/24 16:29:49  niklas
-- Added Warn_Eternal_Loop, with default.
--
-- Revision 1.13  2006/11/26 22:07:23  niklas
-- BT-CH-0039.
--
-- Revision 1.12  2005/06/28 08:33:55  niklas
-- Removed Show_Time and Show_Bounds in favour of Bounds.Opt.Bound_Time
-- and Programs.Execution.Show.Usage, respectively.
-- Using Programs.Execution.Show.Minimal_View instead of local value.
--
-- Revision 1.11  2005/02/23 09:05:12  niklas
-- BT-CH-0005.
--
-- Revision 1.10  2005/02/16 21:11:35  niklas
-- BT-CH-0002.
--
-- Revision 1.9  2004/04/25 07:27:04  niklas
-- Added Cease_Unless_Bounded, Tabulate_Time, Show_Space.
--
-- Revision 1.8  2004/04/25 07:15:20  niklas
-- First Tidorum version. Added Show_Time, Show_Callers.
--
-- Revision 1.7  2001/05/27 10:48:48  holsti
-- Added "show" options.
--
-- Revision 1.6  2000/11/24 12:05:58  sihvo
-- Added stack height analysis.
--
-- Revision 1.5  2000/08/20 20:59:54  holsti
-- Default_Max_Iterations for default value.
--
-- Revision 1.4  2000/08/18 18:08:34  holsti
-- Show-options default to False.
--
-- Revision 1.3  2000/07/24 22:24:26  holsti
-- Show_Loops added.
--
-- Revision 1.2  2000/06/11 21:40:27  holsti
-- Flow-graph display options added.
--
-- Revision 1.1  2000/04/24 18:56:24  holsti
-- Analyser.Opt added
--


with Options.Bool;
with Options.Pos;
with Programs.Execution.Show;


package Analyser.Opt is


   pragma Elaborate_Body;
   --
   -- To register the options.


   Root_Alone_Opt : aliased Options.Bool.Option_T (Default => False);
   --
   -- Whether to analyse only the root subprogram(s) alone, with
   -- no analysis of callees. In other words, all callees that are
   -- not explicitly asserted into stubs will be implicitly made
   -- into stubs with zero resource usage.
   --
   Root_Alone : Boolean renames Root_Alone_Opt.Value;


   function Trace_Unused_Subprograms return Boolean;
   --
   -- Whether to trace the detection of a subprogram that is
   -- known or asserted to be unused (never called in any execution
   -- that interests us) and will therefore not be analysed.
   -- This option tracks Flow.Calls.Opt.Trace_Unused_Calls.


   Trace_Omitted_Subprograms_Opt :
      aliased Options.Bool.Option_T (Default => False);
   --
   -- Whether to trace the detection of a subprogram that is
   -- asserted to be omitted from the analysis (not to be traced or
   -- analysed in any way). Such a subprogram may still be called as
   -- part of an execution, and must then have assertions on its
   -- time/space usage bounds.
   --
   Trace_Omitted_Subprograms : Boolean
      renames Trace_Omitted_Subprograms_Opt.Value;


   Assume_Omit_Alone_Opt : aliased Options.Bool.Option_T (Default => True);
   --
   -- Whether to assume certain properties for subprograms that
   -- are replaced by stubs and not analysed because of specific
   -- "omit" assertions or because we are analysing only root
   -- subprograms "alone" and this subprogram is not a root.
   --
   Assume_Omit_Alone : Boolean renames Assume_Omit_Alone_Opt.Value;


   Max_Iterations_Opt : aliased Options.Pos.Option_T (Default => 50);
   --
   -- Maximum number of iterations of control-flow analysis followed by
   -- data-flow analysis to bound dynamic jumps.
   --
   Max_Iterations : Positive renames Max_Iterations_Opt.Value;


   Show_Flow_Steps_Opt : aliased Options.Bool.Option_T (Default => False);
   --
   -- Whether to output completed flow graphs on the step level.
   --
   Show_Flow_Steps : Boolean renames Show_Flow_Steps_Opt.Value;


   Show_Flow_Nodes_Opt : aliased Options.Bool.Option_T (Default => False);
   --
   -- Whether to output completed flow graphs on the node level
   -- (i.e. the basic-block leve).
   --
   Show_Flow_Nodes : Boolean renames Show_Flow_Nodes_Opt.Value;


   Show_Loops_Opt : aliased Options.Bool.Option_T (Default => False);
   --
   -- Whether to output the loop-structure found in the flow-graph.
   --
   Show_Loops : Boolean renames Show_Loops_Opt.Value;


   Warn_Eternal_Loop_Opt : aliased Options.Bool.Option_T (Default => True);
   --
   -- Whether to emit a warning when an eternal loop is detected in
   -- a flow-graph (even if there is an assertion that bounds this
   -- loop; such assertions are only found later on).
   --
   Warn_Eternal_Loop : Boolean renames Warn_Eternal_Loop_Opt.Value;


   Show_Stack_Path_Opt : aliased Options.Bool.Option_T (Default => False);
   --
   -- Whether to output, for each root subprogram, the path in the
   -- call tree that creates the maximum stack usage.
   -- This option is relevant only if stack-space analysis is chosen.
   --
   Show_Stack_Path : Boolean renames Show_Stack_Path_Opt.Value;


   Tabulate_Time_Opt : aliased Options.Bool.Option_T (Default => False);
   --
   -- Whether to output for each root subprogram a table that shows
   -- how the execution time bound for the root is made up of local
   -- execution time and time in lower-level callees.
   --
   Tabulate_Time : Boolean renames Tabulate_Time_Opt.Value;


   Callers : constant Programs.Execution.Show.Feature_T :=
      Programs.Execution.Show.Callers;
   --
   -- Just a renaming.


   package View_Valued is new Options.Discrete_Set_Valued (
      Item_Type              => Programs.Execution.Show.Feature_T,
      Set_Type               => Programs.Execution.Show.View_T,
      Value_Type_Description => "Set of Booleans",
      Item_Image             => Programs.Execution.Show.Feature_T'Image);
   --
   type View_Option_T is new View_Valued.Option_T with null record;
   --
   -- The "-show" options.
 

   overriding
   procedure Update (
         Option  : access View_Option_T;
         Literal : in     String;
         Member  : in     Boolean);
   --
   -- Handles "-show" items with special treatment for "-show full".


   Show_View_Opt : aliased View_Option_T := (
      Default | Value => Programs.Execution.Show.Minimal_View,
      Enumerable      => <>);
   --
   -- Which features of the execution bounds to show in detailed output.
   --
   Show_View : Programs.Execution.Show.View_T renames Show_View_Opt.Value;


end Analyser.Opt;
