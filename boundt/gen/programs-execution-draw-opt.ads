-- Programs.Execution.Draw.Opt (decl)
--
-- Options that control the drawing of the control-flow graphs
-- and call graphs.
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
-- $Date: 2015/10/24 19:36:51 $
--
-- $Log: programs-execution-draw-opt.ads,v $
-- Revision 1.10  2015/10/24 19:36:51  niklas
-- Moved to free licence.
--
-- Revision 1.9  2011-08-31 04:23:34  niklas
-- BT-CH-0222: Option registry. Option -dump. External help files.
--
-- Revision 1.8  2007/02/24 09:51:52  niklas
-- BT-CH-0046.
--
-- Revision 1.7  2006/11/20 20:20:19  niklas
-- BT-CH-0037.
--
-- Revision 1.6  2005/10/09 08:10:22  niklas
-- BT-CH-0013.
--
-- Revision 1.5  2005/09/20 19:35:59  niklas
-- BT-CH-0011.
--
-- Revision 1.4  2005/09/03 11:50:30  niklas
-- BT-CH-0006.
--
-- Revision 1.3  2005/08/24 11:09:09  niklas
-- Added the Dot_Page option.
--
-- Revision 1.2  2004/04/25 13:41:16  niklas
-- First Tidorum version. Add Code_Address option.
--
-- Revision 1.1  2003/03/11 08:30:04  holsti
-- First version renamed to be a child of Programs.Execution.
-- Normalized some lay-out and commenting.
--


with Options;
with Options.Bool;
with Options.Strings;


package Programs.Execution.Draw.Opt is


   --
   ---   Master options
   --


   function Draw_Graphs return Boolean;
   --
   -- Whether to draw graphs to one or more dot files.
   -- Depends on Dot_File and Dot_Dir.


   --
   ---  DOT format options
   --


   Dot_File : aliased Options.Strings.Option_T;
   --
   -- Indicates that graphs should be drawn, and all graphs should be
   -- placed in one DOT file of this name. If the file already exists,
   -- it is overwritten without warning.
   -- This option may be overridden by Dot_Dir.


   Dot_Dir : aliased Options.Strings.Option_T;
   --
   -- Indicates that graphs should be drawn, and each graph should be
   -- placed in its own DOT file, in the directory (folder) of this name.
   -- The directory must already exist, it is not created. If files already
   -- exist in the directory, they are overwritten without warning.
   -- If set, this option overrides the option Dot_File.


   function Separate_Files return Boolean;
   --
   -- Whether graphs should be drawn, and drawn in separate DOT files.


   Default_Dot_Size : constant String := "7,9";
   --
   -- Default value for Dot_Size.
   -- Use "" to reset Dot_Size.


   Dot_Size : aliased Options.Strings.Option_T :=
      Options.Strings.Set (Default_Dot_Size);
   --
   -- The drawing size for the DOT drawnings.
   -- By default DOT makes the drawing at its "natural" size (see the
   -- DOT manual). This option can specify another size, given as
   -- "width,height" where width and height are the numerical dimensions
   -- of the page (default unit is inch). Note, the value should *not*
   -- contain quotes; quotes will be added around the value of Dot_Size
   -- when the DOT file is written.
   --
   -- To forcefully reduce large graphs onto one page, specify Dot_Size
   -- but do not specify Dot_Page.


   Dot_Page : aliased Options.Strings.Option_T;
   --
   -- The page-size for the DOT drawings.
   -- By default the drawings are not paged (= infinite page).
   -- To make them paged, specify Dot_Page as "a4", for example,
   -- or as "width,height" where width and height are the numerical
   -- dimensions of the page (default unit is inch). Note, the value
   -- should *not* contain quotes; quotes will be added around the
   -- value of Dot_Page when the DOT file is written.


   --
   ---   Options common to call-graphs and flow-graphs
   --


   Qualify_Names_Opt : aliased Options.Bool.Option_T (Default => False);
   --
   -- Whether the display subprogram names qualified with the scope
   -- that contains the subprogram (in the Symbol Table sense).
   --
   Qualify_Names : Boolean renames Qualify_Names_Opt.Value;


   --
   ---   Call-graph options
   --


   Bounds_Graph_Opt : aliased Options.Bool.Option_T (Default => False);
   --
   -- Draw each bounding of a subprogram (Bounds_Ref) as a separate
   -- node in the call-graph, instead of summarising all bounds for
   -- the subprogram into a one node of the call-graph.
   --
   Bounds_Graph : Boolean renames Bounds_Graph_Opt.Value;


   --
   ---   Flow-graph choice options
   --
   --
   -- The following Boolean options select which execution bounds on
   -- control-flow graphs should be drawn for a given subprogram, from
   -- the call-closure of execution bounds for a given root subprogram.
   -- The options are not orthogonal or separate; some of them imply
   -- or cover others.


   All_Bounds_Opt : aliased Options.Bool.Option_T (Default => False);
   --
   -- Draw one flow-graph for each bounding (analysis) of the
   -- subprogram showing the execution counts and times for
   -- these bounds. Include all computed boundings, whether or
   -- not they take part in the overall worst-case execution.
   --
   All_Bounds : Boolean renames All_Bounds_Opt.Value;


   Used_Bounds_Opt : aliased Options.Bool.Option_T (Default => False);
   --
   -- Like All_Bounds but include only the boundings that take
   -- part, or might take part, in the overall worst-case execution.
   -- This excludes the boundings for which the execution count is
   -- bounded to zero in the worst-case execution.
   --
   Used_Bounds : Boolean renames Used_Bounds_Opt.Value;


   Min_Time_Opt : aliased Options.Bool.Option_T (Default => False);
   --
   -- Draw a flow-graph that shows those execution bounds that
   -- have the smallest bound on worst-case execution time.
   --
   Min_Time : Boolean renames Min_Time_Opt.Value;


   Max_Time_Opt : aliased Options.Bool.Option_T (Default => False);
   --
   -- Draw a flow-graph that shows those execution bounds that
   -- have the largest bound on worst-case execution time.
   --
   Max_Time : Boolean renames Max_Time_Opt.Value;


   Total_Bounds_Opt : aliased Options.Bool.Option_T (Default => False);
   --
   -- Draw a flow-graph that shows the total (added up) execution
   -- counts and execution times, over all the bounds on the
   -- given subprogram in the given root closure.
   --
   Total_Bounds : Boolean renames Total_Bounds_Opt.Value;


   Deeply_Opt : aliased Options.Bool.Option_T (Default => True);
   --
   -- Draw the control-flow graphs of all subprograms in the
   -- closure of a root subprogram, not just the root subprograms
   -- themselves.
   --
   Deeply : Boolean renames Deeply_Opt.Value;


   --
   ---   Flow-graph annotation options
   --


   Step_Graph_Opt : aliased Options.Bool.Option_T (Default => False);
   --
   -- Draw the flow-graphs at step level instead of node-level.
   --
   Step_Graph : Boolean renames Step_Graph_Opt.Value;


   Symbol_Opt : aliased Options.Bool.Option_T (Default => False);
   --
   -- Include the symbols connected to each graph node.
   --
   Symbol : Boolean renames Symbol_Opt.Value;


   Line_Number_Opt : aliased Options.Bool.Option_T (Default => True);
   --
   -- Include the source-line numbers (locus) connected to each
   -- graph node.
   --
   Line_Number : Boolean renames Line_Number_Opt.Value;


   Count_Opt : aliased Options.Bool.Option_T (Default => True);
   --
   -- Include the (worst-case) execution count of each graph node
   -- and edge.
   --
   Count : Boolean renames Count_Opt.Value;


   Time_Opt : aliased Options.Bool.Option_T (Default => True);
   --
   -- Include the (worst-case) execution time of each graph node
   -- and edge (if the edge takes some time).
   --
   Time : Boolean renames Time_Opt.Value;


   Index_Opt : aliased Options.Bool.Option_T (Default => False);
   --
   -- Include the internal index of the node, step or edge.
   --
   Index : Boolean renames Index_Opt.Value;


   Step_Tag_Opt : aliased Options.Bool.Option_T (Default => False);
   --
   -- Include the step-tag(s) corresponding to each graph node.
   --
   Step_Tag : Boolean renames Step_Tag_Opt.Value;


   Code_Address_Opt : aliased Options.Bool.Option_T (Default => False);
   --
   -- Include the primary code address of the first and the last step
   -- in the graph node. Note that this is _not_ necessarily the same
   -- as the least and greatest primary code address, nor are necessarily
   -- all intermediate addresses represented in the node.
   --
   -- For a step-level flow-graph, this option includes the primary
   -- code address of each step.
   --
   Code_Address : Boolean renames Code_Address_Opt.Value;


   Decode_Opt : aliased Options.Bool.Option_T (Default => False);
   --
   -- Include the decoded/disassembled instruction for each step.
   --
   Decode : Boolean renames Decode_Opt.Value;


   Effect_Opt : aliased Options.Bool.Option_T (Default => False);
   --
   -- Include the step effect(s) into the graphs.
   --
   Effect : Boolean renames Effect_Opt.Value;


   Condition_Opt : aliased Options.Bool.Option_T (Default => False);
   --
   -- Include the necessary precondition of each flow-graph edge.
   --
   Condition : Boolean renames Condition_Opt.Value;


end Programs.Execution.Draw.Opt;
