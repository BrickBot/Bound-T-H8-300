-- Programs.Execution.Draw.Opt (body)
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
-- $Date: 2015/10/24 20:05:50 $
--
-- $Log: programs-execution-draw-opt.adb,v $
-- Revision 1.2  2015/10/24 20:05:50  niklas
-- Moved to free licence.
--
-- Revision 1.1  2011-08-31 04:17:13  niklas
-- Added for BT-CH-0222: Option registry. Option -dump. External help files.
--


with Options.Groups;


package body Programs.Execution.Draw.Opt is


   --
   ---   Master options
   --


   function Draw_Graphs return Boolean
   is
   begin

      return Dot_File.Set or Dot_Dir.Set;

   end Draw_Graphs;


   function Separate_Files return Boolean
   is
   begin

      return Dot_Dir.Set;

   end Separate_Files;


   procedure Set_Dot_File (
      Option : in Options.Option_Ref;
      Value  : in String)
   --
   -- Side-effects of setting the Dot_File Option.
   --
   is
   begin

      if Dot_Dir.Set then

         Output.Warning ("Option -dot overrides option -dot_dir.");

         Dot_Dir.Reset;

      end if;

   end Set_Dot_File;


   procedure Set_Dot_Dir (
      Option : in Options.Option_Ref;
      Value  : in String)
   --
   -- Side-effects of setting the Dot_Dir Option.
   --
   is
   begin

      if Dot_File.Set then

         Output.Warning ("Option -dot_dir overrides option -dot.");

         Dot_File.Reset;

      end if;

   end Set_Dot_Dir;


   --
   ---   Option groups
   --


   Draw_Group : constant Options.Group_Name_T := Options.Group ("draw");
   --
   -- Whether to draw the call-graph and control-flow graphs and
   -- where to put the drawings.


   Form_Group : constant Options.Group_Name_T :=
      Options.Group ("draw_form");
   --
   -- The general form of the drawings.


   Call_Graph_Group : constant Options.Group_Name_T :=
      Options.Group ("draw_call_graph");
   --
   -- The form of the call-graph drawings.


   Flow_Graph_Group : constant Options.Group_Name_T :=
      Options.Group ("draw_flow_graph");
   --
   -- The choice and form of the flow-graph drawings.


   Flow_Graph_Choice_Group : constant Options.Group_Name_T :=
      Options.Group ("draw_flow_graph_choice");
   --
   -- The choice of the flow-graph drawings.


   Flow_Graph_Annotation_Group : constant Options.Group_Name_T :=
      Options.Group ("draw_flow_graph_info");
   --
   -- The annotation information in the flow-graph drawings.


   Flow_Choice : constant Options.Groups_T := (
      Draw_Group, Flow_Graph_Group, Flow_Graph_Choice_Group);
   --
   -- The groups for options that choose which flow-graphs are drawn.


   Flow_Annotation : constant Options.Groups_T := (
      Draw_Group, Flow_Graph_Group, Flow_Graph_Annotation_Group);
   --
   -- The groups for options that choose how flow-graph drawings
   -- are annotated.


   Draw_Prefix : constant String := "draw";
   --
   -- The prefix for the "-draw" options.


begin  -- Programs.Execution.Draw.Opt

   --
   --- Option group order
   --

   Options.Set_Group_Priority (
      Higher => Draw_Group,
      Lower  => Form_Group);

   Options.Set_Group_Priority (
      Higher => Form_Group,
      Lower  => Call_Graph_Group);

   Options.Set_Group_Priority (
      Higher => Call_Graph_Group,
      Lower  => Flow_Graph_Group);

   Options.Set_Group_Priority (
      Higher => Flow_Graph_Group,
      Lower  => Flow_Graph_Choice_Group);

   Options.Set_Group_Priority (
      Higher => Flow_Graph_Choice_Group,
      Lower  => Flow_Graph_Annotation_Group);

   --
   ---  DOT format options
   --

   Options.Register (
      Option => Dot_File'access,
      Name   => "dot",
      Groups => (Options.Groups.Outputs, Draw_Group),
      Set    => Set_Dot_File'access);

   Options.Register (
      Option => Dot_Dir'access,
      Name   => "dot_dir",
      Groups => (Options.Groups.Outputs, Draw_Group),
      Set    => Set_Dot_Dir'access);

   Options.Register (
      Option => Dot_Size'access,
      Name   => "dot_size",
      Groups => (Draw_Group, Form_Group));

   Options.Register (
      Option => Dot_Page'access,
      Name   => "dot_page",
      Groups => (Draw_Group, Form_Group));

   --
   ---   Options common to call-graphs and flow-graphs
   --

   Options.Register (
      Option => Qualify_Names_Opt'access,
      Name   => Options.Prefixed (Draw_Prefix, "scope"),
      Groups => (Draw_Group, Form_Group));

   --
   ---   Call-graph options
   --

   Options.Register (
      Option => Bounds_Graph_Opt'access,
      Name   => Options.Prefixed (Draw_Prefix, "bounds"),
      Groups => (Draw_Group, Call_Graph_Group));

   --
   ---   Flow-graph choice options
   --

   Options.Register (
      Option => All_Bounds_Opt'access,
      Name   => Options.Prefixed (Draw_Prefix, "all"),
      Groups => Flow_Choice);

   Options.Register (
      Option => Used_Bounds_Opt'access,
      Name   => Options.Prefixed (Draw_Prefix, "used"),
      Groups => Flow_Choice);

   Options.Register (
      Option => Min_Time_Opt'access,
      Name   => Options.Prefixed (Draw_Prefix, "min"),
      Groups => Flow_Choice);

   Options.Register (
      Option => Max_Time_Opt'access,
      Name   => Options.Prefixed (Draw_Prefix, "max"),
      Groups => Flow_Choice);

   Options.Register (
      Option => Total_Bounds_Opt'access,
      Name   => Options.Prefixed (Draw_Prefix, "total"),
      Groups => Flow_Choice);

   Options.Register (
      Option => Deeply_Opt'access,
      Name   => Options.Prefixed (Draw_Prefix, "deeply"),
      Groups => Flow_Choice);

   --
   ---   Flow-graph annotation options
   --

   Options.Register (
      Option => Step_Graph_Opt'access,
      Name   => Options.Prefixed (Draw_Prefix, "step_graph"),
      Groups => Flow_Annotation);

   Options.Register (
      Option => Symbol_Opt'access,
      Name   => Options.Prefixed (Draw_Prefix, "symbol"),
      Groups => Flow_Annotation);

   Options.Register (
      Option => Line_Number_Opt'access,
      Name   => Options.Prefixed (Draw_Prefix, "line"),
      Groups => Flow_Annotation);

   Options.Register (
      Option => Count_Opt'access,
      Name   => Options.Prefixed (Draw_Prefix, "count"),
      Groups => Flow_Annotation);

   Options.Register (
      Option => Time_Opt'access,
      Name   => Options.Prefixed (Draw_Prefix, "time"),
      Groups => Flow_Annotation);

   Options.Register (
      Option => Index_Opt'access,
      Name   => Options.Prefixed (Draw_Prefix, "index"),
      Groups => Flow_Annotation);

   Options.Register (
      Option => Step_Tag_Opt'access,
      Name   => Options.Prefixed (Draw_Prefix, "step"),
      Groups => Flow_Annotation);

   Options.Register (
      Option => Code_Address_Opt'access,
      Name   => Options.Prefixed (Draw_Prefix, "address"),
      Groups => Flow_Annotation);

   Options.Register (
      Option => Decode_Opt'access,
      Name   => Options.Prefixed (Draw_Prefix, "decode"),
      Groups => Flow_Annotation);

   Options.Register (
      Option => Effect_Opt'access,
      Name   => Options.Prefixed (Draw_Prefix, "effect"),
      Groups => Flow_Annotation);

   Options.Register (
      Option => Condition_Opt'access,
      Name   => Options.Prefixed (Draw_Prefix, "cond"),
      Groups => Flow_Annotation);

end Programs.Execution.Draw.Opt;
