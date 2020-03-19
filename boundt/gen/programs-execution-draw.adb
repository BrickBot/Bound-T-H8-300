-- Programs.Execution.Draw (body)
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
-- $Date: 2015/10/24 20:05:50 $
--
-- $Log: programs-execution-draw.adb,v $
-- Revision 1.30  2015/10/24 20:05:50  niklas
-- Moved to free licence.
--
-- Revision 1.29  2015/05/06 07:58:32  niklas
-- Suppress duplicated lines, by means of Label_Text_T.
--
-- Revision 1.28  2014/06/20 21:18:56  niklas
-- Write source-line numbers before decoded instruction.
--
-- Revision 1.27  2013/12/21 21:37:40  niklas
-- Extended Draw_Flow_Graph.Draw_Step (used for step_graph drawings)
-- to indicate the callee, when the step is a call-step.
--
-- Revision 1.26  2013/12/20 21:13:09  niklas
-- Added a catch-all exception-reporter to Draw_Graphs.
--
-- Revision 1.25  2011-08-31 04:23:34  niklas
-- BT-CH-0222: Option registry. Option -dump. External help files.
--
-- Revision 1.24  2009/03/19 10:58:34  niklas
-- Extended Draw_Step_Edge to show the edge's execution time.
--
-- Revision 1.23  2009/03/01 14:34:41  niklas
-- Fully infeasible edges are drawn in dotted style.
--
-- Revision 1.22  2008/11/09 21:43:04  niklas
-- BT-CH-0158: Output.Image (Time_T) replaces Programs.Execution.Image.
--
-- Revision 1.21  2008/07/14 19:16:57  niklas
-- BT-CH-0135: Assertions on "instructions".
--
-- Revision 1.20  2007/12/17 13:54:38  niklas
-- BT-CH-0098: Assertions on stack usage and final stack height, etc.
--
-- Revision 1.19  2007/10/19 09:49:08  niklas
-- Extended Add_Step_Info to put the image of the Prime_Address
-- before the Decoded_Instruction, under Opt.Decode.
--
-- Revision 1.18  2007/06/06 20:09:20  niklas
-- Added procedure Draw_Recursion_Graph. To help, modified procedures
-- Create_Separate_File and Draw_Header to take String parameters instead
-- of a Subprogram parameter, and brought function Node_Name (Subprogram)
-- out to package level.
-- Corrected Create_Separate_File to insert a '_' between the Prefix
-- and the Name.
--
-- Revision 1.17  2007/02/24 09:51:52  niklas
-- BT-CH-0046.
--
-- Revision 1.16  2007/02/13 20:25:23  Niklas
-- BT-CH-0044.
--
-- Revision 1.15  2006/11/26 22:07:26  niklas
-- BT-CH-0039.
--
-- Revision 1.14  2006/11/22 10:47:47  niklas
-- Omit "-draw decode" for call steps.
--
-- Revision 1.13  2006/11/20 20:20:19  niklas
-- BT-CH-0037.
--
-- Revision 1.12  2006/10/28 19:52:16  niklas
-- BT-CH-0031.
--
-- Revision 1.11  2006/06/16 14:53:38  niklas
-- Improved Add_Step_Info to use Output.Formed_Source, thus obeying
-- the -source option also for flow-graph drawings and -draw line.
--
-- Revision 1.10  2005/10/09 08:10:22  niklas
-- BT-CH-0013.
--
-- Revision 1.9  2005/09/23 10:51:41  niklas
-- BT-CH-0012.
--
-- Revision 1.8  2005/09/20 19:35:59  niklas
-- BT-CH-0011.
--
-- Revision 1.7  2005/09/03 11:50:30  niklas
-- BT-CH-0006.
--
-- Revision 1.6  2005/08/24 11:15:56  niklas
-- Using the Dot_Page option.
--
-- Revision 1.5  2005/02/20 15:15:36  niklas
-- BT-CH-0004.
--
-- Revision 1.4  2005/02/16 21:11:47  niklas
-- BT-CH-0002.
--
-- Revision 1.3  2004/05/20 18:23:10  niklas
-- Using Processor.Effort to hide Step_Info.Effort.
--
-- Revision 1.2  2004/05/01 10:59:20  niklas
-- First Tidorum version.
-- Changed source references from SSF to Tidorum.
-- Updated for changes in the parent package.
-- Omitted drawing of subprograms with asserted WCET.
-- Removed Program_T parameters where the target program can now be
-- accessed via some Bounds_Ref.
-- Added drawing of primary code address range (Code_Address_Image) under
-- the option Opt.Code_Address.
--
-- Revision 1.1  2003/03/11 08:30:04  holsti
-- First version renamed to be a child of Programs.Execution.
-- Normalized some lay-out and commenting.
--


with Ada.Strings.Unbounded;
with Decoder;
with Drawing.Dot;
with File_System;
with Flow;
with Flow.Calls;
with Flow.Computation;
with Flow.Execution;
with Flow.Execution.Times;
with Arithmetic;
with Loops;
with Processor;
with Programs.Execution.Draw.Opt;
with Programs.Execution.Tables;
with Options.Strings;
with Output;
with Programs;
with Symbols;
with Version_Id;

with Ada.Strings;
with Ada.Strings.Fixed;


package body Programs.Execution.Draw is


   function "*" (
      Left  : Tables.Count_Range_T;
      Right : Tables.Time_Range_T)
   return Tables.Time_Range_T
   --
   -- Interval product.
   --
   is
   begin

      if Tables.Is_Null (Left)
      or Tables.Is_Null (Right)
      then

         return Tables.No_Time_Range;

      else

         return (
            Min => Left.Min * Right.Min,
            Max => Left.Max * Right.Max);

      end if;

   end "*";


   --
   -- Note that the drawing operations are carefully implemented
   -- to work whether or not the drawn subprograms are partly
   -- or fully bounded. At any point, the Execution Bounds may
   -- be fully bounded, partly bounded, or not bounded at all.
   -- However, they should always be present (not be No_Bounds).
   --
   -- The existence of partial or full bounds merely adds to the
   -- information (labelling and emphasis) in the graphs.
   --


   --
   ---   Image functions etc. for general things
   --


   function Trim (Item : String) return String
   --
   -- Remove leading and trailing blanks.
   --
   is
   begin

      return Ada.Strings.Fixed.Trim (
         Source => Item,
         Side   => Ada.Strings.Both);

   end Trim;


   function Image (Item : Natural) return String
   is
   begin

      return Trim (Natural'Image (Item));

   end Image;


   function Image (Item : Loops.Loop_Index_T) return String
   is
   begin

      return Trim (Loops.Loop_Index_T'Image (Item));

   end Image;


   function Image (Item : Bounds_Index_T) return String
   is
   begin

      return Trim (Bounds_Index_T'Image (Item));

   end Image;


   function Image (Item : Processor.Time_T) return String
   renames Output.Image;


   function Image (Item : Tables.Count_Range_T) return String
   is
      use type Flow.Execution.Count_T;
   begin

      if Tables.Is_Null (Item) then

         return "";

      elsif Item.Min = Item.Max then

         return Image (Item.Min);

      else

         return Image (Item.Min) & " .. " & Image (Item.Max);

      end if;

   end Image;


   function Image (Item : Tables.Time_Range_T) return String
   is
      use type Processor.Time_T;
   begin

      if Tables.Is_Null (Item) then

         return "";

      elsif Item.Min = Item.Max then

         return Image (Item.Min);

      else

         return Image (Item.Min) & " .. " & Image (Item.Max);

      end if;

   end Image;


   function Ular (Number : Natural; Kind : String)
   return String
   --
   -- Singular or plural form of "Number Kind(s)".
   --
   is
   begin

      if Number = 1 then

         return "one " & Kind;

      else

         return Image (Number) & ' ' & Kind & 's';

      end if;

   end Ular;


   function Name_Qual (Subprogram : Subprogram_T) return String
   --
   -- The name of the Subprogram, with optional scope qualification.
   --
   is
   begin

      return Name (Subprogram, Qualified => Opt.Qualify_Names);

   end Name_Qual;


   function Calls_Image (Item : Tables.Share_T) return String
   --
   -- A string that describes the number of calls and paths
   -- to these calls that are included in a share.
   --
   -- The usual form is
   --
   --   <calls> calls from <counted paths> of <all paths> paths
   --
   -- Special cases:
   --
   -- > If there are no counted paths, the form is
   --
   --     <all paths> paths
   --
   -- > If all paths are counted, the form is
   --
   --     <calls> calls from <all paths> paths
   --
   is
   begin

      if Item.Counted_Paths = 0 then

         return Ular (Item.Link_Paths, "path");

      elsif Item.Counted_Paths = Item.Link_Paths then

         return
              Ular (Item.Calls, "call")
            & " from "
            & Ular (Item.Counted_Paths, "path");

       else

         return
              Ular (Item.Calls, "call")
            & " from "
            & Image (Item.Counted_Paths)
            & " of "
            & Ular (Item.Link_Paths, "path");

      end if;

   end Calls_Image;


   function Time_Image (Item : Tables.Share_T) return String
   --
   -- A string that describes the total execution time, the
   -- number of bounded calls, and the time per call, that are
   -- included in the share.
   --
   -- The usual form is
   --
   --    time <total time> = <bounded calls> * <time per call>
   --
   -- where <time per call> is usually <min> .. <max>.
   --
   -- Special cases:
   --
   -- > If <time per call> is null, use <time range> instead (ie.
   --   including all time bounds, even if there are no calls).
   --
   -- > If there is exactly one bounded path and one bounded call,
   --   the form is
   --
   --     time <total time>
   --
   -- > If there are no bounded paths, but there are timed paths,
   --   the form is
   --
   --     time per call <time per call>
   --
   -- > If there are no bounded paths and no timed paths, the
   --   result is null.
   --
   is

      Time_Range : Tables.Time_Range_T;
      -- The time-range to be shown.

   begin

      if Tables.Is_Null (Item.Time_Per_Call) then
         -- The bounds in this share are not on the worst-case path.
         -- Show the full time-range of all bounds, if any.

         Time_Range := Item.Time;

      else
         -- The bounds in this share are on the worst-case path.
         -- Show only the time-range for the bounds with a positive
         -- or unknown execution count.

         Time_Range := Item.Time_Per_Call;

      end if;


      if Item.Bounded_Paths = 1 and Item.Bounded_Calls = 1 then

         return "time " & Image (Item.Total_Time);

      elsif Item.Bounded_Paths > 0 then

         return
              "time "
            & Image (Item.Total_Time)
            & " = "
            & Image (Item.Bounded_Calls)
            & " * "
            & Image (Time_Range);

      elsif Item.Timed_Paths > 0 then

         return "time per call " & Image (Time_Range);

      else

         return "";

      end if;

   end Time_Image;


   procedure Comment_Version (File : in out Drawing.Dot.File_T)
   --
   -- Writes a DOT comment giving the version of Bound-T.
   --
   is
   begin

      Drawing.Dot.Begin_Comment_Block (File);

      Drawing.Dot.Put_Line (File,
           "DOT file generated by "
         & Version_Id
         & ",");

      Drawing.Dot.Put_Line (File,
           "a product of Tidorum Ltd, www.tidorum.fi.");

      Drawing.Dot.End_Comment_Block (File);

      Drawing.Dot.Put_Line (File, "");

   end Comment_Version;


   --
   ---   DOT file naming (for Opt.Separate_Files)
   --
   -- When separate DOT files are created for each drawing, the
   -- file-names are generated as follows:
   --
   -- > Call-graph of root subprogram: cg_<subprogram>_<nnn>.dot
   -- > Flow-graph of any  subprogram: fg_<subprogram>_<nnn>.dot
   --
   -- Here <nnn> is a sequential numbering of DOT files and the
   -- <subprogram> is the (unqualified) subprogram name, filtered
   -- to replace special characters with '_'.


   File_Seq_Number : Natural := 0;
   --
   -- The sequential number of the last DOT file.
   -- Incremented when a new file is opened.


   Zero_Pad_Seq : constant := 3;
   --
   -- The sequence-number part of a DOT file name is padded on the
   -- left with zeros to this length, if originally shorter.


   function Next_File_Seq return String
   --
   -- The next File_Seq_Number. Also increments File_Seq_Number.
   --
   is
      use Ada.Strings, Ada.Strings.Fixed;

      Seq : constant String := Image (File_Seq_Number + 1);

   begin

      File_Seq_Number := File_Seq_Number + 1;

      if Seq'Length < Zero_Pad_Seq then

         return ((Zero_Pad_Seq - Seq'Length) * '0') & Seq;

      else

         return Seq;

      end if;

   end Next_File_Seq;


   function To_Filename_Char (From : Character) return Character
   --
   -- Translates From any character (that occurs in the name of a
   -- subprogram) to a character that can occur in a file-name (on
   -- any host platform).
   --
   -- This is a very conservative mapping.
   --
   is
   begin

      case From is

      when 'a' .. 'z'
         | 'A' .. 'Z'
         | '0' .. '9'
         | '_'
         | '.'
         | '-'    => return From;

      when others => return '_';

      end case;

   end To_Filename_Char;


   function To_Filename_Part (Name : String)
   return String
   --
   -- Filters the given subprogram Name to a string of equal length
   -- but replacing special characters by innocuous characters so
   -- that the result can be used as part of a file-name (on any
   -- host platform).
   --
   is
   begin

      return Ada.Strings.Fixed.Translate (
         Source => Name,
         Mapping => To_Filename_Char'Access);

   end To_Filename_Part;


   procedure Create_Separate_File (
      File   : in out Drawing.Dot.File_T;
      Prefix : in     String;
      Name   : in     String)
   --
   -- Creates and opens a new File for a DOT drawing and gives it a
   -- name that puts it in the directory Opt.Dot_Dir. The name is
   -- constructed from Opt.Dot_Dir, the Prefix, the (filtered)
   -- Name, and File_Seq_Number (which is incremented).
   --
   is

      File_Name : constant String :=
         File_System.Path_Name (
            Path => Options.Strings.Value_Of (Opt.Dot_Dir),
            File =>
                 Prefix
               & '_'
               & To_Filename_Part (Name)
               & '_'
               & Next_File_Seq
               & ".dot");
      --
      -- The name of the file.

   begin

      Drawing.Dot.Create_File (File, File_Name);

      Comment_Version (File);

   end Create_Separate_File;


   --
   ---   Labeling nodes and edges
   --


   Edge_Indent : constant Natural := 2;
   Edge_Align  : constant Drawing.Dot.Align_T := Drawing.Dot.Left;
   --
   -- Parameters for edge labels.


   type Label_Text_T is record
      Text      : Drawing.Dot.Text_T;
      Last_Line : Ada.Strings.Unbounded.Unbounded_String;
   end record;
   --
   -- Text for a label, with a memory of the Last_Line added
   -- to the Text, so that we can elide duplicated lines.


   procedure Add_Left (
      Line   : in     String;
      Indent : in     Natural := 0;
      To     : in out Label_Text_T)
   --
   -- Adds a left-aligned Line To a label, if it is not a
   -- duplicate of the last line added to this label.
   --
   is
      use Ada.Strings.Unbounded;
   begin

      if Line /= To_String (To.Last_Line) then

         Drawing.Dot.Add (
            Line   => Line,
            Indent => Indent,
            Align  => Drawing.Dot.Left,
            To     => To.Text);

         To.Last_Line := To_Unbounded_String (Line);

      end if;

   end Add_Left;


   procedure Add_Edge (
      Line : in     String;
      To   : in out Drawing.Dot.Text_T)
   --
   -- Adds a Line To an edge label.
   --
   -- Edge labels are left aligned, indented a few spaces.
   --
   is
   begin

      Drawing.Dot.Add (
         Line   => Line,
         Indent => Edge_Indent,
         Align  => Edge_Align,
         To     => To);

   end Add_Edge;


   procedure Draw_Header (
      File       : in out Drawing.Dot.File_T;
      Prefix     : in     String;
      Graph_Name : in     String;
      File_Name  : in     String)
   --
   -- Writes the file-header for DOT files, setting some formatting options.
   --
   -- If Opt.Separate_Files is true, we create the File with the file-name
   -- defined by the File_Name and the Prefix and write the header.
   -- Otherwise (common file) we just write the header to the File, which
   -- we assume to be open already.
   --
   is
   begin

      if Opt.Separate_Files then

         Create_Separate_File (
            File   => File,
            Prefix => Prefix,
            Name   => File_Name);

      end if;

      -- A comment at the start:

      Drawing.Dot.Begin_Comment_Block (File);

      Drawing.Dot.Put_Line (File, "Graph : " & Graph_Name);

      Drawing.Dot.End_Comment_Block (File);

      -- Define the graph and set its attributes:

      Drawing.Dot.Begin_Graph ("g", File);

      if       Opt.Dot_Size.Set
      and then Options.Strings.Value_Of (Opt.Dot_Size)'Length > 0
      then
         -- The user has specified the drawing size.

         Drawing.Dot.Set_Size (File, Options.Strings.Value_Of (Opt.Dot_Size));

      else
         -- Drawing size was not specified.

         Drawing.Dot.Set_Ratio (File, "auto");

      end if;

      if       Opt.Dot_Page.Set
      and then Options.Strings.Value_Of (Opt.Dot_Page)'Length > 0
      then
         -- The user has specified pagination to a given page-size.

         Drawing.Dot.Set_Page (File, Options.Strings.Value_Of (Opt.Dot_Page));

      end if;

      Drawing.Dot.Set_Graph_Attribute (
         File  => File,
         Name  => "fontname",
         Value => "Helvetica");

   end Draw_Header;


   procedure Draw_Footer (File : in out Drawing.Dot.File_T)
   --
   -- Writes the file-footer for DOT files, completing the structure
   -- opened by Draw_Header.
   --
   -- If Opt.Separate_Files is true, we write the footer and close the File.
   -- Otherwise (common file) we just write the footer and leave the File
   -- open.
   --
   is
   begin

      Drawing.Dot.End_Graph (File);

      if Opt.Separate_Files then

         Drawing.Dot.Close (File);

      end if;

   end Draw_Footer;


   procedure Set_Font (Attributes : in out Drawing.Dot.Attributes_T)
   --
   -- Sets the common font attributes.
   --
   is
   begin

      Drawing.Dot.Add (
         Name  => "fontname",
         Value => "Helvetica",
         To    => Attributes);

      Drawing.Dot.Add (
         Name  => "fontsize",
         Value => "8",
         To    => Attributes);

   end Set_Font;


   --
   ---   Common operations for call-graphs and bounds-graphs
   --


   procedure Label_Node (
      Subprogram : in     Subprogram_T;
      Path       : in     Call_Path_T;
      Share      : in     Tables.Share_T;
      Self_Time  : in     Processor.Time_T;
      Attribs    : in out Drawing.Dot.Attributes_T)
   --
   -- Adds a label showing the Subprogram, its Share of the execution
   -- and its Self_Time, to the Attributes of a DOT node that represents
   -- the Subprogram or some execution bounds for the Subprogram.
   --
   is
      use type Processor.Time_T;

      Label : Label_Text_T;
      -- The text for the label.

   begin

      Drawing.Dot.Add (
         Line => Name_Qual (Subprogram),
         To   => Label.Text);

      if Path'Length > 0 then
         -- Context-specific bounds; show the path:

         Drawing.Dot.Add (
            Line => Image (Path),
            To   => Label.Text);

      end if;

      Add_Left (
         Line => Calls_Image (Share),
         To   => Label);

      Add_Left (
         Line => Time_Image (Share),
         To   => Label);

      if       Share.Bounded_Calls > 0
      and then Self_Time /= Share.Total_Time
      then

         Add_Left (
            Line =>
                 "self "
               & Image (Self_Time)
               & ", callees "
               & Image (Share.Total_Time - Self_Time),
            To => Label);

      end if;

      if Share.Asserted_Paths > 0 then

         Add_Left (
            Line =>
                 Ular (Share.Asserted_Paths, "path")
               & " with asserted time",
            To => Label);

      end if;

      Drawing.Dot.Add (
         Item => Drawing.Dot.Label (Label.Text),
         To   => Attribs);

   end Label_Node;


   procedure Draw_Edge (
      From  : in     String;
      To    : in     String;
      Share : in     Tables.Share_T;
      File  : in out Drawing.Dot.File_T)
   --
   -- Draws a DOT edge between two DOT nodes, From and To, and labels
   -- it with its execution Share.
   --
   is

      Label : Drawing.Dot.Text_T;
      -- The text for the label.

      Attribs : Drawing.Dot.Attributes_T;
      -- The DOT attributes for the edge.

   begin

      Add_Edge (
         Line  => Calls_Image (Share),
         To    => Label);

      Add_Edge (
         Line  => Time_Image (Share),
         To    => Label);

      Drawing.Dot.Add (
         Item => Drawing.Dot.Label (Label),
         To   => Attribs);

      Drawing.Dot.Define_Edge (
         From       => From,
         To         => To,
         Attributes => Attribs,
         File       => File,
         Indent     => 3);

   end Draw_Edge;


   --
   ---   Drawing call graphs
   --


   procedure Set_Call_Graph_Attributes (File : in Drawing.Dot.File_T)
   --
   -- Sets the default node and edge attributes for call-graphs.
   --
   is

      Attr : Drawing.Dot.Attributes_T;
      -- The default attributes for nodes and edges.

   begin

      -- For edges:

      Set_Font (Attr);

      Drawing.Dot.Set_Edge_Defaults (
         File       => File,
         Attributes => Attr);

      -- More for nodes:

      Drawing.Dot.Add (
         Name  => "height",
         Value => "0.3",
         To    => Attr);

      Drawing.Dot.Add (
         Name  => "width",
         Value => "0.5",
         To    => Attr);

      Drawing.Dot.Add (
         Name  => "shape",
         Value => "box",
         To    => Attr);

      Drawing.Dot.Set_Node_Defaults (
         File       => File,
         Attributes => Attr);

   end Set_Call_Graph_Attributes;


   procedure Set_Call_Graph_Label (
      Kind : in String;
      Root : in Bounds_Ref;
      File : in Drawing.Dot.File_T)
   --
   -- Sets the label of a call-graph.
   --
   is

      Label : Drawing.Dot.Text_T;
      -- The label for the graph.

   begin

      Drawing.Dot.Add (
         Line =>
              Kind
            & " graph of "
            & Name_Qual (Subprogram (Root)),
         To   => Label);

      if not Is_Feasible (Root) then

         Drawing.Dot.Add (
            Line => "no feasible execution path",
            To   => Label);

      end if;

      if Time_Bounded (Root) then

         Drawing.Dot.Add (
            Line => "time " & Image (Time (Root)),
            To   => Label);

      end if;

      Drawing.Dot.Set_Graph_Label (File, Label);

   end Set_Call_Graph_Label;


   function Node_Name (Sub : Subprogram_T) return String
   --
   -- The name of the DOT node for a given Subprogram.
   --
   is
   begin

      return 'n' & Image (Natural (Index (Sub)));

   end Node_Name;


   procedure Draw_Call_Graph (
      Root : in     Bounds_Ref;
      File : in out Drawing.Dot.File_T)
   --
   -- Draws a call-graph diagram for the closure of the Root subprogram,
   -- decorating it with the path and time bounds represented in
   -- the Root bounds.
   --
   -- The resulting diagram has the following properties:
   --
   -- > One node (rectangle) per subprogram in the closure.
   --   All the occurrences (execution bounds) for this subprogram
   --   are represented by this one node.
   --
   -- > One arc per caller-callee relationship in the closure. All
   --   call sites and call executions from the caller to the callee
   --   are represented by this node.
   --
   -- > Subprograms marked to be "hidden" in call-graph drawings
   --   are omitted, as are all calls to and from them.
   --
   is

      -- Principle of Operation:
      --
      -- First , the Subprogram_Table of the closure is traversed to
      -- generate the DOT definitions of the nodes.
      --
      -- Then, the Call_Table of the closure is traversed to generate
      -- the DOT definitions of the call arcs.

      Subprograms : constant Tables.Subprogram_Table_T :=
         Tables.Subprogram_Table (Root);
      -- Tabulated execution times and frequencies for all subprograms
      -- in the Root closure.

      Calls : constant Tables.Call_Table_T :=
         Tables.Call_Table (Root);
      -- Tabulated execution times and frequencies for all calls in
      -- the Root closure.


      function Hide (Call : Tables.Call_Row_T) return Boolean
      --
      -- Whether the Call should be hidden (omitted) in the drawing.
      --
      is
      begin

         return Hide_In_Call_Graph_Drawing (Call.Caller)
             or Hide_In_Call_Graph_Drawing (Call.Callee);

      end Hide;


      procedure Draw_Subprogram (Row : in Tables.Subprogram_Row_T)
      --
      -- Draws one node (subprogram) and its labels in DOT form.
      --
      is

         Attribs : Drawing.Dot.Attributes_T;
         -- The DOT attributes for the node.

      begin

         Label_Node (
            Subprogram => Row.Subprogram,
            Path       => Null_Call_Path,
            Share      => Row.Share,
            Self_Time  => Row.Self_Time,
            Attribs    => Attribs);

         Drawing.Dot.Define_Node (
            Name       => Node_Name (Row.Subprogram),
            Attributes => Attribs,
            File       => File,
            Indent     => 3);

      end Draw_Subprogram;


   begin  -- Draw_Call_Graph

      Draw_Header (
         File       => File,
         Prefix     => "cg",
         Graph_Name => Name_Qual (Subprogram (Root)),
         File_Name  => Name      (Subprogram (Root)));

      Set_Call_Graph_Attributes (File);

      Set_Call_Graph_Label (
         Kind => "Call",
         Root => Root,
         File => File);

      -- The nodes = subprograms:

      for S in Subprograms'Range loop

         if not Hide_In_Call_Graph_Drawing (Subprograms(S).Subprogram) then

            Draw_Subprogram (Subprograms(S));

         end if;

      end loop;

      -- The arcs = calls (or caller-callee pairs, actually):

      for C in Calls'Range loop

         if not Hide (Calls(C)) then

            Draw_Edge (
               From  => Node_Name (Calls(C).Caller),
               To    => Node_Name (Calls(C).Callee),
               Share => Calls(C).Share,
               File  => File);

         end if;

      end loop;

      -- We are done:

      Draw_Footer (File);

   end Draw_Call_Graph;


   --
   ---   Drawing execution-bounds graphs
   --


   procedure Draw_Bounds_Graph (
      Root   : in     Bounds_Ref;
      Within : in     Bounds_Set_T;
      File   : in out Drawing.Dot.File_T)
   --
   -- Draws a bounds-graph diagram for the closure of the Root subprogram,
   -- decorating it with the path and time bounds represented in
   -- the Root bounds.
   --
   -- The resulting diagram has the following properties:
   --
   -- > One node (rectangle) per bounding of a subprogram in the closure.
   --   Even boundings with the same result (in terms of execution time
   --   etc) are drawn as separate nodes if they are distinct objects
   --   (result from different analyses of the same subprogram).
   --
   -- > One arc per "link" between execution bounds, where the bounds
   --   represented by the source node depend on the bounds for a call,
   --   represented in the target node.
   --   The arc is labelled with the total number of times the call
   --   occurs, per execution of the Root.
   --
   -- > Subprograms marked to be "hidden" in call-graph drawings
   --   are omitted, as are all calls to and from them.
   --
   is

      -- Principle of Operation:
      --
      -- First , the Bounds_Table of the closure is traversed to
      -- generate the DOT definitions of the nodes.
      --
      -- Then, the Links_Table of each Bounds_Row is traversed to
      -- generate the DOT definitions of the call arcs.


      Bounds : constant Tables.Bounds_Table_T :=
         Tables.Bounds_Table (Root, Within);
      -- The summary execution share for each bounding under the Root.


      function Hide (Item : Bounds_Ref) return Boolean
      --
      -- Whether the given execution bounds should be hidden (omitted)
      -- in the drawing.
      --
      is
      begin

         return Hide_In_Call_Graph_Drawing (Subprogram (Item));

      end Hide;


      function Hide (Item : Tables.Links_Row_T) return Boolean
      --
      -- Whether the given link between bounds should be hidden (omitted)
      -- in the drawing.
      --
      is
      begin

         return Hide (Item.Caller) or Hide (Item.Callee);

      end Hide;


      function Node_Name (Bounds : Bounds_Ref) return String
      --
      -- The name of the DOT node for a given execution bounds item.
      --
      is
      begin

         return 'b' & Image (Natural (Index (Bounds)));

      end Node_Name;


      procedure Draw_Bounds (Row : in Tables.Bounds_Row_T)
      --
      -- Draws one node for the execution Bounds.
      --
      is

         Attribs : Drawing.Dot.Attributes_T;
         -- The DOT attributes for the node.

      begin

         Label_Node (
            Subprogram => Subprogram (Row.Bounds),
            Path       => Call_Path (Row.Bounds),
            Share      => Row.Share,
            Self_Time  => Row.Self_Time,
            Attribs    => Attribs);

         Drawing.Dot.Define_Node (
            Name       => Node_Name (Row.Bounds),
            Attributes => Attribs,
            File       => File,
            Indent     => 3);

      end Draw_Bounds;


   begin  -- Draw_Bounds_Graph

      Draw_Header (
         File       => File,
         Prefix     => "cg",
         Graph_Name => Name_Qual (Subprogram (Root)),
         File_Name  => Name      (Subprogram (Root)));

      Set_Call_Graph_Attributes (File);

      Set_Call_Graph_Label (
         Kind => "Bounds",
         Root => Root,
         File => File);

      -- Draw the nodes = boundings:

      for B in Bounds'Range loop

         if not Hide (Bounds(B).Bounds) then

            Draw_Bounds (Row => Bounds(B));

         end if;

      end loop;

      -- Draw the edges = links:

      for B in Bounds'Range loop

         declare

            Links : constant Tables.Links_Table_T :=
               Tables.Links (From => Bounds(B));

         begin

            for L in Links'Range loop

               if not Hide (Links(L)) then

                  Draw_Edge (
                     From  => Node_Name (Links(L).Caller),
                     To    => Node_Name (Links(L).Callee),
                     Share => Links(L).Share,
                     File  => File);

               end if;

            end loop;

         end;

      end loop;

      Draw_Footer (File);

   end Draw_Bounds_Graph;


   --
   ---   Drawing control-flow graphs
   --


   procedure Set_Flow_Graph_Attributes (File : in Drawing.Dot.File_T)
   --
   -- Sets the default node and edge attributes for flow-graphs.
   --
   is

      Attr : Drawing.Dot.Attributes_T;
      -- The default attributes for nodes and edges.

   begin

      -- For edges:

      Set_Font (Attr);

      Drawing.Dot.Set_Edge_Defaults (
         File       => File,
         Attributes => Attr);

      -- More for nodes:

      Drawing.Dot.Add (
         Name  => "height",
         Value => "0.3",
         To    => Attr);

      Drawing.Dot.Add (
         Name  => "width",
         Value => "0.5",
         To    => Attr);

      Drawing.Dot.Add (
         Name  => "shape",
         Value => "box",
         To    => Attr);

      Drawing.Dot.Set_Node_Defaults (
         File       => File,
         Attributes => Attr);

   end Set_Flow_Graph_Attributes;


   procedure Set_Flow_Graph_Label (
      Kind   : in String;
      Totals : in Tables.Total_Times_And_Counts_T;
      File   : in Drawing.Dot.File_T)
   --
   -- Sets the label of a flow-graph.
   --
   is
      use type Processor.Time_T;

      Label : Label_Text_T;
      -- The label for the flow-graph.

   begin

      Add_Left (
         Line =>
              Kind
            & " flow-graph of "
            & Name_Qual (Totals.Subprogram),
         To    => Label);

      if Totals.Share.Timed_Paths > Totals.Share.Asserted_Paths then
         -- Probably some execution counts/times will be shown,
         -- in the form <per call> ; <total>.

         Add_Left (
            Line  =>
                 "totals (after ';') for "
               & Calls_Image (Totals.Share),
            To    => Label);

      else
         -- Probably no totals will be shown, but the number of
         -- calls and paths is still interesting.

         Add_Left (
            Line => Calls_Image (Totals.Share),
            To   => Label);

      end if;

      if Totals.Share.Asserted_Paths > 0 then

         Add_Left (
            Line =>
                 Ular (Totals.Share.Asserted_Paths, "path")
               & " with asserted time",
            To   => Label);

      end if;

      if       Totals.Share.Bounded_Calls > 0
      and then Totals.Self_Time /= Totals.Share.Total_Time
      then
         -- Split total time into self + callees.

         Add_Left (
            Line =>
                 Time_Image (Totals.Share)
               & " = self "
               & Image (Totals.Self_Time)
               & " + callees "
               & Image (Totals.Share.Total_Time - Totals.Self_Time),
            To => Label);

      else
         -- No callee-time to show.

         Add_Left (
            Line  => Time_Image (Totals.Share),
            To    => Label);

      end if;

      Drawing.Dot.Set_Graph_Label (File, Label.Text);

   end Set_Flow_Graph_Label;


   function One_And_Total (
      One   : in String;
      Total : in String)
   return String
   --
   -- Combines a quantity per One call with the corresponding
   -- quantity for the Total over all (summarised) calls.
   -- However, shows only One if One = Total.
   --
   is
   begin

      if One = Total then

         return One;

      else

         return One & "; " & Total;

      end if;

   end One_And_Total;


   procedure Add_Count_And_Time (
      Summary : in     Tables.Total_Time_Count_T;
      Share   : in     Tables.Share_T;
      Count   : in     String;
      Indent  : in     Natural := 0;
      To      : in out Label_Text_T)
   --
   -- Adds decoration for the execution count and execution time of
   -- a node or edge in a flow-graph, assuming that some execution
   -- bounds with bounded times and counts (per node/edge) are included
   -- in the Summary.
   --
   -- The Share is the overall Share represented by the execution
   -- bounds that are summarised in the Summary.
   --
   -- The Count is the prefix to be put before the execution count,
   -- for example "count ".
   --
   is
      use type Processor.Time_T;

      Total_Callees : Processor.Time_T;
      -- Total time spent in callees.

   begin

      if Share.Counted_Paths > 0 then
         -- We know the number (or a number) of calls of this flow-graph,
         -- so the values Summary.Total_* are meaningful.

         if Opt.Count then

            Add_Left (
               Line =>
                    Count
                  & One_And_Total (
                       One   => Image (Summary.Count),
                       Total => Image (Summary.Total_Count)),
               Indent => Indent,
               To     => To);

         end if;

         if Summary.Time.Max > 0 and Opt.Time then

            Add_Left (
               Line =>
                    "time "
                  & One_And_Total (
                       One   => Image (Summary.Time),
                       Total => Image (Summary.Total_Time)),
               Indent => Indent,
               To     => To);

         end if;

         if Summary.Callees.Max > 0 and Opt.Time then
            -- Some time was spent in callees.

            Total_Callees := Summary.Total_Time - Summary.Total_Self;

            Add_Left (
               Line =>
                    "callees "
                  & One_And_Total (
                       One   => Image (Summary.Callees),
                       Total => Image (Total_Callees)),
               Indent => Indent,
               To     => To);

         end if;

      else
         -- The number of calls is not known; show only per-call info.

         if Opt.Count then

            Add_Left (
               Line   => Count & Image (Summary.Count),
               Indent => Indent,
               To     => To);

         end if;

         if Summary.Time.Max > 0 and Opt.Time then

            Add_Left (
               Line =>
                    "time "
                  & One_And_Total (
                       One   => Image (Summary.Time),
                       Total => Image (Summary.Count * Summary.Time)),
               Indent => Indent,
               To     => To);

         end if;

         if Summary.Callees.Max > 0 and Opt.Time then

            Add_Left (
               Line =>
                    "callees "
                  & One_And_Total (
                       One   => Image (Summary.Callees),
                       Total => Image (Summary.Count * Summary.Callees)),
               Indent => Indent,
               To     => To);

         end if;

      end if;

   end Add_Count_And_Time;


   function On_Worst_Case_Path (
      Summary : Tables.Total_Time_Count_T;
      Share   : Tables.Share_T)
   return Boolean
   --
   -- Whether the node or edge with the given Summary execution counts
   -- and times (summary taken over execution bounds with the given
   -- total Share) lies on the worst-case path.
   --
   is
   begin

      if Share.Counted_Paths > 0 then
         -- We know the number (or a number) of calls of this flow-graph,
         -- so the values Summary.Total_* are meaningful.

         return Summary.Total_Count > 0;

      else
         -- The number of calls is not known; show only per-call info.

         return Summary.Count.Max > 0;

      end if;

   end On_Worst_Case_Path;


   function Image (Item : Flow.Step_Index_T) return String
   is
   begin

      return Trim (Flow.Step_Index_T'Image (Item));

   end Image;


   function Image (Item : Flow.Node_Index_T) return String
   is
   begin

      return Trim (Flow.Node_Index_T'Image (Item));

   end Image;


   procedure Add_Step_Info (
      Step         : in     Flow.Step_T;
      Model        : in     Flow.Computation.Model_Ref;
      Symbol_Table : in     Symbols.Symbol_Table_T;
      To           : in out Label_Text_T)
   --
   -- Adds lot of optional information about the step To the label:
   --
   -- > subprogram and label symbols
   -- > source-line numbers
   -- > step address
   -- > decoded instruction
   -- > arithmetic effect.
   --
   is

      Step_Tag : constant Flow.Step_Tag_T := Flow.Tag (Step);
      -- The "tag" of the step.

      Code_Addr : constant Processor.Code_Address_T :=
         Flow.Prime_Address (Step_Tag);
      -- The prime code-addresses of the step.

      Conns : constant Symbols.Connection_Set_T :=
         Symbols.Connections_For_Address (Code_Addr, Symbol_Table);
      -- Connections of the step's code_address.

      procedure Append (Line : in String)
      --
      -- Append a line to the text, but not if it duplicates
      -- the preceding added line.
      --
      is
      begin

         Add_Left (Line => Line, To => To);

      end Append;


   begin   -- Add_Step_Info

      -- Subprogram and label names and source-line number(s):

      for C in Conns'Range loop

         case Symbols.Kind_Of(Conns(C)) is

         when Symbols.Subprogram | Symbols.Label =>

            if Opt.Symbol then

               Append (
                    Symbols.Image (Symbols.Scope_Of (Conns(C)))
                  & '.'
                  & Symbols.Name_Of (Conns(C)));

            end if;

         when Symbols.Line_Number =>

            if Opt.Line_Number then

               Append (
                    Output.Formed_Source (
                       Symbols.Source_File_Of (Conns(C)))
                  & ':'
                  & Symbols.Line_Number_Of (Conns(C)));

            end if;

         when others =>

            -- We are not interested in connections of the Cell-kind.

            null;

         end case;

      end loop;

      if Opt.Step_Tag then

         Append (Flow.Image (Step_Tag));

      end if;

      if Opt.Decode
      and then not Flow.Calls.Is_Call (Step)
      then

         Append (
              Processor.Image (Code_Addr)
            & ' '
            & Decoder.Decoded_Instruction (
                 Step    => Step,
                 Program => Flow.Computation.Program (Model)));

      end if;

      if Opt.Effect then

         Append (Arithmetic.Image (Flow.Computation.Effect (Step, Model)));

      end if;

   end Add_Step_Info;


   function Image (Item : Flow.Edge_Index_T) return String
   is
   begin

      return Trim (Flow.Edge_Index_T'Image (Item));

   end Image;


   function Image (Item : Flow.Step_Edge_Index_T) return String
   is
   begin

      return Trim (Flow.Step_Edge_Index_T'Image (Item));

   end Image;


   function Code_Address_Image (Steps : Flow.Step_List_T)
   return String
   --
   -- The primary code address range covered by the steps, in the
   -- form "[first-last]", or just "[first]" if first = last.
   -- Note that "first" may be greater than "last", and the step list
   -- can contain steps before, between or after "first" and "last",
   -- and does not have to contain all addresses between "first" and
   -- "last" even if first < last.
   --
   is
      use type Processor.Code_Address_T;

      First : constant Processor.Code_Address_T :=
         Flow.Prime_Address (Steps(Steps'First));
      -- The primary code address of the first step.

      Last : constant Processor.Code_Address_T :=
         Flow.Prime_Address (Steps(Steps'Last));
      -- The primary code address of the last step.

   begin

     if First = Last then

        return '[' & Processor.Image (First) & ']';

      else

        return
             '['
           & Processor.Image (First)
           & '-'
           & Processor.Image (Last )
           & ']';

      end if;

   end Code_Address_Image;


   procedure Draw_Flow_Graph (
      Kind         : in     String;
      Root         : in     Subprogram_T;
      Subprogram   : in     Subprogram_T;
      Bounds       : in     Tables.Bounds_Table_T;
      Symbol_Table : in     Symbols.Symbol_Table_T;
      File         : in out Drawing.Dot.File_T)
   --
   -- Draws the control flow graph for the Subprogram, decorating it
   -- with the summary totals of the execution Bounds.
   --
   -- Kind
   --    The kind of execution bounds to be drawn: minimum time,
   --    maximum time, totals, ...
   -- Root
   --    The root subprogram for which the call-closure is being drawn.
   -- Subprogram
   --    The subprogram for which the flow-graph should be drawn.
   --    May equal the Root.
   -- Bounds
   --    Some selection of execution bounds for the Subprogram, as they
   --    occur in the execution bounds for the Root, for decorating the
   --    flow-graph with execution times and counts.
   --    Precondition: Bounds'Length >= 1.
   -- Symbol_Table
   --    The symbol-table of the program.
   -- File
   --    The DOT file in which the drawing shall be stored.
   --
   -- Under Opt.Separate_Files, the File is created, written and closed.
   -- Otherwise (common file) we only write the drawing to the File, which
   -- we assume to be open already, and we also leave the File open.
   --
   -- The resulting diagram has the following properties:
   --
   -- > One initial node labelled with the call-path (if Bounds'Length = 1)
   --   or the name of the root subprogram (if Bounds'Length > 1) and the
   --   name of the subprogram, with an arc to the node that
   --   represents the entry point of the flow-graph.
   --
   -- > One node (rectangle) per node (basic block) (or per step, option).
   --
   -- > One arc per control-flow transition from node to node (or from
   --   step to step).
   --
   -- Nothing is drawn for stub subprograms or subprograms where all
   -- execution bounds are asserted.
   --
   is
      use type Processor.Time_T;

      use type Flow.Node_T;
      use type Flow.Step_T;
      use type Flow.Execution.Counts_Ref;
      use type Flow.Execution.Times.Node_Times_Ref;


      Init_Node_Name : constant String := "init";
      -- The DOT name to be used for the initial node.

      Totals : constant Tables.Total_Times_And_Counts_T :=
         Tables.Total_Times_And_Counts (
            Executing => Subprogram,
            Bounds    => Bounds);
      -- The total time and count, summarised over all the Bounds.

      Graph : constant Flow.Graph_T := Flow_Graph (Subprogram);
      -- The flow-graph to be drawn.

      Luups : constant Loops.Loops_T := Loops_Of (Subprogram);
      -- The loops in the flow-graph (empty if irreducible).

      Bounded : constant Natural := Totals.Share.Bounded_Calls;
      -- The number of execution bounds, summarised in Totals,
      -- for which the execution counts and times are known.
      -- If positive, we label the nodes and edges with the
      -- execution counts and times from Totals.

      Draw_Time_And_Count : constant Boolean :=
         (Opt.Time or Opt.Count) and Totals.Share.Timed_Paths > 0;
      -- Whether we should and can show the execution counts and/or
      -- times per node and edge.


      function Never_Feasible (Edge : Flow.Step_Edge_T) return Boolean
      --
      -- Whether the Edge is infeasible in all bounds drawn.
      --
      is
      begin

         for B in Bounds'Range loop

            if Flow.Computation.Is_Feasible (
               Edge  => Edge,
               Under => Computation(Bounds(B).Bounds).all)
            then
               -- The edge is feasible under these bounds.

               return False;

            end if;

         end loop;

         -- The edge is infeasible under all Bounds.

         return True;

      end Never_Feasible;


      function Name (Node : Flow.Node_T) return String
      --
      -- The DOT name of the DOT node that represents this Node.
      --
      is
      begin

         return 'n' & Image (Flow.Index (Node));

      end Name;


      function Name (Step : Flow.Step_T) return String
      --
      -- The DOT name of the DOT node that represents this Step.
      --
      is
      begin

         return 's' & Image (Flow.Index (Step));

      end Name;


      procedure Draw_Init_Node
      --
      -- Draws the initial DOT node.
      --
      is

         Path : constant Call_Path_T :=
            Call_Path (Bounds(Bounds'First).Bounds);
         -- The call-path to the first execution bounds.

         Label : Drawing.Dot.Text_T;
         -- The label.

         Attrib : Drawing.Dot.Attributes_T;
         -- The attributes.

      begin

         if Bounds'Length = 1 then
            -- Only one bounding, so show the call-path:

            for C in Path'Range loop

               Drawing.Dot.Add (
                  Line => Image (Path(C)),
                  To   => Label);

            end loop;

         elsif Subprogram /= Root then
            -- Many boundings. Show the root subprogram
            -- instead of the call-path(s):

            Drawing.Dot.Add (
               Line =>
                    Name_Qual (Root)
                  & "=> ..."
                  & Ular (Totals.Share.Link_Paths, "path"),
               To => Label);

         end if;

         Drawing.Dot.Add (
            Line => Name_Qual (Subprogram),
            To   => Label);

         if Opt.Index then
            -- Show the indices of the execution bounds:

            for B in Bounds'Range loop

               Drawing.Dot.Add (
                  Line => "bounds #" & Image (Index (Bounds(B).Bounds)),
                  To   => Label);

            end loop;

         end if;

         Drawing.Dot.Add (
            Item => Drawing.Dot.Label (Label),
            To   => Attrib);

         Drawing.Dot.Add (
            Name  => "shape",
            Value => "plaintext",
            To    => Attrib);

         Drawing.Dot.Define_Node (
            Name       => Init_Node_Name,
            Attributes => Attrib,
            File       => File,
            Indent     => 3);

      end Draw_Init_Node;


      procedure Draw_Init_Edge (To : in String)
      --
      -- Draws the edge from the initial DOT node To the entry-point
      -- of the real flow-graph, defined as the DOT name of the
      -- entry-point DOT node.
      --
      is

         Label : Drawing.Dot.Text_T;
         -- The label.

         Attrib : Drawing.Dot.Attributes_T;
         -- The attributes.

      begin

         if Totals.Share.Counted_Paths > 0 then

            Add_Edge (
               Line => Ular (Totals.Share.Calls, "call"),
               To   => Label);

            if Totals.Share.Calls > 0 then

               Drawing.Dot.Add (
                  Name  => "style",
                  Value => "bold",
                  To    => Attrib);

            end if;

         end if;

         Drawing.Dot.Add (
            Item => Drawing.Dot.Label (Label),
            To   => Attrib);

         Drawing.Dot.Define_Edge (
            From       => Init_Node_Name,
            To         => To,
            Attributes => Attrib,
            File       => File,
            Indent     => 3);

      end Draw_Init_Edge;


      procedure Draw_Node (Node : in Flow.Node_T)
      --
      -- Draws the DOT node that represents this flow-graph Node.
      --
      is
         use type Flow.Node_T;
         use type Processor.Time_T;

         Steps : constant Flow.Step_List_T := Flow.Steps_In (Node);
         -- All steps in the node.

         Call : Programs.Call_T;
         -- The possible call in Steps(Steps'First).

         Label : Label_Text_T;
         -- The annotation text, initially null.

         Attribs : Drawing.Dot.Attributes_T;
         -- The attributes for the node.

      begin

         if Opt.Index then

            Drawing.Dot.Add (
               Line => "node #" & Image (Flow.Index (Node)),
               To   => Label.Text);

         end if;

         -- Mark loop-head nodes with the loop index:

         for L in Luups'Range loop

            if Loops.Head_Node (Luups(L)) = Node then

               Drawing.Dot.Add (
                  Line => "loop #" & Image (L),
                  To   => Label.Text);

            end if;

         end loop;

         -- Summary info over all steps:

         if Opt.Code_Address then

            Drawing.Dot.Add (
               Line => Code_Address_Image (Steps),
               To   => Label.Text);

         end if;

         -- Info for each step:

         for S in Steps'Range loop

            Add_Step_Info (
               Step         => Steps(S),
               Model        => Computation (Bounds(Bounds'First).Bounds).all,
               Symbol_Table => Symbol_Table,
               To           => Label);

         end loop;

         -- Possible call:

         Call := Flow.Calls.Call_In (Steps(Steps'First));

         if Call /= No_Call then

            Drawing.Dot.Add (
               Line => "call " & Name_Qual (Callee (Call)),
               To   => Label.Text);

         end if;

         -- Execution counts and times:

         if Draw_Time_And_Count then

            Add_Count_And_Time (
               Summary => Totals.Node(Flow.Index (Node)),
               Share   => Totals.Share,
               Count   => "count ",
               To      => Label);

         end if;

         -- And that is the whole label:

         Drawing.Dot.Add (
            Item => Drawing.Dot.Label (Label.Text),
            To   => Attribs);

         -- Define the node:

         Drawing.Dot.Define_Node (
            Name       => Name (Node),
            Attributes => Attribs,
            File       => File,
            Indent     => 3);

      end Draw_Node;


      procedure Draw_Edge (Edge : in Flow.Edge_T)
      --
      -- Draws the DOT edge that represents this flow-graph Edge.
      --
      is

         Edge_Summary : Tables.Total_Time_Count_T;
         -- The execution summary for this Edge.

         Label : Label_Text_T;
         -- The text for the label,

         Attribs : Drawing.Dot.Attributes_T;
         -- The DOT attributes of the edge.

      begin

         if Opt.Index then

            Add_Edge (
               Line => "edge #" & Image (Flow.Index (Edge)),
               To   => Label.Text);

         end if;

         if Opt.Condition then
            -- Show arithmetic condition of edge:

            Add_Edge (
               Line => Arithmetic.Image (Flow.Condition (Edge)),
               To   => Label.Text);

         end if;

         Edge_Summary := Totals.Edge(Flow.Index (Edge));

         if Draw_Time_And_Count then

            Add_Count_And_Time (
               Summary => Edge_Summary,
               Share   => Totals.Share,
               Count   => "",
               Indent  => Edge_Indent,
               To      => Label);

          end if;

         if On_Worst_Case_Path (Edge_Summary, Totals.Share) then
            -- This edge is on (some) worst-case path.

            Drawing.Dot.Add (
               Name  => "style",
               Value => "bold",
               To    => Attribs);

         elsif Never_Feasible (Flow.Step_Edge (Edge)) then

            Drawing.Dot.Add (
               Name  => "style",
               Value => "dotted",
               To    => Attribs);

         end if;

         Drawing.Dot.Add (
            Item => Drawing.Dot.Label (Label.Text),
            To   => Attribs);

         Drawing.Dot.Define_Edge (
            From       => Name (Flow.Source (Edge)),
            To         => Name (Flow.Target (Edge)),
            Attributes => Attribs,
            File       => File,
            Indent     => 3);

      end Draw_Edge;


      procedure Draw_Edges (From : in Flow.Node_T)
      --
      -- Draws the DOT edges that represent all flow-graph edges
      -- leaving From the given node.
      --
      is

         Edges : constant Flow.Edge_List_T := Flow.Edges_From (From, Graph);
         -- All the edges leaving From the node.

      begin

         for E in Edges'Range loop

            Draw_Edge (Edges(E));

         end loop;

      end Draw_Edges;


      procedure Draw_Step (Step : in Flow.Step_T)
      is
         use type Flow.Step_T;

         Label : Label_Text_T;
         -- The annotation text, initially null.

         Call : Programs.Call_T;
         -- The call, if this is a call-step.

         Step_Info : Processor.Step_Info_T := Flow.Info (Step);
         -- The step's information.

         Attribs : Drawing.Dot.Attributes_T;
         -- The DOT attributes for the step (DOT node).

      begin

         if Opt.Index then

            Drawing.Dot.Add (
               Line => "step #" & Image (Flow.Index (Step)),
               To   => Label.Text);

         end if;

         -- Mark loop-head nodes with the loop index:

         for L in Luups'Range loop

            if Loops.Head_Step (Luups(L)) = Step then

               Drawing.Dot.Add (
                  Line => "loop #" & Image (L),
                  To   => Label.Text);

            end if;

         end loop;

         -- Summary info over all (well, just this one) steps:

         if Opt.Code_Address then

            Drawing.Dot.Add (
               Line => Code_Address_Image ((1 => Step)),
               To   => Label.Text);

         end if;

         -- Possible call:

         Call := Flow.Calls.Call_In (Step);

         if Call /= No_Call then

            Drawing.Dot.Add (
               Line => "call " & Name_Qual (Callee (Call)),
               To   => Label.Text);

         end if;

         -- Info for the step:

         Add_Step_Info (
            Step         => Step,
            Model        => Computation (Bounds(Bounds'First).Bounds).all,
            Symbol_Table => Symbol_Table,
            To           => Label);

         -- Execution counts and times:

         if Opt.Time then
            -- Add the step's effort, since the actual WCET is hard to get.
            -- (We would need the assertion_map for the memory wait states).

            Drawing.Dot.Add (
               Line =>
                    "effort "
                  & Processor.Image (Processor.Effort (Step_Info)),
               To => Label.Text);

         end if;

         -- And that is the whole label:

         Drawing.Dot.Add (
            Item => Drawing.Dot.Label (Label.Text),
            To   => Attribs);

         Drawing.Dot.Define_Node (
            Name       => Name (Step),
            Attributes => Attribs,
            File       => File,
            Indent     => 3);

      end Draw_Step;


      function Step_Edge_Count (Edge : Flow.Step_Edge_T)
      return Flow.Execution.Count_T
      --
      -- The number of times the edge between steps is executed
      -- in the Totals.
      --
      is

         Target_Node : constant Flow.Node_T :=
            Flow.Node_Containing (Flow.Target (Edge), Graph);

         Source_Node : constant Flow.Node_T :=
            Flow.Node_Containing (Flow.Source (Edge), Graph);

         Node_Edge : Flow.Edge_T;
         -- The corresponding node-edge, if there is one.

      begin

         if Source_Node = Target_Node then
            -- The edge is within a node, so the edge is executed
            -- as often as the node.

            return Totals.Node(Flow.Index (Source_Node)).Total_Count;

         else
            -- The edge corresponds to an edge between nodes,
            -- so the execution-count was computed in the
            -- worst-case path computation.

            Node_Edge := Flow.Node_Edge (Edge, Graph);

            return Totals.Edge(Flow.Index (Node_Edge)).Total_Count;

         end if;

      end Step_Edge_Count;


      procedure Draw_Step_Edge (Edge : in Flow.Step_Edge_T)
      is

         Edge_Count : Flow.Execution.Count_T;
         -- The execution count of the edge, if known.

         Label : Drawing.Dot.Text_T;
         -- The text for the label,

         Attribs : Drawing.Dot.Attributes_T;
         -- The DOT attributes of the edge.

      begin

         if Opt.Index then

            Add_Edge (
               Line => "edge #" & Image (Flow.Index (Edge)),
               To   => Label);

         end if;

         if Opt.Condition then
            -- Show arithmetic condition of edge:

            Add_Edge (
               Line => Arithmetic.Image (Flow.Condition (Edge)),
               To   => Label);

         end if;

         if Opt.Time and then Flow.Time (Edge) /= 0 then
            -- Show the execution time of the edge:

            Add_Edge (
               Line => "time " & Image (Flow.Time (Edge)),
               To   => Label);

         end if;

         if Bounded > 0 then
            -- We know the execution counts and times.

            Edge_Count := Step_Edge_Count (Edge);

            -- Show just the count since the time is hard to get.

            if Opt.Count then

               Add_Edge (
                  Line => Image (Edge_Count),
                  To   => Label);

            end if;

            if Edge_Count > 0 then
               -- This edge is on (some) worst-case path.

               Drawing.Dot.Add (
                  Name  => "style",
                  Value => "bold",
                  To    => Attribs);

            elsif Never_Feasible (Edge) then

               Drawing.Dot.Add (
                  Name  => "style",
                  Value => "dotted",
                  To    => Attribs);

            end if;

         end if;

         Drawing.Dot.Add (
            Item => Drawing.Dot.Label (Label),
            To   => Attribs);

         Drawing.Dot.Define_Edge (
            From       => Name (Flow.Source (Edge)),
            To         => Name (Flow.Target (Edge)),
            Attributes => Attribs,
            File       => File,
            Indent     => 3);

      end Draw_Step_Edge;


      procedure Draw_Step_Edges (From : in Flow.Step_T)
      --
      -- Draws the DOT edges that represent all flow-graph edges
      -- leaving From the given step.
      --
      is

         Edges : constant Flow.Step_Edge_List_T :=
            Flow.Edges_From (From, Graph);
         -- All the edges leaving From the step.

      begin

         for E in Edges'Range loop

            Draw_Step_Edge (Edges(E));

         end loop;

      end Draw_Step_Edges;


   begin  -- Draw_Flow_Graph

      if      Stub (Subprogram)
      or else Totals.Share.Link_Paths <= Totals.Share.Asserted_Paths
      then
         -- Only asserted bounds, so the flow-graph would be a stub.
         -- No point in drawing it.

         Output.Note (
            Locus => Locus (Subprogram),
            Text  => "Flow-graphs not drawn (stub or asserted bounds).");

      else
         -- Some interesting execution bounds to show.

         Draw_Header (
            File       => File,
            Prefix     => "fg",
            Graph_Name => Name_Qual (Subprogram),
            File_Name  => Name      (Subprogram));

         Set_Flow_Graph_Attributes (File);

         Set_Flow_Graph_Label (Kind, Totals, File);

         Draw_Init_Node;

         if Opt.Step_Graph then
            -- Draw each step (instruction).

            for Index in 1 .. Flow.Max_Step (Graph) loop

               Draw_Step (Flow.Step_At (Index, Graph));

            end loop;

            Draw_Init_Edge (To => Name (Flow.Entry_Step (Graph)));

            for Index in 1 .. Flow.Max_Step (Graph) loop

               Draw_Step_Edges (From => Flow.Step_At (Index, Graph));

            end loop;

         else
            -- Draw nodes (basic blocks).

            for Index in 1 .. Flow.Max_Node (Graph) loop

               Draw_Node (Flow.Node_At (Index, Graph));

            end loop;

            Draw_Init_Edge (To => Name (Flow.Entry_Node (Graph)));

            for Index in 1 .. Flow.Max_Node (Graph) loop

               Draw_Edges (From => Flow.Node_At (Index, Graph));

            end loop;

         end if;

         Draw_Footer (File);

      end if;

   end Draw_Flow_Graph;


   function Drawable_Subprograms_Under (
      Root   : Bounds_Ref;
      Within : Bounds_Set_T)
   return Subprogram_List_T
   --
   -- The subprograms, under the given Root bounds, for which
   -- some kind of control-flow graphs should be drawn.
   --
   is
   begin

      if Opt.Deeply then
         -- Draw all subprograms under Root.

         return Tables.Subprograms_Under (Root, Within);

      else
         -- Draw only the Root subprogram.

         return (1 => Subprogram (Root));

      end if;

   end Drawable_Subprograms_Under;


   procedure Draw_Flow_Graphs (
      Root   : in     Bounds_Ref;
      Within : in     Bounds_Set_T;
      File   : in out Drawing.Dot.File_T;
      Drawn  : in out Bounds_Subset_T)
   --
   -- Draws the control-flow graphs of the Root subprogram, and optionally
   -- of all its callees. For each subprogram, we may do one or more of
   -- the following:
   --
   -- 1. Draw separate flow-graphs for each bounding of this subprogram
   --    within the call-closure of the Root, decorating each flow-graph
   --    with the corresponding execution bounds.
   --
   -- 2. Like (1) but include only boundings that are, or might be, part
   --    of the overall worst-case execution, if not already drawn in (1).
   --
   -- 3. Draw a flow-graph decorated with the minimum bounding (least
   --    time-bound) of this subprogram within the call-closure of the Root,
   --    if not already drawn in (1 .. 2).
   --
   -- 4. Draw a flow-graph decorated with the maximum bounding (greatest
   --    time-bound) of this subprogram within the call-closure of the Root,
   --    if not already drawn in (1 .. 3).
   --
   -- 5. Draw a flow-graph decorated with the cumulated total execution
   --    counts and execution times of this subprogram, over all
   --    executions of this subprogram in the Root bounds, unless the
   --    same information was drawn already in (1 .. 4).
   --
   -- The procedure draws flow-graphs of the first three forms only for
   -- execution-bounds that are not yet in Drawn, and updates Drawn to
   -- contain all execution bounds that have been drawn in this way.
   --
   -- Flow-graphs that would be decorated only with asserted execution
   -- times are not drawn at all.
   --
   is

      Subprograms : constant Subprogram_List_T :=
         Drawable_Subprograms_Under (Root, Within);
      -- All the subprograms in the call-closure of the Root bounds,
      -- including the Root itself, for which control-flow graphs
      -- should be drawn.


      procedure Draw_Flow_Bounds (
         Kind : in String;
         Row  : in Tables.Bounds_Row_T)
      --
      -- Draws a flow-graph decorated with a single counted bounding,
      -- if not already drawn.
      --
      is

         Bounds : constant Bounds_Ref := Row.Bounds;
         -- The execution bounds themselves.

      begin

         if Bounds /= No_Bounds
         and then not Is_Member (Item => Bounds, Of_Set => Drawn)
         then

            Draw_Flow_Graph (
               Kind         => Kind,
               Root         => Subprogram (Root),
               Subprogram   => Subprogram (Bounds),
               Bounds       => (1 => Row),
               Symbol_Table => Symbol_Table (Root),
               File         => File);

            Add (Item => Bounds, To => Drawn);

         end if;

      end Draw_Flow_Bounds;


      procedure Draw_Flow (Subprogram : in Subprogram_T)
      --
      -- Draws the flow-graph(s) for this Subprogram.
      --
      is

         Bounds : constant Tables.Bounds_Table_T :=
            Tables.All_Bounds_For (
               Subprogram => Subprogram,
               Under_Root => Root,
               Within     => Within);
         -- All execution bounds for this Subprogram that are used
         -- in the Root bounds (closure), together with the share
         -- of each bounding in the Root bounds.

      begin

         -- Draw the Choices:

         if Opt.All_Bounds
         or Opt.Used_Bounds
         then

            for B in Bounds'Range loop

               if Opt.All_Bounds
               or Bounds(B).Share.Counted_Paths = 0
               or Bounds(B).Share.Calls > 0
               then
                  -- Either we are showing all bounds, or we don't
                  -- know how the execution count of these bounds so
                  -- they might be part of the worst-case execution,
                  -- or we know that they have a positive count in
                  -- that execution.

                  Draw_Flow_Bounds (
                     Kind => "One",
                     Row  => Bounds(B));

                end if;

            end loop;

         end if;

         if Opt.Min_Time then

            Draw_Flow_Bounds (
               Kind => "Minimum WCET",
               Row  => Tables.Min_Time (Bounds));

         end if;

         if Opt.Max_Time then

            Draw_Flow_Bounds (
               Kind => "Maximum WCET",
               Row  => Tables.Max_Time (Bounds));

         end if;

         if Opt.Total_Bounds
         and not (
            Bounds'Length = 1
            and then Is_Member (
               Item   => Bounds(Bounds'First).Bounds,
               Of_Set => Drawn) )
         then
            -- We have been asked to draw the "total" execution, and
            -- either there is more than one bounding (Bounds'Length > 1)
            -- or the single bounding is still not drawn.

            Draw_Flow_Graph (
               Kind         => "Summary",
               Root         => Programs.Execution.Subprogram (Root),
               Subprogram   => Subprogram,
               Bounds       => Bounds,
               Symbol_Table => Symbol_Table (Root),
               File         => File);

         end if;

      end Draw_Flow;


   begin  -- Draw_Flow_Graphs

      for S in Subprograms'Range loop

         Draw_Flow (Subprograms(S));

      end loop;

   end Draw_Flow_Graphs;


   --
   ---   The master operation
   --


   procedure Draw_Graphs (Bounds_Set : in Bounds_Set_T)
   is

      File : Drawing.Dot.File_T;
      -- The DOT file, here created and written.

      Drawn : Bounds_Subset_T (Max_Size => Number_Of_Bounds (Bounds_Set));
      -- Collects the execution-bounds for which flow graphs have
      -- drawn, under some root subprogram. Avoids multiple drawings
      -- of the same execution bounds, under the same or different
      -- root subprogram.

      Roots : constant Call_Bounds_List_T := Root_Bounds (Bounds_Set);
      -- All the root calls and their universal execution bounds.

      Some_Bounds : Boolean := False;
      -- Whether some Root has non-null bounds.

      Root : Bounds_Ref;
      -- Execution bounds for a root subprogram.

   begin  -- Draw_Graphs

      -- See if there are some bounds:

      for R in Roots'Range loop

         Some_Bounds := Some_Bounds or Roots(R).Bounds /= No_Bounds;

      end loop;

      -- Possibly open the single Dot file:

      if Some_Bounds and not Opt.Separate_Files then
         -- Create one file to rule them all.

         Drawing.Dot.Create_File (
            File => File,
            Name => Options.Strings.Value_Of (Opt.Dot_File));

         Comment_Version (File);

      end if;

      for R in Roots'Range loop

         Root := Roots(R).Bounds;

         if Root = No_Bounds then
            -- This root has no execution bounds at all.

            Output.Note (
               Locus => Locus (Roots(R).Call),
               Text  => "No execution bounds, nothing drawn.");

         else
            -- This root has some execution bounds, but they
            -- may not be fully bounded.

            if Opt.Bounds_Graph then

               Draw_Bounds_Graph (
                  Root   => Root,
                  Within => Bounds_Set,
                  File   => File);

            else

               Draw_Call_Graph (
                  Root => Root,
                  File => File);

            end if;

            Draw_Flow_Graphs (
               Root   => Root,
               Within => Bounds_Set,
               File   => File,
               Drawn  => Drawn);

         end if;

      end loop;

      Drawing.Dot.Close (File);

   exception

   when Drawing.Dot.File_Problem =>

      Drawing.Dot.Close (File);

   when X : others =>

      Output.Exception_Info (
         Text       => "Drawing graphs",
         Occurrence => X);

      raise;

   end Draw_Graphs;


   procedure Draw_Recursion_Graph (Program : in Program_T)
   is

      File : Drawing.Dot.File_T;
      -- The DOT file, here created and written.

      Subprograms : constant Subprogram_List_T :=
         Programs.Subprograms (Program);
      -- All the subprograms currently under analysis, including the
      -- root subprograms.

      Calls : constant Call_List_T := Programs.Calls (Program);
      -- All the calls between the subprograms currently under analysis,
      -- omitting the root calls (from "no subprogram" to a root).
      -- Note that there may be several calls from a given caller
      -- to the same callee.

      type Call_Bundle_T is record
         Caller : Subprogram_T;
         Callee : Subprogram_T;
         Tally  : Positive;
      end record;
      -- A summary of all calls from a Caller to a Callee.

      Bundles : array (1 .. Calls'Length) of Call_Bundle_T;
      Last : Natural := 0;
      -- Bundles(1 .. Last) will contain the summaries of the calls
      -- between the subprograms, with a unique summary for each
      -- caller-callee pair.


      procedure Bundle (Call : in Call_T)
      --
      -- Adds the Call to the Bundles.
      --
      is
      begin

         for B in 1 .. Last loop

            if  Bundles(B).Caller = Caller (Call)
            and Bundles(B).Callee = Callee (Call)
            then
               -- Add the call to this bundle.

               Bundles(B).Tally := Bundles(B).Tally + 1;

               return;

            end if;

         end loop;

         -- This Call is a new caller-callee pair.

         Last := Last + 1;

         Bundles(Last) := (
            Caller => Caller (Call),
            Callee => Callee (Call),
            Tally  => 1);

      end Bundle;


      procedure Set_Label
      --
      -- Sets the graph label.
      --
      is

         Label : Drawing.Dot.Text_T;
         -- The label for the graph.

      begin

         Drawing.Dot.Add (
            Line => "Joint call graph of all roots",
            To   => Label);

         Drawing.Dot.Set_Graph_Label (File, Label);

      end Set_Label;


      procedure Draw_Subprogram (Subprogram : in Subprogram_T)
      --
      -- Draws one node (subprogram) and its labels in DOT form.
      --
      is

         Label : Drawing.Dot.Text_T;
         -- The text for the label.

         Attribs : Drawing.Dot.Attributes_T;
         -- The DOT attributes for the node.

      begin

         Drawing.Dot.Add (
            Line => Name_Qual (Subprogram),
            To   => Label);

         Drawing.Dot.Add (
            Item => Drawing.Dot.Label (Label),
            To   => Attribs);

         Drawing.Dot.Define_Node (
            Name       => Node_Name (Subprogram),
            Attributes => Attribs,
            File       => File,
            Indent     => 3);

      end Draw_Subprogram;


      procedure Draw_Calls (Bundle : in Call_Bundle_T)
      --
      -- Draws a bundle of calls from a caller to a callee.
      --
      is

         Label : Drawing.Dot.Text_T;
         -- The text for the label.

         Attribs : Drawing.Dot.Attributes_T;
         -- The DOT attributes for the edge.

      begin

         Drawing.Dot.Add (
            Line => Image (Bundle.Tally),
            To   => Label);

         Drawing.Dot.Add (
            Item => Drawing.Dot.Label (Label),
            To   => Attribs);

         Drawing.Dot.Define_Edge (
            From       => Node_Name (Bundle.Caller),
            To         => Node_Name (Bundle.Callee),
            Attributes => Attribs,
            File       => File,
            Indent     => 3);

      end Draw_Calls;


   begin  -- Draw_Recursion_Graph

      if not Opt.Separate_Files then
         -- Create one file to rule them all.

         Drawing.Dot.Create_File (
            File => File,
            Name => Options.Strings.Value_Of (Opt.Dot_File));

      end if;

      Draw_Header (
         File       => File,
         Prefix     => "jcg",
         Graph_Name => "All roots",
         File_Name  => "all_roots");

      Set_Call_Graph_Attributes (File);

      Set_Label;

      -- Draw the subprograms:

      for S in Subprograms'Range loop

         Draw_Subprogram (Subprograms(S));

      end loop;

      -- Summarise the calls:

      for C in Calls'Range loop

         Bundle (Calls(C));

      end loop;

      -- Draw the bundled calls:

      for B in 1 .. Last loop

         Draw_Calls (Bundles(B));

      end loop;

      Draw_Footer (File);

      Drawing.Dot.Close (File);

   exception

   when Drawing.Dot.File_Problem =>

      Drawing.Dot.Close (File);

   end Draw_Recursion_Graph;


end Programs.Execution.Draw;
