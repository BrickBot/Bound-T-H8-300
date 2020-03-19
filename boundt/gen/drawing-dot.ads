-- Drawing.Dot (decl)
--
-- Drawing graph diagrams in the DOT form.
--
-- This package provides some types and operations that help to write
-- definitions of graphs (nodes and edges) in the DOT form. However, they
-- do not entirely hide the fact that DOT files are text files, and they
-- are not complete; a client must probably still use Text_IO, too.
--
-- See: www.graphviz.org.
--
-- Author: Niklas Holsti, Tidorum Ltd
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
-- $Date: 2015/10/24 19:36:48 $
--
-- $Log: drawing-dot.ads,v $
-- Revision 1.4  2015/10/24 19:36:48  niklas
-- Moved to free licence.
--
-- Revision 1.3  2007-06-06 19:52:09  niklas
-- Added No_Attributes.
--
-- Revision 1.2  2005/10/09 08:10:21  niklas
-- BT-CH-0013.
--
-- Revision 1.1  2005/09/23 10:55:26  niklas
-- First version, for BT-CH-0012.
--


with Ada.Strings.Unbounded;
with Ada.Text_IO;


package Drawing.Dot is


   subtype File_T is Ada.Text_IO.File_Type;
   --
   -- A text file that contains DOT text.


   File_Problem : exception;
   --
   -- A fatal problem while trying to create (open) a DOT file.


   procedure Create_File (
      File : in out File_T;
      Name : in     String);
   --
   -- Creates and opens a new File for DOT drawing(s) with the
   -- given Name. Catches and reports IO exceptions and propagates
   -- File_Problem if such occur.


   procedure Close (File : in out File_T);
   --
   -- Closes the (completed) DOT File, if it is still open.


   procedure Begin_Comment_Block (File : in File_T);
   --
   -- Starts (opens) a multi-line comment block.
   -- The next text lines written to the File will not be interpreted
   -- as DOT definitions, until the next End_Comment call (unless, of
   -- course, the text lines contain the character sequence that
   -- closes a DOT comment.


   procedure End_Comment_Block (File : in File_T);
   --
   -- Ends (closes) a multi-line comment block that was begun (opened)
   -- by the closest preceding call of Begin_Comment_Block.


   procedure Put_Line (File : in File_T; Text : in String);
   --
   -- Writes a line to the DOT file. This is primarily meant for
   -- writing comment lines into a comment block, that is, between
   -- a call of Begin_Comment_Block and a call of End_Comment_Block.


   --
   --    Attributes of graphs, nodes and edges
   --


   type Text_T is new Ada.Strings.Unbounded.Unbounded_String;
   --
   -- A text that consists of zero or more lines using the DOT
   -- convention to separate lines.
   -- Such text is usually meant for node/edge labels.


   type Align_T is (Left, Center, Right);
   --
   -- The alignment of a line within a text.


   procedure Add (
      Line   : in     String;
      Indent : in     Natural := 0;
      Align  : in     Align_T := Center;
      To     : in out Text_T);
   --
   -- Appends the line to the text, but only if the line is not null.


   type Attributes_T is new Ada.Strings.Unbounded.Unbounded_String;
   --
   -- A list of DOT attributes, separated by commas.


   No_Attributes : constant Attributes_T := Attributes_T (
      Ada.Strings.Unbounded.Null_Unbounded_String);
   --
   -- A void set of attributes.


   procedure Add (
      Item : in     String;
      To   : in out Attributes_T);
   --
   -- Adds an attribute (Item) To a list of attributes.


   procedure Add (
      Name  : in     String;
      Value : in     String;
      To    : in out Attributes_T);
   --
   -- Adds an attribute setting, Name=Value, To a list of attributes.


   function Label (Item : String) return String;
   --
   -- The DOT text to set the "label" attribute to the given Item.


   function Label (Item : Text_T) return String;
   --
   -- The DOT text (attribute item) to set the "label" attribute to
   -- the given Item.


   --
   --    Defining graphs
   --


   procedure Begin_Graph (
      Name : in String;
      File : in File_T);
   --
   -- Begins the definition of a graph with the given Name, within
   -- the given DOT File.


   procedure End_Graph (File : in File_T);
   --
   -- Ends the definition of the graph that was begun by the closest
   -- preceding Begin_Graph, within the given DOT File.


   procedure Set_Node_Defaults (
      File       : in File_T;
      Attributes : in Attributes_T);
   --
   -- Writes the DOT text to set the default Attributes of all nodes.
   -- To be done between Begin_Graph and End_Graph.


   procedure Set_Edge_Defaults (
      File       : in File_T;
      Attributes : in Attributes_T);
   --
   -- Writes the DOT text to set the default Attributes of all edges.
   -- To be done between Begin_Graph and End_Graph.


   procedure Set_Graph_Attribute (
      File  : in File_T;
      Name  : in String;
      Value : in String);
   --
   -- Writes the DOT text to set the attribute of the given Name
   -- to the given Value, for the current graph.
   -- To be done between Begin_Graph and End_Graph.


   procedure Set_Size (File : in File_T; Size : in String);
   --
   -- Writes the DOT text to set the size of a drawing.
   -- The Size string shall not be quoted.
   -- To be done between Begin_Graph and End_Graph.


   procedure Set_Page (File : in File_T; Page : in String);
   --
   -- Writes the DOT text to set the page-size of a drawing.
   -- The Page string shall not be quoted.
   -- To be done between Begin_Graph and End_Graph.


   procedure Set_Ratio (File : in File_T; Ratio : in String);
   --
   -- Writes the DOT text to set the height/width ratio of a drawing.
   -- The Ratio string shall not be quoted.
   -- To be done between Begin_Graph and End_Graph.


   procedure Set_Graph_Label (File : in File_T; Label : in Text_T);
   --
   -- Writes the DOT text to set the label of a drawing.
   -- To be done between Begin_Graph and End_Graph.


   --
   --    Drawing nodes and edges
   --


   procedure Define_Node (
      Name       : in String;
      Attributes : in Attributes_T;
      File       : in File_T;
      Indent     : in Natural);
   --
   -- Writes the DOT text to define a node with the given Name
   -- and the given Attributes, to the File, with the given
   -- Indentation.


   procedure Define_Edge (
      From       : in String;
      To         : in String;
      Attributes : in Attributes_T;
      File       : in File_T;
      Indent     : in Natural);
   --
   -- Writes the DOT text to define an edge From a given node (name)
   -- To a given node (name), with the given Attributes, to the File,
   -- with the given Indentation.


end Drawing.Dot;
