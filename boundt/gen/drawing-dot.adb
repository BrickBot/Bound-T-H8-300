-- Drawing.Dot (body)
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
-- $Revision: 1.3 $
-- $Date: 2015/10/24 20:05:47 $
--
-- $Log: drawing-dot.adb,v $
-- Revision 1.3  2015/10/24 20:05:47  niklas
-- Moved to free licence.
--
-- Revision 1.2  2005-10-09 08:10:21  niklas
-- BT-CH-0013.
--
-- Revision 1.1  2005/09/23 10:55:26  niklas
-- First version, for BT-CH-0012.
--


with Ada.Strings.Fixed;

with Output;


package body Drawing.Dot is


   procedure Create_File (
      File : in out File_T;
      Name : in     String)
   is
   begin

      Output.Note ("Creating DOT file " & Name & '.');

      Ada.Text_IO.Create (
         File => File,
         Name => Name,
         Mode => Ada.Text_IO.Out_File);

   exception

   when Ada.Text_IO.Name_Error
      | Ada.Text_IO.Use_Error =>

      Output.Error (
           "Cannot create DOT file named """
         & Name
         & '"');

      raise File_Problem;

   end Create_File;


   procedure Close (File : in out File_T)
   is
   begin

      if Ada.Text_IO.Is_Open (File) then

         Ada.Text_IO.Close (File);

      end if;

   end Close;


   procedure Begin_Comment_Block (File : in File_T)
   is
   begin

      Ada.Text_IO.Put_Line (File, "/*");

   end Begin_Comment_Block;


   procedure End_Comment_Block (File : in File_T)
   is
   begin

      Ada.Text_IO.Put_Line (File, "*/");

   end End_Comment_Block;


   procedure Put_Line (File : in File_T; Text : in String)
   renames Ada.Text_IO.Put_Line;


   --
   --    Attributes of nodes and edges
   --


   subtype Line_Separator_T is String(1 .. 2);
   --
   -- A DOT line-separator is a two-character string.


   Line_Separator : constant array (Align_T) of Line_Separator_T := (
      Left   => "\l",
      Center => "\n",
      Right  => "\r");
   --
   -- The line separator that aligns the terminated line in
   -- a given way.


   procedure Add (
      Line   : in     String;
      Indent : in     Natural := 0;
      Align  : in     Align_T := Center;
      To     : in out Text_T)
   is
      use Ada.Strings.Fixed;
   begin

      if Line'Length > 0 then

         if Indent > 0 then

            Append (
               Source   => To,
               New_Item => String'(Indent * ' '));

         end if;

         Append (
            Source   => To,
            New_Item => Line);

         Append (
            Source   => To,
            New_Item => Line_Separator (Align));

      end if;

   end Add;


   procedure Add (
      Item : in     String;
      To   : in out Attributes_T)
   --
   -- Adds an attribute (Item) To a list of attributes.
   --
   is
   begin

      if Length (To) > 0 then
         -- Already some attributes, need a separating comma.

         Append (Source => To, New_Item => ',');

      end if;

      Append (Source => To, New_Item => To_Unbounded_String (Item));

   end Add;


   procedure Add (
      Name  : in     String;
      Value : in     String;
      To    : in out Attributes_T)
   is
   begin

      Add (Item => Name & '=' & Value, To => To);

   end Add;


   function Label (Item : String) return String
   is
   begin

      return "label=""" & Item & '"';

   end Label;


   function Label (Item : Text_T) return String
   is
   begin

      return Label (Item => To_String (Item));

   end Label;


   --
   --    Defining graphs
   --


   procedure Begin_Graph (
      Name : in String;
      File : in File_T)
   is
   begin

      Ada.Text_IO.Put_Line (File, "digraph " & Name & " {");

   end Begin_Graph;


   procedure End_Graph (File : in File_T)
   is
   begin

      Ada.Text_IO.Put_Line (File, "}");

   end End_Graph;


   procedure Set_Node_Defaults (
      File       : in File_T;
      Attributes : in Attributes_T)
   is
   begin

      Ada.Text_IO.Put_Line (File,
         "   node [" & To_String (Attributes) & "];");

   end Set_Node_Defaults;


   procedure Set_Edge_Defaults (
      File       : in File_T;
      Attributes : in Attributes_T)
   is
   begin

      Ada.Text_IO.Put_Line (File,
         "   edge [" & To_String (Attributes) & "];");

   end Set_Edge_Defaults;


   procedure Set_Graph_Attribute (
      File  : in File_T;
      Name  : in String;
      Value : in String)
   is
   begin

      Ada.Text_IO.Put_Line (File, "   " & Name & '=' & Value & ';');

   end Set_Graph_Attribute;


   function Quoted (Item : String) return String
   --
   -- Same with quotes on, mate.
   --
   is
   begin

      return '"' & Item & '"';

   end Quoted;


   procedure Set_Size (File : in File_T; Size : in String)
   is
   begin

      Set_Graph_Attribute (
         File  => File,
         Name  => "size",
         Value => Quoted (Size));

   end Set_Size;


   procedure Set_Page (File : in File_T; Page : in String)
   is
   begin

      Set_Graph_Attribute (
         File  => File,
         Name  => "page",
         Value => Quoted (Page));

   end Set_Page;


   procedure Set_Ratio (File : in File_T; Ratio : in String)
   is
   begin

      Set_Graph_Attribute (
         File  => File,
         Name  => "ratio",
         Value => Ratio);

   end Set_Ratio;


   procedure Set_Graph_Label (File : in File_T; Label : in Text_T)
   is
   begin

      Set_Graph_Attribute (
         File  => File,
         Name  => "label",
         Value => Quoted (To_String (Label)));

   end Set_Graph_Label;


   --
   --    Drawing nodes and edges
   --


   procedure Define_Node (
      Name       : in String;
      Attributes : in Attributes_T;
      File       : in File_T;
      Indent     : in Natural)
   is
      use Ada.Text_IO;
   begin

      Set_Col (File, Positive_Count (Indent + 1));

      Put (File, Name);

      if Length (Attributes) > 0 then

         Put (File, " [");
         Put (File, To_String (Attributes));
         Put (File, ']');

      end if;

      Put_Line (File, ";");

   end Define_Node;


   procedure Define_Edge (
      From       : in String;
      To         : in String;
      Attributes : in Attributes_T;
      File       : in File_T;
      Indent     : in Natural)
   is
      use Ada.Text_IO;
   begin

      Set_Col (File, Positive_Count (Indent + 1));

      Put (File, From & " -> " & To);

      if Length (Attributes) > 0 then

         Put (File, " [");
         Put (File, To_String (Attributes));
         Put (File, ']');

      end if;

      Put_Line (File, ";");

   end Define_Edge;


end Drawing.Dot;
