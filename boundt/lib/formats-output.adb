-- Formats.Output (body)
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
-- $Date: 2015/10/24 20:53:53 $
--
-- $Log: formats-output.adb,v $
-- Revision 1.3  2015/10/24 20:53:53  niklas
-- Moved to free licence.
--
-- Revision 1.2  2005-10-26 14:03:20  niklas
-- Added procedure No_Program_File.
--
-- Revision 1.1  2005/10/26 13:46:47  niklas
-- First version.
--


with Ada.Strings.Unbounded;


package body Formats.Output is


   use Ada.Strings.Unbounded;
   use Ada.Text_IO;


   --
   --   Locus setting
   --


   Program_File : Unbounded_String;
   --
   -- The default "Program File" locus field.
   -- Default initialized to a null string.


   procedure Set_Program_File (To : in String)
   is
   begin

      Program_File := To_Unbounded_String (To);

   end Set_Program_File;


   procedure No_Program_File
   is
   begin

      Program_File := Null_Unbounded_String;

   end No_Program_File;


   --
   --   Basic output operations
   --


   procedure Flush_Standard_Output
   renames Basic_Output.Flush_Standard_Output;


   procedure Line (
      Channel : in Ada.Text_IO.File_Type;
      Key     : in String;
      Data    : in String)
   --
   -- Emits an output line to the given Channel, showing the given
   -- Data with the given Key, with the current default Program File
   -- and nothing else as locus.
   --
   is
   begin

      Line (
         Channel      => Channel,
         Key          => Key,
         Program_File => To_String (Program_File),
         Source_File  => "",
         Call_Path    => "",
         Statements   => "",
         Data         => Data);

   end Line;


   procedure Note (
      Text  : in String)
   is
   begin

      if Basic_Output.Show_Notes then

         Line (
            Channel => Standard_Output,
            Key     => Basic_Output.Note_Key,
            Data    => Text);

      end if;

   end Note;


   procedure Warning (
      Text  : in String)
   is
   begin

      Flush_Standard_Output;

      Line (
         Channel => Standard_Output,
         Key     => Basic_Output.Warning_Key,
         Data    => Text);

   end Warning;


   procedure Error (
      Text  : in String)
   is
   begin

      Flush_Standard_Output;

      Line (
         Channel => Standard_Error,
         Key     => Basic_Output.Error_Key,
         Data    => Text);

   end Error;


   procedure Fault (
      Location : in String;
      Text     : in String)
   is
   begin

      Flush_Standard_Output;

      Line (
         Channel => Standard_Error,
         Key     => Basic_Output.Fault_Key,
         Data    => Text);

   end Fault;


   procedure Trace (
      Text : in String)
   is
   begin

      Flush_Standard_Output;

      Line (
         Channel => Standard_Output,
         Key     => Basic_Output.Trace_Key,
         Data    => Text);

   end Trace;


   procedure Exception_Info (
      Text       : in String;
      Occurrence : in Ada.Exceptions.Exception_Occurrence)
   is
      use Ada.Exceptions;
   begin

      Flush_Standard_Output;

      Line (
         Channel => Standard_Error,
         Key     => Basic_Output.Exception_Key,
         Data    => Text
                  & Field_Separator
                  & Exception_Information (Occurrence));

   end Exception_Info;


end Formats.Output;
