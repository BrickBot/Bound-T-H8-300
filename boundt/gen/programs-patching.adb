-- Programs.Patching (body)
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
-- $Date: 2015/10/24 20:05:51 $
--
-- $Log: programs-patching.adb,v $
-- Revision 1.4  2015/10/24 20:05:51  niklas
-- Moved to free licence.
--
-- Revision 1.3  2011-09-01 19:58:47  niklas
-- BT-CH-0222: Option registry.
--
-- Revision 1.2  2008-11-09 21:41:23  niklas
-- BT-CH-0158: Option "-trace patch".
--
-- Revision 1.1  2006/02/27 20:04:50  niklas
-- First version.
--


with Ada.Characters.Latin_1;
with Ada.Strings;
with Ada.Strings.Fixed;
with Ada.Strings.Maps;
with Ada.Text_IO;
with Decoder;
with Output;
with Processor;
with Processor.Properties;
with Symbols;


with Options;
with Options.Groups;
with Options.String_Sets;
with String_Pool;


package body Programs.Patching is


   --
   --    Patch file syntax and meaning
   --
   -- A patch file is a text file that is interpreted line by line as
   -- follows.
   --
   -- > Leading whitespace is ignored.
   --
   -- > A line starting with "--" (possibly with leading whitespace)
   --   is ignored (considered a comment line).
   --
   -- > Blank and null lines are ignored.
   --
   -- > Meaningful lines contain the following fields, in order,
   --   separated by whitespace:
   --
   --   - a Processor.Code_Address_T, to be interpreted by the
   --     function Processor.Properties.Subprogram_Address and
   --     denoting the starting address of the patch.
   --
   --   - a string without embedded whitespace, denoting the main
   --     content of the patch, to be interpreted by Decoder.Patch_Code.
   --
   --   - zero or more strings that represent code addresses or
   --     symbols connected to code addresses, to be interpreted
   --     by the function Processor.Properties.Subprogram_Address and
   --     denoting address parameters to Decoder.Patch_Code.
   --
   -- The patching process reads patch lines one by one, parses them
   -- as defined above, and calls Decoder.Patch_Code once per line (if
   -- parsing succeeds).


   Tab_To_Blank : constant Ada.Strings.Maps.Character_Mapping :=
      Ada.Strings.Maps.To_Mapping (
         From => (1 => Ada.Characters.Latin_1.HT),
         To   => (1 => ' '));
   --
   -- Translates tabs (HT) to blanks.


   Blank : constant Ada.Strings.Maps.Character_Set :=
      Ada.Strings.Maps.To_Set (' ');
   --
   -- The set of non-blank characters.


   procedure Apply_Patch_Line (
      Line    : in     String;
      Program : in     Program_T;
      Valid   : in out Boolean)
   --
   -- Applies a Line from a patch file to a Program, setting Valid to
   -- False if some error is detected. The Line is assumed to be
   -- stripped of leading and trailing whitespace.
   --
   is
      use Ada.Strings;
      use Ada.Strings.Fixed;

      F : constant Positive := Line'First;
      -- Just to mark the start.

      First : Positive := F;
      Last  : Natural  := F - 1;
      -- The current token is Line(First .. Last).

      Start_Address : Processor.Code_Address_T;
      -- The starting address of the patch.

      Data_First, Data_Last : Positive;
      -- The patch data string is Line(Data_First .. Data_Last).

      Max_Params : constant := 10;
      -- The maximum number of parameters allowed.

      Params : Natural := 0;
      -- The number of parameters supplied.

      Param : Code_Address_List_T (1 .. Max_Params);
      -- The parameters are Param(1 .. Params).

      Line_Valid : Boolean;
      -- Whether this line is a valid patch.


      procedure Get_Next_Token
      --
      -- Sets First..Last to show the next token.
      --
      is
      begin

         Find_Token (
            Source => Line(Last + 1 .. Line'Last),
            Set    => Blank,
            Test   => Outside,
            First  => First,
            Last   => Last);

      end Get_Next_Token;


      procedure Get_Parameter
      --
      -- Interprets the current token as a parameter, either a
      -- subprogram/label identifier or a code address.
      -- Sets Line_Valid to False on any error.
      --
      is

         Sub : Subprogram_T;
         -- The "subprogram" that is identified by the parameter.
         -- Note that it may be an arbitrary code address, not
         -- necessarily corresponding to a real subprogram.

      begin

         Identify (
            Identifier => Line(First .. Last),
            Delimiter  => Symbols.Default_Delimiter,
            Program    => Program,
            Subprogram => Sub);

         -- We have a valid address.

         if Params < Max_Params then

            Params := Params + 1;

            Param(Params) := Entry_Address (Sub);

         else

            Output.Error (
                 "At most"
               & Natural'Image (Max_Params)
               & " parameters allowed"
               & Output.Field_Separator
               & Line(First .. Last));

            Line_Valid := False;

          end if;

      exception

      when Subprogram_Not_Found =>

         Output.Error (
              "Patch parameter invalid"
            & Output.Field_Separator
            & Line(First .. Last));

         Line_Valid := False;

      end Get_Parameter;


   begin  -- Apply_Patch_Line

      if  Line'Length  = 0
      or (Line'Length >= 2 and then Line(F .. F + 1) = "--")
      then
         -- A blank or empty line or a comment line.

         null;

      else
         -- A line with at least one non-blank token.

         if Trace_Patching then

            Output.Trace (
                 "Applying patch line"
               & Output.Field_Separator
               & Line);

         end if;

         -- The patch start address:

         Get_Next_Token;

         Start_Address := Processor.Properties.Subprogram_Address (
            Line(First .. Last));

         -- The data string:

         Get_Next_Token;

         if Last = 0 then
            -- Missing patch string.

            Output.Error ("Patch data missing.");

            Valid := False;

         else

            Data_First := First;
            Data_Last  := Last;

            Line_Valid := True;
            -- So far, but we still have the parameters to check.

            -- Parameters if any:

            loop

               Get_Next_Token;

               exit when Last = 0;

               Get_Parameter;

            end loop;

            if Line_Valid then

               Decoder.Patch_Code (
                  Program => Program,
                  Address => Start_Address,
                  Data    => Line(Data_First .. Data_Last),
                  Params  => Param(1 .. Params),
                  Valid   => Line_Valid);

            end if;

            Valid := Valid and Line_Valid;

         end if;

      end if;

   exception

   when Processor.Properties.Address_Error =>

      Output.Error (
           "Patch address invalid"
         & Output.Field_Separator
         & Line(First .. Last));

      Valid := False;

   end Apply_Patch_Line;


   procedure Apply_Patch_File (
      File_Name : in     String;
      Program   : in     Program_T;
      Valid     : in out Boolean)
   --
   -- Applies the patches from the patch file with a given File_Name
   -- to a Program, setting Valid to False if some error is detected.
   --
   is
      use Ada.Strings;
      use Ada.Strings.Fixed;
      use type Ada.Text_IO.Positive_Count;

      File : Ada.Text_IO.File_Type;
      -- The file named File_Name.

      Max_Line_Length : constant := 300;
      -- The maximum length of a line in the file.

      Line : String (1 .. Max_Line_Length);
      Last : Natural;
      -- Line(1..Last) is the current line.

      Number : Output.Line_Number_T;
      -- The number of the current Line in the File.

      Mark : Output.Nest_Mark_T;
      -- Marks the locus File:Number.

   begin

      if Trace_Patching then

         Output.Trace (
              "Reading patches from "
            & File_Name
            & ".");

      end if;

      Ada.Text_IO.Open (
         File => File,
         Name => File_Name,
         Mode => Ada.Text_IO.In_File);

      while not Ada.Text_IO.End_Of_File (File) loop

         Number := Output.Line_Number_T (Ada.Text_IO.Line (File));

         Mark := Output.Nest (Output.Locus (
            Statements => Output."+" (Output.Locus (
               Source_File => File_Name,
               Line_Number => Number))));

         Ada.Text_IO.Get_Line (File, Line, Last);

         if Ada.Text_IO.Col (File) = 1 then
            -- Good, we read all of a line.

            Apply_Patch_Line (
               Line  => Trim (
                  Translate (Line(1 .. Last), Tab_To_Blank),
                  Both),
               Program => Program,
               Valid   => Valid);

         else

            Output.Error (
                 "Patch line too long (over "
               & Output.Image (Natural (Line'Length))
               & " characters).");

            Valid := False;

         end if;

         Output.Unnest (Mark);

      end loop;

      Ada.Text_IO.Close (File);

      if Trace_Patching then

         Output.Trace (
              "Finished patches from "
            & File_Name
            & ".");

      end if;

   exception

   when Ada.Text_IO.Name_Error =>

      Output.Error (
           "Could not open the patch file """
         & File_Name
         & """.");

      if Ada.Text_IO.Is_Open (File) then

         Ada.Text_IO.Close (File);

      end if;

      Valid := False;

   end Apply_Patch_File;


   procedure Apply_Patches (
      Program : in     Program_T;
      Valid   :    out Boolean)
   is

      Files : constant Options.String_Sets.String_List_T :=
         Options.File_Sets.To_List (Patch_Files);
      -- All the patch file names.

   begin

      Valid := True;

      for F in Files'Range loop

         Apply_Patch_File (
            File_Name => String_Pool.To_String (Files(F)),
            Program   => Program,
            Valid     => Valid);

      end loop;

   end Apply_Patches;


begin  -- Programs.Patching

   Options.Register (
      Option => Trace_Patching_Opt'access,
      Name   => Options.Trace_Item ("patch"),
      Groups => (Options.Groups.Inputs, Options.Groups.Trace));

   Options.Register (
      Option => Patch_Files'access,
      Name   => "patch",
      Group  => Options.Groups.Inputs);

end Programs.Patching;
