-- File_System (body) for Unix.
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
-- $Revision: 1.5 $
-- $Date: 2015/10/25 07:28:15 $
--
-- $Log: file_system.adb,v $
-- Revision 1.5  2015/10/25 07:28:15  niklas
-- Moved to free licence.
--
-- Revision 1.4  2007-09-22 13:24:34  niklas
-- Added function Is_Readable_Text.
--
-- Revision 1.3  2007/09/22 08:39:23  niklas
-- Extended function Path_Name to remove trailing path
-- separators from the Path before adding the File name.
-- Added functions With_New_Suffix and With_No_Suffix.
--
-- Revision 1.2  2006/01/23 20:00:56  niklas
-- Added function File_Name.
--
-- Revision 1.1  2005/09/20 19:32:52  niklas
-- First version, for BT-CH-0011.
--


with Ada.Strings;
with Ada.Strings.Fixed;
with Ada.Strings.Maps;
with Ada.Text_IO;


package body File_System is


   Path_Delimiters : Ada.Strings.Maps.Character_Set :=
      Ada.Strings.Maps.To_Set ("/\");
   --
   -- The characters that delimit directories and file-names in
   -- a path-name string. Note that the backslash ('\') is included,
   -- although not used as such in Unix/Linux systems.


   function Path_Name (
      Path : String;
      File : String)
   return String
   is
      use Ada.Strings;
      use Ada.Strings.Fixed;

      Last_Name : constant Natural := Index (
         Source => Path,
         Set    => Path_Delimiters,
         Test   => Outside,
         Going  => Backward);
      -- The index, in Path, of the last valid file-name character,
      -- that is a character other than a path delimiter. We omit
      -- the trailing delimiters to avoid duplicate delimiters
      -- in the result.

   begin

      return Path(Path'First .. Last_Name) & '/' & File;

   end Path_Name;


   function File_Name (Path : String) return String
   is
      use Ada.Strings;
      use Ada.Strings.Fixed;

      Last_Name : Natural;
      -- The index, in Path, of the last valid file-name character,
      -- that is a character other than a path delimiter.

      Last_Delimiter : Natural;
      -- The index, in Path, of the last path delimiter before
      -- the Last_Name character.

      First_Name : Positive;
      -- The index, in Path, of the first valid file-name character
      -- after Last_Delimiter.

   begin

      Last_Name := Index (
         Source => Path,
         Set    => Path_Delimiters,
         Test   => Outside,
         Going  => Backward);

      Last_Delimiter := Index (
         Source => Path(Path'First .. Last_Name),
         Set    => Path_Delimiters,
         Test   => Inside,
         Going  => Backward);

      if Last_Delimiter = 0 then
         -- There are no path delimiters before Last_Name.

         First_Name := Path'First;

      else
         -- There are some delimiters before Last_Name.

         First_Name := Last_Delimiter + 1;

      end if;

      return Path (First_Name .. Last_Name);

   end File_Name;


   function With_New_Suffix (
      Path   : String;
      Suffix : String)
   return String
   is
      use Ada.Strings;
      use Ada.Strings.Fixed;

      Last_Name : constant Natural := Index (
         Source => Path,
         Set    => Path_Delimiters,
         Test   => Outside,
         Going  => Backward);
      -- The index, in Path, of the last valid file-name character,
      -- that is a character other than a path delimiter.

      Path_Name : constant String := Path(Path'First .. Last_Name);
      -- The Path, with any trailing path-delimiters removed.

      Dot : constant Natural := Index (Path_Name, ".", Backward);
      -- The index of the last period in Path_Name, or zero
      -- if there is no period.

      Delim : constant Natural := Index (
         Source => Path_Name,
         Set    => Path_Delimiters,
         Test   => Inside,
         Going  => Backward);
      -- The index of the last path-delimiter in the Path, or zero
      -- if there is no path delimiter in the Path.

   begin

      if Dot > Delim then
         -- There is a suffix in the file-name part.

         return Path_Name(Path_Name'First .. Dot) & Suffix;

      else
         -- There is no suffix in the file-name part.

         return Path_Name & '.' & Suffix;

      end if;

   end With_New_Suffix;


   function With_No_Suffix (Path : String)
   return String
   is

      use Ada.Strings;
      use Ada.Strings.Fixed;

      Last_Name : constant Natural := Index (
         Source => Path,
         Set    => Path_Delimiters,
         Test   => Outside,
         Going  => Backward);
      -- The index, in Path, of the last valid file-name character,
      -- that is a character other than a path delimiter.

      Path_Name : constant String := Path(Path'First .. Last_Name);
      -- The Path, with any trailing path-delimiters removed.

      Dot : constant Natural := Index (Path_Name, ".", Backward);
      -- The index of the last period in Path_Name, or zero
      -- if there is no period.

      Delim : constant Natural := Index (
         Source => Path_Name,
         Set    => Path_Delimiters,
         Test   => Inside,
         Going  => Backward);
      -- The index of the last path-delimiter in the Path, or zero
      -- if there is no path delimiter in the Path.

   begin

      if Dot > Delim then
         -- There is a suffix in the file-name part.

         return Path_Name(Path_Name'First .. Dot - 1);

      else
         -- There is no suffix in the file-name part.

         return Path_Name;

      end if;

   end With_No_Suffix;


   function Is_Readable_Text (Path : String) return Boolean
   is
      use Ada.Text_IO;

      File : File_Type;

   begin

      Open (
         File => File,
         Mode => In_File,
         Name => Path);

      Close (File);

      return True;

   exception

   when Name_Error | Use_Error =>

      if Is_Open (File) then

         Close (File);

      end if;

      return False;

   end Is_Readable_Text;


end File_System;
