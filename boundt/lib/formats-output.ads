-- Formats.Output (decl)
--
-- Output of notes, warnings and errors related to reading and
-- parsing executable files in various formats.
--
-- This package is a simpler form of the Bound-T standard Output
-- package, intended for stand-alone use within the Formats family.
-- This package does not depend on the general Bound-T parts (such
-- as the symbol-table package Symbols) nor on the target-processor
-- specific parts (such as the Processor package). This limits the
-- ways in which the "locus" of an error or other output can be
-- defined.
--
-- The actual output lines generated by this package are compatible
-- with the ones from Standard.Output ("basic output format") but
-- many "locus" fields will be empty.
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
-- $Date: 2015/10/24 20:53:54 $
--
-- $Log: formats-output.ads,v $
-- Revision 1.4  2015/10/24 20:53:54  niklas
-- Moved to free licence.
--
-- Revision 1.3  2014/06/20 19:43:58  niklas
-- Added Trim, renaming Basic_Output.Trim.
--
-- Revision 1.2  2005-10-26 14:03:20  niklas
-- Added procedure No_Program_File.
--
-- Revision 1.1  2005/10/26 13:46:47  niklas
-- First version.
--


with Ada.Exceptions;
with Ada.Text_IO;

with Basic_Output;


package Formats.Output is


   Field_Separator : Character renames Basic_Output.Field_Separator;
   --
   -- The character that separates fields in basic-format output lines.


   --
   ---   Locus setting
   --


   procedure Set_Program_File (To : in String);
   --
   -- Defines the default "Program File" field, overriding any
   -- previous default value.
   --
   -- The initial default value is (of course) null.


   procedure No_Program_File;
   --
   -- Erases the default "Program File" field, making it null
   -- until set again.


   --
   ---   Basic output operations
   --


   procedure Line (
      Channel      : in Ada.Text_IO.File_Type;
      Key          : in String;
      Program_File : in String;
      Source_File  : in String;
      Call_Path    : in String;
      Statements   : in String;
      Data         : in String)
   renames Basic_Output.Line;
   --
   -- Emits a line in the basic format, with all fields given
   -- as parameters. "Data" contains field 6 .. N, with embedded
   -- field separators.


   procedure Note (
      Text  : in String);
   --
   -- Outputs a "Note" line.


   procedure Warning (
      Text  : in String);
   --
   -- Outputs a "Warning" line.


   procedure Error (
      Text  : in String);
   --
   -- Outputs an "Error" line.


   procedure Fault (
      Location : in String;
      Text     : in String);
   --
   -- Outputs a "Fault" line, to signal an internal fault in Bound-T
   -- detected in the Bound-T package or subprogram "Location".


   procedure Trace (
      Text : in String);
   --
   -- Outputs a "Trace" line, to report some on-the-fly execution
   -- tracing information.


   procedure Exception_Info (
      Text       : in String;
      Occurrence : in Ada.Exceptions.Exception_Occurrence);
   --
   -- Outputs an "Exception" line, which is like an error, but
   -- includes more fields giving the exception info.


   --
   ---   Supporting utility operations
   --


   function Trim (Item : String) return String
   renames Basic_Output.Trim;
   --
   -- The given string with leading and trailing blanks removed.


   function Image (Item : Integer) return String
   renames Basic_Output.Image;
   --
   -- A trimmed, base-10 formatting of the number.
   -- Same as Integer'Image but with no leading blank.


end Formats.Output;
