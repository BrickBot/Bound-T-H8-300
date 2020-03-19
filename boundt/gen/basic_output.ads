-- Basic_Output (decl)
--
-- Basic output format for results, notes, warnings and errors.
--
-- This package provides the common parts of the Bound-T basic output
-- functionality (package Output) and the simpler output functions in
-- the Formats library (package Formats.Output).
--
-- This package shall not depend on any other general Bound-T packages,
-- so that such dependencies in the Formats library are limited. The
-- only exception is the ESF package (Execution Skeleton File) that
-- this package (body) depends on, to copy warnings and errors into
-- the ESF as they occur.
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
-- $Revision: 1.5 $
-- $Date: 2015/10/24 19:36:47 $
--
-- $Log: basic_output.ads,v $
-- Revision 1.5  2015/10/24 19:36:47  niklas
-- Moved to free licence.
--
-- Revision 1.4  2012-01-19 20:13:56  niklas
-- BT-CH-0224: Device.Catalog added. Device options updated.
--
-- Revision 1.3  2011-08-31 04:23:33  niklas
-- BT-CH-0222: Option registry. Option -dump. External help files.
--
-- Revision 1.2  2007/10/26 12:44:34  niklas
-- BT-CH-0091: Reanalyse a boundable edge only if its domain grows.
--
-- Revision 1.1  2005/10/26 14:09:46  niklas
-- First version.
--


with Ada.Text_IO;


package Basic_Output is


   --
   ---   Options
   --


   Default_Field_Separator : constant Character := ':';
   --
   -- The default for Field_Separator, below.


   Field_Separator : Character := Default_Field_Separator;
   --
   -- The character that separates fields in basic-format output lines.


   Default_Show_Notes : constant Boolean := False;
   --
   -- Default for Show_Notes, below.


   Show_Notes : Boolean := Default_Show_Notes;
   --
   -- Whether to output "notes".


   --
   ---   Key words
   --


   Note_Key      : constant String := "Note";
   Warning_Key   : constant String := "Warning";
   Error_Key     : constant String := "Error";
   Fault_Key     : constant String := "Fault";
   Trace_Key     : constant String := "Trace";
   Exception_Key : constant String := "Exception";
   Unknown_Key   : constant String := "Unknown";
   See_Also_Key  : constant String := "Also";


   --
   ---   Basic output operations
   --


   procedure Flush_Standard_Output;
   --
   -- Clean up any half-finished lines in standard output.


   procedure Line (
      Channel      : in Ada.Text_IO.File_Type;
      Key          : in String;
      Program_File : in String;
      Source_File  : in String;
      Call_Path    : in String;
      Statements   : in String;
      Data         : in String);
   --
   -- Emit a line in the basic format, with all fields given
   -- as parameters. "Data" contains field 6 .. N, with embedded
   -- field separators.


   procedure Fault (
      Location : in String;
      Text     : in String);
   --
   -- Emit a basic Fault message on Standard Error.
   -- The locus is empty (null).
   -- Does not increment the number of Faults.


   --
   ---   Supporting utility operations
   --


   function Trim (Item : String) return String;
   --
   -- The given string with leading and trailing blanks removed.


   function Image (Item : Integer) return String;
   --
   -- A trimmed, base-10 formatting of the number.
   -- Same as Integer'Image but with no leading blank.


end Basic_Output;
