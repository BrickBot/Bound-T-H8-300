-- Output.Opt (decl)
--
-- Command-line options that control the generation and formatting of
-- the "basic" output.
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
-- $Revision: 1.11 $
-- $Date: 2015/10/24 19:36:51 $
--
-- $Log: output-opt.ads,v $
-- Revision 1.11  2015/10/24 19:36:51  niklas
-- Moved to free licence.
--
-- Revision 1.10  2011-08-31 04:23:34  niklas
-- BT-CH-0222: Option registry. Option -dump. External help files.
--
-- Revision 1.9  2009/05/21 08:08:27  niklas
-- BT-CH-0175: Limits on the number of Warnings, Errors, Faults.
--
-- Revision 1.8  2006/02/28 08:50:59  niklas
-- Extended the option Source_File_Form to apply also to the name
-- of the target program executable file. Changed the default
-- value to Base_Name for brevity and to help testing.
--
-- Revision 1.7  2005/10/26 14:10:14  niklas
-- Using Basic_Output.
--
-- Revision 1.6  2004/04/25 09:29:45  niklas
-- First Tidorum version.
-- Multiple source files per location ("See_Also" lines).
-- Options for source-file name form (full path or basename).
-- Inexact source-line matching (surrounding lines).
-- Operations for tracing output ("Trace" lines).
-- Additional control over "Result" and "Unknown" output.
-- Caching of current default locus computed from locus nest.
--
-- Revision 1.5  2003/02/17 16:17:08  holsti
-- Added option Show_Code_Addresses (-address) to include the code address
-- in all output of code loci. Earlier, this was the default; now the
-- default is to suppress the code addresses and show only source-line
-- numbers if available.
--
-- Revision 1.4  2001/05/27 10:50:59  holsti
-- programs-show.adb
--
-- Revision 1.3  2001/03/15 20:47:37  holsti
-- Show_Notes (option -q, -quiet) added.
--
-- Revision 1.2  2000/05/02 10:53:01  holsti
-- Field_Separator is provided by Output, too.
--
-- Revision 1.1  2000/04/22 12:08:26  holsti
-- Output package added.
--


with Basic_Output;
with Options;
with Options.Bool;
with Options.Char;
with Options.Nat;


package Output.Opt is


   pragma Elaborate_Body;
   --
   -- To register the options.


   Field_Separator_Opt : aliased Options.Char.Option_T (
      Default => Basic_Output.Default_Field_Separator);
   --
   -- The character that separates fields in basic-format output lines.
   -- Changing this value also changes Basic_Output.Field_Separator.
   --
   Field_Separator : Character renames Field_Separator_Opt.Value;


   Show_Notes_Opt : aliased Options.Bool.Option_T (
      Default => Basic_Output.Default_Show_Notes);
   --
   -- Whether to output "notes".
   -- Changing this value also changes Basic_Output.Show_Notes.
   --
   Show_Notes : Boolean renames Show_Notes_Opt.Value;


   package Source_File_Forms is new Options.Discrete_Valued (
      Value_Type  => Real_Source_File_Form_T,
      Value_Image => Real_Source_File_Form_T'Image);
   --
   -- Options with values of type Real_Source_File_Form_T.


   Source_File_Form_Opt : aliased Source_File_Forms.Option_T (Default => Base);
   --
   -- The chosen form in which the names of source files and
   -- target program executable files are shown.
   --
   Source_File_Form : Real_Source_File_Form_T
      renames Source_File_Form_Opt.Value;


   Show_Code_Addresses_Opt : aliased Options.Bool.Option_T (Default => False);
   --
   -- Whether to show the machine-code addresses when indicating the
   -- place (locus) of some code entity (subprogram, statement, loop,
   -- call, instruction).
   --
   Show_Code_Addresses : Boolean renames Show_Code_Addresses_Opt.Value;


   type Source_Line_Precision_T is (
      Exact,
      Around);
   --
   -- The precision required for showing source-line numbers associated
   -- with the place (locus) of some code entity (subprogram, statement,
   -- loop, call, instruction).
   --
   -- Exact
   --    Show only source-line numbers connected to the code address(es)
   --    of the place.
   -- Around
   --    If no source-line numbers are connected to the code address(es)
   --    of the place, show the source-line number(s) for the closest
   --    preceding and/or following address that is/are connected to
   --    source-line numbers.

   package Source_Precisions is new Options.Discrete_Valued (
      Value_Type  => Source_Line_Precision_T,
      Value_Image => Source_Line_Precision_T'Image);


   Source_Line_Precision : aliased Source_Precisions.Option_T (
      Default => Around);
   --
   -- How to choose source-line numbers for a locus.


   function Show_Surrounding_Lines return Boolean;
   --
   -- Whether Source_Line_Precision is set to show the
   -- surrounding lines (Around).


   Trace_Locus_Nesting_Opt : aliased Options.Bool.Option_T (Default => False);
   --
   -- Whether to trace the nesting and unnesting of default output loci.
   --
   Trace_Locus_Nesting : Boolean renames Trace_Locus_Nesting_Opt.Value;


   Trace_Current_Locus_Opt : aliased Options.Bool.Option_T (Default => False);
   --
   -- Whether to show the default locus nest whenever the current locus
   -- is computed from this nest.
   --
   Trace_Current_Locus : Boolean renames Trace_Current_Locus_Opt.Value;


   Max_Warnings_Opt : aliased Options.Nat.Option_T (Default => 3_000);
   Max_Errors_Opt   : aliased Options.Nat.Option_T (Default => 3_000);
   Max_Faults_Opt   : aliased Options.Nat.Option_T (Default =>   100);
   --
   -- Limits on the number of Warning, Error, and Fault messages
   -- that can be emitted, before execution is terminated by raising
   -- the exception Too_Many_Problems.
   --
   -- The exception is raised if the total number of emitted Warnings
   -- (Errors, Faults) exceeds the corresponding limit after emitting
   -- a Warning (Error, Fault). Thus, the actual number of Warnings
   -- (Errors, Faults) emitted is one more than the limit. For example,
   -- if the limit is zero, the exception is raised after the first
   -- Warning (Error, Fault) is emitted.
   --
   -- Furthermore, exceeding the limit is also an error and is
   -- reported by an Error message (which cannot, however, cause
   -- Max_Errors to be exceeded).
   --
   Max_Warnings : Natural renames Max_Warnings_Opt.Value;
   Max_Errors   : Natural renames Max_Errors_Opt.Value;
   Max_Faults   : Natural renames Max_Faults_Opt.Value;


end Output.Opt;
