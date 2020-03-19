-- Assertions.Opt (decl)
--
-- Command-line options for accessing and using assertions that are
-- provided by the user and describe the behaviour of the target
-- program under analysis.
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
-- $Revision: 1.15 $
-- $Date: 2015/10/24 19:36:46 $
--
-- $Log: assertions-opt.ads,v $
-- Revision 1.15  2015/10/24 19:36:46  niklas
-- Moved to free licence.
--
-- Revision 1.14  2011-08-31 04:23:33  niklas
-- BT-CH-0222: Option registry. Option -dump. External help files.
--
-- Revision 1.13  2009-12-21 14:59:28  niklas
-- BT-CH-0201: Role names with blanks. Option -warn [no_]role.
--
-- Revision 1.12  2009-12-15 09:13:28  niklas
-- BT-CH-0193: Less alarms for assertions on absent subprograms.
--
-- Revision 1.11  2009-03-27 13:57:12  niklas
-- BT-CH-0167: Assertion context identified by source-code markers.
--
-- Revision 1.10  2009/03/21 13:09:17  niklas
-- BT-CH-0165: Option -file_match for matching asserted file-names.
--
-- Revision 1.9  2009/03/20 18:19:28  niklas
-- BT-CH-0164: Assertion context identified by source-line number.
--
-- Revision 1.8  2009/01/18 07:59:17  niklas
-- Removed unused context clause.
--
-- Revision 1.7  2008/11/03 07:58:11  niklas
-- BT-CH-0155: Ignore assertions on absent subprograms.
--
-- Revision 1.6  2007/08/03 19:11:20  niklas
-- Added option Trace_Sub_Options.
--
-- Revision 1.5  2007/01/25 21:25:12  niklas
-- BT-CH-0043.
--
-- Revision 1.4  2006/05/27 21:48:45  niklas
-- BT-CH-0020.
--
-- Revision 1.3  2005/06/12 07:20:29  niklas
-- Added procedure Take_File and package body.
--
-- Revision 1.2  2000/12/28 12:37:29  holsti
-- Trace_Map added.
--
-- Revision 1.1  2000/04/21 19:38:58  holsti
-- Renamed child Options to Opt
--


with Options.Bool;
with Options.File_Sets;
with Options.Nat;


package Assertions.Opt is


   pragma Elaborate_Body;
   --
   -- To register the options.


   Assertion_Files : aliased Options.File_Sets.Option_T;
   --
   -- The (names of) the assertion files, if given.


   Mark_Files : aliased Options.File_Sets.Option_T;
   --
   -- The (names of) the mark-definition files, if given.


   Warn_Absent_Subprogram_Opt :
      aliased Options.Bool.Option_T (Default => False);
   --
   -- Whether to emit a warning for assertions on subprograms that
   -- are not present in the target program.
   --
   Warn_Absent_Subprogram : Boolean renames Warn_Absent_Subprogram_Opt.Value;


   Warn_Unused_Role_Opt : aliased Options.Bool.Option_T (Default => True);
   --
   -- Whether to emit a warning for instruction-role assertions that
   -- have not been used (have not been retrieved from the role-map in
   -- the program object).
   --
   Warn_Unused_Role : Boolean renames Warn_Unused_Role_Opt.Value;


   Implicit_Features_Opt : aliased Options.Bool.Option_T (Default => False);
   --
   -- Whether to use nested assertion statements / blocks to
   -- imply features of the containing program part, useful
   -- for matching the assertions on the containing part to
   -- actual program parts.
   --
   Implicit_Features : Boolean renames Implicit_Features_Opt.Value;


   Line_Fuzz_Opt : aliased Options.Nat.Option_T (Default => 1);
   --
   -- The allowable error, or mismatch, when identifying program
   -- parts by source-line numbers or source-line markers.
   -- The fuzz bounds the difference in the compared line numbers.
   --
   Line_Fuzz : Natural renames Line_Fuzz_Opt.Value;


   package File_Matching_Valued is
      new Options.Discrete_Valued (
         Value_Type  => File_Matching_T,
         Value_Image => File_Matching_T'Image);
   --
   -- Options with a File_Matching_T value.


   File_Matching_Opt :
      aliased File_Matching_Valued.Option_T (Default => Base_Name);
   --
   -- How to compare actual file names to file names written in
   -- assertions, for example the source-file names used in features
   -- to identify target program parts.
   --
   File_Matching : File_Matching_T renames File_Matching_Opt.Value;


   Warn_File_Matching_Opt : aliased Options.Bool.Option_T (Default => False);
   --
   -- Whether to emit a warning when the matching of an actual file-name
   -- to a file-name given in an assertion depends essentially on the
   -- current File_Matching option (that is, different option values give
   -- different result for the comparison).
   --
   Warn_File_Matching : Boolean renames Warn_File_Matching_Opt.Value;


   package File_Casing_Valued is new Options.Discrete_Valued (
      Value_Type  => File_Casing_T,
      Value_Image => File_Casing_T'Image);
   --
   -- Options with a File_Casing_T value.


   File_Casing_Opt :
      aliased File_Casing_Valued.Option_T (Default => Case_Sensitive);
   --
   -- Whether comparison of actual file names to file names written
   -- in assertions is sensitive to letter case.
   --
   File_Casing : File_Casing_T renames File_Casing_Opt.Value;


   Warn_File_Casing_Opt : aliased Options.Bool.Option_T (Default => False);
   --
   -- Whether to emit a warning when the matching of an actual file-name
   -- to a file-name given in an assertion depends essentially on the
   -- current File_Casing option (that is, different option values give
   -- different result for the comparison).
   --
   Warn_File_Casing : Boolean renames Warn_File_Casing_Opt.Value;


   package Source_Relation_Valued is new Options.Discrete_Valued (
      Value_Type  => Source_Relation_T,
      Value_Image => Source_Relation_T'Image);
   --
   -- Options with a Source_Relation_T value.


   Marked_Relation_Opt :
      aliased Source_Relation_Valued.Option_T (Default => On);
   --
   -- The positional relationship to be assumed for a source-code marker
   -- and the marked program part, when the part is identified as being
   -- 'marked' by this marker.
   --
   -- This assumed relationship can be overridden in an assertion by
   -- writing, instead of 'marked', that the part "is on/after/before"
   -- the marker.
   --
   Marked_Relation : Source_Relation_T renames Marked_Relation_Opt.Value;


   Trace_Parsing_Opt : aliased Options.Bool.Option_T (Default => False);
   --
   -- Whether to trace the process of parsing assertion files
   -- into assertion sets.
   --
   Trace_Parsing : Boolean renames Trace_Parsing_Opt.Value;


   Trace_Marks_Opt : aliased Options.Bool.Option_T (Default => False);
   --
   -- Whether to trace the process of reading and parsing mark
   -- definitions from mark files.
   --
   Trace_Marks : Boolean renames Trace_Marks_Opt.Value;


   Trace_Sub_Options_Opt : aliased Options.Bool.Option_T (Default => False);
   --
   -- Whether to trace the process of applying asserted subprogram
   -- "options" to subprograms.
   --
   Trace_Sub_Options : Boolean renames Trace_Sub_Options_Opt.Value;


   Trace_Matching_Opt : aliased Options.Bool.Option_T (Default => False);
   --
   -- Whether to trace the process of matching assertions to
   -- parts of the program to be analysed.
   --
   Trace_Matching : Boolean renames Trace_Matching_Opt.Value;


   Trace_To_Be_Mapped_Opt : aliased Options.Bool.Option_T (Default => False);
   --
   -- Whether to trace the result of selecting assertions to
   -- be mapped onto a particular subprogram and its elements
   -- (loops, calls).
   --
   Trace_To_Be_Mapped : Boolean renames Trace_To_Be_Mapped_Opt.Value;


   Trace_Map_Opt : aliased Options.Bool.Option_T (Default => False);
   --
   -- Whether to trace the result of mapping assertions to
   -- subprogram elements (loops, calls).
   --
   Trace_Map : Boolean renames Trace_Map_Opt.Value;


   Deallocate : Boolean := True;
   --
   -- Whether to use Unchecked_Deallocation to release unused
   -- heap memory.


end Assertions.Opt;
