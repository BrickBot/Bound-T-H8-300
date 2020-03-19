-- Output (decl)
--
-- Output of basic results, notes, warnings and errors.
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
-- $Revision: 1.32 $
-- $Date: 2015/10/24 19:36:51 $
--
-- $Log: output.ads,v $
-- Revision 1.32  2015/10/24 19:36:51  niklas
-- Moved to free licence.
--
-- Revision 1.31  2013-11-27 11:44:42  niklas
-- Added Locus (Code_Address_T) return Statement_Locus_T.
--
-- Revision 1.30  2011-08-31 04:23:34  niklas
-- BT-CH-0222: Option registry. Option -dump. External help files.
--
-- Revision 1.29  2009/05/21 08:08:27  niklas
-- BT-CH-0175: Limits on the number of Warnings, Errors, Faults.
--
-- Revision 1.28  2009/03/24 07:48:35  niklas
-- BT-CH-0166: String_Pool.Item_T for source-file and marker names.
--
-- Revision 1.27  2009/03/21 13:09:17  niklas
-- BT-CH-0165: Option -file_match for matching asserted file-names.
--
-- Revision 1.26  2008/11/09 21:43:04  niklas
-- BT-CH-0158: Output.Image (Time_T) replaces Programs.Execution.Image.
--
-- Revision 1.25  2008/02/15 20:27:43  niklas
-- BT-CH-0110: Better "&" for call-paths in Output loci.
--
-- Revision 1.24  2007/10/26 12:44:36  niklas
-- BT-CH-0091: Reanalyse a boundable edge only if its domain grows.
--
-- Revision 1.23  2007/04/30 06:39:55  niklas
-- Made No_Mark public and the default initial value.
--
-- Revision 1.22  2006/06/16 14:51:13  niklas
-- Published the function Formed_Source with a Default value
-- for the Form parameter.
--
-- Revision 1.21  2006/05/17 20:07:13  niklas
-- Added the function Source_File_Item.
--
-- Revision 1.20  2006/02/28 08:47:15  niklas
-- Extended the option -source (Output.Opt.Source_File_Form) to
-- apply also to the name of the target program executable file.
-- Thus, the function Program_File (Locus) now takes also a
-- Form parameter and applies Formed_Source to the file-name.
--
-- Revision 1.19  2005/10/26 14:11:30  niklas
-- Using Basic_Output.
--
-- Revision 1.18  2005/10/09 08:10:22  niklas
-- BT-CH-0013.
--
-- Revision 1.17  2005/08/24 10:06:50  niklas
-- Added an "Address" parameter (Boolean option) to the Image
-- functions for Statement_Locus_T, Statement_Range_T and
-- Source_Interval_T. This lets clients force the display of
-- code addresses in warnings and errors that need it, as if
-- the option -address were in effect.
--
-- Revision 1.16  2005/08/08 17:42:04  niklas
-- Added functions First and Last to get the line-number range
-- directly from a Statement Range.
--
-- Revision 1.15  2005/06/29 09:33:45  niklas
-- Added the Heading procedure.
--
-- Revision 1.14  2005/06/28 07:02:21  niklas
-- Added function Code_Image.
--
-- Revision 1.13  2004/04/25 09:29:45  niklas
-- First Tidorum version.
-- Multiple source files per location ("See_Also" lines).
-- Options for source-file name form (full path or basename).
-- Inexact source-line matching (surrounding lines).
-- Operations for tracing output ("Trace" lines).
-- Additional control over "Result" and "Unknown" output.
-- Caching of current default locus computed from locus nest.
--
-- Revision 1.12  2001/12/10 14:43:11  holsti
-- Locus-nest marks have a "Defined" attribute to avoid accessing
-- undefined (uninitialized) marks.
--
-- Revision 1.11  2001/03/21 20:12:46  holsti
-- Program locations given by Locus_T.
--
-- Revision 1.10  2000/12/28 12:35:07  holsti
-- Image for Integer added.
--
-- Revision 1.9  2000/11/24 12:06:01  sihvo
-- Added stack height analysis.
--
-- Revision 1.8  2000/11/09 14:44:31  saarinen
-- Added function Get_Subprogram.
--
-- Revision 1.7  2000/10/26 09:30:01  saarinen
-- Added procedures Wcet, Wcet_Call, Unknown and Loop_Bound.
--
-- Revision 1.6  2000/06/27 20:05:28  holsti
-- Added procedure Fault.
--
-- Revision 1.5  2000/05/02 10:53:01  holsti
-- Field_Separator is provided by Output, too.
--
-- Revision 1.4  2000/04/24 18:36:20  holsti
-- Added Subprogram field.
--
-- Revision 1.3  2000/04/24 14:28:39  holsti
-- Symbol scopes added.
--
-- Revision 1.2  2000/04/23 21:41:17  holsti
-- Added Flow package.
--
-- Revision 1.1  2000/04/22 12:08:28  holsti
-- Output package added.
--


with Ada.Text_IO;
with Ada.Exceptions;

with Basic_Output;
with Processor;
with String_Pool;
with Symbols;


package Output is
--
-- The "basic output format" consists of lines divided into fields.
-- Fields are separated by a reserved character (by default the
-- colon ':', settable).
--
-- The first five fields have a fixed meaning as follows:
--
-- Field 1    Key string, e.g. "Warning" that defines the type
--            of output line and the interpretation of the variable
--            fields (starting from field 5).
--
-- Field 2    Name of target-program executable file.
--
-- Field 3    Name of target-program source-code file (if known and relevant).
--
-- Field 4    Name of subprogram (if known and relevant), or a call-path
--            containing the names of several subprograms in top-down
--            calling order, with the location of each call indicated.
--            The last, or bottom, subprogram in the call-path is the one
--            to which this output line most directly applies.
--
-- Field 5    Statement(s) in the target-program. A statement or instruction
--            is located by a line number in the source-code file and/or
--            a code address. This may be a single statement (in the case
--            of a call instruction, for example), or a range of statements
--            (corresponding to a subprogram or a loop, for example).
--
-- When field 4 contains a call-path, the higher-level subprograms in
-- the path may reside in different source files than the bottom subprogram.
-- Fields 3 and 5 apply to the bottom subprogram. The call-path (field 4)
-- may contain embedded statement locators (and perhaps source=file names)
-- for the higher levels.
--
-- In some cases, even the bottom subprogram - or other program part to
-- which the output line applies - is related to more than one source-code
-- file and statement range. When this happens, additional output lines
-- are emitted with "See_Also" in the key field (field 1), the same
-- contents in fields 2 and 4, and the other source-code file(s) and
-- statement ranges in fields 3 and 5. No other fields are emitted for
-- these See_Also lines.
--
-- The form and meaning of the remaining "data" fields (6 .. N) depend
-- on the type of output line, i.e. on the value in field 1.


   Field_Separator : Character renames Basic_Output.Field_Separator;
   --
   -- The character that separates fields in basic-format output lines.


   --
   ---  Locus for locating elements of a program
   --


   type Locus_T is private;
   --
   -- A locus defines a element, place or region of the target program
   -- under analysis. Its attributes include the data necessary to generate
   -- fields 2 through 5 for a basic-output line.
   --
   -- Each field may (separately) be undefined or more or less precisely
   -- defined. Operations are provided to create loci with specific
   -- field values, to query the fields, and to combine two loci into a
   -- new locus value that uses the most precisely defined data from both
   -- original loci.
   --
   -- The operations for generating basic-output lines take a locus
   -- parameter, which usually defines the output locus fields fully.
   --
   -- However, to avoid transporting locus parameters from specific
   -- contexts to general routines where output may be generated in
   -- exceptional cases (errors, warnings) a system of default loci is
   -- implemented. The default loci are nested, so that new defaults can
   -- be pushed and popped.
   --
   -- The displayed locus of a basic-output line is derived from the current
   -- default loci, with more recently pushed loci overriding or focussing
   -- the older ones, and with the locus parameter having the final say.


   No_Locus : constant Locus_T;
   --
   -- A locus that is completely undefined.


   --
   ---  Statement locus
   --


   subtype Line_Number_T is Symbols.Line_Number_T;
   --
   -- The number of a line in a source (text) file.
   -- The first line is number 1 (for zero, see No_Line_Number).


   No_Line_Number : constant Line_Number_T := Line_Number_T'First;
   --
   -- This "line number" represents an unknown or irrelevant line-number.


   type Statement_Locus_T is private;
   --
   -- A statement locus locates a statement by its code address
   -- and/or a source-line number in a specific source-file.
   -- It may contain a (reference to) a symbol-table, for later
   -- improvement or widening of the mapping between code addresses
   -- and source-line numbers.


   No_Statement : constant Statement_Locus_T;
   --
   -- Indicates a missing or unknown statement locus.


   type Statement_Range_T is private;
   --
   -- A range of statements, usually representing a contiguous
   -- part of the target program.
   -- A statement range has, as attributes, one or more source-file
   -- names, and for each source-file name a range of source-line
   -- numbers and a range of code addresses.
   -- Note that the min/max line-number need not correspond to the
   -- min/max code address, since the compiler may have reordered
   -- instructions.
   -- The common case is to have just one source-file name and
   -- its associated line-number range and code address range.
   -- It may contain a (reference to) a symbol-table, for later
   -- improvement or widening of the mapping between code addresses
   -- and sourc-line numbers.


   Max_Source_Files_Per_Locus : constant := 3;
   --
   -- The maximum number of source-file names and associated
   -- line-number and code-address ranges for a Statement_Range_T.


   subtype Source_Ordinal_T is
      Positive range 1 .. Max_Source_Files_Per_Locus;
   --
   -- Identifies one of the source-files of a Statement_Range_T.
   -- The order is arbitrary.


   First_Source : constant Source_Ordinal_T := Source_Ordinal_T'First;
   --
   -- The ordinal number of the first (and usually only) source-file
   -- in a statement range.


   function No_Statements return Statement_Range_T;
   --
   -- An undefined range of statements.
   -- Adding this range to another statement range has no effect.


   --
   ---  Querying statement loci attributes
   --


   type Source_File_Form_T is (Full, Base, Default);
   --
   -- Optional forms of the name of a source-file.
   --
   -- Full
   --    The full name, including directory path.
   --    The name "a/b/c" is given in full as "a/b/c".
   -- Base
   --    Only the actual file-name, omitting directories.
   --    The name "a/b/c" is given as "c".
   -- Default
   --    The value (either Full_Path or Base_Name) selected
   --    by command-line option, or the default value if no
   --    option given.


   function Formed_Source (
      Full_Name : String;
      Form      : Source_File_Form_T := Default)
   return String;
   --
   -- Presents the Full source-file Name in the chosen Form.


   subtype Real_Source_File_Form_T is
      Source_File_Form_T range Full .. Base;
   --
   -- All forms except Default.


   function Source_File (
      Item : Statement_Locus_T;
      Form : Source_File_Form_T := Default)
   return String;
   --
   -- The name of the source file that contains the statement, in
   -- the chosen Form. Null string if unknown.


   function Line_Number (Item : Statement_Locus_T)
   return Line_Number_T;
   --
   -- The number of the source-file that contains or corresponds
   -- to the statement.
   -- No_Line_Number if unknown.


   function Code_Address (Item : Statement_Locus_T)
   return Processor.Code_Address_T;
   --
   -- The code address of the statement.
   -- TBD if unknown.


   function Number_Of_Sources (Item : Statement_Range_T)
   return Natural;
   --
   -- The number of source-files associated with the given statement
   -- range. The source-files have consecutive ordinal numbers from
   -- 1 to Number_Of_Sources.
   -- Zero if the statement range is null.


   function Source_File (
      Item   : Statement_Range_T;
      Source : Source_Ordinal_T   := First_Source;
      Form   : Source_File_Form_T := Default)
   return String;
   --
   -- The name of the source file, with ordinal number Source, that
   -- contains a part of the statement range.
   -- Fault if Source is out of range for the Item.
   -- Null string if unknown or Fault.


   function Source_File (
      Item   : Statement_Range_T;
      Source : Source_Ordinal_T := First_Source)
   return Symbols.Source_File_Name_T;
   --
   -- Similar to Source_File (...) return String, but returns a
   -- Symbols.Source_File_Name_T for the full source-file name
   -- (including directory path). If the Source is out of range
   -- for the Item, issues a Fault message and returns Null_Name.


   function First (
      Item   : Statement_Range_T;
      Source : Source_Ordinal_T := First_Source)
   return Statement_Locus_T;
   --
   -- The first (least) statement locus in the source-file with
   -- ordinal number Source in the range.
   -- The result will have the smallest line-number and smallest
   -- code-address that are known to lie in this source-file in the
   -- given range. Note that these need not correspond to each other,
   -- if instructions have been reordered.
   -- If Item is No_Statements, returns No_Statement.


   function Last (
      Item   : Statement_Range_T;
      Source : Source_Ordinal_T := First_Source)
   return Statement_Locus_T;
   --
   -- The last (largest) statement locus in the source-file with
   -- ordinal number Source in the range.
   -- The result will have the largest line-number and largest
   -- code-address that are known to lie in this source-file in the
   -- given range. Note that these need not correspond to each other,
   -- if instructions have been reordered.
   -- If Item is No_Statements, returns No_Statement.


   function First (
      Item   : Statement_Range_T;
      Source : Source_Ordinal_T := First_Source)
   return Line_Number_T;
   --
   -- The first (least) line-number locus in the source-file with
   -- ordinal number Source in the range. This is the combination
   -- of the First function that takes a Statement_Range_T and
   -- returns a Statement_Locus_T, followed by the Line_Number
   -- function applied to this Statement_Locus_T.


   function Last (
      Item   : Statement_Range_T;
      Source : Source_Ordinal_T := First_Source)
   return Line_Number_T;
   --
   -- The first (least) line-number locus in the source-file with
   -- ordinal number Source in the range. This is the combination
   -- of the First function that takes a Statement_Range_T and
   -- returns a Statement_Locus_T, followed by the Line_Number
   -- function applied to this Statement_Locus_T.


   --
   ---  Constructing statement loci and ranges
   --


   function Locus (
      Source_File  : String        := "";
      Line_Number  : Line_Number_T := No_Line_Number;
      Code_Address : Processor.Code_Address_T;
      Symbol_Table : Symbols.Symbol_Table_T)
   return Statement_Locus_T;
   --
   -- A statement in the given source file (if known), at
   -- the given source line (if known), and the given code
   -- address (which is assumed known here).


   function Locus (
      Source_File  : String := "";
      Line_Number  : Line_Number_T)
   return Statement_Locus_T;
   --
   -- A statement in the given source file (if known) and at
   -- the given source line (known) but with an unknown or
   -- irrelevant code address.


   function Locus (Code_Address : Processor.Code_Address_T)
   return Statement_Locus_T;
   --
   -- A statement in the given source file (if known), at
   -- the given code address, without specifying the source line
   -- nor the source file.


   function Rainge (
      First : Statement_Locus_T;
      Last  : Statement_Locus_T)
   return Statement_Range_T;
   --
   -- The statement range from First to Last, where First and Last
   -- must be in the same source-file (when the source-file is known).
   -- The first/last line-numbers and first/last code-addresses are
   -- chosen separately from the attributes of First and Last.
   -- Thus, if First has a smaller line-number than Last, but Last has
   -- a smaller code address than First, the First locus of the result
   -- will have the line-number of the First parameter but the
   -- code-address of the Last parameter.


   function "+" (Item : Statement_Locus_T)
   return Statement_Range_T;
   --
   -- Returns the range than contains exactly the given statement.
   -- If Item = No_Statement, returns No_Statements.


   function "+" (Left, Right : Statement_Range_T)
   return Statement_Range_T;
   --
   -- Returns the smallest range that contains both the given ranges.
   -- If Left and Right refer to the same source-file, the result
   -- will also refer to this source-file. If Left and Right refer
   -- to different (sets of) source files, the ranges will be combined
   -- separately for each source-file.
   -- If the maximum number of source-files per range is exceeded,
   -- some source-files will be left out from the result.


   function "+" (
      Left  : Statement_Range_T;
      Right : Statement_Locus_T)
   return Statement_Range_T;
   --
   -- Returns the smallest range that contains Left and Right,
   -- with the same handling of source-files as for "+" with two
   -- statement-range operands.


   function "&" (Left, Right : Statement_Range_T)
   return Statement_Range_T;
   --
   -- Returns the more precise of the given statement ranges.
   -- If either operand is No_Statement_Range, the other is
   -- returned, otherwise the intersection is returned. When
   -- multiple source-files are involved, the intersection is
   -- computed separately for each source file.


   procedure Add_Surrounding_Lines (To : in out Statement_Range_T);
   --
   -- Adds To a given statement range references to the source-lines
   -- that most closely surround the code address(es) of the rage, if
   -- the range does not already have source-line information. The code
   -- address(es) of the statement range are not changed.
   --
   -- If there is a source-line connection before the given range,
   -- this source-line is added (the logic is that all the code generated
   -- for a source line normally follows the first code generated for
   -- this line). Otherwise, a source-line that is after the given range
   -- is sought and used if found.
   --
   -- This operation is controlled by a command-line option.


   function Image (
      Item    : Statement_Locus_T;
      Address : Boolean := False)
   return String;
   --
   -- Image of the statement locator.
   -- The source-file name is not included.
   -- The source line number, if known, is shown as a string of
   -- base-10 digits with no leading, embedded or trailing blanks.
   -- The code address, if known and chosen by the Address parameter
   -- or a general option, is shown after the line-number, by using
   -- Processor.Image, enclosed in [].
   -- Thus, the possible forms are:
   --    ""      if neither line-number nor code-address known
   --    "line"
   --    "[code]"
   --    "line[code]"


   function Image (
      Item    : Statement_Range_T;
      Source  : Source_Ordinal_T := First_Source;
      Address : Boolean := False)
   return String;
   --
   -- Image of the statement range associated with the source-file
   -- with ordinal Source in the range.
   -- A range of source-lines followed by a range of code addresses,
   -- with enclosing [] as above.
   -- For example, "11-44[a1bf-a2e5]".
   -- In both parts, if the first and last values are the same, only
   -- one is displayed (and the "-" is omitted).
   -- For example, "123[83ef-890a]".
   -- If source-line numbers are known, the code-address is displayed
   -- only if chosen by the Address parameter or by the general option.


   function Code_Image (Item : Statement_Range_T)
   return String;
   --
   -- Image of the overall code-address range for this statement range,
   -- as in the Image function above but showing only the code address
   -- part (in brackets).


   --
   ---  Locus construction and combination
   --


   Call_Locus_Mark : constant String := "@";
   --
   -- The symbol used to separate the name of the caller from the
   -- locus of the call, in the construct Caller @ locus => Callee
   -- that is a part of a call-path locus.


   Call_Mark : constant String := "=>";
   --
   -- The symbol used to denote a call in various outputs, usually
   -- appearing between the names of the caller subprogram and the
   -- callee subprogram, in the construct Caller @ locus => Callee
   -- that is a part of a call-path locus.


   function Locus (
      Program_File : String := "";
      Source_File  : String := "";
      Call_Path    : String := "";
      Statements   : Statement_Range_T := No_Statements)
   return Locus_T;
   --
   -- Creates a locus with the specified attributes.
   -- The default value of each parameter represents an undefined or
   -- unknown value for that attribute.
   -- If no Source_File is given directly, but one is known for
   -- the Statements, the latter is used.


   function "&" (Left, Right : Locus_T)
   return Locus_T;
   --
   -- Combines two loci into a more precise locus by choosing
   -- for each attribute the more precise of the Left and Right values
   -- for that attribute. If the Left and Right values are equally
   -- precise, the Right value is chosen (overriding Left).
   --
   -- The more precise value of each attribute is defined as follows:
   --
   -- Program_File
   --    The null string represents undefined.
   --    All other values are equally precise.
   --
   -- Source_File
   --    As for Program_File.
   --
   -- Call_Path
   --    The null string represents undefined.
   --    A non-null string is more precise than any of its substrings.
   --    That is, extending the call-path at either end (to higher or
   --    lower levels) makes it more precise.
   --
   -- Statements
   --    No_Statement_Range represents undefined. A statement-range
   --    that is contained in another (as a subset) is more precise
   --    than the larger range.
   --
   -- Symbol_Table
   --    No_Symbol_Table, if neither operand has a symbol table.
   --    If one or both operands have a symbol-table, they are
   --    assumed to have the same table, and this table is used.


   procedure Add_Surrounding_Lines (To : in out Locus_T);
   --
   -- Adds To a given locus references to the source-lines that most
   -- closely surround the code address(es) of the locus, if the
   -- locus does not already have source-line information. The code
   -- address(es) of the locus are not changed.
   --
   -- If there is a source-line connection before the given locus,
   -- this source-line is added (the logic is that all the code generated
   -- for a source line normally follows the first code generated for
   -- this line). Otherwise, a source-line that is after the given locus
   -- is sought and used if found.
   --
   -- This operation is controlled by a command-line option.


   --
   ---  Default-locus operations
   --


   type Nest_Mark_T is private;
   --
   -- Identifies a locus in the nest of default loci.
   -- It is used to check that defaults are nested and unnested properly.
   -- The default initial value of a Nest_Mark_T is No_Mark, below.


   No_Mark : constant Nest_Mark_T;
   --
   -- Indicates an undefined (absent) locus mark.
   -- This is the default initial value of all Nest_Mark_T objects.


   function Nest (Locus : in Locus_T) return Nest_Mark_T;
   --
   -- Defines the given locus as a new default, nesting it within
   -- the currently defined default loci.
   -- The return value is needed to remove (un-nest) this locus from
   -- the defaults.


   procedure Unnest (Mark : in Nest_Mark_T);
   --
   -- Removes the innermost (newest) default locus, or all default loci
   -- down to and including the one identified by the given mark.


   function Current_Locus return Locus_T;
   --
   -- Returns the combination of the currently defined default loci.
   -- The result is computed by the "&" operator applied from the
   -- oldest (outermost) to the newest (innermost) currently
   -- defined default locus.


   --
   ---  Locus queries
   --


   function Program_File (
      Item : Locus_T;
      Form : Source_File_Form_T := Default)
   return String;
   --
   -- The name of the target-program executable file that contains the locus.
   -- Null string if not defined.


   function Source_File (
      Item : Locus_T;
      Form : Source_File_Form_T := Default)
   return String;
   --
   -- The name of the source-code file that contains the locus.
   -- Null string if not defined.


   function Call_Path (Item : Locus_T) return String;
   --
   -- The string image of the call-path that leads to the locus.
   -- Null string if not defined.


   function Statements (Item : Locus_T) return Statement_Range_T;
   --
   -- The statements that correspond to the locus.
   -- No_Statements is not defined.


   function Image (Item : Locus_T) return String;
   --
   -- The locus, displayed as in fields 2 through 5.


   --
   ---  Basic output operations
   --

   -- Most operations that can take a locus parameter are defined in
   -- two forms, one with a locus parameter and the other without it.
   -- The form without a locus parameter uses Current_Locus.
   -- The other form uses Current_Locus combined with (and perhaps
   -- overridden by) the parameter locus.


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
   -- Emit a line in the basic format, with all fields given
   -- as parameters. "Data" contains field 6 .. N, with embedded
   -- field separators.


   Too_Many_Problems : exception;
   --
   -- Signals that too many Warning, Error, or Fault messages have
   -- been emitted, compared to the maximum numbers set in Output.Opt.


   procedure Note (
      Text  : in String);

   procedure Note (
      Locus : in Locus_T;
      Text  : in String);
   --
   -- Outputs a "Note" line.


   procedure Warning (
      Text  : in String);

   procedure Warning (
      Locus : in Locus_T;
      Text  : in String);
   --
   -- Outputs a "Warning" line.
   --
   -- Propagates Too_Many_Problems if the total number of Warnings
   -- emitted in this execution now exceeds the chosen maximum,
   -- Output.Opt.Max_Warnings.


   procedure Error (
      Text  : in String);

   procedure Error (
      Locus : in Locus_T;
      Text  : in String);
   --
   -- Outputs an "Error" line.
   --
   -- Propagates Too_Many_Problems if the total number of Errors
   -- emitted in this execution now exceeds the chosen maximum,
   -- Output.Opt.Max_Errors.


   procedure Fault (
      Location : in String;
      Text     : in String);

   procedure Fault (
      Location : in String;
      Locus    : in Locus_T;
      Text     : in String);
   --
   -- Outputs a "Fault" line, to signal an internal fault in Bound-T
   -- detected in the Bound-T package or subprogram "Location".
   --
   -- Propagates Too_Many_Problems if the total number of Faults
   -- emitted in this execution now exceeds the chosen maximum,
   -- Output.Opt.Max_Faults.


   procedure Trace (
      Text : in String);

   procedure Trace (
      Locus : in Locus_T;
      Text  : in String);
   --
   -- Outputs a "Trace" line, to report some on-the-fly execution
   -- tracing information.


   procedure Exception_Info (
      Text       : in String;
      Occurrence : in Ada.Exceptions.Exception_Occurrence);

   procedure Exception_Info (
      Locus      : in Locus_T;
      Text       : in String;
      Occurrence : in Ada.Exceptions.Exception_Occurrence);
   --
   -- Outputs an "Exception" line, which is like an error, but
   -- includes more fields giving the exception info.


   procedure Result (
      Key   : in String;
      Text  : in String);

   procedure Result (
      Key   : in String;
      Locus : in Locus_T;
      Text  : in String);
   --
   -- Outputs a line with the result of an analysis, as identified
   -- by the Key parameter.


   procedure Unknown (
      Key   : in String;
      Text  : in String := "");

   procedure Unknown (
      Key   : in String;
      Locus : in Locus_T;
      Text  : in String := "");
   --
   -- Outputs a line that shows that a result, identified by the
   -- the Key parameter, is unknown (analysis failed), with further
   -- explanation in the optional Text.



   --
   ---  Supporting utility operations
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


   function Image (Item : Processor.Time_T) return String;
   --
   -- A trimmed, base-10 formatting of the time number.
   -- Same as Time_T'Image but with no leading blank.


   function Image (Item : Line_Number_T) return String;
   --
   -- A trimmed, base-10 formatting of the line-number.
   -- Same as Integer'Image but with no leading blank.


   function Either (
      Cond : Boolean;
      Yes  : String;
      No   : String)
   return String;
   --
   -- A conditional choice: if Cond then Yes else No.


   procedure Flush;
   --
   -- Ensures that all output generated so far is physically written to
   -- the output devices (flush buffers) before returning.
   -- This is useful for user interaction.


   --
   ---   Support for raw text-io.
   --


   procedure Heading (
      Text   : in String;
      Margin : in Ada.Text_IO.Positive_Count := 4);
   --
   -- Prints the Text, starting from column 1, then indents to
   -- the given Margin column on the next line. However, first
   -- uses Flush to clear the output buffers.
   --
   -- This procedure is useful to give a heading to some output that
   -- uses raw text-io, not the "basic output" format, and takes its
   -- left margin from the current column.


private

   type Possible_Line_Number_T (Known : Boolean := False) is record
      case Known is
         when False => null;
         when True  => Number : Line_Number_T;
      end case;
   end record;


   type Possible_Code_Address_T (Known : Boolean := False) is record
      case Known is
         when False => null;
         when True  => Address : Processor.Code_Address_T;
      end case;
   end record;


   type Statement_Locus_T is record
      Source_File  : Symbols.Source_File_Name_T;
      Line         : Possible_Line_Number_T;
      Code         : Possible_Code_Address_T;
      Symbol_Table : Symbols.Symbol_Table_T;
   end record;


   No_Statement : constant Statement_Locus_T := (
      Source_File  => Symbols.Null_Name,
      Line         => (Known => False),
      Code         => (Known => False),
      Symbol_Table => Symbols.No_Symbol_Table);


   type Source_Interval_T is record
      File  : Symbols.Source_File_Name_T;
      First : Possible_Line_Number_T;
      Last  : Possible_Line_Number_T;
   end record;
   --
   -- An interval of source lines, all from the same source file.


   No_Source_Interval : constant Source_Interval_T := (
      File  => Symbols.Null_Name,
      First => (Known => False),
      Last  => (Known => False));


   type Source_Interval_List_T is
      array (Source_Ordinal_T range <> ) of Source_Interval_T;
   --
   -- A source-line interval for each source-file associated
   -- with a statement range.


   subtype Source_Intervals_T is Source_Interval_List_T (Source_Ordinal_T);
   --
   -- A list of source-line intervals with the fixed maximum length.


   type Code_Interval_T is record
      First : Possible_Code_Address_T;
      Last  : Possible_Code_Address_T;
   end record;
   --
   -- An interval of instruction (code) addresses, all from
   -- the same executable file (but not necessarily generated
   -- from the same source-code file).


   No_Code_Interval : constant Code_Interval_T := (
      First => (Known => False),
      Last  => (Known => False));


   type Statement_Range_T is record
      Sources      : Natural := 0;
      Source       : Source_Intervals_T;
      Code         : Code_Interval_T;
      Symbol_Table : Symbols.Symbol_Table_T;
   end record;
   --
   -- A range of statements, possibly from several source-code
   -- files. Null if Sources = 0. Note that even if Sources > 0,
   -- the actual Source_File components in one of the Intervals
   -- be Null_Item, meaning an unknown source file.


   No_Statement_Range : constant Statement_Range_T := (
      Sources      => 0,
      Source       => (others => No_Source_Interval),
      Code         => No_Code_Interval,
      Symbol_Table => Symbols.No_Symbol_Table);


   type Locus_T is record
      Program_File : String_Pool.Item_T         := String_Pool.Null_Item;
      Source_File  : Symbols.Source_File_Name_T := Symbols.Null_Name;
      Call_Path    : String_Pool.Item_T         := String_Pool.Null_Item;
      Statements   : Statement_Range_T          := No_Statement_Range;
   end record;


   No_Locus : constant Locus_T := (
      Program_File => String_Pool.Null_Item,
      Source_File  => Symbols.Null_Name,
      Call_Path    => String_Pool.Null_Item,
      Statements   => No_Statement_Range);


   Max_Nest_Depth : constant := 70;
   --
   -- Maximum depth of nested default loci.


   type Nest_Depth_T is range 0 .. Max_Nest_Depth;
   --
   -- The current depth of the default-locus nest.


   type Nest_Mark_T is record
      Defined : Boolean      := False;
      Depth   : Nest_Depth_T := 0;
      Seq     : Natural      := 0;
   end record;
   --
   -- The mark has a depth, which is the primary mark, and
   -- a sequential number, which is used to check that the
   -- depths are not mixed up.
   -- The mark becomes "defined" by the Nest operation.
   -- Unnesting and undefined mark has no effect.


   No_Mark : constant Nest_Mark_T := (
      Defined => False,
      Depth   => 0,
      Seq     => 0);


end Output;
