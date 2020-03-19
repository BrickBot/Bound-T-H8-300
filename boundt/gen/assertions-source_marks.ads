-- Assertions.Source_Marks (decl)
--
-- Marks in source-code files are used to identify source-code lines
-- which in turn identify target-program contexts (subprograms, loops,
-- calls, ...) to which assertions can be applied.
--
-- This package provides operations that load mark definitions from
-- text files ("mark-definition files") into an internal "mark set",
-- and operations that query the mark set for marks with desired 
-- properties. The query operations are used by the process that
-- matches assertions to target-program contexts (not implemented here).
--
-- The mark-definition files have a simple and well-defined tabular
-- format. Mark-definition files are normally created by separate tools,
-- not embedded in this program, that scan the source-code files of the
-- target program under analysis and detect and extract the mark
-- definitions. Mark definitions in source-code files are usually
-- written as comment lines, according to the comment syntax of the
-- relevant programming language, with some specific keyword or symbol
-- that identifies the comment as a marker definition.
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
-- $Revision: 1.2 $
-- $Date: 2015/10/24 19:36:46 $
--
-- $Log: assertions-source_marks.ads,v $
-- Revision 1.2  2015/10/24 19:36:46  niklas
-- Moved to free licence.
--
-- Revision 1.1  2009-03-27 13:57:12  niklas
-- BT-CH-0167: Assertion context identified by source-code markers.
--


package Assertions.Source_Marks is


   --
   ---   Marks in source files
   --


   type Marked_Part_T is (Any, Subprogram, Luup, Call);
   --
   -- A mark definition can specify the kind of program part
   -- that it marks. "Any" means "unspecified".


   type Marker_Relation_T is (Any, Here, Above, Below, Contain, Span);
   --
   -- A mark definition can specify the positional relation between
   -- the marked source-code line and the program part that will be
   -- identified by the mark.
   --
   -- Any
   --    Positional relation not specified (in the mark definition; it
   --    can still be specified in an assertion that uses the marker).
   -- Here
   --    The mark line itself, or a line very close to it.
   -- Above
   --    The program part before the mark line.
   -- Below
   --    The program part after the mark line.
   -- Contain
   --    The program part that contains instruction(s) (flow-graph
   --    steps) marked by this marker. At present, the containing part
   --    can only be a loop.
   -- Span
   --    The program part that spans the mark line, in the sense that
   --    the number of the mark line is in the range of line numbers
   --    connected to the instructions in this program part. At present,
   --    the spanning part can only be a loop.
   --


   type Mark_T is record
      Marker   : Marker_Name_T;
      File     : Source_File_Name_T;
      Line     : Line_Number_T;
      Part     : Marked_Part_T;
      Relation : Marker_Relation_T;
   end record;
   --
   -- A marked point or part in a source file.
   --
   -- Marker
   --    The marker name: an identifying string. Note that there can
   --    be several marker definitions with the same marker name, in
   --    the same or several source-code files. The marker name is a
   --    string of no particular form.
   -- File
   --    The name of the source file that contains the marked point.
   --    The source-file name is basically a string of no particular
   --    form, although for approximate matching the source-file name
   --    can be divided into two parts, the directory path (folder path)
   --    and the base name (file name). If approximate file-name
   --    matching is in use, the File component is "canonized" as
   --    required (by omitting the directory path and/or translating
   --    to lower-case letters).
   -- Line
   --    The number of the Line, in the source-code File, that is
   --    marked with this Marker. The precise meaning depends on the
   --    Relation, see below. The number is positive (>= 1).
   -- Part
   --    Optional specification of the kind of program part that
   --    is marked. "Any" means unspecified.
   -- Relation
   --    The positional relationship of the program-part that is the
   --    real target of the mark, to the marked source-code Line.
   --
   -- When an assertion defines its context by a Marker name, this name
   -- connects the assertion to some mark definitions (Mark_T), and thus
   -- to some pairs of (source File name, source Line number). These
   -- pairs then connect the assertion to the source-line-to-code-addres
   -- mapping in the target program's symbol-table, and thus to some
   -- elements of the control-flow and call graphs of the target program.
   --
   -- The Marker names have no relation to any entities in the target
   -- program's symbol-table. The names are just a link between assertions
   -- that mention this name and source-code lines marked with this name.
   --
   -- The "line" is the smallest grain of marking. We do not resolve
   -- marked points to column positions within the line.
   --
   -- The Line is not always the number of the mark line, but can
   -- be the number of some line before or after the mark line.
   -- If the mark definition specifies "above", the Line is the
   -- number of the closest preceding "markable" source-code line
   -- in the same file. If the definition specifies "below", the
   -- closest following "markable" line is used. If the definition
   -- specifies "here", the marker line itself is used.
   --
   -- What is a "markable" line depends on the source language and
   -- the tool that extracts mark definitions from the source file.
   -- The intent is that "markable" lines should be likely to cause
   -- some instructions to be generated such that the target compiler
   -- connects the addresses of those instructions to the line-number
   -- of the "markable" line. Therefore all blank lines and all
   -- comment lines are normally considered "not markable", and this
   -- usually includes mark lines themselves.
   --
   -- Consider the following example, where the lines are numbered on
   -- the left:
   --
   --   32   foo (3, x, a);
   --   33
   --   34   --Mark below "bar-call"
   --   35   --Mark above "foo-call"
   --   36
   --   37   -- Now to lower the bar:
   --   38   bar (x+y);
   --   39   --Mark "after-bar"
   --
   -- The marker "bar-call" marks line 37; "foo-call" marks line 32;
   -- and "after-bar" marks line 39.
   --
   -- The positional relation specified in a mark definition thus
   -- has a double influence on the resulting Mark_T: firstly, it
   -- defines which Line is picked as the "mark line"; secondly, it
   -- may TBA influence the assertion-matching process by defining the
   -- expected positional relation between this Line and the marked
   -- program part.
   --
   -- A mark definition might specify both "contain" (or "span") and
   -- "above" or "below". In this case, the above/below choice defines
   -- the Line, while the Relation becomes Contain (or Span) to show
   -- that the marked part is expected to contain (or span) the Line.


   function Image (Item : Mark_T) return String;
   --
   -- A description of the mark, for human understanding.


   --
   ---   The mark set
   --
   --
   -- This package maintains an internal set of marks, built up by
   -- loading mark definition files. The marks in the mark set are
   -- keyed (accessed) by the pair (source-file name, marker-name).
   -- The mark set may contain zero, one, or several marks with a
   -- given File name and a given Marker name. Moreover, the same
   -- Line may be marked by different Marker names, with the same
   -- or different Part and Relation properties.
   --
   -- Depending on command-line options, the top-level key, the
   -- source-file name, may be abbreviated to the base file name
   -- (dropping the directory paths) and/or translated to lower-case
   -- letters.


   --
   ---   Loading mark definition files
   --


   procedure Load_File (
      File_Name : in     String;
      Valid     :    out Boolean);
   --
   -- Reads the mark definition file identified File_Name and
   -- loads its mark definitions into the marker set.
   --
   -- Serious errors make Valid False; some marks may nevertheless
   -- be loaded before the error is detected.


   --
   ---   Picking marks from the mark set
   --


   type Mark_List_T is array (Positive range <>) of Mark_T;
   --
   -- A set or list of marks. Order is usually irrelevant.


   function Marks (
      Marker : Marker_Name_T;
      From   : Source_File_Name_T;
      Min    : Line_Number_T;
      Max    : Line_Number_T)
   return Mark_List_T;
   --
   -- All the known (loaded) markings with this Marker name, From
   -- this source-code file, and marking lines in the line-number
   -- range Min .. Max.
   --
   -- Marker-name matching is exact and case-sensitive.
   --
   -- Source-file-name matching can be flexible with respect
   -- to full path vs base-name matching and case-sensitivity.


end Assertions.Source_Marks;

