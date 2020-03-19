-- Assertions.Source_Marks (body)
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
-- $Date: 2015/10/24 20:05:45 $
--
-- $Log: assertions-source_marks.adb,v $
-- Revision 1.4  2015/10/24 20:05:45  niklas
-- Moved to free licence.
--
-- Revision 1.3  2010-01-30 21:13:29  niklas
-- BT-CH-0216: Subprograms have a Return_Method_T attribute.
--
-- Revision 1.2  2009-04-10 08:43:29  niklas
-- Improved error detection and error messages in mark-line parsing.
--
-- Revision 1.1  2009/03/27 13:57:12  niklas
-- BT-CH-0167: Assertion context identified by source-code markers.
--


with Ada.Characters.Handling;
with Ada.Text_IO;
with Assertions.Opt;
with Bags;                     -- MW_Components
with Bags.Bounded_Operations;  -- MW_Components
with File_System;
with Output;
with Symbols;


package body Assertions.Source_Marks is


   use type Source_File_Name_T;
   use type Line_Number_T;


   --
   ---   Marks in source files
   --


   function Image (Item : Mark_T) return String
   is
   begin

      return
           To_String (Item.Marker)
         & Output.Field_Separator
         & Symbols.Image (Item.File)
         & Output.Field_Separator
         & Output.Image (Item.Line)
         & Output.Field_Separator
         & Marked_Part_T'Image (Item.Part)
         & Output.Field_Separator
         & Marker_Relation_T'Image (Item.Relation);

   end Image;


   --
   ---   The mark set
   --

   -- The mark set is organized as a three-level keyed data structure:
   --
   -- The first key is the (canonized) source-file name, which leads
   -- to the subset of all marks in that source file (or all those
   -- source files with matching names).
   --
   -- The second key is the marker name, which leads to the subset of
   -- all marks with this marker, in the given source file.
   --
   -- The third key is the source line number. On this key we can perform
   -- range queries, which leads to all marks with the given marker name
   -- in the given line-number range of the given source file.
   --
   -- The corresponding container types are defined in the reverse order,
   -- from the third key to the first key.


   -- Using a Line number (or range) to find marks at that number (or
   -- within that range) from the set of all markers with a given
   -- Marker name in a given source File.


   function Line_Number_Of (Item : Mark_T) return Line_Number_T
   is
   begin

      return Item.Line;

   end Line_Number_Of;


   package Marks_By_Line is new Bags (
      Key_Type  => Line_Number_T,
      Item_Type => Mark_T,
      Key_Of    => Line_Number_Of,
      Count     => Natural);


   type Marks_By_Line_T is record
      Marker : Marker_Name_T;
      File   : Source_File_Name_T;
      Marks  : Marks_By_Line.Bag (Duplicate_Keys_Allowed => True);
   end record;
   --
   -- All the marks with the given Marker name, in the given source File,
   -- sorted by Line number.
   --
   -- Duplicate keys are allowed TBC for TBD reasons.


   type Marks_By_Line_Ref is access Marks_By_Line_T;
   --
   -- A reference to a Marks_By_Line_T object on the heap.
   -- We need a non-limited type for the next level of the structure.


   -- Using a Marker name to find all its marks, at any Line in
   -- a given (known) file:


   function Marker_Name_Of (Item : Marks_By_Line_Ref)
   return Marker_Name_T
   is
   begin

      return Item.Marker;

   end Marker_Name_Of;


   package Marks_By_Marker is new Bags (
      Key_Type  => Marker_Name_T,
      Item_Type => Marks_By_Line_Ref,
      Key_Of    => Marker_Name_Of,
      Count     => Natural);


   type Marks_By_Marker_T is record
      File  : Source_File_Name_T;
      Marks : Marks_By_Marker.Bag (Duplicate_Keys_Allowed => False);
   end record;
   --
   -- All the marks in the file source File, sorted by Marker name first
   -- and Line number second.
   --
   -- Duplicate keys are not allowed because we want to collect all
   -- marks with the same Marker name into a single Marks set.


   type Marks_By_Marker_Ref is access Marks_By_Marker_T;
   --
   -- A reference to a Marks_By_Marker_T object on the heap.
   -- We need a non-limited type for the next level of the structure.


   -- Using a source File name to find all the marks in this file,
   -- of whatever Marker name and Line number:


   function Source_File_Name_Of (Item : Marks_By_Marker_Ref)
   return Source_File_Name_T
   is
   begin

      return Item.File;

   end Source_File_Name_Of;


   package Marks_By_File is new Bags (
      Key_Type  => Source_File_Name_T,
      Item_Type => Marks_By_Marker_Ref,
      Key_Of    => Source_File_Name_Of,
      Count     => Natural);


   -- The set of all marks:


   Mark_Set : Marks_By_File.Bag (Duplicate_Keys_Allowed => False);
   --
   -- The set of all known (loaded) marks, in any source-code File,
   -- with any Marker name, and at any Line number; sorted by File
   -- name first, Marker name second, and Line number last.
   --
   -- Duplicate keys are not allowed because we want to collect all
   -- markers from a given source file into the same subset (of type
   -- Marks_By_Marker_T).


   --
   ---   Loading mark definition files
   --


   procedure Load (
      Mark    : in     Mark_T;
      Into    : in out Marks_By_Marker.Bag)
   --
   -- Enters the given Mark Into the set of all markers in
   -- the source file named Into.File.
   --
   is

      By_Line : Marks_By_Line_Ref;
      -- The set of all marks of this Mark.Marker name, within
      -- the source code file Marker.File.

   begin

      -- Find the set of marks for this Marker name:

      begin

         By_Line := Marks_By_Marker.Search (
            Key    => Mark.Marker,
            Within => Into);

      exception

      when Marks_By_Marker.Nonexistent_Key =>
         -- The first appearance of this Marker name.

         By_Line := new Marks_By_Line_T;

         By_Line.Marker := Mark.Marker;
         By_Line.File   := Mark.File;

         Marks_By_Marker.Insert (Item => By_Line, Into => Into);

      end;

      -- Insert the Mark at this Line:

      Marks_By_Line.Insert (Item => Mark, Into => By_Line.Marks);

   end Load;


   procedure Load (
      Mark    : in     Mark_T;
      Success : in out Boolean)
   --
   -- Enters the given Mark into the Mark_Set.
   -- If there is an error of some sort, sets Success to False,
   -- otherwise leaves Success unchanged.
   --
   is

      By_Marker : Marks_By_Marker_Ref;
      -- The set of all marks in the source code file Mark.File.

   begin

      if Opt.Trace_Marks then

         Output.Trace (
              "Mark"
            & Output.Field_Separator
            & Image (Mark));

      end if;

      -- Find the set of all marks in this File:

      begin

         By_Marker := Marks_By_File.Search (
            Key    => Mark.File,
            Within => Mark_Set);

      exception

      when Marks_By_File.Nonexistent_Key =>
         -- The first appearance of this File name.

         By_Marker := new Marks_By_Marker_T;

         By_Marker.File := Mark.File;

         Marks_By_File.Insert (Item => By_Marker, Into => Mark_Set);

      end;

      -- Then insert the Mark there:

      Load (Mark => Mark, Into => By_Marker.Marks);

   end Load;


   --
   ---   Mark-definition file syntax
   --
   --
   -- Mark-definition files are comma-separated text files.
   -- Each line defines one mark using five fields:
   --
   -- 1. The marker name.
   -- 2. The source-file name.
   -- 3. The source-line number.
   -- 4. The kind of marked part: "any", "loop", ...
   -- 5. The positional relation: "above", "below", ...
   --
   -- If the marker-name or the source-file name contain commas
   -- the whole name must enclosed in double quotes: "...".
   -- If the marker-name or the source-file name contain double
   -- quote characters (") the whole name must be enclosed in double
   -- quotes and the internal quotes must must be written as two
   -- double quotes in succession. Examples:
   --
   -- Name        Representation in file
   -- ----        ----------------------
   -- foo         foo or "foo"
   -- foo,bar     "foo,bar"
   -- foo"x       "foo""x"
   -- foo",bar    "foo"",bar"


   Mark_Syntax_Error : exception;
   --
   -- Signals a syntax error in a mark definition file.


   Comma : constant Character := ',';
   -- The field separator.


   procedure Scan_Field (
      From   : in     String;
      Start  : in out Positive;
      Value  : in out String;
      Length :    out Natural)
   --
   -- Extracts the comma-separated field starting at From(Start),
   -- updates Start to indicate the start of the next field, and
   -- returns the extracted field as Value(1 .. Length), with
   -- enclosing quotes removed and internal repeated quotes
   -- replaced by a single quote.
   --
   -- Propagates Mark_Syntax_Error in case of syntax error.
   --
   -- Updates Start to point to the comma character in From after
   -- the extracted field, or to From'Last + 1 if the field is
   -- terminated by the end of From.
   --
   is

      Quote : constant Character := '"';
      -- The quotation mark.

      Enclosed : Boolean;
      -- Whether the value is enclosed in quotes
      -- and the closing quote is not yet scanned.

      Scan : Positive := Start;
      -- The current scanned position.

   begin

      Length := 0;

      -- Check for and skip an initial enclosing quote:

      Enclosed := Scan <= From'Last and then From(Scan) = Quote;

      if Enclosed then

         Scan := Scan + 1;

      end if;

      -- Scan the rest of the field:

      loop

         exit when Scan > From'Last 
         or else ((not Enclosed) and From(Scan) = Comma);

         if From(Scan) /= Quote then
            -- The simple case.

            Length := Length + 1;

            Value(Length) := From(Scan);

            Scan := Scan + 1;

         elsif not Enclosed then
            -- Quotes are allowed only in enclosed values.

            Output.Error (
               "Mark lines must use '""' around fields that contain '""'.");

            raise Mark_Syntax_Error;

         elsif Scan < From'Last and then From(Scan + 1) = Quote then
             -- A doubled quote mark.

             Length := Length + 1;

             Value(Length) := Quote;

             Scan := Scan + 2;

         elsif Scan = From'Last or else From(Scan + 1) = Comma then
            -- The end of the quote-enclosed field.

            Enclosed := False;

            Scan := Scan + 1;

            exit;

         else
            -- A single quote in an enclosed field. Tch tch.

            Output.Error ("Mark lines must use '""""' for '""'.");

            raise Mark_Syntax_Error;

         end if;

      end loop;

      if Enclosed then
         -- There was an opening quote but no closing quote.

         Output.Error ("Mark field lacks closing '""'.");

         raise Mark_Syntax_Error;

      end if;

      if Length = 0 then
         -- A field cannot be null.

         Output.Error ("Mark line cannot have empty fields.");

         raise Mark_Syntax_Error;

      end if;

      Start := Scan;

   end Scan_Field;


   procedure Skip_Comma (
      From   : in     String;
      Start  : in out Positive)
   --
   -- Checks that From(Start) = ',' and increments Start.
   -- Propagates Mark_Syntax_Error in case of errors.
   --
   is
   begin

      if Start > From'Last then

         Output.Error ("Mark line has too few fields.");

         raise Mark_Syntax_Error;

      elsif From(Start) /= Comma then

         Output.Fault (
            Location => "Assertions.Source_Marks.Skip_Comma",
            Text     => "No comma between fields.");

         raise Mark_Syntax_Error;

      else

         Start := Start + 1;

      end if;

   end Skip_Comma;


   function To_Line_Number (Item : String)
   return Line_Number_T
   --
   -- Interprets the Item as the source-line number of a mark.
   -- Propagates Mark_Syntax_Error in case of problems.
   --
   is
   begin   

      return Line_Number_T'Value (Item);

   exception

   when Constraint_Error =>

      Output.Error (
           "Marked source-line number is wrong"
         & Output.Field_Separator
         & Item);

      raise Mark_Syntax_Error;

   end To_Line_Number;


   function To_Marked_Part (Item : String) return Marked_Part_T
   --
   -- Decodes the mnemonics for the kind of part that is marked.
   -- Propagates Mark_Syntax_Error in case of problems.
   --
   is
   begin

      if    Item = "any"        then return Any;
      elsif Item = "subprogram" then return Subprogram;
      elsif Item = "loop"       then return Luup;
      elsif Item = "call"       then return Call;
      else

         Output.Error (
             "Marked part kind unknown"
           & Item);

         raise Mark_Syntax_Error;

      end if;

   end To_Marked_Part;


   function To_Marker_Relation (Item : String) return Marker_Relation_T
   --
   -- Decodes the mnemonics for the positional relation between
   -- the mark line and the marked part.
   --
   is
   begin

      if    Item = "any"     then return Any;
      elsif Item = "here"    then return Here;
      elsif Item = "above"   then return Above;
      elsif Item = "below"   then return Below;
      elsif Item = "contain" then return Contain;
      elsif Item = "span"    then return Span;
      else

         Output.Error (
             "Marker relation unknown"
           & Item);

         raise Mark_Syntax_Error;

      end if;

   end To_Marker_Relation;


   function Canonical_Name (Name : String)
   return Symbols.Source_File_Name_T
   --
   -- The Source_File_Name_T that corresponds to the given
   -- source-file Name, optionally "canonized" by omitting
   -- directory paths and/or converting to lower case.
   --
   is
      use Ada.Characters.Handling;
      use File_System;
      use Symbols;
   begin

      case Opt.File_Matching is

      when Base_Name =>

         case Opt.File_Casing is

         when Case_Sensitive =>

            return To_Source_File_Name (File_Name (Name));

         when Case_Oblivious =>

            return To_Source_File_Name (To_Lower (File_Name (Name)));

         end case;

      when Full_Path =>

         case Opt.File_Casing is

         when Case_Sensitive =>

            return To_Source_File_Name (Name);

         when Case_Oblivious =>

            return To_Source_File_Name (To_Lower (Name));

         end case;

      end case;

   end Canonical_Name;


   function Canonical_Name (Name : Symbols.Source_File_Name_T)
   return Symbols.Source_File_Name_T
   --
   -- The possibly "canonized" Name, assuming that the given Name
   -- is the full and case-correct name.
   --
   is
   begin

      if  Opt.File_Matching = Full_Path
      and Opt.File_Casing   = Case_Sensitive
      then
         -- No changed.

         return Name;

      else
         -- Canonize the string:

         return Canonical_Name (Symbols.Image (Name));

      end if;

   end Canonical_Name;


   procedure Parse_And_Load_Mark (
      Line  : in     String;
      Valid : in out Boolean)
   --
   -- Parses the mark-definition Line and, if Valid,
   -- loads the mark definition into the mark set.
   -- Sets Valid to False if any error is detected.
   --
   is

      Start : Positive := Line'First;
      -- The start of the next field.

      Field : String (1 .. Line'Length);
      -- One of the fields on the Line.

      Length : Natural;
      -- The length of the Field.

      Mark : Mark_T;
      -- The mark defined by the Line.

      procedure Scan
      is
      begin

         Scan_Field (
            From  => Line , Start  => Start,
            Value => Field, Length => Length);

      end Scan;

      procedure Skip_Comma
      is
      begin

         Skip_Comma (From => Line, Start => Start);

      end Skip_Comma;


   begin  -- Parse_And_Load_Mark

      -- 1. The marker name.

      Scan;

      Mark.Marker := To_Item (Field(1 .. Length));

      -- 2. The source-file name.

      Skip_Comma; Scan;

      Mark.File := Canonical_Name (Field(1 .. Length));

      -- 3. The source-line number.

      Skip_Comma; Scan;

      Mark.Line := To_Line_Number (Field(1 .. Length));

      -- 4. The kind of marked part: "any", "loop", ...

      Skip_Comma; Scan;

      Mark.Part := To_Marked_Part (Field(1 .. Length));

      -- 5. The positional relation: "above", "below", ...

      Skip_Comma; Scan;

      Mark.Relation := To_Marker_Relation (Field(1 .. Length));

      -- And no more:

      if Start <= Line'Last then
         -- There is something more on the Line.

         Output.Error (
              "Mark line has excess text"
            & Output.Field_Separator
            & '"' & Line(Start .. Line'Last) & '"');

         raise Mark_Syntax_Error;

      end if;

      -- The definition is valid, so we load it:

      Load (Mark => Mark, Success => Valid);

   exception

   when Mark_Syntax_Error =>

      Output.Error (
           "Mark syntax"
         & Output.Field_Separator
         & Line);

      Valid := False;

   when X : others =>

      Output.Exception_Info (
         Text       => "Exception in Parse_And_Load_Mark",
         Occurrence => X);

   end Parse_And_Load_Mark;


   Max_Line_Length : constant := 1_000;
   --
   -- The maximum length of a mark-definition file line.


   procedure Load_File (
      File_Name : in     String;
      Valid     :    out Boolean)
   is
      use type Ada.Text_IO.Count;

      File : Ada.Text_IO.File_Type;
      -- The file named File_Name.

      Text : String (1 .. Max_Line_Length);
      Last : Natural;
      -- An input line.

      Number : Output.Line_Number_T;
      -- The number of the current line (Text) in the File.

      Mark : Output.Nest_Mark_T;
      -- Marks the locus File:Number.

   begin

      Valid := True;
      -- We know nothing to the contrary as yet.

      Output.Note (
           "Reading marks from "
         & File_Name
         & ".");

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

         Ada.Text_IO.Get_Line (File, Text, Last);

         if Ada.Text_IO.Col (File) = 1 then
            -- Good, we read all of a line.

            Parse_And_Load_Mark (
               Line  => Text(1 .. Last),
               Valid => Valid);

         else

            Output.Error (
                 "Mark line is too long (over"
               & Natural'Image (Text'Length)
               & " characters).");

            Valid := False;

         end if;

         Output.Unnest (Mark);

      end loop;

      Ada.Text_IO.Close (File);

      Output.Note (
           "Finished marks from "
         & File_Name
         & ".");

   exception

   when Ada.Text_IO.Name_Error =>

      Output.Error (
           "Could not open the mark file """
         & File_Name
         & """.");

      if Ada.Text_IO.Is_Open (File) then

         Ada.Text_IO.Close (File);

      end if;

      Valid := False;

   end Load_File;


   --
   ---   Picking marks from the mark set
   --


   package Marks_By_Line_Ranges is new Marks_By_Line.Bounded_Operations;
   --
   -- Operations to pick and traverse marks in certain ranges
   -- of line number, within a set of all marks for a given marker
   -- name, in a given source-code file.


   No_Marks : Mark_List_T (1 .. 0);
   --
   -- An empty set of marks.


   function Marks (
      Marker : Marker_Name_T;
      From   : Source_File_Name_T;
      Min    : Line_Number_T;
      Max    : Line_Number_T)
   return Mark_List_T
   is

      From_Marks : Marks_By_Marker_Ref;
      -- The marks in the file From, sorted by marker name.

      From_Marker_Marks : Marks_By_Line_Ref;
      -- The marks in From with this Marker name, sorted by line number.

   begin

      -- Find the marks From this source-code file:

      From_Marks := Marks_By_File.Search (
         Key    => Canonical_Name (From),
         Within => Mark_Set);
      -- May raise Nonexistent_Key.

      -- From them, find the marks with this Marker name:

      From_Marker_Marks := Marks_By_Marker.Search (
         Key    => Marker,
         Within => From_Marks.Marks);
      -- May raise Nonexistent_Key.

      -- From them, find the marks that mark lines in the range Min .. Max:

      declare

         List : Mark_List_T (1 .. Marks_By_Line.Card (From_Marker_Marks.Marks));
         Num  : Natural := 0;
         -- The marks found will be in List(1 .. Num).

         procedure Add_To_List (Item : Mark_T)
         is
         begin
            Num       := Num + 1;
            List(Num) := Item;
         end Add_To_List;

         procedure List_Marks_In_Range is new
            Marks_By_Line_Ranges.Bounded_Traversal (Action => Add_To_List);

      begin

         List_Marks_In_Range (
            On_Bag => From_Marker_Marks.Marks,
            First  => Min,
            Last   => Max);

         if Opt.Trace_Marks then

            Output.Trace (
                 "Number of """
               & To_String (Marker)
               & """ marks in file """
               & Symbols.Image (From)
               & """ on lines "
               & Output.Image (Min)
               & ".."
               & Output.Image (Max)
               & Output.Field_Separator
               & Output.Image (Num));

            for L in 1 .. Num loop

               Output.Trace (
                    "Mark #"
                  & Output.Image (L)
                  & Output.Field_Separator
                  & Image (List(L)));

            end loop;

         end if;

         return List(1 .. Num);

      end;

   exception

   when Marks_By_File.Nonexistent_Key =>
      -- No marks in the file, From.

      if Opt.Trace_Marks then

         Output.Trace (
              "No marks in file """
            & Symbols.Image (From)
            & """.");

      end if;

      return No_Marks;

   when Marks_By_Marker.Nonexistent_Key =>
      -- Some marks in this file, From, but none with this Marker name.

      if Opt.Trace_Marks then

         Output.Trace (
              "No """
            & To_String (Marker)
            & """ marks in file """
            & Symbols.Image (From)
            & """.");

      end if;

      return No_Marks;

   end Marks;


end Assertions.Source_Marks;
