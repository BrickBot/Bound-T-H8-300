-- Output (body)
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
-- $Revision: 1.39 $
-- $Date: 2015/10/24 20:05:50 $
--
-- $Log: output.adb,v $
-- Revision 1.39  2015/10/24 20:05:50  niklas
-- Moved to free licence.
--
-- Revision 1.38  2013-11-27 11:44:42  niklas
-- Added Locus (Code_Address_T) return Statement_Locus_T.
--
-- Revision 1.37  2011-08-31 04:23:34  niklas
-- BT-CH-0222: Option registry. Option -dump. External help files.
--
-- Revision 1.36  2009/05/21 08:08:27  niklas
-- BT-CH-0175: Limits on the number of Warnings, Errors, Faults.
--
-- Revision 1.35  2009/03/24 07:48:35  niklas
-- BT-CH-0166: String_Pool.Item_T for source-file and marker names.
--
-- Revision 1.34  2009/03/21 13:09:17  niklas
-- BT-CH-0165: Option -file_match for matching asserted file-names.
--
-- Revision 1.33  2009/03/19 13:52:02  niklas
-- Extended the "&" operators for Statement_Range_T and Locus_T to
-- add surrounding lines for the Right operand (under -lines around).
--
-- Revision 1.32  2008/11/09 21:43:03  niklas
-- BT-CH-0158: Output.Image (Time_T) replaces Programs.Execution.Image.
--
-- Revision 1.31  2008/02/15 20:27:43  niklas
-- BT-CH-0110: Better "&" for call-paths in Output loci.
--
-- Revision 1.30  2007/04/30 06:39:55  niklas
-- Made No_Mark public and the default initial value.
--
-- Revision 1.29  2006/06/16 14:51:13  niklas
-- Published the function Formed_Source with a Default value
-- for the Form parameter.
--
-- Revision 1.28  2006/05/17 20:07:13  niklas
-- Added the function Source_File_Item.
--
-- Revision 1.27  2006/02/28 08:47:15  niklas
-- Extended the option -source (Output.Opt.Source_File_Form) to
-- apply also to the name of the target program executable file.
-- Thus, the function Program_File (Locus) now takes also a
-- Form parameter and applies Formed_Source to the file-name.
--
-- Revision 1.26  2005/10/26 14:11:16  niklas
-- Using Basic_Output.
--
-- Revision 1.25  2005/10/26 12:19:46  niklas
-- Simplified EFS checks in procedure Line.
--
-- Revision 1.24  2005/10/09 08:10:22  niklas
-- BT-CH-0013.
--
-- Revision 1.23  2005/08/24 10:06:50  niklas
-- Added an "Address" parameter (Boolean option) to the Image
-- functions for Statement_Locus_T, Statement_Range_T and
-- Source_Interval_T. This lets clients force the display of
-- code addresses in warnings and errors that need it, as if
-- the option -address were in effect.
--
-- Revision 1.22  2005/08/08 17:42:04  niklas
-- Added functions First and Last to get the line-number range
-- directly from a Statement Range.
--
-- Revision 1.21  2005/06/29 09:33:45  niklas
-- Added the Heading procedure.
--
-- Revision 1.20  2005/06/28 07:02:21  niklas
-- Added function Code_Image.
--
-- Revision 1.19  2004/04/25 09:29:45  niklas
-- First Tidorum version.
-- Multiple source files per location ("See_Also" lines).
-- Options for source-file name form (full path or basename).
-- Inexact source-line matching (surrounding lines).
-- Operations for tracing output ("Trace" lines).
-- Additional control over "Result" and "Unknown" output.
-- Caching of current default locus computed from locus nest.
--
-- Revision 1.18  2003/02/17 16:17:08  holsti
-- Added option Show_Code_Addresses (-address) to include the code address
-- in all output of code loci. Earlier, this was the default; now the
-- default is to suppress the code addresses and show only source-line
-- numbers if available.
--
-- Revision 1.17  2001/12/10 14:43:11  holsti
-- Locus-nest marks have a "Defined" attribute to avoid accessing
-- undefined (uninitialized) marks.
--
-- Revision 1.16  2001/05/27 10:50:33  holsti
-- "Note" output is conditional on Opt.Show_Notes.
--
-- Revision 1.15  2001/04/06 11:08:07  ville
-- NC_054 fixed
--
-- Revision 1.14  2001/04/04 11:02:14  ville
-- Fixed invalid Code_Address_T uses
--
-- Revision 1.13  2001/03/21 20:12:43  holsti
-- Program locations given by Locus_T.
--
-- Revision 1.12  2001/03/15 20:47:37  holsti
-- Show_Notes (option -q, -quiet) added.
--
-- Revision 1.11  2000/12/28 12:35:03  holsti
-- Image for Integer added.
--
-- Revision 1.10  2000/11/24 12:06:01  sihvo
-- Added stack height analysis.
--
-- Revision 1.9  2000/11/09 14:44:31  saarinen
-- Added function Get_Subprogram.
--
-- Revision 1.8  2000/10/26 09:30:00  saarinen
-- Added procedures Wcet, Wcet_Call, Unknown and Loop_Bound.
--
-- Revision 1.7  2000/07/02 18:44:45  holsti
-- Flush standard-output before messages.
--
-- Revision 1.6  2000/06/27 20:10:03  holsti
-- Made Fault useful...
--
-- Revision 1.5  2000/06/27 20:05:27  holsti
-- Added procedure Fault.
--
-- Revision 1.4  2000/04/24 18:36:19  holsti
-- Added Subprogram field.
--
-- Revision 1.3  2000/04/24 14:28:38  holsti
-- Symbol scopes added.
--
-- Revision 1.2  2000/04/23 21:41:16  holsti
-- Added Flow package.
--
-- Revision 1.1  2000/04/22 12:08:27  holsti
-- Output package added.
--


with Ada.Strings;
with Ada.Strings.Fixed;
with Ada.Strings.Maps;

with Output.Opt;
with Symbols.Show;


package body Output is

   use Ada.Text_IO;

   use type String_Pool.Item_T;

   use type Symbols.Line_Number_T;
   use type Symbols.Source_File_Name_T;
   use type Symbols.Symbol_Table_T;


   --
   ---   Source-file form selection
   --


   Path_Delimiters : Ada.Strings.Maps.Character_Set :=
      Ada.Strings.Maps.To_Set ("/\");
   --
   -- The characters that delimit directories and file-names in
   -- a path-name string.


   function Formed_Source (
      Full_Name : String;
      Form      : Source_File_Form_T := Default)
   return String
   is
      use Ada.Strings;
      use Ada.Strings.Fixed;

      Real_Form : Real_Source_File_Form_T;
      -- The real form after expanding Default.

      Last_Name : Natural;
      -- The index, in Full_Name, of the last valid file-name character,
      -- that is a character other than a path delimiter.

      Last_Delimiter : Natural;
      -- The index, in Full_Name, of the last path delimiter before
      -- the Last_Name character.

      First_Name : Positive;
      -- The index, in Full_Name, of the first valid file-name character
      -- after Last_Delimiter.

   begin

      if Form = Default then

         Real_Form := Opt.Source_File_Form;

      else

         Real_Form := Form;

      end if;

      case Real_Form is

      when Full =>

         return Full_Name;

      when Base =>

         Last_Name := Index (
            Source => Full_Name,
            Set    => Path_Delimiters,
            Test   => Outside,
            Going  => Backward);

         Last_Delimiter := Index (
            Source => Full_Name(Full_Name'First .. Last_Name),
            Set    => Path_Delimiters,
            Test   => Inside,
            Going  => Backward);

         if Last_Delimiter = 0 then
            -- There are no path delimiters before Last_Name.

            First_Name := Full_Name'First;

         else
            -- There are some delimiters before Last_Name.

            First_Name := Last_Delimiter + 1;

         end if;

         return Full_Name (First_Name .. Last_Name);

      end case;

   end Formed_Source;


   --
   ---   Output operations
   --


   procedure Line (
      Channel : in Ada.Text_IO.File_Type;
      Key     : in String;
      Locus   : in Locus_T;
      Data    : in String);


   procedure Flush_Standard_Output
   renames Basic_Output.Flush_Standard_Output;


   function To_Item (S : String) return String_Pool.Item_T
   --
   -- Stores the given string in the String_Pool and returns the
   -- corresponding Item_T value, or return Null_Item if the
   -- given string is null.
   --
   is
   begin
      if S'Length = 0 then
         return String_Pool.Null_Item;
      else
         return String_Pool.To_Item (S);
      end if;
   end To_Item;


   function To_String (Item : String_Pool.Item_T)
   return String
   --
   -- Returns the string identified by the Item in the string pool,
   -- or returns the null string if Item = Null_Item.
   --
   is
   begin
      if Item = String_Pool.Null_Item then
         return "";
      else
         return String_Pool.To_String (Item);
      end if;
   end To_String;


   function No_Statements return Statement_Range_T
   is
   begin
      return No_Statement_Range;
   end No_Statements;


   --
   ---   Diagnostics
   --


   procedure Show (
      Item   : in Statement_Locus_T;
      File   : in Ada.Text_IO.File_Type;
      Indent : in Ada.Text_IO.Positive_Count)
   --
   -- Displays the statement locus on the given file.
   -- Intended for diagnostics.
   --
   is
   begin

      Set_Col (File, Indent);
      Put (File, "Source file : ");

      if Item.Source_File = Symbols.Null_Name then
         Put_Line (File, "null");
      else
         Put_Line (File, '"' & Symbols.Image (Item.Source_File) & '"');
      end if;

      Set_Col (File,Indent);
      Put (File, "Line number : ");

      if Item.Line.Known then
         Put_Line (File, Image (Item.Line.Number));
      else
         Put_Line (File, "?");
      end if;

      Set_Col (File, Indent);
      Put (File,"Code address: ");

      if Item.Code.Known then
         Put_Line (File, Processor.Image (Item.Code.Address));
      else
         Put_Line (File, "?");
      end if;

   end Show;


   procedure Show (
      Item   : in Source_Interval_T;
      File   : in Ada.Text_IO.File_Type;
      Indent : in Ada.Text_IO.Positive_Count)
   --
   -- Displays the source-line interval on the given file.
   -- Intended for diagnostics.
   --
   is
   begin

      Set_Col (File, Indent);
      Put (File, "Source file : ");

      if Item.File = Symbols.Null_Name then
         Put_Line (File, "null");
      else
         Put_Line (File, '"' & Symbols.Image (Item.File) & '"');
      end if;

      Set_Col (File, Indent);
      Put (File, "First line  : ");

      if Item.First.Known then
         Put_Line (File, Image (Item.First.Number));
      else
         Put_Line (File, "?");
      end if;

      Set_Col (File, Indent);
      Put (File, "Last  line  : ");

      if Item.Last.Known then
         Put_Line (File, Image (Item.Last.Number));
      else
         Put_Line (File, "?");
      end if;

   end Show;


   procedure Show (
      Item   : in Code_Interval_T;
      File   : in Ada.Text_IO.File_Type;
      Indent : in Ada.Text_IO.Positive_Count)
   --
   -- Displays the code-address interval on the given file.
   -- Intended for diagnostics.
   --
   is
   begin

      Set_Col (File, Indent);
      Put (File, "First code : ");

      if Item.First.Known then
         Put_Line (File, Processor.Image (Item.First.Address));
      else
         Put_Line (File, "?");
      end if;

      Set_Col (File, Indent);
      Put (File, "Last  code : ");

      if Item.Last.Known then
         Put_Line (File, Processor.Image (Item.Last.Address));
      else
         Put_Line (File, "?");
      end if;

   end Show;


   procedure Show (
      Item : in Statement_Range_T;
      File : in Ada.Text_IO.File_Type)
   --
   -- Displays the statement range on the given file.
   -- Intended for diagnostics.
   --
   is
   begin

      Put_Line (File,
           "Statement Range with"
         & Natural'Image (Item.Sources)
         & " source intervals:");

      for S in 1 .. Item.Sources loop

         Put_Line (File, "   #" & Natural'Image (S) & " :");

         Show (Item.Source(S), File, 6);

      end loop;

      Show (Item.Code, File, 3);

   end Show;


   procedure Show (
      Item : in Locus_T;
      File : in Ada.Text_IO.File_Type)
   --
   -- Displays the locus on the given file.
   -- Intended for diagnostics.
   --
   is
   begin

      Put_Line (File, "Locus:");

      Put_Line (File, "   Program  : " & To_String (Item.Program_File));
      Put_Line (File, "   Source   : " & Symbols.Image (Item.Source_File));
      Put_Line (File, "   Call Path: " & To_String (Item.Call_Path));

      Show (Item.Statements, File);

   end Show;


   --
   ---  Querying statement loci attributes
   --


   function Source_File (
      Item : Statement_Locus_T;
      Form : Source_File_Form_T := Default)
   return String
   is
   begin
      return Formed_Source (Symbols.Image (Item.Source_File), Form);
   end Source_File;


   function Line_Number (Item : Statement_Locus_T)
   return Line_Number_T
   is
   begin
      if Item.Line.Known then
         return Item.Line.Number;
      else
         return No_Line_Number;
      end if;
   end Line_Number;


   function Code_Address (Item : Statement_Locus_T)
   return Processor.Code_Address_T
   is
   begin
      if Item.Code.Known then
         return Item.Code.Address;
      else
         raise Program_Error;
      end if;
   end Code_Address;


   function Number_Of_Sources (Item : Statement_Range_T)
   return Natural
   is
   begin

      return Item.Sources;

   end Number_Of_Sources;


   procedure Wrong_Source (
      Location : in String;
      Source   : in Source_Ordinal_T;
      Sources  : in Natural)
   --
   -- Reports a fault in a Source ordinal parameter, which is
   -- larger than the number of Sources in the statement range.
   --
   is
   begin

      -- This is a Fault, but we cannot call the Fault
      -- output operation because it could lead to a recursive
      -- cycle.

      Line (
         Channel => Standard_Error,
         Key     => "Fault",
         Locus   => No_Locus,
         Data    =>
              Location
            & Field_Separator
            & "Source ordinal out of range"
            & Field_Separator
            & Image (Source)
            & Field_Separator
            & Image (Sources));

   end Wrong_Source;


   function Source_File (
      Item   : Statement_Range_T;
      Source : Source_Ordinal_T := First_Source;
      Form   : Source_File_Form_T := Default)
   return String
   is
   begin

      if Source <= Item.Sources then

         return Formed_Source (Symbols.Image (Item.Source(Source).File), Form);

      else

         Wrong_Source (
            Location => "Output.Source_File",
            Source   => Source,
            Sources  => Item.Sources);

         return "";

      end if;

   end Source_File;


   function Source_File (
      Item   : Statement_Range_T;
      Source : Source_Ordinal_T  := First_Source)
   return Symbols.Source_File_Name_T
   is
   begin

      if Source <= Item.Sources then

         return Item.Source(Source).File;

      else

         Wrong_Source (
            Location => "Output.Source_File_Item",
            Source   => Source,
            Sources  => Item.Sources);

         return Symbols.Null_Name;

      end if;

   end Source_File;


   function First (
      Item   : Statement_Range_T;
      Source : Source_Ordinal_T := First_Source)
   return Statement_Locus_T
   is
   begin

      if Source <= Item.Sources then

         return (
            Source_File  => Item.Source(Source).File,
            Line         => Item.Source(Source).First,
            Code         => Item.Code.First,
            Symbol_Table => Item.Symbol_Table);

      else

         Wrong_Source (
            Location => "Output.First",
            Source   => Source,
            Sources  => Item.Sources);

        return No_Statement;

      end if;

   end First;


   function Last (
      Item   : Statement_Range_T;
      Source : Source_Ordinal_T := First_Source)
   return Statement_Locus_T
   is
   begin

      if Source <= Item.Sources then

         return (
            Source_File  => Item.Source(Source).File,
            Line         => Item.Source(Source).Last,
            Code         => Item.Code.Last,
            Symbol_Table => Item.Symbol_Table);

      else

         Wrong_Source (
            Location => "Output.Last",
            Source   => Source,
            Sources  => Item.Sources);

        return No_Statement;

      end if;

   end Last;


   function First (
      Item   : Statement_Range_T;
      Source : Source_Ordinal_T := First_Source)
   return Line_Number_T
   is
   begin

      return Line_Number (First (Item, Source));

   end First;


   function Last (
      Item   : Statement_Range_T;
      Source : Source_Ordinal_T := First_Source)
   return Line_Number_T
   is
   begin

      return Line_Number (Last (Item, Source));

   end Last;


   --
   ---  Constructing statement loci and ranges
   --


   function Locus (
      Source_File  : String        := "";
      Line_Number  : Line_Number_T := No_Line_Number;
      Code_Address : Processor.Code_Address_T;
      Symbol_Table : Symbols.Symbol_Table_T )
   return Statement_Locus_T
   is

      Statement : Statement_Locus_T :=
         Locus (Source_File, Line_Number);
      --
      -- The result to be, initialised with source-file
      -- and line-number, unknown code-address, no symbol-table.

   begin

      Statement.Code := (
         Known   => True,
         Address => Code_Address);

      Statement.Symbol_Table := Symbol_Table;

      return Statement;

   end Locus;


   function Locus (
      Source_File  : String := "";
      Line_Number  : Line_Number_T)
   return Statement_Locus_T
   is
      Statement : Statement_Locus_T;
      -- The result to be.
   begin

      Statement.Source_File := Symbols.To_Source_File_Name (Source_File);

      if Line_Number = No_Line_Number then
         Statement.Line := (Known => False);
      else
         Statement.Line := (
            Known  => True,
            Number => Line_Number);
      end if;

      Statement.Code := (Known => False);

      Statement.Symbol_Table := Symbols.No_Symbol_Table;

      return Statement;

   end Locus;


   function Locus (Code_Address : Processor.Code_Address_T)
   return Statement_Locus_T
   is
   begin

      return (
         Source_File  => Symbols.Null_Name,
         Line         => (Known => False),
         Code         => (Known => True, Address => Code_Address),
         Symbol_Table => Symbols.No_Symbol_Table);

   end Locus;


   function Min (Left, Right : Possible_Line_Number_T)
   return Possible_Line_Number_T
   --
   -- The smaller line number, ignoring unknown values.
   --
   is
   begin
      if    not Left.Known  then return Right;
      elsif not Right.Known then return Left;
      else
         return (
            Known  => True,
            Number => Line_Number_T'Min (Left.Number, Right.Number));
      end if;
   end Min;


   function Max (Left, Right : Possible_Line_Number_T)
   return Possible_Line_Number_T
   --
   -- The larger line number, ignoring unknown values.
   --
   is
   begin
      if    not Left.Known  then return Right;
      elsif not Right.Known then return Left;
      else
         return (
            Known  => True,
            Number => Line_Number_T'Max (Left.Number, Right.Number));
      end if;
   end Max;


   function Min (Left, Right : Possible_Code_Address_T)
   return Possible_Code_Address_T
   --
   -- The smaller code address, ignoring unknown values.
   --
   is
      use type Processor.Code_Address_T;

      Smaller : Processor.Code_Address_T;
      -- The smaller address, when both known.

   begin

      if    not Left.Known  then return Right;
      elsif not Right.Known then return Left;
      else
         -- Both known.

         if Left.Address < Right.Address then
            Smaller := Left.Address;
         elsif Right.Address < Left.Address then
            Smaller := Right.Address;
         else
            Smaller := Left.Address;
         end if;

         return (
            Known   => True,
            Address => Smaller);

      end if;

   end Min;


   function Max (Left, Right : Possible_Code_Address_T)
   return Possible_Code_Address_T
   --
   -- The larger code address, ignoring unknown values.
   --
   is
      use type Processor.Code_Address_T;

      Larger : Processor.Code_Address_T;
      -- The larger address, when both known.

   begin

      if    not Left.Known  then return Right;
      elsif not Right.Known then return Left;
      else
         -- Both known.

         if Left.Address < Right.Address then
            Larger := Right.Address;
         elsif Right.Address < Left.Address then
            Larger := Left.Address;
         else
            Larger := Left.Address;
         end if;

         return (
            Known   => True,
            Address => Larger);

      end if;

   end Max;


   function Same_Source (
      Left, Right : in Statement_Locus_T;
      Location    : in String)
   return Symbols.Source_File_Name_T
   --
   -- Check that the two statements are in the same source-code
   -- file (if the file is known in both).
   -- Return the common source-code file-name.
   --
   is
   begin

      if Left.Source_File = Symbols.Null_Name
      or Left.Source_File = Right.Source_File then

         return Right.Source_File;

      elsif Right.Source_File = Symbols.Null_Name then

         return Left.Source_File;

      else
         -- This is a Fault, but we cannot call the Fault
         -- output operation because it could lead to a recursive
         -- cycle.

         Line (
            Channel => Standard_Error,
            Key     => "Fault",
            Locus   => No_Locus,
            Data    =>
                 Location
               & Field_Separator
               & "Conflicting source-files"
               & Field_Separator
               & Symbols.Image (Left.Source_File)
               & Field_Separator
               & Symbols.Image (Right.Source_File));

         return Symbols.Null_Name;
         -- On the safe side, we don't know...

      end if;

   end Same_Source;


   function Same_Symbols (
      Left, Right : in Symbols.Symbol_Table_T;
      Location    : in String)
   return Symbols.Symbol_Table_T
   --
   -- Check that the two symbol-tables are the same, or one or both
   -- are null.
   -- Return the common symbol-table.
   --
   is
      use Symbols;
   begin

      if Left = No_Symbol_Table
      or Left = Right then

         return Right;

      elsif Right = No_Symbol_Table then

         return Left;

      else
         -- This is a Fault, but we cannot call the Fault
         -- output operation because it could lead to a recursive
         -- cycle.

         Line (
            Channel => Standard_Error,
            Key     => "Fault",
            Locus   => No_Locus,
            Data    =>
                 Location
               & Field_Separator
               & "Conflicting symbol tables");

         return No_Symbol_Table;
         -- On the safe side, we don't know...

      end if;

   end Same_Symbols;


   function Rainge (
      First : Statement_Locus_T;
      Last  : Statement_Locus_T)
   return Statement_Range_T
   is

      Source : Symbols.Source_File_Name_T;
      -- The common source-file name.

      Table : Symbols.Symbol_Table_T;
      -- The common symbol-table.

      Autograph : constant String := "Output.Rainge";

      Result : Statement_Range_T;

   begin

      Source := Same_Source (First, Last, Autograph);

      Table := Same_Symbols (
         First.Symbol_Table,
         Last .Symbol_Table,
         Autograph);

      if Source = Symbols.Null_Name then
         -- No source-line/source-file info.

         Result.Sources := 0;

      else
         -- Some source-line/source-file info.

         Result.Sources := 1;

         Result.Source(First_Source) := (
            File  => Source,
            First => Min (First.Line, Last.Line),
            Last  => Max (First.Line, Last.Line) );

      end if;

      Result.Code := (
         First => Min (First.Code, Last.Code),
         Last  => Max (First.Code, Last.Code));

      Result.Symbol_Table := Table;

      return Result;

   end Rainge;


   function "+" (Item : Statement_Locus_T)
   return Statement_Range_T
   is

      Result : Statement_Range_T;

   begin

      if Item.Source_File = Symbols.Null_Name then
         -- No source-line/source-file info.

         Result.Sources := 0;

      else
         -- Some source-line information given.

         Result.Sources := 1;

         Result.Source(First_Source) := (
            File  => Item.Source_File,
            First => Item.Line,
            Last  => Item.Line);

      end if;

      Result.Code := (
         First => Item.Code,
         Last  => Item.Code);

      Result.Symbol_Table := Item.Symbol_Table;

      return Result;

   end "+";


   function Same_Source (Left, Right : Source_Interval_T)
   return Boolean
   --
   -- Whether the two source intervals lie in the same source-file.
   --
   is
   begin

      return Left.File = Right.File;

   end Same_Source;


   procedure Check_Same_File (Left, Right : Source_Interval_T)
   --
   -- Checks that the two source intervals come from the same source file.
   --
   is
   begin

      if not Same_Source (Left, Right) then

         Line (
            Channel => Standard_Error,
            Key     => "Fault",
            Locus   => No_Locus,
            Data    =>
                 "Output.Check_Same_File"
               & Field_Separator
               & "Source files different"
               & Field_Separator
               & Symbols.Image (Left.File)
               & Field_Separator
               & Symbols.Image (Right.File));

      end if;

   end Check_Same_File;


   function "+" (Left, Right : Source_Interval_T)
   return Source_Interval_T
   --
   -- Union of two non-null source-line intervals from
   -- the same source-file.
   --
   is
   begin

      Check_Same_File (Left, Right);

      return (
         File  => Left.File,
         First => Min (Left.First, Right.First),
         Last  => Max (Left.Last , Right.Last ));

   end "+";


   function "+" (Left, Right : Code_Interval_T)
   return Code_Interval_T
   --
   -- Union of two non-null code address intervals.
   --
   is
   begin

      return (
         First => Min (Left.First, Right.First),
         Last  => Max (Left.Last , Right.Last ));

   end "+";


   function "+" (Left, Right : Statement_Range_T)
   return Statement_Range_T
   is

      Result : Statement_Range_T;
      -- The result (union) when Left and Right are non-null.

      Shared_Source : Boolean;
      -- Whether the current Right interval shares a common
      -- source-file with an interval in Result.

      S : Source_Ordinal_T;
      -- The source ordinal in Result for the shared source.

      Autograph : constant String := "Output.""+"" (Statement_Range_T)";

   begin

      -- Source line intervals:

      if Left.Sources = 0 then
         -- No source lines in Left: use Right.

         Result.Sources := Right.Sources;
         Result.Source  := Right.Source;

      elsif Right.Sources = 0 then
         -- No source lines in Right: use Left.

         Result.Sources := Left.Sources;
         Result.Source  := Left.Source;

      else
         -- Both Left and Right contain some source lines.
         -- Unite their source intervals per source file:

         Result.Sources := Left.Sources;
         Result.Source  := Left.Source;

         for R in 1 .. Right.Sources loop
            -- Process Right.Source(R).
            -- Look for the same source-file in the Result:

            S := Result.Source'First;

            Finding_Source: loop

               Shared_Source :=
                  Same_Source (
                     Result.Source(S),
                     Right .Source(R));

               exit Finding_Source when
                  Shared_Source or S = Result.Sources;

               S := S + 1;

            end loop Finding_Source;

            if Shared_Source then
               -- This source-file already appears in the Result.
               -- Unite the source intervals:

               Result.Source(S) :=
                    Result.Source(S)
                  + Right .Source(R);

            elsif Result.Sources < Result.Source'Last then
               -- This source does not appear in Result yet,
               -- but there is room to add it:

               Result.Sources := Result.Sources + 1;

               Result.Source(Result.Sources) := Right.Source(R);

            else
               -- This source does not appear in Result, and
               -- Result already has the maximum number of sources.

               -- This is a Fault, but we cannot call the Fault
               -- output operation because it could lead to a recursive
               -- cycle.

               Line (
                  Channel => Standard_Error,
                  Key     => "Fault",
                  Locus   => No_Locus,
                  Data    =>
                       Autograph
                     & Field_Separator
                     & "Too many source files, ignoring"
                     & Field_Separator
                     & Symbols.Image (Right.Source(R).File));

                  Show (Left , Standard_Error);
                  Show (Right, Standard_Error);

            end if;

         end loop;

      end if;

      -- Code address interval:

      Result.Code := Left.Code + Right.Code;

      -- Symbol table:

      Result.Symbol_Table :=
         Same_Symbols (
            Left .Symbol_Table,
            Right.Symbol_Table,
            Autograph);

      -- And that is all:

      return Result;

   end "+";


   function "+" (
      Left  : Statement_Range_T;
      Right : Statement_Locus_T)
   return Statement_Range_T
   is
   begin
      return Left + (+ Right);
   end "+";


   procedure Intersect (
      Left, Right : in     Source_Interval_T;
      Inter       :    out Source_Interval_T;
      Empty       :    out Boolean)
   --
   -- The intersection (common statements) of two source-line intervals
   -- from the same source file.
   -- It is considered a Fault if the intersection is empty.
   --
   is
   begin

      Check_Same_File (Left, Right);

      Inter := (
         File  => Left.File,
         First => Max (Left.First, Right.First),
         Last  => Min (Left.Last , Right.Last ));

      -- Check for empty intersection:

      Empty := False;

      if  (    Inter.First.Known
           and Inter.Last .Known)
      and then
          Inter.Last.Number < Inter.First.Number
      then

          Empty := True;

          Fault (
             Location => "Output.Intersection(Source_Interval_T)",
             Text     => "Line-range intersection is empty.");

      end if;

   end Intersect;


   procedure Intersect (
      Left, Right : in     Code_Interval_T;
      Inter       :    out Code_Interval_T;
      Empty       :    out Boolean)
   --
   -- The intersection (common addresses) of two code address intervals.
   -- It is considered a Fault if the intersection is empty.
   --
   is
      use type Processor.Code_Address_T;
   begin

      Inter := (
         First => Max (Left.First, Right.First),
         Last  => Min (Left.Last , Right.Last ));

      -- Check for empty intersection:

      Empty := False;

      if  (    Inter.First.Known
           and Inter.Last .Known)
      and then
          Inter.Last.Address < Inter.Last.Address
      then

          Empty := True;

          Fault (
             Location => "Output.Intersection(Code_Interval_T)",
             Text     => "Code-range intersection is empty.");

      end if;

   end Intersect;


   function "&" (Left, Right : Statement_Range_T)
   return Statement_Range_T
   is

      Rext : Statement_Range_T := Right;
      -- The Right range, perhaps extended with surrounding lines.

      Inter : Statement_Range_T;
      -- The intersection of the two ranges.

      Shared_Source : Boolean;
      -- Whether the Left source, currently inspected, is also
      -- present in Right.

      R : Source_Ordinal_T;
      -- The source ordinal in Right for the source shared
      -- with Left.

      Interval : Source_Interval_T;
      -- The intersection of the source-line intervals from Left
      -- and Right, for the shared source file.

      Empty : Boolean;
      -- Whether the intersection Interval is empty.

      Autograph : constant String := "Output.""&""(Statement_Range_T)";

   begin

      Add_Surrounding_Lines (Rext);

      -- Source-line intervals:

      if Left.Sources = 0 then
         -- Left has no source-lines, use Right.

         Inter.Sources := Rext.Sources;
         Inter.Source  := Rext.Source;

      elsif Rext.Sources = 0 then
         -- Right has no source-lines, use Left.

         Inter.Sources := Left.Sources;
         Inter.Source  := Left.Source;

      else
         -- Both Left and Right contain source-line intervals.
         -- Intersect their source-line ranges for each source file.
         -- Keep only those source files where the intersection is
         -- not empty.

         Inter.Sources := 0;

         for L in Left.Source'First .. Left.Sources loop

            -- Inspect Left.Source(S).
            -- See if this source is shared with Right:

            Shared_Source := False;

            R := Rext.Source'First;

            Finding_Source: loop

               Shared_Source := Same_Source (Rext.Source(R), Left.Source(L));

               exit Finding_Source when
                  Shared_Source or R = Rext.Sources;

               R := R + 1;

            end loop Finding_Source;

            if Shared_Source then

               Intersect (
                  Left  => Left.Source(L),
                  Right => Rext.Source(R),
                  Inter => Interval,
                  Empty => Empty);

               if not Empty then
                  -- Include the intersection interval in the
                  -- resulting statement range:

                  Inter.Sources := Inter.Sources + 1;
                  Inter.Source(Inter.Sources) := Interval;

               end if;

            end if;

         end loop;

      end if;

      -- Code address interval:

      Intersect (
         Left  => Left.Code,
         Right => Rext.Code,
         Inter => Inter.Code,
         Empty => Empty);
      --
      -- If the intersection was empty, the Intersect operation
      -- has already emitted a Fault message.

      if Empty then

         Inter.Code := No_Code_Interval;

      end if;

      -- Symbol table:

      Inter.Symbol_Table :=
         Same_Symbols (
            Left.Symbol_Table,
            Rext.Symbol_Table,
            Autograph);

      -- And that is all:

      return Inter;

   end "&";


   procedure Find_Line_Before (
      Address : in     Processor.Code_Address_T;
      Within  : in     Symbols.Symbol_Table_T;
      Found   :    out Boolean;
      Line    :    out Statement_Locus_T)
   --
   -- Finds the source-line locus immediately before the given address,
   -- if there is one.
   --
   is

      Befores : constant Symbols.Connection_Set_T :=
         Symbols.Line_Before (Address, Within);
      -- The line before this locus, if any.

   begin

      Found := Befores'Length > 0;

      if Found then

         Line := Symbols.Show.Statement (
            Line   => Befores(Befores'First),
            Source => Within);

      else

         Line := No_Statement;

      end if;

   end Find_Line_Before;


   procedure Find_Line_After (
      Address : in     Processor.Code_Address_T;
      Within  : in     Symbols.Symbol_Table_T;
      Found   :    out Boolean;
      Line    :    out Statement_Locus_T)
   --
   -- Finds the source-line locus immediately after the given address,
   -- if there is one.
   --
   is

      Afters : constant Symbols.Connection_Set_T :=
         Symbols.Line_After (Address, Within);
      -- The line after this locus, if any.

   begin

      Found := Afters'Length > 0;

      if Found then

         Line := Symbols.Show.Statement (
            Line   => Afters(Afters'First),
            Source => Within);

      else

         Line := No_Statement;

      end if;

   end Find_Line_After;


   procedure Add_Surrounding_Lines (To : in out Statement_Range_T)
   is

      Found_Before, Found_After : Boolean;
      -- Whether we found a source-line connection before/after
      -- the given range.

      Line_Before, Line_After : Statement_Locus_T;
      -- The locus of the source-line before/after the given range.

   begin

      if  Opt.Show_Surrounding_Lines
      and To.Sources = 0
      and To.Code.First.Known
      and To.Code.Last.Known
      and To.Symbol_Table /= Symbols.No_Symbol_Table
      then
         -- We should and can find the surrounding source lines.

         Find_Line_Before (
            Address => To.Code.First.Address,
            Within  => To.Symbol_Table,
            Found   => Found_Before,
            Line    => Line_Before);

         if Found_Before then
            -- We are satisfied with this line, because it is likely
            -- to be the source of the code in the given range.

            To.Sources := 1;

            To.Source(First_Source) := (
               File  => Line_Before.Source_File,
               First => Line_Before.Line,
               Last  => (Known => False));

         else
            -- No line found before the given locus.
            -- Try to find a line after it:

            Find_Line_After (
               Address => To.Code.Last.Address,
               Within  => To.Symbol_Table,
               Found   => Found_After,
               Line    => Line_After);

            if Found_After then
               -- We found a source-line after, but not before the
               -- given locus. This is a little unexpected, but we
               -- grin and bear it.

               To.Sources := 1;

               To.Source(First_Source) := (
                  File  => Line_After.Source_File,
                  First => (Known => False),
                  Last  => Line_After.Line);

            end if;

         end if;

      end if;

   end Add_Surrounding_Lines;


   function Enclosed (
      Prefix : String;
      Data   : String;
      Suffix : String)
   return String
   --
   -- The Data between the given Prefix and Suffix, or null if
   -- the Data is null.
   --
   is
   begin
      if Data'Length = 0 then
         return "";
      else
         return Prefix & Data & Suffix;
      end if;
   end Enclosed;


   function Image (Item : Possible_Line_Number_T)
   return String
   --
   -- Line-number or nothing.
   --
   is
   begin
      if Item.Known then
         return Image (Integer (Item.Number));
      else
         return "";
      end if;
   end Image;


   function Image (Item : Possible_Code_Address_T)
   return String
   --
   -- Code-address or nothing.
   --
   is
   begin
      if Item.Known then
         return Processor.Image (Item.Address);
      else
         return "";
      end if;
   end Image;


   function Image (
      Item    : Statement_Locus_T;
      Address : Boolean := False)
   return String
   is

      Line : constant String := Image (Item.Line);

   begin

      if Address
      or Opt.Show_Code_Addresses
      or Line'Length = 0
      then

         return Line
              & Enclosed (
                   Prefix => "[",
                   Data   => Image (Item.Code),
                   Suffix => "]");

      else

         return Line;

      end if;

   end Image;


   function Rainge (First, Last : String)
   return String
   --
   -- A range from First to Last.
   -- If First = Last, only one is shown (not a range).
   -- However, if First and Last are both null, returns null.
   --
   is
   begin
      if First'Length = 0 and Last'Length = 0 then
         return "";
      elsif First = Last then
         return First;
      else
         return First & '-' & Last;
      end if;
   end Rainge;


   function Image (
      Item    : Source_Interval_T;
      Code    : Code_Interval_T;
      Address : Boolean := False)
   return String
   --
   -- Image of the source-line interval.
   -- Code addresses are optionally included.
   --
   is

      Lines : constant String :=
         Rainge (
            First => Image (Item.First),
            Last  => Image (Item.Last ));

   begin

      if Address
      or Opt.Show_Code_Addresses
      or Lines'Length = 0
      then

         return
              Lines
            & Enclosed (
                 Prefix => "[",
                 Data   =>
                    Rainge (
                       First => Image (Code.First),
                       Last  => Image (Code.Last )),
                 Suffix => "]");

      else

         return Lines;

     end if;

   end Image;


   function Image (
      Item    : Statement_Range_T;
      Source  : Source_Ordinal_T := First_Source;
      Address : Boolean := False)
   return String
   is
   begin

      return Image (Item.Source(Source), Item.Code, Address);

   end Image;


   function Code_Image (Item : Statement_Range_T)
   return String
   is
   begin

      return
         Enclosed (
            Prefix => "[",
            Data   =>
               Rainge (
                  First => Image (Item.Code.First),
                  Last  => Image (Item.Code.Last )),
            Suffix => "]");

   end Code_Image;


   --
   ---  Locus construction and combination
   --


   function Begins_With (Prefix, Whole : String) return Boolean
   --
   -- Whether the Whole string begins with the Prefix.
   --
   is
   begin

      return Whole'Length >= Prefix'Length
      and then Whole(Whole'First .. Whole'First + Prefix'Length - 1) = Prefix;

   end Begins_With;


   function Subpath (Part, Whole : String) return Boolean
   --
   -- Whether Part is a sub-call-path of Whole, delimited on the left
   -- by a Call_Mark (if Part is not a prefix of Whole) and on the
   -- right by a Call_Locus_Mark (if Part is not a suffix of Whole).
   --
   is
      use Ada.Strings.Fixed;

      First : Positive := Whole'First;
      -- The next index in Whole where an instance of Part may lie.

      Rest : Integer;
      -- The length of Whole(First .. Whole'Last).

      After : Positive;
      -- The index in Whole after a possible instance of Part.

      Mark : Natural;
      -- The index in Whole of a Call_Mark.

   begin

      loop
         -- There is no instance of Part in Whole(Whole'First .. First - 1),
         -- and this prefix of Whole is empty, or is the Whole, or ends
         -- with a Call_Mark. So Whole(First..) is the next possible
         -- place for an instance of Part.

         Rest := Whole'Last - First + 1;

         exit when Rest < Part'Length;
         -- If the rest of the Whole is shorter than the Part,
         -- it cannot contain an instance of Part.

         After := First + Part'Length;

         if Whole(First .. After - 1) = Part
         and then (Rest = Part'Length
         or else   Begins_With (Call_Locus_Mark, Whole(After .. Whole'Last)))
         then
            -- Here is a Part within the whole, preceded by nothing
            -- or by a Call_Mark, and followed by nothing or by
            -- a Call_Locus_Mark. Yes!

            return True;

         end if;

         -- Find the next possible starting location: after
         -- the next Call_Mark, if any.

         Mark := Index (
            Source  => Whole(First .. Whole'Last),
            Pattern => Call_Mark);

         exit when Mark = 0;
         -- If there are no more Call_Marks in the Whole, there
         -- can be no valid instances of Part.

         First := Mark + Call_Mark'Length;

      end loop;

      -- The loop found no valid instance of Part.

      return False;

   end Subpath;


   function Locus (
      Program_File : String := "";
      Source_File  : String := "";
      Call_Path    : String := "";
      Statements   : Statement_Range_T := No_Statements)
   return Locus_T
   is

      Result : Locus_T;
      -- You guessed it.

   begin

      -- Use the given attributes as such:

      Result := Locus_T'(
         Program_File => To_Item (Program_File),
         Source_File  => Symbols.To_Source_File_Name (Source_File),
         Call_Path    => To_Item (Call_Path),
         Statements   => Statements);

      -- Use source-file from statements, unless given directly:

      if Result.Source_File = Symbols.Null_Name then

         for S in First_Source .. Statements.Sources loop

            if Statements.Source(S).File /= Symbols.Null_Name then

               Result.Source_File := Statements.Source(S).File;

               exit;

            end if;

         end loop;

      end if;

      -- And that is it:

      return Result;

   end Locus;


   type Order_T is (Less, Equal, Greater, Unordered);
   --
   -- Ordering relationship between two Possible_Line_Number_T
   -- or two Possible_Code_Address_T.


   function Order (Left, Right : Possible_Line_Number_T)
   return Order_T
   --
   -- Compare the two possible source lines numbers for ordering.
   --
   is
   begin

      if not (Left.Known and Right.Known) then

         return Unordered;

      elsif Left.Number < Right.Number then

         return Less;

      elsif Left.Number > Right.Number then

         return Greater;

      else

         return Equal;

      end if;

   end Order;


   function Order (Left, Right : Possible_Code_Address_T)
   return Order_T
   --
   -- Compare the two possible code addresses for ordering.
   --
   is
      use type Processor.Code_Address_T;
   begin

      if not (Left.Known and Right.Known) then

         return Unordered;

      elsif Left.Address < Right.Address then

         return Less;

      elsif Right.Address < Left.Address then

         return Greater;

      else

         return Equal;

      end if;

   end Order;


   function "<=" (Part, Whole : Source_Interval_T) return Boolean
   --
   -- Whether Part is a subinterval of Whole.
   --
   is

      First : constant Order_T := Order (Part.First, Whole.First);
      Last  : constant Order_T := Order (Part.Last , Whole.Last );
      -- Ordering of First and Last ends.

   begin

      return  (First = Equal or First = Greater)
          and (Last  = Equal or Last  = Less   );

   end "<=";


   function "<=" (Part, Whole : Code_Interval_T) return Boolean
   --
   -- Whether Part is a subinterval of Whole.
   --
   is

      First : constant Order_T := Order (Part.First, Whole.First);
      Last  : constant Order_T := Order (Part.Last , Whole.Last );
      -- Ordering of First and Last ends.

   begin

      return  (First = Equal or First = Greater)
          and (Last  = Equal or Last  = Less   );

   end "<=";


   function "<=" (Part, Whole : Source_Interval_List_T) return Boolean
   --
   -- Whether Part is a subrange of Whole in the sense that every
   -- source-line interval in Part is a subinterval of an interval
   -- in Whole from the same source.
   --
   is

      Subrange : Boolean := True;
      -- Whether Part is a subrange of Whole.
      -- So far we have seen no evidence to the contrary.

      P : Source_Ordinal_T;
      -- The ordinal of an source-file in Part.

      Shared_Source : Boolean;
      -- Whether the current Part interval shares the source-file
      -- with a Whole interval.

      W : Source_Ordinal_T;
      -- The ordinal of the Whole source for the shared source-file.

   begin

      P := Part'First;

      Scanning_Part: loop

         W := Whole'First;

         Finding_Whole : loop

            Shared_Source := Same_Source (Part(P), Whole(W));

            exit Finding_Whole when
               Shared_Source or W = Whole'Last;

            W := W + 1;

         end loop Finding_Whole;

         Subrange :=
            Shared_Source
            and then Part(P) <= Whole(W);

         exit Scanning_Part when
            (not Subrange) or P = Part'Last;

         P := P + 1;

      end loop Scanning_Part;

      return Subrange;

   end "<=";


   function Sources (Item : Locus_T)
   return Source_Interval_List_T
   --
   -- The source intervals listed in the given locus.
   --
   is
   begin

      return Item.Statements.Source(1 .. Item.Statements.Sources);

   end Sources;


   function "&" (Left, Right : Locus_T)
   return Locus_T
   is
      use String_Pool;

      Rext : Locus_T := Right;
      -- The Right part, perhaps extende with surrounding lines.

      Combo : Locus_T := Left;
      -- The combination to be.

      Autograph : constant String := "Output.""&""(Locus_T)";

   begin

      -- Perhaps extend Right with surrounding lines:

      Add_Surrounding_Lines (Rext);

      -- Program File:

      if Rext.Program_File /= Null_Item then

         Combo.Program_File := Rext.Program_File;

      end if;

      -- Source File:

      if Rext.Source_File /= Symbols.Null_Name then

         Combo.Source_File := Rext.Source_File;

      end if;

      -- Call Path:

      if Combo.Call_Path = Null_Item then

         Combo.Call_Path := Rext.Call_Path;

      elsif Rext.Call_Path /= Null_Item then
         -- Neither Left nor Right call-path is null.

         if not Subpath (
               Part  => String_Pool.To_String (Rext.Call_Path),
               Whole => String_Pool.To_String (Left.Call_Path))
         then
            -- Right call-path is not a substring of Left.
            -- Thus, Right is either more precise (a superstring
            -- of Left), or not comparable and overriding.

            Combo.Call_Path := Rext.Call_Path;

         end if;

      end if;

      -- Source lines:

      if Combo.Statements.Sources = 0 then
         -- Left does not specify statements, use Right.

         Combo.Statements := Rext.Statements;

      elsif Rext.Statements.Sources /= 0 then
         -- Both Left and Rext specify some statements.

         if not (Sources (Left) <= Sources (Rext)) then
            -- The Left source-line set is not a subset of the Rext
            -- source-line set, so Rext is presumably more precise.

            Combo.Statements.Sources := Rext.Statements.Sources;
            Combo.Statements.Source  := Rext.Statements.Source;

         end if;

      end if;

      -- Code address:

      if not (Left.Statements.Code <= Rext.Statements.Code) then
         -- The Left code-address interval is not a subset of the
         -- Right code-address interval, so Right is presumably
         -- more precise.

         Combo.Statements.Code := Rext.Statements.Code;

      end if;

      -- Symbol table:

      Combo.Statements.Symbol_Table :=
         Same_Symbols (
            Left.Statements.Symbol_Table,
            Rext.Statements.Symbol_Table,
            Autograph);

      -- And that is all:

      return Combo;

   end "&";


   procedure Add_Surrounding_Lines (To : in out Locus_T)
   is
   begin

      if  Opt.Show_Surrounding_Lines
      and To.Statements.Sources = 0
      then
         -- We should and perhaps can find surrounding lines.

         Add_Surrounding_Lines (To => To.Statements);

         -- Perhaps we now know the source-code file:

         if  To.Source_File = Symbols.Null_Name
         and To.Statements.Sources > 0
         then

            To.Source_File := To.Statements.Source(1).File;

         end if;

      end if;

   end Add_Surrounding_Lines;


   --
   ---  Default Locus Nest
   --


   type Marked_Locus_T is record
      Mark       : Nest_Mark_T;
      Locus      : Locus_T;
      Surrounded : Boolean;
   end record;
   --
   -- A default locus in the nest.
   --
   -- Mark
   --    The nest mark that identifies this level and locus.
   -- Locus
   --    The locus at this level.
   -- Surrounded
   --    Whether the Locus has been improved by
   --    Add_Surrounding_Lines.


   Default_Locus : array (Nest_Depth_T range 1 .. Nest_Depth_T'Last)
      of Marked_Locus_T;
   --
   -- The stack of nested default loci is Default_Locus(1 .. Nest_Depth).


   Nest_Depth : Nest_Depth_T := 0;
   --
   -- The current depth (number) of nested default loci.


   Last_Mark_Seq : Natural := 0;
   --
   -- The sequence number used for the last nested default locus.


   Cached_Locus_Valid : Boolean := False;
   --
   -- Whether the current default locus has been computed by
   -- combining all nested default loci, since the last Nest or
   -- Unnest operation.


   Cached_Current_Locus : Locus_T;
   --
   -- The current locus computed by combining all the nested
   -- default loci. Valid only if Cached_Locus_Valid.


   function Image (Item : Nest_Mark_T) return String
   --
   -- A readable presentation of the mark.
   --
   is
   begin
      return "(Depth ="
           & Nest_Depth_T'Image (Item.Depth)
           & ", Seq ="
           & Natural'Image (Item.Seq)
           & ')';
   end Image;


   procedure Show_Nest (
      File   : in Ada.Text_IO.File_Type;
      Indent : in Ada.Text_IO.Positive_Count)
   --
   -- Displays the default locus nest on the given File, with
   -- the given Indentation.
   -- Intended for diagnostics.
   --
   is
   begin

      for N in Default_Locus'First .. Nest_Depth loop

         Set_Col (File, Indent);

         Put (File,
              '[' & Image (Natural (N)) & "]: "
            & Image (Default_Locus(N).Locus));

         if Default_Locus(N).Surrounded then

            Put_Line (File, ", surrounded");

         else

            Put_Line (File, ", not surrounded");

         end if;

      end loop;

   end Show_Nest;


   function Nest (Locus : in Locus_T) return Nest_Mark_T
   is

      Mark : Nest_Mark_T;
      -- To be returned.

   begin

      if Opt.Trace_Locus_Nesting then

         Put_Line (Standard_Output,
            "Nest (prev depth" & Nest_Depth_T'Image (Nest_Depth) & ')');

         Show (Locus, Standard_Output);

      end if;

      if Nest_Depth = Nest_Depth_T'Last then

         Error (
            Locus => Locus,
            Text  =>
               "Default output locus too deeply nested.");

         return No_Mark;

      else

         Nest_Depth := Nest_Depth + 1;

         Last_Mark_Seq := Last_Mark_Seq + 1;

         Mark := (
            Defined => True,
            Depth   => Nest_Depth,
            Seq     => Last_Mark_Seq);

         Default_Locus(Nest_Depth) := (
            Mark       => Mark,
            Locus      => Locus,
            Surrounded => False);

         if Opt.Trace_Locus_Nesting then

           Show_Nest (Standard_Output, 3);

         end if;

         Cached_Locus_Valid := False;

         return Mark;

      end if;

   end Nest;


   procedure Unnest (Mark : in Nest_Mark_T)
   is
   begin

      if not Mark.Defined then
         -- The mark was not "nested" or the nest was full, nothing
         -- to unnest.

         null;

      elsif Default_Locus(Mark.Depth).Mark /= Mark then
         -- Mess up.

         Fault (
            Location => "Output.Unnest",
            Text =>
                 "Nested mark = "
               & Image (Default_Locus(Mark.Depth).Mark)
               & " /= given mark = "
               & Image (Mark));

      else

         Nest_Depth := Mark.Depth - 1;

         Cached_Locus_Valid := False;

      end if;

      if Opt.Trace_Locus_Nesting then

         Put_Line (Standard_Output,
            "Output.Unnest to depth" & Nest_Depth_T'Image (Nest_Depth));

         Show_Nest (Standard_Output, 3);

      end if;

   end Unnest;


   function Current_Locus return Locus_T
   is
   begin

      if Opt.Trace_Current_Locus then

         Put_Line (Standard_Output, "Current locus nest:");

         Show_Nest (Standard_Output, 3);

      end if;

      if not Cached_Locus_Valid then
         -- We must compute a new current locus, because the
         -- nest of default loci has changed since the last
         -- computation.

         Cached_Current_Locus := No_Locus;

         for D in 1 .. Nest_Depth loop

            if not Default_Locus(D).Surrounded then
               -- We have not done this yet:

               Add_Surrounding_Lines (To => Default_Locus(D).Locus);

               Default_Locus(D).Surrounded := True;

            end if;

            Cached_Current_Locus :=
                 Cached_Current_Locus
               & Default_Locus(D).Locus;

         end loop;

      end if;

      return Cached_Current_Locus;

   end Current_Locus;



   --
   ---  Locus queries
   --


   function Program_File (
      Item : Locus_T;
      Form : Source_File_Form_T := Default)
   return String
   is
   begin
      return Formed_Source (To_String (Item.Program_File), Form);
   end Program_File;


   function Source_File (
      Item : Locus_T;
      Form : Source_File_Form_T := Default)
   return String
   is
   begin
      return Formed_Source (Symbols.Image (Item.Source_File), Form);
   end Source_File;


   function Call_Path (Item : Locus_T) return String
   is
   begin
      return To_String (Item.Call_Path);
   end Call_Path;


   function Statements (Item : Locus_T) return Statement_Range_T
   is
   begin
      return Item.Statements;
   end Statements;


   function Image (Item : Locus_T) return String
   is
   begin
      return
           Program_File (Item) & Opt.Field_Separator
         & Source_File  (Item) & Opt.Field_Separator
         & Call_Path    (Item) & Opt.Field_Separator
         & Image (Statements (Item));
   end Image;


   --
   ---  Basic output operations
   --


   procedure Line (
      Channel : in Ada.Text_IO.File_Type;
      Key     : in String;
      Locus   : in Locus_T;
      Data    : in String)
   --
   -- Emit a line in the basic format, with the location fields
   -- given by a Locus. The default loci are _not_ applied.
   --
   is
   begin

      if Locus.Statements.Sources <= 1 then
         -- Traditional output form for one or no source file.

         Line (
            Channel      => Channel,
            Key          => Key,
            Program_File => Program_File (Locus),
            Source_File  => Source_File  (Locus),
            Call_Path    => Call_Path    (Locus),
            Statements   => Image (Locus.Statements),
            Data         => Data);

      else
         -- "See also" for the second and further source files.

         Line (
            Channel      => Channel,
            Key          => Key,
            Program_File => Program_File (Locus),
            Source_File  => Source_File  (Locus.Statements, Source => 1),
            Call_Path    => Call_Path    (Locus),
            Statements   => Image (Locus.Statements, Source => 1),
            Data         => Data);

         for S in 2 .. Locus.Statements.Sources loop

            Line (
               Channel      => Channel,
               Key          => Basic_Output.See_Also_Key,
               Program_File => Program_File (Locus),
               Source_File  => Source_File  (Locus.Statements, Source => S),
               Call_Path    => Call_Path    (Locus),
               Statements   => Image (Locus.Statements, Source => S),
               Data         => "");

         end loop;

      end if;

   end Line;


   -- Limits on number of problem messages


   generic
      Kind  : in     String;
      Count : in out Natural;
      Max   : in out Natural;
   procedure One_More_Problem;
   --
   -- Increments the Count of problem messages of this Kind.
   -- Propagates Too_Many_Problems if the tally then exceeds Max.


   procedure One_More_Problem
   is
   begin

      Count := Count + 1;

      if Count > Max then

         Line (
            Channel => Standard_Error,
            Key     => Basic_Output.Error_Key,
            Locus   => Current_Locus,
            Data    => "Over"
               & Natural'Image (Max)
               & ' '
               & Kind
               & "s. Analysis aborted.");

         raise Too_Many_Problems;

      end if;

   end One_More_Problem;


   -- Notes


   procedure Note (
      Text  : in String)
   is
   begin
      if Opt.Show_Notes then
         Line (
            Channel => Standard_Output,
            Key     => Basic_Output.Note_Key,
            Locus   => Current_Locus,
            Data    => Text);
      end if;
   end Note;


   procedure Note (
      Locus : in Locus_T;
      Text  : in String)
   is
   begin
      if Opt.Show_Notes then
         Line (
            Channel => Standard_Output,
            Key     => Basic_Output.Note_Key,
            Locus   => Current_Locus & Locus,
            Data    => Text);
      end if;
   end Note;


   -- Warnings


   Number_Warnings : Natural := 0;
   --
   -- The number of Warnings emitted so far.


   procedure One_More_Warning is new One_More_Problem (
      Kind  => Basic_Output.Warning_Key,
      Count => Number_Warnings,
      Max   => Opt.Max_Warnings);


   procedure Warning (
      Text  : in String)
   is
   begin

      Flush_Standard_Output;

      Line (
         Channel => Standard_Output,
         Key     => Basic_Output.Warning_Key,
         Locus   => Current_Locus,
         Data    => Text);

      One_More_Warning;

   end Warning;


   procedure Warning (
      Locus : in Locus_T;
      Text  : in String)
   is
   begin

      Flush_Standard_Output;

      Line (
         Channel => Standard_Output,
         Key     => Basic_Output.Warning_Key,
         Locus   => Current_Locus & Locus,
         Data    => Text);

      One_More_Warning;

   end Warning;


   -- Errors


   Number_Errors : Natural := 0;
   --
   -- The number of Errors emitted so far.


   procedure One_More_Error is new One_More_Problem (
      Kind  => Basic_Output.Error_Key,
      Count => Number_Errors,
      Max   => Opt.Max_Errors);


   procedure Error (
      Text  : in String)
   is
   begin

      Flush_Standard_Output;

      Line (
         Channel => Standard_Error,
         Key     => Basic_Output.Error_Key,
         Locus   => Current_Locus,
         Data    => Text);

      One_More_Error;

   end Error;


   procedure Error (
      Locus : in Locus_T;
      Text  : in String)
   is
   begin

      Flush_Standard_Output;

      Line (
         Channel => Standard_Error,
         Key     => Basic_Output.Error_Key,
         Locus   => Current_Locus & Locus,
         Data    => Text);

      One_More_Error;

   end Error;


   -- Faults


   Number_Faults : Natural := 0;
   --
   -- The number of Faults emitted so far.


   procedure One_More_Fault is new One_More_Problem (
      Kind  => Basic_Output.Fault_Key,
      Count => Number_Faults,
      Max   => Opt.Max_Faults);


   procedure Fault (
      Location : in String;
      Text     : in String)
   is
   begin

      Flush_Standard_Output;

      Line (
         Channel => Standard_Error,
         Key     => Basic_Output.Fault_Key,
         Locus   => Current_Locus,
         Data    => Location
                  & Opt.Field_Separator
                  & Text);

      One_More_Fault;

   end Fault;


   procedure Fault (
      Location : in String;
      Locus    : in Locus_T;
      Text     : in String)
   is
   begin

      Flush_Standard_Output;

      Line (
         Channel => Standard_Error,
         Key     => Basic_Output.Fault_Key,
         Locus   => Current_Locus & Locus,
         Data    => Location
                  & Opt.Field_Separator
                  & Text);

      One_More_Fault;

   end Fault;


   procedure Trace (
      Text  : in String)
   is
   begin
      Flush_Standard_Output;
      Line (
         Channel => Standard_Output,
         Key     => Basic_Output.Trace_Key,
         Locus   => Current_Locus,
         Data    => Text);
   end Trace;


   procedure Trace (
      Locus : in Locus_T;
      Text  : in String)
   is
   begin
      Flush_Standard_Output;
      Line (
         Channel => Standard_Output,
         Key     => Basic_Output.Trace_Key,
         Locus   => Current_Locus & Locus,
         Data    => Text);
   end Trace;


   -- Exception info


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
         Locus   => Current_Locus,
         Data    => Text
                  & Opt.Field_Separator
                  & Exception_Information (Occurrence));
   end Exception_Info;


   procedure Exception_Info (
      Locus      : in Locus_T;
      Text       : in String;
      Occurrence : in Ada.Exceptions.Exception_Occurrence)
   is
      use Ada.Exceptions;
   begin
      Flush_Standard_Output;
      Line (
         Channel => Standard_Error,
         Key     => Basic_Output.Exception_Key,
         Locus   => Current_Locus & Locus,
         Data    => Text
                  & Opt.Field_Separator
                  & Exception_Information (Occurrence));
   end Exception_Info;


   -- Results and Unknowns


   procedure Result (
      Key   : in String;
      Text  : in String)
   is
   begin
      Flush_Standard_Output;
      Line (
         Channel => Standard_Output,
         Key     => Key,
         Locus   => Current_Locus,
         Data    => Text);
   end Result;


   procedure Result (
      Key   : in String;
      Locus : in Locus_T;
      Text  : in String)
   is
   begin
      Flush_Standard_Output;
      Line (
         Channel => Standard_Output,
         Key     => Key,
         Locus   => Current_Locus & Locus,
         Data    => Text);
   end Result;


   procedure Unknown (
      Key   : in String;
      Text  : in String := "")
   is
   begin

      Flush_Standard_Output;

      if Text'Length = 0 then

         Line (
            Channel => Standard_Output,
            Key     => Basic_Output.Unknown_Key,
            Locus   => Current_Locus,
            Data    => Key);

      else

         Line (
            Channel => Standard_Output,
            Key     => Basic_Output.Unknown_Key,
            Locus   => Current_Locus,
            Data    => Key
                     & Opt.Field_Separator
                     & Text);

      end if;

   end Unknown;


   procedure Unknown (
      Key   : in String;
      Locus : in Locus_T;
      Text  : in String := "")
   is
   begin

      Flush_Standard_Output;

      if Text'Length = 0 then

         Line (
            Channel => Standard_Output,
            Key     => Basic_Output.Unknown_Key,
            Locus   => Current_Locus & Locus,
            Data    => Key);

      else

         Line (
            Channel => Standard_Output,
            Key     => Basic_Output.Unknown_Key,
            Locus   => Current_Locus & Locus,
            Data    => Key
                     & Opt.Field_Separator
                     & Text);

      end if;

   end Unknown;


   --
   ---  Supporting utility operations
   --


   function Image (Item : Processor.Time_T) return String
   is

      Img : constant String := Processor.Time_T'Image (Item);

   begin

      if Img (Img'First) /= ' ' then

         return Img;

      else

         return Img (Img'First + 1 .. Img'Last);

      end if;

   end Image;


   function Image (Item : Line_Number_T) return String
   is

      Img : constant String := Line_Number_T'Image (Item);

   begin

      if Img (Img'First) /= ' ' then

         return Img;

      else

         return Img (Img'First + 1 .. Img'Last);

      end if;

   end Image;


   function Either (
      Cond : Boolean;
      Yes  : String;
      No   : String)
   return String
   is
   begin

      if Cond then return Yes; else return No; end if;

   end Either;


   procedure Flush
   is
   begin
      Ada.Text_IO.Flush (Standard_Output);
      Ada.Text_IO.Flush (Standard_Error);
   end Flush;


   procedure Heading (
      Text   : in String;
      Margin : in Ada.Text_IO.Positive_Count := 4)
   is
   begin

      Set_Col (1);

      Flush;

      Put_Line (Text);

      Set_Col (Margin);

   end Heading;


end Output;
