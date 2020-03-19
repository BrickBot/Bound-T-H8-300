-- Symbols.Text (body)
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
-- $Revision: 1.3 $
-- $Date: 2015/10/24 20:05:52 $
--
-- $Log: symbols-text.adb,v $
-- Revision 1.3  2015/10/24 20:05:52  niklas
-- Moved to free licence.
--
-- Revision 1.2  2011-08-31 04:23:34  niklas
-- BT-CH-0222: Option registry. Option -dump. External help files.
--
-- Revision 1.1  2008/10/11 08:16:14  niklas
-- BT-CH-0148: Symbols from text files and the -symbols option.
--


with Ada.Characters.Latin_1;
with Ada.Strings;
with Ada.Strings.Fixed;
with Ada.Strings.Maps;
with Ada.Text_IO;

with Output;
with Processor.Properties;
with Options.String_Sets;
with Symbols.Opt;


package body Symbols.Text is


   -- File syntax:
   --
   -- The syntax of the symbols file is the following:
   --
   -- > Any line beginning with "--" is a comment and is ignored.
   --   Blanks may precede the "--".
   --
   -- > Any blank or empty line is ignored.
   --
   -- > Any other line should contain a symbol definition.
   --
   -- A symbol definition contains three strings, without embedded
   -- blanks and separated by one or more blanks, as follows:
   --
   -- > The first string is either they keyword "subprogram"
   --   or the keyword "variable".
   --
   -- > The second string is the identifier, possibly qualified
   --   by a scope, using the default scope delimiter.
   --
   -- > The third string is the numerical or mnemonic faddress of the symbol.
   --   For a subprogram symbol, the form is defined by the function
   --   Processor.Properties.Subprogram_Address.
   --   For a variable (cell) symbol, the form is defined by the
   --   function Processsor.Properties.Variable_Cell.
   --
   -- The strings should *not* be quoted.
   --
   -- Leading and trailing blanks are ignored.
   -- Tab characters are equivalent to blanks.
   --
   -- Example:
   --    -- This is a comment.
   --    subprogram rom|init_mem 16#1234#
   --    subprogram rom|boot|startup 0
   --    variable count mw032a


   procedure Define (
      Kind       : in     String;
      Identifier : in     String;
      Address    : in     String;
      Table      : in     Symbol_Table_T;
      Valid      : in out Boolean)
   --
   -- Defines the Identifier at Address in the Table as a subprogram
   -- or a variable, depending on the Kind.
   -- Sets Valid to False if some error is detected.
   --
   is

      Cell : Storage.Cell_T;
      -- The cell defined by this Address, when Kind = "variable".

   begin

      if Kind = "subprogram" then

         Connect_Subprogram (
            Scope   => Scope_Of (Identifier),
            Name    => Name_Of  (Identifier),
            Address => Processor.Properties.Subprogram_Address (Address),
            Within  => Table);

      elsif Kind = "variable" then

         Cell := Processor.Properties.Variable_Cell (Address);

         Connect_Variable (
            Scope    => Scope_Of (Identifier),
            Name     => Name_Of  (Identifier),
            Location => Storage.Spec_Of (Cell),
            Within   => Table);

      else

         Output.Error (
              "Unknown symbol kind """
            & Kind
            & """ for the symbol """
            & Identifier
            & """ at """
            & Address
            & """.");

         Valid := False;

      end if;

   exception

   when Constraint_Error
      | Processor.Properties.Address_Error =>
      -- Probably the Address is wrong in some way.

      Output.Error (
           "Could not define the "
         & Kind
         & " """
         & Identifier
         & """ at """
         & Address
         & """.");

      Valid := False;

   end Define;


   Tab_To_Blank : constant Ada.Strings.Maps.Character_Mapping :=
      Ada.Strings.Maps.To_Mapping (
         From => (1 => Ada.Characters.Latin_1.HT),
         To   => (1 => ' '));
   --
   -- Translates tabs (HT) to blanks.


   White_Space : constant Ada.Strings.Maps.Character_Set :=
      Ada.Strings.Maps.To_Set (
         String'((' ', Ada.Characters.Latin_1.HT)));
   --
   -- The white-space characters that separate tokens on the
   -- input lines. (It is not necessary to include HT, if the
   -- Tab_To_Blank translation is applied first.)


   type Slice_T is record
      First : Positive;
      Last  : Natural;
   end record;
   --
   -- The first and last indices of a slice of a string.
   -- The slice is "null" if Last < First.


   function Is_Null (Item : Slice_T) return Boolean
   is
   begin

      return Item.Last < Item.First;

   end Is_Null;


   function Length (Item : Slice_T) return Integer
   is
   begin

      return Item.Last - Item.First + 1;

   end Length;


   function Next_Token (Within : String; From : Positive)
   return Slice_T
   --
   -- The slice of Within(From..) that contains the first
   -- blank-separated blank-free substring. However, if From is
   -- not in Within'Range, a null slice is returned.
   --
   is
      use Ada.Strings;
      use Ada.Strings.Fixed;

      Slice : Slice_T;
      -- The result.

   begin

      if From in Within'Range then

         Find_Token (
            Source => Within(From .. Within'Last),
            Set    => White_Space,
            Test   => Outside,
            First  => Slice.First,
            Last   => Slice.Last);

      else

         Slice := (First => Within'First, Last => 0);

      end if;

      return Slice;

   end Next_Token;


   procedure Parse_And_Define (
      Line  : in     String;
      Table : in     Symbol_Table_T;
      Valid : in out Boolean)
   --
   -- Parses the input Line (which has been stripped of leading
   -- and trailing blanks) and defines the symbol -- if any -- in
   -- the Table. Sets Valid to False if any error is detected.
   --
   is
      use Ada.Strings;
      use Ada.Strings.Fixed;

      Keyword, Identifier, Address : Slice_T;
      -- The parts of the line that contain the keyword, the
      -- identifier, and the address, respectively.

   begin

      Keyword := Next_Token (Within => Line, From => Line'First);

      if Is_Null (Keyword)
      or else (
         Length (Keyword) >= 2
         and then Line(Keyword.First .. Keyword.First + 1) = "--")
      then
         -- A blank or empty line or a comment line.

         null;

      else
         -- The Line consists of a keyword, an identifier, and the
         -- address value, separated by blanks.

         Identifier := Next_Token (Within => Line, From => Keyword.Last + 1);

         Address := Next_Token (Within => Line, From => Identifier.Last + 1);

         if Is_Null (Identifier)
         or Is_Null (Address)
         or Address.Last /= Line'Last
         then

            Output.Error (
                 "Syntax error in symbol-file line"
               & Output.Field_Separator
               & Line);

            Valid := False;

         else

            Define (
               Kind       => Line(Keyword.First    .. Keyword.Last),
               Identifier => Line(Identifier.First .. Identifier.Last),
               Address    => Line(Address.First    .. Address.Last),
               Table      => Table,
               Valid      => Valid);

         end if;

      end if;

   end Parse_And_Define;


   procedure Read (
      File_Name    : in     String;
      Symbol_Table : in     Symbol_Table_T;
      Valid        :    out Boolean)
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

      Valid := True;
      -- We know nothing to the contrary as yet.

      Output.Note (
           "Reading symbols from "
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

         Ada.Text_IO.Get_Line (File, Line, Last);

         if Ada.Text_IO.Col (File) = 1 then
            -- Good, we read all of a line.

            Parse_And_Define (
               Line  => Trim (
                  Translate (Line(1 .. Last), Tab_To_Blank),
                  Both),
               Table => Symbol_Table,
               Valid => Valid);

         else

            Output.Error (
                 "Symbol-file line is too long (over"
               & Natural'Image (Line'Length)
               & " characters).");

            Valid := False;

         end if;

         Output.Unnest (Mark);

      end loop;

      Ada.Text_IO.Close (File);

      Output.Note (
           "Finished symbols from "
         & File_Name
         & ".");

   exception

   when Ada.Text_IO.Name_Error =>

      Output.Error (
           "Could not open the symbol-file """
         & File_Name
         & """.");

      if Ada.Text_IO.Is_Open (File) then

         Ada.Text_IO.Close (File);

      end if;

      Valid := False;

   end Read;


   procedure Read_From_Files (Symbol_Table : in Symbol_Table_T)
   is

      Files : Options.String_Sets.String_List_T :=
         Options.String_Sets.To_List (Opt.Symbol_File_Names);
      -- All the symbol-file names.

      Valid : Boolean;
      -- Whether a symbol-file was good.

      All_Valid : Boolean := True;
      -- Whether all symbol-files files were good.

   begin

      for F in Files'Range loop

         Read (
            File_Name    => String_Pool.To_String (Files(F)),
            Symbol_Table => Symbol_Table,
            Valid        => Valid);

         All_Valid := All_Valid and Valid;

      end loop;

      if not All_Valid then

         null; -- TBD raise Input_Error;

      end if;

   end Read_From_Files;


end Symbols.Text;
