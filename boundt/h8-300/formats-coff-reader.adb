-- Formats.COFF.Reader (body)
--
-- Author: Niklas Holsti
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
-- $Revision: 1.6 $
-- $Date: 2015/10/26 22:19:14 $
--
-- $Log: formats-coff-reader.adb,v $
-- Revision 1.6  2015/10/26 22:19:14  niklas
-- Version 4b, and moved to free open-source licence.
--
-- Revision 1.5  2015/10/26 21:56:44  niklas
-- Updated to use the Formats endianness type.
--
-- Revision 1.4  2005/03/20 20:14:48  niklas
-- Extended (corrected) Load.Build_Object to consider more reasons
-- for not loading the given section.
--
-- Revision 1.3  2005/03/17 07:24:46  niklas
-- Adapted to privatization of Program_T attributes as follows.
-- Changed Load.Symbol_Table to be "in" mode as befits a
-- reference type. Also removed call of Symbols.Erase because
-- this is now done in the general part.
--
-- Revision 1.2  2004/06/16 07:40:10  niklas
-- Updated for changes in Format (exception name).
--
-- Revision 1.1  2004/05/31 20:28:37  niklas
-- First version.
--


with Format;
with Formats.COFF.Opt;
with Formats.COFF.Parsing.For_H8_300;
with Formats.COFF.Text;
with H8_300;
with Output;
with Processor;


package body Formats.COFF.Reader is


   function Accepts (
      Name : in String;
      File : in IO.File_Type)
   return Boolean
   renames Formats.COFF.Accepts;


   procedure Load (
      File         : in     IO.File_Type;
      Content      : in out Memory.Content_T;
      Symbol_Table : in     Symbols.Symbol_Table_T)
   is

      COFF_File : File_Ref;
      -- The file.

      COFF_Parser : Formats.COFF.Parsing.For_H8_300.Action_T;
      -- Parsing the COFF symbols.


      procedure Load (Section : in Section_T)
      --
      -- Loads the data of a section into the memory Content.
      --
      is

         Address : constant Processor.Address_T :=
            Processor.Address_T (Section.Header.Physical_Address);
         -- The load address (start) of the section.

         Data : Octets_T (0 .. Natural (Section.Header.Length) - 1);
         -- The data for the section, in transit from the COFF file
         -- to the memory content structure.

      begin

         Load_Section (
            Section => Section,
            From    => File,
            Within  => COFF_File,
            Data    => Data);

         Memory.Allocate (
            Address => Address,
            Span    => Data'Last,
            Within  => Content);

         Memory.Load (
            Into   => Address,
            Data   => Data,
            Within => Content);

      end Load;


      procedure Explain_No_Load (
         Section : in Section_T;
         Reason  : in String)
      --
      -- Explains, as a Note, why this section is not loaded.
      --
      is
      begin

         Output.Note (Text =>
              "Section """
            & To_String (Section.Header.Name)
            & """ not loaded"
            & Output.Field_Separator
            & Reason);

      end Explain_No_Load;


      procedure Build_Object
      --
      -- Creates the Object from the COFF data, collecting all
      -- PM and DM sections.
      --
      is

         Section : Section_T;
         -- A section, perhaps to be loaded.

         Flags : Section_Flags_T;
         -- The flags of Section.

      begin

         for S in COFF_File.Sections'range loop

            Section := COFF_File.Sections(S);

            Flags := Section.Header.Flags;

            if Flags.Dummy then

               Explain_No_Load (Section, "Dummy section");

            elsif Flags.No_Load then

               Explain_No_Load (Section, "No-load section");

            elsif Flags.Bss_Data then

               Explain_No_Load (Section, "BSS section");

            elsif Flags.Comment then

               Explain_No_Load (Section, "Comment section");

            elsif Flags.Overlay then

               Explain_No_Load (Section, "Overlay section");

            elsif Flags.Lib then

               Explain_No_Load (Section, "Lib section");

            elsif Section.Header.Length = 0 then

               Explain_No_Load (Section, "Zero length");

            else

               Load (Section);

            end if;

         end loop;

      end Build_Object;


   begin  -- Load

      -- Load the COFF file header information:

      Load (
         From => File,
         File => COFF_File);

      -- Find all relevant sections and build object:

      Build_Object;

      -- TBA Check_Relocations

      -- Handle the symbolic information:

      Formats.COFF.Parsing.Parse_Symbols (
         From         => COFF_File,
         Trace        => Formats.COFF.Opt.Trace_Loading,
         Action       => COFF_Parser,
         Symbol_Table => Symbol_Table);

   exception

      when IO.End_Error =>

         Output.Error (Text => "Unexpected end of COFF file");

         raise Format.Loading_Error;

      when E : others =>

         Output.Exception_Info (
            Text       => "Loading COFF file",
            Occurrence => E);

         raise Format.Loading_Error;

   end Load;


   procedure Dump (File : in IO.File_Type)
   is
   begin

      Load_And_Put (
         From    => File,
         Data    => True,
         Symbols => True,
         Strings => True);

   exception

      when E : others =>

         Output.Exception_Info (
            Text       => "Dumping COFF file",
            Occurrence => E);

   end Dump;


begin  -- Formats.COFF.Reader

   -- Define the COFF options appropriate for GNU H8/300:

   Formats.COFF.Opt.Trace_Loading      := False;
   Formats.COFF.Opt.Short_End          := Big;
   Formats.COFF.Opt.Long_End           := Big;
   Formats.COFF.Opt.Magic_Number       := 16#8300#;
   Formats.COFF.Opt.Line_Number_Length := Formats.COFF.Opt.Long;

end Formats.COFF.Reader;
