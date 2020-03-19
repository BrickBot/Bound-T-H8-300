-- Formats.ELF (body)
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
-- $Date: 2015/10/24 20:53:53 $
--
-- $Log: formats-elf.adb,v $
-- Revision 1.6  2015/10/24 20:53:53  niklas
-- Moved to free licence.
--
-- Revision 1.5  2008-02-29 09:06:40  niklas
-- Added option Opt.Warn.
--
-- Revision 1.4  2006/04/13 20:09:56  niklas
-- Added procedure Get_Header, for checks.
-- Modified procedure Get_Identification to make it useful
-- for Get_Header.
--
-- Revision 1.3  2006/04/13 18:48:54  niklas
-- Corrected the recording of the kinds of the sections, mainly
-- in procedure Get_Section. Removed the literal Not_Loaded from
-- Section_Data_Kind_T and added the literal StabStr for the
-- STABS .stabstr section. This enables the Put procedures to
-- display the data for "binary" sections.
-- Added function Kind_Of (Section) return Section_Data_Kind_T
-- but this is not a crucial part of the correction.
-- Corrected Load_Segment to use Filesz (size of segment in the
-- file) insted of Memsz (size of segment in memory, which may be
-- larger than Filesz if the segment has a BSS part). This error
-- could cause spurious end-of-file exceptions. Updated
-- Put_Binary_Segment accordingly. Also corrected Load_Segment
-- error messages to say "segment" instead of "section".
--
-- Revision 1.2  2004/06/14 18:41:12  niklas
-- Added function Executable (Segment).
--
-- Revision 1.1  2004/04/24 18:02:43  niklas
-- First Tidorum version, as child of Formats.
--
--
-- <Log: elf.adb>
-- Revision 1.4  2003/02/17 14:04:05  holsti
-- Corrected handling of STABS symbols (Stab_Line_T) for multi-module
-- programs, interpreting code 16#00# as N_HdrSym rather than N_Undef
-- and N_Strx as the offset relative to the start of the module.
-- Removed unused Set_Index procedure.
-- Modified the computation of IO.Set_Index parameters to reduce the
-- risk of overflow.
--
-- Revision 1.3  2002/12/30 13:20:14  holsti
-- Check both memory length and file length for segments.
--
-- Revision 1.2  2001/11/19 11:27:13  saarinen
-- Modified some warnings into notes.
--
-- Revision 1.1  2001/08/27 09:12:56  saarinen
-- First version.
--


with Ada.Text_IO;

with Output;
with Formats.ELF.Opt;
with Formats.ELF.Text;


package body Formats.ELF is


   use type IO.Count;


   subtype Short_Bit_T is Integer range 0 .. 15;
   subtype Bit_T       is Integer range 0 .. 31;


   function Bit (Number : Short_Bit_T; Within : Uhalf_T) return Boolean
   --
   -- Picks bit Number Within a short value.
   --
   is
   begin
      return (Within / (2**Number)) mod 2 = 1;
   end Bit;


   function Bit (Number : Bit_T; Within : Uword_T) return Boolean
   --
   -- Picks bit Number Within a long value.
   --
   is
   begin
      return (Within / (2**Number)) mod 2 = 1;
   end Bit;


   -- Symbol info field manipulation.


   function Symbol_Kind (Item : Symbol_Info_T)
   return Symbol_Info_T
   is
   begin
      return Item and 16#F#;
   end Symbol_Kind;


   function Symbol_Bind (Item : Symbol_Info_T)
   return Symbol_Info_T
   is
   begin
      return (Item / 2**4) and 16#F#;
   end Symbol_Bind;


   function Executable (Segment : Segment_T) return Boolean
   is
   begin

      return (Segment.Header.P_Flags and Pf_X) /= 0;

   end Executable;


   function Image (Item : Section_IO_T) return String
   is
   begin
      return "File Index         =" & IO.Count'Image(Item.Index);
   end Image;


   function Image (Item : Segment_IO_T) return String
   is
   begin
      return "File Index         =" & IO.Count'Image(Item.Index);
   end Image;


   function Image (Item : File_IO_T) return String
   is
   begin
      return "File Index         =" & IO.Count'Image (Item.Index);
   end Image;


   function To_String (Name : in String) return String
   --
   -- Return the string with null paddings removed.
   --
   is
      Last : Natural := Name'Last;
   begin

      while Name(Last) = Character'Val(0) loop
         Last := Last - 1;
         exit when Last = 0;
      end loop;

      return Name(Name'First..Last);

   end To_String;


   --
   --    ELF file loading:
   --


   function Accepts (
      Name : in String;
      File : in IO.File_Type)
   return Boolean
   is

      Stream : IO.Stream_Access := IO.Stream (File);
      -- Stream access to the file.

      Ident : Elf_Ident_T;
      -- The leading identification characters of an ELF file.

      Result : Boolean;
      -- The outcome.

   begin

      if IO.Size (File) < Ident'Length then
         -- This file is too short to be an ELF file.

         Result := False;

      else
         -- Read and check the Ident.

         IO.Set_Index (File, IO.Positive_Count'First);

         Elf_Ident_T'Read (Stream, Ident);

         Result :=
                Ident(Ei_Mag0) = Elfmag0
            and Ident(Ei_Mag1) = Elfmag1
            and Ident(Ei_Mag2) = Elfmag2
            and Ident(Ei_Mag3) = Elfmag3;

      end if;

      IO.Set_Index (File, IO.Positive_Count'First);

      return Result;

   end Accepts;


   procedure Get_Identification (
      Stream : in     IO.Stream_Access;
      Valid  : in out Boolean;
      Ident  :    out ELF_Ident_T)
   --
   -- Reads the ELF identification from the Stream.
   -- Updates Formats.Endian according to the Identification.
   -- Sets Valid to False if any error is detected.
   --
   is
   begin

      for I in Ident'Range loop

         Byte_T'Read (Stream, Ident(I));

      end loop;

      case Ident(Ei_Data) is

         when Elfdata2lsb => Formats.Endian := Little;

         when Elfdata2msb => Formats.Endian := Big;

         when others =>

            Output.Error (
                 "Unknown ELF endianness code"
               & Output.Field_Separator
               & Image (Ident(Ei_Data)));

            Valid := False;

      end case;

      if Opt.Trace_Loading then

         Text.Put (Ident);

      end if;

   end Get_Identification;


   procedure Get_Header (
      From   : in     IO.File_Type;
      Valid  :    out Boolean;
      Ident  :    out ELF_Ident_T;
      Header :    out ELF_Header_T)
   is

      Stream : IO.Stream_Access := IO.Stream (From);
      -- Stream access to the file.

   begin

      IO.Set_Index (From, IO.Positive_Count'First);

      Valid := True;

      Get_Identification (
         Stream => Stream,
         Valid  => Valid,
         Ident  => Ident);      

      ELF_Header_T'Read (Stream, Header);

      if Opt.Trace_Loading then

         Text.Put (Header);

      end if;

      IO.Set_Index (From, IO.Positive_Count'First);

   exception

   when IO.End_Error =>

      Output.Error ("Unexpected end of file");

      Valid := False;

      IO.Set_Index (From, IO.Positive_Count'First);

   end Get_Header;


   procedure Get_Symbols (
      From    : in     IO.File_Type;
      File    : in     File_T;
      Section : in out Section_T)
   is

      Num_Entries : constant Natural :=
         Natural (Section.Header.Sh_Size / Section.Header.Sh_Entsize);
      -- The number of entries in the symbol table.

   begin

      if Section.Header.Sh_Size mod Section.Header.Sh_Entsize /= 0 then
         -- Huh, some octets left over.

         Output.Warning (
              "ELF symbol-section length"
            & UWord_T'Image (Section.Header.Sh_Size)
            & " octets is not a multiple of the entry length,"
            & UWord_T'Image (Section.Header.Sh_Entsize)
            & " octets; remainder ignored.");

      end if;

      Section.Data := (
         Kind    => Symtab,
         Symbols => new Symbols_T(0 ..Num_Entries - 1));

      for S in Section.Data.Symbols'Range loop

         IO.Set_Index (
            File => From,
            To   => IO.Count(Section.Header.Sh_Offset)
                  + IO.Count(Uword_T(S) * Section.Header.Sh_Entsize) + 1);

         Symbol_T'Read (File.IO.Stream, Section.Data.Symbols(S));

      end loop;

      if Opt.Trace_Loading then

         Text.Put (Section.Data.Symbols.all, File);

      end if;

   end Get_Symbols;


   procedure Get_Stabs (
      From    : in     IO.File_Type;
      File    : in     File_T;
      Stabs   :    out Section_T;
      StabStr :    out Section_T)
   --
   -- Reads the "stabs" and "stabstr" sections from the ELF file.
   --
   is
   begin

      Stabs.Data := (
         Kind  => Stab,
         Table => null);

      StabStr.Data := (Kind => Formats.ELF.StabStr);
      -- The string-pool is handled by Formats.Stabs.
 
      Formats.Stabs.Load (
         From  => From,
         Lines => (
            Start  => IO.Count(Stabs.Header.Sh_Offset) + 1,
            Octets => IO.Count(Stabs.Header.Sh_Size)),
         Strings => (
            Start  => IO.Count(StabStr.Header.Sh_Offset) + 1,
            Octets => IO.Count(StabStr.Header.Sh_Size)),
         Trace   => Opt.Trace_Loading,  -- TBC, see below.
         Giving  => Stabs.Data.Table);

      if Opt.Trace_Loading then

         Text.Put_Stabs (Stabs, File);

      end if;

   end Get_Stabs;


   procedure Get_Strings (
      From    : in     IO.File_Type;
      File    : in     File_T;
      Section :    out Section_T)
   --
   -- Reads a string section from the ELF file.
   --
   is

      Size : constant Uword_T := Section.Header.Sh_Size;
      -- The length of the string section.

      B : Byte_T;
      -- One of the string bytes.

      Buffer : constant Strings_Ref := new String (1 .. Natural(Size));
      -- To hold the string.

   begin

      Section.Data := (
         Kind    => Strtab,
         Strings => (
            Size   => Size,
            Buffer => Buffer));

      IO.Set_Index (
         File => From,
         To   => IO.Count (Section.Header.Sh_Offset) + 1);

      for I in Buffer'Range loop
         Byte_T'Read (File.IO.Stream, B);
         Buffer(I) := Character'Val(B);
      end loop;

      if Opt.Trace_Loading then
         Text.Put (Section.Data.Strings);
      end if;

   end Get_Strings;


   procedure Get_Section (
      From    : in     IO.File_Type;
      File    : in out File_T;
      Number  : in     Section_Number_T;
      Section :    out Section_T)
   --
   -- Gets a section from the current position in the file.
   -- The section data are loaded now only when the section is a
   -- string table or a symbol table.
   --
   is
   begin

      Section.IO.Index := IO.Index (From);

      -- Get the section header:

      Section_Header_T'Read (File.IO.Stream, Section.Header);

      case Section.Header.Sh_Type is

         when Sht_Null     =>

            Section.Data := (Kind => No_Data);

         when Sht_Progbits =>

            Section.Data := (Kind => Bytes);
            -- Load later.

         when Sht_Symtab   =>

            Get_Symbols (
                From    => From,
                File    => File,
                Section => Section);

            File.Symbols := Number;

         when Sht_Strtab   =>

            Get_Strings (
               From    => From,
               File    => File,
               Section => Section);

         when Sht_Rela     =>

            Section.Data := (Kind => Rela, ELF_Relas => null);
            -- Get Relocations TBA.

         when Sht_Hash     =>

            Section.Data := (Kind => Bytes);
            -- Load later.

         when Sht_Dynamic  =>

            Section.Data := (Kind => Bytes);
            -- Load later.

         when Sht_Note     =>

            Section.Data := (Kind => Bytes);
            -- Load later.

         when Sht_Nobits   =>

            Section.Data := (Kind => No_Data);

         when Sht_Rel      =>

            Section.Data := (Kind => Rel, ELF_Rels => null);
            -- Get Relocations TBA.

         when Sht_Shlib    =>

            Section.Data := (Kind => Bytes);
            -- Load later.

         when Sht_Dynsym   =>

            Get_Symbols (
               From    => From,
               File    => File,
               Section => Section);

         when Sht_Loproc   =>

            Section.Data := (Kind => No_Data);
            -- Format not specified.

         when Sht_Hiproc   =>

            Section.Data := (Kind => No_Data);
            -- Format not specified.

         when Sht_Louser   =>

            Section.Data := (Kind => No_Data);
            -- Format not specified.

         when Sht_Hiuser   =>

            Section.Data := (Kind => No_Data);
            -- Format not specified.

         when others       =>

            if Opt.Warn then

               Output.Warning (
                    "Unknown ELF section type"
                  & UWord_T'Image (Section.Header.Sh_Type)
                  & " = "
                  & Hex_Image (Section.Header.Sh_Type));

            end if;

            Section.Data := (Kind => No_Data);
            -- Format not specified.

      end case;

      if (Section.Header.Sh_Flags and Shf_Alloc) /= 0 then

         if (Section.Header.Sh_Flags and Shf_Execinstr) /= 0 then
            -- Executable section

            File.Code_Sections(Number) := True;

         end if;

         -- Data section, assuming that code sections can include some data:

         File.Data_Sections(Number) := True;

      end if;

      if Opt.Trace_Loading then
         Text.Put (Section.Header);
      end if;

   end Get_Section;


   procedure Get_Segment (
      From    : in     IO.File_Type;
      File    : in     File_T;
      Segment :    out Segment_T)
   --
   -- Gets a segment from the current position in the file.
   -- The segment data are not yet loaded.
   --
   is
   begin

      Segment.IO.Index := IO.Index (From);

      -- Get the segment header:

      Segment_Header_T'Read (File.IO.Stream, Segment.Header);

      if Opt.Trace_Loading then
         Text.Put (Segment.Header);
      end if;


   end Get_Segment;


   procedure Get_File (
      From : in     IO.File_Type;
      File : in out File_T)
   --
   -- Gets ELF information from the (open) ELF file.
   --
   is

      Ident_Valid : Boolean := True;
      -- Whether the File.Ident is valid.
      -- Not that we really care at this point.

   begin

      File.IO.Index := IO.Index (From);

      Get_Identification (
         Stream => File.IO.Stream,
         Valid  => Ident_Valid,
         Ident  => File.Ident);

      -- Get file headers:

      ELF_Header_T'Read (File.IO.Stream, File.Header);

      if Opt.Trace_Loading then
         Text.Put (File.Header);
      end if;

      -- Get segment headers:

      File.Segments := new Segments_T
         (1 .. Segment_Number_T (File.Header.E_Phnum));

      for S in File.Segments'range loop

         IO.Set_Index (
            File => From,
            To   => IO.Count(File.Header.E_Phoff)
                  + IO.Count(S - 1) * IO.Count(File.Header.E_Phentsize) + 1);

         Get_Segment (
            From    => From,
            File    => File,
            Segment => File.Segments(S));

      end loop;

      -- Initialize the section member tables.

      File.Code_Sections := new Member_Sections_T'
         (0 .. Section_Number_T (File.Header.E_Shnum - 1) => False);

      File.Data_Sections := new Member_Sections_T'
         (0 .. Section_Number_T (File.Header.E_Shnum - 1) => False);


      -- Get section headers:

      File.Sections := new Sections_T
         (0 .. Section_Number_T (File.Header.E_Shnum - 1));

      for S in File.Sections'range loop

         IO.Set_Index (
            File => From,
            To   => IO.Count(File.Header.E_Shoff)
                  + IO.Count(S) * IO.Count(File.Header.E_Shentsize) + 1);

         Get_Section (
            From    => From,
            File    => File,
            Number  => S,
            Section => File.Sections(S));

      end loop;

   end Get_File;


   --
   -- Interface procedures
   --


   procedure Put (
      File    : in File_Ref;
      Symbols : in Boolean := False;
      Strings : in Boolean := False)
   is
   begin

      Text.Put (File.Ident);

      Text.Put (File.Header);

      for S in File.Segments'range loop
         Text.Put_Segment (Number => S, Within => File.all);
      end loop;

      for S in File.Sections'range loop

         Text.Put_Section (Number => S, Within => File.all);

         if       Strings
         and then File.Sections(S).Data.Kind = Strtab
         then

            Text.Put (File.Sections(S).Data.Strings);

         elsif    Symbols
         and then File.Sections(S).Data.Kind = Symtab
         then

            Text.Put (File.Sections(S).Data.Symbols.all, File.all);

         elsif    Symbols
         and then File.Sections(S).Data.Kind = Stab
         then

            Text.Put_Stabs (File.Sections(S), File.all);

         end if;

      end loop;

   exception

      when E : others =>

         Output.Exception_Info (
            Text       => "Put (ELF file)",
            Occurrence => E);

   end Put;


   procedure Load (
      From : in     IO.File_Type;
      File :    out File_Ref)
   is

      StabStr_Num : Section_Number_T;
      -- The number of the ".stabstr" section, as indicated by
      -- the Sh_Link field of the ".stab" section if any.

   begin

      -- Create the file object and the stream:

      File := new File_T;

      File.IO.Stream := IO.Stream (From);

      -- Get file, section and segment headers:

      Get_File (
         From => From,
         File => File.all);

      -- Load ".stabs" and ".comments" if any:

      for S in File.Sections'Range loop

         if Name_Of (S, File.all) = ".stab" then

            File.Stabs := S;

            StabStr_Num := Section_Number_T (File.Sections(S).Header.Sh_Link);

            Get_Stabs (
               From    => From,
               File    => File.all,
               Stabs   => File.Sections(S),
               StabStr => File.Sections(StabStr_Num));

         elsif Name_Of (S, File.all) = ".comment" then

            Get_Strings (
               From    => From,
               File    => File.all,
               Section => File.Sections(S));

         end if;

      end loop;

   exception

      when IO.End_Error =>

         Output.Error (Text => "Unexpected end of file");

         raise;

      when E : others =>

         Output.Exception_Info (
            Text       => "Loading ELF file",
            Occurrence => E);

         raise;

   end Load;


   function Is_Binary (
      Number : Section_Number_T;
      File   : File_T)
   return Boolean
   is
   begin
      return File.Sections(Number).Data.Kind = Bytes;
   end Is_Binary;


   procedure Load_Section (
      Number  : in Section_Number_T;
      From    : in     IO.File_Type;
      Within  : in     File_Ref;
      Data    :    out Octets_T)
   is

      Section : Section_T renames Within.Sections (Number);
      -- The section to be read.

      Index : IO.Positive_Count;
      -- The file position for the section data.

   begin

      if Section.Header.Sh_Size /= Data'Length then

         Output.Fault (
            Location => "Formats.ELF.Load_Section",
            Text     =>
                 "Section length /= Data length for section"
               & Section_Number_T'Image (Number));

      elsif Section.Header.Sh_Size = 0 then

         Output.Note (Text =>
              "Section"
            & Section_Number_T'Image (Number)
            & " has zero length, not loaded.");

      elsif Section.Header.Sh_Type = Sht_Nobits then
         -- File contains no data for this section.
         -- The section has only uninitialized data.
         -- Need not be allocated, TBD.

         Output.Note (Text =>
              "Section"
            & Section_Number_T'Image (Number)
            & " has uninitialized data, zero loaded.");

         Data := (others => 0);

      else
         -- Loadable data or code section (could also be a string or
         -- symbol table, but hey, those can also be loaded).

         Index := Within.IO.Index + IO.Count(Section.Header.Sh_Offset);

         IO.Set_Index (
            File => From,
            To   => Index);

         Octets_T'Read (Within.IO.Stream, Data);

      end if;

   exception

      when IO.End_Error =>

         Output.Error (Text =>
             "Unexpected end of file while reading section"
           & Section_Number_T'Image (Number)
           & " from IO index"
           & IO.Positive_Count'Image (Index));

         raise;

      when E : others =>

         Output.Exception_Info (
            Text       => "Loading ELF file section",
            Occurrence => E);

         raise;

   end Load_Section;


   procedure Load_Segment (
      Number  : in     Segment_Number_T;
      From    : in     IO.File_Type;
      Within  : in     File_Ref;
      Data    :    out Octets_T)
   is

      Segment : Segment_T renames Within.Segments (Number);
      -- The section to be read.

      Index : IO.Positive_Count;
      -- The file position for the section data.

   begin

      if Segment.Header.P_Filesz /= Data'Length then

         Output.Fault (
            Location => "Formats.ELF.Load_Segment",
            Text     =>
                 "Segment Filesz length /= Data length for segment"
               & Segment_Number_T'Image (Number));

      elsif Segment.Header.P_Memsz = 0 then

         Output.Note (Text =>
              "Segment" & Segment_Number_T'Image (Number)
            & " has zero memory length, not loaded.");

      elsif Segment.Header.P_Filesz = 0 then

         Output.Note (Text =>
              "Segment" & Segment_Number_T'Image (Number)
            & " has zero file length, not loaded.");

      else

         Index := Within.IO.Index + IO.Count(Segment.Header.P_Offset);

         IO.Set_Index (
            File => From,
            To   => Index);

         Octets_T'Read (Within.IO.Stream, Data);

      end if;

   exception

      when IO.End_Error =>

         Output.Error (Text =>
             "Unexpected end of file while reading segment"
           & Segment_Number_T'Image (Number)
           & " from IO index"
           & IO.Positive_Count'Image (Index));

         raise;

      when E : others =>

         Output.Exception_Info (
            Text       => "Loading ELF file segment",
            Occurrence => E);

         raise;

   end Load_Segment;


   procedure Put_Binary_Section (
      From    : in IO.File_Type;
      File    : in File_Ref;
      Number  : in Section_Number_T;
      Section : in Section_T;
      Unit    : in Positive)
   --
   -- Displays the section data, if it is a binary section.
   -- The Unit parameter is the assumed address unit.
   --
   is
      use Ada.Text_IO;

      Data : Octets_T (0 .. Natural (Section.Header.Sh_Size) - 1);
      -- The data for the section, in transit from the ELF file
      -- to the display.

   begin

      Put_Line (
           "Section """
         & Name_Of (Number, File.all)
         & """: "
         & Section_Data_Kind_T'Image (Kind_Of (Number, File.all))
         & ","
         & IO.Count'Image (Data'Length)
         & " octets.");

      if Is_Binary (Number, File.all) then

         Put_Line ("Data follows (address unit = "
             & Positive'Image (Unit) & " bytes):");

         Load_Section (
            Number => Number,
            From   => From,
            Within => File,
            Data   => Data);

         if       Section.Header.Sh_Entsize > 0
         and then Section.Header.Sh_Entsize rem 4 = 0
         then

            Text.Put_Data (
               Section => Section,
               Data    => Data,
               Format  => (1 .. 2 => 2),
               Columns => Integer (Section.Header.Sh_Entsize / 4),
               Unit    => Unit);

         else

            Text.Put_Data (
               Section => Section,
               Data    => Data,
               Format  => (1 .. 2 => 2),
               Columns => 4,
               Unit    => Unit);

         end if;

         New_Line;

      end if;

   end Put_Binary_Section;


   procedure Put_Binary_Segment (
      From    : in IO.File_Type;
      File    : in File_Ref;
      Number  : in Segment_Number_T;
      Segment : in Segment_T;
      Unit    : in Positive)
   --
   -- Displays the segment data.
   -- The Unit parameter is the assumed address unit.
   --
   is
      use Ada.Text_IO;

      Data : Octets_T (0 .. Natural (Segment.Header.P_Filesz) - 1);
      -- The data for the segment, in transit from the ELF file
      -- to the display.

   begin

      Put_Line (
          "Segment"
         & Segment_Number_T'Image (Number)
         & ","
         & IO.Count'Image (Data'Length)
         & " octets.");

      Load_Segment (
         Number => Number,
         From   => From,
         Within => File,
         Data   => Data);

      Put_Line ("Data follows (address unit = "
          & Positive'Image (Unit) & " bytes):");

      Text.Put_Data (
         Segment => Segment,
         Data    => Data,
         Format  => (1 .. 2 => 2),
         Columns => 8,
         Unit    => Unit);

      New_Line;

   end Put_Binary_Segment;


   procedure Put_Binary_Data (
      From : in IO.File_Type;
      File : in File_Ref)
   is
      use Ada.Text_IO;

      Unit : constant Positive := 1;
      -- Bytes per addressing unit, assumed.

   begin

      -- Display the sections:

      Put_Line ("Section data for binary sections:");

      for S in File.Sections'Range loop

         Put_Binary_Section (
            From    => From,
            File    => File,
            Number  => S,
            Section => File.Sections(S),
            Unit    => Unit);

      end loop;

      -- Display the segments:

      for S in File.Segments'Range loop

         Put_Binary_Segment (
            From    => From,
            File    => File,
            Number  => S,
            Segment => File.Segments(S),
            Unit    => Unit);

      end loop;

   end Put_Binary_Data;


   procedure Load_And_Put (
      From    : in IO.File_Type;
      Data    : in Boolean := False;
      Symbols : in Boolean := False;
      Strings : in Boolean := False)
   is

      File : File_Ref;

   begin

      Load (
        From => From,
        File => File);

      Put (
         File    => File,
         Symbols => Symbols,
         Strings => Strings);

      if Data then

         Put_Binary_Data (
            From => From,
            File => File);

      end if;

      Close (File);

   end Load_And_Put;


   procedure Load_And_Put (
      File_Name : in String;
      Data      : in Boolean := False;
      Symbols   : in Boolean := False;
      Strings   : in Boolean := False)
   is

      From : IO.File_Type;

   begin

      IO.Open (
         File => From,
         Mode => IO.In_File,
         Name => File_Name);

      Load_And_Put (
         From    => From,
         Data    => Data,
         Symbols => Symbols,
         Strings => Strings);

      IO.Close (From);

   exception

      when IO.Name_Error =>
         Output.Error ("File does not exist");
         raise;

      when IO.Use_Error =>
         Output.Error ("Cannot read file");
         raise;

   end Load_And_Put;


   function String_At (
      Offset  : Uword_T;
      Within  : Strings_T)
   return String
   --
   -- Gets a string from a string table.
   --
   -- The string consists of the characters from the given Offset
   -- up to but not including a terminating nul character.
   --
   is

      Start : Positive := Natural(Offset + 1);

      I : Positive := Start;

      Buffer : String renames Within.Buffer.all;

   begin

      -- Search for the end of the string:
      loop
         exit when Buffer(I) = Character'Val(0);
         I := I + 1;
      end loop;

      return Buffer (Start .. I - 1);

   end String_At;


   function Name_Of (
      Section : Section_Number_T;
      Within  : File_T)
   return String
   is

      Sec_Names : constant Section_Number_T := Within.Header.E_Shstrndx;
      -- The number of the section holding the section name string table.

   begin

      if       Sec_Names /= Shn_Undef
      and then Within.Sections(Sec_Names).Data.Kind = Strtab
      then
         -- Section name string table is present and loaded.

         return String_At (
            Offset => Within.Sections(Section).Header.Sh_Name,
            Within => Within.Sections(Sec_Names).Data.Strings);

      else
         -- No Section Names

         return "[No section names]";

      end if;

   end Name_Of;


   function Kind_Of (
      Section : Section_Number_T;
      Within  : File_T)
   return Section_Data_Kind_T
   is
   begin

      return Within.Sections(Section).Data.Kind;

   end Kind_Of;


   function Name_Of (
      Symbol  : Symbol_T;
      Within  : File_T)
   return String
   is

      String_Sec : Section_Number_T;
      -- The number of the String section.

   begin

      if Within.Symbols = Shn_Undef then
         -- No symbol table.

         return Hex_Image (Symbol.St_Name);

      else
         -- It seems we have a symbol table.

         String_Sec := Section_Number_T (
            Within.Sections(Within.Symbols).Header.Sh_Link);

         return String_At (
            Offset => Symbol.St_Name,
            Within => Within.Sections(String_Sec).Data.Strings);

      end if;

   end Name_Of;


   function Name_Of (
      Stab    : Formats.Stabs.Line_T;
      Within  : File_T;
      Cut     : Boolean := False)
   return String
   is

      Cut_Char : Character;
      -- For Cut.

   begin

      if Within.Stabs = Shn_Undef then
         -- There are no Stabs data.

         return "[No Stabs data]";

      else
         -- There are some Stabs data, it seems.

         if Cut then

            Cut_Char := ':';

         else

            Cut_Char := Character'Val(0);

         end if;

         return Formats.Stabs.Symbol (
            Line   => Stab,
            Within => Within.Sections(Within.Stabs).Data.Table.all,
            Cut    => Cut_Char);

      end if;

   end Name_Of;


   function Number_Of (
      Section : String;
      Within  : File_T)
   return Section_Number_T
   is
   begin

      for S in Within.Sections'Range loop

         if Section = Name_Of (S, Within) then

            return S;

         end if;

      end loop;

      -- There is no such section.
      raise Section_Not_Found;

   end Number_Of;


   function Length_Of (
      Section : Section_Number_T;
      Within  : File_T)
   return Natural
   is
   begin
      return Natural (Within.Sections(Section).Header.Sh_Size);
   end Length_Of;


   function Section (
      Name     : String;
      Within   : File_T;
      Optional : Boolean := False)
   return Segment_Desc_T
   is
      use type IO.Count;

      Number : Section_Number_T;
      -- The number of the section.

      Header : Section_Header_T;
      -- The header of the section.

   begin

      Number := Number_Of (Section => Name, Within => Within);

      Header := Within.Sections(Number).Header;

      return (
         Start  => Within.IO.Index + IO.Count (Header.Sh_Offset),
         Octets => IO.Count (Header.Sh_Size));

   exception

      when Section_Not_Found =>

         Output.Note (
              "No ELF section """
            & Name
            & """ is present.");

         if Optional then

            return (Start => IO.Positive_Count'Last, Octets => 0);

         else

            raise;

         end if;

   end Section;


   function String_At (
      Offset  : Uword_T;
      Within  : File_T)
   return String
   is

      String_Sec : Section_Number_T;
      -- The number of the String section.

   begin

      if Within.Symbols = Shn_Undef then
         -- No symbol table.

         return "[No symbol table]";

      else
         -- It seems we have a symbol table.

         String_Sec := Section_Number_T (
            Within.Sections(Within.Symbols).Header.Sh_Link);

         return String_At (
            Offset => Offset,
            Within => Within.Sections(String_Sec).Data.Strings);

      end if;

   end String_At;


   procedure Close (File : in out File_Ref)
   is
   begin

      -- TBA deallocation, perhaps later.

      File := null;

   end Close;


end Formats.ELF;
