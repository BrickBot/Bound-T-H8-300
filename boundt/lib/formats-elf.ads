-- Formats.ELF (decl)
--
-- Format definition for ELF executable and object files.
--
-- References:
-- [1] System V Application Binary Interface. Ch. 4-5.
--     The Santa Cruz Operation, Inc & AT&T. Edition 3.1, March 1997.
-- [2] System V Application Binary Interface SPARC Processor Supplement.
--     Ch. 4-5.
--     The Santa Cruz Operation, Inc & AT&T. Third Edition, 1996.
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
-- $Log: formats-elf.ads,v $
-- Revision 1.6  2015/10/24 20:53:53  niklas
-- Moved to free licence.
--
-- Revision 1.5  2008-02-29 20:41:22  niklas
-- Added Em_ARM = 40.
--
-- Revision 1.4  2006/04/13 20:07:41  niklas
-- Added procedure Get_Header, for checks.
--
-- Revision 1.3  2006/04/13 18:44:19  niklas
-- Corrected the recording of the kinds of the sections, mainly
-- in procedure Get_Section. Removed the literal Not_Loaded from
-- Section_Data_Kind_T and added the literal StabStr for the
-- STABS .stabstr section. This enables the Put procedures to
-- display the data for "binary" sections.
-- Added function Kind_Of (Section) return Section_Data_Kind_T
-- but this is not a crucial part of the correction.
--
-- Revision 1.2  2004/06/14 18:41:12  niklas
-- Added function Executable (Segment).
--
-- Revision 1.1  2004/04/24 18:02:43  niklas
-- First Tidorum version, as child of Formats.
--
--
-- <Log: elf.ads>
-- Revision 1.2  2003/02/17 14:00:18  holsti
-- Corrected handling of STABS symbols (Stab_Line_T) for multi-module
-- programs, interpreting code 16#00# as N_HdrSym rather than N_Undef.
-- Corrected comment on String_At (no 3 character fudge).
--
-- Revision 1.1  2001/08/27 09:12:56  saarinen
-- First version.
--


with Ada.Streams.Stream_IO;

with Formats.Stabs;


package Formats.ELF is
--
-- This package provides operations to access executable, binary
-- programs from a disk file in ELF (Executable and Linking Format) form.
--
-- The physical format of the ELF data is completely exposed here.
-- Only the way the data are read from the external file is
-- encapsulated in the package body.


   package IO renames Ada.Streams.Stream_IO;
   --
   -- The ELF file is read using stream i/o.
   -- We assume that Stream_Element is an octet.


   --
   --    Basic ELF Types and their Read operations.
   --

   subtype Byte_T is Octet_T;
   --
   -- The ELF file is defined in terms of 8-bit items.


   subtype Uhalf_T is Unsigned_16_T;
   --
   -- Sometimes two bytes make up a 16-bit word.


   subtype Uword_T is Unsigned_32_T;
   --
   -- Sometimes four bytes make up a 32-bit word.


   subtype Sword_T is Signed_32_T;
   --
   -- Sometimes 32-bit words are signed.


   --
   --    Derived ELF types:
   --


   type Addr_T is new Uword_T;
   --
   -- Time and date stamp.
   -- Same format as Unix "time_t".


   type Off_T is new Uword_T;
   --
   -- Pointer to a location in the ELF file.
   -- A byte offset relative to the start of the ELF File Header.


   -- Identification Indexes (converted from 0 .. 15 to 1 .. 16)

   Ei_Mag0    : constant := 1;
   Ei_Mag1    : constant := 2;
   Ei_Mag2    : constant := 3;
   Ei_Mag3    : constant := 4;
   Ei_Class   : constant := 5;
   Ei_Data    : constant := 6;
   Ei_Version : constant := 7;
   Ei_Pad     : constant := 8;
   Ei_Nident  : constant := 16;

   -- Constants in ELF identification

   Elfmag0    : constant := 16#7f#;
   Elfmag1    : constant := Character'Pos('E');
   Elfmag2    : constant := Character'Pos('L');
   Elfmag3    : constant := Character'Pos('F');

   Elfclassnone : constant := 0;
   Elfclass32   : constant := 1;
   Elfclass64   : constant := 2;

   Elfdatanone  : constant := 0;
   Elfdata2lsb  : constant := 1;
   Elfdata2msb  : constant := 2;

   type Elf_Ident_T is array (1 .. Ei_Nident) of Byte_T;


   type Section_Number_T is new Uhalf_T;
   type Segment_Number_T is new Uhalf_T;
   -- Sections are numbered from 0 .. E_Shnum - 1,
   -- where the section 0 is the special index 0 entry
   -- see [1] page 4-15.
   -- Segments are numbered from 1 .. E_PhNum with no special entries.


   -- ELF Header:

   type ELF_Header_T is record
      E_Type           : Uhalf_T;
      E_Machine        : Uhalf_T;
      E_Version        : UWord_T;
      E_Entry          : Addr_T;
      E_Phoff          : Off_T;
      E_Shoff          : Off_T;
      E_Flags          : Uword_T;
      E_Ehsize         : Uhalf_T;
      E_Phentsize      : Uhalf_T;
      E_Phnum          : Segment_Number_T;
      E_Shentsize      : Uhalf_T;
      E_Shnum          : Section_Number_T;
      E_Shstrndx       : Section_Number_T;
   end record;


   -- Constants in ELF header:
   Et_None    : constant := 0;
   Et_Rel     : constant := 1;
   Et_Exec    : constant := 2;
   Et_Dyn     : constant := 3;
   Et_Core    : constant := 4;
   Et_Loproc  : constant := 16#Ff00#;
   Et_Hiproc  : constant := 16#Ffff#;

   Em_None    : constant := 0;
   Em_M32     : constant := 1;
   Em_Sparc   : constant := 2;
   Em_386     : constant := 3;
   Em_68k     : constant := 4;
   Em_88k     : constant := 5;
   Em_860     : constant := 7;
   Em_Mips    : constant := 8;
   Em_ARM     : constant := 40;

   Ev_None    : constant := 0;
   Ev_Current : constant := 1;


   -- ELF Section header:

   type Section_Header_T is record
      Sh_Name          : UWord_T;
      Sh_Type          : UWord_T;
      Sh_Flags         : Uword_T;
      Sh_Addr          : Addr_T;
      Sh_Offset        : Off_T;
      Sh_Size          : Uword_T;
      Sh_Link          : Uword_T;
      Sh_Info          : Uword_T;
      Sh_Addralign     : Uword_T;
      Sh_Entsize       : Uword_T;
   end record;


   -- Section types:
   Sht_Null       : constant := 0;
   Sht_Progbits   : constant := 1;
   Sht_Symtab     : constant := 2;
   Sht_Strtab     : constant := 3;
   Sht_Rela       : constant := 4;
   Sht_Hash       : constant := 5;
   Sht_Dynamic    : constant := 6;
   Sht_Note       : constant := 7;
   Sht_Nobits     : constant := 8;
   Sht_Rel        : constant := 9;
   Sht_Shlib      : constant := 10;
   Sht_Dynsym     : constant := 11;
   Sht_Loproc     : constant := 16#70000000#;
   Sht_Hiproc     : constant := 16#7fffffff#;
   Sht_Louser     : constant := 16#80000000#;
   Sht_Hiuser     : constant := 16#ffffffff#;

   -- Section flags
   Shf_Write     : constant := 1;
   Shf_Alloc     : constant := 2;
   Shf_Execinstr : constant := 4;
   Shf_Maskproc  : constant := 16#F0000000#;

   -- Special section indexes
   Shn_Undef     : constant := 0;
   Shn_Loreserve : constant := 16#Ff00#;
   Shn_Loproc    : constant := 16#Ff00#;
   Shn_Hiproc    : constant := 16#Ff1f#;
   Shn_Abs       : constant := 16#Fff1#;
   Shn_Common    : constant := 16#Fff2#;
   Shn_Hireserve : constant := 16#Ffff#;


   -- Symbol info type with functions.

   type Symbol_Info_T is new Byte_T;

   function Symbol_Kind (Item : Symbol_Info_T)
   return Symbol_Info_T;
   --
   -- Returns the Kind part of the info field.

   function Symbol_Bind (Item : Symbol_Info_T)
   return Symbol_Info_T;
   --
   -- Returns the Bind part of the info field.



   -- Symbol table
   type Symbol_T is record
      St_Name   : Uword_T;
      St_Value  : Addr_T;
      St_Size   : Uword_T;
      St_Info   : Symbol_Info_T;
      St_Other  : Byte_T;
      St_Shndx  : Section_Number_T;
   end record;

   -- Symbol binding (St_Info >> 4)
   Stb_Local   : constant := 0;
   Stb_Global  : constant := 1;
   Stb_Weak    : constant := 2;
   Stb_Loproc  : constant := 13;
   Stb_Hiproc  : constant := 15;

   -- Symbol type (St_Info and 16#f#)
   Stt_Notype  : constant := 0;
   Stt_Object  : constant := 1;
   Stt_Func    : constant := 2;
   Stt_Section : constant := 3;
   Stt_File    : constant := 4;
   Stt_Loproc  : constant := 13;
   Stt_Hiproc  : constant := 15;


   -- Segment (Program) Header
   type Segment_Header_T is record
      P_Type   : UWord_T;
      P_Offset : Off_T;
      P_Vaddr  : Addr_T;
      P_Paddr  : Addr_T;
      P_Filesz : UWord_T;
      P_Memsz  : UWord_T;
      P_Flags  : UWord_T;
      P_Align  : UWord_T;
   end record;


   -- Segment types
   Pt_Null     : constant := 0;
   Pt_Load     : constant := 1;
   Pt_Dynamic  : constant := 2;
   Pt_Interp   : constant := 3;
   Pt_Note     : constant := 4;
   Pt_Shlib    : constant := 5;
   Pt_Phdr     : constant := 6;
   Pt_Loproc   : constant := 16#70000000#;
   Pt_Hiproc   : constant := 16#7fffffff#;


   -- Segment flags
   Pf_X         : constant := 1;
   Pf_W         : constant := 2;
   Pf_R         : constant := 4;
   Pf_Maskproc  : constant := 16#F0000000#;

   -- ELF relocation records:

   type ELF_Rel_T is record
      R_Offset : Addr_T;
      R_Info   : Uword_T;
   end record;

   type ELF_Rela_T is record
      R_Offset : Addr_T;
      R_Info   : Uword_T;
      R_Addend : Sword_T;
   end record;

   type ELF_Rels_T is array (Positive range <>) of ELF_Rel_T;
   type ELF_Rels_Ref is access ELF_Rels_T;

   type ELF_Relas_T is array (Positive range <>) of ELF_Rela_T;
   type ELF_Relas_Ref is access ELF_Relas_T;


   -- ELF Line number declaration:

   type Line_Number_T is record
      Line_Address : Uword_T;
      Line_Number  : Uhalf_T;
   end record;
   --
   -- Line_Address contains a symbol table index if Line_Number = 0
   -- and a physical address otherwise.

   type Line_Numbers_T is array (Positive range <>) of Line_Number_T;
   type Line_Numbers_Ref is access Line_Numbers_T;


   --
   -- Types for describing the ELF object internally:
   --


   type Symbols_T is array (Natural range <>) of Symbol_T;
   type Symbols_Ref is access Symbols_T;

   -- The (extended) string area:

   type Strings_Ref is access String;

   type Strings_T is record
      Size   : Uword_T;
      Buffer : Strings_Ref;
   end record;


   --
   -- An ELF section:
   --


   type Section_Data_Kind_T is (
      No_Data,
      Bytes,    -- Unformatted bytes usually for code or data,
                -- or for unsupported data kind.
      Rel,      -- Relocations.
      Rela,     -- Relocations with explicit appends.
      -- Hash,     -- Symbol hash table.
      -- Dynamic,  -- Dynamic linking table.
      SymTab,   -- Symbol table.
      Stab,     -- Stab symbol table.
      StabStr,  -- Stab strings table.
      StrTab,   -- String table.
      Lines     -- Line number information.
      -- TBA
   );


   type Section_Data_T (Kind : Section_Data_Kind_T := No_Data)
   is record

      case Kind is

      when No_Data =>
         null;

      when Bytes =>
         null;
         -- The client stores the data octets elsewhere.

      when Rel =>
         ELF_Rels : ELF_Rels_Ref;

      when Rela =>
         ELF_Relas : ELF_Relas_Ref;

      when Symtab =>
         Symbols : Symbols_Ref;

      when Stab =>
         Table : Formats.Stabs.Table_Ref;

      when StabStr =>
         null;
         -- Data stored in the Stab.Table.

      when Strtab =>
         Strings : Strings_T;

      when Lines  =>
         Line_Numbers : Line_Numbers_Ref;

      end case;

   end record;


   type Section_IO_T is private;


   type Section_T is record
      Header       : Section_Header_T;
      IO           : Section_IO_T;
      Data         : Section_Data_T;
   end record;
   --
   -- The components have meanings obvious from their names,
   -- except perhaps for:
   --
   -- IO      Stuff needed by the access routines.


   type Sections_T is array (Section_Number_T range <>) of Section_T;
   --
   -- Sections are numbered 0 .. N - 1.

   type Sections_Ref is access Sections_T;


   type Member_Sections_T is array (Section_Number_T range <>) of Boolean;
   --
   -- For section membership tables; code sections, data sections, etc.

   type Member_Sections_Ref is access Member_Sections_T;



   type Segment_IO_T is private;

   type Segment_T is record
      Header : Segment_Header_T;
      IO     : Segment_IO_T;
   end record;
   --
   -- The components have meanings obvious from their names,
   -- except perhaps for:
   --
   -- IO      Stuff needed by the access routines.


   function Executable (Segment : Segment_T) return Boolean;
   --
   -- Whether the segment contains executable code.


   type Segments_T is array (Segment_Number_T range <>) of Segment_T;
   --
   -- Segments are numbered 1 .. N.

   type Segments_Ref is access Segments_T;




   --
   -- The whole ELF file:
   --

   type File_IO_T is limited private;


   type File_T is record
      Ident        : Elf_Ident_T;
      Header       : ELF_Header_T;
      Sections     : Sections_Ref;
      Segments     : Segments_Ref;
      Symbols      : Section_Number_T := Shn_Undef;
      Stabs        : Section_Number_T := Shn_Undef;

      Code_Sections: Member_Sections_Ref;
      Data_Sections: Member_Sections_Ref;

      IO           : File_IO_T;
   end record;
   --
   -- The structure and part of the data in an ELF file.
   --
   -- Symbols
   --    Section number of the ELF symbol table.
   -- Stabs
   --    Section number of the stab-symbol table.
   -- IO
   --    Encapsulates the way the file is accessed in the current
   --    implementation.

   type File_Ref is access File_T;


   --
   --    Image functions:
   --


   function Image (Item : Section_IO_T) return String;

   function Image (Item : Segment_IO_T) return String;

   function Image (Item : File_IO_T) return String;

   function To_String (Name : in String) return String;
   --
   -- Returns the string with null paddings removed.
   -- Use e.g. for Section_Name_T.


   --
   -- Exceptions:
   --

   Section_Not_Found : exception;
   --
   -- Raised when a section-accessing operation cannot find a section
   -- with the given name and therefore cannot return the requested
   -- section or section attribute.


   --
   -- Loading and using ELF files:
   --


   function Accepts (
      Name : in String;
      File : in IO.File_Type)
   return Boolean;
   --
   -- Whether the File seems to be an ELF file, as judged by the
   -- file-size and the initial file contents and possibly from the
   -- file Name. Rewinds the file before returning, so the next read
   -- operation will start from the head of the file. On call, the
   -- file can be at any position.


   procedure Get_Header (
      From   : in     IO.File_Type;
      Valid  :    out Boolean;
      Ident  :    out ELF_Ident_T;
      Header :    out ELF_Header_T);
   --
   -- Loads the start of the ELF header from the open ELF file for
   -- checking purposes.
   --
   -- From
   --    The ELF file.
   -- Valid
   --    Whether the Ident and Header seem valid (no error
   --    detected in them yet).
   -- Ident, Header
   --    The first two parts of the ELF header.
   --
   -- The operation rewinds the file before returning. On call, the
   -- file can be at any position.


   procedure Load (
      From : in     IO.File_Type;
      File :    out File_Ref);
   --
   -- Loads the ELF headers from the open ELF File, making it accessible
   -- for using the procedures and functions below.
   --
   -- The actual section-data is not yet loaded; it can be loaded
   -- later, section by section.


   function Is_Binary (
      Number : Section_Number_T;
      File   : File_T)
   return Boolean;
   --
   -- Whether the data of the specified section is binary data, as
   -- opposed to, say, a symbol table.


   procedure Load_Segment (
      Number  : in     Segment_Number_T;
      From    : in     IO.File_Type;
      Within  : in     File_Ref;
      Data    :    out Octets_T);
   --
   -- Loads the data for the specified segment and returns it as an
   -- octet vector. Data'Length should be equal to the length of the
   -- segment.


   procedure Load_Section (
      Number  : in     Section_Number_T;
      From    : in     IO.File_Type;
      Within  : in     File_Ref;
      Data    :    out Octets_T);
   --
   -- Loads the data for the specified section and returns it as an
   -- octet vector. Data'Length should be equal to the length of the
   -- section.


   procedure Put (
      File    : in File_Ref;
      Symbols : in Boolean := False;
      Strings : in Boolean := False);
   --
   -- Displays the ELF information on standard output.
   -- Output of the symbols and strings is optional.
   -- Section and segment data must be output separately; see the
   -- package ELF.Text.


   procedure Put_Binary_Data (
      From : in IO.File_Type;
      File : in File_Ref);
   --
   -- Displays the data in the binary sections and segments.


   procedure Load_And_Put (
      From    : in IO.File_Type;
      Data    : in Boolean := False;
      Symbols : in Boolean := False;
      Strings : in Boolean := False);
   --
   -- Reads the ELF data from the open ELF file and prints it out
   -- on standard output. The data are not retained in memory,
   -- at least not accessibly.


   procedure Load_And_Put (
      File_Name : in String;
      Data      : in Boolean := False;
      Symbols   : in Boolean := False;
      Strings   : in Boolean := False);
   --
   -- Opens the ELF file, reads its data, and prints it out
   -- on standard output. The data are not retained in memory,
   -- at least not accessibly.


   function Name_Of (
      Section : Section_Number_T;
      Within  : File_T)
   return String;
   --
   -- Returns the name of the section.


   function Kind_Of (
      Section : Section_Number_T;
      Within  : File_T)
   return Section_Data_Kind_T;
   --
   -- Returns the kind of data in the section.


   function Name_Of (
      Symbol  : Symbol_T;
      Within  : File_T)
   return String;
   --
   -- Returns the name of the symbol.


   function Name_Of (
      Stab    : Stabs.Line_T;
      Within  : File_T;
      Cut     : Boolean := False)
   return String;
   --
   -- Returns the name of the stab-symbol.
   -- If cut is true, the string is cut at the first ':', that separates,
   -- the symbol name from the additional info.


   function Number_Of (
      Section : String;
      Within  : File_T)
   return Section_Number_T;
   --
   -- Returns the index number of the section named "Section", or
   -- raises Section_Not_Found if there is no such section.


   function Length_Of (
      Section : Section_Number_T;
      Within  : File_T)
   return Natural;
   --
   -- Returns the length in octets of the section.


   function Section (
      Name     : String;
      Within   : File_T;
      Optional : Boolean := False)
   return Segment_Desc_T;
   --
   -- Describes the section of the given Name if it is present Within
   -- the given ELF file.
   -- Raises Section_Not_Found if there is no such section/segment and
   -- the Optional parameter has its default value of False. If Optional
   -- is set to True and there is no such section/segment, the function
   -- returns a descriptor with the Octets (length) component set to
   -- zero, to show the absence of the section/segment, but does not
   -- propagate an exception.


   function String_At (
      Offset  : Uword_T;
      Within  : File_T)
   return String;
   --
   -- Get a string from the string table.
   -- The string consists of the characters from the given Offset
   -- up to but not including a terminating nul character.


   procedure Close (File : in out File_Ref);
   --
   -- Discards the ELF information loaded into File, and closes
   -- any open files associated with File.
   -- This File must not be used with any subprograms defined
   -- above before it has been recreated with the Load
   -- procedure.


private


   type Section_IO_T is record
      Index : IO.Positive_Count;
   end record;
   --
   -- Index is the position in the Stream where the section
   -- starts (location of section header TBC).


   type Segment_IO_T is record
      Index : IO.Positive_Count;
   end record;
   --
   -- Index is the position in the Stream where the segment
   -- starts (location of section header TBC).


   type File_IO_T is record
      Index  : IO.Positive_Count;
      Stream : IO.Stream_Access;
   end record;
   --
   -- Index is the position in the Stream where the ELF file starts.


end Formats.ELF;
