-- Formats.COFF (decl)
--
-- Format definition for COFF executable and object files.
--
-- Author: Sami Saarinen and Niklas Holsti, Space Systems Finland, 2000
--
-- Reference: "DJGPP COFF Spec" by DJ Delorie,
-- http://www.delorie.com/djgpp/doc/coff/
--
-- A component of the Bound-T Worst-Case Execution Time Tool.
--
-------------------------------------------------------------------------------
-- Copyright (c) 2000 .. 2015 Tidorum Ltd
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
-- $Date: 2015/10/24 20:53:52 $
--
-- $Log: formats-coff.ads,v $
-- Revision 1.11  2015/10/24 20:53:52  niklas
-- Moved to free licence.
--
-- Revision 1.10  2013-11-16 19:47:01  niklas
-- BT_CH_0251: 21020/SHARC update: Options and Formats.COFF.
--
-- Revision 1.9  2007-03-06 20:06:38  niklas
-- Added Aux_Unknown symbols.
--
-- Revision 1.8  2007/02/27 22:10:34  Niklas
-- Corrected 'Read procedures for scalar types to use the 'Base
-- subtype of the scalar type and not to use Read operations inherited
-- from parent types (and so having "intrinsic" convention).
--
-- Revision 1.7  2005/10/26 14:07:19  niklas
-- Using Formats.Output instead of standard Output.
-- Changed Load_And_Put (File_Name) to absorb (not propagate) the
-- Name_Error and Use_Error exceptions.
--
-- Revision 1.6  2005/07/25 19:21:17  niklas
-- Added function String_At that takes a Strings_T, which can be
-- an "empty" string table, for example No_Strings, in which case
-- a null string is returned.
-- Changed the function String_At that takes a File_Ref to work in
-- this way, too.
--
-- Revision 1.5  2005/07/24 19:47:39  niklas
-- Added the Symbol_Kind_T Aux_Field for auxiliary records that report
-- the size of a bit-field symbol. Likewise added the Symbol_Nature_T
-- Normal_Field to indicate the nature of bit-field symbols.
--
-- Revision 1.4  2005/05/18 07:34:39  niklas
-- Corrected To_String to stop at the first NUL.
--
-- Revision 1.3  2005/03/24 19:47:50  niklas
-- Corrected the handling of derived types by considering the whole
-- sequence of derivations (Type_Derivation_T). We now classify a
-- pointer (to a pointer ..) to an array as Normal_Array, which means
-- that it can have Aux_Array entries.
--
-- Revision 1.2  2004/05/31 20:21:43  niklas
-- Changes to support Formats.COFF.Parsing and other corrections
-- and changes as follows.
-- Added an option for the length of a line-number entry, with a
-- choice of 16 or 32 bits for the line number component.
-- Added an option for the expected value of the Magic Number in
-- the COFF file header.
-- Added constants for special "section numbers".
-- Added T_Ushort to correct and complete the type list.
-- Added the Level parameter to the Derived_Type function so that
-- deeper derivation levels can be inspected.
-- Added Symbol_Nature_T to help COFF symbol parsing and choosing
-- the kind of the auxiliary symbol records.
-- Added Get_Auxiliary_Symbol to collect common code from the
-- operations Get_Symbol_Entry and Get_Symbol_Entry_2.
-- Added section numbers to optional trace output.
--
-- Revision 1.1  2004/04/24 18:08:34  niklas
-- First Tidorum version, as child of Formats.
--
--
-- <Log: coff.ads>
-- Revision 1.2  2001/06/11 12:28:08  holsti
-- Options for 16/32 bit endianness added.
-- Option for symbol-entry length added.
--
-- Revision 1.1  2000/07/02 18:41:57  holsti
-- First version.
--


with Interfaces;
with Ada.Streams.Stream_IO;


package Formats.COFF is
--
-- This package provides operations to access executable, binary
-- programs from a disk file in COFF (Common Object File Format) form.
--
-- The physical format of the COFF data is completely exposed here.
-- Only the way the data are read from the external file is
-- encapsulated in the package body.


   package IO renames Ada.Streams.Stream_IO;
   --
   -- The COFF file is read using stream i/o.
   -- We assume that Stream_Element is an octet.


   --
   -- Basic COFF Types and their Read operations.
   --
   -- The Read operations are overridden so that we can read
   -- files with different byte orders (endianness) (TBA).
   --


   subtype Byte_T  is Octet_T;
   subtype Bytes_T is Octets_T;
   --
   -- The COFF file is defined in terms of 8-bit items.


   subtype Byte_Index_T is Natural;
   --
   -- The index type of Bytes_T.


   type Ushort_T is mod 2**16;
   --
   -- Sometimes two bytes make up a 16-bit word.

   procedure Read_Ushort (
      Stream : access Ada.Streams.Root_Stream_Type'Class;
      Item   : out Ushort_T'Base);

   for Ushort_T'Read use Read_Ushort;


   type Ulong_T is mod 2**32;
   --
   -- Sometimes four bytes make up a 32-bit word.

   for Ulong_T'Size use 32;

   procedure Read_Ulong (
      Stream : access Ada.Streams.Root_Stream_Type'Class;
      Item   : out Ulong_T'Base);

   for Ulong_T'Read use Read_Ulong;


   type Short_T is range -32_768 .. 32_767;
   --
   -- Sometimes 16-bit words are signed.

   procedure Read_Short (
      Stream : access Ada.Streams.Root_Stream_Type'Class;
      Item   : out Short_T'Base);

   for Short_T'Read use Read_Short;

   --
   -- Image functions (inserted here to be primitive ops and
   -- so inherited by derived types):
   --


   function Image (Item : Byte_T) return String;
   --
   -- Hexadecimal form, 2 digits, no separators.


   function Hex_Image (
      Item  : Ushort_T;
      Width : Positive := 6)
   return String;
   --
   -- The Item formatted in hex as "0xNNNN", with zeros inserted
   -- after the x to bring the total length to Width (or more).


   function Hex_Image (
      Item  : Ulong_T;
      Width : Positive := 10)
   return String;
   --
   -- The Item formatted in hex as "0xNNNN", with zeros inserted
   -- after the x to bring the total length to Width (or more).


   --
   -- Derived COFF types:
   --


   type Time_T is new Ulong_T;
   --
   -- Time and date stamp.
   -- Same format as Unix "time_t".

   -- for Time_T'Read use Read_Ulong;
   --
   -- Time_T inherits the Read procedure for Ulong_T so we do not
   -- have to specify it (and also the above specification would not
   -- work because Ulong_T'Base is not the same as Time_T'Base).


   type File_Loc_T is new Ulong_T;
   --
   -- Pointer to a location in the COFF file.
   -- A byte offset relative to the start of the COFF File Header.

   -- for File_Loc_T'Read use Read_Ulong;
   --
   -- File_Loc_T inherits the Read procedure for Ulong_T so we do not
   -- have to specify it (and also the above specification would not
   -- work because Ulong_T'Base is not the same as File_Loc_T'Base).


   -- Flag bits for File Header:

   type File_Flags_T is record
      Relocation_Info  : Boolean;
      Executable       : Boolean;
      No_Line_Numbers  : Boolean;
      No_Local_Symbols : Boolean;
      Little_End_16    : Boolean;
      Little_End_32    : Boolean;
   end record;

   procedure Read_File_Flags (
      Stream : access Ada.Streams.Root_Stream_Type'Class;
      Item   : out File_Flags_T);

   for File_Flags_T'Read use Read_File_Flags;


   -- COFF File Header:

   type File_Header_T is record
      Magic_Number           : Ushort_T;
      Number_Of_Sections     : Ushort_T;
      Time_And_Date          : Time_T;
      Symbol_Table_Loc       : File_Loc_T;
      Number_Of_Symbols      : Ulong_T;
      Optional_Header_Length : Ushort_T;
      Flags                  : File_Flags_T;
   end record;

   File_Header_Length_C : constant := 20;
   --
   -- Number of (file) bytes in File_Header_T.


   -- Optional file header:

   type Optional_File_Header_T is record
      Magic_Number              : Ushort_T;
      Version_Stamp             : Ushort_T;
      Text_Size                 : Ulong_T;
      Data_Size                 : Ulong_T;
      Bss_Size                  : Ulong_T;
      Entry_Point               : Ulong_T;
      Text_Start                : Ulong_T;
      Data_Start                : Ulong_T;
   end record;

   Optional_Header_Length_C : constant := 28;
   --
   -- Number of (file) bytes in Optional_File_Header_T.



   -- COFF Section Header:


   type Section_Number_T is new Short_T;
   --
   -- The file contains "sections" of data.
   -- A section is a sequence of octets.
   -- Each section has a name, a number, and some other
   -- attributes.
   -- The sections are numbered starting from 1, but in some
   -- symbol records, the "section number" field is negative.

   -- for Section_Number_T'Read use Read_Short;
   --
   -- Section_Number_T inherits the Read procedure for Short_T so we do
   -- not have to specify it (and also the above specification would not
   -- work because Short_T'Base is not the same as Section_Number_T'Base).


   Debug_Section : constant Section_Number_T := -2;
   --
   -- A special "section number" that marks symbolic debugging symbols
   -- that have no relation to an actual section. This includes ".file",
   -- type definitions, tag names, and objects located in registers.


   Abs_Section : constant Section_Number_T := -1;
   --
   -- A special "section number" that marks a symbol that is given an
   -- absolute address, without relocation to a real segment.


   Undefined_Section : constant Section_Number_T := 0;
   --
   -- A special "section number" that marks an external relocatable
   -- symbol that is not defined in the current file.
   -- TBD meaning/existence for fully linked and relocated files.


   subtype Section_Name_T is String(1..8);
   -- Zero-padded section name.

   type Physical_Address_T is new Ulong_T;
   type Virtual_Address_T  is new Ulong_T;
   type Symbol_Index_T     is new Ulong_T;

   -- for Physical_Address_T'Read use Read_Ulong;
   -- for Virtual_Address_T'Read use Read_Ulong;
   -- for Symbol_Index_T'Read use Read_Ulong;
   --
   -- These types inherit the Read procedure for Ulong_T so we do not
   -- have to specify it (and also the above specifications would not
   -- work because Ulong_T'Base is not the base subtype for these
   -- derived types).


   type Section_Flags_T is record
      Regular   : Boolean;
      Dummy     : Boolean;
      No_Load   : Boolean;
      Group     : Boolean;
      Pad       : Boolean;
      Copy      : Boolean;
      Only_Text : Boolean;
      Only_Data : Boolean;
      Bss_Data  : Boolean;
      Comment   : Boolean;
      Overlay   : Boolean;
      Lib       : Boolean;
   end record;

   procedure Read_Section_Flags (
      Stream : access Ada.Streams.Root_Stream_Type'Class;
      Item   : out Section_Flags_T);

   for Section_Flags_T'Read use Read_Section_Flags;


   type Section_Header_T is record
      Name                : Section_Name_T;
      Physical_Address    : Physical_Address_T;
      Virtual_Address     : Virtual_Address_T;
      Length              : Ulong_T;
      Data_Loc            : File_Loc_T;
      Relocation_Loc      : File_Loc_T;
      Line_Number_Loc     : File_Loc_T;
      Relocation_Entries  : Ushort_T;
      Line_Number_Entries : Ushort_T;
      Flags               : Section_Flags_T;
   end record;


   Section_Header_Length_C : constant := 40;
   -- Number of (file) bytes in section_header_t.

   Relocation_Length_C     : constant := 10;
   -- Number of bytes in Relocation entry


   -- COFF relocation record:

   type Relocation_T is record
      Virtual_Address : Virtual_Address_T;
      Symbol_Index    : Symbol_Index_T;
      Relocation_Type : Ushort_T;
   end record;

   type Relocations_T is array (Positive range <>) of Relocation_T;
   type Relocations_Ref is access Relocations_T;


   -- COFF Line number declaration:

   type Line_Number_T is record
      Line_Address : Ulong_T;
      Line_Number  : ULong_T;
   end record;
   --
   -- Line_Address contains a symbol table index if Line_Number = 0
   -- and a physical address otherwise.
   --
   -- Although the Line_Number component is defined as a Ulong_T, in the
   -- COFF file it can consist of 16 or 32 bits, depending on the option
   -- Formats.COFF.Opt.Line_Number_Length. The operation Read_Line_Number
   -- works accordingly.

   procedure Read_Line_Number (
      Stream : access Ada.Streams.Root_Stream_Type'Class;
      Item   : out    Line_Number_T);

   for Line_Number_T'Read use Read_Line_Number;


   function Line_Number_Length return IO.Count;
   --
   -- The number of bytes in Line Number entry depends on the
   -- option Formats.COFF.Opt.Line_Number_Length.


   type Line_Numbers_T is array (Positive range <>) of Line_Number_T;
   type Line_Numbers_Ref is access Line_Numbers_T;


   -- COFF Symbol entry:

   subtype Symbol_Name_T is String (1..8);

   subtype File_Name_T is String (1..14);

   type Dimensions_T is array (0..3) of Ushort_T;


   type Symbol_Type_T is new Ushort_T;
   --
   -- Information about the type of the variable identified
   -- by the symbol.

   -- for Symbol_Type_T'Read use Read_Ushort;
   --
   -- Symbol_Type_T inherits the Read procedure for Ushort_T so we do
   -- not have to specify it (and also the above specification would not
   -- work because Ushort_T'Base is not the same as Symbol_Type_T'Base).


   -- Fundamental types in symbol_type field, encoded in
   -- the lower nybble:

   type Fundamental_Type_T is (
      T_Null,
      T_Arg,
      T_Char,
      T_Short,
      T_Int,
      T_Long,
      T_Float,
      T_Double,
      T_Struct,
      T_Union,
      T_Enum,
      T_Moe,
      T_Uchar,
      T_Ushort,
      T_Uint,
      T_Ulong);

   function Fundamental_Type (Item : Symbol_Type_T)
   return Fundamental_Type_T;
   --
   -- The fundamental type contained in Item.


   -- Derived types in symbol_type field
   -- (in D1 field, bits 4-5 in symbol_type):

   type Derived_Type_T is (
      DT_Non,
      DT_Ptr,
      DT_Fcn,
      DT_Ary);


   subtype Real_Derived_Type_T is Derived_Type_T range DT_Ptr .. DT_Ary;
   --
   -- The real derived types (that is, omitting DT_Non).


   subtype Derivation_Level_T is Positive range 1 .. 6;
   --
   -- There are at most 6 derivation levels.


   type Type_Derivation_T is
      array (Derivation_Level_T range <>) of Real_Derived_Type_T;
   --
   -- The type derivation sequence for a symbol's type.
   -- For example, a pointer to an array of ints will have the derivation
   -- sequence (1 => DT_Ptr, 2 => DT_Ary) and the fundamental type T_Int.


   function Derived_Type (
      Item  : Symbol_Type_T;
      Level : Derivation_Level_T := 1)
   return Derived_Type_T;
   --
   -- The derived type contained in Item, at the given Level.
   -- Level = 1 means the first (outermost) derived type, Level = 2
   -- is the second (next inner) derived type, and so on.


   function Derivation (Item : Symbol_Type_T)
   return Type_Derivation_T;
   --
   -- The type derivation sequence encoded in the Item, possibly null.


   type Storage_Class_Code_T is new Byte_T;
   --
   -- The storage-class (static, stack, ...) of the variable
   -- identified by a symbol, as it appears in the file.


   -- Storage classes:

   type Storage_Class_T is (
      C_Efcn,
      C_Null,
      C_Auto,
      C_Ext,
      C_Stat,
      C_Reg,
      C_Extdef,
      C_Label,
      C_Ulabel,
      C_Mos,
      C_Arg,
      C_Strtag,
      C_Mou,
      C_Untag,
      C_Tpdef,
      C_Ustatic,
      C_Entag,
      C_Moe,
      C_Regparm,
      C_Field,
      C_Block,
      C_Fcn,
      C_Eos,
      C_File,
      C_Line,
      C_Alias,
      C_Hidden,
      C_Shadow,
      C_Weakext,
      C_Unknown);

   Storage_Class : constant array (Storage_Class_Code_T)
                   of Storage_Class_T := (
      16#FF# => C_Efcn,
          0  => C_Null,
          1  => C_Auto,
          2  => C_Ext,
          3  => C_Stat,
          4  => C_Reg,
          5  => C_Extdef,
          6  => C_Label,
          7  => C_Ulabel,
          8  => C_Mos,
          9  => C_Arg,
         10  => C_Strtag,
         11  => C_Mou,
         12  => C_Untag,
         13  => C_Tpdef,
         14  => C_Ustatic,
         15  => C_Entag,
         16  => C_Moe,
         17  => C_Regparm,
         18  => C_Field,
        100  => C_Block,
        101  => C_Fcn,
        102  => C_Eos,
        103  => C_File,
        104  => C_Line,
        105  => C_Alias,
        106  => C_Hidden,
        107  => C_Shadow,
        108  => C_Weakext,
      others => C_Unknown);


   -- Kinds of symbol records:

   type Symbol_Kind_T is (
      Symbol_Entry,
      Symbol_Entry_2,
      Aux_File_Name,
      Aux_Section,
      Aux_Tag_Name,
      Aux_EOS,
      Aux_Field,
      Aux_Function,
      Aux_Array,
      Aux_EOB,
      Aux_EF,
      Aux_BB,
      Aux_BF,
      Aux_Names,
      Aux_Unknown);
   --
   -- All the kinds of symbol records that can occur.
   -- These are divided into two classes: primary symbol records,
   -- which are Symbol_Entry and Symbol_Entry_2, and auxiliary
   -- symbol records which are all the rest. An auxiliary symbol
   -- record is always associated with (owned by) a primary symbol
   -- record. The auxiliary symbol records for one primary symbol
   -- come immediately after the primary symbol in the symbol table.
   --
   -- The Aux_Unkown kind represents some unknown form of auxiliary
   -- symbol.


   subtype Primary_Symbol_T is
      Symbol_Kind_T range Symbol_Entry .. Symbol_Entry_2;
   --
   -- The primary symbol records are of kind Symbol_Entry, when the name
   -- is at most 8 characters long and contained in the record itself, or
   -- Symbol_Entry_2 when the name is longer and is contained in the string
   -- table at the offset given in the symbol record.


   subtype Auxiliary_Symbol_T is
      Symbol_Kind_T range Aux_File_Name .. Aux_Unknown;
   --
   -- All the other kinds of symbol records are auxiliary records that are
   -- owned by the preceding primary symbol record and supply more data
   -- about the entity that the primary symbol record describes.


   -- The symbol record itself:

   type Symbol_T (Kind : Symbol_Kind_T) is record
      case Kind is

      when Symbol_Entry =>
         S1_Name                 : Symbol_Name_T;
         S1_Value                : Ulong_T;
         S1_Section_Number       : Section_Number_T;
         S1_Symbol_Type          : Symbol_Type_T;
         S1_Storage_Class        : Storage_Class_Code_T;
         S1_Number_Aux           : Byte_T;

      when Symbol_Entry_2 =>
         S2_Zeros1               : Ulong_T;
         S2_Offset               : Ulong_T;
         S2_Value                : Ulong_T;
         S2_Section_Number       : Section_Number_T;
         S2_Symbol_Type          : Symbol_Type_T;
         S2_Storage_Class        : Storage_Class_Code_T;
         S2_Number_Aux           : Byte_T;

      when Aux_File_Name =>
         File_Name               : File_Name_T;

      when Aux_Section =>
         Sec_Length              : Ulong_T;
         Sec_Number_Relocation   : Ushort_T;
         Sec_Number_Line_Number  : Ushort_T;

      when Aux_Tag_Name =>
         Tag_Zeros1              : Ulong_T;
         Tag_Zeros2              : Ushort_T;
         Tag_Size                : Ushort_T;
         Tag_Zeros3              : Ulong_T;
         Tag_Next_Entry          : Symbol_Index_T;

      when Aux_EOS =>
         EOS_Tag_Index           : Symbol_Index_T;
         EOS_Zeros1              : Ushort_T;
         EOS_Size                : Ushort_T;

      when Aux_Field =>
         Field_Tag_Index         : Symbol_Index_T;
         Field_Size_Bits         : Ulong_T;

      when Aux_Function =>
         Func_Tag_Index          : Symbol_Index_T;
         Func_Size               : Ulong_T;
         Func_Line_Pointer       : File_Loc_T;
         Func_Next_Entry         : Symbol_Index_T;
         Func_Trans_Vector_Index : Ushort_T;

      when Aux_Array =>
         Array_Tag_Index         : Symbol_Index_T;
         Array_Line_Number       : Ushort_T;
         Array_Size              : Ushort_T;
         Array_Dimensions        : Dimensions_T;

      when Aux_EOB =>
         EOB_Zeros1              : Ulong_T;
         EOB_Line_Number         : Ushort_T;

      when Aux_EF =>
         EF_Zeros1               : Ulong_T;
         EF_Line_Number          : Ushort_T;

      when Aux_BB =>
         BB_Zeros1               : Ulong_T;
         BB_Line_Number          : Ushort_T;
         BB_Zeros2               : Ulong_T;
         BB_Zeros3               : Ushort_T;
         BB_Next_Entry           : Symbol_Index_T;

      when Aux_BF =>
         BF_Zeros1               : Ulong_T;
         BF_Line_Number          : Ushort_T;
         BF_Zeros2               : Ulong_T;
         BF_Zeros3               : Ushort_T;
         BF_Next_Entry           : Symbol_Index_T;

      when Aux_Names =>
         Names_Tag_Index         : Symbol_Index_T;
         Names_Zeros1            : Ushort_T;
         Names_Size              : Ushort_T;

      when Aux_Unknown =>
         null;

      end case;

   end record;


   -- Nature (normal, special, auxiliary) of a symbol:


   type Symbol_Nature_T is (
      Normal,
      Normal_Label,
      Normal_Function,
      Normal_Array,
      Normal_Struct,
      Normal_Union,
      Normal_Enum,
      Normal_Tag,
      Normal_Field,
      File_Name,
      Begin_Function,
      End_Function,
      Begin_Block,
      End_Block,
      End_Struct,
      Text_Section,
      Data_Section,
      Bss_Section,
      Other_Section,
      Target,
      Text_End,
      Data_End,
      Bss_End,
      Auxiliary);
   --
   -- The nature of a symbol entry, where we have Normal symbols, a set
   -- of special symbols, and the auxiliary symbols. The special symbols
   -- are distinguished by a special symbolic Name string and possibly
   -- other special attributes. Most of the special symbols will own one
   -- or more auxiliary symbols. Some of the normal symbols can also own
   -- auxiliary symbols.
   --
   -- Normal
   --    A normal symbol for the symbol-table, but not a type, label,
   --    subprogram, structure, union or enumeration.
   --    Owns no auxiliary entries.
   --
   -- Normal_Label
   --    A normal symbol for a statement label.
   --    Owns no auxiliary entries.
   --
   -- Normal_Function
   --    A normal subprogram symbol, for the symbol-table.
   --    Usually owns an Aux_Function entry.
   --    Followed by Begin_Function, local symbols, and End_Function.
   --
   -- Normal_Array
   --    A normal symbol that denotes an array.
   --    Usually owns an Aux_Array entry.
   --
   -- Normal_Struct, Normal_Union, Normal_Enum
   --    A normal symbol, related a structure, union or enumeration.
   --    Usually owns an Aux_Names entry.
   --
   -- Normal_Tag
   --    A tag name.
   --    Usually owns an Aux_Tag_Name entry.
   --
   -- Normal_Field
   --    A bit field in a record/struct.
   --    Usually owns an Aux_Field entry.
   --
   -- File_Name = ".file"
   --    Marks the start of the symbols for a new source file.
   --    Usually owns an Aux_File_Name entry.
   --
   -- Begin_Function = ".bf"
   --    Marks the start of the symbols for a new subprogram (function).
   --    Usually owns an Aux_BF entry.
   --
   -- End_Function = ".ef"
   --    Marks the end of the symbols for a subprogram.
   --    Usually owns an Aux_EF entry.
   --
   -- Begin_Block = ".bb"
   --    Marks the start of a block within a subprogram.
   --    Usually owns an Aux_BB entry.
   --
   -- End_Block = ".eb"
   --    Marks the end of a block within a subprogram.
   --    Usually owns an Aux_EB entry.
   --
   -- End_Struct = ".eos"
   --    Marks the end of the symbols that define the members of a
   --    structure, union or enumeration.
   --
   -- Text_Section = ".text"
   --    Gives the address of the ".text" section.
   --    Usually owns an Aux_Section entry.
   --
   -- Data_Section = ".data"
   --    Gives the address of the ".data" section.
   --    Usually owns an Aux_Section entry.
   --
   -- Bss_Section = ".bss"
   --    Gives the address of the ".bss" section.
   --    Usually owns an Aux_Section entry.
   --
   -- Other_Section
   --    Gives address and length of some other section.
   --    Usually owns an Aux_Section entry.
   --
   -- Target = ".target"
   --    Pointer to the structure or union returned by a function.
   --    Auxiliary entries TBD.
   --
   -- Text_End = "etext"
   --    Gives the next available address after the ".text" section.
   --
   -- Data_End = "edata"
   --    Gives the next available address after the ".text" section.
   --
   -- Bss_End = "end"
   --    Gives the next available address after the ".text" section.
   --
   -- Auxiliary
   --    An auxiliary symbol of some kind.
   --
   -- Some of the above are TBC and may not exist in all COFF files.
   --
   -- Some COFF definitions include the dummy names for unnamed
   -- structures, unions or enumerations as special symbols, with the
   -- form ".Xfake" where X is a decimal number. We consider them normal
   -- symbols with synthetic, compiler-generated names.


   subtype Normal_Nature_T is Symbol_Nature_T range Normal .. Normal_Field;
   --
   -- The "normal" natures.


   subtype Special_Nature_T is Symbol_Nature_T range File_Name .. Bss_End;
   --
   -- The "special" natures.


   subtype Section_Nature_T is
      Special_Nature_T range Text_Section .. Other_Section;
   --
   -- The special natures for section information.


   subtype Owning_Nature_T is
      Symbol_Nature_T range Normal_Function .. Other_Section;
   --
   -- The symbols that can own specific kinds of auxiliary symbol records.


   function Nature (Symbol : Symbol_T) return Symbol_Nature_T;
   --
   -- The nature of the given symbol record.


   --
   -- Types for describing the COFF object internally:
   --


   --
   -- A COFF section:
   --

   type Section_IO_T is private;


   type Section_T is record
      Header       : Section_Header_T;
      Relocations  : Relocations_Ref;
      Line_Numbers : Line_Numbers_Ref;
      IO           : Section_IO_T;
   end record;
   --
   -- The components have meanings obvious from their names,
   -- except perhaps for:
   --
   -- IO      Stuff needed by the access routines.
   --
   -- The section data are not represented here. It is expected
   -- that they will be loaded into an data structure that represents
   -- the memory content, e.g. an instance of Memories.Content_T.


   type Sections_T is array (Section_Number_T range <>) of Section_T;
   --
   -- Sections are numbered 1 .. N.

   type Sections_Ref is access Sections_T;

   type Symbol_Ref is access Symbol_T;
   type Symbols_T is array (Natural range <>) of Symbol_Ref;
   type Symbols_Ref is access Symbols_T;


   -- The (extended) string area:

   type Strings_Ref is access String;

   type Strings_T is record
      Size   : Ulong_T;
      Buffer : Strings_Ref;
   end record;

   procedure Read_Strings (
      Stream : access Ada.Streams.Root_Stream_Type'Class;
      Item   : out Strings_T);

   for Strings_T'Read use Read_Strings;


   No_Strings : constant Strings_T := (
      Size   => 0,
      Buffer => null);
   --
   -- A special value of String_T that indicates the absence of a
   -- string table.


   function String_At (
      Offset : Ulong_T;
      Within : Strings_T)
   return String;
   --
   -- The string at a given Offset, Within the string table.
   --
   -- The string consists of the characters from the given Offset
   -- (less 3) up to but not including a terminating nul character.
   --
   -- If Within is No_Strings (size zero or buffer null), a null
   -- string is returned.


   --
   -- The whole COFF file:
   --

   type File_IO_T is limited private;


   type File_T is record
      Header   : File_Header_T;
      Optional : Optional_File_Header_T;
      Sections : Sections_Ref;
      Symbols  : Symbols_Ref;
      Strings  : Strings_T;
      IO       : File_IO_T;
   end record;
   --
   -- The IO component encapsulates the way the file is
   -- accessed in the current implementation.

   type File_Ref is access File_T;


   --
   -- Further Image functions:
   --


   function Image (Item : Section_IO_T) return String;

   function Image (Item : File_IO_T) return String;

   function To_String (Name : in String) return String;
   --
   -- Return the string up to but not including the first NUL
   -- character, if any.
   -- Use e.g. for Section_Name_T.


   --
   -- Exceptions:
   --

   Section_Not_Found : exception;
   --
   -- The Number_Of function could not find a section with
   -- the given name.


   --
   -- Loading and using COFF files:
   --


   function Accepts (
      Name : in String;
      File : in IO.File_Type)
   return Boolean;
   --
   -- Whether the File seems to be an COFF file, as judged by the
   -- file-size and the initial file contents and possibly from the
   -- file Name. Rewinds the file before returning, so the next read
   -- operation will start from the head of the file. On call, the
   -- file can be at any position.


   procedure Load (
      From : in     IO.File_Type;
      File :    out File_Ref);
   --
   -- Loads the COFF headers from the open COFF file, making it
   -- accessible for using the procedures and functions below.
   -- The actual section-data is not yet loaded; it can be loaded
   -- loaded later, section by section.


   procedure Load (
      File_Name : in     String;
      File      :    out File_Ref);
   --
   -- Opens the COFF file and loads its headers, making it
   -- accessible for using the procedures and functions below.
   -- The actual section-data is not yet loaded; it can be loaded
   -- loaded later, section by section.


   procedure Load_Section (
      Section : in     Section_T;
      From    : in     IO.File_Type;
      Within  : in     File_Ref;
      Data    :    out Octets_T);
   --
   -- Loads the data for the specified section and returns it as
   -- a byte vector. Data'Length should be equal to the length of
   -- the section.


   procedure Put (
      File    : in File_Ref;
      Symbols : in Boolean := False;
      Strings : in Boolean := False);
   --
   -- Displays the COFF information on standard output.
   -- Output of the symbols and strings is optional.
   -- Section data must be output separately; see the
   -- package COFF.Text.


   procedure Load_And_Put (
      From    : in IO.File_Type;
      Data    : in Boolean := False;
      Symbols : in Boolean := False;
      Strings : in Boolean := False);
   --
   -- Reads the COFF data from the open COFF file and prints it
   -- out on standard output. The data are not retained in memory,
   -- at least not accessibly.


   procedure Load_And_Put (
      File_Name : in String;
      Data      : in Boolean := False;
      Symbols   : in Boolean := False;
      Strings   : in Boolean := False);
   --
   -- Opens the COFF file, reads its data, and prints it out
   -- on standard output. The data are not retained in memory,
   -- at least not accessibly.
   --
   -- If the file cannot be opened (wrong name or wrong access
   -- permissions), the error is reported but the exception is
   -- not propagated.


   function Name_Of (
      Section : Section_Number_T;
      Within  : File_T)
   return String;
   --
   -- Returns the name of the section, without null padding.


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


   function String_At (
      Offset  : Ulong_T;
      Within  : File_Ref)
   return String;
   --
   -- The string at a given Offset in the string table Within the file.


   procedure Close (File : in out File_Ref);
   --
   -- Discards the COFF information loaded into File, and closes
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


   type File_IO_T is record
      Index  : IO.Positive_Count;
      Stream : IO.Stream_Access;
   end record;
   --
   -- Index is the position in the Stream where the COFF file starts.


end Formats.COFF;
