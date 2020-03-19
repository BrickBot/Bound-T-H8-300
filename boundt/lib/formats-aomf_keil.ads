-- Formats.AOMF_Keil (decl)
--
-- The Intel Absolute Object Module format (AOMF subset of OMF-51)
-- with extensions specific for output from the compilers by Keil GmbH.
-- AOMF with extensions is called EAOMF.
--
-- Author: Niklas Holsti, Tidorum Ltd, 2004.
-- Partly after the version by Mikko Ala-Fossi, Space Systems Finland, 1999.
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
-- $Date: 2015/10/24 20:53:51 $
--
-- $Log: formats-aomf_keil.ads,v $
-- Revision 1.6  2015/10/24 20:53:51  niklas
-- Moved to free licence.
--
-- Revision 1.5  2007-08-31 08:55:53  niklas
-- Added the constant Untyped_Index.
--
-- Revision 1.4  2007/01/25 21:25:35  niklas
-- BT-CH-0043.
--
-- Revision 1.3  2005/01/01 20:23:03  niklas
-- Added Kind_Code to Record_Info_T, for error messages.
--
-- Revision 1.2  2004/10/10 10:12:13  niklas
-- Added Source_Browse_TBC to stand for record types hex 60 .. 7F, but
-- the content of these records is not known and must be skipped.
-- Added subtypes etc. to support parsing.
-- Added Predefined and Undefined to Type_T for uniform handling of types.
-- Added function Accepts to auto-detect E-AOMF files. To help, made
-- Check_Sum work silently even in case of error.
-- Added Type_Table_T to store the defined types.
--
-- Revision 1.1  2004/10/03 16:00:24  niklas
-- First version.
--


with Unbounded_Vectors;


package Formats.AOMF_Keil is

   -- The references for this package are:
   --
   -- 1. "External Product Specification for the MCS-51 Object
   --     Module Format", V5.0, Sept 05, 1982, by Intel Corp.
   --
   -- 2. "Additions to the 8051 Object Module Format (OMF-51)",
   --    05/07/2000, Keil Elektronik GmbH.
   --
   -- An AOMF file is a sequence of records of several types.
   -- Each record begins with an octet giving the record type.
   -- The type octet is followed by a 16-bit record length that
   -- gives the number of octets in the record, not including the
   -- three octets giving the type and record length. The record
   -- ends with a check-sum octet that contains the 2's complement
   -- of the sum (mod 256) of all the other octets in the record.
   -- Thus, the sum (mod 256) of all the octets in the record,
   -- including the check-sum octet, is zero.
   --
   -- The AOMF record types (as currently known) are the following,
   -- where the Keil extensions are marked with a '*', and the record
   -- types that should be ignored (in AOMF) are marked with a "-":
   --
   --  Type (hex)  Record
   --  ----------  ------
   --   02         Module Header
   --   04         Module End
   --   06         Content (loadable memory image)
   --   10         Scope Definition
   --   12         Debug Items
   --   0E   -     Segment Definition
   --   16   -     Public Definition
   --   18   -     External Definition
   --   20   *     Type Definition
   --   22   *     Extended Debug Items
   --   24   *     Source Name
   --   2E   *     BL51 Bank Head
   --
   --
   --
   -- Keil GmbH has announced (ref 2) that the record type codes in the
   -- range hex 60 .. hex 7F will be used for "AMAKE and Source Browse"
   -- features and should be skipped by loaders.
   --
   -- Keil GmbH has also slightly modified and extended the basic AOMF
   -- record structures in an upward-compatible way by defining additional
   -- code values for existing encodings or by storing new information in
   -- octets that were unused.
   --
   -- Non-absolute (relocatable) OMF files can contain also other types
   -- of records, but they do not concern us because we read only
   -- absolute (relocated, linked) files.
   --
   -- The sequence of records in an AOMF file obeys the following overall
   -- syntax:
   --
   --    <AOMF file> = <module>
   --
   --    <module> = <header> <data or debug>* <Module End>
   --
   --    <header = [ <BL51 Bank Head> ] <Module Header>
   --
   --    <data or debug> = <Content> | <debug>
   --
   --    <debug> = <scope>
   --            | <Type Definition>
   --            | <Debug Items>
   --            | <Extended Debug Items>
   --
   --    <scope> = <Scope Definition> [ <Source Name> ]
   --
   -- The Scope Definition records occur in pairs: Begin_X and End_X.
   -- Each such pair brackets the Type Definitions, (Extended) Debug
   -- Items and Content for a part of the program (X = module, procedure
   -- or do-block). Such scopes can be nested. Typically there is a
   -- sequence of module scopes, each of which may contain procedure
   -- scopes, which may contain do-block scopes, which may contain nested
   -- do-block scopes. A module-scope typically corresponds to one 'C'
   -- language source file.
   --
   -- The syntax does not mention the records of type Segment Definition,
   -- Public Definition or External Definition, because they will be
   -- ignored where ever they occur.
   --
   -- The syntax is used and checked by the AOMF parsing functions
   -- in the child package Formats.AOMF.Parsing.


   Deallocate : Boolean := True;
   --
   -- Whether to use Unchecked_Deallocation to release unused heap
   -- memory. Declared here to make it usable in instances of
   -- Unbounded_Vectors; meant to be accessed via Formats.AOMF_Keil.Opt.


   --
   --    Basic AOMF types
   --


   type Word16_T is new Unsigned_16_T;
   --
   -- A 16-bit word (little-endian).


   procedure Read_Word16 (
      Stream : access Ada.Streams.Root_Stream_Type'Class;
      Item   :    out Word16_T);

   for Word16_T'Read use Read_Word16;


   type Name_Ref is access String;
   --
   -- A reference to a heap-allocated string giving the name of a
   -- file, module, procedure, variable, ...


   procedure Read_Name_Ref (
      Stream : access Ada.Streams.Root_Stream_Type'Class;
      Item   :    out Name_Ref);
   --
   -- The name is encoded as an octet giving the length and a string
   -- of octet characters of exactly this length.

   for Name_Ref'Read use Read_Name_Ref;


   type Register_Bank_T is new Octet_T range 0 .. 3;
   --
   -- The number of a register bank. The 8051 has four banks of
   -- eight registers (R0 .. R7), located in the first 32 octets of
   -- the (directly addressable) internal data memory.


   type Code_Bank_T is new Octet_T range 0 .. 31;
   --
   -- The number of a code bank.
   -- The 8051 is not architecturally banked, but some devices/boards
   -- can provide code banks and some compilers support this.


   --
   -- Storage (memory) kinds
   --


   type Storage_Kind_T  is (
      Unknown,
      Code,
      Code_Bank,
      XData,
      Data,
      IData,
      Bit,
      Large_Stack,
      Compact_Stack,
      Small_Stack,
      Number);
   --
   -- The kind of memory area that a symbol resides in or that a
   -- segment represents.
   --
   -- Unknown
   --    Unknown or invalid value.
   -- Code
   --    A code entity: subprogram, label.
   -- Code_Bank
   --    A code entity in a specific code bank. The bank number must
   --    be given separately.
   -- XData
   --    An entity in the external data memory.
   -- Data
   --    An entity in the internal data memory, directly addressable part.
   -- IData
   --    An entity in the internal data memory, indirectly addressable part.
   -- Bit
   --    A bit in the bit-addressable part of the internal data memory.
   -- Large_Stack
   --    An entity in the stack of a reentrant procedure using the
   --    "large" memory model.
   -- Compact_Stack
   --    An entity in the stack of a reentrant procedure using the
   --    "compact" memory model.
   -- Small_Stack
   --    An entity in the stack of a reentrant procedure using the
   --    "small" memory model.
   -- Number
   --    A constant number (not addressable, not in a memory).
   --
   -- Values of this type occur in several record types but are encoded
   -- in different ways depending on the context.


   type Storage_T (Kind : Storage_Kind_T := Unknown) is record
      case Kind is
      when Code_Bank => Code_Bank : Code_Bank_T;
      when others    => null;
      end case;
   end record;
   --
   -- Defines the kind of storage for a segment or symbol, also
   -- identifying the specific code bank for Code_Bank storage.


   procedure Read_Storage (
      Stream : access Ada.Streams.Root_Stream_Type'Class;
      Item   :    out Storage_T);
   --
   -- Reads a Storage when encoded as one whole octet.

   for Storage_T'Read use Read_Storage;


   --    Symbol Info


   type Symbol_Info_T is record
      Variable        : Boolean;
      Usage           : Storage_T;
      Indirect        : Boolean;
      Single_Reg_Bank : Boolean;
      Register_Bank   : Register_Bank_T;
   end record;
   --
   -- Symbol Information, encoded by one octet.
   --
   -- Variable
   --    Whether the public symbol is a variable (not a procedure).
   -- Usage
   --    The usage of the symbol (the kind of storage it lies in).
   -- Indirect
   --    Whether the procedure is indirectly callable.
   --    Meaningful only for public procedure symbols.
   -- Single_Reg_Bank
   --    Whether the procedure is restricted to use only the given
   --    Register_Bank. Meaningful only for public procedure symbols.
   --    If Single_Reg_Bank is False, the procedure can use all
   --    register banks.
   -- Register_Bank
   --    The register bank which is used in the procedure.
   --    Meaningful only for public procedure symbols and only if
   --    Single_Reg_Bank is True.
   --
   -- Symbol_Info values occur in a couple of different record types
   -- but are encoded in a uniform way.


   procedure Read_Symbol_Info (
      Stream : access Ada.Streams.Root_Stream_Type'Class;
      Item   :    out Symbol_Info_T);

   for Symbol_Info_T'Read use Read_Symbol_Info;


   --    Segment Info


   type Segment_Info_T is record
      Empty         : Boolean;
      Overlayable   : Boolean;
      Register_Bank : Register_Bank_T;
      Segment_Type  : Storage_T;
   end record;
   --
   -- Information about a segment.


   procedure Read_Segment_Info (
      Stream : access Ada.Streams.Root_Stream_Type'Class;
      Item   :    out Segment_Info_T);

   for Segment_Info_T'Read use Read_Segment_Info;


   --
   --    AOMF record types
   --
   -- We define the record types approximately in the order in which
   -- they occur in the syntax given above.


   type Record_Kind_T is (
      Source_Browse_TBC,
      BL51_Bank_Head,
      Module_Header,
      Module_End,
      Content,
      Scope_Definition,
      Source_Name,
      Type_Definition,
      Debug_Items,
      Extended_Debug_Items,
      Unknown);
   --
   -- The kinds (types) of AOMF records that we use.
   --
   -- The Unknown literal stands for really unknown types as well
   -- as for types that we know of but do not use here (in reading
   -- an AOMF file).
   --
   -- The Source_Browse_TBC literal stands for a record type that we do
   -- not yet understand or process, but that may be the first record in
   -- the file and therefore must be known to the Accepts function.


   function Record_Kind (Encoded : Octet_T) return Record_Kind_T;
   --
   -- Decodes the octet that encodes a Record Kind.


   procedure Read_Record_Kind (
      Stream : access Ada.Streams.Root_Stream_Type'Class;
      Item   :    out Record_Kind_T);
   --
   -- The record type is encoded as one octet, included in the
   -- record check-sum.

   for Record_Kind_T'Read use Read_Record_Kind;


   --    BL51 Bank Head   TBA


   --    Module Header


   type Module_Header_T is record
      Name       : Name_Ref;
      Translator : Octet_T;
      Unused1    : Octet_T;
   end record;
   --
   -- A Module Header record.
   --
   -- Translator
   --    ID code for the translator (assembler, compiler) that
   --    generated the module.


   --    Module End


   type Register_Banks_T is array (Register_Bank_T) of Boolean;
   --
   -- A set of register banks.
   -- Members of the set are marked by True values.


   procedure Read_Register_Banks (
      Stream : access Ada.Streams.Root_Stream_Type'Class;
      Item   :    out Register_Banks_T);
   --
   -- The register-bank set is encoded by one octet where bit N
   -- is set if register bank N is included (N = 0 .. 3).

   for Register_Banks_T'Read use Read_Register_Banks;


   type Module_End_T is record
      Name           : Name_Ref;
      Unused1        : Word16_T;
      Register_Banks : Register_Banks_T;
      Unused2        : Octet_T;
   end record;
   --
   -- A Module End record.
   --
   -- Register_Banks
   --    The register banks used in the module.
   --    TBD meaning for the single module in an AOMF file.


   --    Content


   type Data_T is new Octets_T;
   --
   -- A sequence of code/data octets, included in the check-sum, and
   -- forming part of the program's memory image.


   type Data_Ref is access Data_T;
   --
   -- Reference to heap-allocated Data.


   type Content_T is record
      Seg_ID : Octet_T;
      Offset : Word16_T;
      Data   : Data_Ref;
   end record;
   --
   -- A Content record, holding Data to be loaded into the memory
   -- image starting at a given Offset (an absolute address for AOMF).
   --
   -- For basic AOMF the Seg_ID will be zero, which means that the
   -- Offset is an absolute address in the 64 kB Code space.
   --
   -- For EAOMF with code banking a zero Seg_ID means that the Data
   -- go into the "common" (non-banked) code area at Offset.
   -- Seg_ID values 16#10# .. 16#2F# mean that the Data go into
   -- bank number Seg_ID - 16#10#.


   procedure Read_Content (
      Stream : access Ada.Streams.Root_Stream_Type'Class;
      Length : in     Word16_T;
      Item   :    out Content_T);
   --
   -- Reads the Content record, including its Data, when the Length of
   -- the entire Content record is given.
   -- The Stream is left at the check-sum octet (which is not read).


   --    Scope Definition


   type Block_Type_T is (
      Unknown,
      Begin_Module,
      Begin_Procedure,
      Begin_Do,
      End_Module,
      End_Procedure,
      End_Do);
   --
   -- The kind of a Scope Definition record.
   --
   -- Unknown
   --    Unknown or invalid type.
   -- Begin_Module, End_Module
   --    Begin_Module begins, and End_Module ends, the sequence of debug
   --    records for a module.
   -- Begin_Procedure, End_Procedure
   --    Begin and end the sequence of debug records for a procedure,
   --    within a module.
   -- Begin_Do, End_Do
   --    Begin and end the sequence of debug records for a do-block
   --    within a procedure, within a module.


   procedure Read_Block_Type (
      Stream : access Ada.Streams.Root_Stream_Type'Class;
      Item   :    out Block_Type_T);

   for Block_Type_T'Read use Read_Block_Type;


   subtype Block_Beginner_T is Block_Type_T range Begin_Module .. Begin_Do;
   subtype Block_Ender_T    is Block_Type_T range End_Module   .. End_Do;
   --
   -- The Block Types that begin / end a scope.


   type Scope_Definition_T is record
      Block_Type : Block_Type_T;
      Block_Name : Name_Ref;
   end record;
   --
   -- A Scope Definition record.


   --    Source Name


   type Source_Name_T is record
      Unused1   : Octet_T;
      Unused2   : Octet_T;
      Unused3   : Octet_T;
      File_Name : Name_Ref;
   end record;
   --
   -- A Source Name record.
   --
   -- File_Name
   --    Source-file name, without path or drive.


   --    Type Definition


   type Type_Index_T is new Word16_T;
   --
   -- Identifies the type of a symbol.


   procedure Read_Type_Index (
      Stream : access Ada.Streams.Root_Stream_Type'Class;
      Item   :    out Type_Index_T);
   --
   -- A type index is encoded as one octet, or if over 255 then as
   -- an octet with the reserved value 16#1F# followed by a 16-bit
   -- number (big-endian TBC) giving the real type index.

   for Type_Index_T'Read use Read_Type_Index;


   subtype Basic_Type_Index_T is Type_Index_T range 0 .. 16#C#;
   --
   -- The basic (scalar, non-compound) types.


   subtype Compound_Type_Index_T is
      Type_Index_T range 16#20# .. Type_Index_T'Last;
   --
   -- Type indices assigned to program-specific compound types as
   -- defined in Type Definition records.


   type Type_T is (

      -- Basic (predefined, non-compound):

      Untyped,
      Void,
      Bit,
      Char,
      UChar,
      Int,
      UInt,
      Long,
      ULong,
      Float_Little_Endian,
      Float_Big_Endian,
      Double,
      Code_Label,

      -- Summary container for predefined (basic) type(s):

      Predefined,

      -- Compound or derived:

      Funktion,
      Bit_Field,
      List,
      Aray,
      Struct,
      Pointer,
      Spaced_Pointer,
      Generic_Pointer,
      Type_Tag,

      Undefined);
   --
   -- The various kinds (classes) of "type".
   --
   -- The basic kinds are directly identified by type indices in
   -- the range Basic_Type_Index_T in a one-to-one correspondence,
   -- except that some type indices in this range are not used.
   --
   -- Types of the compound or derived kinds are defined by descriptors
   -- in Type Definition records. Each such kind has a specific form
   -- of descriptor.
   --
   -- Type_Tag
   --    A typedef or struct/union tag.
   -- Undefined
   --    Someone used a type-index that was not defined.


   Untyped_Index : constant Basic_Type_Index_T := 0;
   --
   -- The type-index that means Untyped.


   Basic_Type : constant array (Basic_Type_Index_T) of Type_T := (
       Untyped_Index => Untyped,
       1 => Bit,
       2 => Char,
       3 => UChar,
       4 => Int,
       5 => UInt,
       6 => Long,
       7 => ULong,
       8 => Float_Little_Endian,
       9 => Double,
      10 => Code_Label,
      11 => Void,
      12 => Float_Big_Endian);
   --
   -- The mapping from basic type-index to basic type.


   type Compound_Type_Tag_T is new Type_T range Predefined .. Undefined;
   --
   -- The compound types are encoded as tags to indicate the kind
   -- of type descriptor that follows. The Predefined value is
   -- included to have a way to build a descriptor of any type,
   -- basic or compound. Likewise, the Undefined value is included
   -- to trap invalid Type_Index values.


   procedure Read_Compound_Type_Tag (
      Stream : access Ada.Streams.Root_Stream_Type'Class;
      Item   :    out Compound_Type_Tag_T);

   for Compound_Type_Tag_T'Read use Read_Compound_Type_Tag;


   --    Summary container for predefined types:


   type Predefined_T is record
      Basic : Basic_Type_Index_T;
   end record;
   --
   -- A "basic" type descriptor. Such descriptors never occur in
   -- AOMF files; they are an internal mechanism in this package.


   --    Compound/derived type descriptors:


   type Funktion_T is record
      Returns : Type_Index_T;
      Formals : Type_Index_T;
   end record;
   --
   -- A Funktion type descriptor.
   --
   -- Returns
   --    The return (value) type.
   -- Formals
   --    The formal parameter list "type" (a List compound type).


   type Bit_Field_T is record
      Base   : Octet_T;
      Offset : Octet_T;
      Width  : Octet_T;
   end record;
   --
   -- A Bit_Field type descriptor.
   --
   -- Base
   --    The base scalar: 0 = char, 1 = int.
   -- Offset
   --    Field offset in the base scalar (bits).
   -- Width
   --    Field width in bits.


   type List_Component_T is record
      Offset     : Word16_T;
      Type_Index : Type_Index_T;
      Name       : Name_Ref;
   end record;
   --
   -- An element in a List descriptor, describing one component of
   -- the List type.
   --
   -- Offset
   --    Component offset (e.g. in struct).
   -- Type_Index
   --    The type of the component.
   -- Name
   --    The name of the component.


   type Tuple_T is array (Positive range <>) of List_Component_T;
   --
   -- The sequence of components in a List type.


   type Tuple_Ref is access Tuple_T;
   --
   -- A reference to a heap-allocated List component sequence.


   procedure Read_Tuple_Ref (
      Stream : access Ada.Streams.Root_Stream_Type'Class;
      Item   :    out Tuple_Ref);
   --
   -- The tuple is represented as a 16-bit "number of components"
   -- followed by that many List_Components.

   for Tuple_Ref'Read use Read_Tuple_Ref;


   type List_T is record
      Tuple : Tuple_Ref;
   end record;
   --
   -- A List type descriptor.


   subtype Array_Length_T is Word16_T;
   --
   -- The length of an array along one index dimension.


   type Dimensions_T is array (Positive range <>) of Array_Length_T;
   --
   -- The dimensions of an array, listing the length of each index
   -- dimension.

   type Dimensions_Ref is access Dimensions_T;
   --
   -- A reference to a heap-allocated dimension list.


   procedure Read_Dimensions_Ref (
      Stream : access Ada.Streams.Root_Stream_Type'Class;
      Item   :    out Dimensions_Ref);
   --
   -- The dimension list is represented as an 8-bit "number of indices"
   -- followed by that many Array_Length_T values.

   for Dimensions_Ref'Read use Read_Dimensions_Ref;


   type Aray_T is record
      Dimensions : Dimensions_Ref;
      Component  : Type_Index_T;
   end record;
   --
   -- An Aray type descriptor.
   --
   -- Dimensions
   --    The dimensions of the array.
   -- Component
   --    The type of array components (elements).


   type Struct_T is record
      Total_Size : Word16_T;
      Members    : Type_Index_T;
      Tag_Name   : Type_Index_T;
   end record;
   --
   -- A Struct type descriptor.
   --
   -- Total_Size
   --    The total size of the struct or union, in octets.
   -- Members
   --    Identifies the List of struct/union members.
   -- Tag_Name
   --    Identifies the Type_Tag for the struct/union.


   type Pointer_T is record
      Target : Type_Index_T;
   end record;
   --
   -- A Pointer type descriptor.
   --
   -- Target
   --    The type of object the Pointer refers to.


   type Pointer_Space_T is (
      None,
      IData,
      XData,
      PData,
      Data,
      Code,
      Unknown);
   --
   -- The kind of storage (memory space) that a Spaced_Pointer or
   -- Generic_Pointer can point at.


   procedure Read_Pointer_Space (
      Stream : access Ada.Streams.Root_Stream_Type'Class;
      Item   :    out Pointer_Space_T);

   for Pointer_Space_T'Read use Read_Pointer_Space;


   type Spaced_Pointer_T is record
      Target : Type_Index_T;
      Length : Octet_T;
      Space  : Pointer_Space_T;
   end record;
   --
   -- A Spaced_Pointer type descriptor.
   --
   -- Target
   --    The type of object the pointer refers to.
   -- Length
   --    The size (in octets) of the pointer itself (1 or 2).
   -- Space
   --    The storage space that the pointer refers to.
   --    The value None should not occur according to ref 2.


   type Pointer_Kind_T is (
      Data,
      Funktion,
      Huge,
      Unknown);
   --
   -- The kind of a Generic Pointer.


   procedure Read_Pointer_Kind (
      Stream : access Ada.Streams.Root_Stream_Type'Class;
      Item   :    out Pointer_Kind_T);

   for Pointer_Kind_T'Read use Read_Pointer_Kind;


   type Generic_Pointer_T is record
      Attributes : Octet_T;
      Size       : Octet_T;
      Space      : Pointer_Space_T;
      Kind       : Pointer_Kind_T;
      Unused1    : Octet_T;
      Unused2    : Word16_T;
      Target     : Type_Index_T;
   end record;
   --
   -- A Generic_Pointer type descriptor.
   --
   -- Attributes
   --    What sort of pointer:
   --       1 = C51 3-byte pointer.
   --       2 = C51 spaced pointer
   --       3 = C51 3-byte reverse pointer.
   -- Size
   --    The size (in bits) of the pointer itself (8, 16 or 24).
   -- Kind
   --    What kind of pointer.
   -- Target
   -- Target
   --    The type of object the pointer refers to.


   type Type_Tag_T is record
      Base : Type_Index_T;
      Name : Name_Ref;
   end record;
   --
   -- A Type_Tag type descriptor.
   --
   -- Base
   --   Zero for a struct/union tag-name.
   --   Nonzero for a typedef, and then identifies the base type.
   -- Name
   --   The name, natch.


   --    All compound types collected into a variant record type


   type Compound_Type_T (Kind : Compound_Type_Tag_T := Undefined) is record

      case Kind is

      when Predefined      => Predefined      : Predefined_T;
      when Funktion        => Funktion        : Funktion_T;
      when Bit_Field       => Bit_Field       : Bit_Field_T;
      when List            => List            : List_T;
      when Aray            => Aray            : Aray_T;
      when Struct          => Struct          : Struct_T;
      when Pointer         => Pointer         : Pointer_T;
      when Spaced_Pointer  => Spaced_Pointer  : Spaced_Pointer_T;
      when Generic_Pointer => Generic_Pointer : Generic_Pointer_T;
      when Type_Tag        => Type_Tag        : Type_Tag_T;
      when Undefined       => null;

      end case;

   end record;
   --
   -- A compound type descriptor of some Kind.
   --
   -- A Type Definition EAOMF record consists of just a sequence of
   -- compound type descriptors.


   --    Debug Items
   --
   -- There are four kinds of Debug Items record, depending on the kind
   -- of information represented: Local Symbols, Public Symbols, Segment
   -- Symbols or Line Numbers. Each Debug Items record starts with one
   -- octet that indicates the kind of information, and the rest of
   -- the record consists of a sequence of Items of this kind.
   --
   -- We define types for each kind of Item, but not a type for the
   -- whole Debug Items record; we expect clients to read and process
   -- the Items one by one as follows:
   --
   --   1. Read the record info (kind and length).
   --
   --   2. If it is a Debug Items record then
   --      2a. Read the Def_Kind.
   --      2b. Read Items of this Def_Kind while there is Data_Left
   --          in the record.
   --      2c. Skip to the next record.
   --
   --   3. else if it is an Extended Debug Items record then
   --      3a. Read the Def_Kind.
   --      3b. Read Extended Items of this Def_Kind while there is
   --          Data_Left in the record.
   --      3c. Skip to the next record.


   type Def_Kind_T is (
      Local_Symbols,
      Public_Symbols,
      Segment_Symbols,
      Line_Numbers,
      Unknown);
   --
   -- The kinds of items that can be defined in a Debug Items record.
   --
   -- The item entry for Local Symbols and Public Symbols has the same
   -- form. The item entries for Segment Symbols is a little different
   -- and that for Line Numbers very different.
   --
   -- The Unknown item stands for an unknown or invalid encoding.


   procedure Read_Def_Kind (
      Stream : access Ada.Streams.Root_Stream_Type'Class;
      Item   :    out Def_Kind_T);
   --
   -- The Def_Kind is encoded in one octet.

   for Def_Kind_T'Read use Read_Def_Kind;


   type Symbol_Item_T is record
      Seg_ID : Octet_T;
      Info   : Symbol_Info_T;
      Offset : Word16_T;
      Unused : Octet_T;
      Name   : Name_Ref;
   end record;
   --
   -- Debug Item for a local or public symbol.


   type Segment_Symbol_Item_T is record
      Seg_ID : Octet_T;
      Info   : Segment_Info_T;
      Offset : Word16_T;
      Unused : Octet_T;
      Name   : Name_Ref;
   end record;
   --
   -- Debug Item for a segment symbol.


   type Line_Item_T is record
      Seg_ID : Octet_T;
      Offset : Word16_T;
      Line   : Word16_T;
   end record;
   --
   -- Debug Item for a line-number.
   --
   -- Line
   --    The number (1 ...) of a source line in the current (enclosing)
   --    source file, as defined TBC by the preceding Source_Name record.


   --    Extended Debug Items
   --
   -- An Extended Debug Items record is similar to a Debug Items record
   -- in having four different forms, for Local Symbols, Public Symbols,
   -- Segment Symbols and Line Numbers respectively. However, now the
   -- Local, Public and Segment symbols share the same form of item entry,
   -- which is different from all the non-extended item entries, while
   -- the Line Number items have the same form as in a non-extended
   -- Debug Items record. Therefore, we need only define a type for
   -- the extended Symbol item.


   type Symbol_Item_Extended_T is record
      Seg_ID     : Octet_T;
      Storage    : Storage_T;
      Offset     : Word16_T;
      Type_Index : Type_Index_T;
      Name       : Name_Ref;
   end record;
   --
   -- Extended Debug Item for a Local, Public or Segment Symbol.
   --
   -- TBD if Type_Index uses the 1-or-3-octet form.


   --
   --    Whole AOMF record descriptors
   --


   type Record_Info_T is record
      Start     : IO.Positive_Count;
      Kind_Code : Octet_T;
      Kind      : Record_Kind_T;
      Length    : Word16_T;
   end record;
   --
   -- Describes an AOMF record without giving the contents.
   --
   -- Start
   --    The starting position in the file = file index for the
   --    "record type" octet. This is not represent in the AOMF
   --    file itself but is snapped up by the reading operation.
   -- Kind_Code
   --    The code for the record Kind, for reporting unknown kinds.
   -- Kind
   --    The record type.
   -- Length
   --    The length field, giving the number of octets in the record
   --    after the length field and up to and including the check-sum
   --    octet that ends the record.


   procedure Read_Record_Info (
      File   : in     IO.File_Type;
      Stream : access Ada.Streams.Root_Stream_Type'Class;
      Item   :    out Record_Info_T);
   --
   -- Reads the next record header from the File and the associated
   -- Stream, leaving the Stream at the next octet after the length
   -- field = the start of the record data.


   function Data_Left (
      Within : Record_Info_T;
      File   : IO.File_Type)
   return Boolean;
   --
   -- Whether there is still some data left Within a given AOMF
   -- record, reading from the current position of the given File.
   -- This is deduced by comparing the File index to the Start and
   -- Length of the record.
   --
   -- The final check-sum octet is not considered "data".


   procedure Check_Sum (
      Over   : in     Record_Info_T;
      File   : in     IO.File_Type;
      Stream : access Ada.Streams.Root_Stream_Type'Class;
      Valid  :    out Boolean);
   --
   -- Verifies that the Sum (mod 256) of all the octets Over the given
   -- record in the File is zero. The Valid parameters shows whether
   -- this is the case. The procedure does not emit an error message
   -- if the check-sum is invalid; this is the caller's job.
   --
   -- Leaves the File positioned at the start of the record data,
   -- after the record type octet and the record length field.


   procedure Skip_To_Next_Record (
      After : in Record_Info_T;
      File  : in IO.File_Type);
   --
   -- Skips to the File position immediately After a given record.
   -- This will normally be the start of the next record.


   --
   --    Quick verification of AOMF file type
   --


   function Accepts (
      Name : in String;
      File : in IO.File_Type)
   return Boolean;
   --
   -- Whether the file seems to be an AOMF file, as judged by the
   -- start of the file content.
   -- The file may have to be rewound (reset to start) before being
   -- loaded.


   --
   --    Building representations of AOMF data
   --


   type Type_Table_T is limited private;
   --
   -- A table of all the types defined in a Type Definition record,
   -- and also all the Predefined basic types.


   function Element (From : Type_Table_T; Index : Type_Index_T)
   return Compound_Type_T;
   --
   -- Picks an element From the table.
   -- If the table has no element at this Index, either Constraint_Error
   -- results or an Undefined type results.


   function Defining_Type (Index : Type_Index_T; Table : Type_Table_T)
   return Compound_Type_T;
   --
   -- The underlying Type descriptor that actually defines the Type with the
   -- given Index, in the given Table. This type is the one we reach by
   -- starting at the Index type and stripping off all levels of Typedef.


   procedure Read_Type_Definitions (
      Rec    : in     Record_Info_T;
      File   : in     IO.File_Type;
      Stream : in     IO.Stream_Access;
      Table  :    out Type_Table_T);
   --
   -- Reads the Type Definition record described by Rec and builds
   -- a table of all the types defined in the record. Assumes that
   -- the Stream is placed at the start of the first type descriptor
   -- (after the record length field).


   procedure Clear (Table : in out Type_Table_T);
   --
   -- Clears (erases) the Table, discarding all Compound Type
   -- descriptors it contains.


private


   type Type_List_T is array (Positive range <>) of Compound_Type_T;
   --
   -- An indexed array of Compound Type descriptors.


   package Type_Vectors is new Unbounded_Vectors (
      Element_Type   => Compound_Type_T,
      Vector_Type    => Type_List_T,
      Initial_Size   => 200,
      Size_Increment => 400,
      Deallocate     => Deallocate);


   type Type_Table_T is new Type_Vectors.Unbounded_Vector;
   --
   -- A Type Table is represented as an unbounded vector logically indexed
   -- by Type_Index. However, physically the table starts with the first
   -- compound type index, 16#20#, so the actual (Positive) index is
   -- Type_Index - 16#1F#.


end Formats.AOMF_Keil;
