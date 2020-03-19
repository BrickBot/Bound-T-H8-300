-- Formats.Dwarf (decl)
--
-- Parent package for the DWARF debugging information format.
--
-- DWARF is a structure and notation for so-called "debugging information"
-- that describes the relationship between the source-code form and the
-- compiled and linked machine-code form of a program. It also describes
-- some of the internal structure of the machine-code program, such as
-- the call-frame structure and the management of return address, that
-- has no direct counterpart on the source-code level.
--
-- DWARF was developed by the UNIX International Programming Languages
-- Special Interest Group (SIG). The reference on which this package
-- family is based is DWARF Version 3 as defined in the document
-- "DWARF Debugging Information Format" Revision V3 Draft 7 (October
-- 29, 2001).
--
-- The DWARF information has the following major facets, with each facet
-- typically stored in its own section of a binary program file:
--
-- > Main debugging information entries, representing compilation units,
--   modules, subprograms, types and data objects.
--
--   This information is stored in the ".debug_info" section, with a
--   table of entry abbreviations in the ".debug_abbrev" section.
--
-- > Subprogram call-frame structures, showing where registers are saved
--   and where the return address is kept.
--
--   This information is stored in the ".debug_frame" section.
--
-- > Line-number information that correlates source-code line-numbers 
--   with the machine-code addresses of the instructions generated for
--   the source-code lines.
--
--   This information is stored in the ".debug_line" section.
--
-- > Accelerated access tables for looking up debugging information based
--   on the source-level symbolic name (identifier) of the program element.
--
--   This information is stored in the ".debug_pubnames" section (for
--   subprograms and data objects) and in the ".debug_pubtypes"
--   section (for types).
--
-- There are often references (pointers) from the main information
-- entries in the ".debug_info" section to related information in the
-- other sections.
--
-- This parent package defines the central elements of DWARF and has
-- a child package for each facet to define the details of the facet.
-- The special child package Formats.Dwarf.Omni defines a data structure
-- that can hold all the DWARF information composed of all the facets.
--
-- While DWARF is a powerful, expressive and well-defined format for
-- debugging information, it is aimed to support debuggers rather than
-- static program analyzers such as Bound-T. For maximum flexibility in
-- the face of ever-changing processor architectures and compilation
-- schemes, DWARF has abandoned the strict declarative or tabular basis
-- of the earlier debugging formats, such as STABS, in favour of a
-- procedural representation that provides exactly the functions that
-- debuggers need but is harder to use for Bound-T.
--
-- For example, to define the mapping of data object (variables) to machine
-- storage locations, DWARF does not provide a one-to-one mapping from
-- variable symbol to memory location, but instead provides for each
-- variable a "location expression" (really a small program) that computes
-- the address of the variable using the current program state. This is
-- ideal for a debugger to use when the program is stopped at a breakpoint
-- and the user asks for the value of a variable, but Bound-T needs a mapping
-- in the opposite direction: from the instructions leading up to a certain
-- storage access, Bound-T needs to find out the symbol name of the accessed
-- variable. This can only be done by comparing the target program's address
-- computation with the DWARF location expressions and runs into the
-- undecidable question of the equivalence of two programs.
--
-- Moreover, DWARF is very focussed on compressing the representation of
-- the debugging information. For example, instead of directly storing a
-- table correlating source-code line-numbers with machine addresses,
-- DWARF stores a "line-number program" that must be interpreted from
-- start to end to build such a table. Random access by machine address
-- or line-number to such a table is challenging.
--
-- The physical storage of DWARF information is quite well defined. There
-- are only two major variants: the 32-bit format and the 64-bit format.
-- These numbers (32 or 64) only define the length of a few key fields
-- that have a fixed length. Most real debugging information is encoded
-- in a variable-length form or in the same form in both variants.
--
-- The main DWARF headers show which variant (32 or 64 bits) is in use.
--
-- Author: Niklas Holsti.
--
-- A component of the Bound-T Worst-Case Execution Time Tool.
--
-------------------------------------------------------------------------------
-- Copyright (c) 1999 .. 2015 Tidorum Ltd, except for text copied verbatim
-- from the DWARF standard.
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
-- $Date: 2015/10/24 20:53:52 $
--
-- $Log: formats-dwarf.ads,v $
-- Revision 1.6  2015/10/24 20:53:52  niklas
-- Moved to free licence.
--
-- Revision 1.5  2009-11-19 15:13:43  niklas
-- Removed non-graphic character from comment.
--
-- Revision 1.4  2008-10-15 12:05:53  niklas
-- BT-CH-0150: DWARF location lists, lexical block scopes, CUBAs.
--
-- Revision 1.3  2007/06/14 11:16:41  niklas
-- BT-CH-0060.
--
-- Revision 1.2  2006/04/12 19:45:25  niklas
-- Added an Error report to the Value_Def function when the
-- attribute is missing.
-- Added procedure Find to find an attribute in a value list
-- without raising exceptions for missing attributes.
--
-- Revision 1.1  2004/04/24 18:06:22  niklas
-- First version.
--


with Formats.In_Memory;
with Formats.LEB128;


package Formats.Dwarf is


   --
   --    The DWARF sections
   --


   type Sections_T is record
      Info      : In_Memory.Stream_Ref;
      Strings   : In_Memory.Stream_Ref;
      Locations : In_Memory.Stream_Ref;
   end record;
   --
   -- Access to DWARF information thrugh in-memory copies of the
   -- main DWARF sections. Absent sections are represented by null.
   -- The Info section is always present (not null). The Abbrev
   -- section is not given here because it is used only in the first
   -- phase of loading the Info section.


   --
   --    Primitive DWARF types:
   --


   type Bits_T is (Bits_32, Bits_64, Bits_Unknown);
   --
   -- The DWARF variant: 32-bit or 64-bit.
   -- Initially unknown.


   subtype Known_Bits_T is Bits_T range Bits_32 .. Bits_64;
   --
   -- The two possible (known) bit-widths.


   type Initial_Length_T is record
      Bits   : Bits_T;
      Length : Unsigned_64_T;
   end record;
   --
   -- The "initial length" of a DWARF information block.
   -- This gives the length of the block as the number of octets in
   -- the block, not including the initial length datum itself.
   -- An initial length is physically represented in a way that tells
   -- the reader whether the DWARF information uses the 32-bit variant
   -- or the 64-bit variant.


   Initial_Length_Length :
      constant array (Known_Bits_T) of IO.Positive_Count := (
         Bits_32 =>  4,
         Bits_64 => 12); -- 4 + 8.
   --
   -- The number of octets in the representation of an Initial_Length_T
   -- for a certain (known) DWARF bit-width.


   procedure Read_Initial_Length (
      Stream : access Ada.Streams.Root_Stream_Type'Class;
      Item   : out    Initial_Length_T);
   --
   -- Reads an "initial length" field from a DWARF section.
   -- May propagate Format_Error.
   --
   for Initial_Length_T'Read use Read_Initial_Length;


   function Total_Length (Initial : Initial_Length_T)
   return IO.Positive_Count;
   --
   -- The total number of octets in the representation of an Initial
   -- length and the sub-section of data headed by the Initial length.


   function Image (Item : Initial_Length_T) return String;
   --
   -- Displays the initial length and the 32/64 bit selection.


   function Rounded_Up (
      Count   : Unsigned_64_T;
      Modulus : Unsigned_64_T)
   return Unsigned_64_T;
   --
   -- The Count, rounded up to a multiple of the Modulus.


   function Rounded_Length (Initial : Initial_Length_T)
   return IO.Positive_Count;
   --
   -- The length of the DWARF information block with the given initial
   -- length, rounded up as necessary.


   function Next_Index (
       From  : IO.Positive_Count;
       After : Initial_Length_T)
   return IO.Positive_Count;
   --
   -- The file-IO index of the first octet that follows a DWARF information
   -- block with the given length (After) when the Initial Length field
   -- of the block starts at a given IO-index (From).


   type Version_T is new Unsigned_16_T;
   --
   -- Many DWARF data blocks have a version field that shows
   -- the version of the DWARF standard that is used in the block.
   -- These version fields are (always TBC) 16-bit unsigned
   -- numbers. The value is directly the DWARF version, from 1
   -- to (currently) 3. We support versions 2 and 3; version 3
   -- is upwards compatible with version 2 but not with version 1.
   -- Different blocks in the same executable may have different
   -- versions (it seems).


   Version_Length : constant := 2;
   --
   -- The number of octets in a Version field.


   subtype Addr_Size_T is Unsigned_8_T;
   --
   -- The size in octets of an address on the target architecture.
   -- In a DWARF file, this is a property of each compilation unit,
   -- usually but TBC not necessarily the same for all compilation
   -- units in the same DWARF file.
   --
   -- Attributes values of the "Addr" form are represented with this
   -- number of octets with the endianness defined by the host file
   -- format (e.g. ELF) TBC.


   subtype Supported_Addr_Size_T is Addr_Size_T range 2 .. 8;
   --
   -- The supported range of Addr_Size.


   Addr_Size_Length : constant := 1;
   --
   -- The length of an Address_Size_T value in the DWARF file, in
   -- octet units.


   type Address_T is new Unsigned_64_T;
   --
   -- A machine address, of instructions or data.


   subtype Code_Address_T is Address_T;
   --
   -- The machine address of an instruction.


   type Poss_Code_Address_T (Defined : Boolean := False) is record
      case Defined is
      when False => null;
      when True  => Address : Code_Address_T;
      end case;
   end record;
   --
   -- An instruction address, possibly undefined.
   -- This is not a basic DWARF type but, for example, models the
   -- "base address" of a DWARF compilation unit that may or may not
   -- be defined.


   subtype Code_Offset_T is Address_T;
   --
   -- An unsigned offset (increment or decrement, depending on context)
   -- to a machine instruction address.


   Last_Address : constant array (Supported_Addr_Size_T) of Address_T := (
      2 => 2**16 - 1,
      3 => 2**24 - 1,
      4 => 2**32 - 1,
      5 => 2**40 - 1,
      6 => 2**48 - 1,
      7 => 2**54 - 1,
      8 => Address_T'Last);
   --
   -- The last (largest) representable address for each supported
   -- address size.


   procedure Read_Address (
      Stream : access Ada.Streams.Root_Stream_Type'Class;
      Size   : in     Addr_Size_T;
      Item   :    out Address_T);
   --
   -- Reads an address of the given Size.


   function Address (
      Stream : access Ada.Streams.Root_Stream_Type'Class;
      Size   :        Addr_Size_T)
   return Address_T;
   --
   -- Reads and returns an address of the given Size.


   function Image (Item : Address_T; Size : Addr_Size_T) return String;
   --
   -- Hexadecimal presentation, zero-padded on the left to correspond
   -- to the Size (2 * Size hex digits).


   subtype Variant_Unsigned_T is Unsigned_64_T;
   --
   -- An unsigned number with a size that depends on the DWARF variant,
   -- being either 32 or 64 bits.


   Variant_Unsigned_Length :
      constant array (Known_Bits_T) of IO.Positive_Count := (
         Bits_32 =>  4,
         Bits_64 =>  8);
   --
   -- The number of octets in the representation of a Variant_Unsigned_T
   -- for a certain (known) DWARF bit-width.


   procedure Read_Variant_Unsigned (
      Stream : access Ada.Streams.Root_Stream_Type'Class;
      Bits   : in     Bits_T;
      Item   :    out Variant_Unsigned_T);
   --
   -- Reads an unsigned number from a DWARF section, using the
   -- specified number of bits.
   -- Propagates Format_Error if Bits = Bits_Unknown.


   subtype Section_Offset_T is Variant_Unsigned_T;
   --
   -- A pointer to some information within a DWARF section, given
   -- as an octet offset relative to the start of the section.
   --
   -- In 32-bit DWARF, offsets are 32 bits in size.
   -- In 64-bit DWARF, offsets are 64 bits in size.


   type Boolean_T is new Boolean;
   --
   -- A boolean value represented in the DWARF section as a zero
   -- for False and any non-zero value for True.


   procedure Read_Boolean (
      Stream : access Ada.Streams.Root_Stream_Type'Class;
      Item   : out    Boolean_T);
   --
   -- Reads a boolean (zero/nonzero) field from a DWARF section.
   --
   for Boolean_T'Read use Read_Boolean;


   --
   --    Types represented in LEB128 form.
   --
   -- These types should generally be subtyped before use.
   --


   type Our_Unsigned_32_T is new Unsigned_32_T;
   --
   -- A new 32-bit unsigned type for which we can instantiate
   -- the LEB128 Read operation to become primitive operations.


   procedure Read_Unsigned_32_LEB128
   is new LEB128.Read_Unsigned (Our_Unsigned_32_T);
   --
   -- This is now a primive operation for Our_Unsigned_32_T.
   -- However, we cannot now give a representation clause
   -- to make this the 'Read operation for Our_Unsigned_32_T
   -- because the instantiation has frozen that type.


   type Unsigned_32_LEB128_T is new Our_Unsigned_32_T;
   --
   -- An unsigned number of at most 32 bits, stored in a DWARF
   -- section in LEB128 form.
   --
   -- This type inherits the operation Read_Unsigned_32_LEB128.
   --
   for Unsigned_32_LEB128_T'Read use Read_Unsigned_32_LEB128;


   subtype Timestamp_T is LEB128.Longest_Unsigned_T;
   --
   -- A time-stamp for e.g. file modification dates.
   -- Interpretation is implementation-dependent.


   --
   --    Reading Integer as LEB128
   --


   procedure Read_LEB128_Integer (
      Stream : access Ada.Streams.Root_Stream_Type'Class;
      Item   : out    Integer);
   --
   -- Reads a signed LEB128 number and returns it as Integer type.


   procedure Read_LEB128_Natural (
      Stream : access Ada.Streams.Root_Stream_Type'Class;
      Item   : out    Natural);
   --
   -- Reads an unsigned LEB128 number and returns it as Natural type.


   --
   --   Tags for debugging information entries
   --


   type Tag_T
   --
   -- Defines the type of entity that a debugging information
   -- entry represents.
   --
   -- The enumeration order is chosen to group together tags that
   -- probably are treated in more or less the same way when the DWARF
   -- tree is traversed. Thus, subtypes (ranges) can be defined to
   -- simplify traversal filters and conditional traversal actions.
   --
   is (

      Null_Tag,

      -- Compilation units, modules and the like

      Compile_Unit,
      Partial_Unit,
      Module,
      Namespace,
      Common_Block,
      Namelist,

      -- *  Special properties of the above

      Namelist_Item,

      -- Generics and templates

      Template_Type_Parameter,
      Template_Value_Parameter,

      -- Subprograms, blocks and other scopes

      -- *  Blocks

      Try_Block,
      Catch_Block,
      Lexical_Block,

      -- *  Callable entities

      Subprogram,
      Entry_Point,

      -- *  Inlined callable entities

      Inlined_Subroutine,

      -- *  Executable statements or places

      Label,
      With_Stmt,

      -- Properties of subprograms

      Common_Inclusion,
      Thrown_Type,

      -- Types

      -- *  Simple or stand-alone types

      Base_Type,
      Enumeration_Type,
      Unspecified_Type,

      -- *  Type modifiers

      Const_Type,
      Mutable_Type,
      Packed_Type,
      Restrict_Type,
      Volatile_Type,

      -- *  Unary type constructors

      -- *  *  Pointer types

      Pointer_Type,
      Reference_Type,
      Ptr_To_Member_Type,

      -- *  *  Subtypes

      Subrange_Type,

      -- *  Composite types

      -- *  *  Array types

      Array_Type,
      Set_Type,
      String_Type,

      -- *  *  Sequence types

      File_Type,

      -- *  *  Product types

      Class_Type,
      Interface_Type,
      Structure_Type,
      Union_Type,

      -- *  Subprogram types

      Subroutine_Type,

      -- *  Typedef

      Typedef,

      -- Properties of types

      -- *  Product type properties

      Friend,
      Inheritance,
      Member,
      Variant_Part,
      Variant,

      -- Data objects

      -- *  Named numbers and enumeration literals

      Constant_Value,
      Enumerator,

      -- *  Variables

      Variable,

      -- *  Parameters

      Formal_Parameter,
      Unspecified_Parameters,

      -- Data object modifiers

      Access_Declaration,

      -- Imported stuff

      Imported_Declaration,
      Imported_Module,
      Imported_Unit,

      -- DWARF procedures

      Dwarf_Procedure,

      -- User/vendor extensions

      Vendor_Extension,

      -- Invalid code

      Invalid_Code);


   --    Tag subranges:


   subtype Module_Tag_T is Tag_T range Compile_Unit .. Namelist;
   --
   -- Compilation units, modules and the like.
   -- Tags that represent program parts that usually own a lot
   -- of declarations of types, variables, subprograms.


   subtype Block_Tag_T is Tag_T range Try_Block .. Lexical_Block;
   --
   -- Blocks that can contain local declarations.


   subtype Callable_Tag_T is Tag_T range Subprogram .. Entry_Point;
   --
   -- Callable sub-programs.


   subtype Type_Tag_T is Tag_T range Base_Type .. Typedef;
   --
   -- Type declarations.


   subtype Data_Tag_T is Tag_T range Constant_Value .. Unspecified_Parameters;
   --
   -- Declarations of data constants, variables, parameters etc.


   --    Sets of tags


   type Tag_Set_T is array (Tag_T) of Boolean;
   --
   -- A (sub)set of tags. Members are marked by True values.
   --
   pragma Pack (Tag_Set_T);


   type Tag_Code_T is new Unsigned_16_T;
   --
   -- The internal code of a tag.
   -- Externally, tags are encoded as unsigned LEB128 numbers.


   subtype Standard_Tag_Code_T is Tag_Code_T range 16#00# .. 16#3e#;
   --
   -- The range of tag codes that contains all the standard tags.


   subtype Vendor_Tag_Code_T is Tag_Code_T range 16#4080# .. 16#ffff#;
   --
   -- The range of tag codes that contains all vendor-specific
   -- extended tags.


   function To_Tag (Code : Tag_Code_T) return Tag_T;
   --
   -- The tag represented by the tag-code.


   function Image (Item : Tag_T) return String;
   --
   -- Readable name of the tag.


   --
   --    Attributes for debugging information entries
   --


   type Attribute_T
   --
   -- Identifies an attribute (property) that a debugging information
   -- entry provides and pertains to the program element that the entry
   -- represents. The attribute or property value is represented
   -- separately in a form and endcoding that depends on the attribute.
   --
   is (

      Null_Attribute,

      Sibling,
      Location,
      Name,
      Ordering,
      Byte_Size,
      Bit_Offset,
      Bit_Size,
      Stmt_List,
      Low_PC,
      High_PC,
      Language,
      Discr,
      Discr_Value,
      Visibility,
      Import,
      String_Length,
      Common_Reference,
      Comp_Dir,
      Const_Value,
      Containing_Type,
      Default_Value,
      Inline,
      Is_Optional,
      Lower_Bound,
      Producer,
      Prototyped,
      Return_Addr,
      Start_Scope,
      Stride_Size,
      Upper_Bound,
      Abstract_Origin,
      Accessibility,
      Address_Class,
      Artificial,
      Base_Types,
      Calling_Convention,
      Count,
      Data_Member_Location,

      Decl_Column,
      Decl_File,
      Decl_Line,

      Declaration,
      Discr_List,
      Encoding,
      External,
      Frame_Base,
      Friend,
      Identifier_Case,
      Macro_Info,
      Namelist_Item,
      Priority,
      Segment,
      Specification,
      Static_Link,
      Type_Ref,    -- DW_AT_type
      Use_Location,
      Variable_Parameter,
      Virtuality,
      Vtable_Elem_Location,
      Allocated,
      Associated,
      Data_Location,
      Stride,
      Entry_PC,
      Use_UTF8,
      Extension,
      Ranges,
      Trampoline,

      Call_Column,
      Call_File,
      Call_Line,

      Description,

      -- Invalid code

      Invalid_Code,

      -- User/vendor extensions

      Vendor_Extension);


   type Attribute_Code_T is new Unsigned_16_T;
   --
   -- The internal code of an attribute.
   -- Externally, attributes are encoded as unsigned LEB128 numbers.


   subtype Standard_Attribute_Code_T is
      Attribute_Code_T range 16#00# .. 16#5a#;
   --
   -- The range of attribute codes that contains all the standard 
   -- attributes.


   subtype Vendor_Attribute_Code_T is
      Attribute_Code_T range 16#2000# .. 16#3fff#;
   --
   -- The range of attribute codes that contains all vendor-specific
   -- extended attributes.


   function To_Attribute (Code : Attribute_Code_T) return Attribute_T;
   --
   -- The attribute represented by the attribute-code.


   function Image (Item : Attribute_T) return String;
   --
   -- Readable name of the attribute.


   --
   --    Attribute value forms
   --


   type Form_T
   --
   -- The form (type) of an attribute or property value.
   --
   is (

      Null_Form,

      -- Fixed size address:

      Addr,

      -- Flag forms:

      Flag,

      -- Fixed size constant forms:

      Data1,
      Data2,
      Data4,
      Data8,

      -- Fixed-size signed constants (implementation extension):

      Data1s,
      Data2s,
      Data4s,
      Data8s,

      -- Variable-size constant forms:

      Sdata,
      Udata,

      -- Block forms:

      Block1,
      Block2,
      Block4,
      Block,

      -- Reference forms:

      Ref1,
      Ref2,
      Ref4,
      Ref8,
      Ref_Udata,
      Ref_Addr,

      -- String forms:

      String_Form,  -- DW_FORM_string
      Strp,

      -- Indirection (form defined in the Info entry itself):

      Indirect,

      -- Invalid code

      Invalid_Code

      -- There are no user/vendor extensions.

      );
   --
   -- The DWARF forms are here extended (for internal implementation
   -- reasons) with signed forms of the fixed-size constants:
   --    Data1s is a signed octet.
   --    Data2s is a signed 16-bit quantity.
   --    Data4s is a signed 32-bit quantity.
   --    Data8s is a signed 64-bit quantity.
   -- No external (file-resident) form-codes are defined for these forms.
   -- These forms are used only within this implementation (to represent
   -- the form of the operands for the DWARF expression operators
   -- Const1s, Const2s, Const4s and Const8s).


   subtype Valid_Form_T is Form_T range Addr .. Indirect;
   --
   -- The valid forms, omitting Null_Form and Invalid_Code.


   subtype True_Form_T is Form_T range Addr .. Strp;
   --
   -- The valid and true forms, omitting Null_Form, Indirect and
   -- Invalid_Code.


   subtype Constant_Form_T is Form_T range Data1 .. Udata;
   --
   -- The "constant" forms.


   subtype Section_Offset_Form_T is Form_T range Data4 .. Data8;
   --
   -- The forms used for section offset attributes, eg. loclistptr.


   subtype Block_Form_T is Form_T range Block1 .. Block;
   --
   -- The "block" forms.


   subtype Reference_Form_T is Form_T range Ref1 .. Ref_Addr;
   --
   -- The "reference" forms.


   subtype String_Form_T is Form_T range String_Form .. Strp;
   --
   -- The "string" forms.


   type Form_Code_T is new Unsigned_8_T;
   --
   -- The internal code of a form.
   -- Externally, forms are encoded as unsigned LEB128 numbers.


   subtype Standard_Form_Code_T is Form_Code_T range 16#00# .. 16#16#;
   --
   -- The range of form codes that contains all the standard forms.


   function To_Form (Code : Form_Code_T) return Form_T;
   --
   -- The form represented by the form-code.


   function Image (Item : Form_T) return String;
   --
   -- Readable name of the form.


   --
   --    Attribute values of known form from DWARF streams
   --


   subtype Number_T is Signed_64_T;
   --
   -- A "constant" integer value.


   function Number (
      Info : access Ada.Streams.Root_Stream_Type'Class;
      Form :        Constant_Form_T)
   return Number_T;
   --
   -- A numeric constant of the given Form, read from the current
   -- position of the Info stream.


   function Reference (
      Info   : access Ada.Streams.Root_Stream_Type'Class;
      Form   :        Reference_Form_T;
      Bits   :        Bits_T)
   return Section_Offset_T;
   --
   -- A reference to another DWARF entry, read from the current position
   -- of the Info stream using the given Form and Bits size. This is just
   -- an offset within the .debug_info section, from the start of the
   -- compilation unit or the start of the section, depending on the Form.


   function Reference (
      Info : access Ada.Streams.Root_Stream_Type'Class;
      Form :        Reference_Form_T;
      Bits :        Bits_T;
      Base :        In_Memory.Index_T)
   return In_Memory.Index_T;
   --
   -- A reference to another DWARF entry, read from the current position
   -- of the Info stream using the given Form and Bits size, and adding
   -- the Base index of the compilation unit or of the whole Info,
   -- depending on the Form.


   --
   --    Attribute values embedded in DWARF streams
   --


   procedure Read_True_Form (
      Stream : access Ada.Streams.Root_Stream_Type'Class;
      Item   : out    True_Form_T);
   --
   -- Resolves form indirection by reading the true form-code
   -- from the info Stream and returning the true form in Item.


   function True_Form (
      Given : Form_T;
      From  : access Ada.Streams.Root_Stream_Type'Class)
   return True_Form_T;
   --
   -- Resolved form indirection by returning the Given form as such
   -- unless the Given form is Indirect, in which case the function
   -- reads and returns the true form From the given stream. The From
   -- stream must initially be positioned at the start of the true
   -- form (start of attribute) and will on return be positioned after
   -- the true form code (start of attribute value). If the Given form
   -- is not Indirect, the From stream is not touched.


   function Block_Length (
      Form   : Block_Form_T;
      Stream : access Ada.Streams.Root_Stream_Type'Class)
   return In_Memory.Count_T;
   --
   -- The length, in octets, of the block of the given (true) Form
   -- that resides in the given Stream. The Stream must be positioned
   -- at the start of the block's length field and will, on return,
   -- be positioned after the length field at the first octet of
   -- block data.


   type Value_Def_T is record
      Attribute : Attribute_T;
      Form      : True_Form_T;
      Index     : In_Memory.Index_T;
   end record;
   --
   -- The meaning, form and location of an attribute value in a DWARF
   -- debugging information entry (DIE).
   --
   -- Attribute
   --    The attribute for which this is the value.
   -- Form
   --    The true form of the value encoding, with possible form indirection
   --    resolved.
   -- Index
   --    The index, in some in-memory copy of the DWARF ".debug_info" section,
   --    of the first octet of the value encoded per the Form.
   --    If the abbreviation entry gave an Indirect form, Index refers
   --    to the first octet after the true form code, so it is placed at
   --    the same point as it would be if the abbreviation had specified
   --    the true Form directly instead of Indirect.


   type Value_Def_List_T is array (Positive range <>) of Value_Def_T;
   --
   -- A list that defines the meaning, form and location of all the
   -- attribute values in a DIE.


   Attribute_Absent : exception;
   --
   -- Raised upon an attempt to locate an attribute within a
   -- debugging information entry that does not contain the
   -- desired attribute.


   Wrong_Form : exception;
   --
   -- Raised upon an attempt to read the value of an attribute within
   -- a debugging information entry, which does contain a value for
   -- this attribute,  but in a form that cannot be converted to the
   -- desired type of value.


   function Value_Def (
      Attribute : Attribute_T;
      Within    : Value_Def_List_T)
   return Value_Def_T;
   --
   -- The format and index of the desired attribute, if it is
   -- defined Within the list. Otherwise reports an Error and
   -- propagates Attribute_Absent.


   procedure Find (
      Attribute : in     Attribute_T;
      Within    : in     Value_Def_List_T;
      Found     :    out Boolean;
      Value_Def :    out Value_Def_T);
   --
   -- Find the format and index of the desired attribute, if it is
   -- defined Within the list, and returns in Value_Def. Otherwise
   -- return Found as False.


   --
   --    Blocks in DWARF streams
   --


   type Block_T is record
      Stream    : In_Memory.Stream_Ref;
      Start     : In_Memory.Index_T;
      Octets    : In_Memory.Count_T;
      Addr_Size : Addr_Size_T;
      Bits      : Bits_T;
   end record;
   --
   -- Describes an attribute of type "block" in a DWARF stream.
   --
   -- Stream
   --    The in-memory cope of the DWARF stream.
   -- Start
   --    The index of the start of the block (first data octet).
   -- Octets
   --    The total length of the block. The block comprises the
   --    Stream range Start .. Start - Octets - 1.
   -- Addr_Size, Bits
   --    Properties of the compilation unit that contains the block,
   --    necessary for interpreting the block contents.


end Formats.Dwarf;
