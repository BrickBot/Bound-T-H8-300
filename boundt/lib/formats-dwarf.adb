-- Formats.Dwarf (body)
--
-- Author: Niklas Holsti.
--
-- A component of the Bound-T Worst-Case Execution Time Tool.
--
-------------------------------------------------------------------------------
-- Copyright (c) 1999 .. 2015 Tidorum Ltd except for text copied verbatim
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
-- $Revision: 1.5 $
-- $Date: 2015/10/24 20:53:52 $
--
-- $Log: formats-dwarf.adb,v $
-- Revision 1.5  2015/10/24 20:53:52  niklas
-- Moved to free licence.
--
-- Revision 1.4  2008-10-15 12:05:53  niklas
-- BT-CH-0150: DWARF location lists, lexical block scopes, CUBAs.
--
-- Revision 1.3  2007/06/14 11:16:40  niklas
-- BT-CH-0060.
--
-- Revision 1.2  2006/04/12 19:45:58  niklas
-- Changed the function Rounded_Length to use the new option
-- Block_Alignment instead of a constant 4.
-- Added an Error report to the Value_Def function when the
-- attribute is missing.
-- Added procedure Find to find an attribute in a value list
-- without raising exceptions for missing attributes.
--
-- Revision 1.1  2004/04/24 18:06:22  niklas
-- First version.
--


with Ada.Characters.Handling;
with Formats.Dwarf.Opt;
with Hex;
with Output;


package body Formats.Dwarf is


   --
   --    Reading operations:
   --


   procedure Read_Initial_Length (
      Stream : access Ada.Streams.Root_Stream_Type'Class;
      Item   : out    Initial_Length_T)
   is

      Length_32 : Unsigned_32_T;
      -- The first four octets of the initial length field.
      -- The whole length in case of 32-bit DWARF.

   begin

      Unsigned_32_T'Read (Stream, Length_32);

      if Length_32 < 16#FFFF_FF00# then
         -- 32-bit variant.

         Item := (
            Bits   => Bits_32,
            Length => Unsigned_64_T (Length_32));

      elsif Length_32 = 16#FFFF_FFFF# then
         -- 64-bit variant.

         Item.Bits := Bits_64;

         Unsigned_64_T'Read (Stream, Item.Length);

      else
         -- The first 32 bits are invalid for DWARF.

         Output.Error (
            "DWARF ""initial length"" invalid");

         raise Format_Error;

      end if;

   end Read_Initial_Length;


   function Total_Length (Initial : Initial_Length_T)
   return IO.Positive_Count
   is
      use type IO.Positive_Count;
   begin

      return
           Initial_Length_Length (Initial.Bits)
         + IO.Positive_Count (Initial.Length);

   end Total_Length;


   function Image (Item : Initial_Length_T) return String
   is

      Length : constant String := Unsigned_64_T'Image (Item.Length);

   begin

      case Item.Bits is
      when Bits_32      => return Length & " (32-bit format)";
      when Bits_64      => return Length & " (64-bit format)";
      when Bits_Unknown => return Length & " (unknown format)";
      end case;

   end Image;


   function Rounded_Up (
      Count   : Unsigned_64_T;
      Modulus : Unsigned_64_T)
   return Unsigned_64_T
   is

      Rest : constant Unsigned_64_T := Count mod Modulus;

   begin

      if Rest = 0 then

         return Count;

      else

         return Count + (Modulus - Rest);

      end if;

   end Rounded_Up;


   function Rounded_Length (Initial : Initial_Length_T)
   return IO.Positive_Count
   is
      use type IO.Count;
   begin

      return
           Initial_Length_Length(Initial.Bits)
         + IO.Count (Rounded_Up (
              Count   => Initial.Length,
              Modulus => Unsigned_64_T (Opt.Block_Alignment)));
      --
      -- The need to round the length up to a multiple of Block_Alignment
      -- has been discovered empirically in DWARF V2 files from the
      -- ARM and IAR compilers for the ARM7; I can find no mention
      -- of this in the DWARF standard.

   end Rounded_Length;


   function Next_Index (
       From  : IO.Positive_Count;
       After : Initial_Length_T)
   return IO.Positive_Count
   is
      use type IO.Count;
   begin

      return From + Rounded_Length (After);

   end Next_Index;


   procedure Read_Address (
      Stream : access Ada.Streams.Root_Stream_Type'Class;
      Size   : in     Addr_Size_T;
      Item   :    out Address_T)
   is
   begin

      case Size is

      when 2 => Unsigned_16_T'Read (Stream, Unsigned_16_T (Item));

      when 4 => Unsigned_32_T'Read (Stream, Unsigned_32_T (Item));

      when others =>

         Output.Fault (
            Location => "Formats.Dwarf.Read_Address",
            Text     =>
                 "Address size"
               & Addr_Size_T'Image (Size)
               & " not supported.");

         raise Format_Error;

      end case;

   end Read_Address;


   function Address (
      Stream : access Ada.Streams.Root_Stream_Type'Class;
      Size   :        Addr_Size_T)
   return Address_T
   is
      Result : Address_T;
   begin

      Read_Address (Stream, Size, Result);

      return Result;

   end Address;


   function Image (Item : Address_T; Size : Addr_Size_T) return String
   is
   begin

      return
         Hex.Image (
            Value  => Hex.Word64_T (Item),
            Octets => Positive (Size));

   end Image;


   procedure Read_Variant_Unsigned (
      Stream : access Ada.Streams.Root_Stream_Type'Class;
      Bits   : in     Bits_T;
      Item   :    out Variant_Unsigned_T)
   is
   begin

      case Bits is

      when Bits_32 => Unsigned_32_T'Read (Stream, Unsigned_32_T (Item));

      when Bits_64 => Unsigned_64_T'Read (Stream, Item);

      when Bits_Unknown =>

         Output.Fault (
            Location => "Formats.Dwarf.Read_Variant_Unsigned",
            Text     => "DWARF number of bits is unknown");

         raise Format_Error;

      end case;

   end Read_Variant_Unsigned;


   procedure Read_Boolean (
      Stream : access Ada.Streams.Root_Stream_Type'Class;
      Item   : out    Boolean_T)
   is
      Octet : Unsigned_8_T;
   begin

      Unsigned_8_T'Read (Stream, Octet);

      Item := Boolean_T (Octet /= 0);

   end Read_Boolean;


   procedure Read_LEB128_Integer (
      Stream : access Ada.Streams.Root_Stream_Type'Class;
      Item   : out    Integer)
   is
   begin

      LEB128.Read_Longest_Signed (
         Stream => Stream,
         Item   => LEB128.Longest_Signed_T (Item));

   end Read_LEB128_Integer;


   procedure Read_LEB128_Natural (
      Stream : access Ada.Streams.Root_Stream_Type'Class;
      Item   : out    Natural)
   is
   begin

      LEB128.Read_Longest_Unsigned (
         Stream => Stream,
         Item   => LEB128.Longest_Unsigned_T (Item));

   end Read_LEB128_Natural;


   --
   --    Tags for debugging information entries
   --


   Tag_Coding : constant array (Standard_Tag_Code_T) of Tag_T := (

      16#00# => Null_Tag,
      16#01# => Array_Type,
      16#02# => Class_Type,
      16#03# => Entry_Point,
      16#04# => Enumeration_Type,
      16#05# => Formal_Parameter,
      16#06# => Invalid_Code,
      16#07# => Invalid_Code,
      16#08# => Imported_Declaration,
      16#09# => Invalid_Code,
      16#0a# => Label,
      16#0b# => Lexical_Block,
      16#0c# => Invalid_Code,
      16#0d# => Member,
      16#0e# => Invalid_Code,
      16#0f# => Pointer_Type,

      16#10# => Reference_Type,
      16#11# => Compile_Unit,
      16#12# => String_Type,
      16#13# => Structure_Type,
      16#14# => Invalid_Code,
      16#15# => Subroutine_Type,
      16#16# => Typedef,
      16#17# => Union_Type,
      16#18# => Unspecified_Parameters,
      16#19# => Variant,
      16#1a# => Common_Block,
      16#1b# => Common_Inclusion,
      16#1c# => Inheritance,
      16#1d# => Inlined_Subroutine,
      16#1e# => Module,
      16#1f# => Ptr_To_Member_Type,

      16#20# => Set_Type,
      16#21# => Subrange_Type,
      16#22# => With_Stmt,
      16#23# => Access_Declaration,
      16#24# => Base_Type,
      16#25# => Catch_Block,
      16#26# => Const_Type,
      16#27# => Constant_Value,
      16#28# => Enumerator,
      16#29# => File_Type,
      16#2a# => Friend,
      16#2b# => Namelist,
      16#2c# => Namelist_Item,
      16#2d# => Packed_Type,
      16#2e# => Subprogram,
      16#2f# => Template_Type_Parameter,

      16#30# => Template_Value_Parameter,
      16#31# => Thrown_Type,
      16#32# => Try_Block,
      16#33# => Variant_Part,
      16#34# => Variable,
      16#35# => Volatile_Type,
      16#36# => Dwarf_Procedure,
      16#37# => Restrict_Type,
      16#38# => Interface_Type,
      16#39# => Namespace,
      16#3a# => Imported_Module,
      16#3b# => Unspecified_Type,
      16#3c# => Partial_Unit,
      16#3d# => Imported_Unit,
      16#3e# => Mutable_Type);
   --
   -- The mapping from (standard) tag-code to tag.


   function To_Tag (Code : Tag_Code_T) return Tag_T
   is
   begin

      if Code in Tag_Coding'Range then

         return Tag_Coding(Code);

      elsif Code in Vendor_Tag_Code_T then

         return Vendor_Extension;

      else

         return Invalid_Code;

      end if;

   end To_Tag;


   function Image (Item : Tag_T) return String
   is
   begin

      return Tag_T'Image (Item);

   end Image;


   --
   --    Attributes for debugging information entries
   --


   Attribute_Coding :
      constant array (Standard_Attribute_Code_T) of Attribute_T := (

      16#00# => Null_Attribute,
      16#01# => Sibling,
      16#02# => Location,
      16#03# => Name,
                                 16#04# .. 16#08# => Invalid_Code,
      16#09# => Ordering,
                                 16#0a# => Invalid_Code,
      16#0b# => Byte_Size,
      16#0c# => Bit_Offset,
      16#0d# => Bit_Size,
                                 16#0e# .. 16#0f# => Invalid_Code,
      16#10# => Stmt_List,
      16#11# => Low_PC,
      16#12# => High_PC,
      16#13# => Language,
                                 16#14# => Invalid_Code,
      16#15# => Discr,
      16#16# => Discr_Value,
      16#17# => Visibility,
      16#18# => Import,
      16#19# => String_Length,
      16#1a# => Common_Reference,
      16#1b# => Comp_Dir,
      16#1c# => Const_Value,
      16#1d# => Containing_Type,
      16#1e# => Default_Value,
                                 16#1f# => Invalid_Code,
      16#20# => Inline,
      16#21# => Is_Optional,
      16#22# => Lower_Bound,
                                 16#23# .. 16#24# => Invalid_Code,
      16#25# => Producer,
                                 16#26# => Invalid_Code,
      16#27# => Prototyped,
                                 16#28# .. 16#29# => Invalid_Code,
      16#2a# => Return_Addr,
                                 16#2b# => Invalid_Code,
      16#2c# => Start_Scope,
                                 16#2d# => Invalid_Code,
      16#2e# => Stride_Size,
      16#2f# => Upper_Bound,
                                 16#30# => Invalid_Code,
      16#31# => Abstract_Origin,
      16#32# => Accessibility,
      16#33# => Address_Class,
      16#34# => Artificial,
      16#35# => Base_Types,
      16#36# => Calling_Convention,
      16#37# => Count,
      16#38# => Data_Member_Location,
      16#39# => Decl_Column,
      16#3a# => Decl_File,
      16#3b# => Decl_Line,
      16#3c# => Declaration,
      16#3d# => Discr_List,
      16#3e# => Encoding,
      16#3f# => External,
      16#40# => Frame_Base,
      16#41# => Friend,
      16#42# => Identifier_Case,
      16#43# => Macro_Info,
      16#44# => Namelist_Item,
      16#45# => Priority,
      16#46# => Segment,
      16#47# => Specification,
      16#48# => Static_Link,
      16#49# => Type_Ref,    -- DW_AT_type
      16#4a# => Use_Location,
      16#4b# => Variable_Parameter,
      16#4c# => Virtuality,
      16#4d# => Vtable_Elem_Location,
      16#4e# => Allocated,
      16#4f# => Associated,
      16#50# => Data_Location,
      16#51# => Stride,
      16#52# => Entry_PC,
      16#53# => Use_UTF8,
      16#54# => Extension,
      16#55# => Ranges,
      16#56# => Trampoline,
      16#57# => Call_Column,
      16#58# => Call_File,
      16#59# => Call_Line,
      16#5a# => Description);
   --
   -- The mapping from (standard) attribute-code to attribute.


   function To_Attribute (Code : Attribute_Code_T) return Attribute_T
   is
   begin

      if Code in Attribute_Coding'Range then

         return Attribute_Coding(Code);

      elsif Code in Vendor_Attribute_Code_T then

         return Vendor_Extension;

      else

         return Invalid_Code;

      end if;

   end To_Attribute;


   function Image (Item : Attribute_T) return String
   is
      use Ada.Characters.Handling;

      Crude : constant String := Attribute_T'Image (Item);

   begin

      return Crude(Crude'First)
         & To_Lower (Crude(Crude'First + 1 .. Crude'Last));

   end Image;



   --
   --    Attribute value forms
   --


   Form_Coding : constant array (Standard_Form_Code_T) of Form_T := (
      16#00# => Null_Form,
      16#01# => Addr,
                                 16#02# => Invalid_Code,
      16#03# => Block2,
      16#04# => Block4,
      16#05# => Data2,
      16#06# => Data4,
      16#07# => Data8,
      16#08# => String_Form,  -- DW_FORM_string
      16#09# => Block,
      16#0a# => Block1,
      16#0b# => Data1,
      16#0c# => Flag,
      16#0d# => Sdata,
      16#0e# => Strp,
      16#0f# => Udata,
      16#10# => Ref_Addr,
      16#11# => Ref1,
      16#12# => Ref2,
      16#13# => Ref4,
      16#14# => Ref8,
      16#15# => Ref_Udata,
      16#16# => Indirect);
   --
   -- The mapping from (standard) form-code to form.


   function To_Form (Code : Form_Code_T) return Form_T
   is
   begin

      if Code in Form_Coding'Range then

         return Form_Coding(Code);

      else

         return Invalid_Code;

      end if;

   end To_Form;


   function Image (Item : Form_T) return String
   is
   begin

      return Ada.Characters.Handling.To_Lower (Form_T'Image (Item));

   end Image;


   --
   --    Attribute values of known form from DWARF streams
   --


   function Number (
      Info : access Ada.Streams.Root_Stream_Type'Class;
      Form :        Constant_Form_T)
   return Number_T
   is
   begin

      case Form is

      when Data1 =>

         return Number_T (Unsigned_8_T'Input (Info));

      when Data2 =>

         return Number_T (Unsigned_16_T'Input (Info));

      when Data4 =>

         return Number_T (Unsigned_32_T'Input (Info));

      when Data8 =>

         return Number_T (Unsigned_64_T'Input (Info));

      when Data1s =>

         return Number_T (Signed_8_T'Input (Info));

      when Data2s =>

         return Number_T (Signed_16_T'Input (Info));

      when Data4s =>

         return Number_T (Signed_32_T'Input (Info));

      when Data8s =>

         return Number_T (Signed_64_T'Input (Info));

      when Sdata =>

         return Number_T (LEB128.Longest_Signed_T'Input (Info));

      when Udata =>

         return Number_T (LEB128.Longest_Unsigned_T'Input (Info));

      end case;

   end Number;


   function Reference (
      Info   : access Ada.Streams.Root_Stream_Type'Class;
      Form   :        Reference_Form_T;
      Bits   :        Bits_T)
   return Section_Offset_T
   is

      Offset : Section_Offset_T;
      -- The offset value that identifies the referee.
      -- For Ref1 .. Ref_Udata, this is an offset within the Info
      -- of the current compilation unit, relative to the first
      -- octet of the compilation unit header.
      -- For Ref_Addr, this is an offset within the ".debug_info"
      -- section from the beginning of the section, and can thus
      -- refer to another compilation unit. It can also refer to
      -- another shared object or static executable, in a TBD way.

   begin

      -- Get the Offset, depending on the Form:

      case Form is

      when Ref1 =>

         Offset := Section_Offset_T (Unsigned_8_T'Input (Info));

      when Ref2 =>

         Offset := Section_Offset_T (Unsigned_16_T'Input (Info));

      when Ref4 =>

         Offset := Section_Offset_T (Unsigned_32_T'Input (Info));

      when Ref8 =>

         Offset := Section_Offset_T (Unsigned_64_T'Input (Info));

      when Ref_Udata =>

         Offset := Section_Offset_T (
            LEB128.Longest_Unsigned_T'Input (Info));

      when Ref_Addr =>

         Read_Variant_Unsigned (
            Stream => Info,
            Bits   => Bits,
            Item   => Variant_Unsigned_T (Offset));

      end case;

      return Offset;

   end Reference;


   function Reference (
      Info : access Ada.Streams.Root_Stream_Type'Class;
      Form :        Reference_Form_T;
      Bits :        Bits_T;
      Base :        In_Memory.Index_T)
   return In_Memory.Index_T
   is
      use type In_Memory.Index_T;

      Offset : constant Section_Offset_T := Reference (Info, Form, Bits);
      -- The reference as an offset to some base index (Info position)
      -- that depends on the Form.

      Origin : In_Memory.Index_T;
      -- The origin for the offset.

   begin

      case Form is

      when Ref1 .. Ref_Udata =>
         -- The base is the Base.

         Origin := Base;

      when Ref_Addr =>
         -- The base is the start of the debug Info stream.

         Origin := In_Memory.Index_T'First;

      end case;

      return Origin + In_Memory.Offset_T (Offset);

   end Reference;


   --
   --    Attribute values embedded in DWARF streams
   --


   procedure Read_True_Form (
      Stream : access Ada.Streams.Root_Stream_Type'Class;
      Item   : out    True_Form_T)
   is

      True_Form_Code : Form_Code_T;
      -- The code for the true form.

      Form : Form_T;
      -- The form corresponding to True_Form_Code.

   begin

      Read_LEB128_Natural (Stream, Natural (True_Form_Code));

      Form := To_Form (True_Form_Code);

      if Form in True_Form_T then

         Item := Form;

      else

         Output.Error (
              "Invalid DWARF true form code"
            & Form_Code_T'Image (True_Form_Code));

         raise Format_Error;

      end if;

   end Read_True_Form;


   function True_Form (
      Given : Form_T;
      From  : access Ada.Streams.Root_Stream_Type'Class)
   return True_Form_T
   is

      Result : True_Form_T;
      -- The result.

   begin

      if Given in True_Form_T then

         Result := Given;

      else

         Read_True_Form (Stream => From, Item => Result);

      end if;

      return Result;

   end True_Form;


   function Block_Length (
      Form   : Block_Form_T;
      Stream : access Ada.Streams.Root_Stream_Type'Class)
   return In_Memory.Count_T
   is

      Len : In_Memory.Count_T;
      -- The result.

   begin

      case Form is

      when Block1 =>

         Len := In_Memory.Count_T (Unsigned_8_T'Input (Stream));

      when Block2 =>

         Len := In_Memory.Count_T (Unsigned_16_T'Input (Stream));

      when Block4 =>

         Len := In_Memory.Count_T (Unsigned_32_T'Input (Stream));

      when Block =>

         Read_LEB128_Natural (Stream, Natural (Len));

      end case;

      return Len;

   end Block_Length;


   function Value_Def (
      Attribute : Attribute_T;
      Within    : Value_Def_List_T)
   return Value_Def_T
   is
   begin

      for W in Within'Range loop

         if Within(W).Attribute = Attribute then

            return Within(W);

         end if;

      end loop;

      if Opt.Trace_Loading then

         Output.Trace (
              "DWARF attribute "
             & Attribute_T'Image (Attribute)
             & " missing.");

      end if;

      raise Attribute_Absent;

   end Value_Def;


   procedure Find (
      Attribute : in     Attribute_T;
      Within    : in     Value_Def_List_T;
      Found     :    out Boolean;
      Value_Def :    out Value_Def_T)
   is
   begin

      for W in Within'Range loop

         if Within(W).Attribute = Attribute then
            -- Success.

            Found     := True;
            Value_Def := Within(W);

            return;

         end if;

      end loop;

      -- Failure.

      Found := False;

      Value_Def := (
         Attribute => Null_Attribute,
         Form      => True_Form_T'First,
         Index     => In_Memory.Index_T'First);

   end Find;


end Formats.Dwarf;
