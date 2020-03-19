-- Formats.Dwarf.Text (body)
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
-- $Revision: 1.4 $
-- $Date: 2015/10/24 20:53:52 $
--
-- $Log: formats-dwarf-text.adb,v $
-- Revision 1.4  2015/10/24 20:53:52  niklas
-- Moved to free licence.
--
-- Revision 1.3  2013-02-03 10:50:28  niklas
-- BT-CH-0238: DWARF expression abstract evaluation - start.
--
-- Revision 1.2  2007-06-14 11:16:40  niklas
-- BT-CH-0060.
--
-- Revision 1.1  2004/04/24 18:06:21  niklas
-- First version.
--


with Ada.Strings.Fixed;
with Ada.Text_IO;


package body Formats.Dwarf.Text is


   use Ada.Text_IO;


   Margin : constant Positive_Count := 10;
   --
   -- The left margin for indented output.


   procedure Begin_Line (Indent : in String)
   is
   begin

      Set_Col (Margin);
      Put (Indent);

   end Begin_Line;


   procedure Put_Line (
      Indent : in String;
      Line   : in String)
   is
   begin

      Begin_Line (Indent);

      Ada.Text_IO.Put_Line (Line);

   end Put_Line;


   function Addr_Image (
      Info      : access Ada.Streams.Root_Stream_Type'Class;
      Addr_Size :        Addr_Size_T)
   return String
   is
   begin

      return Image (Address (Info, Addr_Size), Addr_Size);

   exception

   when Format_Error =>
      -- Addr_Size not supported.

      return "[address size not supported]";  -- TBA other Addr_Sizes.

   end Addr_Image;


   function Flag_Image (
      Info : access Ada.Streams.Root_Stream_Type'Class)
   return String
   --
   -- Textual form of an Flag value from the Info stream.
   --
   is

      Flag : constant Unsigned_8_T := Unsigned_8_T'Input (Info);
      -- The flag octet.

   begin

      if Flag = 0 then

         return "absent";

      else

         return "present (" & Unsigned_8_T'Image (Flag) & " )";

      end if;

   end Flag_Image;


   function Trim (Item : String) return String
   is
   begin

      return Ada.Strings.Fixed.Trim (Item, Ada.Strings.Both);

   end Trim;


   function Constant_Image (
      Info : access Ada.Streams.Root_Stream_Type'Class;
      Form :        Constant_Form_T)
   return String
   is
   begin

      case Form is

      when Sdata =>

         return Trim (LEB128.Longest_Signed_T'Image (
            LEB128.Longest_Signed_T'Input (Info)));

      when Udata =>

         return Trim (LEB128.Longest_Unsigned_T'Image (
            LEB128.Longest_Unsigned_T'Input (Info)));

      when others =>
         -- The Number type can hold all the values.

         return Trim (Number_T'Image (Number (Info, Form)));

      end case;

   end Constant_Image;


   procedure Dump_Block (
      Info   : access Ada.Streams.Root_Stream_Type'Class;
      Form   : in     Block_Form_T;
      Indent : in     String)
   is
      use type IO.Count;

      Length : IO.Count;
      -- The length of the block, in octets.

      Offset : IO.Count := 0;
      -- The offset of the current octet in the block.

      Octet : Octet_T;
      -- The current octet in the block.

      Octets_Per_Line : constant := 20;
      -- Number of octets dumped per line.

      Ind_Col : Positive_Count;
      -- The column number after Indent.

   begin

      -- Read the length, which is the only part where Form matters:

      case Form is

      when Block1 =>

         Length := IO.Count (Unsigned_8_T'Input (Info));

      when Block2 =>

         Length := IO.Count (Unsigned_16_T'Input (Info));

      when Block4 =>

         Length := IO.Count (Unsigned_32_T'Input (Info));

      when Block =>

         Length := IO.Count (LEB128.Longest_Unsigned_T'Input (Info));

      end case;

      Put_Line (
         "Block length"
        & IO.Count'Image (Length)
        & " octets:");

      while Offset < Length loop

         Octet_T'Read (Info, Octet);

         if Offset mod Octets_Per_Line = 0 then
            -- New line.

            Begin_Line (Indent);

            Ind_Col := Col;

            Put (IO.Count'Image (Offset));

            Set_Col (Ind_Col + 7);

            Put (':');

         end if;

         Put (' '); Put (Hex_Image (Octet));

         Offset := Offset + 1;

      end loop;

      Set_Col (1);

   end Dump_Block;


   function Reference_Image (
      Info : access Ada.Streams.Root_Stream_Type'Class;
      Form :        Reference_Form_T;
      Bits :        Bits_T;
      Base :        In_Memory.Index_T)
   return String
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

      return
           Section_Offset_T'Image (Offset)
         & " [base"
         & In_Memory.Index_T'Image (Origin)
         & "] ="
         & In_Memory.Index_T'Image (Origin + In_Memory.Offset_T (Offset));

   end Reference_Image;


   procedure Dump_Reference (
      Info   : access Ada.Streams.Root_Stream_Type'Class;
      Form   : in     Reference_Form_T;
      Bits   : in     Bits_T;
      Base   :        In_Memory.Index_T;
      Indent : in     String)
   --
   -- Dumps a reference value from the Info stream, using the given
   -- kind of reference Form, the given bit-width and the given
   -- Indentation string.
   --
   is
   begin

      Put_Line (Reference_Image (Info, Form, Bits, Base));

      -- TBA display the referee.

   end Dump_Reference;


   function String_Image (
      Info : access Ada.Streams.Root_Stream_Type'Class;
      Form : in     String_Form_T;
      Bits : in     Bits_T;
      Str  : in     In_Memory.Stream_Ref)
   return String
   --
   -- Textual form of an string value from the Info stream, using
   -- the given kind of string form and the given Bits width, and
   -- possibly taking the string itself from the Str section.
   --
   is
      use type In_Memory.Index_T;

      Str_Offset : Section_Offset_T;
      -- The offset in the .debug_str section, when Form = Strp.

   begin

      case Form is

      when String_Form =>

         return String_To_Null (Info);

      when Strp =>

         Read_Variant_Unsigned (
            Stream => Info,
            Bits   => Bits,
            Item   => Variant_Unsigned_T (Str_Offset));

         return
              ".debug_str["
            & Trim (Section_Offset_T'Image (Str_Offset))
            & "] = "
            & In_Memory.String_To_Null (
                 From  => Str,
                 Index => In_Memory.Index_T'First
                        + In_Memory.Offset_T (Str_Offset));

      end case;

   end String_Image;


   procedure Dump_Attribute (
      Info      : access Ada.Streams.Root_Stream_Type'Class;
      Attribute : in     Attribute_T;
      Form      : in     Form_T;
      Bits      : in     Bits_T;
      Addr_Size : in     Addr_Size_T;
      Ref_Base  : in     In_Memory.Index_T;
      Str       : in     In_Memory.Stream_Ref;
      Indent    : in     String)
   is
      use Ada.Text_IO;

      True_Form_Code : Form_Code_T;
      -- The code for the true form, if Form = Indirect.

      True_Form : Form_T;
      -- The form, after resolving a possible Indirect form.

   begin

      Begin_Line (Indent);

      Put (Image (Attribute)
         & " ["
         & Image (Form)
         & "]: ");

      -- Resolve form indirection:

      if Form = Indirect then
         -- The attribute value is prefixed with the true form:

         Read_LEB128_Natural (Info, Natural (True_Form_Code));

         True_Form := To_Form (True_Form_Code);

         if True_Form = Invalid_Code then

            Put (
                 "Form code"
               & Form_Code_T'Image (True_Form_Code)
               & "is invalid.");

         else

            Put ('[' & Image (True_Form) & ']');

         end if;

      else
         -- The attribute value has the given Form.

         True_Form := Form;

      end if;

      -- Display the attribute value:

      case True_Form is

      when Null_Form =>

         Put_Line ("(null form)");

      when Addr =>

         Put_Line (Addr_Image (Info, Addr_Size));

      when Flag =>

         Put_Line (Flag_Image (Info));

      when Constant_Form_T =>

         Put_Line (Constant_Image (Info, True_Form));

      when Block_Form_T =>

         Dump_Block (
            Info   => Info,
            Form   => True_Form,
            Indent => Indent & "   ");

      when Reference_Form_T =>

         Dump_Reference (
            Info   => Info,
            Form   => True_Form,
            Bits   => Bits,
            Base   => Ref_Base,
            Indent => Indent & "   ");

      when String_Form_T =>

         Put_Line (String_Image (Info, True_Form, Bits, Str));

      when Indirect =>

         Put_Line (" Multiple indirection not allowed!");

         raise Format_Error;

      when Invalid_Code =>

         New_Line;

         raise Format_Error;

      end case;

   end Dump_Attribute;


end Formats.Dwarf.Text;
