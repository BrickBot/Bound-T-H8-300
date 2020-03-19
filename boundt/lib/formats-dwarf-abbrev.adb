-- Formats.Dwarf.Abbrev (body)
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
-- $Revision: 1.8 $
-- $Date: 2015/10/24 20:53:52 $
--
-- $Log: formats-dwarf-abbrev.adb,v $
-- Revision 1.8  2015/10/24 20:53:52  niklas
-- Moved to free licence.
--
-- Revision 1.7  2009-01-18 08:37:02  niklas
-- Removed unused context clause.
--
-- Revision 1.6  2008/10/15 12:05:52  niklas
-- BT-CH-0150: DWARF location lists, lexical block scopes, CUBAs.
--
-- Revision 1.5  2007/06/14 11:16:39  niklas
-- BT-CH-0060.
--
-- Revision 1.4  2007/06/05 13:53:26  niklas
-- Modified Read_Enty to increase the maximum number of attribute pairs
-- per abbreviation from 200 to 600. Some GNAT ERC32 code needs this.
--
-- Revision 1.3  2007/01/25 21:25:35  niklas
-- BT-CH-0043.
--
-- Revision 1.2  2006/04/12 19:28:15  niklas
-- Added the component View_T.Str for getting attribute values
-- of Form = Strp from the .debug_str section.
-- Added corresponding "in" parameter View.Str.
--
-- Revision 1.1  2004/04/24 18:06:18  niklas
-- First version.
--


with Ada.Text_IO;
with Formats.Dwarf.Expressions;
with Formats.Dwarf.Opt;
with Formats.Dwarf.Text;
with Formats.In_Memory;
with Output;


package body Formats.Dwarf.Abbrev is


   procedure Read_Code (
      Stream : access Ada.Streams.Root_Stream_Type'Class;
      Item   :    out Code_T)
   is
   begin

      Read_LEB128_Natural (Stream, Natural (Item));

   end Read_Code;


   --
   --    Abbreviations (templates) for debugging information entries
   --


   type Form_Length_T is array (Valid_Form_T) of In_Memory.Count_T;
   --
   -- Holds the fixed length, in octets, of a value encoded in a given
   -- form, with zero for forms with a variable-length encoding.
   --
   -- This table can be slightly different for different compilation
   -- units because the lengths of the forms Addr, Ref_Addr and Strp
   -- depend on the Addr_Size and the bit-width of the compilation.


   Standard_Form_Length : constant Form_Length_T := (
      Addr         => 0,   -- Addr_Size of compilation.
      Flag         => 1,
      Data1        => 1,
      Data2        => 2,
      Data4        => 4,
      Data8        => 8,
      Data1s       => 1,
      Data2s       => 2,
      Data4s       => 4,
      Data8s       => 8,
      Sdata        => 0,
      Udata        => 0,
      Block1       => 0,
      Block2       => 0,
      Block4       => 0,
      Block        => 0,
      Ref1         => 1,
      Ref2         => 2,
      Ref4         => 4,
      Ref8         => 8,
      Ref_Udata    => 0,
      Ref_Addr     => 0,   -- Bit-width of compilation.
      String_Form  => 0,
      Strp         => 0,   -- Bit-width of compilation.
      Indirect     => 0);
   --
   -- The standard form-length table, to be customized for each
   -- compilation as follows:
   --
   -- > The length of an Addr form is the Addr_Size of the compilation.
   --
   -- > The lenth of the Ref_Addr and Strp forms is 4 octets for 32-bit
   --   DWARF or 8 octets for 64-bit DWARF.


   procedure Read_Att_Form (
      Stream : access Ada.Streams.Root_Stream_Type'Class;
      Item   : out    Att_Form_T)
   is

      Attr_Code : Attribute_Code_T;
      Form_Code : Form_Code_T;

   begin

      Read_LEB128_Natural (Stream, Natural (Attr_Code));
      Read_LEB128_Natural (Stream, Natural (Form_Code));

      Item := (
         Attribute  => To_Attribute (Attr_Code),
         Form       => To_Form      (Form_Code));

   end Read_Att_Form;


   function Image (Item : Att_Form_T) return String
   is
   begin

      return
           Image (Item.Attribute)
         & " : "
         & Image (Item.Form);

   end Image;


   procedure Read_Att_Form_List (
      Stream : access Ada.Streams.Root_Stream_Type'Class;
      Item   : out    Att_Form_List_T;
      Last   : out    Natural)
   --
   -- Reads a list of Att_Form_T elements from the Stream, storing
   -- it in the given list as Item(1 .. Last).
   --
   is

      Pair : Att_Form_T;
      -- One of the atttribute-and-form pairs.

   begin

      Last := 0;

      loop

         Att_Form_T'Read (Stream, Pair);

         exit when Pair.Attribute = Null_Attribute;
         -- That is the end of the list.
         -- It should also have Pair.Form = Null_Form.

         Last := Last + 1;

         if Last <= Item'Last then

            Item(Last) := Pair;

         end if;

      end loop;

      if Last > Item'Last then

         Output.Fault (
            Location => "Formats.Dwarf.Tree.Read_Att_Form_List",
            Text =>
                 "DWARF abbreviation contains"
               & Natural'Image (Last)
               & " attributes; maximum supported is"
               & Natural'Image (Item'Length));

         Last := Item'Last;

      end if;

   end Read_Att_Form_List;


   type Fixed_Layout_T is record
      Fix_Offset : In_Memory.Count_T;
      Prev_Flex  : Natural;
   end record;
   --
   -- The fixed (not dependent on attribute values) information about
   -- the position of an attribute value within a debugging information
   -- entry (DIE).
   --
   -- Fix_Offset
   --    The fixed part of the offset of the first octet of the attribute
   --    value, relative to the start of the first attribute value in this
   --    DIE, which is the first octet after the abbreviation code.
   --    Fix_Offset includes only the offset due to those earlier attributes
   --    that have values of fixed length, e.g. Data1, but not those that
   --    have variable or flexible length, e.g. Udata. It is the sum of the
   --    encoding lengths of the preceding attributes with fixed-length
   --    encoding.
   -- Prev_Flex
   --    The index, in the list of attribute forms for this abbreviation,
   --    of the closest preceding attribute form that has a flexible
   --    length. Zero if all preceding attribute forms have a fixed length,
   --    in which case Fix_Offset is the true offset of the value of
   --    this attribute.


   function Image (Item : Fixed_Layout_T) return String
   --
   -- Readable presentation of the Item.
   --
   is
   begin

      return "(fixed offset ="
         & In_Memory.Count_T'Image (Item.Fix_Offset)
         & ", prev flex ="
         & Natural'Image (Item.Prev_Flex)
         & ')';

   end Image;


   type Fixed_Layout_List_T is array (Natural range <>) of Fixed_Layout_T;
   --
   -- A list of fixed layout information, typically associated with an
   -- Att_Form_List_T with, however, a shift in the indexing as
   -- explained for Enty_T below.


   type Enty_T (Num_Attributes : Natural) is record
      Code       : Code_T := 0;
      Tag_Code   : Tag_Code_T;
      Children   : Boolean;
      Att_Forms  : Att_Form_List_T (1 .. Num_Attributes);
      Layout     : Fixed_Layout_List_T (0 .. Num_Attributes);
   end record;
   --
   -- An abbreviation entry, which is a mold or template for each
   -- debugging information entry (info entry) that is an instance
   -- of this abbreviation.
   --
   -- Num_Attributes
   --    The number of attributes defined in the abbreviation and
   --    for which each debugging information entry will provide
   --    values.
   -- Code
   --    The code that identifies this abbreviation among all the
   --    the abbreviations within the same compilation unit.
   --    If Code = 0, this is an undefined abbreviation entry and
   --    all the other components of this Abbrev_T are meaningless and
   --    undefined.
   -- Tag_Code
   --    Encodes the tag of the info entry.
   -- Children
   --    Whether the info entry has child entries.
   --    Note that even if Children is True, the list of child entries
   --    may still be empty (the first child may be a null entry).
   -- Att_Forms
   --    The list of attributes provided in the info entry, the form
   --    in which the attribute values are encoded, and information for
   --    computing the location of each attribute's value in the entry.
   -- Layout
   --    The list of fixed layout information for this info entry.
   --    The slice Layout(1 .. Num_Attributes) defines the fixed part
   --    of the layout for Att_Forms(1 .. Num_Attributes).
   --    The element Layout(0) stands for the position of a notional
   --    attribute that comes after Att_Forms'Last. Thus, the Fix_Offset
   --    component of this element is the sum of the encoding lengths of
   --    all the fixed-length attributes, and the Prev_Flex component
   --    is the index of the last variable-length attribute in Att_Forms
   --    and Layout (or zero if all attributes are fixed-length).


   procedure Read_Enty (
      Stream : access Ada.Streams.Root_Stream_Type'Class;
      Item   : out    Enty_Ref)
   is

      Max_Attributes : constant := 600;
      --
      -- Maximum number of attributes pairs for one abbreviation entry.

      Code : Code_T;
      -- The abbreviation code.

      Tag_Code : Tag_Code_T;
      -- The tag-code for the abbreviation.

      Children_Code : Unsigned_8_T;
      -- The code octet for the presence of children.

      Children : Boolean;
      -- Whether children are (or may be) present.

      Att_Forms : Att_Form_List_T (1 .. Max_Attributes);
      Num_Attributes : Natural;
      -- The attribute-form pairs are Att_Forms(1 .. Num_Attributes).

   begin

      Read_LEB128_Natural (Stream, Natural (Code));

      if Code = 0 then
         -- A null abbreviation entry.
         -- This is the end of the sequence of abbreviations for
         -- one compilation unit.

         Item := null;

      else
         -- A real abbreviation entry.

         Read_LEB128_Natural (Stream, Natural (Tag_Code));

         Unsigned_8_T'Read (Stream, Children_Code);

         case Children_Code is

            when 0 =>
               -- DW_CHILDREN_no

               Children := False;

            when 1 =>
               -- DW_CHILDREN_yes

               Children := True;

            when others =>

               Output.Error (
                    "DWARF abbreviation entry has invalid value"
                  & Unsigned_8_T'Image (Children_Code)
                  & " for the 'children' property.");

               Children := False;

         end case;

         Read_Att_Form_List (
            Stream => Stream,
            Item   => Att_Forms,
            Last   => Num_Attributes);

         Item := new Enty_T'(
            Num_Attributes => Num_Attributes,
            Code           => Code,
            Tag_Code       => Tag_Code,
            Children       => Children,
            Att_Forms      => Att_Forms(1 .. Num_Attributes),
            Layout         => (others => (0,0)));

      end if;

   end Read_Enty;


   procedure Put (Item : in Enty_T)
   --
   -- Displays the given abbreviation entry on standard output.
   --
   is
      use Ada.Text_IO;
   begin

      Put_Line (
           "   Code"
         & Code_T'Image (Item.Code)
         & ", tag"
         & Tag_Code_T'Image (Item.Tag_Code)
         & " ("
         & Image (To_Tag (Item.Tag_Code))
         & "), children = "
         & Boolean'Image (Item.Children)
         & ", attributes ="
         & Natural'Image (Item.Num_Attributes));

      for A in Item.Att_Forms'Range loop

         Put_Line (
              "      ["
            & Positive'Image (A)
            & " ]  "
            & Image (Item.Att_Forms(A))
            & ' '
            & Image (Item.Layout(A)));

      end loop;

      Put_Line (
           "      Overall "
         & Image (Item.Layout(0)));

      New_Line;

   end Put;


   function Code (Enty : Enty_Ref) return Code_T
   is
   begin

      return Enty.Code;

   end Code;


   function Tag_Code (Enty : Enty_Ref) return Tag_Code_T
   is
   begin

      return Enty.Tag_Code;

   end Tag_Code;


   function Number_Of_Attributes (Enty : Enty_Ref) return Natural
   is
   begin

      return Enty.Num_Attributes;

   end Number_Of_Attributes;


   function Attributes (Enty : Enty_Ref) return Att_Form_List_T
   is
   begin

      return Enty.Att_Forms;

   end Attributes;


   procedure Compute_Fixed_Layout (
      Length : in     Form_Length_T;
      Enty   : in out Enty_T)
   --
   -- Computes the fixed layout information for the given
   -- abbreviation Enty using the given Length of each form.
   --
   is
      use type In_Memory.Count_T;

      Next : Fixed_Layout_T := (
         Fix_Offset => 0,
         Prev_Flex  => 0);
      -- The layout info for the next attribute.

      Len : In_Memory.Count_T;
      -- The length of the encoding for the next attribute.
      -- Zero if variable.

   begin

      for A in Enty.Att_Forms'Range loop

         Enty.Layout(A) := Next;

         Len := Length(Enty.Att_Forms(A).Form);

         if Len > 0 then
            -- Fixed-length encoding.

            Next.Fix_Offset := Next.Fix_Offset + Len;

         else
            -- Variable-length encoding.

            Next.Prev_Flex := A;

         end if;

      end loop;

      Enty.Layout(0) := Next;

   end Compute_Fixed_Layout;


   function Length_Of (
      Value  : Att_Form_T;
      Within : In_Memory.Stream_Ref;
      Index  : In_Memory.Index_T;
      Length : Form_Length_T)
   return In_Memory.Count_T
   --
   -- The total length, in octets, of the encoded Value of an attribute
   -- Within an in-memory stream. The starting Index of the value is
   -- also given, as is the table of form Lengths. The current position
   -- of the stream may be affected if something must be read from
   -- the stream for a variable-length encoding. The initial stream
   -- position is irrelevant and the final stream position is undefined.
   --
   is
      use type In_Memory.Count_T;

      -- Principles of Operation
      --
      -- The encoded attribute value, and thus the length of the
      -- value, is composed of up to four parts as follows, where
      -- most parts can be present or absent depending on the form:
      --
      -- 1. An indirect form code (if Value.Form = Indirect).
      -- 2. An encoded length value (for Block_Form_T).
      -- 3. The encoded attribute value itself (always).
      -- 4. A terminating null octet (for String_Form).
      --
      -- We try to use the actual stream as little as possible.

      True_Form : True_Form_T;
      -- The true form of the value, after resolving Indirection.

      Indirect_Form_Len : In_Memory.Count_T := 0;
      -- The length of the indirect form-code, if Value.Form = Indirect.
      -- Initial value assumes no indirection.

      True_Index : In_Memory.Index_T;
      -- The stream index after the indirect form-code.
      -- Same as Index if there is no indirection.
      -- Defined only when for variable-length encodings.

      Len : In_Memory.Count_T;
      -- The length of the value, excluding indirect form-code
      -- if any.

      Dummy : LEB128.Longest_Unsigned_T;
      -- Some LEB128 number to be ignored.

   begin

      -- Resolve Form indirection:

      if Value.Form = Indirect then
         -- The attribute value is prefixed with the true form:

         In_Memory.Set_Index (Stream => Within.all, To => Index);

         Read_True_Form (
            Stream => Within,
            Item   => True_Form);

         True_Index := In_Memory.Index (Within.all);

         Indirect_Form_Len := True_Index - Index;
         -- The number of octets in the LEB128 encoding of
         -- the true form code.

      else
         -- The attribute value has the given fixed Form.

         True_Form := Value.Form;

      end if;

      -- Check for fixed or variable length:

      Len := Length(True_Form);

      if Len = 0 then
         -- Variable-length encoding; must read the stream.

         if Value.Form /= Indirect then
            -- We have not yet read from the stream.

            In_Memory.Set_Index (Stream => Within.all, To => Index);

            True_Index := Index;

         end if;

         case True_Form is

         when Sdata | Udata | Ref_Udata =>
            -- The value is encoded as one LEB128 number.

            LEB128.Read_Longest_Unsigned (Within, Dummy);

            Len := In_Memory.Index (Within.all) - True_Index;

         when Block_Form_T =>

            Len := Block_Length (Form => True_Form, Stream => Within);

            Len := Len + In_Memory.Index (Within.all) - True_Index;

         when String_Form =>

            loop
               exit when Unsigned_8_T'Input (Within) = 0;
            end loop;

            Len := In_Memory.Index (Within.all) - True_Index;

         when others =>

            Output.Fault (
               Location => "Formats.Dwarf.Tree.Length_Of",
               Text     =>
                    "Form "
                  & Image (True_Form)
                  & " has unknown variable encoding length.");

            raise Format_Error;

         end case;

      end if;

      return Indirect_Form_Len + Len;

   end Length_Of;


   --
   --    Abbreviation tables, indexed by abbreviation codes
   --


   type Table_T is array (Code_T range <>) of Enty_Ref;
   --
   -- A table of (references to) info-entry abbreviations (templates)
   -- as read from the ".debug_abbrev" section.


   type Table_Ref is access Table_T;
   --
   -- Refers to a table of info-entry abbrevations in heap memory.


   procedure Read_Table (
      Stream : access Ada.Streams.Root_Stream_Type'Class;
      Item   :    out Table_Ref);
   --
   -- Reads a set (table) of info-entry abbreviations (templates)
   -- from the given Stream and stores it in heap memory.
   --
   for Table_Ref'Read use Read_Table;


   procedure Read_Table (
      Stream : access Ada.Streams.Root_Stream_Type'Class;
      Item   :    out Table_Ref)
   --
   -- Reads a set (table) of info-entry abbreviations (templates)
   -- from the given Stream.
   --
   is

      Max_Code : constant Code_T := 1_000;
      -- We assume that no DWARF producer will generate abbrevations
      -- with a Code value greater than this.

      Table : Table_T (0 .. Max_Code);
      -- Buffer into which abbrevations are read.
      -- All elements are default initialized to Code = 0, which
      -- means "undefined abbreviation".

      Last_Code : Code_T := 0;
      -- The last (largest) defined abbreviation code in Table.
      -- The significant slice is Table(0 .. Last_Code).

      Enty : Enty_Ref;
      -- A reference to an abbreviation entry.

   begin

      loop

         Enty_Ref'Read (Stream, Enty);

         exit when Enty = null;

         -- Put (Enty);

         if Enty.Code in Table'Range then

            Table(Enty.Code) := Enty;

            Last_Code := Code_T'Max (Last_Code, Enty.Code);

         else
            -- Blast, the producer uses large codes.

            Output.Fault (
               Location => "Formats.Dwarf.Abbrev.Read_Table",
               Text =>
                    "Abbreviation code"
                  & Code_T'Image (Enty.Code)
                  & " is too large; maximum is"
                  & Code_T'Image (Table'Last));

         end if;

      end loop;

      Item := new Table_T'(Table(0 .. Last_Code));

   end Read_Table;


   procedure Put (Item : in Table_T)
   --
   -- Displays the abbreviation table on standard output.
   --
   is
      use Ada.Text_IO;

      Null_Entries : Natural := 0;
      -- Number of null (Code = 0) entries.

   begin

      Put_Line (
           "DWARF abbreviation table with"
         & Natural'Image (Item'Length)
         & " entries, including nulls:");

      New_Line;

      for I in Item'Range loop

         if Item(I)/= null then

            Put (Item(I).all);

         elsif I > 0 then

            Null_Entries := Null_Entries + 1;

         end if;

      end loop;

      Put_Line (
           "end of DWARF abbreviation table (there were"
         & Natural'Image (Null_Entries)
         & " null entries).");

      New_Line;

   end Put;


   type Set_T is record
      Key    : Set_Key_T;
      Table  : Table_Ref;
      Length : Form_Length_T;
   end record;
   --
   -- A set of info-entry abbreviations (templates) as read from the
   -- ".debug_abbrev" section starting at a given Key.Offset and using
   -- given values for Key.Addr_Size and Key.Bits. The Key identifies
   -- the set uniquely. Each compilation unit that uses this set refers
   -- to it by the Offset value; we add the Addr_Size and Bits to make
   -- up the whole key.
   --
   -- Key
   --    The identifying key.
   -- Table
   --    The table of abbreviations.
   -- Length
   --    The length of the attribute value encodings for each form,
   --    with zero for forms with variable-length encodings. Some of
   --    the lengths depend on Key.Addr_Size and Key.Bits.


   procedure Put (Item : Set_T)
   --
   -- Displays the abbreviation set on standard output.
   --
   is
      use Ada.Text_IO;
   begin

      Put_Line (
           "Abbreviation set at Offset ="
         & Section_Offset_T'Image (Item.Key.Offset)
         & ", Addr_Size ="
         & Addr_Size_T'Image (Item.Key.Addr_Size)
         & ", "
         & Bits_T'Image (Item.Key.Bits));

      Put (Item.Table.all);

      -- We elide the output of Item.Length.

   end Put;


   procedure Compute_Fixed_Layout (Set : in out Set_T)
   --
   -- Computes the fixed (not attribute-value-dependent) layout
   -- information for the given abbreviation set.
   --
   is
   begin

      -- Set up the form-length table:

      Set.Length := Standard_Form_Length;

      Set.Length(Addr) := In_Memory.Count_T (Set.Key.Addr_Size);

      Set.Length(Ref_Addr) :=
         In_Memory.Count_T (Variant_Unsigned_Length (Set.Key.Bits));

      Set.Length(Strp) := Set.Length(Ref_Addr);

      -- Compute the fixed layout for each defined abbreviation:

      for T in Set.Table'Range loop

         if Set.Table(T) /= null then
            -- The abbreviation code T is defined.

            Compute_Fixed_Layout (
               Length => Set.Length,
               Enty   => Set.Table(T).all);

         end if;

      end loop;

   end Compute_Fixed_Layout;


   function Key (Set : Set_Ref) return Set_Key_T
   is
   begin

      return Set.Key;

   end Key;


   function Enty (Code : Code_T; Set : Set_Ref) return Enty_Ref
   is
   begin

      if Code in Set.Table'Range
      and then Set.Table(Code).Code /= 0
      then

         return Set.Table(Code);

      else

         return No_Enty;

      end if;

   end Enty;


   function Is_Null (View : View_T) return Boolean
   is
   begin

      return View.Enty = No_Enty;

   end Is_Null;


   function Code (View : View_T) return Code_T
   is
   begin

      return View.Enty.Code;

   end Code;


   function Tag_Code (View : View_T) return Tag_Code_T
   is
   begin

      return View.Enty.Tag_Code;

   end Tag_Code;


   function Tag (View : View_T) return Tag_T
   is
   begin

      return To_Tag (View.Enty.Tag_Code);

   end Tag;


   function Number_Of_Attributes (View : View_T) return Natural
   is
   begin

      return View.Enty.Num_Attributes;

   end Number_Of_Attributes;


   function Attributes (View : View_T) return Att_Form_List_T
   is
   begin

      return View.Enty.Att_Forms;

   end Attributes;


   function Children (View : View_T) return Boolean
   is
   begin

      return View.Enty.Children;

   end Children;


   function View (
      Sect : Sections_T;
      Set  : Set_Ref;
      Refz : In_Memory.Index_T)
   return View_T
   is

      Code : Code_T;
      -- The abbreviation code.

      Enty : Enty_Ref;
      -- The abbreviation.

   begin

      Code_T'Read (Sect.Info, Code);

      if Code = 0 then
         -- A null entry.

         Enty := No_Enty;

      elsif Code not in Set.Table'Range
      or else Set.Table(Code) = null
      then

         Output.Error (
              "Invalid or undefined DWARF abbreviation code"
            & Code_T'Image (Code));

         raise Format_Error;

      else

         Enty := Set.Table(Code);

      end if;

      return (
         Sect => Sect,
         Base => In_Memory.Index (Sect.Info.all),
         Enty => Enty,
         Set  => Set,
         Refz => Refz);

   end View;


   procedure Locate (
      Attribute : in     Attribute_T;
      Thru      : in     View_T;
      Form      :    out True_Form_T)
   is
      use type In_Memory.Count_T;


      function Var_Length (Up_To : Att_Form_Index_T)
      return In_Memory.Count_T
      --
      -- The total length of the attribute values with variable
      -- encoding length before and including i.e. Up_To a given
      -- such attribute.
      --
      is

         Layout : Fixed_Layout_T renames Thru.Enty.Layout(Up_To);

         Prev_Var : In_Memory.Count_T := 0;
         -- The total length of attribute values with variable
         -- encoding length before but not including Up_To.
         -- Initial value assumes there are no such attributes.

      begin

         if Layout.Prev_Flex > 0 then
            -- Some variable-length attributes before this one.
            -- Recursively compute their total length:

            Prev_Var := Var_Length (Up_To => Layout.Prev_Flex);

         end if;

         return
              Prev_Var
            + Length_Of (
                 Value  => Thru.Enty.Att_Forms(Up_To),
                 Within => Thru.Sect.Info,
                 Index  => Layout.Fix_Offset + Prev_Var,
                 Length => Thru.Set.Length);

      end Var_Length;


      Att : Att_Form_Index_T := 0;
      -- The index of the desired Attribute, if present, else zero.

      Layout : Fixed_Layout_T;
      -- The fixed layout of the desired attribute.

      Var_Offset : In_Memory.Count_T := 0;
      -- The variable part of the offset of this attribute.
      -- Initial value assumes no variable-length attributes
      -- precede this one.

   begin  -- Locate

      -- Scan the attribute list for this Attribute:

      for A in Thru.Enty.Att_Forms'Range loop

         if Thru.Enty.Att_Forms(A).Attribute = Attribute then
            -- Found it.

            Att := A;

            exit;

         end if;

      end loop;

      if Att = 0 then
         -- Asking for the impossible...

         if Opt.Trace_Loading then

            Output.Trace (
                 "DWARF attribute"
              & Attribute_T'Image (Attribute)
              & " missing.");

         end if;

         raise Attribute_Absent;

      else
         -- Attribute present; find its location.

         Layout := Thru.Enty.Layout(Att);

         if Layout.Prev_Flex > 0 then
            -- Some variable-length attributes precede this one.

            Var_Offset := Var_Length (Up_To => Layout.Prev_Flex);

         end if;

         In_Memory.Set_Index (
            Stream => Thru.Sect.Info.all,
            To     => Thru.Base + Var_Offset + Layout.Fix_Offset);

         Form := True_Form (
            Given => Thru.Enty.Att_Forms(Att).Form,
            From  => Thru.Sect.Info);

      end if;

   end Locate;


   procedure Digest (
      View   : in     View_T;
      Giving :    out Value_Def_List_T;
      Next   :    out In_Memory.Index_T)
   is
      use type In_Memory.Count_T;

      -- Principle of Operation
      --
      -- Scan the attribute-form list defined in the abbreviation
      -- entry, update the index of the current attribute by the
      -- fixed or variable length of the value and resolve form
      -- indirection by reading true forms from the stream.

      Att_Forms : Att_Form_List_T renames View.Enty.Att_Forms;

      Index : In_Memory.Index_T := View.Base;
      -- The index for the current attribute value.

      This : Att_Form_T;
      -- The current attribute.

      Length : In_Memory.Count_T;
      -- The length of the current attribute value, not including
      -- the true form code in case of form indirection.

   begin

      for A in Att_Forms'Range loop

         This := Att_Forms(A);

         -- Resolve form indirection if needed:

         if This.Form = Indirect then

            In_Memory.Set_Index (View.Sect.Info.all, Index);

            Read_True_Form (View.Sect.Info, This.Form);

            Index := In_Memory.Index (View.Sect.Info.all);

         end if;

         -- We know the true form and position of this value:

         Giving(A) := (
            Attribute => This.Attribute,
            Form      => This.Form,
            Index     => Index);

         -- Skip the value:

         Length := View.Set.Length(This.Form);

         if Length = 0 then
            -- Variable-length encoding.
            -- Note that the form indirection, if any, was
            -- already resolved, and the true form (in This)
            -- is supplied to the Length_Of function.

            Length := Length_Of (
               Value  => This,
               Within => View.Sect.Info,
               Index  => Index,
               Length => View.Set.Length);

         end if;

         Index := Index + Length;

      end loop;

      Next := Index;

   end Digest;


   function Next_Entry (After : View_T) return In_Memory.Index_T
   is
      use type In_Memory.Count_T;

      -- Principle of Operation
      --
      -- Scan the attribute-form list defined in the abbreviation
      -- entry, update the index of the current attribute by the
      -- fixed or variable length of the value and resolve form
      -- indirection by reading true forms from the stream.

      Att_Forms : Att_Form_List_T renames After.Enty.Att_Forms;

      Index : In_Memory.Index_T := After.Base;
      -- The index for the current attribute value.

      This : Att_Form_T;
      -- The current attribute.

      Length : In_Memory.Count_T;
      -- The length of the current attribute value, not including
      -- the true form code in case of form indirection.

   begin

      for A in Att_Forms'Range loop

         This := Att_Forms(A);

         -- Resolve form indirection if needed:

         if This.Form = Indirect then

            In_Memory.Set_Index (After.Sect.Info.all, Index);

            Read_True_Form (After.Sect.Info, This.Form);

            Index := In_Memory.Index (After.Sect.Info.all);

         end if;

         -- Skip the value:

         Length := After.Set.Length(This.Form);

         if Length = 0 then
            -- Variable-length encoding.
            -- Note that the form indirection, if any, was
            -- already resolved, and the true form (in This)
            -- is supplied to the Length_Of function.

            Length := Length_Of (
               Value  => This,
               Within => After.Sect.Info,
               Index  => Index,
               Length => After.Set.Length);

         end if;

         Index := Index + Length;

      end loop;

      return Index;

   end Next_Entry;


   procedure Put_Location (
      View   : in View_T;
      Index  : in In_Memory.Index_T;
      Form   : in Form_T;
      Indent : in String;
      CUBA   : in Poss_Code_Address_T)
   --
   -- Displays the value of a location-valued attribute that starts
   -- in the stream View.Info at Index and has a given Form (possibly
   -- Indirect). The initial position of the stream is not important
   -- and the stream is left at the position after the attribute
   -- value. The Compilation Unit Base Address may be defined, or not.
   --
   is
      use Ada.Text_IO;
      use type In_Memory.Stream_Ref;
      use type In_Memory.Index_T;

      Real_Form : True_Form_T;
      -- The real form of the value, after resolving indirection.

      Loc_Offset : In_Memory.Count_T;
      -- The offset of the location list in View.Loc.

      Loc_Base : Code_Address_T;
      -- The base address for the location list address ranges.

      Block_Len : In_Memory.Count_T;
      -- The length of the location block in View.Info.

   begin

      In_Memory.Set_Index (Stream => View.Sect.Info.all, To => Index);

      Real_Form := True_Form (Given => Form, From => View.Sect.Info);

      case Real_Form is

      when Data2 | Data4 =>
         -- Pointer to location-list in the Loc section.

         if Real_Form = Data2 then

            Unsigned_16_T'Read (View.Sect.Info, Unsigned_16_T (Loc_Offset));

         else

            Unsigned_32_T'Read (View.Sect.Info, Unsigned_32_T (Loc_Offset));

         end if;

         if CUBA.Defined then

            Loc_Base := CUBA.Address;

         else

            Text.Put_Line (Indent, "Compilation Unit Base Address undefined!");

            Loc_Base := 0;

         end if;

         if View.Sect.Locations /= null then

            Expressions.Dump_Location_List (
               Block  => (
                  Stream    => View.Sect.Locations,
                  Start     => In_Memory.Index_T'First + Loc_Offset,
                  Octets    => 0,    -- not known.
                  Addr_Size => View.Set.Key.Addr_Size,
                  Bits      => View.Set.Key.Bits),
               Base   => Loc_Base,
               Indent => Indent);

         else

            Text.Put_Line (Indent, "No location-list section!");

         end if;

      when Block_Form_T =>
         -- Single location expression in the Block in the Info section.

         Block_Len := Block_Length (
            Form   => Real_Form,
            Stream => View.Sect.Info);

         Text.Begin_Line (Indent);
         -- For the first line of the expression.

         Expressions.Dump_Expression (
            Block => (
               Stream    => View.Sect.Info,
               Start     => In_Memory.Index (View.Sect.Info.all),
               Octets    => Block_Len,
               Addr_Size => View.Set.Key.Addr_Size,
               Bits      => View.Set.Key.Bits),
            Indent  => Indent);

      when others =>

         Text.Put_Line (Indent, "Incorrect form of Location attribute!");

      end case;

   end Put_Location;


   procedure Put (
      View   : in     View_T;
      Indent : in     String;
      CUBA   : in     Poss_Code_Address_T;
      Next   :    out In_Memory.Index_T)
   is
      use Ada.Text_IO;

      AF : Att_Form_T;
      -- The current attribute and form.

      A_Index : In_Memory.Index_T;
      -- The Info stream index of the current attribute.

   begin

      Text.Put_Line (Indent,
           Image (Tag (View))
         & " (abbrev"
         & Code_T'Image (Code (View))
         & "), children = "
         & Boolean'Image (Children (View)));

      In_Memory.Set_Index (
         Stream => View.Sect.Info.all,
         To     => View.Base);

      for A in View.Enty.Att_Forms'Range loop

         AF := View.Enty.Att_Forms(A);

         A_Index := In_Memory.Index (View.Sect.Info.all);

         Text.Dump_Attribute (
            Info      => View.Sect.Info,
            Attribute => AF.Attribute,
            Form      => AF.Form,
            Bits      => View.Set.Key.Bits,
            Addr_Size => View.Set.Key.Addr_Size,
            Ref_Base  => View.Refz,
            Str       => View.Sect.Strings,
            Indent    => Indent & "   - ");

         if AF.Attribute = Location
         or AF.Attribute = Frame_Base
         then
            -- Display the location expression or list:

            Put_Location (
               View   => View,
               Index  => A_Index,
               Form   => AF.Form,
               Indent => Indent & "     @  ",
               CUBA   => CUBA);

         end if;

      end loop;

      Next := In_Memory.Index (View.Sect.Info.all);

   end Put;


   function Ref_To_Set (
      Key    : Set_Key_T;
      Within : Sets_T)
   return Set_Ref
   --
   -- A reference to the abbreviation set that has the given Key value, if
   -- such a set exists Within the given collection, or null otherwise.
   --
   is

      Ref : Set_Ref:= null;
      -- The result, with a pessimistic initial value.

   begin

      for I in 1 .. Length (Within) loop

         if Element (Within, I).Key = Key then
            -- Found it!

            Ref := Element (Within, I);

            exit;

         end if;

      end loop;

      return Ref;

   end Ref_To_Set;


   procedure Find_Or_Load_Set (
      Key    : in     Set_Key_T;
      Stream : in     In_Memory.Stream_Ref;
      Sets   : in out Sets_T;
      Ref    :    out Set_Ref;
      Novel  :    out Boolean)
   is
   begin

      Ref := Ref_To_Set (Key => Key, Within => Sets);

      Novel := Ref = null;

      if Novel then
         -- A new abbreviation set must be loaded.

         -- Create the abbreviation set:

         Ref := new Set_T;

         Ref.Key := Key;

         -- Load the abbreviation table:

         In_Memory.Set_Offset(
            Stream => Stream.all,
            To     => In_Memory.Count_T (Key.Offset));

         Table_Ref'Read (Stream, Ref.Table);

         -- Compute the fixed layout:

         Compute_Fixed_Layout (Set => Ref.all);

         -- Add the set to the collection:

         Append (To => Sets, Value => Ref);

      end if;

   end Find_Or_Load_Set;


   procedure Put (Item : Set_Ref)
   is
   begin

      Put (Item.all);

   end Put;


   procedure Put (Item : Sets_T)
   is
      use Ada.Text_IO;
   begin

      Put_Line (
           "DWARF abbrevation-set collection with"
         & Natural'Image (Length (Item))
         & " sets:");

      for I in 1 .. Last (Item) loop

         Put (Element (Item, I).all);

      end loop;

      Put_Line ("end of abbreviation-set collection.");

      New_Line;

   end Put;


end Formats.Dwarf.Abbrev;
