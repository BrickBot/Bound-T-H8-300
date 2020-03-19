-- Formats.AOMF_Keil (body)
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
-- $Revision: 1.5 $
-- $Date: 2015/10/24 20:53:51 $
--
-- $Log: formats-aomf_keil.adb,v $
-- Revision 1.5  2015/10/24 20:53:51  niklas
-- Moved to free licence.
--
-- Revision 1.4  2007-09-27 07:06:02  niklas
-- Added "AOMF" to some warnings/errors, for TR-TN-AOMF-001.
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


with Output;


package body Formats.AOMF_Keil is


   use type IO.Count;


   procedure Read_Word16 (
      Stream : access Ada.Streams.Root_Stream_Type'Class;
      Item   :    out Word16_T)
   is

      Lsb, Msb : Word16_T;

   begin

      Octet_T'Read (Stream, Octet_T (Lsb));

      Octet_T'Read (Stream, Octet_T (Msb));

      Item := Shift_Left (Value => Msb, Amount => 8) or Lsb;

   end Read_Word16;


   procedure Read_Name_Ref (
      Stream : access Ada.Streams.Root_Stream_Type'Class;
      Item   :    out Name_Ref)
   is

      Length : Octet_T;
      -- The octet giving the length of the Name.

   begin

      Octet_T'Read  (Stream, Length);

      Item := new String (1 .. Natural (Length));

      String'Read (Stream, Item.all);

   end Read_Name_Ref;


   --    Storage (memory) kinds


   function Storage (Encoding : in Octet_T)
   return Storage_T
   --
   -- Decodes an encoded Storage specification.
   --
   is
   begin

      case Encoding is

      when 16#00# => return (Kind => Code);
      when 16#01# => return (Kind => XData);
      when 16#02# => return (Kind => Data);
      when 16#03# => return (Kind => IData);
      when 16#04# => return (Kind => Bit);
      when 16#05# => return (Kind => Number);
      when 16#09# => return (Kind => Large_Stack);
      when 16#0A# => return (Kind => Compact_Stack);
      when 16#0B# => return (Kind => Small_Stack);

      when 16#10# .. 16#2F# =>

         return (
            Kind      => Code_Bank,
            Code_Bank => Code_Bank_T (Encoding - 16#10#));

      when others =>

         return (Kind => Unknown);

      end case;

   end Storage;


   procedure Read_Storage (
      Stream : access Ada.Streams.Root_Stream_Type'Class;
      Item   :    out Storage_T)
   is

      Encoding : Octet_T;

   begin

      Octet_T'Read (Stream, Encoding);

      Item := Storage (Octet_T (Encoding));

   end Read_Storage;


   --    Symbol Info


   procedure Read_Symbol_Info (
      Stream : access Ada.Streams.Root_Stream_Type'Class;
      Item   :    out Symbol_Info_T)
   is

      B : Octet_T;
      -- The symbol-info encoded as one octet.

      Usage_Code : Octet_T range 0 .. 7;
      -- The three code bits for the symbol usage.

   begin

      Octet_T'Read (Stream, B);

      Item.Indirect        := Bit (7, B);
      Item.Variable        := Bit (6, B);
      Item.Single_Reg_Bank := Bit (5, B);

      Item.Register_Bank := Register_Bank_T (
         Bits (High => 4, Low => 3, From => B));

      Usage_Code := Bits (High => 2, Low => 0, From => B);

      Item.Usage := Storage (Usage_Code);

      if Item.Usage.Kind = Unknown then

         Output.Error (
              "Unknown AOMF Symbol_Info.Usage ="
            & Octet_T'Image (Usage_Code));

      end if;

   end Read_Symbol_Info;


   --    Segment Info


   procedure Read_Segment_Info (
      Stream : access Ada.Streams.Root_Stream_Type'Class;
      Item   :    out Segment_Info_T)
   is

      B : Octet_T;
      -- The segment-info encoded as an octet.

      Type_Code : Octet_T range 0 .. 7;
      -- The three code bits for the segment type.

   begin

      Octet_T'Read (Stream, B);

      Item.Empty         := Bit (7, B);
      Item.Overlayable   := Bit (5, B);

      Item.Register_Bank := Register_Bank_T (
         Bits (High => 4, Low => 3, From => B));

      Type_Code := Bits (High => 2, Low => 0, From => B);

      Item.Segment_Type := Storage (Type_Code);

      if Item.Segment_Type.Kind = Unknown then

         Output.Error (
              "Unknown AOMF Segment_Info.Seg_Type ="
            & Octet_T'Image (Type_Code));

      end if;

   end Read_Segment_Info;


   --    AOMF record types


   function Record_Kind (Encoded : Octet_T) return Record_Kind_T
   is

      Kind : Record_Kind_T;

   begin

      case Encoded is

      when 16#02# => Kind := Module_Header;
      when 16#04# => Kind := Module_End;
      when 16#06# => Kind := Content;
      when 16#10# => Kind := Scope_Definition;
      when 16#12# => Kind := Debug_Items;
      when 16#0E# => Kind := Unknown;  -- Segment Definition
      when 16#16# => Kind := Unknown;  -- Public Definition
      when 16#18# => Kind := Unknown;  -- External Definition
      when 16#20# => Kind := Type_Definition;
      when 16#22# => Kind := Extended_Debug_Items;
      when 16#24# => Kind := Source_Name;
      when 16#2E# => Kind := BL51_Bank_Head;

      when 16#60# .. 16#70# => Kind := Source_Browse_TBC;

      when others => Kind := Unknown;

      end case;

      return Kind;

   end Record_Kind;


   procedure Read_Record_Kind (
      Stream : access Ada.Streams.Root_Stream_Type'Class;
      Item   :    out Record_Kind_T)
   is

      T : Octet_T;
      -- The encoded record type.

   begin

      Octet_T'Read (Stream, T);

      Item := Record_Kind (T);

      if Item = Unknown then

         Output.Warning (
              "Unknown AOMF Record Type = "
            & Hex_Image (T));

      end if;

   end Read_Record_Kind;


   --    BL 51 Bank Head TBA


   --    Module Header


   --    Module End


   procedure Read_Register_Banks (
      Stream : access Ada.Streams.Root_Stream_Type'Class;
      Item   :    out Register_Banks_T)
   is

      B : Octet_T;
      -- The code octet.

   begin

      Octet_T'Read (Stream, B);

      for I in Item'Range loop

         Item(I) := Bit (Number => Octet_Bit_T (I), From => B);

      end loop;

   end Read_Register_Banks;


   --    Content


   procedure Read_Content (
      Stream : access Ada.Streams.Root_Stream_Type'Class;
      Length : in     Word16_T;
      Item   :    out Content_T)
   is

      Non_Data_Length : constant Word16_T := 4;
      -- The number of non-Data octets counted in the Length.

      Data_Length : Natural;
      -- The length of Data in the record.

   begin

      if Length < Non_Data_Length then
         -- Too short for a Content record, even for an empty Data.

         Output.Error (
              "Too short AOMF Content.Length ="
            & Word16_T'Image (Length));

         raise Format_Error;

      end if;

      Data_Length := Natural (Length - Non_Data_Length);

      Octet_T'Read  (Stream, Item.Seg_ID);

      Word16_T'Read (Stream, Item.Offset);

      Item.Data := new Data_T (0 .. Data_Length - 1);

      Data_T'Read (Stream, Item.Data.all);

   end Read_Content;


   --    Scope Definition


   procedure Read_Block_Type (
      Stream : access Ada.Streams.Root_Stream_Type'Class;
      Item   :    out Block_Type_T)
   is

      B : Octet_T;
      -- The code for the block type.

   begin

      Octet_T'Read (Stream, B);

      case B is

      when 0 => Item := Begin_Module;
      when 1 => Item := Begin_Do;
      when 2 => Item := Begin_Procedure;
      when 3 => Item := End_Module;
      when 4 => Item := End_Do;
      when 5 => Item := End_Procedure;

      when others =>

         Output.Error (
              "Unknown AOMF Scope_Definition.Block_Type ="
            & Octet_T'Image (B));

         Item := Unknown;

      end case;

   end Read_Block_Type;


   --    Source Name


   --    Type Definition


   procedure Read_Type_Index (
      Stream : access Ada.Streams.Root_Stream_Type'Class;
      Item   :    out Type_Index_T)
   is
   begin

      Octet_T'Read (Stream, Octet_T (Item));

      if Item = 16#1F# then
         -- This introduces a 2-byte value.

         Word16_T'Read (Stream, Word16_T (Item));   -- TBM to big-endian TBC!

      end if;

   end Read_Type_Index;


   procedure Read_Compound_Type_Tag (
      Stream : access Ada.Streams.Root_Stream_Type'Class;
      Item   :    out Compound_Type_Tag_T)
   is

      Tag : Octet_T;
      -- The encoded compound type kind.

   begin

      Octet_T'Read (Stream, Tag);

      case Tag is

      when 16#20# => Item := List;
      when 16#21# => Item := Pointer;
      when 16#22# => Item := Aray;
      when 16#23# => Item := Funktion;
      when 16#24# => Item := Type_Tag;
      when 16#25# => Item := Struct;
      when 16#26# => Item := Bit_Field;
      when 16#27# => Item := Spaced_Pointer;
      when 16#28# => Item := Generic_Pointer;

      when others =>

         Output.Error (
              "Unknown AOMF Compound Type Tag = "
            & Hex_Image (Tag));

         raise Format_Error;

      end case;

   end Read_Compound_Type_Tag;


   procedure Read_Tuple_Ref (
      Stream : access Ada.Streams.Root_Stream_Type'Class;
      Item   :    out Tuple_Ref)
   is

      Number : Word16_T;
      -- The number of components in the tuple.

   begin

      Word16_T'Read (Stream, Number);

      Item := new Tuple_T (1 .. Natural (Number));

      Tuple_T'Read (Stream, Item.all);

   end Read_Tuple_Ref;


   procedure Read_Dimensions_Ref (
      Stream : access Ada.Streams.Root_Stream_Type'Class;
      Item   :    out Dimensions_Ref)
   is

      Number : Octet_T;
      -- The number of dimensions (indices).

   begin

      Octet_T'Read (Stream, Number);

      Item := new Dimensions_T (1 .. Natural (Number));

      Dimensions_T'Read (Stream, Item.all);

   end Read_Dimensions_Ref;


   procedure Read_Pointer_Space (
      Stream : access Ada.Streams.Root_Stream_Type'Class;
      Item   :    out Pointer_Space_T)
   is

      Space : Octet_T;
      -- The encoded space.

   begin

      Octet_T'Read (Stream, Space);

      case Space is

      when 0 => Item := None;
      when 1 => Item := IData;
      when 2 => Item := XData;
      when 3 => Item := PData;
      when 4 => Item := Data;
      when 5 => Item := Code;

      when others =>

         Output.Warning (
              "Unknown Pointer Space ="
            & Octet_T'Image (Space));

         Item := Unknown;

      end case;

   end Read_Pointer_Space;


   procedure Read_Pointer_Kind (
      Stream : access Ada.Streams.Root_Stream_Type'Class;
      Item   :    out Pointer_Kind_T)
   is

      Kind : Octet_T;
      -- The encoded kind.

   begin

      Octet_T'Read (Stream, Kind);

      case Kind is

      when 1 => Item := Data;
      when 2 => Item := Funktion;
      when 4 => Item := Huge;

      when others =>

         Output.Warning (
              "Unknown Pointer Kind ="
            & Octet_T'Image (Kind));

         Item := Unknown;

      end case;

   end Read_Pointer_Kind;


   --    Debug Items


   procedure Read_Def_Kind (
      Stream : access Ada.Streams.Root_Stream_Type'Class;
      Item   :    out Def_Kind_T)
   is

      B : Octet_T;
      -- The encoded Def_Kind.

   begin

      Octet_T'Read (Stream, B);

      case B is

      when 0 => Item := Local_Symbols;
      when 1 => Item := Public_Symbols;
      when 2 => Item := Segment_Symbols;
      when 3 => Item := Line_Numbers;

      when others =>

         Output.Error (
              "Unknown AOMF Debug_Items.Def_Type ="
            & Octet_T'Image (B)
            & "; skipping record.");

         Item := Unknown;

      end case;

   end Read_Def_Kind;


   --
   --    Whole AOMF record descriptors
   --


   procedure Read_Record_Info (
      File   : in     IO.File_Type;
      Stream : access Ada.Streams.Root_Stream_Type'Class;
      Item   :    out Record_Info_T)
   is
   begin

      Item.Start := IO.Index (File);

      Octet_T'Read (Stream, Item.Kind_Code);

      Item.Kind := Record_Kind (Item.Kind_Code);

      if Item.Kind = Unknown then

         Output.Warning (
              "Unknown AOMF Record Type = "
            & Hex_Image (Item.Kind_Code));

      end if;

      Word16_T'Read      (Stream, Item.Length);

   end Read_Record_Info;


   function Last_Index (Within : in Record_Info_T)
   return IO.Positive_Count
   --
   -- The file index of the last octet (the check-sum) Within the
   -- given record.
   --
   is
   begin

      return Within.Start + IO.Count (Within.Length) + 2;

   end Last_Index;


   function Data_Left (
      Within : Record_Info_T;
      File   : IO.File_Type)
   return Boolean
   is
   begin

      return IO.Index (File) < Last_Index (Within);

   end Data_Left;


   procedure Check_Sum (
      Over   : in     Record_Info_T;
      File   : in     IO.File_Type;
      Stream : access Ada.Streams.Root_Stream_Type'Class;
      Valid  :    out Boolean)
   is

      Sum : Octet_T := 0;
      -- The sum of all the octets in the record.

      Octet : Octet_T;
      -- One of the octets in the record.

   begin

      IO.Set_Index (File, Over.Start);

      for N in Over.Start .. Last_Index (Over) loop

         Octet_T'Read (Stream, Octet);

         Sum := Sum + Octet;

      end loop;

      Valid := Sum = 0;

      IO.Set_Index (File, Over.Start + 3);

   end Check_Sum;


   procedure Skip_To_Next_Record (
      After : in Record_Info_T;
      File  : in IO.File_Type)
   is
   begin

      IO.Set_Index (File, Last_Index (After) + 1);

   end Skip_To_Next_Record;


   --
   --    Quick verification of AOMF file type
   --


   function Accepts (
      Name : in String;
      File : in IO.File_Type)
   return Boolean
   is

      Stream : constant IO.Stream_Access := IO.Stream (File);
      -- For reading the file.

      Kind_Code : Octet_T;
      -- The encoded record kind.

      Rec : Record_Info_T;
      -- For the first record in the file.

      Valid : Boolean;
      -- Whether the first record has a valid check-sum.

   begin

      IO.Set_Index (File, IO.Positive_Count'First);

      -- We read the Rec components one by one here, to avoid error or
      -- warning messages for e.g. invalid Record Kind code.

      Rec.Start := IO.Index (File);

      Octet_T'Read (Stream, Kind_Code);

      Rec.Kind := Record_Kind (Kind_Code);

      if  Rec.Kind /= Source_Browse_TBC
      and Rec.Kind /= BL51_Bank_Head
      and Rec.Kind /= Module_Header
      then
         -- Nope, it ain't AOMF as we know it.

         return False;

      else
         -- Looks like AOMF; check sum to be sure.

         Word16_T'Read (Stream, Rec.Length);

         if Rec.Length = 0 then
            -- Nope, it ain't an AOMF record.

            return False;

         else

            Check_Sum (Rec, File, Stream, Valid);

            return Valid;

         end if;

      end if;

   end Accepts;


   --
   --    Building representations of AOMF data
   --


   Type_Table_Origin : constant Type_Index_T :=
      Compound_Type_Index_T'First - 1;
   --
   -- The value to be subtracted from a Type_Index to give the
   -- Positive index in a Type_Table.


   function Element (From : Type_Table_T; Index : Type_Index_T)
   return Compound_Type_T
   is
   begin

      if Index in Basic_Type_Index_T then

         return (Kind => Predefined, Predefined => (Basic => Index));

      elsif Index in Compound_Type_Index_T then

         return

            Type_Vectors.Element (
               Vector => Type_Vectors.Unbounded_Vector (From),
               Index  => Positive (Index - Type_Table_Origin));

      else

         raise Constraint_Error;

      end if;

   end Element;


   function Defining_Type (Index : Type_Index_T; Table : Type_Table_T)
   return Compound_Type_T
   is

      Var : Compound_Type_T := Element (Table, Index);
      -- The surface so far.

      Inner : Type_Index_T;
      -- The inner type, below the surface.

   begin

      loop

         case Var.Kind is

         when Type_Tag =>

            exit when Var.Type_Tag.Base = 0;

            Inner := Var.Type_Tag.Base;

         when others => exit;

         end case;

         Var := Element (Table, Inner);

      end loop;

      return Var;

   end Defining_Type;


   procedure Read_Type_Definitions (
      Rec    : in     Record_Info_T;
      File   : in     IO.File_Type;
      Stream : in     IO.Stream_Access;
      Table  :    out Type_Table_T)
   is

      Index : Compound_Type_Index_T := Compound_Type_Index_T'First;
      -- The index for the next type descriptor.

      Item : Compound_Type_T;
      -- One of the type descriptors.

   begin

      Erase (Table);

      while Data_Left (Rec, File) loop

         Compound_Type_T'Read (Stream, Item);

         Set (
            Vector => Table,
            Index  => Positive (Index - Type_Table_Origin),
            To     => Item);

         Index := Index + 1;

      end loop;

   end Read_Type_Definitions;


   procedure Clear (Table : in out Type_Table_T)
   is
   begin

      Erase (Table);

   end Clear;


end Formats.AOMF_Keil;
