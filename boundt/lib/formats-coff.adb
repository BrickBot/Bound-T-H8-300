-- Formats.COFF (body)
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
-- $Revision: 1.17 $
-- $Date: 2015/10/24 20:53:52 $
--
-- $Log: formats-coff.adb,v $
-- Revision 1.17  2015/10/24 20:53:52  niklas
-- Moved to free licence.
--
-- Revision 1.16  2015/05/22 06:37:32  niklas
-- Moved to opt.
--
-- Revision 1.15  2013-11-24 14:07:46  niklas
-- BT-CH-0253: SHARC again models call to _exit as return.
--
-- Revision 1.14  2013-11-16 19:47:01  niklas
-- BT_CH_0251: 21020/SHARC update: Options and Formats.COFF.
--
-- Revision 1.13  2007-03-28 09:21:31  niklas
-- Extended function Accepts to consider a constraint-error
-- exception as proof of a non-COFF file.
--
-- Revision 1.12  2007/03/12 13:58:33  niklas
-- Corrected and extended Read_Strings to return an empty strings
-- table if the COFF file ends before the string table.
--
-- Revision 1.11  2007/03/06 20:15:15  niklas
-- Extended Get_Auxiliary_Symbols to display unexpected auxiliary symbols
-- in hex form using the new procedure Dump_Symbol.
-- Modified Get_Auxiliary_Symbols to place references to the sole
-- Aux_Unknown symbol, The_Unknown_Aux_Symbol, in the Symbols table,
-- instead of the null references hitherto used to mark unexpected
-- auxiliary symbols (which could be confused with null used to mark the
-- end of the Symbols list in the Parsing package).
--
-- Revision 1.10  2007/02/27 22:10:34  Niklas
-- Corrected 'Read procedures for scalar types to use the 'Base
-- subtype of the scalar type and not to use Read operations inherited
-- from parent types (and so having "intrinsic" convention).
--
-- Revision 1.9  2006/11/26 22:01:52  niklas
-- Improved Read_Strings to check the length of the string
-- table and reject it (with a Note) if it is too short.
-- Added Note to Get_File re file-position of string table.
--
-- Revision 1.8  2005/10/26 14:07:19  niklas
-- Using Formats.Output instead of standard Output.
-- Changed Load_And_Put (File_Name) to absorb (not propagate) the
-- Name_Error and Use_Error exceptions.
--
-- Revision 1.7  2005/07/25 19:21:17  niklas
-- Added function String_At that takes a Strings_T, which can be
-- an "empty" string table, for example No_Strings, in which case
-- a null string is returned.
-- Changed the function String_At that takes a File_Ref to work in
-- this way, too.
--
-- Revision 1.6  2005/07/24 19:48:22  niklas
-- Added the Symbol_Kind_T Aux_Field for auxiliary records that report
-- the size of a bit-field symbol. Likewise added the Symbol_Nature_T
-- Normal_Field to indicate the nature of bit-field symbols.
-- Extended the Nature function to detect bit-field symbols (C_Field)
-- and return Normal_Field.
-- Changed Get_Auxiliary_Symbols to skip unexpected auxiliary records
-- instead of letting them be interpreted as primary records.
--
-- Revision 1.5  2005/05/18 07:34:39  niklas
-- Corrected To_String to stop at the first NUL.
--
-- Revision 1.4  2005/03/24 19:47:50  niklas
-- Corrected the handling of derived types by considering the whole
-- sequence of derivations (Type_Derivation_T). We now classify a
-- pointer (to a pointer ..) to an array as Normal_Array, which means
-- that it can have Aux_Array entries.
--
-- Revision 1.3  2005/03/20 20:04:33  niklas
-- Corrected Load_And_Put so that it does not try to load sections
-- that should not be loaded, such as dummy or bss sections.
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
-- <Log: coff.adb>
-- Revision 1.4  2001/12/18 20:34:07  holsti
-- Not loading an empty section merits a Note, not a Warning.
--
-- Revision 1.3  2001/06/11 12:28:08  holsti
-- Options for 16/32 bit endianness added.
-- Option for symbol-entry length added.
--
-- Revision 1.2  2001/02/14 15:18:16  saarinen
-- Fixed NC_102 String indexing.
--
-- Revision 1.1  2000/07/02 18:41:56  holsti
-- First version.
--


with Ada.Text_IO;
with Ada.Strings.Fixed;

with Hex;
with Formats.Output;
with Formats.COFF.Opt;
with Formats.COFF.Text;


package body Formats.COFF is


   use type IO.Count;


   subtype Short_Bit_T is Integer range 0 .. 15;
   subtype Bit_T       is Integer range 0 .. 31;


   function Bit (Number : Short_Bit_T; Within : Ushort_T) return Boolean
   --
   -- Picks bit Number Within a short value.
   --
   is
   begin
      return (Within / (2**Number)) mod 2 = 1;
   end Bit;


   function Bit (Number : Bit_T; Within : Ulong_T) return Boolean
   --
   -- Picks bit Number Within a long value.
   --
   is
   begin
      return (Within / (2**Number)) mod 2 = 1;
   end Bit;


   --
   -- Image functions:
   --


   function Image (Item : Byte_T) return String
   is
   begin
      return Hex.Image (Hex.Byte_T(Item));
   end Image;


   function Hex_Image (
      Item  : in Ushort_T;
      Width : in Positive := 6)
   return String
   is
      Digs  : constant String := Hex.Image (Hex.Word16_T(Item));
      Zeros : constant Natural :=
         Natural'Max (0, Width - 2 - Digs'Length);
      use Ada.Strings.Fixed;
   begin
      return "0x" & Zeros * '0' & Digs;
   end Hex_Image;


   function Hex_Image (
      Item  : in Ulong_T;
      Width : in Positive := 10)
   return String
   is
      Digs  : constant String := Hex.Image (Hex.Word32_T(Item));
      Zeros : constant Natural :=
         Natural'Max (0, Width - 2 - Digs'Length);
      use Ada.Strings.Fixed;
   begin
      return "0x" & Zeros * '0' & Digs;
   end Hex_Image;


   function Image (Item : Section_IO_T) return String
   is
   begin
      return "File Index         =" & IO.Count'Image(Item.Index);
   end Image;


   function Image (Item : File_IO_T) return String
   is
   begin
      return "File Index         =" & IO.Count'Image (Item.Index);
   end Image;


   function Fundamental_Type (Item : Symbol_Type_T)
   return Fundamental_Type_T
   is
   begin
      return Fundamental_Type_T'Val (Item and 16#0F#);
   end Fundamental_Type;


   function Derived_Type (
      Item  : Symbol_Type_T;
      Level : Derivation_Level_T := 1)
   return Derived_Type_T
   is
      use Interfaces;

      Bits : Unsigned_16;
      -- The two bits for the derived type.

   begin

      if Level <= 6 then
         -- Level in range.

         Bits :=
            Shift_Right (
               Value  => Unsigned_16 (Item),
               Amount => 2 + 2 * Level)
            and 2#11#;

         -- The Amount is computed so that Level = 1 shifts by four bits,
         -- which shifts out the fundamental type and brings the first
         -- derived type (d1) into the lowest bits.

         return Derived_Type_T'Val (Bits);

      else
         -- Too deep a level.

         return DT_Non;

      end if;

   end Derived_Type;


   function Derivation (Item : Symbol_Type_T)
   return Type_Derivation_T
   is

      Deriv : Type_Derivation_T (Derivation_Level_T);
      Last  : Natural := 0;
      -- The result will be Deriv(1 .. Last).

      DT : Derived_Type_T;
      -- The derived type at a level, or DT_Non.

   begin

      for L in Derivation_Level_T loop

         DT := Derived_Type (Item => Item, Level => L);

         exit when DT = DT_Non;
         -- Marks the end of the derivation sequence.
         -- Note that DT_Non is *not* returned in the result.

         Deriv(L) := DT;
         Last     := L;

      end loop;

      return Deriv(1 .. Last);

   end Derivation;


   function To_String (Name : in String) return String
   is

      Nul : Natural := Name'First;
      -- The index in Name of the first NUL character, if any,
      -- otherwise Name'Last + 1.

   begin

      while Nul <= Name'Last and then Name(Nul) /= Character'Val(0)
      loop
         Nul := Nul + 1;
      end loop;

      return Name(Name'First .. Nul - 1);

   end To_String;



   --
   -- Stream reading:
   --


   procedure Read_Ushort (
      Stream : access Ada.Streams.Root_Stream_Type'Class;
      Item   : out Ushort_T'Base)
   is
      Lsb, Msb : Byte_T;
   begin

      case Opt.Short_End is
         when Little =>
            Byte_T'Read (Stream, Lsb);
            Byte_T'Read (Stream, Msb);
         when Big =>
            Byte_T'Read (Stream, Msb);
            Byte_T'Read (Stream, Lsb);
      end case;

      Item := Ushort_T(Msb) * 256 + Ushort_T(Lsb);

   end Read_Ushort;


   procedure Read_Short (
      Stream : access Ada.Streams.Root_Stream_Type'Class;
      Item   : out Short_T'Base)
   is
      Lsb, Msb : Byte_T;
      Sign     : Short_T := 1;
   begin

      case Opt.Short_End is
         when Little =>
            Byte_T'Read (Stream, Lsb);
            Byte_T'Read (Stream, Msb);
         when Big =>
            Byte_T'Read (Stream, Msb);
            Byte_T'Read (Stream, Lsb);
      end case;

      if ((Msb and 16#80#) = 0) then
         Item := Short_T(Msb) * 256 + Short_T(Lsb);
      else
         Msb := Msb and 16#7f#;
         Item := -32768 + Short_T(Msb) * 256 + Short_T(Lsb);
      end if;

   end Read_Short;


   procedure Read_Ulong (
      Stream : access Ada.Streams.Root_Stream_Type'Class;
      Item   : out Ulong_T'Base)
   is
      Lsb, Msb : Ushort_T;
   begin

      case Opt.Long_End is
         when Little =>
            Ushort_T'Read (Stream, Lsb);
            Ushort_T'Read (Stream, Msb);
         when Big =>
            Ushort_T'Read (Stream, Msb);
            Ushort_T'Read (Stream, Lsb);
      end case;

      Item := Ulong_T(Msb) * (2**16) + Ulong_T(Lsb);

   end Read_Ulong;


   procedure Read_File_Flags (
      Stream : access Ada.Streams.Root_Stream_Type'Class;
      Item   : out File_Flags_T)
   is
      Uf : Ushort_T;
   begin

      Ushort_T'Read (Stream, Uf);

      Item := (
         Relocation_Info  => Bit (0, Uf),
         Executable       => Bit (1, Uf),
         No_Line_Numbers  => Bit (2, Uf),
         No_Local_Symbols => Bit (3, Uf),
         Little_End_16    => Bit (7, Uf),
         Little_End_32    => Bit (8, Uf));

   end Read_File_Flags;


   procedure Read_Section_Flags (
      Stream : access Ada.Streams.Root_Stream_Type'Class;
      Item   : out Section_Flags_T)
   is
      Ul : Ulong_T;
   begin

      Ulong_T'Read (Stream, Ul);

     Item := (
         Regular   => Ul = 0,
         Dummy     => Bit ( 0, Ul),
         No_Load   => Bit ( 1, Ul),
         Group     => Bit ( 2, Ul),
         Pad       => Bit ( 3, Ul),
         Copy      => Bit ( 4, Ul),
         Only_Text => Bit ( 5, Ul),
         Only_Data => Bit ( 6, Ul),
         Bss_Data  => Bit ( 7, Ul),
         Comment   => Bit ( 9, Ul),
         Overlay   => Bit (10, Ul),
         Lib       => Bit (11, Ul));

   end Read_Section_Flags;


   procedure Read_Line_Number (
      Stream : access Ada.Streams.Root_Stream_Type'Class;
      Item   : out    Line_Number_T)
   is
   begin

      Ulong_T'Read (Stream, Item.Line_Address);

      case Opt.Line_Number_Length is

      when Opt.Short =>

         Ushort_T'Read (Stream, Ushort_T (Item.Line_Number));

      when Opt.Long =>

         Ulong_T'Read (Stream, Item.Line_Number);

      end case;

   end Read_Line_Number;


   function Line_Number_Length return IO.Count
   is
   begin

      case Opt.Line_Number_Length is
      when Opt.Short => return 6;   -- 4 + 2
      when Opt.Long  => return 8;   -- 4 + 4
      end case;

   end Line_Number_Length;


   procedure Read_Strings (
      Stream : access Ada.Streams.Root_Stream_Type'Class;
      Item   : out Strings_T)
   is
      B : Byte_T;
   begin

      begin

         Ulong_T'Read (Stream, Item.Size);

         Output.Note ("COFF Strings.Size = " & Ulong_T'Image (Item.Size));

      exception

      when IO.End_Error =>

         Output.Note ("COFF Strings table absent (end of file).");

         Item.Size := 0;

      end;

      if Item.Size < 4 then
         -- Too short: conclude there is no strings table.

         Output.Note ("No COFF Strings table.");

         Item := No_Strings;

      else
         -- There seems to be a string table.

         Item.Size := Item.Size - Ulong_T(4);
         -- subtracting the size of the first field

         Item.Buffer := new String (1 .. Natural(Item.Size));

         for I in Item.Buffer'Range loop
            Byte_T'Read (Stream, B);
            Item.Buffer(I) := Character'Val(B);
         end loop;

      end if;

      if Opt.Trace_Loading then
         COFF.Text.Put (Item);
      end if;

   end Read_Strings;


   function String_At (
      Offset : Ulong_T;
      Within : Strings_T)
   return String
   is

      Start : Positive := Natural(Offset) - 3;
      -- The size information is included in the offset, so offset - 3
      -- converts from table offset to character index
      -- (f.ex. the first offset 4 is converted to 1 which is
      -- where the buffer starts)

      I : Positive := Start;
      -- The index of a character in Within.Buffer.
      -- Initially the index of the first character of the string.

   begin

      if Within.Size > 0 and Within.Buffer /= null then
         -- We have a non-null string table.

         -- Search for the end of the string:

         loop

            exit when Within.Buffer(I) = Character'Val (0);

            I := I + 1;

         end loop;

         return Within.Buffer (Start .. I - 1);

      else
         -- We have no strings table.

         return "";

      end if;

   end String_At;


   --
   -- COFF file loading:
   --


   procedure Set_Index (
      To_Symbol : in Natural;
      From      : in IO.File_Type;
      Within    : in File_T)
   --
   -- Sets the IO index to the start of symbol number To_Symbol
   -- within the given file.
   --
   is
   begin

      IO.Set_Index (
         File => From,
         To   => IO.Count (Within.Header.Symbol_Table_Loc)
              +  IO.Count (To_Symbol * Opt.Symbol_Entry_Length + 1) );

   end Set_Index;


   --
   --    Type predicates
   --


   function Is_Function (Deriv : Type_Derivation_T) return Boolean
   --
   -- Whether the type is a function.
   -- Pointers to functions etc. are not included.
   --
   is
   begin

      return Deriv'Length > 0 and then Deriv(1) = DT_Fcn;

   end Is_Function;


   function Is_Array (Deriv : Type_Derivation_T) return Boolean
   --
   -- Whether the type is an array or a pointer (to a pointer ..)
   -- to an array.
   --
   is
   begin

      for D in Deriv'Range loop

         if Deriv(D) = DT_Ary then

            return True;

         end if;

         exit when Deriv(D) /= DT_Ptr;
         -- and then return False.

      end loop;

      return False;

   end Is_Array;


   --
   --    Deducing the "nature" of a type
   --


   function Nature (
      Symbol_Type  : in Symbol_Type_T;
      Storage_Code : in Storage_Class_Code_T;
      Name         : in String)
   return Symbol_Nature_T
   --
   -- Deduces the nature of a symbol-record from the type and storage class
   -- of its primary symbol. For some special symbols, the name of the symbol
   -- must also be used.
   --
   is

      Funda : constant Fundamental_Type_T := Fundamental_Type (Symbol_Type);
      Deriv : constant Type_Derivation_T  := Derivation       (Symbol_Type);
      Store : constant Storage_Class_T    := Storage_Class (Storage_Code);

      Result : Symbol_Nature_T;
      -- The result.

   begin

      if Is_Function (Deriv) then
         -- A real subprogram.

         Result := Normal_Function;

      elsif Store = C_Strtag
      or    Store = C_Untag
      or    Store = C_Entag
      then
         -- A tag for a structure, union or enumeration.

         Result := Normal_Tag;

      elsif Is_Array (Deriv) then
         -- An array or pointer(s) to an array (perhaps with more
         -- derived type or a type-def).

         Result := Normal_Array;

      elsif Funda = T_Struct then
         -- A structure (perhaps with a derived type or a type-def).

         Result := Normal_Struct;

      elsif Funda = T_Union then
         -- A union (perhaps with a derived type or a type-def).

         Result := Normal_Union;

      elsif Funda = T_Enum then
         -- An enumeration (perhaps with a derived type or a type-def).

         Result := Normal_Enum;

      else
         -- Not a function, structure, union or enumeration.
         -- The remaining cases are best separated by storage class.

         case Store is

         when C_File =>

            Result := File_Name;

         when C_Stat =>

            if Funda = T_Null and Deriv'Length = 0 then
               -- A section.

               if    Name = ".text" then Result := Text_Section;
               elsif Name = ".data" then Result := Data_Section;
               elsif Name = ".bss"  then Result := Bss_Section;
               else                      Result := Other_Section;
               end if;

            else
               -- An object.

               Result := Normal;

            end if;

         when C_Fcn =>
            -- A bracket for local symbols in a function.

            if    Name = ".bf" then Result := Begin_Function;
            elsif Name = ".ef" then Result := End_Function;
            else

               Output.Error (
                    "Unknown COFF C_Fcn symbol"
                  & Output.Field_Separator
                  & Name);

               Result := End_Function;
               -- Although who knows...

            end if;

         when C_Block =>
           -- A bracket for local symbols in a block.

            if    Name = ".bb" then Result := Begin_Block;
            elsif Name = ".eb" then Result := End_Block;
            else

               Output.Error (
                    "Unknown COFF C_Block symbol"
                  & Output.Field_Separator
                  & Name);

               Result := End_Block;
               -- Although who knows...

            end if;

         when C_Label =>

            Result := Normal_Label;

         when C_Eos =>

            Result := End_Struct;

         when C_Field =>

            Result := Normal_Field;

         when C_Auto
            | C_Ext
            | C_Reg
            | C_Mos
            | C_Arg
            | C_Mou
            | C_Moe
            | C_Regparm
            | C_Tpdef =>

            Result := Normal;

         when C_Efcn
            | C_Null
            | C_Extdef
            | C_Ulabel
            | C_Strtag
            | C_Untag
            | C_Ustatic
            | C_Entag
            | C_Line
            | C_Alias
            | C_Hidden
            | C_Shadow
            | C_Weakext
            | C_Unknown =>

            if Opt.Warn_Storage_Class then

               Output.Warning (
                    "Unsure about COFF symbol with Storage Class "
                  & Storage_Class_T'Image (Store)
                  & Output.Field_Separator
                  & Name);

            end if;

            Result := Normal;

         end case;

      end if;

      return Result;

   end Nature;


   function Nature (Symbol : Symbol_T) return Symbol_Nature_T
   is
   begin

      case Symbol.Kind is

      when Symbol_Entry =>

         return Nature (
            Symbol_Type  => Symbol.S1_Symbol_Type,
            Storage_Code => Symbol.S1_Storage_Class,
            Name         => To_String (Symbol.S1_Name));

      when Symbol_Entry_2 =>

         return Nature (
            Symbol_Type  => Symbol.S2_Symbol_Type,
            Storage_Code => Symbol.S2_Storage_Class,
            Name         => "");
         --
         -- All the symbols for which the Name determines the Nature
         -- are of the Symbol_Entry Kind. Here the Name is unimportant.

      when Auxiliary_Symbol_T =>

         return Auxiliary;

      end case;

   end Nature;


   Aux_Kind : constant array (Owning_Nature_T) of Auxiliary_Symbol_T := (
      Normal_Function  => Aux_Function,
      Normal_Array     => Aux_Array,
      Normal_Struct    => Aux_Names,
      Normal_Union     => Aux_Names,
      Normal_Enum      => Aux_Names,
      Normal_Tag       => Aux_Tag_Name,
      Normal_Field     => Aux_Field,
      File_Name        => Aux_File_Name,
      Begin_Function   => Aux_BF,
      End_Function     => Aux_EF,
      Begin_Block      => Aux_BB,
      End_Block        => Aux_EOB,
      End_Struct       => Aux_EOS,
      Section_Nature_T => Aux_Section);
   --
   -- The kind of auxiliary symbol(s) that can be owned by a primary
   -- symbol of a given owning nature.


   procedure Trace_Symbol (
      Item  : in Symbol_T;
      Index : in Natural)
   --
   -- Handle Trace_Loading for this symbol.
   --
   is
   begin

      if Opt.Trace_Loading then

         COFF.Text.Put (
            Item    => Item,
            Index   => Index,
            Strings => No_Strings,
            Header  => Index = 0);

      end if;

   end Trace_Symbol;


   procedure Dump_Symbol (
      From   : in     IO.File_Type;
      File   : in out File_T;
      Index  : in     Natural)
   --
   -- Dumps the Symbol record at the given Index From the given File,
   -- in hexadecimal octet form.
   --
   is

      Sym : Octets_T (1 .. COFF.Opt.Symbol_Entry_Length);
      -- The octets in the symbol.

      Hex : String (1 .. 3 * COFF.Opt.Symbol_Entry_Length);
      -- The hex string, "xx xx xx ...".

      Next : Positive := Hex'First;
      -- The ndex index in Hex.

   begin

      Set_Index (
         To_Symbol => Index,
         From      => From,
         Within    => File);

      Octets_T'Read (File.IO.Stream, Sym);

      for S in Sym'Range loop

         Hex(Next .. Next + 2) := Image (Sym(S)) & ' ';

         Next := Next + 3;

      end loop;

      Output.Error (
           "Skipping symbol"
         & Natural'Image (Index)
         & Output.Field_Separator
         & Hex(Hex'First .. Hex'Last - 1));

   end Dump_Symbol;


   The_Unknown_Aux_Symbol : constant Symbol_Ref :=
      new Symbol_T'(Kind => Aux_Unknown);
   --
   -- A symbol that represents an unknown kind of Auxiliary symbol.
   -- Single such Symbol_T records carry no information, a single
   -- object is enough to represent all such symbols.


   procedure Get_Auxiliary_Symbols (
      From   : in     IO.File_Type;
      Number : in     Byte_T;
      Nature : in     Symbol_Nature_T;
      File   : in out File_T;
      Index  : in out Natural)
   --
   -- Gets a given Number of Auxiliary symbols, for a primary symbol of
   -- a given Nature, From a given file, and stores them in the array
   -- File.Symbols starting at the given Index.
   --
   -- If the primary symbol cannot have auxiliary symbols (of a type
   -- that we know about), but Number is positive, the procedure skips
   -- this number of symbol records and places Aux_Unknown symbols in the
   -- corresponding locations of File.Symbols.
   --
   -- Returns Index as the next unused location in File.Symbols.
   --
   is

      Symbols : Symbols_T renames File.Symbols.all;

      Kind : Auxiliary_Symbol_T;
      -- The kind of the auxiliary symbols.

   begin

      if Number = 0 then
         -- No auxiliary symbols. OK.

         null;

      elsif Nature in Owning_Nature_T then
         -- This primary symbol can own auxiliary symbols.

         Kind := Aux_Kind(Nature);

         for N in 1 .. Number loop

            Set_Index (
               To_Symbol => Index,
               From      => From,
               Within    => File);

            Symbols(Index) := new Symbol_T (Kind);

            Symbol_T'Read (File.IO.Stream, Symbols(Index).all);

            Trace_Symbol (
               Item  => Symbols(Index).all,
               Index => Index);

            Index := Index + 1;

         end loop;

      else
         -- The primary symbol cannot be an owner.

         Output.Error (
              "COFF "
            & Symbol_Nature_T'Image (Nature)
            & " symbol"
            & Natural'Image (Index - 1)
            & " has"
            & Byte_T'Image (Number)
            & " unexpected auxiliary symbols; skipped.");

         for N in 1 .. Number loop

            Dump_Symbol (From, File, Index);

            Symbols(Index) := The_Unknown_Aux_Symbol;

            Trace_Symbol (
               Item  => Symbols(Index).all,
               Index => Index);

            Index := Index + 1;

         end loop;

      end if;

   end Get_Auxiliary_Symbols;


   procedure Get_Symbol_Entry (
      From  : in     IO.File_Type;
      File  : in out File_T;
      Index : in out Natural)
   --
   -- Gets File.Symbols(Index) as a Symbol_Entry record from
   -- the file, and also gets the associated auxiliary symbols,
   -- placing them in File.Symbols(Index+1 ...).
   -- Returns Index as the next unused location in Symbols.
   --
   is

      Primary : constant Natural := Index;
      -- The index of the primary symbol.

      Symbols : Symbols_T renames File.Symbols.all;

   begin
      -- Create and read the primary symbol:

      Symbols(Primary) := new Symbol_T (Symbol_Entry);

      Symbol_T'Read (File.IO.Stream, Symbols(Primary).all);

      Trace_Symbol (
         Item  => Symbols(Primary).all,
         Index => Primary);

      Index := Primary + 1;

      -- Get the auxiliary symbols if any:

      Get_Auxiliary_Symbols (
         From   => From,
         Number => Symbols(Primary).S1_Number_Aux,
         Nature => Nature (Symbols(Primary).all),
         File   => File,
         Index  => Index);

   end Get_Symbol_Entry;


   procedure Get_Symbol_Entry_2 (
      From  : in     IO.File_Type;
      File  : in out File_T;
      Index : in out Natural)
   --
   -- Gets File.Symbols(Index) as a Symbol_Entry_2 record from
   -- the file, and also gets the associated auxiliary symbols,
   -- placing them in File.Symbols(Index+1 ...).
   -- Returns Index as the next unused location in Symbols.
   --
   is

      Primary : constant Natural := Index;
      -- The index of the primary symbol.

      Symbols : Symbols_T renames File.Symbols.all;

   begin
      -- Create and read the primary symbol:

      Symbols(Primary) := new Symbol_T (Symbol_Entry_2);

      Symbol_T'Read (File.IO.Stream, Symbols(Primary).all);

      Trace_Symbol (
         Item  => Symbols(Primary).all,
         Index => Primary);

      Index := Primary + 1;

      -- Get the auxiliary symbols if any:

      Get_Auxiliary_Symbols (
         From   => From,
         Number => Symbols(Primary).S2_Number_Aux,
         Nature => Nature (Symbols(Primary).all),
         File   => File,
         Index  => Index);

   end Get_Symbol_Entry_2;


   procedure Get_Symbols (
      From : in     IO.File_Type;
      File : in out File_T)
   --
   -- Reads COFF symbol table from the stream associated with the file.
   -- On entry, File.Header.Number_Of_Symbols must be set, and the
   -- file and stream must be open.
   -- Returns freshly allocated File.Symbols array containing the
   -- specified number of symbols.
   --
   is

      Index : Natural;
      -- The running index of the next symbol to be read.

      Ul : Ulong_T;
      -- The first "long" of the symbol entry, to show the type of
      -- symbol being read.

   begin

      File.Symbols := new Symbols_T (
         0 .. Integer (File.Header.Number_Of_Symbols) - 1);

      Index := File.Symbols'First;

      while Index <= File.Symbols'Last loop

         -- For every symbol_table_line
         -- get it with possible auxiliary fields.

         Set_Index (
            To_Symbol => Index,
            From      => From,
            Within    => File);

         Ulong_T'Read (File.IO.Stream, Ul);
         -- Dummy read to check the form of the symbol entry.

         Set_Index (
            To_Symbol => Index,
            From      => From,
            Within    => File);

         if Ul = 0 then
            Get_Symbol_Entry_2 (From, File, Index);
         else
            Get_Symbol_Entry   (From, File, Index);
         end if;

      end loop;

   end Get_Symbols;


   procedure Get_Section (
      From    : in     IO.File_Type;
      File    : in     File_T;
      Section :    out Section_T)
   --
   -- Get a section from the current position in the file.
   -- The section data are not yet loaded.
   --
   is

      Line_Len : constant IO.Count := Line_Number_Length;
      -- The length, in octets, of a line-number entry.

   begin

      Section.IO.Index := IO.Index (From);

      -- Get the section header:

      Section_Header_T'Read (File.IO.Stream, Section.Header);

      if Opt.Trace_Loading then

         COFF.Text.Put (Section.Header);

      end if;

      -- Get the relocation info:

      Section.Relocations := new Relocations_T
         (1 .. Integer(Section.Header.Relocation_Entries));

      for I in Section.Relocations'Range loop

         IO.Set_Index (
            File => From,
            To   => IO.Count (Section.Header.Relocation_Loc)
                  + IO.Count((I - 1) * Relocation_Length_C + 1));

         Relocation_T'Read (File.IO.Stream, Section.Relocations(I));

         if Opt.Trace_Loading then

            COFF.Text.Put (Section.Relocations(I));

         end if;

      end loop;

      -- Get the line-number info:

      Section.Line_Numbers := new Line_Numbers_T
         (1 .. Integer(Section.Header.Line_Number_Entries));

      for I in Section.Line_Numbers'Range loop

         IO.Set_Index (
            File => From,
            To   => IO.Count(Section.Header.Line_Number_Loc)
                  + IO.Count(I - 1) * Line_Len + 1);

         Line_Number_T'Read (File.IO.Stream, Section.Line_Numbers(I));

         if Opt.Trace_Loading then

            COFF.Text.Put (Section.Line_Numbers(I));

         end if;

      end loop;

   end Get_Section;


   procedure Get_File (
      From : in     IO.File_Type;
      File : in out File_T)
   --
   -- Get COFF information from the (open) COFF file.
   --
   is

      Sect_Orig : IO.Positive_Count;
      -- The file location where the sections start.

   begin

      File.IO.Index := IO.Index (From);

      -- Get file headers:

      File_Header_T'Read (File.IO.Stream, File.Header);

      if Opt.Trace_Loading then

         COFF.Text.Put (File.Header);

      end if;

      if File.Header.Optional_Header_Length = Optional_Header_Length_C
      then

         Optional_File_Header_T'Read (File.IO.Stream, File.Optional);

         Sect_Orig := IO.Index (From);

         if Opt.Trace_Loading then

            COFF.Text.Put (File.Optional);

         end if;

      else

         if File.Header.Optional_Header_Length /= 0 then

            Output.Warning (Text =>
                "Unexpected length "
               & Ushort_T'Image (File.Header.Optional_Header_Length)
               & " of optional file header (skipped).");

         end if;

         Sect_Orig := IO.Index (From)
           + IO.Count (File.Header.Optional_Header_Length);

      end if;

      -- Get section headers with relocation and line number information:

      File.Sections := new Sections_T
         (1 .. Section_Number_T (File.Header.Number_Of_Sections));

      for S in File.Sections'range loop

         if Opt.Trace_Loading then

            Ada.Text_IO.Put_Line (
               "Section Number" & Section_Number_T'Image (S));

         end if;

         IO.Set_Index (
            File => From,
            To   => Sect_Orig
                  + IO.Count( (S - 1) * Section_Header_Length_C));

         Get_Section (
            From    => From,
            File    => File,
            Section => File.Sections(S));

      end loop;

      -- Get the symbols:

      Get_Symbols (From, File);

      -- Get the strings:

      IO.Set_Index (
         File => From,
         To   => IO.Count (File.Header.Symbol_Table_Loc)
               + IO.Count (Integer(File.Header.Number_Of_Symbols)
                               * Opt.Symbol_Entry_Length + 1));

      Output.Note (
           "COFF string table at"
         & IO.Count'Image (IO.Index (From)));

      Read_Strings (File.IO.Stream, File.Strings);

   end Get_File;


   --
   -- Interface procedures
   --


   function Accepts (
      Name : in String;
      File : in IO.File_Type)
   return Boolean
   is

      Stream : IO.Stream_Access := IO.Stream (File);
      -- Stream access to the file.

      File_Header : File_Header_T;
      -- The COFF file header.

      Result : Boolean;
      -- The outcome.

   begin

      if IO.Size (File) < File_Header_Length_C then
         -- This file is too short to be a COFF file.

         Result := False;

      else
         -- Read and check the Ident.

         IO.Set_Index (File, IO.Positive_Count'First);

         File_Header_T'Read (Stream, File_Header);

         Result :=
               File_Header.Magic_Number = Opt.Magic_Number
           and IO.Count (File_Header.Symbol_Table_Loc) < IO.Size (File);

      end if;

      IO.Set_Index (File, IO.Positive_Count'First);

      return Result;

   exception

   when Constraint_Error =>
      -- Can happen for example when Symbol_Table_Loc is larger
      -- than IO.Count'Last.

      return False;

   end Accepts;


   procedure Put (
      File    : in File_Ref;
      Symbols : in Boolean := False;
      Strings : in Boolean := False)
   is
      use Ada.Text_IO;
   begin

      Put_Line (Image (File.IO));

      COFF.Text.Put (File.Header);

      if File.Header.Optional_Header_Length = Optional_Header_Length_C
      then

         COFF.Text.Put (File.Optional);

      end if;

      for S in File.Sections'range loop

         New_Line;

         COFF.Text.Put (File.Sections(S));

      end loop;

      if Symbols then

         if File.Symbols /= null then

            COFF.Text.Put (
               Item    => File.Symbols.all,
               Strings => File.Strings);

         else

            Put_Line ("No symbols.");

         end if;

      end if;

      if Strings then

         COFF.Text.Put (File.Strings);

      end if;

   exception

      when E : others =>

         Output.Exception_Info (
            Text       => "Put (COFF file)",
            Occurrence => E);

   end Put;


   procedure Load (
      From : in     IO.File_Type;
      File :    out File_Ref)
   is
   begin

      File := new File_T;

      File.IO.Stream := IO.Stream (From);

      Get_File (
         From => From,
         File => File.all);

   exception

      when IO.End_Error =>
         Output.Error (Text => "Unexpected end of COFF file");
         raise;

      when E : others =>

         Output.Exception_Info (
            Text       => "Loading COFF file",
            Occurrence => E);

         raise;

   end Load;


   procedure Load (
      File_Name : in     String;
      File      :    out File_Ref)
   is

      From : IO.File_Type;

   begin

      Output.Set_Program_File (To => File_Name);

      IO.Open (
         File => From,
         Mode => IO.In_File,
         Name => File_Name);

      Load (
         From => From,
         File => File);

      IO.Close (From);

      Output.No_Program_File;

   exception

   when IO.Name_Error =>

      Output.Error (Text => "COFF file not found");

      Output.No_Program_File;

      raise;

   when IO.Use_Error =>

      Output.Error (Text => "Cannot read COFF file");

      Output.No_Program_File;

      raise;

   end Load;


  procedure Load_Section (
      Section : in     Section_T;
      From    : in     IO.File_Type;
      Within  : in     File_Ref;
      Data    :    out Octets_T)
   is

      Index : IO.Positive_Count;
      -- The file position for the section data.

   begin

      if Data'Length /= Section.Header.Length then

         Output.Fault (
            Location =>
               "Formats.COFF.Load_Section",
            Text =>
                 "Section """
               & To_String (Section.Header.Name)
               & """ length /= " & Natural'Image (Data'Length)
               & ", not loaded.");

         raise Constraint_Error;

      else

         Index := Within.IO.Index + IO.Count(Section.Header.Data_Loc);

         IO.Set_Index (
            File => From,
            To   => Index);

         Octets_T'Read (Within.IO.Stream, Data);

      end if;

   exception

      when IO.End_Error =>

         Output.Error (Text =>
             "Unexpected end of COFF file while reading section """
            & To_String (Section.Header.Name)
            & """ from IO index"
            & IO.Positive_Count'Image (Index));

         raise;

      when E : others =>

         Output.Exception_Info (
            Text       => "Loading COFF file section",
            Occurrence => E);

         raise;

   end Load_Section;


   procedure Load_And_Put (
      From    : in IO.File_Type;
      Data    : in Boolean := False;
      Symbols : in Boolean := False;
      Strings : in Boolean := False)
   is
      use Ada.Text_IO;

      File : File_Ref;

      Unit : constant Positive := 1;
      -- Bytes per addressing unit, assumed.


      procedure Load_And_Put (Section : in Section_T)
      --
      -- Loads and displays the given section.
      --
      is

         Octets : Octets_T (0 .. Natural (Section.Header.Length) - 1);

      begin

         Load_Section (
            Section => Section,
            From    => From,
            Within  => File,
            Data    => Octets);

         COFF.Text.Put_Data (
            Section => Section,
            Data    => Octets,
            Format  => (1 => 4),
            Columns => 8,
            Unit    => Unit);

      end Load_And_Put;


      procedure Put_Section (Section : in Section_T)
      --
      -- Loads and displays the given section, unless it is a
      -- dummy, no-load, bss, overlay or lib section.
      --
      is

         Flags : Section_Flags_T renames Section.Header.Flags;

      begin

         Put ("Section " & To_String (Section.Header.Name));

         if Flags.Dummy
         or Flags.No_Load
         or Flags.Bss_Data
         or Flags.Comment
         or Flags.Overlay
         or Flags.Lib
         then

            Put_Line (", not loaded.");

         elsif Section.Header.Length = 0 then

            Put_Line (", empty.");

         else

            Put_Line (
                " data follows (address unit = "
               & Positive'Image (Unit) & " bytes):");

            Load_And_Put (Section);

         end if;

         New_line;

      end Put_Section;

   begin  -- Load_And_Put

      Load (
        From => From,
        File => File);

      Put (
         File    => File,
         Symbols => Symbols,
         Strings => Strings);

      if Data then

         for S in File.Sections'Range loop

            Put_Section (File.Sections(S));

         end loop;

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

      Output.Set_Program_File (To => File_Name);

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

      Output.No_Program_File;

   exception

   when IO.Name_Error =>

      Output.Error (Text => "COFF file not found");

      Output.No_Program_File;

   when IO.Use_Error =>

      Output.Error (Text => "Cannot read COFF file");

      Output.No_Program_File;

   end Load_And_Put;


   function Name_Of (
      Section : Section_Number_T;
      Within  : File_T)
   return String
   is
   begin
      return To_String (Within.Sections(Section).Header.Name);
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
      return Natural (Within.Sections(Section).Header.Length);
   end Length_Of;


   function String_At (
      Offset  : Ulong_T;
      Within  : File_Ref)
   return String
   is
   begin

      return String_At (Offset, Within.Strings);

   end String_At;


   procedure Close (File : in out File_Ref)
   is
   begin

      -- TBA deallocation, perhaps later.

      File := null;

   end Close;


end Formats.COFF;
