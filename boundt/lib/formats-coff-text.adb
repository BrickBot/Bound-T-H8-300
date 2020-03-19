-- Formats.COFF.Text (body)
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
-- $Revision: 1.11 $
-- $Date: 2015/10/24 20:53:51 $
--
-- $Log: formats-coff-text.adb,v $
-- Revision 1.11  2015/10/24 20:53:51  niklas
-- Moved to free licence.
--
-- Revision 1.10  2007-03-06 20:21:08  niklas
-- Extended Put (Optional_File_Header_T) to show the Magic number
-- also in hex.
-- Extended Put (Name : String ..) to handle Aux_Unknown symbols.
-- Extended Put (Symbols_T) to handle null elements in Symbols.
--
-- Revision 1.9  2006/11/26 22:02:11  niklas
-- Added New_Line after Symbol Table.
-- Checking for null Strings table.
--
-- Revision 1.8  2005/08/24 10:46:47  niklas
-- Removed extra "(Section)" output.
--
-- Revision 1.7  2005/07/30 09:36:24  niklas
-- Added hex form of Magic Number.
--
-- Revision 1.6  2005/07/26 11:49:32  niklas
-- Corrected output of Begin/End Block.
--
-- Revision 1.5  2005/07/25 19:26:53  niklas
-- Added a Strings parameter to Put for Symbol_T and Symbols_T, to
-- display the symbol names even when longer than 8 characters.
-- Changed Put for Symbol_T to allow more space for symbol names,
-- to display the fundamental type always (even when it is T_Null)
-- and to use a comma to separate it from the storage class.
--
-- Revision 1.4  2005/07/24 19:48:44  niklas
-- Changed Put (Symbol_T) to show all auxiliary symbols in a less
-- obtrusive form and to show Aux_Field records.
--
-- Revision 1.3  2005/03/24 19:47:10  niklas
-- Procedure Put (Symbol_T) now displays the whole type derivation
-- sequence, and displays the fundamental type afterwards, so that
-- a pointer to an array of ints is displayed as DT_PTR DT_ARY T_INT.
--
-- Revision 1.2  2004/05/31 20:22:40  niklas
-- Line numbers are now Ulong_T.
-- Added some heading lines for the parts of Put (File_T).
-- Added section number output.
--
-- Revision 1.1  2004/04/24 18:08:34  niklas
-- First Tidorum version, as child of Formats.
--
--
-- <Log: coff-text.adb>
-- Revision 1.1  2000/07/02 18:41:54  holsti
-- First version.
--


with Ada.Text_IO;


package body Formats.COFF.Text is


   procedure Put (Item : in Relocation_T)
   is
      use Ada.Text_IO;
   begin
      Put ("Virtual Address: "  & Hex_Image (Item.Virtual_Address));
      Put (" Symbol Index: " & Symbol_Index_T'Image (Item.Symbol_Index));
      Put (" Relocation type: " & Hex_Image (Item.Relocation_Type));
      New_Line;
   end Put;


   procedure Put (Item : in Line_Number_T)
   is
      use Ada.Text_IO;
   begin
      if Item.Line_Number = 0 then
         Put ("Symbol Index: " & Ulong_T'Image (Item.Line_Address));
      else
         Put ("          Physical Address: "
            & Hex_Image (Item.Line_Address));
         Put (" Line Number: " & Ulong_T'Image (Item.Line_Number));
      end if;
      New_Line;
   end Put;


   procedure Put (Item : in Section_Header_T)
   is
      use Ada.Text_IO;
   begin

      Put_Line ("Section Name: " & To_String (Item.Name));

      Put_Line ("Physical Address   = " & Hex_Image (Item.Physical_Address));
      Put_Line ("Virtual  Address   = " & Hex_Image (Item.Virtual_Address));
      Put_Line ("Section  length    = " & Hex_Image (Item.Length));

      Put_Line ("Data location      = " & Hex_Image (Item.Data_Loc));
      Put_Line ("Relocation loc     = " & Hex_Image (Item.Relocation_Loc));
      Put_Line ("Line number loc    = " & Hex_Image (Item.Line_Number_Loc));
      Put_Line ("Relocation entries ="
                & Ushort_T'Image (Item.Relocation_Entries));
      Put_Line ("Line-num entries   ="
                & Ushort_T'Image (Item.Line_Number_Entries));

      Put_Line ("Section Header flags:");
      Put_Line ("   Regular : " & Boolean'Image(Item.Flags.Regular  ));
      Put_Line ("   Dummy   : " & Boolean'Image(Item.Flags.Dummy    ));
      Put_Line ("   No Load : " & Boolean'Image(Item.Flags.No_Load  ));
      Put_Line ("   Group   : " & Boolean'Image(Item.Flags.Group    ));
      Put_Line ("   Pad     : " & Boolean'Image(Item.Flags.Pad      ));
      Put_Line ("   Copy    : " & Boolean'Image(Item.Flags.Copy     ));
      Put_Line ("   Text    : " & Boolean'Image(Item.Flags.Only_Text));
      Put_Line ("   Data    : " & Boolean'Image(Item.Flags.Only_Data));
      Put_Line ("   BSS     : " & Boolean'Image(Item.Flags.Bss_Data ));
      Put_Line ("   Comment : " & Boolean'Image(Item.Flags.Comment  ));
      Put_Line ("   Overlay : " & Boolean'Image(Item.Flags.Overlay  ));
      Put_Line ("   Lib     : " & Boolean'Image(Item.Flags.Lib      ));

   end Put;


   procedure Put (Item : in Section_T)
   is
      use Ada.Text_IO;
   begin

      Put (Item.Header);

      Put_Line (Image (Item.IO));

      if Item.Header.Relocation_Entries > 0 then

         Put_Line ("Relocation entries:");

         for I in Item.Relocations'Range loop
            Put (Item.Relocations(I));
         end loop;

      end if;

      if Item.Header.Line_Number_Entries > 0 then

         Put_Line ("Line Number entries:");

         for I in Item.Line_Numbers'Range loop
            Put (Item.Line_Numbers(I));
         end loop;

      end if;

   end Put;


   procedure Put (Item : in File_Header_T)
   is
      use Ada.Text_IO;
   begin

      Put_Line ("Magic              ="
                & Ushort_T'Image(Item.Magic_Number)
                & " = "
                & Hex_Image (Item.Magic_Number));

      Put_Line ("Number of sections ="
                & Ushort_T'Image(Item.Number_Of_Sections));

      Put_Line ("Time and date      ="
                & Time_T'Image(Item.Time_And_Date));

      Put_Line ("Symbol table loc   ="
                & File_Loc_T'Image(Item.Symbol_Table_Loc));

      Put_Line ("Number of symbols  ="
                & Ulong_T'Image(Item.Number_Of_Symbols));

      Put_Line ("Optional header    ="
                & Ushort_T'Image(Item.Optional_Header_Length));

      Put_Line ("File Header flags:");
      Put_Line ("   Relocation Info : "
                & Boolean'Image (Item.Flags.Relocation_Info));
      Put_Line ("   Executable      : "
                & Boolean'Image (Item.Flags.Executable));
      Put_Line ("   No Line Numbers : "
                & Boolean'Image (Item.Flags.No_Line_Numbers));
      Put_Line ("   No Local Symbols: "
                & Boolean'Image (Item.Flags.No_Local_Symbols));
      Put_Line ("   Reversed 16-bit : "
                & Boolean'Image (Item.Flags.Little_End_16));
      Put_Line ("   Reversed 32-bit : "
                & Boolean'Image (Item.Flags.Little_End_32));

   end Put;


   procedure Put (Item : in Optional_File_Header_T)
   is
      use Ada.Text_IO;
   begin
      Put_Line("Optional Header");

      Put_Line ("Magic              ="
                & Ushort_T'Image(Item.Magic_Number)
                & " = "
                & Hex_Image (Item.Magic_Number));

      Put_Line ("Version stamp      ="
                & Ushort_T'Image(Item.Version_Stamp));

      Put_Line ("Text size          ="
                & Ulong_T'Image(Item.Text_Size));

      Put_Line ("Data size          ="
                & Ulong_T'Image(Item.Data_Size));

      Put_Line ("Bss size           ="
                & Ulong_T'Image(Item.Bss_Size));

      Put_Line ("Entry point        ="
                & Hex_Image(Item.Entry_Point));

      Put_Line ("Text start         ="
                & Hex_Image(Item.Text_Start));

      Put_Line ("Data start         ="
                & Hex_Image(Item.Data_Start));

   end Put;


   procedure Put (
      Item    : in Symbol_T;
      Index   : in Natural;
      Strings : in Strings_T;
      Header  : in Boolean := False)
   is

      use Ada.Text_IO;

      Col_Name   : constant := 10;
      Col_Offset : constant := 30;
      Col_Value  : constant := 42;
      Col_SecNum : constant := 54;
      Col_Type   : constant := 63;
      Col_Sclass : constant := 72;
      Col_NumAux : constant := 80;
      Col_Stype  : constant := 90;

      Col_Aux    : constant := 30;


      procedure Begin_Aux (Kind : in String)
      is
      begin

         Set_Col (Col_Aux); Put ('(' & Kind & ")  ");

      end Begin_Aux;


      procedure Put (
         Name           : in String;
         Offset         : in String;
         Value          : in Ulong_T;
         Section_Number : in Section_Number_T;
         Symbol_Type    : in Symbol_Type_T;
         Storage_Class  : in Storage_Class_Code_T;
         Number_Aux     : in Byte_T)
      is
         Funda : constant Fundamental_Type_T := Fundamental_Type (Symbol_Type);
         Deriv : constant Type_Derivation_T  := Derivation       (Symbol_Type);

      begin

         Set_Col (Col_Name  ); Put (Name);
         Set_Col (Col_Offset); Put (Offset);
         Set_Col (Col_Value ); Put (Hex_Image (Value));
         Set_Col (Col_SecNum); Put (Section_Number_T'Image (Section_Number));
         Set_Col (Col_Type  ); Put (Hex_Image (Symbol_Type));
         Set_Col (Col_Sclass);
            Put (Storage_Class_Code_T'Image (Storage_Class));
         Set_Col (Col_NumAux); Put (Byte_T'Image (Number_Aux));

         Set_Col (Col_Stype );

         for D in Deriv'Range loop
            Put (Derived_Type_T'Image (Deriv(D)) & ' ');
         end loop;

         Put (Fundamental_Type_T'Image (Funda) & ", ");

         Put (Storage_Class_T'Image (COFF.Storage_Class (Storage_Class)));

      end Put;


   begin  -- Put Symbol_T

      if Header then
                               Put ("Index"  );
         Set_Col (Col_Name  ); Put ("Name"   );
         Set_Col (Col_Offset); Put ("Offset" );
         Set_Col (Col_Value ); Put ("Value"  );
         Set_Col (Col_SecNum); Put ("SecNum" );
         Set_Col (Col_Type  ); Put ("Type"   );
         Set_Col (Col_Sclass); Put ("Sclass" );
         Set_Col (Col_NumAux); Put ("NumAux" );
         New_Line;
      end if;

      Put (Natural'Image (Index));

      case Item.Kind is

      when Symbol_Entry =>

         Put (
            Name           => To_String(Item.S1_Name),
            Offset         => "",
            Value          => Item.S1_Value,
            Section_Number => Item.S1_Section_Number,
            Symbol_Type    => Item.S1_Symbol_Type,
            Storage_Class  => Item.S1_Storage_Class,
            Number_Aux     => Item.S1_Number_Aux);

      when Symbol_Entry_2 =>

         Put (
            Name           => String_At (Item.S2_Offset, Strings),
            Offset         => Hex_Image (Item.S2_Offset),
            Value          => Item.S2_Value,
            Section_Number => Item.S2_Section_Number,
            Symbol_Type    => Item.S2_Symbol_Type,
            Storage_Class  => Item.S2_Storage_Class,
            Number_Aux     => Item.S2_Number_Aux);

      when Aux_File_Name =>

         Begin_Aux ("File name");
         Put ("fname: ");
         Put (To_String(Item.File_Name));

      when Aux_Section =>

         Begin_Aux ("Section");
         Put ("SecLen: " & Hex_Image(Item.Sec_Length));
         Put (" nReloc:" & Ushort_T'Image(Item.Sec_Number_Relocation));
         Put (" nLnum:" & Ushort_T'Image(Item.Sec_Number_Line_Number));

      when Aux_Tag_Name =>

         Begin_Aux ("Tag Name");
         Put ("Size:" & Ushort_T'Image(Item.Tag_Size));
         Put (" Next:" &  Symbol_Index_T'Image(Item.Tag_Next_Entry));

      when Aux_EOS =>

         Begin_Aux ("End Of Structure");
         Put ("TagIdx:" &  Symbol_Index_T'Image(Item.EOS_Tag_Index));
         Put (" Size:" & Ushort_T'Image(Item.EOS_Size));

      when Aux_Field =>

         Begin_Aux ("Bit Field");
         Put ("TagIdx:" &  Symbol_Index_T'Image(Item.Field_Tag_Index));
         Put (" Size in bits:" & Ulong_T'Image(Item.Field_Size_Bits));

      when Aux_Function =>

         Begin_Aux ("Function");
         Put ("TagIdx:" &  Symbol_Index_T'Image(Item.Func_Tag_Index));
         Put (" Size:" & Ulong_T'Image(Item.Func_Size));
         Put (" LinePtr: " & Hex_Image(Item.Func_Line_Pointer));
         Put (" Next:" &  Symbol_Index_T'Image(Item.Func_Next_Entry));

      when Aux_Array =>

         Begin_Aux ("Array");
         Put ("TagIdx:" &  Symbol_Index_T'Image(Item.Array_Tag_Index));
         Put (" LNum:" & Ushort_T'Image(Item.Array_Line_Number));
         Put (" Size:" & Ushort_T'Image(Item.Array_Size));
         Put (" Dims:" & Ushort_T'Image(Item.Array_Dimensions(0)) & " "
              & Ushort_T'Image(Item.Array_Dimensions(1)) & " "
              & Ushort_T'Image(Item.Array_Dimensions(2)) & " "
              & Ushort_T'Image(Item.Array_Dimensions(3)) & " " );

      when Aux_EOB =>

         Begin_Aux ("End Block");
         Put (" LineNum:" & Ushort_T'Image(Item.EOB_Line_Number));

      when Aux_EF =>

         Begin_Aux ("End Function");
         Put (" LineNum:" & Ushort_T'Image(Item.EF_Line_Number));

      when Aux_BB =>

         Begin_Aux ("Begin Block");
         Put (" LineNum:" & Ushort_T'Image(Item.BB_Line_Number));
         Put (" Next:" & Symbol_Index_T'Image(Item.BB_Next_Entry));

      when Aux_BF =>

         Begin_Aux ("Begin Function");
         Put (" LineNum:" & Ushort_T'Image(Item.BF_Line_Number));
         Put (" Next:" & Symbol_Index_T'Image(Item.BF_Next_Entry));

      when Aux_Names =>

         Begin_Aux ("Struct/Union/Enum Name");
         Put ("TagIdx:" & Symbol_Index_T'Image(Item.Names_Tag_Index));
         Put (" Size:" & Ushort_T'Image(Item.Names_Size));

      when Aux_Unknown =>

         Begin_Aux ("Unknown");

      end case;

      New_Line;

   end Put;


   procedure Put (
      Item    : in Symbols_T;
      Strings : in Strings_T)
   is
      use Ada.Text_IO;

   begin

      Put_Line("Symbol Table");

      for I in Item'Range loop

         if Item(I) /= null then

            Put (
               Item    => Item(I).all, 
               Index   => I,
               Strings => Strings,
               Header  => I = Item'First);

         else

            Put_Line (
                 Natural'Image (I)
               & " - unknown symbol");

         end if;

      end loop;

      New_Line;

   end Put;


   procedure Put (Item : in Strings_T)
   is
      use Ada.Text_IO;
      New_String : Boolean := True;

      Col_Offset : constant := 1;
      Col_Item   : constant := 15;
   begin

      if Item.Size > 0 and Item.Buffer /= null then

         Put_Line (
              "String Table, size: "
            & Ulong_T'Image(Item.Size));

         Set_Col (Col_Offset); Put ("Offset");
         Set_Col (Col_Item  ); Put ("Item"  );
         New_Line;

         for I in Item.Buffer'Range loop

            if New_String then
               New_String := False;
               Set_Col (Col_Offset); Put (Hex_Image(Ulong_T(3 + I)));
               Set_Col (Col_Item);
            end if;

            if Item.Buffer(I) = Character'Val(0) then
               New_Line;
               New_String := True;
            else
               Put (Item.Buffer(I));
            end if;

         end loop;

      else

         Put_Line ("String table, empty.");

      end if;

      New_Line;

   end Put;


   procedure Put (Item : in File_T)
   is
      use Ada.Text_IO;
   begin

      Put_Line ("COFF File:");

      New_Line;

      Put (Item.Header);

      New_Line;

      Put (Item.Optional);  -- TBC if always

      for S in Item.Sections'Range loop

         New_Line;

         Put_Line ("Section Number " & Section_Number_T'Image (S));

         Put (Item.Sections(S));

      end loop;

      New_Line;

      Put_Line ("COFF Symbol Table:");

      Put (Item => Item.Symbols.all, Strings => Item.Strings);

      New_Line;

      Put_Line ("COFF String Table:");

      Put (Item.Strings);

   end Put;


   procedure Put_Format (
      Bytes   : in     Octets_T;
      Format  : in     Format_T;
      Next    : in out Natural)
   --
   -- Outputs bytes in hex, using Format once (no cycling),
   -- starting from Bytes(Next).
   -- Returns in Next the index of the first unwritten byte.
   -- This may be > Bytes'Last to indicate that all bytes were
   -- written.
   --
   is
      use Ada.Text_IO;
   begin

      for F in Format'Range loop

         for K in 1 .. Format(F) loop

            if Next <= Bytes'Last then
               Put (Image (Bytes(Next)));
            else
               Put ("??");
            end if;

            Next := Next + 1;

         end loop;

         if F < Format'Last then
            Put ('_');
         end if;

      end loop;

   end Put_Format;


   procedure Put_Data (
      Section : in Section_T;
      Data    : in Octets_T;
      Format  : in Format_T;
      Columns : in Positive := 1;
      Unit    : in Positive := 1)
   is

      use Ada.Text_IO;

      Next : Natural := Data'First;
      -- The index of the next byte to be output.

      Col : Positive := 1;
      -- The next column to be output.

      Physical_Address : Physical_Address_T;
      -- The virtual address of the next byte to be output
      -- (when it starts a new line).

      Col_Data : constant := 12;

   begin

                          Put ("Phys Addr");
      Set_Col (Col_Data); Put ("COFF data");
      New_Line;

      while Next <= Data'Last loop

         if Col = 1 then

            Physical_Address := 
                 Section.Header.Physical_Address
               + Physical_Address_T (
                    (Next - Data'First) / Positive'Pos (Unit));

            Put (Hex_Image (Physical_Address));
            Set_Col (Col_Data);

         end if;

         loop
            Put_Format (
               Bytes  => Data,
               Format => Format,
               Next   => Next);

            exit when Col  = Columns
                   or Next > Data'Last;

            Put ("  ");
            Col := Col + 1;
         end loop;

         New_Line;

         Col := 1;

      end loop;

   end Put_Data;


end Formats.COFF.Text;
