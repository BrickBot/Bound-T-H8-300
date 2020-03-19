-- Formats.ELF.Text (body)
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
-- $Revision: 1.4 $
-- $Date: 2015/10/24 20:53:53 $
--
-- $Log: formats-elf-text.adb,v $
-- Revision 1.4  2015/10/24 20:53:53  niklas
-- Moved to free licence.
--
-- Revision 1.3  2008-02-29 20:41:22  niklas
-- Added Em_ARM = 40.
--
-- Revision 1.2  2006/04/13 18:50:25  niklas
-- Header now says _ELF_ Symbol Table.
--
-- Revision 1.1  2004/04/24 18:03:42  niklas
-- First Tidorum version, as child of Formats.
--
--
-- <Log: elf-text.adb>
-- Revision 1.2  2003/02/17 14:05:29  holsti
-- Corrected handling of STABS symbols (Stab_Line_T) for multi-module
-- programs, interpreting code 16#00# as N_HdrSym rather than N_Undef.
-- Included the string-section number in the STABS output header.
--
-- Revision 1.1  2001/08/27 09:12:56  saarinen
-- First version.
--


with Ada.Text_IO;

with Formats.Stabs.Text;


package body Formats.ELF.Text is


--     procedure Put (Item : in Relocation_T)
--     is
--        use Ada.Text_IO;
--     begin
--        --Put ("Virtual Address: "  & Hex_Image (Item.Virtual_Address));
--        --Put (" Symbol Index: " & Symbol_Index_T'Image (Item.Symbol_Index));
--        --Put (" Relocation type: " & Hex_Image (Item.Relocation_Type));
--        New_Line;
--     end Put;


   procedure Put (Item : in Line_Number_T)
   is
      use Ada.Text_IO;
   begin
      --if Item.Line_Number = 0 then
      --   Put ("Symbol Index: " & Uword_T'Image (Item.Line_Address));
      --else
      --   Put ("          Physical Address: "
      --      & Hex_Image (Item.Line_Address));
      --   Put (" Line Number: " & Ushort_T'Image (Item.Line_Number));
      --end if;
      New_Line;

   end Put;


   procedure Put (Item : in Section_Header_T)
   is
      use Ada.Text_IO;
   begin

      Put_Line ("Section Name         : " & Hex_Image (Item.Sh_Name));

      Put      ("Section type         : " & Hex_Image (Item.Sh_Type) & " = ");

      case Item.Sh_Type is
         when Sht_Null     =>
            Put_Line ("SHT_NULL");

         when Sht_Progbits =>
            Put_Line ("SHT_PROGBITS");

         when Sht_Symtab   =>
            Put_Line ("SHT_SYMTAB");

         when Sht_Strtab   =>
            Put_Line ("SHT_STRTAB");

         when Sht_Rela     =>
            Put_Line ("SHT_RELA");

         when Sht_Hash     =>
            Put_Line ("SHT_HASH");

         when Sht_Dynamic  =>
            Put_Line ("SHT_DYNAMIC");

         when Sht_Note     =>
            Put_Line ("SHT_NOTE");

         when Sht_Nobits   =>
            Put_Line ("SHT_NOBITS");

         when Sht_Rel      =>
            Put_Line ("SHT_REL");

         when Sht_Shlib    =>
            Put_Line ("SHT_SHLIB");

         when Sht_Dynsym   =>
            Put_Line ("SHT_DYNSYM");

         when Sht_Loproc   =>
            Put_Line ("SHT_LOPROC");

         when Sht_Hiproc   =>
            Put_Line ("SHT_HIPROC");

         when Sht_Louser   =>
            Put_Line ("SHT_LOUSER");

         when Sht_Hiuser   =>
            Put_Line ("SHT_HIUSER");

         when others       =>
            Put_Line ("Undefined");

      end case;

      Put      ("Flags                : " & Hex_Image (Item.Sh_Flags) & " = ");

      if (Item.SH_Flags and Shf_Write) /= 0
      then
         Put ("Write ");
      else
         Put ("      ");
      end if;

      if (Item.Sh_Flags and Shf_Alloc) /= 0
      then
         Put ("Alloc ");
      else
         Put ("      ");
      end if;

      if (Item.Sh_Flags and Shf_Execinstr) /= 0
      then
         Put ("Exec ");
      else
         Put ("     ");
      end if;

      New_Line;

      Put_Line ("Address              : " & Hex_Image (Item.Sh_Addr));

      Put_Line ("Section loc          :" & Off_T'Image (Item.Sh_Offset));

      Put_Line ("Section size         :" & Uword_T'Image (Item.Sh_Size));

      Put_Line ("Section link         :" & Uword_T'Image (Item.Sh_Link));

      Put_Line ("Section info         :" & Uword_T'Image (Item.Sh_Info));

      Put_Line ("Address align        :" & Uword_T'Image (Item.Sh_Addralign));

      Put_Line ("Entry size           :" & Uword_T'Image (Item.Sh_Entsize));

      New_Line;

   end Put;


   procedure Put_Section (Number : Section_Number_T; Within : File_T)
   is
      use Ada.Text_IO;

      Item : Section_T renames Within.Sections(Number);

   begin

      Put_Line ("Section number       :" & Section_Number_T'Image (Number));

      Put_Line ("Section name         : " & Name_Of (Number, Within));

      Put (Item.Header);

   end Put_Section;


   procedure Put (Item : in Segment_Header_T)
   is
      use Ada.Text_IO;
   begin

      Put ("Segment type         : " & Hex_Image(Item.P_Type) & " = ");

      case Item.P_Type is
         when Pt_Null    =>
            Put_Line ("PT_NULL");

         when Pt_Load    =>
            Put_Line ("PT_LOAD");

         when Pt_Dynamic =>
            Put_Line ("PT_DYNAMIC");

         when Pt_Interp  =>
            Put_Line ("PT_INTERP");

         when Pt_Note    =>
            Put_Line ("PT_NOTE");

         when Pt_Shlib   =>
            Put_Line ("PT_SHLIB");

         when Pt_Phdr    =>
            Put_Line ("PT_PHDR");

         when Pt_Loproc  =>
            Put_Line ("PT_LOPROC");

         when Pt_Hiproc  =>
            Put_Line ("PT_HIPROC");

         when others =>
            Put_Line ("Undefined");

      end case;

      Put_Line ("Segment offset       : " & Hex_Image (Item.P_Offset));
      Put_Line ("Virtual address      : " & Hex_Image (Item.P_Vaddr));
      Put_Line ("Physical address     : " & Hex_Image (Item.P_Paddr));
      Put_Line ("File size            : " & Hex_Image (Item.P_Filesz));
      Put_Line ("Memory size          : " & Hex_Image (Item.P_Memsz));
      Put      ("Flags                : " & Hex_Image (Item.P_Flags) & "  ");

      if (Item.P_Flags and Pf_X) /= 0
      then
         Put ('X');
      else
         Put (' ');
      end if;

      if (Item.P_Flags and Pf_W) /= 0
      then
         Put ('W');
      else
         Put (' ');
      end if;

      if (Item.P_Flags and Pf_R) /= 0
      then
         Put ('R');
      else
         Put (' ');
      end if;

      New_Line;

      Put_Line ("Alignment            : " & Hex_Image (Item.P_Align));

      New_Line;

   end Put;


   procedure Put (Item : in Segment_T)
   is
   begin

      Put (Item.Header);

   end Put;


   procedure Put_Segment (Number : Segment_Number_T; Within : File_T)
   is
      use Ada.Text_IO;

      Item : Segment_T renames Within.Segments(Number);

   begin

      Put_Line ("Segment number       :" & Segment_Number_T'Image (Number));

      Put (Item);

   end Put_Segment;


   procedure Put (Item : in ELF_Header_T)
   is
      use Ada.Text_IO;
   begin

      Put     ("Object file type     :"
                & Uhalf_T'Image(Item.E_Type) & " = ");

      case Item.E_Type is
         when Et_None   =>
            Put_Line ("No file type");

         when Et_Rel    =>
            Put_Line ("Relocatable file");

         when Et_Exec   =>
            Put_Line ("Executable file");

         when Et_Dyn    =>
            Put_Line ("Shared object file");

         when Et_Core   =>
            Put_Line ("Core file");

         when Et_Loproc =>
            Put_Line ("Processor specific");

         when Et_Hiproc =>
            Put_Line ("Processor specific");

         when others    =>
            Put_Line ("No file type");

      end case;

      Put      ("Architecture         :"
                & Uhalf_T'Image (Item.E_Machine) & " = ");

      case Item.E_Machine is
      when Em_None  => Put_Line ("No machine");
      when Em_M32   => Put_Line ("AT&T WE 32100");
      when Em_Sparc => Put_Line ("SPARC");
      when Em_386   => Put_Line ("Intel 80386");
      when Em_68K   => Put_Line ("Motorola 68000");
      when Em_88K   => Put_Line ("Motorola 88000");
      when Em_860   => Put_Line ("Intel 80860");
      when Em_Mips  => Put_Line ("MIPS RS3000");
      when Em_ARM   => Put_Line ("ARM");
      when others   => Put_Line ("Undefined");
      end case;

      Put      ("Version              :"
                & Uword_T'Image(Item.E_Version) & " = ");

      case Item.E_Version is
         when Ev_None =>
            Put_Line ("Invalid version");

         when Ev_Current =>
            Put_Line ("Current version");

         when others =>
            Put_Line ("Undefined version");

      end case;

      Put_Line ("Entry point          : "
                & Hex_Image(Item.E_Entry));

      Put_Line ("Segment header loc   :"
                & Off_T'Image(Item.E_Phoff));

      Put_Line ("Section header loc   :"
                & Off_T'Image(Item.E_Shoff));

      Put_Line ("Flags                : "
                & Hex_Image (Item.E_Flags));

      Put_line ("ELF header size      :"
                & Uhalf_T'Image (Item.E_Ehsize));

      Put_line ("Segment header size  :"
                & Uhalf_T'Image (Item.E_Phentsize));

      Put_line ("Number of segments   :"
                & Segment_Number_T'Image (Item.E_Phnum));

      Put_line ("Section header size  :"
                & Uhalf_T'Image (Item.E_Shentsize));

      Put_line ("Number of sections   :"
                & Section_Number_T'Image (Item.E_Shnum));

      Put_line ("String table section :"
                & Section_Number_T'Image (Item.E_Shstrndx));

      New_Line;

   end Put;


   procedure Put (Item : in Elf_Ident_T)
   is
      use Ada.Text_IO;
   begin

      Put_Line ("Magic number         : 0x" & Image (Item(Ei_Mag0))
                & ' ' & Character'Val(Item(Ei_Mag1))
                & ' ' & Character'Val(Item(Ei_Mag2))
                & ' ' & Character'Val(Item(Ei_Mag3))
               );

      Put ("File's class         : 0x" & Image (Item(Ei_Class)) & " = ");

      case Item(Ei_Class) is
         when Elfclassnone =>
            Put_Line ("Invalid class");

         when Elfclass32 =>
            Put_Line ("32-bit objects");

         when Elfclass64 =>
            Put_Line ("64-bit objects");

         when others =>
            Put_Line ("Undefined class");

      end case;

      Put ("Data encoding        : 0x" & Image (Item(Ei_Data)) & " = ");

      case Item(Ei_Data) is
         when Elfdatanone =>
            Put_Line ("Invalid data encoding");

         when Elfdata2lsb =>
            Put_Line ("Least significant byte first");

         when Elfdata2msb =>
            Put_Line ("Most significant byte first");

         when others =>
            Put_Line ("Undefined data encoding");

      end case;

      Put ("Elf header version   : 0x" & Image (Item(Ei_Version)) & " = ");

      case Item(Ei_Version) is
         when Ev_None =>
            Put_Line ("Invalid version");

         when Ev_Current =>
            Put_Line ("Current version");

         when others =>
            Put_Line ("Undefined version");

      end case;

      Put ("Padding              :");

      for I in Ei_Pad .. Ei_Nident
      loop
         Put (" 0x" & Image (Item(I)));
      end loop;

      New_Line;
      New_Line;

   end Put;


   procedure Put (
      Item   : in Symbol_T;
      Index  : in Natural;
      Header : in Boolean := False;
      Name   : in String)
   is
      use Ada.Text_IO;

      Col_Name   : constant := 10;
      Col_Value  : constant := 21;
      Col_Size   : constant := 32;
      Col_Info   : constant := 44;
      Col_Bind   : constant := 49;
      Col_Type   : constant := 58;
      Col_Other  : constant := 65;
      Col_Secndx : constant := 71;
      Col_Str    : constant := 80;

      Kind       : Symbol_Info_T := Symbol_Kind(Item.St_Info);
      Bind       : Symbol_Info_T := Symbol_Bind(Item.St_Info);

   begin  -- Put Symbol_T

      if Header then
                                Put ("Index" );
         Set_Col (Col_Name   ); Put ("Name"  );
         Set_Col (Col_Value  ); Put ("Value" );
         Set_Col (Col_Size   ); Put ("Size"  );
         Set_Col (Col_Info   ); Put ("Info"  );
         Set_Col (Col_Bind   ); Put ("Bind"  );
         Set_Col (Col_Type   ); Put ("Type"  );
         Set_Col (Col_Other  ); Put ("Other" );
         Set_Col (Col_Secndx ); Put ("Secndx");
         Set_Col (Col_Str    ); Put ("Name string");
         New_Line;
      end if;

      Put (Natural'Image(Index));

      Set_Col (Col_Name);

      Put (Hex_Image (Item.St_Name));

      Set_Col (Col_Value  );

      Put (Hex_Image (Item.St_Value));

      Set_Col (Col_Size   );

      Put (Uword_T'Image (Item.St_Size));

      Set_Col (Col_Info   );

      Put (Image (Item.St_Info));

      Set_Col (Col_Bind   );

      case Bind is
         when Stb_Local =>
            Put("Local" );
         when Stb_Global=>
            Put("Global");
         when Stb_Weak  =>
            Put("Weak"  );
         when Stb_Loproc=>
            Put("Loproc");
         when Stb_Hiproc=>
            Put("Hiproc");
         when others  =>
            Put("Undef" );
      end case;

      Set_Col (Col_Type   );

      case Kind is
         when Stt_Notype =>
            Put ("No type");
         when Stt_Object =>
            Put ("Object" );
         when Stt_Func   =>
            Put ("Func"   );
         when Stt_Section=>
            Put ("Section");
         when Stt_File   =>
            Put ("File"   );
         when Stt_Loproc =>
            Put ("Loproc" );
         when Stt_Hiproc =>
            Put ("Hiproc" );
         when others =>
            Put ("Undef"  );
      end case;

      Set_Col (Col_Other  );

      Put (Byte_T'Image (Item.St_Other));

      Set_Col (Col_Secndx );

      case Item.St_Shndx is
         when Shn_Abs =>
            Put("Abs   ");

         when Shn_Common =>
            Put("Common");

         when others =>
            Put (Section_Number_T'Image (Item.St_Shndx));

      end case;

      Set_Col (Col_Str );

      Put (Name);

      New_Line;

   end Put;


   procedure Put (
      Item   : in Symbols_T;
      Within : in File_T)
   is
      use Ada.Text_IO;
   begin

      Put_Line("ELF Symbol Table");

      for I in Item'Range loop

         Put (
            Item   => Item(I),
            Index  => I,
            Header => I = Item'First,
            Name   => Name_Of (Item(I), Within));

      end loop;

      New_Line;

   end Put;


   procedure Put_Stabs (
      Item   : in Section_T;
      Within : in File_T)
   is
      use Ada.Text_IO;
      
      Str_Sec : constant Section_Number_T :=
         Section_Number_T (Item.Header.Sh_Link);

   begin

      Put_Line (
           "Stab - Symbol Table, strings in section"
	  & Section_Number_T'Image (Str_Sec));

      Formats.Stabs.Text.Put (Item.Data.Table.all);

      New_Line;

   end Put_Stabs;


   procedure Put (Item : in Strings_T)
   is
      use Ada.Text_IO;
      New_String : Boolean := True;

      Col_Offset : constant := 1;
      Col_Item   : constant := 15;
   begin

      Put_Line (
         "String Table      Size: " & Uword_T'Image(Item.Size));

      Set_Col (Col_Offset); Put ("Offset");
      Set_Col (Col_Item  ); Put ("Item"  );
      New_Line;

      for I in Item.Buffer'Range loop

         if New_String then
            New_String := False;
               Set_Col (Col_Offset); Put (Hex_Image(Uword_T(I-1)));
               Set_Col (Col_Item);
            end if;

            if Item.Buffer(I) = Character'Val(0) then
               New_Line;
               New_String := True;
            else
               Put (Item.Buffer(I));
            end if;

         end loop;

      New_Line;

   end Put;


   procedure Put (Item : in File_T)
   is
   begin

      Put (Item.Ident);

      Put (Item.Header);

      for S in Item.Sections'Range loop
         Put_Section (Number => S, Within => Item);
      end loop;

      for S in Item.Segments'Range loop
         Put_Segment (Number => S, Within => Item);
      end loop;

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

      Physical_Address : Addr_T;
      -- The virtual address of the next byte to be output
      -- (when it starts a new line).

      Col_Data : constant := 12;

   begin

                          Put ("Phys Addr");
      Set_Col (Col_Data); Put ("ELF data");
      New_Line;

      while Next <= Data'Last loop

         if Col = 1 then

            Physical_Address :=
                 Section.Header.Sh_Addr
               + Addr_T (
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


   procedure Put_Data (
      Segment : in Segment_T;
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

      Physical_Address : Addr_T;
      -- The virtual address of the next byte to be output
      -- (when it starts a new line).

      Col_Data : constant := 12;

   begin

                          Put ("Phys Addr");
      Set_Col (Col_Data); Put ("ELF data");
      New_Line;

      while Next <= Data'Last loop

         if Col = 1 then

            Physical_Address :=
                 Segment.Header.P_Paddr
               + Addr_T (
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


end Formats.ELF.Text;
