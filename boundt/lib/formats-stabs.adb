-- Formats.Stabs (body)
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
-- $Revision: 1.2 $
-- $Date: 2015/10/24 20:53:54 $
--
-- $Log: formats-stabs.adb,v $
-- Revision 1.2  2015/10/24 20:53:54  niklas
-- Moved to free licence.
--
-- Revision 1.1  2004-04-24 17:24:37  niklas
-- First version.
--


with Ada.Text_IO;
with Hex;
with Output;


package body Formats.Stabs is


   function Symbol (
      Line   : Line_T;
      Within : Table_T;
      Cut    : Character := Character'Val (0))
   return String
   is

      Start : Positive := Positive (Line.N_Strx + 1);
      -- The index of the start of the string.

      Next : Positive := Start;
      -- The next index after the end of the (scanned part
      -- of the) string.

   begin

      -- Search for the end of the string:

      loop

         if Next > Within.Strings'Last then

            Output.Error (Text =>
               "STABS string index exceeds string buffer");

            exit;

         end if;

         exit when Within.Strings(Next) = Character'Val(0);

         exit when Within.Strings(Next) = Cut;

         Next := Next + 1;

      end loop;

      return Within.Strings (Start .. Next - 1);

   end Symbol;


   function Identifier (Symbol : String) return String
   is
   begin

      for S in Symbol'Range loop

         if Symbol(S) = ':' then

            return Symbol(Symbol'First .. S - 1);

         end if;

      end loop;

      -- No colon in the string.

      return Symbol;

   end Identifier;


   subtype Type_Sym_T is String (1 .. 8);
   --
   -- The mnemonic for an N_Type_T value.


   Unknown_Type : constant Type_Sym_T := "?       ";
   --
   -- The mnemonic for a strange N_Type value.


   Type_Sym : constant array (N_Type_T) of Type_Sym_T := (
      N_HdrSym   =>   "HdrSym  ",
      N_Abs      =>   "Abs     ",
      N_Ext_Abs  =>   "Ext_Abs ",
      N_Text     =>   "Text    ",
      N_Ext_Text =>   "Ext_Text",
      N_Data     =>   "Data    ",
      N_Ext_Data =>   "Ext_Data",
      N_Bss      =>   "Bss     ",
      N_Ext_Bss  =>   "Ext_Bss ",
      N_Fn_Seq   =>   "Fn_Seq  ",
      N_Indr     =>   "Indr    ",
      N_Comm     =>   "Comm    ",
      N_Seta     =>   "Seta    ",
      N_Sett     =>   "Sett    ",
      N_Setd     =>   "Setd    ",
      N_Setb     =>   "Setb    ",
      N_Setv     =>   "Setv    ",
      N_Warning  =>   "Warning ",
      N_Fn       =>   "Fn      ",
      N_Gsym     =>   "Gsym    ",
      N_Fname    =>   "Fname   ",
      N_Fun      =>   "Fun     ",
      N_Stsym    =>   "Stsym   ",
      N_Lcsym    =>   "Lcsym   ",
      N_Main     =>   "Main    ",
      N_Rosym    =>   "Rosym   ",
      N_Pc       =>   "Pc      ",
      N_Nsyms    =>   "Nsyms   ",
      N_Nomap    =>   "Nomap   ",
      N_Obj      =>   "Obj     ",
      N_Opt      =>   "Opt     ",
      N_Rsym     =>   "Rsym    ",
      N_M2c      =>   "M2c     ",
      N_Sline    =>   "Sline   ",
      N_Dsline   =>   "Dsline  ",
      N_Bsline   =>   "Bsline  ",
      N_Brows    =>   "Brows   ",
      N_Defd     =>   "Defd    ",
      N_Fline    =>   "Fline   ",
      N_Ehdecl   =>   "Ehdecl  ",
      N_Mod2     =>   "Mod2    ",
      N_Catch    =>   "Catch   ",
      N_Ssym     =>   "Ssym    ",
      N_Endm     =>   "Endm    ",
      N_So       =>   "So      ",
      N_Lsym     =>   "Lsym    ",
      N_Bincl    =>   "Bincl   ",
      N_Sol      =>   "Sol     ",
      N_Psym     =>   "Psym    ",
      N_Eincl    =>   "Eincl   ",
      N_Entry    =>   "Entry   ",
      N_Lbrac    =>   "Lbrac   ",
      N_Excl     =>   "Excl    ",
      N_Scope    =>   "Scope   ",
      N_Rbrac    =>   "Rbrac   ",
      N_Bcomm    =>   "Bcomm   ",
      N_EComm    =>   "EComm   ",
      N_Ecoml    =>   "Ecoml   ",
      N_With     =>   "With    ",
      N_Nbtext   =>   "Nbtext  ",
      N_Nbdata   =>   "Nbdata  ",
      N_Nbbss    =>   "Nbbss   ",
      N_Nbsts    =>   "Nbsts   ",
      N_Nblcs    =>   "Nblcs   ",
      others     =>   Unknown_Type);
   --
   -- The mnemonic symbols for N_Type values.


   Type_Col  : constant :=  8;
   Other_Col : constant := 17;
   Desc_Col  : constant := 25;
   Value_Col : constant := 32;
   SVal_Col  : constant := 42;
   Strx_Col  : constant := 52;
   Sym_Col   : constant := 61;
   --
   -- Column positions for Line display.


   procedure Put_Header
   --
   -- Prints a header for Stabs lines.
   --
   is
      use Ada.Text_IO;
   begin

                           Put ("Symnum"   );
      Set_Col (Type_Col ); Put ("n_type"   );
      Set_Col (Other_Col); Put ("n_othr"   );
      Set_Col (Desc_Col ); Put ("n_desc"   );
      Set_Col (Value_Col); Put ("n_value"  );
      Set_Col (SVal_Col ); Put ("+/- dec"  );
      Set_Col (Strx_Col ); Put ("n_strx"   );
      Set_Col (Sym_Col  ); Put ("String"   );

      New_Line;

   end Put_Header;


   procedure Put_Line (
      Index : in Integer;
      Table : in Table_T)
   --
   -- Prints the Line on the standard output.
   --
   is
      use Ada.Text_IO;

      Line : constant Line_T := Table.Lines(Index);
      -- The line to be displayed.

      Mnem : constant Type_Sym_T := Type_Sym (Line.N_Type);
      -- The mnemonic of the line type.

      Unsigned_Value : Unsigned_32_T;
      -- Line.N_Value as an unsigned value.

   begin

      Put (Integer'Image (Index));

      Set_Col (Type_Col);

      if Mnem /= Unknown_Type then
         Put (Mnem);
      else
         Put (N_Type_T'Image (Line.N_Type));
      end if;

      Set_Col (Other_Col);
      Put (Octet_T'Image (Line.N_Other));

      Set_Col (Desc_Col);
      Put (N_Desc_T'Image (Line.N_Desc));

      Set_Col (Value_Col);

      if Line.N_Value >= 0 then

         Unsigned_Value := Unsigned_32_T (Line.N_Value);

      else

         Unsigned_Value :=
            Unsigned_32_T (Line.N_Value - N_Value_T'First)
            or 16#8000_0000#;

      end if;

      Put (Hex.Image (Hex.Word32_T (Unsigned_Value)));

      Set_Col (SVal_Col);
      Put (N_Value_T'Image (Line.N_Value));

      Set_Col (Strx_Col);
      Put (N_Strx_T'Image (Line.N_Strx));

      Set_Col (Sym_Col);
      Put (Symbol (Line => Line, Within => Table));

      New_Line;

   end Put_Line;


   procedure Load (
      From    : in     IO.File_Type;
      Lines   : in     Segment_Desc_T;
      Strings : in     Segment_Desc_T;
      Trace   : in     Boolean := False;
      Giving  :    out Table_Ref)
   is
      use type IO.Count;

      Num_Lines : Natural;
      -- The (apparent) number of Stabs lines.

      Stream : IO.Stream_Access;
      -- A stream to read the From file.

      Hdr_Strx : N_Strx_T := 0;
      -- The offset in the .stabstr of the strings for the current
      -- module. To be added to file-value of N_Strx to give the
      -- absolute offset of the symbol string.

      Last_Size : Unsigned_32_T := 0;
      --
      -- The total size of the .stabstr strings for the last module.
      -- When the module ends, this is added to Hdr_Strx to prepare
      -- for the next module.

      Line : Line_T;
      -- One Stabs line as read from the file.

   begin

      -- Find the number of Stabs lines:

      if Lines.Octets mod Octets_Per_Line /= 0 then

         Output.Warning (Text =>
              "STABS segment size"
            & IO.Positive_Count'Image (Lines.Octets)
            & " is not a multiple of"
            & Natural'Image (Octets_Per_Line)
            & " octets");

      end if;

      Num_Lines := Natural (Lines.Octets / Octets_Per_Line);

      -- Create the Table:

      if Trace then

         Ada.Text_IO.Put_Line (
              "Loading STABS table with"
            & Natural'Image (Num_Lines)
            & " lines and"
            & IO.Count'Image (Strings.Octets)
            & " string characters.");

         Put_Header;

      end if;

      Giving := new Table_T (
         Max_Line  => Num_Lines - 2,
         Num_Chars => Natural (Strings.Octets));

      Stream := IO.Stream (From);

      -- Read the string table:

      IO.Set_Index (File => From, To => Strings.Start);

      String'Read (Stream, Giving.Strings);

      -- Read the Stabs lines:

      IO.Set_Index (File => From, To => Lines.Start);

      -- We assume that the Stabs lines are packed tightly in
      -- the file and not separated by padding octets.

      for L in Giving.Lines'Range loop

         Line_T'Read (Stream, Line);

         if Line.N_Type = N_HdrSym then
            -- A new module starts.

            Hdr_Strx := Hdr_Strx + N_Strx_T (Last_Size);
            -- The base offset in .stabstr for the new module.

            Last_Size := Unsigned_32_T (Line.N_Value);
            -- The size of all the strings in the new module.

         end if;

         Line.N_Strx := Line.N_Strx + Hdr_Strx;
         -- Convert to offset relative to start of .stabstr.

         Giving.Lines(L) := Line;

         if Trace then

            Put_Line (Index => L, Table => Giving.all);

         end if;

      end loop;

   end Load;


end Formats.Stabs;
