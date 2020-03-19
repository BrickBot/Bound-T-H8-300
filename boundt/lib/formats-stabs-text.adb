-- Formats.Stabs.Text (body)
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
-- $Revision: 1.3 $
-- $Date: 2015/10/24 20:53:54 $
--
-- $Log: formats-stabs-text.adb,v $
-- Revision 1.3  2015/10/24 20:53:54  niklas
-- Moved to free licence.
--
-- Revision 1.2  2004-06-12 11:24:10  niklas
-- Index may be signed (Integer, not Natural).
--
-- Revision 1.1  2004/04/24 17:25:14  niklas
-- First version.
--


with Ada.Text_IO;


package body Formats.Stabs.Text is


   procedure Put (
      Item   : in Line_T;
      Index  : in Integer;
      Header : in Boolean := False;
      Name   : in String)
   is

      use Ada.Text_IO;

      Col_Name   : constant := 10;
      Col_Type   : constant := 21;
      Col_Other  : constant := 35;
      Col_Desc   : constant := 45;
      Col_Value  : constant := 55;
      Col_Str    : constant := 66;

   begin  -- Put Stab_Line_T

      if Header then
                                Put ("Index"  );
         Set_Col (Col_Name   ); Put ("NamePtr");
         Set_Col (Col_Type   ); Put ("Type"   );
         Set_Col (Col_Other  ); Put ("Other"  );
         Set_Col (Col_Desc   ); Put ("Desc"   );
         Set_Col (Col_Value  ); Put ("Value"  );
         Set_Col (Col_Str    ); Put ("Name string");
         New_Line;
      end if;

      Put (Integer'Image(Index));

      Set_Col (Col_Name);

      Put (Hex_Image (Item.N_Strx));

      Set_Col (Col_Type  );

      Put ("0x" & Image(Item.N_Type) & "  ");

      case Item.N_Type is
      
         when N_HdrSym =>
            Put ("HdrSym");

         when N_Sline =>
            Put ("Sline");

         when N_Dsline =>
            Put ("Dsline");

         when N_Bsline =>
            Put ("Bsline");

         when N_Fline =>
            Put ("Fline");

         when N_Endm =>
            Put ("Endm");

         when N_So =>
            Put ("Source");

         when N_Lsym  =>
            Put ("Lsym");

         when N_Psym =>
            Put ("Psym");

         when N_Gsym =>
            Put ("Gsym");

         when N_Fname =>
            Put ("Fname");

         when N_Fun =>
            Put ("Fun");

         when N_Stsym =>
            Put ("Stsym");

         when N_Lcsym =>
            Put ("Lcsym");

         when N_Main =>
            Put ("Main");

         when N_Nsyms =>
            Put ("Nsyms");

         when N_Opt =>
            Put ("Opt");

         when N_Rsym =>
            Put ("Rsym");

         when N_Sol =>
            Put ("Sol");

         when N_Bincl =>
            Put ("B incl");

         when N_Eincl =>
            Put ("E incl");

         when N_Excl =>
            Put ("Ex Incl");

         when N_Lbrac =>
            Put ("{");

         when N_Rbrac =>
            Put ("}");

         when others =>
            null;
      end case;

      Set_Col (Col_Other );

      Put (Octet_T'Image (Item.N_Other));

      Set_Col (Col_Desc   );

      Put (N_Desc_T'Image (Item.N_Desc));

      Set_Col (Col_Value  );

      Put (Hex_Image (Item.N_Value));

      Set_Col (Col_Str );

      Put (Name);

      New_Line;

   end Put;


   procedure Put (Item : in Table_T)
   is
   begin

      for L in Item.Lines'Range loop

         Put (
            Item   => Item.Lines(L),
            Index  => L,
            Header => L = Item.Lines'First,
            Name   => Symbol (Item.Lines(L), Item));
      end loop;

   end Put;


end Formats.Stabs.Text;
