-- AOMF_Keil.Text (body)
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
-- $Log: formats-aomf_keil-text.adb,v $
-- Revision 1.5  2015/10/24 20:53:51  niklas
-- Moved to free licence.
--
-- Revision 1.4  2007-08-31 09:00:31  niklas
-- Changed procedure Put (Compound_Type_T) not to use nor to update
-- Compound_Type_Index, making the procedure usable from outside.
-- The new procedure Put_And_Count implements the earlier functions
-- of Put and takes its place in Dump_Type_Definition.
--
-- Revision 1.3  2006/04/28 08:56:22  niklas
-- Added output of some unused parts of the records.
-- Added output of Kind_Code in a record dump.
--
-- Revision 1.2  2004/10/10 10:14:54  niklas
-- Added dumping (as stuff) of Source_Browse_TBC records.
-- Added handling of Predefined and Undefined Type_T's.
-- Added reporting of Check_Sum errors as that operation is now silent.
--
-- Revision 1.1  2004/10/03 16:00:24  niklas
-- First version.
--


with Ada.Text_IO;
with Ada.Characters.Handling;
with Ada.Strings;
with Ada.Strings.Fixed;
with Ada.Strings.Unbounded;


package body Formats.AOMF_Keil.Text is


   use Ada.Text_IO;


   --    Basic types


   function Graphic_Image (Item : Octet_T) return Character
   --
   -- The octet as a printable graphic, or '.' otherwise.
   --
   is
      use Ada.Characters.Handling;

      Char : constant Character := Character'Val (Item);
      -- The Item as a character, graphic or not.

   begin

      if Is_Graphic (Char) then

         return Char;

      else

         return '.';

      end if;

   end Graphic_Image;


   procedure Put (
      Data   : in Data_T;
      Offset : in Word16_T)
   --
   -- Outputs the Data in hexadecimal and character form.
   --
   is

      Octets_Per_Line : constant := 16;
      -- The number of Data octets displayed per output line.

      Start_Col : constant := 4;
      -- The left margin column for all output.

      Hex_Col  : constant := Start_Col + 9;
      -- Output column for the hex Data.

      Text_Col : constant := Hex_Col + 3 * Octets_Per_Line + 2;
      -- Output column for the graphic Data.

      Address : Word16_T := Offset;
      -- The address (Offset, Offset + 1, ...) of the first octet
      -- on the current output line.

      Text : String (1 .. Octets_Per_Line);
      -- The graphic image of the 16 octets displayed on this line.

      Column : Positive range Text'Range := Text'First;
      -- The Text column for the graphic of the current Data octet.

   begin

      New_Line;

      for D in Data'Range loop

         if Column = Text'First then

            Text := (others => ' ');

            Set_Col (Start_Col);

            Put (Hex_Image (Address));

            Put(" :");

            Set_Col (Hex_Col);

         end if;

         Put (Hex_Image (Data(D)));

         Put(' ');

         Text(Column) := Graphic_Image (Data(D));

         if Column = Text'Last
         or D      = Data'Last
         then
            -- Output the graphics.

            Set_Col (Text_Col);

            Put_Line (Text);

            Column := Text'First;

         else

            Column := Column + 1;

         end if;

         Address := Address + 1;

      end loop;

   end Put;


   function Image (Item : Storage_T) return String
   is
      Kind : constant String := Storage_Kind_T'Image (Item.Kind);
   begin

      case Item.Kind is

      when Code_Bank =>

         return Kind & Code_Bank_T'Image (Item.Code_Bank);

      when others =>

         return Kind;

      end case;

   end Image;


   function Choice (Cond : Boolean; When_True, When_False : String)
   return String
   is
   begin

      if Cond then return When_True;
              else return When_False;
      end if;

   end Choice;


   function Image (Item : Symbol_Info_T) return String
   is
   begin

      return
           Image  (Item.Usage)
         & Choice (Item.Variable, " variable", "")
         & Choice (Item.Indirect, " indirect", "")
         & Choice (Item.Single_Reg_Bank,
              " reg bank" & Register_Bank_T'Image (Item.Register_Bank),
              "");

   end Image;


   function Image (Item : Segment_Info_T) return String
   is
   begin

      return
           Image  (Item.Segment_Type)
         & Choice (Item.Empty      , " empty", "")
         & Choice (Item.Overlayable, " overlayable", "")
         & " reg bank" & Register_Bank_T'Image (Item.Register_Bank);

   end Image;


   --    Put for various AOMF record types


   procedure Put (Item : in Module_Header_T)
   is
   begin

      Put_Line ("   Name       : " & Item.Name.all);
      Put_Line ("   Translator : " & Image (Item.Translator));
      Put_Line ("   Unused1    : " & Image (Item.Unused1));

   end Put;


   function Image (Item : Register_Banks_T) return String
   --
   -- Lists the register banks in the set.
   --
   is
      use Ada.Strings.Unbounded;

      List : Unbounded_String;

   begin

      for I in Item'Range loop

         if Item(I) then

            Append (List, Register_Bank_T'Image (I));

         end if;

      end loop;

      return Ada.Strings.Fixed.Trim (
         To_String (List),
         Ada.Strings.Left);

   end Image;


   procedure Put (Item : in Module_End_T)
   is
   begin

      Put_Line ("   Name           : " & Item.Name.all);
      Put_Line ("   Register Banks : " & Image (Item.Register_Banks));
      Put_Line ("   Unused1        : " & Hex_Image (Item.Unused1,
                                                    Width  => 4,
                                                    Prefix => False));
      Put_Line ("   Unused2        : " & Image     (Item.Unused2));

   end Put;


   procedure Put (Item : in Content_T)
   is
   begin

      Put_Line ("   Seg_ID  :"  & Octet_T'Image (Item.Seg_ID));
      Put_Line ("   Offset  : " & Hex_Image (Item.Offset));

      Put (
         Data   => Item.Data.all,
         Offset => Item.Offset);

   end Put;


   procedure Put (Item : in Scope_Definition_T)
   is
   begin

      Put_Line ("   Type : " & Block_Type_T'Image (Item.Block_Type));
      Put_Line ("   Name : " & Item.Block_Name.all);

   end Put;


   procedure Put (Item : in Source_Name_T)
   is
   begin

      Put_Line ("   File Name : " & Item.File_Name.all);

   end Put;


   --    Image functions for compound type descriptors


   function Image (Item : Type_Index_T) return String
   is
      use Ada.Strings;
      use Ada.Strings.Fixed;
   begin

      case Item is

      when Basic_Type'Range =>

         return Type_T'Image (Basic_Type(Item));

      when Compound_Type_Index_T =>

         return '#' & Trim (Type_Index_T'Image (Item), Left);

      when others =>

         return 
              Trim (Type_Index_T'Image (Item), Left)
            & " = invalid";

      end case;

   end Image;


   function Image (Item : Predefined_T) return String
   is
   begin

      return "basic type " & Image (Item.Basic);

   end Image;


   function Image (Item : Funktion_T) return String
   is
   begin

      return
           "return type "   & Image (Item.Returns)
         & ", formals type " & Image (Item.Formals);

   end Image;


   function Image (Item : Bit_Field_T) return String
   is
   begin

      return
           "base"     & Octet_T'Image (Item.Base)
         & ", offset" & Octet_T'Image (Item.Offset)
         & ", width"  & Octet_T'Image (Item.Width);

   end Image;


   function Image (Item : List_Component_T) return String
   is
   begin

      return
           '"' & Item.Name.all
         & """, offset " & Hex_Image (Item.Offset)
         & ", type "     & Image (Item.Type_Index);

   end Image;


   procedure Put (Item : in List_T)
   is
   begin

      for T in Item.Tuple'Range loop

         Put_Line ("      " & Image (Item.Tuple(T)));

      end loop;

   end Put;


   function Image (Item : Aray_T) return String
   is
      use Ada.Strings.Unbounded;

      Dims : Unbounded_String;

   begin

      for D in Item.Dimensions'Range loop

         if D > Item.Dimensions'First then

            Append (Dims, ',');

         end if;

         Append (Dims, Array_Length_T'Image (Item.Dimensions(D)));

      end loop;

      return '[' & To_String (Dims) & "] of type " & Image (Item.Component);

   end Image;


   function Image (Item : Struct_T) return String
   is
   begin

      return
           "total size"             & Word16_T'Image (Item.Total_Size)
         & " octets, members type " & Image (Item.Members)
         & ", tag-name type "       & Image (Item.Tag_Name);

   end Image;


   function Image (Item : in Pointer_T) return String
   is
   begin

      return "to type " & Image (Item.Target);

   end Image;


   function Image (Item : in Spaced_Pointer_T) return String
   is
   begin

      return
           "to type "         & Image (Item.Target)
         & ", pointer length" & Octet_T'Image (Item.Length) & " octets"
         & ", target space "  & Pointer_Space_T'Image (Item.Space);

   end Image;


   function Image (Item : in Generic_Pointer_T) return String
   is
   begin

      return
           "to type "        & Image (Item.Target)
         & ", pointer size"  & Octet_T'Image (Item.Size) & " bits"
         & ", target space " & Pointer_Space_T'Image (Item.Space)
         & ", kind "         & Pointer_Kind_T'Image (Item.Kind);

   end Image;


   function Image (Item : in Type_Tag_T) return String
   is
   begin

      if Item.Base = 0 then

         return "tag-name """ & Item.Name.all & '"';

      else

         return
              "type-def """  & Item.Name.all
            & """ for type " & Image (Item.Base);

      end if;

   end Image;


   Compound_Type_Index : Compound_Type_Index_T;
   --
   -- The type index for the next Compound_Type.


   procedure Put (Item : in Compound_Type_T)
   --
   -- Displays the Item.
   --
   is
   begin

      Put (Compound_Type_Tag_T'Image (Item.Kind)
         & ' ');

      case Item.Kind is

      when Predefined       => Put_Line (Image (Item.Predefined));
      when Funktion         => Put_Line (Image (Item.Funktion));
      when Bit_Field        => Put_Line (Image (Item.Bit_Field));

      when List =>

         New_Line;

         Put (Item.List);

      when Aray             => Put_Line (Image (Item.Aray));
      when Struct           => Put_Line (Image (Item.Struct));
      when Pointer          => Put_Line (Image (Item.Pointer));
      when Spaced_Pointer   => Put_Line (Image (Item.Spaced_Pointer));
      when Generic_Pointer  => Put_Line (Image (Item.Generic_Pointer));
      when Type_Tag         => Put_Line (Image (Item.Type_Tag));
      when Undefined        => New_Line;

      end case;

   end Put;


   procedure Put_And_Count (Item : in Compound_Type_T)
   --
   -- Displays the Item, labelling it with the current Compound_Type_Index,
   -- and increments Compound_Type_Index.
   --
   is
      use Ada.Strings;
      use Ada.Strings.Fixed;
   begin

      Put ("   #"
         & Trim (Type_Index_T'Image (Compound_Type_Index), Left)
         & ": ");

      Put (Item);

      Compound_Type_Index := Compound_Type_Index + 1;

   end Put_And_Count;


   procedure Put (Item : in Symbol_Item_T)
   is
   begin

      Put_Line ("   "
         & "Symbol """    & Item.Name.all
         & """ Seg_ID = " & Hex_Image (Item.Seg_ID)
         & ", Info = "    & Image (Item.Info)
         & ", Offset = "  & Hex_Image (Item.Offset));

   end Put;


   procedure Put (Item : in Segment_Symbol_Item_T)
   is
   begin

      Put_Line ("   "
         & "Symbol """    & Item.Name.all
         & """ Seg_ID = " & Hex_Image (Item.Seg_ID)
         & ", Info = "    & Image (Item.Info)
         & ", Offset = "  & Hex_Image (Item.Offset));

   end Put;


   procedure Put (Item : in Line_Item_T)
   is
   begin

      Put_Line ("   "
         & "Seg_ID "  & Hex_Image (Item.Seg_ID)
         & " offset " & Hex_Image (Item.Offset)
         & " line"    & Word16_T'Image (Item.Line));

   end Put;


   procedure Put (Item : in Symbol_Item_Extended_T)
   is
   begin

      Put_Line ("   "
         & "Symbol """    & Item.Name.all
         & """ Seg_ID = " & Hex_Image (Item.Seg_ID)
         & ", Storage = " & Image (Item.Storage)
         & ", Type = "    & Image (Item.Type_Index)
         & ", Offset = "  & Hex_Image (Item.Offset));

   end Put;


   --     General dumpers


   procedure Dump_Stuff (
      Stuff  : in Record_Info_T;
      Stream : in IO.Stream_Access)
   --
   -- Dumps the content of the Stuff record as hex and characters,
   -- assuming that the Stream is placed at the first octet after
   -- the record-length field.
   --
   is

      Data : Data_T (1 .. Natural (Stuff.Length));
      -- The record contents, as a single Data sequence.
      -- Includes the check-sum at the end.

   begin

      Data_T'Read (Stream, Data);

      Put (Data => Data, Offset => 0);

   end Dump_Stuff;


   generic

      type Item_Type is private;

      with procedure Put (Item : in Item_Type) is <>;

   procedure Dump_Item (Stream : in IO.Stream_Access);
   --
   -- Reads an Item_Type and displays it using Put.


   procedure Dump_Item (Stream : in IO.Stream_Access)
   is
      Item : Item_Type;
   begin

      Item_Type'Read (Stream, Item);

      Put (Item);

   end Dump_Item;


   generic 

      type Item_Type is private;

      with procedure Put (Item : in Item_Type) is <>;

   procedure Dump_Items (
      Rec    : in Record_Info_T;
      File   : in IO.File_Type;
      Stream : in IO.Stream_Access);
   --
   -- Reads and dumps Items as long as there is Data_Left in the Record.


   procedure Dump_Items (
      Rec    : in Record_Info_T;
      File   : in IO.File_Type;
      Stream : in IO.Stream_Access)
   is
      Item : Item_Type;
   begin

      while Data_Left (Rec, File) loop

         Item_Type'Read (Stream, Item);

         Put (Item);

      end loop;

   end Dump_Items;


   --    Dump instances


   procedure Dump_Module_Header is new Dump_Item (Module_Header_T);

   procedure Dump_Module_End    is new Dump_Item (Module_End_T);


   procedure Dump_Content (
      Rec    : in Record_Info_T;
      Stream : in IO.Stream_Access)
   --
   -- Reads a Content record (for which Rec gives info) and
   -- displays it.
   --
   is
      Item : Content_T;
   begin

      Read_Content (Stream, Rec.Length, Item);

      Put (Item);

   end Dump_Content;


   procedure Dump_Scope_Definition is new Dump_Item (Scope_Definition_T);

   procedure Dump_Source_Name      is new Dump_Item (Source_Name_T);

   procedure Dump_Type_Definition
   is new Dump_Items (
      Item_Type => Compound_Type_T,
      Put       => Put_And_Count);

   procedure Dump_Symbols          is new Dump_Items (Symbol_Item_T);

   procedure Dump_Segment_Symbols  is new Dump_Items (Segment_Symbol_Item_T);

   procedure Dump_Line_Numbers     is new Dump_Items (Line_Item_T);

   procedure Dump_Symbols_Extended is new Dump_Items (Symbol_Item_Extended_T);


   procedure Dump_Debug_Items (
      Rec    : in Record_Info_T;
      File   : in IO.File_Type;
      Stream : in IO.Stream_Access)
   --
   -- Reads and dumps a Debug Item record, including all its Items.
   --
   is
      use type IO.Count;

      Def_Kind : Def_Kind_T;
      -- The kind of items contained in the Debug Items record.

   begin

      Def_Kind_T'Read (Stream, Def_Kind);

      Put_Line ("   Def_Kind : " & Def_Kind_T'Image (Def_Kind));

      case Def_Kind is

      when Local_Symbols | Public_Symbols =>

         Dump_Symbols (Rec, File, Stream);

      when Segment_Symbols =>

         Dump_Segment_Symbols (Rec, File, Stream);

      when Line_Numbers =>

         Dump_Line_Numbers (Rec, File, Stream);

      when Unknown =>

         IO.Set_Index (File, Rec.Start + 3);

         Dump_Stuff (Rec, Stream);

      end case;

   end Dump_Debug_Items;


   procedure Dump_Extended_Debug_Items (
      Rec    : in Record_Info_T;
      File   : in IO.File_Type;
      Stream : in IO.Stream_Access)
   --
   -- Reads and dumps an Extended Debug Item record, including all
   -- its Items.
   --
   is
      use type IO.Count;

      Def_Kind : Def_Kind_T;
      -- The kind of items contained in the Extended Debug Items record.

   begin

      Def_Kind_T'Read (Stream, Def_Kind);

      Put_Line ("   Def_Kind : " & Def_Kind_T'Image (Def_Kind));

      case Def_Kind is

      when Local_Symbols | Public_Symbols | Segment_Symbols =>

         Dump_Symbols_Extended (Rec, File, Stream);

      when Line_Numbers =>

         Dump_Line_Numbers (Rec, File, Stream);

      when Unknown =>

         IO.Set_Index (File, Rec.Start + 3);

         Dump_Stuff (Rec, Stream);

      end case;

   end Dump_Extended_Debug_Items;



   --    Dump file


   procedure Dump (File : in IO.File_Type)
   is
      use type IO.Count;

      Stream : constant IO.Stream_Access := IO.Stream (File);

      Rec : Record_Info_T;
      -- A record (header) from the file.

      Valid_Sum : Boolean;
      -- Whether the Rec has as valid check-sum.

   begin

      while not IO.End_Of_File (File) loop

         Read_Record_Info (File, Stream, Rec);

         Put_Line (
              '['
            & IO.Count'Image (Rec.Start)
            & " ] "
            & Record_Kind_T'Image (Rec.Kind)
            & " (code "
            & Image (Rec.Kind_Code)
            & ") :"
            & Word16_T'Image (Rec.Length)
            & " octets");

         Check_Sum (
            Over   => Rec,
            File   => File,
            Stream => Stream,
            Valid  => Valid_Sum);

         if not Valid_Sum then

            Put_Line ("** Record checksum mismatch **");

            Rec.Kind := Unknown;
            -- Pretend we don't know.

         end if;

         case Rec.Kind is

         when Source_Browse_TBC =>

            Dump_Stuff (Rec, Stream);

         when BL51_Bank_Head =>

            Dump_Stuff (Rec, Stream);   -- TBA real dump

         when Module_Header =>

            Dump_Module_Header (Stream);

         when Module_End =>

            Dump_Module_End (Stream);

         when Content =>

            Dump_Content (Rec, Stream);

         when Scope_Definition =>

            Dump_Scope_Definition (Stream);

         when Source_Name =>

            Dump_Source_Name (Stream);

         when Type_Definition =>

            Compound_Type_Index := Compound_Type_Index_T'First;

            Dump_Type_Definition (Rec, File, Stream);

         when Debug_Items =>

            Dump_Debug_Items (Rec, File, Stream);

         when Extended_Debug_Items =>

            Dump_Extended_Debug_Items (Rec, File, Stream);

         when Unknown =>

            Dump_Stuff (Rec, Stream);

         end case;

         New_Line;

         Skip_To_Next_Record (After => Rec, File => File);

      end loop;

   end Dump;


end Formats.AOMF_Keil.Text;
