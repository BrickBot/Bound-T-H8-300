-- Format (body)
--
-- Author: Niklas Holsti.
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
-- $Revision: 1.10 $
-- $Date: 2015/10/26 22:19:13 $
--
-- $Log: format.adb,v $
-- Revision 1.10  2015/10/26 22:19:13  niklas
-- Version 4b, and moved to free open-source licence.
--
-- Revision 1.9  2009/12/29 13:38:20  niklas
-- Updated for overloaded Processor.Shift.
--
-- Revision 1.8  2009-07-15 12:03:30  niklas
-- Updated Allocate_Range for removed exception Allocation_Overlap.
--
-- Revision 1.7  2007-12-27 23:12:33  niklas
-- BT-CH-0104: S-record program format for Bound-T/H8-300.
--
-- Revision 1.6  2005/08/25 21:07:52  niklas
-- Added Load_S_Record_File.
--
-- Revision 1.5  2005/05/09 16:06:35  niklas
-- Added a Form attribute to Object_T. To support this, moved the types
-- Form_T and Known_Form_T here, from Format.Opt. The Decoder uses the
-- Form attribute to choose between GCC or IAR calling protocols.
--
-- Revision 1.4  2005/04/01 14:21:34  niklas
-- Added the Word function.
-- Added UBROF support.
-- Made Object.Content aliased to support UBROF.
--
-- Revision 1.3  2005/03/17 07:22:25  niklas
-- Adapted to privatization of Program_T attributes as follows.
-- Changed Object_T to be a record type instead of an access.
-- The access (reference) level is now provided by the type
-- Processor.Program.Info_T. File_Object_T became redundant.
-- Changed Load_File.Object to be "access" mode, to allow the
-- direct use of the query function Programs.Processor_Info.
-- Changed Load_File.Symbol_Table to be "in" mode as befits a
-- reference type.
--
-- Revision 1.2  2005/03/16 13:31:00  niklas
-- Corrected the Code function to handle Memory_Undefined_Value and
-- signal undefined memory by raising Blank_Address.
--
-- Revision 1.1  2004/06/16 07:41:37  niklas
-- First version.
--


with Ada.Exceptions;
with Ada.Text_IO;

with Format.Opt;
with Formats;
with Formats.COFF.Reader;
with Formats.S_Record;
with Formats.S_Record.Text;
with Formats.UBROF.Reader;
with Interfaces;
with Output;


package body Format is


   package IO renames Formats.IO;


   procedure Load_S_Rec_File (
      File      : in     IO.File_Type;
      File_Name : in     String;
      Object    : access Object_T);
   --
   -- Loads S-records from the given File, already opened.


   --
   --    Loading code and data into a memory-image object.
   --


   function Form (Object : Object_T) return Known_Form_T
   is
   begin

      return Object.Form;

   end Form;


   procedure Open (
      Name : in     String;
      File :    out IO.File_Type;
      Form :    out Form_T)
   is
   begin

      IO.Open (
         File => File,
         Mode => IO.In_File,
         Name => Name);

      Form := Opt.Form;

      if Form /= Unknown then

         Output.Note (Text =>
              "File format assumed to be "
            & Form_T'Image (Form));

      else
         -- Poll the available formats.

         if Formats.COFF.Reader.Accepts (Name, File) then

            Form := COFF;

         elsif Formats.UBROF.Reader.Accepts (Name, File) then

            Form := UBROF;

         elsif Formats.S_Record.Accepts (Name, File) then

            Form := SRec;

         else

            Output.Error (Text =>
               "Cannot determine executable file type.");

            raise Loading_Error;

         end if;

         Output.Note (Text =>
              "File format is "
            & Form_T'Image (Form));

      end if;

      IO.Set_Index (File, IO.Positive_Count'First);

   exception

   when IO.Name_Error =>

      Output.Error (Text => "File not found");

      raise Loading_Error;

   end Open;


   procedure Close (File_Object : in out IO.File_Type)
   is
   begin

      IO.Close (File_Object);
      -- TBA deallocation.

   end Close;


   procedure Dump_File (File_Name : in String)
   is
      File : IO.File_Type;
      Mark : Output.Nest_Mark_T;
      Form : Form_T;
   begin

      Mark := Output.Nest (Output.Locus (Program_File => File_Name));

      Open (Name => File_Name, File => File, Form => Form);

      case Known_Form_T (Form)  is

      when COFF  => Formats.COFF.Reader.Dump (File);

      when UBROF => Formats.UBROF.Reader.Dump (File);

      when SRec  => Formats.S_Record.Text.Dump (File);

      end case;

      Close(File);

      Output.Unnest (Mark);

   exception

   when Loading_Error =>
      -- Already reported, and caller doesn't care.

      Output.Unnest (Mark);

   when IO.Use_Error =>

      Output.Error (Text => "Cannot read file");
      Output.Unnest (Mark);

   when IO.End_Error =>

      Output.Error (Text => "Unexpected end of file");
      Output.Unnest (Mark);

   end Dump_File;


   procedure Load_File (
      File_Name    : in     String;
      Object       : access Object_T;
      Symbol_Table : in     Symbols.Symbol_Table_T)
   is
      File : IO.File_Type;
      Mark : Output.Nest_Mark_T;
      Form : Form_T;
   begin

      Mark := Output.Nest (Output.Locus (Program_File => File_Name));

      Memory.Initialize (Object.Content);

      Open (File_Name, File, Form);

      Object.Form := Form;

      case Object.Form is

      when COFF =>

         Formats.COFF.Reader.Load (
            File         => File,
            Content      => Object.Content,
            Symbol_Table => Symbol_Table);

      when UBROF =>

         Formats.UBROF.Reader.Load (
            File         => File,
            Content      => Object.Content'Access,
            Symbol_Table => Symbol_Table);

      when SRec =>

         Load_S_Rec_File (
            File      => File,
            File_Name => File_Name,
            Object    => Object);

      end case;

      -- Memory.Show_Maps (Object.Content);
      -- This could be optional. TBA.

      Close (File);

      Output.Unnest (Mark);

   exception

   when IO.Use_Error =>

      Output.Error (Text => "Cannot read file");
      Output.Unnest (Mark);

   when IO.End_Error =>

      Output.Error (Text => "Unexpected end of file");
      Output.Unnest (Mark);

   when others =>

      Output.Unnest (Mark);
      raise;

   end Load_File;


   --
   --    S-record files
   --


   procedure Allocate_Range (
      Address : in     Formats.Unsigned_16_T;
      Length  : in     Natural;
      Content : in out Memory.Content_T)
   --
   -- Allocates this address-range in the Memory image, for loading
   -- S-records into it.
   --
   is
   begin

      Memory.Allocate (
         Address => Processor.Address_T (Address),
         Span    => Length,
         Within  => Content);

   end Allocate_Range;


   procedure Allocate_S_Record_Ranges
   is new Formats.S_Record.Get_Address_Ranges (
      Receiver_T => Memory.Content_T,
      Action     => Allocate_Range);


   procedure Load_S_Record (
      Item    : in     Formats.S_Record.S_Record_T;
      Content : in out Memory.Content_T)
   --
   -- Loads the Item data into the Content.
   --
   is
   begin

      Memory.Load (
         Into    => Processor.Address_T (Item.Address),
         Data    => Item.Data,
         Within  => Content);

   end Load_S_Record;


   procedure Load_S_Records
   is new Formats.S_Record.Get_Records (
      Receiver_T => Memory.Content_T,
      Action     => Load_S_Record);


   procedure Load_S_Rec_File (
      File      : in     IO.File_Type;
      File_Name : in     String;
      Object    : access Object_T)
   is
   begin

      Output.Note ("Loading S-records from " & File_Name);

      Allocate_S_Record_Ranges (
         File     => File,
         Name     => File_Name,
         Receiver => Object.Content);

      Load_S_Records (
         File     => File,
         Name     => File_Name,
         Receiver => Object.Content);

      Output.Note ("Finished S-records from " & File_Name);

   end Load_S_Rec_File;


   procedure Load_S_Record_File (
      File_Name : in     String;
      Object    : access Object_T)
   is

      File : IO.File_Type;
      -- The S-record file.

   begin

      IO.Open (
         File => File,
         Mode => IO.In_File,
         Name => File_Name);

      Load_S_Rec_File (File, File_Name, Object);

      IO.Close (File);

   exception

   when IO.Name_Error =>

      Output.Error (
           "Cannot open the S-record file """
         & File_Name
         & """.");

      if IO.Is_Open (File) then

         IO.Close (File);

      end if;

      raise Loading_Error;

   when Loading_Error =>
      -- Already reported elsewhere.

      raise;

   when X : others =>

      Output.Exception_Info (
         Text       => "Loading S-record file",
         Occurrence => X);

      if IO.Is_Open (File) then

         IO.Close (File);

      end if;

      raise Loading_Error;

   end Load_S_Record_File;


   --
   --    Reading code and data from a memory-image object.
   --


   function Octet (
      Address : Processor.Address_T;
      From    : Object_T)
   return H8_300.Octet_T
   is
   begin

      return H8_300.Octet_T (
         Memory.Datum (
            From   => Address,
            Within => From.Content));

   exception

   when Memory.Undefined_Value =>

      raise Blank_Address;

   end Octet;


   function Word (
      Address : Processor.Address_T;
      From    : Object_T)
   return H8_300.Unsigned_Word_T
   is
      use type H8_300.Unsigned_Word_T;
      use type Processor.Code_Address_T;

      High, Low : Formats.Octet_T;
      -- The two octets in the word.

   begin

      if Address mod 2 /= 0 then

         Output.Warning (
              "Reading word from odd address "
            & Processor.Image (Address));

      end if;

      -- The high octet is always fetched from the even address
      -- and the low octet from the odd address:

      High := Memory.Datum (
         From   => Address and (not 1),
         Within => From.Content);

      Low := Memory.Datum (
         From   => Address or 1,
         Within => From.Content);

      return
            H8_300.Shift_Left (H8_300.Unsigned_Word_T (High), 8)
         or                    H8_300.Unsigned_Word_T (Low);

   exception

   when Memory.Undefined_Value =>
      -- Either the High or Low octet is undefined in the Memory.

      raise Blank_Address;

   end Word;


   function Code (
      Address : Processor.Code_Address_T;
      From    : Object_T)
   return H8_300.Unsigned_Word_T
   is
      use type H8_300.Unsigned_Word_T;
      use type Processor.Code_Address_T;

      High, Low : Formats.Octet_T;
      -- The two octets in the word.

   begin

      if Address mod 2 /= 0 then

         Output.Warning (
              "Fetching from odd address "
            & Processor.Image (Address));

      end if;

      -- The high octet is always fetched from the even address
      -- and the low octet from the odd address:

      High := Memory.Datum (
         From   => Address and (not 1),
         Within => From.Content);

      Low := Memory.Datum (
         From   => Address or 1,
         Within => From.Content);

      return
            H8_300.Shift_Left (H8_300.Unsigned_Word_T (High), 8)
         or                    H8_300.Unsigned_Word_T (Low);

   exception

   when Memory.Undefined_Value =>
      -- Either the High or Low octet is undefined in the Memory.

      raise Blank_Address;

   end Code;


   procedure Fetch_Code (
      From    : in     Object_T;
      Address : in     Processor.Code_Address_T;
      Into    :    out H8_300.Word_List_T;
      Last    :    out Integer)
   is
      use type Processor.Code_Address_T;

      Next_Address : Processor.Code_Address_T := Address;
      -- The address of the next word.

   begin

      Last := Into'First - 1;
      -- To be sure of returning some defined value.

      for I in Into'Range loop

         Into(I) := Code (
            Address => Next_Address,
            From    => From);

         Last := I;

         Next_Address := Processor.Shift (
            Base   => Next_Address,
            Offset => Processor.Code_Offset_T'(2));

      end loop;

   exception

   when Blank_Address =>

      Output.Note (
           "Only"
         & Integer'Image (Last - Into'First + 1)
         & " instruction words fetched.");

   end Fetch_Code;


end Format;
