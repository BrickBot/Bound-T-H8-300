-- Formats.Intel_Hex (body)
--
-- Author: Niklas Holsti, Tidorum Ltd
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
-- $Revision: 1.7 $
-- $Date: 2015/10/24 20:53:53 $
--
-- $Log: formats-intel_hex.adb,v $
-- Revision 1.7  2015/10/24 20:53:53  niklas
-- Moved to free licence.
--
-- Revision 1.6  2013-04-17 05:16:07  niklas
-- BT-CH-0249: Removed unreachable code from Get_Next_Range.
--
-- Revision 1.5  2011-05-28 12:52:18  niklas
-- Strip trailing CR from input lines.
--
-- Revision 1.4  2008-10-16 19:48:52  niklas
-- Corrected error message wording.
--
-- Revision 1.3  2008/10/11 05:56:44  niklas
-- BT-CH-0147: Intel-Hex with linear 32-bit addresses.
--
-- Revision 1.2  2007/09/27 11:03:07  niklas
-- Added "Intel-Hex" to warnings and errors, for TR-TN-IHEX-001.
--
-- Revision 1.1  2007/09/22 14:19:08  niklas
-- First version.
--


with Ada.Characters.Handling;
with Ada.Characters.Latin_1;
with Formats.Intel_Hex.Opt;
with Formats.Intel_Hex.Text;
with Output;


package body Formats.Intel_Hex is


   procedure Error (Text : in String)
   --
   -- Reports an error with an "Intel-Hex" prefix.
   --
   is
   begin

      Output.Error (
           "Intel-Hex"
         & Output.Field_Separator
         & Text);

   end Error;


   function Value (Digs : String) return Unsigned_32_T
   --
   -- Reads the Digs as a 32-bit hexadecimal number.
   --
   -- Propagates Format_Error on failure.
   --
   is
   begin

      return Unsigned_32_T'Value ("16#" & Digs & '#');

   exception

   when Constraint_Error =>

      Error (
           "The characters """
         & Digs
         & """ are not a 32-bit hexadecimal number.");

      raise Format_Error;

   end Value;


   function Value (Digs : String) return Unsigned_16_T
   --
   -- Reads the Digs as a 16-bit hexadecimal number.
   --
   -- Propagates Format_Error on failure.
   --
   is
   begin

      return Unsigned_16_T'Value ("16#" & Digs & '#');

   exception

   when Constraint_Error =>

      Error (
           "The characters """
         & Digs
         & """ are not a 16-bit hexadecimal number.");

      raise Format_Error;

   end Value;


   function Value (Digs : String) return Unsigned_8_T
   --
   -- Reads the Digs as an 8-bit hexadecimal number.
   --
   -- Propagates Format_Error on failure.
   --
   is
   begin

      return Unsigned_8_T'Value ("16#" & Digs & '#');

   exception

   when Constraint_Error =>

      Error (
           "The characters """
         & Digs
         & """ are not an 8-bit hexadecimal number.");

      raise Format_Error;

   end Value;


   subtype Type_Code_T is Unsigned_8_T range 0 .. 5;
   --
   -- The record type codes.


   Rec_Type : constant array (Type_Code_T) of Rec_Type_T :=  (
      0 => Data,
      1 => End_Of_File,
      2 => Extended_Segment_Address,
      3 => Start_Segment_Address,
      4 => Extended_Linear_Address,
      5 => Start_Linear_Address);
   --
   -- The record types of which we are aware.


   type Lengths_T is record
      Min : Unsigned_8_T;
      Max : Unsigned_8_T;
   end record;
   --
   -- A minimum and maximum data length.


   Data_Lengths : constant array (Rec_Type_T) of Lengths_T := (
      Data                     => (0, 255),
      End_Of_File              => (0,   0),
      Extended_Segment_Address => (2,   2),
      Start_Segment_Address    => (4,   4),
      Extended_Linear_Address  => (2,   2),
      Start_Linear_Address     => (4,   4));
   --
   -- The permitted range of data length for each type of record.


   Supported : constant array (Rec_Type_T) of Boolean := (
      Data                     => True,
      End_Of_File              => True,
      Extended_Segment_Address => False,
      Start_Segment_Address    => False,
      Extended_Linear_Address  => True,
      Start_Linear_Address     => True);
   --
   -- Whether we know how to handle a given type of record.


   function I_Hex_Rec (
      Line   : String;
      Upper  : Unsigned_32_T;
      Silent : Boolean := False)
   return Record_T
   --
   -- Parses the given Line of text as an I-Hex record, with a given
   -- Upper Linear Base Address.
   -- Propagates Format_Error if this fails. Emits error
   -- messages unless told to be Silent.
   --
   is

      F : constant Positive := Line'First;
      -- The index of the first character.

      Type_Code   : Unsigned_8_T;
      Data_Length : Unsigned_8_T;
      Address     : Unsigned_32_T;
      Checksum    : Unsigned_8_T;
      -- Components for the I-Hex record.

      Loc : Natural;
      -- The location in Line of a data/code octet.

      Sum : Octet_T;
      -- The sum (mod 2**8) of the record-length octet, the address
      -- octets, the record type octet and the data/code octets.


      procedure Optional_Error (Text : in String)
      is
      begin

         if not Silent then

            Error (Text);

         end if;

      end Optional_Error;


   begin  -- I_Hex_Rec

      if Line'Length < 11
      or Line'Length mod 2 = 0
      then

         Optional_Error (
              "Record cannot have a length of"
            & Natural'Image (Line'Length)
            & " characters.");

         raise Format_Error;

      elsif Line(F) /= ':' then

         Optional_Error (
              "Record cannot start with '"
            & Line(F)
            & "'.");

         raise Format_Error;

      else

         -- Check that all characters are hex digits:

         for L in F + 1 .. Line'Last loop

            if not Ada.Characters.Handling.Is_Hexadecimal_Digit (Line(L)) then

               Optional_Error (
                    "Record cannot contain a '"
                  & Line(L)
                  & "' character.");

               raise Format_Error;

            end if;

         end loop;

         -- Get and check the field values:

         Type_Code := Value (Line(F + 7 .. F + 8));

         Data_Length := Value (Line(F + 1 .. F + 2));

         Address := Value (Line(F + 3 .. F + 6)) + Upper;

         if Line'Length /= 11 + 2 * Natural (Data_Length) then

            Optional_Error (
                 "Record of length"
               & Natural'Image (Line'Length)
               & " characters cannot have"
               & Unsigned_8_T'Image (Data_Length)
               & " data octets.");

            raise Format_Error;

         elsif Type_Code not in Rec_Type'Range then

            Optional_Error (
                 "Record type"
               & Image (Type_Code)
               & " is not understood.");

            raise Unknown_Type;

         elsif Data_Length not in
               Data_Lengths(Rec_Type(Type_Code)).Min
            .. Data_Lengths(Rec_Type(Type_Code)).Max
         then

            Optional_Error (
                 "Record of type"
               & Type_Code_T'Image (Type_Code)
               & " cannot have"
               & Unsigned_8_T'Image (Data_Length)
               & " data octets.");

            raise Format_Error;

         else
            -- Data length matches line length and the record type
            -- is good and matches the data length.

            declare

               Rec : Record_T (Max_Index => Integer (Data_Length) - 1);
               -- This will be the result, barring error.

            begin

               Rec.Rec_Type    := Rec_Type(Type_Code);
               Rec.Data_Length := Data_Length;
               Rec.Address     := Address;

               -- Read the data/code octets:

               Sum :=
                  + Data_Length
                  + Octet_T (Bits ( 7, 0, Address))
                  + Octet_T (Bits (15, 8, Address))
                  + Type_Code;

               for D in Rec.Data'Range loop

                  Loc := F + 9 + 2 * (D - Rec.Data'First);

                  Rec.Data(D) := Value (Line(Loc .. Loc + 1));

                  Sum := Sum + Rec.Data(D);

               end loop;

               -- Check the sum:

               Sum := (not Sum) + 1;

               Checksum := Value (Line(Line'Last - 1 .. Line'Last));

               if Checksum /= Sum then

                  Optional_Error (
                       "Computed checksum "
                     & Image (Sum)
                     & " differs from record value "
                     & Image (Checksum));

                  raise Format_Error;

               elsif not Supported(Rec.Rec_Type) then

                  Optional_Error (
                       "Records of type"
                     & Type_Code_T'Image (Type_Code)
                     & " are not supported.");

                  raise Format_Error;

               else
                  -- All ok.

                  Rec.Checksum    := Checksum;

                  return Rec;

               end if;

            end;

         end if;

      end if;

   end I_Hex_Rec;


   Max_Line_Length : constant := 1000;
   --
   -- Maximum length that we allow for an I-Hex-record line.
   -- A valid I-Hex-record line is at most 11 + 2*255 = 521 characters.
   -- Typical lengths are quite short, less than 80.


   function Accepts (
      Name : in String;
      File : in IO.File_Type)
   return Boolean
   is
      use Ada.Characters;

      Stream : constant IO.Stream_Access := IO.Stream (File);
      -- For reading the file.

      Char : Character;
      -- One character from the file.

      Line : String(1 .. Max_Line_Length);
      Last : Natural := 0;
      -- The line will be Line(1 .. Last).

   begin

      -- Read a line of text from the start of the file:

      IO.Set_Index (File, IO.Positive_Count'First);

      loop

         Character'Read (Stream, Char);

         exit when Char = Latin_1.CR;
         exit when Char = Latin_1.LF;

         Last := Last + 1;

         if Last > Line'Last
         or (Last > 1 and not Handling.Is_Hexadecimal_Digit (Char))
         then
            -- Cannot be an I-Hex record.

            return False;

         end if;

         Line(Last) := Char;

      end loop;

      -- Try to parse the line as an I-Hex record:

      declare

         Rec : constant Record_T := I_Hex_Rec (
            Line   => Line(1 .. Last),
            Upper  => 0,
            Silent => True);

      begin

         return True;

      end;

   exception

   -- TBA end-of-file error => return False.

   when Format_Error
      | Unknown_Type =>

      return False;

   end Accepts;


   function Next_Line (
      File  : Ada.Text_IO.File_Type;
      Upper : Unsigned_32_T;
      Name  : String := "")
   return Record_T
   is
      use type Ada.Text_IO.Positive_Count;

      Line : String(1 .. Max_Line_Length);
      Last : Natural;
      -- The line will be Line(1 .. Last).

      Number : constant Output.Line_Number_T :=
        Output.Line_Number_T (Ada.Text_IO.Line (File));
      -- The line-number of this S-record, for error-message locus.

      Mark : Output.Nest_Mark_T;
      -- Marks the locus of this line, if Name is not null.

   begin

      if Name'Length > 0 then
         -- Define the locus.

         Mark := Output.Nest (Output.Locus (
            Statements => Output."+" (Output.Locus (
               Source_File => Name,
               Line_Number => Number))));

      end if;

      Ada.Text_IO.Get_Line (File, Line, Last);

      if Last >= Line'First
      and then Line(Last) = Ada.Characters.Latin_1.CR
      then
         -- The line ends with an "extra" carriage return.
         -- Probably we are running on Unix/Linux and the file
         -- was created on Microsoft Windows or some other system
         -- with CR-LF line terminators. Anyway, skip it.

         Last := Last - 1;

      end if;

      if Ada.Text_IO.Col (File) /= 1 then

         Error (
              "Record too long; at most"
            & Natural'Image (Line'Length)
            & " characters allowed.");

         raise Format_Error;

      else

         declare

           Result : constant Record_T :=
              I_Hex_Rec (
                 Line  => Line(Line'First .. Last),
                 Upper => Upper);

         begin

            if Name'Length > 0 then

               Output.Unnest (Mark);

            end if;

            if Opt.Trace_Loading then

               Text.Put (Result);

            end if;

            return Result;

         end;

      end if;

   exception

   when others =>

      if Name'Length > 0 then

         Output.Unnest (Mark);

      else

         Error (
              "The error is in record number "
            & Output.Image (Number)
            & '.');

      end if;

      raise;

   end Next_Line;


   function Next_Known_Line (
      File  : Ada.Text_IO.File_Type;
      Upper : Unsigned_32_T;
      Name  : String := "")
   return Record_T
   is
   begin

      loop

         begin

            return Next_Line (File, Upper, Name);
            --
            -- May propagate Format_Error or Unknown_Type.

         exception

         when Unknown_Type =>
            -- Look at the next record.

            null;

         end;

      end loop;

   end Next_Known_Line;


   procedure Set_Upper_Address (
      Item  : in     Record_T;
      Upper :    out Unsigned_32_T)
   is
   begin

      Upper := Shift_Left (
         Value  => Value (Octets => Item.Data, Endian => Big),
         Amount => 16);

      if Opt.Trace_Loading then

         Output.Trace (
              "Intel-Hex file sets Upper Linear Base Address to "
            & Hex_Image (Upper));

      end if;

   end Set_Upper_Address;


   --
   --    Address ranges
   --


   type Range_T is record
      Address : Unsigned_32_T;
      Length  : Unsigned_32_T;
   end record;
   --
   -- An address range, from Address to Address + Length - 1.


   function Range_Of (Item : Record_T) return Range_T
   --
   -- The address range for which the Item defines Data.
   --
   is
   begin

      return (
         Address => Item.Address,
         Length  => Unsigned_32_T (Item.Data'Length));

   end Range_Of;


   procedure Get_Next_Range (
      File   : in     Ada.Text_IO.File_Type;
      Upper  : in out Unsigned_32_T;
      Rainge :    out Range_T;
      Name   : in     String := "")
   --
   -- Finds the address-range defined by the next Data hex-record in
   -- the File of a known (supported) record type.
   --
   -- The File is advanced by one line, or as many lines as necessary
   -- to find the next line of Data type. If an Extended_Linear_Address
   -- record is found, the Upper LBA is updated. The Name parameter has
   -- the same role as in the function Next_Line.
   --
   is
   begin

      loop

         declare

            Rec : constant Record_T := Next_Known_Line (File, Upper, Name);

         begin

            case Rec.Rec_Type is

            when Data
               | End_Of_File =>

               Rainge := Range_Of (Rec);

               return;

            when Start_Linear_Address =>

               null;

            when Extended_Linear_Address =>

               Set_Upper_Address (Rec, Upper);

            when Extended_Segment_Address
               | Start_Segment_Address =>
               -- Should never happen, because they are treated
               -- as format errors in I_Hex_Rec.

               Output.Fault (
                  Location => "Formats.Intel_Hex.Get_Next_Range",
                  Text     => Rec_Type_T'Image (Rec.Rec_Type));

            end case;

         end;

      end loop;

   end Get_Next_Range;


   function Follows (First, Next : Range_T) return Boolean
   --
   -- Whether the Next range immediately follows the First range,
   -- leaving no gap.
   --
   is
   begin

      return First.Address + First.Length = Next.Address;

   end Follows;


   procedure Unite (Into : in out Range_T; More : in Range_T)
   --
   -- Unites the More range Into a range that is contiguous.
   --
   is
   begin

      Into.Length := Into.Length + More.Length;

   end Unite;


   procedure Get_Address_Ranges (
      File     : in     Ada.Text_IO.File_Type;
      Name     : in     String := "";
      Receiver : in out Receiver_T)
   is

      Contig : Range_T;
      -- The contiguous range being collected from consecutive
      -- S-records in the File.

      Upper_Address : Unsigned_32_T := 0;
      -- The Upper Linear Base Address.

      Next : Range_T;
      -- The range of the next S-record.


      procedure Act
      --
      -- Take Action on the current Contig range, if not null.
      --
      is
      begin

         if Contig.Length > 0 then

            if Opt.Trace_Loading then

               Output.Trace (
                    "Intel-Hex address range from "
                  & Hex_Image (Contig.Address)
                  & " for "
                  & Hex_Image (Contig.Length)
                  & " octets.");

            end if;

            Action (
               Address  => Contig.Address,
               Length   => Positive (Contig.Length),
               Receiver => Receiver);

         end if;

      end Act;


   begin  -- Get_Address_Ranges

      if Opt.Trace_Loading then

         Output.Trace (
              "Reading address ranges from Intel-Hex file"
            & Output.Field_Separator
            & Name);

      end if;

      Get_Next_Range (
          File   => File,
          Upper  => Upper_Address,
          Rainge => Contig,
          Name   => Name);

      while not Ada.Text_IO.End_Of_File (File) loop

         Get_Next_Range (
             File   => File,
             Upper  => Upper_Address,
             Rainge => Next,
             Name   => Name);

         if Follows (Contig, Next) then
            -- Keep extending the contiguous range.

            Unite (Into => Contig, More => Next);

         else
            -- Here is break. Report the range we have
            -- collected and start a new range.

            Act;

            Contig := Next;

         end if;

      end loop;

      -- Report the last range, if any:

      Act;

      if Opt.Trace_Loading then

         Output.Trace (
              "Intel-Hex file ends"
            & Output.Field_Separator
            & Name);

      end if;

   exception

   when Ada.Text_IO.End_Error =>
      -- The file seems to be empty.

      Output.Warning (
           "Intel-Hex file is empty"
         & Output.Field_Separator
         & Name);

   end Get_Address_Ranges;


   --
   --    I-Hex record data
   --


   procedure Get_Records (
      File     : in     Ada.Text_IO.File_Type;
      Name     : in     String := "";
      Receiver : in out Receiver_T)
   is

      Lines : Natural := 0;
      -- The number of lines processed.

      Upper_Address : Unsigned_32_T := 0;
      -- The Upper Linear Base Address.

   begin

      if Opt.Trace_Loading then

         Output.Trace (
              "Reading data from Intel-Hex file"
            & Output.Field_Separator
            & Name);

      end if;

      while not Ada.Text_IO.End_Of_File (File) loop

         declare

           Rec : constant Record_T := Next_Known_Line (
              File  => File,
              Upper => Upper_Address,
              Name  => Name);

         begin

            case Rec.Rec_Type is

            when Data =>

               Action (
                  Item     => Rec,
                  Receiver => Receiver);

            when End_Of_File =>

               exit;

            when Start_Linear_Address =>

               null;

            when Extended_Linear_Address =>

               Set_Upper_Address (Rec, Upper_Address);

            when Extended_Segment_Address
               | Start_Segment_Address =>
               -- Should never happen, because they are treated
               -- as format errors in I_Hex_Rec.

               Output.Fault (
                  Location => "Formats.Intel_Hex.Get_Records",
                  Text     => Rec_Type_T'Image (Rec.Rec_Type));

            end case;

         end;

         Lines := Lines + 1;

      end loop;

      if Opt.Trace_Loading then

         Output.Trace (
              "Intel-Hex file ends after"
            & Natural'Image (Lines)
            & " processed lines"
            & Output.Field_Separator
            & Name);

      end if;

   end Get_Records;


end Formats.Intel_Hex;
