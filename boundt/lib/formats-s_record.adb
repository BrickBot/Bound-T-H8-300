-- Formats.S_Record (body)
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
-- $Revision: 1.5 $
-- $Date: 2015/10/24 20:53:54 $
--
-- $Log: formats-s_record.adb,v $
-- Revision 1.5  2015/10/24 20:53:54  niklas
-- Moved to free licence.
--
-- Revision 1.4  2007-12-27 22:32:01  niklas
-- BT-CH-0103: Formats.S_Record uses Stream_IO, not Text_IO.
--
-- Revision 1.3  2007/09/22 14:15:50  niklas
-- Corrected function S_Rec to avoid wrap-around in Rec.Max_Index.
--
-- Revision 1.2  2005/08/25 13:35:00  niklas
-- Changed S_Record_T.Data to be indexed from 0, not 1, for
-- a better match with the procedure Memories.Load.
--
-- Revision 1.1  2005/08/25 12:17:25  niklas
-- First version.
--


with Ada.Characters.Latin_1;
with Output;


package body Formats.S_Record is


   procedure Set_To_Start (
      File  : in     IO.File_Type; 
      Name  : in     String;
      Locus : in out Locus_T)
   is
   begin

      IO.Set_Index (File, 1);

      Locus := (
         Name => To_Unbounded_String (Name),
         Line => 1);

   end Set_To_Start;


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

      Output.Error (
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

      Output.Error (
           "The characters """
         & Digs
         & """ are not an 8-bit hexadecimal number.");

      raise Format_Error;

   end Value;


   function S_Rec (
      Line   : String;
      Silent : Boolean := False)
   return S_Record_T
   --
   -- Parses the given Line of text as an S-record.
   -- Propagates Format_Error if this fails. Setting Silent
   -- suppresses other error messages, leaving only Format_Error.
   --
   is

      F : constant Positive := Line'First;
      -- The index of the first character.

      S_Type     : S_Type_T;
      Rec_Length : Unsigned_8_T;
      Address    : Unsigned_16_T;
      Checksum   : Unsigned_8_T;
      -- Components for the S-record.

      Loc : Natural;
      -- The location in Line of a data/code octet.

      Sum : Octet_T;
      -- The sum (mod 2**8) of the record-length octet, the
      -- address octets and the data/code octets.


   begin  -- S_Rec

      if Line'Length < 8
      or Line'Length mod 2 /= 0
      then

         if not Silent then

            Output.Error (
                 "A line of length"
               & Natural'Image (Line'Length)
               & " characters cannot be an S-record.");

         end if;

      elsif Line(F) /= 'S'
         or Line(F+1) not in '1' .. '9'
      then

         if not Silent then

            Output.Error (
                 "A line that starts with """
               & Line(F .. F + 1)
               & """ cannot be an S-record.");

         end if;

      else
         
         S_Type := S_Type_T'Value (Line(F + 1 .. F + 1));

         Rec_Length := Value (Line(F + 2 .. F + 3));

         Address := Value (Line(F + 4 .. F + 7));

         if Line'Length /= 4 + 2 * Natural (Rec_Length) then

            if not Silent then

               Output.Error (
                    "A line of length"
                  & Natural'Image (Line'Length)
                  & " characters cannot be an S-record with"
                  & Unsigned_8_T'Image (Rec_Length)
                  & " octets.");

            end if;

         else
            -- Record length matches line length.

            declare

               Rec : S_Record_T (Max_Index => Integer (Rec_Length) - 4);
               -- This will be the result, barring error.

            begin

               -- Read the data/code octets:

               Sum :=
                  + Rec_Length
                  + Octet_T (Bits ( 7, 0, Address))
                  + Octet_T (Bits (15, 8, Address));

               for D in Rec.Data'Range loop

                  Loc := F + 8 + 2 * (D - Rec.Data'First);

                  Rec.Data(D) := Value (Line(Loc .. Loc + 1));

                  Sum := Sum + Rec.Data(D);

               end loop;

               -- Check the sum:

               Sum := not Sum;

               Checksum := Value (Line(Line'Last - 1 .. Line'Last));

               if Checksum = Sum then
                  -- All ok.

                  Rec.S_Type     := S_Type;
                  Rec.Rec_Length := Rec_Length;
                  Rec.Address    := Address;
                  Rec.Checksum   := Checksum;

                  return Rec;

               elsif not Silent then

                  Output.Error (
                       "Computed S-record checksum "
                     & Image (Sum)
                     & " differs from record value "
                     & Image (Checksum));

               end if;

            end;

         end if;

      end if;
               
      -- Some error prevented us from returning an S-rec.

      raise Format_Error;

   end S_Rec;


   Max_Line_Length : constant := 1000;
   --
   -- Maximum length that we allow for an S-record line.
   -- Typical lengths are quite short, less than 80.


   function Next_Line (
      File   : IO.File_Type;
      Locus  : Locus_T;
      Silent : Boolean := False)
   return S_Record_T
   is
      use type IO.Positive_Count;

      Line : String(1 .. Max_Line_Length);
      Last : Natural;
      -- The line will be Line(1 .. Last).

      Mark : Output.Nest_Mark_T;
      -- Marks the locus of this line, if Name is not null.

      Stream : IO.Stream_Access := IO.Stream (File);
      -- The S-record octet stream.

      Octet : Octet_T;
      -- An octet from the File.

      Char : Character;
      -- The Octet as a character.

   begin

      if Length (Locus.Name) > 0 then
         -- Define the locus.

         Mark := Output.Nest (Output.Locus (
            Statements => Output."+" (Output.Locus (
               Source_File => To_String (Locus.Name),
               Line_Number => Output.Line_Number_T (Locus.Line)))));

      end if;

      -- Get the octets from the line:

      Last := 0;

      loop

         Octet_T'Read (Stream, Octet);

         Char := Character'Val (Octet);

         exit when Char = Ada.Characters.Latin_1.CR
                or Char = Ada.Characters.Latin_1.LF;

         Last := Last + 1;

         if Last <= Line'Last then

            Line(Last) := Char;

         elsif Silent then
            -- Only checking, not reading. This line is too
            -- long, so this is not an acceptable S-record.

            raise Format_Error;

         end if;

      end loop;

      -- Skip line-end markers:

      loop

         exit when IO.End_Of_File (File);

         Octet_T'Read (Stream, Octet);

         Char := Character'Val (Octet);

         if  Char /= Ada.Characters.Latin_1.CR
         and Char /= Ada.Characters.Latin_1.LF
         then
            -- Ah, this is the first char of the next line.
            -- Retreat one position to re-read this char on
            -- the next call of Next_Line.

            IO.Set_Index (File, IO.Index (File) - 1);

            exit;

         end if;

      end loop;

      if Last > Line'Last then

         Output.Error (
              "S-record too long; at most"
            & Natural'Image (Line'Length)
            & " characters allowed.");

         raise Format_Error;

      else

         declare

            Result : constant S_Record_T := S_Rec (
               Line   => Line(Line'First .. Last),
               Silent => Silent);

         begin

            if Length (Locus.Name) > 0 then

               Output.Unnest (Mark);

            end if;

            return Result;

         end;

      end if;

   exception

   when others =>

      if Length (Locus.Name) > 0 then

         Output.Unnest (Mark);

      elsif not Silent then

         Output.Error (
              "The error is in S-record number "
            & Output.Image (Output.Line_Number_T (Locus.Line))
            & '.');

      end if;

      raise;

   end Next_Line;


   function Accepts (
      Name : String;
      File : IO.File_Type)
   return Boolean
   is

      Locus : Locus_T;
      -- The place of the first line.

   begin

      Set_To_Start (File, Name, Locus);

      declare

         First_Rec : constant S_Record_T := Next_Line (
            File   => File,
            Locus  => Locus,
            Silent => True);

      begin
         -- The file starts with a valid S-record, First_Rec.

         return True;

      exception

      when others =>
         -- The file does not start with a valid S-record.

         return False;

      end;

   end Accepts;


   --
   --    Address ranges
   --


   type Range_T is record
      Address : Unsigned_16_T;
      Length  : Unsigned_16_T;
   end record;
   --
   -- An address range, from Address to Address + Length - 1.


   function Range_Of (Item : S_Record_T) return Range_T
   --
   -- The address range for which the Item defines Data.
   --
   is
   begin

      return (
         Address => Item.Address,
         Length  => Unsigned_16_T (Item.Data'Length));

   end Range_Of;


   function Next_Range (
      File  : IO.File_Type;
      Locus : Locus_T)
   return Range_T
   --
   -- The address-range defined by the next S-record in the File.
   -- The File is advanced by one line. The Locus parameter has the
   -- same role as in the function Next_Line; its Line component
   -- is not increased.
   --
   is
   begin

      return Range_Of (Next_Line (File, Locus));

   end Next_Range;


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
      File     : in     IO.File_Type;
      Name     : in     String := "";
      Receiver : in out Receiver_T)
   is

      Locus : Locus_T;
      -- For keeping track of the place in the file.

      Contig : Range_T;
      -- The contiguous range being collected from consecutive
      -- S-records in the File.

      Next : Range_T;
      -- The range of the next S-record.

   begin

      Set_To_Start (File, Name, Locus);

      Contig := Next_Range (File, Locus);

      while not IO.End_Of_File (File) loop

         Locus.Line := Locus.Line + 1;

         Next := Next_Range (File, Locus);

         if Follows (Contig, Next) then
            -- Keep extending the contiguous range.

            Unite (Into => Contig, More => Next);

         else
            -- Here is break. Report the range we have
            -- collected and start a new range.

            if Contig.Length > 0 then

               Action (
                  Address  => Contig.Address,
                  Length   => Positive (Contig.Length),
                  Receiver => Receiver);

            end if;

            Contig := Next;

         end if;

      end loop;

      -- Report the last range, if any:

      if Contig.Length > 0 then

         Action (
            Address  => Contig.Address,
            Length   => Positive (Contig.Length),
            Receiver => Receiver);

      end if;

   exception

   when IO.End_Error =>
      -- The file seems to be empty.

      null;

   end Get_Address_Ranges;


   --
   --    S-record data
   --


   procedure Get_Records (
      File     : in     IO.File_Type;
      Name     : in     String := "";
      Receiver : in out Receiver_T)
   is

      Locus : Locus_T;
      -- For keeping track of the place in the file.

   begin

      Set_To_Start (File, Name, Locus);

      while not IO.End_Of_File (File) loop

         Action (
            Item     => Next_Line (File, Locus),
            Receiver => Receiver);

         Locus.Line := Locus.Line + 1;

      end loop;

   end Get_Records;


end Formats.S_Record;
