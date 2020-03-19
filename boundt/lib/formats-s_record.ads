-- Formats.S_Record (decl)
--
-- Reading binary files in Motorola S-record format.
--
-- S-record files are text files with one line being one S-record.
-- An S-record is a character string that contains five fields as
-- follows:
--
-- > Type field, two characters, S1 .. S9.
-- > Record length, two hex digits, giving the number of two-digit
--   hex numbers that follow in this record (excluding the type and
--   length fields). In other words, the number of characters remaining
--   in this record is twice this number.
-- > Address, four hex digits, giving the load address of the first
--   data octet.
-- > Data, a number of code or data octets, each encoded as a two-digit
--   hex number. The number of data octets is (record length) - 3, where
--   the subtracted constant 3 represents two octets of Address and
--   one octet of checksum.
-- > Checksum, two hex digits defining the least significant byte of
--   the one's complement of the sum of all the octets in the record
--   length, address and data fields.
--
-- The meaning of the different record types may depend somewhat on
-- the software that generates and uses the files. However, generally:
--
-- > S1 is a data/code record as described above. In a typical S-record
--   file, all lines are S1 lines, except perhaps for the last line,
--
-- > S9 is a termination record. Only one S9-record is allowed per file
--   and it must be the last record. The Address field may contain the
--   start/entry address for the code given in the S1-records. There is
--   no Data field.
--
-- Although S-record files are logically text files, here we access them
-- as octet-stream files, to be compatible with the other file formats
-- implemented in the Formats family.
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
-- $Revision: 1.4 $
-- $Date: 2015/10/24 20:53:54 $
--
-- $Log: formats-s_record.ads,v $
-- Revision 1.4  2015/10/24 20:53:54  niklas
-- Moved to free licence.
--
-- Revision 1.3  2007-12-27 22:32:01  niklas
-- BT-CH-0103: Formats.S_Record uses Stream_IO, not Text_IO.
--
-- Revision 1.2  2005/08/25 13:35:01  niklas
-- Changed S_Record_T.Data to be indexed from 0, not 1, for
-- a better match with the procedure Memories.Load.
--
-- Revision 1.1  2005/08/25 12:17:25  niklas
-- First version.
--


with Ada.Strings.Unbounded;


package Formats.S_Record is


   type S_Type_T is range 1 .. 9;
   --
   -- The record type.


   type S_Record_T (Max_Index : Integer) is record
      S_Type     : S_Type_T;
      Rec_Length : Unsigned_8_T;
      Address    : Unsigned_16_T;
      Checksum   : Unsigned_8_T;
      Data       : Octets_T (0 .. Max_Index);
   end record;
   --
   -- An S-record.
   --
   -- The meaning of the components should be evident from the
   -- description of the S-record structure given above.


   type File_Name_T is new Ada.Strings.Unbounded.Unbounded_String;
   --
   -- The name of an S-record file.


   type Locus_T is record
      Name : File_Name_T;
      Line : Positive;
   end record;
   --
   -- A place in an S-record file, consisting of the file Name
   -- and the Line number.


   procedure Set_To_Start (
      File  : in     IO.File_Type; 
      Name  : in     String;
      Locus : in out Locus_T);
   --
   -- Sets the file to its starting position and initializes
   -- the Locus likewise.


   function Next_Line (
      File   : IO.File_Type;
      Locus  : Locus_T;
      Silent : Boolean := False)
   return S_Record_T;
   --
   -- Reads the next line from the given (text) File and tries to
   -- parse it as an S-record. Propagates Format_Error if this
   -- parsing fails.
   --
   -- If Locus.Name is known (not null), the error messages will
   -- be provided with the source-file and line-number locus.
   -- Otherwise an additional Output.Error line will identify the
   -- number of the S-record that causes problems, if any.
   --
   -- Setting Silent suppresses all error messages, leaving only
   -- the Format_Error.


   function Accepts (
      Name : String;
      File : IO.File_Type)
   return Boolean;
   --
   -- Whether the File seems to be an S-record file, as judged by the
   -- initial file contents (first line) and possibly from the file Name.
   -- Rewinds the file before returning, so the next read operation
   -- will start from the head of the file. On call, the file can be
   -- at any position.


   generic

      type Receiver_T (<>) is limited private;

      with procedure Action (
         Address  : in     Unsigned_16_T; 
         Length   : in     Positive;
         Receiver : in out Receiver_T);

   procedure Get_Address_Ranges (
      File     : in     IO.File_Type;
      Name     : in     String := "";
      Receiver : in out Receiver_T);
   --
   -- Traverses the S-records in the given File and finds the
   -- contiguous address ranges that are covered (defined) by the
   -- consecutive S-records. That is, if the first S-record defines
   -- data for certain address range, and the next S-record continues
   -- that range without leaving a gap, these two S-records together
   -- define a wider contiguous range, and so on for a possible third,
   -- fourth etc. S-record.
   --
   -- For each such contiguous address range defined by consecutive
   -- S-records, the Action procedure is invoked once, with the Address
   -- parameter defining the start of the range and the Length parameter
   -- defining the length of the range in octets. Thus, the range extends
   -- from Address to Address + Length - 1, inclusive.
   --
   -- The Receiver type and parameter refer to the object that will
   -- make use of the address-range information in some way.
   --
   -- The Name has the same role as Locus.Name in Next_Line.
 

   generic

      type Receiver_T (<>) is limited private;

      with procedure Action (
         Item     : in     S_Record_T; 
         Receiver : in out Receiver_T);

   procedure Get_Records (
      File     : in     IO.File_Type;
      Name     : in     String := "";
      Receiver : in out Receiver_T);
   --
   -- Traverses the S-records in the given File and presents them to
   -- the Action procedure in the same order.
   --
   -- The Receiver type and parameter refer to the object that will
   -- make use of the S-record information in some way.
   --
   -- The Name has the same role as Locus.Name in Next_Line.
 

end Formats.S_Record;
