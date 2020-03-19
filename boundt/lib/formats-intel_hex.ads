-- Formats.Intel_Hex (decl)
--
-- Reading binary files in "Intel Hex" (I-Hex) format.
--
-- I-Hex files are text files with one line being one I-Hex-record.
-- There are two kinds of I-Hex records: Data records and End-Of-File
-- records. In both cases an I-Hex-record is a character string that
-- contains six fields as follows:
--
-- > Record Mark field, a single colon ':'.
--
-- > Data Length, two hex digits, giving the number of data octets
--   in this record. All other fields are excluded. In other words,
--   the number of characters in the Data field (see below) is twice
--   this number. An End-Of-File record has a zero data-length.
--
-- > Address, four hex digits, giving the load address of the first
--   data octet. In an End-Of-File record the address is zero or is
--   the program entry address. (We ignore the latter case.)
--
-- > Record Type, two hex digits, giving the record type as 00 for
--   a data record, 01 for an End-Of-File record, and so on.
--
-- > Data, a number of code or data octets, each encoded as a two-digit
--   hex number. The number of data octets is the Data Length, thus
--   the Data field contains two times Data Length characters (hex
--   digits).
--
-- > Checksum, two hex digits defining the least significant byte of
--   the two's complement of the 8-bit sum of all the octets in the
--   data length, address, record type, and data fields.
--
-- All values are encoded in hex in big-endian order.
--
-- Author: Niklas Holsti, Tidorum Ltd
--
-- Reference:
--    Alan R. Baldwin: "ASxxxx Assemblers and ASLINK Relocating Linker",
--    Version 2.0, Augustu 1998. Kent State University, Physics
--    Department, Kent, Ohio 44242, USA.
--    http://shop-pdp.kent.edu/ashtml/asxxxx.htm
--    
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
-- $Date: 2015/10/24 20:53:53 $
--
-- $Log: formats-intel_hex.ads,v $
-- Revision 1.3  2015/10/24 20:53:53  niklas
-- Moved to free licence.
--
-- Revision 1.2  2008-10-11 05:56:44  niklas
-- BT-CH-0147: Intel-Hex with linear 32-bit addresses.
--
-- Revision 1.1  2007/09/22 14:19:09  niklas
-- First version.
--


with Ada.Text_IO;


package Formats.Intel_Hex is


   function Accepts (
      Name : in String;
      File : in IO.File_Type)
   return Boolean;
   --
   -- Whether the file seems to be an Intel-Hex file, as judged by
   -- the start of the file content.
   -- The file may have to be rewound (reset to start) before being
   -- loaded.


   type Rec_Type_T is (
      Data,
      End_Of_File,
      Extended_Segment_Address,
      Start_Segment_Address,
      Extended_Linear_Address,
      Start_Linear_Address);
   --
   -- The types of an I-Hex record.


   type Record_T (Max_Index : Integer) is record
      Rec_Type    : Rec_Type_T;
      Data_Length : Unsigned_8_T;
      Address     : Unsigned_32_T;
      Checksum    : Unsigned_8_T;
      Data        : Octets_T (0 .. Max_Index);
   end record;
   --
   -- An Intel-Hex record.
   --
   -- The meaning of the components should be evident from the
   -- description of the I-Hex structure given above.


   Unknown_Type : exception;
   --
   -- Signals the occurence in the file of a record of an
   -- unknown or unsupported type.


   function Next_Line (
      File  : Ada.Text_IO.File_Type;
      Upper : Unsigned_32_T;
      Name  : String := "")
   return Record_T;
   --
   -- Reads the next line from the given text File and tries to
   -- parse it as an I-Hex record, with the given Upper Linear
   -- Base Address.
   --
   -- Propagates Format_Error if this parsing fails and Unknown_Type
   -- if the record type is unknown or unsupported (and therefore
   -- not parsed).
   --
   -- If the file Name is known (not null), the error messages will
   -- be provided with the source-file and line-number locus.
   -- Otherwise an additional Output.Error line will identify the
   -- number of the I-Hex record that causes problems, if any.


   function Next_Known_Line (
      File  : Ada.Text_IO.File_Type;
      Upper : Unsigned_32_T;
      Name  : String := "")
   return Record_T;
   --
   -- Like Next_Line, but skips lines (records) of unknown type.


   procedure Set_Upper_Address (
      Item  : in     Record_T;
      Upper :    out Unsigned_32_T);
   --
   -- Given a record Item of type Extended_Linear_Address, the
   -- data in the record is used to set the Upper Linear Base Address.


   generic

      type Receiver_T (<>) is limited private;

      with procedure Action (
         Address  : in     Unsigned_32_T; 
         Length   : in     Positive;
         Receiver : in out Receiver_T);

   procedure Get_Address_Ranges (
      File     : in     Ada.Text_IO.File_Type;
      Name     : in     String := "";
      Receiver : in out Receiver_T);
   --
   -- Traverses the I-Hex records in the given File and finds the
   -- contiguous address ranges that are covered (defined) by the
   -- consecutive I-Hex records. That is, if the first record defines
   -- data for certain address range, and the next record continues
   -- that range without leaving a gap, these two records together
   -- define a wider contiguous range, and so on for a possible third,
   -- fourth etc. I-Hex record.
   --
   -- For each such contiguous address range defined by consecutive
   -- I-Hex records, the Action procedure is invoked once, with the Address
   -- parameter defining the start of the range and the Length parameter
   -- defining the length of the range in octets. Thus, the range extends
   -- from Address to Address + Length - 1, inclusive.
   --
   -- The Receiver type and parameter refer to the object that will
   -- make use of the address-range information in some way.
   --
   -- The Name parameter has the same role as in the function Next_Line.
 

   generic

      type Receiver_T (<>) is limited private;

      with procedure Action (
         Item     : in     Record_T; 
         Receiver : in out Receiver_T);

   procedure Get_Records (
      File     : in     Ada.Text_IO.File_Type;
      Name     : in     String := "";
      Receiver : in out Receiver_T);
   --
   -- Traverses the I-Hex records in the given File and presents them to
   -- the Action procedure in the same order.
   --
   -- The Receiver type and parameter refer to the object that will
   -- make use of the I-Hex record information in some way.
   --
   -- The Name parameter has the same role as in the function Next_Line.
 

end Formats.Intel_Hex;
