-- S_Record_Dump (main procedure)
--
-- Dumping an S-record file.
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
-- $Revision: 1.2 $
-- $Date: 2015/10/24 20:53:55 $
--
-- $Log: s_record_dump.adb,v $
-- Revision 1.2  2015/10/24 20:53:55  niklas
-- Moved to free licence.
--
-- Revision 1.1  2005-10-24 17:20:45  niklas
-- First version.
--


with Ada.Command_Line;
with Ada.Text_IO;

with Formats.S_Record.Text;


procedure S_Record_Dump
is

   Name : constant String := Ada.Command_Line.Argument (1);

   File : Ada.Text_IO.File_Type;


   procedure Report_Range (
      Address : in     Formats.Unsigned_16_T;
      Length  : in     Positive;
      Number  : in out Natural)
   is
   begin

      Number := Number + 1;

      Ada.Text_IO.Put_Line (
           "Address range" & Natural'Image (Number)
         & ": "
         & Formats.Hex_Image (Address)
         & " for"
         & Positive'Image (Length));

   end Report_Range;


   procedure Report_Ranges
   is new Formats.S_Record.Get_Address_Ranges (
      Receiver_T => Natural,
      Action     => Report_Range);


   Range_Count : Natural := 0;

begin

   Ada.Text_IO.Open (
      File => File,
      Name => Name,
      Mode => Ada.Text_IO.In_File);

   Formats.S_Record.Text.Dump (File, Name);

   Ada.Text_IO.Reset (File);

   Report_Ranges (File, Name, Range_Count);

end S_Record_Dump;
