-- Formats.ELF.Text (decl)
--
-- Printing ELF executable and object files.
--
-- References:
-- System V Application Binary Interface. Ch. 4.
-- The Santa Cruz Operation, Inc & AT&T. Edition 3.1, March 1997.
-- System V Application Binary Interface SPARC Processor Supplement. Ch. 4.
-- The Santa Cruz Operation, Inc & AT&T. Third Edition, 1996.
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
-- $Date: 2015/10/24 20:53:53 $
--
-- $Log: formats-elf-text.ads,v $
-- Revision 1.2  2015/10/24 20:53:53  niklas
-- Moved to free licence.
--
-- Revision 1.1  2004-04-24 18:03:43  niklas
-- First Tidorum version, as child of Formats.
--
--
-- <Log: elf-text.ads>
-- Revision 1.1  2001/08/27 09:12:56  saarinen
-- First version.
--


package Formats.ELF.Text is


   type Format_T is array (Positive range <>) of Positive;
   --
   -- Describes a way to format a sequence of octets in hex.
   -- The octets are written in groups, using the numbers in the
   -- format array to define the length of each group, cycling
   -- through the array.
   -- Each group is separated from the next one by '_', except
   -- when wrapping around from the end of the format to the
   -- start, when the separator is '  ' (two blanks).
   -- For example, if the Format is (1,2) the output has the
   -- form "xx_xxxx  yy_yyyy  zz_zzzz" and so on, as long as there
   -- are bytes left in the input.
   --
   -- If the input ends in the middle of the format, the
   -- output is filled out with '?' instead of digits.
   -- For example, the hex string "123456789A" formatted with (1,2)
   -- yields "12_3456  78_9A??".
   --
   -- The "span" of a Format_T is defined as the number of
   -- bytes it displays, i.e. the sum of the numbers in the
   -- Format_T. For example, the span of (1,2) is 3.


   -- The following "Put" procedures output their Item
   -- parameter on standard output, usually in multi-line
   -- form.


--   procedure Put (Item : in Relocation_T);

   procedure Put (Item : in Line_Number_T);

   procedure Put (Item : in Section_Header_T);

--   procedure Put (Item : in Section_T);

   procedure Put_Section (Number : Section_Number_T; Within : File_T);

   procedure Put (Item : in Segment_Header_T);

   procedure Put (Item : in Segment_T);

   procedure Put_Segment (Number : Segment_Number_T; Within : File_T);

   procedure Put (Item : in Elf_Header_T);

   procedure Put (Item : in Elf_Ident_T);

   procedure Put (
      Item   : in Symbol_T;
      Index  : in Natural;
      Header : in Boolean := False;
      Name   : in String);

   procedure Put (
      Item   : in Symbols_T;
      Within : in File_T);


   procedure Put_Stabs (
      Item   : in Section_T;
      Within : in File_T);
   --
   -- Outputs the Stabs symbol-table held in Item.


   procedure Put (Item : in Strings_T);

   procedure Put (Item : in File_T);


   procedure Put_Data (
      Section : in Section_T;
      Data    : in Octets_T;
      Format  : in Format_T;
      Columns : in Positive := 1;
      Unit    : in Positive := 1);
   --
   -- Outputs the data from the section, using the given Format,
   -- with a line-break after every "Columns" repetitions of
   -- the Format.
   -- Prefixes each line with the Physical Address of the first
   -- byte of the line, considering the Unit as the number of bytes
   -- in one addressable unit. Unit should be a multiple of the span
   -- of the Format.


   procedure Put_Data (
      Segment : in Segment_T;
      Data    : in Octets_T;
      Format  : in Format_T;
      Columns : in Positive := 1;
      Unit    : in Positive := 1);
   --
   -- Outputs the data from the section, using the given Format,
   -- with a line-break after every "Columns" repetitions of
   -- the Format.
   -- Prefixes each line with the Physical Address of the first
   -- byte of the line, considering the Unit as the number of bytes
   -- in one addressable unit. Unit should be a multiple of the span
   -- of the Format.


end Formats.ELF.Text;
