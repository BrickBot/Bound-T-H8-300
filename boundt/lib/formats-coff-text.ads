-- Formats.COFF.Text (decl)
--
-- Printing COFF executable and object files.
--
-- Author: Sami Saarinen and Niklas Holsti, Space Systems Finland, 2000
--
-- Reference: "DJGPP COFF Spec" by DJ Delorie,
-- http://www.delorie.com/djgpp/doc/coff/
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
-- $Date: 2015/10/24 20:53:52 $
--
-- $Log: formats-coff-text.ads,v $
-- Revision 1.3  2015/10/24 20:53:52  niklas
-- Moved to free licence.
--
-- Revision 1.2  2005-07-25 19:26:06  niklas
-- Added a Strings parameter to Put for Symbol_T and Symbols_T, to
-- display the symbol names even when longer than 8 characters.
--
-- Revision 1.1  2004/04/24 18:08:34  niklas
-- First Tidorum version, as child of Formats.
--
--
-- <Log: coff-text.ads>
-- Revision 1.1  2000/07/02 18:41:55  holsti
-- First version.
--


package Formats.COFF.Text is


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


   procedure Put (Item : in Relocation_T);

   procedure Put (Item : in Line_Number_T);

   procedure Put (Item : in Section_Header_T);

   procedure Put (Item : in Section_T);

   procedure Put (Item : in File_Header_T);

   procedure Put (Item : in Optional_File_Header_T);


   procedure Put (
      Item    : in Symbol_T;
      Index   : in Natural;
      Strings : in Strings_T;
      Header  : in Boolean := False);
   --
   --
   -- If the name (identifier string) of the Item is over 8 characters
   -- long, the true name is in the Strings table, at the offset given
   -- in Item. If the Strings table is absent, no name is displayed for
   -- the symbol, otherwise the name is displayed from the Strings section.


   procedure Put (
      Item    : in Symbols_T;
      Strings : in Strings_T);
   --
   -- The Strings table is used for symbol names over 8 characters long.


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


end Formats.COFF.Text;
