-- Formats.Dwarf.Text (decl)
--
-- Printing DWARF information in readable form.
--
-- Author: Niklas Holsti.
--
-- A component of the Bound-T Worst-Case Execution Time Tool.
--
-------------------------------------------------------------------------------
-- Copyright (c) 1999 .. 2015 Tidorum Ltd, except for text copied verbatim
-- from the DWARF standard.
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
-- $Date: 2015/10/24 20:53:52 $
--
-- $Log: formats-dwarf-text.ads,v $
-- Revision 1.4  2015/10/24 20:53:52  niklas
-- Moved to free licence.
--
-- Revision 1.3  2013-02-03 10:50:28  niklas
-- BT-CH-0238: DWARF expression abstract evaluation - start.
--
-- Revision 1.2  2007-06-14 11:16:40  niklas
-- BT-CH-0060.
--
-- Revision 1.1  2004/04/24 18:06:21  niklas
-- First version.
--


package Formats.Dwarf.Text is


   function Trim (Item : String) return String;
   --
   -- Trims leading and trailing blanks.


   procedure Begin_Line (Indent : in String);
   --
   -- Begins a standard output line by a given Indentation string,
   -- starting from a defined Margin column.


   procedure Put_Line (
      Indent : in String;
      Line   : in String);
   --
   -- Emits a standard output Line, beginning the line with a given
   -- Indentation string.


   function Addr_Image (
      Info      : access Ada.Streams.Root_Stream_Type'Class;
      Addr_Size :        Addr_Size_T)
   return String;
   --
   -- Textual form of an Addr value from the Info stream, using
   -- the given number of octets.


   function Constant_Image (
      Info : access Ada.Streams.Root_Stream_Type'Class;
      Form :        Constant_Form_T)
   return String;
   --
   -- Textual form of an constant value from the Info stream,
   -- using the given kind of constant form.


   procedure Dump_Block (
      Info   : access Ada.Streams.Root_Stream_Type'Class;
      Form   : in     Block_Form_T;
      Indent : in     String);
   --
   -- Dumps a block value from the Info stream, using the given
   -- kind of block form.


   function Reference_Image (
      Info : access Ada.Streams.Root_Stream_Type'Class;
      Form :        Reference_Form_T;
      Bits :        Bits_T;
      Base :        In_Memory.Index_T)
   return String;
   --
   -- Textual form of a reference value from the Info stream, using the
   -- given kind of reference Form, the given bit-width, and the Base
   -- index (stream position) of the compilation unit.


   procedure Dump_Attribute (
      Info      : access Ada.Streams.Root_Stream_Type'Class;
      Attribute : in     Attribute_T;
      Form      : in     Form_T;
      Bits      : in     Bits_T;
      Addr_Size : in     Addr_Size_T;
      Ref_Base  : in     In_Memory.Index_T;
      Str       : in     In_Memory.Stream_Ref;
      Indent    : in     String);
   --
   -- Displays on standard output the value of a given Attribute from
   -- the current position of the given Info stream, assuming that the
   -- attribute value is represented in the given Form and the given
   -- Bits width. The Reference Base index is also given as is the Str
   -- section for references to .debug_str.
   --
   -- All output lines are prefixed with the given Indentation string.


end Formats.Dwarf.Text;
