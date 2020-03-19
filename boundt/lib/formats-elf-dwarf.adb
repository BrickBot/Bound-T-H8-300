-- Formats.ELF.Dwarf (body)
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
-- $Date: 2015/10/24 20:53:53 $
--
-- $Log: formats-elf-dwarf.adb,v $
-- Revision 1.5  2015/10/24 20:53:53  niklas
-- Moved to free licence.
--
-- Revision 1.4  2007-06-14 11:16:41  niklas
-- BT-CH-0060.
--
-- Revision 1.3  2006/04/12 19:47:55  niklas
-- Added the "out" parameter Define_Symbols.Found to report
-- if some DWARF information was found.
-- Extended Define_Symbols and Dump_Info to use the .debug_str
-- section.
--
-- Revision 1.2  2005/06/12 08:25:47  niklas
-- Using "in" mode for Symbol_Table_T as it has reference semantics.
--
-- Revision 1.1  2004/04/24 18:05:13  niklas
-- First version.
--


with Formats.Dwarf;
with Formats.Dwarf.Abbrev;
with Formats.Dwarf.Expressions;
with Formats.Dwarf.Info;
with Formats.Dwarf.Line_Numbers;
with Formats.Dwarf.Strings;
with Output;


package body Formats.ELF.Dwarf is


   procedure Define_Symbols (
      From         : in     IO.File_Type;
      File         : in     File_Ref;
      Action       : in out Formats.Dwarf.Info.Parsing.Action_T'Class;
      Symbol_Table : in     Symbols.Symbol_Table_T;
      Found        :    out Boolean)
   is
      use Formats.Dwarf;
      -- For the tag names, mainly.

      Info_Section : Segment_Desc_T;
      -- The .debug_info section, if any.

      Abbrev_Section : Segment_Desc_T;
      -- The .debug_abbrev section, if any.

      Str_Section : Segment_Desc_T;
      -- The .debug_str section, if any,

      Loc_Section : Segment_Desc_T;
      -- The .debug_loc section, if any, else a null section.

      Lines_Section : Segment_Desc_T;
      -- The .debug_line section, if any.

      Tree : Info.Tree_T;
      -- The loaded DWARF tree.

   begin

      Info_Section := Section (
         Name   => Formats.Dwarf.Info.Section_Name,
         Within => File.all);

      Abbrev_Section := Section (
         Name   => Formats.Dwarf.Abbrev.Section_Name,
         Within => File.all);

      Str_Section := Section (
         Name     => Formats.Dwarf.Strings.Section_Name,
         Within   => File.all,
         Optional => True);

      Loc_Section := Section (
         Name     => Formats.Dwarf.Expressions.Location_List_Section_Name,
         Within   => File.all,
         Optional => True);

      Output.Note ("Loading DWARF info section.");

      Formats.Dwarf.Info.Load (
         Info   => Info_Section,
         Abbrev => Abbrev_Section,
         Str    => Str_Section,
         Loc    => Loc_Section,
         From   => From,
         Tree   => Tree);

      Action.Kit.Table := Symbol_Table;
      --
      -- Since this is a by-reference type, all additions made thru
      -- Definer.Kit.Table are permanent and visible also thru
      -- Symbol_Table or any other reference to this symbol-table object.

      -- If we have a line-number section, load it so that Definer
      -- can define line-numbers too:

      Lines_Section := Section (
         Name     => Line_Numbers.Section_Name,
         Within   => File.all,
         Optional => True);

      if Is_Null (Lines_Section) then
         -- There is no line-number section.

         Output.Note ("DWARF line-number section missing.");

      else
         -- There is a line-number section.

         Line_Numbers.Load (
           From    => From,
           Section => Lines_Section,
           Giving  => Action.Lines);

      end if;

      Output.Note ("Parsing DWARF symbols.");

      Formats.Dwarf.Info.Traverse (
         Tree     => Tree,
         Opening  => Formats.Dwarf.Info.Parsing.Tags_To_Open,
         Visiting => Formats.Dwarf.Info.Parsing.Tags_To_Visit,
         Depth    => 100,
         Visitor  => Action);

      Found := True;

   exception

   when Section_Not_Found =>

      Output.Note ("Some DWARF section(s) missing.");

      Found := False;

   end Define_Symbols;


   procedure Dump_Info (
      From : in IO.File_Type;
      File : in File_Ref)
   --
   -- Dumps the DWARF data from the ".debug_info" section if present
   -- (and is also the ".debug_abbrev" section is present).
   --
   is
      use Formats.Dwarf;
      -- For the tag names, mainly.

      Info_Section : Segment_Desc_T;
      -- The .debug_info section, if any.

      Abbrev_Section : Segment_Desc_T;
      -- The .debug_abbrev section, if any.

      Str_Section : Segment_Desc_T;
      -- The .debug_str section, if any, else a null section.

      Loc_Section : Segment_Desc_T;
      -- The .debug_loc section, if any, else a null section.

   begin

      Info_Section := Section (
         Name   => Formats.Dwarf.Info.Section_Name,
         Within => File.all);

      Abbrev_Section := Section (
         Name   => Formats.Dwarf.Abbrev.Section_Name,
         Within => File.all);

      Str_Section := Section (
         Name     => Formats.Dwarf.Strings.Section_Name,
         Within   => File.all,
         Optional => True);

      Loc_Section := Section (
         Name     => Formats.Dwarf.Expressions.Location_List_Section_Name,
         Within   => File.all,
         Optional => True);

      Formats.Dwarf.Info.Dump (
         Info   => Info_Section,
         Abbrev => Abbrev_Section,
         Str    => Str_Section,
         Loc    => Loc_Section,
         From   => From);

   exception

      when Section_Not_Found =>

         Output.Note ("DWARF Info/Abbrev sections missing.");

   end Dump_Info;


   procedure Dump_Lines (
      From : in IO.File_Type;
      File : in File_Ref)
   --
   -- Dumps the DWARF line-number information if this section
   -- is present in the ELF file.
   --
   is
      use Formats.Dwarf;
      -- For the tag names, mainly.

      Lines_Section : Segment_Desc_T;
      -- The .debug_line section, if any.

   begin

      Lines_Section := Section (
         Name   => Line_Numbers.Section_Name,
         Within => File.all);

      Line_Numbers.Dump (
         Section => Lines_Section,
         From    => From);

   exception

      when Section_Not_Found =>

         Output.Note ("DWARF line-number section missing.");

   end Dump_Lines;


   procedure Dump_Symbols (
      From : in IO.File_Type;
      File : in File_Ref)
   is
   begin

      Dump_Info  (From, File);

      Dump_Lines (From, File);

   end Dump_Symbols;


end Formats.ELF.Dwarf;
