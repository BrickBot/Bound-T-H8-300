-- Formats.COFF.Stabs (body)
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
-- $Date: 2015/10/24 20:53:51 $
--
-- $Log: formats-coff-stabs.adb,v $
-- Revision 1.3  2015/10/24 20:53:51  niklas
-- Moved to free licence.
--
-- Revision 1.2  2005-06-12 08:19:04  niklas
-- Using "in" mode for Symbol_Table_T as it has reference semantics.
--
-- Revision 1.1  2004/04/24 18:09:08  niklas
-- First version.
--


with Output;


package body Formats.COFF.Stabs is


   procedure Define_Symbols (
      From         : in     IO.File_Type;
      File         : in     File_Ref;
      Action       : in out Formats.Stabs.Parsing.Action_T'Class;
      Symbol_Table : in     Symbols.Symbol_Table_T)
   is


      function Segment (Name : String)
      return Formats.Segment_Desc_T
      --
      -- Describes the section of the given name.
      -- Raises Section_Not_Found if there is no such section/segment.
      --
      is
         use type IO.Count;

         Number : Section_Number_T;
         -- The number of the section.

         Header : Section_Header_T;
         -- The header of the section.

      begin

         Number := Number_Of (Section => Name, Within => File.all);

         Header := File.Sections(Number).Header;

         return (
            Start  => File.IO.Index + IO.Count (Header.Data_Loc),
            Octets => IO.Count (Header.Length));

      exception

         when Section_Not_Found =>

            Output.Note (Text =>
                 "No STABS section "
               & Name
               & " is present.");

            raise;

      end Segment;
         

   begin  -- Define_Stabs_Symbols

      Formats.Stabs.Parsing.Parse (
         From         => From,
         Lines        => Segment (".stab"   ),
         Strings      => Segment (".stabstr"),
         Action       => Action,
         Trace        => False,
         Symbol_Table => Symbol_Table);

   exception

      when Section_Not_Found =>

         Output.Note (Text =>
            "No STABS symbols (section(s) missing)");

   end Define_Symbols;


end Formats.COFF.Stabs;
