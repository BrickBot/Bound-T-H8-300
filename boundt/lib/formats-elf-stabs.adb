-- Formats.ELF.Stabs (body)
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
-- $Date: 2015/10/24 20:53:53 $
--
-- $Log: formats-elf-stabs.adb,v $
-- Revision 1.4  2015/10/24 20:53:53  niklas
-- Moved to free licence.
--
-- Revision 1.3  2006-04-12 19:51:10  niklas
-- Added the "out" parameter Define_Symbols.Found to show if
-- a STABS symbol table was found.
--
-- Revision 1.2  2005/02/25 20:58:16  niklas
-- Changed all Symbol_Table_T parameters from "in out" to "in" as
-- befits a reference type. Related to BT-CH-0004.
--
-- Revision 1.1  2004/04/24 18:04:10  niklas
-- First version.
--


with Output;


package body Formats.ELF.Stabs is


   procedure Define_Symbols (
      From         : in     IO.File_Type;
      File         : in     File_Ref;
      Action       : in out Formats.Stabs.Parsing.Action_T'Class;
      Symbol_Table : in     Symbols.Symbol_Table_T;
      Found        :    out Boolean)
   is
   begin

      Formats.Stabs.Parsing.Parse (
         From         => From,
         Lines        => Section (Name => ".stab"   , Within => File.all),
         Strings      => Section (Name => ".stabstr", Within => File.all),
         Action       => Action,
         Trace        => False,
         Symbol_Table => Symbol_Table);

      Found := True;

   exception

   when Section_Not_Found =>

      Output.Note ("No STABS symbols (section(s) missing)");

      Found := False;

   end Define_Symbols;


end Formats.ELF.Stabs;
