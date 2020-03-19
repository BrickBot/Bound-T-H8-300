-- Formats.COFF.Stabs (decl)
--
-- Handling symbol tables in the Stabs (or STABS) form, embedded in COFF
-- files.
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
-- $Log: formats-coff-stabs.ads,v $
-- Revision 1.3  2015/10/24 20:53:51  niklas
-- Moved to free licence.
--
-- Revision 1.2  2005-06-12 08:19:04  niklas
-- Using "in" mode for Symbol_Table_T as it has reference semantics.
--
-- Revision 1.1  2004/04/24 18:09:08  niklas
-- First version.
--


with Formats.Stabs.Parsing;
with Symbols;


package Formats.COFF.Stabs is


   procedure Define_Symbols (
      From         : in     IO.File_Type;
      File         : in     File_Ref;
      Action       : in out Formats.Stabs.Parsing.Action_T'Class;
      Symbol_Table : in     Symbols.Symbol_Table_T);
   --
   -- Loads Stabs symbols from the given COFF file, if the file contains
   -- the Stabs sections (.stabs and stabstr).
   --
   -- Note that Symbol_Table_T is a reference type, so Symbol_Table can
   -- be updated although it is "in" mode.


end Formats.COFF.Stabs;
