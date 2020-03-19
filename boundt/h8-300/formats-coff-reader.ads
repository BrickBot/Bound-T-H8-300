-- Formats.COFF.Reader (decl)
--
-- Loading and dumping H8/300 programs stored in COFF files.
--
-- Author: Niklas Holsti.
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
-- $Date: 2015/10/26 22:19:14 $
--
-- $Log: formats-coff-reader.ads,v $
-- Revision 1.3  2015/10/26 22:19:14  niklas
-- Version 4b, and moved to free open-source licence.
--
-- Revision 1.2  2005/03/17 07:24:46  niklas
-- Adapted to privatization of Program_T attributes as follows.
-- Changed Load.Symbol_Table to be "in" mode as befits a
-- reference type. Also removed call of Symbols.Erase because
-- this is now done in the general part.
--
-- Revision 1.1  2004/05/31 20:28:38  niklas
-- First version.
--


with Memory;
with Symbols;


package Formats.COFF.Reader is


   function Accepts (
      Name : String;
      File : IO.File_Type)
   return Boolean;
   --
   -- Whether the file seems to be a COFF file, as judged by the
   -- file-name (suffix) and file contents (magic number etc).
   -- The file may have to be rewound (reset to start) before being
   -- loaded.


   procedure Load (
      File         : in     IO.File_Type;
      Content      : in out Memory.Content_T;
      Symbol_Table : in     Symbols.Symbol_Table_T);
   --
   -- Loads the (open) COFF file identified.
   -- Stores the data in an Memory Content object.
   -- Stores the symbolic (debug) information in a symbol-table.
   -- Raises Format_Error upon any (unrecoverable) problem, after
   -- reporting the error (using Output).


   procedure Dump (File : in IO.File_Type);
   --
   -- Reads the COFF file via the given (open) file and describes its
   -- contents on standard output.
   -- Does not propagate Format_Error, whatever happens, but
   -- reports errors via Output.


end Formats.COFF.Reader;
