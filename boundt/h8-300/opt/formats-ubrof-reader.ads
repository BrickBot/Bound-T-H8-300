-- Formats.UBROF.Reader (decl)
--
-- Loading and dumping Renesas H8/300 programs stored in IAR UBROF files.
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
-- $Revision: 1.1 $
-- $Date: 2015/10/29 14:51:18 $
--
-- $Log: formats-ubrof-reader.ads,v $
-- Revision 1.1  2015/10/29 14:51:18  niklas
-- UBROF support made locally optional.
--


with Memory;
with Symbols;


package Formats.UBROF.Reader is


   function Accepts (
      Name : String;
      File : IO.File_Type)
   return Boolean;
   --
   -- Whether the file seems to be an UBROF file, as judged by the
   -- file-name (suffix) and file contents (initial record tags).
   -- The file may have to be rewound (reset to start) before being
   -- loaded.


   procedure Load (
      File         : in     IO.File_Type;
      Content      : access Memory.Content_T;
      Symbol_Table : in     Symbols.Symbol_Table_T);
   --
   -- Loads the (open) UBROF file identified.
   -- Stores the code and data in a Memory Content object.
   -- Stores the symbolic (debug) information in a symbol-table.
   --
   -- Raises Format_Error upon any (unrecoverable) problem, after
   -- reporting the error (using Output).


   procedure Dump (File : in IO.File_Type);
   --
   -- Reads the UBROF file via the given (open) file and describes its
   -- contents on standard output.
   --
   -- Does not propagate Format_Error, whatever happens, but
   -- reports errors via Output.


end Formats.UBROF.Reader;
