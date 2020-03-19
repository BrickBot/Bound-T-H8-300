-- Formats.S_Record.Text (decl)
--
-- Printing Motorola S-record formats.
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
-- $Revision: 1.3 $
-- $Date: 2015/10/24 20:53:54 $
--
-- $Log: formats-s_record-text.ads,v $
-- Revision 1.3  2015/10/24 20:53:54  niklas
-- Moved to free licence.
--
-- Revision 1.2  2007-12-27 22:32:01  niklas
-- BT-CH-0103: Formats.S_Record uses Stream_IO, not Text_IO.
--
-- Revision 1.1  2005/08/25 12:17:24  niklas
-- First version.
--


with Ada.Text_IO;


package Formats.S_Record.Text is


   procedure Put (Item : S_Record_T);
   --
   -- Prints the given S-record in a readable form.


   procedure Dump (
      File : in IO.File_Type;
      Name : in String := "");
   --
   -- Dumps the given S-record file in a readable form.
   --
   -- If the Name parameter is not null, it is taken as the name of
   -- the S-record file for error-message locus purposes.


end Formats.S_Record.Text;
