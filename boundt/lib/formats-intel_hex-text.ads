-- Formats.Intel_Hex.Text (decl)
--
-- Printing Intel Hex record formats.
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
-- $Revision: 1.2 $
-- $Date: 2015/10/24 20:53:53 $
--
-- $Log: formats-intel_hex-text.ads,v $
-- Revision 1.2  2015/10/24 20:53:53  niklas
-- Moved to free licence.
--
-- Revision 1.1  2007-09-22 14:19:12  niklas
-- First version.
--


with Ada.Text_IO;


package Formats.Intel_Hex.Text is


   procedure Put (Item : Record_T);
   --
   -- Prints the given I-Hex record in a readable form.


   procedure Dump (
      File : in Ada.Text_IO.File_Type;
      Name : in String := "");
   --
   -- Dumps the given Intel-Hex file in a readable form.
   --
   -- If the Name parameter is not null, it is taken as the name of
   -- the Intex-Hex record file for error-message locus purposes.


end Formats.Intel_Hex.Text;
