-- Formats.AOMF_Keil.Text (decl)
--
-- Textual display of AOMF subset of OMF-51 executable files
-- with extensions specific to the Keil GmbH compilers (EAOMF)
--
-- Author: Niklas Holsti, Tidorum Ltd, 2004.
-- Partly after the version by Mikko Ala-Fossi, Space Systems Finland, 1999.
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
-- $Log: formats-aomf_keil-text.ads,v $
-- Revision 1.3  2015/10/24 20:53:51  niklas
-- Moved to free licence.
--
-- Revision 1.2  2007-08-31 08:57:11  niklas
-- Published some operations for use in parsing.
--
-- Revision 1.1  2004/10/03 16:00:25  niklas
-- First version.
--


package Formats.AOMF_Keil.Text is


   -- The following Image functions return a human-readable
   -- description of the parameter.


   function Image (Item : Storage_T) return String;


   function Image (Item : Type_Index_T) return String;


   -- The following Put functions output one or more lines that
   -- describe the parameter.


   procedure Put (Item : in Compound_Type_T);


   procedure Dump (File : in IO.File_Type);
   --
   -- Reads the AOMF file via the given (open) file and describes its
   -- contents on standard output.
   -- Does not propagate Format_Error, whatever happens, but
   -- reports errors via Output.


end Formats.AOMF_Keil.Text;
