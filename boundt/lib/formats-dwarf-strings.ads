-- Formats.Dwarf.Strings (decl)
--
-- String values for DWARF information entries
--
-- String-values DWARF attributes or properties can be encoded in two
-- forms: Form_T = String_Form or Form_T = Strp. In the latter (Strp)
-- case, the string value is contained in a section dedicated to such
-- string valuaes, and the DWARF attribute contains an offset that
-- points to the start of the string in the strings section. This
-- package, Formats.Dwarf.Strings, describes the string section.
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
-- $Revision: 1.2 $
-- $Date: 2015/10/24 20:53:52 $
--
-- $Log: formats-dwarf-strings.ads,v $
-- Revision 1.2  2015/10/24 20:53:52  niklas
-- Moved to free licence.
--
-- Revision 1.1  2006-04-12 19:35:17  niklas
-- First version.
--


package Formats.Dwarf.Strings is


   Section_Name : constant String := ".debug_str";
   --
   -- The name of the DWARF section that contains the string
   -- values for attributes or properties with Form = Strp.


end Formats.Dwarf.Strings;
