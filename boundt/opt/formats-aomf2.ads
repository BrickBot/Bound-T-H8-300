-- Formats.AOMF2 (dummy decl)
--
-- Accessing exexutable files in the Keil OMF2 format -- dummy version.
--
-- OMF2 is an Object Module Format defined by Keil Elektronik GmbH for
-- use in the Keil compiler tools for the 8051 and related processors.
-- OMF2 borrows its name from the Intel (Absolute) Object Module Format
-- but is not an extension of OMF; all record types in OMF2 are new and
-- unique to OMF2, although some of them naturally resemble the OMF
-- records that carry similar information.
--
-- AOMF2 is Tidorum's name for the Absolute subset of OMF2, that is,
-- executable OMF2 files as emitted by a linker such as the Keil LX51.
-- The relocatable forms of OMF2 are not supported here.
--
-- However, the OMF2 specification is proprietary information of
-- Keil Elektronik GmbH (now an ARM company), labelled "Keil
-- confidential". Therefore, the open source code of Bound-T does
-- not include the actual form of the (A)OMF2 packages, and instead
-- provides just enough dummy packages to allow building.
--
-- Author: Niklas Holsti, Tidorum Ltd, 2014.
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
-- $Date: 2015/10/24 21:11:59 $
--
-- $Log: formats-aomf2.ads,v $
-- Revision 1.2  2015/10/24 21:11:59  niklas
-- Moved to free licence for the dummy options.
--
-- Revision 1.1  2014/07/16 19:33:30  niklas
-- Made AOMF2 support optional, to help open-source Bound-T.
--


package Formats.AOMF2 is


   Supported : constant Boolean := False;
   --
   -- This version of Bound-T does not support AOMF2.


end Formats.AOMF2;
