-- Formats.AOMF2.Opt (dummy decl)
--
-- Options for accessing AOMF2 information -- dummy version.
--
-- AOMF2 is the (Absolute) Object Module Format defined by Keil Elektronik
-- GmbH and confidential to Keil. This package and its relatives provide
-- the Bound-T tool with access to target programs in AOMF2 form.
--
-- However, the OMF2 specification is proprietary information of
-- Keil Elektronik GmbH (now an ARM company), labelled "Keil
-- confidential". Therefore, the open source code of Bound-T does
-- not include the actual form of the (A)OMF2 packages, and instead
-- provides just enough dummy packages to allow building.
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
-- $Revision: 1.2 $
-- $Date: 2015/10/24 21:11:59 $
--
-- $Log: formats-aomf2-opt.ads,v $
-- Revision 1.2  2015/10/24 21:11:59  niklas
-- Moved to free licence for the dummy options.
--
-- Revision 1.1  2014/07/16 19:33:29  niklas
-- Made AOMF2 support optional, to help open-source Bound-T.
--


package Formats.AOMF2.Opt is

   Trace_Loading : Boolean := False;
   Trace_Symbols : Boolean := False;
   Deallocate    : Boolean := False;

end Formats.AOMF2.Opt;
