-- Formats.UBROF.Opt (decl)
--
-- Options for accessing UBROF information -- dummy version.
--
-- UBROF is the Universal Binary Relocatable Object Format defined
-- by IAR Systems and proprietary to IAR Systems. This package and its
-- relatives, in their actual form, provide the Bound-T tool with
-- access to target programs in UBROF form. However, access to the
-- UBROF specification requires a Non-Disclosure Agreement with
-- IAR Systems; therefore, the open source code of Bound-T does
-- not include the actual form of the UBROF packages, and instead
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
-- $Log: formats-ubrof-opt.ads,v $
-- Revision 1.2  2015/10/24 21:11:59  niklas
-- Moved to free licence for the dummy options.
--
-- Revision 1.1  2014/07/14 20:57:44  niklas
-- Dummy versions, for use when UBROF is not supported.
--


package Formats.UBROF.Opt is

   Trace_Loading                : Boolean := False;
   Trace_Statement_Info_Program : Boolean := False;
   Trace_Classes                : Boolean := False;
   Trace_Class_Members          : Boolean := False;
   Deallocate                   : Boolean := False;

end Formats.UBROF.Opt;
