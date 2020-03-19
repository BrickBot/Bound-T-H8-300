-- Formats.AOMF.Opt (decl)
--
-- Options for accessing AOMF information, with extensions by
-- Keil Elektronik GmbH.
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
-- $Revision: 1.6 $
-- $Date: 2015/10/24 20:53:51 $
--
-- $Log: formats-aomf_keil-opt.ads,v $
-- Revision 1.6  2015/10/24 20:53:51  niklas
-- Moved to free licence.
--
-- Revision 1.5  2008-02-27 19:39:51  niklas
-- Added the option Define_Segment_Symbols (default False) to control
-- whether Segment_Symbols are entered in the Bound-T symbol table.
--
-- Revision 1.4  2007/08/31 11:30:00  niklas
-- Added Warn_Line_No_Sub.
--
-- Revision 1.3  2007/08/31 08:56:20  niklas
-- Added option Trace_Symbols.
--
-- Revision 1.2  2007/01/25 21:25:35  niklas
-- BT-CH-0043.
--
-- Revision 1.1  2004/10/10 10:03:28  niklas
-- First version.
--


package Formats.AOMF_Keil.Opt is


   Trace_Loading : Boolean := False;
   --
   -- Whether to trace on standard output the process of accessing
   -- and interpreting AOMF information.


   Trace_Symbols : Boolean := False;
   --
   -- Whether to trace on standard output the AOMF symbols offered
   -- to the target-specific parsing actions.


   Warn_Line_No_Sub : Boolean := False;
   --
   -- Whether to issue warning when the AOMF file contains line-number
   -- information for which the subprogram name is not known.


   Define_Segment_Symbols : Boolean := False;
   --
   -- Whether to include "Segment Symbols" when the AOMF file is
   -- parsed to define symbols for subprograms and variables.


   Deallocate : Boolean renames Formats.AOMF_Keil.Deallocate;
   --
   -- Whether to use Unchecked_Deallocation to release unused
   -- heap memory.


end Formats.AOMF_Keil.Opt;
