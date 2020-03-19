-- Format.Opt (decl)
--
-- Command-line options for the H8/300 executable format reader.
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
-- $Revision: 1.8 $
-- $Date: 2015/10/29 15:35:19 $
--
-- $Log: format-opt.ads,v $
-- Revision 1.8  2015/10/29 15:35:19  niklas
-- Added "-trace load" option.
--
-- Revision 1.7  2015/10/26 22:19:13  niklas
-- Version 4b, and moved to free open-source licence.
--
-- Revision 1.6  2015/10/26 22:01:04  niklas
-- Updated to use current Options services.
--
-- Revision 1.5  2007/05/03 07:34:53  niklas
-- Added Set_Trace_Option and List_Trace_Options.
--
-- Revision 1.4  2007/01/25 21:27:44  niklas
-- Updated for BT-CH-0043.
--
-- Revision 1.3  2005/05/09 16:07:29  niklas
-- Moved the types Form_T and Known_Form_T to the parent package.
--
-- Revision 1.2  2005/04/01 14:13:10  niklas
-- Added UBROF support.
--
-- Revision 1.1  2004/06/16 07:41:37  niklas
-- First version.
--


with Options;
with Options.Bool;


package Format.Opt is


   pragma Elaborate_Body;
   --
   -- To register the options here declared.


   package Form_Valued is new Options.Discrete_Valued (
      Value_Type  => Form_T,
      Value_Image => Form_T'Image);
   --
   Form_Opt : aliased Form_Valued.Option_T (Default => Unknown);
   --
   -- The assumed format of the executable file.
   --
   Form : Form_T renames Form_Opt.Value;


   Trace_Loading_Opt : aliased Options.Bool.Option_T (Default => False);
   --
   -- Whether to trace (dump) all data read, on the fly.
   --
   Trace_Loading : Boolean renames Trace_Loading_Opt.Value;


   procedure Handle_Wild_Option (
      Name  : in     String;
      Valid :    out Boolean);
   --
   -- Handles a command-line option of the form "-<Name>", when
   -- the Name is not the name of any registered option.


   procedure Set_No_Deallocation;
   --
   -- Implements the target-specific part of "-imp no_dealloc", the
   -- option that prevents all uses of Unchecked_Deallocation, for
   -- the executable format functions.


end Format.Opt;
