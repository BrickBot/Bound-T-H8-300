-- Formats.ELF.Opt (decl)
--
-- Options for the Formats.ELF package.
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
-- $Revision: 1.4 $
-- $Date: 2015/10/24 20:53:53 $
--
-- $Log: formats-elf-opt.ads,v $
-- Revision 1.4  2015/10/24 20:53:53  niklas
-- Moved to free licence.
--
-- Revision 1.3  2011-09-01 13:03:30  niklas
-- Updated for BT-CH-0222: Option registry.
--
-- Revision 1.2  2008-02-29 09:06:40  niklas
-- Added option Opt.Warn.
--
-- Revision 1.1  2004/04/24 18:04:53  niklas
-- First Tidorum version, as child of Formats.
--
--
-- <Log: elf-opt.ads>
-- Revision 1.1  2001/08/27 09:12:56  saarinen
-- First version.
--


with Options.Bool;


package Formats.ELF.Opt is


   pragma Elaborate_Body;
   --
   -- To register the options and define the priority ordering
   -- of the option groups.


   Group : constant Options.Group_Name_T := Options.Group ("elf");
   --
   -- All the ELF options.


   Warn_Opt : aliased Options.Bool.Option_T (Default => False);
   --
   -- Whether to issue Warnings about slight problems such as unknown
   -- section types.
   --
   Warn : Boolean renames Warn_Opt.Value;


   Trace_Loading_Opt : aliased Options.Bool.Option_T (Default => False);
   --
   -- If true, during the loading of a ELF file all elements are
   -- immediately output after being loaded.
   -- This can help understand loading problems.
   --
   Trace_Loading : Boolean renames Trace_Loading_Opt.Value;


end Formats.ELF.Opt;

