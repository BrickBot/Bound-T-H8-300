-- Programs.Patching (decl)
--
-- Patching (modifying) the memory image of a target program, after it
-- has been loaded and before it is analysed.
--
-- When Bound-T analyses a target program, it starts from the statically
-- generated (linker-generated) memory image. In some programs, important
-- parts of the memory image are defined or altered by the start-up code
-- in ways that Bound-T does not track, giving a wrong analysis.
-- The most common example is setting up the trap/interrupt vectors.
-- Such dynamic initializations can be imitated by the patching services
-- in this package, although this tends to be cumbersome and sensitive
-- to changes in the memory layout of the target program.
--
-- The patches are given in "patch files" which are text files with a
-- generic surface syntax but where the detailed syntax and meaning depend
-- on the target processor. The present package provides operations to
-- name patch files and to read them and parse the surface syntax, but
-- the detailed interpretation and actual patching are delegated to the
-- target-specific operation Decoder.Patch_Code. At the moment, there is
-- no similar operation for patching constant data, but there may be
-- target-specific ways to make Patch_Code change data areas instead of
-- code areas.
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
-- $Date: 2015/10/24 19:36:52 $
--
-- $Log: programs-patching.ads,v $
-- Revision 1.4  2015/10/24 19:36:52  niklas
-- Moved to free licence.
--
-- Revision 1.3  2011-09-01 19:58:47  niklas
-- BT-CH-0222: Option registry.
--
-- Revision 1.2  2008-11-09 21:41:24  niklas
-- BT-CH-0158: Option "-trace patch".
--
-- Revision 1.1  2006/02/27 20:04:50  niklas
-- First version.
--


with Options.Bool;
with Options.File_Sets;


package Programs.Patching is


   Trace_Patching_Opt : aliased Options.Bool.Option_T (Default => False);
   --
   -- Whether to trace the patching actions.
   --
   Trace_Patching : Boolean renames Trace_Patching_Opt.Value;


   Patch_Files : aliased Options.File_Sets.Option_T;
   --
   -- The (names of) the patch files, if given.
   -- Empty set by default.


   procedure Apply_Patches (
      Program : in     Program_T;
      Valid   :    out Boolean);
   --
   -- Applies the set of patch files that has been defined
   -- by the preceding (zero or more) calls of Add_Patch_File,
   -- in the same order. That is, the patches from the file
   -- named in the first Add_Patch_File are applied first.
   -- Later patches may override earlier patches.
   --
   -- Program
   --    The program to be patched. It has been initialized
   --    by Decoder.Initialize.
   -- Valid
   --    Returned as True on success, False on failure.


end Programs.Patching;
