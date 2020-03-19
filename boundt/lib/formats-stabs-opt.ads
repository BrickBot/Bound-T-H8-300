-- Formats.Stabs.Opt (decl)
--
-- Command-line options for Formats.Stabs.
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
-- $Revision: 1.5 $
-- $Date: 2015/10/24 20:53:54 $
--
-- $Log: formats-stabs-opt.ads,v $
-- Revision 1.5  2015/10/24 20:53:54  niklas
-- Moved to free licence.
--
-- Revision 1.4  2012-02-14 20:39:16  niklas
-- Updated for BT-CH-0222.
--
-- Revision 1.3  2009-10-18 17:27:58  niklas
-- Corrected comment.
--
-- Revision 1.2  2007/08/18 20:25:41  niklas
-- Added Trace_Loading.
--
-- Revision 1.1  2007/01/25 21:25:36  niklas
-- BT-CH-0043.
--


with Options;
with Options.Bool;


package Formats.Stabs.Opt is


pragma Elaborate_Body;
   --
   -- To register the options and set the priority for the
   -- option groups, here defined.


   Group : constant Options.Group_Name_T := Options.Group ("stabs");
   --
   -- All the Stabs options.


   Trace_Loading_Opt : aliased Options.Bool.Option_T (Default => False);
   --
   -- Whether to trace on standard output the process of accessing
   -- and interpreting STABS information.
   --
   Trace_Loading : Boolean renames Trace_Loading_Opt.Value;


   Deallocate : Boolean := True;
   --
   -- Whether to use Unchecked_Deallocation to release unused
   -- heap memory.


end Formats.Stabs.Opt;
