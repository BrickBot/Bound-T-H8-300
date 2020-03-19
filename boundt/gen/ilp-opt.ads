-- ILP.Opt (decl)
--
-- Command-line options that control the determination of the
-- worst-case paths using Integer Linear Programming.
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
-- $Date: 2015/10/24 19:36:50 $
--
-- $Log: ilp-opt.ads,v $
-- Revision 1.4  2015/10/24 19:36:50  niklas
-- Moved to free licence.
--
-- Revision 1.3  2011-08-31 04:23:34  niklas
-- BT-CH-0222: Option registry. Option -dump. External help files.
--
-- Revision 1.2  2004/05/01 10:28:33  niklas
-- First Tidorum version.
-- Added the option Keep_Files.
-- Corrected description of file names.
--
-- Revision 1.1  2001/11/19 11:10:18  saarinen
-- First versions.
--
-- Revision 1.3  2001/09/28 09:14:10  holsti
-- Platform-dependent parts moved to Paths.Platform.
-- Added Paths.Opt.Trace_IO.
--
-- Revision 1.2  2000/09/08 13:08:30  saarinen
-- Added filenames for lp_solve input and output debug files.
--
-- Revision 1.1  2000/04/21 19:39:05  holsti
-- Renamed child Options to Opt
--


with Options;
with Options.Bool;
with Options.Strings;
with ILP.Platform;


package ILP.Opt is


   pragma Elaborate_Body;
   --
   -- To register the options.


   Prog_LP_SOLVE : aliased Options.Strings.Option_T :=
      Options.Strings.Set (ILP.Platform.Program);
   --
   -- The name of (path to) the ILP solver program.
   -- It can be an absolute path, or just a name. In the latter
   -- case, the user's command-lookup path is used to find the
   -- program.


   Keep_Files_Opt : aliased Options.Bool.Option_T (Default => False);
   --
   -- Whether the ILP solver input and output files should
   -- be kept as permanent disk files at the end of the run.
   --
   Keep_Files : Boolean renames Keep_Files_Opt.Value;


   LP_Input_File : aliased Options.Strings.Option_T :=
      Options.Strings.Set ("lp_in");
   --
   -- The prefix of the file name for the lp input debug file.
   -- The actual file name is this prefix plus a sequential number
   -- of the ILP solver instance started in this Bound-T run.


   LP_Output_File : aliased Options.Strings.Option_T :=
      Options.Strings.Set ("lp_out");
   --
   -- The prefix of the file name for the lp output debug file.
   -- The actual file name is this prefix plus a sequential number
   -- of the ILP solver instance started in this Bound-T run.


   Trace_IO_Opt : aliased Options.Bool.Option_T (Default => False);
   --
   -- Whether to trace input commands to the ILP solver engine
   -- and outputs from the engine in real time, interactively.
   -- The inputs and outputs are always logged (if the platform
   -- implements it) but the real-time trace is optional.
   --
   Trace_IO : Boolean renames Trace_IO_Opt.Value;


end ILP.Opt;
