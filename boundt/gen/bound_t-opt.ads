-- Bound_T.Opt (decl)
--
-- Command-line options that control the main operation mode of
-- the Bound-T static program analysis tool.
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
-- $Date: 2015/10/24 19:36:47 $
--
-- $Log: bound_t-opt.ads,v $
-- Revision 1.8  2015/10/24 19:36:47  niklas
-- Moved to free licence.
--
-- Revision 1.7  2011-09-01 22:15:42  niklas
-- Added Show_Licence(_Opt), registered as "-licence" and "-license".
-- Added synonym "-synonyms" for "-synonym".
--
-- Revision 1.6  2011-08-31 04:23:33  niklas
-- BT-CH-0222: Option registry. Option -dump. External help files.
--
-- Revision 1.5  2007/07/27 20:24:18  niklas
-- BT-CH-0068. Option -synonyms.
--
-- Revision 1.4  2007/03/16 07:08:50  niklas
-- Added Time_Unlimited.
--
-- Revision 1.3  2007/03/01 12:28:37  niklas
-- Added Max_Analysis_Time.
--
-- Revision 1.2  2006/11/03 06:33:52  niklas
-- BT-CH-0035.
--
-- Revision 1.1  2005/08/08 13:58:27  niklas
-- First version.
--


with Options;
with Options.Bool;


package Bound_T.Opt is


   pragma Elaborate_Body;
   --
   -- To register the options.


   Show_Version_Opt : aliased Options.Bool.Option_T (Default => False);
   --
   -- Whether to display the version of Bound-T at the start of the run.
   -- This identifies the target processor for this Bound-T and the
   -- implementation level (version number) of this Bound-T.
   --
   Show_Version : Boolean renames Show_Version_Opt.Value;


   Show_Host_Version_Opt : aliased Options.Bool.Option_T (Default => False);
   --
   -- Whether to display the host-version of Bound-T at the start of
   -- the run. This identifies the host system and compiler on which
   -- this Bound-T was compiled and linked.
   --
   Show_Host_Version : Boolean renames Show_Host_Version_Opt.Value;


   Show_Licence_Opt : aliased Options.Bool.Option_T (Default => False);
   --
   -- Whether to display the licence of Bound-T at the start of
   -- the run. This identifies both the active licensing mechanism(s)
   -- and the user's present valid licence, if there is one.
   --
   Show_Licence : Boolean renames Show_Licence_Opt.Value;


   Dump_Program_Opt : aliased Options.Bool.Option_T (Default => False);
   --
   -- Whether to dump the contents of the target program file.
   -- At present this is all or none, but should be made selective so
   -- that one could dump only the symbol tables, for example.
   --
   Dump_Program : Boolean renames Dump_Program_Opt.Value;


   HRT_Opt : aliased Options.Bool.Option_T (Default => False);
   --
   -- Whether the HRT (Hard Real Time) analysis mode is chosen.
   --
   HRT : Boolean renames HRT_Opt.Value;


   Time_Analysis_Opt : aliased Options.Bool.Option_T (Default => False);
   --
   -- Whether to measure and report the analysis time.
   --
   Time_Analysis : Boolean renames Time_Analysis_Opt.Value;


   Time_Unlimited : constant Duration := Duration'Last;
   --
   -- A practically unlimited duration of analysis.


   function Time_Image (Item : Duration) return String;
   --
   -- For Max_Analysis_Time, below.


   package Duration_Valued is new Options.Valued (
      Value_Type             => Duration,
      Value_Type_Description => "Duration in seconds",
      Value                  => Duration'Value,
      Image                  => Time_Image);
   --
   -- Options with values of type Duration.


   Max_Analysis_Time_Opt : aliased Duration_Valued.Option_T :=
      Duration_Valued.Default (Time_Unlimited);
   --
   -- Maximum analysis time, after which the analysis is aborted.
   --
   Max_Analysis_Time : Duration renames Max_Analysis_Time_Opt.Value;


   List_Synonyms_Opt : aliased Options.Bool.Option_T (Default => False);
   --
   -- Whether to list all synonyms of all known subprograms, at
   -- the end of the analysis.
   --
   List_Synonyms : Boolean renames List_Synonyms_Opt.Value;


   Deallocate_Opt : aliased Options.Bool.Option_T (Default => True);
   --
   -- Whether to deallocate dynamically allocated heap memory by
   -- instances of Unchecked Deallocation.
   --
   Deallocate : Boolean renames Deallocate_Opt.Value;


end Bound_T.Opt;
