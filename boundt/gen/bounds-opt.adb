-- Bounds.Opt (body)
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
-- $Revision: 1.3 $
-- $Date: 2015/10/24 20:05:46 $
--
-- $Log: bounds-opt.adb,v $
-- Revision 1.3  2015/10/24 20:05:46  niklas
-- Moved to free licence.
--
-- Revision 1.2  2013-02-05 20:09:11  niklas
-- Using Options.General.Trace_Resolution for Trace_Data_Resolution.
--
-- Revision 1.1  2011-08-31 04:17:12  niklas
-- Added for BT-CH-0222: Option registry. Option -dump. External help files.
--


with Flow.Opt;
with License;
with Options.General;
with Options.Groups;
with Output;


package body Bounds.Opt is


   function Trace_Data_Resolution return Boolean
   is
   begin

      return Options.General.Trace_Resolution;

   end Trace_Data_Resolution;


   procedure Set_Stack_Analysis (To : in Boolean)
   is
   begin

      if not To then

         Bound_Stack := False;

      elsif License.Allows_Stack_Analysis then

         Bound_Stack := True;

      else

         Output.Error ("Not licensed for stack (-path) analysis.");

      end if;

   end Set_Stack_Analysis;


begin  -- Bounds.Opt

   if License.Allows_Time_Analysis then

      Options.Register (
         Option => Bound_Time_Opt'access,
         Name   => "time",
         Group  => Options.Groups.Analysis);

   end if;

   if License.Allows_Stack_Analysis then

      Options.Register (
         Option => Bound_Stack_Opt'access,
         Name   => "stack",
         Group  => Options.Groups.Analysis);

   end if;

   Options.Register (
      Option => Trace_Phase_Opt'access,
      Name   => Options.Trace_Item ("phase"),
      Group  => Options.Groups.Trace);

   Options.Register (
      Option => Trace_Arith_Opt'access,
      Name   => Options.Trace_Item ("arith"),
      Group  => Options.Groups.Trace);

   Options.Register (
      Option => Trace_Context_Opt'access,
      Name   => Options.Trace_Item ("context"),
      Group  => Options.Groups.Trace);

   Options.Register (
      Option => Trace_Nubs_Opt'access,
      Name   => Options.Trace_Item ("nubs"),
      Group  => Options.Groups.Trace);

   Options.Register (
      Option => Trace_Cell_Sets_Opt'access,
      Name   => Options.Trace_Item ("cells"),
      Group  => Options.Groups.Trace);

   Options.Register (
      Option => Warn_Unresolved_Data_Opt'access,
      Name   => Options.Warn_Item ("access"),
      Group  => Options.Groups.Warn);

   Options.Register (
      Option => Trace_Graphs_Opt'access,
      Name   => Options.Trace_Item ("graph"),
      Group  => Options.Groups.Trace);

   Options.Register (
      Option => Trace_Instructions_Opt'access,
      Name   => Options.Trace_Item ("instr"),
      Group  => Options.Groups.Trace);

   Options.Register (
      Option => Max_Dependency_Depth_Opt'access,
      Name   => "max_par_depth",
      Groups => (Options.Groups.Analysis,
                 Options.Groups.Resource_Limits));

   Options.Register (
      Option => Max_Restarts_Opt'access,
      Name   => "model_iter",
      Group  => Options.Groups.Resource_Limits);

end Bounds.Opt;
