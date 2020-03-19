-- Flow.Opt (body)
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
-- $Date: 2015/10/24 20:05:48 $
--
-- $Log: flow-opt.adb,v $
-- Revision 1.3  2015/10/24 20:05:48  niklas
-- Moved to free licence.
--
-- Revision 1.2  2013-02-05 20:08:31  niklas
-- Using Options.General.Trace_Resolution for Trace_Flow_Resolution.
--
-- Revision 1.1  2011-08-31 04:17:12  niklas
-- Added for BT-CH-0222: Option registry. Option -dump. External help files.
--


with Options;
with Options.General;
with Options.Groups;


package body Flow.Opt is


   function Trace_Flow_Resolution return Boolean
   is
   begin

      return Options.General.Trace_Resolution;

   end Trace_Flow_Resolution;


   Flow_Trace : constant Options.Groups_T := (
      Options.Groups.Control_Flow,
      Options.Groups.Trace);


begin  -- Flow.Opt

   Options.Register (
      Option => Warn_Dynamic_Flow_Opt'access,
      Name   => Options.Warn_Item ("flow"),
      Groups => (Options.Groups.Control_Flow, Options.Groups.Warn));

   Options.Register (
      Option => Trace_Construction_Opt'access,
      Name   => Options.Trace_Item ("flow"),
      Groups => Flow_Trace);

   Options.Register (
      Option => Trace_Data_States_Opt'access,
      Name   => Options.Trace_Item ("data"),
      Groups => Flow_Trace);

   Options.Register (
      Option => Trace_Data_Refinement_Opt'access,
      Name   => Options.Trace_Item ("data_refine"),
      Groups => Flow_Trace);

   Options.Register (
      Option => Check_Consistency_Opt'access,
      Name   => Options.Imp_Item ("check_flow"),
      Groups => (Options.Groups.Control_Flow,
                 Options.Groups.Imp));

   Options.Register (
      Option => Virtual_Mood_Opt'access,
      Name   => "virtual",
      Group  => Options.Groups.Control_Flow);

end Flow.Opt;
