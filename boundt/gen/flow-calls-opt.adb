-- Flow.Calls.Opt (body)
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
-- $Date: 2015/10/24 20:05:47 $
--
-- $Log: flow-calls-opt.adb,v $
-- Revision 1.2  2015/10/24 20:05:47  niklas
-- Moved to free licence.
--
-- Revision 1.1  2011-08-31 04:17:12  niklas
-- Added for BT-CH-0222: Option registry. Option -dump. External help files.
--


-- with Analyser.Opt;
with Options;
with Options.Groups;


package body Flow.Calls.Opt is

begin  -- Flow.Calls.Opt

   Options.Register (
      Option => Trace_Calls_Opt'access,
      Name   => Options.Trace_Item ("calls"),
      Groups => (Options.Groups.Control_Flow,
                 Options.Groups.Trace));

   Options.Register (
      Option => Trace_Unused_Calls_Opt'access,
      Name   => Options.Trace_Item ("unused"),
      Group  => Options.Groups.Trace);

   Options.Register (
      Option => Trace_Call_Effects_Opt'access,
      Name   => Options.Trace_Item ("call_eff"),
      Group  => Options.Groups.Trace);

   Options.Register (
      Option => Warn_No_Return_Opt'access,
      Name   => Options.Warn_Item ("return"),
      Groups => (Options.Groups.Control_Flow,
                 Options.Groups.Warn));

   Options.Register (
      Option => Warn_Computed_Return_Opt'access,
      Name   => Options.Warn_Item ("computed_return"),
      Groups => (Options.Groups.Control_Flow,
                 Options.Groups.Warn));

   Options.Register (
      Option => Detect_Tail_Calls_Opt'access,
      Name   => "tail_calls",
      Groups => (Options.Groups.Analysis,
                 Options.Groups.Control_Flow));

end Flow.Calls.Opt;
