-- Flow.Origins.Opt (body)
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
-- $Date: 2015/10/24 20:05:48 $
--
-- $Log: flow-origins-opt.adb,v $
-- Revision 1.4  2015/10/24 20:05:48  niklas
-- Moved to free licence.
--
-- Revision 1.3  2014/06/01 10:30:00  niklas
-- Added Trace_Flag_Origins option.
--
-- Revision 1.2  2013-02-12 08:47:19  niklas
-- BT-CH-0245: Global volatile cells and "volatile" assertions.
--
-- Revision 1.1  2011-08-31 04:17:12  niklas
-- Added for BT-CH-0222: Option registry. Option -dump. External help files.
--


with Options;
with Options.Groups;


package body Flow.Origins.Opt is


   Origs : constant Options.Group_Name_T := Options.Group ("value_origin");
   --
   -- The options related to value-origin analysis.


   Origs_Trace : constant Options.Groups_T := (
      Origs,
      Options.Groups.Trace);
   --
   -- The tracing options related to value-origin analysis.


begin

   Options.Register (
      Option => Propagate_Opt'access,
      Name   => "orig",
      Groups => (Options.Groups.Analysis,
                 Origs));

   Options.Register (
      Option => Trace_Iteration_Opt'access,
      Name   => Options.Trace_Item ("orig_fixp"),
      Groups => Origs_Trace);

   Options.Register (
      Option => Trace_Volatile_Opt'access,
      Name   => Options.Trace_Item ("orig_volatile"),
      Groups => Origs_Trace);

   Options.Register (
      Option => Show_Results_Opt'access,
      Name   => Options.Trace_Item ("orig"),
      Groups => Origs_Trace);

   Options.Register (
      Option => Trace_Invariant_Cells_Opt'access,
      Name   => Options.Trace_Item ("orig_inv"),
      Groups => Origs_Trace);

   Options.Register (
      Option => Trace_Flag_Origins_Opt'access,
      Name   => Options.Trace_Item ("flag_origins"),
      Groups => Origs_Trace);

end Flow.Origins.Opt;
