-- Flow.Const.Opt (body)
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
-- $Log: flow-const-opt.adb,v $
-- Revision 1.3  2015/10/24 20:05:48  niklas
-- Moved to free licence.
--
-- Revision 1.2  2013/12/20 21:09:41  niklas
-- Added Resolve_Opt (option -const_resolve), to control the use of
-- constant propagation for resolving dynamic flow and stack usage.
-- This was useful when testing SWEET for dynamic jump analysis.
--
-- Revision 1.1  2011-08-31 04:17:12  niklas
-- Added for BT-CH-0222: Option registry. Option -dump. External help files.
--


with Options;
with Options.Groups;


package body Flow.Const.Opt is


   Const_Trace : constant Options.Groups_T := (
      Options.Groups.Const_Prop,
      Options.Groups.Trace);

begin

   Options.Register (
      Option => Propagate_Opt'access,
      Name   => "const",
      Groups => (Options.Groups.Const_Prop,
                 Options.Groups.Analysis));

   Options.Register (
      Option => Relative_Values_Opt'access,
      Name   => Options.Imp_Item ("const_rel"),
      Groups => (Options.Groups.Const_Prop,
                 Options.Groups.Imp));

   Options.Register (
      Option => Trace_Iteration_Opt'access,
      Name   => Options.Trace_Item ("const_fixp"),
      Groups => Const_Trace);

   Options.Register (
      Option => Show_Results_Opt'access,
      Name   => Options.Trace_Item ("const"),
      Groups => Const_Trace);

   Options.Register (
      Option => Refine_Opt'access,
      Name   => "const_refine",
      Group  => Options.Groups.Const_Prop);

   Options.Register (
      Option => Trace_Refinements_Opt'access,
      Name   => Options.Trace_Item ("refine"),
      Groups => Const_Trace);

   Options.Register (
      Option => Resolve_Opt'access,
      Name   => "const_resolve",
      Groups => (Options.Groups.Const_Prop,
                 Options.Groups.Control_Flow));

   Options.Register (
      Option => Max_Pointer_Iterations_Opt'access,
      Name   => "const_iter",
      Groups => (Options.Groups.Const_Prop,
                 Options.Groups.Resource_Limits));

end Flow.Const.Opt;
