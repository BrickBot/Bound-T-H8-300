-- Symbols.Opt (body)
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
-- $Date: 2015/10/24 20:05:52 $
--
-- $Log: symbols-opt.adb,v $
-- Revision 1.3  2015/10/24 20:05:52  niklas
-- Moved to free licence.
--
-- Revision 1.2  2011-08-31 04:23:34  niklas
-- BT-CH-0222: Option registry. Option -dump. External help files.
--
-- Revision 1.1  2008/10/11 08:16:14  niklas
-- BT-CH-0148: Symbols from text files and the -symbols option.
--


with Options;
with Options.Groups;


package body Symbols.Opt is

begin  -- Symbols.Opt

   Options.Register (
      Option => Symbol_File_Names_Opt'Access,
      Name   => "symbols",
      Group   => Options.Groups.Inputs);

   Options.Register (
      Option => Trace_Opt'Access,
      Name   => Options.Trace_Item ("symbols"),
      Group   => Options.Groups.Trace);

   Options.Register (
      Option => Trace_Scopes_Opt'Access,
      Name   => Options.Trace_Item ("scopes"),
      Group   => Options.Groups.Trace);

   Options.Register (
      Option => Trace_Insertion_Opt'Access,
      Name   => Options.Trace_Item ("symins"),
      Group   => Options.Groups.Trace);

   Options.Register (
      Option => Warn_Duplicated_Symbol_Opt'Access,
      Name   => Options.Warn_Item ("symbol"),
      Group   => Options.Groups.Warn);

   Options.Register (
      Option => Warn_Shallow_Line_Scope_Opt'Access,
      Name   => Options.Warn_Item ("line_scope"),
      Group   => Options.Groups.Warn);

end Symbols.Opt;
