-- Assertions.Opt (body)
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
-- $Date: 2015/10/24 20:05:45 $
--
-- $Log: assertions-opt.adb,v $
-- Revision 1.5  2015/10/24 20:05:45  niklas
-- Moved to free licence.
--
-- Revision 1.4  2011-08-31 04:23:33  niklas
-- BT-CH-0222: Option registry. Option -dump. External help files.
--
-- Revision 1.3  2009/03/27 13:57:11  niklas
-- BT-CH-0167: Assertion context identified by source-code markers.
--
-- Revision 1.2  2006/05/27 21:48:45  niklas
-- BT-CH-0020.
--
-- Revision 1.1  2005/06/12 07:20:29  niklas
-- Added procedure Take_File and package body.
--


with Options;
with Options.Groups;


package body Assertions.Opt is


   Assertion_Trace : constant Options.Groups_T := (
      Options.Groups.Trace, Options.Groups.Assertions);
   --
   -- The assertion options that control tracing.


   Assertion_Warn : constant Options.Groups_T := (
      Options.Groups.Warn, Options.Groups.Assertions);
   --
   -- The assertion options that control warnings.


begin  -- Assertions.Opt

   Options.Register (
      Option => Assertion_Files'access,
      Name   => "assert",
      Groups => (Options.Groups.Inputs, Options.Groups.Assertions));

   Options.Register (
      Option => Mark_Files'access,
      Name   => "mark",
      Groups => (Options.Groups.Inputs, Options.Groups.Assertions));

   Options.Register (
      Option => Warn_Absent_Subprogram_Opt'access,
      Name   => Options.Warn_Item ("sub_miss"),
      Groups => Assertion_Warn);

   Options.Register (
      Option => Warn_Unused_Role_Opt'access,
      Name   => Options.Warn_Item ("role"),
      Groups => Assertion_Warn);

   Options.Register (
      Option => Implicit_Features_Opt'access,
      Name   => "implicit",
      Group  => Options.Groups.Assertions);

   Options.Register (
      Option => Line_Fuzz_Opt'access,
      Name   => "line_fuzz",
      Group  => Options.Groups.Assertions);

   Options.Register (
      Option => File_Matching_Opt'access,
      Name   => "file_match",
      Group  => Options.Groups.Assertions);

   Options.Register (
      Option => Warn_File_Matching_Opt'access,
      Name   => Options.Warn_Item ("file_match"),
      Groups => Assertion_Warn);

   Options.Register (
      Option => File_Casing_Opt'access,
      Name   => "file_case",
      Group  => Options.Groups.Assertions);

   Options.Register (
      Option => Warn_File_Casing_Opt'access,
      Name   => Options.Warn_Item ("file_case"),
      Groups => Assertion_Warn);

   Options.Register (
      Option => Marked_Relation_Opt'access,
      Name   => "mark_relation",
      Group  => Options.Groups.Assertions);

   Options.Register (
      Option => Trace_Parsing_Opt'access,
      Name   => Options.Trace_Item ("parse"),
      Groups => Assertion_Trace);

   Options.Register (
      Option => Trace_Marks_Opt'access,
      Name   => Options.Trace_Item ("marks"),
      Groups => Assertion_Trace);

   Options.Register (
      Option => Trace_Sub_Options_Opt'access,
      Name   => Options.Trace_Item ("subopt"),
      Groups => Assertion_Trace);

   Options.Register (
      Option => Trace_Matching_Opt'access,
      Name   => Options.Trace_Item ("match"),
      Groups => Assertion_Trace);

   Options.Register (
      Option => Trace_To_Be_Mapped_Opt'access,
      Name   => Options.Trace_Item ("to_map"),
      Groups => Assertion_Trace);

   Options.Register (
      Option => Trace_Map_Opt'access,
      Name   => Options.Trace_Item ("map"),
      Groups => Assertion_Trace);

end Assertions.Opt;
