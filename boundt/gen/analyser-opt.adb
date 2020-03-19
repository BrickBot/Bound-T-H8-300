-- Analyser.Opt (body)
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
-- $Date: 2015/10/24 20:05:44 $
--
-- $Log: analyser-opt.adb,v $
-- Revision 1.3  2015/10/24 20:05:44  niklas
-- Moved to free licence.
--
-- Revision 1.2  2011-09-01 21:27:20  niklas
-- Replaced some Analysis group memberships with Control_Flow memberships.
--
-- Revision 1.1  2011-08-31 04:17:11  niklas
-- Added for BT-CH-0222: Option registry. Option -dump. External help files.
--


with Bounds.Opt;
with Flow.Calls.Opt;
with License;
with Options.Groups;
with Output;


package body Analyser.Opt is

   use type Options.Groups_T;


   function Trace_Unused_Subprograms return Boolean
   is
   begin

      return Flow.Calls.Opt.Trace_Unused_Calls;

   end Trace_Unused_Subprograms;


   overriding
   procedure Update (
      Option  : access View_Option_T;
      Literal : in     String;
      Member  : in     Boolean)
   is
      use type Programs.Execution.Show.View_T;
   begin

      if Literal = "full" then

         if Member then

            Option.Value := Option.Value
               or Programs.Execution.Show.Full_View;

         else

            Option.Value := Option.Value
               and not Programs.Execution.Show.Full_View;

         end if;

      else

         View_Valued.Update (
            Option  => View_Valued.Option_Ref (Option),
            Literal => Literal,
            Member  => Member);

      end if;

   end Update;


   Analysis_Trace : constant Options.Groups_T := (
      Options.Groups.Analysis,
      Options.Groups.Trace);
   --
   -- For "-trace" items related to Analysis.


   Flow_Trace : constant Options.Groups_T := (
      Options.Groups.Control_Flow,
      Options.Groups.Trace);
   --
   -- For "-trace" items related to control flow.


   procedure When_Stack_Path_Set (
      Option : in Options.Option_Ref;
      Value  : in String)
   --
   -- After setting "-stack_path".
   --
   is
   begin

      if Show_Stack_Path then

         Bounds.Opt.Set_Stack_Analysis (To => True);

      end if;

   end When_Stack_Path_Set;


begin  --  Analyser.Opt

   Options.Register (
      Option => Root_Alone_Opt'access,
      Name   => "alone",
      Group  => Options.Groups.Analysis);

   Options.Register (
      Option => Trace_Omitted_Subprograms_Opt'access,
      Name   => Options.Trace_Item ("omit"),
      Groups => Analysis_Trace);

   Options.Register (
      Option => Assume_Omit_Alone_Opt'access,
      Name   => "assume",
      Group  => Options.Groups.Analysis);

   Options.Register (
      Option => Max_Iterations_Opt'access,
      Name   => "flow_iter",
      Groups => (Options.Groups.Analysis,
                 Options.Groups.Control_Flow,
                 Options.Groups.Resource_Limits));

   Options.Register (
      Option => Show_Flow_Steps_Opt'access,
      Name   => Options.Trace_Item ("steps"),
      Groups => Flow_Trace);

   Options.Register (
      Option => Show_Flow_Nodes_Opt'access,
      Name   => Options.Trace_Item ("nodes"),
      Groups => Flow_Trace);

   Options.Register (
      Option => Show_Loops_Opt'access,
      Name   => Options.Trace_Item ("loops"),
      Groups => Flow_Trace
              & Options.Groups.Loops);

   Options.Register (
      Option => Warn_Eternal_Loop_Opt'access,
      Name   => Options.Warn_Item ("eternal"),
      Groups => (Options.Groups.Control_Flow,
                 Options.Groups.Warn));

   if License.Allows_Stack_Analysis then

      Options.Register (
         Option => Show_Stack_Path_Opt'access,
         Name   => "stack_path",
         Group  => Options.Groups.Analysis,
         Set    => When_Stack_Path_Set'Access);

   end if;

   Options.Register (
      Option => Tabulate_Time_Opt'access,
      Name   => "table",
      Groups => (Options.Groups.Analysis, Options.Groups.Outputs));

   Options.Register (
      Option => Show_View_Opt'access,
      Name   => "show",
      Group  => Options.Groups.Outputs);

end Analyser.Opt;
