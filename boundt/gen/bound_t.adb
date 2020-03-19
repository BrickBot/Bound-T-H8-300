-- Bound_T (body)
--
-- Authors: Space Systems Finland, 1999:
--     Niklas Holsti
--     Thomas Långbacka
--     Sami Saarinen
--     Ville Sipinen
--     Mikko Ala-Fossi
--     Eemeli Kuumola
--     Satu Sihvo
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
-- $Revision: 1.31 $
-- $Date: 2015/10/25 21:29:59 $
--
-- $Log: bound_t.adb,v $
-- Revision 1.31  2015/10/25 21:29:59  niklas
-- Moved option-parsing from Analysis to Main, because -max_anatime is needed in Main.
--
-- Revision 1.30  2015/10/24 20:05:45  niklas
-- Moved to free licence.
--
-- Revision 1.29  2013/12/23 09:47:56  niklas
-- Added call to SWEET.Finish_Options.
--
-- Revision 1.28  2012-01-19 19:43:29  niklas
-- BT-CH-0223: Package License does not depend on package Flow.
--
-- Revision 1.27  2011-09-01 22:13:51  niklas
-- The options -version, -host_version, -licence set Took_Action
-- and thus the absence of a target program does not cause an Error.
-- The final action of -synonym is omitted if the Program is not
-- initialized (does not Exist).
--
-- Revision 1.26  2011-08-31 04:23:33  niklas
-- BT-CH-0222: Option registry. Option -dump. External help files.
--
-- Revision 1.25  2009-12-27 22:34:31  niklas
-- BT-CH-0205: Licensing based on code size and time/space dimensions.
--
-- Revision 1.24  2009-05-21 08:08:27  niklas
-- BT-CH-0175: Limits on the number of Warnings, Errors, Faults.
--
-- Revision 1.23  2009/03/27 13:57:13  niklas
-- BT-CH-0167: Assertion context identified by source-code markers.
--
-- Revision 1.22  2008/12/25 08:31:53  niklas
-- Removed unnecessary context clauses.
--
-- Revision 1.21  2008/11/21 13:57:19  niklas
-- BT-CH-0162: Use package Tasking to hide tasking statements.
--
-- Revision 1.20  2008/10/11 08:16:13  niklas
-- BT-CH-0148: Symbols from text files and the -symbols option.
--
-- Revision 1.19  2007/12/17 13:54:34  niklas
-- BT-CH-0098: Assertions on stack usage and final stack height, etc.
--
-- Revision 1.18  2007/08/14 12:36:38  niklas
-- BT-CH-0072: Corrections to handling ambiguous names.
--
-- Revision 1.17  2007/07/27 20:24:19  niklas
-- BT-CH-0068. Option -synonyms.
--
-- Revision 1.16  2007/06/25 19:55:06  niklas
-- Moved the "delay" work-around from Analyse to Analysis; it works
-- better here (perhaps because exceptions are handled in Analyse) and
-- the delay is not included in the measured analysis time.
--
-- Revision 1.15  2007/06/24 12:11:29  niklas
-- Enabled max_anatime with work-around (delay).
--
-- Revision 1.14  2007/06/06 19:51:23  niklas
-- Removed exception handler for Bounds.Recursion from the Analysis
-- procedure because this exception is now trapped in Analyser.
--
-- Revision 1.13  2007/03/27 11:31:00  niklas
-- Disabled bound on analysis time because it made Bound-T on
-- Linux sometimes hang before exiting.
--
-- Revision 1.12  2007/03/16 07:12:32  niklas
-- Extended Analysis to make the analysis-time limit apply only when
-- the limit has been set (to something else than Time_Unlimited).
-- Combined declaration and body of Get_Roots.
--
-- Revision 1.11  2007/03/01 13:40:15  niklas
-- Modified the check for license and the analysis time-out,
-- trying to stop the program hanging instead of terminating.
-- Also eliminated the embedded return statements in Analyse.
--
-- Revision 1.10  2007/03/01 12:30:50  niklas
-- Implemented Opt.Max_Analysis_Time.
--
-- Revision 1.9  2006/11/03 06:33:52  niklas
-- BT-CH-0035.
--
-- Revision 1.8  2006/08/22 13:50:48  niklas
-- BT-CH-0025.
--
-- Revision 1.7  2006/05/27 21:51:17  niklas
-- Updated for BT-CH-0020.
--
-- Revision 1.6  2006/05/17 20:10:31  niklas
-- Removed use of RapiTime in favour of RapiTime.Target and
-- other target-specific things such as Decoder.Post_Analysis.
--
-- Revision 1.5  2006/04/21 20:33:16  niklas
-- Extended procedure Main to catch Decoder_Error silently,
-- assuming that the problem has been reported earlier.
--
-- Revision 1.4  2006/03/25 13:26:08  niklas
-- Added RapiTime and procedure Post_Analysis.
--
-- Revision 1.3  2005/10/09 08:10:20  niklas
-- BT-CH-0013.
--
-- Revision 1.2  2005/09/05 11:23:37  niklas
-- BT-CH-0007.
--
-- Revision 1.1  2005/08/08 13:58:27  niklas
-- First version.
--


with Analyser;
with Assertions;
with Bounds.Opt;
with Bound_T.Opt;
with Bound_T.Opt.Deallocation;
with Decoder;
with Host_Version;
with Hrt.Skeleton;
with License;
with Options.Command;
with Output;
with Programs;
with Programs.Synonyms;
with SWEET;
with Symbols;
with Symbols.Text;
with Tasking;
with Version_Id;

with Ada.Calendar;
with Ada.Command_Line;
with Ada.Float_Text_IO;
with Ada.Strings.Fixed;
with Ada.Text_IO;


package body Bound_T is


   procedure Get_Roots (
      Program : in out Programs.Program_T;
      First   : in     Positive)
   --
   -- Gets the root subprogram names from the rest of the command-line
   -- arguments, starting at argument number First, and puts them into
   -- the program object.
   --
   is
      use Ada.Command_Line;

      Roots : Programs.Subprogram_Set_T;
      -- The root subprograms defined so far.
      -- Just to check if a subprogram is mentioned twice.

      Root_Call : Programs.Call_T;
      -- The virtual call constructed for a root subprogram.
      -- Unused output from Programs.Add_Root.

   begin

      Programs.Erase (Roots);

      for R in First .. Argument_Count loop

         declare

            Name : constant String := Argument(R);
            -- The name (identifier) of the subprogram.

            Sub : Programs.Subprogram_T;
            -- The subprogram, if found.

         begin

            Programs.Identify (
               Identifier => Name,
               Delimiter  => Symbols.Default_Delimiter,
               Program    => Program,
               Subprogram => Sub);

            -- We found it.

            Output.Note (
               Locus => Programs.Locus (Sub),
               Text  => "Root subprogram");

            if Programs.Is_Member (Sub, Roots) then

               Output.Note (
                  Locus => Programs.Locus (Sub),
                  Text  => "Subprogram already mentioned");

            else

               Programs.Add_Root (
                  Root => Sub,
                  To   => Program,
                  Call => Root_Call);

               Programs.Add (To => Roots, Adding => Sub);

            end if;

         exception

         when Symbols.Ambiguous_Name =>

            Output.Error (
               Locus => Output.Locus (Call_Path => Name),
               Text  => "Root subprogram name is ambiguous.");

         when Programs.Subprogram_Not_Found =>

            Output.Error (
               Locus => Output.Locus (Call_Path => Name),
               Text  =>
                  "Root subprogram not found or address in wrong form.");

         end;

      end loop;

   end Get_Roots;


   function Image (Item : Duration) return String
   --
   -- The duration with three decimals, no leading space.
   --
   is
      use Ada.Strings, Ada.Strings.Fixed;

      Image : String (1 .. 20);
      -- The duration, formatted.

   begin

      Ada.Float_Text_IO.Put (
         To   => Image,
         Item => Float (Item),
         Aft  => 3,
         Exp  => 0);

      return Trim (Image, Both);

   end Image;


   Took_Action : Boolean;
   --
   -- Whether the options led to some positive action even
   -- without a target program file-name.
   -- Set in procedure Main, used in procedure Analysis.


   Exe_Arg : Positive;
   --
   -- The number of the command-line argument that states
   -- the name of the target program executable file.
   -- Set in procedure Main, used in procedure Analysis.


   procedure Analysis
   --
   -- The analysis itself and all of it.
   --
   -- Precondition: User has a valid license.
   --
   -- Leaves the default Output locus set to show the
   -- target program executable.
   --
   is

      Roots_Given : Boolean;
      -- Whether some root subprograms are given as arguments.

      Mark : Output.Nest_Mark_T;
      -- Marks the default Output locus for the program file.

   begin  -- Analysis

      -- Show versions if requested:

      if Opt.Show_Version then

         Ada.Text_IO.Put_Line (Version_Id);

         Took_Action := True;

      end if;

      if Opt.Show_Host_Version then

         Ada.Text_IO.Put_Line ("This Bound-T was compiled on:");
         Ada.Text_IO.Put_Line ("Host version : " & Host_Version.Summary);
         Ada.Text_IO.Put_Line ("Host system  : " & Host_Version.System );
         Ada.Text_IO.Put_Line ("GNAT version : " & Host_Version.GNAT   );

         Took_Action := True;

      end if;

      if Opt.Show_Licence then

         Ada.Text_IO.Put_Line (License.Image);

         Took_Action := True;

      end if;

      SWEET.Finish_Options (Took_Action);

      -- Control heap deallocation:

      Opt.Deallocation (Allow => Opt.Deallocate);

      -- Check option consistency:

      if Bound_T.Opt.HRT
      and not Bounds.Opt.Bound_Time
      then

         Output.Error (
            "Option conflict: HRT analysis requires time bounds.");

      end if;

      -- Check for the presence and need for a program-file argument:

      if Exe_Arg > Ada.Command_Line.Argument_Count then
         -- There is no target-program file-name argument.

         if not Took_Action then
            -- This seems to be a mistake; the command has no effect.

            Output.Error ("Target-program file-name not specified.");

            Options.Command.Advise_Help;

         end if;

         -- Anyway we have nothing more to do.

      else
          -- We have a target program file-name to process.

          -- The arguments following the target program file-name are not
          -- yet handled above; their meaning depends on whether the
          -- HRT option is used.

          Mark := Output.Nest (Output.Locus (
                  Program_File => Ada.Command_Line.Argument(Exe_Arg)));

          Output.Note (Text => Version_Id);

          Roots_Given := Exe_Arg < Ada.Command_Line.Argument_Count;

         if Opt.Dump_Program then
            -- Dump the target program file. No analysis.

            Ada.Text_IO.New_Line;
            Ada.Text_IO.Put_Line (
                 "Dumping the file """
               & Ada.Command_Line.Argument (Exe_Arg)
               & """:");
            Ada.Text_IO.New_Line;

            Decoder.Dump_Program (
               File_Name => Ada.Command_Line.Argument (Exe_Arg));

            if Opt.HRT or Roots_Given then

               Output.Warning ("Option -dump disables analysis.");

            end if;

         else

            if Opt.HRT or Roots_Given then

               -- Read the target program and initialise decoding:

               Programs.Initialize (
                  From_File => Ada.Command_Line.Argument (Exe_Arg),
                  Program   => Program);

               Symbols.Text.Read_From_Files (
                  Symbol_Table => Programs.Symbol_Table (Program));

               Assertions.Get_Marks;

               Assertions.Get_Assertions (
                  Program       => Program,
                  Assertion_Set => Asserts);

               Assertions.Apply_Options (
                  Assertion_Set => Asserts,
                  Program       => Program);

            end if;

            if Opt.HRT then

               -- Generate the HRT Execution Skeleton and WCETs:

               if Roots_Given then
                  -- TPOF name given in command line.

                  HRT.Skeleton.Generate (
                    TPOF_Name  => Ada.Command_Line.Argument (Exe_Arg + 1),
                    Program    => Program,
                    Asserts    => Asserts,
                    Bounds_Set => Bounds_Set);

               else

                  Output.Error (Text => "No TPOF name given.");

                  raise Options.Command.Argument_Error;

               end if;

            elsif Roots_Given then

               -- Just estimate the WCETs:

               Get_Roots (
                  Program => Program,
                  First   => Exe_Arg + 1);

               Analyser.Analyse (
                  Program    => Program,
                  Asserts    => Asserts,
                  Bounds_Set => Bounds_Set);

            else
               -- No HRT, no root subprograms: nothing to do.

               Output.Warning ("No root subprograms defined.");

            end if;

         end if;

      end if;

   exception

   when Options.Command.Argument_Error =>
      -- Some command-line option/argument is in error.
      -- Analysis is aborted.

      null;

   when Decoder.Decoder_Error =>
      -- Some unrecoverable problem in loading or decoding
      -- the executable target programs.

      null;

   when Assertions.Input_Error =>
      -- Some assertion file or mark-definition file is in error.
      -- Analysis is aborted.

      null;

   when Output.Too_Many_Problems =>

      -- Too many problem messages emitted.
      -- Analysis is aborted.

      raise;

   when X : others =>

      Output.Exception_Info (
         Text       => "Bound_T.Main",
         Occurrence => X);

   end Analysis;


   procedure Main
   is
      use Ada.Calendar;

      Starting_Time : constant Time := Clock;
      -- The wall-clock time now, at the start of the analysis.

      Analysis_Duration : Duration;
      -- The total duration of the analysis.

      Aborted : Boolean;
      -- Whether analysis was aborted because it exceeded the
      -- (optional) maximum time.

   begin

      Options.Finish_Definition;

      -- Scan all options (arguments beginning with '-') and parse
      -- them, storing the values in various options packages:

      Options.Command.Get_From_Command (
         Took_Action   => Took_Action,
         Next_Argument => Exe_Arg);

      if License.Valid_Date (Measure => 0) then

         -- Analyse, possibly with a time limit:

         if Opt.Max_Analysis_Time = Opt.Time_Unlimited then
            -- Analyse, however long it takes:

            Analysis;

         else
            -- Analyse, but abort if it is taking too long:

            Output.Note (
                 "Maximum analysis time "
               & Image (Opt.Max_Analysis_Time));

            Tasking.Do_With_Timeout (
               Action  => Analysis'Access,
               Timeout => Opt.Max_Analysis_Time,
               Aborted => Aborted);

            if Aborted then

               Output.Error ("Maximum analysis time exceeded.");

            end if;

         end if;

         if Opt.Time_Analysis then

            Analysis_Duration := Clock - Starting_Time;

            Output.Result (
               Key  => "Analysis_Time",
               Text => Image (Analysis_Duration));

         end if;

         if  Opt.List_Synonyms
         and Programs.Exists (Program)
         then

            Programs.Synonyms.Show (Program);

         end if;

         Tasking.Do_Delay (0.1);
         -- Work-around for a probable bug in Gnat 3.15p where the
         -- mere presence of a select-delay-then-abort construct, as
         -- normally used to implement Do_With_Timeout, seems to make
         -- the program (sometimes) hang indefinitely somewhere in the
         -- termination/finalization phase.

      end if;

   exception

   when Output.Too_Many_Problems =>

      -- The maximum number of problem messages was exceeded in
      -- the post-Analysis phase (eg. Programs.Synonyms.Show).

      null;

   end Main;


end Bound_T;
