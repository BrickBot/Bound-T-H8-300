-- Verify
--
-- Verifying the decoding and disassembly of Renesas H8/300 instructions.
--
-- Author: Niklas Holsti
--
-- This file is a component of the Bound-T Worst-Case Execution Time Tool.
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
-- $Date: 2015/10/26 22:19:15 $
--
-- $Log: verify.adb,v $
-- Revision 1.4  2015/10/26 22:19:15  niklas
-- Version 4b, and moved to free open-source licence.
--
-- Revision 1.3  2005/10/24 13:45:50  niklas
-- Added command argument parsing.
--
-- Revision 1.2  2005/10/24 11:34:19  niklas
-- Check_All_Words if there are some command-line arguments.
--
-- Revision 1.1  2004/10/22 07:53:42  niklas
-- First version.
--


with Ada.Command_Line;
with Ada.Text_IO;

with H8_300.Verify;


procedure Verify
is

   use Ada.Text_IO;


   Repeats : Positive := 1;
   --
   -- The number of repetitions of Try_All, for speed measurement.


   Check_All_Words : Boolean := False;
   --
   -- Whether to check all instruction words.


   Good_Arguments : Boolean := True;
   --
   -- Whether all the command-line arguments are valid.


   procedure Use_Argument (Arg : in String)
   --
   -- Use a command-line argument.
   --
   is
   begin

      if Arg = "all" then
         -- We should check all the instruction words.

         Check_All_Words := True;

      else
         -- Assume its a repetition count.

         Repeats := Positive'Value (Arg);

      end if;

   exception

   when Constraint_Error =>

      Put_Line (Standard_Error,
         "Argument """ & Arg & """ not understood.");

      Good_Arguments := False;

   end Use_Argument;


begin

   for A in 1 .. Ada.Command_Line.Argument_Count loop

      Use_Argument (Ada.Command_Line.Argument (A));

   end loop;

   if Good_Arguments then

      for R in 1 .. Repeats loop

         Put_Line (Standard_Error, "Repetition" & Positive'Image (R));

         H8_300.Verify.Try_All (Check_All_Words => Check_All_Words);

      end loop;

   end if;

end Verify;
