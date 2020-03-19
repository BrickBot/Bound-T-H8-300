-- SWEET (body)
--
-- This is the dummy/null version of this package, for use when the
-- SWEET interface is omitted.
--
-- Author: Niklas Holsti, Tidorum Ltd
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
-- $Date: 2015/10/24 21:12:00 $
--
-- $Log: sweet.adb,v $
-- Revision 1.2  2015/10/24 21:12:00  niklas
-- Moved to free licence for the dummy options.
--
-- Revision 1.1  2013/12/23 21:17:39  niklas
-- First version, to implement Finish_Options.
--


with Ada.Text_IO;
with Options;
with Options.Bool;
with Options.Groups;


package body SWEET is


   Show_SWEET_Version_Opt : aliased Options.Bool.Option_T (Default => False);
   --
   -- Whether to display the version of the SWEET tool at the start
   -- of the run, if use of the SWEET tool is included in this
   -- version of Bound-T.
   --
   Show_SWEET_Version : Boolean renames Show_SWEET_Version_Opt.Value;


   procedure Finish_Options (Took_Action : in out Boolean)
   is
   begin

      if Show_SWEET_Version then

         Ada.Text_IO.Put_Line (
            "SWEET is not available in this version of Bound-T.");

         Took_Action := True;

      end if;

   end Finish_Options;


begin  -- SWEET

   Options.Register (
      Option => Show_SWEET_Version_Opt'access,
      Name   => "sweet_version",
      Group  => Options.Groups.Outputs);

end SWEET;
