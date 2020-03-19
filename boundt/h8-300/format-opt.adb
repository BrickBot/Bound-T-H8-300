-- Format.Opt (body)
--
-- Author: Niklas Holsti.
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
-- $Revision: 1.7 $
-- $Date: 2015/10/29 15:35:19 $
--
-- $Log: format-opt.adb,v $
-- Revision 1.7  2015/10/29 15:35:19  niklas
-- Added "-trace load" option.
--
-- Revision 1.6  2015/10/26 22:19:13  niklas
-- Version 4b, and moved to free open-source licence.
--
-- Revision 1.5  2015/10/26 22:01:04  niklas
-- Updated to use current Options services.
--
-- Revision 1.4  2007/05/03 07:38:38  niklas
-- Added Set_Trace_Option and List_Trace_Options, replacing the
-- options "-coff_trace" and "-ubrof_trace" by the target-specific
-- trace item "load", thus defining the option "-trace load".
--
-- Revision 1.3  2005/04/01 14:13:10  niklas
-- Added UBROF support.
--
-- Revision 1.2  2005/03/24 22:10:38  niklas
-- Added the option "-coff_trace".
--
-- Revision 1.1  2004/06/16 07:41:36  niklas
-- First version.
--


with Formats.COFF.Opt;
with Formats.In_Memory.Opt;
with Formats.Stabs.Opt;
with Formats.UBROF.Opt;
with Options;
with Options.Groups;
with Options.H8_300;


package body Format.Opt is


   procedure Handle_Wild_Option (
      Name  : in     String;
      Valid :    out Boolean)
   is
   begin

      -- Perhaps the name of an executable format?

      Form_Valued.Set (
         Option => Form_Opt'access,
         Value  => Name);
      -- Propagates Constraint_Error if the Name is
      -- not a valid format name.

      Valid := True;

   exception

   when Constraint_Error =>

      Valid := False;

   end Handle_Wild_Option;


   procedure Set_Trace_Option (
      Item  : in     String;
      Valid :    out Boolean)
   is
   begin

      Valid := True;
      -- Optimistic as ever...

      if Item = "load" then

         Formats.COFF.Opt.Trace_Loading  := True;
         Formats.UBROF.Opt.Trace_Loading := True;

      else

         Valid := False;

      end if;

   end Set_Trace_Option;


   procedure Set_No_Deallocation
   is
   begin

      Formats.In_Memory.Opt.Deallocate := False;
      -- Formats.Intel_Hex.Opt has no such option.
      Formats.Stabs.Opt.Deallocate     := False;
      Formats.UBROF.Opt.Deallocate     := False;

   end Set_No_Deallocation;


begin  -- Format.Opt

   Options.Register (
      Option => Form_Opt'Access,
      Name   => "format",
      Groups => (Options.Groups.Inputs,
                 Options.H8_300.Group));

   Options.Register (
      Option => Trace_Loading_Opt'access,
      Name   => Options.Trace_Item ("load"),
      Groups => (Options.Groups.Trace,
                 Options.H8_300.Group));


end Format.Opt;
