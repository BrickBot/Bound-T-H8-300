-- Decoder.IAR.Opt (body)
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
-- $Date: 2015/10/26 22:19:13 $
--
-- $Log: decoder-iar-opt.adb,v $
-- Revision 1.3  2015/10/26 22:19:13  niklas
-- Version 4b, and moved to free open-source licence.
--
-- Revision 1.2  2015/10/26 22:01:04  niklas
-- Updated to use current Options services.
--
-- Revision 1.1  2005/05/09 15:56:14  niklas
-- First version.
--


with Options;
with Options.Groups;
with Options.H8_300;


package body Decoder.IAR.Opt is


   procedure Handle_Wild_Option (
      Name  : in     String;
      Valid :    out Boolean)
   is

      F : constant Positive := Name'First;
      L : constant Natural  := Name'Last;
      -- The index range of the Name string.

   begin

      if Name'Length /= 4 then

        Valid := False;

      elsif Name(F .. F + 2) = "-ur"
        and Name(L) in '0' .. '3'
      then
         -- Possibly ok.

         Param_Regs_Opt.Set (Value  => Name(L .. L));

      elsif Name(F .. F + 2) = "-uu"
        and Name(L) in '0' .. '4'
      then
         -- Possibly ok:

         Trash_Regs_Opt.Set (Value => Name(L .. L));

      else
         -- Nope. Stranger.

         Valid := False;

      end if;

   exception

   when Constraint_Error =>

      Valid := False;

   end Handle_Wild_Option;


   procedure Finish
   is
   begin

      null;
      --
      -- Nothing to check at this point -- we don't yet know if
      -- the program is in UBROF or which compiler was used.

   end Finish;


   IAR_Group : constant Options.Group_Name_T := Options.Group ("iar");
   --
   -- Options for analysing IAR-compiled H8/300 programs.


begin  -- Decoder.IAR.Opt

   Options.Set_Group_Priority (
      Higher => Options.H8_300.Group,
      Lower  => IAR_Group);

   Options.Register (
      Option => Param_Regs_Opt'Access,
      Name   => "ur",
      Groups => (Options.H8_300.Group, IAR_Group));

   Options.Register (
      Option => Param_Regs_Opt'Access,
      Name   => "uu",
      Groups => (Options.H8_300.Group, IAR_Group));

end Decoder.IAR.Opt;
