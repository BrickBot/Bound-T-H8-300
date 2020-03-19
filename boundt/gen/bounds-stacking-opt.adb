-- Bounds.Stacking.Opt (body)
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
-- $Date: 2015/10/24 20:05:46 $
--
-- $Log: bounds-stacking-opt.adb,v $
-- Revision 1.2  2015/10/24 20:05:46  niklas
-- Moved to free licence.
--
-- Revision 1.1  2012-02-13 17:51:49  niklas
-- First version, for BT-CH-0230.
--


with Arithmetic;
with Options;
with Options.Groups;
with Output;


package body Bounds.Stacking.Opt is


   procedure Ignore_Huge_Bounds (
      Limit : in out Programs.Execution.Stack_Limit_T)
   is
      use Storage.Bounds;
      use type Arithmetic.Value_T;
   begin

      if Known (Limit.Min)
      and then Over (abs Value (Limit.Min), Max_Abs_Height)
      then

         Output.Warning (
              "Large lower bound on stack height ignored as spurious"
            & Output.Field_Separator
            & Arithmetic.Image (Value (Limit.Min)));

         Limit.Min := Not_Limited;

      end if;

      if Known (Limit.Max)
      and then Over (abs Value (Limit.Max), Max_Abs_Height)
      then

         Output.Warning (
              "Large upper bound on stack height ignored as spurious"
            & Output.Field_Separator
            & Arithmetic.Image (Value (Limit.Max)));

         Limit.Max := Not_Limited;

      end if;

   end Ignore_Huge_Bounds;


begin  -- Bounds.Stacking.Opt

   Options.Register (
      Option => Max_Abs_Height_Opt'Access,
      Name   => "max_stack",
      Group  => Options.Groups.Stack_Usage);

end Bounds.Stacking.Opt;
