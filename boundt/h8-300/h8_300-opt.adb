-- H8_300.Opt (body)
--
-- Author: Niklas Holsti
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
-- $Date: 2015/10/26 22:19:14 $
--
-- $Log: h8_300-opt.adb,v $
-- Revision 1.2  2015/10/26 22:19:14  niklas
-- Version 4b, and moved to free open-source licence.
--
-- Revision 1.1  2015/10/26 21:56:08  niklas
-- Added to use the current Options.Devices services.
--


with Devices;
with H8_300.Devices;
with Options;
with Options.Groups;
with Options.H8_300;
with Output;


package body H8_300.Opt is


   procedure Set_Device (
      Option : in Options.Option_Ref;
      Value  : in String)
   --
   -- Sets Device from Device_Opt.Value, checking that it is an
   -- H8/300 device.
   --
   is
      use type Standard.Devices.Device_Ref;
   begin

      Output.Note ("H8_300.Set_Device");

      if Device_Opt.Value = null
      or else Device_Opt.Value.all in H8_300.Devices.Device_T'Class
      then
         -- Ok.

         Device := H8_300.Devices.Device_Ref (Device_Opt.Value);

         Device.Initialize;

      else

         Output.Fault (
            Location => "H8_300.Opt.Set_Device",
            Text     => "Not an H8/300 device.");

         raise Constraint_Error;

      end if;

   end Set_Device;


begin  -- H8_300.Opt

   Options.Register (
      Option => Device_Opt'Access,
      Name   => Device_Option_Name,
      Group  => Options.H8_300.Group,
      Set    => Set_Device'Access);

end H8_300.Opt;
