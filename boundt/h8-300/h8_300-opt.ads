-- H8_300.Opt (decl)
--
-- Options for H8/300 processors.
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
-- $Log: h8_300-opt.ads,v $
-- Revision 1.2  2015/10/26 22:19:14  niklas
-- Version 4b, and moved to free open-source licence.
--
-- Revision 1.1  2015/10/26 21:56:08  niklas
-- Added to use the current Options.Devices services.
--


with H8_300.Devices;
with Options.Bool;
with Options.Device_Valued;


package H8_300.Opt is


   pragma Elaborate_Body;
   --
   -- To register the options here defined.


   Device_Option_Name : constant String := "device";
   --
   -- The name of the device-setting option.
   

   Device_Opt : aliased Options.Device_Valued.Option_T (Default => null);
   --
   -- The H8/300 device (chip) for which the target program is intended.
   -- This is the user-visible form of the option; see Device, below,
   -- for the internal form.
   --
   -- The device must be known in order to model the execution time.
   --
   -- The initial value is "no device". A default device may be
   -- substituted if no other device is chosen before the analysis
   -- starts.
   --
   Device : H8_300.Devices.Device_Ref := null;
   --
   -- Same as Device_Opt.Value, but validated as an H8/300
   -- device to avoid type conversions at points of use.
   --
   -- This is the internal form of the option. The user-visible form
   -- is Device_Opt.


end H8_300.Opt;
