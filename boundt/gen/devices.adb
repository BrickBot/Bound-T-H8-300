-- Devices (body)
--
-- A component of the Bound-T Timing Analysis Tool.
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
-- $Date: 2015/10/24 20:05:47 $
--
-- $Log: devices.adb,v $
-- Revision 1.3  2015/10/24 20:05:47  niklas
-- Moved to free licence.
--
-- Revision 1.2  2012-01-19 20:13:56  niklas
-- BT-CH-0224: Device.Catalog added. Device options updated.
--
-- Revision 1.1  2008-06-11 08:18:15  niklas
-- BT-CH-0129: Devices and the -device option.
--


package body Devices is


   function Name (Device : Device_T) return String
   is
   begin

      if Device.Privates.Name /= null then

         return Device.Privates.Name.all;

      else

         return "(not defined)";

      end if;

   end Name;


   procedure Set_Name (
      Device : in out Device_T'Class;
      To     : in     String)
   is
   begin

      -- TBA deallocate the old name (if last ref).

      Device.Privates.Name := new String'(To);

   end Set_Name;


end Devices;
