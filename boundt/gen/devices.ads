-- Devices (decl)
--
-- Support for selecting the processor device (chip, target system)
-- on which the target program is meant to run.
--
-- The implementations are defined as a tagged-type hierarchy rooted at
-- an abstract device type. Implementation-specific attributes are defined
-- by overriding primitive operations of the standard implementation.
--
-- The general attributes and properties of the Device_T are:
--
-- > A name, used (on the general level) purely for documentation.
--
-- An application of Bound-T to a particular target processor family
-- can use these services, or can ignore them, perhaps substituting
-- its own design of a device hierarchy.
--
-- The child package Devices.Catalog can also be used, or not. It helps
-- to define mappings from device names to specific devices; these
-- device objects may already exist, or they may be created on the fly
-- depending on the device names given.
--
-- Author: Niklas Holsti, Tidorum Ltd.
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
-- $Revision: 1.4 $
-- $Date: 2015/10/24 19:36:48 $
--
-- $Log: devices.ads,v $
-- Revision 1.4  2015/10/24 19:36:48  niklas
-- Moved to free licence.
--
-- Revision 1.3  2014/06/01 10:29:23  niklas
-- Made Device_T limited so that derived types can add
-- limited components.
--
-- Revision 1.2  2012-01-19 20:13:56  niklas
-- BT-CH-0224: Device.Catalog added. Device options updated.
--
-- Revision 1.1  2008-06-11 08:18:15  niklas
-- BT-CH-0129: Devices and the -device option.
--


package Devices is


   type Privates_T is private;
   --
   -- The private parts of the root class.


   type Device_T is abstract tagged limited record
      Privates : Privates_T;
   end record;
   --
   -- The root of the device hierarchy.


   type Device_Ref is access all Device_T'Class;
   --
   -- A reference to a device of some class.


   function Name (Device : Device_T) return String;
   --
   -- The name of the device (implementation, model).
   -- The default implementation returns the name that
   -- was given in Set_Name (see below).


   procedure Set_Name (
      Device : in out Device_T'Class;
      To     : in     String);
   --
   -- Changes the name of the Device, To the given string.


private


   type Name_Ref is access String;
   --
   -- A heap-allocated string.


   type Privates_T is record
      Name : Name_Ref;
   end record;


end Devices;
