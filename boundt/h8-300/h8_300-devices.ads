-- H8_300.Devices (decl)
--
-- Implementation-specific attributes of various processors that implement
-- the Renesas H8/300 instruction set.
--
-- The implementation-specific attributes include:
--
-- > Names for On-Chip Registers (i/o and control registers) TBA.
--
-- > Memory size and memory address space map, specifically
--   -  whether a memory location is on-chip, external or reserved (absent)
--   -  whether an on-chip location is read-only (constant or read-write.
--   -  the number of read/write wait-states for an external location.
--
-- > Support for specific instructions. Some instructions may not be
--   supported on all implementations.
--
-- Some properties of a given implementation may be further defined by
-- command-line options. For example, the "Mode" of operation can influence
-- the address-space map.
--
-- The implementations are defined as a tagged-type hierarchy rooted at
-- an abstract device type. Implementation-specific attributes are defined
-- by overriding primitive operations of the standard implementation.
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
-- $Date: 2015/10/26 22:19:14 $
--
-- $Log: h8_300-devices.ads,v $
-- Revision 1.4  2015/10/26 22:19:14  niklas
-- Version 4b, and moved to free open-source licence.
--
-- Revision 1.3  2015/10/26 21:58:46  niklas
-- Updated to use current Options services and Options.Catalog.
--


with Devices;
with Devices.Catalog;
with H8_300;
with Interval_Maps;


package H8_300.Devices is


   --
   ---   Useful types
   --


   -- On-Chip Register Names TBA


   type Memory_Kind_T is record
      Location   : Location_T;
      Writable   : Boolean;
      Read_Wait  : Natural;
      Write_Wait : Natural;
   end record;
   --
   -- The properties of a memory address given in the address-space map.
   --
   -- Location
   --    The location where the address is mapped.
   --    The rest of the components are relevant only if the
   --    Location is not Reserved.
   -- Writable
   --    Whether the memory cell can be written (RAM, not ROM).
   -- Read_Wait
   --    The number of wait-states inserted for a read access.
   --    Relevant only for External memory.
   -- Write_Wait
   --    The number of wait-states inserted for a write access.
   --    Relevant only for External and Writable memory.


   function Image (Item : Memory_Kind_T) return String;
   --
   -- A readable description of the Item.


   Deallocate : Boolean := True;
   --
   -- Whether to use Unchecked_Deallocation to release unused
   -- heap memory.


   package Memory_Maps is new Interval_Maps (
      Index_Type => Address_T,
      Value_Type => Memory_Kind_T,
      Image      => Image,
      Deallocate => Deallocate);


   subtype Memory_Map_T is Memory_Maps.Interval_Map_T;
   --
   -- The memory address space map for an H8/300 device.


   --
   ---   Root class for devices
   --


   type Device_T is new Standard.Devices.Device_T
   with record
      Memory_Map  : Memory_Map_T;
   end record;
   --
   -- The root of the H8-300 device hierarchy.
   --
   -- Device_Name
   --    The device name, inherited from Devices.Device_T.
   -- Memory_Map
   --    The memory address space map for this device.
   --    All undefined addresses are considered Reserved.


   type Device_Ref is access all Device_T'Class;
   --
   -- A reference to a device object of some class.


   not overriding
   procedure Initialize (Device : in out Device_T)
   is null;
   --
   -- Initializes the Device. This is done once, after the Device has
   -- been identified in the Catalog (and possibly created as a side
   -- effect) and after all command-line options have been processed, 
   -- just before opening and loading the executable file to be analyzed.


   not overriding
   function Supports (
      Device      : Device_T;
      Instruction : Instruction_T)
   return Boolean;
   --
   -- Whether the Device supports (implements) the Instruction.
   --
   -- The default implementation supports all Instructions.


   not overriding
   function Memory_Kind (
      Address : Address_T;
      Device  : Device_T)
   return Memory_Kind_T;
   --
   -- The location and other properties for the given memory Address
   -- in the given Device.
   --
   -- The default implementation uses Device.Memory_Map.


   package Catalog renames Standard.Devices.Catalog;
   --
   -- We use the standard device catalog.


end H8_300.Devices;
