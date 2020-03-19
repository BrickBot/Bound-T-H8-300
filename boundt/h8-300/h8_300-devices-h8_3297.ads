-- H8_300.Devices.H8_3297 (decl)
--
-- Implementation-specific attributes of the H8/3297 series of the
-- Renesas H8/300 family.
--
-- This series consists of the devices H8/3292, H8/3294, H8/3296 and
-- H8/3297, in various packages.
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
-- $Revision: 1.3 $
-- $Date: 2015/10/26 22:19:14 $
--
-- $Log: h8_300-devices-h8_3297.ads,v $
-- Revision 1.3  2015/10/26 22:19:14  niklas
-- Version 4b, and moved to free open-source licence.
--
-- Revision 1.2  2015/10/26 21:58:46  niklas
-- Updated to use current Options services and Options.Catalog.
--
-- Revision 1.1  2005/01/26 19:23:26  niklas
-- First version.
--


package H8_300.Devices.H8_3297 is


   type Part_T is (
      H8_3292,
      H8_3294,
      H8_3296,
      H8_3297);
   --
   -- The specific part, in the H8/3297 Series.
   --
   -- The difference between the parts is the amount of on-chip memory
   -- and consequently the memory address-space map.


   type Mode_T is range 1 .. 3;
   --
   -- The processor operating mode, which determines the memory address
   -- space map.
   --
   -- The operating mode is set by two input pins (read-only) and is
   -- assumed to be constant throughout the execution (or at least the
   -- part of the execution that we analyze).


   type Device_T is new Devices.Device_T
   with record
      Part : Part_T;
      Mode : Mode_T;
      RAME : Boolean;
   end record;
   --
   -- A device in the H8/3297 series.
   --
   -- Part
   --    The specific part in this series.
   -- Mode
   --    The assumed operating mode of the processor.
   -- RAME
   --    The assumed value of the SYSCR bit RAME = internal RAM enabled.


   overriding
   procedure Initialize (Device : in out Device_T);
   --
   -- Initializes the Device, assuming that this Device was found
   -- in the Catalog (and possibly created as a side effect), and that
   -- all device-specific command-line options have been processed.


   overriding
   function Supports (
      Device      : Device_T;
      Instruction : Instruction_T)
   return Boolean;
   --
   -- The H8/32927 series supports all instructions except MOVFPE and MOVTPE.


end H8_300.Devices.H8_3297;
