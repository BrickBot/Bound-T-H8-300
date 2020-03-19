-- H8_300.Devices (body)
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
-- $Log: h8_300-devices.adb,v $
-- Revision 1.3  2015/10/26 22:19:14  niklas
-- Version 4b, and moved to free open-source licence.
--
-- Revision 1.2  2015/10/26 21:58:46  niklas
-- Updated to use current Options services and Options.Catalog.
--
-- Revision 1.1  2005/01/26 18:58:24  niklas
-- First version.
--


with Ada.Strings.Unbounded;
with Ada.Text_IO;
with H8_300.Devices.H8_3297;


package body H8_300.Devices is


   --
   ---   Useful types
   --


   function Image (Item : Memory_Kind_T) return String
   is
      use Ada.Strings.Unbounded;

      Text : Unbounded_String;
      -- The image to be built.

   begin

      Text := To_Unbounded_String (Location_T'Image (Item.Location));

      if Item.Location /= Reserved then

         if Item.Writable then

            Append (Text, ", read-write");

         else

            Append (Text, ", read-only");

         end if;

         if Item.Location = External then
            -- The wait-states may apply.

            Append (Text,
                 ','
               & Natural'Image (Item.Read_Wait)
               & " wait-states reading");

            if Item.Writable then

               Append (Text,
                    ','
                  & Natural'Image (Item.Write_Wait)
                  & " wait-states writing");

            end if;

         end if;

      end if;

      return To_String (Text);

   end Image;


   --
   ---   Root class for devices
   --


   function Supports (
      Device      : Device_T;
      Instruction : Instruction_T)
   return Boolean
   is
   begin

      return True;

   end Supports;


   function Memory_Kind (
      Address : Address_T;
      Device  : Device_T)
   return Memory_Kind_T
   is
   begin

      return Memory_Maps.Value (
         Map   => Device.Memory_Map,
         Index => Address);

   exception

   when Memory_Maps.Undefined_Value =>

      return (
         Location   => Reserved,
         Writable   => False,
         Read_Wait  => 0,
         Write_Wait => 0);

   end Memory_Kind;


   procedure Set_Option (
      Argument : in     String;
      Device   : in out Device_T;
      Valid    :    out Boolean)
   is
   begin

      Valid := False;

   end Set_Option;


end H8_300.Devices;
