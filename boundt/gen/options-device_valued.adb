-- Options.Devices_Valued (body)
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
-- $Date: 2015/10/24 20:05:50 $
--
-- $Log: options-device_valued.adb,v $
-- Revision 1.2  2015/10/24 20:05:50  niklas
-- Moved to free licence.
--
-- Revision 1.1  2012-01-19 21:03:39  niklas
-- First version.
--


package body Options.Device_Valued is


   use type Devices.Device_Ref;
   use type Devices.Catalog.Item_Ref;


   overriding
   function Type_And_Default (Option : access Option_T)
   return String
   is
   begin

      if Option.Default /= null then

         return "Device name, default "
            & Devices.Name (Option.Default.all)
            & '.';

      else

         return "Device name, no default";

      end if;

   end Type_And_Default;


   overriding
   procedure Reset (Option : access Option_T)
   is
   begin

      Option.Value := Option.Default;

   end Reset;


   overriding
   procedure Set (
      Option : access Option_T;
      Value  : in     String)
   is

      Device : constant Devices.Device_Ref :=
         Devices.Catalog.Device_By (Name => Value);
      -- The device named by the Value, or null if the Catalog
      -- knows of no such device.

   begin

      if Device = null then

         raise Constraint_Error;

      end if;

      Option.Value := Device;

   end Set;


   overriding
   function Enumerator (Option : Option_T)
   return Enumerator_T'Class
   is
   begin

      return Enumerator_T'Class (Enum_T'(Enumerator_T with Iter => <>));

   end Enumerator;


   overriding
   function Current_Value (Enum : Enum_T) return String
   is

      Item : constant Devices.Catalog.Item_Ref :=
         Devices.Catalog.Item (Enum.Iter);
      -- The current catalog item in the iteration (enumeration)
      -- of the device catalog.

   begin

      if Item /= null then

         return Devices.Catalog.Name_Of (Item.all);

      else

         return "(no devices available)";

      end if;

   end Current_Value;


   overriding
   procedure Next (
      Enum  : in out Enum_T;
      Ended :    out Boolean)
   is
   begin

      Devices.Catalog.Next (Enum.Iter);

      Ended := Devices.Catalog.Item (Enum.Iter) = null;

   end Next;


end Options.Device_Valued;
