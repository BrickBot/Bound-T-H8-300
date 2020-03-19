-- Options.Devices_Valued (decl)
--
-- Options with values of type Devices.Device_Ref, picked by name
-- from the Devices.Catalog.
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
-- $Date: 2015/10/24 19:36:50 $
--
-- $Log: options-device_valued.ads,v $
-- Revision 1.2  2015/10/24 19:36:50  niklas
-- Moved to free licence.
--
-- Revision 1.1  2012-01-19 21:03:39  niklas
-- First version.
--


with Devices;
with Devices.Catalog;


package Options.Device_Valued is


   type Option_T (Default : Devices.Device_Ref)
   is new Options.Option_T
   with record
      Value : Devices.Device_Ref := Default;
   end record;
   --
   -- An option that has a default value and a current value,
   -- both of which are (references to) devices.
   --
   -- Options of this kind are enumerable, with the result being an
   -- enumeration of the items in Devices.Catalog.


   overriding
   function Type_And_Default (Option : access Option_T)
   return String;


   overriding
   procedure Reset (Option : access Option_T);
   --
   -- Option.Value := Option.Default.


   overriding
   procedure Set (
      Option : access Option_T;
      Value  : in     String);
   --
   -- Option.Value is set to the device named by the Value, if such a
   -- device is found in Devices.Catalog, else Constraint_Error is
   -- propagated.


   overriding
   function Enumerator (Option : Option_T)
   return Enumerator_T'Class;
   --
   -- Lists the items in Devices.Catalog.


   type Enum_T is new Enumerator_T with record
      Iter : Devices.Catalog.Iterator_T;
   end record;
   --
   -- Enumerates the items in Devices.Catalog.


   overriding
   function Current_Value (Enum : Enum_T) return String;


   overriding
   procedure Next (
      Enum  : in out Enum_T;
      Ended :    out Boolean);


end Options.Device_Valued;
