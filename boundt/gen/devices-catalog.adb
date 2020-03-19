-- Devices.Catalog (body)
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
-- $Revision: 1.2 $
-- $Date: 2015/10/24 20:05:47 $
--
-- $Log: devices-catalog.adb,v $
-- Revision 1.2  2015/10/24 20:05:47  niklas
-- Moved to free licence.
--
-- Revision 1.1  2012-01-19 20:13:56  niklas
-- BT-CH-0224: Device.Catalog added. Device options updated.
--


with Ada.Characters.Handling;
with Ada.Strings.Unbounded;
with Basic_Output;


package body Devices.Catalog is


   --
   ---   Catalog items that represent a single device
   --


   overriding
   function Device_By (
      Name : String;
      From : One_Device_Item_T)
   return Device_Ref
   is
      use Ada.Characters.Handling;
      use Ada.Strings.Unbounded;

      Result : Device_Ref;
      -- The result, from the Factory.

   begin

      if To_Upper (To_String (From.Name)) = To_Upper (Name) then

         Result := From.Factory.all;

         Set_Name (
            Device => Result.all,
            To     => To_String (From.Name));

         return Result;

      else

         return null;

      end if;

   end Device_By;


   overriding
   function Name_Of (Item : One_Device_Item_T)
   return String
   is
   begin

      return Ada.Strings.Unbounded.To_String (Item.Name);

   end Name_Of;


   --
   ---   Catalog operations
   --


   Max_Items : constant := 200;
   --
   -- The maximum number of items in the catalog.


   Items     : array (1 .. Max_Items) of Item_Ref;
   Last_Item : Natural := 0;
   --
   -- The catalog consists of Items(1..Last_Item).


   procedure Enter (Item : in Item_Ref)
   is
   begin

      if Last_Item < Items'Last then

         Last_Item := Last_Item + 1;

         Items(Last_Item) := Item;

      else

         Basic_Output.Fault (
            Location => "Devices.Catalog.Enter",
            Text     =>
                 "Catalog full, item """
               & Name_Of (Item.all)
               & """ not entered.");

      end if;

   end Enter;


   procedure Enter (
      Name   : in String;
      Device : in One_Device_Factory_T)
   is
      use Ada.Strings.Unbounded;
   begin

      Enter (Item => new One_Device_Item_T'(
         Name    => To_Unbounded_String (Name),
         Factory => Device));

   end Enter;


   function Device_By (Name : String)
   return Device_Ref
   is

      Result : Device_Ref := null;
      -- The result.

   begin

      for I in Items'First .. Last_Item loop

         Result := Device_By (
            Name => Name,
            From => Items(I).all);

         exit when Result /= null;

      end loop;

      return Result;

   end Device_By;


   procedure Reset (Iter : in out Iterator_T)
   is
   begin

      Iter.Index := 1;

   end Reset;


   function Item (Iter : Iterator_T) return Item_Ref
   is
   begin

      if Iter.Index <= Last_Item then

         return Items(Iter.Index);

      else

         return null;

      end if;

   end Item;


   procedure Next (Iter : in out Iterator_T)
   is
   begin

      Iter.Index := Iter.Index + 1;

   end Next;


end Devices.Catalog;
