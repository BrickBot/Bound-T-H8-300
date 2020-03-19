-- H8_300.Devices.H8_3297 (body)
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
-- $Revision: 1.6 $
-- $Date: 2015/10/29 15:36:58 $
--
-- $Log: h8_300-devices-h8_3297.adb,v $
-- Revision 1.6  2015/10/29 15:36:58  niklas
-- Removed "H8/" prefix from the name of this device series.
--
-- Revision 1.5  2015/10/26 22:19:14  niklas
-- Version 4b, and moved to free open-source licence.
--
-- Revision 1.4  2015/10/26 21:58:46  niklas
-- Updated to use current Options services and Options.Catalog.
--
-- Revision 1.3  2007/02/15 13:17:47  Niklas
-- Removed unnecessary context clause.
--
-- Revision 1.2  2005/01/26 20:59:36  niklas
-- Removed the option -stack=xxx from List_Options. This option belongs
-- in Decoder.Opt because it is not device-specific.
--
-- Revision 1.1  2005/01/26 19:23:26  niklas
-- First version.
--


with Ada.Characters.Handling;
with Hex;
with Options;
with Options.Groups;
with Options.H8_300;
with Output;
with Processor.Opt;


package body H8_300.Devices.H8_3297 is


   -- Reference:
   --    Hitachi Single-Chip Microcomputer, H8/3297 Series Hardware
   --    Manual, 3rd Edition, September 1997.


   type String_Ref is access String;
   --
   -- For device names.


   Part_Name : constant array (Part_T) of String_Ref := (
      H8_3292 => new String'("H8/3292"),
      H8_3294 => new String'("H8/3294"),
      H8_3296 => new String'("H8/3296"),
      H8_3297 => new String'("H8/3297"));
   --
   -- The (canonical) names of the parts in this series.


   --
   ---   Device-specific options
   --


   Group : constant Options.Group_Name_T := Options.Group ("h8-3297");
   --
   -- The group for H8/3297 options.
   

   function Image (Item : Mode_T) return String
   is
   begin
   
      return Output.Trim (Mode_T'Image (Item));

   end Image;
   
   
   package Mode_Valued is new Options.Discrete_Valued (
      Value_Type             => Mode_T,
      Value_Type_Description => "Mode number",
      Value_Image            => Image,
      Use_Discrete_Image     => False,
      Quote_Image            => False); 


   Mode_Opt : aliased Mode_Valued.Option_T (Default => 2);
   --
   -- The operating mode to be assumed for these devices.
   -- Can be changed by command-line option.
   --
   Mode : Mode_T renames Mode_Opt.Value;


   type RAME_T is (Enabled, Disabled);
   --
   -- Whether Internal RAM is enabled or disabled.


   package RAME_Valued is new Options.Discrete_Valued (
      Value_Type  => RAME_T,
      Value_Image => RAME_T'Image);


   RAME_Opt : aliased RAME_Valued.Option_T (Default => Enabled);
   --
   -- The value of SYSCR.RAME to be assumed for these devices.
   -- Can be changed by command-line option.
   --
   RAME : RAME_T renames RAME_Opt.Value;


   --
   ---   Memory map stuff
   --


   Int_RAM_Start : array (Part_T) of Address_T := (
      H8_3292 => 16#FD80#,
      H8_3294 => 16#FB80#,
      H8_3296 => 16#F780#,
      H8_3297 => 16#F780#);
   --
   -- The starting address of the internal (on-chip) RAM in the
   -- several devices.


   procedure Initialize (Device : in out Device_T)
   is

      Register : constant Memory_Kind_T := (
         Location   => H8_300.Register,
         Writable   => True,   -- At least some parts.
         Read_Wait  => 0,
         Write_Wait => 0);
      -- Describes the on-chip register field in the memory map.

      Internal_ROM : constant Memory_Kind_T := (
         Location   => Internal,
         Writable   => False,
         Read_Wait  => 0,
         Write_Wait => 0);
      -- Describes the internal ROM/PROM in the memory map.

      Internal_RAM : constant Memory_Kind_T := (
         Location   => Internal,
         Writable   => True,
         Read_Wait  => 0,
         Write_Wait => 0);
      -- Describes the internal RAM in the memory map.

      External_Mem : constant Memory_Kind_T := (
         Location   => External,
         Writable   => True,
         Read_Wait  => Processor.Opt.Read_Wait,
         Write_Wait => Processor.Opt.Write_Wait);
      -- Describes the external RAM in the memory map.

      Reserved : constant Memory_Kind_T := (
         Location   => H8_300.Reserved,
         Writable   => False,
         Read_Wait  => 0,
         Write_Wait => 0);
      -- Describes the reserved areas in the memory map.
      -- Only the Location value is relevant.

      Reserved_Else_External : constant array (Boolean) of Memory_Kind_T := (
         False => External_Mem,
         True  => Reserved);
      -- If <condition> then Reserved else External_Mem.

      Internal_Else_External : constant array (Boolean) of Memory_Kind_T := (
         False => External_Mem,
         True  => Internal_RAM);
      -- If <condition> then Internal_RAM else External_Mem.


      Int_RAM : constant Address_T := Int_RAM_Start(Device.Part);
      -- The starting address of the internal RAM.


      procedure Set (
         First, Last : in Address_T;
         Kind        : in Memory_Kind_T)
      --
      -- Sets the memory map to Kind for the addresses First .. Last.
      --
      is
      begin

         Output.Note (
              "Memory "
            & Hex.Image (Hex.Word16_T (First))
            & " .. "
            & Hex.Image (Hex.Word16_T (Last))
            & " is "
            & Image (Kind));

         Memory_Maps.Set (
            Map   => Device.Memory_Map,
            From  => (First => First, Last => Last),
            To    => Kind);

      end Set;


   begin  -- Initialize

      -- Interactions between options:

      if Mode = 3 then
         -- Single-chip mode.

         if RAME = Disabled then

            Output.Warning ("Mode 3 implies -internal_ram=enabled.");

            RAME := Enabled;

         end if;

         if Processor.Opt.Stack_Location = External then

            Output.Warning ("Mode 3 implies -stack=internal.");

            Processor.Opt.Stack_Location := Internal;

         end if;

      end if;

      if RAME = Disabled then
         -- Internal RAM is (now) disabled (so we are not in Mode 3).

         if Processor.Opt.Stack_Location = Internal then

            Output.Warning ("Disabled internal RAM implies -stack=external.");

            Processor.Opt.Stack_Location := External;

         end if;

      end if;


      Device.Mode := Mode;
      Device.RAME := RAME = Enabled;

      Output.Note ("Creating the memory map for " & Name (Device));

      Memory_Maps.Initialize (Device.Memory_Map);

      -- Memory map for the areas before internal RAM:

      case Device.Part is

      when H8_3292 =>

         case Mode is

         when 1 =>

            Set (16#0000#, 16#FB7F#   , External_Mem);
            Set (16#FB80#, Int_RAM - 1, Reserved_Else_External(Device.RAME));

         when 2 =>

            Set (16#0000#, 16#3FFF#   , Internal_ROM);
            Set (16#4000#, 16#7FFF#   , Reserved    );
            Set (16#8000#, 16#FB7F#   , External_Mem);
            Set (16#FB80#, Int_RAM - 1, Reserved_Else_External(Device.RAME));

         when 3 =>

            Set (16#0000#, 16#3FFF#   , Internal_ROM);
            Set (16#4000#, Int_RAM - 1, Reserved     );
            -- TBC that 8000 .. Int_RAM - 1 is reserved (blank in HW doc).

         end case;

      when H8_3294 =>

         -- The following is TBC. The memory map diagram for this
         -- part is unreadable in the HW doc.

         case Mode is

         when 1 =>

            Set (16#0000#, Int_RAM - 1, External_Mem);

         when 2 =>

            Set (16#0000#, 16#7FFF#   , Internal_ROM);
            Set (16#8000#, Int_RAM - 1, External_Mem);

         when 3 =>

            Set (16#0000#, 16#7FFF#   , Internal_ROM);
            Set (16#8000#, Int_RAM - 1, Reserved    );

         end case;

      when H8_3296 =>

         case Mode is

         when 1 =>

            Set (16#0000#, Int_RAM - 1, External_Mem);

         when 2 =>

            Set (16#0000#, 16#BFFF#   , Internal_ROM);
            Set (16#C000#, 16#EF7F#   , Reserved    );
            Set (16#EF80#, Int_RAM - 1, External_Mem);

         when 3 =>

            Set (16#0000#, 16#BFFF#   , Internal_ROM);
            Set (16#C000#, Int_RAM - 1, Reserved    );

         end case;

      when H8_3297 =>

         case Mode is

         when 1 =>

            Set (16#0000#, Int_RAM - 1, External_Mem);

         when 2 =>

            Set (16#0000#, 16#EF7F#   , Internal_ROM);
            Set (16#EF80#, Int_RAM - 1, External_Mem);

         when 3 =>

            Set (16#0000#, Int_RAM - 1, Internal_ROM);

         end case;

      end case;


      -- Memory map for internal RAM and on-chip register field:

      Set (Int_RAM , 16#FF7F#, Internal_Else_External(Device.RAME));

      Set (16#FF80#, 16#FF87#, Reserved_Else_External(Mode = 3));

      Set (16#FF88#, 16#FFFF#, Register);

   end Initialize;


   function Supports (
      Device      : Device_T;
      Instruction : Instruction_T)
   return Boolean
   is
   begin

      return Instruction.Kind /= Move_From_Peri
         and Instruction.Kind /= Move_To_Peri;

   end Supports;


   --
   ---   Selecting a device by name from the Catalog
   --


   function Significant_Part (Name : String) return String
   --
   -- If Name is "H8/xxx", you get the "xxx", otherwise the whole Name.
   --
   is
   begin

      if       Name'Length >= 3
      and then Name(Name'First .. Name'First + 2) = "h8/"
      then
         -- Skip the "h8/".

         return Name(Name'First + 3 .. Name'Last);

      else

         return Name;

      end if;

   end Significant_Part;


   type Device_Ref is access all Device_T'Class;
   --
   -- Accesses a device in the H8/3297 series.


   type Catalog_Item_T is new Catalog.Item_T with null record;
   --
   -- The device catalog entry for the H8/3297 devices,


   overriding
   function Device_By (
      Name : String;
      From : Catalog_Item_T)
   return Standard.Devices.Device_Ref;
   
   
   overriding
   function Name_Of (Item : Catalog_Item_T)
   return String;


   overriding
   function Device_By (
      Name : String;
      From : Catalog_Item_T)
   return Standard.Devices.Device_Ref
   is
      use Ada.Characters.Handling;

      Main_Name : constant String := Significant_Part (To_Lower (Name));
      -- The main part of the name, skipping any "h8/" prefix
      -- after converting to lower-case.

      Result : Device_Ref := null;
      -- The result, changed to non-null if the Name is a valid one.

   begin

      if Main_Name = "3292"
      or Main_Name = "lego"
      then
         -- An H8/3292 as in the Lego (TM) Mindstorms (TM).

         Result := new Device_T;

         Result.Part := H8_3292;

      elsif Main_Name = "3294" then

         Result := new Device_T;

         Result.Part := H8_3294;

      elsif Main_Name = "3296" then

         Result := new Device_T;

         Result.Part := H8_3296;

      elsif Main_Name = "3297" then

         Result := new Device_T;

         Result.Part := H8_3297;

      end if;
      
      if Result /= null then
         -- A known device in this series.
         
         Standard.Devices.Set_Name (
            Device => Result.all,
            To     => Part_Name(Result.Part).all);
            
      end if;

      return Standard.Devices.Device_Ref (Result);

   end Device_By;
   

   overriding
   function Name_Of (Item : Catalog_Item_T)
   return String
   is
   begin
   
      return "3297-series";
      
   end Name_Of;
   
   
begin  -- H8_300.Devices.H8_3297
   
   Catalog.Enter (new Catalog_Item_T);
   
   Options.Register (
      Option => Mode_Opt'Access,
      Name   => "mode",
      Groups => (
         Options.H8_300.Group,
         Group));
      
   Options.Register (
      Option => RAME_Opt'Access,
      Name   => "internal_ram",
      Groups => (
         Options.H8_300.Group,
         Group));
   
end H8_300.Devices.H8_3297;
