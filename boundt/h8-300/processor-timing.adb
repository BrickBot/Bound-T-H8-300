-- Processor.Timing (body)
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
-- $Date: 2015/10/26 22:19:15 $
--
-- $Log: processor-timing.adb,v $
-- Revision 1.6  2015/10/26 22:19:15  niklas
-- Version 4b, and moved to free open-source licence.
--
-- Revision 1.5  2015/10/26 21:58:47  niklas
-- Updated to use current Options services and Options.Catalog.
--
-- Revision 1.4  2005/09/03 12:00:00  niklas
-- BT-CH-0006.
--
-- Revision 1.3  2005/04/23 07:49:18  niklas
-- Added Time_To_Auto_Mod.
--
-- Revision 1.2  2005/03/22 20:39:48  niklas
-- Updated for the removal of Memory_Indirect from Operand_T.
-- Added function Time_To_Vector to handle Memory_Indirect.
--
-- Revision 1.1  2005/01/26 20:04:14  niklas
-- First version.
--


with H8_300;
with H8_300.Devices;
with H8_300.Opt;
with Output;
with Processor.Opt;


package body Processor.Timing is


   subtype Access_Cycle_T is Cycle_Type_T range Fetch .. Word;
   --
   -- The cycle-types that involve memory access.


   Basic_Time : constant array (
      Access_Cycle_T,
      H8_300.Location_T range H8_300.Register .. H8_300.External)
   of Effort_T := (
              --  Register  Internal  External
      Fetch   => (   6,        2,        6   ),
      Address => (   6,        2,        6   ),
      Stack   => (   6,        2,        6   ),
      Byte    => (   3,        2,        3   ),
      Word    => (   6,        2,        6   ));
   --
   -- The basic access time of each kind of cycle, for each kind
   -- of accessed memory.
   --
   -- Note that the order of Locations is different from the tables
   -- in the H8/300 manuals. Here the order is Register, Internal,
   -- External; in the manuals it is Internal, Register, External.
   --
   -- The wait-states for external memory are _not_ included here.


   Waits : constant array (Access_Cycle_T) of Natural := (
      Fetch   => 2,
      Address => 2,
      Stack   => 2,
      Byte    => 1,
      Word    => 2);
   --
   -- The number of times the wait-cycles occur for one access to
   -- external memory by each kind of cycle.


   function Memory_Kind (Address : H8_300.Address_T)
   return H8_300.Devices.Memory_Kind_T
   --
   -- The kind of memory in the current device at the given Address.
   -- However, Reserved memory is converted to External, and an
   -- error message is emitted.
   --
   is
      use type H8_300.Location_T;

      Memory : constant H8_300.Devices.Memory_Kind_T :=
         H8_300.Devices.Memory_Kind (
            Address => Address,
            Device  => H8_300.Opt.Device.all);
      -- The actual kind of memory at this Address.

   begin

      if Memory.Location /= H8_300.Reserved then

         return Memory;

      else

         Output.Error (
              "Access to reserved memory address "
            & Image (Address_T (Address))
            & " is assumed to reach external memory.");

         return (
            Location   => H8_300.External,
            Writable   => True,
            Read_Wait  => Opt.Read_Wait,
            Write_Wait => Opt.Write_Wait);

      end if;

   end Memory_Kind;


   function Time_To_Read (
      Address : H8_300.Address_T;
      Cycle   : Access_Cycle_T)
   return Effort_T
   --
   -- The time required for a given type of read Cycle, using a given
   -- memory Address and including wait-states for external memory.
   --
   is
      use type H8_300.Location_T;

      Memory : constant H8_300.Devices.Memory_Kind_T := Memory_Kind (Address);
      -- The kind of memory at this Address.

      Time : Effort_T;
      -- The result.

   begin

      if Cycle = Fetch and Memory.Location = H8_300.Register then

         Output.Warning (
              "Fetching code from on-chip register field at "
            & Image (Address_T (Address)));

      end if;

      Time := Basic_Time(Cycle, Memory.Location);

      if Memory.Location = H8_300.External then

         Time := Time + Effort_T (Waits(Cycle) * Memory.Read_Wait);

      end if;

      return Time;

   end Time_To_Read;


   function Time_To_Write (
      Address : H8_300.Address_T;
      Cycle   : Access_Cycle_T)
   return Effort_T
   --
   -- The time required for a given type of write Cycle, using a given
   -- memory Address and including wait-states for external memory.
   --
   is
      use type H8_300.Location_T;

      Memory : constant H8_300.Devices.Memory_Kind_T := Memory_Kind (Address);
      -- The kind of memory at this Address.

      Time : Effort_T;
      -- The result.

   begin

      if Cycle = Fetch then
         -- A Fetch cycle cannot write to memory.

         Output.Fault (
            Location => "Processor.Timing.Time_To_Write",
            Text     => "Cycle is Fetch");

      end if;

      Time := Basic_Time(Cycle, Memory.Location);

      if Memory.Location = H8_300.External then

         Time := Time + Effort_T (Waits(Cycle) * Memory.Write_Wait);

      end if;

      return Time;

   end Time_To_Write;


   function Time_To_Fetch (
      From   : Flow.Step_Tag_T;
      Length : H8_300.Instruction_Length_T)
   return Effort_T
   is
      use type H8_300.Instruction_Length_T;

      PC : constant Address_T := Processor.Prime_Address (From.State);
      -- The address of the instruction (first word).

      Time : Effort_T;
      -- The time.

   begin

      -- Fetch the first word:

      Time := Time_To_Read (
         Address => H8_300.Address_T (PC),
         Cycle   => Fetch);

      if Length = 4 then
         -- Fetch the second word:

         Time := Time
            + Time_To_Read (
                 Address => H8_300.Address_T (PC + 2),
                 Cycle   => Fetch);

      end if;

      return Time;

   end Time_To_Fetch;


   Data_Cycle : constant array (H8_300.Width_T) of Access_Cycle_T := (
      H8_300.Octet => Byte,
      H8_300.Word  => Word);
   --
   -- The kind of access cycle that is used for a given Width of
   -- the data operand.


   function Time_To_Read (
      Operand : H8_300.Operand_T;
      Width   : H8_300.Width_T)
   return Effort_T
   is
      use H8_300;

      Cycle : constant Access_Cycle_T := Data_Cycle(Width);
      -- The kind of cycle to be used.

      Location : Location_T;
      -- The (known or assumed) location for an indirect operand.

      Time : Effort_T;
      -- The result.

   begin

      case Operand.Kind is

      when Immediate =>
         -- The time is included in the Fetch time.

         Time := 0;

      when Absolute =>

         Time := Time_To_Read (Operand.Address, Cycle);

      when Register =>
         -- Instant access in the CPU.

         Time := 0;

      when Register_Indirect =>

         if Operand.Pointer = SP then
            -- Stack access, so we assume we know the Location.

            Location := Opt.Stack_Location;

         else
            -- Not stack access. Assume the worst.

            Location := External;

         end if;

         Time := Basic_Time(Cycle, Location);

         if Location = External then

            Time := Time + Effort_T (Waits(Cycle) * Opt.Read_Wait);

         end if;

         if Operand.Auto_Mod /= None then
            -- Two internal operation cycles are needed to
            -- modify the pointer register.

            Time := Time + Time_To_Auto_Mod;

         end if;

      end case;

      return Time;

   end Time_To_Read;


   function Time_To_Write (
      Operand : H8_300.Operand_T;
      Width   : H8_300.Width_T)
   return Effort_T
   is
      use H8_300;

      Cycle : constant Access_Cycle_T := Data_Cycle(Width);
      -- The kind of cycle to be used.

      Location : Location_T;
      -- The (known or assumed) location for an indirect operand.

      Time : Effort_T;
      -- The result.

   begin

      case Operand.Kind is

      when Immediate =>
         -- Cannot write an Immediate operand.

         Output.Fault (
            Location => "Processor.Timing.Time_To_Write",
            Text     => "Immediate operand.");

         Time := 0;

      when Absolute =>

         Time := Time_To_Write (Operand.Address, Cycle);

      when Register =>
         -- Instant access in the CPU.

         Time := 0;

      when Register_Indirect =>

         if Operand.Pointer = SP then
            -- Stack access, so we assume we know the Location.

            Location := Opt.Stack_Location;

         else
            -- Not stack access. Assume the worst.

            Location := External;

         end if;

         Time := Basic_Time(Cycle, Location);

         if Location = External then

            Time := Time + Effort_T (Waits(Cycle) * Opt.Write_Wait);

         end if;

         if Operand.Auto_Mod /= None then
            -- Two internal operation cycles are needed to
            -- modify the pointer register.

            Time := Time + Time_To_Auto_Mod;

         end if;

      end case;

      return Time;

   end Time_To_Write;


   function Time_To_Vector (Through : H8_300.Vector_T)
   return Effort_T
   is
   begin

      return Time_To_Read (
         Address => Through.Target_Pointer,
         Cycle   => Address);

   end Time_To_Vector;


   function Time_To_Push return Effort_T
   is
      use H8_300;
   begin

      return Time_To_Write (
         Operand => (
            Kind         => Register_Indirect,
            Pointer      => SP,
            Auto_Mod     => Pre_Decrement,
            Displacement => 0),
         Width => Word);

   end Time_To_Push;


   function Time_To_Pop return Effort_T
   is
      use H8_300;
   begin

      return Time_To_Read (
         Operand => (
            Kind         => Register_Indirect,
            Pointer      => SP,
            Auto_Mod     => Post_Increment,
            Displacement => 0),
         Width => Word);

   end Time_To_Pop;


end Processor.Timing;
