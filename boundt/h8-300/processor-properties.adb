-- Processor.Properties (body)
--
-- Author: Niklas Holsti
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
-- $Revision: 1.15 $
-- $Date: 2015/10/26 22:19:15 $
--
-- $Log: processor-properties.adb,v $
-- Revision 1.15  2015/10/26 22:19:15  niklas
-- Version 4b, and moved to free open-source licence.
--
-- Revision 1.14  2015/10/26 22:01:05  niklas
-- Updated to use current Options services.
--
-- Revision 1.13  2010/02/03 21:24:18  niklas
-- Added Offset_Return per BT-CH-0216.
--
-- Revision 1.12  2009-12-02 14:20:01  niklas
-- BT-CH-0188: H8/300 updates for bit-widths and Word_T.
--
-- Revision 1.11  2009-07-15 12:02:09  niklas
-- Added Assumed_Net_Change, for BT-CH-0179.
--
-- Revision 1.10  2008-04-29 20:13:52  niklas
-- Updated for BT-CH-0122.
--
-- Revision 1.9  2007/03/22 18:51:35  niklas
-- BT-CH-0053.
--
-- Revision 1.8  2006/11/05 21:19:37  niklas
-- BT-CH-0036: Property BCC_Signed.
--
-- Revision 1.7  2006/10/30 23:09:53  niklas
-- Updated for BT-CH-0033.
--
-- Revision 1.6  2006/08/23 19:32:42  niklas
-- Updates for BT-CH-0025.
--
-- Revision 1.5  2006/08/22 11:10:12  niklas
-- Per BT-CH-0021 moved the Root_Info function from Processor.Program
-- to Processor.Properties and added parameters.
--
-- Revision 1.4  2005/04/14 21:27:19  niklas
-- Removed Error message for unidentified subprogram, because this
-- problem is reported in a general (target-independent) way.
--
-- Revision 1.3  2005/04/01 21:01:47  niklas
-- Implemented function Variable_Cell.
--
-- Revision 1.2  2005/03/22 20:38:49  niklas
-- Defined the constant Entry_Stack_Height for use in stacked
-- parameter mapping as well as in Entry_Bounds.
--
-- Revision 1.1  2005/03/17 07:12:50  niklas
-- First version.
--


with Ada.Characters.Handling;
with Arithmetic;
with Hex;
with Output;
with Processor.Cells;


package body Processor.Properties is


   function Hex_Address (Hex : String) return Processor.Address_T
   --
   -- The address denoted by the given hex digit string.
   -- Raises Constraint_Error if the string is invalid.
   --
   is
   begin

       return Processor.Address_T'Value ("16#" & Hex & '#');

   end Hex_Address;



   function Subprogram_Address (Sub_Address : String)
   return Code_Address_T
   --
   -- The subprogram address is just a hex string, with no decoration
   -- of any kind.
   --
   is
   begin

      return Hex_Address (Sub_Address);

   exception

   when Constraint_Error =>
      -- The address string is wrong in some way.

      raise Address_Error;

   end Subprogram_Address;


   function Code_Offset (Offset : String)
   return Code_Offset_T
   --
   -- The offset is just a hex string, with no decoration of any kind,
   -- but an initial '-' sign is allowed and obeyed.
   --
   is
   begin

      if Offset'Length = 0 then

         raise Address_Error;

      elsif Offset(Offset'First) /= '-' then

         return Code_Offset_T (Hex_Address (Offset));

      else

         return -Code_Offset_T (Hex_Address (
            Offset(Offset'First + 1 .. Offset'Last)));

      end if;

   end Code_Offset;


   function Offset_Image (From, To : Code_Address_T) return String
   is
   begin

      if To >= From then

         return Hex.Image (Hex.Word16_T (To - From));

      else

         return '-' & Hex.Image (Hex.Word16_T (From - To));

      end if;

   end Offset_Image;


   function Offset_Return (
      From : Flow_State_T;
      By   : Code_Offset_T)
   return Flow_State_T
   is
   begin

      if From.Kind /= Normal then

         Output.Error (
              "Return by offset "
            & Image (By)
            & " from strange state "
            & Image (From));

      end if;

      return (
         Kind    => Normal,
         Address => Shift (Base => From.Address, Offset => By));

   end Offset_Return;


   function Variable_Cell (Var_Address : String)
   return Storage.Cell_T
   --
   -- The (case-insensitive) syntax for cells is as follows:
   --
   -- Registers    : r<n>, r<n>h, r<n>l
   -- Flags        : no syntax.
   -- Memory octet : b<hex address>
   -- Memory word  : w<hex address>
   -- Param  octet : pb<decimal offset>
   -- Param  words : pw<decimal offset>
   -- Local  octet : lb<decimal offset>
   -- Local  word  : lw<decimal offset>
   --
   is
      use Processor;

      Addr : constant String :=
         Ada.Characters.Handling.To_Upper (Var_Address);
      -- All in uppercase for convenience.

      First : constant Positive := Addr'First;
      Last  : constant Natural  := Addr'Last;
      -- The index bounds.

      Reg_Num : H8_300.Register_Number_T;
      -- The number of the register cell.

      Reg_Part : H8_300.Register_Part_T;
      -- The register part for the register cell.

      Mem_Addr : Address_T;
      -- The address of the memory cell.

      Offset : Address_Offset_T;
      -- The offset of a parameter or local cell.

      Width : H8_300.Width_T;
      -- The width of the memory, parameter or local cell.

   begin

      if Addr'Length < 2 then
         -- Too short.

         raise Address_Error;

      else
         -- Long enough, let's see...

         case Addr(First) is

         when 'R' =>
            -- A register or nothing.

            Reg_Num := H8_300.Register_Number_T'Value (
               Addr(First + 1 .. First + 1));

            if Addr'Length = 2 then
               -- R<n>.

               Reg_Part := H8_300.Word;

            elsif Addr(First + 2 .. Last) = "L" then
               -- R<n>L.

               Reg_Part := H8_300.Low_Byte;

            elsif Addr(First + 2 .. Last) = "H" then
               -- R<n>H.

               Reg_Part := H8_300.High_Byte;

            else
               -- Wrong form.

               raise Address_Error;

            end if;

            return Cells.Register_Cell(Reg_Num, Reg_Part);

         when 'B' | 'W' =>
             -- Memory byte or word.

             Mem_Addr := Hex_Address (Addr(First + 1 .. Last));

             if Addr(First) = 'B' then Width := H8_300.Octet;
                                  else Width := H8_300.Word;
             end if;

             return Cells.Memory (Mem_Addr, Width);

         when 'P' | 'L' =>
            -- Parameter or Local.

            Offset := Address_Offset_T'Value (Addr(First + 2 .. Last));

            if Addr(First + 1) = 'B' then

               Width := H8_300.Octet;

            elsif Addr(First + 1) = 'W' then

               Width := H8_300.Word;

            else

               raise Address_Error;

            end if;

            if Addr(First) = 'P' then

               return Cells.Param (Offset, Width);

            else

               return Cells.Local (Offset, Width);

            end if;

         when others =>

            raise Address_Error;

         end case;

      end if;

   exception

   when Constraint_Error
      | Address_Error =>

      raise Address_Error;

   end Variable_Cell;


   function Blank_Info (
      Address : Code_Address_T;
      Program : Programs.Program_T)
   return Processor.Program.Sub_Info_T
   is
   begin

      return Processor.Program.Initial_Sub_Info;

   end Blank_Info;


   procedure Define_As_Root (
      Subprogram : in     Programs.Subprogram_T;
      Info       : in out Processor.Program.Sub_Info_T)
   is
   begin

      null;

   end Define_As_Root;


   procedure Inform_By_Call (
      Caller      : in     Programs.Subprogram_T;
      Callee      : in     Programs.Subprogram_T;
      Call_Info   : in     Processor.Program.Sub_Info_T;
      Callee_Info : in out Processor.Program.Sub_Info_T)
   is
   begin

      null;

      -- For the H8/300, no actions have been identified.

   end Inform_By_Call;


   function Default_Properties (
      Subprogram : Programs.Subprogram_T)
   return Assertions.Property_Table_T
   is
   begin

      return (others => Storage.Bounds.Universal_Interval);

   end Default_Properties;


   function Power (
      Within : Flow.Node_T;
      Assert : Assertions.Assertion_Map_T)
   return Processor.Power_T
   is
   begin

      return (null record);

   end Power;


   procedure Apply (
      Property : in Property_T;
      Values   : in Storage.Bounds.Interval_T;
      To       : in Programs.Subprogram_T)
   is
      use Storage.Bounds;
      use type Arithmetic.Value_T;

      Info : Program.Sub_Info_T := Programs.Processor_Info (To);
      -- Process-specific info for this subprogram.

   begin

      if Property = BCC_Signed then
         -- Zero means: use the command-line state of "-bcc".
         -- One  means: use "-bcc=signed" (signed conditions
         -- are unknown).

         if       Singular     (Values)
         and then Single_Value (Values) in 0 .. 1
         then

            Info.Signed_Cond_Unknown := Single_Value (Values) = 1;

            Programs.Set_Processor_Info (
               Sub => To,
               To  => Info);

         else

            Output.Warning (
               Locus => Programs.Locus (To),
               Text  =>
                    "Meaning of "
                  & Storage.Bounds.Image (
                      Item => Values,
                      Name => Property_T'Image (Property))
                  & " not understood and thus ignored.");

         end if;

      end if;

   end Apply;


   function Assumed_Net_Change (
      Stack : Programs.Stack_T;
      Stub  : Programs.Subprogram_T)
   return Storage.Bounds.Limit_T
   is
      use type Arithmetic.Value_T;

      Height : constant Cell_Spec_T :=
         Storage.Spec_Of (Programs.Height (Stack));
      -- The stack-height cell for this Stack.

   begin

      case Height.Kind is

      when Stack_Height =>
         -- The SP = R7 stack.

         return Storage.Bounds.Limit (
            - Arithmetic.Value_T (H8_300.Stack_Operand_Octets));
         --
         -- The return address is popped.

      when others =>

         Output.Fault (
            Location => "Processor.Properties.Assumed_Net_Change",
            Text     => "Stack-height cell is " & Image (Height));

         return Storage.Bounds.Not_Limited;

      end case;

   end Assumed_Net_Change;


   function Entry_State (Subprogram : Programs.Subprogram_T)
   return Flow_State_T
   is
   begin

      return (
         Kind    => Normal,
         Address => Programs.Entry_Address (Subprogram));

   end Entry_State;


   function Entry_Bounds (
      Subprogram : Programs.Subprogram_T;
      Sub_Info   : Processor.Program.Sub_Info_T)
   return Storage.Bounds.Cell_Interval_List_T
   is
   begin

      return Storage.Bounds.Empty;

   end Entry_Bounds;


end Processor.Properties;
