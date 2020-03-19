-- Processor (body)
--
-- Authors:
--    Samuel Petersson, Mälardalen University
--    Niklas Holsti, Tidorum Ltd
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
-- $Revision: 1.17 $
-- $Date: 2015/10/26 22:19:15 $
--
-- $Log: processor.adb,v $
-- Revision 1.17  2015/10/26 22:19:15  niklas
-- Version 4b, and moved to free open-source licence.
--
-- Revision 1.16  2015/10/26 22:01:42  niklas
-- Updated to current generic/target interface.
--
-- Revision 1.15  2009/12/29 13:37:48  niklas
-- Added Zero_Code_Offset and Image (Code_Offset_T) per BT-CH-0196.
-- This required making Code_Offset_T a type, not a subtype, which
-- requires a separate Shift function for Code addresses and offsets.
-- Added Instruction_Role_T per BT-CH-0197.
--
-- Revision 1.14  2009-12-02 14:20:01  niklas
-- BT-CH-0188: H8/300 updates for bit-widths and Word_T.
--
-- Revision 1.13  2008-02-02 14:52:25  niklas
-- Updated for BT-CH-0108: added Code_Address_List_T and All_Addresses.
--
-- Revision 1.12  2006/08/23 19:32:42  niklas
-- Updates for BT-CH-0025.
--
-- Revision 1.11  2005/09/03 11:52:20  niklas
-- BT-CH-0006.
--
-- Revision 1.10  2005/05/11 18:02:49  niklas
-- Removed the Compose steps.
--
-- Revision 1.9  2005/04/14 21:28:00  niklas
-- Added trimming to the Image of Value_T. This is important for stack
-- usage results, for example.
--
-- Revision 1.8  2005/04/01 12:28:29  niklas
-- Added the Step_Kind_T literals Cmp_Subx and Sub_Subx.
-- Made Cell_Spec_T a definite type (added default Kind).
--
-- Revision 1.7  2005/03/25 16:55:56  niklas
-- Extended for analysing dynamic jumps via address tables as follows.
-- Added the Step_Kind_T literal Jump_Via_Table.
-- Added the Cell_Kind_T literal Jump_Index, for a (single) synthetic
-- cell that holds the index (octet offset) in the address table.
-- Defined Loose_Edge_Info_T as a discriminated record. The variant
-- Jump_Via_Table carries the base address of the table of jump
-- addresses between the two instructions (mov.w and jmp @Rn) that
-- form the jump-via-table pattern.
--
-- Revision 1.6  2005/03/23 20:03:04  niklas
-- Added the Step_Kind_T literal Add_Addx to represent an
-- Add_With_Carry instruction for which the arithmetic effect is
-- combined into the effect of the preceding Add instruction.
-- Changed the <" operator for Step_Address_T to sort first by
-- Address and then by Kind; this keeps the combined Add and
-- Add_With_Carry steps together in the -trace listing.
--
-- Revision 1.5  2005/03/22 20:38:15  niklas
-- Support arithmetic analysis in the Decoder as follows.
-- Added Step_Kind_T literals Compose and Clobber.
-- Added subtype Address_Value_T.
-- Replaced the Cell_Kind_T literals for the condition flags by
-- two Boolean cells, Equal and Less, which will be defined only
-- to model the relationship between unsigned integer values.
-- Split the Cell_Kind_T literals Param and Local into Param_Byte,
-- Param_Word, Local_Byte and Local_Word and defined their
-- components in Cell_Spec_T.
--
-- Revision 1.4  2005/03/17 07:12:36  niklas
-- Added the Stack_Height cell-kind.
--
-- Revision 1.3  2005/01/26 19:33:15  niklas
-- Changed Effort_T to a count of execution states and Power_T to null.
-- The total time for an instruction / a step is now computed in the
-- Decoder by adding the number of states for each cycle and taking into
-- account the kind of memory access in each cycle, using functions in
-- the new package Processor.Timing and a device-specific memory map.
--
-- Revision 1.2  2004/09/27 08:45:39  niklas
-- Added type Memory_T and its Image and Alias_Range functions, to
-- support the use of memory-reference variables and expressions in
-- the Arithmetic model.
--
-- Revision 1.1  2004/06/16 07:41:39  niklas
-- First version.
--


with Ada.Strings.Fixed;
with Hex;


package body Processor is


   --
   ---   Program sequencing model
   --


   function Hash (Item : Code_Address_T) return Natural
   is
   begin

      return Natural (Item);

   end Hash;


   function Image (Item : Code_Address_T) return String
   is
   begin

      return Hex.Image (Hex.Word16_T (Item));

   end Image;


   function Shift (Base : Address_T; Offset : Address_Offset_T)
   return Address_T
   is
   begin

      if Offset >= 0 then

         return Base + Address_T (Offset);

      else

         return Base - Address_T (-Offset);

      end if;

   end Shift;


   function Image (Item : Code_Offset_T) return String
   is
   begin

      if Item >= 0 then

         return Image (Code_Address_T (Item));

      else

         return '-' & Image (Code_Address_T (- Item));

      end if;

   end Image;


   function Shift (Base : Code_Address_T; Offset : Code_Offset_T)
   return Code_Address_T
   is
   begin

      if Offset >= 0 then

         return Base + Code_Address_T (Offset);

      else

         return Base - Code_Address_T (-Offset);

      end if;

   end Shift;


   function "<" (Left, Right : Flow_State_T) return Boolean
   is
   begin

      return Left.Address < Right.Address
         or else (
                Left.Address = Right.Address
            and Left.Kind    < Right.Kind);

   end "<";


   function Prime_Address (Item : in Flow_State_T)
   return Code_Address_T
   is
   begin

      return Item.Address;

   end Prime_Address;


   function All_Addresses (Item : in Flow_State_T) return Code_Address_List_T
   is
   begin

      return (1 => Prime_Address (Item));

   end All_Addresses;


   function Image (Item : Flow_State_T) return String
   is

      Address : constant String := Image (Item.Address);
      -- The image of the address part.

   begin

      case Item.Kind is
      when Normal         => return Address;
      when Add_Addx
         | Cmp_Subx
         | Sub_Subx       => return Address & 'x';
      when Jump_Via_Table => return Address & 'j';
      when EEPMOV         => return Address & 'm';
      when Clobber        => return Address & '-';
      end case;

   end Image;


   --
   ---   Execution time model
   --


   function Memory_Reads (Effort : Effort_T) return Natural
   is
   begin

      return 0;
      --
      -- Not really implemented.
      -- TBD if the HRT Execution Skeleton is used with the H8/300.

   end Memory_Reads;


   function Memory_Writes (Effort : Effort_T) return Natural
   is
   begin

      return 0;
      --
      -- Not really implemented.
      -- TBD if the HRT Execution Skeleton is used with the H8/300.

   end Memory_Writes;


   function Image (Item : Effort_T) return String
   is
      use Ada.Strings;
   begin

      return Fixed.Trim (Effort_T'Image (Item), Left);

   end Image;


   procedure Add_Step (
      Taking : in     Effort_T;
      Using  : in     Power_T;
      To     : in out Work_T)
   is
   begin

      To := To + Work_T (Taking);

   end Add_Step;


   procedure Add_Edge (
      Taking : in     Time_T;
      Using  : in     Power_T;
      To     : in out Work_T)
   is
   begin

      To := To + Work_T (Taking);

   end Add_Edge;


   function Time_To_Finish (
      Work  : Work_T;
      Using : Power_T)
   return Time_T
   is
   begin

      return Time_T (Work);

   end Time_To_Finish;


   --
   ---   Arithmetic computation model
   --


   function "<" (Left, Right : Cell_Spec_T) return Boolean
   is
      use type H8_300.Register_Number_T;
   begin

      if Left.Kind < Right.Kind then

         return True;

      elsif Left.Kind > Right.Kind then

         return False;

      else
         -- Cells of equal Kind.

         case Left.Kind is

         when Flag_Kind_T  => return False;
         when Reg_Kind_T   => return Left.Num < Right.Num;
         when Param_Kind_T => return Left.Param < Right.Param;
         when Local_Kind_T => return Left.Local < Right.Local;
         when Mem_Kind_T   => return Left.Addr < Right.Addr;
         when Stack_Height => return False;
         when Jump_Index   => return False;

         end case;

      end if;

   end "<";


   function Width_Of (Cell : Cell_Spec_T) return Positive
   is
      use H8_300;
   begin

      case Cell.Kind is

      when Flag_Kind_T =>

         return 1;

      when Cell_Width'Range =>

         return Octet_Bits * Positive (Operand_Octets(Cell_Width(Cell.Kind)));

      when Stack_Height =>

         return Stack_Pointer_Bits;

      when Jump_Index =>

         return Address_Bits;

      end case;

   end Width_Of;


   Counter_Kind : constant array (Cell_Kind_T) of Boolean := (
      Flag_Kind_T  => False,
      Reg_Kind_T   => True,
      Param_Kind_T => True,
      Local_Kind_T => True,
      Mem_Kind_T   => True,
      Stack_Height => False,
      Jump_Index   => False);
   --
   -- Whether a given Kind of cell can be a loop counter.


   function Can_Count (Cell : Cell_Spec_T) return Boolean
   is
   begin

      return Counter_Kind (Cell.Kind);

   end Can_Count;


   function Image (Item : H8_300.Register_Number_T) return String
   --
   -- The register number as a decimal digit.
   --
   is
      use Ada.Strings, Ada.Strings.Fixed;
   begin

      return Trim (H8_300.Register_Number_T'Image (Item), Left);

   end Image;


   function Image (Item : Address_Offset_T) return String
   --
   -- The offset in decimal form.
   --
   is
      use Ada.Strings, Ada.Strings.Fixed;
   begin

      return Trim (Address_Offset_T'Image (Item), Left);

   end Image;


   function Image (Item : Cell_Spec_T) return String
   is
   begin

      case Item.Kind is

      when Equal =>

         return "z";

      when Less =>

         return "c";

      when RnH =>

         return 'r' & Image (Item.Num) & 'h';

      when RnL =>

         return 'r' & Image (Item.Num) & 'l';

      when Rn =>

         return 'r' & Image (Item.Num);

      when Param_Byte =>

         return "pb" & Image (Item.Param);

      when Param_Word =>

         return "pw" & Image (Item.Param);

      when Local_Byte =>

         return "lb" & Image (Item.Local);

      when Local_Word =>

         return "lw" & Image (Item.Local);

      when Mem_Byte =>

         return 'b' & Image (Item.Addr);

      when Mem_Word =>

         return 'w' & Image (Item.Addr);

      when Stack_Height =>

         return "sh";

      when Jump_Index =>

         return "ji";

      end case;

   end Image;


   function Valid_Cell_Range (From, To : Cell_Spec_T)
   return Boolean
   is
   begin

      return False;   -- TBM!

   end Valid_Cell_Range;


   function Cell_In_Range (Cell : Cell_Spec_T; From, To : Cell_Spec_T)
   return Boolean
   is
   begin

      return False;   -- TBM!

   end Cell_In_Range;


   --
   ---   Memory aliasing model
   --


   function Alias_Group (Cell : Cell_Spec_T) return Alias_Group_T
   is
   begin

      return TBD;

   end Alias_Group;


   function May_Alias (Left, Right : Alias_Range_T) return Boolean
   is
   begin

      return (Left and Right) /= Isolated;

   end May_Alias;


   function Alias_Range (Group : Alias_Group_T) return Alias_Range_T
   is
   begin

      return (others => True);  -- TBD.

   end Alias_Range;


   function Image (Item : Memory_T) return String
   is
   begin

      return Memory_T'Image (Item);

   end Image;


   function Alias_Range (Memory : Memory_T) return Alias_Range_T
   is
   begin

      return (others => True);  -- TBD

   end Alias_Range;


   --
   ---   Attaching target-specific information to control-flow steps
   --


   function Effort (Info : Step_Info_T) return Effort_T
   is
   begin

      return Info.Effort;

   end Effort;


   function Image (Item : Step_Info_T) return String
   is
   begin

      return Image (Item.Effort);

   end Image;


   --
   ---   Attaching target-specific information to "loose" edges
   ---   in control-flow graphs under construction.
   --


   --
   ---   Target-specific assertable properties
   --


end Processor;
