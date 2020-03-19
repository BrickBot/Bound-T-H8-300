-- H8_300 (body)
--
-- Authors: Samuel Petersson, Niklas Holsti
--
-- References:
--    H8/300 Programmer's Manual
--    Renesas (Hitachi) 1st Edition, December 1989.
--
-- H8/300 is TBC a registered trademark of Renesas.
--
-- This file is a component of the Bound-T Worst-Case Execution Time Tool.
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
-- $Revision: 1.12 $
-- $Date: 2015/10/26 22:19:14 $
--
-- $Log: h8_300.adb,v $
-- Revision 1.12  2015/10/26 22:19:14  niklas
-- Version 4b, and moved to free open-source licence.
--
-- Revision 1.11  2005/10/24 11:31:00  niklas
-- Corrected Decode to signal Invalid_Instruction when the high
-- octet of the instruction is 16#78#, instead of returning an
-- undefined instruction.
--
-- Revision 1.10  2005/03/19 07:39:30  niklas
-- Added Target_T for use in Jump and Jump_Subroutine, removing
-- the Memory_Indirect case from Operand_T.
--
-- Revision 1.9  2004/12/03 20:15:24  niklas
-- Made Decode raise Invalid_Instruction instead of returning an
-- Undefined instruction.
-- Added Checks of non-coding bits in Decode, to detect invalid
-- instructions. Added some local variables to help.
--
-- Revision 1.8  2004/11/19 21:51:49  niklas
-- Changed Seventh_Bits to Boolean.
-- Added function First_Register_Indirect_Auto for Moves.
-- Added procedure Make_Move to use Seventh_Bit better.
--
-- Revision 1.7  2004/11/19 20:28:33  niklas
-- Renamed Get_Byte_Part to Byte_Part (function name).
-- Removed redundant function Get_Register_Part.
-- Defined local subprograms for common operands.
-- Removed tab chars. Changed indentation to Bound-T form.
-- Removed unreachable "else" branches.
-- Changed "elsif"-cascades to "case" form.
-- Corrected Bit number for "BTST Rn,@Rd".
--
-- Revision 1.6  2004/11/19 20:00:17  niklas
-- Implemented by Samuel Petersson:
-- Added when-else statement for decoding instructions in H/300.
-- Corrected some bugs.
-- Added function Get_Byte_Part.
-- Corrected some missunderstandings concerning "Width" and "Part".
-- Added function Get_Register_Part.
-- Tested and corrected bugs and faulty decodings.
-- Added function Branch_Offset.
-- No errors were present after running both small and large
-- test with h8_300-verify.
-- Added subtype One_T (moved from spec to body by NH).
--
-- Revision 1.1  2004/06/16 07:41:38  niklas
-- First version.
--


package body H8_300 is


   Immediate_1 : constant Operand_T := (Kind => Immediate, Value => 1);
   Immediate_2 : constant Operand_T := (Kind => Immediate, Value => 2);
   --
   -- Two immediate operands used in several places.


   Condition : constant array (Quartet_T) of Condition_T := (
      16#0# => Always,
      16#1# => Never,
      16#2# => High,
      16#3# => Low_Or_Same,
      16#4# => Carry_Clear,
      16#5# => Carry_Set,
      16#6# => Not_Equal,
      16#7# => Equal,
      16#8# => No_Overflow,
      16#9# => Overflow,
      16#A# => Plus,
      16#B# => Minus,
      16#C# => Greater_Or_Equal,
      16#D# => Less,
      16#E# => Greater,
      16#F# => Less_Or_Equal);
   --
   -- The branch condition as a function of bits 8 .. 11 of the first
   -- instruction word.


   subtype Bit_T is Unsigned_Word_T range 0 .. 1;
   --
   -- The value of one bit in a word.


   function Bit (Number : Word_Bit_T; From : Unsigned_Word_T)
   return Bit_T
   --
   -- The value of the bit with the given Number, From the given word.
   --
   is
   begin

      return Field (Low => Number, High => Number, From => From);

   end Bit;


   procedure Decode (
      From        : in     Word_List_T;
      Instruction :    out Instruction_T;
      Length      :    out Instruction_Length_T)
   is

      Word_One : constant Unsigned_Word_T := From(From'First);
      -- The first instruction word.

      Word_Two : Unsigned_Word_T := 16#0000#;
      -- The second instruction word, if needed.

      High_Octet : constant Octet_T :=
         Field (Low => 8, High => 15, From => Word_One);
      -- The high octet of the first Word.
      -- This indicates the type of instruction.

      High_Octet_Word_Two : Octet_T;
      -- The high octet of the second Word.
      -- Needed to decode some of the bit-instructions.

      Low_Octet : constant Octet_T :=
         Field (Low => 0, High => 7, From => Word_One);
      -- The low octet of the first Word.
      -- This is sometimes an immediate 8-bit operand.

      First_Quartet: constant Quartet_T :=
         Field (Low => 0, High => 3, From => Word_One);
      -- The bits 0 to 3 in the first word.
      -- Often defines the second register operand.

      Second_Quartet: constant Quartet_T :=
         Field (Low => 4, High => 7, From => Word_One);
      -- The bits 4 to 7 in the first word.
      -- Often defines the first register operand.

      Third_Bit: constant Boolean :=
         Bit (Number => 3, From => Word_One) = 1;
      -- The bit 3 in word one as a boolean (1 = True).

      Seventh_Bit: constant Boolean :=
         Bit (Number => 7, From => Word_One) = 1;
      -- The bit 7 in word one as a boolean (1 = True).

      First_Quartet_Word_Two : Quartet_T;
      -- The bits 0 to 3 in word two.

      Seventh_Bit_Word_Two: Boolean;
      -- The bit 7 in word two as a boolean (1 = True).


      procedure Signal_Invalid
      --
      -- The instruction is invalid in some way.
      --
      is
      begin

         raise Invalid_Instruction;

      end Signal_Invalid;


      procedure Check (Condition : in Boolean)
      --
      -- Checks a Condition for the instruction to be valid.
      -- Signals Invalid if the Condition is False.
      --
      is
      begin

         if not Condition then

            Signal_Invalid;

         end if;

      end Check;


      procedure Get_Word_Two
      --
      -- Gets Word_Two for a two-word instruction.
      -- Also sets High_Octet_Word_Two, First_Quartet_Word_Two
      -- and Seventh_Bit_Word_Two.
      --
      is
      begin

         if From'Length < 2 then
            -- We have only one word at hand.

            Signal_Invalid;

         end if;

         Word_Two := From(From'First + 1);

         High_Octet_Word_Two :=
            Field (Low => 8, High => 15, From => Word_Two);

         First_Quartet_Word_Two :=
            Field (Low => 0, High => 3, From => Word_Two);

         Seventh_Bit_Word_Two :=
            Bit (Number => 7, From => Word_Two) = 1;

         Length := 4;

      end Get_Word_Two;


      function Branch_Offset (Low_Octet : Octet_T) return Branch_Offset_T
      --
      -- The branch offset (relative to the branch instruction itself) as
      -- decoded from the low octet of the branch instruction.
      --
      is
      begin

         if Low_Octet mod 2 /= 0 then
            -- Invalid instruction: branch offset must be even.

            raise Invalid_Instruction;

         elsif Low_Octet < 128 then
            -- Non-negative offset relative to PC+2.

            return Branch_Offset_T (Low_Octet) + 2;

         else
            -- Negative offset relative to PC+2.

            return Branch_Offset_T (Low_Octet - 128) - 126;

         end if;

      end Branch_Offset;


      function Byte_Part (Number : Quartet_T)
      return Byte_Part_T
      --
      -- The part of a register to which a coded register Number refers.
      --
      is

         Byte : Byte_Part_T;
         -- Determines the chosen byte of the register, low or high.

      begin

         case Number is
         when 0 ..  7 => Byte := High_Byte;
         when 8 .. 15 => Byte := Low_Byte;
         end case;

         return Byte;

      end Byte_Part;


      function Register_Operand (
         Code  : Quartet_T;
         Width : Width_T)
      return Operand_T
      --
      -- The register operand encoded in the given 4 bits.
      -- If Width = Word, only the lowest 3 bits are used.
      --
      is

         Number : constant Register_Number_T :=
            Register_Number_T (Field (0, 2, Code));
         -- The register number.

         Part : Register_Part_T;
         -- The register part.

      begin

         case Width is
         when Octet => Part := Byte_Part (Code);
         when Word  => Part := Word;
         end case;

         return (Kind => Register, Number => Number, Part => Part);

      end Register_Operand;


      function Sole_Register (Width : Width_T) return Operand_T
      --
      -- The register operand encoded in bits 8 .. 11 of Word_One.
      -- If Width = Word, only bits 8 .. 10 are used.
      --
      is
      begin

         return Register_Operand (Field (8, 11, Word_One), Width);

      end Sole_Register;


      function First_Register return Register_Number_T
      --
      -- The register number encoded in bits 4 .. 6 of Word_One.
      --
      is
      begin

         return Register_Number_T (Field (4, 6, Word_One));

      end First_Register;


      function First_Register (Width : Width_T) return Operand_T
      --
      -- The register operand encoded in bits 4 .. 7 of Word_One.
      -- If Width = Word, only bits 4 .. 6 are used.
      --
      is
      begin

         return Register_Operand (Second_Quartet, Width);

      end First_Register;


      function Second_Register return Register_Number_T
      --
      -- The register number encoded in bits 0 .. 2 of Word_One.
      --
      is
      begin

         return Register_Number_T (Field (0, 2, Word_One));

      end Second_Register;


      function Second_Register (Width : Width_T) return Operand_T
      --
      -- The register operand encoded in bits 0 .. 3 of Word_One.
      -- If Width = Word, only bits 0 .. 2 are used.
      --
      is
      begin

         return Register_Operand (First_Quartet, Width);

      end Second_Register;


      function Absolute_8 return Operand_T
      --
      -- The "@aa:8" operand encoded in the Low_Octet of Word_One.
      --
      is
      begin

         return (
            Kind    => Absolute,
            Address => 16#FF00# or Address_T (Low_Octet));

      end Absolute_8;


      function Absolute_16 return Operand_T
      --
      -- The "@aa:16" operand encoded in Word_Two.
      --
      is
      begin

         return (
            Kind    => Absolute,
            Address => Address_T (Word_Two));

      end Absolute_16;


      function First_Register_Indirect (
         Auto_Mod     : Auto_Mod_T  := None;
         Displacement : Offset_16_T := 0)
      return Operand_T
      --
      -- The indirect operand with address from First_Register with optional
      -- automatic pointer modification and optional displacement.
      --
      is
      begin

         return (
            Kind         => Register_Indirect,
            Pointer      => First_Register,
            Auto_Mod     => Auto_Mod,
            Displacement => Displacement);

      end First_Register_Indirect;


      function First_Register_Indirect_Auto
      return Operand_T
      --
      -- The indirect operand with address from First_Register with automatic
      -- pointer modification selected by Seventh_Bit and zero displacement.
      --
      is

         Auto : Auto_Mod_T;
         -- The automatic modification.

      begin

         if Seventh_Bit then

            Auto := Pre_Decrement;

         else

            Auto := Post_Increment;

         end if;

         return First_Register_Indirect (Auto_Mod => Auto);

      end First_Register_Indirect_Auto;


      function One_Register return One_Op_T
      --
      -- The register operand for an octet-wide one-operand instruction,
      -- as encoded in bits 0 .. 3 of Word_One.
      --
      is
      begin

         return (
            Register => Second_Register,
            Part     => Byte_Part (First_Quartet));

      end One_Register;


      function CCR_Transfer_Op return CCR_Transfer_Op_T
      --
      -- The register operand for a Load/Store-CCR instruction,
      -- as encoded in bits 0 .. 3 of Word_One.
      --
      is
      begin

         return (
            Register => Second_Register,
            Part     => Byte_Part (First_Quartet));

      end CCR_Transfer_Op;


      function Two_Registers return Two_Op_T
      --
      -- The two register operands for an octet-wide two-operand instruction.
      --
      is
      begin

         return (
            Source      => First_Register  (Octet),
            Destination => Second_Register (Octet),
            Width       => Octet);

      end Two_Registers;


      function Immediate_And_Register return Two_Op_T
      --
      -- An immediate octet operand from the Low_Octet and a Sole_Register
      -- octet-register operand, together for a two-operand instruction.
      --
      is
      begin

         return (
            Source      => (Kind => Immediate, Value => Low_Octet),
            Destination => Sole_Register (Octet),
            Width       => Octet);

      end Immediate_And_Register;


      function Bit_Registers (Inverse : Boolean) return Bit_Op_T
      --
      -- The two register operands (Bit, Host) for a bit operation with a
      -- given Inverse flag.
      --
      is
      begin

         return (
            Bit     => First_Register  (Octet),
            Host    => Second_Register (Octet),
            Inverse => Inverse);

      end Bit_Registers;


      function Bit_Immediate (Inverse : Boolean) return Bit_Op_T
      --
      -- The operands for a bit operation with an immediate bit number
      -- encoded in bits 4 .. 6 of Word_One, a register host octet given
      -- by Second_Register, and a given Inverse flag.
      --
      is
      begin

        return (
           Bit     => (Kind => Immediate, Value => Field (4, 6, Word_One)),
           Host    => Second_Register (Octet),
           Inverse => Inverse);

      end Bit_Immediate;


      function Bit_Register_Indirect (Inverse : Boolean) return Bit_Op_T
      --
      -- The operands for a bit operation with the bit number given by
      -- the octet register encoded in bits 4 .. 7 of Word_Two, a host octet
      -- addressed indirectly by First_Register_Indirect, and a given
      -- Inverse flag.
      --
      is
      begin

         return (
            Bit => Register_Operand (
               Code  => Field (4, 7, Word_Two),
               Width => Octet),
            Host    => First_Register_Indirect,
            Inverse => Inverse);

      end Bit_Register_Indirect;


      function Bit_Register_Absolute (Inverse : Boolean) return Bit_Op_T
      --
      -- The operands for a bit operation with the bit number given by
      -- the octet register encoded in bits 4 .. 7 of Word_Two, a host octet
      -- defined by a Absolute_8 address in the Low_Octet of Word_One, and
      -- a given Inverse flag.
      --
      is
      begin

         return (
            Bit => Register_Operand (
               Code  => Field (4, 7, Word_Two),
               Width => Octet),
            Host    => Absolute_8,
            Inverse => Inverse);

      end Bit_Register_Absolute;


      function Bit_Immediate_Absolute (Inverse : Boolean) return Bit_Op_T
      --
      -- The operands for a bit operation with an immediate bit number encoded
      -- in bits 4 .. 6 of Word_Two, a host octet defined by a Absolute_8
      -- address in the Low_Octet of Word_One, and a given Inverse flag.
      --
      is
      begin

         return (
            Bit     => (Kind => Immediate, Value => Field (4, 6, Word_Two)),
            Host    => Absolute_8,
            Inverse => Inverse);

      end Bit_Immediate_Absolute;


      function Bit_Immediate_Indirect (Inverse : Boolean) return Bit_Op_T
      --
      -- The operands for a bit operation with an immediate bit number encoded
      -- in bits 4 .. 6 of Word_Two, a host octet addressed indirectly by
      -- First_Register_Indirect, and a given Inverse flag.
      --
      is
      begin

         return (
             Bit     => (Kind => Immediate, Value => Field (4, 6, Word_Two)),
             Host    => First_Register_Indirect,
             Inverse => Inverse);

      end Bit_Immediate_Indirect;


      procedure Make_Move (
         Source      : in Operand_T;
         Destination : in Operand_T;
         Width       : in Width_T;
         Flip        : in Boolean := False)
      --
      -- Sets Instruction to a Move with the given operands and the
      -- given Width. If Flip is False (default), the move is from
      -- Source to Destination, otherwise (when Flip is True) the
      -- other way around, from Destination to Source.
      --
      is

         Operands : Two_Op_T;
         -- The operands for the Move instruction.

      begin

         case Flip is

         when False =>

            Operands := (
               Source      => Source,
               Destination => Destination,
               Width       => Width);

         when True =>

            Operands := (
               Source      => Destination,
               Destination => Source,
               Width       => Width);

         end case;

         Instruction := (
            Kind => Move,
            Two  => Operands);

      end Make_Move;


   begin  -- Decode

      -- The instructions, with all their different address modes, are here
      -- decoded in order after their High_Octet-bit value. In case where
      -- you need finer granularity to separate them, different bit fields
      -- are used. For example Second_Quartet, High_Octet_Word_Two etc.

      Length := 2;
      -- Unless we find that the instruction needs two words.

      case High_Octet is

      when 16#00# =>

         Check (Low_Octet = 16#00#);

         Instruction := (Kind => No_Op);

      when 16#01# =>

         Check (Low_Octet = 16#80#);

         Instruction := (Kind => Sleep);

      when 16#02# =>

         Check (Second_Quartet = 16#0#);

         Instruction := (
            Kind     => Store_CCR,
            Transfer => CCR_Transfer_Op);

      when 16#03# =>

         Check (Second_Quartet = 16#0#);

         Instruction := (
            Kind     => Load_CCR,
            Transfer => CCR_Transfer_Op);

      when 16#04# =>

         Instruction := (
            Kind   => Or_CCR,
            Update => (Value => Low_Octet));

      when 16#05# =>

         Instruction := (
            Kind   => Xor_CCR,
            Update => (Value => Low_Octet));

      when 16#06# =>

         Instruction := (
            Kind   => And_CCR,
            Update => (Value => Low_Octet));

      when 16#07# =>

         Instruction := (
            Kind   => Set_CCR,
            Update => (Value => Low_Octet));

      when 16#08# =>

         Instruction := (
            Kind => Add,
            Two  => Two_Registers);

      when 16#09# =>

         Check (not Seventh_Bit and not Third_Bit);

         Instruction := (
             Kind => Add,
             Two  => (
                Source      => First_Register  (Word),
                Destination => Second_Register (Word),
                Width       => Word));

      when 16#0A# =>

         Check (Second_Quartet = 0);

         Instruction := (
            Kind => Increment,
            One  => One_Register);

      when 16#0B# =>

         Check (not Third_Bit);

         if Second_Quartet = 16#0# then

            Instruction := (
               Kind => Add_With_Sign_Ext,
               Two  => (
                  Source      => Immediate_1,
                  Destination => Second_Register (Word),
                  Width       => Word));

         elsif Second_Quartet = 16#8# then

            Instruction := (
               Kind => Add_With_Sign_Ext,
               Two  => (
                  Source      => Immediate_2,
                  Destination => Second_Register (Word),
                  Width       => Word));

         else

            Signal_Invalid;

         end if;

      when 16#0C# =>

         Instruction := (
            Kind => Move,
            Two  => Two_Registers);

      when 16#0D# =>

         Check (not Seventh_Bit and not Third_Bit);

         Make_Move (
            Source      => First_Register  (Word),
            Destination => Second_Register (Word),
            Width       => Word);

      when 16#0E# =>

         Instruction := (
            Kind => Add_With_Carry,
            Two  => Two_Registers);

      when 16#0F# =>

         Check (Second_Quartet = 0);

         Instruction := (
            Kind => Decimal_Adjust,
            One  => One_Register);

      when 16#10# =>

         if Second_Quartet = 16#0# then

            Instruction := (
               Kind => Shift_Logical_Left,
               One  => One_Register);

         elsif Second_Quartet = 16#8# then

            Instruction := (
               Kind => Shift_Arith_Left,
               One  => One_Register);

         else

            Signal_Invalid;

         end if;

      when 16#11# =>

         if Second_Quartet = 16#0# then

            Instruction := (
               Kind => Shift_Logical_Right,
               One  => One_Register);

         elsif Second_Quartet = 16#8# then

            Instruction := (
               Kind => Shift_Arith_Right,
               One  => One_Register);

         else

            Signal_Invalid;

         end if;

      when 16#12# =>

         if Second_Quartet = 16#0# then

            Instruction := (
               Kind => Rotate_Left_Via_Carry,
               One  => One_Register);

         elsif Second_Quartet = 16#8# then

            Instruction := (
               Kind => Rotate_Left,
               One  => One_Register);

         else

            Signal_Invalid;

         end if;

      when 16#13# =>

         if Second_Quartet = 16#0# then

            Instruction := (
               Kind => Rotate_Right_Via_Carry,
               One  => One_Register);

         elsif Second_Quartet = 16#8# then

            Instruction := (
               Kind => Rotate_Right,
               One  => One_Register);

         else

            Signal_Invalid;

         end if;

      when 16#14# =>

         Instruction := (
            Kind => Bitwise_Or,
            Two  => Two_Registers);

      when 16#15# =>

         Instruction := (
             Kind => Bitwise_Xor,
             Two  => Two_Registers);

      when 16#16# =>

         Instruction := (
             Kind => Bitwise_And,
             Two  => Two_Registers);

      when 16#17# =>

         if Second_Quartet = 16#0# then

            Instruction := (
               Kind => Bitwise_Not,
               One  => One_Register);

         elsif Second_Quartet = 16#8# then

            Instruction := (
               Kind => Negate,
               One  => One_Register);

         else

            Signal_Invalid;

         end if;

      when 16#18# =>

         Instruction := (
            Kind => Subtract,
            Two  => Two_Registers);

      when 16#19# =>

         Check (not Seventh_Bit and not Third_Bit);

         Instruction := (
            Kind => Subtract,
            Two  => (
               Source      => First_Register  (Word),
               Destination => Second_Register (Word),
               Width       => Word));

      when 16#1A# =>

         Check (Second_Quartet = 0);

         Instruction := (
            Kind => Decrement,
            One  => One_Register);

      when 16#1B# =>

         Check (not Third_Bit);

         if Second_Quartet = 16#0# then

            Instruction := (
               Kind => Subtract_With_Sign_Ext,
               Two  => (
                  Source      => Immediate_1,
                  Destination => Second_Register (Word),
                  Width       => Word));

         elsif Second_Quartet = 16#8# then

            Instruction := (
               Kind => Subtract_With_Sign_Ext,
               Two  => (
                  Source      => Immediate_2,
                  Destination => Second_Register (Word),
                  Width       => Word));

         else

            Signal_Invalid;

         end if;

      when 16#1C# =>

         Instruction := (
            Kind => Compare,
            Two  => Two_Registers);

      when 16#1D# =>

         Check (not Seventh_Bit and not Third_Bit);

         Instruction := (
            Kind => Compare,
            Two  => (
               Source      => First_Register  (Word),
               Destination => Second_Register (Word),
               Width       => Word));

      when 16#1E# =>

         Instruction := (
            Kind => Subtract_With_Carry,
            Two  => Two_Registers);

      when 16#1F# =>

         Check (Second_Quartet = 0);

         Instruction := (
            Kind => Decimal_Adjust_Subtract,
            One  => One_Register);

      when 16#20# .. 16#2F# =>

         Make_Move (
            Source      => Absolute_8,
            Destination => Sole_Register (Octet),
            Width       => Octet);

      when 16#30# .. 16#3F# =>

         Make_Move (
            Source      => Sole_Register (Octet),
            Destination => Absolute_8,
            Width       => Octet);

      when 16#40# .. 16#4F# =>

         Instruction := (
             Kind      => Branch,
             Branch_Op => (
                Condition => Condition(Field (8, 11, Word_One)),
                Offset    => Branch_Offset (Low_Octet)));

      when 16#50# =>

         Check (not Third_Bit);

         Instruction := (
            Kind => Multiply,
            Two  => (
               Source      => First_Register  (Octet),
               Destination => Second_Register (Word),
               Width       => Octet));

      when 16#51# =>

         Check (not Third_Bit);

         Instruction := (
            Kind => Divide,
            Two  => (
               Source      => First_Register  (Octet),
               Destination => Second_Register (Word),
               Width       => Octet));

      when 16#52# .. 16#53# =>

         Signal_Invalid;

      when 16#54# =>

         Check (Low_Octet = 16#70#);

         Instruction := (Kind => Return_From_Subroutine);

      when 16#55# =>

         Instruction := (
            Kind              => Branch_Subroutine,
            Subroutine_Offset => Branch_Offset (Low_Octet));

      when 16#56# =>

         Check (Low_Octet = 16#70#);

         Instruction := (Kind => Return_From_Exception);

      when 16#57# .. 16#58# =>

         Signal_Invalid;

      when 16#59# =>

         Check (First_Quartet = 0 and not Seventh_Bit);

         Instruction := (
            Kind  => Jump,
            Target => (
               Kind    => Register_Indirect,
               Pointer => First_Register));

      when 16#5A# =>

         Check (Low_Octet = 0);

         Get_Word_Two;

         Instruction := (
            Kind   => Jump,
            Target => (
               Kind    => Absolute,
               Address => Address_T (Word_Two)));

      when 16#5B# =>

         Instruction := (
            Kind   => Jump,
            Target => (
               Kind           => Memory_Indirect,
               Target_Pointer => Address_T (Low_Octet)));

      when 16#5C# =>

         Signal_Invalid;

      when 16#5D# =>

         Check (First_Quartet = 0 and not Seventh_Bit);

         Instruction := (
            Kind   => Jump_Subroutine,
            Target => (
               Kind    => Register_Indirect,
               Pointer => First_Register));

      when 16#5E# =>

         Check (Low_Octet = 0);

         Get_Word_Two;

         Instruction := (
            Kind   => Jump_Subroutine,
            Target => (
               Kind    => Absolute,
               Address => Address_T (Word_Two)));

      when 16#5F# =>

         Instruction := (
            Kind   => Jump_Subroutine,
            Target => (
               Kind           => Memory_Indirect,
               Target_Pointer => Address_T (Low_Octet)));

      when 16#60# =>

         Instruction := (
            Kind => Bit_Set,
            Bit  => Bit_Registers (Inverse => False));

      when 16#61# =>

         Instruction := (
            Kind => Bit_Not,
            Bit  => Bit_Registers (Inverse => False));

      when 16#62# =>

         Instruction := (
            Kind => Bit_Clear,
            Bit  => Bit_Registers (Inverse => False));

      when 16#63# =>

         Instruction := (
            Kind => Bit_Test,
            Bit  => Bit_Registers (Inverse => False));

      when 16#64# .. 16#66# =>

         Signal_Invalid;

      when 16#67# =>

         Instruction := (
            Kind => Bit_Store,
            Bit  => (
               Bit => (
                  Kind  => Immediate,
                  Value => Field (4, 6, Word_One)),
               Host    => Second_Register (Octet),
               Inverse => Seventh_Bit));

      when 16#68# =>

         Make_Move (
            Source      => First_Register_Indirect,
            Destination => Second_Register (Octet),
            Width       => Octet,
            Flip        => Seventh_Bit);

      when 16#69# =>

         Check (not Third_Bit);

         Make_Move (
            Source      => First_Register_Indirect,
            Destination => Second_Register (Word),
            Width       => Word,
            Flip        => Seventh_Bit);

      when 16#6A# =>

         Get_Word_Two;

         case Second_Quartet is

         when 16#0# | 16#8# =>

            Make_Move (
               Source      => Absolute_16,
               Destination => Second_Register (Octet),
               Width       => Octet,
               Flip        => Seventh_Bit);

         when 16#4# =>

            Instruction := (
               Kind => Move_From_Peri,
               Two  => (
                  Source      => Absolute_16,
                  Destination => Second_Register (Octet),
                  Width       => Octet));

         when 16#C# =>

            Instruction := (
               Kind => Move_To_Peri,
               Two  => (
                  Source      => Second_Register (Octet),
                  Destination => Absolute_16,
                  Width       => Octet));

         when others =>

            Signal_Invalid;

         end case;

      when 16#6B# =>

         Check (not Third_Bit);

         Get_Word_Two;

         case Second_Quartet is

         when 16#0# | 16#8# =>

            Make_Move (
               Source      => Absolute_16,
               Destination => Second_Register (Word),
               Width       => Word,
               Flip        => Seventh_Bit);

         when others =>

            Signal_Invalid;

         end case;

      when 16#6C# =>

         Make_Move (
            Source      => First_Register_Indirect_Auto,
            Destination => Second_Register (Octet),
            Width       => Octet,
            Flip        => Seventh_Bit);

      when 16#6D# =>

         Check (not Third_Bit);

         case Second_Quartet is

         when 16#7# =>

            Instruction := (
               Kind  => Pop,
               Stack => (Register => Second_Register));

         when 16#F# =>

            Instruction := (
               Kind  => Push,
               Stack => (Register => Second_Register));

         when others =>

            Make_Move (
               Source      => First_Register_Indirect_Auto,
               Destination => Second_Register (Word),
               Width       => Word,
               Flip        => Seventh_Bit);

         end case;

      when 16#6E# =>

         Get_Word_Two;

         Make_Move (
            Source => First_Register_Indirect (
               Displacement => Address_T (Word_Two)),
            Destination => Second_Register (Octet),
            Width       => Octet,
            Flip        => Seventh_Bit);

      when 16#6F# =>

         Check (not Third_Bit);

         Get_Word_Two;

         Make_Move (
            Source => First_Register_Indirect (
               Displacement => Address_T (Word_Two)),
            Destination => Second_Register (Word),
            Width       => Word,
            Flip        => Seventh_Bit);

      when 16#70# =>

         Check (not Seventh_Bit);

         Instruction := (
            Kind => Bit_Set,
            Bit  => Bit_Immediate (Inverse => False));

      when 16#71# =>

         Check (not Seventh_Bit);

         Instruction := (
            Kind => Bit_Not,
            Bit  => Bit_Immediate (Inverse => False));

      when 16#72# =>

         Check (not Seventh_Bit);

         Instruction := (
            Kind => Bit_Clear,
            Bit  => Bit_Immediate (Inverse => False));

      when 16#73# =>

         Check (not Seventh_Bit);

         Instruction := (
             Kind => Bit_Test,
             Bit  => Bit_Immediate (Inverse => False));

      when 16#74# =>

         Instruction := (
            Kind => Bit_Or,
            Bit  => Bit_Immediate (Inverse => Seventh_Bit));

      when 16#75# =>

         Instruction := (
            Kind => Bit_Xor,
            Bit  => Bit_Immediate (Inverse => Seventh_Bit));

      when 16#76# =>

         Instruction := (
            Kind => Bit_And,
            Bit  => Bit_Immediate (Inverse => Seventh_Bit));

      when 16#77# =>

         Instruction := (
            Kind => Bit_Load,
            Bit  => Bit_Immediate (Inverse => Seventh_Bit));

      when 16#78# =>

         Signal_Invalid;

      when 16#79# =>

         Check (Second_Quartet = 0 and not Third_Bit);

         Get_Word_Two;

         Make_Move (
            Source      => (Kind => Immediate, Value => Word_Two),
            Destination => Second_Register (Word),
            Width       => Word);

      when 16#7A# =>

         Signal_Invalid;

      when 16#7B# =>

         Get_Word_Two;

         if Low_Octet = 16#5C# and Word_Two = 16#598F# then

            Instruction := (Kind => Move_Block);

         else

            Signal_Invalid;

         end if;

      when 16#7C# =>

         Check (First_Quartet = 0 and not Seventh_Bit);

         Get_Word_Two;

         Check (First_Quartet_Word_Two = 0);

         case High_Octet_Word_Two is

         when 16#63# =>

            Instruction := (
               Kind => Bit_Test,
               Bit  => Bit_Register_Indirect (Inverse => False));

         when 16#73# =>

            Check (not Seventh_Bit_Word_Two);

            Instruction := (
               Kind => Bit_Test,
               Bit  => Bit_Immediate_Indirect (Inverse => False));

         when 16#74# =>

            Instruction := (
               Kind => Bit_Or,
               Bit  => Bit_Immediate_Indirect (
                  Inverse => Seventh_Bit_Word_Two));

         when 16#75# =>

            Instruction := (
               Kind => Bit_Xor,
               Bit  => Bit_Immediate_Indirect (
                  Inverse => Seventh_Bit_Word_Two));

         when 16#76# =>

            Instruction := (
               Kind => Bit_And,
               Bit  => Bit_Immediate_Indirect (
                  Inverse => Seventh_Bit_Word_Two));

         when 16#77# =>

            Instruction := (
               Kind => Bit_Load,
               Bit  => Bit_Immediate_Indirect (
                  Inverse => Seventh_Bit_Word_Two));

         when others =>

            Signal_Invalid;

         end case;

      when 16#7D# =>

         Check (First_Quartet = 0 and not Seventh_Bit);

         Get_Word_Two;

         Check (First_Quartet_Word_Two = 0);

         case High_Octet_Word_Two is

         when 16#60# =>

            Instruction := (
               Kind => Bit_Set,
               Bit  => Bit_Register_Indirect (Inverse => False));

         when 16#61# =>

            Instruction := (
               Kind => Bit_Not,
               Bit  => Bit_Register_Indirect (Inverse => False));

         when 16#62# =>

            Instruction := (
               Kind => Bit_Clear,
               Bit  => Bit_Register_Indirect (Inverse => False));

         when 16#67# =>

            Instruction := (
               Kind => Bit_Store,
               Bit  => Bit_Immediate_Indirect (
                  Inverse => Seventh_Bit_Word_Two));

         when 16#70# =>

            Instruction := (
               Kind => Bit_Set,
               Bit  => Bit_Immediate_Indirect (Inverse => False));

         when 16#71# =>

            Check (not Seventh_Bit_Word_Two);

            Instruction := (
               Kind => Bit_Not,
               Bit  => Bit_Immediate_Indirect (Inverse => False));

         when 16#72# =>

            Check (not Seventh_Bit_Word_Two);

            Instruction := (
               Kind => Bit_Clear,
               Bit  => Bit_Immediate_Indirect (Inverse => False));

         when others =>

            Signal_Invalid;

         end case;

      when 16#7E# =>

         Get_Word_Two;

         Check (First_Quartet_Word_Two = 0);

         case High_Octet_Word_Two is

         when 16#63# =>

            Instruction := (
                Kind => Bit_Test,
                Bit  => Bit_Register_Absolute (Inverse => False));

         when 16#73# =>

            Check (not Seventh_Bit_Word_Two);

            Instruction := (
               Kind => Bit_Test,
               Bit  => Bit_Immediate_Absolute (Inverse => False));

         when 16#74# =>

            Instruction := (
               Kind => Bit_Or,
               Bit  => Bit_Immediate_Absolute (
                  Inverse => Seventh_Bit_Word_Two));

         when 16#75# =>

            Instruction := (
               Kind => Bit_Xor,
               Bit  => Bit_Immediate_Absolute (
                  Inverse => Seventh_Bit_Word_Two));

         when 16#76# =>

            Instruction := (
               Kind => Bit_And,
               Bit  => Bit_Immediate_Absolute (
                  Inverse => Seventh_Bit_Word_Two));

         when 16#77# =>

            Instruction := (
               Kind => Bit_Load,
               Bit  => Bit_Immediate_Absolute (
                  Inverse => Seventh_Bit_Word_Two));

         when others =>

            Signal_Invalid;

         end case;

      when 16#7F# =>

         Get_Word_Two;

         Check (First_Quartet_Word_Two = 0);

         case High_Octet_Word_Two is

         when 16#60# =>

            Instruction := (
                Kind => Bit_Set,
                Bit  => Bit_Register_Absolute (Inverse => False));

         when 16#61# =>

            Instruction := (
                Kind => Bit_Not,
                Bit  => Bit_Register_Absolute (Inverse => False));

         when 16#62# =>

            Check (First_Quartet_Word_Two = 0);

            Instruction := (
                Kind => Bit_Clear,
                Bit  => Bit_Register_Absolute (Inverse => False));

         when 16#67# =>

            Check (First_Quartet_Word_Two = 0);

            Instruction := (
                Kind => Bit_Store,
                Bit  => Bit_Immediate_Absolute (
                   Inverse => Seventh_Bit_Word_Two));

         when 16#70# =>

            Instruction := (
                Kind => Bit_Set,
                Bit  => Bit_Immediate_Absolute (Inverse => False));

         when 16#71# =>

            Check (not Seventh_Bit_Word_Two);

            Instruction := (
                Kind => Bit_Not,
                Bit  => Bit_Immediate_Absolute (Inverse => False));

         when 16#72# =>

            Check (not Seventh_Bit_Word_Two);

            Instruction := (
                Kind => Bit_Clear,
                Bit  => Bit_Immediate_Absolute (Inverse => False));

         when others =>

            Signal_Invalid;

         end case;

      when 16#80# .. 16#8F# =>

         Instruction := (
            Kind => Add,
            Two  => Immediate_And_Register);

      when 16#90# .. 16#9F# =>

         Instruction := (
            Kind => Add_With_Carry,
            Two  => Immediate_And_Register);

      when 16#A0# .. 16#AF# =>

         Instruction := (
            Kind => Compare,
            Two  => Immediate_And_Register);

      when 16#B0# .. 16#BF# =>

         Instruction := (
            Kind => Subtract_With_Carry,
            Two  => Immediate_And_Register);

      when 16#C0# .. 16#CF# =>

         Instruction := (
            Kind => Bitwise_Or,
            Two  => Immediate_And_Register);

      when 16#D0# .. 16#DF# =>

         Instruction := (
            Kind => Bitwise_Xor,
            Two  => Immediate_And_Register);

      when 16#E0# .. 16#EF# =>

         Instruction := (
            Kind => Bitwise_And,
            Two  => Immediate_And_Register);

      when 16#F0# .. 16#FF# =>

         Instruction := (
            Kind => Move,
            Two  => Immediate_And_Register);

      when others =>

         Signal_Invalid;

      end case;

   end Decode;


end H8_300;
