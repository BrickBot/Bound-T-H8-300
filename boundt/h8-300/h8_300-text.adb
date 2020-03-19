-- H8_300.Text (body)
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
-- $Revision: 1.6 $
-- $Date: 2015/10/26 22:19:14 $
--
-- $Log: h8_300-text.adb,v $
-- Revision 1.6  2015/10/26 22:19:14  niklas
-- Version 4b, and moved to free open-source licence.
--
-- Revision 1.5  2005/03/19 07:40:11  niklas
-- Added Target_T for use in Jump and Jump_Subroutine, removing
-- the Memory_Indirect case from Operand_T.
--
-- Revision 1.4  2004/10/22 07:53:22  niklas
-- Added Decimal_Adjust_Subtract.
-- Changed Width_Mark to a function returning String so that the
-- mark can be omitted from those instructions that have a single
-- implicit width.
-- Corrected Image for Branch_Offset_T to include a '+' for a non-
-- negative offset.
-- Moved One_Or_Two_Operand_Kind_T to the spec of H8_300, to be
-- together with the other subtypes of Instruction_Kind_T.
--
-- Revision 1.3  2004/10/11 20:01:52  niklas
-- Added Memory_Indirect operands (for Jump and Jump_Subroutine).
-- Added Move_Block instruction (EEPMOV).
-- Corrected Image for absolute-address operand to include '@'.
--
-- Revision 1.2  2004/06/24 19:46:48  niklas
-- Completed disassembly of all kinds of instructions.
--
-- Revision 1.1  2004/06/16 07:41:38  niklas
-- First version.
--


with Ada.Strings.Fixed;
with Ada.Strings.Unbounded;
with Hex;


package body H8_300.Text is


   function Trim (Item : String) return String
   --
   -- The given string with leading blanks removed.
   --
   is
   begin

      return Ada.Strings.Fixed.Trim (
         Item,
         Ada.Strings.Left);

   end Trim;


   function Image (Item : Address_T) return String
   --
   -- Hexadecimal image of operand at a static address.
   --
   is
   begin

      return "@H'" & Hex.Image (Hex.Word16_T (Item));

   end Image;


   function Image (Item : Immediate_Value_T) return String
   --
   -- Decimal image of an immediate value as operand.
   --
   is
   begin

      return '#' & Trim (Immediate_Value_T'Image (Item));

   end Image;


   function Image (Item : Register_Number_T) return String
   --
   -- The register name, "r<n>", except that R7 is "sp".
   --
   is
   begin

      if Item = SP then

         return "sp";

      else

         return 'r' & Trim (Register_Number_T'Image (Item));

      end if;

   end Image;


   function Image (
      Item : Register_Number_T;
      Part : Register_Part_T)
   return String
   --
   -- Disassembled register name.
   --
   is

      Reg : constant String := Image (Item);
      -- The register name without "part" suffix.

   begin

      case Part is
      when High_Byte => return Reg & 'h';
      when Low_Byte  => return Reg & 'l';
      when Word      => return Reg;
      end case;

   end Image;


   function Image (
      Item : Register_Number_T;
      Auto : Auto_Mod_T)
   return String
   --
   -- Disassembled register name with possible automatic modifier.
   --
   is

      Reg : constant String := Image (Item);
      -- The register number, without auto-modifier decoration.

   begin

      case Auto is
      when None            => return       Reg;
      when Post_Increment  => return       Reg & '+';
      when Pre_Decrement   => return '-' & Reg;
      end case;

   end Image;


   function Image (
      Item : Register_Number_T;
      Auto : Auto_Mod_T;
      Disp : Offset_16_T)
   return String
   --
   -- Disassembled register-indirect operand.
   --
   is

      Reg : constant String := Image (Item, Auto);
      -- The register name with auto-modifier decoration.

   begin

      if Disp = 0 then
         -- No displacement.

         return '@' & Reg;

      else
         -- Some displacement.

         return
              "@("
            & Trim (Offset_16_T'Image (Disp))
            & ','
            & Reg
            & ')';

      end if;

   end Image;


   function Image (Item : Operand_T) return String
   --
   -- Disassembly of the operand.
   --
   is
   begin

      case Item.Kind is

      when Immediate =>

         return Image (Item.Value);

      when Absolute =>

         return Image (Item.Address);

      when Register =>

         return Image (Item.Number, Item.Part);

      when Register_Indirect =>

         return Image (Item.Pointer, Item.Auto_Mod, Item.Displacement);

      end case;

   end Image;


   function Image (Item : Target_T) return String
   --
   -- Disassembly of the JMP/JSR target.
   --
   is
   begin

      case Item.Kind is

      when Absolute =>

         return Image (Item.Address);

      when Register_Indirect =>

         return '@' & Image (Item.Pointer);

      when Memory_Indirect =>

         return '@' & Image (Item.Target_Pointer);

      end case;

   end Image;


   function Image (Operation : One_Or_Two_Operand_Kind_T)
   return String
   --
   -- The assembly mnemonic of a one- or two-operand instruction.
   --
   is
   begin

      case Operation is

      -- Two operands:

      when Move                    => return "mov"   ;
      when Move_From_Peri          => return "movfpe";
      when Move_To_Peri            => return "movtpe";
      when Add                     => return "add"   ;
      when Add_With_Carry          => return "addx"  ;
      when Add_With_Sign_Ext       => return "adds"  ;
      when Subtract                => return "sub"   ;
      when Subtract_With_Carry     => return "subx"  ;
      when Subtract_With_Sign_Ext  => return "subs"  ;
      when Compare                 => return "cmp"   ;
      when Multiply                => return "mulxu" ;
      when Divide                  => return "divxu" ;
      when Bitwise_And             => return "and"   ;
      when Bitwise_Or              => return "or"    ;
      when Bitwise_Xor             => return "xor"   ;

      -- One operand:

      when Increment               => return "inc";
      when Decrement               => return "dec";
      when Negate                  => return "neg";
      when Bitwise_Not             => return "not";
      when Shift_Arith_Left        => return "shal";
      when Shift_Arith_Right       => return "shar";
      when Shift_Logical_Left      => return "shll";
      when Shift_Logical_Right     => return "shlr";
      when Rotate_Left             => return "rotl";
      when Rotate_Left_Via_Carry   => return "rotxl";
      when Rotate_Right            => return "rotr";
      when Rotate_Right_Via_Carry  => return "rotxr";
      when Decimal_Adjust          => return "daa";
      when Decimal_Adjust_Subtract => return "das";

      end case;

   end Image;


   function Width_Mark (
      Operation : Two_Operand_Kind_T;
      Width     : Width_T)
   return String
   --
   -- The mnemonic suffix that indicates an 8-bit or 16-bit wide operation.
   --
   is
   begin

      case Operation is

      when Move
         | Add
         | Subtract
         | Compare =>
         -- Word or byte operands. Mark which.

         case Width is
         when Octet => return ".b";
         when Word  => return ".w";
         end case;

      when Move_From_Peri
         | Move_To_Peri
         | Add_With_Carry
         | Add_With_Sign_Ext
         | Subtract_With_Carry
         | Subtract_With_Sign_Ext
         | Multiply
         | Divide
         | Bitwise_And
         | Bitwise_Or
         | Bitwise_Xor =>
         -- Only applies to one (implicit) width. No mark.

         return "";

      end case;

   end Width_Mark;


   function Image (
      Operation : Two_Operand_Kind_T;
      Operands  : Two_Op_T)
   return String
   --
   -- Disassembly of a two-operand instruction.
   --
   is
   begin

      return
           Image (Operation)
         & Width_Mark (Operation, Operands.Width)
         & ' '
         & Image (Operands.Source)
         & ','
         & Image (Operands.Destination);

   end Image;


   function Image (
      Operation : One_Operand_Kind_T;
      Operand   : One_Op_T)
   return String
   --
   -- Disassembly of a one-operand instruction.
   --
   is
   begin

      return
           Image (Operation)
         & ' '
         & Image (Operand.Register, Operand.Part);

   end Image;


   function Image (
      Operation : CCR_Transfer_Kind_T;
      Operand   : CCR_Transfer_Op_T)
   return String
   --
   -- Disassembly of a CCR transfer (load or store) operation where
   -- the source or target operand is a general 8-bit register.
   --
   is

      Reg : constant String := Image (
         Item => Operand.Register,
         Part => Operand.Part);
      -- The disassembled source or target.

   begin

      case Operation is
      when Load_CCR  => return "ldc " & Reg & ",ccr";
      when Store_CCR => return "stc ccr," & Reg;
      end case;

   end Image;


   function Image (
      Operation : CCR_Update_Kind_T;
      Imm_Value : Octet_T)
   return String
   --
   -- Disassembly of an operation that updates the CCR by a logical
   -- bit-wise Operation with an Immediate Value as the second operand.
   --
   is

      Operands : constant String :=
         " #H'" & Hex.Image (Hex.Byte_T (Imm_Value)) & ",ccr";
      --
      -- The immediate operand (in hex) and the impliede "ccr" operand.

   begin

      case Operation is
      when Set_CCR => return "ldc"  & Operands;
      when And_CCR => return "andc" & Operands;
      when Or_CCR  => return "orc"  & Operands;
      when Xor_CCR => return "xorc" & Operands;
      end case;

   end Image;


   function Image (
      Operation : Bit_Kind_T;
      Operands  : Bit_Op_T)
   return String
   --
   -- Disassembly of a bit operation.
   --
   is
      use Ada.Strings.Unbounded;

      Mnem : Unbounded_String := To_Unbounded_String ("b");
      -- The mnemonic, built up incrementally.
      -- All of them start with 'b'.

   begin

      if  Operation in Bit_Invertible_Kind_T
      and Operands.Inverse
      then
         -- Add an 'i' after the 'b' to mark inversion.

         Append (Mnem, "i");

      end if;

      case Operation is
      when Bit_Set   => Append (Mnem, "set");
      when Bit_Clear => Append (Mnem, "clr");
      when Bit_Not   => Append (Mnem, "not");
      when Bit_Test  => Append (Mnem, "tst");
      when Bit_Load  => Append (Mnem, "ld" );
      when Bit_Store => Append (Mnem, "st" );
      when Bit_And   => Append (Mnem, "and");
      when Bit_Or    => Append (Mnem, "or" );
      when Bit_Xor   => Append (Mnem, "xor");
      end case;

      return
           To_String (Mnem)
         & ' '
         & Image (Operands.Bit)
         & ','
         & Image (Operands.Host);

   end Image;


   function Image (Item : Branch_Offset_T)
   return String
   --
   -- Disassembly of a branch or branch-subroutine target address,
   -- given as a octet offset to the address of the source
   -- instruction.
   --
   is

      Number : String := Branch_Offset_T'Image (Item);
      -- The numerical part.

   begin

      if Item >= 0 then
         -- We need a plus sign.

         Number(Number'First) := '+';

      -- else
      --    There is a minus sign already in place.

      end if;

      return '*' & Number;
      --
      -- TBC if the assembler syntax uses '*' as the address
      -- of the current instruction.

   end Image;


   subtype Cond_Mnem_T is String(1 .. 2);
   --
   -- The Condition mnemonics are all two characters.


   Cond_Mnem : constant array (Condition_T) of Cond_Mnem_T := (
      Always           => "ra",  -- or "t"
      Never            => "rn",  -- or "f"
      High             => "hi",
      Low_Or_Same      => "ls",
      Carry_Clear      => "hs",  -- or "cc"
      Carry_Set        => "lo",  -- or "cs"
      Not_Equal        => "ne",
      Equal            => "eq",
      No_Overflow      => "vc",
      Overflow         => "vs",
      Plus             => "pl",
      Minus            => "mi",
      Greater_Or_Equal => "ge",
      Less             => "lt",
      Greater          => "gt",
      Less_Or_Equal    => "le");
   --
   -- The mnemonics for the condition codes.
   -- Some of them have alternative forms; we have chosen to
   -- use the "arithmetically" oriented forms as if the conditions
   -- come from a comparison of two numbers.


   function Image (Item : Cond_Branch_Op_T)
   return String
   --
   -- Disassembly of a conditional branch operation, including the
   -- unconditional case too.
   --
   is
   begin

      return
           'b'
         & Cond_Mnem(Item.Condition)
         & ' '
         & Image (Item.Offset);

   end Image;


   --
   --   Main disassembler function
   --


   function Image (Instruction : Instruction_T) return String
   is
   begin

      case Instruction.Kind is

      when Two_Operand_Kind_T =>

         return Image (
            Operation => Instruction.Kind,
            Operands  => Instruction.Two);

      when One_Operand_Kind_T =>

         return Image (
            Operation => Instruction.Kind,
            Operand   => Instruction.One);

      when Push =>

         return "push " & Image (Instruction.Stack.Register);

      when Pop =>

         return "pop " & Image (Instruction.Stack.Register);

      when CCR_Transfer_Kind_T =>

         return Image (
            Operation => Instruction.Kind,
            Operand   => Instruction.Transfer);

      when CCR_Update_Kind_T =>

         return Image (
            Operation => Instruction.Kind,
            Imm_Value => Instruction.Update.Value);

      when Bit_Kind_T =>

         return Image (
            Operation => Instruction.Kind,
            Operands  => Instruction.Bit);

      when Branch =>

         return Image (Instruction.Branch_Op);

      when Branch_Subroutine =>

         return "bsr " & Image (Instruction.Subroutine_Offset);

      when Jump =>

         return "jmp " & Image (Instruction.Target);

      when Jump_Subroutine =>

         return "jsr " & Image (Instruction.Target);

      when Return_From_Subroutine =>

         return "rts";

      when Return_From_Exception =>

         return "rte";

      when Move_Block =>

         return "eepmov";

      when No_Op =>

         return "nop";

      when Sleep =>

         return "sleep";

      when Undefined =>

         return "<invalid>";

      end case;

   end Image;


end H8_300.Text;
