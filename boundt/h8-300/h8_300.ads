-- H8_300 (decl)
--
-- Renesas (ex Hitachi) H8/300 architecture machine instruction structures.
--
-- This package provides types that represent all the instructions in
-- the H8/300 instruction set, for the purpose of decoding and understanding
-- such instructions in the context of static code analysis.
--
-- The goal is to represent all the H8/300 instructions in a form that
-- closely corresponds to the machine instruction level but elides the
-- representation in terms of 16-bit instruction words, with their various
-- lay-outs and encodings. Thus, an instruction is represented precisely
-- enough to define its effects and resource consumption (including
-- execution time), and to allow its disassembly. However, the original
-- instruction word may not be uniquely recovered when there are different
-- instructions that are functionally equivalent and have the same
-- assembly-language form.
--
-- Authors:
--    Samuel Petersson, Mälardalen University
--    Niklas Holsti, Tidorum Ltd
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
-- $Log: h8_300.ads,v $
-- Revision 1.12  2015/10/26 22:19:14  niklas
-- Version 4b, and moved to free open-source licence.
--
-- Revision 1.11  2009/12/02 14:20:01  niklas
-- BT-CH-0188: H8/300 updates for bit-widths and Word_T.
--
-- Revision 1.10  2009-07-15 12:00:37  niklas
-- Added Stack_Operand_Octets.
--
-- Revision 1.9  2005-03-22 20:36:14  niklas
-- Added Real_Mod_T and Operand_Octets to support arithmetic analysis
-- in the Decoder.
-- Added Vector_T to support Processor.Timing.Time_To_Vector.
--
-- Revision 1.8  2005/03/19 07:38:28  niklas
-- Added Target_T for use in Jump and Jump_Subroutine, removing
-- the Memory_Indirect case from Operand_T.
-- Changed the order of bit instructions in Instruction_Kind_T
-- to allow the correct definition of Bit_Kind_Write_T and
-- Bit_Invertible_Kind_T.
-- Added the Negation array to help with conditional branches.
--
-- Revision 1.7  2005/01/26 18:31:05  niklas
-- Added Location_T and Memory_Location_T.
--
-- Revision 1.6  2005/01/25 08:24:26  niklas
-- Samuel re-arranged the bit-instructions in Instruction_Kind_T for
-- the sake of type Bit_Kind_Write_T.
--
-- Revision 1.5  2004/11/19 19:54:40  niklas
-- Added pragma Elaborate_All (Bits).
--
-- Revision 1.4  2004/10/22 07:47:19  niklas
-- Added Decimal_Adjust_Subtract.
--
-- Revision 1.3  2004/10/11 20:00:47  niklas
-- Added Memory_Indirect operands (for Jump and Jump_Subroutine).
-- Added Move_Block instruction (EEPMOV).
--
-- Revision 1.2  2004/06/24 19:44:50  niklas
-- Corrected description of Branch_Offset_T.
-- Corrected One_Op_T to reflect the fact that all instructions in
-- the "one operand" group have an 8-bit general register as operand.
-- Corrected Condition_T for naming convention (suffix _T, not _C)
-- and to include the condition Low_Or_Same.
-- Modified the Branch_Subroutine case of Instruction_T to have just
-- a branch offset operand, no condition, as "bsr" is always
-- unconditional.
--
-- Revision 1.1  2004/06/16 07:41:38  niklas
-- First version.
--


with Bits;
with Interfaces;

pragma Elaborate_All (Bits);


package H8_300 is


   --
   ---   Instruction components
   --
   -- The following types represents the components (parts, fields)
   -- of H8/300 instructions.
   --
   -- For all enumeration types, assume that the encoding is significant.
   -- If a representation clause is not explicitly given, the default
   -- representation encoding is valid and significant.


   type Unsigned_Word_T is new Interfaces.Unsigned_16;
   --
   -- Unsigned 16-bit word.
   -- H8/300 instructions are usually 16 bits long. Some instructions
   -- have a trailing 16-bit literal (immediate) operand or opcode
   -- extension.


   Word_Bits : constant := 16;
   --
   -- The number of bits in a word.


   subtype Immediate_Value_T is Unsigned_Word_T;
   --
   -- An immediate operand can be up to 16 bits in length. It is defined
   -- here as unsigned, although immediate operands are probably often
   -- signed.


   Octet_Bits : constant := 8;
   --
   -- The number of bits in an octet.


   subtype Octet_T is Unsigned_Word_T range 0 .. 2 ** Octet_Bits - 1;
   --
   -- An 8-bit quantity (immediate operand) that can be interpreted as
   -- a signed or unsigned number.


   subtype Sextet_T is Unsigned_Word_T range 0 .. 2**6 - 1;
   --
   -- A 6-bit unsigned quantity (immediate operand).


   subtype Quartet_T is Unsigned_Word_T range 0 .. 2**4 - 1;
   --
   -- A 4-bit unsigned quantity.


   subtype Trio_T is Unsigned_Word_T range 0 .. 2**3 - 1;
   --
   -- A 3-bit unsigned quantity.


   type Word_List_T is array (Natural range <>) of Unsigned_Word_T;
   --
   -- A sequence of unsigned 16-bit words, for each instruction words.


   function Field is new Bits (Value_T => Unsigned_Word_T);
   --
   -- Picks a bit-field from an instruction word.


   type Address_T is new Interfaces.Unsigned_16;
   --
   -- An address, of code or data.
   --
   -- The H8/300 is a "von Neumann" architecture where code and data
   -- share the same address space. The addressable unit is one octet
   -- and the address is 16 bits long.
   --
   -- Instructions are 16 or 32 bits in length and always start at
   -- an even address.
   --
   -- Data can be 8 or 16 bits in length; 16-bit accesses always start
   -- at an even address.
   --
   -- Byte order is big-endian: more significant byte (octet) first,
   -- at the even address, then the less significant byte (octet) at
   -- the odd address.


   Address_Bits : constant := 16;
   --
   -- The number of bits in an address.


   type Location_T is (
      Register,
      Internal,
      External,
      Reserved);
   --
   -- Whether a memory address is mapped to internal (on-chip) memory,
   -- to the on-chip register field, to external (off-chip) memory, or
   -- is reserved (unusable, absent).
   --
   -- The location of an address determines the access time.


   subtype Memory_Location_T is Location_T range Internal .. External;
   --
   -- The possible locations for ordinary, real memory, RAM or ROM
   -- but not control registers.


   subtype Octet_Count_T is Address_T;
   --
   -- A number of octets in the memory.


   subtype Instruction_Length_T is Octet_Count_T range 2 .. 4;
   --
   -- An instruction contains one real instruction word and in some
   -- cases a second 16-bit word that contains an immediate operand
   -- or additional instruction specification. Thus, the length is
   -- either 2 or 4 octets.


   subtype Offset_16_T is Octet_Count_T;
   --
   -- A 16-bit address offset (displacement).


   subtype Bit_Value_T is Unsigned_Word_T range 0 .. 1;
   --
   -- The value of one instruction or data bit.


   subtype Word_Bit_T is Natural range 0 .. Word_Bits - 1;
   --
   -- The number of a bit in a 16-bit word.
   -- The least significant bit is number zero.


   subtype Octet_Bit_T is Natural range 0 .. Octet_Bits - 1;
   --
   -- The number of a bit in an octet.
   -- The least significant bit is number zero.


   type Flag_T is (C, V, Z, N, U4, H, U6, I);
   --
   -- The status flags in the Condition Code Register (CCR), listed in
   -- order of increasing bit number, when bit 0 is the least significant
   -- bit. Thus, 'Pos and 'Val map to and from bit numbers.
   --
   -- C   Carry
   -- V   Overflow
   -- Z   Zero
   -- N   Negative
   -- U4  User-defined
   -- H   Half-carry
   -- U6  User-defined
   -- I   Interrupt mask


   type Register_Number_T is range 0 .. 7;
   --
   -- The number of a general register. However, one must also say
   -- whether the whole word or one of the octets is meant.


   SP : constant Register_Number_T := 7;
   --
   -- Register 7 (R7) is the Stack Pointer (SP) with a special role
   -- in subroutine calls, returns and interrupts.


   type Register_Part_T is (High_Byte, Low_Byte, Word);
   --
   -- The part of a register that is meant. One must also give a
   -- register number.
   --
   -- High_Byte   The high byte (bits 8 .. 15).
   -- Low_Byte    The low byte  (bits 0 .. 7).
   -- Word        The whole word (bits 0 .. 15).


   subtype Byte_Part_T is Register_Part_T range High_Byte .. Low_Byte;
   --
   -- A byte-sized part of a register.


   type Auto_Mod_T is (None, Post_Increment, Pre_Decrement);
   --
   -- A possible automatic update of a pointer register when
   -- using the register-indirect addressing mode. The amount of
   -- increment or decrement depends on the width of the operand,
   -- see below.


   subtype Real_Mod_T is Auto_Mod_T range Post_Increment .. Pre_Decrement;
   --
   -- When a real automatic update occurs.


   type Width_T is (Octet, Word);
   --
   -- Whether the operands are 8-bit octets (bytes) or 16-bit words.
   -- The signedness is not represented (usually not known).


   Operand_Octets : constant array (Width_T) of Octet_Count_T := (
      Octet => 1,
      Word  => 2);
   --
   -- The number of octets in an operand of a given width.
   --
   -- This defines the change in a pointer register when subjected
   -- to Post_Increment or Pre_Decrement.


   Stack_Pointer_Bits : constant := 16;
   --
   -- The number of bits in the Stack Pointer register.


   Stack_Operand_Octets : constant Octet_Count_T := Operand_Octets(Word);
   Stack_Operand_Bits   : constant               := Word_Bits;
   --
   -- All SP stack operands are words.
   -- This includes the return address for subprogram calls.


   type Operand_Kind_T is (
      Immediate,
      Absolute,
      Register,
      Register_Indirect);
   --
   -- The ways in which an instruction can identify one of its operands.
   -- This is a slight abstraction of the H8/300 addressing modes; we
   -- elide the size (number of bits) of the displacements, immediate
   -- operands and absolute addresses. As a side effect, the abstract
   -- form of an instruction can potentially use addressing modes for
   -- which there is no space in the actual instruction word, for
   -- example a BAND with an absolute 16-bit address.
   --
   -- Immediate
   --    The operand is a static constant value.
   -- Absolute
   --    The operand is a memory octet or word at a statically known
   --    and constant address.
   -- Register
   --    The operand is a byte or word register, statically known.
   -- Register_Indirect
   --    The operand is a memory octet or word, at an address that
   --    is held in a word register, with a possible displacement and
   --    a possible pre-decrement or post-increment of the pointer
   --    register.


   type Operand_T (Kind : Operand_Kind_T := Immediate) is record

      case Kind is

      when Immediate =>

         Value : Immediate_Value_T;

      when Absolute =>

         Address : Address_T;

      when Register =>

         Number : Register_Number_T;
         Part   : Register_Part_T;

      when Register_Indirect =>

         Pointer      : Register_Number_T;
         Auto_Mod     : Auto_Mod_T;
         Displacement : Offset_16_T;

      end case;

   end record;
   --
   -- Specifies an operand for an instruction, to be a source of data,
   -- a result destination, or both.


   type Target_Kind_T is (
      Absolute,
      Register_Indirect,
      Memory_Indirect);
   --
   -- The ways in which a Jump or Jump_Subroutine instruction can
   -- identify its target address.
   --
   -- Absolute
   --    The target is a statically known and constant address.
   -- Register_Indirect
   --    The target address is held in a word register.
   -- Memory_Indirect
   --    The target address is shown by a memory word with a statically
   --    known address (a vector slot).


   type Target_T (Kind : Target_Kind_T := Absolute) is record

      case Kind is

      when Absolute =>

         Address : Address_T;

      when Register_Indirect =>

         Pointer : Register_Number_T;

      when Memory_Indirect =>

         Target_Pointer : Address_T;

      end case;

   end record;
   --
   -- Specifies the target address of a Jump or Jump_Subroutine.


   subtype Vector_T is Target_T (Kind => Memory_Indirect);
   --
   -- A Jump/Jump_Subroutine target that is "vectored" through an
   -- address (function pointer) in memory.


   type Branch_Offset_T is range -126 .. +128;
   --
   -- The PC-relative branch offset, in octets relative to the address
   -- of the branching instruction. In the machine encoding, the offset
   -- is an even, signed, 8-bit octet number that is applied to the
   -- incremented PC (address of instruction after branch).


   --
   ---   Instruction kinds, depending also on the kinds of operands
   --


   type Instruction_Kind_T is (

      -- Two-operand instructions:

      Move,
      Move_From_Peri,
      Move_To_Peri,

      Add,
      Add_With_Carry,
      Add_With_Sign_Ext,
      Subtract,
      Subtract_With_Carry,
      Subtract_With_Sign_Ext,
      Compare,

      Multiply,
      Divide,

      Bitwise_And,
      Bitwise_Or,
      Bitwise_Xor,

      -- One-operand instructions:

      Increment,
      Decrement,
      Negate,
      Bitwise_Not,

      Shift_Arith_Left,
      Shift_Arith_Right,
      Shift_Logical_Left,
      Shift_Logical_Right,

      Rotate_Left,
      Rotate_Left_Via_Carry,
      Rotate_Right,
      Rotate_Right_Via_Carry,

      Decimal_Adjust,
      Decimal_Adjust_Subtract,

      -- Stack access operations:

      Push,
      Pop,

      -- Condition-Code-Register transfer operations:

      Load_CCR,
      Store_CCR,

      -- Condition-Code-Register update operations:

      Set_CCR,
      And_CCR,
      Or_CCR,
      Xor_CCR,

      -- Bit operations:

      Bit_Clear,
      Bit_Not,
      Bit_Set,
      Bit_Store,
      Bit_Load,
      Bit_And,
      Bit_Or,
      Bit_Xor,
      Bit_Test,

      -- Control operations:

      Branch,
      Branch_Subroutine,
      Jump,
      Jump_Subroutine,

      Return_From_Subroutine,
      Return_From_Exception,

      -- Special operations:

      Move_Block,
      No_Op,
      Sleep,

      Undefined);
   --
   -- All the kinds of H8/300 instructions.


   subtype Two_Operand_Kind_T is
      Instruction_Kind_T range Move .. Bitwise_Xor;
   --
   -- All the two-operand instructions.


   subtype One_Operand_Kind_T is
      Instruction_Kind_T range Increment .. Decimal_Adjust_Subtract;
   --
   -- All the one-operand instructions.
   -- The operand is always an 8-bit general register.


   subtype One_Or_Two_Operand_Kind_T is
      Instruction_Kind_T range Move .. Decimal_Adjust_Subtract;
   --
   -- All the one-operand and two-operand instructions.


   subtype Stack_Kind_T is
      Instruction_Kind_T range Push .. Pop;
   --
   -- The stack-access instructions.


   subtype CCR_Transfer_Kind_T is
      Instruction_Kind_T range Load_CCR .. Store_CCR;
   --
   -- The instructions that transfer the CCR from or to a register.


   subtype CCR_Update_Kind_T is
      Instruction_Kind_T range Set_CCR .. Xor_CCR;
   --
   -- The instructions that modify the CCR by operating on it
   -- with an 8-bit immediate operand.


   subtype Bit_Kind_T is
      Instruction_Kind_T range Bit_Clear .. Bit_Test;
   --
   -- The bit-operation instructions.


   subtype Bit_Kind_Write_T is
      Bit_Kind_T range Bit_Clear .. Bit_Store;

   -- The bit-operation instructions that write the result to the
   -- host octet (register or memory) rather than to the C or Z flag.


   subtype Bit_Invertible_Kind_T is
      Bit_Kind_T range Bit_Store .. Bit_Xor;
   --
   -- The bit-operation instructions where the result can be logically
   -- inverted.


   subtype Jump_Kind_T is
      Instruction_Kind_T range Jump .. Jump_Subroutine;
   --
   -- Jump-instructions.
   --


   --
   ---   Instruction operands
   --


   type Two_Op_T is record
      Source      : Operand_T;
      Destination : Operand_T;
      Width       : Width_T;
   end record;
   --
   -- The operands for an instruction with two operands of various kinds.
   --
   -- Source
   --    The source operand that is read.
   -- Destination
   --    Destination operand that is usually written (except for CMP)
   --    and is usually also read (except for MOV).
   -- Width
   --    The data width.
   --
   -- The Destination can of course not be an Immediate operand.


   type One_Op_T is record
      Register : Register_Number_T;
      Part     : Byte_Part_T;
   end record;
   --
   -- The operand for an instruction with one operand, which is always
   -- an 8-bit general register.
   --
   -- Register
   --    The number of the single operand register.
   -- Part
   --    Chooses the low or high byte of the register.


   type Bit_Op_T is record
      Bit     : Operand_T;
      Host    : Operand_T;
      Inverse : Boolean;
   end record;
   --
   -- The operands for an instruction that operates on one bit of an
   -- octet operand.
   --
   -- Bit
   --    This operand defines the bit number.
   -- Host
   --    This operand is the octet in which the bit lies.
   -- Inverse
   --    Whether the bit is used/stored with inversion.
   --    Significant only for Bit_Invertible_Kind_T instructions.
   --
   -- The Host can of course not be an Immediate operand.
   --
   -- The Carry and Zero flags may be implicit third operands.


   type Stack_Op_T is record
      Register : Register_Number_T;
   end record;
   --
   -- The operand(s) for a stack operation: Push or Pop.
   -- The Stack Pointer (SP = R7) is a 16-bit pointer and points to
   -- the top (filled) stack location. The stack grows downward in
   -- memory (Push decreases SP). All data are stacked as 16-bit words.
   -- Push and Pop are always word operations, and it would be risky
   -- TBC to use MOV.B to push/pop octets while interrupts are enabled
   -- because then an interrupt could TBC update the SP in a peculiar
   -- way, since it assumes that SP is an even address TBC.
   --
   -- It is probably not wise to try to Push or Pop SP itself.


   type CCR_Transfer_Op_T is record
      Register : Register_Number_T;
      Part     : Byte_Part_T;
   end record;
   --
   -- The operands for an instruction that stores or loads the CCR to
   -- or from a byte register.


   type CCR_Update_Op_T is record
      Value : Octet_T;
   end record;
   --
   -- The operand(s) for an instruction updates the CCR by a logical
   -- operation using an 8-bit immediate operand.


   type Condition_T is (
      Always,
      Never,
      High,
      Low_Or_Same,
      Carry_Clear,  -- High_Or_Same
      Carry_Set,    -- Low
      Not_Equal,
      Equal,
      No_Overflow,
      Overflow,
      Plus,
      Minus,
      Greater_Or_Equal,
      Less,
      Greater,
      Less_Or_Equal);
   --
   -- The conditions for conditional branch (Bcc).


   Negation : constant array (Condition_T) of Condition_T := (
      Always           => Never,
      Never            => Always,
      High             => Low_Or_Same,
      Low_Or_Same      => High,
      Carry_Clear      => Carry_Set,
      Carry_Set        => Carry_Clear,
      Not_Equal        => Equal,
      Equal            => Not_Equal,
      No_Overflow      => Overflow,
      Overflow         => No_Overflow,
      Plus             => Minus,
      Minus            => Plus,
      Greater_Or_Equal => Less,
      Less             => Greater_Or_Equal,
      Greater          => Less_Or_Equal,
      Less_Or_Equal    => Greater);
   --
   -- The negation (logical inverse) of a Condition.


   type Cond_Branch_Op_T is record
      Condition : Condition_T;
      Offset    : Branch_Offset_T;
   end record;
   --
   -- The operands for a conditional branch instruction.


   --
   ---   An (abstracted) instruction
   --


   type Instruction_T (Kind : Instruction_Kind_T := Undefined)
   is record

      case Kind is

       when Two_Operand_Kind_T =>

          Two : Two_Op_T;

       when One_Operand_Kind_T =>

          One : One_Op_T;

       when Stack_Kind_T =>

          Stack : Stack_Op_T;

       when CCR_Transfer_Kind_T =>

          Transfer : CCR_Transfer_Op_T;

       when CCR_Update_Kind_T =>

          Update : CCR_Update_Op_T;

       when Bit_Kind_T =>

          Bit : Bit_Op_T;

       when Branch =>

          Branch_Op : Cond_Branch_Op_T;

       when Branch_Subroutine =>

          Subroutine_Offset : Branch_Offset_T;

       when Jump
          | Jump_Subroutine =>

          Target : Target_T;

       when Move_Block
          | No_Op
          | Return_From_Subroutine
          | Return_From_Exception
          | Sleep
          | Undefined =>

          null;

      end case;

   end record;
   --
   -- An H8/300 instruction in partly abstracted form.
   --
   -- The separation of instructions is intended to be complete enough
   -- to generate the assembly-language form of the instruction, but
   -- it is not always possible to generate the binary instruction
   -- because there are different instructions that are functionally
   -- equivalent and have the same representation as Instruction_Ts.


   Invalid_Instruction : exception;
   --
   -- Raised by an attempt to decode an invalid instruction.


   procedure Decode (
      From        : in     Word_List_T;
      Instruction :    out Instruction_T;
      Length      :    out Instruction_Length_T);
   --
   -- Decodes the H8/300 instruction defined by the given 16-bit instruction
   -- word sequence, using one or two words as proper for the instruction.
   --
   -- Raises Invalid_Instruction if the words do not represent a valid
   -- instruction, also when the first word is a valid first word of a
   -- two-word instruction, but there is no second word in the From list.
   --
   -- Also returns the actual instruction length (2 or 4 octets).


end H8_300;
