-- H8_300.Verify (body)
--
-- Author: Niklas Holsti
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
-- $Revision: 1.9 $
-- $Date: 2015/10/26 22:19:14 $
--
-- $Log: h8_300-verify.adb,v $
-- Revision 1.9  2015/10/26 22:19:14  niklas
-- Version 4b, and moved to free open-source licence.
--
-- Revision 1.8  2006/02/04 09:13:10  niklas
-- Corrected procedure Try to check Decoded.Kind instead of
-- Instr.Kind for an Undefined instruction.
--
-- Revision 1.7  2005/10/24 11:33:44  niklas
-- Changed Check_All_Words from a global option to a parameter of
-- Try_All so that it can be controlled eg by command-line options.
-- Using Compiler.Native_Natural to count errors.
--
-- Revision 1.6  2005/04/20 19:29:49  niklas
-- Added "and" for two registers.
--
-- Revision 1.5  2005/03/19 07:40:43  niklas
-- Added Target_T for use in Jump and Jump_Subroutine, removing
-- the Memory_Indirect case from Operand_T.
--
-- Revision 1.4  2005/03/15 19:54:24  niklas
-- Added one more "bsr" case.
--
-- Revision 1.3  2004/12/03 20:18:11  niklas
-- Added Try_Invalid procedures to verify checks of non-coding bits.
-- Modified output format to help.
-- Disabled Check_All_Words by default, because this is now rather
-- slow as H8_300.Decode reports invalid instructions with exceptions.
--
-- Revision 1.2  2004/11/19 20:54:15  niklas
-- Changes by Samuel Petersson:
-- Set Check_All_Words to True for full test.
-- Corrected encoding and Width for "addx #255,r0h".
-- Corrected encoding for "bclr r1l,@r0".
-- Changed Width of Divide and Multiply from Word to Octet.
-- Corrected encoding for "mulxu r3h,r1".
--
-- Revision 1.1  2004/10/22 07:53:43  niklas
-- First version.
--


with Ada.Exceptions;
with Ada.Text_IO;

with Compiler;
with H8_300.Text;


package body H8_300.Verify is

   use Ada.Text_IO;


   package Word_IO is new Ada.Text_IO.Modular_IO (Unsigned_Word_T);

   use Word_IO;


   -- Options:


   Check_Decode : constant Boolean := True;
   --
   -- Whether to check the operation H8_300.Decode and its result.
   -- If this option is False, only H8_300.Text.Image is checked.


   -- End of options.


   Error_Mark : constant String := "    **** ";
   -- Signals an error message.


   type Error_Count_T is new Compiler.Native_Natural;
   --
   -- For counting the number of errors.


   Error_Count : Error_Count_T := 0;
   -- Number of errors reported.


   Undefined_Instruction : exception;
   --
   -- When H8_300.Decode yields Instruction.Kind = Undefined.


   procedure Error (Message : in String)
   --
   -- Reports and counts an error.
   --
   is
   begin

      Error_Count := Error_Count + 1;

      Put_Line (
           Error_Mark
         & Error_Count_T'Image (Error_Count)
         & " : "
         & Message);

   end Error;


   Msg_Col : constant := 30;
   --
   -- Output column for messages, after hex Words.


   procedure Show (Words : in Word_List_T)
   --
   -- Displays the Words in hex.
   --
   is
   begin

      for W in Words'Range loop

        Put (Words(W), Base => 16);

        Put (' ');

      end loop;

   end Show;


   procedure Prefix_Message (Words : in Word_List_T)
   --
   -- Displays the Words in hex, then spaces to Msg_Col.
   --
   is
   begin

      Show (Words);

      Set_Col (Msg_Col);

   end Prefix_Message;


   procedure Try (
      Words : in     Word_List_T;
      Valid : in out Boolean)
   --
   -- Decodes the given words and reports exceptions.
   --
   is

      Instr : Instruction_T;
      -- The decoded instruction.

      Length : Instruction_Length_T;
      -- The length of the instruction.


   begin  -- Try

      Decode (From => Words, Instruction => Instr, Length => Length);

      if Instr.Kind = Undefined then

         raise Undefined_Instruction;

      end if;

      if not Valid then
         -- Now we have again valid Words.

         Prefix_Message (Words);

         Put_Line (
              "Valid"
            & Natural'Image (Words'Length)
            & "-word instruction");

      end if;

      Valid := True;

   exception

   when Invalid_Instruction =>

      if Valid then
         -- First invalid Words for a while.

         Prefix_Message (Words);

         Put_Line (
              "Invalid"
            & Natural'Image (Words'Length)
            & "-word instruction");

      end if;

      Valid := False;

   when X : others =>

      Prefix_Message (Words);

      Put_Line (Ada.Exceptions.Exception_Information (X));

      Error ("Exception.");

      Valid := False;

   end Try;


   procedure Try_Text (
      Words  : in Word_List_T;
      Stage  : in String;
      Source : in String;
      Instr  : in Instruction_T)
   --
   -- Disassembles the given Instruction and compares against
   -- the expected Source string. Error reports start with the
   -- Words in hex, followed by Stage.
   --
   is

      Disass : constant String := H8_300.Text.Image (Instr);
      -- The disassembled instruction.

   begin

      Put_Line (Stage & ": " & Disass);

      if Disass /= Source then

         Prefix_Message (Words);

         Put_Line (
              Stage
            & " disassembled as """
            & Disass
            & '"');

         Error (Stage & " disassembly error");

      end if;

   exception

   when X : others =>

      Prefix_Message (Words);

      Put_Line (Ada.Exceptions.Exception_Information (X));

      Error ("Exception.");

   end Try_Text;


   procedure Try (
      Source : in String;
      Words  : in Word_List_T;
      Instr  : in Instruction_T)
   is

      Decoded : Instruction_T;
      -- The decoded instruction.

      Length : Instruction_Length_T;
      -- The length of the Decoded instruction.

   begin

      Put_Line ("Expect  : " & Source);

      Try_Text (
         Words  => Words,
         Stage  => "Display ",
         Source => Source,
         Instr  => Instr);

      if Check_Decode then

         Decode (From => Words, Instruction => Decoded, Length => Length);

         if Decoded.Kind = Undefined then

            raise Undefined_Instruction;

         end if;

         if Length /= 2 * Words'Length then

            Error ("Instruction length");

         end if;

         if Decoded /= Instr then

            Error ("Decoding error.");

         end if;

         Try_Text (
            Words  => Words,
            Stage  => "Decoding",
            Source => Source,
            Instr  => Decoded);

      end if;

      New_Line;

   exception

   when Invalid_Instruction =>

      Prefix_Message (Words);

      Error ("Invalid instruction.");

      New_Line;

   end Try;


   procedure Try (
      Source : in String;
      Word   : in Unsigned_Word_T;
      Instr  : in Instruction_T)
   is
   begin

      Try (
         Source => Source,
         Words  => (1 => Word),
         Instr  => Instr);

   end Try;


   procedure Try (
      Source : in String;
      Word   : in Unsigned_Word_T;
      More   : in Unsigned_Word_T;
      Instr  : in Instruction_T)
   is
   begin

      Try (
         Source => Source,
         Words  => (1 => Word, 2 => More),
         Instr  => Instr);

   end Try;


   procedure Try_Invalid (
      What  : in String;
      Words : in Word_List_T)
   --
   -- Decodes the given Words which are expected to be invalid as
   -- instruction words, as explained in What. Reports exceptions
   -- other than Invalid_Instruction.
   --
   is

      Instr : Instruction_T;
      -- The decoded instruction. Dummy.

      Length : Instruction_Length_T;
      -- The length of the instruction.


   begin  

      Put ("Invalid : ");
      Prefix_Message (Words);
      Put_Line (What);

      Decode (From => Words, Instruction => Instr, Length => Length);

      if Instr.Kind = Undefined then

         raise Undefined_Instruction;

      end if;

      Error ("Decoded as " & H8_300.Text.Image (Instr));

      New_Line;

   exception

   when Invalid_Instruction =>
      -- Just as expected.

      Put_Line ("Decoding: invalid.");

      New_Line;

   when X : others =>

      Prefix_Message (Words);

      Put_Line (Ada.Exceptions.Exception_Information (X));

      Error ("Exception.");

      New_Line;

   end Try_Invalid;


   procedure Try_Invalid (
      What : in String;
      Word : in Unsigned_Word_T)
   is
   begin

      Try_Invalid (
         What  => What,
         Words => (1 => Word));

   end Try_Invalid;


   procedure Try_Invalid (
      What : in String;
      Word : in Unsigned_Word_T;
      More : in Unsigned_Word_T)
   is
   begin

      Try_Invalid (
         What  => What,
         Words => (Word, More));

   end Try_Invalid;


   function Off (Item : Branch_Offset_T) return Octet_T
   --
   -- The two's-complement encoding of a branch offset in the
   -- lesser octet of the branch instruction.
   --
   is
   begin

      if Item mod 2 /= 0 then
         -- Branch offsets must be even.

         Error ("Odd branch offset");

      end if;

      if Item >= 2 then
         -- Non-negative code (relative to _next_ instruction).

         return Octet_T (Item - 2);

      else
         -- Negative code, use 2's complement.

         return (not (Octet_T (2 - Item)) + 1) and 16#FF#;

      end if;

   end Off;


   procedure Try_All_Words
   --
   -- Try to decode all 16-bit words.
   -- Report invalid instructions and other exceptions.
   --
   is

      Valid : Boolean := False;
      -- Keeping track of valid/invalid status.

   begin

      Put_Line ("Try all instruction words:");

      for W1 in Unsigned_Word_T loop

         Try (Words => (1 => W1), Valid => Valid);

         if not Valid then
            -- W1 is not a valid one-word instruction.
            -- Check for valid two-word instructions starting with W1:

            for W2 in Unsigned_Word_T loop

               Try (Words => (W1, W2), Valid => Valid);

            end loop;

         end if;

      end loop;

   end Try_All_Words;


   procedure Try_All (Check_All_Words : in Boolean)
   is
   begin

      Put_Line ("Verifying H8/300 decoder & disassembler.");
      New_Line;


      --
      -- Try some examples of each instruction type.
      --
      -- Instruction order is the same as in the User Manual.


      -- ADD

      Try ("add.b #21,r3l",      16#8B00# or 21,
          (Kind => Add,
           Two  => (
              Source      => (Immediate, Value => 21),
              Destination => (Register , 3, Low_Byte), 
              Width       => Octet)));

      Try ("add.b r1h,spl"     , 2#0000_1000_0001_1111#,
          (Kind => Add,
           Two  => (
              Source      => (Register, 1, High_Byte),
              Destination => (Register, 7, Low_Byte), 
              Width       => Octet)));

      Try ("add.w r2,r5"       , 2#0000_1001_0010_0101#,
          (Kind => Add,
           Two  => (
              Source      => (Register, 2, Word),
              Destination => (Register, 5, Word), 
              Width       => Word)));

      Try ("adds #1,r0"        , 2#0000_1011_0000_0000#,
          (Kind => Add_With_Sign_Ext,
           Two  => (
              Source      => (Immediate, 1),
              Destination => (Register , 0, Word), 
              Width       => Word)));

      Try ("adds #2,r6"        , 2#0000_1011_1000_0110#,
          (Kind => Add_With_Sign_Ext,
           Two  => (
              Source      => (Immediate, 2),
              Destination => (Register , 6, Word), 
              Width       => Word)));

      Try ("addx #255,r0h"     , 2#1001_0000_1111_1111#,
          (Kind => Add_With_Carry,
           Two  => (
              Source      => (Immediate, 255),
              Destination => (Register , 0, High_Byte), 
              Width       => Octet)));

      Try ("addx sph,r5l"      , 2#0000_1110_0111_1101#,
          (Kind => Add_With_Carry,
           Two  => (
              Source      => (Register, 7, High_Byte),
              Destination => (Register, 5, Low_Byte), 
              Width       => Octet)));


      -- AND

      Try ("and #55,r3l"       , 2#1110_1011_0011_0111#,
          (Kind => Bitwise_And,
           Two  => (
              Source      => (Immediate, 55),
              Destination => (Register , 3, Low_Byte), 
              Width       => Octet)));

      Try ("and r4h,r1l"       , 16#1649#,
          (Kind => Bitwise_And,
           Two  => (
              Source      => (Register , 4, High_Byte),
              Destination => (Register , 1, Low_Byte), 
              Width       => Octet)));

      Try ("andc #H'13,ccr"    , 2#0000_0110_0001_0011#,
          (Kind   => And_CCR,
           Update => (Value => 19)));


      -- BAND

      Try ("band #5,r1l"       , 2#0111_0110_0101_1001#,
          (Kind => Bit_And,
           Bit  => (
              Bit     => (Immediate, 5),
              Host    => (Register , 1, Low_Byte),
              Inverse => False)));

      Try ("band #2,@r3"       , 2#0111_1100_0011_0000#,
                                 2#0111_0110_0010_0000#,
          (Kind => Bit_And,
           Bit  => (
              Bit     => (Immediate, 2),
              Host    => (Register_Indirect, 3, None, 0),
              Inverse => False)));

      Try ("band #7,@H'FF83"   , 16#7E83#, 16#7670#,
          (Kind => Bit_And,
           Bit  => (
              Bit     => (Immediate, 7),
              Host    => (Absolute , 16#FF83#),
              Inverse => False)));


      -- Bcc

      Try ("bra *+24",           16#4000# or Off (24),
          (Kind      => Branch,
           Branch_Op => (Always, 24)));

      Try ("brn *+128",          16#4100# or Off (128),
          (Kind      => Branch,
           Branch_Op => (Never, 128)));

      Try ("bhi *-2",            16#4200# or Off (-2),
          (Kind      => Branch,
           Branch_Op => (High, -2)));

      Try ("bls *-126",          16#4300# or Off (-126),
          (Kind      => Branch,
           Branch_Op => (Low_Or_Same, -126)));

      Try ("bhs *+0",            16#4400# or Off (0),
          (Kind      => Branch,
           Branch_Op => (Carry_Clear, 0)));

      Try ("blo *+2",            16#4500# or Off (2),
          (Kind      => Branch,
           Branch_Op => (Carry_Set, 2)));

      Try ("bne *+4",            16#4600# or Off (4),
          (Kind      => Branch,
           Branch_Op => (Not_Equal, 4)));

      Try ("beq *-4",            16#4700# or Off (-4),
          (Kind      => Branch,
           Branch_Op => (Equal, -4)));

      Try ("bvc *+6",            16#4800# or Off (6),
          (Kind      => Branch,
           Branch_Op => (No_Overflow, 6)));

      Try ("bvs *-6",            16#4900# or Off (-6),
          (Kind      => Branch,
           Branch_Op => (Overflow, -6)));

      Try ("bpl *+48",           16#4A00# or Off (48),
          (Kind      => Branch,
           Branch_Op => (Plus, 48)));

      Try ("bmi *-48",           16#4B00# or Off (-48),
          (Kind      => Branch,
           Branch_Op => (Minus, -48)));

      Try ("bge *+102",          16#4C00# or Off (102),
          (Kind      => Branch,
           Branch_Op => (Greater_Or_Equal, 102)));

      Try ("blt *-102",          16#4D00# or Off (-102),
          (Kind      => Branch,
           Branch_Op => (Less, -102)));

      Try ("bgt *+122",          16#4E00# or Off (122),
          (Kind      => Branch,
           Branch_Op => (Greater, 122)));

      Try ("ble *-122",          16#4F00# or Off (-122),
          (Kind      => Branch,
           Branch_Op => (Less_Or_Equal, -122)));


      -- BCLR

      Try ("bclr #1,r2h",        16#7212#,
          (Kind => Bit_Clear,
           Bit  => (
              Bit     => (Immediate, 1),
              Host    => (Register , 2, High_Byte),
              Inverse => False)));

      Try ("bclr #5,@r4",        16#7D40#, 16#7250#,
          (Kind => Bit_Clear,
           Bit  => (
              Bit     => (Immediate, 5),
              Host    => (Register_Indirect, 4, None, 0),
              Inverse => False)));

      Try ("bclr #6,@H'FF62",    16#7F62#, 16#7260#,
          (Kind => Bit_Clear,
           Bit  => (
              Bit     => (Immediate, 6),
              Host    => (Absolute , 16#FF62#),
              Inverse => False)));

      Try ("bclr r6l,r3h",       16#62E3#,
          (Kind => Bit_Clear,
           Bit  => (
              Bit     => (Register, 6, Low_Byte),
              Host    => (Register, 3, High_Byte),
              Inverse => False)));

      Try ("bclr r1l,@r0",       16#7D00#, 16#6290#,
          (Kind => Bit_Clear,
           Bit  => (
              Bit     => (Register, 1, Low_Byte),
              Host    => (Register_Indirect, 0, None, 0),
              Inverse => False)));

      Try ("bclr r1h,@H'FF0F",    16#7F0F#, 16#6210#,
          (Kind => Bit_Clear,
           Bit  => (
              Bit     => (Register, 1, High_Byte),
              Host    => (Absolute, 16#FF0F#),
              Inverse => False)));


      -- BIAND

      Try ("biand #2,r1h",       16#76A1#,
          (Kind => Bit_And,
           Bit  => (
              Bit     => (Immediate, 2),
              Host    => (Register , 1, High_Byte),
              Inverse => True)));

      Try ("biand #5,@sp",       16#7C70#, 16#76D0#,
          (Kind => Bit_And,
           Bit  => (
              Bit     => (Immediate, 5),
              Host    => (Register_Indirect, 7, None, 0),
              Inverse => True)));

      Try ("biand #0,@H'FF31",   16#7E31#, 16#7680#,
          (Kind => Bit_And,
           Bit  => (
              Bit     => (Immediate, 0),
              Host    => (Absolute , 16#FF31#),
              Inverse => True)));


      -- BILD

      Try ("bild #3,r2h",        16#77B2#,
          (Kind => Bit_Load,
           Bit  => (
              Bit     => (Immediate, 3),
              Host    => (Register , 2, High_Byte),
              Inverse => True)));

      Try ("bild #6,@r1",        16#7C10#, 16#77E0#,
          (Kind => Bit_Load,
           Bit  => (
              Bit     => (Immediate, 6),
              Host    => (Register_Indirect, 1, None, 0),
              Inverse => True)));

      Try ("bild #7,@H'FF07",    16#7E07#, 16#77F0#,
          (Kind => Bit_Load,
           Bit  => (
              Bit     => (Immediate, 7),
              Host    => (Absolute , 16#FF07#),
              Inverse => True)));


      -- BIOR

      Try ("bior #4,r3l",        16#74CB#,
          (Kind => Bit_Or,
           Bit  => (
              Bit     => (Immediate, 4),
              Host    => (Register , 3, Low_Byte),
              Inverse => True)));

      Try ("bior #7,@r2",        16#7C20#, 16#74F0#,
          (Kind => Bit_Or,
           Bit  => (
              Bit     => (Immediate, 7),
              Host    => (Register_Indirect, 2, None, 0),
              Inverse => True)));

      Try ("bior #1,@H'FF52",    16#7E52#, 16#7490#,
          (Kind => Bit_Or,
           Bit  => (
              Bit     => (Immediate, 1),
              Host    => (Absolute , 16#FF52#),
              Inverse => True)));


      -- BIST

      Try ("bist #5,r4h",        16#67D4#,
          (Kind => Bit_Store,
           Bit  => (
              Bit     => (Immediate, 5),
              Host    => (Register , 4, High_Byte),
              Inverse => True)));

      Try ("bist #0,@r3",        16#7D30#, 16#6780#,
          (Kind => Bit_Store,
           Bit  => (
              Bit     => (Immediate, 0),
              Host    => (Register_Indirect, 3, None, 0),
              Inverse => True)));

      Try ("bist #2,@H'FF18",    16#7F18#, 16#67A0#,
          (Kind => Bit_Store,
           Bit  => (
              Bit     => (Immediate, 2),
              Host    => (Absolute , 16#FF18#),
              Inverse => True)));


      -- BIXOR

      Try ("bixor #3,r6l",       16#75BE#,
          (Kind => Bit_Xor,
           Bit  => (
              Bit     => (Immediate, 3),
              Host    => (Register , 6, Low_Byte),
              Inverse => True)));

      Try ("bixor #6,@r1",       16#7C10#, 16#75E0#,
          (Kind => Bit_Xor,
           Bit  => (
              Bit     => (Immediate, 6),
              Host    => (Register_Indirect, 1, None, 0),
              Inverse => True)));

      Try ("bixor #1,@H'FF63",   16#7E63#, 16#7590#,
          (Kind => Bit_Xor,
           Bit  => (
              Bit     => (Immediate, 1),
              Host    => (Absolute , 16#FF63#),
              Inverse => True)));


      -- BLD

      Try ("bld #4,r3h",         16#7743#,
          (Kind => Bit_Load,
           Bit  => (
              Bit     => (Immediate, 4),
              Host    => (Register , 3, High_Byte),
              Inverse => False)));

      Try ("bld #5,@r2",         16#7C20#, 16#7750#,
          (Kind => Bit_Load,
           Bit  => (
              Bit     => (Immediate, 5),
              Host    => (Register_Indirect, 2, None, 0),
              Inverse => False)));

      Try ("bld #3,@H'FF56",     16#7E56#, 16#7730#,
          (Kind => Bit_Load,
           Bit  => (
              Bit     => (Immediate, 3),
              Host    => (Absolute , 16#FF56#),
              Inverse => False)));


      -- BNOT

      Try ("bnot #5,r3l",        16#715B#,
          (Kind => Bit_Not,
           Bit  => (
              Bit     => (Immediate, 5),
              Host    => (Register , 3, Low_Byte),
              Inverse => False)));

      Try ("bnot #2,@r3",        16#7D30#, 16#7120#,
          (Kind => Bit_Not,
           Bit  => (
              Bit     => (Immediate, 2),
              Host    => (Register_Indirect, 3, None, 0),
              Inverse => False)));

      Try ("bnot #3,@H'FF65",    16#7F65#, 16#7130#,
          (Kind => Bit_Not,
           Bit  => (
              Bit     => (Immediate, 3),
              Host    => (Absolute , 16#FF65#),
              Inverse => False)));

      Try ("bnot r5l,r4h",       16#61D4#,
          (Kind => Bit_Not,
           Bit  => (
              Bit     => (Register, 5, Low_Byte),
              Host    => (Register, 4, High_Byte),
              Inverse => False)));

      Try ("bnot r2h,@r1",       16#7D10#, 16#6120#,
          (Kind => Bit_Not,
           Bit  => (
              Bit     => (Register, 2, High_Byte),
              Host    => (Register_Indirect, 1, None, 0),
              Inverse => False)));

      Try ("bnot r3h,@H'FFA8",    16#7FA8#, 16#6130#,
          (Kind => Bit_Not,
           Bit  => (
              Bit     => (Register, 3, High_Byte),
              Host    => (Absolute, 16#FFA8#),
              Inverse => False)));


      -- BOR

      Try ("bor #2,r5h",         16#7425#,
          (Kind => Bit_Or,
           Bit  => (
              Bit     => (Immediate, 2),
              Host    => (Register , 5, High_Byte),
              Inverse => False)));

      Try ("bor #1,@r6",         16#7C60#, 16#7410#,
          (Kind => Bit_Or,
           Bit  => (
              Bit     => (Immediate, 1),
              Host    => (Register_Indirect, 6, None, 0),
              Inverse => False)));

      Try ("bor #2,@H'FFC9",     16#7EC9#, 16#7420#,
          (Kind => Bit_Or,
           Bit  => (
              Bit     => (Immediate, 2),
              Host    => (Absolute , 16#FFC9#),
              Inverse => False)));


      -- BSET

      Try ("bset #3,r5l",        16#703D#,
          (Kind => Bit_Set,
           Bit  => (
              Bit     => (Immediate, 3),
              Host    => (Register , 5, Low_Byte),
              Inverse => False)));

      Try ("bset #2,@sp",        16#7D70#, 16#7020#,
          (Kind => Bit_Set,
           Bit  => (
              Bit     => (Immediate, 2),
              Host    => (Register_Indirect, 7, None, 0),
              Inverse => False)));

      Try ("bset #3,@H'FF40",    16#7F40#, 16#7030#,
          (Kind => Bit_Set,
           Bit  => (
              Bit     => (Immediate, 3),
              Host    => (Absolute , 16#FF40#),
              Inverse => False)));

      Try ("bset r6h,r5l",       16#606D#,
          (Kind => Bit_Set,
           Bit  => (
              Bit     => (Register, 6, High_Byte),
              Host    => (Register, 5, Low_Byte),
              Inverse => False)));

      Try ("bset r3l,@r2",       16#7D20#, 16#60B0#,
          (Kind => Bit_Set,
           Bit  => (
              Bit     => (Register, 3, Low_Byte),
              Host    => (Register_Indirect, 2, None, 0),
              Inverse => False)));

      Try ("bset r4l,@H'FFEA",   16#7FEA#, 16#60C0#,
          (Kind => Bit_Set,
           Bit  => (
              Bit     => (Register, 4, Low_Byte),
             Host    => (Absolute, 16#FFEA#),
              Inverse => False)));


      -- BSR

      Try ("bsr *+40",           16#5500# or Off (40),
          (Kind              => Branch_Subroutine,
           Subroutine_Offset => 40));

      Try ("bsr *-76",           16#5500# or Off (-76),
          (Kind              => Branch_Subroutine,
           Subroutine_Offset => -76));

      Try ("bsr *-70",           16#5500# or Off (-70),
          (Kind              => Branch_Subroutine,
           Subroutine_Offset => -70));


      -- BST

      Try ("bst #5,r4l",         16#675C#,
          (Kind => Bit_Store,
           Bit  => (
              Bit     => (Immediate, 5),
              Host    => (Register , 4, Low_Byte),
              Inverse => False)));

      Try ("bst #6,@r3",         16#7D30#, 16#6760#,
          (Kind => Bit_Store,
           Bit  => (
              Bit     => (Immediate, 6),
              Host    => (Register_Indirect, 3, None, 0),
              Inverse => False)));

      Try ("bst #4,@H'FF1E",     16#7F1E#, 16#6740#,
          (Kind => Bit_Store,
           Bit  => (
              Bit     => (Immediate, 4),
              Host    => (Absolute , 16#FF1E#),
              Inverse => False)));


      -- BTST

      Try ("btst #4,r6h",        16#7346#,
          (Kind => Bit_Test,
           Bit  => (
              Bit     => (Immediate, 4),
              Host    => (Register , 6, High_Byte),
              Inverse => False)));

      Try ("btst #1,@r0",        16#7C00#, 16#7310#,
          (Kind => Bit_Test,
           Bit  => (
              Bit     => (Immediate, 1),
              Host    => (Register_Indirect, 0, None, 0),
              Inverse => False)));

      Try ("btst #4,@H'FF9A",    16#7E9A#, 16#7340#,
          (Kind => Bit_Test,
           Bit  => (
              Bit     => (Immediate, 4),
              Host    => (Absolute , 16#FF9A#),
              Inverse => False)));

      Try ("btst spl,r6h",       16#63F6#,
          (Kind => Bit_Test,
           Bit  => (
              Bit     => (Register, 7, Low_Byte),
              Host    => (Register, 6, High_Byte),
              Inverse => False)));

      Try ("btst r5h,@r4",       16#7C40#, 16#6350#,
          (Kind => Bit_Test,
           Bit  => (
              Bit     => (Register, 5, High_Byte),
              Host    => (Register_Indirect, 4, None, 0),
              Inverse => False)));

      Try ("btst r6l,@H'FFA2",   16#7EA2#, 16#63E0#,
          (Kind => Bit_Test,
           Bit  => (
              Bit     => (Register, 6, Low_Byte),
              Host    => (Absolute, 16#FFA2#),
              Inverse => False)));


      -- BXOR

      Try ("bxor #0,r5l",        16#750D#,
          (Kind => Bit_Xor,
           Bit  => (
              Bit     => (Immediate, 0),
              Host    => (Register , 5, Low_Byte),
              Inverse => False)));

      Try ("bxor #7,@sp",        16#7C70#, 16#7570#,
          (Kind => Bit_Xor,
           Bit  => (
              Bit     => (Immediate, 7),
              Host    => (Register_Indirect, 7, None, 0),
              Inverse => False)));

      Try ("bxor #6,@H'FF7D",    16#7E7D#, 16#7560#,
          (Kind => Bit_Xor,
           Bit  => (
              Bit     => (Immediate, 6),
              Host    => (Absolute , 16#FF7D#),
              Inverse => False)));


      -- CMP

      Try ("cmp.b #153,r5l",     16#AD00# or 153,
          (Kind => Compare,
           Two  => (
              Source      => (Immediate, 153),
              Destination => (Register , 5, Low_Byte),
              Width       => Octet)));

      Try ("cmp.b r2l,r5h",      16#1CA5#,
          (Kind => Compare,
           Two  => (
              Source      => (Register, 2, Low_Byte),
              Destination => (Register, 5, High_Byte),
              Width       => Octet)));

      Try ("cmp.w r6,r4",        16#1D64#,
          (Kind => Compare,
           Two  => (
              Source      => (Register, 6, Word),
              Destination => (Register, 4, Word),
              Width       => Word)));


      -- DAA

      Try ("daa r1l",            16#0F09#,
          (Kind => Decimal_Adjust,
           One  => (Register => 1, Part => Low_Byte)));


      -- DAS

      Try ("das r3h",            16#1F03#,
          (Kind => Decimal_Adjust_Subtract,
           One  => (Register => 3, Part => High_Byte)));


      -- DEC

      Try ("dec r4l",            16#1A0C#,
          (Kind => Decrement,
           One  => (Register => 4, Part => Low_Byte)));


      -- DIVXU

      Try ("divxu r2l,r6",       16#51A6#,
          (Kind => Divide,
           Two  => (
              Source      => (Register, 2, Low_Byte),
              Destination => (Register, 6, Word),
              Width       => Octet)));


      -- EEPMOV

      Try ("eepmov",             16#7B5C#, 16#598F#,
          (Kind => Move_Block));


      -- INC

      Try ("inc r1h",            16#0A01#,
          (Kind => Increment,
           One  => (Register => 1, Part => High_Byte)));


      -- JMP

      Try ("jmp @r3",            16#5930#,
          (Kind   => Jump,
           Target => (Register_Indirect, 3)));

      Try ("jmp @H'1234",        16#5A00#, 16#1234#,
          (Kind   => Jump,
           Target => (Absolute, 16#1234#)));

      Try ("jmp @@H'0049",       16#5B49#,
          (Kind   => Jump,
           Target => (Memory_Indirect, 16#49#)));


      -- JSR

      Try ("jsr @r6",            16#5D60#,
          (Kind   => Jump_Subroutine,
           Target => (Register_Indirect, 6)));

      Try ("jsr @H'FEDC",        16#5E00#, 16#FEDC#,
          (Kind   => Jump_Subroutine,
           Target => (Absolute, 16#FEDC#)));

      Try ("jsr @@H'00AB",       16#5FAB#,
          (Kind   => Jump_Subroutine,
           Target => (Memory_Indirect, 16#AB#)));


      -- LDC

      Try ("ldc #H'31,ccr",      16#0731#,
          (Kind   => Set_CCR,
          Update => (Value => 16#31#)));

      Try ("ldc r3l,ccr",        16#030B#,
          (Kind     => Load_CCR,
           Transfer => (Register => 3, Part => Low_Byte)));


      -- MOV

      --    Register to register, word or byte:

      Try ("mov.b r4h,r2l",      16#0C4A#,
          (Kind => Move,
           Two  => (
              Source      => (Register, 4, High_Byte),
              Destination => (Register, 2, Low_Byte),
              Width       => Octet)));

      Try ("mov.w r1,sp",        16#0D17#,
          (Kind => Move,
           Two  => (
              Source      => (Register, 1, Word),
              Destination => (Register, 7, Word),
              Width       => Word)));

      --    Other sources to register, byte:

      Try ("mov.b #17,r3l",      16#FB00# or 17,
          (Kind => Move,
           Two  => (
              Source      => (Immediate, 17),
              Destination => (Register, 3, Low_Byte),
              Width       => Octet)));

      Try ("mov.b @r4,r5h",      16#6845#,
          (Kind => Move,
           Two  => (
              Source      => (Register_Indirect, 4, None, 0),
              Destination => (Register, 5, High_Byte),
              Width       => Octet)));

      Try ("mov.b @(321,r2),r5l", 16#6E2D#, 321,
          (Kind => Move,
           Two  => (
              Source      => (Register_Indirect, 2, None, 321),
              Destination => (Register, 5, Low_Byte),
              Width       => Octet)));

      Try ("mov.b @r5+,r1l",     16#6C59#,
          (Kind => Move,
           Two  => (
              Source      => (Register_Indirect, 5, Post_Increment, 0),
              Destination => (Register, 1, Low_Byte),
              Width       => Octet)));

      Try ("mov.b @H'FFC3,r1h",  16#21C3#,
          (Kind => Move,
           Two  => (
              Source      => (Absolute, 16#FFC3#),
              Destination => (Register, 1, High_Byte),
              Width       => Octet)));

      Try ("mov.b @H'A1B2,r1h",  16#6A01#, 16#A1B2#,
          (Kind => Move,
           Two  => (
              Source      => (Absolute, 16#A1B2#),
              Destination => (Register, 1, High_Byte),
              Width       => Octet)));

      --    Other sources to register, word:

      Try ("mov.w #2754,r4",     16#7904#, 2754,
          (Kind => Move,
           Two  => (
              Source      => (Immediate, 2754),
              Destination => (Register, 4, Word),
              Width       => Word)));

      Try ("mov.w @r1,r5",       16#6915#,
          (Kind => Move,
           Two  => (
              Source      => (Register_Indirect, 1, None, 0),
              Destination => (Register, 5, Word),
              Width       => Word)));

      Try ("mov.w @(729,r0),sp", 16#6F07#, 729,
          (Kind => Move,
           Two  => (
              Source      => (Register_Indirect, 0, None, 729),
              Destination => (Register, 7, Word),
              Width       => Word)));

      Try ("mov.w @r3+,r4",      16#6D34#,
          (Kind => Move,
           Two  => (
              Source      => (Register_Indirect, 3, Post_Increment, 0),
              Destination => (Register, 4, Word),
              Width       => Word)));

      Try ("mov.w @H'1987,r1",   16#6B01#, 16#1987#,
          (Kind => Move,
           Two  => (
              Source      => (Absolute, 16#1987#),
              Destination => (Register, 1, Word),
              Width       => Word)));

      --    Register to other destinations, byte:

      Try ("mov.b r4l,@r3",      16#68BC#,
          (Kind => Move,
           Two  => (
              Source      => (Register, 4, Low_Byte),
              Destination => (Register_Indirect, 3, None, 0),
              Width       => Octet)));

      Try ("mov.b r1h,@(667,r5)", 16#6ED1#, 667,
          (Kind => Move,
           Two  => (
              Source      => (Register, 1, High_Byte),
              Destination => (Register_Indirect, 5, None, 667),
              Width       => Octet)));

      Try ("mov.b r1l,@-r2",     16#6CA9#,
          (Kind => Move,
           Two  => (
              Source      => (Register, 1, Low_Byte),
              Destination => (Register_Indirect, 2, Pre_Decrement, 0),
              Width       => Octet)));

      Try ("mov.b r6h,@H'FF59",  16#3659#,
          (Kind => Move,
           Two  => (
              Source      => (Register, 6, High_Byte),
              Destination => (Absolute, 16#FF59#),
              Width       => Octet)));

      Try ("mov.b r4l,@H'C3E7",  16#6A8C#, 16#C3E7#,
          (Kind => Move,
           Two  => (
              Source      => (Register, 4, Low_Byte),
              Destination => (Absolute, 16#C3E7#),
              Width       => Octet)));

      --    Register to other destinations, word:

      Try ("mov.w r4,@r2",       16#69A4#,
          (Kind => Move,
           Two  => (
              Source      => (Register, 4, Word),
              Destination => (Register_Indirect, 2, None, 0),
              Width       => Word)));

      Try ("mov.w r3,@(10001,sp)", 16#6FF3#, 10001,
          (Kind => Move,
           Two  => (
              Source      => (Register, 3, Word),
             Destination => (Register_Indirect, 7, None, 10001),
              Width       => Word)));

      Try ("mov.w sp,@-r1",      16#6D97#,
          (Kind => Move,
           Two  => (
              Source      => (Register, 7, Word),
              Destination => (Register_Indirect, 1, Pre_Decrement, 0),
              Width       => Word)));

      Try ("mov.w r5,@H'2004",   16#6B85#, 16#2004#,
          (Kind => Move,
           Two  => (
              Source      => (Register, 5, Word),
              Destination => (Absolute, 16#2004#),
              Width       => Word)));


      -- MOVFPE

      Try ("movfpe @H'1A3C,r0h", 16#6A40#, 16#1A3C#,
          (Kind => Move_From_Peri,
           Two  => (
              Source      => (Absolute, 16#1A3C#),
              Destination => (Register, 0, High_Byte),
              Width       => Octet)));


      -- MOVTPE

      Try ("movtpe r3l,@H'2B4E", 16#6ACB#, 16#2B4E#,
          (Kind => Move_To_Peri,
           Two  => (
              Source      => (Register, 3, Low_Byte),
              Destination => (Absolute, 16#2B4E#),
              Width       => Octet)));


      -- MULXU

      Try ("mulxu r3h,r1",       16#5031#,
          (Kind => Multiply,
           Two  => (
              Source      => (Register, 3, High_Byte),
              Destination => (Register, 1, Word),
              Width       => Octet)));


      -- NEG

      Try ("neg r2l",            16#178A#,
          (Kind => Negate,
           One  => (Register => 2, Part => Low_Byte)));


      -- NO-OPERATION

      Try ("nop",                16#0000#,
          (Kind => No_Op));


      -- NOT

      Try ("not r3h",            16#1703#,
          (Kind => Bitwise_Not,
           One  => (Register => 3, Part => High_Byte)));


      -- OR

      Try ("or #92,r1h",         16#C100# or 92,
          (Kind => Bitwise_Or,
           Two  => (
              Source      => (Immediate, 92),
              Destination => (Register, 1, High_Byte),
              Width       => Octet)));

      Try ("or r5h,r6l",         16#145E#,
          (Kind => Bitwise_Or,
           Two  => (
              Source      => (Register, 5, High_Byte),
              Destination => (Register, 6, Low_Byte),
              Width       => Octet)));


      -- ORC

      Try ("orc #H'A5,ccr",      16#04A5#,
          (Kind   => Or_CCR,
           Update => (Value => 16#A5#)));


      -- POP

      Try ("pop r5",             16#6D75#,
          (Kind  => Pop,
           Stack => (Register => 5)));


      -- PUSH

      Try ("push r2",            16#6DF2#,
          (Kind  => Push,
           Stack => (Register => 2)));


      -- ROTL

      Try ("rotl r1l",           16#1289#,
          (Kind => Rotate_Left,
           One  => (Register => 1, Part => Low_Byte)));


      -- ROTR

      Try ("rotr r4h",           16#1384#,
          (Kind => Rotate_Right,
           One  => (Register => 4, Part => High_Byte)));


      -- ROTXL

      Try ("rotxl spl",          16#120F#,
          (Kind => Rotate_Left_Via_Carry,
           One  => (Register => 7, Part => Low_Byte)));


      -- ROTXR

      Try ("rotxr r6h",          16#1306#,
          (Kind => Rotate_Right_Via_Carry,
           One  => (Register => 6, Part => High_Byte)));


      -- RTE

      Try ("rte",                16#5670#,
          (Kind => Return_From_Exception));


      -- RTS

      Try ("rts",                16#5470#,
          (Kind => Return_From_Subroutine));


      -- SHAL

     Try ("shal r2h",           16#1082#,
          (Kind => Shift_Arith_Left,
           One  => (Register => 2, Part => High_Byte)));


      -- SHAR

      Try ("shar r3l",           16#118B#,
          (Kind => Shift_Arith_Right,
           One  => (Register => 3, Part => Low_Byte)));


      -- SHLL

      Try ("shll sph",           16#1007#,
          (Kind => Shift_Logical_Left,
           One  => (Register => 7, Part => High_Byte)));


      -- SHLR

      Try ("shlr r6l",           16#110E#,
          (Kind => Shift_Logical_Right,
           One  => (Register => 6, Part => Low_Byte)));


      -- SLEEP

      Try ("sleep",              16#0180#,
          (Kind => Sleep));


      -- STC

      Try ("stc ccr,r4h",        16#0204#,
          (Kind     => Store_CCR,
           Transfer => (Register => 4, Part => High_Byte)));


      -- SUB

      Try ("sub.b r4l,r2h",      16#18C2#,
          (Kind => Subtract,
           Two  => (
              Source      => (Register, 4, Low_Byte),
              Destination => (Register, 2, High_Byte),
              Width       => Octet)));

      Try ("sub.w r0,sp",        16#1907#,
          (Kind => Subtract,
           Two  => (
              Source      => (Register, 0, Word),
              Destination => (Register, 7, Word),
              Width       => Word)));


      -- SUBS

      Try ("subs #1,r6",         16#1B06#,
          (Kind => Subtract_With_Sign_Ext,
           Two  => (
              Source      => (Immediate, 1),
              Destination => (Register , 6, Word),
             Width       => Word)));

      Try ("subs #2,sp",         16#1B87#,
          (Kind => Subtract_With_Sign_Ext,
           Two  => (
              Source      => (Immediate, 2),
              Destination => (Register , 7, Word),
              Width       => Word)));


      -- SUBX

      Try ("subx #21,r5l",       16#BD00# or 21,
          (Kind => Subtract_With_Carry,
           Two  => (
              Source      => (Immediate, 21),
              Destination => (Register , 5, Low_Byte),
              Width       => Octet)));

      Try ("subx r4l,r2h",       16#1EC2#,
          (Kind => Subtract_With_Carry,
           Two  => (
              Source      => (Register, 4, Low_Byte),
              Destination => (Register, 2, High_Byte),
              Width       => Octet)));


      -- XOR

      Try ("xor #84,r6l",        16#DE00# or 84,
          (Kind => Bitwise_Xor,
           Two  => (
              Source      => (Immediate, 84),
              Destination => (Register, 6, Low_Byte),
              Width       => Octet)));

      Try ("xor spl,r2h",        16#15F2#,
          (Kind => Bitwise_Xor,
           Two  => (
              Source      => (Register, 7, Low_Byte),
              Destination => (Register, 2, High_Byte),
              Width       => Octet)));


      -- XORC

      Try ("xorc #H'7F,ccr",     16#057F#,
          (Kind   => Xor_CCR,
           Update => (Value => 16#7F#)));


      --
      -- Check invalid instructions
      --
      -- Abbreviations in the descriptions:
      --   -w2     Word two is missing.
      --   w1.7    Bit 7 in word 1 is wrong.
      --   w1.0-3  Bits 0 .. 3 in word 1 are wrong.
      --   w2.7    Bit 7 in word 2 is wrong.
      --   w2.0-3  Bits 0 .. 3 in word 2 are wrong.
      -- and so on for other bit numbers.
      --
      -- To some extent, the test cases are chosen with knowledge
      -- of the design and code of H8_300.Decode. An exhaustive
      -- test is not attempted. The focus is on checking that an
      -- instruction is rejected as invalid if it has wrong values
      -- in specific "dummy" fields or bits that do not take part
      -- in the encoding of the instruction or its operands.


      Try_Invalid ("add.w w1.7",          16#0980#);
      Try_Invalid ("add.w w1.3",          16#0908#);
      Try_Invalid ("add.w w1.3,7",        16#0908#);

      Try_Invalid ("add.s #1 w1.3",       16#0B08#);
      Try_Invalid ("add.s #2 w1.3",       16#0B88#);

      Try_Invalid ("band #,@Rd -w2",      16#7C00#);
      Try_Invalid ("band #,@Rd w1.7",     16#7C80#, 16#7600#);
      Try_Invalid ("band #,@Rd w1.0-3",   16#7C02#, 16#7600#);
      Try_Invalid ("band #,@Rd w2.0-3",   16#7C00#, 16#7601#);

      Try_Invalid ("band #,@aa -w2",      16#7E23#);
      Try_Invalid ("band #,@aa w2.0-3",   16#7E23#, 16#7601#);


      Try_Invalid ("bclr #,Rd w1.7",      16#7280#);

      Try_Invalid ("bclr #,@Rd -w2",      16#7D80#);
      Try_Invalid ("bclr #,@Rd bit 7",    16#7D80#, 16#7200#);
      Try_Invalid ("bclr #,@Rd w1.0-3",   16#7D03#, 16#7200#);
      Try_Invalid ("bclr #,@Rd w2.7",     16#7D00#, 16#7280#);
      Try_Invalid ("bclr #,@Rd w2.0-3",   16#7D03#, 16#7209#);

      Try_Invalid ("bclr #,@aa -w2",      16#7F99#);
      Try_Invalid ("bclr #,@aa w2.7",     16#7F99#, 16#7280#);
      Try_Invalid ("bclr #,@aa w2.0-3",   16#7F99#, 16#7209#);

      Try_Invalid ("bcrl Rn,@Rd -w2",     16#7D80#);
      Try_Invalid ("bcrl Rn,@Rd w1.7",    16#7D80#, 16#6200#);
      Try_Invalid ("bcrl Rn,@Rd w1.0-3",  16#7D09#, 16#6200#);
      Try_Invalid ("bcrl Rn,@Rd w2.0-3",  16#7D00#, 16#6209#);

      Try_Invalid ("bcrl Rn,@aa -w2",     16#7F00#);
      Try_Invalid ("bcrl Rn,@aa w2.0-3",  16#7F00#, 16#6209#);

      Try_Invalid ("biand #,@Rd -w2",     16#7C00#);
      Try_Invalid ("biand #,@Rd w1.7",    16#7C80#, 16#7680#);
      Try_Invalid ("biand #,@Rd w2.0-3",  16#7C00#, 16#7689#);

      Try_Invalid ("biand #,@aa -w2",     16#7E00#);
      Try_Invalid ("biand #,@aa w2.0-3",  16#7E00#, 16#7689#);

      Try_Invalid ("bild #,@Rd -w2",      16#7C80#);
      Try_Invalid ("bild #,@Rd w1.7",     16#7C80#, 16#7780#);
      Try_Invalid ("bild #,@Rd w2.0-3",   16#7C00#, 16#7789#);

      Try_Invalid ("bild #,@aa -w2",      16#7E00#);
      Try_Invalid ("bild #,@aa w2.0.3",   16#7E00#, 16#7789#);

      Try_Invalid ("bior #,@Rd -w2",      16#7C00#);
      Try_Invalid ("bior #,@Rd w1.7",     16#7C80#, 16#7480#);
      Try_Invalid ("bior #,@Rd w1.0-3",   16#7C09#, 16#7480#);
      Try_Invalid ("bior #,@Rd w2.0-3",   16#7C00#, 16#7489#);

      Try_Invalid ("bist #,@Rd -w2",      16#7D00#);
      Try_Invalid ("bist #,@Rd w1.7",     16#7D80#, 16#6780#);
      Try_Invalid ("bist #,@Rd w1.0-3",   16#7D09#, 16#6780#);
      Try_Invalid ("bist #,@Rd w2.0-3",   16#7D00#, 16#6789#);

      Try_Invalid ("bist #,@aa -w2",      16#7F00#);
      Try_Invalid ("bist #,@aa w2.0-3",   16#7F00#, 16#6786#);

      Try_Invalid ("bixor #,@Rd -w2",     16#7C00#);
      Try_Invalid ("bixor #,@Rd w1.0-3",  16#7C06#, 16#7580#);
      Try_Invalid ("bixor #,@Rd w2.0-3",  16#7C00#, 16#7505#);

      Try_Invalid ("bixor #,@aa -w2",     16#7E00#);
      Try_Invalid ("bixor #,@aa w2.0-3",  16#7E00#, 16#7583#);

      Try_Invalid ("bld #,@Rd -w2",       16#7C00#);
      Try_Invalid ("bld #,@Rd w1.7",      16#7C80#, 16#7700#);
      Try_Invalid ("bld #,@Rd w1.0-3",    16#7C09#, 16#7700#);
      Try_Invalid ("bld #,@Rd w2.0-3",    16#7C00#, 16#7709#);

      Try_Invalid ("bld #,@aa -w2",       16#7E00#);
      Try_Invalid ("bld #,@aa w2.0-3",    16#7E00#, 16#7703#);

      Try_Invalid ("bnot #,Rd w1.7",      16#7180#);

      Try_Invalid ("bnot #,@Rd -w2",      16#7D00#);
      Try_Invalid ("bnot #,@Rd w1.7",     16#7D80#, 16#7100#);
      Try_Invalid ("bnot #,@Rd w1.0-3",   16#7D07#, 16#7100#);
      Try_Invalid ("bnot #,@Rd w2.7",     16#7D00#, 16#7180#);
      Try_Invalid ("bnot #,@Rd w2.0-3",   16#7D00#, 16#7108#);

      Try_Invalid ("bnot #¸@aa -w2",      16#7F00#);
      Try_Invalid ("bnot #,@aa w2.7",     16#7F00#, 16#7180#);
      Try_Invalid ("bnot #,@aa w2.0-3",   16#7F00#, 16#7101#);

      Try_Invalid ("bnot Rn,@Rd -w2",     16#7D00#);
      Try_Invalid ("bnot Rn,@Rd w1.7",    16#7D80#, 16#6100#);
      Try_Invalid ("bnot Rd,@Rd w1.0-3",  16#7D09#, 16#6100#);
      Try_Invalid ("bnot Rd,@Rd w2.0-3",  16#7D00#, 16#6109#);

      Try_Invalid ("bnot Rn,@aa -w2",     16#7F00#);
      Try_Invalid ("bnot Rn,@aa w2.0-3",  16#7F00#, 16#6109#);


      Try_Invalid ("bor #,@Rd -w2",       16#7C00#);
      Try_Invalid ("bor #,@Rd w1.7",      16#7C80#, 16#7400#);
      Try_Invalid ("bor #,@Rd w1.0-3",    16#7C09#, 16#7400#);
      Try_Invalid ("bor #,@Rd w2.0-3",    16#7C00#, 16#7407#);

      Try_Invalid ("bor #,@aa -w2",       16#7E00#);
      Try_Invalid ("bor #,@aa w2.0-3",    16#7E00#, 16#7406#);

      Try_Invalid ("bset #,Rd w1.7",      16#7080#);

      Try_Invalid ("bset #,@Rd -w2",      16#7D00#);
      Try_Invalid ("bset #,@Rd w1.7",     16#7D80#, 16#7000#);
      Try_Invalid ("bset #,@Rd w1.0-3",   16#7D09#, 16#7000#);
      Try_Invalid ("bset #,@Rd w2.0-3",   16#7D20#, 16#6004#);

      Try_Invalid ("bset #,@aa w2.0-3",   16#7Fab#, 16#6063#);

      Try_Invalid ("bst #,@Rd -w2",       16#7D00#);
      Try_Invalid ("bst #,@Rd w1.7",      16#7D80#, 16#6700#);
      Try_Invalid ("bst #,@Rd w1.0-3",    16#7D02#, 16#6700#);
      Try_Invalid ("bst #,@Rd w2.0-3",    16#7D10#, 16#6732#);

      Try_Invalid ("bst #,@aa -w2",       16#7Fcc#);
      Try_Invalid ("bst #,@aa w2.0-3",    16#7Fcc#, 16#6751#);

      Try_Invalid ("btst #,Rd w1.7",      16#7380#);

      Try_Invalid ("btst #,@Rd -w2",      16#7C00#);
      Try_Invalid ("btst #,@Rd w1.7",     16#7C80#, 16#7300#);
      Try_Invalid ("btst #,@Rd w1.0-3",   16#7C0A#, 16#7300#);
      Try_Invalid ("btst #,@Rd w2.7",     16#7C00#, 16#7380#);
      Try_Invalid ("btst #,@Rd w2.0-3",   16#7C00#, 16#7305#);

      Try_Invalid ("btst #,@aa -w2",      16#7E66#);
      Try_Invalid ("btst #,@aa w2.7",     16#7E66#, 16#7380#);
      Try_Invalid ("btst #,@aa w2.0-3",   16#7E22#, 16#7309#);

      Try_Invalid ("btst Rn,@Rd -w2",     16#7C00#);
      Try_Invalid ("btst Rn,@Rd w1.7",    16#7C80#, 16#6300#);
      Try_Invalid ("btst Rn,@Rd w1.0-3",  16#7C02#, 16#6300#);
      Try_Invalid ("btst Rn,@Rd w2.0-3",  16#7C00#, 16#6358#);

      Try_Invalid ("btst Rn,@aa -w2",     16#7E00#);
      Try_Invalid ("btst Rn,@aa w2.0-3",  16#7E00#, 16#63E2#);

      Try_Invalid ("bxor #,@Rd -w2",      16#7C00#);
      Try_Invalid ("bxor #,@Rd w1.7",     16#7C90#, 16#7500#);
      Try_Invalid ("bxor #,@Rd w1.0-3",   16#7C61#, 16#7500#);
      Try_Invalid ("bxor #,@Rd w2.03",    16#7C40#, 16#7509#);

      Try_Invalid ("bxor #,@aa -w2",      16#7Eef#);
      Try_Invalid ("bxor #,@aa w2.0-3",   16#7Eef#, 16#7557#);

      Try_Invalid ("cmp.w w1.7",          16#1D80#);
      Try_Invalid ("cmp.w w1.3",          16#1D08#);

      Try_Invalid ("daa w1.4-7",          16#0F15#);

      Try_Invalid ("das w1.4-7",          16#1F29#);

      Try_Invalid ("dec w1.4-7",          16#1A7a#);

      Try_Invalid ("divxu w1.3",          16#51A8#);

      Try_Invalid ("inc w1.4-7",          16#0A27#);

      Try_Invalid ("jmp @Rn w1.7",        16#5980#);
      Try_Invalid ("jmp @Rn w1.0-3",      16#5973#);

      Try_Invalid ("jmp @aa -w2",         16#5A00#);
      Try_Invalid ("jmp @aa w1.0-8",      16#5A11#, 16#1234#);

      Try_Invalid ("jsr @Rn w1.7",        16#5D80#);
      Try_Invalid ("jsr @Rn w1.0-3",      16#5D73#);

      Try_Invalid ("jsr @aa -w2",         16#5E00#);
      Try_Invalid ("jsr @aa w1.0-8",      16#5E11#, 16#1234#);

      Try_Invalid ("ldc Rs,CCR w1.4-7",   16#037A#);

      Try_Invalid ("mov.w Rs,Rd w1.7",    16#0D80#);
      Try_Invalid ("mov.w Rs,Rd w1.3",    16#0D08#);

      Try_Invalid ("mov.b @aa,Rd w1.4-7", 16#6A27#);

      Try_Invalid ("mov.w #,Rd -w2",      16#7900#);
      Try_Invalid ("mov.w #,Rd w1.4-7",   16#7940#, 16#1234#);
      Try_Invalid ("mov.w #,Rd w1.3",     16#7908#, 16#1234#);

      Try_Invalid ("mov.w @Rs,Rd w1.3",   16#6908#);

      Try_Invalid ("mov.w @(d,Rs),Rd -w2",  16#6F00#);
      Try_Invalid ("mow.w @(d,Rs),Rd w1.3", 16#6F48#, 16#1234#);

      Try_Invalid ("mov.w @Rs+,Rd w1.3",  16#6D28#);

      Try_Invalid ("mov.w @aa,Rd -w2",    16#6B00#);
      Try_Invalid ("mov.w @aa,Rd w1.4-7", 16#6B30#, 5566);
      Try_Invalid ("mov.w @aa,Rd w1.3",   16#6B08#, 6677);

      Try_Invalid ("mov.w Rs,@Rd w1.3",   16#69F8#);

      Try_Invalid ("mov.w.Rs,@(d,Rd) -w2",  16#6F80#);
      Try_Invalid ("mov.w Rs,@(d,Rd) w1.3", 16#6FA9#, 1122);

      Try_Invalid ("mov.w Rs,@-Rd w1.3",  16#6D8E#);

      Try_Invalid ("mov.w Rs,@aa -w2",    16#6B80#);
      Try_Invalid ("mov.w Rs,@aa w1.4-7", 16#6B90#, 16#4321#);
      Try_Invalid ("mov.w Rs,@aa w1.3",   16#6B89#, 16#4321#);

      Try_Invalid ("movfpe @aa,Rd -w2",     16#6A40#);
      Try_Invalid ("movfpe @aa,Rd w1.4-7",  16#6A20#, 16#3121#);

      Try_Invalid ("movtpe Rs,@aa -w2",     16#6AC3#);
      Try_Invalid ("movtpe Rs,@aa w1.4-7",  16#6A23#, 16#3121#);

      Try_Invalid ("mulxu w1.3",          16#50C8#);

      Try_Invalid ("nop w1.0-7",          16#0011#);

      Try_Invalid ("pop Rd w1.3",         16#6D7F#);

      Try_Invalid ("push Rd w1.3",        16#6DF8#);

      Try_Invalid ("rte w1.0-7",          16#5600#);

      Try_Invalid ("rts w1.0-7",          16#5400#);

      Try_Invalid ("sleep w1.0-7",        16#0100#);

      Try_Invalid ("sub.w Rs,Rd w1.7",    16#1980#);
      Try_Invalid ("sub.w Rs,Rd w1.3",    16#1908#);

      Try_Invalid ("subs #1,Rd w1.3",     16#1B09#);
      Try_Invalid ("subs #2,Rd w1.3",     16#1B89#);


      --
      -- Try all possible instruction words:
      --


      if Check_All_Words then

         Try_All_Words;

      end if;


      --
      -- Done
      --

      if Error_Count = 0 then

         Put_Line ("No errors.");

      else

         Put_Line ("Errors:" & Error_Count_T'Image (Error_Count));

      end if;

   end Try_All;


end H8_300.Verify;
