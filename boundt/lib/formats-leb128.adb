-- Formats.LEB128 (body)
--
-- Author: Niklas Holsti.
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
-- $Revision: 1.4 $
-- $Date: 2015/10/24 20:53:53 $
--
-- $Log: formats-leb128.adb,v $
-- Revision 1.4  2015/10/24 20:53:53  niklas
-- Moved to free licence.
--
-- Revision 1.3  2007-02-27 22:10:36  Niklas
-- Corrected 'Read procedures for scalar types to use the 'Base
-- subtype of the scalar type and not to use Read operations inherited
-- from parent types (and so having "intrinsic" convention).
--
-- Revision 1.2  2005/03/03 17:50:31  niklas
-- Corrected the procedure Read_Signed to prevent overflow for
-- negative values (the Sign value is too large for conversion to
-- Number_Type, but Sign - 1 is small enough).
-- Added use of Opt.Max_Value_Bits to work around invalid (?)
-- encodings in output from IAR XLINK.
-- Added the LEB_Encoding buffer for troubleshooting purposes.
--
-- Revision 1.1  2004/04/24 17:28:17  niklas
-- First version.
--


with Formats.LEB128.Opt;
with Output;


package body Formats.LEB128 is


   Max_Bits : constant := 64;
   --
   -- The maximum number of bits in the LEB128 string that
   -- this implementation can handle.
   --
   -- Corresponds to the number of bits in Longest_Signed_T and
   -- Longest_Unsigned_T.


   Max_Octets : constant := (Max_Bits + 6) / 7;
   --
   -- The maximum number of octets in the LEB128 sequence, each
   -- carrying at most 7 bits of the encoded value.


   LEB_Encoding : array (1 .. Max_Octets) of Unsigned_8_T;
   --
   -- Holds the LEB128 encoding that was last read from a stream,
   -- for trouble-shooting purposes.


   LEB_Length : Natural := 0;
   --
   -- The number of octets in LEB_Encoding.


   procedure Collect_Bits (
      Stream : access Ada.Streams.Root_Stream_Type'Class;
      Item   : out    Longest_Unsigned_T;
      Bits   : out    Natural)
   --
   -- Reads LEB128 code from the stream and acculumates the data
   -- bits into Item. Also returns the number of data bits.
   --
   -- Raises Format_Error if some non-zero data bits overflow or
   -- if the length of the LEB128 octet sequence exceeds the maximum
   -- supported length.
   --
   is

      Octet : Longest_Unsigned_T;
      -- One octet from the LEB128 string.

      Data : Longest_Unsigned_T;
      -- The data bits in the Octet.

      Add : Longest_Unsigned_T;
      -- The Data bits shifted left to their correct position.

   begin

      Item := 0;

      Bits := 0;

      LEB_Length := 0;

      loop

         Unsigned_8_T'Read (Stream, Unsigned_8_T (Octet));

         -- Memorize the sequence:

         LEB_Length := LEB_Length + 1;

         LEB_Encoding(LEB_Length) := Unsigned_8_T (Octet);

         -- Add the data bits from this Octet to Number:

         Data := Octet and 2#0111_1111#;

         Add := Shift_Left (Value => Data, Amount => Bits);

         if Data /= Shift_Right (Value => Add, Amount => Bits) then
            -- Some non-zero bits were lost in the left shift.

            Output.Error (
                "LEB128 value has more than "
               & Output.Image (Natural'(Max_Bits))
               & " bits; overflow.");

            raise Format_Error;

         end if;

         Item := Item or Add;

         -- Count the number of bits and check for last Octet:

         Bits := Bits + 7;

         exit when (Octet and 2#1000_0000#) = 0;

         if Bits >= Max_Bits then
            -- This Octet was not marked as the last octet, but we
            -- have already collected as many bits as we can.

            Output.Error (
                 "LEB128 value has more than "
               & Output.Image (Natural'(Max_Bits))
               & " coding bits.");

            raise Format_Error;

         end if;

      end loop;

   end Collect_Bits;


   procedure Read_Signed (
      Stream : access Ada.Streams.Root_Stream_Type'Class;
      Item   : out    Number_Type'Base)
   is

      Number : Longest_Unsigned_T := 0;
      -- The accumulating number.

      Num_Bits : Natural;
      -- The number of bits read from the LEB128 string.

      Sign : Longest_Unsigned_T;
      -- The value 2**(Num_Bits - 1), which selects the "sign bit"
      -- in Number. Here we limit Num_Bits to no more than Max_Bits.

   begin

      -- Accumulate the bits in the number:

      Collect_Bits (
         Stream => Stream,
         Item   => Number,
         Bits   => Num_Bits);

      -- Handle the sign:

      Sign := Shift_Left (
         Value  => 1,
         Amount => Natural'Min (Max_Bits,
                   Natural'Min (Opt.Max_Value_Bits,
                                Num_Bits))
                   - 1);

      if (Number and Sign) = 0 then
         -- Non-negative number.

         Item := Number_Type (Number);

      else
         -- Negative number represented in two's complement.
         -- Note that Number >= Sign in this case.

         Item := (Number_Type (Number - Sign) - Number_Type (Sign - 1)) - 1;
         ---
         -- This peculiar expression subtracts 2**bits from the
         -- Number while trying to avoid overflow in the conversion
         -- from Longest_Unsigned_T to a signed Number_Type.

      end if;

   end Read_Signed;


   procedure Read_Unsigned (
      Stream : access Ada.Streams.Root_Stream_Type'Class;
      Item   : out    Number_Type'Base)
   is

      Num_Bits : Natural;
      -- The number of bits read from the LEB128 string.
      -- Not needed here.

   begin

      Collect_Bits (
         Stream => Stream,
         Item   => Longest_Unsigned_T (Item),
         Bits   => Num_Bits);

   end Read_Unsigned;


   procedure Read_Longest_With_Sign
   is new Read_Signed (Longest_Signed_T);


   procedure Read_Longest_Without_Sign
   is new Read_Unsigned (Longest_Unsigned_T);


   procedure Read_Longest_Signed (
      Stream : access Ada.Streams.Root_Stream_Type'Class;
      Item   : out    Longest_Signed_T'Base)
   is
   begin

      Read_Longest_With_Sign (Stream, Item);

   end Read_Longest_Signed;


   procedure Read_Longest_Unsigned (
      Stream : access Ada.Streams.Root_Stream_Type'Class;
      Item   : out    Longest_Unsigned_T'Base)
   is
   begin

      Read_Longest_Without_Sign (Stream, Item);

   end Read_Longest_Unsigned;


end Formats.LEB128;
