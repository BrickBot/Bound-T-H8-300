-- Formats (body)
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
-- $Revision: 1.8 $
-- $Date: 2015/10/24 20:53:54 $
--
-- $Log: formats.adb,v $
-- Revision 1.8  2015/10/24 20:53:54  niklas
-- Moved to free licence.
--
-- Revision 1.7  2014/06/20 19:46:33  niklas
-- Added write operations for Unsigned_16 and Unsigned_32.
--
-- Revision 1.6  2008-10-11 05:56:44  niklas
-- BT-CH-0147: Intel-Hex with linear 32-bit addresses.
--
-- Revision 1.5  2007/02/27 22:10:39  Niklas
-- Corrected 'Read procedures for scalar types to use the 'Base
-- subtype of the scalar type and not to use Read operations inherited
-- from parent types (and so having "intrinsic" convention).
--
-- Revision 1.4  2005/10/26 13:50:06  niklas
-- Using Formats.Output instead of standard Output.
--
-- Revision 1.3  2004/09/27 08:37:40  niklas
-- Corrected the Hex_Image functions to take into account the optional "0x"
-- prefix in the number of padding zeros.
-- Corrected Hex_Image for Signed_32_T to emit a '-' for negative values.
--
-- Revision 1.2  2004/08/23 13:23:31  niklas
-- Added bit-field extraction functions.
-- Made "0x" prefix optional in hexadecimal images.
--
-- Revision 1.1  2004/04/24 17:22:43  niklas
-- First version.
--


with Ada.Strings.Fixed;
with Hex;
with Formats.Output;


package body Formats is


   One_Octet : constant := 8;
   --
   -- The number of bits (bit positions) in one octet.


   --
   ---   Signed/unsigned conversions
   --


   function To_Signed (Item : Unsigned_8_T) return Signed_8_T
   is
      Sign_Bit : constant := 2**7;
   begin

      if (Item and Sign_Bit) = 0 then
         -- Sign bit zero => non-negative number 0 .. 2**7 - 1.

         return Signed_8_T (Item);

      else
         -- Sign bit one => negative number -2**7 .. -1.

         return Signed_8_T (Item xor Sign_Bit)
              + Signed_8_T'First;

      end if;

   end To_Signed;


   function To_Signed (Item : Unsigned_16_T) return Signed_16_T
   is
      Sign_Bit : constant := 2**15;
   begin

      if (Item and Sign_Bit) = 0 then
         -- Sign bit zero => non-negative number 0 .. 2**15 - 1.

         return Signed_16_T (Item);

      else
         -- Sign bit one => negative number -2**15 .. -1.

         return Signed_16_T (Item xor Sign_Bit)
              + Signed_16_T'First;

      end if;

   end To_Signed;


   function To_Signed (Item : Unsigned_32_T) return Signed_32_T
   is
      Sign_Bit : constant := 2**31;
   begin

      if (Item and Sign_Bit) = 0 then
         -- Sign bit zero => non-negative number 0 .. 2**31 - 1.

         return Signed_32_T (Item);

      else
         -- Sign bit one => negative number -2**31 .. -1.

         return Signed_32_T (Item xor Sign_Bit)
              + Signed_32_T'First;

      end if;

   end To_Signed;


   function To_Signed (Item : Unsigned_64_T) return Signed_64_T
   is
      Sign_Bit : constant := Unsigned_64_T'Modulus / 2;
   begin

      if (Item and Sign_Bit) = 0 then
         -- Sign bit zero => non-negative number 0 .. 2**63 - 1.

         return Signed_64_T (Item);

      else
         -- Sign bit one => negative number -2**63 .. -1.

         return Signed_64_T (Item xor Sign_Bit)
              + Signed_64_T'First;

      end if;

   end To_Signed;


   --
   ---   Assembling octet sequences into integer numbers
   --


   function Value (
      Octets : Octets_T;
      Endian : Endian_T)
   return Unsigned_32_T
   is

      Total : Unsigned_32_T := 0;

   begin

      case Endian is

      when Little =>

         for K in reverse Octets'Range loop

            Total := Shift_Left (Value => Total, Amount => One_Octet)
                  or Unsigned_32_T (Octets(K));

         end loop;

      when Big =>

         for K in Octets'Range loop

            Total := Shift_Left (Value => Total, Amount => One_Octet)
                  or Unsigned_32_T (Octets(K));

         end loop;

      end case;

      return Total;

   end Value;


   --
   ---   Reading and writing integer numbers
   --


   procedure Read_Unsigned_16 (
      Stream : access Ada.Streams.Root_Stream_Type'Class;
      Item   : out Unsigned_16_T'Base)
   is

      Less, More : Octet_T;
      -- The less and more significant octets.

   begin

      case Endian is

      when Little =>

         Octet_T'Read (Stream, Less);
         Octet_T'Read (Stream, More);

      when Big =>

         Octet_T'Read (Stream, More);
         Octet_T'Read (Stream, Less);

      end case;

      Item :=
         Shift_Left (
            Value  => Unsigned_16_T (More),
            Amount => One_Octet)
         or Unsigned_16_T (Less);

   end Read_Unsigned_16;


   procedure Write_Unsigned_16 (
      Stream : access Ada.Streams.Root_Stream_Type'Class;
      Item   : in     Unsigned_16_T'Base)
   is

      Less : constant Octet_T := Octet_T (Item and 16#FF#);
      More : constant Octet_T :=
         Octet_T (Shift_Right (Value => Item, Amount => 8) and 16#FF#);
      -- The less and more significant octets.

   begin

      case Endian is

      when Little =>

         Octet_T'Write (Stream, Less);
         Octet_T'Write (Stream, More);

      when Big =>

         Octet_T'Write (Stream, More);
         Octet_T'Write (Stream, Less);

      end case;

   end Write_Unsigned_16;


   procedure Read_Signed_16 (
      Stream : access Ada.Streams.Root_Stream_Type'Class;
      Item   : out Signed_16_T'Base)
   is

      Unsigned : Unsigned_16_T;
      -- The 16 bits as an unsigned number.

   begin

      Unsigned_16_T'Read (Stream, Unsigned);

      Item := To_Signed (Unsigned);

   end Read_Signed_16;


   procedure Read_Unsigned_32 (
      Stream : access Ada.Streams.Root_Stream_Type'Class;
      Item   : out Unsigned_32_T'Base)
   is

      Octets : Octets_T (0 .. 3);
      -- The octets in order from most to least significant.

   begin

      -- Read the octets in endianness order:

      case Endian is

      when Little =>

         for I in reverse Octets'Range loop
            Octet_T'Read (Stream, Octets(I));
         end loop;

      when Big =>

         for I in Octets'Range loop
            Octet_T'Read (Stream, Octets(I));
         end loop;

      end case;

      -- Compose the value:

      Item := 0;

      for I in Octets'Range loop

         Item :=
               Shift_Left (Value => Item, Amount => One_Octet)
            or Unsigned_32_T (Octets(I));

      end loop;

   end Read_Unsigned_32;


   procedure Write_Unsigned_32 (
      Stream : access Ada.Streams.Root_Stream_Type'Class;
      Item   : in     Unsigned_32_T'Base)
   is

      Less : constant Unsigned_16_T := Unsigned_16_T (Item and 16#FFFF#);
      More : constant Unsigned_16_T :=
         Unsigned_16_T (Shift_Right (Value => Item, Amount => 16)
                        and 16#FFFF#);
      -- The less and more significant 16-bit halves.

   begin

      case Endian is

      when Little =>

         Unsigned_16_T'Write (Stream, Less);
         Unsigned_16_T'Write (Stream, More);

      when Big =>

         Unsigned_16_T'Write (Stream, More);
         Unsigned_16_T'Write (Stream, Less);

      end case;

   end Write_Unsigned_32;


   procedure Read_Signed_32 (
      Stream : access Ada.Streams.Root_Stream_Type'Class;
      Item   : out Signed_32_T'Base)
   is

      Unsigned : Unsigned_32_T;
      -- The 32 bits as an unsigned number.

   begin

      Unsigned_32_T'Read (Stream, Unsigned);

      Item := To_Signed (Unsigned);

   end Read_Signed_32;


   procedure Read_Unsigned_64 (
      Stream : access Ada.Streams.Root_Stream_Type'Class;
      Item   : out    Unsigned_64_T'Base)
   is

      Octets : Octets_T (0 .. 7);
      -- The octets in order from most to least significant.

   begin

      -- Read the octets in endianness order:

      case Endian is

      when Little =>

         for I in reverse Octets'Range loop
            Octet_T'Read (Stream, Octets(I));
         end loop;

      when Big =>

         for I in Octets'Range loop
            Octet_T'Read (Stream, Octets(I));
         end loop;

      end case;

      -- Compose the value:

      Item := 0;

      for I in Octets'Range loop

         Item :=
               Shift_Left (Value => Item, Amount => One_Octet)
            or Unsigned_64_T (Octets(I));

      end loop;

   end Read_Unsigned_64;


   procedure Read_Signed_64 (
      Stream : access Ada.Streams.Root_Stream_Type'Class;
      Item   : out    Signed_64_T'Base)
   is

      Unsigned : Unsigned_64_T;
      -- The 64 bits as an unsigned number.

   begin

      Unsigned_64_T'Read (Stream, Unsigned);

      Item := To_Signed (Unsigned);

   end Read_Signed_64;


   --
   --    Extracting bit fields
   --


   function Bits (
      High : Octet_Bit_T;
      Low  : Octet_Bit_T;
      From : Octet_T)
   return Octet_T
   is
   begin

      return Octet_T (
         Bits (High => High, Low => Low, From => Unsigned_32_T (From)));

   end Bits;


   function Bits (
      High : Bit_16_T;
      Low  : Bit_16_T;
      From : Unsigned_16_T)
   return Unsigned_16_T
   is
   begin

      return Unsigned_16_T (
         Bits (High => High, Low => Low, From => Unsigned_32_T (From)));

   end Bits;


   function Bits (
      High : Bit_32_T;
      Low  : Bit_32_T;
      From : Unsigned_32_T)
   return Unsigned_32_T
   is

      All_One : constant Unsigned_32_T := Unsigned_32_T'Last;

   begin

      return Shift_Right (From     , Low            )
         and Shift_Right (All_One  , Low + 31 - High);

   end Bits;


   function Bit (Number : Octet_Bit_T; From : Octet_T) return Boolean
   is
   begin

      return Bits (High => Number, Low => Number, From => From) = 1;

   end Bit;


   function Bit (Number : Bit_16_T; From : Unsigned_16_T) return Boolean
   is
   begin

      return Bits (High => Number, Low => Number, From => From) = 1;

   end Bit;


   function Bit (Number : Bit_32_T; From : Unsigned_32_T) return Boolean
   is
   begin

      return Bits (High => Number, Low => Number, From => From) = 1;

   end Bit;


   --
   --    Strings
   --


   Max_String_Length : constant := 1_000;
   --
   -- Maximum length of a null-terminated string.
   -- Directory paths and file names are hardly longer.


   function String_To_Null (From : access Ada.Streams.Root_Stream_Type'Class)
   return String
   is

      Buff : String (1 .. Max_String_Length);
      Last : Natural := 0;
      -- The string is Buff(1 .. Last).

      Byte : Octet_T;
      -- From the string or null.

   begin

      -- Collect the bytes:

      loop

         Octet_T'Read (From, Byte);

         exit when Byte = 0;

         Last := Last + 1;

         if Last <= Buff'Last then

            Buff(Last) := Character'Val (Byte);

         end if;

      end loop;

      -- Warn if the string is too long:

      if Last > Buff'Last then

         Output.Warning (
              "String contains "
            & Output.Image (Last)
            & " characters, only "
            & Output.Image (Natural'(Buff'Length))
            & " kept.");

      end if;

      -- Return the string but not the null:

      return Buff(1 .. Last);

   end String_To_Null;


   --
   --    File segments:
   --


   function Is_Null (Item : Segment_Desc_T) return Boolean
   is
      use type IO.Count;
   begin

      return Item.Octets = 0;

   end Is_Null;


   --
   --    Image functions:
   --


   function Image (Item : Octet_T) return String
   is
   begin

      return Hex.Image (Hex.Byte_T (Item));

   end Image;


   function Image (Item : Octets_T) return String
   is

      Value : String (1 .. 2*Item'Length);
      -- The concatenated value.

      V : Positive := Value'First;
      -- The next index in Value.

   begin

      for I in Item'Range loop
         Value (V .. V+1) := Image (Item(I));
         V := V + 2;
      end loop;

      return Value;

   end Image;


   function Perhaps_0x (Prefix : Boolean) return String
   --
   -- Either "0x" or nothing.
   --
   is
   begin

      if Prefix then

         return "0x";

      else

         return "";

      end if;

   end Perhaps_0x;


   function Maybe_Minus (Negative : Boolean) return String
   --
   -- A minus sign if Negative value, else nothing.
   --
   is
   begin

      if Negative then

         return "-";

      else

         return "";

      end if;

   end Maybe_Minus;


   function Hex_Image (
      Item   : Unsigned_16_T;
      Width  : Positive := 6;
      Prefix : Boolean := True)
   return String
   is
      use Ada.Strings.Fixed;

      Digs  : constant String := Hex.Image (Hex.Word16_T (Item));
      -- The unpadded image.

      Pref : constant String := Perhaps_0x (Prefix);
      -- The possible prefix.

      Zeros : constant Natural :=
         Natural'Max (0, Width - Pref'Length - Digs'Length);
      -- The number of padding zeros we need.

   begin

      return Pref & Zeros * '0' & Digs;

   end Hex_Image;


   function Hex_Image (
      Item   : Unsigned_32_T;
      Width  : Positive := 10;
      Prefix : Boolean  := True)
   return String
   is
      use Ada.Strings.Fixed;

      Digs  : constant String := Hex.Image (Hex.Word32_T (Item));
      -- The unpadded image.

      Pref : constant String := Perhaps_0x (Prefix);
      -- The possible prefix.

      Zeros : constant Natural :=
         Natural'Max (0, Width - Pref'Length - Digs'Length);
      -- The number of padding zeros we need.

   begin

      return Pref & Zeros * '0' & Digs;

   end Hex_Image;


   function Hex_Image (
      Item   : Signed_32_T;
      Width  : Positive := 10;
      Prefix : Boolean  := True)
   return String
   is
      use Ada.Strings.Fixed;

      Digs  : constant String := Hex.Image (Hex.Word32_T (abs Item));
      -- The unpadded image.

      Sign : constant String := Maybe_Minus (Item < 0);
      -- The possible minus sign.

      Pref : constant String := Perhaps_0x (Prefix);
      -- The possible prefix.

      Zeros : constant Natural :=
         Natural'Max (0, Width - Sign'Length - Pref'Length - Digs'Length);
      -- The number of padding zeros we need.

   begin

      return Sign & Pref & Zeros * '0' & Digs;

   end Hex_Image;


end Formats;
