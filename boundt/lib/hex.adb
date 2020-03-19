-- Hex (body)
--
-- Hexadecimal string images of unsigned numbers.
-- Author: Mikko Ala-Fossi, Space Systems Finland, 1999
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
-- $Revision: 1.5 $
-- $Date: 2015/10/24 20:53:54 $
--
-- $Log: hex.adb,v $
-- Revision 1.5  2015/10/24 20:53:54  niklas
-- Moved to free licence.
--
-- Revision 1.4  2008-04-29 05:33:52  niklas
-- Corrected Image for Word24_T.
--
-- Revision 1.3  2007/03/11 13:23:27  niklas
-- Added flexible-width Image for Word32_T.
--
-- Revision 1.2  2004/04/24 17:15:26  niklas
-- First Tidorum version. Types from Interfaces, add 64-bit.
--
-- Revision 1.1  2000/12/03 10:41:01  holsti
-- Moved from gen to lib.
--
-- Revision 1.2  2000/07/02 08:13:27  holsti
-- Renamed Nibble to Nybble.
--
-- Revision 1.1  2000/04/23 21:41:15  holsti
-- Added Flow package.
--


package body Hex is


   Hexa_Char : constant array (Nybble_T) of Character :=
     ('0', '1', '2', '3', '4', '5', '6', '7', '8', '9',
      'A', 'B', 'C', 'D', 'E', 'F');


   function Image (Value : Nybble_T) return String
   is
   begin

      return (1 => Hexa_Char(Value));

   end Image;


   function Image (Value : Byte_T) return String
   is
   begin

      return
         (1 => Hexa_Char(Nybble_T (Shift_Right (Value, 4))),
          2 => Hexa_Char(Nybble_T (Value and 16#F#)));

   end Image;


   function Image (Value : Word16_T) return String
   is
   begin

      return
           Image (Byte_T (Shift_Right (Value, 8)))
         & Image (Byte_T (Value and 16#FF#));

   end Image;


   function Image (Value : Word24_T) return String
   is
   begin

      return
           Image (  Byte_T (Shift_Right (Word32_T (Value), 16)))
         & Image (Word16_T (Value and 16#FFFF#));

   end Image;


   function Image (Value : Word32_T) return String
   is
   begin

      return
           Image (Word16_T (Shift_Right (Value, 16)))
         & Image (Word16_T (Value and 16#FFFF#));

   end Image;


   function Image (Value : Word64_T) return String
   is
   begin

      return
           Image (Word32_T (Shift_Right (Value, 32)))
         & Image (Word32_T (Value and 16#FFFF_FFFF#));

   end Image;


   function Image (Value : Word32_T; Octets : in Positive)
   return String
   is

      Max_Digs : constant := 16;
      -- Maximum number of hex digits.

      Digs  : String (1 .. Max_Digs);
      First : Positive := Digs'Last + 1;
      -- The hex digits for the value are Digs(First .. Digs'Last).

      Num_Digs : Natural := 0;
      -- Number of hex digits encoded.

      Rest : Word32_T := Value;
      -- What remains to encode.

   begin

      loop

         First := First - 1;

         Digs(First) := Hexa_Char(Nybble_T (Rest and 16#F#));

         Num_Digs := Num_Digs + 1;

         Rest := Shift_Right (Rest, 4);

         exit when Rest = 0 and Num_Digs >= 2 * Octets;

      end loop;

      return Digs(First .. Digs'Last);

   end Image;


   function Image (Value : Word64_T; Octets : in Positive)
   return String
   is

      Max_Digs : constant := 16;
      -- Maximum number of hex digits.

      Digs  : String (1 .. Max_Digs);
      First : Positive := Digs'Last + 1;
      -- The hex digits for the value are Digs(First .. Digs'Last).

      Num_Digs : Natural := 0;
      -- Number of hex digits encoded.

      Rest : Word64_T := Value;
      -- What remains to encode.

   begin

      loop

         First := First - 1;

         Digs(First) := Hexa_Char(Nybble_T (Rest and 16#F#));

         Num_Digs := Num_Digs + 1;

         Rest := Shift_Right (Rest, 4);

         exit when Rest = 0 and Num_Digs >= 2 * Octets;

      end loop;

      return Digs(First .. Digs'Last);

   end Image;


end Hex;
