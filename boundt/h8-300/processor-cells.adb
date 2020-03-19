-- Processor.Cells (body)
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
-- $Revision: 1.7 $
-- $Date: 2015/10/26 22:19:14 $
--
-- $Log: processor-cells.adb,v $
-- Revision 1.7  2015/10/26 22:19:14  niklas
-- Version 4b, and moved to free open-source licence.
--
-- Revision 1.6  2009/12/02 14:20:01  niklas
-- BT-CH-0188: H8/300 updates for bit-widths and Word_T.
--
-- Revision 1.5  2005-10-20 15:31:05  niklas
-- Added function Veiling to support dynamic calls, and
-- for this reason moved Register_Set_T here, from its
-- old place in Decoder.IAR.
--
-- Revision 1.4  2005/09/06 12:18:20  niklas
-- Added (restored) Word_From_Octets, but this time as an array of
-- expressions, not assignments, so that it can be used in defining
-- assignments as well as in range-constraint assignments.
--
-- Revision 1.3  2005/05/18 13:54:04  niklas
-- Corrected the function Local to round an odd word offset upwards.
--
-- Revision 1.2  2005/05/11 18:04:46  niklas
-- As a consequence of changes in Decoder, removed the (now unused)
-- arrays Register_Less_Max, Register_Positive and Word_From_Octets.
--
-- Revision 1.1  2005/03/22 20:35:17  niklas
-- First version, to support arithmetic analysis in the Decoder.
--


with Output;


package body Processor.Cells is

   use H8_300;

   use type Arithmetic.Expr_Ref;


   --
   ---   Constructing cells with an address or offset:
   --


   function Local (
      Offset : Local_Offset_T;
      Width  : Width_T)
   return Storage.Cell_T
   is
   begin

      case Width is

      when Octet =>

         return Storage.Cell (Spec => (
            Kind  => Local_Byte,
            Local => Offset));

      when Word =>

         if Offset mod 2 /= 0 then

            Output.Note (
                 "Local Word with odd offset"
               & Local_Offset_T'Image (Offset));

         end if;

         return Storage.Cell (Spec => (
            Kind  => Local_Word,
            Local => Offset + (Offset mod 2)));
            --
            -- The Local offset is rounded *up* to an even number because
            -- local offsets increase towards lower addresses.

      end case;

   end Local;


   function Local (
      Offset : Local_Offset_T;
      Width  : Width_T)
   return Arithmetic.Variable_T
   is
   begin

      return Arithmetic.Expr (Storage.Cell_T'(Local (Offset, Width)));

   end Local;


   function Param (
      Offset : Param_Offset_T;
      Width  : Width_T)
   return Storage.Cell_T
   is
   begin

      case Width is

      when Octet =>

         return Storage.Cell (Spec => (
            Kind  => Param_Byte,
            Param => Offset));

      when Word => 

         if Offset mod 2 /= 0 then

            Output.Note (
                 "Parameter Word with odd offset"
               & Param_Offset_T'Image (Offset));

         end if;

         return Storage.Cell (Spec => (
            Kind  => Param_Word,
            Param => Offset - (Offset mod 2)));


      end case;

   end Param;


   function Param (
      Offset : Param_Offset_T;
      Width  : Width_T)
   return Arithmetic.Variable_T
   is
   begin

      return Arithmetic.Expr (Storage.Cell_T'(Param (Offset, Width)));

   end Param;


   function Memory (
      Address : Address_T;
      Width   : Width_T)
   return Storage.Cell_T
   is
   begin

      case Width is

      when Octet =>

         return Storage.Cell (Spec => (
            Kind => Mem_Byte,
            Addr => Address));

      when Word =>

         if Address mod 2 /= 0 then

            Output.Note (
                 "Memory Word with odd address"
               & Address_T'Image (Address));

         end if;

         return Storage.Cell (Spec => (
            Kind => Mem_Word,
            Addr => Address - (Address mod 2)));

      end case;

   end Memory;


   function Memory (
      Address : Address_T;
      Width   : Width_T)
   return Arithmetic.Variable_T
   is
   begin

      return Arithmetic.Expr (Storage.Cell_T'(Memory (Address, Width)));

   end Memory;


   --
   ---   Sub-cells and super-cells
   --


   Octet_Off : constant array (Byte_Part_T) of Address_Offset_T := (
      Low_Byte  => 1,
      High_Byte => 0);
   --
   -- The offset from the (even) memory address of a 16-bit word to
   -- its octet parts. This is a big-endian machine: the high byte is
   -- at the given address, the low byte at the next address.


   function Octet_Part (
      Whole : Storage.Cell_T;
      Part  : Byte_Part_T)
   return Arithmetic.Variable_T
   is

      Spec : constant Cell_Spec_T := Storage.Spec_Of (Whole);

   begin

      case Spec.Kind is

      when Rn =>

         return Register_Var(Spec.Num, Part);

      when Param_Word =>

         return Param (
            Offset => Spec.Param - (Spec.Param mod 2) + Octet_Off(Part),
            Width  => Octet);

      when Local_Word =>

         return Local (
            Offset => Spec.Local + (Spec.Local mod 2) - Octet_Off(Part),
            Width  => Octet);
         --
         -- The Local offset is rounded *up* to an even number because
         -- local offsets increase towards lower addresses. For the
         -- same reason, Octet_Off is subtracted instead of added.

      when Mem_Word =>

         return Memory (
            Address => Shift (
               Base   => Spec.Addr - (Spec.Addr mod 2),
               Offset => Octet_Off(Part)),
            Width => Octet);

      when others =>

         Output.Fault (
            Location => "Processor.Cells.Octet_Part",
            Text     =>
                 Storage.Image (Whole)
               & " is not a 16-bit word cell.");

         raise Program_Error;

      end case;

   end Octet_Part;


   function Low_Octet (Whole : Storage.Cell_T) 
   return Arithmetic.Variable_T
   is
   begin

      return Octet_Part (Whole, Low_Byte);

   end Low_Octet;


   function High_Octet (Whole : Storage.Cell_T)
   return Arithmetic.Variable_T
   is
   begin

      return Octet_Part (Whole, High_Byte);

   end High_Octet;


   function Whole_Word (Part : Storage.Cell_T)
   return Arithmetic.Variable_T
   is

      Spec : constant Cell_Spec_T := Storage.Spec_Of (Part);

   begin

      case Spec.Kind is

      when RnH | RnL =>

         return Register_Var(Spec.Num, Word);

      when Param_Byte =>

         return Param (
            Offset => Spec.Param - (Spec.Param mod 2),
            Width  => Word);

      when Local_Byte =>

         return Local (
            Offset => Spec.Local + (Spec.Local mod 2),
            Width  => Word);
         --
         -- The Local offset is rounded *up* to an even number because
         -- local offsets increase towards lower addresses.

      when Mem_Byte =>

         return Memory (
            Address => Spec.Addr - (Spec.Addr mod 2),
            Width   => Word);

      when others =>

         Output.Fault (
            Location => "Processor.Cells.Whole_Word",
            Text     =>
                 Storage.Image (Part)
               & " is not an 8-bit octet cell.");

         raise Program_Error;

      end case;

   end Whole_Word;


   --
   ---   Helpful operations
   --


   function Width_In_Bits (Item : H8_300.Width_T)
   return Arithmetic.Width_T
   is
      use type Arithmetic.Width_T;
   begin

      return Octet_Bits * Arithmetic.Width_T (Operand_Octets (Item));

   end Width_In_Bits;


   function Veiling (Registers : Register_Set_T)
   return Arithmetic.Effect_Ref
   is

      Veil : Arithmetic.Assignment_Set_T (
         Max_Size => Register_Var'Length(1) * Register_Var'Length(2));
      --
      -- Assigns an opaque value to each register in Registers and to
      -- each 8-bit part of that register.

   begin

      for R in Register_Number_T loop

         if Registers(R) then
            -- Register R shall be veiled.

            for P in Register_Part_T loop

               Arithmetic.Add (
                  To   => Veil,
                  More => Arithmetic.Set (Register_Var(R,P)));

            end loop;

         end if;

      end loop;

      return Arithmetic.To_Effect_Ref (Veil);

   end Veiling;


   --
   --    Package elaboration
   --


begin  -- Processor.Cells

   -- Create the register cells and corresponding expressions:

   for N in Register_Number_T loop

      Register_Cell(N, High_Byte) := Storage.Cell (Spec => (
         Kind => Processor.RnH,
         Num  => N));

      Register_Cell(N, Low_Byte) := Storage.Cell (Spec => (
         Kind => Processor.RnL,
         Num  => N));

      Register_Cell(N, Word) := Storage.Cell (Spec => (
         Kind => Processor.Rn,
         Num  => N));

   end loop;

   -- Create the corresponding expressions:

   for N in Register_Number_T loop

      for P in Register_Part_T loop

         Register_Var(N, P) := Arithmetic.Expr (Register_Cell(N, P));

      end loop;

   end loop;

   -- Create the frequently used expressions:

   for N in Register_Number_T loop

      Word_From_Octets(N) := Arithmetic.Conc (
         Register_Var(N, High_Byte),
         Register_Var(N, Low_Byte ));

   end loop;

end Processor.Cells;
