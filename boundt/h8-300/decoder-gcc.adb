-- Decoder.GCC (body)
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
-- $Revision: 1.9 $
-- $Date: 2015/10/26 22:19:13 $
--
-- $Log: decoder-gcc.adb,v $
-- Revision 1.9  2015/10/26 22:19:13  niklas
-- Version 4b, and moved to free open-source licence.
--
-- Revision 1.8  2009/12/02 14:20:01  niklas
-- BT-CH-0188: H8/300 updates for bit-widths and Word_T.
--
-- Revision 1.7  2007-03-22 18:51:34  niklas
-- BT-CH-0053.
--
-- Revision 1.6  2007/01/21 22:07:59  niklas
-- Updated for BT-CH-0041.
--
-- Revision 1.5  2005/10/20 17:05:33  niklas
-- Added function Unresolved_Call_Effect to support dynamic calls
-- as implemented in BT-CH-0014.
--
-- Revision 1.4  2005/07/28 20:10:43  niklas
-- Corrected COFF_Data_Cell to consider the type derivation so
-- that all pointers are taken as word-sized cells, whatever the
-- fundamental type is.
--
-- Revision 1.3  2005/07/26 12:12:58  niklas
-- Added function COFF_Data_Cell.
--
-- Revision 1.2  2005/03/25 16:57:02  niklas
-- Declared the new Jump_Index cell to have Fixed parameter mapping.
--
-- Revision 1.1  2005/03/22 20:45:36  niklas
-- First version.
--


with Decoder.GCC.Opt;
with H8_300;
with Output;
with Processor.Cells;
with Storage.Bounds;


package body Decoder.GCC is

   use Processor;


   --
   --    COFF symbol parsing
   --


   function COFF_Data_Cell (
      Object : Formats.COFF.Parsing.Symbol_Denotation_T)
   return Processor.Cell_Spec_T
   is
      use Formats.COFF;
      use Formats.COFF.Parsing;
      use H8_300;

      Fund : constant Fundamental_Type_T := Fundamental_Type (Object.Sym_Type);
      -- The fundamental type of the object.

      Deriv : constant Type_Derivation_T := Derivation (Object.Sym_Type);
      -- The type derivation.

      Width : H8_300.Width_T;
      -- The width of the object: octet or word.

   begin

      -- Is it a byte or a word? Or neither?

      if Deriv'Length = 0 then
         -- A basic type, not derived.

         case Fund is

         when T_Null | T_Char | T_Uchar =>

            Width := Octet;

         when T_Short | T_Ushort =>

            Width := Word;

         when T_Int | T_Uint =>

            -- An "int" can be 16 or 32 bits, depending on the
            -- compiler option -mint32.

            case GCC.Opt.Int_Size is

            when GCC.Opt.Bits16 =>

               Width := Word;

            when GCC.Opt.Bits32 =>

               raise No_Such_Cell;

            end case;

         when others =>
            -- A type that we do not track.

            raise No_Such_Cell;

         end case;

      elsif Deriv(Deriv'First) = DT_PTR then
         -- A pointer.

         Width := Word;

      else
         -- Some other derived type. Not for us.

         raise No_Such_Cell;

      end if;


      -- Which cell holds the object?

      case Object.Class is

      when C_Ext | C_Stat =>

         case Width is

         when Octet =>

            return (
               Kind => Mem_Byte,
               Addr => Processor.Address_T (Object.Value));

         when Word =>

            return (
               Kind => Mem_Word,
               Addr => Processor.Address_T (Object.Value));

         end case;

      when C_Reg | C_Regparm =>

         case Width is

         when Octet =>

            if Object.Value > Ulong_T (Register_Number_T'Last) then
               -- There is no such register.

               raise No_Such_Cell;

            end if;

            return (
               Kind => RnL,  -- TBD how to decide RnL / RnH
               Num  => Register_Number_T (Object.Value));

         when Word =>

            if Object.Value > Ulong_T (Register_Number_T'Last) then
               -- There is no such register.

               raise No_Such_Cell;

            end if;

            return (
               Kind => Rn,
               Num  => Register_Number_T (Object.Value));

         end case;

      when C_Arg | C_Auto =>

         raise No_Such_Cell;  -- TBA param/local

      when others =>

         raise No_Such_Cell;

      end case;

   end COFF_Data_Cell;


   --
   --    Calling protocols
   --


   Reg_Map :
      constant array (H8_300.Register_Number_T) of Calling.Map_Kind_T := (
      0 .. 3 => Calling.Fixed,
      4 .. 7 => Calling.Privy);
   --
   -- The parameter mapping used for a Register in the GCC Calling
   -- Protocol. The main implication is that "scratch" (clobbered)
   -- registers use a Fixed map; the "local" registers also use the
   -- same names but are preserved across the call so they can be defined
   -- as Privy cells. The same map holds for 8-bit (RnH, RnL) and
   -- 16-bit (Rn) registers.


   function Map_Kind (
      Callee : Storage.Cell_T;
      Under  : Protocol_T)
   return Calling.Map_Kind_T
   is

      Spec : constant Cell_Spec_T := Storage.Spec_Of (Callee);
      -- The cell specification.

   begin

      case Spec.Kind is

      when Flag_Kind_T =>
         -- The CCR condition flags are global cells.

         return Calling.Fixed;

      when Reg_Kind_T =>

         return Reg_Map(Spec.Num);

      when Param_Kind_T =>
         -- A cell that the callee sees as a Parameter is mapped to
         -- a Local cell in the caller; the mapping is dynamic and
         -- depends on the caller's stack-height at the call.

         return Calling.Dynamic;

      when Local_Kind_T =>
         -- A cell that the callee sees as a Local cell is not
         -- accessible to the caller.

         return Calling.Privy;

      when Mem_Kind_T =>
         -- Statically allocated memory cells are fixed.

         return Calling.Fixed;

      when Stack_Height =>
         -- The stack-height is private to each subprogram.

         return Calling.Privy;

      when Jump_Index =>
         -- The jump-index is a kind of global cell, although it
         -- should never be significant on entry to a subprogram
         -- or return from a subprogram.

         return Calling.Fixed;

      end case;

   end Map_Kind;


   function Caller_Cell (
      Callee : Storage.Cell_T;
      Under  : Protocol_T)
   return Storage.Cell_T
   is
      use type Arithmetic.Value_T;

      Callee_Spec : constant Cell_Spec_T := Storage.Spec_Of (Callee);
      -- The specification of the cell as the callee sees it.

      Param : Arithmetic.Value_T;
      -- The parameter offset of the callee cell.

      Caller_Height : Arithmetic.Value_T;
      -- The caller's local stack height at the call, if the
      -- cell is dynamically mapped.

      Local : Arithmetic.Value_T;
      -- The local offset of the caller cell.

   begin

      if Callee_Spec.Kind in Param_Kind_T then
         -- Dynamically mapped cell.

         Param := Arithmetic.Value_T (Callee_Spec.Param);

         Caller_Height := Storage.Bounds.Single_Value (Under.Intervals(1));
         -- Hoping that the stack-height is bounded to a single value.
         -- If not, the exception Storage.Bounds.Multiple_Values arises.

         Local := Caller_Height - Param;

         if Local >= Arithmetic.Value_T (Local_Offset_T'First) then
            -- The parameter reference corresponds to a local cell
            -- in the caller.

            return Processor.Cells.Local (
               Offset => Local_Offset_T (Local),
               Width  => Cell_Width(Callee_Spec.Kind));

         else
            -- The parameter offset is too large to refer to the
            -- caller's stack frame.

            Output.Error (
               "Parameter reference exceeds caller's frame");

            return Storage.No_Cell;

         end if;

      else
         -- A fixed cell (or a privy cell, but that should not happen).

         return Callee;

      end if;

   exception

   when Storage.Bounds.Multiple_Values =>
      -- The Stack Height in Upon is not bounded to a single value.

      return Storage.No_Cell;

   end Caller_Cell;


   function Sure_Invariant (
      Caller : Storage.Cell_T;
      Under  : Protocol_T;
      Stub   : Calling.Stub_Level_T)
   return Boolean
   is
      use type Calling.Map_Kind_T;

      Spec : constant Cell_Spec_T := Storage.Spec_Of (Caller);
      -- The cell specification.

   begin

      case Spec.Kind is

      when Flag_Kind_T =>
         -- The CCR condition flags can be modified by any call.

         return False;

      when Reg_Kind_T =>

         return Reg_Map(Spec.Num) = Calling.Privy;

      when Param_Kind_T =>
         -- A cell that the caller sees as a Parameter should
         -- normally not be modified by a call. TBM if the
         -- address of the cell is made available to the callee.

         return True;

      when Local_Kind_T =>
         -- A cell that the caller sees as a Local cell could
         -- be modified by a call if its address is made available
         -- to the callee. TBA address-taken analysis.

         return False;

      when Mem_Kind_T =>
         -- Statically allocated memory cells are available to
         -- any subprogram and so could be modified by any call.

         return False;

      when Stack_Height =>
         -- The stack-height is usually not preserved across a
         -- call because the callee pops the return address.

         return False;

      when Jump_Index =>
         -- The jump-index is a kind of global cell, although it
         -- should never be significant on entry to a subprogram
         -- or return from a subprogram. There should be no harm in
         -- considering to be volatile across calls.

         return False;

      end case;

   end Sure_Invariant;


   function Protocol (Program : Programs.Program_T)
   return Calling.Protocol_Ref
   is

      Proto : Protocol_Ref;
      -- The new protocol.

   begin

      Proto := new Protocol_T (
         Num_Stacks => Programs.Number_Of_Stacks (Program));

      Proto.Program := Program;

      return Calling.Protocol_Ref (Proto);

   end Protocol;


   function Unresolved_Call_Effect (Program : Programs.Program_T)
   return Arithmetic.Effect_Ref
   is
      use type Calling.Map_Kind_T;

      Scratch : Processor.Cells.Register_Set_T;
      -- The registers that are not mapped as "Privy" cells in the
      -- calling protocol. These are the "scratch" registers that may
      -- be altered by a call.

   begin

      for R in Scratch'Range loop

         Scratch(R) := Reg_Map(R) /= Calling.Privy;

      end loop;

      return Processor.Cells.Veiling (Scratch);

   end Unresolved_Call_Effect;


end Decoder.GCC;
