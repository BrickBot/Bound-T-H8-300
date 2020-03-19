-- Processor.Cells (decl)
--
-- The main storage cells for the Renesas H8/300 processor.
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
-- $Date: 2015/10/26 22:19:15 $
--
-- $Log: processor-cells.ads,v $
-- Revision 1.8  2015/10/26 22:19:15  niklas
-- Version 4b, and moved to free open-source licence.
--
-- Revision 1.7  2009/12/02 14:20:01  niklas
-- BT-CH-0188: H8/300 updates for bit-widths and Word_T.
--
-- Revision 1.6  2005-10-20 15:31:05  niklas
-- Added function Veiling to support dynamic calls, and
-- for this reason moved Register_Set_T here, from its
-- old place in Decoder.IAR.
--
-- Revision 1.5  2005/09/06 12:18:20  niklas
-- Added (restored) Word_From_Octets, but this time as an array of
-- expressions, not assignments, so that it can be used in defining
-- assignments as well as in range-constraint assignments.
--
-- Revision 1.4  2005/05/18 13:54:04  niklas
-- Corrected the function Local to round an odd word offset upwards.
--
-- Revision 1.3  2005/05/11 18:04:46  niklas
-- As a consequence of changes in Decoder, removed the (now unused)
-- arrays Register_Less_Max, Register_Positive and Word_From_Octets.
--
-- Revision 1.2  2005/03/25 16:56:22  niklas
-- Added the Jump_Index cell and variable, for implementing jumps
-- via address tables.
--
-- Revision 1.1  2005/03/22 20:35:17  niklas
-- First version, to support arithmetic analysis in the Decoder.
--


with Arithmetic;
with H8_300;
with Storage;

pragma Elaborate_All (Arithmetic);
pragma Elaborate_All (Storage);


package Processor.Cells is


   pragma Elaborate_Body;
   --
   -- To initialize those public variables that are not directly
   -- initialized in their declarations.


   use H8_300;


   --
   ---   Basic processor cells:
   --


   Equal_Cell : constant Storage.Cell_T :=
      Storage.Cell (Spec => (Kind => Processor.Equal));
   --
   -- The cell that models the "zero" or "equal" condition.


   Equal_Flag : constant Arithmetic.Expr_Ref :=
      Arithmetic.Expr (Equal_Cell);
   --
   -- The expression that consists of just the Equal flag value.


   Less_Cell : constant Storage.Cell_T :=
      Storage.Cell (Spec => (Kind => Less));
   --
   -- The cell that models the "negative" or "less than" condition.


   Less_Flag : constant Arithmetic.Expr_Ref :=
      Arithmetic.Expr (Less_Cell);
   --
   -- The expression that consists of just the Less flag value.


   Flag : constant array (Flag_Kind_T) of Arithmetic.Variable_T := (
      Equal => Equal_Flag,
      Less  => Less_Flag);
   --
   -- The flag cells as variables indexed by the cell kind.


   Register_Cell :
      array (Register_Number_T, Register_Part_T) of Storage.Cell_T;
   --
   -- The cells that model registers and register parts.
   -- The array is initialized by the elaboration code of the body.


   Register_Var :
      array (Register_Number_T, Register_Part_T) of Arithmetic.Variable_T;
   --
   -- The expressions (variables) that consist of one register cell.
   -- The array is initialized by the elaboration code of the body.


   SP_Cell : Storage.Cell_T renames Register_Cell(H8_300.SP, Word);
   --
   -- The stack pointer = R7.


   SP : Arithmetic.Variable_T renames Register_Var(H8_300.SP, Word);
   --
   -- The stack pointer as an expression.


   SH_Cell : constant Storage.Cell_T :=
      Storage.Cell (Spec => (Kind => Processor.Stack_Height));
   --
   -- The cell that models the local stack height.


   SH : constant Arithmetic.Variable_T :=
      Arithmetic.Expr (SH_Cell);
   --
   -- The local stack height as an expression or variable.


   Jump_Index_Cell : constant Storage.Cell_T :=
      Storage.Cell (Spec => (Kind => Processor.Jump_Index));
   --
   -- The cell that models the jump index for a jump-via-table.


   Jump_Index_Var : constant Arithmetic.Variable_T :=
      Arithmetic.Expr (Jump_Index_Cell);
   --
   -- The jump index cell as an expression or variable.



   --
   ---   Constructing cells with an address or offset:
   --


   function Local (
      Offset : Local_Offset_T;
      Width  : Width_T)
   return Storage.Cell_T;
   --
   -- A local variable cell.
   --
   -- If Width = Word and Offset is odd, the returned Cell will
   -- have a Local offset rounded up to an even number.


   function Local (
      Offset : Local_Offset_T;
      Width  : Width_T)
   return Arithmetic.Variable_T;
   --
   -- A local variable cell, as a Variable_T.


   function Param (
      Offset : Param_Offset_T;
      Width  : Width_T)
   return Storage.Cell_T;
   --
   -- A parameter cell.
   --
   -- If Width = Word and Offset is odd, the returned Cell will
   -- have a Param offset rounded down to an even number.


   function Param (
      Offset : Param_Offset_T;
      Width  : Width_T)
   return Arithmetic.Variable_T;
   --
   -- A parameter cell, as a Variable_T.


   function Memory (
      Address : Address_T;
      Width   : Width_T)
   return Storage.Cell_T;
   --
   -- A memory cell.
   --
   -- If Width = Word and Address is odd, the returned Cell will
   -- have a memoy Addr rounded down to an even number.


   function Memory (
      Address : Address_T;
      Width   : Width_T)
   return Arithmetic.Variable_T;
   --
   -- A memory cell, as a Variable_T.


   --
   ---   Frequently used special expressions
   --


   Max_Octet : constant Arithmetic.Expr_Ref := Arithmetic.Const (
      Value  => Arithmetic.Word_T (Octet_T'Last),
      Width  => Octet_Bits,
      Signed => False);
   --
   -- The maximum value of an unsigned octet.


   Word_From_Octets : array (Register_Number_T) of Arithmetic.Expr_Ref;
   --
   -- The expression RnH*256 + RnL which computes the value of the
   -- word register Rn from its octet parts RnH and RnL.


   --
   ---   Sub-cells and super-cells
   --


   function Octet_Part (
      Whole : Storage.Cell_T;
      Part  : Byte_Part_T)
   return Arithmetic.Variable_T;
   --
   -- The chosen octet Part of the given Whole word cell.


   function Low_Octet (Whole : Storage.Cell_T)
   return Arithmetic.Variable_T;
   --
   -- The variable that models the low octet of the Whole word cell.


   function High_Octet (Whole : Storage.Cell_T)
   return Arithmetic.Variable_T;
   --
   -- The variable that models the high octet of the Whole word cell.


   function Whole_Word (Part : Storage.Cell_T)
   return Arithmetic.Variable_T;
   --
   -- The variable that models the word of which the Part is one octet.


   --
   ---   Helpful operations
   --


   function Width_In_Bits (Item : Width_T)
   return Arithmetic.Width_T;
   --
   -- The width in bits, of an octet or word cell.


   type Register_Set_T is array (Register_Number_T) of Boolean;
   --
   -- A set of H8/300 registers.
   -- Members of the set are marked by True values.


   function Veiling (Registers : Register_Set_T)
   return Arithmetic.Effect_Ref;
   --
   -- An effect that veils (assigns unknown values to) the given Registers.


   --
   ---   Useful constants
   --


   Zero_Octet : constant Arithmetic.Expr_Ref :=
      Arithmetic.Zero (Width => Octet_Bits);
   --
   -- A zero octet.


   Zero_Word : constant Arithmetic.Expr_Ref :=
      Arithmetic.Zero (Width => Word_Bits);
   --
   -- A zero word.


   Zero : constant array (Width_T) of Arithmetic.Expr_Ref := (
      Octet => Zero_Octet,
      Word  => Zero_Word);
   --
   -- A zero octet, or a zero word, as chosen by the width.


   One_Octet : constant Arithmetic.Expr_Ref :=
      Arithmetic.One (Width => Octet_Bits);
   --
   -- An octet-wide unit.


   One_Word : constant Arithmetic.Expr_Ref :=
      Arithmetic.One (Width => Word_Bits);
   --
   -- A word-wide unit.


end Processor.Cells;
