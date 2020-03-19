-- Arithmetic (body)
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
-- $Revision: 1.94 $
-- $Date: 2015/10/24 20:05:45 $
--
-- $Log: arithmetic.adb,v $
-- Revision 1.94  2015/10/24 20:05:45  niklas
-- Moved to free licence.
--
-- Revision 1.93  2015/10/02 19:13:49  niklas
-- Improved Report_Width_Mismatch to display the full operands.
--
-- Revision 1.92  2014/06/20 21:19:16  niklas
-- Added Image (Width_T).
--
-- Revision 1.91  2013-02-12 08:47:19  niklas
-- BT-CH-0245: Global volatile cells and "volatile" assertions.
--
-- Revision 1.90  2013-02-05 20:15:48  niklas
-- Extended Reference_Bounded (Expr_Ref) to trace the resolved or
-- constrained boundable references, as well as the applied bounds,
-- optional on Options.General.Trace_Resolution.
--
-- Revision 1.89  2011-09-08 08:45:09  niklas
-- Added Is_Small_Negative and Opposite_Sign for Interval_T.
-- Note: The latter function may be partially redundant with
-- the unary "-" operator for Word_Interval_T in Storage.Bounds.
--
-- Revision 1.88  2011-09-06 17:45:17  niklas
-- Corrected Defining_Assignments (Effect_T) return Effect_T to return
-- only the defined part of its local array Defs. (Error found while
-- implementing ALF export.)
--
-- Revision 1.87  2010-03-23 07:38:11  niklas
-- Added Truncate_Low and Truncate_High on Word_T.
-- Corrected functions "+" and "-" on Expr_Ref not to refer to the
-- Signed component of a non-Const operand when simplifying for
-- small negative constant operands.
--
-- Revision 1.86  2010-01-02 20:27:38  niklas
-- BT-CH-0211: Better error checks in Accumulate.
--
-- Revision 1.85  2010-01-01 12:35:40  niklas
-- BT-CH-0209: Arithmetic.Binary allows Conc with mixed width.
--
-- Revision 1.84  2009-12-21 14:31:41  niklas
-- BT-CH-0200: Corrected flag decoding from register-based shifts.
--
-- Revision 1.83  2009-12-06 13:44:50  niklas
-- BT-CH-0184, more: More width-mismatch checks.
--
-- Revision 1.82  2009-11-27 11:28:06  niklas
-- BT-CH-0184: Bit-widths, Word_T, failed modular analysis.
--
-- Revision 1.81  2009-10-07 19:26:09  niklas
-- BT-CH-0183: Cell-sets are a tagged-type class.
--
-- Revision 1.80  2009/05/15 12:16:46  niklas
-- BT-CH-0173: Handling cells used by Range_Post constraints.
--
-- Revision 1.79  2009/04/16 10:06:18  niklas
-- BT-CH-0170: Corrected Flow.Origins.Is_Identical to Is_Initial_Value.
--
-- Revision 1.78  2009/03/10 17:06:43  niklas
-- Added two functions Definition_Of to find and return the assignment
-- defining a given target cell within an effect or an assignment set.
-- Added the No_Such_Assignment exception to report failures.
-- Extended the binary "+" and "-" expression constructors to omit zero
-- terms and compute a constant result for constant operands. As a
-- result, the unary "-" constructor also does so.
--
-- Revision 1.77  2008/10/24 12:10:57  niklas
-- BT-CH-0152: Added function Is_Variable.
--
-- Revision 1.76  2008/10/18 20:14:58  niklas
-- Added function Defining_Assignments, for ALF export.
--
-- Revision 1.75  2008/06/29 06:13:50  niklas
-- Added convenience functions Is_Constant and Constant_Value, and
-- the related exception Expression_Not_Constant.
--
-- Revision 1.74  2008/06/20 10:11:53  niklas
-- BT-CH-0132: Data pointers using Difference (Expr, Cell, Bounds).
--
-- Revision 1.73  2008/06/18 20:52:55  niklas
-- BT-CH-0130: Data pointers relative to initial-value cells.
--
-- Revision 1.72  2008/04/18 12:11:31  niklas
-- Added function Is_Cell.
--
-- Revision 1.71  2007/10/31 12:16:00  niklas
-- BT-CH-0095: Arithmetic analysis of "live" dynamic data refs.
--
-- Revision 1.70  2007/10/28 09:32:44  niklas
-- BT-CH-0092: Arithmetic analysis of dynamic data refs is optional.
--
-- Revision 1.69  2007/10/04 19:48:51  niklas
-- BT-CH-0083: Resolving dynamic access to constant data.
--
-- Revision 1.68  2007/10/02 20:37:57  niklas
-- BT-CH-0080: One-bit bitwise Boolean arithmetic operators.
--
-- Revision 1.67  2007/08/02 11:11:31  niklas
-- Added Image function for Assignment_Set_T.
-- Implemented the option Check_Dup_Target in the procedures that Add
-- assignments or effects to an assignment set, with the help of the
-- new functions Same_Variable and Is_New.
--
-- Revision 1.66  2007/07/26 11:08:52  niklas
-- BT-CH-0066. Fracture assignments.
--
-- Revision 1.65  2007/07/21 18:18:40  niklas
-- BT-CH-0064. Support for AVR/IAR switch-handler analysis.
--
-- Revision 1.64  2007/01/13 13:51:01  niklas
-- BT-CH-0041.
--
-- Revision 1.63  2006/11/20 18:43:04  niklas
-- Added procedure Veil for a single variable.
--
-- Revision 1.62  2006/10/24 08:44:29  niklas
-- BT-CH-0028.
--
-- Revision 1.61  2006/08/22 13:20:09  niklas
-- Extended function Cell to return Unknown for No_Cell.
--
-- Revision 1.60  2006/05/06 06:59:19  niklas
-- BT-CH-0021.
--
-- Revision 1.59  2006/04/28 09:26:11  niklas
-- Added a new primitive function Values (Expr, Bounds_T).
-- Corrected Interval (Cell, Bounds) to redispatch on Bounds.
-- Added a not-implemented-Fault to Interval (Expr, Bounds).
--
-- Revision 1.58  2006/04/22 21:41:42  niklas
-- Added function Set (Flag Cell, Boolean).
--
-- Revision 1.57  2006/04/10 08:18:32  niklas
-- Changed function Const (Value) return Expr_Ref to accept any
-- Value, even values that exceed the calculator range, to let
-- eg. constant propagation work on such values.
--
-- Revision 1.56  2006/02/27 09:34:12  niklas
-- Added function Length (Expr_Ref).
-- Extended functions "+" and "-" for Expr_Ref to convert
-- addition or subtraction of a negative constant (right
-- operand) to subtraction or addition of the corresponding
-- positive constant, respectively. This makes for a neater
-- display of such expressions.
--
-- Revision 1.55  2005/10/20 19:34:00  niklas
-- BT-CH-0016.
--
-- Revision 1.54  2005/09/12 19:02:56  niklas
-- BT-CH-0008.
--
-- Revision 1.53  2005/09/06 12:35:40  niklas
-- Removed from Add_Cells_Defined (Effect) the check and Fault for
-- range-targets that are not also definition-targets in the same
-- effect. This check was, firstly, wrongly implemented (because the
-- To set may initially contain target cells from other effects), and
-- secondly unmotivated (I can no longer see any reason why a target
-- cell should not appear only in a range-assignment).
--
-- Revision 1.52  2005/06/28 07:03:52  niklas
-- Changed image of arithmetic effect to separate assignments
-- by commas instead of periods. Much nicer.
--
-- Revision 1.51  2005/05/09 15:25:07  niklas
-- Added function Defining_Assignments.
--
-- Revision 1.50  2005/04/20 12:04:35  niklas
-- Added procedure Add_Reference_Bases.
--
-- Revision 1.49  2005/04/18 10:50:04  niklas
-- Added functions to check if expressions and assignments contain
-- (unresolved) dynamic memory references: Targets_Reference and
-- two forms of Uses_Reference, for expressions and assignments.
--
-- Revision 1.48  2005/03/24 18:09:19  niklas
-- Added Value_T as a renaming of Processor.Value_T.
-- Added function Sign (Value_T).
-- Added subtype Relation_Kind_T for the relational operators.
-- Added procedure Accumulate to create simplified affine expressions.
--
-- Revision 1.47  2005/03/21 22:21:18  niklas
-- Added Variable_List_T and procedure Veil.
--
-- Revision 1.46  2005/03/20 12:29:15  niklas
-- Added unary "-" expression constructor.
--
-- Revision 1.45  2005/03/19 19:51:16  niklas
-- Added function Set_Flag.
--
-- Revision 1.44  2005/02/16 21:11:36  niklas
-- BT-CH-0002.
--
-- Revision 1.43  2004/08/09 19:51:27  niklas
-- BT-CH-0001.
--
-- Revision 1.42  2004/04/25 14:19:22  niklas
-- First Tidorum version.
-- Moved all Cell_T stuff to the new package Storage.
-- Added support for multiplication where neither operand is a constant.
-- Added bitwise operations.
-- Added range constraints ("range assignments").
-- Added support for crude aliasing analysis using alias ranges.
-- Support Storage Location_Ts where the storage cell(s) for a variable
-- depend on the code address.
-- Added List_Values and List_Difference to help enumerate the values that
-- satisfy given bounds and alignment (multiplicity) constraints, to help
-- resolve the dynamic flow in case/switch code.
-- Added function parameter to Image cells in the Image of expressions.
--
-- Revision 1.41  2003/02/27 14:36:00  holsti
-- Some warnings are conditional on Opt.Warn_Large_Literal.
--
-- Revision 1.40  2003/02/17 16:23:02  holsti
-- Added operators "<" and ">" to check if a value is less or greater
-- than the lower or upper limit given by a bound.
--
-- Revision 1.39  2003/02/17 14:13:03  holsti
-- Added option -calc_max, replacing the constants Min/Max_Int_Omega,
-- and reduced the default limit on literals passed to the calculator.
-- Added version identification to the "usage" print-out.
--
-- Revision 1.38  2001/12/03 13:52:11  holsti
-- Is_Member checks index in range, first.
--
-- Revision 1.37  2001/05/20 13:23:17  holsti
-- Added Erase for Assignment_Set_T (NC_117).
--
-- Revision 1.36  2001/03/15 07:16:25  holsti
-- Intersection of cell-sets added.
--
-- Revision 1.35  2001/03/14 16:53:48  holsti
-- Unknown expression added, and propagates in operators.
--
-- Revision 1.34  2001/03/10 22:36:28  holsti
-- Image of No_Cell_Set is [undefined]".
--
-- Revision 1.33  2001/03/10 00:36:53  holsti
-- Cell attributes Global and Var/Const removed, and also all operations
-- related to them.
-- In Add_Cells_Used (By Assignment), the Added parameter is "in out".
-- Operator "and" Cell_Bound_List_T added.
--
-- Revision 1.32  2001/02/11 13:51:00  holsti
-- Editorial and coding style corrections.
--
-- Revision 1.31  2001/02/01 10:50:27  ville
-- Removed unused cells from live cells
--
-- Revision 1.30  2001/01/07 21:55:40  holsti
-- Cell-set operations added to support liveness analysis.
--
-- Revision 1.29  2000/12/29 13:19:57  holsti
-- Removed tbas etc. in favour of NCs.
--
-- Revision 1.28  2000/12/28 17:47:11  holsti
-- Card added (number of elements in cell-set).
--
-- Revision 1.27  2000/12/28 12:23:00  holsti
-- Single_Value and Multiple_Values added.
--
-- Revision 1.26  2000/12/21 10:12:35  sihvo
-- Added function Is_Defined.
--
-- Revision 1.25  2000/12/05 15:34:05  holsti
-- Deleted Stack_Pointer, Macro stuff. Added No_Cell.
--
-- Revision 1.24  2000/11/29 14:52:15  holsti
-- Added function Max for Limit_T, and "&" for Bound_T.
--
-- Revision 1.23  2000/11/29 09:11:59  holsti
-- Unnecessary "with" clause removed.
--
-- Revision 1.22  2000/11/23 13:07:25  saarinen
-- Here are really what was promised below.
-- Sorry for mix-up.
--
-- Revision 1.21  2000/11/23 12:57:15  saarinen
-- Addes properties Counter and Global into Cell_Object_T
-- and functions to handle cells with these properties.
-- Added Image functions for Cell_Set_T and Cell_List_T.
--
-- Revision 1.20  2000/10/19 12:50:17  saarinen
-- Fixed function Ceil.
--
-- Revision 1.19  2000/10/19 10:18:58  saarinen
-- Function '=' fixed for different ranges of operands.
--
-- Revision 1.18  2000/10/06 13:28:21  saarinen
-- Added function Bounds_For_Cells and fixed function Ceil.
--
-- Revision 1.17  2000/09/20 18:42:54  saarinen
-- Added function Constant_Cells.
--
-- Revision 1.16  2000/09/08 12:59:55  saarinen
-- Changed the handling of oversized (for omega) constants.
-- Cell_Set operations fixed for Sets of different number of cells.
--
-- Revision 1.15  2000/08/21 13:08:57  holsti
-- Avoided erroneous recursion in = for cell sets.
--
-- Revision 1.14  2000/08/20 21:02:25  holsti
-- Add_Cells_Used accepts null expression.
--
-- Revision 1.13  2000/08/18 18:10:52  holsti
-- Empty cell-set available even if no cells defined.
--
-- Revision 1.12  2000/08/04 08:23:07  saarinen
-- Added functions Empty for Cell_Bound_List_T,
-- Is_Empty for Cell_Set_T,and Known for Bound_T.
--
-- Revision 1.11  2000/07/24 22:28:20  holsti
-- Bound_T operators Void, "<=", "-" added.
--
-- Revision 1.10  2000/07/18 19:52:37  holsti
-- Min for Limit_T added.
--
-- Revision 1.9  2000/07/17 20:58:19  holsti
-- Added some Cell_Set_T and Limit_T operations.
--
-- Revision 1.8  2000/07/16 13:09:51  holsti
-- Getting the cells used/defined by expressions and effects.
--
-- Revision 1.7  2000/07/12 12:25:29  holsti
-- Cell_Bound_T added. Step_Name and Get_Step_Cell deleted.
--
-- Revision 1.6  2000/06/29 14:06:34  holsti
-- Modified Bound_T (using Limit_T), add Floor & Ceil.
--
-- Revision 1.5  2000/06/28 13:39:25  holsti
-- Assignment_Set_T added.
--
-- Revision 1.4  2000/06/22 10:16:21  saarinen
-- Added constant Cell_T for Step Cell
--
-- Revision 1.3  2000/06/16 14:13:53  saarinen
-- Improved Cell_Set operations.
--
-- Revision 1.2  2000/06/16 10:37:50  saarinen
--
-- Added const flag to Cell_Object_T with operations.
-- Implemented Cell_Set operations.
--
-- Revision 1.1  2000/06/11 19:03:12  holsti
-- Arithmetic package separated from Calculator.
--


with Arithmetic.Opt;
with Options.General;
with Output;
with Storage.List_Cell_Sets;


package body Arithmetic is


   use Arithmetic_Base;
   use type Storage.Cell_T;


   --
   ---   Integer (signed) numbers
   --


   function Sign (Value : Value_T) return Value_T
   is
   begin

      if    Value > 0 then return  1;
      elsif Value < 0 then return -1;
      else                 return  0;
      end if;

   end Sign;


   --
   ---   Binary (unsigned) bit-string words
   --


   function Image (Item : Width_T) return String
   is
   begin

      return Image (Value_T (Item));

   end Image;


   function Image (
      Item   : Word_T;
      Width  : Width_T;
      Signed : Boolean)
   return String
   is

      Word_Image : constant String := Image (Item);
      -- The unsigned view.

      Sign_Value : Value_T;
      -- The signed value, if Signed.

   begin

      if Signed then
         -- Show the unsigned value first.

         Sign_Value := Signed_Value (Item, Width);

         if Sign_Value < 0 then
            -- Add the signed value after the unsigned value.

            return Word_Image & '[' & Image (Sign_Value) & ']';

         elsif Opt.Show_Plus_Sign then
            -- Add a mark to show that the value is Signed.

            return Word_Image & "[+]";

         else
            -- Just show the value itself (signed = unsigned).

            return Word_Image;

         end if;

      else

         return Word_Image;

      end if;

   end Image;


   function Max_Word (Width : Width_T) return Word_T
   is
   begin

      return Shift_Left (Value => 1, Amount => Natural (Width)) - 1;

   end Max_Word;


   function Mask (Word : Word_T) return Word_T
   is

      Ones : Word_T := 0;
      -- All "n" lowest bits = 1.
      -- Initially n = 0.

   begin

      -- Slow sequential implementation.
      -- Could do a binary search.

      while Ones < Word loop

         Ones := Shift_Left (Value => Ones, Amount => 1) or 1;

      end loop;

      return Ones;

   end Mask;


   function Unsigned_Word (
      Value : Value_T;
      Width : Width_T)
   return Word_T
   is

      Max : constant Word_T := Max_Word (Width);
      -- The maximum value of a word of this size.

      Word_Value : Word_T;
      -- The converted value, full size.

      Word : Word_T;
      -- The converted value, Word_Size.

      Excess : Boolean;
      -- Whether the value was too large to be understood
      -- as an unsigned word of Word_Size bits.

   begin

      if Value >= 0 then
         -- A non-negative value is converted directly.

         Word_Value := Word_T (Value);
         -- Possible constraint error.

         Word := Word_Value and Max;

         Excess := Word /= Word_Value;

      else
         -- A negative value is reinterpreted using the sign
         -- as the most significant value bit:

         Word_Value := Word_T (- Value);
         -- Possible constraint error, else the result is
         -- in the range from 1 (for Value = -1) to Word_T'Last.

         Word_Value := Word_T'Last - Word_Value + 1;

         Word := Word_Value and Max;

         Excess := (Word_Value or Max) /= Word_T'Last;

      end if;

      if Excess then
         -- The abs Value is too large to be a signed
         -- interpretation of an unsigned Word_Size value.

         Output.Warning (
              "Cannot interpret constant as unsigned with"
            & Width_T'Image (Width)
            & " bits"
            & Output.Field_Separator
            & Image (Value));

         Word_Value := 0;

      end if;

      return Word;

   exception

   when Constraint_Error =>
      -- Probably the conversion to Word_T failed.

      Output.Warning (
           "Constant out of range for unsigned"
         & Width_T'Image (Width)
         & "-bit word"
         & Output.Field_Separator
         & Image (Value));

      raise Word_Out_Of_Range;

   end Unsigned_Word;


   function Unsigned_Value (
      Value : Value_T;
      Width : Width_T)
   return Value_T
   is
   begin

      return Value_T (Unsigned_Word (Value, Width));

   exception

   when Constraint_Error =>

      Output.Warning (
           "Constant out of range for unsigned"
         & Width_T'Image (Width)
         & "-bit value"
         & Output.Field_Separator
         & Image (Value));

      raise Word_Out_Of_Range;

   end Unsigned_Value;


   function Sign_Bit (
      Word  : Word_T;
      Width : Width_T)
   return Bit_T
   is
   begin

      return Shift_Right (Word, Natural (Width - 1)) and 1;

   end Sign_Bit;


   function Negative (
      Word  : Word_T;
      Width : Width_T)
   return Boolean
   is
   begin

      return Sign_Bit (Word, Width) = 1;

   end Negative;


   function Sign (Word : Word_T; Width : Width_T) return Value_T
   is
   begin

      if Word = 0 then

         return 0;

      elsif Negative (Word, Width) then

         return -1;

      else

         return 1;

      end if;

   end Sign;


   function Max_Positive (Width : Width_T) return Word_T
   is
   begin

      if Width > 1 then

         return Max_Word (Width - 1);

      else

         return 0;

      end if;

   end Max_Positive;


   function Min_Negative (Width : Width_T) return Value_T
   is
   begin

      if Width > 1 then

         return - Value_T (Max_Word (Width - 1)) - 1;

      else

         return -1;

      end if;

   end Min_Negative;


   function Opposite_Sign (
      Word  : Word_T;
      Width : Width_T)
   return Word_T
   is
   begin

      return ((not Word) + 1) and Max_Word (Width);

   end Opposite_Sign;


   function Abs_Value (
      Word  : Word_T;
      Width : Width_T)
   return Word_T
   is
   begin

      if Negative (Word, Width) then

         return Opposite_Sign (Word, Width);

      else

         return Word and Max_Word (Width);

      end if;

   end Abs_Value;


   function Signed_Value (
      Word  : Word_T;
      Width : Width_T)
   return Value_T
   is

      Max : constant Word_T := Max_Word (Width);
      -- A word with all relevant bits 1.

      Masked : constant Word_T := Word and Max;
      -- The Word with the higher bits masked off.

   begin

      if Masked <= Max / 2 then
         -- The sign bit is off: a non-negative number.

         return Value_T (Masked);

      else
         -- The sign bit is on: a negative number.

         return - Value_T (Max - Masked) - 1;

      end if;

   exception

   when Constraint_Error =>
      -- Probably the conversion to Value_T failed.

      Output.Warning (
           "Unsigned"
         & Width_T'Image (Width)
         & "-bit constant too large for signed interpretation"
         & Output.Field_Separator
         & Word_T'Image (Word));

      raise Word_Out_Of_Range;

   end Signed_Value;


   function Mulu (Left, Right : Word_T; Width : Width_T)
   return Word_T
   is

      Max : constant Word_T := Max_Word (Width);

   begin

      return ((Left and Max) * (Right and Max)) and Max;

   end Mulu;


   function Muls (Left, Right : Word_T; Width : Width_T)
   return Word_T
   is

      Abs_L : constant Word_T := Abs_Value (Left , Width);
      Abs_R : constant Word_T := Abs_Value (Right, Width);
      -- The absolute values of Left and Right.

      Abs_Prod : Word_T;
      -- The product of the absolute values.

   begin

      Abs_Prod := Abs_L * Abs_R;

      if Negative (Left, Width) xor Negative (Right, Width) then
         -- Product is negative.

         return Opposite_Sign (Abs_Prod, Width);

      else

         return Abs_Prod;

      end if;

   end Muls;


   function Carry_From_Plus (Left, Right : Word_T; Width : Width_T)
   return Bit_T
   is
   begin

      if Left <= Max_Word (Width) - Right then
         -- Left + Right <= Max_Word (Width): No carry.

         return 0;

      else
         -- Left + Right > Max_Word (Width): Carry.

         return 1;

      end if;

   end Carry_From_Plus;


   function Negative_From_Plus (Left, Right : Word_T; Width : Width_T)
   return Bit_T
   is
   begin

      return Sign_Bit (Left + Right, Width);

   end Negative_From_Plus;


   function Overflow_From_Plus (Left, Right : Word_T; Width : Width_T)
   return Bit_T
   is

      LS : constant Bit_T := Sign_Bit (Left, Width);

   begin

      if      LS /= Sign_Bit (Right, Width)
      or else LS  = Sign_Bit (Left + Right, Width)
      then
         -- Either the addends have different sign, which means that
         -- the sum cannot overflow, or the sum has the same sign as
         -- both addends, which means that it did not overflow.

         return 0;

      else
         -- The addends have the same sign but the sum has the other
         -- sign, which indicates overflow.

         return 1;

      end if;

   end Overflow_From_Plus;


   function Borrow_From_Minus (Left, Right : Word_T; Width : Width_T)
   return Bit_T
   is
   begin

      return Boolean_Value(Left < Right);

   end Borrow_From_Minus;


   function Carry_From_Minus (Left, Right : Word_T; Width : Width_T)
   return Bit_T
   is
   begin

      return Boolean_Value(Left >= Right);

   end Carry_From_Minus;


   function Negative_From_Minus (Left, Right : Word_T; Width : Width_T)
   return Bit_T
   is
   begin

      return Sign_Bit (Left - Right, Width);

   end Negative_From_Minus;


   function Overflow_From_Minus (Left, Right : Word_T; Width : Width_T)
   return Bit_T
   is

      Minus_R : constant Word_T := Opposite_Sign (Right, Width);

   begin

      if Right /= Minus_R or Right = 0 then
         -- Left - Right = Left + Minus_R.

         return Overflow_From_Plus (Left, Minus_R, Width);

      else
         -- Right is the least negative number -2**(Width-1).
         -- The number -Right is too large for Width; -Right-1 is
         -- the largest (non-negative) representable number.

         if Negative (Left, Width) then
            -- Left - Right is in range.

            return 0;

         else
            -- Left is zero or positive, making Left - Right >= -Right
            -- too large for Width.

            return 1;

         end if;

      end if;

   end Overflow_From_Minus;


   function Carry_From_Plus (
      Left, Right : Word_T;
      Carry       : Bit_T;
      Width       : Width_T)
   return Bit_T
   is

      Max : constant Word_T := Max_Word (Width);

   begin

      if Left < Max or Carry = 0 then

         return Carry_From_Plus (Left + Carry, Right, Width);

      elsif Right < Max then

         return Carry_From_Plus (Left, Right + Carry, Width);

      else
         -- Left = Right = Max: There will be a carry.

         return 1;

      end if;

   end Carry_From_Plus;


   function Negative_From_Plus (
      Left, Right : Word_T;
      Carry       : Bit_T;
      Width       : Width_T)
   return Bit_T
   is
   begin

      return Sign_Bit (Left + Right + Carry, Width);

   end Negative_From_Plus;


   function Overflow_From_Plus (
      Left, Right : Word_T;
      Carry       : Bit_T;
      Width       : Width_T)
   return Bit_T
   is

      Oflo : Bit_T := Overflow_From_Plus (Left, Right, Width);

   begin

      if Carry = 1 then
         -- We do not yet have the answer in Oflo.

         if ((Left + Right) and Max_Word (Width)) = Max_Positive (Width) then

            Oflo := 1 - Oflo;

         end if;

         -- Rationale for the above:
         -- case Oflo is
         -- when 0 =>
               -- If Left+Right does not overflow, when
               -- can adding 1 (Carry) cause overflow?
         --    if ((Left + Right) and Max) = Max_Positive then
               -- Adding one more makes it negative, causing overflow.
         --       Oflo := 1;
         --    end if;
         -- when 1 =>
               -- If Left+Right does overflow, when can
               -- adding 1 (Carry) avoid the overflow?
         --    if ((Left + Right) and Max) = Max_Positive then
               -- This is also "minimum negative" - 1, so
               -- adding one keeps the total negative as
               -- it should be, avoiding overflow.
         --       Oflo := 0;
         --    end if;
         -- end case;

      end if;

      return Oflo;

   end Overflow_From_Plus;


   function Borrow_From_Minus (
      Left, Right : Word_T;
      Borrow      : Bit_T;
      Width       : Width_T)
   return Bit_T
   is
   begin

      if Left >= Borrow then

         return Borrow_From_Minus (Left - Borrow, Right, Width);

      elsif Right < Max_Word (Width) then

         return Borrow_From_Minus (Left, Right + Borrow, Width);

      else
         -- Left = 0, Right = Max_Word, and Borrow = 1.
         -- This is 0 - Max_Word - 1, which always gives a borrow bit.

         return 1;

      end if;

   end Borrow_From_Minus;


   function Carry_From_Minus (
      Left, Right : Word_T;
      Carry       : Bit_T;
      Width       : Width_T)
   return Bit_T
   is
   begin

      return 1 - Borrow_From_Minus (Left, Right, 1 - Carry, Width);

   end Carry_From_Minus;


   function Negative_From_Minus_Borrow (
      Left, Right : Word_T;
      Borrow      : Bit_T;
      Width       : Width_T)
   return Bit_T
   is
   begin

      return Sign_Bit (Left - Right - Borrow, Width);

   end Negative_From_Minus_Borrow;


   function Negative_From_Minus_Carry (
      Left, Right : Word_T;
      Carry       : Bit_T;
      Width       : Width_T)
   return Bit_T
   is
   begin

      return Sign_Bit (Left - Right + Carry - 1, Width);

   end Negative_From_Minus_Carry;


   function Overflow_From_Minus_Borrow (
      Left, Right : Word_T;
      Borrow      : Bit_T;
      Width       : Width_T)
   return Bit_T
   is

      Oflo : Bit_T := Overflow_From_Minus (Left, Right, Width);

   begin

      if Borrow = 1 then
         -- We do not yet have the answer in Oflo.

         if ((Left - Right) and Max_Word (Width)) = (Max_Positive (Width) + 1)
         then

            Oflo := 1 - Oflo;

         end if;

         -- Rationale for the above, where Min_Negative = Max_Positive + 1:
         -- case Oflo is
         -- when 0 =>
               -- If Left-Right does not overflow, when
               -- can subtracting 1 (Borrow) cause overflow?
         --    if ((Left - Right) and Max) = Min_Negative then
               -- Subtracting one more makes it positive, causing overflow.
         --       Oflo := 1;
         --    end if;
         -- when 1 =>
               -- If Left-Right does overflow, when can
               -- subtracting 1 (Borrow) avoid the overflow?
         --    if ((Left - Right) and Max) = Min_Negative then
               -- This is also Max_Positive + 1, so
               -- subtracting one keeps the total negative as
               -- it should be, and avoids overflow.
         --       Oflo := 0;
         --    end if;
         -- end case;

      end if;

      return Oflo;

   end Overflow_From_Minus_Borrow;


   function Overflow_From_Minus_Carry (
      Left, Right : Word_T;
      Carry       : Bit_T;
      Width       : Width_T)
   return Bit_T
   is
   begin

      return Overflow_From_Minus_Borrow (
         Left   => Left,
         Right  => Right,
         Borrow => 1 - Carry,
         Width  => Width);

   end Overflow_From_Minus_Carry;


   function Lts (Left, Right : Word_T; Width : Width_T)
   return Boolean
   is
   begin

      return Signed_Value (Left, Width) < Signed_Value (Right, Width);

   end Lts;


   function Gts (Left, Right : Word_T; Width : Width_T)
   return Boolean
   is
   begin

      return Signed_Value (Left, Width) > Signed_Value (Right, Width);

   end Gts;


   function Les (Left, Right : Word_T; Width : Width_T)
   return Boolean
   is
   begin

      return Signed_Value (Left, Width) <= Signed_Value (Right, Width);

   end Les;


   function Ges (Left, Right : Word_T; Width : Width_T)
   return Boolean
   is
   begin

      return Signed_Value (Left, Width) >= Signed_Value (Right, Width);

   end Ges;


   function Shift_Left (
      Value  : Word_T;
      Amount : Natural;
      Width  : Width_T)
   return Word_T
   is
   begin

      return Shift_Left (Value, Amount) and Max_Word (Width);

   end Shift_Left;


   function Shift_Right (
      Value  : Word_T;
      Amount : Natural;
      Width  : Width_T)
   return Word_T
   is
   begin

      return Shift_Right (Value and Max_Word (Width), Amount);

   end Shift_Right;


   function Shift_Right_Arithmetic (
      Value  : Word_T;
      Amount : Natural;
      Width  : Width_T)
   return Word_T
   is

      Max : constant Word_T := Max_Word (Width);

   begin

      if Negative (Value, Width) then

         return Shift_Right_Arithmetic (Value or (not Max), Amount) and Max;

      else

         return Shift_Right (Value and Max, Amount);

      end if;

   end Shift_Right_Arithmetic;


   function Rotate_Left (
      Value  : Word_T;
      Amount : Natural;
      Width  : Width_T)
   return Word_T
   is

      Max : constant Word_T := Max_Word (Width);

   begin

      return (Shift_Left  (Value        , Amount)
          or  Shift_Right (Value and Max, Natural (Width) - Amount))
          and Max;

   end Rotate_Left;


   function Rotate_Right (
      Value  : Word_T;
      Amount : Natural;
      Width  : Width_T)
   return Word_T
   is

      Max : constant Word_T := Max_Word (Width);

   begin

      return (Shift_Right (Value and Max, Amount)
          or  Shift_Left  (Value        , Natural (Width) - Amount))
          and Max;

   end Rotate_Right;


   function Complement (
      Value : Word_T;
      Width : Width_T)
   return Word_T
   is
   begin

      return (not Value) and Max_Word (Width);

   end Complement;


   function Sign_Extend (
      Value : Word_T;
      From  : Width_T;
      To    : Width_T)
   return Word_T
   is
   begin

      if Negative (Value, From) then

         return (Value or (not Max_Word (From))) and Max_Word (To);

      else

         return Value and Max_Word (To);

      end if;

   end Sign_Extend;


   function Zero_Extend (
      Value : Word_T;
      From  : Width_T)
   return Word_T
   is
   begin

      return Value and Max_Word (From);

   end Zero_Extend;


   function Truncate_Low (
      Value : Word_T;
      To    : Width_T)
   return Word_T
   is
   begin

      return Value and Max_Word (To);

   end Truncate_Low;


   function Truncate_High (
      Value : Word_T;
      From  : Width_T;
      To    : Width_T)
   return Word_T
   is
   begin

      if To <= From then

         return Shift_Right (
            Value  => Value,
            Amount => Natural (From - To),
            Width  => From);

      else

         Output.Fault (
            Location => "Arithmetic.Truncate_High",
            Text     =>
                 "From"
               & Width_T'Image (From)
               & " bits to"
               & Width_T'Image (To)
               & " bits.");

         return Value;

      end if;

   end Truncate_High;


   --
   ---   Expressions
   --


   function Is_Variable (Item : Expr_Ref) return Boolean
   is
   begin

      return Item /= Unknown and then Item.Kind in Variable_Kind_T;

   end Is_Variable;


   function Is_Cell (Cell : Cell_T; Expr : Expr_Ref)
   return Boolean
   is
   begin

      return Expr.Kind = Arithmetic.Cell
      and then Expr.Cell = Cell;

   end Is_Cell;


   function Is_Constant (Item : Expr_Ref) return Boolean
   is
   begin

      return Item.Kind = Const;

   end Is_Constant;


   function Is_Negative_Constant (Item : Expr_Ref) return Boolean
   is
   begin

      return Item.Kind = Const
      and then Negative (Item.Value, Item.Width);

   end Is_Negative_Constant;


   function Unsigned_Value (Item : Expr_Ref) return Word_T
   is
   begin

      if Item.Kind /= Const then

         raise Expression_Not_Constant;

      end if;

      return Item.Value;

   end Unsigned_Value;


   function Signed_Value (Item : Expr_Ref) return Value_T
   is
   begin

      if Item.Kind /= Const then

         raise Expression_Not_Constant;

      end if;

      return Signed_Value (Item.Value, Item.Width);

   end Signed_Value;


   function Const_Value (Item : Expr_Ref) return Value_T
   is
   begin

      if Item.Kind /= Const then

         raise Expression_Not_Constant;

      end if;

      if Item.Signed then

         return Signed_Value (Item.Value, Item.Width);

      else

         return Value_T (Item.Value);

      end if;

   end Const_Value;


   function Width_Of (Item : Expr_Ref) return Width_T
   is
   begin

      return Item.Width;

   end Width_Of;


   function To_Variables (Cells : Cell_List_T)
   return Variable_List_T
   is

      Vars : Variable_List_T (Cells'Range);

   begin

      for V in Vars'Range loop

         Vars(V) := Cell (Cells(V));

      end loop;

      return Vars;

   end To_Variables;


   --
   ---   Constructing expressions
   --


   function Cell (Item : Cell_T) return Expr_Ref
   is
   begin

      if Item = Storage.No_Cell then

         return Unknown;

      else

         return new Expr_T'(
            Kind  => Cell,
            Width => Storage.Width_Of (Item),
            Cell  => Item);

      end if;

   end Cell;


   function Cell (Spec : Processor.Cell_Spec_T) return Expr_Ref
   is
   begin

      return Cell (Storage.Cell (Spec));

   end Cell;


   function Const (
      Value  : Word_T;
      Width  : Width_T;
      Signed : Boolean)
   return Expr_Ref
   is

      Max : constant Word_T := Max_Word (Width);

   begin

     if  Value > Max then

         Output.Fault (
            Location => "Arithmetic.Const",
            Text     =>
                 "Constant "
               & Image (Value)
               & " exceeds"
               & Width_T'Image (Width)
               & "-bit range.");

      end if;

      return new Expr_T' (
         Kind   => Const,
         Width  => Width,
         Value  => Value and Max,
         Signed => Signed);

   end Const;


   function Const (
      Value  : Value_T;
      Width  : Width_T;
      Signed : Boolean)
   return Expr_Ref
   is

      Max : constant Word_T := Max_Word (Width);

   begin

      if      Value >   Value_T (Max)
      or else Value < - Value_T (Max / 2 + 1)
      then

         Output.Fault (
            Location => "Arithmetic.Const",
            Text     =>
                 "Constant "
               & Image (Value)
               & " exceeds"
               & Width_T'Image (Width)
               & "-bit range.");

      end if;

      return new Expr_T' (
         Kind   => Const,
         Width  => Width,
         Value  => Unsigned_Word (Value, Width),
         Signed => Signed);

   end Const;


   Zeros      : array (Width_T) of Expr_Ref := (others => No_Expr);
   Ones       : array (Width_T) of Expr_Ref := (others => No_Expr);
   Minus_Ones : array (Width_T) of Expr_Ref := (others => No_Expr);
   --
   -- Constant expressions of value zero, one, minus one, for
   -- various widths. Avoids duplicates of these constants.
   -- Default initiali


   function Zero (Width : Width_T) return Expr_Ref
   is
   begin

      if Zeros(Width) = No_Expr then

         Zeros(Width) := Const (Word_T'(0), Width, False);

      end if;

      return Zeros(Width);

   end Zero;


   function One (Width : Width_T) return Expr_Ref
   is
   begin

      if Ones(Width) = No_Expr then

         Ones(Width) := Const (Word_T'(1), Width, False);

      end if;

      return Ones(Width);

   end One;


   function Minus_One (Width : Width_T) return Expr_Ref
   is
   begin

      if Minus_Ones(Width) = No_Expr then

         Minus_Ones(Width) := Const (Max_Word (Width), Width, True);

      end if;

      return Minus_Ones(Width);

   end Minus_One;


   function Reference (To : Storage.References.Boundable_Ref)
   return Expr_Ref
   is
   begin

      return new Expr_T'(
         Kind  => Ref,
         Width => To.Width,
         Ref   => To);

   end Reference;


   -- Arithmetic additive operators:


   procedure Report_Width_Mismatch (Where : String; L, R : Expr_Ref)
   --
   -- Reports the fault that the two operands have different width.
   --
   is
   begin

      Output.Trace (
           "Arithmetic." & Where
         & Output.Field_Separator
         & "Left operand"
         & Width_T'Image (L.Width)
         & " bits, right operand"
         & Width_T'Image (R.Width)
         & " bits.");

      Output.Fault (
         Location => "Arithmetic." & Where,
         Text     =>
              "Left operand ("
            & Image (L)
            & ") is"
            & Width_T'Image (L.Width)
            & " bits, right operand ("
            & Image (R)
            & ") is"
            & Width_T'Image (R.Width)
            & " bits.");

   end Report_Width_Mismatch;


   procedure Report_Width_Mismatch (Where : String; L, R, C : Expr_Ref)
   --
   -- Reports the fault that the three operands have mismatched widths.
   --
   is
   begin

      Output.Fault (
         Location => "Arithmetic." & Where,
         Text     =>
              "Left operand ("
            & Image (L)
            & ") is"
            & Width_T'Image (L.Width)
            & " bits, right operand ("
            & Image (R)
            & ") is"
            & Width_T'Image (R.Width)
            & " bits, third operand ("
            & Image (C)
            & ") is"
            & Width_T'Image (C.Width)
            & " bits.");

   end Report_Width_Mismatch;


   procedure Report_Width_Not_One (Where : String; Expr : Expr_Ref)
   --
   -- Reports the fault that the given Expr is not one-bit wide.
   --
   is
   begin

      Output.Fault (
         Location => "Arithmetic." & Where,
         Text     =>
              "Expression is "
            & Width_T'Image (Expr.Width)
            & " bits.");

   end Report_Width_Not_One;


   function Is_Small_Negative (
      Word  : Word_T;
      Width : Width_T)
   return Boolean
   --
   -- Whether the Word represents a small negative number, in the
   -- spirit of Opt.Swap_Small_Negatives.
   --
   renames Negative;


   function "+" (L, R : Expr_Ref) return Expr_Ref
   is
   begin

      if L = Unknown or R = Unknown then

         return Unknown;

      elsif L.Width /= R.Width then

         Report_Width_Mismatch ("""+""", L, R);

         return Unknown;

      elsif L.Kind = Const and R.Kind = Const then
         -- Simplify sum of constants to a single constant.

         return Const (
            Value  => (L.Value + R.Value) and Max_Word (L.Width),
            Width  => L.Width,
            Signed => L.Signed or R.Signed);

      elsif (R.Kind = Const and Opt.Swap_Small_Negatives)
      and then Is_Small_Negative (R.Value, R.Width)
      then
         -- Simplify to L + (-N) to L - N, where N is a constant.

         return new Expr_T'(
            Kind   => Binary_Kind,
            Width  => L.Width,
            Binary => Minus,
            L_Expr => L,
            R_Expr => Const (
               Value  => Opposite_Sign (R.Value, R.Width),
               Width  => R.Width,
               Signed => R.Signed));

      elsif (L.Kind = Const and Opt.Swap_Small_Negatives)
      and then Is_Small_Negative (L.Value, L.Width)
      then
         -- Simplify to (-N) + R to R - N, where N is a constant.

         return new Expr_T'(
            Kind   => Binary_Kind,
            Width  => L.Width,
            Binary => Minus,
            L_Expr => R,
            R_Expr => Const (
               Value  => Opposite_Sign (L.Value, L.Width),
               Width  => L.Width,
               Signed => L.Signed));

      elsif L = Word_T'(0) then

         return R;

      elsif R = Word_T'(0) then

         return L;

      else

         return new Expr_T'(
            Kind   => Binary_Kind,
            Width  => L.Width,
            Binary => Plus,
            L_Expr => L,
            R_Expr => R);

      end if;

   end "+";


   function "-" (L, R : Expr_Ref) return Expr_Ref
   is
   begin

      if L = Unknown or R = Unknown then

         return Unknown;

      elsif L.Width /= R.Width then

         Report_Width_Mismatch ("""-""", L, R);

         return Unknown;

      elsif L.Kind = Const and R.Kind = Const then
         -- Simplify difference of constants to a single constant.

         return Const (
            Value  => (L.Value - R.Value) and Max_Word (L.Width),
            Width  => L.Width,
            Signed => L.Signed or R.Signed);

      elsif (R.Kind = Const and Opt.Swap_Small_Negatives)
      and then Is_Small_Negative (R.Value, R.Width)
      then
         -- Simplify to L - (-N) to L + N, where N is a constant.

         return new Expr_T'(
            Kind   => Binary_Kind,
            Width  => L.Width,
            Binary => Plus,
            L_Expr => L,
            R_Expr => Const (
               Value  => Opposite_Sign (R.Value, R.Width),
               Width  => R.Width,
               Signed => R.Signed));

      elsif R = Word_T'(0) then

         return L;

      else

         return new Expr_T'(
            Kind   => Binary_Kind,
            Width  => L.Width,
            Binary => Minus,
            L_Expr => L,
            R_Expr => R);

      end if;

   end "-";


   function "-" (E : Expr_Ref) return Expr_Ref
   is
   begin

      return Zero (E.Width) - E;

   end "-";


   function Plus_With_Carry  (Left, Right, Carry : Expr_Ref)
   return Expr_Ref
   is
   begin

      if Left = Unknown or Right = Unknown or Carry = Unknown then

         return Unknown;

      elsif Left.Width /= Right.Width or Carry.Width /= 1 then

         Report_Width_Mismatch ("""+""", Left, Right, Carry);

         return Unknown;

      elsif Carry = Word_T'(0) then

         return Left + Right;

      else

         return new Expr_T'(
            Kind    => Ternary_Kind,
            Width   => Left.Width,
            Ternary => Plus,
            L3_Expr => Left,
            R3_Expr => Right,
            C3_Expr => Carry);

      end if;

   end Plus_With_Carry;


   function Minus_With_Borrow (Left, Right, Borrow : Expr_Ref)
   return Expr_Ref
   is
   begin

      if Left = Unknown or Right = Unknown or Borrow = Unknown then

         return Unknown;

      elsif Left.Width /= Right.Width or Borrow.Width /= 1 then

         Report_Width_Mismatch ("""-""", Left, Right, Borrow);

         return Unknown;

      elsif Borrow = Word_T'(0) then

         return Left - Right;

      else

         return new Expr_T'(
            Kind    => Ternary_Kind,
            Width   => Left.Width,
            Ternary => Minus_With_B,
            L3_Expr => Left,
            R3_Expr => Right,
            C3_Expr => Borrow);

      end if;

   end Minus_With_Borrow;


   function Minus_With_Carry (Left, Right, Carry : Expr_Ref)
   return Expr_Ref
   is
   begin

      if Left = Unknown or Right = Unknown or Carry = Unknown then

         return Unknown;

      elsif Left.Width /= Right.Width or Carry.Width /= 1 then

         Report_Width_Mismatch ("""-""", Left, Right, Carry);

         return Unknown;

      elsif Carry = Word_T'(1) then

         return Left - Right;

      else

         return new Expr_T'(
            Kind    => Ternary_Kind,
            Width   => Left.Width,
            Ternary => Minus_With_C,
            L3_Expr => Left,
            R3_Expr => Right,
            C3_Expr => Carry);

      end if;

   end Minus_With_Carry;


   -- Arithmetic multiplicative operators:


   function Mulu (L, R : Expr_Ref) return Expr_Ref
   is
   begin

      if L = Unknown or R = Unknown then

         return Unknown;

      elsif L.Width /= R.Width then

         Report_Width_Mismatch ("mulu", L, R);

         return Unknown;

      else

         return new Expr_T'(
            Kind   => Binary_Kind,
            Width  => L.Width,    -- = R.Width
            Binary => Muls,
            L_Expr => L,
            R_Expr => R);

      end if;

   end Mulu;


   function Muls (L, R : Expr_Ref) return Expr_Ref
   is
   begin

      if L = Unknown or R = Unknown then

         return Unknown;

      elsif L.Width /= R.Width then

         Report_Width_Mismatch ("muls", L, R);

         return Unknown;

      else

         return new Expr_T'(
            Kind   => Binary_Kind,
            Width  => L.Width,    -- = R.Width
            Binary => Muls,
            L_Expr => L,
            R_Expr => R);

      end if;

   end Muls;


   procedure Accumulate (
      Factor : in     Value_T;
      Expr   : in     Expr_Ref;
      Sum    : in out Expr_Ref)
   is

      Term : Expr_Ref;
      -- The thing to be added to Sum or subtracted from Sum.

      Pos : Boolean := Factor > 0;
      -- Whether the Term should be added to Sum, otherwise it
      -- should be subtracted (when Factor /= 0).

   begin

      if Factor = 0 or Sum = Unknown then
         -- Nothing to do.

         null;

      elsif Expr = Unknown then
         -- We are lost in space...

         Sum := Unknown;

      else
         -- There is something interesting to add to Sum.

         if Expr.Width /= Sum.Width then

            Output.Fault (
               Location => "Arithmetic.Accumulate",
               Text     =>
                    "Adding "
                  & Image (Factor)
                  & " * "
                  & Image (Expr)
                  & ','
                  & Width_T'Image (Expr.Width)
                  & " bits, to "
                  & Image (Sum)
                  & ','
                  & Width_T'Image (Sum.Width)
                  & " bits");

         end if;

         if abs Factor = 1 then
            -- No need to multiply.

            Term := Expr;

         else

            Term := Muls (Const (abs Factor, Expr.Width, True),
                          Expr);

         end if;

         if Sum = Value_T'(0) then
            -- We can omit the initial value of Sum.

            if Pos then Sum :=   Term;
                   else Sum := - Term;
            end if;

         else
            -- Must include initial Sum.

            if Pos then Sum := Sum + Term;
                   else Sum := Sum - Term;
            end if;

         end if;

      end if;

   end Accumulate;


   -- Predicates from integer operators:


   function Predicate (
      Pred : Binary_Predicate_Op_T;
      L, R : Expr_Ref)
   return Expr_Ref
   --
   -- Creates a Predicate expression of two operands.
   --
   is
   begin

      if L = Unknown or R = Unknown then

         return Unknown;

      elsif L.Width /= R.Width then

         Report_Width_Mismatch (Binary_Predicate_Op_T'Image (Pred), L, R);

         return Unknown;

      else

         return new Expr_T'(
            Kind   => Binary_Kind,
            Width  => 1,
            Binary => Pred,
            L_Expr => L,
            R_Expr => R);

      end if;

   end Predicate;


   function Predicate (
      Pred    : Ternary_Predicate_Op_T;
      L, R, C : Expr_Ref)
   return Expr_Ref
   --
   -- Creates a Predicate expression of three operands.
   --
   is
   begin

      if L = Unknown or R = Unknown or C = Unknown then

         return Unknown;

      elsif L.Width /= R.Width or C.Width /= 1 then

         Report_Width_Mismatch (Ternary_Predicate_Op_T'Image (Pred), L, R, C);

         return Unknown;

      else

         return new Expr_T'(
            Kind    => Ternary_Kind,
            Width   => 1,
            Ternary => Pred,
            L3_Expr => L,
            R3_Expr => R,
            C3_Expr => C);

      end if;

   end Predicate;


   function Carry_From_Plus (Left, Right : Expr_Ref)
   return Expr_Ref
   is
   begin

      return Predicate (Plus_C, Left, Right);

   end Carry_From_Plus;


   function Negative_From_Plus (Left, Right : Expr_Ref)
   return Expr_Ref
   is
   begin

      return Predicate (Plus_N, Left, Right);

   end Negative_From_Plus;


   function Overflow_From_Plus (Left, Right : Expr_Ref)
   return Expr_Ref
   is
   begin

      return Predicate (Plus_V, Left, Right);

   end Overflow_From_Plus;


   function Borrow_From_Minus (Left, Right : Expr_Ref) return Expr_Ref
   is
   begin

      return Predicate (Minus_B, Left, Right);

   end Borrow_From_Minus;


   function Carry_From_Minus (Left, Right : Expr_Ref) return Expr_Ref
   is
   begin

      return Predicate (Minus_C, Left, Right);

   end Carry_From_Minus;


   function Negative_From_Minus (Left, Right : Expr_Ref)
   return Expr_Ref
   is
   begin

      return Predicate (Minus_N, Left, Right);

   end Negative_From_Minus;


   function Overflow_From_Minus (Left, Right : Expr_Ref)
   return Expr_Ref
   is
   begin

      return Predicate (Minus_V, Left, Right);

   end Overflow_From_Minus;


   function Carry_From_Plus (Left, Right, Carry : Expr_Ref)
   return Expr_Ref
   is
   begin

      return Predicate (Plus_C, Left, Right, Carry);

   end Carry_From_Plus;


   function Negative_From_Plus (Left, Right, Carry : Expr_Ref)
   return Expr_Ref
   is
   begin

      return Predicate (Plus_N, Left, Right, Carry);

   end Negative_From_Plus;


   function Overflow_From_Plus (Left, Right, Carry : Expr_Ref)
   return Expr_Ref
   is
   begin

      return Predicate (Plus_V, Left, Right, Carry);

   end Overflow_From_Plus;


   function Borrow_From_Minus (Left, Right, Borrow : Expr_Ref)
   return Expr_Ref
   is
   begin

      return Predicate (Minus_B, Left, Right, Borrow);

   end Borrow_From_Minus;


   function Carry_From_Minus (Left, Right, Carry : Expr_Ref)
   return Expr_Ref
   is
   begin

      return Predicate (Minus_C, Left, Right, Carry);

   end Carry_From_Minus;


   function Negative_From_Minus_Borrow (Left, Right, Borrow : Expr_Ref)
   return Expr_Ref
   is
   begin

      return Predicate (Minus_BN, Left, Right, Borrow);

   end Negative_From_Minus_Borrow;


   function Negative_From_Minus_Carry (Left, Right, Carry : Expr_Ref)
   return Expr_Ref
   is
   begin

      return Predicate (Minus_CN, Left, Right, Carry);

   end Negative_From_Minus_Carry;


   function Overflow_From_Minus_Borrow (Left, Right, Borrow : Expr_Ref)
   return Expr_Ref
   is
   begin

      return Predicate (Minus_BV, Left, Right, Borrow);

   end Overflow_From_Minus_Borrow;


   function Overflow_From_Minus_Carry (Left, Right, Carry : Expr_Ref)
   return Expr_Ref
   is
   begin

      return Predicate (Minus_CV, Left, Right, Carry);

   end Overflow_From_Minus_Carry;


   -- Equality operator:


   function "="  (L, R : Expr_Ref) return Expr_Ref
   is
   begin

      if L = Unknown or R = Unknown then

         return Unknown;

      elsif L.Width /= R.Width then

         Report_Width_Mismatch ("""=""", L, R);

         return Unknown;

      else

         return new Expr_T'(
            Kind   => Binary_Kind,
            Width  => 1,
            Binary => Eq,
            L_Expr => L,
            R_Expr => R);

      end if;

   end "=";


   function "=" (L : Expr_Ref; R : Word_T) return Expr_Ref
   is
   begin

      return L = Const (
         Value  => R,
         Width  => L.Width,
         Signed => False);

   end "=";


   function "=" (L : Expr_Ref; R : Value_T) return Expr_Ref
   is
   begin

      return L = Const (
          Value  => Unsigned_Word (R, L.Width),
          Width  => L.Width,
          Signed => True);

   end "=";


   -- Relational operators for signed and unsigned views:


   function Lts (L, R : Expr_Ref) return Expr_Ref
   is
   begin

      return Predicate (Lts, L, R);

   end Lts;


   function Gts (L, R : Expr_Ref) return Expr_Ref
   is
   begin

      return Predicate (Gts, L, R);

   end Gts;


   function Les (L, R : Expr_Ref) return Expr_Ref
   is
   begin

      return Predicate (Les, L, R);

   end Les;


   function Ges (L, R : Expr_Ref) return Expr_Ref
   is
   begin

      return Predicate (Ges, L, R);

   end Ges;


   function Ltu (L, R : Expr_Ref) return Expr_Ref
   is
   begin

      return Predicate (Ltu, L, R);

   end Ltu;


   function Gtu (L, R : Expr_Ref) return Expr_Ref
   is
   begin

      return Predicate (Gtu, L, R);

   end Gtu;


   function Leu (L, R : Expr_Ref) return Expr_Ref
   is
   begin

      return Predicate (Leu, L, R);

   end Leu;


   function Geu  (L, R : Expr_Ref) return Expr_Ref
   is
   begin

      return Predicate (Geu, L, R);

   end Geu;


   -- Boolean operators:


   function "and" (L, R : Expr_Ref) return Expr_Ref
   is
   begin

      if L = Unknown or R = Unknown then

         return Unknown;

      elsif L.Width /= 1 or R.Width /= 1 then

         Output.Fault (
            Location => "Arithmetic.""and""",
            Text     =>
                 "Left operand"
               & Width_T'Image (L.Width)
               & " bits, right"
               & Width_T'Image (R.Width)
               & " bits.");

         return Unknown;

      elsif L = Always then

         return R;

      elsif R = Always then

         return L;

      elsif L = Never or R = Never then

         return Never;

      else

         return new Expr_T'(
            Kind   => Binary_Kind,
            Width  => 1,
            Binary => Andx,
            L_Expr => L,
            R_Expr => R);

      end if;

   end "and";


   function "or" (L, R : Expr_Ref) return Expr_Ref
   is
   begin

      if L = Unknown or R = Unknown then

         return Unknown;

      elsif L.Width /= 1 or R.Width /= 1 then

         Output.Fault (
            Location => "Arithmetic.""or""",
            Text     =>
                 "Left operand"
               & Width_T'Image (L.Width)
               & " bits, right"
               & Width_T'Image (R.Width)
               & " bits.");

         return Unknown;

      elsif L = Never then

         return R;

      elsif R = Never then

         return L;

      elsif L = Always or R = Always then

         return Always;

      else

         return new Expr_T'(
            Kind   => Binary_Kind,
            Width  => 1,
            Binary => Orx,
            L_Expr => L,
            R_Expr => R);

      end if;

   end "or";


   Eq_Opposite_Bit : constant array (Unary_Eq_Bit_T) of Unary_Eq_Bit_T := (
      EqZero => EqOne,
      EqOne  => EqZero);
   --
   -- Negating the comparison of a 1-bit operand to a 1-bit constant
   -- by comparing the operand to the negated 1-bit constant.


   function "not" (E : Expr_Ref) return Expr_Ref
   is
   begin

      if E = Unknown then

         return Unknown;

      elsif E.Width /= 1 then

         Output.Fault (
            Location => "Arithmetic.""not""",
            Text     =>
                 "Operand"
               & Width_T'Image (E.Width)
               & " bits.");

         return Unknown;

      elsif E = Always then

         return Never;

      elsif E = Never then

         return Always;

      elsif    E.Kind       =  Unary_Kind
      and then E.Unary      in Unary_Eq_Bit_T
      and then E.Expr.Width =  1
      then
         -- not (EqZero/EqOne) = EqOne/EqZero, when operand Width = 1.

         return new Expr_T'(
            Kind  => Unary_Kind,
            Width => 1,
            Unary => Eq_Opposite_Bit(E.Unary),
            Expr  => E.Expr);

      else

         return new Expr_T'(
            Kind  => Unary_Kind,
            Width => 1,
            Unary => Notx,
            Expr  => E);

      end if;

   end "not";


   -- Bit-wise operators:
   --
   -- These operators all have an implicit width scheme: they expect
   -- their operands to have the same width and create an expression
   -- with this width.
   --
   -- All the operators return the Unknown expression if either or
   -- both of the operands is Unknown.


   function Andw  (L, R : Expr_Ref) return Expr_Ref
   is
   begin

      if L = Unknown or R = Unknown then

         return Unknown;

      elsif L.Width /= R.Width then

         Report_Width_Mismatch ("andw", L, R);

         return Unknown;

      elsif L = Value_T'(0) or R = Value_T'(0) then

         return Zero (L.Width);

      elsif L = Max_Word (L.Width) then

         return R;

      elsif R = Max_Word (R.Width) then

         return L;

      else

         return new Expr_T'(
            Kind   => Binary_Kind,
            Width  => L.Width,    -- = R.Width
            Binary => Andw,
            L_Expr => L,
            R_Expr => R);

      end if;

   end Andw;


   function Orw   (L, R : Expr_Ref) return Expr_Ref
   is
   begin

      if L = Unknown or R = Unknown then

         return Unknown;

      elsif L.Width /= R.Width then

         Report_Width_Mismatch ("orw", L, R);

         return Unknown;

      elsif L = Max_Word (L.Width) or R = Max_Word (L.Width) then

         return Const (Value_T (Max_Word (L.Width)), L.Width, False);

      elsif L = Value_T'(0) then

         return R;

      elsif R = Value_T'(0) then

         return L;

      else

         return new Expr_T'(
            Kind   => Binary_Kind,
            Width  => L.Width,    -- = R.Width
            Binary => Orw,
            L_Expr => L,
            R_Expr => R);

      end if;

   end Orw;


   function Xorw  (L, R : Expr_Ref) return Expr_Ref
   is
   begin

      if L = Unknown or R = Unknown then

         return Unknown;

      elsif L.Width /= R.Width then

         Report_Width_Mismatch ("Xorw", L, R);

         return Unknown;

      elsif L = Value_T'(0) then

         return R;

      elsif R = Value_T'(0) then

         return L;

      elsif L = Max_Word (L.Width) then

         return Notw (R);

      elsif R = Max_Word (L.Width) then

         return Notw (L);

      else

         return new Expr_T'(
            Kind   => Binary_Kind,
            Width  => L.Width,    -- = R.Width
            Binary => Xorw,
            L_Expr => L,
            R_Expr => R);

      end if;

   end Xorw;


   function Notw  (E : Expr_Ref) return Expr_Ref
   is
   begin

      if E = Unknown then

         return Unknown;

      elsif Is_Constant (E) then

         return new Expr_T'(
            Kind   => Const,
            Width  => E.Width,
            Value  => Complement (E.Value, E.Width),
            Signed => E.Signed);

      else

         return new Expr_T'(
            Kind  => Unary_Kind,
            Width => E.Width,
            Unary => Notw,
            Expr  => E);

      end if;

   end Notw;


   -- Shift and rotate operators:


   function Slz   (L, R : Expr_Ref) return Expr_Ref
   is
   begin

      if L = Unknown or R = Unknown then

         return Unknown;

      elsif R = Value_T'(0) then

         return L;

      elsif Is_Constant (R) and then R.Value >= Word_T (L.Width) then

         return Zero (L.Width);

      elsif Is_Constant (L) and Is_Constant (R) then

         return new Expr_T'(
            Kind  => Const,
            Width => L.Width,
            Value => Shift_Left (
               Value  => L.Value,
               Amount => Natural (R.Value),
               Width  => L.Width),
            Signed => L.Signed);

      else

         return new Expr_T'(
            Kind   => Binary_Kind,
            Width  => L.Width,
            Binary => Slz,
            L_Expr => L,
            R_Expr => R);

      end if;

   end Slz;


   function Srz   (L, R : Expr_Ref) return Expr_Ref
   is
   begin

      if L = Unknown or R = Unknown then

         return Unknown;

      elsif R = Value_T'(0) then

         return L;

      elsif Is_Constant (R) and then R.Value >= Word_T (L.Width) then

         return Zero (L.Width);

      elsif Is_Constant (L) and Is_Constant (R) then

         return new Expr_T'(
            Kind  => Const,
            Width => L.Width,
            Value => Shift_Right (
               Value  => L.Value,
               Amount => Natural (R.Value),
               Width  => L.Width),
            Signed => L.Signed);

      else

         return new Expr_T'(
            Kind   => Binary_Kind,
            Width  => L.Width,
            Binary => Srz,
            L_Expr => L,
            R_Expr => R);

      end if;

   end Srz;


   function Sra   (L, R : Expr_Ref) return Expr_Ref
   is
   begin

      if L = Unknown or R = Unknown then

         return Unknown;

      elsif R = Value_T'(0) then

         return L;

      elsif Is_Constant (L) and Is_Constant (R) then

         return new Expr_T'(
            Kind  => Const,
            Width => L.Width,
            Value => Shift_Right_Arithmetic (
               Value  => L.Value,
               Amount => Natural (Word_T'Min (R.Value, Word_T (L.Width))),
               Width  => L.Width),
            Signed => L.Signed);

      else

         return new Expr_T'(
            Kind   => Binary_Kind,
            Width  => L.Width,
            Binary => Sra,
            L_Expr => L,
            R_Expr => R);

      end if;

   end Sra;


   function Rotl  (L, R : Expr_Ref) return Expr_Ref
   is
   begin

      if L = Unknown or R = Unknown then

         return Unknown;

      elsif R = Value_T'(0) then

         return L;

      elsif Is_Constant (L) and Is_Constant (R) then

         return new Expr_T'(
            Kind  => Const,
            Width => L.Width,
            Value => Rotate_Left (
               Value  => L.Value,
               Amount => Natural (R.Value mod Word_T (L.Width)),
               Width  => L.Width),
            Signed => L.Signed);

      else

         return new Expr_T'(
            Kind   => Binary_Kind,
            Width  => L.Width,
            Binary => Rotl,
            L_Expr => L,
            R_Expr => R);

      end if;

   end Rotl;


   function Rotr  (L, R : Expr_Ref) return Expr_Ref
   is
   begin

      if L = Unknown or R = Unknown then

         return Unknown;

      elsif R = Value_T'(0) then

         return L;

      elsif Is_Constant (L) and Is_Constant (R) then

         return new Expr_T'(
            Kind  => Const,
            Width => L.Width,
            Value => Rotate_Right (
               Value  => L.Value,
               Amount => Natural (R.Value mod Word_T (L.Width)),
               Width  => L.Width),
            Signed => L.Signed);

      else

         return new Expr_T'(
            Kind   => Binary_Kind,
            Width  => L.Width,
            Binary => Rotr,
            L_Expr => L,
            R_Expr => R);

      end if;

   end Rotr;


   -- Concatenation operator:


   function Conc (L, R : Expr_Ref) return Expr_Ref
   is
   begin

      if L = Unknown or R = Unknown then

         return Unknown;

      else

         return new Expr_T'(
            Kind   => Binary_Kind,
            Width  => L.Width + R.Width,
            Binary => Conc,
            L_Expr => L,
            R_Expr => R);

      end if;

   end Conc;


   function Can_Shift_By_Mul (N : Word_T) return Boolean
   is
   begin

      if N <= Word_T (Opt.Max_Shift_Mul_Bits) then

         return True;

      else

         if Opt.Warn_Large_Shift then

            Output.Warning (
                 "Left-shift by "
               & Image (N)
               & " bits not modelled by multiplication.");

         end if;

         return False;

      end if;

   end Can_Shift_By_Mul;


   function Can_Shift_By_Mul (Expr : Expr_Ref) return Boolean
   is
   begin

      return   Expr.Kind        = Binary_Kind
      and then Expr.Binary      = Slz
      and then Expr.R_Expr.Kind = Const
      and then Can_Shift_By_Mul (Expr.R_Expr.Value);

   end Can_Shift_By_Mul;


   -- Bit-extraction operators and unary predicates:


   Cond_Value : constant array (Boolean) of Expr_Ref := (
      False => Const (
         Value  => Boolean_Value(False),
         Width  => 1,
         Signed => False),
      True  => Const (
         Value  => Boolean_Value(True),
         Width  => 1,
         Signed => False));
   --
   -- The mapping of a Boolean to an arithmetic value.


   function EqZero(Expr : Expr_Ref) return Expr_Ref
   is
   begin

      if Expr = Unknown then

         return Unknown;

      elsif Is_Constant (Expr) then

         return Cond_Value(Expr.Value = 0);

      else

         return new Expr_T'(
            Kind  => Unary_Kind,
            Width => 1,
            Unary => EqZero,
            Expr  => Expr);

      end if;

   end EqZero;


   function EqOne (Expr : Expr_Ref) return Expr_Ref
   is
   begin

      if Expr = Unknown then

         return Unknown;

      elsif Is_Constant (Expr) then

         return Cond_Value(Expr.Value = 1);

      else

         return new Expr_T'(
            Kind  => Unary_Kind,
            Width => 1,
            Unary => EqOne,
            Expr  => Expr);

      end if;

   end EqOne;


   function Signw (Expr : Expr_Ref) return Expr_Ref
   is
   begin

      if Expr = Unknown then

         return Unknown;

      elsif Is_Constant (Expr) then

         return Const (
            Value => Sign_Bit (
               Word  => Expr.Value,
               Width => Expr.Width),
            Width  => 1,
            Signed => False);

      else

         return new Expr_T'(
            Kind  => Unary_Kind,
            Width => 1,
            Unary => Signw,
            Expr  => Expr);

      end if;

   end Signw;


   -- Extension operators:


   function Exts (Expr : Expr_Ref; Width : Width_T) return Expr_Ref
   is
   begin

      if Expr = Unknown then

         return Unknown;

      elsif Expr.Width = Width then

         return Expr;

      elsif Expr.Width > Width then

         Output.Fault (
            Location => "Arithmetic.Exts",
            Text     =>
                 "Extension narrows from"
               & Width_T'Image (Expr.Width)
               & " to"
               & Width_T'Image (Width));

         return Unknown;

      elsif Is_Constant (Expr) then

         return Const (
            Value => Sign_Extend (
               Value => Expr.Value,
               From  => Expr.Width,
               To    => Width),
            Width  => Width,
            Signed => True);

      else

         return new Expr_T'(
            Kind  => Unary_Kind,
            Width => Width,
            Unary => Exts,
            Expr  => Expr);

      end if;

   end Exts;


   function Extz (Expr : Expr_Ref; Width : Width_T) return Expr_Ref
   is
   begin

      if Expr = Unknown then

         return Unknown;

      elsif Expr.Width = Width then

         return Expr;

      elsif Expr.Width > Width then

         Output.Fault (
            Location => "Arithmetic.Extz",
            Text     =>
                 "Extension narrows from"
               & Width_T'Image (Expr.Width)
               & " to"
               & Width_T'Image (Width));

         return Unknown;

      elsif Is_Constant (Expr) then

         return Const (
            Value => Zero_Extend (
               Value => Expr.Value,
               From  => Expr.Width),
            Width  => Width,
            Signed => False);

      else

         return new Expr_T'(
            Kind  => Unary_Kind,
            Width => Width,
            Unary => Extz,
            Expr  => Expr);

      end if;

   end Extz;


   function Trunl (Expr : Expr_Ref; Width : Width_T) return Expr_Ref
   is
   begin

      if Expr = Unknown then

         return Unknown;

      elsif Expr.Width = Width then

         return Expr;

      elsif Expr.Width < Width then

         Output.Fault (
            Location => "Arithmetic.Trunl",
            Text     =>
                 "Truncation widens from"
               & Width_T'Image (Expr.Width)
               & " to"
               & Width_T'Image (Width));

         return Unknown;

      elsif Is_Constant (Expr) then

         return Const (
            Value  => Expr.Value and Max_Word (Width),
            Width  => Width,
            Signed => False);

      else

         return new Expr_T'(
            Kind  => Unary_Kind,
            Width => Width,
            Unary => Trunl,
            Expr  => Expr);

      end if;

   end Trunl;


   function Trunh (Expr : Expr_Ref; Width : Width_T) return Expr_Ref
   is
   begin

      if Expr = Unknown then

         return Unknown;

      elsif Expr.Width = Width then

         return Expr;

      elsif Expr.Width < Width then

         Output.Fault (
            Location => "Arithmetic.Trunh",
            Text     =>
                 "Truncation widens from"
               & Width_T'Image (Expr.Width)
               & " to"
               & Width_T'Image (Width));

         return Unknown;

      elsif Is_Constant (Expr) then

         return Const (
            Value => Shift_Right (
               Value  => Expr.Value,
               Amount => Natural (Expr.Width - Width),
               Width  => Expr.Width),
            Width  => Width,
            Signed => False);

      else

         return new Expr_T'(
            Kind  => Unary_Kind,
            Width => Width,
            Unary => Trunh,
            Expr  => Expr);

      end if;

   end Trunh;


   -- Generic unary and binary expressions


   function Unary (
      Operation : Unary_Op_T;
      Operand   : Expr_Ref;
      Width     : Width_T)
   return Expr_Ref
   is
   begin

      if Operand = Unknown then

         return Unknown;

      else

         return new Expr_T'(
            Kind  => Unary_Kind,
            Width => Width,
            Unary => Operation,
            Expr  => Operand);

      end if;

   end Unary;


   function Binary (
      Operation   : Binary_Op_T;
      Left, Right : Expr_Ref;
      Width       : Width_T)
   return Expr_Ref
   is
   begin

      if Left = Unknown or Right = Unknown then

         return Unknown;

      elsif Operation in Equal_Width_Op_T
      and then Left.Width /= Right.Width then

         Report_Width_Mismatch (
            Where => "Binary " & Binary_Op_T'Image (Operation),
            L     => Left,
            R     => Right);

         return Unknown;

      else

         return new Expr_T'(
            Kind   => Binary_Kind,
            Width  => Width,
            Binary => Operation,
            L_Expr => Left,
            R_Expr => Right);

      end if;

   end Binary;


   function Ternary (
      Operation          : Ternary_Op_T;
      Left, Right, Carry : Expr_Ref;
      Width              : Width_T)
   return Expr_Ref
   is
   begin

      if Left = Unknown or Right = Unknown or Carry = Unknown then

         return Unknown;

      elsif Left.Width /= Right.Width or Carry.Width /= 1 then

         Report_Width_Mismatch (
            Where => "Ternary " & Ternary_Op_T'Image (Operation),
            L     => Left,
            R     => Right,
            C     => Carry);

         return Unknown;

      else

         return new Expr_T'(
            Kind    => Ternary_Kind,
            Width   => Width,
            Ternary => Operation,
            L3_Expr => Left,
            R3_Expr => Right,
            C3_Expr => Carry);

      end if;

   end Ternary;


   function Length (Expr : Expr_Ref) return Natural
   is
   begin

      if Expr = null then

         Output.Fault (
            Location => "Arithmetic.Length",
            Text     => "Expression is null");

         return 0;

      else

         case Expr.Kind is

         when Opaque
            | Const
            | Variable_Kind_T =>

            return 1;

         when Unary_Kind =>

            return 1 + Length (Expr.Expr);

         when Binary_Kind =>

            return 1 + Length (Expr.L_Expr) + Length (Expr.R_Expr);

         when Ternary_Kind =>

            return 1 + Length (Expr.L3_Expr)
                     + Length (Expr.R3_Expr)
                     + Length (Expr.C3_Expr);

         end case;

      end if;

   end Length;


   function "=" (Left : Expr_Ref; Right : Value_T) return Boolean
   is
   begin

      return   Left      /= null
      and then Left.Kind  = Const
      and then Signed_Value (Left.Value, Left.Width) = Right;

   end "=";


   function "=" (Left : Expr_Ref; Right : Word_T) return Boolean
   is
   begin

      return   Left      /= null
      and then Left.Kind  = Const
      and then Left.Value = Right;

   end "=";


   function "=" (Left : Expr_Ref; Right : Cell_T) return Boolean
   is
   begin

      return   Left     /= null
      and then Left.Kind = Cell
      and then Left.Cell = Right;

   end "=";


   function Op_Is (Op : Binary_Op_T; Expr : Expr_Ref) return Boolean
   is
   begin

      return Expr.Kind = Binary_Kind and then Expr.Binary = Op;

   end Op_Is;


   function Op_Is (Op : Unary_Op_T; Expr : Expr_Ref) return Boolean
   is
   begin

      return Expr.Kind = Unary_Kind and then Expr.Unary = Op;

   end Op_Is;


   --
   ---   Operations on intervals
   --


   function Is_Small_Negative (
      Interval : Storage.Bounds.Interval_T;
      Width    : Width_T)
   return Boolean
   is
      use Storage.Bounds;
   begin

      return   Bounded (Interval)
      and then Min (Interval) > 0
      and then (Is_Small_Negative (
                   Word  => Word_T (Min (Interval)),
                   Width => Width)
      and       Is_Small_Negative (
                   Word  => Word_T (Max (Interval)),
                   Width => Width));

   end Is_Small_Negative;


   function Opposite_Sign (
      Interval : Storage.Bounds.Interval_T;
      Width    : Width_T)
   return Storage.Bounds.Interval_T
   is
      use Storage.Bounds;
   begin

      if not Is_Small_Negative (Interval, Width) then

         raise Word_Out_Of_Range;
         -- Handled below.

      end if;

      return Storage.Bounds.Interval (
         Min => Signed_Value (
                   Word  => Unsigned_Word (Min (Interval), Width),
                   Width => Width),
         Max => Signed_Value (
                   Word  => Unsigned_Word (Max (Interval), Width),
                   Width => Width));

   exception

   when Word_Out_Of_Range | Constraint_Error =>

      Output.Warning (
           "Cannot reverse sign of"
         & Width_T'Image (Width)
         & "-bit interval"
         & Output.Field_Separator
         & Image (Interval));

      return Interval;

   end Opposite_Sign;


   --
   ---   Bounds on expressions
   --


   function Interval (Cell : Storage.Cell_T; Under : Bounds_T)
   return Storage.Bounds.Interval_T
   is
   begin

      return Interval (Arithmetic.Cell (Cell), Bounds_T'Class (Under));
      --
      -- TBM: The above leaks an Expr_T object.

   end Interval;


   -- overriding
   function Difference (To, From : Storage.Cell_T; Under : Bounds_T)
   return Storage.Bounds.Interval_T
   is
   begin

      return Interval (
         Expr  => Cell (To) - Cell (From),
         Under => Bounds_T'Class (Under));
      --
      -- TBM: The above leaks three Expr_T objects.

   end Difference;


   function Interval (Expr : Expr_Ref; Under : Bounds_T)
   return Storage.Bounds.Interval_T
   is
   begin

      Pragma Warnings (Off, Expr);
      Pragma Warnings (Off, Under);
      -- Yes, GNAT, we know Expr and Under are not referenced.

      Output.Fault (
         Location => "Arithmetic.Interval (Expr, Bounds_T)",
         Text     => "This function is not implemented.");

      return Storage.Bounds.Universal_Interval;   -- TBA implementation

   end Interval;


   function Difference (
      To    : Expr_Ref;
      From  : Storage.Cell_T;
      Under : Bounds_T)
   return Storage.Bounds.Interval_T
   is
   begin

      return Interval (
         Expr  => To - Cell (From),
         Under => Bounds_T'Class (Under));
      --
      -- TBM: The above leaks two Expr_T objects.

   end Difference;


   function Single_Value (Expr : Expr_Ref; Under : Bounds_T)
   return Word_T
   is

      Expr_Interval : Storage.Bounds.Interval_T;
      -- The interval range of the Expr.

   begin

      Expr_Interval := Interval (Expr, Bounds_T'Class (Under));

      if Storage.Bounds.Singular (Expr_Interval) then

         return Unsigned_Word (
            Value => Storage.Bounds.Single_Value (Expr_Interval),
            Width => Width_Of (Expr));

      else

         raise Storage.Bounds.Unbounded;

      end if;

   end Single_Value;


   function Values (Expr : Expr_Ref; Under : Bounds_T)
   return Storage.Bounds.Value_List_T
   is
   begin

      return Storage.Bounds.Values (
         Interval (
            Expr  => Expr,
            Under => Bounds_T'Class (Under)));

   end Values;


   --
   ---   Assignments and effects
   --


   function Identity (Item : Assignment_T) return Boolean
   is
   begin

      return      Item.Kind = Regular
         and then Item.Value.Kind  = Cell
         and then Item.Target.Kind = Cell
         and then Item.Value.Cell  = Item.Target.Cell;

   end Identity;


   function Image (Item : Assignment_List_T) return String
   is
   begin

      if Item'Length = 0 then

         return "";

      elsif Item'Length = 1 then

         return Image (Item(Item'First));

      else

         return
              Image (Item(Item'First))
            & ", "
            & Image (Item(Item'First + 1 .. Item'Last));

      end if;

   end Image;


   --
   ---   Creating assignments and effects
   --


   function Set (
      Target : Variable_T;
      Value  : Expr_Ref)
   return Assignment_T
   is
   begin

      -- Check that Target is a Variable_T:

      if Target.Kind not in Variable_Kind_T then

         Output.Fault (
            Location => "Arithmetic.Set (Target, Value)",
            Text     =>
                 "Target.Kind = "
               & Expr_Kind_T'Image (Target.Kind)
               & " is not a variable.");

      end if;

      if Value /= Unknown
      and then Target.Width /= Value.Width
      then

         Report_Width_Mismatch ("Set", Target, Value);

      end if;

      return Assignment_T'(
         Kind   => Regular,
         Target => Target,
         Value  => Value);

   end Set;


   function Set (Target : Variable_T) return Assignment_T
   is
   begin

      return Assignment_T'(
         Kind   => Regular,
         Target => Target,
         Value  => Unknown);

   end Set;


   function Set (
      Target : Variable_T;
      Cond   : Condition_T;
      Value1 : Expr_Ref;
      Value2 : Expr_Ref)
   return Assignment_T
   is
   begin

      if Value1 /= Unknown
      and then Target.Width /= Value1.Width then

         Report_Width_Mismatch ("Set (cond, value1)", Target, Value1);

      end if;

      if Value2 /= Unknown
      and then Target.Width /= Value2.Width then

         Report_Width_Mismatch ("Set (cond, value2)", Target, Value2);

      end if;

      if Cond /= Unknown then

         return Assignment_T' (
            Kind   => Conditional,
            Target => Target,
            Cond   => Cond,
            Value1 => Value1,
            Value2 => Value2);

      else

         return Assignment_T' (
            Kind   => Regular,
            Target => Target,
            Value  => Unknown);

      end if;

   end Set;


   function Set_Flag (
      Flag : Variable_T;
      Cond : Condition_T)
   return Assignment_T
   is
   begin

      if Flag.Width /= 1 then

         Report_Width_Not_One ("Set_Flag (Condition_T)", Flag);

      end if;

      if Cond /= Unknown then

         return Assignment_T' (
            Kind   => Conditional,
            Target => Flag,
            Cond   => Cond,
            Value1 => Cond_Value(True),
            Value2 => Cond_Value(False));

      else

         return Assignment_T' (
            Kind   => Regular,
            Target => Flag,
            Value  => Unknown);

      end if;

   end Set_Flag;


   function Set (
      Flag : Variable_T;
      Cond : Boolean)
   return Assignment_T
   is
   begin

      if Flag.Width /= 1 then

         Report_Width_Not_One ("Set_Flag (Boolean)", Flag);

      end if;

      return Assignment_T'(
         Kind   => Regular,
         Target => Flag,
         Value  => Cond_Value(Cond));

   end Set;


   function Set_Flag_If_Zero (
      Flag : Variable_T;
      Expr : Expr_Ref)
   return Assignment_T
   is

      Shift : Natural;
      -- The amount of shift, for shift operations.

      Width :  Width_T;
      -- The number of bits in the shifted value.

   begin

      if Is_Constant (Expr) then
         -- The condition can be computed statically.

         return Set (Flag, Expr.Value = 0);
         --
         -- We assume, as a precondition, that Expr.Value is
         -- already limited to Expr.Width bits.

      elsif (Op_Is (Srz, Expr)
         or  Op_Is (Sra, Expr))
      and then Is_Constant (Expr.R_Expr)
      then
         -- Some simplifications are possible, depending
         -- on the amount of shift.

         Width := Expr.Width;

         Shift := Natural (Word_T'Min (
            Expr.R_Expr.Value,
            Word_T (Width)));
         --
         -- Shifting by more than Width is the same as
         -- shifting by Width.

         -- No shift at all:

         if Shift = 0 then
            -- The shift returns the left operand unchanged.

            return Set_Flag_If_Zero (Flag, Expr.L_Expr);

         -- Shift that leaves some bits:

         elsif Shift < Natural (Width) then
            -- Right-shift (Srz or Sra) of a negative number by less than
            -- Width bits leaves the sign bit (1), so the operand must be
            -- non-negative for the result to be zero. Moreover, the
            -- operand must fit in the bits that are dropped by the Sra.

            return Set_Flag (
               Flag => Flag,
               Cond => Ltu (Expr.L_Expr,
                            Const (Word_T'(2 ** Shift), Width, False)));

         -- Sra for >= Width:

         elsif Expr.Binary = Sra then
            -- Sra of a negative number remains negative, even for a
            -- shift of Width or more, while all non-negative numbers
            -- yield zero.

            return Set_Flag (
               Flag => Flag,
               Cond => Ges (Expr.L_Expr, Zero (Width)));

         -- Srz for >= Width:

         else
            -- Srz for Width or more gives zero always.

            return Set (Flag, True);

         end if;

      elsif Op_Is (Rotl, Expr)
         or Op_Is (Rotr, Expr)
      then
         -- Rotation only twirls bits, zeroness is preserved.

         return Set_Flag_If_Zero (Flag, Expr.L_Expr);

      else
         -- General case.

         return Set (
            Target => Flag,
            Value  => EqZero (Expr));

      end if;

   end Set_Flag_If_Zero;


   function Set_Flag_If_Negative (
      Flag : Variable_T;
      Expr : Expr_Ref)
   return Assignment_T
   is
   begin

      if Expr.Kind = Const then
         -- The condition can be computed statically.

         return Set (Flag, Negative (Expr.Value, Expr.Width));

      elsif Op_Is (Sra, Expr) then
         -- Shift-right-arithmetic preserves the sign.

         return Set_Flag_If_Negative (Flag, Expr.L_Expr);

      elsif Op_Is (Srz, Expr) then
         -- Quite a special case.

         if Expr.R_Expr.Kind /= Const then
            -- Need to evaluate both operands.

            return Set_Flag (
               Flag => Flag,
               Cond => Signw (Expr.L_Expr) and EqZero (Expr.R_Expr));

         elsif Expr.R_Expr.Value > 0 then
            -- The result cannot be negative.

            return Set (Flag, False);

         else
            -- Shift zero positions.
            -- The result is the left operand, so is the sign.

            return Set_Flag_If_Negative (Flag, Expr.L_Expr);

         end if;

      else
         -- General case.

         return Set (
            Target => Flag,
            Value  => Signw (Expr));

      end if;

   end Set_Flag_If_Negative;


   function Fracture (Target : Variable_T) return Assignment_T
   is
   begin

      return Assignment_T'(
         Kind   => Fracture,
         Target => Target);

   end Fracture;


   function Range_Pre (
      Target : Variable_T;
      Min    : Expr_Ref;
      Max    : Expr_Ref)
   return Assignment_T
   is
   begin

      if Min /= Unknown and then Target.Width /= Min.Width then

         Report_Width_Mismatch ("Range_Pre (Min)", Target, Min);

      end if;

      if Max /= Unknown and then Target.Width /= Max.Width then

         Report_Width_Mismatch ("Range_Pre (Max)", Target, Max);

      end if;

      return Assignment_T'(
         Kind   => Range_Pre,
         Target => Target,
         Min    => Min,
         Max    => Max);

   end Range_Pre;


   function Range_Rel (
      Target : Variable_T;
      Min    : Expr_Ref;
      Max    : Expr_Ref)
   return Assignment_T
   is
   begin

      if Min /= Unknown and then Target.Width /= Min.Width then

         Report_Width_Mismatch ("Range_Rel (Min)", Target, Min);

      end if;

      if Max /= Unknown and then Target.Width /= Max.Width then

         Report_Width_Mismatch ("Range_Rel (Max)", Target, Max);

      end if;

      return Assignment_T'(
         Kind   => Range_Rel,
         Target => Target,
         Min    => Min,
         Max    => Max);

   end Range_Rel;


   function Range_Post (
      Target : Variable_T;
      Min    : Expr_Ref;
      Max    : Expr_Ref)
   return Assignment_T
   is
   begin

      if Min /= Unknown and then Target.Width /= Min.Width then

         Report_Width_Mismatch ("Range_Post (Min)", Target, Min);

      end if;

      if Max /= Unknown and then Target.Width /= Max.Width then

         Report_Width_Mismatch ("Range_Post (Max)", Target, Max);

      end if;

      return Assignment_T'(
         Kind   => Range_Post,
         Target => Target,
         Min    => Min,
         Max    => Max);

   end Range_Post;


   Null_Assignment : constant Assignment_T := (
      Kind   => Regular,
      Target => null,
      Value  => null);
   --
   -- A dummy assignment, for use in No_Effect only.
   -- Since No_Effect returns an empty vector, this value is
   -- not accessible to users, and would have no meaning
   -- to them.


   Null_Effect : constant Effect_Ref :=
      new Effect_T'(1..0 => Null_Assignment);
   --
   -- For use in No_Effect.


   function No_Effect return Effect_Ref
   is
   begin
      return Null_Effect;
   end No_Effect;


   function To_Effect_Ref (Item : Assignment_T) return Effect_Ref
   is
   begin
      return new Effect_T'((1 => Item));
   end To_Effect_Ref;


   function To_Effect_Ref (Item : Effect_T) return Effect_Ref
   is
   begin
      return new Effect_T'(Item);
   end To_Effect_Ref;


   function Positive_Length (Effect : Effect_T) return Positive
   is
   begin

      return Natural'Max (Effect'Length, 1);

   end Positive_Length;


   function Positive_Length (Effect : Effect_Ref) return Positive
   is
   begin

      return Natural'Max (Effect'Length, 1);

   end Positive_Length;


   --
   ---   Querying effects for particular assignments
   --


   function Pick (
      Kind : Assignment_Kind_T;
      From : Effect_T)
   return Effect_T
   is

      Picked : Effect_T (1 .. From'Length);
      Last   : Natural := 0;
      -- The result will be Picked(1 .. Last).

   begin

      for F in From'Range loop

         if From(F).Kind = Kind then

            Last := Last + 1;

            Picked(Last) := From(F);

         end if;

      end loop;

      return Picked(1 .. Last);

   end Pick;


   function Defining_Assignments (Effect : Effect_T) return Natural
   is

      Defs : Natural := 0;
      -- The result.

   begin

      for E in Effect'Range loop

         if Effect(E).Kind in Defining_Kind_T then

            Defs := Defs + 1;

         end if;

      end loop;

      return Defs;

   end Defining_Assignments;


   function Defining_Assignments (Effect : Effect_T) return Effect_T
   is

      Defs : Effect_T (1 .. Effect'Length);
      Last : Natural := 0;
      -- The result is Defs(1 .. Last).

   begin

      for E in Effect'Range loop

         if Effect(E).Kind in Defining_Kind_T then

            Last       := Last + 1;
            Defs(Last) := Effect(E);

         end if;

      end loop;

      return Defs(1 .. Last);

   end Defining_Assignments;


   procedure Find (
      Target : in     Cell_T;
      Within : in     Effect_T;
      Index  :    out Natural)
   is
   begin

     Index := 0;

     for W in Within'range loop

        if  Within(W).Kind in Defining_Kind_T
        and then Within(W).Target.Kind = Cell
        and then Within(W).Target.Cell = Target
        then

           Index := W;

           exit;

        end if;

     end loop;

   end Find;


   function Definition_Of (
      Target : Cell_T;
      Within : Effect_T)
   return Assignment_T
   is

      Index : Natural;
      -- The index of the assignment to Target, or zero if none.

   begin

      Find (Target, Within, Index);

      if Index = 0 then

         raise No_Such_Assignment;

      end if;

      return Within(Index);

   end Definition_Of;


   --
   ---   Aliasing effects
   --


   function Alias_Range (Item : Variable_T)
   return Storage.Alias_Range_T
   --
   -- The alias range of the variable reference, be it statically
   -- or dynamically addressed.
   --
   is
   begin

      case Variable_Kind_T (Item.Kind) is

      when Cell =>

         return Storage.Alias_Range (Item.Cell);

      when Ref =>

         return Storage.References.Alias_Range (Item.Ref.all);

      end case;

   end Alias_Range;


   function Target_Range (Item  : Assignment_T)
   return Storage.Alias_Range_T
   is
   begin

      case Item.Kind is

      when Defining_Kind_T =>

         return Alias_Range (Item.Target);

      when Range_Kind_T =>

         return Storage.Isolated;

      end case;

   end Target_Range;


   function May_Alias (
      Cell   : Storage.Cell_T;
      Target : Variable_T)
   return Boolean
   is
   begin

      case Target.Kind is

      when Arithmetic.Cell =>
         -- We are comparing two cells.

         return Storage.May_Alias (Cell, Target.Cell);

      when Ref =>
         -- We are comparing a cell and a dynamic memory reference.

         return Storage.May_Alias (
            Left  => Storage.Alias_Range (Cell),
            Right => Storage.References.Alias_Range (Target.Ref.all));

      when others =>

         Output.Fault (
            Location => "Arithmetic.May_Alias",
            Text     =>
                 "Target.Kind is "
               & Expr_Kind_T'Image (Target.Kind));

         return False;

      end case;

   end May_Alias;


   function Target_Range (Effect : Effect_T)
   return Storage.Alias_Range_T
   is
      use type Storage.Alias_Range_T;

      Total : Storage.Alias_Range_T := Storage.Isolated;
      -- Accumulates the alias ranges of the target cells.

   begin

      for E in Effect'Range loop

         Total :=
               Total
            or Alias_Range (Effect(E).Target);

      end loop;

      return Total;

   end Target_Range;


   function May_Alias (
      Cell   : Storage.Cell_T;
      Effect : Effect_T)
   return Boolean
   is
   begin

      for E in Effect'Range loop

         if Effect(E).Kind in Defining_Kind_T
         and then May_Alias (Cell, Effect(E).Target) then

            if Opt.Trace_Alias then

               Output.Trace (
                    "Cell "
                  & Storage.Image (Cell)
                  & " may alias with "
                  & Image (Effect));

            end if;

            return True;

         end if;

      end loop;

      return False;

   end May_Alias;


   --
   ---   Assignment sets of flexible (but bounded) size
   --


   function Image (Item : Assignment_Set_T) return String
   is
   begin

      return Image (To_Effect (Item));

   end Image;


   procedure Erase (Item : in out Assignment_Set_T)
   is
   begin
      Bounded_Effects.Erase (Item.Set);
   end Erase;


   function Last (Item : Assignment_Set_T) return Positive
   is
   begin
      return Bounded_Effects.Last (Item.Set);
   end Last;


   function Length (Item : Assignment_Set_T) return Natural
   is
   begin

      return Bounded_Effects.Length (Item.Set);

   end Length;


   function Same_Variable (Left, Right : Variable_T) return Boolean
   --
   -- Whether Left and Right are the same variable. This is an
   -- incomplete decision: a True result is certain, a False result
   -- is uncertain for dynamic memory references.
   --
   is
      use type Storage.References.Boundable_T'Class;
   begin

      if Left.Kind /= Right.Kind then

         return False;

      else

         case Variable_Kind_T (Left.Kind) is

         when Cell =>

            return Left.Cell = Right.Cell;

         when Ref =>

           return Left.Ref.all = Right.Ref.all;

         end case;

      end if;

   end Same_Variable;


   function Is_New (
      Target : Variable_T;
      Within : Effect_T)
   return Boolean
   --
   -- Checks that this Target is not already the target of some
   -- assignment Within the given assignment set, and issues a
   -- Fault if it is. Returns True for a new target, False for
   -- a duplicated one.
   --
   is

      Result : Boolean := True;
      -- No duplicating seen so far.

   begin

      for W in Within'Range loop

         if Same_Variable (Within(W).Target, Target) then

            Output.Fault (
               Location => "Arithmetic.Check_New (Target)",
               Text     =>
                    "Target "
                  & Image (Target)
                  & " already present in "
                  & Image (Within));

            Result := False;

         end if;

      end loop;

      return Result;

   end Is_New;


   procedure Add (
      To   : in out Assignment_Set_T;
      More : in     Assignment_T)
   is
   begin

      if (not Opt.Check_Dup_Target)
      or else Is_New (Target => More.Target, Within => To_Effect (To))
      then
         -- Not a duplicated target, as far as we know.

         Bounded_Effects.Append (
            To    => To.Set,
            Value => More);

      end if;

   end Add;


   procedure Add (
      To   : in out Assignment_Set_T;
      More : in     Effect_T)
   is
      use Bounded_Effects;
   begin

      for M in More'Range loop

         Add (To => To, More => More(M));

      end loop;

   end Add;


   procedure Veil (
      Variable : in     Variable_T;
      Within   : in out Assignment_Set_T)
   is
   begin

      Add (To => Within, More => Set (Target => Variable));

   end Veil;


   procedure Veil (
      Variables : in     Variable_List_T;
      Within    : in out Assignment_Set_T)
   is
   begin

      for V in Variables'Range loop

         Add (
            To   => Within,
            More => Set (Target => Variables(V)));

      end loop;

   end Veil;


   procedure Fracture (
      Variable : in     Variable_T;
      Within   : in out Assignment_Set_T)
   is
   begin

      Add (To => Within, More => Fracture (Target => Variable));

   end Fracture;


   procedure Fracture (
      Variables : in     Variable_List_T;
      Within    : in out Assignment_Set_T)
   is
   begin

      for V in Variables'Range loop

         Add (
            To   => Within,
            More => Fracture (Target => Variables(V)));

      end loop;

   end Fracture;


   function Element (
      From   : Assignment_Set_T;
      Number : Positive)
   return Assignment_T
   is
   begin

      return Bounded_Effects.Element (From.Set, Number);

   end Element;


   procedure Find (
      Target : in     Cell_T;
      Within : in     Assignment_Set_T;
      Index  :    out Natural)
   is

      Ass : Assignment_T;
      -- One of the assignments in Within.

   begin

     Index := 0;

     for W in reverse 1 ..  Length (Within) loop

        Ass := Element (Within, W);

        if       Ass.Kind in Defining_Kind_T
        and then Ass.Target.Kind = Cell
        and then Ass.Target.Cell = Target
        then

           Index := W;

           exit;

        end if;

     end loop;

   end Find;


   function Definition_Of (
      Target : Cell_T;
      Within : Assignment_Set_T)
   return Assignment_T
   is

      Index : Natural;
      -- The index of the assignment to Target, or zero if none.

   begin

      Find (Target, Within, Index);

      if Index = 0 then

         raise No_Such_Assignment;

      end if;

      return Bounded_Effects.Element (Within.Set, Index);

   end Definition_Of;


   function To_Effect (Item : Assignment_Set_T) return Effect_T
   is
      use Bounded_Effects;
   begin

      return To_Vector (Item.Set);

   end To_Effect;


   function To_Effect_Ref (Item : Assignment_Set_T) return Effect_Ref
   is
      use Bounded_Effects;
   begin

      return To_Effect_Ref (To_Vector (Item.Set));

   end To_Effect_Ref;


   --
   ---   Cells used or defined in arithmetic expressions and effects
   --


   function Is_Defined (
      Cell : Cell_T;
      By   : Effect_T)
   return Boolean
   is

      Index : Natural;
      -- The index of the defining assignment in By, or zero.

   begin

     Find (Target => Cell, Within => By, Index => Index);

     return Index /= 0;

   end Is_Defined;


   function Is_Defined (
      Cell : Cell_T;
      By   : Assignment_Set_T)
   return Boolean
   is
   begin

      return Is_Defined (Cell, To_Effect (By));
      --
      -- TBA faster implementation?

   end Is_Defined;


   function Is_Used (
      Cell : Cell_T;
      By   : Expr_Ref)
   return Boolean
   is
   begin

      if By = null then

         return False;

      else

         case By.Kind is

         when Opaque | Const =>

            return False;

         when Arithmetic.Cell =>

            return By.Cell = Cell;

         when Ref =>

            return Storage.Is_Member (
               Cell    => Cell,
               Of_List => Storage.References.Basis (By.Ref.all));

         when Unary_Kind =>

            return Is_Used (Cell => Cell, By => By.Expr);

         when Binary_Kind =>

            return     Is_Used (Cell => Cell, By => By.L_Expr)
               or else Is_Used (Cell => Cell, By => By.R_Expr);

         when Ternary_Kind =>

            return     Is_Used (Cell => Cell, By => By.L3_Expr)
               or else Is_Used (Cell => Cell, By => By.R3_Expr)
               or else Is_Used (Cell => Cell, By => By.C3_Expr);

         end case;

      end if;

   end Is_Used;


   function Is_Used (
      Cell : Cell_T;
      By   : Expressions_T)
   return Boolean
   is
   begin

      for B in By'Range loop

         if Is_Used (Cell => Cell, By => By(B)) then

            return True;

         end if;

      end loop;

      return False;

   end Is_Used;


   function Is_Used (
      Cell : Cell_T;
      By   : Effect_T)
   return Boolean
   is
   begin

      for B in By'range loop

         case By(B).Kind is

            when Regular =>

               if      Is_Used (Cell => Cell, By => By(B).Value)
               or else Is_Used (Cell => Cell, By => By(B).Target)
               then

                  return True;

               end if;

            when Conditional =>

               if      Is_Used (Cell => Cell, By => By(B).Cond)
               or else Is_Used (Cell => Cell, By => By(B).Value1)
               or else Is_Used (Cell => Cell, By => By(B).Value2)
               or else Is_Used (Cell => Cell, By => By(B).Target)
               then

                  return True;

               end if;

            when Fracture =>

               return Is_Used (Cell => Cell, By => By(B).Target);

            when Range_Kind_T =>

               null;

         end case;

      end loop;

      -- Found no uses in Defining assignments.

      return False;

   end Is_Used;


   function Any_Cell_Is_Used (
      From : Cell_Set_T;
      By   : Expr_Ref)
   return Boolean
   is
   begin

      if By = null then

         return False;

      else

         case By.Kind is

         when Opaque | Const =>

            return False;

         when Arithmetic.Cell =>

            return Storage.Is_Member (Cell => By.Cell, Of_Set => From);

         when Ref =>

            return Storage.Any_Member (
               Cells  => Storage.References.Basis (By.Ref.all),
               Of_Set => From);

         when Unary_Kind =>

            return Any_Cell_Is_Used (From => From, By => By.Expr);

         when Binary_Kind =>

            return     Any_Cell_Is_Used (From => From, By => By.L_Expr)
               or else Any_Cell_Is_Used (From => From, By => By.R_Expr);

         when Ternary_Kind =>

            return     Any_Cell_Is_Used (From => From, By => By.L3_Expr)
               or else Any_Cell_Is_Used (From => From, By => By.R3_Expr)
               or else Any_Cell_Is_Used (From => From, By => By.C3_Expr);

         end case;

      end if;

   end Any_Cell_Is_Used;


   function Any_Cell_Is_Used (
      From : Cell_Set_T;
      By   : Assignment_T)
   return Boolean
   is
   begin

      if By.Target.Kind = Ref
      and then
         Storage.Any_Member (
            Cells  => Storage.References.Basis (By.Target.Ref.all),
            Of_Set => From)
      then
         -- Some of the Basis cells for the Target reference
         -- are From this set.

         return True;

      else

         case By.Kind is

         when Regular =>

            return Any_Cell_Is_Used (From => From, By => By.Value);

         when Conditional =>

            return  Any_Cell_Is_Used (From => From, By => By.Cond  )
            or else Any_Cell_Is_Used (From => From, By => By.Value1)
            or else Any_Cell_Is_Used (From => From, By => By.Value2);

         when Fracture =>

            return False;

         when Range_Pre =>

            return
               (By.Target.Kind = Cell
                and then
                Storage.Is_Member (Cell => By.Target.Cell, Of_Set => From))
            or else Any_Cell_Is_Used (From => From, By => By.Min)
            or else Any_Cell_Is_Used (From => From, By => By.Max);

         when Range_Rel =>

            return  Any_Cell_Is_Used (From => From, By => By.Min)
            or else Any_Cell_Is_Used (From => From, By => By.Max);

         when Range_Post =>

            return False;

         end case;

      end if;

   end Any_Cell_Is_Used;


   procedure Add_Cells_Used (
      By   : in     Expr_Ref;
      Refs : in     Boolean;
      To   : in out Cell_Set_T)
   is
   begin

      if By /= null then

         case By.Kind is

         when Opaque | Const =>

            null;

         when Cell =>

            Storage.Add (Cell => By.Cell, To => To);

         when Ref =>

            if Refs then

               Storage.References.Add_Basis_Cells (
                  From => By.Ref.all,
                  To   => To);

            end if;

         when Unary_Kind =>

            Add_Cells_Used (By => By.Expr, Refs => Refs, To => To);

         when Binary_Kind =>

            Add_Cells_Used (By => By.L_Expr, Refs => Refs, To => To);
            Add_Cells_Used (By => By.R_Expr, Refs => Refs, To => To);

         when Ternary_Kind =>

            Add_Cells_Used (By => By.L3_Expr, Refs => Refs, To => To);
            Add_Cells_Used (By => By.R3_Expr, Refs => Refs, To => To);
            Add_Cells_Used (By => By.C3_Expr, Refs => Refs, To => To);

         end case;

      end if;

   end Add_Cells_Used;


   procedure Add_Cells_Used (
      By   : in     Expressions_T;
      Refs : in     Boolean;
      To   : in out Cell_Set_T)
   is
   begin

      for B in By'Range loop

         Add_Cells_Used (By => By(B), Refs => Refs, To => To);

      end loop;

   end Add_Cells_Used;


   procedure Add_Cells_Used (
      By    : in     Expr_Ref;
      Refs  : in     Boolean;
      To    : in out Cell_Set_T;
      Added : in out Boolean)
   is
   begin

      if By /= null then

         case By.Kind is

         when Opaque | Const =>

            null;

         when Cell =>

            Storage.Add (Cell => By.Cell, To => To, Added => Added);

         when Ref =>

            if Refs then

               Storage.References.Add_Basis_Cells (
                  From  => By.Ref.all,
                  To    => To,
                  Added => Added);

            end if;

         when Unary_Kind =>

            Add_Cells_Used (
               By    => By.Expr,
               Refs  => Refs,
               To    => To,
               Added => Added);

         when Binary_Kind =>

            Add_Cells_Used (
               By    => By.L_Expr,
               Refs  => Refs,
               To    => To,
               Added => Added);

            Add_Cells_Used (
               By    => By.R_Expr,
               Refs  => Refs,
               To    => To,
               Added => Added);

         when Ternary_Kind =>

            Add_Cells_Used (
               By    => By.L3_Expr,
               Refs  => Refs,
               To    => To,
               Added => Added);

            Add_Cells_Used (
               By    => By.R3_Expr,
               Refs  => Refs,
               To    => To,
               Added => Added);

            Add_Cells_Used (
               By    => By.C3_Expr,
               Refs  => Refs,
               To    => To,
               Added => Added);

         end case;

      end if;

   end Add_Cells_Used;


   procedure Add_Cells_Used (
      By   : in     Effect_T;
      Refs : in     Boolean;
      Post : in     Boolean;
      To   : in out Cell_Set_T)
   is

      Added : Boolean := False;
      -- Whether some cells were actually added to To.
      -- Unused output from Add_Cells_Used (Assignment).

   begin

      for B in By'range loop

         Add_Cells_Used (
            By    => By(B),
            Refs  => Refs,
            Post  => Post,
            To    => To,
            Added => Added);

      end loop;

   end Add_Cells_Used;


   procedure Add_Cells_Used (
      By    : in     Assignment_T;
      Refs  : in     Boolean;
      Post  : in     Boolean;
      To    : in out Cell_Set_T;
      Added : in out Boolean)
   is
   begin

      if Refs and By.Target.Kind = Ref then
         -- The target address uses some cells:

         Storage.References.Add_Basis_Cells (
            From  => By.Target.Ref.all,
            To    => To,
            Added => Added);

      end if;

      case By.Kind is

      when Regular =>

         Add_Cells_Used (
            By    => By.Value,
            Refs  => Refs,
            To    => To,
            Added => Added);

      when Conditional =>

         Add_Cells_Used (
            By    => By.Cond,
            Refs  => Refs,
            To    => To,
            Added => Added);

         Add_Cells_Used (
            By    => By.Value1,
            Refs  => Refs,
            To    => To,
            Added => Added);

         Add_Cells_Used (
            By    => By.Value2,
            Refs  => Refs,
            To    => To,
            Added => Added);

      when Fracture =>

         null;

      when Range_Pre =>

         if By.Target.Kind = Cell then
            -- The target cell is used, too.

            Storage.Add (Cell => By.Target.Cell, To => To);

         end if;

         Add_Cells_Used (
            By    => By.Min,
            Refs  => Refs,
            To    => To,
            Added => Added);

         Add_Cells_Used (
            By    => By.Max,
            Refs  => Refs,
            To    => To,
            Added => Added);

      when Range_Rel =>

         if Post and By.Target.Kind = Cell then
            -- The target cell is used, too.

            Storage.Add (Cell => By.Target.Cell, To => To);

         end if;

         Add_Cells_Used (
            By    => By.Min,
            Refs  => Refs,
            To    => To,
            Added => Added);

         Add_Cells_Used (
            By    => By.Max,
            Refs  => Refs,
            To    => To,
            Added => Added);

      when Range_Post =>

         if Post then

            if By.Target.Kind = Cell then
               -- The target cell is used, too.

               Storage.Add (Cell => By.Target.Cell, To => To);

            end if;

            Add_Cells_Used (
               By    => By.Min,
               Refs  => Refs,
               To    => To,
               Added => Added);

            Add_Cells_Used (
               By    => By.Max,
               Refs  => Refs,
               To    => To,
               Added => Added);

         end if;

      end case;

   end Add_Cells_Used;


   procedure Add_Reference_Bases (
      From : in     Expr_Ref;
      To   : in out Cell_Set_T)
   is
   begin

      if From /= null then

         case From.Kind is

         when Opaque | Cell | Const => null;

         when Ref =>

            Storage.References.Add_Basis_Cells (
               From => From.Ref.all,
               To   => To);

         when Unary_Kind =>

            Add_Reference_Bases (From => From.Expr, To => To);

         when Binary_Kind =>

            Add_Reference_Bases (From => From.L_Expr, To => To);
            Add_Reference_Bases (From => From.R_Expr, To => To);

         when Ternary_Kind =>

            Add_Reference_Bases (From => From.L3_Expr, To => To);
            Add_Reference_Bases (From => From.R3_Expr, To => To);
            Add_Reference_Bases (From => From.C3_Expr, To => To);

         end case;

      end if;

   end Add_Reference_Bases;


   procedure Add_Cells_Defined (
      By : in     Effect_T;
      To : in out Cell_Set_T)
   is
      use Storage;

      Target : Storage.Cell_T;
      -- The target cell, if statically known.

   begin

      for B in By'range loop

         if By(B).Target.Kind = Cell then
            -- The target cell is statically known.

            Target := By(B).Target.Cell;

            if By(B).Kind in Defining_Kind_T then

               Add (Cell => Target, To => To);

            end if;

         -- else
         --    TBD/TBA for Target.Kind = Ref (dynamic addressing).

         end if;

      end loop;

   end Add_Cells_Defined;


   procedure Remove_Cells_Defined (
      By   : in     Effect_T;
      From : in out Cell_Set_T)
   is
   begin

      for B in By'range loop

         if       By(B).Kind in Defining_Kind_T
         and then By(B).Target.Kind = Cell
         then

            Storage.Remove (Cell => By(B).Target.Cell, From => From);

         end if;

      end loop;

   end Remove_Cells_Defined;


   function Cells_Used (By : Expr_Ref) return Cell_List_T
   is
   begin

      return Cells_Used (By => Expressions_T'(1 => By));

   end Cells_Used;


   function Cells_Used (By : Expressions_T) return Cell_List_T
   is

      Set : Storage.List_Cell_Sets.Set_T;
      -- The cells used, initially none.

   begin

      for B in By'Range loop

         Add_Cells_Used (By => By(B), Refs => True, To => Set);

      end loop;

      return Storage.List_Cell_Sets.To_List (Set);

   end Cells_Used;


   --
   ---   Checking for (unresolved) dynamic references
   --


   function Dynamic (Expr : Expr_Ref) return Boolean
   is
   begin

      if Expr = null then

         return False;

      else

         case Expr.Kind is

         when Opaque | Cell | Const => return False;

         when Ref => return True;

         when Unary_Kind => return Dynamic (Expr.Expr);

         when Binary_Kind =>

            return     Dynamic (Expr.L_Expr)
               or else Dynamic (Expr.R_Expr);

         when Ternary_Kind =>

            return     Dynamic (Expr.L3_Expr)
               or else Dynamic (Expr.R3_Expr)
               or else Dynamic (Expr.C3_Expr);

         end case;

      end if;

   end Dynamic;


   function Dynamic_Values (Assignment : Assignment_T) return Boolean
   is
   begin

      case Assignment.Kind is

      when Regular => return Dynamic (Assignment.Value);

      when Conditional =>

         return     Dynamic (Assignment.Cond)
            or else Dynamic (Assignment.Value1)
            or else Dynamic (Assignment.Value2);

      when Fracture => return False;

      when Range_Kind_T =>

         return     Dynamic (Assignment.Min)
            or else Dynamic (Assignment.Max);

      end case;

   end Dynamic_Values;


   function Dynamic (Assignment : Assignment_T) return Boolean
   is
   begin

      return  Dynamic        (Assignment.Target)
      or else Dynamic_Values (Assignment);

   end Dynamic;


   function Dynamic_Values (Effect : Effect_T) return Boolean
   is
   begin

      for E in Effect'Range loop

         if Dynamic_Values (Effect(E)) then

            return True;

         end if;

      end loop;

      return False;

   end Dynamic_Values;


   function Dynamic (Effect : Effect_T) return Boolean
   is
   begin

      for E in Effect'Range loop

         if Dynamic (Effect(E)) then

            return True;

         end if;

      end loop;

      return False;

   end Dynamic;


   function Targets_Reference (Item : Assignment_T) return Boolean
   is
   begin

      return Item.Target.Kind = Ref;

   end Targets_Reference;


   function Uses_Reference (Item : Expr_Ref) return Boolean
   is
   begin

      if Item = null then

         return False;

      else

         case Item.Kind is

         when Opaque | Cell | Const =>

            return False;

         when Ref =>

            return True;

         when Unary_Kind =>

            return Uses_Reference (Item.Expr);

         when Binary_Kind =>

            return     Uses_Reference (Item.L_Expr)
               or else Uses_Reference (Item.R_Expr);

         when Ternary_Kind =>

            return     Uses_Reference (Item.L3_Expr)
               or else Uses_Reference (Item.R3_Expr)
               or else Uses_Reference (Item.C3_Expr);

         end case;

      end if;

   end Uses_Reference;


   function Uses_Reference (Item : Assignment_T) return Boolean
   is
   begin

      case Item.Kind is

      when Regular =>

         return Uses_Reference (Item.Value);

      when Conditional =>

         return     Uses_Reference (Item.Cond  )
            or else Uses_Reference (Item.Value1)
            or else Uses_Reference (Item.Value2);

      when Fracture =>

         return False;

      when Range_Kind_T =>

         return     Uses_Reference (Item.Min)
            or else Uses_Reference (Item.Max);

      end case;

   end Uses_Reference;


   --
   ---   Resolving references within expressions
   --


   function Reference_Bounded (
      Expr   : Expr_Ref;
      Bounds : Storage.Bounds.Bounds_T'Class)
   return Expr_Ref
   is
      use type Storage.References.Boundable_Ref;

      Operand, Left, Right, Carry : Expr_Ref;
      -- The bounded operands of a unary, binary, or ternary operation.

      Ref_Cell : Storage.Cell_T;
      -- Perhaps the referent of a Ref.

      Const_Cell : Boolean;
      -- Whether the Ref_Cell is a constant cell.

      Const_Value : Word_T;
      -- The value of a constant Ref_Cell, when Const_Cell.

      New_Ref : Storage.References.Boundable_Ref;
      -- Perhaps a more constrained Ref.

   begin

      -- Ordinary structural induction/recursion:

      case Expr.Kind is

      when Opaque | Cell | Const => return Expr;

      when Ref =>

         -- So here we are with a boundable reference.
         -- To work: first we boldly try to resolve the
         -- reference to a single referent cell:

         Ref_Cell := Storage.References.Referent (
            Ref   => Expr.Ref.all,
            Under => Bounds);

         if Ref_Cell /= Storage.No_Cell then
            -- Joy and jubilation, we have referent cell.

            if Options.General.Trace_Resolution then

               Output.Trace (
                    "Resolved pointer "
                  & Storage.References.Image (Expr.Ref.all)
                  & " to cell "
                  & Storage.Image (Ref_Cell)
                  & " under "
                  & Storage.Bounds.Image (Bounds));

            end if;

            Storage.References.Check_Constancy (
               Cell  => Ref_Cell,
               Ref   => Expr.Ref.all,
               Const => Const_Cell,
               Value => Const_Value);

            if Const_Cell then
               -- The reference cell has a constant value.

               return Arithmetic.Const (
                  Value  => Const_Value,
                  Width  => Expr.Ref.Width,
                  Signed => False);
               --
               -- The choice of an unsigned value is arbitrary, TBM.
               -- If the "Signed" flag is retained, the choice should
               -- be made by Check_Constancy.

            else
               -- The referent cell has a variable value.

               return Arithmetic.Cell (Ref_Cell);

            end if;

         else
            -- The Ref is not yet resolved to a cell.
            -- So try to for the lesser good: a more
            -- constrained reference:

            Storage.References.Apply (
               Bounds => Bounds,
               Upon   => Expr.Ref.all,
               Giving => New_Ref);

            if New_Ref /= null then
               -- Calm content, we have a more constrained ref.

               if Options.General.Trace_Resolution then

                  Output.Trace (
                       "Constrained pointer "
                     & Storage.References.Image (Expr.Ref.all)
                     & " to "
                     & Storage.References.Image (New_Ref.all)
                     & " under "
                     & Storage.Bounds.Image (Bounds));

               end if;

               return Reference (New_Ref);

            else
               -- Sad resignation, no dice.

               return Expr;

            end if;

         end if;

      when Unary_Kind =>

         Operand := Reference_Bounded (
            Expr   => Expr.Expr,
            Bounds => Bounds);

         if Operand = Expr.Expr then
            -- No change.

            return Expr;

         else
            -- The operand changed.

            return Unary (Expr.Unary, Operand, Expr.Width);

         end if;

      when Binary_Kind =>

         Left := Reference_Bounded (
            Expr   => Expr.L_Expr,
            Bounds => Bounds);

         Right := Reference_Bounded (
            Expr   => Expr.R_Expr,
            Bounds => Bounds);

         if Left = Expr.L_Expr and Right = Expr.R_Expr then
            -- No change.

            return Expr;

         else
            -- One or both operands changed.

            return Binary (
               Operation => Expr.Binary,
               Left      => Left,
               Right     => Right,
               Width     => Expr.Width);

         end if;

      when Ternary_Kind =>

         Left := Reference_Bounded (
            Expr   => Expr.L3_Expr,
            Bounds => Bounds);

         Right := Reference_Bounded (
            Expr   => Expr.R3_Expr,
            Bounds => Bounds);

         Carry := Reference_Bounded (
            Expr   => Expr.C3_Expr,
            Bounds => Bounds);

         if  Left  = Expr.L3_Expr
         and Right = Expr.R3_Expr
         and Carry = Expr.C3_Expr
         then
            -- No change.

            return Expr;

         else
            -- Some operand changed.

            return Ternary (
               Operation => Expr.Ternary,
               Left      => Left,
               Right     => Right,
               Carry     => Carry,
               Width     => Expr.Width);

         end if;

      end case;

   end Reference_Bounded;


   procedure Bound_References (
      Within  : in out Expr_Ref;
      Bounds  : in     Storage.Bounds.Bounds_T'Class;
      Bounded : in out Boolean)
   --
   -- Applies the Bounds to Within, using Reference_Bounded.
   -- If this changes Within, sets Bounded to True, otherwise
   -- returns Bounded unchanged.
   --
   is

      New_Expr : constant Expr_Ref := Reference_Bounded (Within, Bounds);
      -- The original expression with some references perhaps bounded.

   begin

      if New_Expr /= Within then

         Bounded := True;

         Within := New_Expr;

      end if;

   end Bound_References;


   procedure Bound_References (
      Within  : in out Assignment_T;
      Bounds  : in     Storage.Bounds.Bounds_T'Class;
      Bounded :    out Boolean)
   is
   begin

      Bounded := False;
      -- Initial value, changed below if we manage to bound some refs.

      Bound_References (Within.Target, Bounds, Bounded);

      case Within.Kind is

      when Regular =>

         Bound_References (Within.Value , Bounds, Bounded);

      when Conditional =>

         Bound_References (Within.Cond  , Bounds, Bounded);
         Bound_References (Within.Value1, Bounds, Bounded);
         Bound_References (Within.Value2, Bounds, Bounded);

      when Fracture =>

         null;

      when Range_Kind_T =>

         Bound_References (Within.Min   , Bounds, Bounded);
         Bound_References (Within.Max   , Bounds, Bounded);

      end case;

   end Bound_References;


   function Reference_Bounded (
      Effect : Effect_Ref;
      Bounds : Storage.Bounds.Bounds_T'Class)
   return Effect_Ref
   is

      Result : Effect_Ref := Effect;
      -- The result, initially assumed to be the same as the input.

      Assignment : Assignment_T;
      -- One of the assignments in the Effect, perhaps changed.

      Bounded : Boolean;
      -- Whether the Assignment is changed by the Bounds.

   begin

      for E in Effect'Range loop

         Assignment := Effect(E);

         Bound_References (
            Within  => Assignment,
            Bounds  => Bounds,
            Bounded => Bounded);

         if Bounded then
            -- This Assignment changed.

            if Result = Effect then
               -- This is the first changed assignment and shows
               -- that the whole effect changes.

               Result := new Effect_T'(Effect.all);
               -- A copy of the original Effect.

            end if;

            Result(E) := Assignment;

         end if;

      end loop;

      return Result;

   end Reference_Bounded;


   --
   ---   Image functions
   --


   -- not overriding
   function Opaque_Image (Imager : Expr_Imager_T)
   return String
   is
   begin

      Pragma Warnings (Off, Imager);
      -- Yes, GNAT, we know Imager is not referenced.

      return "?";

   end Opaque_Image;


   -- not overriding
   function Const_Image (
      Item   : Const_Expr_T;
      Imager : Expr_Imager_T)
   return String
   is
   begin

      Pragma Warnings (Off, Imager);
      -- Yes, GNAT, we know Imager is not referenced.

      return Image (
         Item   => Item.Value,
         Width  => Item.Width,
         Signed => Item.Signed);

   end Const_Image;


   -- not overriding
   function Cell_Image (
      Item   : Cell_Expr_T;
      Imager : Expr_Imager_T)
   return String
   is
   begin

      Pragma Warnings (Off, Imager);
      -- Yes, GNAT, we know Imager is not referenced.

      return Storage.Image (Item.Cell);

   end Cell_Image;


   -- not overriding
   function Ref_Image (
      Item   : Ref_Expr_T;
      Imager : Expr_Imager_T)
   return String
   is
   begin

      Pragma Warnings (Off, Imager);
      -- Yes, GNAT, we know Imager is not referenced.

      return Storage.References.Image (Item.Ref.all);

   end Ref_Image;


   -- not overriding
   function Unary_Op_Image (
      Item   : Unary_Op_T;
      Imager : Expr_Imager_T)
   return String
   is
   begin

      Pragma Warnings (Off, Imager);
      -- Yes, GNAT, we know Imager is not referenced.

      case Item is

      when Notw   => return "notw";
      when Notx   => return "not";
      when EqZero => return "eq0";
      when EqOne  => return "eq1";
      when Signw  => return "sgn" ;
      when Exts   => return "exts";
      when Extz   => return "extz";
      when Trunl  => return "trunl";
      when Trunh  => return "trunh";

      end case;

   end Unary_Op_Image;


   -- not overriding
   function Unary_Image (
      Item   : Unary_Expr_T;
      Imager : Expr_Imager_T)
   return String
   is
   begin

      return Unary_Op_Image (Item.Unary, Expr_Imager_T'Class (Imager))
           & '('
           &    Image (Item.Expr.all, Expr_Imager_T'Class (Imager))
           & ')';

   end Unary_Image;


   -- not overriding
   function Binary_Op_Image (
      Item   : Binary_Op_T;
      Imager : Expr_Imager_T)
   return String
   is
   begin

      Pragma Warnings (Off, Imager);
      -- Yes, GNAT, we know Imager is not referenced.

      case Item is

      when Plus    => return "+" ;
      when Minus   => return "-" ;
      when Mulu    => return "#*";
      when Muls    => return "*";

      when Plus_C  => return "+<";
      when Plus_N  => return "+-";
      when Plus_V  => return "+^";

      when Minus_B => return "->";
      when Minus_C => return "-<";
      when Minus_N => return "--";
      when Minus_V => return "-^";

      when Eq      => return "=" ;

      when Lts     => return "<" ;
      when Gts     => return ">" ;
      when Les     => return "<=";
      when Ges     => return ">=";

      when Ltu     => return "#<" ;
      when Gtu     => return "#>" ;
      when Leu     => return "#<=";
      when Geu     => return "#>=";

      when Andx    => return " and ";
      when Orx     => return " or ";

      when Andw    => return " andw ";
      when Orw     => return " orw ";
      when Xorw    => return " xorw ";

      when Slz     => return " slz ";
      when Srz     => return " srz ";
      when Sra     => return " sra ";
      when Rotl    => return " rotl ";
      when Rotr    => return " rotr ";

      when Conc    => return " & ";

      end case;

   end Binary_Op_Image;


   -- not overriding
   function Binary_Image (
      Item   : Binary_Expr_T;
      Imager : Expr_Imager_T)
   return String
   is

      Img : Expr_Imager_T'Class renames Expr_Imager_T'Class (Imager);

   begin

      return '('
           &           Image (Item.L_Expr.all, Img)
           & Binary_Op_Image (Item.Binary    , Img)
           &           Image (Item.R_Expr.all, Img)
           & ')';

   end Binary_Image;


   -- not overriding
   function Ternary_Op_Image (
      Item   : Ternary_Op_T;
      Imager : Expr_Imager_T)
   return String
   is
   begin

      Pragma Warnings (Off, Imager);
      -- Yes, GNAT, we know Imager is not referenced.

      case Item is

      when Plus          => return "+" ;
      when Minus_With_B  => return "->";
      when Minus_With_C  => return "-<";

      when Plus_C        => return "+<";
      when Plus_N        => return "+-";
      when Plus_V        => return "+^";

      when Minus_B       => return "->";
      when Minus_C       => return "-<";
      when Minus_BN      => return "->-";
      when Minus_CN      => return "-<-";
      when Minus_BV      => return "->^";
      when Minus_CV      => return "-<^";

      end case;

   end Ternary_Op_Image;


   -- not overriding
   function Ternary_Image (
      Item   : Ternary_Expr_T;
      Imager : Expr_Imager_T)
   return String
   is

      Img : Expr_Imager_T'Class renames Expr_Imager_T'Class (Imager);

   begin

      case Item.Ternary is

      when Plus | Minus_With_B | Minus_With_C =>

         return
             '('
           &            Image (Item.L3_Expr.all, Img)
           & Ternary_Op_Image (Item.Ternary    , Img)
           &            Image (Item.R3_Expr.all, Img)
           & Ternary_Op_Image (Item.Ternary    , Img)
           &            Image (Item.C3_Expr.all, Img)
           & ')';

      when Ternary_Predicate_Op_T =>

         return
             Ternary_Op_Image (Item.Ternary, Img)
           & '('
           & Image (Item.L3_Expr.all, Img)
           & ','
           & Image (Item.R3_Expr.all, Img)
           & ','
           & Image (Item.C3_Expr.all, Img)
           & ')';

      end case;

   end Ternary_Image;


   function Variable_Image (
      Item   : Expr_T;
      Imager : Expr_Imager_T'Class)
   return String
   is
   begin

      case Variable_Kind_T (Item.Kind) is

      when Cell =>

         return Cell_Image (Item, Imager);

      when Ref =>

         return Ref_Image (Item, Imager);

      end case;

   end Variable_Image;


   function Image (
      Item   : Expr_T;
      Imager : Expr_Imager_T'Class)
   return String
   is
   begin

      case Item.Kind is

      when Opaque =>

         return Opaque_Image (Imager);

      when Const =>

         return Const_Image (Item, Imager);

      when Variable_Kind_T =>

         return Variable_Image (Item, Imager);

      when Unary_Kind =>

         return Unary_Image (Item, Imager);

      when Binary_Kind =>

         return Binary_Image (Item, Imager);

      when Ternary_Kind =>

         return Ternary_Image (Item, Imager);

      end case;

   end Image;


   function Image (
      Item   : Expr_Ref;
      Imager : Expr_Imager_T'Class)
   return String
   is
   begin

      if Item = Always then

         return "true";

      elsif Item = Never then

         return "false";

      elsif Item = null then

         return "[]";

      else

         return Image (Item.all, Imager);

      end if;

   end Image;


   --
   -- Default Image functions:
   --


   Basic_Imager : Expr_Imager_T;
   --
   -- The basic, or default, way to Image an expression.


   function Var_Image (Item : Expr_T) return String
   is
   begin

      return Variable_Image (Item, Basic_Imager);

   end Var_Image;


   function Image (Item : Expr_T) return String
   is
   begin

      return Image (Item, Basic_Imager);

   end Image;


   function Image (Item : Expr_Ref) return String
   is
   begin

      return Image (Item, Basic_Imager);

   end Image;


   function Image (Kind : Range_Kind_T) return String
   --
   -- A small mark to show Pre or Post.
   --
   is
   begin

      case Kind is
      when Range_Pre  => return "pre ";
      when Range_Rel  => return "rel ";
      when Range_Post => return "post ";
      end case;

   end Image;


   function Image (Item : Assignment_T) return String
   is

      Target : constant String := Var_Image (Item.Target.all);
      -- The target variable.

   begin

      case Item.Kind is

      when Regular =>

         return Target
            & " = "
            & Image (Item.Value);

      when Conditional =>

         return Target
            & " = (if "
            & Image (Item.Cond)
            & " then "
            & Image (Item.Value1)
            & " else "
            & Image (Item.Value2)
            & ')';

      when Fracture =>

         return Target & '?';

      when Range_Kind_T =>

         if Item.Min = Item.Max then
            -- Min and Max are the same expression, so
            -- thus must be an equality constraint.

            return
                 Image (Item.Kind)
               & Target
               & " = "
               & Image (Item.Min);  -- or Max, equal.

         else
            -- Min and Max are probably different expressions.

            return
                 Image (Item.Kind)
               & Image (Item.Min)
               & " <= "
               & Target
               & " <= "
               & Image (Item.Max);

         end if;

      end case;

   end Image;


   --
   ---   Abstract transfer functions
   --


   function Is_Initial_Value (
      Cell : Storage.Cell_T;
      From : Storage.Cell_T;
      Thru : Transfer_Function_T)
   return Boolean
   is

      Def : constant Expr_Ref :=
         Definition (Cell, Transfer_Function_T'Class (Thru));

   begin

      return   Def.Kind = Arithmetic.Cell
      and then Def.Cell = From;

   end Is_Initial_Value;


   function Is_Identical (
      Cell : Storage.Cell_T;
      Thru : Transfer_Function_T)
   return Boolean
   is
   begin

      return Is_Initial_Value (
         Cell => Cell,
         From => Cell,
         Thru => Transfer_Function_T'Class (Thru));

   end Is_Identical;


end Arithmetic;
