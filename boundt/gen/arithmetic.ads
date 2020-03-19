-- Arithmetic (decl)
--
-- Representation of integral arithmetic, including:
--
--   > Expressions with arithmetic and logical ops on variables and literals.
--   > Assignment of expressions to variables.
--   > Boolean conditions based on integer relations.
--   > Statically and dynamically accessed variables.
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
-- $Revision: 1.82 $
-- $Date: 2015/10/24 19:36:46 $
--
-- $Log: arithmetic.ads,v $
-- Revision 1.82  2015/10/24 19:36:46  niklas
-- Moved to free licence.
--
-- Revision 1.81  2014/06/20 21:19:16  niklas
-- Added Image (Width_T).
--
-- Revision 1.80  2013-02-19 09:13:12  niklas
-- BT-CH-0245 clean-up. Only descriptions changed.
--
-- Revision 1.79  2013-02-12 08:47:19  niklas
-- BT-CH-0245: Global volatile cells and "volatile" assertions.
--
-- Revision 1.78  2011-09-08 14:13:35  niklas
-- Added Natural and Positive subtypes of Value_T.
--
-- Revision 1.77  2011-09-08 08:45:09  niklas
-- Added Is_Small_Negative and Opposite_Sign for Interval_T.
-- Note: The latter function may be partially redundant with
-- the unary "-" operator for Word_Interval_T in Storage.Bounds.
--
-- Revision 1.76  2011-09-06 17:47:59  niklas
-- Added Magnitude_Rel_T and the arrays Opposite_Bit_Value
-- and Opposite_Rel, to help with ALF export.
-- Corrected the description of Minus_V.
--
-- Revision 1.75  2010-03-23 07:33:45  niklas
-- Added Truncate_Low and Truncate_High on Word_T.
--
-- Revision 1.74  2010-01-01 12:35:40  niklas
-- BT-CH-0209: Arithmetic.Binary allows Conc with mixed width.
--
-- Revision 1.73  2009-11-27 11:28:06  niklas
-- BT-CH-0184: Bit-widths, Word_T, failed modular analysis.
--
-- Revision 1.72  2009-10-07 19:26:09  niklas
-- BT-CH-0183: Cell-sets are a tagged-type class.
--
-- Revision 1.71  2009/05/15 12:16:47  niklas
-- BT-CH-0173: Handling cells used by Range_Post constraints.
--
-- Revision 1.70  2009/04/16 10:06:18  niklas
-- BT-CH-0170: Corrected Flow.Origins.Is_Identical to Is_Initial_Value.
--
-- Revision 1.69  2009/03/10 17:06:43  niklas
-- Added two functions Definition_Of to find and return the assignment
-- defining a given target cell within an effect or an assignment set.
-- Added the No_Such_Assignment exception to report failures.
-- Extended the binary "+" and "-" expression constructors to omit zero
-- terms and compute a constant result for constant operands. As a
-- result, the unary "-" constructor also does so.
--
-- Revision 1.68  2008/10/24 12:10:57  niklas
-- BT-CH-0152: Added function Is_Variable.
--
-- Revision 1.67  2008/10/18 20:14:59  niklas
-- Added function Defining_Assignments, for ALF export.
--
-- Revision 1.66  2008/06/29 06:13:51  niklas
-- Added convenience functions Is_Constant and Constant_Value, and
-- the related exception Expression_Not_Constant.
--
-- Revision 1.65  2008/06/20 10:11:53  niklas
-- BT-CH-0132: Data pointers using Difference (Expr, Cell, Bounds).
--
-- Revision 1.64  2008/06/18 20:52:55  niklas
-- BT-CH-0130: Data pointers relative to initial-value cells.
--
-- Revision 1.63  2008/04/18 12:11:31  niklas
-- Added function Is_Cell.
--
-- Revision 1.62  2007/10/31 12:16:00  niklas
-- BT-CH-0095: Arithmetic analysis of "live" dynamic data refs.
--
-- Revision 1.61  2007/10/28 09:32:44  niklas
-- BT-CH-0092: Arithmetic analysis of dynamic data refs is optional.
--
-- Revision 1.60  2007/10/02 20:37:58  niklas
-- BT-CH-0080: One-bit bitwise Boolean arithmetic operators.
--
-- Revision 1.59  2007/08/25 18:55:24  niklas
-- Added the abstract Transfer_Function_T operation Is_Identical
-- to show when a cell is definitely unchanged by the function.
--
-- Revision 1.58  2007/08/02 11:07:02  niklas
-- Added Image function for Assignment_Set_T.
--
-- Revision 1.57  2007/07/26 11:08:52  niklas
-- BT-CH-0066. Fracture assignments.
--
-- Revision 1.56  2007/07/21 18:18:40  niklas
-- BT-CH-0064. Support for AVR/IAR switch-handler analysis.
--
-- Revision 1.55  2007/01/13 13:51:01  niklas
-- BT-CH-0041.
--
-- Revision 1.54  2006/11/20 18:42:51  niklas
-- Added the null variable list, No_Variables.
-- Added procedure Veil for a single variable.
--
-- Revision 1.53  2006/10/24 08:44:29  niklas
-- BT-CH-0028.
--
-- Revision 1.52  2006/08/22 13:19:54  niklas
-- Extended function Cell to return Unknown for No_Cell.
-- Added description of the Unknown inputs and results for
-- arithmetic, boolean and bitwise operators.
--
-- Revision 1.51  2006/05/06 06:59:19  niklas
-- BT-CH-0021.
--
-- Revision 1.50  2006/04/28 09:25:41  niklas
-- Added a new primitive function Values (Expr, Bounds_T).
-- Improved the description primitive operations on Bounds_T
-- to note the redispatching.
--
-- Revision 1.49  2006/04/22 21:41:43  niklas
-- Added function Set (Flag Cell, Boolean).
--
-- Revision 1.48  2006/02/27 09:31:03  niklas
-- Added function Length (Expr_Ref).
--
-- Revision 1.47  2005/10/20 19:34:00  niklas
-- BT-CH-0016.
--
-- Revision 1.46  2005/09/12 19:02:57  niklas
-- BT-CH-0008.
--
-- Revision 1.45  2005/05/09 15:25:07  niklas
-- Added function Defining_Assignments.
--
-- Revision 1.44  2005/04/20 12:04:35  niklas
-- Added procedure Add_Reference_Bases.
--
-- Revision 1.43  2005/04/18 10:50:04  niklas
-- Added functions to check if expressions and assignments contain
-- (unresolved) dynamic memory references: Targets_Reference and
-- two forms of Uses_Reference, for expressions and assignments.
--
-- Revision 1.42  2005/03/24 18:09:20  niklas
-- Added Value_T as a renaming of Processor.Value_T.
-- Added function Sign (Value_T).
-- Added subtype Relation_Kind_T for the relational operators.
-- Added procedure Accumulate to create simplified affine expressions.
--
-- Revision 1.41  2005/03/21 22:21:19  niklas
-- Added Variable_List_T and procedure Veil.
--
-- Revision 1.40  2005/03/20 12:29:15  niklas
-- Added unary "-" expression constructor.
--
-- Revision 1.39  2005/03/19 19:51:16  niklas
-- Added function Set_Flag.
--
-- Revision 1.38  2005/02/16 21:11:37  niklas
-- BT-CH-0002.
--
-- Revision 1.37  2004/10/10 09:57:50  niklas
-- Include Bits1 in Word_Size_T.
--
-- Revision 1.36  2004/08/09 19:51:28  niklas
-- BT-CH-0001.
--
-- Revision 1.35  2004/04/25 14:19:22  niklas
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
-- Revision 1.34  2003/02/17 16:23:02  holsti
-- Added operators "<" and ">" to check if a value is less or greater
-- than the lower or upper limit given by a bound.
--
-- Revision 1.33  2001/05/20 13:23:20  holsti
-- Added Erase for Assignment_Set_T (NC_117).
--
-- Revision 1.32  2001/03/15 07:16:28  holsti
-- Intersection of cell-sets added.
--
-- Revision 1.31  2001/03/14 16:53:51  holsti
-- Unknown expression added, and propagates in operators.
--
-- Revision 1.30  2001/03/10 00:34:49  holsti
-- No_Cell_Set added.
-- Cell_Pair_T deleted.
-- Cell attributes Global and Const/Var deleted, and also all operations
-- related to them.
-- In Add_Cells_Used (By Assignment_T), the Added parameter is "in out".
--
-- Revision 1.29  2001/02/11 13:51:00  holsti
-- Editorial and coding style corrections.
--
-- Revision 1.28  2001/02/01 10:50:23  ville
-- Removed unused cells from live cells
--
-- Revision 1.27  2001/01/07 21:55:43  holsti
-- Cell-set operations added to support liveness analysis.
--
-- Revision 1.26  2000/12/29 13:20:00  holsti
-- Removed tbas etc. in favour of NCs.
--
-- Revision 1.25  2000/12/28 17:47:17  holsti
-- Card added (number of elements in cell-set).
--
-- Revision 1.24  2000/12/28 12:23:05  holsti
-- Single_Value and Multiple_Values added.
--
-- Revision 1.23  2000/12/21 10:12:58  sihvo
-- Added function Is_Defined.
--
-- Revision 1.22  2000/12/05 15:34:08  holsti
-- Deleted Stack_Pointer, Macro stuff. Added No_Cell.
--
-- Revision 1.21  2000/11/29 14:52:18  holsti
-- Added function Max for Limit_T, and "&" for Bound_T.
--
-- Revision 1.20  2000/11/29 09:11:10  holsti
-- Unbounded constant Bound_T added.
--
-- Revision 1.19  2000/11/24 12:06:00  sihvo
-- Added stack height analysis.
--
-- Revision 1.18  2000/11/23 13:06:25  saarinen
-- Added properties Counter and Global into Cell_Object_T
-- and functions to handle cells with these properties.
-- Added Image function for Cell_Set_T and Cell_List_T.
--
-- Revision 1.17  2000/10/19 10:19:43  saarinen
-- Added types Cell_Pair_T and Cell_Pair_List_T.
--
-- Revision 1.16  2000/10/06 13:24:29  saarinen
-- Added function Bounds_For_Cells.
--
-- Revision 1.15  2000/09/20 18:42:54  saarinen
-- Added function Constant_Cells.
--
-- Revision 1.14  2000/08/21 13:08:59  holsti
-- Avoided erroneous recursion in = for cell sets.
--
-- Revision 1.13  2000/08/20 21:02:26  holsti
-- Add_Cells_Used accepts null expression.
--
-- Revision 1.12  2000/08/04 08:23:08  saarinen
-- Added functions Empty for Cell_Bound_List_T,
-- Is_Empty for Cell_Set_T,and Known for Bound_T.
--
-- Revision 1.11  2000/07/24 22:28:20  holsti
-- Bound_T operators Void, "<=", "-" added.
--
-- Revision 1.10  2000/07/18 19:52:38  holsti
-- Min for Limit_T added.
--
-- Revision 1.9  2000/07/17 20:58:21  holsti
-- Added some Cell_Set_T and Limit_T operations.
--
-- Revision 1.8  2000/07/16 13:09:52  holsti
-- Getting the cells used/defined by expressions and effects.
--
-- Revision 1.7  2000/07/14 20:31:38  holsti
-- Type Cell_List_Ref added.
--
-- Revision 1.6  2000/07/12 12:25:30  holsti
-- Cell_Bound_T added. Step_Name and Get_Step_Cell deleted.
--
-- Revision 1.5  2000/06/29 14:06:35  holsti
-- Modified Bound_T (using Limit_T), add Floor & Ceil.
--
-- Revision 1.4  2000/06/28 13:39:25  holsti
-- Assignment_Set_T added.
--
-- Revision 1.3  2000/06/22 10:16:23  saarinen
-- Added constant Cell_T for Step Cell
--
-- Revision 1.2  2000/06/16 10:37:51  saarinen
--
-- Added const flag to Cell_Object_T with operations.
-- Implemented Cell_Set operations.
--
-- Revision 1.1  2000/06/11 19:03:12  holsti
-- Arithmetic package separated from Calculator.
--


with Arithmetic_Base;
with Bounded_Vectors;
with Processor;
with Storage;
with Storage.Bounds;
with Storage.References;

pragma Elaborate_All (Bounded_Vectors);


package Arithmetic is


   -- Contents:
   --
   -- > Operations on concrete integer numbers, signed and unsigned,
   --   with two's complement arithmetic. For example, whether an
   --   addition produces a carry bit.
   --
   -- > Operations on concrete bit-strings(binary words) of any
   --   length (up to a maximum, Max_Width). For example, left and
   --   right shifts and rotations.
   --
   -- > Expressions that combine the integer or boolean values of
   --   constants or variables (storage cells) using operations on
   --   integers or bit-strings, using a traditional tree structure
   --   where leaf nodes represents constants or variables and
   --   internal nodes represent operations applied to descendant
   --   sub-trees.
   --
   -- > Assignments where the value of an expression is assigned to
   --   a target (a storage cell). Sets of assignments to model the
   --   effect of an instruction.
   --
   -- > Transfer functions, an abstract data-type to represent the
   --   total effect of a set of paths between two points in a
   --   program.


   --
   ---   Abbreviations for frequently used Storage items:
   --


   subtype Cell_T       is Storage.Cell_T;
   subtype Cell_Set_T   is Storage.Cell_Set_T;
   subtype Cell_List_T  is Storage.Cell_List_T;


   --
   ---   Integer (signed) numbers
   --


   subtype Value_T is Arithmetic_Base.Value_T;
   --
   -- A renaming to partially hide the foreign source of this type.
   --
   use type Value_T;


   subtype Natural_Value_T is Value_T  range 0 .. Value_T'Base'Last;
   subtype Positive_Value_T is Value_T range 1 .. Value_T'Base'Last;
   --
   -- Sometimes we need values that are not negative, or
   -- positively larger than zero.


   function Image (Item : Value_T) return String
   renames Arithmetic_Base.Image;
   --
   -- The decimal representation without leading blanks but
   -- with leading '-' for a negative value.


   function Sign (Value : Value_T) return Value_T;
   --
   -- The usual -1, 0, +1 sign function.


   --
   ---   Binary (unsigned) bit-string words
   --


   Max_Width : constant := Arithmetic_Base.Max_Width;
   --
   -- The maximum bit-width supported; the maximum number
   -- of bits in a word.


   subtype Word_T is Arithmetic_Base.Word_T;
   --
   -- A renaming to partially hide the foreign source of this type.
   --
   use type Word_T;


   function Image (Item : Word_T) return String
   renames Arithmetic_Base.Image;


   function Shift_Right (Value : Word_T; Amount : Natural)
   return Word_T
   renames Arithmetic_Base.Shift_Right;


   function Shift_Left (Value : Word_T; Amount : Natural)
   return Word_T
   renames Arithmetic_Base.Shift_Left;


   subtype Bit_T is Word_T range 0 .. 1;
   --
   -- A 1-bit value.


   subtype Width_T is Arithmetic_Base.Width_T;
   --
   -- The number of bits in (the value of) an expression.


   function Image (Item : Width_T) return String;
   --
   -- The decimal representation without leading blanks.


   function Image (
      Item   : Word_T;
      Width  : Width_T;
      Signed : Boolean)
   return String;
   --
   -- The Item as an unsigned value. If the Item should be seen
   -- as a Signed number of the given Width, and this number is
   -- negative, the unsigned value is followed by the negative value
   -- in brackets; if the signed number is non-negative, the unsigned
   -- value may (optionally) be followed by "[+]".


   function Max_Word (Width : Width_T) return Word_T;
   --
   -- The maximum (all-ones) value of a binary word of the given Width.
   -- For example, Max_Word(8) = 255.


   function Mask (Word : Word_T) return Word_T;
   --
   -- The minimum "mask" word, consisting of all "1" bits in the low bits,
   -- that covers (is no less than) the given Word. The mask word is thus
   -- of the form 2**(n-1), where "n-1" is the position of the highest "1"
   -- in the given Word. For example, Mask(1) = 1, Mask(2) = Mask(3) = 3,
   -- Mask(4) = Mask(5) = Mask(6) = Mask(7) = 7. As a special case,
   -- Mask(0) = 0.


   Word_Out_Of_Range : exception;
   --
   -- Raised when a value, to be converted to a bit word, is
   -- out of range for the given size of the word.


   function Unsigned_Word (
      Value : Value_T;
      Width : Width_T)
   return Word_T;
   --
   -- Converts a general operand Value into a bit-word value of the given
   -- Width, converting negative operands by reinterpreting them as
   -- unsigned numbers. Thus, -1 becomes Max_Word(Width).
   --
   -- Raises Word_Out_Of_Range if the Value is not in the Word_T range.
   -- Masks off high-order bits if the Value is not in the range of
   -- the given width.


   function Unsigned_Value (
      Value : Value_T;
      Width : Width_T)
   return Value_T;
   --
   -- Same as Unsigned_Word (Value, Width) converted to Value_T.
   -- Raises Word_Out_Of_Range for any range/size problem.


   function Sign_Bit (
      Word  : Word_T;
      Width : Width_T)
   return Bit_T;
   --
   -- The sign bit (bit number "Width - 1") of the given Word.
   -- The result is 1 for a negative sign, 0 for non-negative.


   function Negative (
      Word  : Word_T;
      Width : Width_T)
   return Boolean;
   --
   -- Whether the Word represents a negative number, when interpreted
   -- in two's complement for the given Width.


   function Sign (Word : Word_T; Width : Width_T) return Value_T;
   --
   -- The usual -1, 0, +1 sign function, when the Word is seen as
   -- a two's complement number of the given Width.


   function Max_Positive (Width : Width_T) return Word_T;
   --
   -- The maximum value of a binary word of the given Width that
   -- represents a positive (non-negative) value, when interpreted
   -- in two's complement for the given Width.
   -- For example, Max_Positive (8) = 127.
   -- Max_Positive (1) is zero.


   function Min_Negative (Width : Width_T) return Value_T;
   --
   -- The value of a binary word of the given Width that represents
   -- the most negative (smallest) value, when the word is interpreted
   -- in two's complement for the given Width.
   -- For example, Min_Negative (8) = -128.
   -- Min_Negative (1) is -1.


   function Opposite_Sign (
      Word  : Word_T;
      Width : Width_T)
   return Word_T;
   --
   -- The word that represents the same absolute value as the
   -- given Word, but with the opposite sign, when both are viewed
   -- as two's complement numbers of the given Width. If the given
   -- Word is 2**(Width - 1), representing Min_Negative (Width), the
   -- result is the same (the positive value cannot be represented in
   -- this Width) without any indication of overflow.


   function Abs_Value (
      Word  : Word_T;
      Width : Width_T)
   return Word_T;
   --
   -- The absolute value of the Word, considered as a possibly
   -- negative two's complement value for the given Width. If the
   -- given Word is 2**(Width - 1), representing Min_Negative (Width),
   -- the result is the same (the positive value cannot be represented
   -- in this Width) without any indication of overflow.


   function Signed_Value (
      Word  : Word_T;
      Width : Width_T)
   return Value_T;
   --
   -- The signed interpretation of a binary Word, using two's complement
   -- representation for Width bits. Higher bits in the Word are
   -- ignored (masked out).
   --
   -- Raises Word_Out_Of_Range if the result is not in Value_T.


   function Mulu (Left, Right : Word_T; Width : Width_T)
   return Word_T;
   --
   -- The unsigned product of two unsigned factors.
   -- In case of overflow, the lowest Width bits are returned.


   function Muls (Left, Right : Word_T; Width : Width_T)
   return Word_T;
   --
   -- The signed product of two signed factors.
   -- In case of overflow, the lowest Width bits are returned.


   function Carry_From_Plus (Left, Right : Word_T; Width : Width_T)
   return Bit_T;
   --
   -- The carry bit from the addition Left + Right assuming the
   -- given Width. That is, bit number Width of the unbounded sum.


   function Negative_From_Plus (Left, Right : Word_T; Width : Width_T)
   return Bit_T;
   --
   -- The sign bit from the addition Left + Right assuming the
   -- given Width. That is, bit number Width - 1 of the sum.


   function Overflow_From_Plus (Left, Right : Word_T; Width : Width_T)
   return Bit_T;
   --
   -- The overflow bit from the addition Left + Right assuming the
   -- given Width. The overflow bit is zero iff the sum, computed with
   -- Width bits, accurately represents the sum of Left and Right
   -- in the signed (two's complement) view.


   function Borrow_From_Minus (Left, Right : Word_T; Width : Width_T)
   return Bit_T;
   --
   -- The borrow bit from the subtraction Left - Right assuming the
   -- given Width. That is, bit number Width of the unbounded
   -- subtraction.


   function Carry_From_Minus (Left, Right : Word_T; Width : Width_T)
   return Bit_T;
   --
   -- The carry bit from the subtraction Left - Right assuming the
   -- given Width. This is the complement of the Borrow bit,
   -- computed as "1 - Borrow" (numerical) or "not Borrow" (boolean).


   function Negative_From_Minus (Left, Right : Word_T; Width : Width_T)
   return Bit_T;
   --
   -- The sign bit from the subtraction Left - Right assuming the
   -- given Width. That is, bit number Width - 1 of the difference.


   function Overflow_From_Minus (Left, Right : Word_T; Width : Width_T)
   return Bit_T;
   --
   -- The overflow bit from the subtraction Left - Right assuming the
   -- given Width. The overflow bit is zero iff the difference, computed
   -- with Width bits, accurately represents the difference of Left
   -- and Right in the signed (two's complement) view.


   function Carry_From_Plus (
      Left, Right : Word_T;
      Carry       : Bit_T;
      Width       : Width_T)
   return Bit_T;
   --
   -- The carry bit from the addition Left + Right + Carry assuming the
   -- given Width. That is, bit number Width of the unbounded sum.


   function Negative_From_Plus (
      Left, Right : Word_T;
      Carry       : Bit_T;
      Width       : Width_T)
   return Bit_T;
   --
   -- The sign bit from the addition Left + Right + Carry assuming the
   -- given Width. That is, bit number Width - 1 of the sum.


   function Overflow_From_Plus (
      Left, Right : Word_T;
      Carry       : Bit_T;
      Width       : Width_T)
   return Bit_T;
   --
   -- The overflow bit from the addition Left + Right + Carry assuming the
   -- given Width. The overflow bit is zero iff the sum, computed with
   -- Width bits, accurately represents the sum of Left, Right, and
   -- Carry in the signed (two's complement) view.


   function Borrow_From_Minus (
      Left, Right : Word_T;
      Borrow      : Bit_T;
      Width       : Width_T)
   return Bit_T;
   --
   -- The borrow bit from the subtraction Left - Right - Borrow assuming
   -- the given Width. That is, bit number Width of the unbounded
   -- subtraction.


   function Carry_From_Minus (
      Left, Right : Word_T;
      Carry       : Bit_T;
      Width       : Width_T)
   return Bit_T;
   --
   -- The carry bit from the subtraction Left - Right - (1 - Carry)
   -- assuming the given Width. This is the complement of the Borrow bit,
   -- computed as "1 - Borrow" (numerical) or "not Borrow" (boolean).


   function Negative_From_Minus_Borrow (
      Left, Right : Word_T;
      Borrow      : Bit_T;
      Width       : Width_T)
   return Bit_T;
   --
   -- The sign bit from the subtraction Left - Right - Borrow assuming
   -- the given Width. That is, bit number Width - 1 of the difference.


   function Negative_From_Minus_Carry (
      Left, Right : Word_T;
      Carry       : Bit_T;
      Width       : Width_T)
   return Bit_T;
   --
   -- The sign bit from the subtraction Left - Right + Carry - 1 assuming
   -- the given Width. That is, bit number Width - 1 of the difference.


   function Overflow_From_Minus_Borrow (
      Left, Right : Word_T;
      Borrow      : Bit_T;
      Width       : Width_T)
   return Bit_T;
   --
   -- The overflow bit from the subtraction Left - Right - Borrow assuming
   -- the given Width. The overflow bit is zero iff the difference,
   -- computed with Width bits, accurately represents the difference
   -- of Left and Right, with Borrow, in the signed (two's complement) view.


   function Overflow_From_Minus_Carry (
      Left, Right : Word_T;
      Carry       : Bit_T;
      Width       : Width_T)
   return Bit_T;
   --
   -- The overflow bit from the subtraction Left - Right + Carry - 1
   -- assuming the given Width. The overflow bit is zero iff the
   -- difference, computed with Width bits, accurately represents the
   -- difference of Left and Right, with Carry, in the signed (two's
   -- complement) view.


   function Lts (Left, Right : Word_T; Width : Width_T)
   return Boolean;
   --
   -- Whether Left < Right when viewed as two's complement numbers
   -- of the given Width.


   function Gts (Left, Right : Word_T; Width : Width_T)
   return Boolean;
   --
   -- Whether Left > Right when viewed as two's complement numbers
   -- of the given Width.


   function Les (Left, Right : Word_T; Width : Width_T)
   return Boolean;
   --
   -- Whether Left <= Right when viewed as two's complement numbers
   -- of the given Width.


   function Ges (Left, Right : Word_T; Width : Width_T)
   return Boolean;
   --
   -- Whether Left >= Right when viewed as two's complement numbers
   -- of the given Width.


   function Shift_Left (
      Value  : Word_T;
      Amount : Natural;
      Width  : Width_T)
   return Word_T;
   --
   -- Like Interfaces.Shift_Left but for the given Width.
   -- Silently ignores excess high bits in the Value and
   -- returns a value within the Width.


   function Shift_Right (
      Value  : Word_T;
      Amount : Natural;
      Width  : Width_T)
   return Word_T;
   --
   -- Like Interfaces.Shift_Right but for the given Width.
   -- Silently ignores excess high bits in the Value and
   -- returns a value within the Width.


   function Shift_Right_Arithmetic (
      Value  : Word_T;
      Amount : Natural;
      Width  : Width_T)
   return Word_T;
   --
   -- Like Interfaces.Shift_Right_Arithmetic but for the given Width.
   -- Silently ignores excess high bits in the Value and
   -- returns a value within the Width.


   function Rotate_Left (
      Value  : Word_T;
      Amount : Natural;
      Width  : Width_T)
   return Word_T;
   --
   -- Like Interfaces.Rotate_Left but for the given Width.
   -- Silently ignores excess high bits in the Value and
   -- returns a value within the Width.


   function Rotate_Right (
      Value  : Word_T;
      Amount : Natural;
      Width  : Width_T)
   return Word_T;
   --
   -- Like Interfaces.Rotate_Left but for the given Width.
   -- Silently ignores excess high bits in the Value and
   -- returns a value within the Width.


   function Complement (
      Value : Word_T;
      Width : Width_T)
   return Word_T;
   --
   -- The logically complemented Value ("not"), but limited to
   -- the given Width.


   function Sign_Extend (
      Value : Word_T;
      From  : Width_T;
      To    : Width_T)
   return Word_T;
   --
   -- Extends the Value, From a given width, To a larger width,
   -- by setting the new bits to the sign bit of the Value.


   function Zero_Extend (
      Value : Word_T;
      From  : Width_T)
   return Word_T;
   --
   -- Extends the Value, From a given width, To a larger width,
   -- by setting the new bits to zero. The new width in fact has
   -- no effect, so it is not given as a parameter.


   function Truncate_Low (
      Value : Word_T;
      To    : Width_T)
   return Word_T;
   --
   -- Truncates the Value, To a given number of low-end bits, by
   -- clearing all higher bits.


   function Truncate_High (
      Value : Word_T;
      From  : Width_T;
      To    : Width_T)
   return Word_T;
   --
   -- Truncates the Value, From a given number of bits To a given
   -- (smaller) number of bits, by taking the "To" high-end bits
   -- and shifting them down to the low end, clearing all other
   -- bits in the result.


   --
   ---   Expressions
   --


   type Expr_T;
   --
   -- An arithmetical, boolean, or bit-word expression of known
   -- width, involving processor cells as input variables, typical
   -- operations on finite-width binary integers or words, and the
   -- memory (de-referencing (indirect addressing) operation.
   --
   -- Integer operations assume two's complement representation of
   -- known width. Overflow, underflow, and carry/borrow can be
   -- modelled or ignored. Ternary operations are included to model
   -- "with-carry/borrow" addition and subtraction.
   --
   -- Boolean expressions are formed by integer relational operations
   -- ("=", "<", ">", "<=", ">=") applied to known-width integers and
   -- using either signed or unsigned magnitudes, or by boolean
   -- operations (and, or, not) applied to boolean subexpressions.
   -- Boolean expressions are considered to have the values False or
   -- True, which correspond to bit-values 0 and 1.
   --
   -- Bit operations are assumed to be performed on binary word
   -- representations of known width.
   --
   -- Each Expr_T has a "width" component that sets the number of bits
   -- in the result (value) of the expression. The width of the operands
   -- is defined by their "width" components, but for most operations
   -- the operands and the result have the same width.
   --
   -- Most operations are modular in the sense that the (concrete)
   -- result in the modelled machine consists of the given number
   -- (width) of the less-significant bits of the result, while the
   -- excess bits are discarded (or enter some carry or overflow flags,
   -- possibly modelled separately).
   --
   -- A special case is an expression that represents an unknown
   -- (opaque) value, that is, a value that is not modelled.
   --
   -- The set of operands and operations is intended to allow exact
   -- modelling (simulation) of the computations in most binary, two's
   -- complement processors. Various approximations, assumptions, or
   -- restrictions may have to be used to translate such expressions
   -- to more symbolic models, for example Presburger Arithmetic models.
   -- The only concession to such translations in Expr_T is that a
   -- constant integer operand can be flagged (hinted) as a signed value,
   -- to be translated to a signed constant in analyses that cannot
   -- model unsigned (modular) arithmetic.
   --
   -- If an expression uses (reads) the values of a volatile cell more
   -- than once, that is, if the same volatile cell appears as more than
   -- one source operand in some expression in the Expr_T, then we assume
   -- that all uses of this cell represent the same value. In other words,
   -- we assume that the volatile cell is read only once, when this Expr_T
   -- is evaluated, or (equivalently) that the value of the cell does not
   -- change during the evaluation of the Expr_T. This allows us to apply
   -- simplifications such as x-x = 0 even when x is a volatile cell.


   type Expr_Ref is access Expr_T;
   --
   -- Epressions are constructed as a tree structure in the
   -- usual fashion.


   subtype Condition_T is Expr_Ref;
   --
   -- A boolean expression.


   subtype Variable_T is Expr_Ref;
   --
   -- An expression that refers to a variable, either statically
   -- addressed (an expression that consists just of a known Cell_T)
   -- or dynamically addressed (an expression with "memory reference"
   -- as the root operator). Sometimes the "variable" may in fact be
   -- a constant, for example when a ROM location is read.


   function Is_Variable (Item : Expr_Ref) return Boolean;
   --
   -- Whether this expression consists of a single variable.
   -- The Unknown expression is not considered a variable.


   function Is_Cell (Cell : Cell_T; Expr : Expr_Ref)
   return Boolean;
   --
   -- Whether the Expr is the given given Cell, and nothing more.


   Expression_Not_Constant : exception;
   --
   -- Signals an attempt to get the constant value of a non-constant
   -- expression. See function Constant_Value.


   function Is_Constant (Item : Expr_Ref) return Boolean;
   --
   -- Whether the expression is just a constant value.


   function Is_Negative_Constant (Item : Expr_Ref) return Boolean;
   --
   -- Whether the expression is just a constant value, and this
   -- constant is negative in the two's complement view.
   --
   -- The "Signed" flag on the constant is not used, only the
   -- actual (word) value of the constant.


   function Unsigned_Value (Item : Expr_Ref) return Word_T;
   --
   -- The constant value of the expression, assuming Is_Constant (Item),
   -- otherwise propagates Expression_Not_Constant.


   function Signed_Value (Item : Expr_Ref) return Value_T;
   --
   -- The signed value of the expression, assuming Is_Constant (Item),
   -- otherwise propagates Expression_Not_Constant.
   --
   -- The "Signed" flag on the constant is not used, only the
   -- actual (word) value of the constant.


   function Const_Value (Item : Expr_Ref) return Value_T;
   --
   -- The Unsigned or Signed value of the expression, depending
   -- on Item.Signed and assuming Is_Constant (Item), otherwise
   -- propagates Expression_Not_Constant.


   function Width_Of (Item : Expr_Ref) return Width_T;
   --
   -- The width of the expression's value.


   type Variable_List_T is array (Positive range <>) of Variable_T;
   --
   -- A list of variables.


   No_Variables : constant Variable_List_T := (1 .. 0 => null);
   --
   -- A list of no variables at all.


   function To_Variables (Cells : Cell_List_T)
   return Variable_List_T;
   --
   -- The list of variables corresponding to the given list of Cells.


   No_Expr : constant Expr_Ref;
   --
   -- Represents the absence of an expression. Note that this is
   -- not the same as an expression that yields an unknown value
   -- (see Unknown below).


   Always : constant Condition_T;
   --
   -- Represents the boolean expression "True".


   Never : constant Condition_T;
   --
   -- Represents the boolean expression "False".


   Unknown : constant Expr_Ref;
   --
   -- Represents an unknown (opaque) value. This should be the
   -- unique and only representation of opaque values. Note that
   -- this is not the same as No_Expr (see above).
   --
   -- "Unknown" may or may not be null; clients shall not dereference
   -- it. "Unknown" does not have a definite width; it represents an
   -- unknown value of any desired width.


   --
   ---   Expression structure
   --


   type Expr_Kind_T is (
      Opaque,         -- An unknown value.
      Const,          -- A literal.
      Cell,           -- A statically addressed variable.
      Ref,            -- A dynamically addressed variable, that is, the value
                      -- of a memory location at an address that is computed
                      -- dynamically.
      Unary_Kind,     -- An operation (function) of one operand.
      Binary_Kind,    -- An operation (function) of two operands.
      Ternary_Kind);  -- An operation (function) of three operands.
   --
   -- The major kinds of expression.
   --
   -- Clients of this package should never create expressions
   -- of the Opaque kind, but always use instead the constant
   -- Unknown defined above.


   subtype Variable_Kind_T is Expr_Kind_T range Cell .. Ref;
   --
   -- The subset of Expr_Kind_T that represents a variable, whether
   -- statically (Cell) or dynamically (Ref) addressed.


   type Unary_Op_T is (
      Notw,      -- Bitwise negation of binary words.
      Notx,      -- Boolean negation of a subexpression.
      EqZero,    -- Whether the operand is zero.
      EqOne,     -- Whether the operand is one (1).
      Signw,     -- The sign bit (bit number Width - 1) of a value.
      Exts,      -- Sign-extend a value to a larger width.
      Extz,      -- Zero-extend a value to a larger width.
      Trunl,     -- Truncate a value, taking the low bits.
      Trunh);    -- Truncate a value, taking the high bits.
   --
   -- The unary (one-operand) operators that can occur in expressions.


   type Binary_Op_T is (

      -- Two-operand (binary) arithmetic operators:

      Plus,      -- The sum of two integral operands.
      Minus,     -- The difference of two integral operands.
      Mulu,      -- The product of two unsigned integral operands.
      Muls,      -- The product of two signed integral operands.

      -- Two-operand (binary) predicates:

      Plus_C,    -- The Carry bit from a Plus operation.
      Plus_N,    -- The Negative (sign) bit from a Plus operation.
      Plus_V,    -- The oVerflow bit from a Plus operation.
      Minus_B,   -- The Borrow bit from a Minus operation.
      Minus_C,   -- The Carry bit (1 - Borrow) from a Minus operation.
      Minus_N,   -- The Negative (sign) bit from a Minus operation.
      Minus_V,   -- The oVerflow bit from a Minus operation.

      Eq,        -- The "=" relation (signed and unsigned).

      Lts,       -- The "<" relation between two signed quantities.
      Gts,       --     ">"
      Les,       --     "<="
      Ges,       --     ">="

      Ltu,       -- The "<" relation between two unsigned quantities.
      Gtu,       --     ">"
      Leu,       --     "<="
      Geu,       --     ">="

      Andx,      -- Boolean conjunction of two subexpressions.
      Orx,       -- Boolean disjunction ditto.

      -- Two-operand bitwise operators:

      Andw,      -- Bitwise conjunction of binary words.
      Orw ,      -- Bitwise disjunction of binary words.
      Xorw,      -- Bitwise exclusive-or of binary words.

      Slz,       -- Shift  op1 left  op2 bits, zero fill.
      Srz,       -- Shift  op1 right op2 bits, zero fill.
      Sra,       -- Shift  op1 right op2 bits, sign fill.
      Rotl,      -- Rotate op1 left  op2 bits.
      Rotr,      -- Rotate op1 right op2 bits.

      Conc);     -- Concatenation of op1 (high bits) with op2 (low bits).
   --
   -- The binary (two-operand) operators that can occur in expressions.
   --
   -- For subtyping reasons, the above list should be kept in the
   -- following order:
   --
   -- - Integer operators
   -- - Boolean operators (predicates)
   -- - Bit-word operators.


   type Ternary_Op_T is (

      -- Three-operand (ternary) arithmetic operators:

      Plus,         -- The sum of two integral operands and a carry bit.
      Minus_With_B, -- The diff of two integral operands and a borrow bit.
      Minus_With_C, -- The diff of two integral operands and a carry bit.

      -- Three-operand (ternary) predicates:

      Plus_C,       -- The Carry bit from a Plus operation.
      Plus_N,       -- The Negative bit from a Plus operation.
      Plus_V,       -- The oVerflow bit from a Plus operation.
      Minus_B,      -- The Borrow bit from a Minus_With_B operation.
      Minus_C,      -- The Carry bit from a Minus_With_C operation.
      Minus_BN,     -- The Negative bit from a Minus_With_B operation.
      Minus_CN,     -- The Negative bit from a Minus_With_C operation.
      Minus_BV,     -- The oVerflow bit from a Minus_With_B operation.
      Minus_CV);    -- The oVerflow bit from a Minus_With_C operation.

   -- The ternary (three-operand) operators that can occur in expressions.

   -- A note on the use of both "carry" and "borrow" for subtractions:
   -- Processors differ on how they use the carry bits for subtractions.
   -- We use the terms borrow and carry as follows:
   --
   -- > If X-Y sets   the flag when X<Y, it is a "borrow" flag.
   -- > If X-Y clears the flag when X<Y, it is a "carry"  flag.
   --
   -- For example, the 8051 processors use the C flag as "borrow" from
   -- a subtraction, while ARM7 uses the C flag as "carry". The two
   -- interpretations are simply logical complements; when the carry
   -- is set the borrow is clear and vice versa.
   --
   -- The "borrow/carry" view is visible also in chained subtractions.
   -- After the subtraction of the less-significant parts X0-Y0
   -- results in a borrow flag B, the next more significant parts
   -- are subtracted as X1-Y1-B. This is the Minus_With_B operation.
   -- In contrast, in the "carry" view, if X0-Y0 results in the carry
   -- flag C, the next more significant subtraction is X1-Y1+C-1 which
   -- is the same as X1-Y1-(1-C). This is the Minus_With_C operation.
   --
   -- Note that the functions that yield a Borrow (resp. a Carry) from
   -- a three-operand subtraction assume that the third operand is a
   -- "borrow" (resp. "carry") flag. That is, the functions differ not
   -- only in their results, but also in the role of the third input
   -- operand.
   --
   -- The simple logical correlation between "carry" and "borrow" of
   -- course means that having both in the model is redundant. However,
   -- it alerts the programmer to the difference, and may simplify the
   -- translation to certain forms of analysis.


   subtype Unary_Eq_Bit_T is Unary_Op_T range EqZero .. EqOne;
   --
   -- The unary operators that compare an operand to a 1-bit constant.


   Opposite_Bit_Value : constant array (Unary_Eq_Bit_T) of Unary_Eq_Bit_T := (
      EqZero => EqOne,
      EqOne  => EqZero);
   --
   -- The opposite value/condition of a 1-bit operand.


   subtype Binary_Predicate_Op_T is Binary_Op_T range Plus_C .. Geu;
   --
   -- The subset of binary operators that represents Boolean-valued
   -- predicates.


   subtype Ternary_Predicate_Op_T is Ternary_Op_T range Plus_C .. Minus_CV;
   --
   -- The subset of ternary operators that represents Boolean-valued
   -- predicates.


   subtype Relation_Op_T is Binary_Op_T range Eq .. Geu;
   --
   -- The subset of binary operators that represents Boolean-valued
   -- relational operators.


   subtype Magnitude_Rel_T is Relation_Op_T range Lts .. Geu;
   --
   -- The subset of relational operators that omits Eq (equality).


   Opposite_Rel : constant array (Magnitude_Rel_T) of Magnitude_Rel_T := (
      Lts => Ges,
      Gts => Les,
      Les => Gts,
      Ges => Lts,
      Ltu => Geu,
      Gtu => Leu,
      Leu => Gtu,
      Geu => Ltu);
   --
   -- The opposite relational comparison. For any op in Magnitude_Rel_T,
   -- the conditions (x op y) and (x Opposite_Rel (op) y) are
   -- logical complements.


   subtype Binary_Boolean_Op_T is Binary_Op_T range Plus_C .. Orx;
   --
   -- The subset of binary operators that yield a Boolean value.


   subtype Ternary_Boolean_Op_T is Ternary_Op_T range Plus_C .. Minus_CV;
   --
   -- The subset of ternary operators that yield a Boolean value.


   subtype Binary_Bitwise_Op_T is Binary_Op_T range Andw .. Xorw;
   --
   -- The subset of binary operators that work bitwise on words.


   subtype Shift_Rotate_Op_T is Binary_Op_T range Slz .. Rotr;
   --
   -- The subset of binary operators that represents shift
   -- and rotation operators.


   subtype Equal_Width_Op_T is Binary_Op_T range Plus .. Xorw;
   --
   -- The subset of binary operators in which the left and right
   -- operand must have the same width. For the other operators
   -- (which are the Shift_Rotate_Op_T and Conc) the left and
   -- right operands can have quite different widths.


   type Expr_T (Kind : Expr_Kind_T := Opaque) is record

      Width : Width_T;

      case Kind is

      when Opaque =>

         null;

      when Cell =>

         Cell : Cell_T;

      when Const =>

         Value  : Word_T;
         Signed : Boolean;

      when Ref =>

         Ref : Storage.References.Boundable_Ref;

      when Unary_Kind =>

         Unary : Unary_Op_T;
         Expr  : Expr_Ref;

      when Binary_Kind =>

         Binary : Binary_Op_T;
         L_Expr : Expr_Ref;
         R_Expr : Expr_Ref;

      when Ternary_Kind =>

         Ternary : Ternary_Op_T;
         L3_Expr : Expr_Ref;
         R3_Expr : Expr_Ref;
         C3_Expr : Expr_Ref;

      end case;

   end record;
   --
   -- An expression object.
   --
   -- Expressions are represented in a tree structure in the
   -- traditional way, where a leaf node is a primitive operand
   -- (constant, cell, memory reference, or unknown) and an internal
   -- node is a unary, binary, or ternary operator, with the operand
   -- or operands as children.
   --
   -- The Width defines the number of bits in the result (value)
   -- of the expression. The operands (Expr, L_Expr, R_Expr, L3_Expr,
   -- R3_Expr, C3_Expr) have their own Widths.
   --
   -- The Value of a Const expression is usually the unsigned view
   -- of the result with Width bits. Higher bits in the Value are
   -- zero. Negative values are represented in two's complement, so a
   -- value is negative iff bit "Width - 1" is 1, but higher bits
   -- are still zero (that is, the value is not represented in
   -- sign-extended form). The Signed component is a hint, for some
   -- analyses (in particular the Omega Calculator) that the signed
   -- view should be used for this constant; it has an effect only
   -- for negative Values.
   --
   -- Expression nodes are allocated from the heap. We currently
   -- freely reuse and share nodes and subtrees between different
   -- expressions. Therefore, the components of an Expr_T should
   -- never be updated in place. (A future implementation may use
   -- a shared pool of expression nodes, with hashed access.)


   subtype Const_Expr_T   is Expr_T (Kind => Const       );
   subtype Cell_Expr_T    is Expr_T (Kind => Cell        );
   subtype Ref_Expr_T     is Expr_T (Kind => Ref         );
   subtype Unary_Expr_T   is Expr_T (Kind => Unary_Kind  );
   subtype Binary_Expr_T  is Expr_T (Kind => Binary_Kind );
   subtype Ternary_Expr_T is Expr_T (Kind => Ternary_Kind);
   --
   -- Some interesting subtypes of Expr_T.


   --
   ---   Constructing expressions
   --


   -- Primary expressions:


   function Cell (Item : Cell_T) return Expr_Ref;
   --
   -- The expression that consists of just the given Cell. The value
   -- of the expression is the value of the Cell, when the Cell is
   -- not Storage.No_Cell. This expression can also stand as the Target
   -- in an Assignment, which means that a new value is assigned to
   -- the cell. The width of the expression is the width of the Cell.
   --
   -- When Cell = Storage.No_Cell, the function returns an opaque
   -- expression (Unknown).


   function Cell (Spec : Processor.Cell_Spec_T) return Expr_Ref;
   --
   -- Short for Cell (Storage.Cell (Spec)).


   function Expr (C : Cell_T) return Expr_Ref
   renames Cell;
   --
   -- Obsolete (deprecated) name for the function that turns a
   -- cell into an expression.


   function Const (
      Value  : Word_T;
      Width  : Width_T;
      Signed : Boolean)
   return Expr_Ref;
   --
   -- The expression that consists of the given constant Value
   -- considered to have the given Width, with perhaps a hint
   -- that the value should be viewed as Signed, when this is
   -- important for the analysis.


   function Const (
      Value  : Value_T;
      Width  : Width_T;
      Signed : Boolean)
   return Expr_Ref;
   --
   -- The expression that consists of the given constant Value
   -- considered to have the given Width, with perhaps a hint
   -- that the value should be viewed as Signed, when this is
   -- important for the analysis. Note that a negative Value
   -- does not imply a signed view. The Signed flag has no
   -- effect for a non-negative Value.


   function Zero (Width : Width_T) return Expr_Ref;
   --
   -- The constant zero as an expression.


   function One (Width : Width_T) return Expr_Ref;
   --
   -- The constant one as an expression.


   Zero_Bit : constant Expr_Ref;
   One_Bit  : constant Expr_Ref;
   --
   -- The constants zero and one, as 1-bit expressions.


   function Minus_One (Width : Width_T) return Expr_Ref;
   --
   -- The constant minus-one as an expression.


   -- Reference to variable in memory:


   function Reference (To : Storage.References.Boundable_Ref)
   return Expr_Ref;
   --
   -- The expression that consists of the cell To which a dynamic
   -- reference refers. The value of the expression is the value of
   -- the referenced cell, Referent (To). Even if the identity if the
   -- cell is yet unknown, its width is known, being To.Width.
   --
   -- This expression can also stand as the Target in an Assignment,
   -- which means that a new value is assigned to the referenced cell.


   -- Arithmetic additive operators:
   --
   -- The binary forms (no carry input) are declared as Ada operators,
   -- eg. "+", and expect their two operands to have the same width,
   -- and create expressions with this same width. So the width is
   -- "sticky" and is initially set at the Const and Variable leaves.
   --
   -- The ternary forms (with carry input) are declared as Ada functions
   -- with main operands Left) and Right) of equal width and the Carry
   -- or Borrow operand expected to be a single bit (width = 1).
   -- The sum or difference has the same width as the main operands Left
   -- and Right.
   --
   -- All the operators return the Unknown expression if one or several
   -- of the operands is Unknown. Some operators propagate constants or
   -- do special things for special constants, for example adding zero
   -- to a non-constant operand returns the non-constant operand unchanged.


   function "+" (L, R : Expr_Ref) return Expr_Ref;
   --
   -- If either operand is constant zero, the result is the other operand.
   -- If both operands are constants the result is their constant sum.
   -- If one operand is a large unsigned constant that represents a small
   -- negative constant, the result may (optionally) be a subtraction of
   -- that small constant from the other operand.

   function "-" (L, R : Expr_Ref) return Expr_Ref;
   --
   -- If the R operand is zero, the result is the L operand.
   -- If both operands are constants the result is their constant difference.
   -- If the R operand is a large unsigned constant that represents a small
   -- negative constant, the result may (optionally) be an addition of
   -- that small constant to the L operand.

   function "-" (E : Expr_Ref) return Expr_Ref;
   --
   -- If the operand is a constant the result is the negated constant.

   function Plus_With_Carry  (Left, Right, Carry : Expr_Ref)
   return Expr_Ref;

   function Minus_With_Borrow (Left, Right, Borrow : Expr_Ref)
   return Expr_Ref;

   function Minus_With_Carry (Left, Right, Carry : Expr_Ref)
   return Expr_Ref;


   -- Arithmetic multiplicative operators:
   --
   -- These cannot be declared as the operator "*" because we
   -- separate between unsigned and signed operations.


   function Mulu (L, R : Expr_Ref) return Expr_Ref;
   --
   -- Multiplication of two unsigned operands giving an unsigned result.


   function Muls (L, R : Expr_Ref) return Expr_Ref;
   --
   -- Multiplication of two signed operands giving a signed result.


   procedure Accumulate (
      Factor : in     Value_T;
      Expr   : in     Expr_Ref;
      Sum    : in out Expr_Ref);
   --
   -- Sets Sum to Sum + Factor * Expr, trying to simplify the
   -- expression by considering special cases:
   --
   -- > Factor = 0, +1 or -1.
   -- > Sum = 0 initially.
   --
   -- The initial width of Sum sets the width of the addition and
   -- multiplication and of the Factor value.


   -- Predicates from integer operators:
   --
   -- The binary operators (no carry input) expect operands of equal
   -- width and create an expression of width 1 bit.
   --
   -- The ternary forms (with carry input) expect two main operands
   -- L(eft) and R(ight) of equal width. The third C(arry) operand
   -- is expected to be a single bit (width = 1).
   --
   -- If any operand is Unknown the result is also Unknown.


   function Carry_From_Plus (Left, Right : Expr_Ref) return Expr_Ref;
   --
   -- The carry bit from the addition Left + Right, where Left
   -- and Right are viewed as unsigned words. The operands shall
   -- have the same width; the result is of width 1 bit.


   function Negative_From_Plus (Left, Right : Expr_Ref) return Expr_Ref;
   --
   -- The sign bit of the result of addition Left + Right. This is
   -- the most significant bit (bit number "width - 1") in the
   -- sum of the (unsigned) Left and Right words.


   function Overflow_From_Plus (Left, Right : Expr_Ref) return Expr_Ref;
   --
   -- The overflow bit from the addition Left + Right, where Left
   -- and Right are viewed as signed words, defined as follows:
   -- > If Left and Right have different signs, the overflow is zero.
   -- > If Left and Right have the same sign:
   --    o  if the sum has the same sign the overflow is zero.
   --    o  if the sum has the opposite sign the overflow is one.


   function Borrow_From_Minus (Left, Right : Expr_Ref) return Expr_Ref;
   --
   -- The borrow bit from the subtraction Left - Right, where Left
   -- and Right are viewed as unsigned words.
   --
   -- The borrow bit is defined as the complement of the carry from
   -- the addition Left + Right' where Right' is the two's complement
   -- of Right (the carry from the two's complement operation is ignored).


   function Carry_From_Minus (Left, Right : Expr_Ref) return Expr_Ref;
   --
   -- The carry bit from the subtraction Left - Right, where Left
   -- and Right are viewed as unsigned words.
   --
   -- The carry bit is defined as the carry from the addition
   -- Left + Right' where Right' is the two's complement of Right
   -- (the carry from the two's complement operation is ignored).
   -- This is the complement of the Borrow bit from the same addition.


   function Negative_From_Minus (Left, Right : Expr_Ref) return Expr_Ref;
   --
   -- The sign bit of the result of subtraction Left - Right. This is
   -- the most significant bit (bit number "width - 1") in the
   -- sum of the (unsigned) words Left and Right', where Right' is the
   -- two's complement of Right.


   function Overflow_From_Minus (Left, Right : Expr_Ref) return Expr_Ref;
   --
   -- The overflow bit from the subtraction Left - Right, where Left
   -- and Right are viewed as signed words. The operands shall have
   -- the same width; the result is of width 1 bit.
   --
   -- The overflow bit is defined as the overflow from the addition
   -- Left + Right' where Right' is the two's complement of Right.


   function Carry_From_Plus (Left, Right, Carry : Expr_Ref)
   return Expr_Ref;
   --
   -- The carry bit from the addition Left + Right + Carry, where all
   -- operands are viewed as unsigned words. The Left and Right operands
   -- shall have the same width; the Carry operand shall have width 1;
   -- and the result is of width 1 bit.


   function Negative_From_Plus (Left, Right, Carry : Expr_Ref)
   return Expr_Ref;
   --
   -- The sign bit of the result of addition Left + Right + Carry. This is
   -- the most significant bit (bit number "width - 1") in the
   -- sum of the (unsigned) Left and Right words and the Carry bit.


   function Overflow_From_Plus (Left, Right, Carry : Expr_Ref)
   return Expr_Ref;
   --
   -- The overflow bit from the addition Left + Right + Carry, where all
   -- operands are viewed as signed words. The overflow bit is zero
   -- if the result of the addition can be represented in the same width
   -- as the Left and Right operands and one otherwise. The Carry
   -- operand shall be a single bit (width 1).


   function Borrow_From_Minus (Left, Right, Borrow : Expr_Ref)
   return Expr_Ref;
   --
   -- The borrow bit from the subtraction Left - Right - Borrow,
   -- where all operands are viewed as unsigned words; Left and Right
   -- have the same width; and Borrow has width 1.
   --
   -- The borrow bit is defined as the complement of the carry
   -- from the addition Left + Right' + Borrow' where the aprostrophe
   -- represents two's complement (the carry from the two's complement
   -- operation is ignored).


   function Carry_From_Minus (Left, Right, Carry : Expr_Ref)
   return Expr_Ref;
   --
   -- The carry bit from the subtraction Left - Right - (1 - Carry),
   -- where all operands are viewed as unsigned words; Left and Right
   -- have the same width; and Carry has width 1.
   --
   -- The carry bit is the complement of the borrow bit from the same
   -- subtraction.


   function Negative_From_Minus_Borrow (Left, Right, Borrow : Expr_Ref)
   return Expr_Ref;
   --
   -- The sign bit of the result of subtraction Left - Right - Borrow.
   -- This is the most significant bit (bit number "width - 1") in the
   -- sum Left + (Right + Borrow)', where the term in parentheses is
   -- the two's complement of Right + Borrow. The Left and Right
   -- operands shall have the same width; the Borrow operand shall be
   -- one bit; and the result is of width 1 bit.


   function Negative_From_Minus_Carry (Left, Right, Carry : Expr_Ref)
   return Expr_Ref;
   --
   -- The sign bit of the result of subtraction Left - Right + Carry - 1.
   -- The Left and Right operands shall have the same width; the Carry
   -- operand shall be one bit; and the result is of width 1 bit.


   function Overflow_From_Minus_Borrow (Left, Right, Borrow : Expr_Ref)
   return Expr_Ref;
   --
   -- The overflow bit from the subtraction Left - Right - Borrow, where
   -- Left and Right are viewed as signed words. The Left and Right
   -- operands shall have the same width; the Borrow operand shall be
   -- one bit; and the result is of width 1 bit.
   --
   -- The overflow bit is defined as the overflow from the addition
   -- Left + (Right + Borrow)' where the term in parentheses is the
   -- two's complement of Right + Borrow.


   function Overflow_From_Minus_Carry (Left, Right, Carry : Expr_Ref)
   return Expr_Ref;
   --
   -- The overflow bit from the subtraction Left - Right + Carry - 1,
   -- where Left and Right are viewed as signed words. The Left and Right
   -- operands shall have the same width; the Carry operand shall be
   -- one bit; and the result is of width 1 bit.


   -- Equality operators:


   function "="  (L, R : Expr_Ref) return Expr_Ref;
   --
   -- Assumes that L and R have the same width (or are Unknown).
   -- Creates an Eq expression of width 1 bit.
   -- Returns Unknown if either or both operands are Unknown.


   function "=" (L : Expr_Ref; R : Word_T) return Expr_Ref;
   --
   -- Same as "=" (L, Const (R, L.Width, False)), that is, an
   -- equality comparison of L to the unsigned value R.


   function "=" (L : Expr_Ref; R : Value_T) return Expr_Ref;
   --
   -- Same as "=" (L, Const (Unsigned_Word (R, L.Width), L.Width, True)),
   -- that is, an equality comparison of L to the signed value R.


   -- Relational operators for signed and unsigned views:
   --
   -- These operators expect their two operands to have the same
   -- width (or be Unknown), and create expressions of width 1 bit.
   --
   -- All the operators return the Unknown expression if either or
   -- both of the operands is Unknown.


   function Lts (L, R : Expr_Ref) return Expr_Ref;
   function Gts (L, R : Expr_Ref) return Expr_Ref;
   function Les (L, R : Expr_Ref) return Expr_Ref;
   function Ges (L, R : Expr_Ref) return Expr_Ref;

   function Ltu (L, R : Expr_Ref) return Expr_Ref;
   function Gtu (L, R : Expr_Ref) return Expr_Ref;
   function Leu (L, R : Expr_Ref) return Expr_Ref;
   function Geu (L, R : Expr_Ref) return Expr_Ref;


   -- Boolean operators:
   --
   -- All the operators expect their operands to be 1 bit wide (or
   -- Unknown) and also create expressions of width 1 bit.
   --
   -- All the operators return the Unknown expression if either or
   -- both of the operands is Unknown. Furthermore, the "and" and
   -- "or" operators check for irrelevant operands (Always or Never)
   -- and return the appropriate simpler expression.


   function "and" (L, R : Expr_Ref) return Expr_Ref;
   --
   -- This conjunction operator has the following special cases:
   --    > if one operand is Always, the result is the other operand.
   --    > if one operand is Never , the result is Never.


   function "or"  (L, R : Expr_Ref) return Expr_Ref;
   --
   -- This disjunction operator has the following special cases:
   --    > if one operand is Always, the result is Always.
   --    > if one operand is Never , the result is the other operand.


   function "not" (E : Expr_Ref) return Expr_Ref;
   --
   -- This negation operator has the following special cases:
   --
   --   not Always  = Never
   --   not Never   = Always
   --   not Unknown = Unknown.


   -- Bit-wise operators:
   --
   -- These operators expect their operands (if more than one) to have
   -- the same width and create an expression with this width.
   --
   -- All the operators return the Unknown expression if either or
   -- both of the operands is Unknown.


   function Andw (L, R : Expr_Ref) return Expr_Ref;
   --
   -- Bitwise conjunction. Special cases:
   -- >  If L or R is zero, the result is zero.
   -- >  If L is all-ones, the result is R.
   -- >  If R is all-ones, the result is L.


   function Orw  (L, R : Expr_Ref) return Expr_Ref;
   --
   -- Bitwise conjunction. Special cases:
   -- >  If L or R is all-ones, the result is all-ones.
   -- >  If L is zero, the result is R.
   -- >  If R is zero, the result is L.


   function Xorw (L, R : Expr_Ref) return Expr_Ref;
   --
   -- Bitwise exclusive disjunction. Special cases:
   -- >  If L is zero, the result is R.
   -- >  If R is zero, the result is L.
   -- >  If L is all-ones, the result is "notw R".
   -- >  If R is all-ones, the result is "notw L".


   function Notw (E : Expr_Ref) return Expr_Ref;
   --
   -- Bitwise negation or logical complement.
   -- If E is a constant, the result is the complemented constant.


   -- Shift and rotate operators:
   --
   -- In these operators, the left operand is the word to be shifted
   -- or rotated and the right operand is the unsigned amount (number
   -- of bit positions) of shift or rotation. The result expression
   -- has the same width as the left operand. If either operand is
   -- Unknown, the result is Unknown. If both operands are constants,
   -- the result is also a constant.


   function Slz  (L, R : Expr_Ref) return Expr_Ref;
   --
   -- Shift L left by R bits, zero fill on the right.
   -- Special cases:
   -- >  If R is zero, the result is L.
   -- >  If R is a constant >= L.Width, the result is zero.


   function Srz  (L, R : Expr_Ref) return Expr_Ref;
   --
   -- Shift L right by R bits, zero fill on the left.
   -- Special cases:
   -- >  If R is zero, the result is L.
   -- >  If R is a constant >= L.Width, the result is zero.


   function Sra  (L, R : Expr_Ref) return Expr_Ref;
   --
   -- Shift L right by R bits, sign fill on the left.
   -- Special cases:
   -- >  If R is zero, the result is L.
   -- >  If R is a negative constant, the result is a Fault.


   function Rotl (L, R : Expr_Ref) return Expr_Ref;
   --
   -- Rotate L left by R bits.
   -- Special cases:
   -- >  If R is zero, the result is L.
   -- >  If R is a negative constant, the result is a Fault.


   function Rotr (L, R : Expr_Ref) return Expr_Ref;
   --
   -- Rotate L right by R bits.
   -- Special cases:
   -- >  If R is zero, the result is L.
   -- >  If R is a negative constant, the result is a Fault.


   -- Concatenation operator:
   --
   -- The width of the result is the sum of the widths of L and R.
   -- even in the special cases. If either operand is Unknown,
   -- the result is Unknown.


   function Conc (L, R : Expr_Ref) return Expr_Ref;
   --
   -- Concatenate the bits of L and R, making a word with L in
   -- the high bits and R in the low bits.


   function Can_Shift_By_Mul (N : Word_T) return Boolean;
   --
   -- Whether a shift-left-zero-fill for N bits can be implemented
   -- by a multiplication of the unshifted value with 2**N.
   -- This is the case if N is in 0 .. Opt.Max_Shift_Mul_Bits.
   -- If the result is no, an optional warning results.


   function Can_Shift_By_Mul (Expr : Expr_Ref) return Boolean;
   --
   -- Whether the Expr is an Slz with a constant amount N of shift
   -- (in Expr.R_Expr) such that the shift can be implemented by a
   -- multiplication of the unshifted value with 2**N.


   -- Bit-extraction operators and unary predicates:
   --
   -- These operators create and return a 1-bit-wide expression.
   -- If the operand is Unknown, so is the result.


   function EqZero (Expr : Expr_Ref) return Expr_Ref;
   --
   -- One if the Expr is zero, zero if the Expr is non-zero.


   function EqOne (Expr : Expr_Ref) return Expr_Ref;
   --
   -- One if the Expr is one (1), zero if the Expr has some other value.


   function Signw (Expr : Expr_Ref) return Expr_Ref;
   --
   -- The sign bit (most significant bit) of the operand.


   -- Extension operators:
   --
   -- These operators take an operand of some width and extend
   -- it to a given larger width by adding zero or sign bits at
   -- the left (high) end.


   function Exts (Expr : Expr_Ref; Width : Width_T) return Expr_Ref;

   function Extz (Expr : Expr_Ref; Width : Width_T) return Expr_Ref;


   -- Truncation operators:
   --
   -- These operators take an operand of some width and truncate
   -- (contract) it to a given smaller width by taking the given
   -- number of bits at the low (right) or high (left) end and
   -- discarding the other bits at the high or low end.
   --
   -- Trunl is equivalent to a modulus operation (remainder of division).
   -- Trunh is equivalent to a quotient operation.


   function Trunl (Expr : Expr_Ref; Width : Width_T) return Expr_Ref;

   function Trunh (Expr : Expr_Ref; Width : Width_T) return Expr_Ref;


   -- Generic unary and binary expressions:


   function Unary (
      Operation : Unary_Op_T;
      Operand   : Expr_Ref;
      Width     : Width_T)
   return Expr_Ref;
   --
   -- The expression that applies the given unary Operation to the
   -- given Operand, to create an expression with the given Width.
   -- If the Operand is Unknown, so is the result. No other special
   -- cases (ie. Operation-specific special cases) are considered.


   function Binary (
      Operation   : Binary_Op_T;
      Left, Right : Expr_Ref;
      Width       : Width_T)
   return Expr_Ref;
   --
   -- The expression that applies the given binary Operation to
   -- the given Left and Right operands, to create an expression
   -- with the given Width. If either operand is Unknown, so is
   -- the result. No other special cases (ie. Operation-specific
   -- special cases) are considered.


   function Ternary (
      Operation          : Ternary_Op_T;
      Left, Right, Carry : Expr_Ref;
      Width              : Width_T)
   return Expr_Ref;
   --
   -- The expression that applies the given ternary Operation to the
   -- given Left, Right, and Carry operands, to create an expression
   -- with the given Width. If any operand is Unknown, so is the
   -- result. No other special cases (ie. Operation-specific special
   -- cases) are considered.

   function Length (Expr : Expr_Ref) return Natural;
   --
   -- The number of elements (primitive operands and operators) in
   -- the expression. In other words, the number of nodes in the
   -- expression tree.
   --
   -- Length (Unknown) = 1. Length (null) is not defined.


   type Expressions_T is array (Positive range <>) of Expr_Ref;
   --
   -- A list of expressions.
   -- The meaning of the index depends on usage context.


   No_Expressions : Expressions_T (1 .. 0);
   --
   -- An empty expression list.


   function "=" (Left : Expr_Ref; Right : Value_T) return Boolean;
   --
   -- Whether the Left expression is a constant and, when viewed as
   -- a signed (2's complement) word of its specified width, equals
   -- the Right value. The flag Left.Signed has no effect.


   function "=" (Left : Expr_Ref; Right : Word_T) return Boolean;
   --
   -- Whether the Left expression is a constant and, when viewed as
   -- an unsigned word of its specified width, equals the Right value.
   -- The flag Left.Signed has no effect.


   function "=" (Left : Expr_Ref; Right : Cell_T) return Boolean;
   --
   -- Whether the expression is just the given cell, that is, the
   -- Left operand is the result of Cell (Right).


   function Op_Is (Op : Binary_Op_T; Expr : Expr_Ref) return Boolean;
   --
   -- Whether the topmost (outermost, root) operation in the Expr is
   -- the binary operation Op.


   function Op_Is (Op : Unary_Op_T; Expr : Expr_Ref) return Boolean;
   --
   -- Whether the topmost (outermost, root) operation in the Expr is
   -- the unary operation Op.


   --
   ---   Operations on intervals
   --


   function Is_Small_Negative (
      Interval : Storage.Bounds.Interval_T;
      Width    : Width_T)
   return Boolean;
   --
   -- Whether the Interval contains only values that, when interpreted
   -- as signed 2's complement numbers of the given Width, represent
   -- small negative numbers, by Arithmetic.Is_Small_Negative.
   -- This can be the case only for intervals that are Bounded; for
   -- other intervals the function yields False.


   function Opposite_Sign (
      Interval : Storage.Bounds.Interval_T;
      Width    : Width_T)
   return Storage.Bounds.Interval_T;
   --
   -- The interval that represents the numbers in the given Interval
   -- but with the opposite sign, when the numbers are viewed
   -- as two's complement numbers of the given Width.
   --
   -- For example, the interval 254..255 of 8-bit wide numbers yields
   -- the result -2..-1.
   --
   -- Precondition: the Interval is Bounded; if not, the function
   -- propagates Unbounded. Moreover, the Interval Is_Small_Negative;
   -- if not, the original Interval is returned, with a Warning.
   --
   -- If any conversion raises Word_Out_Of_Range or Constraint_Error,
   -- the original Interval is returned, with a Warning.


   --
   ---   Bounds on expressions
   --


   type Bounds_T is abstract new Storage.Bounds.Bounds_T
   with null record;
   --
   -- Bounds (constraints, limits, restrictions) on the (joint or
   -- separate) values of a set of variables at some point(s) in a
   -- program or an execution. The additional feature with respect
   -- to the parent type is the ability to bound expressions composed
   -- of these variables; see function Interval (Expr), below.


   -- overriding
   function Interval (Cell : Storage.Cell_T; Under : Bounds_T)
   return Storage.Bounds.Interval_T;
   --
   -- The interval (range) of values of the Cell, permitted Under the
   -- given bounds.
   --
   -- The default implementation uses Interval (Expr, Under) with an
   -- expression that consists of the single Cell and redispatching
   -- on Under.


   -- overriding
   function Difference (To, From : Storage.Cell_T; Under : Bounds_T)
   return Storage.Bounds.Interval_T;
   --
   -- The interval (range) of values of the difference To - From,
   -- permitted Under the given bounds.
   --
   -- The default implementation uses Interval (Expr, Under) with
   -- an expression that computes the difference To - From, and
   -- redispatching on Under.


   function Interval (Expr : Expr_Ref; Under : Bounds_T)
   return Storage.Bounds.Interval_T;
   --
   -- The interval (range) of values of the Expr, permitted Under the
   -- the given bounds. Note that the Expr does not necessarily take
   -- on every value in this interval. The function shall not propagate
   -- Storage.Bounds.Unbounded but may return an interval where either
   -- or both Min and Max are unlimited.
   --
   -- The default implementation computes the interval for each cell
   -- used in Expr, using Storage.Bounds.Interval (Cell, Under), with
   -- redispatching on Under, and then uses interval arithmetic to compute
   -- the interval for the Expr. The precision of the result of course
   -- depends greatly on the form of the Expr and the kind of operations
   -- used therein.


   function Difference (
      To    : Expr_Ref;
      From  : Storage.Cell_T;
      Under : Bounds_T)
   return Storage.Bounds.Interval_T;
   --
   -- The interval (range) of values of the difference To - From,
   -- permitted Under the given bounds.
   --
   -- The default implementation uses Interval (Expr, Under) with
   -- the expression Expr => To - From, and redispatching on Under.


   function Single_Value (Expr : Expr_Ref; Under : Bounds_T)
   return Word_T;
   --
   -- The single value of the Expr permitted Under the given bounds.
   -- Propagates Storage.Bounds.Unbounded if the bounds do not
   -- constrain the expression to a single value.
   --
   -- The default operation is to compute Interval (Expr, Under) with
   -- redispatching on Under and then apply Storage.Bounds.Single_Value.


   function Values (Expr : Expr_Ref; Under : Bounds_T)
   return Storage.Bounds.Value_List_T;
   --
   -- The list of possible values of the Expr, permitted Under the
   -- given Bounds. This list should contain all and only the possible
   -- values of the Expr. If such a list cannot be derived from the
   -- bounds, the function should propagate Storage.Bounds.Unbounded.
   --
   -- The maximum length of the returned list may be bounded by a
   -- command-line option.
   --
   -- The default function is Storage.Bounds.Values (Interval (Expr, Under))
   -- with redispatching on Under.


   --
   ---   Assignments and effects
   --

   -- An assignment represents a part of the effect of a processor
   -- instruction, where the value of an integer expression is
   -- assigned to an integer variable (a processor cell).


   type Assignment_Kind_T is (
      Regular,
      Conditional,
      Fracture,
      Range_Pre,
      Range_Rel,
      Range_Post);
   --
   -- There are six kinds of assignment.
   --
   -- Regular
   --    A regular assignment is unconditional, corresponding to
   --    an effect as in a statement:
   --
   --       Target := Expression
   --
   -- Conditional
   --    A conditional assignment involves one boolean expression
   --    and two integer expressions, and corresponds to a
   --    statement like
   --
   --       if Bool_Expr then
   --          Target := Int_Expr_1;
   --       else
   --          Target := Int_Expr_2;
   --       end if;
   --
   -- Fracture
   --    A pseudo-assignment that signals that the value of the Target
   --    is changed as a side-effect of another assignment (in the
   --    same "effect" or set of assignments) that assigns only a
   --    part of the Target cell (for example, some bit-field within
   --    the Target cell) or that assigns some larger cell of which
   --    the Target is a part. For most analyses a Fracture assignment
   --    should be understood as equivalent to the Regular assignment
   --    Target := Unknown, but some analyses may do better, perhaps
   --    even by ignoring Fracture assignments if the analysis can
   --    handle the overlapping cells in a better way. For example,
   --    in a target processor that has 8-bit general registers that
   --    can also be paired to make 16-bit registers, an instruction
   --    that assigns something to an 8-bit register could be modelled
   --    as a Regular assignment to the 8-bit register cell and a
   --    Fracture assignment to the 16-bit register-pair (or pairs)
   --    of which the 8-bit register is a part. Likewise, an instruction
   --    that computes something into a 16-bit register pair could be
   --    modelled as a Regular assignment to the 16-bit register cell
   --    and two Fracture assignments to the 8-bit part cells.
   --
   -- Range_Pre, Range_Rel, Range_Post
   --    A range assignment is not really an assignment to the Target,
   --    but instead constrains the value of the Target to lie in an
   --    arithmetic range that is defined by an expression Min for
   --    for the lower bound and an expression Max for the upper
   --    bound. This corresponds to an assertion of the form
   --
   --       Assert (Target in Min .. Max)
   --
   --    Either Min or Max (or both, in principle) can be Unknown,
   --    meaning that this end of the range is undefined (void
   --    condition).
   --
   --    Note that a Range assignment does not change the value of
   --    the Target cell, it merely constrains the existing value of
   --    the cell.
   --
   --    The difference between the three sorts of range assignments
   --    appears when the Range assignment is part of the effect of an
   --    instruction that changes the values of some cells (using Regular
   --    or Conditional assignments). In such a context, we can separate
   --    between the old value (pre-value) and the new value (post-value)
   --    of any cell (if the cell is not assigned, pre = post). Then:
   --
   --    > In a Range_Pre assignment, any cell name, whether in the Target
   --      or in the Min and Max expressions, refers to the pre-value.
   --
   --    > In a Range_Rel assignment, the Target refers to the post-value
   --      but all cell names in Min and Max refer to pre-values (the
   --      assignment expresses a Relation between the pre- and post-
   --      values).
   --
   --    > In a Range_Post assignment, all cell names refer to post-values.
   --
   --    Note that implementation restrictions (in Calculator.Formulas)
   --    mean that references to the post-values of cells (in Range_Rel
   --    or Range_Post assignments) are possible only for cells that are
   --    also assigned in the same effect (by a Regular, Conditional or
   --    Fracture assignment). Otherwise, the "dashed" identifier of
   --    the post value is undefined.


   subtype Value_Defining_Kind_T is
      Assignment_Kind_T range Regular .. Conditional;
   --
   -- The assignments that define a new value of the Target cell (write
   -- the cell as a whole). Note that Fracture assignments are excluded
   -- because they give no information about the assigned value.


   subtype Defining_Kind_T is
      Assignment_Kind_T range Regular .. Fracture;
   --
   -- The assignments that define a new value of the Target cell (change
   -- the value of the cell). Note that Fracture assignments are included
   -- although they only signal the presence of some other (Regular or
   -- Conditional) assignment that actually writes to the cell or to a
   -- part of the cell.


   subtype Range_Kind_T is
      Assignment_Kind_T range Range_Pre .. Range_Post;
   --
   -- The assignments that constrain the range of the Target
   -- cell without defining a new value for it (no write to
   -- the cell).


   type Assignment_T (Kind : Assignment_Kind_T := Regular)
   is record

      Target : Variable_T;

      case Kind is

      when Regular =>
         Value  : Expr_Ref;

      when Conditional =>
         Cond   : Expr_Ref;  -- Boolean expression
         Value1 : Expr_Ref;  -- if cond = true
         Value2 : Expr_Ref;  -- if cond = false

      when Fracture =>

         null;

      when Range_Kind_T =>
         Min    : Expr_Ref;  -- lower bound
         Max    : Expr_Ref;  -- upper bound

      end case;

   end record;


   function Identity (Item : Assignment_T) return Boolean;
   --
   -- Whether the assignment is an identity (no-effect) assignment
   -- of the form Target := Target.


   type Assignment_List_T is array (Positive range <>) of Assignment_T;
   --
   -- A list of assignments, with purpose and interpretation to be
   -- specified.


   function Image (Item : Assignment_List_T) return String;
   --
   -- Shows all the assignments in the list.


   type Effect_T is new Assignment_List_T;
   --
   -- Represents the arithmetic effect of a processor instruction
   -- as a set of assignments of expression values to integer
   -- variables (processor cells).
   --
   -- Regular assignments, conditional assignments and range
   -- assignments can be mixed.
   --
   -- The Defining assignments are considered to be executed in
   -- parallel so that all the expressions (and conditions are
   -- evaluated first, using the current values of the referenced
   -- cells, and only then is each result assigned to the corresponding
   -- target cell. For this reason, the order of the assignments in
   -- the list is irrelevant. Also, it is usually nonsense to have,
   -- in the same Effect, more than one defining assignment to the
   -- same target cell.
   --
   -- The Range assignments are not executed but provide constraints
   -- on the values of the other cells in the Effect. A given cell
   -- can be the Target of zero or more Range_Pre constraints and
   -- zero or Range_Post constraints. If there are several constraints
   -- of the same kind on the same cell they are logically conjoined
   -- (intersection of ranges).
   --
   -- In some target processors, an Effect may contain assignments
   -- where the target cells are potential aliases of each other.
   -- If this happens, the final values in these cells are considered
   -- unknown (opaque).
   --
   -- If an Effect uses (reads) the values of a volatile cell more than
   -- once, that is, if the same volatile cell appears as more than one
   -- source operand in some expression in the Effect, then we assume
   -- that all uses of this cell represent the same value. In other words,
   -- we assume that the volatile cell is read only once, when this Effect
   -- is evaluated, or (equivalently) that the value of the cell does not
   -- change during the evaluation of the Effect. This allows us to apply
   -- simplifications such as x-x = 0 even when x is a volatile cell.
   --
   -- When an effect contains Range_Rel or Range_Post assignments the
   -- following consistency rules must be followed, to ensure that the
   -- algorithms that find the "used" and "defined" cells of an effect
   -- work correctly:
   --
   -- > The target cell of any Range_Rel or Range_Post assignment must
   --   be the target of a Defining assignment in the same effect.
   --
   -- > The cells that appear in the Min and Max expressions of any
   --   Range_Post assignment must be targets of Defining assignments
   --   in the same effect.


   type Effect_Ref is access Effect_T;
   --
   -- It can often be convenient and efficient to refer to
   -- heap-allocated effects. For example, a program often
   -- contains numerous copies of simple register instructions
   -- such as "INC DPTR", and the Decoder can make all the
   -- decoded steps share the same Effect_T.


   --
   ---   Creating assignments and effects
   --


   function Set (
      Target : Variable_T;
      Value  : Expr_Ref)
   return Assignment_T;
   --
   -- For normal Target := Value where the Target may be statically
   -- known or dynamically addressed.


   function Set (Target : Variable_T) return Assignment_T;
   --
   -- For unanalysable effects ( Target := ?? ) where the Target may be
   -- statically known or dynamically addressed. The result is a
   -- Regular assignment, not a Fracture.


   function Set (
      Target : Variable_T;
      Cond   : Condition_T;
      Value1 : Expr_Ref;
      Value2 : Expr_Ref)
   return Assignment_T;
   --
   -- For Conditional effects:
   --    if Cond then Target := Value1 else Target := Value2.
   -- If the Cond is ??, the result is simply Target := ??.
   --
   -- This version, with Target of type Variable_T, is for use when the
   -- Target may be statically known or dynamically addressed.


   function Set_Flag (
      Flag : Variable_T;
      Cond : Condition_T)
   return Assignment_T;
   --
   -- Sets the Flag to 1 if the Cond is True and to 0 otherwise.
   --
   -- Equivalent to
   --   Set (Target => Flag, Cond => Cond, Value1 => One, Value2 => Zero);


   function Set (
      Flag : Variable_T;
      Cond : Boolean)
   return Assignment_T;
   --
   -- Like Set_Flag, but using a statically known Condition which means
   -- that the result is a Regular assignment, not a Conditional one.


   function Set_Flag_If_Zero (
      Flag : Variable_T;
      Expr : Expr_Ref)
   return Assignment_T;
   --
   -- Like Set_Flag, but for the special condition Expr = 0, and
   -- yields the regular assignment Flag := EqZero (Expr).
   -- Applies some simplifications depending on Expr, for example
   -- if Expr is a constant.


   function Set_Flag_If_Negative (
      Flag : Variable_T;
      Expr : Expr_Ref)
   return Assignment_T;
   --
   -- Like Set_Flag, but for the special condition Expr < 0 (in
   -- binary terms, the highest bit of Expr is on (1), using the
   -- width of the Expr). Yields the assignment Flag := Signw (Expr).
   -- Applies some simplifications depending on Expr, for example
   -- if Expr is a constant.


   function Fracture (Target : Variable_T) return Assignment_T;
   --
   -- A Fracture assignment to the Target, signifying that the
   -- value of the Target changes because some other assignment
   -- in the same effect (same set of assignments) assigns a value
   -- to a part of the Target, or to some larger cell of which
   -- the Target is a part.


   function Range_Pre (
      Target : Variable_T;
      Min    : Expr_Ref;
      Max    : Expr_Ref)
   return Assignment_T;
   --
   -- For Range_Pre constraint assignments:
   --    Min <= Target <= Max
   -- using old (pre-effect) values of cells in all places.
   --
   -- This version, with Target of type Variable_T, is for use when the
   -- Target may be statically known or dynamically addressed.


   function Range_Rel (
      Target : Variable_T;
      Min    : Expr_Ref;
      Max    : Expr_Ref)
   return Assignment_T;
   --
   -- For Range_Rel constraint assignments:
   --    Min <= Target <= Max
   -- using old (pre-effect) values of cells in Min and Max but
   -- the new (post-effect) value of the Target cell.
   --
   -- This version, with Target of type Variable_T, is for use when the
   -- Target may be statically known or dynamically addressed.


   function Range_Post (
      Target : Variable_T;
      Min    : Expr_Ref;
      Max    : Expr_Ref)
   return Assignment_T;
   --
   -- For Range_Post constraint assignments:
   --    Min <= Target <= Max
   -- using new (post-effect) values of cells in all places.
   --
   -- This version, with Target of type Variable_T, is for use when the
   -- Target may be statically known or dynamically addressed.


   function No_Effect return Effect_Ref;
   --
   -- Represents an instruction that has no effect on the
   -- program's variables (cells).


   function To_Effect_Ref (Item : Assignment_T) return Effect_Ref;
   --
   -- Copies the given assignment (to the heap) and returns a
   -- reference to the copy, as an effect.


   function To_Effect_Ref (Item : Effect_T) return Effect_Ref;
   --
   -- Copies the given effect (to the heap) and returns a
   -- reference to the copy.
   -- However, if the Item is empty (has no assignments),
   -- then No_Effect is returned, to avoid allocating multiple
   -- copies of the null effect.


   function Positive_Length (Effect : Effect_T) return Positive;
   --
   -- The Effect'Length, but at least 1. This is useful to set up
   -- an assignment set with a Max_Size that can hold all of the
   -- Effect. (Note that Assignment_Set.Max_Size is Positive, not
   -- Natural. See the implementation for why this is so; it has
   -- to do with avoiding null vectors where 'First > 'Last.)


   function Positive_Length (Effect : Effect_Ref) return Positive;
   --
   -- As above, but for a reference to an effect.


   --
   ---   Querying effects for particular assignments
   --


   function Pick (
      Kind : Assignment_Kind_T;
      From : Effect_T)
   return Effect_T;
   --
   -- The assignments of the chosen Kind, From the given effect.


   function Defining_Assignments (Effect : Effect_T) return Natural;
   --
   -- The number of Defining assignments in the Effect.


   function Defining_Assignments (Effect : Effect_T) return Effect_T;
   --
   -- The Defining assignments in the Effect.


   procedure Find (
      Target : in     Cell_T;
      Within : in     Effect_T;
      Index  :    out Natural);
   --
   -- Finds the (unique) Defining assignment Within a given effect that
   -- defines a given Target cell. Returns the (unique) Index in Within
   -- of the assignment to Target. If the given effect does not define
   -- the Target cell, returns zero in Index.
   --
   -- Range constraint assignments are ignored.
   --
   -- Target
   --    The cell for which a defining assignment is sought.
   -- Within
   --    The effect that might contain such an assignment.
   -- Index
   --    If Is_Defined (Cell => Target, By => Within) is True, the
   --    index such that Within(Index).Target = Target. Otherwise
   --    Index is returned as zero.


   No_Such_Assignment : exception;
   --
   -- Signals a failed attempt to find an assignment of a particular
   -- kind (for example, defining a particular target cell) within
   -- a given effect or assignment set.


   function Definition_Of (
      Target : Cell_T;
      Within : Effect_T)
   return Assignment_T;
   --
   -- The assignment that defines To, assuming that To is defined
   -- Within this effect. If there is no defining assignment Within
   -- this effect To this cell, No_Such_Assignment is propagated.


   --
   ---   Aliasing effects
   --


   function Target_Range (Item  : Assignment_T)
   return Storage.Alias_Range_T;
   --
   -- The alias range that can be reached by the given assignment
   -- when it assigns a value to its target cell. For dynamically
   -- addressed targets the widest possible range (no bounds on
   -- cell values) is taken.
   --
   -- The value Storage.Isolated is returned for Range assignments
   -- since they do not change the target cell.


   function May_Alias (
      Cell   : Storage.Cell_T;
      Target : Variable_T)
   return Boolean;
   --
   -- Whether the Cell and the Target can be aliases of the same storage
   -- location, so that an assignment to the Target may change the value
   -- stored in the Cell.


   function Target_Range (Effect : Effect_T)
   return Storage.Alias_Range_T;
   --
   -- The alias range that can be reached by the Defining  assignments
   -- in the given effect, when they assign values to their target cells.
   -- For dynamically addressed targets the widest possible range (no
   -- bounds on cell values) is taken.


   function May_Alias (
      Cell   : Storage.Cell_T;
      Effect : Effect_T)
   return Boolean;
   --
   -- Whether the Cell may alias with some target of an assignment in
   -- the Effect, so that executing the Effect may change the value
   -- stored in the Cell.


   --
   ---   Assignment sets of flexible (but bounded) size
   --


   type Assignment_Set_T (Max_Size : Positive) is private;
   --
   -- A set of assignments with a dynamic but bounded size.
   -- The default initial value is the empty set.
   -- The assignments in the set are numbered 1, 2, ... in the
   -- order they are added to the set.
   -- This type has value semantics.


   function Image (Item : Assignment_Set_T) return String;
   --
   -- For human consumption. Same as Image (To_Effect (Item)).


   procedure Erase (Item : in out Assignment_Set_T);
   --
   -- Erases the set, removing all its assignments.
   -- The result is the empty set.


   function Last (Item : Assignment_Set_T) return Positive;
   --
   -- The number of the last assignment added to the set.
   -- May raise Constraint_Error if the set is empty.


   function Length (Item : Assignment_Set_T) return Natural;
   --
   -- The number of assignments in the set.


   procedure Add (
      To   : in out Assignment_Set_T;
      More : in     Assignment_T);
   --
   -- Adds one More assignment To an assignment set.


   procedure Add (
      To   : in out Assignment_Set_T;
      More : in     Effect_T);
   --
   -- Adds More assignments To an assignment set.


   procedure Veil (
      Variable : in     Variable_T;
      Within   : in out Assignment_Set_T);
   --
   -- Adds an assignment Within the given set to make the given
   -- Variable opaque (unknown).


   procedure Veil (
      Variables : in     Variable_List_T;
      Within    : in out Assignment_Set_T);
   --
   -- Adds assignments Within the given set to make all the listed
   -- Variables opaque (unknown).


   procedure Fracture (
      Variable : in     Variable_T;
      Within   : in out Assignment_Set_T);
   --
   -- Adds a Fracture assignment to the given Variable Within the
   -- given set of assignments.


   procedure Fracture (
      Variables : in     Variable_List_T;
      Within    : in out Assignment_Set_T);
   --
   -- Adds Fracture assignments Within the given set to all the
   -- listed Variables.


   function Element (
      From   : Assignment_Set_T;
      Number : Positive)
   return Assignment_T;
   --
   -- The assignment with the given Number From the given set.
   -- The Number should be in the range 1 .. Length(From).


   procedure Find (
      Target : in     Cell_T;
      Within : in     Assignment_Set_T;
      Index  :    out Natural);
   --
   -- Same as the Find for Effect, but searches an Assignment_Set_T.


   function Definition_Of (
      Target : Cell_T;
      Within : Assignment_Set_T)
   return Assignment_T;
   --
   -- Same as Definition_Of for Effect, but searches an Assignment_Set_T.


   function To_Effect (Item : Assignment_Set_T) return Effect_T;
   --
   -- All the assignments from the Item, indexed according
   -- to the numbering in the set (1, 2, ..).


   function To_Effect_Ref (Item : Assignment_Set_T) return Effect_Ref;
   --
   -- All the assignments cumulated into Item, indexed according
   -- to the numbering in the set (1, 2, ..).
   --
   -- The result is independent of any storage allocated for
   -- Item (i.e. it is a copy, real or virtual).
   --
   -- The Item can be deallocated or modified without impacting
   -- the validity of the Effect_Ref returned, nor the data
   -- in the Effect_T it refers to.
   --
   -- However, note that although the assignments in the result
   -- are independent of the given item, they refer to the
   -- same target cells and expressions; the copy is not deep.
   --
   -- If the Item is the empty set, then No_Effect is returned.


   --
   ---   Cells used or defined in arithmetic expressions and effects
   --


   function Is_Defined (
      Cell : Cell_T;
      By   : Effect_T)
   return Boolean;
   --
   -- Return true if the given cell is defined (assigned, written)
   -- by a Defining assignment in the given effect, otherwise return
   -- false.


   function Is_Defined (
      Cell : Cell_T;
      By   : Assignment_Set_T)
   return Boolean;
   --
   -- Whether the given Cell is defined (assigned, written) By a Defining
   -- assignment in the given assignment set.


   function Is_Used (
      Cell : Cell_T;
      By   : Expr_Ref)
   return Boolean;
   --
   -- Whether the given cell is used (read) by the given expression.
   --
   -- If the expression contains dynamic memory references, the Basis
   -- cells of the Storage.References.Boundable_T are included.


   function Is_Used (
      Cell : Cell_T;
      By   : Expressions_T)
   return Boolean;
   --
   -- Whether the given cell is used (read) by one or more of the
   -- given expressions. See also Is_Used for a single Exp_Ref.


   function Is_Used (
      Cell : Cell_T;
      By   : Effect_T)
   return Boolean;
   --
   -- Whether the given cell is used (read) by a Defining assignment
   -- in the given effect, otherwise return false.
   --
   -- If the effect contains dynamic memory references (in the targets
   -- or expressions), the Basis cells of the referenced Boundable_Refs
   -- are included.
   --
   -- Note that uses in Range constraints are not considered.


   function Any_Cell_Is_Used (
      From : Cell_Set_T;
      By   : Expr_Ref)
   return Boolean;
   --
   -- Whether any cell From the given set is used By the given
   -- expression. This includes use as an operand and use as a
   -- basis cell for some dynamic memory reference.


   function Any_Cell_Is_Used (
      From : Cell_Set_T;
      By   : Assignment_T)
   return Boolean;
   --
   -- Whether any cell From the given set is used By the given
   -- assignment. This includes uses in the condition of a conditional
   -- assignment and use as a Basis cell for some dynamic memory
   -- reference.
   --
   -- For a Range_Post assignment, uses in the Min and Max expressions
   -- are not included.


   procedure Add_Cells_Used (
      By   : in     Expr_Ref;
      Refs : in     Boolean;
      To   : in out Cell_Set_T);
   --
   -- Finds all the cells used (read) by the given expression
   -- and adds them to the given cell-set.
   --
   -- If the expression contains dynamic memory references, the Basis
   -- cells of these Boundable_Refs are included if Refs is True,
   -- otherwise (when Refs is False) these cells are not included.
   --
   -- A null expression can be given, and is understood as not
   -- using (reading) any cells. This special case is meant to
   -- handle expressions involving opaque values.


   procedure Add_Cells_Used (
      By    : in     Expr_Ref;
      Refs  : in     Boolean;
      To    : in out Cell_Set_T;
      Added : in out Boolean);
   --
   -- Finds all the cells used (read) by the given expressions
   -- and adds them to the given cell-set. See also Add_Cells_Used
   -- without the Added parameter, above.
   --
   -- The parameter "Added" is set True if any new cells were
   -- added to the cell set (i.e. if the set grew). Otherwise
   -- it is returned unchanged.


   procedure Add_Cells_Used (
      By   : in     Expressions_T;
      Refs : in     Boolean;
      To   : in out Cell_Set_T);
   --
   -- Finds all the cells used (read) by the given expressions
   -- and adds them to the given cell-set. See also Add_Cells_Used
   -- for a single Expr_Ref.
   --
   -- If the expression contains dynamic memory references, the Basis
   -- cells of these Boundable_Refs are included if Refs is True,
   -- otherwise (when Refs is False) these cells are not included.


   procedure Add_Cells_Used (
      By   : in     Effect_T;
      Refs : in     Boolean;
      Post : in     Boolean;
      To   : in out Cell_Set_T);
   --
   -- Finds all the cells whose pre-values are used (read) by the given
   -- effect's expressions and adds them to the given cell-set.
   --
   -- A cell that occurs only as a target (assigned) cell in a Defining
   -- assignment is not included.
   --
   -- If the effect contains dynamic memory references (in the targets
   -- or expressions), the Basis cells of these Boundable_Refs are
   -- included if Refs is True, otherwise (when Refs is False) these
   -- cells are not included.
   --
   -- The Post parameter selects inclusion of cells whose post-values
   -- are used (the values possibly defined By this effect). Thus,
   -- if Post is False, the operation includes no cells from Range_Post
   -- assignments, and for Range_Rel assignments it includes only the
   -- cells used in the Min and Max expressions but omits the target cell.
   -- If Post is True, the operation includes the cells used in the Min
   -- and Max expressions and also the target cell for all kinds of range
   -- constraint assignments. Whatever the value of Post the operation
   -- always includes the cells used in the Min and Max expressions and
   -- the target cell of Range_Pre assignments.


   procedure Add_Cells_Used (
      By    : in     Assignment_T;
      Refs  : in     Boolean;
      Post  : in     Boolean;
      To    : in out Cell_Set_T;
      Added : in out Boolean);
   --
   -- Finds all the cells whose pre-values are used (read) by the given
   -- assignment's expressions and adds them to the given cell-set.
   --
   -- A cell that occurs only as the target (assigned) cell in
   -- a Defining assignment is not included.
   --
   -- If the effect contains dynamic memory references (in the targets
   -- or expressions), the Basis cells of these Boundable_Refs
   -- are included if Refs is True, otherwise (when Refs is False)
   -- these cells are not included.
   --
   -- The Post parameter selects inclusion of cells whose post-values
   -- are used (the values possibly defined by the effect of which this
   -- assignment is a part). Thus, if Post is False, the operation
   -- includes no cells from Range_Post assignments, and for Range_Rel
   -- assignments it includes only the cells used in the Min and Max
   -- expressions but omits the target cell. If Post is True, the
   -- operation includes the cells used in the Min and Max expressions
   -- and also the target cell for all kinds of range constraint
   -- assignments. Whatever the value of Post the operation always
   -- includes the cells used in the Min and Max expressions and
   -- the target cell of Range_Pre assignments.
   --
   -- The parameter "Added" is set True if any new cells were
   -- added to the cell set (i.e. if the set grew). Otherwise
   -- it is returned unchanged.


   procedure Add_Reference_Bases (
      From : in     Expr_Ref;
      To   : in out Cell_Set_T);
   --
   -- Adds To the given cell-set all the Basis cells of all dynamic
   -- memory references From the given expression.


   procedure Add_Cells_Defined (
      By : in     Effect_T;
      To : in out Cell_Set_T);
   --
   -- Finds all the cells Defined (assigned, written) by the given
   -- effect and adds them to the given cell-set.
   -- These cells may or may not be used (read) by the expressions
   -- in the effect.
   -- Ignores Range assignments.


   procedure Remove_Cells_Defined (
     By   : in     Effect_T;
     From : in out Cell_Set_T);
   --
   -- Finds all the cells Defined (assigned, written) by the given
   -- effect and removes them from the given cell-set.
   -- These cells may or may not be used (read) by the expressions
   -- in the effect.
   -- Ignores Range assignments.


   function Cells_Used (By : Expr_Ref) return Cell_List_T;
   --
   -- All the cells that are used (read) by the given expression.
   --
   -- If the expression contains dynamic memory references, the Basis
   -- cells of these Boundable_Refs are included.


   function Cells_Used (By : Expressions_T) return Cell_List_T;
   --
   -- All the cells that are used (read) by the given expressions.
   --
   -- If the expressions contain dynamic memory references, the Basis
   -- cells of these Boundable_Refs are included.


   --
   ---   Checking for (unresolved) dynamic references
   --


   function Dynamic (Expr : Expr_Ref) return Boolean;
   --
   -- Whether the Expression contains some dynamically addressed
   -- memory references.


   function Dynamic_Values (Assignment : Assignment_T) return Boolean;
   --
   -- Whether the Assignment contains some dynamically addressed
   -- memory references in the assigned value expressions or in
   -- the condition for a conditional assignment. A dynamic memory
   -- references in the target variable is not considered.


   function Dynamic (Assignment : Assignment_T) return Boolean;
   --
   -- Whether the Assignment contains some dynamically addressed
   -- memory references, either in the target variable(s) or in
   -- the assigned value expressions.


   function Dynamic_Values (Effect : Effect_T) return Boolean;
   --
   -- Whether the value expressions or assignment conditions in the
   -- Effect contain some dynamically addressed memory references.
   -- Dynamic memory references in the target variable(s) are not
   -- considered.


   function Dynamic (Effect : Effect_T) return Boolean;
   --
   -- Whether the Effect contains some dynamically addressed
   -- memory references.


   function Targets_Reference (Item : Assignment_T) return Boolean;
   --
   -- Whether the target variable of the assignment is a dynamic
   -- memory reference.


   function Uses_Reference (Item : Expr_Ref) return Boolean;
   --
   -- Whether the expression contains an operand that is a dynamic
   -- memory reference.


   function Uses_Reference (Item : Assignment_T) return Boolean;
   --
   -- Whether the assigned values (or the condition for a conditional
   -- assignment) contain dynamic memory references.


   --
   ---   Resolving references within expressions, assignments, effects
   --


   function Reference_Bounded (
      Expr   : Expr_Ref;
      Bounds : Storage.Bounds.Bounds_T'Class)
   return Expr_Ref;
   --
   -- Tries to bound the dynamic memory references in the Expr
   -- by applying the given Bounds. If some references are thus
   -- resolved to single cells, or give rise to more constrained
   -- references, a new expression is constructed in which these
   -- references are replaced accordingly, and the new expression
   -- is returned. Otherwise, the original Expr is returned.
   --
   -- The underlying Expr_T (Expr.all) is never changed. However,
   -- the new expression may reuse (refer to) parts of the original
   -- expression.


   procedure Bound_References (
      Within  : in out Assignment_T;
      Bounds  : in     Storage.Bounds.Bounds_T'Class;
      Bounded :    out Boolean);
   --
   -- Tries to bound the dynamic memory references Within the given
   -- assignment by applying the given Bounds to all parts of the
   -- assignment (the target, the value expression(s), and the
   -- condition if any). If some references are thus resolved to
   -- single cells, or give rise to more constrained references,
   -- the assignment is updated accordingly and Bounded is returned
   -- as True. If the assignment is not changed, Bounded is False.
   -- The Kind of the assignment is never changed.


   function Reference_Bounded (
      Effect : Effect_Ref;
      Bounds : Storage.Bounds.Bounds_T'Class)
   return Effect_Ref;
   --
   -- Tries to bound the dynamic memory references in the Effect
   -- by applying the given Bounds. If some references are thus
   -- resolved to single cells, or give rise to more constrained
   -- references, a new effect is constructed in which these
   -- references are replaced accordingly, and the new effect
   -- is returned. Otherwise, the original Effect is returned.
   --
   -- The underlying Effect_T (Effect.all) is never changed. Any
   -- change in any part of the Effect leads to a new (heap-allocated)
   -- Effect_T. However, the new Effect_T may reuse (refer to) those
   -- parts of the original Effect.all that did not change when the
   -- Bounds were applied to them.


   --
   ---   Image functions
   --


   type Expr_Imager_T is tagged null record;
   --
   -- Hooks for creating custom Images of Expressions.


   -- not overriding
   function Opaque_Image (Imager : Expr_Imager_T)
   return String;
   --
   -- The image of an Opaque value, in the given Imager.
   -- The default is "?".


   -- not overriding
   function Const_Image (
      Item   : Const_Expr_T;
      Imager : Expr_Imager_T)
   return String;
   --
   -- The image of a Constant value, in the given Imager.
   -- The default is Arithmetic.Image (Item.Value).


   -- not overriding
   function Cell_Image (
      Item   : Cell_Expr_T;
      Imager : Expr_Imager_T)
   return String;
   --
   -- The image of a cell, in the given Imager.
   -- The default is Storage.Image (Item.Cell).


   -- not overriding
   function Ref_Image (
      Item   : Ref_Expr_T;
      Imager : Expr_Imager_T)
   return String;
   --
   -- The image of a dynamic memory reference.
   -- The default is Storage.References.Image (Item.Ref), with dispatch.


   -- not overriding
   function Unary_Op_Image (
      Item   : Unary_Op_T;
      Imager : Expr_Imager_T)
   return String;
   --
   -- The image of the unary operator, in the given Imager.
   -- See the body for the default images.


   -- not overriding
   function Unary_Image (
      Item   : Unary_Expr_T;
      Imager : Expr_Imager_T)
   return String;
   --
   -- The image of a unary expression, in the given Imager.
   -- The default is the name of the Item.Unary operator, followed by
   -- the Image of the sub-Expression, the whole enclosed in parentheses.


   -- not overriding
   function Binary_Op_Image (
      Item   : Binary_Op_T;
      Imager : Expr_Imager_T)
   return String;
   --
   -- The image of a binary operator, in the given Imager.
   -- See the body for the default images.


   -- not overriding
   function Binary_Image (
      Item   : Binary_Expr_T;
      Imager : Expr_Imager_T)
   return String;
   --
   -- The image of a binary expression, in the given Imager.
   -- The default is the Image of the Left sub-expression, followed by
   -- the name of the Item.Binary operator, followed by the Image of
   -- the Right sub-expression, the whole enclosed in parentheses.


   -- not overriding
   function Ternary_Op_Image (
      Item   : Ternary_Op_T;
      Imager : Expr_Imager_T)
   return String;
   --
   -- The image of a ternary operator, in the given Imager.
   -- See the body for the default images.


   -- not overriding
   function Ternary_Image (
      Item   : Ternary_Expr_T;
      Imager : Expr_Imager_T)
   return String;
   --
   -- The image of a ternary expression, in the given Imager.
   -- See the body for the default images.


   function Variable_Image (
      Item   : Expr_T;
      Imager : Expr_Imager_T'Class)
   return String;
   --
   -- The image of a Variable_Kind_T item, whether statically or
   -- dynamically addressed, in the given Imager.


   function Image (
      Item   : Expr_T;
      Imager : Expr_Imager_T'Class)
   return String;
   --
   -- The expression as an infix, parenthesized formula, using the
   -- given Imager.


   function Image (
      Item   : Expr_Ref;
      Imager : Expr_Imager_T'Class)
   return String;
   --
   -- Same as Image (Item.all, Imager), with the special cases
   -- "true" for Always, "false" for Never, "[]" for a null Expr_Ref.


   --
   ---    Default Image functions
   --


   function Image (Item : Expr_T) return String;
   --
   -- The expression as an infix, parenthesized formula, using
   -- the default Imager.


   function Image (Item : Expr_Ref) return String;
   --
   -- Same as Image (Item.all), with the special cases "true"
   -- for Always, "false" for Never, "[]" for a null Expr_Ref.


   function Image (Item : Assignment_T) return String;
   --
   -- Presents the assignment as "Image (Target) = Image (Expr)", or
   -- the equivalent for conditional and range-constraint assignments,
   -- using the default Imager. Intended for human reading.


   --
   ---   Abstract transfer functions
   --


   type Transfer_Function_T is abstract tagged null record;
   --
   -- Represents the overall arithmetic computation from some point
   -- in an execution to some later point in the same execution.


   function Definition (
      Cell : Storage.Cell_T;
      Thru : Transfer_Function_T)
   return Expr_Ref
   is abstract;
   --
   -- The definition of the value that the Cell is assigned Thru a
   -- given computation, as an expression of the initial values of
   -- cells before this computation was started. May return Unknown
   -- if the computation is too complex or is modelled too vaguely
   -- to give a sharper result.


   function Is_Initial_Value (
      Cell : Storage.Cell_T;
      From : Storage.Cell_T;
      Thru : Transfer_Function_T)
   return Boolean;
   --
   -- Whether the value of the Cell passed Thru a given computation
   -- is the initial value From some other cell (or the same Cell,
   -- if From = Cell). This predicate must be safe (if the predicate
   -- is True, the transfer function copies the initial value From the
   -- other cell to the given Cell) but may give false negatives.
   --
   -- The default implementation checks if Definition (Cell, Thru)
   -- is the expression consisting of the From cell and nothing more.


   function Is_Identical (
      Cell : Storage.Cell_T;
      Thru : Transfer_Function_T)
   return Boolean;
   --
   -- Whether the value of the Cell passed Thru a given computation
   -- is identical to the initial value of the Cell. This predicate
   -- must be safe (if the predicate is True, the transfer function
   -- does not modify the Cell) but may give false negatives (if the
   -- predicate is False, the transfer function may or may not modify
   -- the cell).
   --
   -- The default implementation uses Is_Initial_Value to check if
   -- the value of Cell after the computation is the initial value
   -- of the same Cell before the computation.


private


   No_Expr : constant Expr_Ref := null;


   True_Value  : constant Bit_T := 1;
   False_Value : constant Bit_T := 0;
   --
   Boolean_Value : constant array (Boolean) of Bit_T := (
      False => False_Value,
      True  => True_Value);
   --
   -- The numeric coding of Boolean values.
   -- When a Boolean expression is evaluated to a constant (numeric)
   -- value, the Boolean value is represented by these numbers.
   -- Note: The evaluation of Boolean operators (and, or, xor, not)
   -- may depend on using these particular values. So do not change
   -- them. See also Boolean_Cond, below.


   Always : constant Condition_T := new Expr_T'(
      Kind   => Const,
      Width  => 1,
      Value  => Boolean_Value(True),
      Signed => False);
   --
   -- Represents the Boolean condition "True".
   -- The Kind and Value are significant for the child package
   -- Arithmetic.Evaluation, at least.


   Never : constant Condition_T := new Expr_T'(
      Kind   => Const,
      Width  => 1,
      Value  => Boolean_Value(False),
      Signed => False);
   --
   -- Represents the Boolean condition "False".
   -- The Kind and Value are significant for the child package
   -- Arithmetic.Evaluation, at least.


   Boolean_Cond : constant array (Bit_T) of Condition_T := (
      0 => Never,
      1 => Always);
   --
   -- The false and true conditions corresponding to the numeric
   -- coding of Boolean values.


   Unknown : constant Expr_Ref := new Expr_T '(
      Kind  => Opaque,
      Width => 1);
   --
   -- This should be the only place where an Opaque expression
   -- is created. All clients should use Unknown instead of
   -- creating new Opaque expressions.


   Zero_Bit : constant Expr_Ref := new Expr_T'(
      Kind   => Const,
      Width  => 1,
      Value  => 0,
      Signed => False);

   One_Bit  : constant Expr_Ref := new Expr_T'(
      Kind   => Const,
      Width  => 1,
      Value  => 1,
      Signed => False);


   package Bounded_Effects is new Bounded_Vectors (
      Index_Type   => Positive,
      Element_Type => Assignment_T,
      Vector_Type  => Effect_T);

   type Assignment_Set_T (Max_Size : Positive) is record
      Set : Bounded_Effects.Bounded_Vector (1, Max_Size);
   end record;


end Arithmetic;
