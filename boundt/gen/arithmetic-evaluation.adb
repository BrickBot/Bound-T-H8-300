-- Arithmetic.Evaluation (body)
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
-- $Revision: 1.19 $
-- $Date: 2015/10/24 20:05:44 $
--
-- $Log: arithmetic-evaluation.adb,v $
-- Revision 1.19  2015/10/24 20:05:44  niklas
-- Moved to free licence.
--
-- Revision 1.18  2013-02-05 20:19:00  niklas
-- Extended Value_Ref (Expr_Ref) to trace the resolved data reference,
-- optional on Options.General.Trace_Resolution.
--
-- Revision 1.17  2010-03-23 07:46:35  niklas
-- Updated Unary_Operation to use the new functions Truncate_Low/High.
--
-- Revision 1.16  2010-01-01 12:37:05  niklas
-- BT-CH-0209: Trace refinements from Combine_Terms.
--
-- Revision 1.15  2009-11-27 11:28:06  niklas
-- BT-CH-0184: Bit-widths, Word_T, failed modular analysis.
--
-- Revision 1.14  2009/02/13 19:07:33  niklas
-- Corrected Eval (Assignment, .., Target, ..) to check Value2, when
-- evaluated, for bounded dynamic references, instead of Value1.
--
-- Revision 1.13  2008/07/20 09:08:59  niklas
-- BT-CH-0138: Handling Relative value of resolved referent.
--
-- Revision 1.12  2008/06/20 10:11:52  niklas
-- BT-CH-0132: Data pointers using Difference (Expr, Cell, Bounds).
--
-- Revision 1.11  2008/06/18 20:52:55  niklas
-- BT-CH-0130: Data pointers relative to initial-value cells.
--
-- Revision 1.10  2007/10/04 19:48:51  niklas
-- BT-CH-0083: Resolving dynamic access to constant data.
--
-- Revision 1.9  2007/10/02 20:37:58  niklas
-- BT-CH-0080: One-bit bitwise Boolean arithmetic operators.
--
-- Revision 1.8  2007/07/26 11:08:52  niklas
-- BT-CH-0066. Fracture assignments.
--
-- Revision 1.7  2006/11/07 15:55:23  niklas
-- Corrected function Bitwise_Not to mask the negated value
-- to the word-size of the "not" operation. Otherwise the value
-- is not only (probably) incorrect but may overflow the range
-- of Processor.Value_T for no good reason.
--
-- Revision 1.6  2006/10/14 13:52:14  niklas
-- Corrected Binary_Operation.Bitwise_Xor, when one operand is
-- all 1's, to use the new function Bitwise_Xor_Ones which knows
-- that the original expression is a binary one while the residual
-- expression is a unary operation. This replaces the wrong use
-- of Binary_Not which assumes that the original expression is
-- a unary "not" and raises constraint-error otherwise.
-- Added the function Unary_Residual_From_Binary to make a unary
-- residual from a binary operation with one remaining non-constant
-- operand.
--
-- Revision 1.5  2006/05/06 06:59:18  niklas
-- BT-CH-0021.
--
-- Revision 1.4  2006/02/27 09:59:47  niklas
-- Extended function Residual (Result_T) to further simplify the
-- residual expression by combining similar terms, including
-- constant cell values from the evaluation domain.
--
-- Revision 1.3  2005/02/16 21:11:36  niklas
-- BT-CH-0002.
--
-- Revision 1.2  2004/08/09 19:51:25  niklas
-- BT-CH-0001.
--
-- Revision 1.1  2004/04/25 16:54:26  niklas
-- First version.
--


with Arithmetic.Algebra;
with Arithmetic.Evaluation.Opt;
with Options.General;
with Output;
with Storage;


package body Arithmetic.Evaluation is


   use type Value_T;
   use type Width_T;


   --
   ---   Levels of evaluation
   --


   function Signed_Value (Item : Known_Cell_Value_T) return Value_T
   is
   begin

      return Signed_Value (Word => Item.Value, Width => Item.Width);

   end Signed_Value;


   function Image (Item : Cell_Value_T) return String
   is
   begin

      case Item.Level is

      when Unknown  =>

         return Unknown_Image;

      when Variable =>

         return Variable_Image;

      when Relative =>

         if Item.Offset >= 0 then

            return Storage.Image (Item.Base) & '+' & Image (Item.Offset);

         else

            return Storage.Image (Item.Base) & Image (Item.Offset);

         end if;

      when Known =>

         return Image (
            Item   => Item.Value,
            Width  => Item.Width,
            Signed => Item.Signed);

      end case;

   end Image;


   function To_Value (
      Word   : Word_T;
      Width  : Width_T;
      Signed : Boolean)
   return Cell_Value_T
   is
   begin

      return (
         Level  => Known,
         Value  => Word and Max_Word (Width),
         Width  => Width,
         Signed => Signed);

   end To_Value;


   function Width_Of (Item : Result_T) return Width_T
   is
   begin

      return Width_Of (Item.Expr);

   end Width_Of;


   function Signed_Value (Item : Known_Result_T) return Value_T
   is
   begin

      return Signed_Value (
         Word  => Item.Value,
         Width => Item.Expr.Width);

   end Signed_Value;


   function Image (Item : Result_T) return String
   is

      Pref : constant String :=
           Level_T'Image (Item.Level)
         & ", "
         & Image (Item.Expr)
         & ", rb "
         & Boolean'Image (Item.Ref_Bounded);

   begin

      case Item.Level is

      when Unknown | Variable =>

         return Pref;

      when Relative =>

         return Pref
            & " = "
            & Storage.Image (Item.Base)
            & " + "
            & Image (Item.Offset);

      when Known =>

         return Pref
            & " ="
            & Image (
                 Item   => Item.Value,
                 Width  => Width_Of (Item),
                 Signed => Item.Signed);

      end case;

   end Image;


   function Image (Item : Target_Result_T) return String
   is

      Prefix : constant String :=
           Target_Level_T'Image (Item.Level)
         & ", rb "
         & Boolean'Image (Item.Ref_Bounded)
         & ", source "
         & Image (Item.Source);

   begin

      case Item.Level is

      when Dynamic_Ref =>

         return Prefix & ", ref " & Storage.References.Image (Item.Ref.all);

      when Known_Cell =>

         return Prefix & ", cell " & Storage.Image (Item.Cell);

      end case;

   end Image;


   function Image (Item : Assignment_Result_T) return String
   is

      Prefix : constant String :=
           Defining_Kind_T'Image (Item.Kind)
         & ", target "
         & Image (Item.Target)
         & "; refined "
         & Boolean'Image (Item.Refined)
         & ", rb "
         & Boolean'Image (Item.Ref_Bounded)
         & ", value "
         & Image (Item.Value);

   begin

      case Item.Kind is

      when Regular | Fracture =>

         return Prefix;

      when Conditional =>

         return Prefix
            & ", if ("
            & Image (Item.Cond)
            & ") then ("
            & Image (Item.Value1)
            & ") else ("
            & Image (Item.Value2);

      end case;

   end Image;


   --
   ---   Creating results and residual expressions
   --


   function Fuzzy_Cond_Level (
      Cond   : Unk_Var_T;
      Value1 : Level_T;
      Value2 : Level_T)
   return Unk_Var_T
   --
   -- The evaluation level of a conditional expression where the
   -- condition value is not (yet) Known and the two alternative
   -- expressions have either Known or Fuzzy levels and are not
   -- known to be the same expression or have the same value (so
   -- the result strictly depends on the condition).
   --
   is
   begin

      case Cond is

      when Unknown =>
         -- The condition is yet Unknown.

         if Value1 = Variable and Value2 = Variable then
            -- Whatever the condition turns out to be, the
            -- final result must be variable.

            return Variable;

         else
            -- One of the alternative expressions is Unknown,
            -- Relative or Known, not Variable, and we may still
            -- find out later which alternative is chosen.

            return Unknown;

         end if;

      when Variable =>
          -- The condition itself is already considered variable,
          -- so we must consider both alternatives.

          if Value1 = Variable or Value2 = Variable then
             -- Variability is and will remain a possible choice.

             return Variable;

          elsif Value1 = Unknown or Value2 = Unknown then
             -- One alternative is Known or Relative, and the other
             -- is Unknown. The Unknown alternative may become Known or
             -- Relative and found to equal the other alternative, so
             -- there is still some (slight) hope that the result
             -- can be Known or Relative.

             return Unknown;

          else
             -- Both alternatives are Known or Relative, but different,
             -- so we have variation between (at least) two values.

             return Variable;

          end if;

      end case;

   end Fuzzy_Cond_Level;


   function Result (
      Value       : Word_T;
      Signed      : Boolean;
      Source      : Expr_Ref;
      Ref_Bounded : Boolean)
   return Result_T
   --
   -- A constant result Value, evaluated from a Source expression,
   -- which may be hinted to be Signed.
   --
   is

      Refined : constant Boolean := Source.Kind /= Const;

   begin

      if Refined and Opt.Trace_Refinement_Path then

         Output.Trace (
             "Source expression "
            & Image (Source)
            & " refined to constant "
            & Image (
                 Item   => Value,
                 Width  => Width_Of (Source),
                 Signed => Signed));

      end if;

      return (
         Level       => Known,
         Expr        => Source,
         Refined     => Refined,
         Ref_Bounded => Ref_Bounded,
         Value       => Value,
         Signed      => Signed);

   end Result;


   function Result (
      Base        : Storage.Cell_T;
      Offset      : Value_T;
      Source      : Expr_Ref;
      Refined     : Boolean;
      Ref_Bounded : Boolean)
   return Result_T
   --
   -- A result of the form Base + Offset, evaluated from a Source
   -- expression.
   --
   is
   begin

      if Refined and Opt.Trace_Refinement_Path then

         Output.Trace (
             "Source expression "
            & Image (Source)
            & " refined to relative "
            & Storage.Image (Base)
            & " + "
            & Image (Offset));

      end if;

      return (
         Level       => Relative,
         Expr        => Source,
         Refined     => Refined,
         Ref_Bounded => Ref_Bounded,
         Base        => Base,
         Offset      => Offset);

   end Result;


   function Result (
      Expr        : Expr_Ref;
      Level       : Unk_Var_T;
      Refined     : Boolean;
      Ref_Bounded : Boolean)
   return Result_T
   --
   -- A non-constant, non-relative expression result.
   --
   is
   begin

      case Level is

      when Unknown =>

         if Refined and Opt.Trace_Refinement_Path then

            Output.Trace (
                "Source expression "
               & Image (Expr)
               & " refined to unknown value.");

         end if;

         return (
            Level       => Unknown,
            Expr        => Expr,
            Refined     => Refined,
            Ref_Bounded => Ref_Bounded);

      when Variable =>

         if Refined and Opt.Trace_Refinement_Path then

            Output.Trace (
                "Source expression "
               & Image (Expr)
               & " refined to variable value.");

         end if;

         return (
            Level       => Variable,
            Expr        => Expr,
            Refined     => Refined,
            Ref_Bounded => Ref_Bounded);

      end case;

   end Result;


   function Result (
      Expr  : Expr_Ref;
      Only  : Result_T;
      Other : Result_T)
   return Result_T
   --
   -- A result that is refined to Only one of two operands in the
   -- source Expression and does not depend on the Other operand.
   -- However, the result shows if the evaluation of the Other
   -- operand has bounded any references in the Other operand (or
   -- in the Only operand).
   --
   -- This function is useful when the result of a binary operation is
   -- just one of its operands (for example, x + 0 = x), but we still
   -- want to record if some dynamic references were bounded in the
   -- other operand, and also that the result is a refined expression.
   --
   is

      Res : Result_T := Only;
      -- The result to be.

   begin

      Res.Refined     := True;
      Res.Ref_Bounded := Res.Ref_Bounded or Other.Ref_Bounded;

      if Opt.Trace_Refinement_Path then

         Output.Trace (
             "Source expression "
            & Image (Expr)
            & " refined to operand "
            & Image (Only));

      end if;

      return Res;

   end Result;


   function "=" (Left : Result_T; Right : Word_T)
   return Boolean
   --
   -- Whether the Left operand is a constant and equal to Right.
   --
   is
   begin

      return Left.Level = Known and then Left.Value = Right;

   end "=";


   function Same (Left, Right : Result_T) return Boolean
   is
   begin

      if Left.Level = Known and Right.Level = Known then
         -- We can compare the known values, not expressions.

         return Left.Value = Right.Value;

      else

         return Left = Right;

         -- For two Relative results we compare the Base cells
         -- and the Offset values. This is safe and rather exact,
         -- giving a false negative only if the Base cells are
         -- different but their initial values are such as to make
         -- the same Base + Offset, which is rather out of the model.
         --
         -- For Unknown and Variable results we compare only
         -- expressions (and levels). This is safe but very
         -- conservative.

      end if;

   end Same;


   function Value_Of (Result : Result_T) return Cell_Value_T
   is
   begin

      case Result.Level is

      when Unknown =>

         return (Level => Unknown);

      when Variable =>

         return (Level => Variable);

      when Relative =>

         return (
            Level  => Relative,
            Base   => Result.Base,
            Offset => Result.Offset);

      when Known =>

         return (
            Level  => Known,
            Value  => Result.Value,
            Width  => Width_Of (Result),
            Signed => Result.Signed);

      end case;

   end Value_Of;


   function To_Condition (Result : Result_T) return Condition_T
   is
   begin

      case Result.Level is

      when Fuzzy_Level_T =>

         return Result.Expr;

      when Known =>

         if Result.Value in Bit_T then

            return Boolean_Cond(Result.Value);

         else

            Output.Fault (
               Location => "Arithmetic.Evaluation.To_Condition",
               Text =>
                    "Non-boolean value"
                  & Output.Field_Separator
                  & Image (Result.Expr)
                  & " = "
                  & Image (Result.Value));

            return Arithmetic.Unknown;

         end if;

      end case;

   end To_Condition;


   function Residual (
      From    : Expr_Ref;
      Operand : Result_T;
      Partly  : Boolean)
   return Result_T
   --
   -- Residual expression From a unary-operation expression, with
   -- the single operand evaluated, but to an Operand expression
   -- (rather than to a Known constant value).
   --
   -- The original expression is returned if partial evaluation
   -- is inhibited by Partly = False, or if the evaluation of
   -- the Operand had no effect on it.
   --
   is

      Residual_Level : Level_T;
      -- The level of the residual expression.

   begin

      case Fuzzy_Level_T (Operand.Level) is

      when Unk_Var_T =>
         -- A unary operation on an Unknown value yields Unknown,
         -- and ditto for a Variable value.

         Residual_Level := Operand.Level;

      when Relative =>
         -- This unary operation on a Relative value is taken to
         -- give a Variable result, since we have not handled this
         -- operation-operand combination specifically.

         Residual_Level := Variable;

      end case;

      if Partly and Operand.Refined then
         -- The operand was partially evaluated and changed
         -- into a residual expression that differs substantially
         -- from the original. Also, partial evaluation is allowed.
         -- We make a new expression that has the same unary operator
         -- and result-width, but the residual Operand:

         return Result (
            Expr => Unary (
               Operation => From.Unary,
               Operand   => Residual (Operand),
               Width     => From.Width),
            Level       => Residual_Level,
            Refined     => Operand.Refined,
            Ref_Bounded => Operand.Ref_Bounded);

      else
         -- Partial evaluation not allowed, or had no significant
         -- effect on the operand.
         --
         -- Return the original expression but with new level
         -- information from the Operand:

         return Result (
             Expr        => From,
             Level       => Residual_Level,
             Refined     => False,
             Ref_Bounded => Operand.Ref_Bounded);

      end if;

   end Residual;


   function Fuzzier (Left, Right : Level_T) return Level_T
   --
   -- The less known level of the binary operation that combines
   -- the Left and Right operands, in the general case (ie.
   -- special cases like x*1 = x are not included).
   --
   is
      Min : constant Level_T := Level_T'Min (Left, Right);
   begin

     if Min = Relative then
        -- The combinations of Relative and Known values that
        -- yield Relative or Known values are considered special
        -- cases.

        return Variable;

     else

        return Min;

     end if;

   end Fuzzier;


   function Residual (
      From   : Expr_Ref;
      Op     : Binary_Op_T;
      Left   : Result_T;
      Right  : Result_T;
      Partly : Boolean)
   return Result_T
   --
   -- Residual binary expression From a binary or ternary operation
   -- expression with the given (residual) binary Op, with Left and
   -- Right operands evaluated, but in which one or both of the two
   -- operands was evaluated to an expression (rather than both to
   -- constants) and for which no special case such as x*1 = x applies.
   -- In this case the result of the expression depends strictly on
   -- both the (residual) operands, in so far as we know.
   --
   -- The original (binary or ternary) expression is returned if
   -- partial evaluation is inhibited by Partly = False, or if the
   -- evaluation of the operands had no effect on them.
   --
   is

      New_Level : constant Level_T := Fuzzier (Left.Level, Right.Level);
      -- The combined evaluation level.

      Refs_Bounded : constant Boolean :=
         Left.Ref_Bounded or Right.Ref_Bounded;
      -- Whether some dynamic references were bounded, Left or Right.

   begin

      if Partly
      and then (
         Left.Refined
      or Right.Refined
      or From.Kind /= Binary_Kind)
      then
         -- One or both operands were partially evaluated and
         -- changed into a residual expression that differs
         -- substantially from the original, or a ternary
         -- operation is refined to a binary operation since the
         -- third operand (not visible here) was eliminated.
         -- Also, partial evaluation (refining) is allowed.
         --
         -- We make a new, refined expression that has the same
         -- operator (or the binary operator equivalent to the
         -- original ternary one) but the residual operands:

         return Result (
            Expr => Binary (
               Operation => Op,
               Left      => Residual (Left ),
               Right     => Residual (Right),
               Width     => From.Width),
            Level       => New_Level,
            Refined     => True,
            Ref_Bounded => Refs_Bounded);

      else
         -- Partial evaluation not allowed, or had no significant
         -- effect on any operand (binary or ternary).
         -- Return the original expression but with new level
         -- information from the operands:

         return Result (
            Expr        => From,
            Level       => New_Level,
            Refined     => False,
            Ref_Bounded => Refs_Bounded);

      end if;

   end Residual;


   function Residual (
      From   : Expr_Ref;
      Left   : Result_T;
      Right  : Result_T;
      Carry  : Result_T;
      Partly : Boolean)
   return Result_T
   --
   -- Residual expression From a ternary operation expression, with
   -- Left, Right, and Carry operands evaluated, but in which one or
   -- more operand was evaluated to an expression (rather than
   -- all to constants) and for which no special case such as
   -- x*1 = x applies. In this case the result of the expression
   -- depends strictly on all the operands, in so far as we know.
   --
   -- The original expression is returned if partial evaluation
   -- is inhibited by Partly = False, or if the evaluation of
   -- the operands had no effect on them.
   --
   is

      New_Level : constant Level_T :=
         Fuzzier (Left.Level, Fuzzier (Right.Level, Carry.Level));
      -- The combined evaluation level.

      Refs_Bounded : constant Boolean :=
            Left.Ref_Bounded
         or Right.Ref_Bounded
         or Carry.Ref_Bounded;
      -- Whether some dynamic references were bounded in
      -- some operand.

      Residue : Expr_T;
      -- The residual expression, if not the same as From.

      Rez : Result_T;  -- TBM, debug

   begin

      if        Partly
      and then (Left.Refined or Right.Refined or Carry.Refined)
      then
         -- One or more operands were partially evaluated into
         -- residual expressions that differ substantially from the
         -- originals. Also, partial evaluation (refining) is allowed.
         -- Thus we make a new (refined) expression that has the same
         -- operator but the residual operands:

         Residue := From.all;

         Residue.L3_Expr := Residual (Left );
         Residue.R3_Expr := Residual (Right);
         Residue.C3_Expr := Residual (Carry);

         Rez := Result (
            Expr        => new Expr_T'(Residue),
            Level       => New_Level,
            Refined     => True,
            Ref_Bounded => Refs_Bounded);

         return Rez;

      else
         -- Partial evaluation not allowed, or had no significant
         -- effect on any operand, so this expression is not refined.
         -- Return the original expression but with new level
         -- information from the operands:

         return Result (
            Expr        => From,
            Level       => New_Level,
            Refined     => False,
            Ref_Bounded => Refs_Bounded);

      end if;

   end Residual;


   function Unary_Residual_From_Binary (
      From     : Expr_Ref;
      Operand  : Result_T;
      Other    : Result_T;
      Unary_Op : Unary_Op_T;
      Partly   : Boolean)
   return Result_T
   --
   -- Residual unary expression From a binary-operation expression
   -- where one Operand was evaluated to an expression (rather than
   -- to a constant value) and the Other operand was evaluated "away"
   -- (to a constant result). The remaining non-constant Operand becomes
   -- the single operand of the residual Unary_Op expression.
   --
   -- There are not many binary operations that turn into a unary
   -- operation when one operand is a constant. One case is a bitwise
   -- "xor" when the constant operand is all ones (2#11..1#) which
   -- turns into a bitwise "not" of the remaining operand.
   --
   -- The original expression is returned if partial evaluation
   -- is inhibited by Partly = False.
   --
   is

      New_Level : constant Level_T := Level_T'Min (Operand.Level, Other.Level);
      -- The combined evaluation level.

      Refs_Bounded : constant Boolean :=
         Operand.Ref_Bounded or Other.Ref_Bounded;
      -- Whether some dynamic references were bounded in
      -- either operand of the original binary expression.

   begin

      if Partly then
         -- Form the partially evaluated residual expression
         -- with the remaining (non-constant) operand, whether or
         -- not the remaining operand was refined by the evaluation.

         return Result (
            Expr => Unary (
               Operation => Unary_Op,
               Operand   => Residual (Operand),
               Width     => From.Width),
            Level       => New_Level,
            Refined     => True,
            Ref_Bounded => Refs_Bounded);

      else
         -- Partial evaluation not allowed.
         -- Return the original expression but with new level
         -- information from the two operands:

         return Result (
            Expr        => From,
            Level       => New_Level,
            Refined     => False,
            Ref_Bounded => Refs_Bounded);

      end if;

   end Unary_Residual_From_Binary;


   --
   ---   Bitwise logical expressions
   --


   function ">=" (Left : Result_T; Right : Width_T)
   return Boolean
   --
   -- Whether the Left operand is a constant and greater than
   -- or equal to the number of bits, Right.
   --
   is
   begin

      return Left.Level = Known and then Left.Value >= Word_T (Right);

   end ">=";


   --
   ---   Evaluation domains (sources of cell values)
   --


   -- overriding
   function Interval (Cell : Cell_T; Under : Domain_T)
   return Storage.Bounds.Interval_T
   is

      Val : constant Cell_Value_T := Value_Of (
         Cell   => Cell,
         Within => Domain_T'Class (Under));
      -- What the domain tells us about the value.

   begin

      case Val.Level is

      when Fuzzy_Level_T =>

         return Storage.Bounds.Universal_Interval;

      when Known =>

         return Storage.Bounds.Singleton (Signed_Value (Val));

      end case;

   end Interval;


   -- overriding
   function Difference (To, From : Cell_T; Under : Domain_T)
   return Storage.Bounds.Interval_T
   is
      use type Storage.Cell_T;

      To_Val : constant Cell_Value_T := Value_Of (
         Cell   => To,
         Within => Domain_T'Class (Under));

      From_Val : constant Cell_Value_T := Value_Of (
         Cell   => From,
         Within => Domain_T'Class (Under));
      --
      -- What the domain tells us of the values of the To
      -- and From cells.

   begin

      if  To_Val.Level   = Known
      and From_Val.Level = Known
      then

         return Storage.Bounds.Singleton (
            Signed_Value (To_Val) - Signed_Value (From_Val));

      elsif (  To_Val.Level   = Relative
      and      From_Val.Level = Relative)
      and then To_Val.Base    = From_Val.Base
      then

         return Storage.Bounds.Singleton (To_Val.Offset - From_Val.Offset);

      else

         return Interval (
            Expr  => Expr (To) - Expr (From),
            Under => Domain_T'Class (Under));
         --
         -- TBM this leaks an Expr_T object.

      end if;

   end Difference;


   -- overriding
   function Interval (Expr : Expr_Ref; Under : Domain_T)
   return Storage.Bounds.Interval_T
   is

      Val : constant Result_T := Eval (
         Expr   => Expr,
         Within => Domain_T'Class (Under),
         Partly => False);
      -- The value of the Expression, Under this domain.

   begin

      case Val.Level is

      when Fuzzy_Level_T =>

         return Storage.Bounds.Universal_Interval;

      when Known =>

         return Storage.Bounds.Singleton (Signed_Value (Val));

      end case;

   end Interval;


   -- overriding
   function Difference (
      To    : Expr_Ref;
      From  : Storage.Cell_T;
      Under : Domain_T)
   return Storage.Bounds.Interval_T
   is
      use type Storage.Cell_T;

      From_Val : constant Cell_Value_T := Value_Of (
         Cell   => From,
         Within => Domain_T'Class (Under));
      --
      -- What the domain tells us of the values of the From cell.

      To_Res : constant Result_T :=
         Eval (Expr => To, Within => Under, Partly => False);
      -- The result of evaluating To, Under this domain.

   begin

      if  To_Res.Level   = Known
      and From_Val.Level = Known
      then

         return Storage.Bounds.Singleton (
            Signed_Value (To_Res) - Signed_Value (From_Val));

      elsif (  To_Res.Level   = Relative
      and      From_Val.Level = Relative)
      and then To_Res.Base    = From_Val.Base
      then

         return Storage.Bounds.Singleton (To_Res.Offset - From_Val.Offset);

      else
         -- Some more heterogeneus combination of To_Res, From_Val.
         -- We may hope to that the complexity cancels out in
         -- the difference expression.

         return Interval (
            Expr  => To - Expr (From),
            Under => Domain_T'Class (Under));
         --
         -- TBM this leaks an Expr_T object.

      end if;

   end Difference;


   --
   ---   Evaluation of expressions and assignments within a domain
   --


   function Bitwise_Xor_Ones (
      Expr    : Expr_Ref;
      Operand : Result_T;
      Other   : Result_T;
      Partly  : Boolean)
   return Result_T
   --
   -- Evaluates a bit-wise exclusive-or ("xor") operation (Expr) where
   -- the Other operand is an all-ones word (2#11...11#), which means
   -- that the operation simply negates the remaining Operand.
   -- The Partly parameter has the same role as in the function Eval.
   --
   is

      Refs_Bounded : constant Boolean :=
         Operand.Ref_Bounded or Other.Ref_Bounded;
      -- Whether some dynamic references were bounded in
      -- either operand of the original Xor expression.

   begin

      if Operand.Level = Known then
         -- We can compute the final value.

         return Result (
            Value       => (not Operand.Value) and Max_Word (Expr.Width),
            Signed      => False,
            Ref_Bounded => Refs_Bounded,
            Source      => Expr);

      else
         -- We have a bitwise-not as the residual expression.

         return Unary_Residual_From_Binary (
            From     => Expr,
            Operand  => Operand,
            Other    => Other,
            Unary_Op => Notw,
            Partly   => Partly);

      end if;

   end Bitwise_Xor_Ones;


   function Unary_Operation (
      Expr    : Expr_Ref;
      Op      : Unary_Op_T;
      Operand : Result_T;
      Partly  : Boolean)
   return Result_T
   --
   -- Evaluates a unary operation (Expr with Unary = Op) on its sole
   -- Operand expressed as a Result_T object and returns the value
   -- as a Result_T. The Partly parameter has the same role as
   -- in the function Eval.
   --
   is

      Value : Word_T;
      -- The result of evaluating the operation on a known operand.

   begin

      if Operand.Level /= Known then
         -- Nothing we can do.

         return Residual (
            From    => Expr,
            Operand => Operand,
            Partly  => Partly);

      else
         -- The operand value is known, so we can evaluate
         -- the operation.

         case Op is

         when Notw =>

            Value := (not Operand.Value) and Max_Word (Expr.Width);

         when Notx =>

            if Operand.Value in Bit_T then

               Value := (not Operand.Value) and 1;

            else

               Output.Fault (
                  Location => "Arithmetic.Evaluation.Unary_Operation",
                  Text     => "Logical negation of " & Image (Operand.Value));

               Value := Operand.Value;

            end if;

         when EqZero =>

            Value := Boolean_Value (Operand.Value = 0);

         when EqOne =>

            Value := Boolean_Value (Operand.Value = 1);

         when Signw =>

            Value := Boolean_Value (
               Negative (Operand.Value, Width_Of (Operand)));

         when Exts =>

            Value := Sign_Extend (
               Value => Operand.Value,
               From  => Width_Of (Operand),
               To    => Expr.Width);

         when Extz =>

            Value := Zero_Extend (
               Value => Operand.Value,
               From  => Width_Of (Operand));

         when Trunl =>

            Value := Truncate_Low (
               Value => Operand.Value,
               To    => Expr.Width);

         when Trunh =>

            Value := Truncate_High (
               Value => Operand.Value,
               From  => Width_Of (Operand),
               To    => Expr.Width);

         end case;

         return Result (
            Value       => Value,
            Signed      => Width_Of (Expr) > 1
                           and then Operand.Signed,
            Source      => Expr,
            Ref_Bounded => Operand.Ref_Bounded);

      end if;

   end Unary_Operation;


   function Binary_Operation (
      Expr   : Expr_Ref;
      Op     : Binary_Op_T;
      Left   : Result_T;
      Right  : Result_T;
      Partly : Boolean)
   return Result_T
   --
   -- Evaluates a binary operation (Expr with Kind Op) on its Left
   -- and Right operands expressed as Result_T objects and returns
   -- the value as a Result_T. The Partly parameter has the same
   -- role as in the function Eval. The Expr expression may also
   -- be a ternary expression in which one operand has been evaluated
   -- to a constant and eliminated, converting the Expr to the
   -- equivalent binary expression Left Op Right.
   --
   is
      use type Storage.Cell_T;

      Width : constant Width_T := Expr.Width;
      -- The width of the result, and usually also of Left and Right.

      Both_Known : constant Boolean :=
         Left.Level = Known and Right.Level = Known;
      -- Whether both operands were evaluated to constants so that
      -- we can concretely execute the Op on the operand values.

      Refs_Bounded : constant Boolean :=
         Left.Ref_Bounded or Right.Ref_Bounded;
      -- Whether some references were bounded, Left or Right.


      procedure Warn_If_Overflow (L, R, Value : Word_T)
      --
      -- Warns if the Op on known Left and Right operand values
      -- causes overflow so that the resulting Value (not yet
      -- limited to this Width) will be wrapped.
      --
      is

         Overflow : Word_T;
         -- Whether (we know that) there is overflow.

      begin

         case Op is

         when Plus    => Overflow := Overflow_From_Plus  (L, R, Width);
         when Minus   => Overflow := Overflow_From_Minus (L, R, Width);

         -- For other operations, we do not check:

         when others  => Overflow := 0;

         end case;

         if Overflow /= 0
         or Value > Max_Word (Width)
         then

            Output.Warning (
                 "Overflow in"
               & Width_T'Image (Width)
               & "-bit expression "
               & Image (Expr)
               & " with operands "
               & Image (L)
               & " and "
               & Image (R)
               & " giving "
               & Image (Value and Max_Word (Width)));

         end if;

      end Warn_If_Overflow;


      function Op_On_Known (L, R : Word_T) return Result_T
      --
      -- The result of Op on known Left and Right operand values.
      --
      is

         Value : Word_T;
         -- The value of the result.

      begin

         case Op is

         when Plus    => Value := L + R;
         when Minus   => Value := L - R;
         when Mulu    => Value := Mulu (L, R, Width);
         when Muls    => Value := Muls (L, R, Width);

         when Plus_C  => Value := Carry_From_Plus    (L, R, Width_Of (Left));
         when Plus_N  => Value := Negative_From_Plus (L, R, Width_Of (Left));
         when Plus_V  => Value := Overflow_From_Plus (L, R, Width_Of (Left));

         when Minus_B => Value := Borrow_From_Minus   (L, R, Width_Of (Left));
         when Minus_C => Value := Carry_From_Minus    (L, R, Width_Of (Left));
         when Minus_N => Value := Negative_From_Minus (L, R, Width_Of (Left));
         when Minus_V => Value := Overflow_From_Minus (L, R, Width_Of (Left));

         when Eq      => Value := Boolean_Value (L = R);

         when Lts     => Value := Boolean_Value (Lts (L, R, Width_Of (Left)));
         when Gts     => Value := Boolean_Value (Gts (L, R, Width_Of (Left)));
         when Les     => Value := Boolean_Value (Les (L, R, Width_Of (Left)));
         when Ges     => Value := Boolean_Value (Ges (L, R, Width_Of (Left)));

         when Ltu     => Value := Boolean_Value (L <  R);
         when Gtu     => Value := Boolean_Value (L >  R);
         when Leu     => Value := Boolean_Value (L <= R);
         when Geu     => Value := Boolean_Value (L >= R);

         when Andx    => Value := Bit_T (L and R);
         when Orx     => Value := Bit_T (L or  R);

         when Andw    => Value := L and R;
         when Orw     => Value := L or  R;
         when Xorw    => Value := L xor R;

         when Slz =>

            Value := Shift_Left (
               Value  => L,
               Amount => Natural (Word_T'Min (R, Word_T (Width))),
               Width  => Width);

         when Srz =>

            Value := Shift_Right (
               Value  => L,
               Amount => Natural (Word_T'Min (R, Word_T (Width))),
               Width  => Width_Of (Left));

         when Sra =>

            Value := Shift_Right_Arithmetic (
               Value  => L,
               Amount => Natural (Word_T'Min (R, Word_T (Width))),
               Width  => Width_Of (Left));

         when Rotl =>

            Value := Rotate_Left (
               Value  => L,
               Amount => Natural (R mod Word_T (Width)),
               Width  => Width);

         when Rotr =>

            Value := Rotate_Right (
               Value  => L,
               Amount => Natural (R mod Word_T (Width)),
               Width  => Width);

         when Conc =>

            Value :=
               Shift_Left (
                  Value  => L,
                  Amount => Natural (Width_Of (Right)),
                  Width  => Width)
               or (R and Max_Word (Width_Of (Right)));

         end case;

         if Opt.Warn_Overflow then

            Warn_If_Overflow (L, R, Value);

         end if;

         return Result (
            Value       => Value and Max_Word (Width),
            Signed      => Width > 1
                           and then (Left.Signed or Right.Signed),
            Source      => Expr,
            Ref_Bounded => Refs_Bounded);

      end Op_On_Known;


      function Residue return Result_T
      --
      -- The residual expression, if needed.
      --
      is
      begin

         return Residual (
            From   => Expr,
            Op     => Op,
            Left   => Left,
            Right  => Right,
            Partly => Partly);

      end Residue;


      -- The following functions are used when at least one of
      -- the operands, Left and Right, is not Known. These functions
      -- check for special cases in which the result is still known,
      -- or where the expression can be simplified to depend only
      -- on one of the operands.


      function Only_Left return Result_T
      --
      -- The result is only the Left operand.
      --
      is
      begin

         return Result (
            Expr  => Expr,
            Only  => Left,
            Other => Right);

      end Only_Left;


      function Only_Right return Result_T
      --
      -- The result is only the Right operand.
      --
      is
      begin

         return Result (
            Expr  => Expr,
            Only  => Right,
            Other => Left);

      end Only_Right;


      function Product return Result_T
      --
      -- Evaluates a multiplication (Mulu or Muls) operation.
      --
      is
      begin

         if Left = 0 or Right = 0 then
            -- Zero times x = zero, for all x.

            return Result (
               Value       => Word_T'(0),
               Signed      => Op = Muls,
               Source      => Expr,
               Ref_Bounded => Refs_Bounded);

         elsif Left = 1 then
            -- One times x = x, for all x.

            return Only_Right;

         elsif Right = 1 then
            -- X times one = x, for all x.

            return Only_Left;

         else

            return Residue;

         end if;

      end Product;


      function Sum return Result_T
      --
      -- Evaluates an addition (Plus) operation.
      --
      is
      begin

         if Left = 0 then
            -- Zero plus x = x, for all x.

            return Only_Right;

         elsif Right = 0 then
            -- X plus zero = x, for all x.

            return Only_Left;

         elsif Left.Level  = Relative
         and   Right.Level = Known
         then
            -- Adding more offset to a relative value.
            -- This is not itself considered a refinement because it
            -- happens for every expression of the form base-cell + constant
            -- and so refinement would not terminate.

            return Result (
               Base        => Left.Base,
               Offset      => Left.Offset
                            + Signed_Value (Right),
               Source      => Expr,
               Refined     => Left.Refined or Right.Refined,
               Ref_Bounded => Refs_Bounded);

         elsif Right.Level = Relative
         and   Left.Level  = Known
         then
            -- Ditto, the other way around.
            -- Not itself considered a refinement; see above.

            return Result (
               Base        => Right.Base,
               Offset      => Right.Offset
                            + Signed_Value (Left),
               Source      => Expr,
               Refined     => Left.Refined or Right.Refined,
               Ref_Bounded => Refs_Bounded);

         else

            return Residue;

         end if;

      end Sum;


      function Difference return Result_T
      --
      -- Evaluates a subtraction (Minus) operation.
      --
      is
      begin

         -- TBA when we add an "invert sign" op:
         -- if Left = 0 then
         --
         --   return Invert_Sign (Right);

         if Right = 0 then
            -- X minus zero = x, for all x.

            return Only_Left;

         elsif Left = Right then
            -- X minus x = zero, for all x.
            -- Perhaps too special a case: same expression.

            return Result (
               Value       => Word_T'(0),
               Signed      => False,
               Source      => Expr,
               Ref_Bounded => Refs_Bounded);

         elsif Left.Level  = Relative
         and   Right.Level = Known
         then
            -- Subtracting some offset from a relative value.
            -- Not itself considered a refinement; see above in
            -- the function Sum.

            return Result (
               Base        => Left.Base,
               Offset      => Left.Offset
                            - Signed_Value (Right),
               Source      => Expr,
               Refined     => Left.Refined or Right.Refined,
               Ref_Bounded => Refs_Bounded);

         elsif (  Left.Level  = Relative
         and      Right.Level = Relative)
         and then Left.Base   = Right.Base
         then
            -- Difference between two offsets from the same Base.

            return Result (
               Value       => Unsigned_Word (Left.Offset - Right.Offset, Width),
               Signed      => True,
               Source      => Expr,
               Ref_Bounded => Refs_Bounded);

         else

            return Residue;

         end if;

      end Difference;


      function Relation (Equal : Boolean) return Result_T
      --
      -- Common evaluation of all relational operators, with
      -- Equal giving the result in case Left = Right.
      --
      is
      begin

         if Left = Right then

            return Result (
               Value       => Boolean_Value(Equal),
               Signed      => False,
               Source      => Expr,
               Ref_Bounded => Refs_Bounded);

         else

            return Residue;

         end if;

      end Relation;


      procedure Report_Many_Bits
      --
      -- Reports the fault that the width of a Boolean expression
      -- is more than one bit.
      --
      is
      begin

         Output.Fault (
            Location => "Arithmetic.Evaluation.Binary_Operation",
            Text     =>
                 "Boolean operation width is"
               & Width_T'Image (Width)
               & Output.Field_Separator
               & Image (Expr));

      end Report_Many_Bits;


      function Bitwise_And return Result_T
      --
      -- Evaluates a bit-wise conjunction ("and") operation.
      --
      is
      begin

         if Left = 0 or Right = 0 then
            -- Zero and x = x and zero = zero, for all x.

            return Result (
               Value       => Word_T'(0),
               Signed      => False,
               Source      => Expr,
               Ref_Bounded => Refs_Bounded);

         elsif Left = Max_Word (Width) then
            -- "All ones" and x = x, for all x.

            return Only_Right;

         elsif Right = Max_Word (Width) then
            -- X and "all ones" = x, for all x.

            return Only_Left;

         elsif Left = Right then
            -- X and x = x, for all x.

            return Only_Left;

         else

            return Residue;

         end if;

      end Bitwise_And;


      function Bitwise_Or return Result_T
      --
      -- Evaluates a bit-wise disjunction ("or") operation.
      --
      is
      begin

         if Left  = Max_Word (Width)
         or Right = Max_Word (Width)
         then
            -- "All ones" or x = x or "all ones" = "all ones", for all x.

            return Result (
               Value       => Max_Word (Width),
               Signed      => False,
               Source      => Expr,
               Ref_Bounded => Refs_Bounded);

         elsif Left = 0 then
            -- Zero or x = x, for all x.

            return Only_Right;

         elsif Right = 0 then
            -- X or zero = x, for all x.

            return Only_Left;

         elsif Left = Right then
            -- X or x = x, for all x.

            return Only_Left;

         else

            return Residue;

         end if;

      end Bitwise_Or;


      function Bitwise_Xor return Result_T
      --
      -- Evaluates a bit-wise exclusive disjunction ("xor") operation.
      --
      is
      begin

         if Left = Max_Word (Width) then
            -- "All ones" xor x = not x, for all x.

            return Bitwise_Xor_Ones (
               Expr    => Expr,
               Operand => Right,
               Other   => Left,
               Partly  => Partly);

         elsif Right = Max_Word (Width) then
            -- X xor "all ones" = not x, for all x.

            return Bitwise_Xor_Ones (
               Expr    => Expr,
               Operand => Left,
               Other   => Right,
               Partly  => Partly);

         elsif Left = 0 then
            -- Zero xor x = x, for all x.

            return Only_Right;

         elsif Right = 0 then
            -- X xor zero = x, for all x.

            return Only_Left;

         elsif Left = Right then
            -- X xor x = zero, for all x.

            return Result (
               Value       => Word_T'(0),
               Signed      => False,
               Source      => Expr,
               Ref_Bounded => Refs_Bounded);

         else

            return Residue;

         end if;

      end Bitwise_Xor;


      function Shift_Left_Z return Result_T
      --
      -- Evaluates a shift left, zero fill (Slz).
      -- The Left operand is the value to be shifted, the Right
      -- operand is the amount of shift (number of bits).
      --
      is
      begin

         if      Left   = 0
         or else Right >= Width
         then
            -- Shifting left a zero for any number of bits, or
            -- shifting left any value for at least its width (with
            -- zero fill) both result in zero.

            return Result (
               Value       => Word_T'(0),
               Signed      => False,
               Source      => Expr,
               Ref_Bounded => Refs_Bounded);

         elsif Right = 0 then
            -- Shifting any value for zero bits leaves the same value.

            return Only_Left;

         else

            return Residue;

         end if;

      end Shift_Left_Z;


      function Shift_Right_Z return Result_T
      --
      -- Evaluates a shift right, zero fill (Srz).
      -- The Left operand is the value to be shifted, the Right
      -- operand is the amount of shift (number of bits).
      --
      is
      begin

         if      Left   = 0
         or else Right >= Width
         then
            -- Shifting right a zero for any number of bits, or
            -- shifting right any value for at least its width (with
            -- zero fill) both result in zero.

            return Result (
               Value       => Word_T'(0),
               Signed      => False,
               Source      => Expr,
               Ref_Bounded => Refs_Bounded);

         elsif Right = 0 then
            -- Shifting any value for zero bits leaves the same value.

            return Only_Left;

         else

            return Residue;

         end if;

      end Shift_Right_Z;


      function Shift_Right_A return Result_T
      --
      -- Evaluates a shift right, sign fill (Sra).
      -- The Left operand is the value to be shifted, the Right
      -- operand is the amount of shift (number of bits).
      --
      is
      begin

         if Left = 0 then
            -- Shifting right a zero for any number of bits gives zero.

            return Result (
               Value       => Word_T'(0),
               Signed      => True,
               Source      => Expr,
               Ref_Bounded => Refs_Bounded);

         elsif Left = Max_Word (Width) then
            -- Shifting right (arithmetic) an all-ones value, by
            -- any amount, gives an all-ones value.

            return Result (
               Value       => Max_Word (Width),
               Signed      => True,
               Source      => Expr,
               Ref_Bounded => Refs_Bounded);

         elsif Right = 0 then
            -- Shifting any value for zero bits leaves the same value.

            return Only_Left;

         else

            return Residue;

         end if;

      end Shift_Right_A;


      function Rots return Result_T
      --
      -- Common cases for left/right rotations (Rotl and Rotr).
      -- The Left operand is the value to be rotated, the Right
      -- operand is the amount of rotation (number of bits).
      --
      is
      begin

         if Left = 0 then
            -- Rotating a zero for any number of bits gives zero still.

            return Result (
               Value       => Word_T'(0),
               Signed      => False,
               Source      => Expr,
               Ref_Bounded => Refs_Bounded);

         elsif Left = Max_Word (Width) then
            -- Rotating all ones for any number of bits gives all ones still.

            return Result (
               Value       => Max_Word (Width),
               Signed      => False,
               Source      => Expr,
               Ref_Bounded => Refs_Bounded);

         elsif Right = 0 then
            -- Rotating any value for zero bits leaves the same value.
            -- TBA TBD same if Right mod Width = 0.

            return Only_Left;

         else

            return Residue;

         end if;

      end Rots;


      function Concatenation return Result_T
      --
      -- Evaluates the concatenation of Left and Right.
      --
      is
      begin

         if Left = 0 then
            -- This operation just zero-extends Right.

            return Unary_Residual_From_Binary (
               From     => Expr,
               Operand  => Right,
               Other    => Left,
               Unary_Op => Extz,
               Partly   => Partly);

         else

            return Residue;

         end if;

      end Concatenation;


   begin  -- Binary_Operation

      if Both_Known then

         return Op_On_Known (L => Left.Value, R => Right.Value);

      else
         -- One or both of Left and Right is another expression,
         -- not a known constant.

         case Op is

         when Mulu | Muls =>

            return Product;

         when Plus =>

            return Sum;

         when Minus =>

            return Difference;

         when Plus_C | Plus_V =>

            if Left = 0 or Right = 0 then
               -- Adding nothing cannot give a carry or overflow.

               return Result (
                  Value       => Bit_T'(0),
                  Signed      => False,
                  Source      => Expr,
                  Ref_Bounded => Refs_Bounded);

            else

               return Residue;

            end if;

         when Minus_B | Minus_V =>

            if Right = 0 then
               -- Subtracting nothing cannot give a borrow or overflow.

               return Result (
                  Value       => Bit_T'(0),
                  Signed      => False,
                  Source      => Expr,
                  Ref_Bounded => Refs_Bounded);

            else

               return Residue;

            end if;

         when Minus_C =>

            if Right = 0 then
               -- Subtracting nothing gives a carry.

               return Result (
                  Value       => Bit_T'(1),
                  Signed      => False,
                  Source      => Expr,
                  Ref_Bounded => Refs_Bounded);

            else

               return Residue;

            end if;

         when Plus_N =>

            if Left = 0 then
               -- The sign is that of Right.

               return Unary_Residual_From_Binary (
                  From     => Expr,
                  Operand  => Right,
                  Other    => Left,
                  Unary_Op => Signw,
                  Partly   => Partly);

            elsif Right = 0 then
               -- The sign is that of Left.

               return Unary_Residual_From_Binary (
                  From     => Expr,
                  Operand  => Left,
                  Other    => Right,
                  Unary_Op => Signw,
                  Partly   => Partly);

            else

               return Residue;

            end if;

         when Minus_N =>

            if Right = 0 then
               -- The sign is that of Left.

               return Unary_Residual_From_Binary (
                  From     => Expr,
                  Operand  => Left,
                  Other    => Right,
                  Unary_Op => Signw,
                  Partly   => Partly);

            else

               return Residue;

            end if;

         -- TBD: Whether in the relational operators, below, we
         -- should have special cases when exactly one of the
         -- operands is constant and either Value_T'First or
         -- Value_T'Last (or the processor-specific smallest
         -- and largest values TBD). In such cases the result for
         -- many comparisons is known in principle and does not
         -- depend on the other operand.
         --
         -- TBD: Whether the domain should also include information
         -- on the intrinsic range of cells (e.g. signed vs unsigned,
         -- number of bits in cell, symbol-table information on the
         -- range of the variable, or ranges deduced from data flow).
         -- Such info could also imply the result of comparisons
         -- without more exact knowledge of cell values.
         --
         -- TBC: It is doubtful if the special cases for Left = Right
         -- (representation identity) are worth-while.
         --
         -- TBD: If it is worth-while to add cases for comparing two
         -- offsets to the same Base register.

         when Lts | Gts | Ltu | Gtu =>

            return Relation (Equal => False);

         when Eq | Les | Ges | Leu | Geu =>

            return Relation (Equal => True);

         when Andx =>

            if Width /= 1 then

               Report_Many_Bits;

            end if;

            return Bitwise_And;

         when Orx =>

            if Width /= 1 then

               Report_Many_Bits;

            end if;

            return Bitwise_Or;

         when Andw =>

            return Bitwise_And;

         when Orw =>

            return Bitwise_Or;

         when Xorw =>

            return Bitwise_Xor;

         when Slz =>

            return Shift_Left_Z;

         when Srz =>

            return Shift_Right_Z;

         when Sra =>

            return Shift_Right_A;

         when Rotl | Rotr =>

            return Rots;

         when Conc =>

            return Concatenation;

         end case;

      end if;

   exception

   when Word_Out_Of_Range =>

      return (
         Level       => Variable,
         Refined     => False,
         Ref_Bounded => Refs_Bounded,
         Expr        => Expr);

   end Binary_Operation;


   Neutral_Third : constant array (Ternary_Op_T) of Bit_T := (
      Plus         => 0,
      Minus_With_B => 0,
      Minus_With_C => 1,
      Plus_C       => 0,
      Plus_N       => 0,
      Plus_V       => 0,
      Minus_B      => 0,
      Minus_C      => 1,
      Minus_BN     => 0,
      Minus_CN     => 1,
      Minus_BV     => 0,
      Minus_CV     => 1);
   --
   -- The "neutral" value of the third (carry/borrow) operand for
   -- each of the ternary operations. That is, the carry/borrow
   -- value that has no effect on the total result.


   Equivalent_Binary : constant array (Ternary_Op_T) of Binary_Op_T := (
      Plus         => Plus,
      Minus_With_B => Minus,
      Minus_With_C => Minus,
      Plus_C       => Plus_C,
      Plus_N       => Plus_N,
      Plus_V       => Plus_V,
      Minus_B      => Minus_B,
      Minus_C      => Minus_C,
      Minus_BN     => Minus_N,
      Minus_CN     => Minus_N,
      Minus_BV     => Minus_V,
      Minus_CV     => Minus_V);
   --
   -- The binary operation that is equivalent to a given ternary
   -- operation when the third (carry/borrow) operand of the ternary
   -- operation is Neutral_Third.


   function Ternary_Operation (
      Expr   : Expr_Ref;
      Op     : Ternary_Op_T;
      Left   : Result_T;
      Right  : Result_T;
      Carry  : Result_T;
      Partly : Boolean)
   return Result_T
   --
   -- Evaluates a ternary operation (Expr with Kind Op) on its Left,
   -- Right, and Carry operands expressed as Result_T objects and returns
   -- the value as a Result_T. The Partly parameter has the same
   -- role as in the function Eval.
   --
   is
      use type Storage.Cell_T;

      All_Known : constant Boolean :=
             Left.Level  = Known
         and Right.Level = Known
         and Carry.Level = Known;
      -- Whether all operands were evaluated to constants so that
      -- we can concretely execute the Op on the operand values.

      Refs_Bounded : constant Boolean :=
            Left.Ref_Bounded
         or Right.Ref_Bounded
         or Carry.Ref_Bounded;
      -- Whether some references were bounded, in Left, Right, or Carry.


      procedure Warn_If_Overflow (L, R, C, Value : Word_T)
      --
      -- Warns if the Op on known Left, Right, and Carry/Borrow operand
      -- values causes overflow so that the resulting Value (not yet
      -- limited to this Width) will be wrapped.
      --
      is

         Width : constant Width_T := Width_Of (Left);

         Overflow : Word_T;
         -- Whether (we know that) there is overflow.

      begin

         case Op is

         when Plus =>

            Overflow := Overflow_From_Plus  (L, R, C, Width);

         when Minus_With_B =>

            Overflow := Overflow_From_Minus_Borrow (L, R, C, Width);

         when Minus_With_C =>

            Overflow := Overflow_From_Minus_Carry (L, R, C, Width);

         -- For other operations, we do not check:

         when others =>

            Overflow := 0;

         end case;

         if Overflow /= 0
         or Value > Max_Word (Expr.Width)
         then

            Output.Warning (
                 "Overflow in"
               & Width_T'Image (Expr.Width)
               & "-bit expression "
               & Image (Expr)
               & " with operands "
               & Image (L)
               & ", "
               & Image (R)
               & ", and "
               & Image (C)
               & " giving "
               & Image (Value and Max_Word (Expr.Width)));

         end if;

      end Warn_If_Overflow;


      function Op_On_Known (L, R, C : Word_T) return Result_T
      --
      -- The result of Op on known Left, Right, and Carry operand values.
      --
      is

         Width : constant Width_T := Width_Of (Left);

         Value : Word_T;
         -- The value of the result.

      begin

         case Op is

         when Plus         => Value := L + R + C;
         when Minus_With_B => Value := L - R - C;
         when Minus_With_C => Value := L - R + C - 1;

         when Plus_C =>

            Value := Carry_From_Plus (L, R, C, Width);

         when Plus_N =>

            Value := Negative_From_Plus (L, R, C, Width);

         when Plus_V =>

            Value := Overflow_From_Plus (L, R, C, Width);

         when Minus_B =>

            Value := Borrow_From_Minus (L, R, C, Width);

         when Minus_C =>

            Value := Carry_From_Minus (L, R, C, Width);

         when Minus_BN =>

            Value := Negative_From_Minus_Borrow  (L, R, C, Width);

         when Minus_CN =>

            Value := Negative_From_Minus_Carry (L, R, C, Width);

         when Minus_BV =>

            Value := Overflow_From_Minus_Borrow (L, R, C, Width);

         when Minus_CV =>

            Value := Overflow_From_Minus_Carry (L, R, C, Width);

         end case;

         if Opt.Warn_Overflow then

            Warn_If_Overflow (L, R, C, Value);

         end if;

         return Result (
            Value       => Value and Max_Word (Expr.Width),
            Signed      => Expr.Width > 1
                           and then (Left.Signed or Right.Signed),
            Source      => Expr,
            Ref_Bounded => Refs_Bounded);

      end Op_On_Known;


   begin  -- Ternary_Operation

      if All_Known then

         return Op_On_Known (
            L => Left.Value,
            R => Right.Value,
            C => Carry.Value);

      elsif Carry = Neutral_Third(Op) then
         -- Equivalent to the binary operation on Left and Right.

         return Binary_Operation (
            Expr   => Expr,
            Op     => Equivalent_Binary(Op),
            Left   => Left,
            Right  => Right,
            Partly => Partly);

      else
         -- One operand is not Known, and Carry is not "neutral".
         -- Give up for now. TBA handling other partly known values.

         return Residual (
            From   => Expr,
            Left   => Left,
            Right  => Right,
            Carry  => Carry,
            Partly => Partly);

      end if;

   exception

   when Word_Out_Of_Range =>

      return (
         Level       => Variable,
         Refined     => False,
         Ref_Bounded => Refs_Bounded,
         Expr        => Expr);

   end Ternary_Operation;


   function Value_Ref (
      Expr   : Expr_Ref;
      Within : Domain_T'Class;
      Partly : Boolean)
   return Result_T
   --
   -- Evaluates a dynamic reference Expression (Expr.Kind = Ref) Within
   -- a domain and with possible partial evaluation. The evaluation
   -- returns a value, ie. the reference Expression is assumed to
   -- occur within an expression and not as the target of an assignment.
   --
   is
      use type Storage.Cell_T;
      use type Storage.References.Boundable_Ref;

      Pointee : Storage.Cell_T;
      -- The cell to which the Expr refers.

      Const_Cell : Boolean;
      -- Whether the Pointee is known as a constant cell.

      Const_Value : Word_T;
      -- The value of Pointee when it is a Const_Cell.

      Cell_Value: Cell_Value_T;
      -- The value of the Pointee cell.

      Bounded_Reference : Storage.References.Boundable_Ref;
      -- The partly bounded reference, when it is not bounded to a
      -- single Pointee cell.

   begin

      Pointee := Storage.References.Referent (
         Ref   => Expr.Ref.all,
         Under => Within);

      if Pointee /= Storage.No_Cell then
         -- The reference is resolved to a single cell.

         if Options.General.Trace_Resolution then

            Output.Trace (
                 "Resolved pointer "
               & Storage.References.Image (Expr.Ref.all)
               & " to cell "
               & Storage.Image (Pointee)
               & " under "
               & Image (Within));

         end if;

         Storage.References.Check_Constancy (
            Cell  => Pointee,
            Ref   => Expr.Ref.all,
            Const => Const_Cell,
            Value => Arithmetic_Base.Word_T (Const_Value));

         if Const_Cell then
            -- The Pointee has a constant value, whatever the
            -- computation does.

            Cell_Value := (
               Level  => Known,
               Value  => Const_Value,
               Width  => Expr.Ref.Width,
               Signed => False);
               --
               -- The choice of an unsigned value is arbitrary, TBM.
               -- If the "Signed" flag is retained, the choice should
               -- be made by Check_Constancy.

         else
            -- The Pointee is a variable cell, but its value may
            -- be defined by the computation:

            Cell_Value := Value_Of (Pointee, Within);

         end if;

         if Cell_Value.Level = Known then
            -- The referent even has a known value, gee!

            return Result (
               Value       => Cell_Value.Value,
               Signed      => Cell_Value.Signed,
               Source      => Expr,
               Ref_Bounded => True);

         elsif Cell_Value.Level = Relative then
            -- The referent has a value of the form Base + Offset.

            return Result (
               Base        => Cell_Value.Base,
               Offset      => Cell_Value.Offset,
               Source      => Expr,
               Refined     => True,
               Ref_Bounded => True);

         elsif Partly then
            -- The referent cell has no known value, or has
            -- possibly a variable value, but we can still
            -- refine away the pointer.

            return Result (
               Expr        => Arithmetic.Expr (Pointee),
               Level       => Cell_Value.Level,
               Refined     => True,
               Ref_Bounded => True);

         else
            -- The reference cell has no known value, or has
            -- possibly a variable value, and the expression is
            -- not to be refined.

            return Result (
               Expr        => Expr,
               Level       => Cell_Value.Level,
               Refined     => False,
               Ref_Bounded => True);

         end if;

      elsif not Partly then
         -- The reference is unresolved and partial evaluation is
         -- not requested.

         return Result (
            Expr        => Expr,
            Level       => Variable,
            Refined     => False,
            Ref_Bounded => False);

      else
         -- We can try to bound the unresolved reference a bit.

         Storage.References.Apply (
            Bounds => Within,
            Upon   => Expr.Ref.all,
            Giving => Bounded_Reference);

         if Bounded_Reference = null then
            -- No change in the reference.

            return Result (
               Expr        => Expr,
               Level       => Variable,
               Refined     => False,
               Ref_Bounded => False);

         else
            -- Managed to refine the reference.

            return Result (
               Expr        => Reference (Bounded_Reference),
               Level       => Variable,
               Refined     => True,
               Ref_Bounded => True);

         end if;

      end if;

   end Value_Ref;


   --
   ---   Provided operations
   --


   function Eval (
      Expr   : Expr_Ref;
      Within : Domain_T'Class;
      Partly : Boolean)
   return Result_T
   is
      use type Storage.Cell_T;

      Cell_Value: Cell_Value_T;
      -- The value of the cell, when the Expr is a cell.

   begin

      case Expr.Kind is

      when Opaque =>

         return Result (
            Expr        => Expr,
            Level       => Variable,
            Refined     => False,
            Ref_Bounded => False);

      when Const =>

         return Result (
            Value       => Expr.Value,
            Source      => Expr,
            Ref_Bounded => False,
            Signed      => Expr.Signed);

      when Cell =>

         Cell_Value := Value_Of (Expr.Cell, Within);

         case Cell_Value.Level is

         when Unk_Var_T =>

            return Result (
               Expr        => Expr,
               Level       => Cell_Value.Level,
               Refined     => False,
               Ref_Bounded => False);

         when Relative =>

            return Result (
               Base        => Cell_Value.Base,
               Offset      => Cell_Value.Offset,
               Source      => Expr,
               Refined     => Cell_Value.Base /= Expr.Cell,
               Ref_Bounded => False);

         when Known =>

            return Result (
               Value       => Cell_Value.Value,
               Signed      => Cell_Value.Signed,
               Source      => Expr,
               Ref_Bounded => False);

         end case;

      when Ref =>

         return Value_Ref (
            Expr   => Expr,
            Within => Within,
            Partly => Partly);

      when Unary_Kind =>

         return Unary_Operation (
            Expr    => Expr,
            Op      => Expr.Unary,
            Operand => Eval (Expr.Expr, Within, Partly),
            Partly  => Partly);

      when Binary_Kind =>

         return Binary_Operation (
            Expr   => Expr,
            Op     => Expr.Binary,
            Left   => Eval (Expr.L_Expr, Within, Partly),
            Right  => Eval (Expr.R_Expr, Within, Partly),
            Partly => Partly);

      when Ternary_Kind =>

         return Ternary_Operation (
            Expr   => Expr,
            Op     => Expr.Ternary,
            Left   => Eval (Expr.L3_Expr, Within, Partly),
            Right  => Eval (Expr.R3_Expr, Within, Partly),
            Carry  => Eval (Expr.C3_Expr, Within, Partly),
            Partly => Partly);

      end case;

   end Eval;


   function Eval (
      Target : Variable_T;
      Within : Domain_T'Class;
      Partly : Boolean)
   return Target_Result_T
   is
      use type Storage.Cell_T;
      use type Storage.References.Boundable_Ref;

      Pointee : Storage.Cell_T;
      -- The cell to which the Target refers, when Target is a reference.

      Bounded_Reference : Storage.References.Boundable_Ref;
      -- The partly bounded pointer, when Target is a reference and the
      -- reference is not bounded to a single Pointee cell.

   begin

      case Variable_Kind_T (Target.Kind) is

      when Cell =>
         -- A known cell.

         return (
            Level       => Known_Cell,
            Ref_Bounded => False,
            Source      => Target,
            Cell        => Target.Cell);

      when Ref =>
         -- A dynamic reference to a cell.

         Pointee := Storage.References.Referent (
            Ref   => Target.Ref.all,
            Under => Within);

         if Pointee /= Storage.No_Cell then
            -- The reference is resolved to a single cell.

            return (
               Level       => Known_Cell,
               Ref_Bounded => True,
               Source      => Target,
               Cell        => Pointee);

         elsif not Partly then
            -- The reference is unresolved and partial evaluation is
            -- not requested.

            return (
               Level       => Dynamic_Ref,
               Ref_Bounded => False,
               Source      => Target,
               Ref         => Target.Ref);

         else
            -- We can try to bound the unresolved reference a bit.

            Storage.References.Apply (
               Bounds => Within,
               Upon   => Target.Ref.all,
               Giving => Bounded_Reference);

            if Bounded_Reference = null then
               -- No change in the reference.

               return (
                  Level       => Dynamic_Ref,
                  Ref_Bounded => False,
                  Source      => Target,
                  Ref         => Target.Ref);

            else
               -- Managed to refine the reference.

               return (
                  Level       => Dynamic_Ref,
                  Ref_Bounded => True,
                  Source      => Target,
                  Ref         => Bounded_Reference);

            end if;

         end if;

     end case;

   end Eval;


   function Eval (
      Assignment : Assignment_T;
      Within     : Domain_T'Class;
      Target     : Target_Result_T;
      Partly     : Boolean)
   return Assignment_Result_T
   is

      Refined : Boolean := Target.Ref_Bounded;
      -- Whether some parts of the assignment were refined in
      -- the evaluation. Initialized to show the possible refinement
      -- of a dynamic-reference Target.

      Refs_Bounded : Boolean := Target.Ref_Bounded;
      -- Whether some dynamic references were bounded in the evaluation.
      -- Initialized to show this for the Target.

      Value : Result_T;
      -- The single value of an unconditional assignment, or the
      -- final result (choice) of a conditional assignment.

      Cond, Value1, Value2 : Result_T;
      -- The results of evaluating the parts of a conditional assignment.

   begin

      -- Evaluate the assigned expression(s):

      case Defining_Kind_T (Assignment.Kind) is

      when Arithmetic.Regular =>

         Value := Eval (
            Expr   => Assignment.Value,
            Within => Within,
            Partly => Partly);

         Refined      := Refined      or Value.Refined;
         Refs_Bounded := Refs_Bounded or Value.Ref_Bounded;

         return (
            Kind        => Regular,
            Refined     => Refined or Refs_Bounded,
            Ref_Bounded => Refs_Bounded,
            Target      => Target,
            Value       => Value);

      when Arithmetic.Conditional =>

         Cond := Eval (
            Expr   => Assignment.Cond,
            Within => Within,
            Partly => Partly);

         Refined      := Refined      or Cond.Refined;
         Refs_Bounded := Refs_Bounded or Cond.Ref_Bounded;

         if Cond.Level /= Known
         or else Cond.Value = True_Value
         then
            -- Condition not known or True.
            -- We must evaluate the first alternative value.

            Value1 := Eval (
               Expr   => Assignment.Value1,
               Within => Within,
               Partly => Partly);

            Refined      := Refined      or Value1.Refined;
            Refs_Bounded := Refs_Bounded or Value1.Ref_Bounded;

         end if;

         if Cond.Level /= Known
         or else Cond.Value = False_Value
         then
            -- Condition not known or False.
            -- We must evaluate the second alternative value.

            Value2 := Eval (
               Expr   => Assignment.Value2,
               Within => Within,
               Partly => Partly);

            Refined      := Refined      or Value2.Refined;
            Refs_Bounded := Refs_Bounded or Value2.Ref_Bounded;

         end if;

         if Cond.Level = Known then
            -- The condition is constant, so we can here
            -- choose which value to use (and we have in
            -- fact evaluated just this value):

            if Cond.Value = True_Value then

               return (
                  Kind        => Regular,
                  Refined     => True,
                  Ref_Bounded => Refs_Bounded,
                  Target      => Target,
                  Value       => Value1);

            else

               return (
                  Kind        => Regular,
                  Refined     => True,
                  Ref_Bounded => Refs_Bounded,
                  Target      => Target,
                  Value       => Value2);

            end if;

         elsif Same (Value1, Value2) then
            -- We have the same value whatever the condition:

            return (
               Kind        => Regular,
               Refined     => True,
               Ref_Bounded => Refs_Bounded,
               Target      => Target,
               Value       => Value1);

         else
            -- The condition is not known, so neither is the
            -- assigned value. The level of the value can be
            -- computed, but not the Value itself:

            case Fuzzy_Cond_Level (
               Cond   => Cond.Level,
               Value1 => Value1.Level,
               Value2 => Value2.Level)
            is

            when Unknown =>

               Value := (
                  Level       => Unknown,
                  Refined     => False,
                  Ref_Bounded => False,
                  Expr        => null);

            when Variable =>

               Value := (
                  Level       => Variable,
                  Refined     => False,
                  Ref_Bounded => False,
                  Expr        => null);

            end case;

            return (
               Kind        => Conditional,
               Refined     => Refined or Refs_Bounded,
               Ref_Bounded => Refs_Bounded,
               Target      => Target,
               Value       => Value,
               Cond        => Cond,
               Value1      => Value1,
               Value2      => Value2);

         end if;

      when Arithmetic.Fracture =>
         -- There is no value to evaluate.

         Value := Eval (
            Expr   => Arithmetic.Unknown,
            Within => Within,
            Partly => Partly);

         Refined      := Refined      or Value.Refined;
         Refs_Bounded := Refs_Bounded or Value.Ref_Bounded;
         -- It would be quite surprising if Value.Refined or
         -- Value.Ref_Bounded were True, but let's be very formal.

         return (
            Kind        => Fracture,
            Refined     => Refined or Refs_Bounded,
            Ref_Bounded => Refs_Bounded,
            Target      => Target,
            Value       => Value);

      end case;

   end Eval;


   function Eval (
      Assignment : Assignment_T;
      Within     : Domain_T'Class;
      Partly     : Boolean)
   return Assignment_Result_T
   is
   begin

      return Eval (
         Assignment => Assignment,
         Within     => Within,
         Target     => Eval (Assignment.Target, Within, Partly),
         Partly     => Partly);

   end Eval;


   --
   ---   Understanding the results
   --


   function Same (Left : Result_T; Right : Expr_Ref)
   return Boolean
   is

      Konst : Boolean;
      Diff  : Value_T;
      -- From Find_Difference between Left.Base and Right.

   begin

      case Left.Level is

      when Unk_Var_T =>

         return Left.Expr = Right;
         --
         -- The Right expression is identical with the Left result
         -- (as expression references, so this is a conservative
         -- but incomplete comparison).
         -- Comparing Expr_Ref values means that we check representation
         -- identity, not semantic identity. This is a safe check
         -- ("equal" is correct) but not a complete check (two
         -- expressions may be "not equal" by this check but still
         -- be semantically equivalent).

      when Relative =>

         Algebra.Find_Difference (
            From  => Left.Base,
            To    => Right,
            Const => Konst,
            Diff  => Diff);

         return Konst and then Diff = Left.Offset;
         --
         -- The Right expression is of the form Left.Base + Diff,
         -- or Left.Base - (-Diff), or Diff + Left.Base. Safe but
         -- incomplete equality.

      when Known =>

         return Right.Kind = Const and then Right.Value = Left.Value;
         --
         -- The Right expression is a constant expression with
         -- the same value as the Left constant result.
         --
         -- This is a stronger check of semantic identity than
         -- the above check of Expr_Refs, but still incomplete.
         -- However, if the check falsely claims that Left and
         -- Right are not the same, then the Right expression must
         -- always evaluate to Left.Value, so a Const expression
         -- derived from Left.Value will be simpler than the
         -- Right expression, but fully equivalent.

      end case;

   end Same;


   function Same (Left : Assignment_Result_T; Right : Assignment_T)
   return Boolean
   is
   begin

      if Left.Kind /= Right.Kind
      or Left.Ref_Bounded
      then

         return False;

      else
         -- The Kind was not changed and no references were bounded.
         -- The assumption Left = Eval (Right, Partly => True) means
         -- that Left and Right have the same target variable.
         -- Thus, the changes are in the value(s), if anywhere.

         case Left.Kind is

         when Regular =>

            return Same (Left.Value, Right.Value);

         when Conditional =>

            return Same (Left.Cond  , Right.Cond)
               and Same (Left.Value1, Right.Value1)
               and Same (Left.Value2, Right.Value2);

         when Fracture =>

            return True;

         end case;

      end if;

   end Same;


   function Identity (Result : Assignment_Result_T)
   return Boolean
   is
      use type Storage.Cell_T;
   begin

      return      Result.Kind            = Regular
         and then Result.Value.Level    in Fuzzy_Level_T
         and then Result.Value.Expr.Kind = Cell
         and then Result.Target.Level    = Known_Cell
         and then Result.Value.Expr.Cell = Result.Target.Cell;

   end Identity;


   function Residual (From : Result_T) return Expr_Ref
   is

      Combined : Expr_Ref;
      -- An unknown or variable result after combining terms and
      -- thus possibly refining it.

   begin

      -- To form the residual expression from a partially or
      -- completely evaluated result, we try to re-use as much
      -- as possible of the existing expressions.
      --
      -- The evaluation level of the residual expression, although
      -- not explicitly returned, is logically the same as From.Level.

      case From.Level is

      when Unknown | Variable =>
         -- The residual expression is not constant yet, but it may
         -- have gained new constant parts, so it may be useful to try
         -- to combine similar terms, including combining multiple
         -- constant terms that represent known cell values from the
         -- evaluation domain.

         Combined := Algebra.Combine_Terms (From.Expr);

         if Opt.Trace_Refinement_Path
         and then Combined /= From.Expr
         then

            Output.Trace (
                 "Source expression "
               & Image (From.Expr)
               & " refined by combining terms to "
               & Image (Combined));

         end if;

         return Combined;

      when Relative =>
         -- The residual expression is the unknown value of a Base
         -- cell plus a constant Offset. The Base cell is a constant
         -- throughout the subprogram, so we can replace the original
         -- expression with Base + Offset.

         return Cell  (From.Base)
              + Const (Value  => From.Offset,
                       Width  => From.Expr.Width,
                       Signed => True);

      when Known =>
         -- The residual expression will be a constant.

         if        From.Expr.Kind   = Const
         and then (From.Expr.Value  = From.Value
         and       From.Expr.Signed = From.Signed)
         then
            -- The source Expr is a constant expression with the
            -- same value as was evaluated.
            -- No need to create a new expression, it would
            -- be the same as From.Expr.

            return From.Expr;

         else
            -- No such luck, so we create a new constant
            -- expression to hold the evaluated value.

            return Const (
               Value  => From.Value,
               Width  => From.Expr.Width,
               Signed => From.Signed);

         end if;

      end case;

   end Residual;


   function Target_Cell (From : Target_Result_T) return Cell_T
   is
   begin

      case From.Level is
      when Dynamic_Ref => return Storage.No_Cell;
      when Known_Cell  => return From.Cell;
      end case;

   end Target_Cell;


   function Residual (From : Target_Result_T)
   return Variable_T
   is
   begin

      if not From.Ref_Bounded then
         -- The target was not modified by evaluation.

         return From.Source;

      else
         -- The target was resolved, at least to some extent.

         case From.Level is

         when Dynamic_Ref =>

            return Reference (From.Ref);

         when Known_Cell =>

            return Cell (From.Cell);

         end case;

      end if;

   end Residual;


   function Residual (From : Assignment_Result_T)
   return Assignment_T
   is

      Target : constant Variable_T := Residual (From.Target);
      -- The residual target.

   begin

      case From.Kind is

      when Regular =>

         return (
            Kind   => Regular,
            Target => Target,
            Value  => Residual (From.Value));

      when Conditional =>

         return (
            Kind   => Conditional,
            Target => Target,
            Cond   => Residual (From.Cond),
            Value1 => Residual (From.Value1),
            Value2 => Residual (From.Value2));

      when Fracture =>

         return (
            Kind   => Fracture,
            Target => Target);

      end case;

   end Residual;


end Arithmetic.Evaluation;
