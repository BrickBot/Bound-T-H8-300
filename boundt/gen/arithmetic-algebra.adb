-- Arithmetic.Algebra (body)
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
-- $Revision: 1.17 $
-- $Date: 2015/10/24 20:05:44 $
--
-- $Log: arithmetic-algebra.adb,v $
-- Revision 1.17  2015/10/24 20:05:44  niklas
-- Moved to free licence.
--
-- Revision 1.16  2010-01-02 20:28:07  niklas
-- BT-CH-0211: Corrected operations on Bounded_Sum_T.
--
-- Revision 1.15  2010-01-01 12:36:23  niklas
-- BT-CH-0209: Add_Combined_Terms uses correct widths internally.
--
-- Revision 1.14  2009-11-27 11:28:06  niklas
-- BT-CH-0184: Bit-widths, Word_T, failed modular analysis.
--
-- Revision 1.13  2009/04/16 16:29:23  niklas
-- BT-CH-0171: Stack pointers in call effects.
--
-- Revision 1.12  2009/03/04 13:05:07  niklas
-- Corrected Find_Difference (Cell, Expr, ..) to consider the trivial
-- case where the expression is just the cell itself (zero difference).
--
-- Revision 1.11  2008/06/18 20:52:54  niklas
-- BT-CH-0130: Data pointers relative to initial-value cells.
--
-- Revision 1.10  2007/10/12 18:19:25  niklas
-- Added function Plus_Difference, for cleaner updates of coupled
-- cells such as stack-height cells. Added procedure Add_Coupled_Update
-- for such purposes (where Update_Couple does not fit in). Modified
-- Update_Couple to use Add_Coupled_Update. Note that this now uses
-- Plus_Difference instead of Difference.
--
-- Revision 1.9  2007/07/26 11:08:52  niklas
-- BT-CH-0066. Fracture assignments.
--
-- Revision 1.8  2007/07/21 18:18:39  niklas
-- BT-CH-0064. Support for AVR/IAR switch-handler analysis.
--
-- Revision 1.7  2007/05/02 09:30:34  niklas
-- Added procedure Find_Difference.
--
-- Revision 1.6  2007/03/09 13:52:22  niklas
-- Added function Difference and procedure Update_Couple to help with
-- coupled cells such as a stack pointer and the local stack height.
--
-- Revision 1.5  2006/10/24 08:44:29  niklas
-- BT-CH-0028.
--
-- Revision 1.4  2006/08/22 12:42:06  niklas
-- Added functions Same_Offset and Opposite_Offset.
--
-- Revision 1.3  2006/02/27 09:53:00  niklas
-- Added the function Combine_Terms to simplify a given expression
-- by combining similar terms (constant terms and variable terms)
-- both on the top level and in subexpressions.
-- To support Combine_Terms, extended Bounded_Sum_T to detect if
-- the sum contains combined terms or Unknown terms.
-- Changed To_Expr (Bounded_Sum_T) to return Unknown if the
-- sum has an Unknown term.
--
-- Revision 1.2  2005/03/24 18:19:17  niklas
-- Added simplification of affine expressions by combining terms
-- with the same variable (Cell or Ref). This is implemented by the
-- types Bounded_Sum_T, Term_T, Terms_T and the related operations.
--
-- Revision 1.1  2004/04/25 16:54:25  niklas
-- First version.
--


with Arithmetic.Algebra.Opt;
with Output;


package body Arithmetic.Algebra is


   use type Value_T, Word_T;


   --
   ---   Checking for additions and subtractions of constants (offsets)
   --


   function Plus_Or_Minus (Expr : Expr_Ref) return Boolean
   --
   -- Whether the Expr is a binary "+" or a binary "-"-
   --
   is
   begin

      return Expr.Kind = Binary_Kind
      and then (Expr.Binary = Plus
             or Expr.Binary = Minus);

   end Plus_Or_Minus;


   function Constant_Offset (
      From : Cell_T;
      To   : Expr_Ref)
   return Expr_Ref
   is
   begin

      if not Plus_Or_Minus (To) then
         -- Not of the form X +- Y.

         return Unknown;

      elsif To.L_Expr = From and To.R_Expr.Kind = Const then
         -- From +- Const

         if To.Binary = Plus then
            -- From + Const.

            return To.R_Expr;

         else
            -- From - Const.
            -- The offset is -Const.

            return Const (
               Value  => - Signed_Value (To.R_Expr),
               Width  => To.R_Expr.Width,
               Signed => True);

         end if;

      elsif Is_Constant (To.L_Expr) and To.R_Expr = From
      and   Op_Is (Plus, To)
      then
         -- Const + From

         return To.L_Expr;

      else
         -- The form of To is not recognized as From + Offset.

         return Unknown;

      end if;

   end Constant_Offset;


   function Same_Offset (
      From : Storage.Cell_T;
      To   : Expr_Ref;
      Base : Expr_Ref)
   return Expr_Ref
   is
   begin

      if not Plus_Or_Minus (To) then
         -- Not of the form X +- Y.

         return Unknown;

      elsif To.L_Expr = From then
         -- From +- Offset

         if To.Binary = Plus then
            -- From + Offset.

            return Base + To.R_Expr;

         else
            -- From - Offset.

            return Base - To.R_Expr;

         end if;

      elsif To.R_Expr = From
      and   Op_Is (Plus, To)
      then
         -- Offset + From

         return To.L_Expr + Base;

      else
         -- The form of To is not recognized as From + Offset.

         return Unknown;

      end if;

   end Same_Offset;


   function Opposite_Offset (
      From : Storage.Cell_T;
      To   : Expr_Ref;
      Base : Expr_Ref)
   return Expr_Ref
   is
   begin

      if not Plus_Or_Minus (To) then
         -- Not of the form X +- Y.

         return Unknown;

      elsif To.L_Expr = From then
         -- From +- Offset

         if To.Binary = Plus then
            -- From + Offset.

            return Base - To.R_Expr;

         else
            -- From - Offset.

            return Base + To.R_Expr;

         end if;

      elsif To.R_Expr = From
      and   Op_Is (Plus, To)
      then
         -- Offset + From

         return Base - To.L_Expr;

      else
         -- The form of To is not recognized as From + Offset.

         return Unknown;

      end if;

   end Opposite_Offset;


   procedure Split_Affine (
      Expr         : in     Expr_Ref;
      Addend       :    out Value_T;
      Multiplier   :    out Value_T;
      Multiplicand :    out Expr_Ref)
   is

      Residue : Expr_Ref := Expr;
      -- Progressively deconstructed Expr.

   begin

      -- Pessimistic initial values:

      Addend       := 0;
      Multiplier   := 1;

      -- Is the whole expression constant?

      if Residue.Kind = Const then
         -- Const.

         Addend     := Signed_Value (Expr);
         Multiplier := 0;
         Residue    := Zero (Expr.Width);

      end if;

      -- Is there a constant addend?

      if Plus_Or_Minus (Residue) then
         -- <something> +- <something>

         if Residue.L_Expr.Kind = Const then
            -- Const +- <more>

            Addend := Signed_Value (Residue.L_Expr);

            if Residue.Binary = Minus then
               -- Invert the sign of the multiplier for the R_Expr.

               Multiplier := -Multiplier;

            end if;

            Residue := Residue.R_Expr;

         elsif Residue.R_Expr.Kind = Const then
            -- <more> +- Const

            Addend := Signed_Value (Residue.R_Expr);

            if Residue.Binary = Minus then
               -- The addend is a subtrahend.

               Addend := -Addend;

            end if;

            Residue := Residue.L_Expr;

         end if;

      end if;

      -- Is there a constant multiplier?

      if Op_Is (Muls, Residue) then
         -- <something> * <something>

         if Residue.L_Expr.Kind = Const then
            -- Const * <more>

            Multiplier := Multiplier * Signed_Value (Residue.L_Expr);

            Residue := Residue.R_Expr;

         elsif Residue.R_Expr.Kind = Const then
            -- <more> * Const

            Multiplier := Multiplier * Signed_Value (Residue.R_Expr);

            Residue := Residue.L_Expr;

         end if;

      elsif Can_Shift_By_Mul (Residue) then
         -- <Left> slz <Right> = 2**Right * Left

         Multiplier := Multiplier * (2 ** Natural (Residue.R_Expr.Value));

         Residue := Residue.L_Expr;

      end if;

      -- Whatever is left:

      Multiplicand := Residue;

   end Split_Affine;


   --
   ---   Simplification of sums of terms
   --


   function "*" (Factor : Value_T; Variable : Expr_Ref) return Term_T
   is
   begin

      return (Variable => Variable, Factor => Factor);

   end "*";


   function Image (Item : Term_T) return String
   is
   begin

      return '[' & Image (Item.Factor) & '*' & Image (Item.Variable) & ']';

   end Image;


   function Image (Item : Terms_T) return String
   is
   begin

      if Item'Length = 0 then

         return "none";

      elsif Item'Length = 1 then

         return Image (Item(Item'First));

      else

         return Image (Item(Item'First))
              & " + "
              & Image (Item(Item'First + 1 .. Item'Last));

      end if;

   end Image;


   function Image (Item : Bounded_Sum_T) return String
   --
   -- For human consumption.
   --
   is
   begin

      return
           "Bounded_Sum,"
         & Width_T'Image (Item.Width)
         & " bits, max"
         & Natural'Image (Item.Max_Terms)
         & " terms, opaque "
         & Boolean'Image (Item.Opaque)
         & ", has const "
         & Boolean'Image (Item.Has_Const)
         & ", combined "
         & Boolean'Image (Item.Combined)
         & ", const "
         & Image (Item.Const)
         & ", terms "
         & Image (Item.Terms(1 .. Item.Length));

   end Image;


   function Not_Null (Sum : Bounded_Sum_T) return Boolean
   is
   begin

      return Sum.Opaque
          or Sum.Has_Const
          or Sum.Length > 0;

   end Not_Null;


   function Is_Opaque (Sum : Bounded_Sum_T) return Boolean
   is
   begin

      return Sum.Opaque;

   end Is_Opaque;


   function Is_Combined (Within : Bounded_Sum_T) return Boolean
   is
   begin

      return Within.Combined;

   end Is_Combined;


   function Is_Const (Sum : Bounded_Sum_T) return Boolean
   is

      Const : Boolean := not Sum.Opaque;
      -- The result, initially only showing that an opaque
      -- value is not constant.

   begin

      for T in 1 .. Sum.Length loop

         exit when not Const;
         -- No need to look further.

         Const := Const and Sum.Terms(T).Factor = 0;

      end loop;

      return Const;

   end Is_Const;


   function Value (Sum : Bounded_Sum_T) return Word_T
   is
   begin

      return Sum.Const;

   end Value;


   function Signed_Value (Sum : Bounded_Sum_T) return Value_T
   is
   begin

      return Signed_Value (Sum.Const, Sum.Width);

   end Signed_Value;


   procedure Add_Opaque_Term (To : in out Bounded_Sum_T)
   is
   begin

      if Not_Null (To) then
         -- There are already some terms in the sum.
         -- Adding an opaque term is considered to "combine" terms.

         To.Combined := True;

      end if;

      To.Opaque := True;

   end Add_Opaque_Term;


   procedure Add (
      Const : in     Word_T;
      To    : in out Bounded_Sum_T)
   is
   begin

      To.Const := (To.Const + Const) and Max_Word (To.Width);

      if To.Has_Const or To.Opaque then
         -- The sum already has a constant term or an opaque term.

         To.Combined := True;

      else

         To.Has_Const := True;

      end if;

   end Add;


   procedure Add (
      Factor : in     Value_T;
      Const  : in     Word_T;
      To     : in out Bounded_Sum_T)
   --
   -- Adds Factor * Const To the sum.
   --
   is

      Abs_Add : constant Word_T :=
         Unsigned_Word (abs Factor, To.Width) * Const;
      -- The absolute value of the addend.

   begin

      if Factor > 0 then

         Add (
            Const => Abs_Add,
            To    => To);

      else
         -- Factor < 0.

         Add (
            Const => Opposite_Sign (Abs_Add, To.Width),
            To    => To);

      end if;

   end Add;


   function Same (X, Y : Variable_T) return Boolean
   --
   -- Whether the two variables X, Y and "identical" in the sense
   -- defined for combining two Terms (see the description of the
   -- operation Add (Term_T, Bounded_Sum_T)).
   --
   is
   begin

      return X = Y or else X.all = Y.all;
      --
      -- The first condition (X = Y) checks for reference equality.
      -- The second condition (X.all = Y.all) covers the case of
      -- Cells and Refs, but may also show that other expressions are
      -- the same, for example two expressions that are computed by
      -- the same operator from the same (reference-equal) operands.

   end Same;


   procedure Add (
      Term : in     Term_T;
      To   : in out Bounded_Sum_T)
   is

      Combined : Boolean := False;
      -- Whether the Term was combined with an existing term.

   begin

      if Term.Variable.Kind = Opaque then
         -- Adding an opaque term.

         Add_Opaque_Term (To => To);

      elsif To.Opaque then
         -- Adding any term to an opaque sum is considered
         -- to "combine terms".

         To.Combined := True;

      else
         -- Adding a non-opaque term to a non-opaque sum.
         -- Try to combine with an existing term:

         for T in 1 .. To.Length loop

            if Same (Term.Variable, To.Terms(T).Variable) then

               Combined := True;

               To.Terms(T).Factor := To.Terms(T).Factor + Term.Factor;

               exit;

            end if;

         end loop;

         if Combined then
            -- The Term was combined with an existing similar Term.

            To.Combined := True;

         elsif To.Length < To.Max_Terms then
            -- The Term has a new Variable for this sum, and
            -- the sum has room for this new Term.

            To.Length := To.Length + 1;

            To.Terms(To.Length) := Term;

         else
            -- The Term has a new Variable for this sum, but
            -- the sum does not have room for this new Term.

            Output.Fault (
               Location => "Arithmetic.Algebra.Add",
               Text     =>
                    "No room to add "
                  & Image (Term)
                  & " to "
                  & Image (To));

            raise Too_Many_Terms;

         end if;

      end if;

   end Add;


   procedure Add (
      Terms : in     Terms_T;
      To    : in out Bounded_Sum_T)
   is
   begin

      for T in Terms'Range loop

         Add (Term => Terms(T), To => To);

      end loop;

   end Add;


   procedure Add (
      Sum : in     Bounded_Sum_T;
      To  : in out Bounded_Sum_T)
   is
   begin

      if Sum.Opaque or To.Opaque then
         -- The result is opaque.

         if (To.Opaque  and Not_Null (Sum))
         or (Sum.Opaque and Not_Null (To ))
         then
            -- Adding something to an opaque sum is
            -- considered to "combine terms".

            To.Combined := True;

         end if;

         To.Opaque := True;

      else
         -- Adding two non-opaque sums.

         if Sum.Has_Const then

            Add (
               Const => Sum.Const,
               To    => To);

         end if;

         for T in 1 .. Sum.Length loop

            Add (
               Term => Sum.Terms(T),
               To   => To);

         end loop;

         if Sum.Combined then

            To.Combined := True;

         end if;

      end if;

   end Add;


   procedure Add (
      Factor : in     Value_T;
      Expr   : in     Expr_Ref;
      Term   : in     Expr_Ref;
      To     : in out Bounded_Sum_T;
      Rest   : in out Expr_Ref)
   is

      R_Factor : Value_T;
      -- The factor for the right sub-expression of Plus or Minus.


      procedure All_To_Rest
      --
      -- Adds Factor * Expr to Rest, because nothing could be
      -- added To the sum.
      --
      is
      begin

         if Term /= null then
            -- We already have Term = Factor * Expr.

            Accumulate (
               Factor => 1,
               Expr   => Term,
               Sum    => Rest);

         else
            -- We may have to compute Factor * Expr.

            Accumulate (
               Factor => Factor,
               Expr   => Expr,
               Sum    => Rest);

         end if;

      end All_To_Rest;


   begin  -- Add

      if Factor = 0 then
         -- Nothing to add. To and Rest are unchanged.

         null;

      else

         case Expr.Kind is

         when Opaque =>

            Rest := Unknown;
            -- This is the same as Rest + Factor * Expr.

         when Const =>

            Add (Factor => Factor, Const => Expr.Value, To => To);

         when Cell | Ref =>

            Add (Term => Factor * Expr, To => To);

         when Unary_Kind =>

            case Expr.Unary is

            when Notx | EqZero | EqOne | Signw =>

               Output.Fault (
                  Location => "Arithmetic.Algebra.Add",
                  Text     =>
                       "Expression contains Boolean operator"
                     & Output.Field_Separator
                     & Image (Expr));

            when Notw | Exts | Extz | Trunl | Trunh =>

               All_To_Rest;

            end case;

         when Binary_Kind =>

            case Expr.Binary is

            when Mulu | Muls =>

               if Expr.L_Expr.Kind = Const then

                  Add (
                     Factor => Signed_Value (Expr.L_Expr) * Factor,
                     Expr   => Expr.R_Expr,
                     Term   => Expr,
                     To     => To,
                     Rest   => Rest);

               elsif Expr.R_Expr.Kind = Const then

                  Add (
                     Factor => Signed_Value (Expr.R_Expr) * Factor,
                     Expr   => Expr.L_Expr,
                     Term   => Expr,
                     To     => To,
                     Rest   => Rest);

               else
                  -- Here we could improve the implementation by algebraically
                  -- expanding the product of the left and right terms. TBA.

                  All_To_Rest;

               end if;

            when Slz =>

               if Can_Shift_By_Mul (Expr) then
                  -- The Expr is equivalent to 2**R_Expr * L_Expr.

                  Add (
                     Factor => (2 ** Natural (Expr.R_Expr.Value)) * Factor,
                     Expr   => Expr.L_Expr,
                     Term   => Expr,
                     To     => To,
                     Rest   => Rest);

               else

                  All_To_Rest;

               end if;

            when Conc =>

               if Can_Shift_By_Mul (Word_T (Width_Of (Expr.R_Expr))) then
                  -- The Expr is equivalent to W * L_Expr + R_Expr,
                  -- where W is 2**Width_Of (R_Expr).

                  Add (
                     Factor => (2 ** Natural (Width_Of (Expr.R_Expr))) * Factor,
                     Expr   => Extz (Expr.L_Expr, Expr.Width),
                     Term   => null,
                     To     => To,
                     Rest   => Rest);

                  Add (
                     Factor => Factor,
                     Expr   => Extz (Expr.R_Expr, Expr.Width),
                     Term   => null,
                     To     => To,
                     Rest   => Rest);

               else

                  All_To_Rest;

               end if;

            when Plus | Minus =>

               if Expr.Binary = Plus then R_Factor := Factor;
                                     else R_Factor := - Factor;
               end if;

               Add (
                  Factor => Factor,
                  Expr   => Expr.L_Expr,
                  Term   => null,
                  To     => To,
                  Rest   => Rest);

               Add (
                  Factor => R_Factor,
                  Expr   => Expr.R_Expr,
                  Term   => null,
                  To     => To,
                  Rest   => Rest);

            when Binary_Boolean_Op_T =>

               Output.Fault (
                  Location => "Arithmetic.Algebra.Add",
                  Text     =>
                       "Expression contains Boolean operator"
                     & Output.Field_Separator
                     & Image (Expr));

            when Andw | Orw | Xorw | Srz | Sra | Rotl | Rotr =>
               -- Cannot (in general) add anything To the sum.
               -- TBD TBC Sra as division of Factor by 2**N, if no remainder.

               All_To_Rest;

            end case;

         when Ternary_Kind =>

            case Expr.Ternary is

            when Plus =>

               -- Left + Right:

               Add (
                  Factor => Factor,
                  Expr   => Expr.L3_Expr,
                  Term   => null,
                  To     => To,
                  Rest   => Rest);

               Add (
                  Factor => Factor,
                  Expr   => Expr.R3_Expr,
                  Term   => null,
                  To     => To,
                  Rest   => Rest);

               -- ... + Carry:

               Add (
                  Factor => Factor,
                  Expr   => Extz (Expr.C3_Expr, To.Width),
                  Term   => null,
                  To     => To,
                  Rest   => Rest);

            when Minus_With_B =>

               -- Left - Right:

               Add (
                  Factor => Factor,
                  Expr   => Expr.L3_Expr,
                  Term   => null,
                  To     => To,
                  Rest   => Rest);

               Add (
                  Factor => - Factor,
                  Expr   => Expr.R3_Expr,
                  Term   => null,
                  To     => To,
                  Rest   => Rest);

               -- ... - Borrow:

               Add (
                  Factor => - Factor,
                  Expr   => Extz (Expr.C3_Expr, To.Width),
                  Term   => null,
                  To     => To,
                  Rest   => Rest);

            when Minus_With_C =>

               -- Left - Right:

               Add (
                  Factor => Factor,
                  Expr   => Expr.L3_Expr,
                  Term   => null,
                  To     => To,
                  Rest   => Rest);

               Add (
                  Factor => - Factor,
                  Expr   => Expr.R3_Expr,
                  Term   => null,
                  To     => To,
                  Rest   => Rest);

               -- ... + Carry - 1:

               Add (
                  Factor => Factor,
                  Expr   => Extz (Expr.C3_Expr, To.Width),
                  Term   => null,
                  To     => To,
                  Rest   => Rest);

               Add (Factor => - Factor, Const => 1, To => To);

            when Ternary_Boolean_Op_T =>

               Output.Fault (
                  Location => "Arithmetic.Algebra.Add",
                  Text     =>
                       "Expression contains Boolean operator"
                     & Output.Field_Separator
                     & Image (Expr));

            end case;

         end case;

      end if;

   end Add;


   function To_Expr (Sum : Bounded_Sum_T) return Expr_Ref
   is

      Result : Expr_Ref;
      -- The cumulated result.

   begin  -- To_Expr

      if Sum.Opaque then
         -- The result is opaque.

         Result := Unknown;

      else
         -- No opaque terms.

         Result := Zero (Sum.Width);

         -- Add the variable terms:

         for T in 1 .. Sum.Length loop

            Accumulate ( 
               Factor => Sum.Terms(T).Factor,
               Expr   => Sum.Terms(T).Variable,
               Sum    => Result);

         end loop;

         -- Add the constant term:

         if Sum.Const /= 0 then
            -- Some constant term to add.

            Accumulate (
               Factor => Sign (Sum.Const, Sum.Width),
               Expr   => Const (
                  Value  => Abs_Value (Sum.Const, Sum.Width),
                  Width  => Sum.Width,
                  Signed => False),
               Sum => Result);

         end if;

      end if;

      return Result;

   end To_Expr;


   function Length (Terms : Terms_T) return Natural
   --
   -- The total length of the term expressions.
   --
   is

      Total : Natural := 0;
      -- The total length.

   begin

      for T in Terms'Range loop

         Total := Total + Length (Terms(T).Variable);

      end loop;

      return Total;

   end Length;


   function To_Expr (
      Terms : Terms_T;
      Const : Word_T := 0;
      Width : Width_T)
   return Expr_Ref
   is

      Sum : Bounded_Sum_T (Width => Width, Max_Terms => Length (Terms));
      -- A working sum.

      Rest : Expr_Ref := Zero (Width);
      -- The total remainder of the added Terms.

      Result : Expr_Ref;
      -- The result, Sum + Rest.

   begin

      Add (Const => Const, To => Sum);

      for T in Terms'Range loop

         Add (
            Factor => Terms(T).Factor,
            Expr   => Terms(T).Variable,
            Term   => null,
            To     => Sum,
            Rest   => Rest);

      end loop;

      Result := To_Expr (Sum);

      if Rest /= Value_T'(0) then
         -- Something could not be put in the Sum.

         Result := Result + Rest;

      end if;

      return Result;

   exception

   when X : Too_Many_Terms =>

      Output.Exception_Info (
         Text =>
              "Arithmetic.Algebra.To_Expr (Terms)"
            & Output.Field_Separator
            & Image (Terms),
         Occurrence => X);

      raise;

   end To_Expr;


   function Possibly_Combined_Terms (
      Expr : Expr_Ref;
      Sum  : Bounded_Sum_T)
   return Expr_Ref
   --
   -- Given an Expression and the Sum that represents an attempt
   -- to combine terms in the Expression, returns either the
   -- simplified expression from Sum, if some terms could be
   -- combined, or otherwise the original Expression.
   --
   is
   begin

      if Is_Combined (Sum) then
         -- Some terms could be combined when the Sum was
         -- added up, so Sum should be simpler than Expr.

         return To_Expr (Sum);

      else
         -- No terms could be combined, Expr is a simple as
         -- we can make it.

         return Expr;

      end if;

   end Possibly_Combined_Terms;


   procedure Add_Combined_Terms (
      Factor : in     Value_T;
      Expr   : in     Expr_Ref;
      To     : in out Bounded_Sum_T)
   --
   -- Simplifies the expression V = To + Factor * Expr by combining
   -- similar terms in the subpexpressions of Expr, and then adding
   -- the result, times Factor, To the given sum, again combining
   -- similar terms. On return, To = V.
   --
   is

      Multiply : constant Boolean := Op_Is (Mulu, Expr) or Op_Is (Muls, Expr);
      -- The top operator is multiplication.

      R_Factor : Value_T;
      -- The factor for the right sub-expression of Plus or Minus.

   begin

      case Expr.Kind is

      when Opaque =>

         Add_Opaque_Term (To => To);

      when Const =>

         Add (Factor => Factor, Const => Expr.Value, To => To);

      when Cell | Ref =>

         Add (Term => Factor * Expr, To => To);

      when Unary_Kind =>
         -- Simplify the operand and apply the unary operator.

         declare

            Sum : Bounded_Sum_T (Expr.Expr.Width, To.Max_Terms);
            -- The sum generated from the operand.

            Sub_Expr : Expr_Ref;
             -- The possibly simplified operand.

         begin

            -- Simplify the operand:

            Add_Combined_Terms (
               Factor => 1,
               Expr   => Expr.Expr,
               To     => Sum);

            Sub_Expr := Possibly_Combined_Terms (
               Expr => Expr.Expr,
               Sum  => Sum);

            if Is_Opaque (Sum) then
               -- An opaque result.

               Add_Opaque_Term (To => To);

            elsif Sub_Expr = Expr.Expr then
               -- No simplification.

               Add (Term => Factor * Expr, To => To);

            else
               -- The operand was simplified. Add a term
               -- that applies the same unary operation to
               -- the simplified operand.

               Add (
                  Term => Factor * Unary (
                     Operation => Expr.Unary,
                     Operand   => Sub_Expr,
                     Width     => Expr.Width),
                  To => To);

            end if;

         end;

      when Binary_Kind =>

         case Expr.Binary is

         when Plus | Minus =>
            -- We can continue to add both subexpressions To
            -- the same sum, but may only have to change the
            -- sign of the right subexpression.

            if Expr.Binary = Plus then R_Factor := Factor;
                                  else R_Factor := - Factor;
            end if;

            Add_Combined_Terms (
               Factor => Factor,
               Expr   => Expr.L_Expr,
               To     => To);

            Add_Combined_Terms (
               Factor => R_Factor,
               Expr   => Expr.R_Expr,
               To     => To);

         when others =>
            -- Binary operator other than Plus or Minus.
            -- Simplify the operands, then (for Mulu and Muls) see
            -- if one factor is constant and try to combine the expanded
            -- product into To.

            declare

               L_Sum : Bounded_Sum_T (Expr.L_Expr.Width, To.Max_Terms);
               R_Sum : Bounded_Sum_T (Expr.R_Expr.Width, To.Max_Terms);
               -- The sums generated from the operands.

               L_Expr : Expr_Ref;
               R_Expr : Expr_Ref;
               -- The possibly simplified operands.

               Simple : Expr_Ref;
               -- The possibly simplified Expr.

            begin

               -- Simplify the operands:

               Add_Combined_Terms (
                  Factor => 1,
                  Expr   => Expr.L_Expr,
                  To     => L_Sum);

               Add_Combined_Terms (
                  Factor => 1,
                  Expr   => Expr.R_Expr,
                  To     => R_Sum);

               if Is_Opaque (L_Sum) or Is_Opaque (R_Sum) then
                  -- An opaque result.

                  Add_Opaque_Term (To => To);

               elsif Multiply and Is_Const (L_Sum) then
                  -- A constant times a subexpression, so we can
                  -- continue to build the same sum.

                  Add_Combined_Terms (
                     Factor => Factor * Signed_Value (L_Sum),
                     Expr   => Possibly_Combined_Terms (
                        Expr => Expr.R_Expr,
                        Sum  => R_Sum),
                     To => To);

               elsif Multiply and Is_Const (R_Sum) then
                  -- A subexpression times a constant, so we can
                  -- continue to build the same sum.

                  Add_Combined_Terms (
                     Factor => Factor * Signed_Value (R_Sum),
                     Expr   => Possibly_Combined_Terms (
                        Expr => Expr.L_Expr,
                        Sum  => L_Sum),
                     To => To);

               else
                  -- The Expr will be one term in To, because we
                  -- cannot decompose it into smaller terms.
                  --
                  -- TBA: Cancel like terms on both sides of a
                  -- Relation_Kind_T operation.

                  L_Expr := Possibly_Combined_Terms (
                     Expr => Expr.L_Expr,
                     Sum  => L_Sum);

                  R_Expr := Possibly_Combined_Terms (
                     Expr => Expr.R_Expr,
                     Sum  => R_Sum);

                  if  L_Expr = Expr.L_Expr
                  and R_Expr = Expr.R_Expr
                  then
                     -- No change in the operands, so we keep
                     -- the same Expr.

                     Simple := Expr;

                  else
                     -- One or both operands were simplified, so we
                     -- make a new Simple expression with the same
                     -- operation as in Expr but with the simpler
                     -- operands.

                     Simple := Binary (
                        Operation => Expr.Binary,
                        Left      => L_Expr,
                        Right     => R_Expr,
                        Width     => Expr.Width);

                  end if;

                  Add (Term => Factor * Simple, To => To);

               end if;

            end;

         end case;

      when Ternary_Kind =>

         case Expr.Ternary is

         when Plus =>

            -- Left + Right + Carry:

            Add_Combined_Terms (
               Factor => Factor,
               Expr   => Expr.L3_Expr,
               To     => To);

            Add_Combined_Terms (
               Factor => Factor,
               Expr   => Expr.R3_Expr,
               To     => To);

            Add_Combined_Terms (
               Factor => Factor,
               Expr   => Extz (Expr.C3_Expr, To.Width),
               To     => To);

         when Minus_With_B =>

            -- Left - Right - Borrow:

            Add_Combined_Terms (
               Factor => Factor,
               Expr   => Expr.L3_Expr,
               To     => To);

            Add_Combined_Terms (
               Factor => - Factor,
               Expr   => Expr.R3_Expr,
               To     => To);

            Add_Combined_Terms (
               Factor => - Factor,
               Expr   => Extz (Expr.C3_Expr, To.Width),
               To     => To);

         when Minus_With_C =>

            -- Left - Right + Carry - 1:

            Add_Combined_Terms (
               Factor => Factor,
               Expr   => Expr.L3_Expr,
               To     => To);

            Add_Combined_Terms (
               Factor => - Factor,
               Expr   => Expr.R3_Expr,
               To     => To);

            Add_Combined_Terms (
               Factor => Factor,
               Expr   => Extz (Expr.C3_Expr, To.Width),
               To     => To);

            Add (Factor => - Factor, Const => 1, To => To);

         when Ternary_Boolean_Op_T =>
            -- No simplification is possible.

            Add (Term => Factor * Expr, To => To);

         end case;

      end case;

   end Add_Combined_Terms;


   function Combine_Terms (Expr : Expr_Ref) return Expr_Ref
   is

      Sum : Bounded_Sum_T (Width => Expr.Width, Max_Terms => Length (Expr));
      -- The Expr as a sum of terms, with similar terms combined.

   begin

      if Opt.Combine_Terms then
         -- Do it.

         Add_Combined_Terms (
            Factor => 1,
            Expr   => Expr,
            To     => Sum);

         return Possibly_Combined_Terms (
            Expr => Expr,
            Sum  => Sum);

      else
         -- Don't do it.

         return Expr;

      end if;

   exception

   when X : Too_Many_Terms =>

      Output.Exception_Info (
         Text =>
              "Arithmetic.Algebra.Combine_Terms"
            & Output.Field_Separator
            & Image (Expr),
         Occurrence => X);

      raise;

   end Combine_Terms;


   procedure Find_Difference (
      From  : in     Expr_Ref;
      To    : in     Expr_Ref;
      Const :    out Boolean;
      Diff  :    out Value_T)
   is

      Sum : Bounded_Sum_T (
         Width     => From.Width,
         Max_Terms => Length (To) + Length (From));
      -- To - From as a sum of terms, with similar terms combined.
      -- We implicitly assume From.Width = To.Width.

   begin

      Add_Combined_Terms (
         Factor => 1,
         Expr   => To,
         To     => Sum);

      Add_Combined_Terms (
         Factor => -1,
         Expr   => From,
         To     => Sum);

      -- Sum is now To - From.

      Const := Is_Const (Sum);

      if Const then

         Diff := Signed_Value (Sum);

      else

         Diff := 0;

      end if;

   end Find_Difference;


   procedure Find_Difference (
      From  : in     Storage.Cell_T;
      To    : in     Expr_Ref;
      Const :    out Boolean;
      Diff  :    out Value_T)
   is
      use type Storage.Cell_T;
   begin

      Const := True;
      -- Optimistic, sadly may be changed below.

      if        To.Kind        = Cell
      and then  To.Cell        = From
      then
         -- Just From.

         Diff := 0;

      elsif     Op_Is (Plus, To)
      and then (To.L_Expr      = From
      and       To.R_Expr.Kind = Arithmetic.Const)
      then
         -- From + Const.

         Diff := Signed_Value (To.R_Expr);

      elsif     Op_Is (Minus, To)
      and then (To.L_Expr      = From
      and       To.R_Expr.Kind = Arithmetic.Const)
      then
         -- From - Const.
         -- The offset is -Const.

         Diff := - Signed_Value (To.R_Expr);

      elsif     Op_Is (Plus, To)
      and then (To.L_Expr.Kind = Arithmetic.Const
      and       To.R_Expr      = From)
      then
         -- Const + From

         Diff := Signed_Value (To.L_Expr);

      else
         -- The form of To is not recognized as From + Offset.

         Const := False;
         Diff  := 0;

      end if;

   end Find_Difference;


   --
   ---   Updating cells that change by the same or opposite amounts
   --


   function Difference (
      Var  : Variable_T;
      Expr : Expr_Ref;
      Way  : Coupling_T)
   return Expr_Ref
   is
   begin

      case Way is

      when Same =>

         return To_Expr (
            Terms => ( 1 * Expr, (-1) * Var),
            Const => 0,
            Width => Expr.Width);

      when Opposite =>

         return To_Expr (
            Terms => ( 1 * Var, (-1) * Expr),
            Const => 0,
            Width => Expr.Width);

      end case;

   end Difference;


   function Plus_Difference (
      Base : Variable_T;
      Var  : Variable_T;
      Expr : Expr_Ref;
      Way  : Coupling_T)
   return Expr_Ref
   is
   begin

      case Way is

      when Same =>

         return To_Expr (
            Terms => ( 1 * Expr, (-1) * Var, 1 * Base),
            Const => 0,
            Width => Expr.Width);

      when Opposite =>

         return To_Expr (
            Terms => ( 1 * Var, (-1) * Expr, 1 * Base),
            Const => 0,
            Width => Expr.Width);

      end case;

   end Plus_Difference;


   procedure Add_Coupled_Update (
      Absolute : in     Assignment_T;
      Relative : in     Variable_T;
      Coupling : in     Coupling_T;
      To       : in out Assignment_Set_T)
   is

      New_Rel, New_Rel1, New_Rel2 : Expr_Ref;
      -- Expressions for the new value of Relative.

      Rel_Ass : Assignment_T;
      -- The updating assignment to Relative, if one is needed.

   begin

      case Defining_Kind_T (Absolute.Kind) is

      when Regular =>

         New_Rel := Plus_Difference (
               Base => Relative,
               Var  => Absolute.Target,
               Expr => Absolute.Value,
               Way  => Coupling);

         Rel_Ass := Set (
            Target => Relative,
            Value  => New_Rel);

      when Conditional =>

         New_Rel1 := Plus_Difference (
               Base => Relative,
               Var  => Absolute.Target,
               Expr => Absolute.Value1,
               Way  => Coupling);

         New_Rel2 := Plus_Difference (
               Base => Relative,
               Var  => Absolute.Target,
               Expr => Absolute.Value2,
               Way  => Coupling);

         Rel_Ass := Set (
            Target => Relative,
            Cond   => Absolute.Cond,
            Value1 => New_Rel1,
            Value2 => New_Rel2);

      when Fracture =>

         Rel_Ass := Fracture (Target => Relative);

      end case;

      Add (To => To, More => Rel_Ass);

   end Add_Coupled_Update;


   procedure Update_Couple (
      Absolute : in     Variable_T;
      Relative : in     Variable_T;
      Coupling : in     Coupling_T;
      Within   : in out Assignment_Set_T)
   is

      Abs_Ass_Index: Natural;
      -- The index of the assignment to Absolute, if any, else zero.

   begin

      -- Find the assignment to Absolute if any:

      Find (
         Target => Absolute.Cell,
         Within => Within,
         Index  => Abs_Ass_Index);

      -- Add assignment to Relative if needed:

      if Abs_Ass_Index > 0 then
         -- The effect has a defining assignment to Absolute.

         Add_Coupled_Update (
            Absolute => Element (Within, Abs_Ass_Index),
            Relative => Relative,
            Coupling => Coupling,
            To       => Within);

      end if;

   end Update_Couple;


   --
   ---   Finding updates of variables in effects
   --


   procedure Find_Update (
      Target : in     Storage.Cell_T;
      Within : in     Assignment_Set_T;
      Found  :    out Boolean;
      Op     :    out Binary_Op_T;
      Other  :    out Expr_Ref)
   is
      use type Storage.Cell_T;

      Asg : Assignment_T;
      -- One of the assignments Within the effect.

   begin

      for W in 1 .. Length (Within) loop

         Asg := Element (Within, W);

         if       (Asg.Kind        = Regular
         and       Asg.Target.Kind = Cell)
         and then (Asg.Target.Cell = Target
         and       Asg.Value.Kind  = Binary_Kind)
         and then  Asg.Value.L_Expr.Kind = Cell
         and then  Asg.Value.L_Expr.Cell = Target
         then
            -- Match.

            Found := True;
            Op    := Asg.Value.Binary;
            Other := Asg.Value.R_Expr;

            return;

         end if;

      end loop;

      -- No match.

      Found := False;
      Op    := Binary_Op_T'First;
      Other := Unknown;

   end Find_Update;


end Arithmetic.Algebra;
