-- Arithmetic.Algebra (decl)
--
-- Algebraic (symbolic) inspection, manipulation and simplification
-- of arithmetic expressions.
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
-- $Revision: 1.13 $
-- $Date: 2015/10/24 19:36:46 $
--
-- $Log: arithmetic-algebra.ads,v $
-- Revision 1.13  2015/10/24 19:36:46  niklas
-- Moved to free licence.
--
-- Revision 1.12  2010-01-02 20:28:07  niklas
-- BT-CH-0211: Corrected operations on Bounded_Sum_T.
--
-- Revision 1.11  2009-11-27 11:28:06  niklas
-- BT-CH-0184: Bit-widths, Word_T, failed modular analysis.
--
-- Revision 1.10  2009/03/04 13:05:07  niklas
-- Corrected Find_Difference (Cell, Expr, ..) to consider the trivial
-- case where the expression is just the cell itself (zero difference).
--
-- Revision 1.9  2008/06/18 20:52:54  niklas
-- BT-CH-0130: Data pointers relative to initial-value cells.
--
-- Revision 1.8  2007/10/12 18:19:25  niklas
-- Added function Plus_Difference, for cleaner updates of coupled
-- cells such as stack-height cells. Added procedure Add_Coupled_Update
-- for such purposes (where Update_Couple does not fit in). Modified
-- Update_Couple to use Add_Coupled_Update. Note that this now uses
-- Plus_Difference instead of Difference.
--
-- Revision 1.7  2007/07/21 18:18:39  niklas
-- BT-CH-0064. Support for AVR/IAR switch-handler analysis.
--
-- Revision 1.6  2007/05/02 09:30:34  niklas
-- Added procedure Find_Difference.
--
-- Revision 1.5  2007/03/09 13:52:22  niklas
-- Added function Difference and procedure Update_Couple to help with
-- coupled cells such as a stack pointer and the local stack height.
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


package Arithmetic.Algebra is


   --
   ---   Checking for additions and subtractions of constants (offsets)
   --


   function Constant_Offset (
      From : Storage.Cell_T;
      To   : Expr_Ref)
   return Expr_Ref;
   --
   -- Checks whether there is a constant offset From a given cell
   -- To an expression, that is, whether the To expression is of
   -- the form Cell + Offset, where Offset is a constant expression.
   -- If To is of this form, the Offset expression is returned,
   -- otherwise Unknown is returned.
   --
   -- Precondition: Storage.Width_Of (From) = To.Width.


   function Same_Offset (
      From : Storage.Cell_T;
      To   : Expr_Ref;
      Base : Expr_Ref)
   return Expr_Ref;
   --
   -- Checks whether there is an additive or subtractive offset
   -- From a given cell To an expression, that is, whether the To
   -- expression is of the form From + Offset, From - Offset, or
   -- Offset + From, where Offset is any expression. If To is of
   -- this form, the same expression is returned with Base instead
   -- of From (that is, Base + Offset, Base - Offset, or Offset + Base),
   -- otherwise Unknown is returned.
   --
   -- Precondition: Storage.Width_Of (From) = To.Width.


   function Opposite_Offset (
      From : Storage.Cell_T;
      To   : Expr_Ref;
      Base : Expr_Ref)
   return Expr_Ref;
   --
   -- Checks whether there is an additive or subtractive offset
   -- From a given cell To an expression, that is, whether the To
   -- expression is of the form Cell + Offset, Cell - Offset, or
   -- Offset + Cell, where Offset is any expression. If To is of
   -- this form, the same expression is returned with Base instead
   -- of From and the sign of the Offset reversed, that is, the
   -- returned expressions are respectively Base - Offset,
   -- Base + Offset, or Base - Offset, otherwise Unknown is returned.
   --
   -- Precondition: Storage.Width_Of (From) = To.Width.


   procedure Split_Affine (
      Expr         : in     Expr_Ref;
      Addend       :    out Value_T;
      Multiplier   :    out Value_T;
      Multiplicand :    out Expr_Ref);
   --
   -- Splits the given Expr into an expression of the form
   --
   --    Addend + Multiplier * Multiplicand
   --
   -- where the Addend and the Multiplier are literal numbers and
   -- only the Multiplicand may be a expression.
   --
   -- If the Expr is not naturally of this form, the Addend is
   -- returned as 0 and the Multiplier as 1, with the original
   -- Expr returned in Multiplicand.
   --
   -- If the Expr is a Const(N), then Addend is returned as N and
   -- the Multiplier as 0 and the Multiplicand as Arithmetic.Zero.


   --
   ---   Simplification of sums of terms
   --


   type Term_T is record
      Factor   : Value_T;
      Variable : Expr_Ref;
   end record;
   --
   -- A term in an expression, representing the value Factor * Variable.
   --
   -- If the Variable is in Variable_T (that is, a cell or a dynamic
   -- reference to a cell) then this is a term in a linear (or affine)
   -- expression.
   --
   -- Note that this type is not meant to represent a constant
   -- term, although it can do so if the Variable is known to
   -- have a constant value.


   function "*" (Factor : Value_T; Variable : Expr_Ref) return Term_T;
   --
   -- The term Factor * Variable.


   type Terms_T is array (Positive range <>) of Term_T;
   --
   -- A list of terms, representing the sum of all the terms.
   -- The list is usually in normal form in the sense that no two terms
   -- in the list have the same Variable component. That is, all
   -- occurrences of the same Variable are collected into one term.
   -- The value of an empty list is zero.


   type Bounded_Sum_T (Width : Width_T; Max_Terms : Natural) is private;
   --
   -- An extensible sum of terms with a bounded maximum length.
   -- There is also a constant term. There can be opaque terms.
   -- The width of the sum, as a binary word, is known a priori.
   --
   -- The default initial value of an object of this type is the
   -- null sum: no variable terms and a zero constant term.
   --
   -- When a variable term is added to the sum and the same variable
   -- term already appears in the, sum the two terms are combined by
   -- adding their factors. Likewise, if a constant term is added
   -- it is combined with any existing constant term(s).
   --
   -- If an opaque term is added with a non-zero factor, the whole
   -- sum becomes opaque. It then remains opaque. If any terms are
   -- added to an opaque sum, or an opaque term is added to a sum
   -- that already has some terms, the sum is considered to have
   -- "combined terms".


   function Not_Null (Sum : Bounded_Sum_T) return Boolean;
   --
   -- Whether the Sum contains some terms. Note that a sum
   -- with some terms is Not_Null even if all the terms have
   -- zero factors or a zero value.


   function Is_Opaque (Sum : Bounded_Sum_T) return Boolean;
   --
   -- Whether some opaque terms (with non-zero factors) were
   -- added this Sum.


   function Is_Combined (Within : Bounded_Sum_T) return Boolean;
   --
   -- Whether some terms were combined when the given sum was
   -- constructed by adding up some terms. Adding something to
   -- an opaque sum, or adding an opaque term to a non-null sum,
   -- is also considered to "combine" terms.


   function Is_Const (Sum : Bounded_Sum_T) return Boolean;
   --
   -- Whether the Sum has only constant terms (and is not opaque).
   -- This includes the case where the Sum has variable terms but
   -- only with zero factors. A null is also constant and has
   -- the value zero.


   function Value (Sum : Bounded_Sum_T) return Word_T;
   --
   -- The value of a Sum that has only constant terms.
   -- Precondition: Is_Const (Sum).


   function Signed_Value (Sum : Bounded_Sum_T) return Value_T;
   --
   -- The value of a Sum that has only constant terms, taken
   -- as a signed two's complement value of Sum.Width bits.
   -- Precondition: Is_Const (Sum).


   procedure Add_Opaque_Term (To : in out Bounded_Sum_T);
   --
   -- Adds an opaque term to a sum, which makes the sum opaque.
   -- If the sum already had some terms, it is now considered to
   -- have "combined" terms.


   procedure Add (
      Const : in     Word_T;
      To    : in out Bounded_Sum_T);
   --
   -- Adds a constant term to a sum. This is always successful.
   -- The constant term is taken to have the same width as the sum.
   --
   -- If the sum already has constant term(s), these terms are
   -- combined. Same if the sum is opaque.


   Too_Many_Terms : exception;
   --
   -- Signals an attempt to add too many terms to a bounded sum.


   procedure Add (
      Term : in     Term_T;
      To   : in out Bounded_Sum_T);
   --
   -- Adds a Term To a sum, keeping the sum in normal form by
   -- combining the new Term with any existing term that has the
   -- same variable.
   --
   -- Variable identity is defined as follows:
   --
   -- > Two variables that refer to the same Expr_T object are
   --   identical (reference equality).
   --
   -- > Two variables of type Cell are identical if the Cells
   --   are identical (refer to the same Cell_Object_T).
   --   This is precise up to aliasing of Cells with different
   --   cell-specs.
   --
   -- > Two variables of type Ref are identical if they refer
   --   to the same Boundable_Ref object.
   --
   -- > Other variables may be identical if they are found to
   --   represent the same expression. However, this is not a
   --   complete check for expression equivalence.
   --
   -- If the Term is opaque, the sum becomes opaque.
   --
   -- If the sum (To) was opaque before the Term is added, it
   -- is considered to have "combined terms".
   --
   -- Note that this procedure does not try to deconstruct the
   -- given Term into some more primitive terms. This function
   -- is performed by the procedure Add (Expr, To, Rest) defined
   -- later below.
   --
   -- Propagates Too_Many_Terms if the maximum length of the
   -- sum is exceeded.


   procedure Add (
      Terms : in     Terms_T;
      To    : in out Bounded_Sum_T);
   --
   -- Adds all the Terms to the sum, one by one.


   procedure Add (
      Sum : in     Bounded_Sum_T;
      To  : in out Bounded_Sum_T);
   --
   -- Adds one Sum To another sum.
   --
   -- The result has "combined terms" (in the sense of the function
   -- Is_Combined) if Sum has combined terms, or if the initial
   -- value of To has combined terms, or if some terms are combined
   -- when Sum is added to To.
   --
   -- The result is opaque if one or both of Sum and To is opaque.
   -- In this case the result is also considered to have "combined
   -- terms".


   procedure Add (
      Factor : in     Value_T;
      Expr   : in     Expr_Ref;
      Term   : in     Expr_Ref;
      To     : in out Bounded_Sum_T;
      Rest   : in out Expr_Ref);
   --
   -- Adds the expression Factor * Expr To a sum, trying to parse
   -- (deconstruct) the given expression into a sum of Cell or Ref terms
   -- and keeping the sum in normal form by combining terms with the same
   -- variable (Cell or Ref).
   --
   -- If the given expression is not itself an affine sum, the rest of
   -- the expression (perhaps all of it) is added to the Rest parameter.
   -- The governing rule is that the expression Factor * Expr + To + Rest,
   -- before the addition, is equivalent to the expression To + Rest
   -- after the addition.
   --
   -- If all of the expression Factor * Expr could be added to the sum,
   -- Rest is not changed.
   --
   -- The Term parameter is either null or is the expression Factor * Expr.
   -- In some cases this avoids constructing new expressions Factor * Expr
   -- when it turns out that the whole expression must be added to Rest.
   -- This strange parameter structure is designed to help recursion
   -- within the operation.
   --
   -- Preconditionn:
   -- > Expr contains only integer-valued operators, not
   --   boolean-valued operators.
   -- > The widths of Expr, Term, and Sum are the same.
   --
   -- Propagates Too_Many_Terms if the maximum length of the
   -- sum is exceeded.


   function To_Expr (Sum : Bounded_Sum_T) return Expr_Ref;
   --
   -- The value of the Sum as a general expression, including the
   -- constant term (if nonzero) and the variable terms (those with
   -- a non-zero Factor).


   function To_Expr (
      Terms : Terms_T;
      Const : Word_T := 0;
      Width : Width_T)
   return Expr_Ref;
   --
   -- The value of the expression Const + sum (Terms), where we treat
   -- each Term as a general expression, using Add (Factor, Expr, To, Rest))
   -- to split the Term into Cell and Ref terms and simplifying the
   -- sum by combining terms with the same Cell or Ref. The Width of the
   -- result is known a priori.


   function Combine_Terms (Expr : Expr_Ref) return Expr_Ref;
   --
   -- The given expression, with all affine sub-expressions simplified
   -- by combining all terms that are multiples of the same variable
   -- or the same non-affine sub-expression and all constants that are
   -- terms in the same affine sub-expression.


   procedure Find_Difference (
      From  : in     Expr_Ref;
      To    : in     Expr_Ref;
      Const :    out Boolean;
      Diff  :    out Value_T);
   --
   -- The Difference To - From, if it is Constant. The method is
   -- to express To - From as a Bounded_Sum_T and then using the
   -- functions Is_Const and Value.
   --
   -- Precondition: From.Width = To.Width.


   procedure Find_Difference (
      From  : in     Storage.Cell_T;
      To    : in     Expr_Ref;
      Const :    out Boolean;
      Diff  :    out Value_T);
   --
   -- The Difference To - From, if it is Constant. The method is
   -- very incomplete, but safe, and is simply an inspection of
   -- the To expression for the forms "From", "From + Const",
   -- "Const + From", or "From - (-Const)".
   --
   -- Precondition: Storage.Width_Of (From) = To.Width.


   --
   ---   Updating cells that change by the same or opposite amounts
   --


   type Coupling_T is (Same, Opposite);
   --
   -- Describes the coupling between two cells, the Absolute and the
   -- Relative:
   --
   -- Same
   --    When the Absolute changes by D, the Relative also changes by D.
   -- Opposite
   --    When the Absolute changes by D, the Relative changes by -D.


   function Difference (
      Var  : Variable_T;
      Expr : Expr_Ref;
      Way  : Coupling_T)
   return Expr_Ref;
   --
   -- The difference between the Var and the Expr (usually a new value
   -- being assigned to the Var) computed as Expr - Var if Way is Same
   -- and Var - Expr if Way is Opposite. We try to simplify the
   -- difference by treating it as Terms_T and using To_Expr.


   function Plus_Difference (
      Base : Variable_T;
      Var  : Variable_T;
      Expr : Expr_Ref;
      Way  : Coupling_T)
   return Expr_Ref;
   --
   -- The sum of the Base and the difference between the Var and
   -- the Expr computed as Expr - Var if Way is Same and Var - Expr
   -- if Way is Opposite. We try to simplify the whole expression
   -- by treating it as Terms_T and using To_Expr.


   procedure Add_Coupled_Update (
      Absolute : in     Assignment_T;
      Relative : in     Variable_T;
      Coupling : in     Coupling_T;
      To       : in out Assignment_Set_T);
   --
   -- Given a defining assignment to an Absolute cell, this operation
   -- adds the corresponding assignment to the Relative cell so that
   -- the Relative cell changes by the Same amount or the Opposite amount
   -- as the Absolute cell, as specified by the Coupling.
   --
   -- Precondition: Absolute.Kind is Regular, Conditional or Fracture.
   -- Absolute.Target and Relative are Cell variables, not boundable
   -- memory references.


   procedure Update_Couple (
      Absolute : in     Variable_T;
      Relative : in     Variable_T;
      Coupling : in     Coupling_T;
      Within   : in out Assignment_Set_T);
   --
   -- If the given assignment set contains an assignment to the
   -- Absolute cell, this operation adds the corresponding assignment
   -- to the Relative cell so that the Relative cells changes by the
   -- Same amount or the Opposite amount as the Absolute cell, as
   -- specified by the Coupling.
   --
   -- Precondition: Absolute and Relative are Cell variables, not
   -- boundable memory references.


   --
   ---   Finding updates of variables in effects
   --


   procedure Find_Update (
      Target : in     Storage.Cell_T;
      Within : in     Assignment_Set_T;
      Found  :    out Boolean;
      Op     :    out Binary_Op_T;
      Other  :    out Expr_Ref);
   --
   -- Searches Within the given assignment set for an assignment of
   -- the form
   --
   --      Target := Target Op Other
   --
   -- and, if Found, returns the Op and the Other operand.


private


   type Bounded_Sum_T (Width : Width_T; Max_Terms : Natural) is record
      Opaque    : Boolean := False;
      Has_Const : Boolean := False;
      Combined  : Boolean := False;
      Const     : Word_T  := 0;
      Length    : Natural := 0;
      Terms     : Terms_T (1 .. Max_Terms);
   end record;
   --
   -- A sum of Terms(1 .. Length) and the Const term.
   --
   -- Opaque
   --    Whether some opaque terms have been added (with non-zero
   --    factors).
   -- Has_Const
   --    Whether a constant term has been added to this sum.
   --    If not, Const is zero.
   -- Combined
   --    Whether some terms, added to this sum, have been combined.
   -- Const
   --    The value of the constant term (if any).
   -- Length
   --    The number of non-constant terms.
   -- Terms
   --    The non-constant terms.
   --
   -- The Terms list (1 .. Length) is in normal form: no two terms
   -- have the same variable (see the description of the Add operation
   -- for the definition of variable identity).
   --
   -- A term may have a zero Factor and still be included in the
   -- list Terms (1 .. Length).
   --
   -- Opaque terms are not included in Terms. They are only flagged
   -- by the Opaque component.
   --
   -- The other Terms (Length + 1 .. Max_Terms) are undefined.


end Arithmetic.Algebra;
