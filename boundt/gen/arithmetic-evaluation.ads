-- Arithmetic.Evaluation (decl)
--
-- Evaluation of arithmetic expressions with known or partially known
-- operands, giving known results of simplified (partially evaluated)
-- residual expressions. The arithmetic expressions can contain dynamic
-- memory references (boundable pointers) which are also partially
-- evaluated (bounded) by applying the known or partially known operands.
-- When a dynamic memory reference is evaluated (resolved) fully, so
-- that it must refer to a single statically known cell, this cell may
-- or may not already belong to set of operands that are known or partially
-- known. If it does not, the evaluation may have to be repeated with an
-- operand set that is augmented with this cell.
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
-- $Revision: 1.10 $
-- $Date: 2015/10/24 19:36:46 $
--
-- $Log: arithmetic-evaluation.ads,v $
-- Revision 1.10  2015/10/24 19:36:46  niklas
-- Moved to free licence.
--
-- Revision 1.9  2009-11-27 11:28:06  niklas
-- BT-CH-0184: Bit-widths, Word_T, failed modular analysis.
--
-- Revision 1.8  2008/06/20 10:11:52  niklas
-- BT-CH-0132: Data pointers using Difference (Expr, Cell, Bounds).
--
-- Revision 1.7  2008/06/18 20:52:55  niklas
-- BT-CH-0130: Data pointers relative to initial-value cells.
--
-- Revision 1.6  2007/07/26 11:08:52  niklas
-- BT-CH-0066. Fracture assignments.
--
-- Revision 1.5  2007/07/21 18:18:40  niklas
-- BT-CH-0064. Support for AVR/IAR switch-handler analysis.
--
-- Revision 1.4  2006/05/06 06:59:18  niklas
-- BT-CH-0021.
--
-- Revision 1.3  2005/02/16 21:11:36  niklas
-- BT-CH-0002.
--
-- Revision 1.2  2004/08/09 19:51:26  niklas
-- BT-CH-0001.
--
-- Revision 1.1  2004/04/25 16:54:26  niklas
-- First version.
--


with Storage.References;


package Arithmetic.Evaluation is


   --
   ---   Levels of evaluation
   --


   type Level_T is (Unknown, Variable, Relative, Known);
   --
   -- The four levels of evaluation of an expression or a cell,
   -- showing the level of knowledge we have about the possible
   -- values of the expression or a cell.
   --
   -- Unknown
   --    We know nothing yet.
   -- Variable
   --    We have seen at least two different values, so we consider
   --    the expression or cell to be variable (not constant).
   --    Another possibility is that the expression depends on
   --    a dynamically addressed variable (cell) where the pointer
   --    is not evaluated (resolved) to a single value.
   -- Relative
   --    The value is a certain constant "base" cell, plus a known
   --    constant (offset). The base cell is a name for a constant
   --    value that is usually defined as the value of some other
   --    variable cell at a certain earlier point in time, for example
   --    (and usually) on entry to the subprogram under analysis.
   --    In other words, the difference between the current value of
   --    this expression, and the value of the variable cell at
   --    the chosen earlier time, is a known constant. The base cell
   --    is usually an "initial-value" cell, as defined in Storage.
   --    This level is meant to detect references to stack locations.
   --    The base cell of a Relative value never has a Known value,
   --    as the Relative sum value would then reduce to Known value
   --    itself.
   -- Known
   --    We have seen a single value so far, so we believe that the
   --    cell is constant or the expression can be fully evaluated.
   --    Evaluation results at this level also carry the actual value
   --    of type Arithmetic.Word_T.
   --
   -- The order of the literals is chosen so that the 'Min function
   -- usually calculates the level of the result of a binary operator,
   -- given the levels of the two operands, thus:
   --
   --     Unknown  <op> any other level              => Unknown.
   --     Variable <op> Variable, Relative, or Known => Variable.
   --     Known    <op> Known                        => Known.
   --
   -- The Relative level is special in that it depends on the <op>
   -- and also on the base cells, when there are two Relative
   -- operands:
   --
   --     Relative +   Relative, same base   => Variable
   --     Relative -   Relative, same base   => Known
   --     Relative +/- Relative, other base  => Variable
   --     Relative +/- Known                 => Relative (same base)
   --     Relative <other op> Relative/Known => Variable.
   --
   -- There are, of course, some exceptions to these general "level"
   -- rules. For example, a Known zero value times (*) any other
   -- kind of value results in a Known zero.
   --
   -- The present order is also convenient for some subtype
   -- definitions.
   --
   -- When considered as a domain for data-flow purposes, such as
   -- constant propagation, the domain order (lattice order) is
   --
   --     Unknown         (bottom)
   --     Known, Relative (middle)
   --     Variable        (top).
   --
   -- However, for two Known values with the same Value and Width
   -- a value with Signed = False is considered lower than a value
   -- with Signed = True, which splits the Known level into two
   -- levels.


   subtype Fuzzy_Level_T is Level_T range Unknown .. Relative;
   --
   -- The evaluation levels at which the value is not yet known.
   -- Evaluation results at these levels usually carry also the
   -- original or residual expression that was (partially) evaluated.


   subtype Unk_Var_T is Fuzzy_Level_T range Unknown .. Variable;
   --
   -- The evaluation levels Unknown and Variable.


   type Target_Level_T is (Dynamic_Ref, Known_Cell);
   --
   -- The possible evaluation levels for the target variable of
   -- an assignment.
   --
   -- Dynamic_Ref
   --    The variable is a dynamic reference that has not been
   --    resolved to a single referent cell.
   -- Known_Cell
   --    The variable is either a statically known cell, or a
   --    dynamic reference that has been resolved to a single
   --    referent cell.


   type Cell_Value_T (Level : Level_T := Unknown)
   is record

      case Level is

      when Unk_Var_T =>

         null;

      when Relative =>

         Base   : Storage.Cell_T;
         Offset : Value_T;

      when Known =>

         Signed : Boolean;
         Value  : Word_T;
         Width  : Width_T;

      end case;

   end record;
   --
   -- The possible values of an operand cell as used in an
   -- expression.
   --
   -- Level
   --    The general level of our knowledge.
   -- Base, Offset
   --    The base cell and static offset such that the operand
   --    value is the Base cell plus the Offset. We assume that the
   --    Base cell has a constant (though unknown) value throughout
   --    the subprogram under analysis, 
   -- Value
   --    The known, single, constant value of the cell. The Width
   --    in bits is also given, for a possible signed view, as is
   --    a hint that the Value should be taken as signed.
   -- Width
   --    The number of bits in the Value.
   -- Signed
   --    A hint that the Value should be taken as a signed number,
   --    in those analyses that use signed values.
   --    TBM: This component is declared before Value and Width
   --    because the logical order triggers a bug in GNAT 4.3.2.


   subtype Known_Cell_Value_T is Cell_Value_T (Level => Known);
   --
   -- A cell with a known value.


   function Signed_Value (Item : Known_Cell_Value_T) return Value_T;
   --
   -- The signed view of Item.Value, for a cell with a known value.


   Unknown_Image  : constant String := "?";
   Variable_Image : constant String := "*";
   --
   -- The Images of unknown and variable cell values.


   function Image (Item : Cell_Value_T) return String;
   --
   -- Presents the Item for human consumption, using Unknown_Image
   -- and Variable_Image when indicated. Relative values are shown
   -- as the Base-cell name +/- the offset, and Known values as
   -- the Value itself, with a trailing "[+]" or "[-nnn]" for Signed
   -- values.


   function To_Value (
      Word   : Word_T;
      Width  : Width_T;
      Signed : Boolean)
   return Cell_Value_T;
   --
   -- The Known cell-value that consists of the lowest Width
   -- bits of the Word and perhaps should be considered Signed.


   type Result_T (Level : Level_T := Unknown)
   is record

      Expr        : Expr_Ref;
      Refined     : Boolean;
      Ref_Bounded : Boolean;

      case Level is

      when Unk_Var_T =>

         null;

      when Relative =>

         Base   : Storage.Cell_T;
         Offset : Value_T;

      when Known =>

         Value  : Word_T;
         Signed : Boolean;

      end case;

   end record;
   --
   -- The result of (partially) evaluating an expression.
   --
   -- Level
   --    The evaluation level of the result, specifically:
   --
   --    Unknown  if the result depends on a cell with unknown value.
   --    Variable if the result depends on a cell with variable value
   --             and depends on no cells with unknown value.
   --    Relative if the result is the sum of the Base cell (with an
   --             unknown but constant value) and a known Offset.
   --    Known    if the result depends only on cells with known values.
   --
   --    "Dependence" is used here in the sense of non-strict evaluation
   --    dependence. For example, the result of the expression x*0, where
   --    x is a cell, does not depend on x because the result is zero
   --    for any x.
   -- Expr
   --    The original (source) expression or a residual (partially
   --    evaluated) expression, depending on evaluation options.
   -- Refined
   --    Whether the Expr was actually refined (simplified) through
   --    partial evaluation giving a simpler residual Expr.
   -- Ref_Bounded
   --    Whether one or more dynamic variable references were resolved
   --    to unique referent cells or were bounded to smaller ranges
   --    during this evaluation. If so, it may be worth-while to
   --    re-evaluate the data flow in the subprogram that contains the
   --    partially evaluated expression.
   -- Value
   --    The constant result, if Known. Expr is then the source of
   --    the Value; the expression that was evaluated to give Value.
   --    The width of the Value is Expr.Width.
   -- Signed
   --     A hint that the constant Value should be considered as a
   --     signed number, in those analyses that use signed numbers.
   --
   -- The values of Refined and Ref_Bounded are not independent.
   -- Ref_Bounded implies Refined because bounding a dynamic variable
   -- reference is counted as a refinement.


   subtype Known_Result_T is Result_T (Level => Known);
   --
   -- A result with a known value.


   function Width_Of (Item : Result_T) return Width_T;
   --
   -- The width of the result value.
   -- Precondition: Item.Expr is present and not Unknown.


   function Signed_Value (Item : Known_Result_T) return Value_T;
   --
   -- The signed view of the known result value.


   function Image (Item : Result_T) return String;
   --
   -- For human consumption.


   type Target_Result_T (Level : Target_Level_T := Dynamic_Ref)
   is record

      Ref_Bounded : Boolean;
      Source      : Variable_T;

      case Level is
      when Dynamic_Ref => Ref  : Storage.References.Boundable_Ref;
      when Known_Cell  => Cell : Storage.Cell_T;
      end case;

   end record;
   --
   -- The result of (partially) evaluating the target variable of an
   -- assignment. This is trivial if the target variable is a statically
   -- named cell, but is non-trivial if it is a dynamic reference.
   --
   -- Level
   --    The evaluation level of the target variable.
   --    See definition of Target_Level_T, above.
   -- Ref_Bounded
   --    Whether the dynamic variable reference was resolved
   --    to a unique referent Cell or was bounded to a narrower
   --    Ref during this evaluation. If so, it may be worth-while to
   --    re-evaluate the data flow in the subprogram that contains the
   --    assignment.
   -- Source
   --    The source variable that was evaluated to give either the
   --    Cell or the Ref.
   -- Ref
   --    The dynamic reference, when it was not possible to resolve a
   --    reference Source to a unique referent Cell. This may be the
   --    original reference (as given in Source) or a residual (partially
   --    evaluated) reference, depending on evaluation options.
   --    Ref_Bounded is False or True, respectively.
   -- Cell
   --    The target cell, if the Source is a static cell or if the
   --    Source is a Ref that was resolved to a unique Cell referent.


   function Image (Item : Target_Result_T) return String;
   --
   -- For human consumption.


   type Assignment_Result_T (Kind : Defining_Kind_T := Regular)
   is record

      Refined     : Boolean;
      Ref_Bounded : Boolean;

      Target : Target_Result_T;
      Value  : Result_T;

      case Kind is

      when Regular | Fracture =>
         null;

      when Conditional =>
         Cond   : Result_T;
         Value1 : Result_T;
         Value2 : Result_T;

      end case;

   end record;
   --
   -- The result of (partially) evaluating the two sides of a
   -- Defining assignment, which is an expression or a conditional
   -- expression with a condition and two alternatives (true/false
   -- choices).
   --
   -- Kind
   --    Whether the assignment was regular, conditional or a fracture.
   --    A fracture represents an indirect effect of some other assignment
   --    on cell that shares some storage with the target of the other
   --    assignment.
   -- Refined
   --    Whether the assignment was refined (simplified) in some ways by
   --    partial evaluation. This can happen if the value expressions
   --    (or the condition of a conditional assignment) are simplified,
   --    if some dynamic variable references are resolved or bounded
   --    (in the value expressions, or the target), or if a conditional
   --    assignment is simplified to a regular assignment because the
   --    condition is evaluated to a constant.
   -- Ref_Bounded
   --    Whether one or more dynamic variable references were resolved
   --    to unique referent cells or were bounded to smaller ranges
   --    during this evaluation. If so, it may be worth-while to
   --    re-evaluate the assignment in the subprogram that contains it.
   -- Target
   --    The target variable to which the Value will be assigned.
   --    The identity of the target cell is not changed by the
   --    (partial) evaluation of an assignment.
   -- Value
   --    The result of evaluating the single expression in a regular
   --    assignment, or of choosing one of the two alternative
   --    expressions in a conditional assignment. Irrelevant (Unknown)
   --    for a fracture assignment.
   -- Cond
   --    The result of evaluating the condition expression (Boolean
   --    expression) in a conditional assignment.
   -- Value1, Value2
   --    The results of evaluating the two alternative expressions
   --    in a conditional assignment. Value1 is for True condition,
   --    Value2 for the False condition.
   --
   --    The evaluation is non-strict, so if Cond is evaluated to
   --    True, only Value1 is evaluated and Value2 is Unknown, and
   --    vice versa if Cond is evaluated to False.
   --
   -- In a conditional assignment, Value is the result of choosing
   -- Value1 or Value2, depending on Cond. Note that if Cond is not
   -- Known then Value may be Unknown even if both Value1 and Value2
   -- are Known. See Fuzzy_Cond_Level for details. If Value is Unknown
   -- or Variable then Value.Expr is null (because the expression is
   -- really a conditional one, which cannot be presented as a single
   -- Expr.Ref). In this case, the components Cond, Value1 and Value2
   -- represent the conditional expression that was evaluated to
   -- produce Value.
   --
   -- The parts Cond, Value1 and Value2 are useful when it is desired
   -- to create a residual (partially evaluated) version of the
   -- conditional assignment, when Partly is True.
   --
   -- Ref_Bounded implies Refined.
   --
   -- Note that all components except Kind are default initialized
   -- to Unknown.


   function Image (Item : Assignment_Result_T) return String;
   --
   -- For human consumption.


   function Same (Left, Right : Result_T) return Boolean;
   --
   -- Whether the Left result is exactly the same as the Right result.
   -- Note that predefined equality ("=") on Result_T is not very useful,
   -- because for fuzzy results it compares only the expression references,
   -- which may be different (different addresses) for identically formed
   -- expressions, and for known results it compares the value and also the
   -- source expression reference, which can again be different although the
   -- values are the same.
   --
   -- This operation corrects the problem for known values, but not for
   -- fuzzy values. Thus, this check is not complete: if Same returns
   -- False, the Left and Right results may still be entirely equivalent.
   -- However, the check is safe: if Same returns True, Left and Right
   -- are equivalent.


   function Value_Of (Result : Result_T) return Cell_Value_T;
   --
   -- The result as a cell-value, as far as possible, forgetting
   -- the source expression.


   function To_Condition (Result : Result_T) return Condition_T;
   --
   -- The result as a condition, mapping True_Value to Always and
   -- False_Value to Never and fuzzy values to the (partially)
   -- evaluated expression.


   --
   ---   Evaluation domains (sources of cell values)
   --


   type Domain_T is abstract new Arithmetic.Bounds_T with null record;
   --
   -- A data structure that defines the value of some cells.
   -- Such a structure is one form of "bounds" on the cell values and
   -- is therefore derived from Storage.Bounds.Bounds_T.


   function Value_Of (Cell : Storage.Cell_T; Within : Domain_T)
   return Cell_Value_T
   is abstract;
   --
   -- The value of the Cell, as far as it is defined Within the domain.
   -- This is a primitive (overridable) operation of Domain_T.


   -- overriding
   function Interval (Cell : Cell_T; Under : Domain_T)
   return Storage.Bounds.Interval_T;
   --
   -- The interval of values for the Cell, allowed Under the domain.
   -- For a Cell with a Known value the result is the point interval
   -- containing exactly this value, otherwise the result is the
   -- universal interval.


   -- overriding
   function Difference (To, From : Cell_T; Under : Domain_T)
   return Storage.Bounds.Interval_T;
   --
   -- The interval of values for the difference To - From, allowed
   -- Under the domain. If both cells are Known, or both are Relative
   -- with the same Base cell, the result is the point interval
   -- containing the difference, otherwise the problem is delegated
   -- to Interval (Expr => To - From, Under) with redispatch.


   -- overriding
   function Interval (Expr : Expr_Ref; Under : Domain_T)
   return Storage.Bounds.Interval_T;
   --
   -- The interval of values for the Expr, allowed Under the domain.
   -- This is computed by evaluating the expression using the Eval
   -- function (see below). If this gives a Known value, the result
   -- is the point interval containing exactly this value, otherwise
   -- the result is the universal interval.


   -- overriding
   function Difference (
      To    : Expr_Ref;
      From  : Storage.Cell_T;
      Under : Domain_T)
   return Storage.Bounds.Interval_T;
   --
   -- The interval (range) of values of the difference To - From,
   -- allowed Under the domain. We first evaluate To and From Under
   -- this domain. If To and From both have Known values, the result
   -- is the point interval of the difference between these values.
   -- If To and From both have Relative values based on the same
   -- initial-value cell, the result is the point interval of the
   -- difference of the offsets. Otherwise the problem is delegated
   -- to Interval (Expr => To - From).


   --
   ---   Evaluation of expressions and assignments within a domain
   --


   function Eval (
      Expr   : Expr_Ref;
      Within : Domain_T'Class;
      Partly : Boolean)
   return Result_T;
   --
   -- Evaluates the given Expression Within the given domain.
   --
   -- If the domain defines the value of all cells used in the Expression,
   -- the result of the evaluation is a Known constant.
   -- 
   -- Otherwise the result is an expression, with either Unknown,
   -- Variable, or Relative value, depending on the status of the
   -- cells that the expression uses.
   --
   -- When the result is an expression, the Partly parameter controls
   -- the partial evaluation of the result. If Partly is True, the
   -- result is a simplified (residual, partially evaluated) expression.
   -- If Partly is False, and evaluation does not yield a constant
   -- result, the original expression is returned unchanged.


   function Eval (
      Target : Variable_T;
      Within : Domain_T'Class;
      Partly : Boolean)
   return Target_Result_T;
   --
   -- Evaluates the given Target variable Within the given domain.
   -- The chief good of this evaluation is that it may resolve or
   -- constrain a dynamic reference (variable pointer). Note, we
   -- are interested in the target _cell_, not  in the value of the
   -- target cell. This is "left hand side", not "right hand side".


   function Eval (
      Assignment : Assignment_T;
      Within     : Domain_T'Class;
      Target     : Target_Result_T;
      Partly     : Boolean)
   return Assignment_Result_T;
   --
   -- Evaluates the value expressions in the given Defining Assignment,
   -- Within the given domain of cell-values that enter the assignment.
   -- Precondition: Assignment.Kind in Defining_Kind_T.
   --
   -- The Target parameter is assumed to result from
   -- Eval (Assignment.Target) under the same domain.
   --
   -- Each expression is evaluated by Eval (Expr), except that
   -- for a conditional assignment in which the condition evaluates
   -- to Known only one of the alternative expressions is evaluated.
   --
   -- If the condition of a conditional assignment evaluates to Known,
   -- then an Assignment_Result_T of Kind = Regular is returned. In
   -- all other cases, the Kind of the result is the same as the Kind
   -- of the Assignment parameter.
   --
   -- The Partly parameter has the same effect as in Eval (Expr).


   function Eval (
      Assignment : Assignment_T;
      Within     : Domain_T'Class;
      Partly     : Boolean)
   return Assignment_Result_T;
   --
   -- Short for Eval, above, with its Target parameter computed
   -- by Eval (Assignment.Target, Within, Partly).


   --
   ---   Understanding the results
   --


   function Same (Left : Result_T; Right : Expr_Ref)
   return Boolean;
   --
   -- Whether the Left result is exactly the same as the Right
   -- expression. This is a safe (all True results are really true)
   -- but not complete (some False results are false) comparison.
   -- If Left = Eval (Right, Partly => True) then this Same check
   -- can detect (by Same = False) that there was some significant
   -- partial evaluation so that Left is really a simpler expression
   -- than Right.


   function Same (Left : Assignment_Result_T; Right : Assignment_T)
   return Boolean;
   --
   -- Whether the Left result is exactly the same as the Right
   -- assignment, when Left = Eval (Right, Partly => True).
   --
   -- This is a safe (all True results are really true) but not
   -- complete (some False results are false) comparison. This check
   -- can detect (by Same = False) that there was some significant
   -- partial evaluation so that Left is really a simpler assignment
   -- than Right.


   function Identity (Result : Assignment_Result_T)
   return Boolean;
   --
   -- Whether the Result of (partially) evaluating an assignment is
   -- only an identity assignment of the target cell, that is, an
   -- assignment of the form Target := Target. Such identity
   -- assignments are useless and can be deleted from the effect
   -- of a step in a flow-graph.


   function Residual (From : Result_T) return Expr_Ref;
   --
   -- Converts the result of evaluating an expression back into
   -- an expression, possibly partially evaluated.


   function Target_Cell (From : Target_Result_T) return Cell_T;
   --
   -- The cell that is the residual target, if such is the case,
   -- otherwise No_Cell.


   function Residual (From : Target_Result_T)
   return Variable_T;
   --
   -- Converts the result of evaluating an assignment target variable
   -- into an ordinary variable, possibly partially evaluated (it if
   -- was a reference, so here evaluation means de-referencing).


   function Residual (From : Assignment_Result_T)
   return Assignment_T;
   --
   -- Converts the result of evaluating an assignment back into
   -- an assignment, possibly partially evaluated.


end Arithmetic.Evaluation;
