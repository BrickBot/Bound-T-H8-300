-- Calculator.Formulas (decl)
--
-- Formula construction and execution for the Calculator subsystem.
-- Also provides creation of new pools and fluxes.
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
-- $Revision: 1.22 $
-- $Date: 2015/10/24 19:36:47 $
--
-- $Log: calculator-formulas.ads,v $
-- Revision 1.22  2015/10/24 19:36:47  niklas
-- Moved to free licence.
--
-- Revision 1.21  2013/12/22 20:14:56  niklas
-- Value enumerators obey Storage.Bounds.Opt.Max_Listed_Values and raise
-- Unbounded_Set or Storage.Bounds.Unbounded if more values are offered;
-- they do not return a truncated list.
--
-- Revision 1.20  2013-02-12 08:47:19  niklas
-- BT-CH-0245: Global volatile cells and "volatile" assertions.
--
-- Revision 1.19  2009-11-27 11:28:07  niklas
-- BT-CH-0184: Bit-widths, Word_T, failed modular analysis.
--
-- Revision 1.18  2009-10-07 19:26:09  niklas
-- BT-CH-0183: Cell-sets are a tagged-type class.
--
-- Revision 1.17  2008-04-26 19:19:43  niklas
-- BT-CH-0124: Joint loop counters and induction variables.
--
-- Revision 1.16  2008/04/22 12:43:30  niklas
-- Added function Is_Null (Set).
--
-- Revision 1.15  2006/04/28 09:37:13  niklas
-- Noted that the default initial value of Formula_T is Nil.
-- Added function Interval (Interval_T) return Set.
-- Corrected parameter Hull_Bound.Set to be type Set_T.
-- Added function Values (Set) giving Value_List_T or
-- the new exception Unbounded_Set.
--
-- Revision 1.14  2005/10/20 11:28:29  niklas
-- BT-CH-0015.
--
-- Revision 1.13  2005/02/16 21:11:40  niklas
-- BT-CH-0002.
--
-- Revision 1.12  2004/05/01 20:12:17  niklas
-- First Tidorum version.
-- Taking Cell_T stuff from Storage, not from Arithmetic. Removed "use" for
-- Arithmetic, added some "use type" for Storage types.
-- Added predicates Is_Nil and Not_Nil.
-- Added function Set to formulate Range_Pre constraints.
-- Added Null_Relation to model false conditions.
-- Replaced the function Relation (Effect) by Transformation that maps
-- a null Effect to a Nil relation, for use in Join or Apply.
-- Changed the Apply function to treat the Nil relation as a neutral
-- element (no change in the set).
-- Replaced the Convert function by two variants of the Adapted function,
-- for pools and fluxes respectively. The new functions detect when no real
-- adaptation is required (no change in the cell set) and return the
-- input without any further calculation.
-- Added some support for unsigned expressions.
-- Operators that are not "calculable", such as bit-wise logical operators,
-- are mapped to inequalities or to unknown values (unconstrained).
-- A conditional assignment with an unknown (or not calculable) condition
-- can give valid constraints because the true/false cases can imply some
-- common constraints.
-- A bound that admits only a single value is now expressed as an equality
-- constraint, not two inequalities.
-- Added a set of Conjoin operations to build formulas from conjuncts.
-- Added support for range assignment constraints.
-- Added support for the option Keep_Files.
--
-- Revision 1.11  2001/01/07 21:58:15  holsti
-- Comment operation added.
--
-- Revision 1.10  2001/01/04 16:09:23  saarinen
-- Moved function Convert for calculator to here.
--
-- Revision 1.9  2000/12/28 13:28:22  saarinen
-- Added function Complement.
--
-- Revision 1.8  2000/11/23 12:49:22  saarinen
-- Deleted procedure Symbolic and variables
-- Next_Pool_ID and Next_Flux_ID.
--
-- Revision 1.7  2000/09/20 18:46:23  saarinen
-- Added function Symbolic.
-- Made little changes regarding modified Calc_Handle_T.
--
-- Revision 1.6  2000/08/17 13:00:15  holsti
-- Mapping domain to expression added.
--
-- Revision 1.5  2000/07/17 21:00:07  holsti
-- Cell_Set_T instead of Cell_List_T for domains and ranges.
--
-- Revision 1.4  2000/07/16 18:40:33  holsti
-- Transitive_Closure added.
--
-- Revision 1.3  2000/07/14 20:32:20  holsti
-- Include creation of new pools and fluxes.
--
-- Revision 1.2  2000/07/12 20:47:41  holsti
-- Start/Stop calculator implemented here.
--
-- Revision 1.1  2000/07/12 12:26:13  holsti
-- Calculator.Formulas added and used.
--


with Ada.Strings.Unbounded;


private package Calculator.Formulas is


   subtype Formula_T is -- private!
      Ada.Strings.Unbounded.Unbounded_String;
   --
   -- Contains a calculation formula expressed in Omega syntax.
   -- Please treat this type as a private type, although it is
   -- publicly declared.
   --
   -- The default initial value of every Formula_T is the
   -- special value Nil, which means "no formula" but acts
   -- as a neutral element in some formula-combining operations.


   subtype Identifier_T is Formula_T;
   --
   -- A formula that is just the identifier of a set or relation.


   subtype Set_T is Formula_T;
   --
   -- A formula that denotes a set.


   subtype Relation_T is Set_T;
   --
   -- A formula that denotes a relation.


   subtype Tuple_T is Formula_T;
   --
   -- A formula (well, almost) that denotes a tuple, as in
   -- the domain or range of a set or relation.


   --
   ---   Formula-construction operations.
   --
   -- NOTE that the operations DO NOT ensure that the precedence
   -- and associativity of the Omega operations in the resulting
   -- formula (string) match the construction tree (since the
   -- operations do not insert parentheses everywhere).
   --
   -- To control precedence and association, please use the
   -- function Parens to enclose subexpressions.
   --


   function Nil return Formula_T;
   --
   -- An "empty" formula that acts as a neutral element in
   -- union, intersection and join operations.
   --
   -- This is also the default initial value of any Formula_T.


   function Is_Nil (Item : Formula_T) return Boolean;
   --
   -- Whether the Item is a null, empty formula.


   function Not_Nil (Item : Formula_T) return Boolean;
   --
   -- Whether the Item is a non-null formula.
   -- Same as not Is_Nil (Item).


   function Parens (Item : Formula_T) return Formula_T;
   --
   -- The given formula "in parentheses" to ensure that it
   -- is evaluated as a subformula and its operands are not
   -- associated with adjacent operators outside the formula.


   function Id (Item : Pool_T'Class) return Identifier_T;
   --
   -- The formula-identifier for the given pool.


   function Id (Item : Flux_T'Class) return Identifier_T;
   --
   -- The formula-identifier for the given flux.


   function Tuple (Cells : Cell_Set_T) return Tuple_T;
   --
   -- The tuple that contains the given cells.


   function Tuple (Cell : Storage.Cell_T) return Tuple_T;
   --
   -- The singleton tuple that contains just the given cell.


   function Tuple (
      Cells         : Cell_Set_T;
      Dashed        : Cell_Set_T;
      Dash_Volatile : Boolean)
   return Tuple_T;
   --
   -- The tuple that contains the given cells, with a
   -- dash mark for the cells that are in Dashed.
   -- Note that Dashed does not have to be a subset of Cells.
   -- Moreover, if Dash_Volatile is True, all the Cells that are
   -- volatile, are also dashed.


   function Null_Set (Cells : Tuple_T)
   return Set_T;
   --
   -- The empty set of tuples of the given Cells.


   function Set (
      Cells : Tuple_T;
      Cond  : Arithmetic.Condition_T)
   return Set_T;
   --
   -- The set that consists of those tuples of the given cells
   -- that satisfy the given condition.


   function Interval (Within : Storage.Bounds.Interval_T)
   return Set_T;
   --
   -- The one-dimensional set that consists of the values Within
   -- the given interval, which may be unbounded at either end.
   -- If the interval is unbounded at both ends, the result is Nil.
   -- The basis cell is not named.


   function Set (
      Cells  : Tuple_T;
      Bounds : Storage.Bounds.Cell_Interval_List_T)
   return Set_T;
   --
   -- The set that consists of those tuples of the given cells
   -- that satisfy the given bounds.


   function Set (
      Cells : Tuple_T;
      Pre   : Arithmetic.Effect_T)
   return Set_T;
   --
   -- The set that consists of those tuples of the given cells
   -- that satisfy the Range_Pre assignment constraints in Pre.
   -- Other kinds of assignments in Pre are ignored.


   function Singleton (Value : Arithmetic.Value_T)
   return Set_T;
   --
   -- The one-dimensional singleton set {[Value]}.


   function Singleton (Values : Storage.Bounds.Value_List_T)
   return Set_T;
   --
   -- The set that consists of a single tuple that has the values
   -- in the given list. The basis cells are not named.


   function Identity_Relation (Domain : Tuple_T)
   return Relation_T;
   --
   -- The identity relation that maps the given domain cells to
   -- the same range cells with the same values.


   function Identity_Relation (Domain : Cell_Set_T)
   return Relation_T;
   --
   -- The identity relation that maps the given domain cells to
   -- the same range cells with the same values.


   function Null_Relation (Domain : Tuple_T)
   return Relation_T;
   --
   -- The relation that maps the given domain cells to the same
   -- range cells with the same values, but is empty (False
   -- condition).


   function Relation (
      Domain : Tuple_T;
      Rainge : Tuple_T)
   return Relation_T;
   --
   -- The relation that maps the cells in Domain to the cells
   -- in Rainge (should be Range, but that word is reserved),
   -- listed in the given orders, with no constraints, except
   -- for the equality constraints implied by cells that occur
   -- both in Domain and in Rainge (and are not dashed in the
   -- Rainge).


   function Relation (
      Domain : Tuple_T;
      Rainge : Tuple_T;
      Where  : Formula_T)
   return Relation_T;
   --
   -- The relation that maps the cells in Domain to the cells
   -- in Rainge (should be Range, but that word is reserved),
   -- listed in the given orders, constrained by the Where formula
   -- and the equality constraints implied by cells that occur
   -- both in Domain and in Rainge (and are not dashed in the
   -- Rainge).


   function Warp (
      From : Cell_Set_T;
      To   : Cell_Set_T)
   return Relation_T;
   --
   -- The relation with domain defined by From and range defined
   -- by To, which preserves the cells in common to From and To,
   -- discards the cells in From - To, and leaves free (introduces
   -- new free variables for) the cells in To - From.
   --
   -- If From and To are not the same set, the result is the same
   -- as from Relation (Domain => From, Rainge => To).
   --
   -- If From and To are identical, Nil is returned. Thus, the
   -- result should nearly always be used as a Join operand or in
   -- an Apply operation, where Nil is an identity element


   function Transformation (
      Domain : Cell_Set_T;
      Effect : Arithmetic.Effect_T)
   return Relation_T;
   --
   -- The relation that transforms the given domain cells as
   -- defined by the cell assignments in the given effect.
   --
   -- If the Effect is null (no assignments), the Nil relation
   -- is returned. Therefore, the result should ordinarily be used
   -- only as an operand for Join or Apply, where Nil is a neutral
   -- value. If a real "no transformation" relation is desired, use
   -- Identity_Relation (Domain).
   --
   -- Assignments to volatile cells result in no constraint. The
   -- target cell is left free.


   function Projection (
      Domain : Cell_Set_T;
      To     : Storage.Cell_T)
   return Relation_T;
   --
   -- The relation that projects the Domain cells, To the
   -- one-dimensional space consisting of the single cell.
   --
   -- If the Width of the Cell is not too large, the projection
   -- also applies the "mod 2**Width" function.


   function Mapping (
      Domain : Cell_Set_T;
      Value  : Arithmetic.Expr_Ref)
   return Relation_T;
   --
   -- The relation that transforms the given domain cells into
   -- one value defined by the arithmetic expression.
   --
   -- If the Width of the Value is not too large, the mapping
   -- also applies the "mod 2**Width" function.


   function First_Of_Two return Relation_T;
   --
   -- The relation { [a, b] -> [a] }.


   function Second_Of_Two return Relation_T;
   --
   -- The relation { [a, b] -> [b] }.


   function Sum_Of_Two return Relation_T;
   --
   -- The relation { [a, b] -> [a+b] }.


   function Union (Left, Right : Set_T) return Set_T;
   --
   -- The union of two sets or relations.
   -- If either operand is Nil, the result is the other operand.


   function Intersection (Left, Right : Set_T) return Set_T;
   --
   -- The Intersection of two sets or relations.
   -- If either operand is Nil, the result is the other operand.


   function Difference (Left, Right : Set_T) return Set_T;
   --
   -- The difference set: Left minus Right.
   -- If Left is Nil, the result is Nil.
   -- If Right is Nil, the result is Left.


   function "-" (Left, Right : Set_T) return Set_T
   renames Difference;
   --
   -- Infix set difference.


   function Join (Left, Right : Relation_T) return Relation_T;
   --
   -- The join (.) of two relations.
   -- If either operand is Nil, the result is the other operand.


   function Transitive_Closure (Item : Relation_T)
   return Relation_T;
   --
   -- The transitive closure of the given relation.
   -- This is defined as the union of the relation sequence
   -- R(i) where R(0) = Item and R(i+1) = Join (R(i), Item).
   -- Note that the Omega Calculator may compute an approximation
   -- of the real transitive closure.
   --
   -- As an exception to the general rule about user-controlled
   -- precedence and association, the result of this function
   -- is protected and will not mix harmfully with adjacent
   -- parts of a formula.


   function Domain (Item : Relation_T) return Set_T;
   --
   -- The domain of the relation, including all constraints
   -- of the relation.


   function Rainge (Item : Relation_T) return Set_T;
   --
   -- The range of the relation, including all constraints
   -- of the relation. Sorry for the ugly spelling, but
   -- "range" is a reserved word.


   function Complement (Item : Set_T) return Set_T;
   --
   -- The complement of the set.


   function Apply (
      Relation : Relation_T;
      To_Set   : Set_T)
   return Set_T;
   --
   -- The set that results from applying the given relation to
   -- the given set as domain, and collecting the related range
   -- tuples.
   --
   -- If Relation is Nil, To_Set is returned with no change.


   function Restrict_Range (
      Relation : Relation_T;
      Subrange : Set_T)
   return Relation_T;
   --
   -- The given relation, restricted to only those tuple pairs
   -- where the range part is in the given Subrange.


   function Restrict_Domain (
      Relation  : Relation_T;
      Subdomain : Set_T)
   return Relation_T;
   --
   -- The given relation, restricted to only those tuple pairs
   -- where the domain part is in the given Subdomain.


   function Adapted (
      Pool : Pool_T'Class;
      To   : Cell_Set_T)
   return Set_T;
   --
   -- The set defined by the given Pool, but adapted (projected, widened)
   -- To a different (usually larget) set of cells, so that the adapted
   -- set is based on the To domain.
   --
   -- The adapted set equals Pool when projected to Pool.Cells. It does
   -- not constrain the remaining cells in To, that is To - Pool.Cells.
   --
   -- If the To set is not a superset of Pool.Cells, the constraints
   -- implied by Pool on the cells in Pool.Cells - To are lost in the
   -- result, because these cells are elided.
   --
   -- If it happens that Pool.Cells equals the To set, the function
   -- returns the Id of Pool without any further calculation.


   function Adapted (
      Flux : Flux_T;
      To   : Cell_Set_T)
   return Relation_T;
   --
   -- The relation defined by the given Flux, but adapted To a different
   -- (usually larger) set of domain and range cells, so that the
   -- adapted relation is To -> To.
   --
   -- The adapted relation equals Flux on the sub-domain formed by
   -- Flux.Cells and is the identity relation on the sub-domain formed
   -- by the remaining cells in To, that is To - Flux.Cells.
   --
   -- In other words, the adapted relation corresponds to a computation
   -- that takes a valuation of the cells in To, changes the values of the
   -- cells in Flux.Cells as defined by Flux, and leaves the other cells
   -- unchanged.
   --
   -- If the To set is not a superset of Flux.Cells, the adapted relation
   -- hides the effect of Flux on the cells in Flux.Cells - To, because
   -- these cells are elided from the domain and range.
   --
   -- If it happens that Flux.Cells equals the To set, the function
   -- returns the Id of Flux without any further calculation.


   --
   ---   Induction variables and iteration counters
   --
   -- In a loop, induction variables are those cells that have a
   -- bounded (at least from one side) step (additive change) on
   -- each loop iteration. To find loop bounds, we introduce a
   -- synthetic, joint iteration counter and constrain each induction
   -- variable to its initial value plus its step times the counter.
   --
   -- Note that this joint iteration counter is not a Storage.Cell_T
   -- and is therefore not included in the set of Cells of fluxes and
   -- pools, even if included in the domain and/or range. In the
   -- calculator formulae, the iteration counter is represented by
   -- an identifier that must not equal the Name_Of any real cell.


   function Counter_Tuple return Tuple_T;
   --
   -- The tuple that contains only the synthetic joint iteration
   -- counter, with no dash mark.


   function Tuple_With_Counter (Cells : Cell_Set_T)
   return Tuple_T;
   --
   -- The tuple that contains the given Cells, followed by the
   -- synthetic joint iteration counter. No dash marks.


   function Induction_Model (
      Cells     : Cell_Set_T;
      Invariant : Cell_Set_T;
      Induction : Storage.Cell_List_T;
      Step      : Storage.Bounds.Interval_List_T)
   return Relation_T;
   --
   -- The relation that expresses the constraints on the values of
   -- the Induction variables as a function of the initial values,
   -- the Steps of the Induction variables, and the iteration counter.
   --
   -- The domain of the relation is the Cells, representing the initial
   -- values on entry to the loop, plus the iteration counter,
   -- representing the number of the iteration: 0, 1, 2, ...
   --
   -- The range of the relation is the Cells, representing the values
   -- at the start of the loop on the given iteration. A cell is marked
   -- with a dash (changed) if it is an Induction variable or is not
   -- an Invariant cell. Each Induction variable is constrained to its
   -- initial value (from the domain) plus its Step times the iteration
   -- counter.
   --
   -- This relation expresses no other loop properties, such as the
   -- actual initial values or the termination conditions.


   --
   ---   Calculation operations
   --


   function Start return Calc_Handle_T;
   --
   -- Starts a calculator instance.
   --
   -- Using  : nothing
   -- Giving : calculator handle.


   procedure Stop (Calc : in out Calc_Handle_T);
   --
   -- Stops a calculator instance.
   --
   -- Using  : calculator-handle
   -- Giving : updated (now invalid) calculator handle.


   procedure Comment (
      Text : in String;
      Calc : in Calc_Handle_T);
   --
   -- Provides a textual comment or description of the calculation
   -- about to be performed, or of the result(s) just calculated.
   -- This has no effect on the calculation, but may help the user
   -- understand problems and correct them.


   procedure Assign (
      Target : in Identifier_T;
      Value  : in Formula_T;
      Calc   : in Calc_Handle_T);
   --
   -- Assigns the given value to the named target in the
   -- specified calculator. There are no output parameters,
   -- but the calculator stores the Value under the Target
   -- identifier for later access in formulae.


   function Value (
      Formula : Formula_T;
      Calc    : Calc_Handle_T)
   return Formula_T;
   --
   -- Evaluates a formula in the specified calculator and
   -- returns the result (with echos and empty lines removed).


   function Subset (
      Left  : Set_T;
      Right : Set_T;
      Calc  : Calc_Handle_T)
   return Boolean;
   --
   -- Whether Left is calculated to be a subset of Right.


   function Is_Null (
      Set   : Set_T;
      Cells : Tuple_T;
      Calc  : Calc_Handle_T)
   return Boolean;
   --
   -- Whether the Set is calculated to be a null (empty) set
   -- for the given Cells.


   function Is_Null (
      Set  : Set_T;
      Calc : Calc_Handle_T)
   return Boolean;
   --
   -- Whether the one-dimensional Set is calculated to be a null
   -- (empty) set. The cells contained in the Set are irrelevant.


   function Superset (
      Set         : Set_T;
      Of_Interval : Storage.Bounds.Interval_T;
      Calc        : Calc_Handle_T)
   return Boolean;
   --
   -- Whether the one-dimensional Set is calculated to be a superset
   -- Of the given Interval, that is, whether the Interval is a
   -- subset of the Set.


   function Hull_Bound (
      Set  : Set_T;
      Calc : Calc_Handle_T)
   return Storage.Bounds.Interval_T;
   --
   -- Given a one-dimensional set, this function computes bounds
   -- on the convex hull of the set.


   Unbounded_Set : exception;
   --
   -- Signals an attempt to list the Values in an unbounded set.


   function Values (
      Set  : Set_T;
      Calc : Calc_Handle_T)
   return Storage.Bounds.Value_List_T;
   --
   -- The list of values in the one-dimensional Set, provided
   -- that the set is bounded (using Hull_Bound). Propagates
   -- Unbounded_Set otherwise.
   --
   -- The length of the result is bounded by
   -- Storage.Bounds.Opt.Max_Listed_Values. If there are more
   -- values, the function propagates Unbounded_Set.

   --
   -- Creation of new pools and fluxes:
   --

   function New_Pool (
      Cells : Cell_Set_T;
      Value : Set_T;
      Calc  : Calc_Handle_T)
   return Pool_T;
   --
   -- Creates a new pool, with the given cells as the domain
   -- tuple, and assigns it the given set value using the given
   -- calculator.


   function New_Flux (
      Cells : Cell_Set_T;
      Value : Relation_T;
      Calc  : Calc_Handle_T)
   return Flux_T;
   --
   -- Creates a new flux, with the given cells as the domain
   -- and range tuples, and assigns it the given relation value
   -- using the given calculator.


   function Identity_Flux (
      Domain : Cell_Set_T;
      Calc   : Calc_Handle_T)
   return Flux_T;
   --
   -- The identity-relation flux on the given cells, with
   -- no domain or range constraints.


   --
   ---   Identifiers specific to Bound-T algorithms
   --

   function Into_Id (Step : Flow.Step_T      ) return Identifier_T;
   function Into_Id (Node : Flow.Node_Index_T) return Identifier_T;
   --
   -- The identifier for the "Into" set or relation for the step/node.
   -- This set or relation is the union of the incoming pools or fluxes,
   -- as represented by the "flow" sets or relations on the incoming edges.
   -- The node is given by a node index, rather than a node, so that
   -- this function can be applied as well to "slimmed" nodes.


   function Eff_Id (Step : Flow.Step_T      ) return Identifier_T;
   function Eff_Id (Node : Flow.Node_Index_T) return Identifier_T;
   --
   -- The identifier for the "Effect" relation for the step/node.
   -- This relation contains the cell-value transformations that
   -- represent the arithmetic effect of the instructions in the node.


   function From_Id (Step : Flow.Step_T      ) return Identifier_T;
   function From_Id (Node : Flow.Node_Index_T) return Identifier_T;
   --
   -- The identifier for the "From" set or relation for the step/node.
   -- This set or relation represents the data state after the node
   -- is executed, and is just "Into" joined with "Effect".


   function Flow_Id (Edge : Flow.Step_Edge_T ) return Identifier_T;
   function Flow_Id (Edge : Flow.Edge_Index_T) return Identifier_T;
   --
   -- The identifier for the "Flow" set or relation for the edge.
   -- This set or relation represents the data flowing along the edge,
   -- and is the "From" set or relation of the source node, restricted
   -- to the range defined by the precondition of the edge.


end Calculator.Formulas;
