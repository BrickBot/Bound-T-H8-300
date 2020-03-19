-- Arithmetic.Pointers (decl)
--
-- Dynamic references to storage cells where the identity of the
-- referent cell is defined by one or more arithmetic expressions.
-- For example, references to addressable memory using an address
-- expression.
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
-- $Revision: 1.4 $
-- $Date: 2015/10/24 19:36:46 $
--
-- $Log: arithmetic-pointers.ads,v $
-- Revision 1.4  2015/10/24 19:36:46  niklas
-- Moved to free licence.
--
-- Revision 1.3  2006-02-27 10:03:06  niklas
-- Added function Image_Tag for use in function Image so that
-- derived types can use short "tags" without reimplementing
-- the whole Image function.
--
-- Revision 1.2  2005/03/04 09:36:41  niklas
-- Added implementation for Alias_Range (Boundable_Pointer_T) as
-- a redispatch to Alias_Range with an Intervals parameter, too.
-- Added type Interval_Pointer_T.
--
-- Revision 1.1  2005/02/16 21:11:36  niklas
-- BT-CH-0002.
--


with Storage.References;


package Arithmetic.Pointers is


   --
   --    References based on (address) expressions
   --


   type Boundable_Pointer_T (Dim : Positive)
   is
      abstract new Storage.References.Boundable_T
   with record
      Expr : Expressions_T (1 .. Dim);
   end record;
   --
   -- A dynamic reference to a storage cell where the identity of the
   -- cell is defined by the values of some arithmetic expressions, Expr.
   -- For example, the expressions can compute memory addresses.


   type Boundable_Pointer_Ref is access all Boundable_Pointer_T'Class;
   --
   -- A reference to a heap-allocated boundable pointer object.


   function Alias_Range (Ref : Boundable_Pointer_T)
   return Storage.Alias_Range_T;
   --
   -- The alias range that can be reached by the Pointer, with no known
   -- bounds on cell values.
   --
   -- The default implementation redispatches to the Alias_Range function
   -- that takes an Interval_List_T - see below - with Universal Intervals
   -- in the list.
   --
   -- Overrides (implements) Storage.References.Alias_Range.


   function Alias_Range (
      Pointer  : Boundable_Pointer_T;
      Interval : Storage.Bounds.Interval_List_T)
   return Storage.Alias_Range_T
   is abstract;
   --
   -- The alias range that can be reached by the Pointer, when the value
   -- of each Pointer.Expr(i) is bounded to Interval(i).


   function Referent (
      Pointer : Boundable_Pointer_T;
      Value   : Storage.Bounds.Value_List_T)
   return Storage.Cell_T
   is abstract;
   --
   -- The cell to which the Pointer refers, when the value of each
   -- Pointer.Expr(i) equals Value(i).
   --
   -- There is no default implementation (the function is abstract)
   -- because this function should always return a real cell and
   -- never return No_Cell.
   --
   -- TBD if it should raise False_Path on errors.


   procedure Constrain (
      Pointer  : in     Boundable_Pointer_T;
      Interval : in     Storage.Bounds.Interval_List_T;
      Giving   :    out Boundable_Pointer_Ref);
   --
   -- Constrains the given Pointer, using the knowledge that each
   -- Pointer.Expr(i) is bounded to Interval(i), perhaps Giving a more
   -- constrained pointer. If the Pointer is not further constrained,
   -- the procedure returns Giving as null.
   --
   -- The default implementation returns Giving as null.


   function Basis (Item : Boundable_Pointer_T) return Storage.Cell_List_T;
   --
   -- The basis of a Boundable_Pointer_T consists of the cells used in
   -- the arithmetic expressions, Item.Expr.
   --
   -- Overrides (implements) Storage.References.Basis.


   procedure Add_Basis_Cells (
      From : in     Boundable_Pointer_T;
      To   : in out Storage.Cell_Set_T);
   --
   -- Adds all the cells From the boundable Item's Basis set To the
   -- given set of cells.
   --
   -- Overrides Storage.References.Add_Basis_Cells by scanning the pointer
   -- expressions and adding cells directly To the desired set, without
   -- using the Basis function.


   procedure Add_Basis_Cells (
      From  : in     Boundable_Pointer_T;
      To    : in out Storage.Cell_Set_T;
      Added : in out Boolean);
   --
   -- Adds all the cells From the boundable Item's Basis set To the
   -- given set of cells. If some of the added cells were not already
   -- in the set, Added is set to True, otherwise (no change in the
   -- set) Added is not changed.
   --
   -- Overrides Storage.References.Add_Basis_Cells by scanning the pointer
   -- expressions and adding cells directly To the desired set, without
   -- using the Basis function.


   function Is_Used (Cell : Storage.Cell_T; By : Boundable_Pointer_T)
   return Boolean;
   --
   -- Whether the given Cell is used By the boundable, in other words
   -- whether the Cell belongs to the Basis of the boundable.
   --
   -- Overrides Storage.References.Add_Basis_Cells by scanning the pointer
   -- expressions directly, without using the Basis function.


   function Image_Tag (Item : Boundable_Pointer_T) return String;
   --
   -- The "tag" to appear before the index expressions in the Image
   -- of the Item. See function Image below.
   --
   -- The default implementation uses Ada.Tags.Expanded_Name.


   function Image (Item : Boundable_Pointer_T) return String;
   --
   -- Displays the Item's expressions, in brackets, after the Image_Tag,
   -- as in:
   --
   --   tag [ expr1, expr2, ..., exprN ]
   --
   -- except that no blanks are included.
   --
   -- Overrides (implements) Storage.References.Image.


   function Alias_Range (
      Ref   : Boundable_Pointer_T;
      Under : Storage.Bounds.Bounds_T'Class)
   return Storage.Alias_Range_T;
   --
   -- Computes the alias-range of the Ref, Under the bounds, as follows:
   --
   -- > If Under is in Arithmetic.Bounds_T'Class, uses Alias_Range for
   --   such bounds (see below) with redispatch on Ref.
   --
   -- > Otherwise, uses Arithmetic.Pointers.Alias_Range (Ref) without
   --   bounds but with redispatch on Ref.
   --
   -- Overrides (implements) Storage.References.Alias_Range.


   function Arithmetic_Alias_Range (
      Ref   : Boundable_Pointer_T;
      Under : Arithmetic.Bounds_T'Class)
   return Storage.Alias_Range_T;
   --
   -- Computes the interval of values possible for each Ref.Expr, Under
   -- the given bounds, and then uses Alias_Range (Pointer, Interval)
   -- declared above, with redispatching on Ref.
   --
   -- As a special case, if it turns out that the bounds define a unique
   -- value for each Ref.Expr, the function instead computes the
   -- Referent cell using Referent (Pointer, Value) (declared above,
   -- with redispatching on Ref) and then uses Processor.Alias_Group (Cell)
   -- to find the alias range of this cell.
   --
   -- I would like to name the function Alias_Range, but Gnat 3.15p
   -- considers this ambiguous in a call, although it accepts it here.
   -- So the name Arithmetic_Alias_Range is a work-around.
   --
   -- This operation does not override Storage.References.Alias_Range
   -- because the Under parameter is of type Arithmetic.Bounds_T'Class
   -- and not Storage.Bounds.Bounds_T'Class (and because the name is
   -- different as explained above).


   function Referent (
      Ref   : Boundable_Pointer_T;
      Under : Storage.Bounds.Bounds_T'Class)
   return Storage.Cell_T;
   --
   -- Tries to find the cell referenced by Ref, Under the given bounds,
   -- as follows:
   --
   -- > If Under is in Arithmetic.Bounds_T'Class, uses Referent for
   --   such bounds (see below) with redispatching on Ref.
   --
   -- > Otherwise returns No_Cell. This case is ripe for overriding
   --   in derived types.
   --
   -- Overrides (implements) Storage.References.Referent.


   function Arithmetic_Referent (
      Ref   : Boundable_Pointer_T;
      Under : Arithmetic.Bounds_T'Class)
   return Storage.Cell_T;
   --
   -- Tries to find the cell referenced by Ref, by computing the unique
   -- value for each Ref.Expr, Under the given bounds, and then using
   -- Referent (Pointer, Value) declared above, with redispatching on Ref.
   -- If the bounds do not give a unique value for each Ref.Expr the
   -- function returns No_Cell.
   --
   -- I would like to name the function Referent, but Gnat 3.15p
   -- considers this ambiguous in a call, although it accepts it here.
   -- So the name Arithmetic_Referent is a work-around.
   --
   -- This operation does not override Storage.References.Referent
   -- because the Under parameter is of type Arithmetic.Bounds_T'Class
   -- and not Storage.Bounds.Bounds_T'Class (and because the name is
   -- different as explained above).


   procedure Apply (
      Bounds : in     Storage.Bounds.Bounds_T'Class;
      Upon   : in     Boundable_Pointer_T;
      Giving :    out Storage.References.Boundable_Ref);
   --
   -- Applies the Bounds Upon the boundable pointer as follows:
   --
   -- > If Bounds is in Arithmetic.Bounds_T'Class, uses the Apply
   --   operation for such bounds, declared below, with redispatch on Upon.
   --
   -- > Otherwise returns Giving as null. This case is ripe for
   --   overriding in derived types.
   --
   -- Overrides (implements) Storage.References.Apply.


   procedure Apply (
      Bounds : in     Arithmetic.Bounds_T'Class;
      Upon   : in     Boundable_Pointer_T;
      Giving :    out Boundable_Pointer_Ref);
   --
   -- Applies the Bounds Upon the boundable pointer by computing the
   -- interval of values possible for each Upon.Expr under the given
   -- Bounds and then using Constrain (Pointer => Upon, Interval, Giving)
   -- declared above, with redispatch on Upon.
   --
   -- This operation does not override Storage.References.Apply because
   -- of the different types of the parameters Bounds and Giving.
 

   --
   --    Some subtypes with small numbers of expressions.
   --


   subtype Boundable_Pointer_1_T is Boundable_Pointer_T (Dim => 1);


   --
   --    References based on (address) expressions which can
   --    be constrained to intervals
   --


   type Interval_Pointer_T (Dim : Positive)
   is
      abstract new Boundable_Pointer_T (Dim)
   with record
      Interval : Storage.Bounds.Interval_List_T (1 .. Dim);
   end record;
   --
   -- Extends the parent type with interval bounds on the (address)
   -- expressions, thus allowing the reference to be incrementally
   -- constrained (refined) to ever narrower (address) intervals.


   type Interval_Pointer_Ref is access all Interval_Pointer_T'Class;
   --
   -- A reference to a dynamic reference of Interval Pointer class.


   function Alias_Range (Ref : Interval_Pointer_T)
   return Storage.Alias_Range_T;
   --
   -- The alias range that can be reached by the Ref, with no known
   -- bounds on cell values beyond those already recorded in
   -- Ref.Interval.
   --
   -- The default implementation redispatches to the Reduced_Alias_Range
   -- function - see below - with the intervals in Ref.Interval.
   --
   -- Overrides (implements) Alias_Range (Boundable_Pointer_T).


   function Alias_Range (
      Pointer  : Interval_Pointer_T;
      Interval : Storage.Bounds.Interval_List_T)
   return Storage.Alias_Range_T;
   --
   -- The alias range that can be reached by the Pointer, when the value
   -- of each Pointer.Expr(i) is (further) bounded to Interval(i),
   -- in addition to the bonds from Pointer.Interval.
   --
   -- The default implementation computes the intersection of each
   -- given Interval with the corresponding Pointer.Interval and then
   -- redispatches to Reduced_Alias_Range - see below.


   procedure Constrain (
      Pointer  : in     Interval_Pointer_T;
      Interval : in     Storage.Bounds.Interval_List_T;
      Giving   :    out Boundable_Pointer_Ref);
   --
   -- Constrains the given Pointer, using the knowledge that each
   -- Pointer.Expr(i) is bounded to Interval(i).
   --
   -- The procedure computes the intersection of each given Interval
   -- with the corresponding (same index) Pointer.Interval. If the
   -- intersection is narrower than the corresponding Pointer.Interval,
   -- the procedure creates Giving as a copy of Pointer but with the
   -- narrower Interval(s). Otherwise, that is if each given Interval
   -- contains the corresponding Pointer.Interval, the procedure
   -- returns Giving as null.
   --
   -- Overrides Constrain (Boundable_Pointer_T).


   function Reduced_Alias_Range (
      Pointer  : Interval_Pointer_T;
      Interval : Storage.Bounds.Interval_List_T)
   return Storage.Alias_Range_T
   is abstract;
   --
   -- The alias range that can be reached by the Pointer, when the value
   -- of each Pointer.Expr(i) is bounded to Interval(i). Here the given
   -- Interval(i) is always known to be a subset of Pointer.Interval(i)
   -- (but may be equal to or identical with Pointer.Interval(i)).


end Arithmetic.Pointers;
