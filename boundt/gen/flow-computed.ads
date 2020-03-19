-- Flow.Computed (decl)
--
-- Dynamic flow edges where the target address is defined by an
-- arithmetic expression, either directly (the expression computes
-- the address) or indirectly (some function computes the address
-- from the value of the expression).
--
-- The analysis of these edges is more precise than for the edges
-- defined in the sibling package Flow.Indexed because here we are
-- not content with an interval-based analysis but try to enumerate
-- all feasible values of the expression.
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
-- $Date: 2015/10/24 19:36:48 $
--
-- $Log: flow-computed.ads,v $
-- Revision 1.8  2015/10/24 19:36:48  niklas
-- Moved to free licence.
--
-- Revision 1.7  2013/12/20 21:18:29  niklas
-- Extended procedures Apply and Apply_Arithmetic to signal False_Path
-- if the bounds provide no values at all.
--
-- Revision 1.6  2013/12/08 20:27:36  niklas
-- Extended the Apply operation, in the case of non-arithmetic bounds
-- for a dynamic edge where the target is a single cell, to try to derive
-- the list of values of this cell, and thereby resolve the edge. Also,
-- this operation now emits a Note if it does nothing else.
-- Uncommmented the [not] overriding indicators, bringing them into
-- effect.
--
-- Revision 1.5  2007-07-21 18:18:41  niklas
-- BT-CH-0064. Support for AVR/IAR switch-handler analysis.
--
-- Revision 1.4  2007/01/25 21:25:14  niklas
-- BT-CH-0043.
--
-- Revision 1.3  2006/06/03 12:14:29  niklas
-- Changed Edge_T to keep track of the earlier resolved values
-- (as in the former Values_Edge_T) and deleted the functions
-- and parameters relating to target-step tags. This makes Edge_T
-- easier to use for dynamic calls. Deleted Values_Edge_T; use
-- instead Edge_T or the new type Flow.Computed.Tags.Edge_T.
-- Moved type Tags_Edge_T to the new child package Tags to form
-- the type Flow.Computed.Tags.Edge_T. Note, however, that the
-- new type keeps track of new resolved targets with a value list,
-- like the old Values_Edge_T, and not by looking at the existing
-- static successors of the source of the boundable edge.
--
-- Revision 1.2  2006/05/06 06:59:20  niklas
-- BT-CH-0021.
--
-- Revision 1.1  2006/04/28 09:53:36  niklas
-- First version.
--


with Arithmetic;
with Flow.Opt;
with Storage.Bounds;
with Unbounded_Vectors;


package Flow.Computed is


   subtype Value_List_T is Storage.Bounds.Value_List_T;
   --
   -- A list of values.


   package Value_Vectors
   is new Unbounded_Vectors (
      Element_Type => Arithmetic.Value_T,
      Vector_Type  => Value_List_T,
      Deallocate   => Flow.Opt.Deallocate);
   --
   -- For listing the values that have already been resolved
   -- from the boundable edge and traced in the flow-graph.


   type Unbounded_Value_Vector_T is new Value_Vectors.Unbounded_Vector;
   --
   -- A list of values that have been used to resolve an edge.
   -- The default initial value is the empty list.


   --
   ---   Edge_T
   --


   type Edge_T is abstract new Boundable_Edge_T
   with record
      Target : Arithmetic.Expr_Ref;
      Traced : Unbounded_Value_Vector_T;
   end record;
   --
   -- A dynamic transfer of control to an address that is computed by
   -- a Target expression.
   --
   -- The Traced vector contains the Target values that have been
   -- derived from bounds on the edge and processed by the operations
   -- that resolve targets to static edges.
   --
   -- To resolve an Edge_T we enumerate the set of feasible values of
   -- the Target expression that have not yet been traced. The precision
   -- of this set depends on the nature of the Target expression, but
   -- it is the most precise method we have.


   type Edge_Ref is access all Edge_T'Class;
   --
   -- A reference to a heap-allocated computed edge object.


   overriding
   function Basis (Item : Edge_T) return Storage.Cell_List_T;
   --
   -- The basis of a Tags_Edge_T consists of the cells used in
   -- the target expression, Item.Target.
   --
   -- Overrides (implements) Storage.Bounds.Basis.


   overriding
   procedure Add_Basis_Cells (
      From : in     Edge_T;
      To   : in out Storage.Cell_Set_T);
   --
   -- Adds all the cells From the boundable item's Basis set To the
   -- given set of cells.
   --
   -- Overrides Storage.Bounds.Add_Basis_Cells by scanning the Target
   -- expression and adding cells directly To the desired set, without
   -- using the Basis function.


   overriding
   procedure Add_Basis_Cells (
      From  : in     Edge_T;
      To    : in out Storage.Cell_Set_T;
      Added : in out Boolean);
   --
   -- Adds all the cells From the boundable item's Basis set To the
   -- given set of cells. If some of the added cells were not already
   -- in the set, Added is set to True, otherwise (no change in the
   -- set) Added is not changed.
   --
   -- Overrides Storage.Bounds.Add_Basis_Cells by scanning the Target
   -- expression and adding cells directly To the desired set, without
   -- using the Basis function.


   overriding
   function Is_Used (Cell : Storage.Cell_T; By : Edge_T)
   return Boolean;
   --
   -- Whether the given Cell is used By the computed edge, in other
   -- words whether the Cell belongs to the Basis of the edge.
   --
   -- Overrides Storage.Bounds.Is_Used by scanning the Target
   -- expression directly, without using the Basis function.


   function Image (Item : Edge_T) return String;
   --
   -- Displays the Item's tag and Target expression.
   --
   -- Overrides Storage.Bounds.Image.


   overriding
   procedure Apply (
      Bounds : in     Storage.Bounds.Bounds_T'Class;
      Upon   : in out Edge_T;
      Graph  : in     Graph_T);
   --
   -- Applies the Bounds Upon the boundable edge as explained for the
   -- abstract declaration Flow.Apply (Upon : Boundable_Edge_T).
   --
   -- The default implementation works as follows:
   --
   -- > If Bounds is in Arithmetic.Bounds_T'Class, uses the Apply
   --   operation for such bounds, declared below, with redispatch on Upon.
   --
   -- > Otherwise, if the Target is a single cell, asks the Bounds for
   --   the list of Values of this cell under these Bounds, and then
   --   applies Add_New_Targets, below. However, if the value list is
   --   empty, the procedure instead propagates False_Path.

   -- > Otherwise does nothing (leaving the edge Unresolved). This
   --   case is ripe for overriding in derived types.
   --
   -- Overrides (implements) Flow.Apply (Upon : Boundable_Edge_T).


   not overriding
   procedure Apply_Arithmetic (
      Bounds : in     Arithmetic.Bounds_T'Class;
      Upon   : in out Edge_T;
      Graph  : in     Graph_T);
   --
   -- A special version of the Apply operation, above, for the case where
   -- the Bounds can bound arithmetic expressions.
   --
   -- The default implementation computes the list of values possible
   -- for the Upon.Target expression under the given Bounds and passes
   -- it to Add_New_Targets (see below) with redispatching on Upon.
   --
   -- There are three special cases:
   --
   -- > If the value list is empty, the procedure propagates False_Path.
   --
   -- > If the Bounds are so loose that there would be too many new
   --   Target values, the operation does nothing (and leaves Upon in
   --   the Unresolved state).
   --
   -- > If the Bounds are so tight that there are no new Target values,
   --   or if Add_New_Targets added no new static edges, the operation
   --   calls Mark_Stable (Upon) to indicate that this dynamic edge is
   --   fully resolved (within the present computation model).
   --
   -- I would like to name the procedure Apply, but Gnat 3.15p
   -- considers this ambiguous in a call, although it accepts it here.
   -- So the name Apply_Arithmetic is a work-around.


   not overriding
   procedure Add_New_Targets (
      Edge   : in out Edge_T;
      Values : in     Value_List_T;
      Graph  : in     Graph_T);
   --
   -- Adds the new static edges resolved from the boundable Edge
   -- to the Graph, when some bounds have produced the list of
   -- possible Values of Edge.Target, some of which may be new and
   -- others not.
   --
   -- The default implementation filters the Values against Edge.Traced
   -- and for each new target value calls Add_Target (see below) with
   -- redispatching on the Edge. The default implementation adds all
   -- new target values to Edge.Traced so that Add_Target is never
   -- called twice with the same Value for the same Edge.


   not overriding
   procedure Add_Target (
      Edge  : in out Edge_T;
      Value : in     Arithmetic.Value_T;
      Graph : in     Graph_T)
   is abstract;
   --
   -- Adds the new static edge resolved from the boundable Edge to the
   -- Graph, when some bounds have produced a new Value of Edge.Target.
   -- For some kinds of Edge, the given Value may be invalid (eg. for
   -- reasons of alignment) and in this case the operation should do
   -- nothing (but may of course emit a warning message).


   not overriding
   function Edge_Cond (
      Edge  : Edge_T;
      Value : Arithmetic.Value_T;
      Graph : Graph_T)
   return Arithmetic.Condition_T;
   --
   -- The precondition to be assigned to the edge that will (or may)
   -- be added by Add_Target for the same parameters.
   --
   -- The default implementation returns Edge.Target = Value.


   not overriding
   function Edge_Time (
      Edge  : Edge_T;
      Value : Arithmetic.Value_T;
      Graph : Graph_T)
   return Processor.Time_T;
   --
   -- The execution time to be assigned to the edge that will (or may)
   -- be added by Add_Target for the same parameters.
   --
   -- The default implementation returns zero.


   not overriding
   procedure Report_Bounds (
      Edge   : in out Edge_T;
      Values : in     Value_List_T);
   --
   -- Reports the Values computed for the Target of the Edge.
   -- The Values lists the possible Target values (if any), including
   -- target values already processed by Add_Target.
   -- The default implementation is null.


end Flow.Computed;
