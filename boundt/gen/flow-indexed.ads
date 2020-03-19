-- Flow.Indexed (decl)
--
-- Dynamic flow edges where the target is defined by an arithmetic expression.
-- For example, jumps via an indexed table of target addresses in constant
-- memory.
--
-- The possible targets are found by an interval analysis followed by a
-- congruence (alignment) filter. This method is less precise than the
-- method used in the sibling package Flow.Computed, which see.
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
-- $Revision: 1.6 $
-- $Date: 2015/10/24 19:36:49 $
--
-- $Log: flow-indexed.ads,v $
-- Revision 1.6  2015/10/24 19:36:49  niklas
-- Moved to free licence.
--
-- Revision 1.5  2009-11-27 11:28:07  niklas
-- BT-CH-0184: Bit-widths, Word_T, failed modular analysis.
--
-- Revision 1.4  2008/04/22 13:27:52  niklas
-- Added Edge_T.Allowed, to bound the Index values a priori.
--
-- Revision 1.3  2007/07/21 18:18:42  niklas
-- BT-CH-0064. Support for AVR/IAR switch-handler analysis.
--
-- Revision 1.2  2006/02/27 09:20:31  niklas
-- Added Report_Bounds, typically used for warnings about
-- address tables that are assumed to be constant.
--
-- Revision 1.1  2005/02/16 21:11:44  niklas
-- BT-CH-0002.
--


package Flow.Indexed is


   type Edge_T is abstract new Boundable_Edge_T
   with record
      Index   : Arithmetic.Expr_Ref;
      Aligned : Storage.Bounds.Positive_Value_T;
      Allowed : Storage.Bounds.Interval_T := Storage.Bounds.Universal_Interval;
      Traced  : Storage.Bounds.Interval_T := Storage.Bounds.Void_Interval;
   end record;
   --
   -- A dynamic transfer of control to an address that is defined by
   -- an Index expression (and usually other stuff, such as an address
   -- table, not given here).
   --
   -- Index
   --    The expression that defines the target address in some way
   --    not further specified here (for example, by acting as the
   --    index into an address table).
   -- Aligned
   --    Only Index values that are multiples of Aligned are good
   --    and meaningful. This can be useful when instructions or
   --    address-table elements are larger that the least addressable
   --    memory element and must be aligned accordingly.
   -- Allowed
   --    Only Index values in this interval are good and meaningful.
   -- Traced
   --    The Index values in the Traced range have already been processed
   --    and possibly resolved into static edges in the flow-graph.
   --    Only new Index values outside the Traced range can give rise
   --    to new static edges in the flow-graph.
   --
   -- To resolve an Edge_T we compute the interval of Index values,
   -- intersect it with the Allowed interval, and within the intersection
   -- find the values that are multiples of Aligned and have not yet
   -- been Traced. If there are other constraints on the Index this
   -- method may include some infeasible values.


   type Edge_Ref is access all Edge_T'Class;
   --
   -- A reference to a heap-allocated indexed edge object.


   function Basis (Item : Edge_T) return Storage.Cell_List_T;
   --
   -- The basis of an Indexed Edge_T consists of the cells used in
   -- the index expression, Item.Index.
   --
   -- Overrides (implements) Storage.Bounds.Basis.


   procedure Add_Basis_Cells (
      From : in     Edge_T;
      To   : in out Storage.Cell_Set_T);
   --
   -- Adds all the cells From the boundable item's Basis set To the
   -- given set of cells.
   --
   -- Overrides Storage.Bounds.Add_Basis_Cells by scanning the Index
   -- expression and adding cells directly To the desired set, without
   -- using the Basis function.


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
   -- Overrides Storage.Bounds.Add_Basis_Cells by scanning the index
   -- expression and adding cells directly To the desired set, without
   -- using the Basis function.


   function Is_Used (Cell : Storage.Cell_T; By : Edge_T)
   return Boolean;
   --
   -- Whether the given Cell is used By the indexed edge, in other words
   -- whether the Cell belongs to the Basis of the edge.
   --
   -- Overrides Storage.Bounds.Is_Used by scanning the index
   -- expression directly, without using the Basis function.


   function Image (Item : Edge_T) return String;
   --
   -- Displays the Item's tag, Index expression and Traced range.
   --
   -- Overrides Storage.Bounds.Image.


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
   -- > Otherwise does nothing (leaving the edge Unresolved). This
   --   case is ripe for overriding in derived types.
   --
   -- Overrides (implements) Flow.Apply (Upon : Boundable_Edge_T).


   procedure Apply_Arithmetic (
      Bounds : in     Arithmetic.Bounds_T'Class;
      Upon   : in out Edge_T;
      Graph  : in     Graph_T);
   --
   -- A special version of the Apply operation, above, for the case where
   -- the Bounds can bound arithmetic expressions.
   --
   -- The default implementation computes the interval of values possible
   -- for the Upon.Index expression under the given Bounds, lists the new
   -- Index values allowed by this interval and Upon.Aligned but not
   -- covered by Upon.Traced, and calls the Index operation (see below)
   -- for each such Value. Finally Upon.Traced is updated to be the new
   -- internal allowed by the Bounds.
   --
   -- There are two special cases:
   --
   -- > If the Bounds are so loose that there would be too many new
   --   Index values, the operation does nothing (and leaves Upon in
   --   the Unresolved state).
   --
   -- > If the Bounds are so tight that there are no new Index values,
   --   the operation calls Mark_Stable (Upon) to indicate that this
   --   dynamic edge is fully resolved (within the present computation
   --   model).
   --
   -- I would like to name the procedure Apply, but Gnat 3.15p
   -- considers this ambiguous in a call, although it accepts it here.
   -- So the name Apply_Arithmetic is a work-around.


   procedure Report_Bounds (
      Edge     : in out Edge_T;
      Interval : in     Storage.Bounds.Interval_T;
      Values   : in     Storage.Bounds.Value_List_T);
   --
   -- Reports the bounds computed for the Index of the Edge.
   -- Interval is the newest computed Index interval and Values
   -- lists the new values (if any) allowed by this Interval
   -- relative to the bounds computed in earlier iterations.
   -- The default implementation is null.


   procedure Index (
      Edge  : in out Edge_T;
      Value : in     Arithmetic.Value_T;
      Graph : in     Graph_T)
   is abstract;
   --
   -- Applies a new Value as an index to the indexable Edge, possibly
   -- giving new static edges in the Graph (usually only one new edge).
   --
   -- This operation is usually called from the Apply operation for
   -- arithmetic bounds, above. The index Value is a value allowed
   -- by Apply.Bounds for Edge.Index and is a multiple of Edge.Align
   -- but is not in Edge.Traced.


end Flow.Indexed;
