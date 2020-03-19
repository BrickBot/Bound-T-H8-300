-- Calling.Single_Cell (decl)
--
-- Calling protocols that depend on the value of a single cell.
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
-- $Date: 2015/10/24 19:36:48 $
--
-- $Log: calling-single_cell.ads,v $
-- Revision 1.4  2015/10/24 19:36:48  niklas
-- Moved to free licence.
--
-- Revision 1.3  2009-11-27 11:28:07  niklas
-- BT-CH-0184: Bit-widths, Word_T, failed modular analysis.
--
-- Revision 1.2  2005/03/04 09:40:21  niklas
-- Added the new primitive operations Static_Caller_Cell and
-- Dynamic_Caller_Cell, and added a default implementation of
-- Caller_Cell that makes use of them.
--
-- Revision 1.1  2005/02/23 09:05:16  niklas
-- BT-CH-0005.
--


with Arithmetic;
with Storage;
with Storage.Bounds;


package Calling.Single_Cell is


   type Protocol_T is abstract new Calling.Protocol_T with record
      Cell     : Storage.Cell_T;
      Interval : Storage.Bounds.Interval_T :=
         Storage.Bounds.Universal_Interval;
   end record;
   --
   -- A class of calling protocols for which dynamic cell mapping
   -- depends only on the value of one Cell in the caller at the point
   -- of call. For example, this cell might be the local stack height.
   --
   -- The protocol can be constrained by bounding the value of the Cell
   -- using an Interval_T. The protocol is static if the Interval defines
   -- a single value, that is, if Storage.Bounds.Singular (Interval)
   -- is True.
   --
   -- The initial value for Interval may be reduced if there is good
   -- reason. For example, if the Cell is a stack-height, then it should
   -- have only non-negative values.


   type Protocol_Ref is access all Protocol_T'Class;
   --
   -- A reference to a single-cell-protocol object on the heap.


   function Static (Item : Protocol_T) return Boolean;
   --
   -- True if Item.Interval allows a single value for Interval.Cell.
   --
   -- Overrides (implements) Storage.Bounds.Static.


   function Basis (Item : Protocol_T) return Storage.Cell_List_T;
   --
   -- Returns the Item.Cell, or a null list if the protocol is constrained
   -- to be static.
   --
   -- Overrides (implements) Storage.Bounds.Basis.


   function Image (Item : Protocol_T) return String;
   --
   -- Returns "Single_Cell" and the Image of Item.Interval as
   -- bounds on Item.Cell.


   function Static_Caller_Cell (
      Callee : Storage.Cell_T;
      Under  : Protocol_T)
   return Storage.Cell_T;
   --
   -- The caller-frame cell that is associated with (describes the
   -- same data-location as) the given callee-frame cell, Under the
   -- given calling protocol when the Callee has a Static Map_Kind.
   --
   -- This is a new primitive operation for Protocol_T, not inherited
   -- from the parent type. This operation is used in the default
   -- implementation of the inherited Caller_Cell function, see below.
   --
   -- The default implementation emits a Fault and raises Program_Error.
   -- This is not a working implementation but it means that derived
   -- types that override the inherited Caller_Cell function and have
   -- no need for this new Static_Caller_Cell function do not have to
   -- provide another implementation of this new function.


   function Dynamic_Caller_Cell (
      Callee : Storage.Cell_T;
      Under  : Protocol_T;
      Value  : Arithmetic.Value_T)
   return Storage.Cell_T;
   --
   -- The caller-frame cell that is associated with (describes the
   -- same data-location as) the given callee-frame cell, Under the
   -- given calling protocol when the Value of Under.Cell is known.
   --
   -- This is a new primitive operation for Protocol_T, not inherited
   -- from the parent type. This operation is used in the default
   -- implementation of the inherited Caller_Cell function, see below.
   --
   -- The default implementation emits a Fault and raises Program_Error.
   -- This is not a working implementation but it means that derived
   -- types that override the inherited Caller_Cell function and have
   -- no need for this new Dynamic_Caller_Cell function do not have to
   -- provide another implementation of this new function.


   function Caller_Cell (
      Callee : Storage.Cell_T;
      Under  : Protocol_T)
   return Storage.Cell_T;
   --
   -- The caller-frame cell that is associated with (describes the
   -- same data-location as) the given callee-frame cell, Under the
   -- given calling protocol.
   --
   -- The default implementation works as follows:
   --
   -- > if the Callee cell has a Dynamic mapping:
   --
   --      if Under.Interval constrains Under.Cell to a single value
   --         return Dynamic_Caller_Cell (Callee, Under, Value),
   --            with redispatching on Under
   --      else
   --         return No_Cell (unbounded)
   --
   -- > else if the Callee cell has a Static mapping:
   --
   --      return Static_Caller_Cell (Callee, Under)
   --         with redispatching on Under
   --
   -- > else return the Callee unchanged.
   --
   -- Overrides (implements) Calling.Caller_Cell.


   procedure Apply (
      Bounds : in     Storage.Bounds.Bounds_T'Class;
      Upon   : in     Protocol_T;
      Giving :    out Calling.Protocol_Ref);
   --
   -- Uses the Bounds on Upon.Cell to constrain Upon.Interval and thus
   -- perhaps Giving a more constrained protocol which is the same as
   -- the given one but with a narrower Upon.Interval. Returns Giving
   -- as null if the Bounds do not constrain Upon.Interval further.
   --
   -- Overrides (implements) Calling.Apply.


end Calling.Single_Cell;
