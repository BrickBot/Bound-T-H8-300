-- Calling.Stacked (decl)
--
-- Calling protocols that depend on the stack-heights at the call.
-- This class of protocol should be used only when the processor
-- and program under analysis have some stacks.
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
-- $Revision: 1.3 $
-- $Date: 2015/10/24 19:36:48 $
--
-- $Log: calling-stacked.ads,v $
-- Revision 1.3  2015/10/24 19:36:48  niklas
-- Moved to free licence.
--
-- Revision 1.2  2007-12-17 13:54:35  niklas
-- BT-CH-0098: Assertions on stack usage and final stack height, etc.
--
-- Revision 1.1  2005/02/23 09:05:17  niklas
-- BT-CH-0005.
--


with Programs;
with Storage;
with Storage.Bounds;


package Calling.Stacked is


   type Stack_Height_Intervals_T is
      array (Programs.Stack_Index_T range <>) of Storage.Bounds.Interval_T;
   --
   -- Interval bounds on the stack height of all stacks.


   type Protocol_T (Num_Stacks : Natural) is abstract new Calling.Protocol_T
   with record
      Program   : Programs.Program_T;
      Intervals : Stack_Height_Intervals_T (1 .. Num_Stacks) := (
         others => Storage.Bounds.Universal_Interval);
   end record;
   --
   -- A class of calling protocols for which dynamic cell mapping
   -- depends only on the values of the local stack height for all
   -- stacks in the caller at the point of call.
   --
   -- The protocol can be constrained by bounding the value of the
   -- local stack heights using the Intervals. The protocol is static
   -- if the Intervals define a single value for each stack height,
   -- that is, if Storage.Bounds.Singular (Intervals(S)) is True
   -- for all S in Intervals'Range.
   --
   -- The initial value for Intervals reflects a completely unknown
   -- value or range of local stack height.
   --
   -- Program
   --    The program in which the protocol occurs.
   --    The basis cells are Programs.Height for Programs.Stacks (Program)
   --    excluding those bounded to a single value.
   -- Intervals
   --    The current bounds on the stack-height cells as determined for
   --    this instance of the protocol.


   type Protocol_Ref is access all Protocol_T'Class;
   --
   -- A reference to a stacked protocol object on the heap.


   function Static (Item : Protocol_T) return Boolean;
   --
   -- True if Item.Intervals allows a single value for each stack-height
   -- cell.
   --
   -- Overrides (implements) Storage.Bounds.Static.


   function Basis (Item : Protocol_T) return Storage.Cell_List_T;
   --
   -- Returns those stack-height cells for which Item.Intervals allows
   -- more than one value.
   --
   -- Overrides (implements) Storage.Bounds.Basis.


   function Image (Item : Protocol_T) return String;
   --
   -- Returns "Stack" followed by the Intervals, separated by commas.


   procedure Apply (
      Bounds : in     Storage.Bounds.Bounds_T'Class;
      Upon   : in     Protocol_T;
      Giving :    out Calling.Protocol_Ref);
   --
   -- Uses the Bounds on the stack-height cells to constrain Upon.Intervals
   -- and thus perhaps Giving a more constrained protocol which is the
   -- same as the given one but with a narrower Upon.Interval. Returns
   -- Giving as null if the Bounds do not constrain Upon.Intervals further.
   --
   -- Overrides (implements) Calling.Apply.


end Calling.Stacked;
