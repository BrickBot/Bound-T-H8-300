-- Interval_Maps (decl)
--
-- Data structures that implement partial functions (mappings) from
-- an ordered (index) domain to a value range such that the support
-- is a set of (non-intersecting) index intervals and the value is
-- constant within each interval. However, the intervals can be
-- adjacent, so in principle any mapping from the index domain to
-- the range set can be represented.
--
-- The data structure is generic in the index type and value type.
--
-- Author: Niklas Holsti.
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
-- $Date: 2015/10/24 20:53:54 $
--
-- $Log: interval_maps.ads,v $
-- Revision 1.6  2015/10/24 20:53:54  niklas
-- Moved to free licence.
--
-- Revision 1.5  2008-11-08 17:39:44  niklas
-- Added function Next_Undefined.
--
-- Revision 1.4  2007/04/18 18:35:49  niklas
-- BT-CH-0057.
--
-- Revision 1.3  2007/03/29 12:51:49  niklas
-- BT-CH-0056.
--
-- Revision 1.2  2007/01/25 21:25:37  niklas
-- BT-CH-0043.
--
-- Revision 1.1  2004/04/24 17:17:59  niklas
-- First version.
--

--:dbpool with GNAT.Debug_Pools;


generic

   type Index_Type is mod <>;
   --
   -- The index type.

   type Value_Type is private;
   --
   -- The type of the vector elements.

   with function Image (Item : Value_Type) return String;
   --
   -- Just for Show.

   Deallocate : in out Boolean;
   --
   -- Option variable to enable or disable the use of
   -- Unchecked_Deallocation to discard unused heap memory.

package Interval_Maps is


   type Interval_Map_T is limited private;
   --
   -- The mapping is like "array (Index_Type) of Value_Type", but
   -- is defined only for some set of intervals of the index type,
   -- and has a constant value throughout each such interval.
   -- It is highly likely that heap memory is used to represent
   -- the mapping, so we make it limited.


   type Interval_T is record
      First : Index_Type;
      Last  : Index_Type;
   end record;
   --
   -- An interval of indices, containing all the index values
   -- in the range First .. Last, inclusive.
   --
   -- If First > Last, the interval is empty and the values of
   -- First and Last are (otherwise) irrelevant.


   procedure Initialize (Map : in out Interval_Map_T);
   --
   -- Creats and initializes a map object.
   -- This operation should be called once on each Interval_Map
   -- object before it is used in any other operation of this
   -- package.


   procedure Set (
      Map  : in out Interval_Map_T;
      From : in     Interval_T;
      To   : in     Value_Type);
   --
   -- Sets the Map to have the given value To for all indices
   -- in the Interval. If the map is already defined in some part
   -- of this interval, the new value To overrides the old definition
   -- (but only in Interval). In this case, the definition intervals
   -- may be split by intersections with the given Interval.
   -- If the interval is empty, the operation has no effect.


   Undefined_Value : exception;
   --
   -- Raised upon an attempt to access the value of a map for
   -- an index where the map value is not defined or is multiply
   -- defined.


   function Value (
      Map   : Interval_Map_T;
      Index : Index_Type)
   return Value_Type;
   --
   -- The value of the map at the given index.
   -- Raises Undefined_Value if the value is not defined.


   function Value (
      Map  : Interval_Map_T;
      Over : Interval_T)
   return Value_Type;
   --
   -- The value of the map in the given interval, where the
   -- map is expected to have the same value in all the interval.
   -- Raises Undefined_Value if the map is not defined in all
   -- of the interval or takes different values within the
   -- interval or if the interval is empty.


   function First (Map : Interval_Map_T) return Index_Type;
   --
   -- The first (least) index for which the Map defines a value.
   -- Raises Undefined_Value if the Map is defined for no indices.


   function Next (After : Index_Type; Map : Interval_Map_T)
   return Index_Type;
   --
   -- The next (least) index that is > After and for which Map defines
   -- a value. Raises Undefined_Value if there are no such indices.


   function Last (Map : Interval_Map_T) return Index_Type;
   --
   -- The last (greatest) index for which the Map defines a value.
   -- Raises Undefined_Value if the Map is defined for no indices.


   function Defined (Map : Interval_Map_T; Index : Index_Type)
   return Boolean;
   --
   -- Whether the Map is defined for the given Index.


   function Defined (
      Map    : Interval_Map_T;
      Within : Interval_T)
   return Boolean;
   --
   -- Whether the Map is defined for some indices in the Interval.
   -- Note that a True value does not mean that the Map is defined
   -- for all indices in this interval; use Fully_Defined (below)
   -- for such a check.
   -- False is returned if the interval is empty.


   function Fully_Defined (
      Map  : Interval_Map_T;
      Over : Interval_T)
   return Boolean;
   --
   -- Whether the Map is defined for all indices in the interval.
   -- Note that a True value does not mean that the Map has the
   -- same value for all indices in the interval.
   -- True is returned if the interval is empty.


   function Span (
      Around : Index_Type;
      Within : Interval_Map_T)
   return Interval_T;
   --
   -- The maximal interval that contains the index Around and
   -- maps to the same value as Around.
   -- Raises Undefined_Value if the map is not defined for Around.


   function Next_Undefined (After : Index_Type; Map : Interval_Map_T)
   return Index_Type;
   --
   -- The next (least) index that is > After and for which Map does not
   -- define a value. Raises Undefined_Value if there are no such indices.


   procedure Show (Item : in Interval_Map_T);
   --
   -- Displays the map, for checking, on the standard output.


private


   type Map_Object_T;


   type Interval_Map_T is access Map_Object_T;

   --:dbpool Interval_Map_Pool : GNAT.Debug_Pools.Debug_Pool;

   --:dbpool for Interval_Map_T'Storage_Pool use Interval_Map_Pool;


end Interval_Maps;
