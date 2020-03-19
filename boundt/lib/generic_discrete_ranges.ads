-- Generic_Discrete_Ranges (decl)
--
-- Ranges (intervals first .. last) of values of a discrete type.
--
-- Author: Niklas Holsti
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
-- $Revision: 1.2 $
-- $Date: 2015/10/24 20:53:54 $
--
-- $Log: generic_discrete_ranges.ads,v $
-- Revision 1.2  2015/10/24 20:53:54  niklas
-- Moved to free licence.
--
-- Revision 1.1  2014/06/28 20:07:36  niklas
-- First version.
--


generic

   type Base_Type is (<>);

   with function Image (Item : Base_Type)
   return String
   is <>;

package Generic_Discrete_Ranges
is


   type Range_Type is record
      First : Base_Type;
      Last  : Base_Type;
   end record;
   --
   -- A range of Base_Type values, First .. Last, inclusive.


   Empty_Range : constant Range_Type := (
      First => Base_Type'Succ (Base_Type'First),
      Last  => Base_Type'First);
   --
   -- Any Range with First > Last represents an empty range.


   Full_Range : constant Range_Type := (
      First => Base_Type'First,
      Last  => Base_Type'Last );
   --
   -- The range of all possible Base_Type values.


   function In_Range (
      Value  : Base_Type;
      Rainge : Range_Type)
   return Boolean;
   --
   -- Whether the Value lies in the Ra(i)nge.


   function "<=" (Left, Right : Range_Type)
   return Boolean;
   --
   -- Whether the Left range is a subinterval of the Right one.


   function Singleton (Value : Base_Type)
   return Range_Type;
   --
   -- A range containing just the given Value.


   procedure Widen (
      Rainge     : in out Range_Type;
      To_Include : in     Base_Type);
   --
   -- Widens the given Ra(i)nge To Include the given value.
   -- Of course, the new Range may have to include other new
   -- values, too, if the new value is not next to the old
   -- Rainge.


   function Image (Item : Range_Type) return String;
   --
   -- A textual display of the range, for use in outputs
   -- of various kinds, mainly for human reading.


end Generic_Discrete_Ranges;
