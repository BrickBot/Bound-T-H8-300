-- Options.Interval_Valued (decl)
--
-- Options with values of type Storage.Bounds.Interval_T.
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
-- $Date: 2015/10/24 19:36:51 $
--
-- $Log: options-interval_valued.ads,v $
-- Revision 1.2  2015/10/24 19:36:51  niklas
-- Moved to free licence.
--
-- Revision 1.1  2011-10-18 20:17:07  niklas
-- First version, for Toy ALF export.
--


with Storage.Bounds;


package Options.Interval_Valued is


   type Option_T is new Options.Option_T with record
      Default : Storage.Bounds.Interval_T := Storage.Bounds.Universal_Interval;
      Value   : Storage.Bounds.Interval_T := Storage.Bounds.Universal_Interval;
   end record;
   --
   -- An option that has a default value and a current value,
   -- both of which are intervals of integer values.
   -- If the Value is Universal_Interval the option can be
   -- considered unset (since Universal_Interval puts no
   -- constraint on a value).
   --
   -- Options of this kind are not enumerable.


   overriding
   function Type_And_Default (Option : access Option_T)
   return String;


   overriding
   procedure Reset (Option : access Option_T);
   --
   -- Option.Value := Universal_Interval.


   overriding
   procedure Set (
      Option : access Option_T;
      Value  : in     String);
   --
   -- Option.Value is set to the interval presented in the Value,
   -- which has the approximate syntax <min, optional> .. <max, optional>.
   -- Thus ".." represents the universal interval.


   function Set (Value : Storage.Bounds.Interval_T) return Option_T;
   --
   -- Constructs an option that is Set to the given Value,
   -- which is also the Default value.


   function Value_Of (Option : Option_T) return Storage.Bounds.Interval_T;
   --
   -- The Value of the Option.


end Options.Interval_Valued;
