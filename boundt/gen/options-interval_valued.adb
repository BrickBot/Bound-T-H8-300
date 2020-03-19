-- Options.Interval_Valued (body)
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
-- $Date: 2015/10/24 20:05:50 $
--
-- $Log: options-interval_valued.adb,v $
-- Revision 1.3  2015/10/24 20:05:50  niklas
-- Moved to free licence.
--
-- Revision 1.2  2013-02-03 12:37:56  niklas
-- Using Storage.Bounds.Get.
--
-- Revision 1.1  2011-10-18 20:17:06  niklas
-- First version, for Toy ALF export.
--


with Storage.Bounds.Get;


package body Options.Interval_Valued is


   overriding
   function Type_And_Default (Option : access Option_T)
   return String
   is
   begin

      return "Interval, default "
         & Storage.Bounds.Image (Option.Default);

   end Type_And_Default;


   overriding
   procedure Reset (Option : access Option_T)
   is
   begin

      Option.Value := Storage.Bounds.Universal_Interval;

   end Reset;


   overriding
   procedure Set (
      Option : access Option_T;
      Value  : in     String)
   is
   begin

      Option.Value := Storage.Bounds.Get.Interval_Value (From => Value);

   end Set;


   function Set (Value : Storage.Bounds.Interval_T) return Option_T
   is
   begin

      return (Options.Option_T with
         Default => Value,
         Value   => Value);

   end Set;


   function Value_Of (Option : Option_T) return Storage.Bounds.Interval_T
   is
   begin

      return Option.Value;

   end Value_Of;


end Options.Interval_Valued;