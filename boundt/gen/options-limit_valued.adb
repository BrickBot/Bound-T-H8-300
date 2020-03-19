-- Options.Limit_Valued (body)
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
-- $Log: options-limit_valued.adb,v $
-- Revision 1.3  2015/10/24 20:05:50  niklas
-- Moved to free licence.
--
-- Revision 1.2  2012-01-20 17:59:13  niklas
-- Default value is "none" both for input and output.
--
-- Revision 1.1  2012-01-19 21:32:07  niklas
-- First version.
--


with Arithmetic;


package body Options.Limit_Valued is


   overriding
   function Type_And_Default (Option : access Option_T)
   return String
   is
   begin

      return "Limit, default "
         & Storage.Bounds.Image (
            Item    => Option.Default,
            Unknown => "none");

   end Type_And_Default;


   overriding
   procedure Reset (Option : access Option_T)
   is
   begin

      Option.Value := Storage.Bounds.Not_Limited;

   end Reset;


   overriding
   procedure Set (
      Option : access Option_T;
      Value  : in     String)
   is

      New_Value : Storage.Bounds.Limit_T;
      -- The new value, possibly not valid.

   begin

      if Value = "none" then

         New_Value := Storage.Bounds.Not_Limited;

      else

         New_Value := Storage.Bounds.Limit (
            Value => Arithmetic.Value_T'Value (Value));
         --
         -- May raise Constraint_Error.

      end if;

      Option.Value := New_Value;

   end Set;


   function Set (Value : Storage.Bounds.Limit_T) return Option_T
   is
   begin

      return (Options.Option_T with Default => Value, Value => Value);

   end Set;


   function Value_Of (Option : Option_T) return Storage.Bounds.Limit_T
   is
   begin

      return Option.Value;

   end Value_Of;


end Options.Limit_Valued;
