-- Options.May_Modular_Integer (body)
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
-- $Date: 2015/10/24 20:05:50 $
--
-- $Log: options-may_modular_integer.adb,v $
-- Revision 1.2  2015/10/24 20:05:50  niklas
-- Moved to free licence.
--
-- Revision 1.1  2014/06/01 10:36:37  niklas
-- First version.
--


package body Options.May_Modular_Integer is


   overriding
   function Type_And_Default (Option : access Option_T)
   return String
   is
   begin

      return Type_Alone (Option)
           & ", not defined by default";

   end Type_And_Default;


   overriding
   procedure Reset (Option : access Option_T)
   is
   begin

      Option.Set := False;

   end Reset;


   overriding
   procedure Set (
      Option : access Option_T;
      Value  : in     String)
   is
   begin

      Number_Valued.Set (
         Option => Number_Valued.Option_T (Option.all)'Access,
         Value  => Value);

      Option.Set := True;

   exception
   when Constraint_Error =>

      Option.Set := False;

      raise;

   end Set;


   function Set (Value : Value_Type) return Option_T
   is
   begin

      return (
         Number_Valued.Option_T'(
            Default => Value,
            Value   => Value)
         with
            Set => True);

   end Set;


end Options.May_Modular_Integer;
