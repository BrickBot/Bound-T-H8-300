-- Options.May_Integer (decl)
--
-- Options witn integer values that may be undefined (unset)
-- or defined (set),
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
-- $Log: options-may_integer.ads,v $
-- Revision 1.2  2015/10/24 19:36:51  niklas
-- Moved to free licence.
--
-- Revision 1.1  2012-02-12 14:12:22  niklas
-- First version, for BT-CH-0229.
--


with Options;


generic
   type Value_Type is range <>;
   Base : in Options.Number_Base_T := 10;
package Options.May_Integer is


   package Number_Valued is new Integer_Valued (Value_Type, Base);
   --
   -- Options with (defined) values of Value_Type.


   type Option_T
   is new Number_Valued.Option_T (Default => Value_Type'First)
   with record
      Set : Boolean := False;
   end record;
   --
   -- An option that initially has no value, but can be Set
   -- to some value of Value_Type. (There is a Default component,
   -- but it is not used.)
   --
   -- Options of this kind are not enumerable.


   overriding
   function Type_And_Default (Option : access Option_T)
   return String;


   overriding
   procedure Reset (Option : access Option_T);
   --
   -- Option.Set := False.


   overriding
   procedure Set (
      Option : access Option_T;
      Value  : in     String);
   --
   -- Option.Set := True; Option.Value := Value.


   function Set (Value : Value_Type) return Option_T;
   --
   -- Constructs an option that is Set to the given Value,
   -- which is also the Default value.


end Options.May_Integer;
