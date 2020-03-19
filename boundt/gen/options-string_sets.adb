-- Options.String_Sets (body)
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
-- $Log: options-string_sets.adb,v $
-- Revision 1.3  2015/10/24 20:05:50  niklas
-- Moved to free licence.
--
-- Revision 1.2  2011-09-01 18:05:45  niklas
-- Changed warning for repeated value.
--
-- Revision 1.1  2011-08-31 04:17:13  niklas
-- Added for BT-CH-0222: Option registry. Option -dump. External help files.
--


with Output;


package body Options.String_Sets is


   procedure Add (
      Item : in     String;
      To   : in out String_Set_T)
   is
   begin

      Add (
         Item => String_Pool.To_Item (Item),
         To   => To);

   end Add;


   procedure Add (
      Item : in     String_Pool.Item_T;
      To   : in out String_Set_T)
   is

      Index : Natural;

   begin

      Find_Or_Add (
         Value  => Item,
         Vector => To,
         Index  => Index);

   end Add;


   function Is_Member (Item : String; Of_Set : String_Set_T)
   return Boolean
   is
   begin

      return Is_Element (Of_Set, String_Pool.To_Item (Item));

   end Is_Member;


   function To_List (Set : String_Set_T) return String_List_T
   is
   begin

      return To_Vector (Set);

   end To_List;


   overriding
   function Type_And_Default (Option : access Option_T)
   return String
   is
   begin

      return "Set of strings, empty by default";

   end Type_And_Default;


   overriding
   procedure Reset (Option : access Option_T)
   is
   begin

      Erase (Option.Value);

   end Reset;


   overriding
   procedure Set (
      Option : access Option_T;
      Value  : in     String)
   is
   begin

      if Is_Member (Value, Option.Value) then

         Output.Warning (
              "Ignoring repeated """
            & Name_Of (Option)
            & """ value"
            & Output.Field_Separator
            & Value);

      else

         Add (
            Item => Value,
            To   => Option.Value);

      end if;

   end Set;


   function To_List (Option : Option_T) return String_List_T
   is
   begin

      return To_List (Option.Value);

   end To_List;


end Options.String_Sets;
