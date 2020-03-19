-- Options.String_Sets (decl)
--
-- Sets of strings as options.
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
-- $Log: options-string_sets.ads,v $
-- Revision 1.2  2015/10/24 19:36:51  niklas
-- Moved to free licence.
--
-- Revision 1.1  2011-08-31 04:17:13  niklas
-- Added for BT-CH-0222: Option registry. Option -dump. External help files.
--


with String_Pool;
with String_Pool.Opt;
with Unbounded_Vectors;


package Options.String_Sets is


   type String_Set_T is private;
   --
   -- A set of strings.
   -- The default initial value of any String_Set_T is the empty set.
   -- Options of this kind are not enumerable.


   type String_List_T is array (Positive range <>) of String_Pool.Item_T;
   --
   -- A list of strings (as String_Pool items).


   procedure Add (
      Item : in     String;
      To   : in out String_Set_T);
   --
   -- Adds the Item To the set, unless it already is in the set.


   procedure Add (
      Item : in     String_Pool.Item_T;
      To   : in out String_Set_T);
   --
   -- Same as Add (String, ..) but for an Item_T.


   function Is_Member (Item : String; Of_Set : String_Set_T)
   return Boolean;
   --
   -- Whether the given Item string is a member Of the given Set.


   function To_List (Set : String_Set_T) return String_List_T;
   --
   -- All the strings in the Set, as a list in some order.
   -- Most probably not an alphabetic order.


   type Option_T is new Options.Option_T with record
      Value : String_Set_T;
   end record;
   --
   -- An option that has a string-set as its value.
   -- The default value is the null set.


   overriding
   function Type_And_Default (Option : access Option_T)
   return String;


   overriding
   procedure Reset (Option : access Option_T);
   --
   -- Clears the Option by settings its value to the null set.


   overriding
   procedure Set (
      Option : access Option_T;
      Value  : in     String);
   --
   -- The Value is added to the Option's value, if not already
   -- present.


   function To_List (Option : Option_T) return String_List_T;
   --
   -- All the strings in the Option, as a list in some order.
   -- Most probably not an alphabetic order.


private


   package String_Vectors
   is new Unbounded_Vectors (
      Element_Type   => String_Pool.Item_T,
      Vector_Type    => String_List_T,
      Initial_Size   => 50,
      Size_Increment => 50,
      Deallocate     => String_Pool.Opt.Deallocate);
   --
   -- The size and increment may not be good for all uses :-)


   type String_Set_T is new String_Vectors.Unbounded_Vector;


end Options.String_Sets;
