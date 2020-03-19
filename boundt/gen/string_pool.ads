-- String_Pool (decl)
--
-- String storage for symbol tables.
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
-- $Revision: 1.4 $
-- $Date: 2015/10/24 19:36:53 $
--
-- $Log: string_pool.ads,v $
-- Revision 1.4  2015/10/24 19:36:53  niklas
-- Moved to free licence.
--
-- Revision 1.3  2009-03-24 07:48:35  niklas
-- BT-CH-0166: String_Pool.Item_T for source-file and marker names.
--
-- Revision 1.2  2007/04/26 11:28:05  niklas
-- BT-CH-0059.
--
-- Revision 1.1  2000/05/02 19:41:29  holsti
-- String_Pool added, Symbols partly implemented.
--


package String_Pool is
--
-- To ease the handling of strings which occur many times in
-- the symbol tables (e.g. source-file names, module names)
-- this package provides a "string pool" that identifies
-- string occurrences and stores them uniquely.


   type Item_T is private;
   --
   -- A (reference to a) string in the string pool.
   --
   -- Must be initialised by To_Item (or via assignment from
   -- an initialised Item_T) before use.
   --
   -- Any function that takes an Item_T parameter will raise
   -- Constraint_Error if the parameter is not initialised.
   --
   -- An important aspect of the pool is that for two items
   -- A and B (of type Item_T), A = B iff and only if the
   -- string referred to by A is equal to the string referred
   -- to by B, or in other words:
   --
   --     A = B  if and only if To_String(A) = To_String(B).
   --
   -- The comparison A = B should, however, be faster than
   -- string comparison.


   Null_Item : constant Item_T;
   --
   -- A special value of Item_T that does not refer to any string.
   -- If this value is used as a parameter, Constraint_Error
   -- is raised.


   function To_Item (S : String) return Item_T;
   --
   -- Puts the string S in the pool (if not already there)
   -- and return an item that refers to this string.


   function To_String (Item : Item_T) return String;
   --
   -- Returns the string referred to by Item.


   function Length (Item : Item_T) return Natural;
   --
   -- The length of the string referred to by Item.


   function "<" (Left, Right : Item_T) return Boolean;
   --
   -- Same as To_String(Left) < To_String(Right).


   function "=" (Left : String; Right : Item_T) return Boolean;
   function "=" (Left : Item_T; Right : String) return Boolean;
   --
   -- Direct comparison of strings for equality without using
   -- To_String or To_Item.


private

   type Item_T is access String;

   Null_Item : constant Item_T := null;

end String_Pool;

