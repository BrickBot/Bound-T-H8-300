-- String_Pool (body)
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
-- $Revision: 1.5 $
-- $Date: 2015/10/24 20:05:52 $
--
-- $Log: string_pool.adb,v $
-- Revision 1.5  2015/10/24 20:05:52  niklas
-- Moved to free licence.
--
-- Revision 1.4  2011-08-31 04:23:34  niklas
-- BT-CH-0222: Option registry. Option -dump. External help files.
--
-- Revision 1.3  2009/03/24 07:48:35  niklas
-- BT-CH-0166: String_Pool.Item_T for source-file and marker names.
--
-- Revision 1.2  2007/04/26 11:28:05  niklas
-- BT-CH-0059.
--
-- Revision 1.1  2000/05/02 19:41:28  holsti
-- String_Pool added, Symbols partly implemented.
--


with Hash_G;             -- LGL components
with Hash_G.Changes_G;   -- LGL components

with Interfaces;

package body String_Pool is


   function Char_XOR (S : String) return Natural
   is
      use Interfaces;

      Hash : Unsigned_32 := S'Length;

   begin
      for I in S'range loop
         Hash :=     Rotate_Left (Hash, 8)
                 xor Unsigned_32 (Character'pos(S(I)));
      end loop;

      return Natural (Hash mod Unsigned_32(Natural'Last));

   end Char_XOR;


   package String_Hash is new Hash_G (
      Key_Type  => String,
      Data_Type => Item_T,
      Hash      => Char_XOR);

   package String_Hash_Changes is new String_Hash.Changes_G;


   Initial_Size_C : constant := 1023;
   --
   -- Initial size of the hash table.


   Size_Increment_C : constant := 511;
   --
   -- Amount by which hash table is grown when it gets full.


   Pool : String_Hash.Hash_Table_Type :=
      String_Hash.Create (Minimum_Size => Initial_Size_C);
   --
   -- The string pool.
   -- It will be resized (grown) if necessary.


   procedure Add_Item (Item : Item_T)
   --
   -- Add the item to the string pool.
   --
   is
      use String_Hash, String_Hash_Changes;

      Old_Size, New_Size : Positive;

   begin

      if Is_Full (Pool) then

         Old_Size := Size (Pool);
         New_Size := Old_Size + Size_Increment_C;

         Resize (
           Hash_Table   => Pool,
           Minimum_Size => New_Size);

      end if;

      Insert (
         Hash_Table => Pool,
         Key        => Item.all,
         Data       => Item);

   end Add_Item;


   function To_Item (S : String) return Item_T
   is
      Item : Item_T;
      -- The value to be returned.

      use String_Hash;

   begin

      Item := Retrieve (
         Hash_Table => Pool,
         Key        => S);

      -- The string is already in the pool.
      return Item;

   exception

      when Key_Not_Found_Error =>

         -- New string, add it to pool.
         Item := new String'(S);
         Add_Item (Item);

         return Item;

   end To_Item;


   function To_String (Item : Item_T) return String
   is
   begin
      return Item.all;
   end To_String;


   function Length (Item : Item_T) return Natural
   is
   begin

      return Item.all'Length;

   end Length;


   function "<" (Left, Right : Item_T) return Boolean
   is
   begin
      return Left.all < Right.all;
   end "<";


   function "=" (Left : String; Right : Item_T) return Boolean
   is
   begin

      return Left = Right.all;

   end "=";


   function "=" (Left : Item_T; Right : String) return Boolean
   is
   begin

      return Left.all = Right;

   end "=";


end String_Pool;


