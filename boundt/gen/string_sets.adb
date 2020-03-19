-- String_Sets (body)
--
-- Author: Niklas Holsti, Tidorum Ltd
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
-- $Date: 2015/10/24 20:05:52 $
--
-- $Log: string_sets.adb,v $
-- Revision 1.3  2015/10/24 20:05:52  niklas
-- Moved to free licence.
--
-- Revision 1.2  2007-02-27 20:54:58  Niklas
-- Corrected renaming-as-body for To_List to real body, because the
-- renamed function To_Vector is inherited through type derivation
-- and therefore has "Intrinsic" convention and so cannot be used in
-- renaming-as-body.
--
-- Revision 1.1  2006/05/27 21:26:38  niklas
-- First version for BT-CH-0020.
--


package body String_Sets is


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


end String_Sets;
