-- Bounded_Set_Pool (body)
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
-- $Date: 2015/10/24 20:05:46 $
--
-- $Log: bounded_set_pool.adb,v $
-- Revision 1.3  2015/10/24 20:05:46  niklas
-- Moved to free licence.
--
-- Revision 1.2  2004-04-25 09:12:23  niklas
-- First Tidorum version. Support empty pool.
--
-- Revision 1.1  2001/01/07 21:54:14  holsti
-- First version.
--


package body Bounded_Set_Pool is


   type Pool_Item_Type is record
      Element : Element_Type;
      Next    : Link_Type;
   end record;
   --
   -- Each pool-item contains a set element, and a pointer
   -- to the next item in the set (or in the list of deleted
   -- and recycable items).


   Pool : array (Index_Type) of Pool_Item_Type;
   --
   -- The pool for queue elements.


   Population : Natural := 0;
   --
   -- Total number of elements in the pool.


   Last_Used : Link_Type := None;
   --
   -- The last pool item that has never been used (or None).
   -- The items Pool (Last_Used + 1 .. Pool'Last) are available
   -- for use, as are the elements in the Recycled list, below.


   Recycled : Link_Type := None;
   --
   -- The head of a list that holds pool items that have been used
   -- in a set, but were released and are now available for reuse.
   -- Space for new elements is taken from Recycled if available,
   -- otherwise from Last_Used + 1.


   function Empty (Item : Set_Type) return Boolean
   is
   begin
      return Item.Head = None;
   end Empty;


   procedure Add (
      Element : in     Element_Type;
      To      : in out Set_Type)
   is

      Index : Link_Type;
      -- Location for the new element.

   begin

      -- See if the element is already a member:

      Index := To.Head;
      loop
         exit when Index = None or else Pool(Index).Element = Element;
         Index := Pool(Index).Next;
      end loop;

      if Index = None then

         -- The element is not a member. Add it.

         if Population >= Max_Elements then
            raise Overflow;
         end if;

         -- Find a slot for the new element:

         if Recycled /= None then
            -- Pool slots are available for recycling.

            Index := Recycled;
            Recycled := Pool(Recycled).Next;

         else
            -- No recycled slots are available.
            -- Take a fresh slot:

            if Last_Used = None then
               Index := Pool'First;
            else
               Index := Last_Used + 1;
            end if;

            Last_Used := Index;

         end if;

         -- Store the new element (as the new list-head):

         Pool(Index) := (
            Element => Element,
            Next    => To.Head);

         Population := Population + 1;

         -- Update the set:

         To := (Head => Index);

      end if;

   end Add;


   procedure Take (
      From    : in out Set_Type;
      Element :    out Element_Type)
   is

      Index : Index_Type;
      -- Location of the removed element.

   begin

      if Empty (From) then
         raise Underflow;
      end if;

      -- Remember the slot that holds the element to be returned:

      Index := From.Head;

      -- Update the set:

      From := (Head => Pool(Index).Next);

      -- Extract the element from the pool:

      Element := Pool(Index).Element;

      Population := Population - 1;

      Pool(Index).Next := Recycled;
      Recycled := Index;

   end Take;


end Bounded_Set_Pool;
