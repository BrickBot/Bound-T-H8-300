-- Bounded_Queues (decl)
--
-- Bounded queue structures, generic in element-type.
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
-- $Date: 2015/10/24 19:36:47 $
--
-- $Log: bounded_queues.ads,v $
-- Revision 1.4  2015/10/24 19:36:47  niklas
-- Moved to free licence.
--
-- Revision 1.3  2007-10-26 12:44:34  niklas
-- BT-CH-0091: Reanalyse a boundable edge only if its domain grows.
--
-- Revision 1.2  2004/04/25 07:19:47  niklas
-- First Tidorum version. Corrected descriptions.
--
-- Revision 1.1  2001/01/07 21:54:14  holsti
-- First version.
--



generic

   type Element_Type is private;
   --
   -- The type of the elements held in the queues.

package Bounded_Queues is


   type Queue_Type (Max_Length : Natural) is private;
   --
   -- A queue of at most Max_Length elements of type Element_Type.
   -- The default initialisation sets the queue empty.


   Overflow : exception;
   --
   -- Raised by an attempt to put an element into a full queue.


   Underflow : exception;
   --
   -- Raised by an attempt to get an element from an empty queue.


   function Length (Item : Queue_Type) return Natural;
   --
   -- The number of elements currently in the queue.
   -- Zero for an empty queue, Item.Max_Length for a full queue.


   procedure Put (
      Element : in     Element_Type;
      Into    : in out Queue_Type);
   --
   -- Puts the element into the queue as the new last element.
   -- Raises Overflow if the queue was full.


   procedure Get (
      From    : in out Queue_Type;
      Element :    out Element_Type);
   --
   -- Gets the first element from the queue, and removes it
   -- from the queue.
   -- Raises Underflow if the queue was empty.


private


   subtype List_Index_Type is Positive;
   --
   -- For indexing queue elements in a list.


   type Element_List_Type is
      array (List_Index_Type range <>) of Element_Type;
   --
   -- For holding the elements in the queue.


   type Queue_Type (Max_Length : Natural) is record
      Length : Natural         := 0;
      First  : List_Index_Type := 1;
      Room   : Element_List_Type (1 .. Max_Length);
   end record;
   --
   -- The elements in the queue are Room (First .. First + Length - 1)
   -- with wrap-around from Room'Last to Room'First.


end Bounded_Queues;
