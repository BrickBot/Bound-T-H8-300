-- Bounded_Set_Pool (decl)
--
-- Finite-set structures, generic in the element type, and using
-- a common storage pool, with generic size.
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
-- $Date: 2015/10/24 19:36:47 $
--
-- $Log: bounded_set_pool.ads,v $
-- Revision 1.2  2015/10/24 19:36:47  niklas
-- Moved to free licence.
--
-- Revision 1.1  2001-01-07 21:54:16  holsti
-- First version.
--


generic

   type Element_Type is private;
   --
   -- The type of elements in the sets.

   Max_Elements : Natural;
   --
   -- The maximum total number of elements in the sets.

package Bounded_Set_Pool is


   type Set_Type is limited private;
   --
   -- A set of elements of type Value_Type, empty by default.
   -- All sets created from one instance of Bounded_Set_Pool use
   -- a shared pool holding a total of at most Max_Elements elements.
   -- Any single set can contain up to Max_Elements elements, but
   -- this maximum value can be realized only if all other sets
   -- in the same pool (same instance of Bounded_Set_Pool) are
   -- empty.
   --
   -- Since sets are handled as linear lists, this package is intended
   -- chiefly for use with small sets.


   Overflow : exception;
   --
   -- Raised by an attempt to add an element into a set when
   -- the pool is full (Max_Elements already present, in total,
   -- from all sets in the pool).


   Underflow : exception;
   --
   -- Raised by an attempt to take an element from an empty set.


   function Empty (Item : Set_Type) return Boolean;
   --
   -- Whether the set is currently empty.


   procedure Add (
      Element : in     Element_Type;
      To      : in out Set_Type);
   --
   -- Adds the element into the set. No effect if the element is
   -- already a member of the set.
   -- Raises Overflow if the pool was full, and the element is not
   -- already a member.


   procedure Take (
      From    : in out Set_Type;
      Element :    out Element_Type);
   --
   -- Takes one (arbitrary) element from the set, and removes it
   -- from the set.
   -- Raises Underflow if the set was empty.


private


   type Link_Type is new Natural range 0 .. Max_Elements;
   --
   -- A reference to an element (item) in the pool.
   -- The value zero means "none".

   None : constant Link_Type := 0;


   subtype Index_Type is Link_Type range 1 .. Link_Type'Last;
   --
   -- A reference to an element (item) in the pool, excluding
   -- the case "none".


   type Set_Type is record
      Head : Link_Type := None;
   end record;
   --
   -- The elements in the set are stored as a list of pool
   -- locations. The Set_Type points to the head of the list,
   -- and each element points to the next element, except the
   -- last element where the pointer is None.


end Bounded_Set_Pool;
