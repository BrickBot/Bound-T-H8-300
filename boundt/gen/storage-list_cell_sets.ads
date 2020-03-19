-- Storage.List_Cell_Sets (decl)
--
-- An implementation of Cell_Set_T using (unbounded) lists of cells.
--
-- The basic set operations (membership test, add member, remove member)
-- take time linear in the size of the set, but so does the enumeration
-- operation (To_List), and the emptiness check is fast (constant-time).
-- This implementation of cell sets is suitable for small sets.
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
-- $Date: 2015/10/24 19:36:52 $
--
-- $Log: storage-list_cell_sets.ads,v $
-- Revision 1.2  2015/10/24 19:36:52  niklas
-- Moved to free licence.
--
-- Revision 1.1  2009-10-07 19:26:10  niklas
-- BT-CH-0183: Cell-sets are a tagged-type class.
--


with Unbounded_Controlled_Vectors;


package Storage.List_Cell_Sets is


   type Set_T is new Root_Cell_Set_T with private;
   --
   -- A (small) set of cells with an order(N) access time, but able
   -- to hold cells created at any time, before or after the set is
   -- created.


   -- overriding
   function Empty return Set_T;


   -- overriding
   procedure Erase (Set : in out Set_T);


   -- overriding
   function Is_Empty (Item : Set_T) return Boolean;


   -- overriding
   function Is_Member (
      Cell   : Cell_T;
      Of_Set : Set_T)
   return Boolean;


   -- overriding
   function Card (Set : Set_T) return Natural;


   -- overriding
   function To_Set (List : Cell_List_T) return Set_T;


   -- overriding
   function To_List (Set : Set_T) return Cell_List_T;


   -- overriding
   procedure Add (
      Cell : in     Cell_T;
      To   : in out Set_T);


   -- overriding
   procedure Add (
      Cell  : in     Cell_T;
      To    : in out Set_T;
      Added : in out Boolean);


   -- overriding
   procedure Remove (
      Cell : in     Cell_T;
      From : in out Set_T);


   -- overriding
   procedure Move_Cells (
      From : in out Set_T;
      To   : in out Set_T);


   -- overriding
   function Copy (Item : Cell_Set_T) return Set_T;


   -- overriding
   function "=" (Left, Right : Set_T) return Boolean;


   procedure Print_Debug_Pool;
   --
   -- Prints the information collected by GNAT.Debug_Pool, if enabled.


private


   package Cell_Vectors
   is new Unbounded_Controlled_Vectors (
      Element_Type   => Cell_T,
      Vector_Type    => Cell_List_T,
      Initial_Size   => 40,
      Size_Increment => 40,
      Deallocate     => Deallocate);
   --
   -- The initial size is made large enough to accommodate the typical
   -- size of the Basis Sets in the arithmetic analysis.


   subtype Unbounded_Cell_List_T is Cell_Vectors.Unbounded_Vector;
   --
   -- A dynamically growing list of cells.


   Empty_List : Unbounded_Cell_List_T;
   --
   -- Default initialized to the empty list.


   type Set_T is new Root_Cell_Set_T with record
      List : Unbounded_Cell_List_T;
   end record;
   --
   -- The set consists of the cells in the List, which are listed
   -- in no particular order. An assignment of Set_T variables
   -- copies the List (no storage sharing).


end Storage.List_Cell_Sets;
