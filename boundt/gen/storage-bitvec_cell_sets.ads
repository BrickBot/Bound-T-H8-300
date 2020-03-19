-- Storage.Bitvec_Cell_Sets (decl)
--
-- An implementation of Cell_Set_T suitable for large sets of cells
-- that need constant-time operations of membership check, insert
-- cell, remove cell.
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
-- $Log: storage-bitvec_cell_sets.ads,v $
-- Revision 1.2  2015/10/24 19:36:52  niklas
-- Moved to free licence.
--
-- Revision 1.1  2009-10-07 19:26:10  niklas
-- BT-CH-0183: Cell-sets are a tagged-type class.
--


with Ada.Finalization;
--:dbpool with GNAT.Debug_Pools;


package Storage.Bitvec_Cell_Sets is


   type Set_T is new Root_Cell_Set_T with private;
   --
   -- A (possibly large) set of cells with Order(1) access time for
   -- cell-wise operations, but Order(number of cells in universe)
   -- time for creating a list of all member cells, and unable to
   -- hold cells that are created after the first cell is entered
   -- in the set.


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
      Cells : in     Set_T;
      To    : in out Set_T);


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


   -- overriding
   function Union (Left, Right : Set_T) return Cell_Set_T;


   -- overriding
   function "-" (Left, Right : Set_T) return Cell_Set_T;


   -- overriding
   function Intersection (Left, Right : Set_T) return Cell_Set_T;


   procedure Print_Debug_Pool;
   --
   -- Prints the information collected by GNAT.Debug_Pool, if enabled.


private


   type Bitvec_T is array (Cell_Index_T range <>) of Boolean;
   --
   -- The bit-vector representation of a set of cells. The members
   -- of the set are those cells where Bitset_T(Index(Cell)) is True.
   --
   pragma Pack (Bitvec_T);


   type Bitset_T (Max_Index : Cell_Index_T) is record
      Ref_Count : Natural := 0;
      Bitvec    : Bitvec_T (1 .. Max_Index);
   end record;
   --
   -- A bit-vector representation of a set of cells, with a
   -- reference count for managing copies. A "copy on change"
   -- policy is applied; if several cell-set variables share the same
   -- underlying Bitset_T, and any change is made through one
   -- variable, that variable gets is own copy of the Bitset_T
   -- and the change is applied to the copy.


   type Bitset_Ref is access Bitset_T;

   --:dbpool Bitset_Pool : GNAT.Debug_Pools.Debug_Pool;

   --:dbpool for Bitset_Ref'Storage_Pool use Bitset_Pool;


   type Counted_Bitset_Ref is new Ada.Finalization.Controlled with record
      Ref : Bitset_Ref;
   end record;
   --
   -- A controlled, counted reference to a Bitset_T.
   -- Ref.Ref_Count shows how many such references exist to the
   -- relevant Bitset_T.


   -- overriding
   procedure Adjust (Object : in out Counted_Bitset_Ref);


   -- overriding
   procedure Finalize (Object : in out Counted_Bitset_Ref);


   type Set_T is new Root_Cell_Set_T with record
      Set : Counted_Bitset_Ref;
   end record;
   --
   -- A cell-set that contains those cells C for which Set.Ref(Index(C))
   -- is True. If Set.Ref is null, the set is considered to be empty.


end Storage.Bitvec_Cell_Sets;
