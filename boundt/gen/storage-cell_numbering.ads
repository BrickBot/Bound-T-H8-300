-- Storage.Cell_Numbering (decl)
--
-- Numbering cells in a cell-set consecutively, for purposes of
-- using this subset of cells as indices in arrays.
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
-- $Revision: 1.6 $
-- $Date: 2015/10/24 19:36:52 $
--
-- $Log: storage-cell_numbering.ads,v $
-- Revision 1.6  2015/10/24 19:36:52  niklas
-- Moved to free licence.
--
-- Revision 1.5  2009-10-07 19:26:10  niklas
-- BT-CH-0183: Cell-sets are a tagged-type class.
--
-- Revision 1.4  2007/04/18 18:34:39  niklas
-- BT-CH-0057.
--
-- Revision 1.3  2007/03/29 15:18:04  niklas
-- BT-CH-0056.
--
-- Revision 1.2  2005/02/16 21:11:49  niklas
-- BT-CH-0002.
--
-- Revision 1.1  2004/04/25 09:33:23  niklas
-- First version.
--

--:dbpool with GNAT.Debug_Pools;


package Storage.Cell_Numbering is


   type Number_T is new Natural;
   --
   -- A number assigned to a cell, except that zero is not assigned
   -- to any cell. Numbering starts at one.


   type Map_T is private;
   --
   -- Assigns consecutive numbers of type Number_T, starting from 1,
   -- to a given subset of cells, and assigns no number to the other
   -- cells.
   --
   -- The order of numbering may be arbitrary with respect to the
   -- indices (Cell_Index_T) of the cells, or it may be somehow
   -- related to the indices, for example numbering the cells in
   -- order of increasing index. This depends on the method used to
   -- traverse the set of cells when the numbering is created, which
   -- may in turn depend on the implementation of the cell-set.
   --
   -- Objects of this type may contain dynamically assigned
   -- memory and should be Discarded (see below) when no longer
   -- needed.


   Not_Numbered : exception;
   --
   -- Raised if there is a request for the number of a cell
   -- that is not in the subset of numbered cells under the
   -- given map.


   function Map (Cells : Cell_Set_T) return Map_T;
   --
   -- The map that assigns consecutive numbers to all the Cells
   -- in the given set, and assigns no number to the other cells
   -- (including cells that are created afterwards).


   function Numbered (Cell : Cell_T; Under : Map_T) return Boolean;
   --
   -- Whether the Cell is numbered Under a given map.


   function Number (Cell : Cell_T; Under : Map_T) return Number_T;
   --
   -- The number of the given Cell, Under the given map.
   -- Raises Not_Numbered if the cell is not numbered.


   function First (Under : Map_T) return Number_T;
   function Last  (Under : Map_T) return Number_T;
   --
   -- The range of numbers assigned to cells Unde the given
   -- map is First(Under) .. Last (Under).


   procedure Discard (Map : in out Map_T);
   --
   -- Discards the map, deallocating any dynamically allocated
   -- memory it uses.


   type List_T is array (Number_T range <>) of Cell_T;
   --
   -- An inverse map from numbers to cells.


   function Inverse (Map : Map_T) return List_T;
   --
   -- The inverse map, listing the cells in numbering order.
   -- If the result is L, then L'Range = First(Map) .. Last(Map) and
   -- for each N in L'Range, Number(Cell => L(N), Under => Map) = N.


   procedure Show (Map : in Map_T);
   --
   -- Displays the cell numbering Map on standard output.


private


   No_Number : constant Number_T := 0;
   --
   -- The "number" given a cell that is has no number (is not
   -- in the subset of cells that are numbered).


   type Numbers_T is array (Cell_Index_T range <>) of Number_T;
   --
   -- The number for each cell, or zero if no number assigned.


   type Numbers_Ref is access Numbers_T;

   --:dbpool Numbers_Pool : GNAT.Debug_Pools.Debug_Pool;

   --:dbpool for Numbers_Ref'Storage_Pool use Numbers_Pool;


   type Map_T is record
      Last    : Number_T;
      Numbers : Numbers_Ref;
   end record;
   --
   -- A numbering of cells, valid for cells with indices from
   -- Numbers'range. For other cells, no number is assigned.
   --
   -- The number for a cell with index i is Numbers(i), unless
   -- this is No_Number which means that the cell is not
   -- numbered.
   --
   -- The numbers in use are 1 .. Last.
   --
   -- The numbering may be arbitrary with respect to the cell
   -- index, or may have some relationship to the cell index,
   -- for example numbering cells in ascending index order.


end Storage.Cell_Numbering;
