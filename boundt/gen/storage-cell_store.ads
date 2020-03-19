-- Storage.Cell_Store (decl)
--
-- Cell storage for the arithmetic modelling.
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
-- $Date: 2015/10/24 19:36:52 $
--
-- $Log: storage-cell_store.ads,v $
-- Revision 1.5  2015/10/24 19:36:52  niklas
-- Moved to free licence.
--
-- Revision 1.4  2013-02-12 08:47:20  niklas
-- BT-CH-0245: Global volatile cells and "volatile" assertions.
--
-- Revision 1.3  2011-09-09 14:51:51  niklas
-- Added function Initial_Cell_Variables, for use in ALF export.
--
-- Revision 1.2  2008-06-18 20:52:57  niklas
-- BT-CH-0130: Data pointers relative to initial-value cells.
--
-- Revision 1.1  2004/04/25 09:33:07  niklas
-- First Tidorum version.
--


private package Storage.Cell_Store is
--
-- The set of cells, identified by their external names
-- and directly accessed by Cell_T handles.
--


   function Cell_By_Spec (
      Spec              : Processor.Cell_Spec_T;
      Initial_Value_For : Cell_T)
   return Cell_T;
   --
   -- Creates a cell with the given specification, or if such
   -- a cell already exists, returns the existing cell.
   --
   -- This cell can be defined as the Initial_Value_For an
   -- existing variable cell, in which case it is an error if
   -- a cell with this Spec already exists or if the variable
   -- cell already has an initial-value cell.


   function Cell_By_Name (Name : String) return Cell_T;
   --
   -- Returns the cell with the given name, if it exists,
   -- or else raises Storage.No_Such_Cell.


   function Number_Of_Cells return Natural;
   --
   -- The total number of cells in the store.


   function Number_Of_Volatile_Cells return Natural;
   --
   -- The total number of volatile cells in the store.


   function Cell_At (Index : Cell_Index_T) return Cell_T;
   --
   -- The cell with the given index.


   function Initial_Cell_Variables
   return Cell_List_T;


end Storage.Cell_Store;
