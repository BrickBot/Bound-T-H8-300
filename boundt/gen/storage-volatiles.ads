-- Storage.Volatiles (decl)
--
-- Information about volatile storage areas.
--
-- This package maintains a table of volatile storage ranges. The table
-- can be defined by target-specific code (for standard volatile areas
-- such as I/O mapped peripheral registers) or by "volatile range"
-- assertions. Whenever a new cell is created, the table is inspected;
-- if the new cell is found to lie in or touch a volatile range, the
-- new cell is marked volatile. When a new volatile range is defined,
-- all existing cells are checked against the new range, and cells that
-- lie in or touch the new range are marked volatile.
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
-- $Log: storage-volatiles.ads,v $
-- Revision 1.2  2015/10/24 19:36:52  niklas
-- Moved to free licence.
--
-- Revision 1.1  2013-02-12 08:47:20  niklas
-- BT-CH-0245: Global volatile cells and "volatile" assertions.
--


with Processor;


package Storage.Volatiles is


   procedure Mark_Range (
      From, To    : in     Processor.Cell_Spec_T;
      Valid_Range :    out Boolean);
   --
   -- Defines the range From .. To as volatile storage, in the sense
   -- that any cell, old or new, that lies in or touches that range
   -- should be considered volatile.
   --
   -- If the two cell-specs, From and To, are "compatible" (more or less,
   -- lie in the same address space) and can form a storage range,
   -- Valid_Range is returned as True, the range is defined as volatile,
   -- and any existing cell that lies in or touches the range is marked
   -- as volatile too. Otherwise, Valid_Range is returned as False and
   -- nothing else is done.


   function In_Volatile_Range (Cell : Processor.Cell_Spec_T)
   return Boolean;
   --
   -- Compares the given Cell against all known volatile storage ranges
   -- and returns True iff the Cell lies in or touches any such range.


end Storage.Volatiles;
