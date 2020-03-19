-- Programs.Sort (decl)
--
-- Sorting program elements.
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
-- $Log: programs-sort.ads,v $
-- Revision 1.2  2015/10/24 19:36:52  niklas
-- Moved to free licence.
--
-- Revision 1.1  2002-11-29 11:01:22  holsti
-- First version, to fix NC_0146.
--


with Topo_Sort;


package Programs.Sort is


   function Top_Down is new Topo_Sort (
      Element      => Subprogram_T,
      Pair         => Call_T,
      Element_List => Subprogram_List_T,
      Pair_List    => Call_List_T,
      Lesser       => Caller,
      Greater      => Callee);
   --
   -- Sorts a list of subprograms into a top-down calling order
   -- according to a given list of calls, so that the calling subprogram
   -- comes before the called subprogram.


   function Bottom_Up is new Topo_Sort (
      Element      => Subprogram_T,
      Pair         => Call_T,
      Element_List => Subprogram_List_T,
      Pair_List    => Call_List_T,
      Lesser       => Callee,
      Greater      => Caller);
   --
   -- Sorts a list of subprograms into bottom-up calling order
   -- according to a given list of calls, so that the called subprogram
   -- comes before the calling subprogram.


end Programs.Sort;
