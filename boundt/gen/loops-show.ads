-- Loops.Show (decl)
--
-- Textual output of control-flow loop-structures.
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
-- $Date: 2015/10/24 19:36:50 $
--
-- $Log: loops-show.ads,v $
-- Revision 1.2  2015/10/24 19:36:50  niklas
-- Moved to free licence.
--
-- Revision 1.1  2001-03-21 20:31:24  holsti
-- First version.
--


with Output;
with Symbols;


package Loops.Show is


   function Locus (
      Luup   : Loop_T;
      Within : Flow.Graph_T;
      Source : Symbols.Symbol_Table_T)
   return Output.Locus_T;
   --
   -- Describes the location of the given loop in the target
   -- program being analysed.
   -- The symbol-table is needed to map code addresses to
   -- source locations.


   procedure Show_Loops (Luups : in Loops_T);
   --
   -- Displays the given loop-structure, including head and
   -- member nodes, and the loop-nesting hierarchy.
   -- Results on standard output.

   procedure Show_Loop (Luup : in Loop_T);
   --
   -- Displays the structure of one loop.


end Loops.Show;

