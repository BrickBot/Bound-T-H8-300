-- Topo_Sort (decl)
--
-- Topological sorting.
-- Reference: D.Knuth, Fundamental Algorithms, 1969, page 259.
-- Author: Niklas Holsti, Space Systems Finland, 2000.
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
-- $Date: 2015/10/24 19:36:53 $
--
-- $Log: topo_sort.ads,v $
-- Revision 1.4  2015/10/24 19:36:53  niklas
-- Moved to free licence.
--
-- Revision 1.3  2004-04-25 07:53:26  niklas
-- First Tidorum version. Handling duplicates.
--
-- Revision 1.2  2000/07/12 20:41:24  holsti
-- Added Elements parameter for disconnected or singleton graphs.
--
-- Revision 1.1  2000/05/07 12:40:02  holsti
-- First version
--


generic
   type Element is private;
   type Pair    is private;
   type Element_List is array (Positive range <>) of Element;
   type Pair_List    is array (Positive range <>) of Pair;
   with function Lesser  (P : Pair) return Element;
   with function Greater (P : Pair) return Element;

function Topo_Sort (
   Elements : Element_List;
   Pairs    : Pair_List)
return Element_List;
--
-- Topological sorting of a directed, acyclic graph.
--
-- The lists of Elements and Pairs define a directed graph as follows:
--
-- > The nodes of the graph are the Lesser and Greater elements of
--   all the pairs (a given element will usually occur in many pairs)
--   plus all the listed Elements.
--
-- > The arcs of the graph correspond exactly to the given pairs:
--   the pair P defines an arc from Lesser(P) to Greater(P).
--
-- It is assumed that the graph is acyclic.
--
-- The Elements list need not contain all, or even any elements,
-- if the missing elements are present in the Pairs list as
-- Lesser or Greater elements of some pair.
--
-- The result V of Topo_Sort is a vector of elements which contains
-- all the elements (nodes) in the graph (without duplication) in
-- an order that agrees with all the given pairs. In other words,
-- for any input pair P, the index of Lesser(P) in V is less than
-- the index of Greater(P).
--
-- If (contrary to assumption) the graph contains a cycle, then
-- the elements in the cycle, and any direct or indirect successor
-- elements will be missing from the result of Topo_Sort. If all
-- elements belong to cycles or are successors of cycle elements,
-- the result of Topo_Sort will be a null vector.
--
-- The Elements and Pairs lists may contain duplicates. The result
-- of Topo_Sort does not contain duplicates.
