-- Flow.Show (decl)
--
-- Textual output of flow-graphs and related data structures.
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
-- $Revision: 1.8 $
-- $Date: 2015/10/24 19:36:49 $
--
-- $Log: flow-show.ads,v $
-- Revision 1.8  2015/10/24 19:36:49  niklas
-- Moved to free licence.
--
-- Revision 1.7  2008-01-31 21:57:45  niklas
-- BT-CH-0108: Fixes to BT-CH-0098.
--
-- Revision 1.6  2007/12/22 15:23:47  niklas
-- BT-CH-0101: Option "-trace graph".
--
-- Revision 1.5  2006/05/29 11:22:34  niklas
-- BT-CH-0023.
--
-- Revision 1.4  2006/04/27 08:36:29  niklas
-- Added function Locus (Step_Tag_T).
--
-- Revision 1.3  2005/04/18 09:30:46  niklas
-- Added Report_Unresolved_Flow.
--
-- Revision 1.2  2004/04/26 18:27:58  niklas
-- First Tidorum version.
-- Include Symbol Table parameter in Show_Steps and Show_Nodes.
-- Show also Dynamics of Step.
-- Catch exceptions in Show_Steps and Show_Nodes.
--
-- Revision 1.1  2001/03/21 20:31:24  holsti
-- First version.
--



with Output;
with Symbols;


package Flow.Show is


   function Statements (
      Step   : Step_T;
      Source : Symbols.Symbol_Table_T)
   return Output.Statement_Range_T;
   --
   -- The statement range corresponding to the prime address of
   -- the given step.


   function All_Statements (
      Step   : Step_T;
      Source : Symbols.Symbol_Table_T)
   return Output.Statement_Range_T;
   --
   -- The statement range corresponding to all instruction addresses
   -- involved with the given step.


   function Statements (
      Steps  : Step_List_T;
      Source : Symbols.Symbol_Table_T)
   return Output.Statement_Range_T;
   --
   -- The statement range corresponding to the prime addresses of
   -- the given steps.


   function Locus (
      Graph  : Graph_T;
      Source : Symbols.Symbol_Table_T)
   return Output.Locus_T;
   --
   -- Identifies the location of the instructions covered by
   -- the flow-graph, in the program under analysis.
   -- The "statements" part of the result is defined, and
   -- perhaps also the source-file.
   -- The symbol-table is needed to map code addresses to
   -- source locations.


   function Locus (
      Tag    : Step_Tag_T;
      Source : Symbols.Symbol_Table_T)
   return Output.Locus_T;
   --
   -- Location of the step-tag in the target program.
   -- The "statements" part of the result is defined, and
   -- perhaps also the source-file.


   function Locus (
      Step   : Step_T;
      Source : Symbols.Symbol_Table_T)
   return Output.Locus_T;
   --
   -- Location of the step in the target program.
   -- The "statements" part of the result is defined, and
   -- perhaps also the source-file.


   function Locus (
      Node   : Node_T;
      Source : Symbols.Symbol_Table_T)
   return Output.Locus_T;
   --
   -- Location of the node in the target program.
   -- The "statements" part of the result is defined, and
   -- perhaps also the source-file.


   function Locus (
      Edge   : Boundable_Edge_T'Class;
      Source : Symbols.Symbol_Table_T)
   return Output.Locus_T;
   --
   -- Location of the Edge (actuall the source step) in the
   -- target program.
   -- The "statements" part of the result is defined, and
   -- perhaps also the source-file.


   procedure Show_Steps (
      Graph  : in Graph_T;
      Source : in Symbols.Symbol_Table_T);
   --
   -- Displays the individual steps and step-edges.
   -- Results on standard output.


   procedure Show_Nodes (
      Graph  : in Graph_T;
      Source : in Symbols.Symbol_Table_T);
   --
   -- Displays the basic-block nodes and the related edges.
   -- Results on standard output.


   procedure Trace_Graph (
      Graph : in Graph_T;
      Source : in Symbols.Symbol_Table_T);
   --
   -- Emits the steps and edges in the Graph as Trace lines, in a
   -- form that allows sorting and comparing by address while
   -- omitting the step and edge indices (which are more or less
   -- randomly assigned).


   procedure Report_Unresolved_Flow (
      Graph  : in Graph_T;
      Source : in Symbols.Symbol_Table_T);
   --
   -- Reports the state of the remaining dynamic edges.
   -- An Unresolved edge is reported as an Error.
   -- A Stable or Growing edge is reported as a Note.


end Flow.Show;
