-- Programs.Execution.Draw (decl)
--
-- Generates drawings of control-flow graphs and call-graphs.
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
-- $Date: 2015/10/24 19:36:51 $
--
-- $Log: programs-execution-draw.ads,v $
-- Revision 1.6  2015/10/24 19:36:51  niklas
-- Moved to free licence.
--
-- Revision 1.5  2011-08-31 04:23:34  niklas
-- BT-CH-0222: Option registry. Option -dump. External help files.
--
-- Revision 1.4  2007/06/06 20:05:52  niklas
-- Added procedure Draw_Recursion_Graph.
--
-- Revision 1.3  2005/10/09 08:10:23  niklas
-- BT-CH-0013.
--
-- Revision 1.2  2004/05/01 10:59:20  niklas
-- First Tidorum version.
-- Changed source references from SSF to Tidorum.
-- Updated for changes in the parent package.
-- Omitted drawing of subprograms with asserted WCET.
-- Removed Program_T parameters where the target program can now be
-- accessed via some Bounds_Ref.
-- Added drawing of primary code address range (Code_Address_Image) under
-- the option Opt.Code_Address.
--
-- Revision 1.1  2003/03/11 08:30:04  holsti
-- First version renamed to be a child of Programs.Execution.
-- Normalized some lay-out and commenting.
--


package Programs.Execution.Draw is


   procedure Draw_Graphs (Bounds_Set : in Bounds_Set_T);
   --
   -- Draws diagrams of the call-graphs and control-flow graphs of
   -- the root subprograms in the given Bounds_Set and possibly of
   -- their lower-level callees, depending on the chosen options.
   --
   -- The control-flow graphs for subprograms with an asserted time
   -- bound are not drawn (because they are only stubs).


   procedure Draw_Recursion_Graph (Program : in Program_T);
   --
   -- Draws a diagram of the recursive call-graph of the closure
   -- of the root subprograms, as far as currently explored. Note
   -- that this is not certain to be the full call-graph because
   -- some subprograms may have unresolved dynamic control flow.


end Programs.Execution.Draw;
