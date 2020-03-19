-- Flow.Computation.Show (decl)
--
-- Textual output of computation models.
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
-- $Date: 2015/10/24 19:36:48 $
--
-- $Log: flow-computation-show.ads,v $
-- Revision 1.4  2015/10/24 19:36:48  niklas
-- Moved to free licence.
--
-- Revision 1.3  2007-10-11 11:58:47  niklas
-- Added Report_Unresolved_Flow, as in Flow.Show.
--
-- Revision 1.2  2005/04/18 10:50:43  niklas
-- Added Report_Unresolved_Data.
--
-- Revision 1.1  2005/02/16 21:11:42  niklas
-- BT-CH-0002.
--


with Symbols;


package Flow.Computation.Show is


   Feasible_Mark : constant array (Boolean) of Character := (
      False => '-',
      True  => '+');
   --
   -- A mark that can be used to show whether some part of a flow-graph
   -- is feasible or infeasible under a computation model.
   --
   -- A '+' means feasible, a '-' means infeasible.


   procedure Show (Model : in Model_T);
   --
   -- Displays the computation Model, taking the current Col position
   -- as the left margin for each line (some lines may be more indented).


   procedure Show (Model : in Model_Ref);
   --
   -- Same as Show above, but for a Model_Ref.


   procedure Report_Unresolved_Data (
      Model  : in Model_Ref;
      Source : in Symbols.Symbol_Table_T);
   --
   -- Reports any remaining (unresolved) dynamic data references in
   -- the computation Model, as Warnings.


   procedure Report_Unresolved_Flow (
      Model  : in Model_Ref;
      Source : in Symbols.Symbol_Table_T);
   --
   -- Reports the state of the remaining feasible dynamic edges.
   -- An Unresolved feasible edge is reported as an Error.
   -- A Stable or Growing feasible edge is reported as a Note.


end Flow.Computation.Show;
