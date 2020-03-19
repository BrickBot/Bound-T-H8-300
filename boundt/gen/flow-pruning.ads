-- Flow.Pruning (decl)
--
-- Pruning of infeasible parts and paths in the control-flow graphs
-- or actually in computation models associated with flow-graphs.
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
-- $Date: 2015/10/24 19:36:49 $
--
-- $Log: flow-pruning.ads,v $
-- Revision 1.4  2015/10/24 19:36:49  niklas
-- Moved to free licence.
--
-- Revision 1.3  2007-12-17 13:54:37  niklas
-- BT-CH-0098: Assertions on stack usage and final stack height, etc.
--
-- Revision 1.2  2005/02/16 21:11:45  niklas
-- BT-CH-0002.
--
-- Revision 1.1  2004/04/28 19:07:50  niklas
-- First version.
--


with Flow.Computation;


package Flow.Pruning is


   procedure Prune (Model : in out Computation.Model_Ref);
   --
   -- Given a computation Model for a flow-graph with some parts (steps
   -- or edges) marked as infeasible, this operation propagates the 
   -- infeasibility to adjacent parts of the Model, as necessary.
   -- The flow-graph should not contain loose edges, but may contain
   -- dynamic edges.
   --
   -- A flow-graph part is feasible if and only if:
   --
   -- > there is a feasible path from the entry step to this part, and
   --
   -- > there is a feasible path from this part to a return step, or
   --   to a call that does not return to the caller, or to the head
   --   of an eternal loop (a loop with no feasible exit edges).
   --
   -- All the changes are applied to the computation Model. The
   -- underlying flow-graph is not modified.


end Flow.Pruning;
