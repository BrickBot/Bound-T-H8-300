-- Flow.Life.Opt (decl)
--
-- Command-line options for the live-cell analysis.
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
-- $Revision: 1.9 $
-- $Date: 2015/10/24 19:36:49 $
--
-- $Log: flow-life-opt.ads,v $
-- Revision 1.9  2015/10/24 19:36:49  niklas
-- Moved to free licence.
--
-- Revision 1.8  2013-02-12 08:47:19  niklas
-- BT-CH-0245: Global volatile cells and "volatile" assertions.
--
-- Revision 1.7  2011-08-31 04:23:34  niklas
-- BT-CH-0222: Option registry. Option -dump. External help files.
--
-- Revision 1.6  2007/12/21 10:09:39  niklas
-- BT-CH-0100: Fix Flow.Life re edge preconditions.
--
-- Revision 1.5  2007/08/02 11:16:36  niklas
-- Added the option Trace_Joining.
--
-- Revision 1.4  2007/01/25 21:25:15  niklas
-- BT-CH-0043.
--
-- Revision 1.3  2005/09/12 19:02:59  niklas
-- BT-CH-0008.
--
-- Revision 1.2  2001/03/10 00:31:29  holsti
-- All_Live option removed.
--
-- Revision 1.1  2001/01/07 21:54:17  holsti
-- First version.
--


with Options.Bool;


package Flow.Life.Opt is


   pragma Elaborate_Body;
   --
   -- To register the options.


   Trace_Iteration_Opt : aliased Options.Bool.Option_T (Default => False);
   --
   -- Whether to trace the steps in the least-fixpoint iteration that
   -- computes the set of live cells after each step.
   --
   Trace_Iteration : Boolean renames Trace_Iteration_Opt.Value;


   Trace_Cells_Opt : aliased Options.Bool.Option_T (Default => False);
   --
   -- Whether to display the set of cells found to be "live" after
   -- each step.
   --
   Trace_Cells  : Boolean renames Trace_Cells_Opt.Value;


   Trace_Volatile_Opt : aliased Options.Bool.Option_T (Default => False);
   --
   -- Whether to display the occurrences of volatile cells in
   -- the liveness analysis and its results.
   --
   Trace_Volatile  : Boolean renames Trace_Volatile_Opt.Value;


   Show_Per_Step_Opt : aliased Options.Bool.Option_T (Default => False);
   --
   -- Whether to display the "live" assignments found for each step,
   -- ordered by step index.
   --
   Show_Per_Step : Boolean renames Show_Per_Step_Opt.Value;


   Show_Per_Node_Opt : aliased Options.Bool.Option_T (Default => False);
   --
   -- Whether to display the "live" assignments found for each step,
   -- ordered by nodes (basic blocks).
   --
   Show_Per_Node : Boolean renames Show_Per_Node_Opt.Value;


   Show_Dead_Opt : aliased Options.Bool.Option_T (Default => False);
   --
   -- Whether to display the "dead" assignments found.
   --
   Show_Dead : Boolean renames Show_Dead_Opt.Value;


   Statistics_Opt : aliased Options.Bool.Option_T (Default => False);
   --
   -- Whether to display the number of assignments found to be live
   -- resp. dead.
   --
   Statistics : Boolean renames Statistics_Opt.Value;


   Trace_Joining_Opt : aliased Options.Bool.Option_T (Default => False);
   --
   -- Whether to display the process of joining the effects of
   -- consecutive steps, as they are done. (Trace_Joint_Effect, below,
   -- shows only the end result, the joined effect.)
   --
   Trace_Joining : Boolean renames Trace_Joining_Opt.Value;


   Trace_Joint_Effect_Opt : aliased Options.Bool.Option_T (Default => False);
   --
   -- Whether to display the consecutive steps for which a joint
   -- effect is created. The joint effect is also displayed if so.
   --
   Trace_Joint_Effect : Boolean renames Trace_Joint_Effect_Opt.Value;


   Deallocate : Boolean := True;
   --
   -- Whether to use Unchecked_Deallocation to release unused
   -- heap memory.


end Flow.Life.Opt;
