-- Flow.Origins.Opt (decl)
--
-- Command-line options for the value-origin analysis.
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
-- $Date: 2015/10/24 19:36:49 $
--
-- $Log: flow-origins-opt.ads,v $
-- Revision 1.6  2015/10/24 19:36:49  niklas
-- Moved to free licence.
--
-- Revision 1.5  2014/06/01 10:30:00  niklas
-- Added Trace_Flag_Origins option.
--
-- Revision 1.4  2013-02-12 08:47:19  niklas
-- BT-CH-0245: Global volatile cells and "volatile" assertions.
--
-- Revision 1.3  2011-08-31 04:23:34  niklas
-- BT-CH-0222: Option registry. Option -dump. External help files.
--
-- Revision 1.2  2007/01/25 21:25:15  niklas
-- BT-CH-0043.
--
-- Revision 1.1  2005/05/09 15:24:22  niklas
-- First version.
--


with Options.Bool;


package Flow.Origins.Opt is


   pragma Elaborate_Body;
   --
   -- To register the options.


   Propagate_Opt : aliased Options.Bool.Option_T (Default => True);
   --
   -- Whether to apply value-origin analysis at all.
   -- If this is False, the options below are irrelevant.
   --
   Propagate : Boolean renames Propagate_Opt.Value;


   Trace_Iteration_Opt : aliased Options.Bool.Option_T (Default => False);
   --
   -- Whether to trace the steps in the least-fixpoint iteration that
   -- propagates the origins of values of cells.
   --
   Trace_Iteration : Boolean renames Trace_Iteration_Opt.Value;


   Trace_Volatile_Opt : aliased Options.Bool.Option_T (Default => False);
   --
   -- Whether to trace the occurrences of volatile cells in the value-origin
   -- analysis: firstly, when a volatile cell is omitted from the analysis,
   -- and secondly, when a copy assignment is considered an origin (and not
   -- a copy) because the source cell is volatile.
   --
   Trace_Volatile : Boolean renames Trace_Volatile_Opt.Value;


   Show_Results_Opt : aliased Options.Bool.Option_T (Default => False);
   --
   -- Whether to display the results of the value-origin propagation.
   --
   Show_Results : Boolean renames Show_Results_Opt.Value;


   Trace_Invariant_Cells_Opt : aliased Options.Bool.Option_T (Default => False);
   --
   -- Whether to show the cells that are found to be invariant as
   -- a result of the value-origin analysis.
   --
   Trace_Invariant_Cells : Boolean renames Trace_Invariant_Cells_Opt.Value;


   Trace_Flag_Origins_Opt : aliased Options.Bool.Option_T (Default => False);
   --
   -- Whether to trace problems (and perhaps successes) in the
   -- analysis and use of the origins of condition flags.
   --
   Trace_Flag_Origins : Boolean renames Trace_Flag_Origins_Opt.Value;


   Deallocate : Boolean := True;
   --
   -- Whether to use Unchecked_Deallocation to release unused
   -- heap memory.


end Flow.Origins.Opt;
