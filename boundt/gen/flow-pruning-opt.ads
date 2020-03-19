-- Flow.Pruning.Opt (decl)
--
-- Command-line options for the pruning of infeasible parts of flow-graphs.
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
-- $Log: flow-pruning-opt.ads,v $
-- Revision 1.4  2015/10/24 19:36:49  niklas
-- Moved to free licence.
--
-- Revision 1.3  2011-08-31 04:23:34  niklas
-- BT-CH-0222: Option registry. Option -dump. External help files.
--
-- Revision 1.2  2005/06/29 12:52:22  niklas
-- Added Warn_Unreachable.
--
-- Revision 1.1  2004/04/28 19:07:49  niklas
-- First version.
--


with Options.Bool;


package Flow.Pruning.Opt is


   pragma Elaborate_Body;
   --
   -- To register the options.


   Prune_Opt : aliased Options.Bool.Option_T (Default => True);
   --
   -- Whether to prune infeasible parts at all.
   --
   -- Note that if pruning is disabled, it may be necessary to disable
   -- some other options, for consistency reasons.
   --
   -- In particular, if Prune is set to False then the option Flow.Const.-
   -- Refine_Edge_Conditions must also be set False. Otherwise, the latter
   -- action (refining edge conditions when constant) may mark some edges
   -- as infeasible ("Never" taken) which may make their target steps
   -- unreachable for e.g. liveness analysis. However, without Prune these
   -- unreachable steps are not "neutralized" e.g. for the arithmetic
   -- analysis, which may lead to failures in the Omega calculation such
   -- as Omega expressions that refer to non-existent variables.
   --
   Prune : Boolean renames Prune_Opt.Value;


   Warn_Unreachable_Opt : aliased Options.Bool.Option_T (Default => True);
   --
   -- Whether to issue a warning when the analysis shows that some
   -- flow-graph part is unreachable. Such warnings may be useful
   -- because the analysis may be misled by conflicting assertions or
   -- errors in the model (eg. undetected aliasing).
   --
   Warn_Unreachable : Boolean renames Warn_Unreachable_Opt.Value;


   Trace_Opt : aliased Options.Bool.Option_T (Default => False);
   --
   -- Whether to trace the pruning events.
   --
   Trace : Boolean renames Trace_Opt.Value;


end Flow.Pruning.Opt;
