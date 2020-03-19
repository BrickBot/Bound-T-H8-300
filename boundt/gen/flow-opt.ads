-- Flow.Opt (decl)
--
-- Command-line options for the control-flow analysis.
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
-- $Revision: 1.14 $
-- $Date: 2015/10/24 19:36:49 $
--
-- $Log: flow-opt.ads,v $
-- Revision 1.14  2015/10/24 19:36:49  niklas
-- Moved to free licence.
--
-- Revision 1.13  2013-02-05 20:08:31  niklas
-- Using Options.General.Trace_Resolution for Trace_Flow_Resolution.
--
-- Revision 1.12  2011-08-31 04:23:34  niklas
-- BT-CH-0222: Option registry. Option -dump. External help files.
--
-- Revision 1.11  2007/08/25 18:53:20  niklas
-- Added option Check_Consistency.
--
-- Revision 1.10  2007/07/21 18:18:42  niklas
-- BT-CH-0064. Support for AVR/IAR switch-handler analysis.
--
-- Revision 1.9  2007/07/11 18:47:03  niklas
-- Added Virtual_Mood_T (for AVR/IAR). Not yet used in a generic way.
--
-- Revision 1.8  2007/01/25 21:25:15  niklas
-- BT-CH-0043.
--
-- Revision 1.7  2006/11/26 22:07:26  niklas
-- BT-CH-0039.
--
-- Revision 1.6  2006/10/24 08:44:31  niklas
-- BT-CH-0028.
--
-- Revision 1.5  2006/08/22 12:12:45  niklas
-- Added the option Warn_No_Return (-warn return), for warning about
-- calls to non-returning subprograms. Before, these warnings were
-- always emitted; now they are disabled by default.
--
-- Revision 1.4  2006/05/06 06:59:21  niklas
-- BT-CH-0021.
--
-- Revision 1.3  2005/02/16 21:11:45  niklas
-- BT-CH-0002.
--
-- Revision 1.2  2003/02/17 16:16:18  holsti
-- Added option Trace_Calls (-trace calls) to display calls as they are found.
--
-- Revision 1.1  2003/01/03 11:45:53  holsti
-- First version.
--


with Options.Bool;


package Flow.Opt is


   pragma Elaborate_Body;
   --
   -- To register the options.


   Warn_Dynamic_Flow_Opt : aliased Options.Bool.Option_T (Default => True);
   --
   -- Whether to emit a warning whenever a dynamic (boundable) control
   -- transition element (edge, call) is added to a flow-graph.
   --
   Warn_Dynamic_Flow : Boolean renames Warn_Dynamic_Flow_Opt.Value;


   Trace_Construction_Opt : aliased Options.Bool.Option_T (Default => False);
   --
   -- Whether to trace the actions for constructing the control-flow graph.
   -- The actions are: add a step, add an edge, take a loose edge, bind a
   -- loose edge.
   --
   Trace_Construction : Boolean renames Trace_Construction_Opt.Value;


   Trace_Data_States_Opt : aliased Options.Bool.Option_T (Default => False);
   --
   -- Whether to trace the transformations of the data state associated
   -- with flow-graph elements (Step_Tag_T.Data) as such elements are
   -- created.
   --
   Trace_Data_States : Boolean renames Trace_Data_States_Opt.Value;


   Trace_Data_Refinement_Opt : aliased Options.Bool.Option_T (Default => False);
   --
   -- Whether to trace the occasions when some property of an element
   -- being added to a flow-graph becomes refined (partially evaluated)
   -- by the data state (Step_Tag_T.Data) associated with the element.
   --
   Trace_Data_Refinement : Boolean renames Trace_Data_Refinement_Opt.Value;


   function Trace_Flow_Resolution return Boolean;
   --
   -- Whether to trace the process of resolving dynamic control flow
   -- which here means the calls of Add_Resolved_Edge.
   -- Tracks Options.General.Trace_Resolution.


   Check_Consistency_Opt : aliased Options.Bool.Option_T (Default => False);
   --
   -- Whether to check the flow-graph construction for internal
   -- consistency. This can be quite time-consuming.
   --
   Check_Consistency : Boolean renames Check_Consistency_Opt.Value;


   Deallocate : Boolean := True;
   --
   -- Whether to use Unchecked_Deallocation to release unused
   -- heap memory.


   --
   ---   Options for virtual function calls
   --
   -- These options might be more at home in Flow.Calls.Opt, but they
   -- are defined here to make them available in Processor.Program.


   type Possible_Virtual_Mood_T is (Any, Static, Dynamic);
   --
   -- Whether virtual function calls (ie. dispatching calls) should
   -- be analysed Statically (when possible) or as Dynamic calls.
   -- The value Any means "not specified here".


   subtype Virtual_Mood_T is Possible_Virtual_Mood_T range Static .. Dynamic;
   --
   -- A specified virtual-mood, that is, not Any mood.


   package Virtual_Mood_Valued is new Options.Discrete_Valued (
      Value_Type  => Virtual_Mood_T,
      Value_Image => Virtual_Mood_T'Image);
   --
   -- Options with values of type Virtual_Mood_T.


   Virtual_Mood_Opt : aliased Virtual_Mood_Valued.Option_T (Default => Static);
   --
   -- Whether virtual function calls (ie. dispatching calls) should
   -- be analysed statically (when possible) or as dynamic calls, to
   -- be resolved by analysis or by assertions.
   --
   -- If virtual calls are analysed statically it may not be possible
   -- to limit the set of callees by an assertion.
   --
   Virtual_Mood : Virtual_Mood_T renames Virtual_Mood_Opt.Value;


end Flow.Opt;
