-- Flow.Calls.Opt (decl)
--
-- Command-line options for subprogram calls in flow-graphs.
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
-- $Revision: 1.5 $
-- $Date: 2015/10/24 19:36:48 $
--
-- $Log: flow-calls-opt.ads,v $
-- Revision 1.5  2015/10/24 19:36:48  niklas
-- Moved to free licence.
--
-- Revision 1.4  2011-08-31 04:23:34  niklas
-- BT-CH-0222: Option registry. Option -dump. External help files.
--
-- Revision 1.3  2008/09/07 07:29:08  niklas
-- BT-CH-0142: Conditional calls and option -[no_]tail_calls.
--
-- Revision 1.2  2007/01/13 13:51:03  niklas
-- BT-CH-0041.
--
-- Revision 1.1  2006/11/26 22:07:25  niklas
-- BT-CH-0039.
--


with Options.Bool;


package Flow.Calls.Opt is


   pragma Elaborate_Body;
   --
   -- To register the options.


   Trace_Calls_Opt : aliased Options.Bool.Option_T (Default => False);
   --
   -- Whether to display each call as it is found during decoding.
   --
   Trace_Calls : Boolean renames Trace_Calls_Opt.Value;


   Trace_Unused_Calls_Opt : aliased Options.Bool.Option_T (Default => False);
   --
   -- Whether to trace the detection of calls to a subprogram that is
   -- known or asserted to be unused (never called in any execution
   -- that interests us) and will therefore not be analysed.
   --
   Trace_Unused_Calls : Boolean renames Trace_Unused_Calls_Opt.Value;


   Trace_Call_Effects_Opt : aliased Options.Bool.Option_T (Default => False);
   --
   -- Whether to display the effects of call-steps when they
   -- are computed or updated.
   --
   Trace_Call_Effects : Boolean renames Trace_Call_Effects_Opt.Value;


   Warn_No_Return_Opt : aliased Options.Bool.Option_T (Default => False);
   --
   -- Whether to issue a warning when we find a call to a subprogram
   -- that we know will never return to the caller.
   --
   Warn_No_Return : Boolean renames Warn_No_Return_Opt.Value;


   Warn_Computed_Return_Opt : aliased Options.Bool.Option_T (Default => False);
   --
   -- Whether to issue a warning when we find a call where the
   -- return point is not statically known but computed in some
   -- dynamic way.
   --
   Warn_Computed_Return : Boolean renames Warn_Computed_Return_Opt.Value;


   Detect_Tail_Calls_Opt : aliased Options.Bool.Option_T (Default => True);
   --
   -- Whether to detect tail calls that have been optimized into
   -- some non-call form, typically a simple jump to the callee.
   -- This detection is a target-specific function; the option is
   -- is defined for all targets, but tail-call detection may not
   -- be implemented for a given target, whatever the option value.
   -- When such an optimized tail-call structure is detected, and
   -- the option is enabled, the tail call should be modelled as a
   -- real call, immediately followed by a return from the caller.
   --
   Detect_Tail_Calls : Boolean renames Detect_Tail_Calls_Opt.Value;


end Flow.Calls.Opt;
