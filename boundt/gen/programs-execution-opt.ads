-- Programs.Execution.Opt (decl)
--
-- Command-line options for the management of program execution models
-- and bounds.
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
-- $Revision: 1.10 $
-- $Date: 2015/10/24 19:36:51 $
--
-- $Log: programs-execution-opt.ads,v $
-- Revision 1.10  2015/10/24 19:36:51  niklas
-- Moved to free licence.
--
-- Revision 1.9  2011-08-31 04:23:34  niklas
-- BT-CH-0222: Option registry. Option -dump. External help files.
--
-- Revision 1.8  2008/07/23 09:07:16  niklas
-- BT-CH-0139: Fix recursion in Programs.Execution.Paths.
--
-- Revision 1.7  2008/02/23 13:34:03  niklas
-- BT-CH-0115: Wcet_Loop output and option -loop_time.
--
-- Revision 1.6  2007/12/17 13:54:39  niklas
-- BT-CH-0098: Assertions on stack usage and final stack height, etc.
--
-- Revision 1.5  2007/07/09 13:40:14  niklas
-- Added Trace_Call_Input_Bounds.
--
-- Revision 1.4  2007/03/18 12:50:38  niklas
-- BT-CH-0050.
--
-- Revision 1.3  2007/01/25 21:25:17  niklas
-- BT-CH-0043.
--
-- Revision 1.2  2006/12/05 18:48:36  niklas
-- BT-CH-0040.
--
-- Revision 1.1  2004/05/01 09:51:50  niklas
-- First version.
--


with Options.Bool;


package Programs.Execution.Opt is


   pragma Elaborate_Body;
   --
   -- To register the options.


   Trace_Bounds_Opt : aliased Options.Bool.Option_T (Default => False);
   --
   -- Whether to trace on standard output the creation and
   -- processing of execution bounds.
   --
   Trace_Bounds : Boolean renames Trace_Bounds_Opt.Value;


   Trace_Stack_Bounds_Opt : aliased Options.Bool.Option_T (Default => False);
   --
   -- Whether to trace on standard output all forms of stack bounds:
   -- take-off height, global usage, local usage, final local height.
   --
   Trace_Stack_Bounds : Boolean renames Trace_Stack_Bounds_Opt.Value;


   Trace_Call_Input_Bounds_Opt : aliased Options.Bool.Option_T (
      Default => False);
   --
   -- Whether to trace on standard output the bounds on input parameters
   -- for calls, when such bounds are set.
   --
   Trace_Call_Input_Bounds : Boolean renames Trace_Call_Input_Bounds_Opt.Value;


   Warn_Unbounded_Call_Opt : aliased Options.Bool.Option_T (Default => False);
   --
   -- Whether to warn about each call with unbounded time/space
   -- that (probably) also contributes to the caller's unbounded
   -- time/space.
   --
   Warn_Unbounded_Call : Boolean renames Warn_Unbounded_Call_Opt.Value;


   Deallocate : Boolean := True;
   --
   -- Whether to use Unchecked_Deallocation to release unused
   -- heap memory.


end Programs.Execution.Opt;
