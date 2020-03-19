-- Options.Groups (decl)
--
-- Common, but distributed, groups of options.
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
-- $Date: 2015/10/24 19:36:50 $
--
-- $Log: options-groups.ads,v $
-- Revision 1.4  2015/10/24 19:36:50  niklas
-- Moved to free licence.
--
-- Revision 1.3  2012-01-28 12:56:14  niklas
-- BT-CH-0226: Options update for AVR.
--
-- Revision 1.2  2011-09-01 13:04:38  niklas
-- Added the Timing group.
--
-- Revision 1.1  2011-08-31 04:17:13  niklas
-- Added for BT-CH-0222: Option registry. Option -dump. External help files.
--


package Options.Groups is


   pragma Elaborate_Body;
   --
   -- To define the priority ordering of the groups here defined.


   Inputs : constant Group_Name_T := Group ("inputs");
   --
   -- Options to define the inputs for the analysis.


   Outputs : constant Group_Name_T := Group ("outputs");
   --
   -- Options to define the output (files) from the analysis.


   Analysis : constant Group_Name_T := Group ("analysis");
   --
   -- Options to define what should be analysed, and which
   -- analyses to do.


   Assertions : constant Group_Name_T := Group ("assertions");
   --
   -- Options relating to assertions.


   Control_Flow : constant Group_Name_T := Options.Group ("control_flow");
   --
   -- Options relating to control-flow analysis.


   Loops : constant Group_Name_T := Options.Group ("loops");
   --
   -- Options relating to the detection and analysis of loops.


   Calls : constant Group_Name_T := Options.Group ("calls");
   --
   -- Options relating to the modelling and analysis of calls.


   Const_Prop : constant Group_Name_T := Options.Group ("const_prop");
   --
   -- Options that control details of the constant-propagation analysis.


   Arithmetic : constant Group_Name_T := Options.Group ("arithmetic");
   --
   -- Options that control details of the arithmetic analysis.


   Timing : constant Group_Name_T := Options.Group ("timing");
   --
   -- Options relating to the execution-time modelling and analysis.


   Stack_Usage : constant Group_Name_T := Options.Group ("stack");
   --
   -- Options relating to the stack-usage analysis.


   Trace : constant Group_Name_T := Group ("trace");
   --
   -- Options to trace significant events during analysis.
   -- On the command line, these Boolean options are defined as
   --       -trace <item>
   -- where the option-name is "trace-<item>".


   Warn : constant Group_Name_T := Group ("warn");
   --
   -- Options to warn about potential problems found during analysis.
   -- On the command line, these Boolean options are defined as
   --    -warn [no_]<item>
   -- where the option-name is "warn-<item>".


   Imp : constant Group_Name_T := Group ("imp");
   --
   -- Options that control internal implementation choices.
   -- On the command line, these options are defined as
   --    -imp [no_]<item>
   -- where the option-name is "imp-item".


   Host_Memory : constant Group_Name_T := Group ("host_memory");
   --
   -- Options (typically implementation choices) that control the
   -- memory management of Bound-T itself, on the host computer.


   Resource_Limits : constant Group_Name_T := Group ("rlimit");
   --
   -- Options that control or limit the amount of computational
   -- resources (processing time, memory, error messages, etc.)
   -- that an analysis can use, before the analysis is stopped.


end Options.Groups;
