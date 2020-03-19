-- Options.Common (decl)
--
-- Options that are common to many target processors, and therefore
-- defined and registered in this target-independent package, although
-- the options are used only in target-specific modules.
--
-- Author: Niklas Holsti, Tidorum Ltd
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
-- $Revision: 1.3 $
-- $Date: 2015/10/24 19:36:50 $
--
-- $Log: options-common.ads,v $
-- Revision 1.3  2015/10/24 19:36:50  niklas
-- Moved to free licence.
--
-- Revision 1.2  2011-09-01 20:00:46  niklas
-- Renamed Trace to Trace_Decode, per its meaning.
--
-- Revision 1.1  2011-08-31 04:17:13  niklas
-- Added for BT-CH-0222: Option registry. Option -dump. External help files.
--


with Options.Bool;


package Options.Common is


   pragma Elaborate_Body;
   --
   -- To register the options.


   Trace_Decode : aliased Options.Bool.Option_T (Default => False);
   --
   -- Whether to trace on standard output the decoding of target
   -- program instructions, showing the address and the instruction
   -- word (usually in hex) and the disassembled instruction.
   --
   -- Possibly unsafe actions are always displayed (as warnings),
   -- and not controlled by this option.


   Trace_Effect : aliased Options.Bool.Option_T (Default => False);
   --
   -- Whether the arithmetic effect is included in the tracing of the
   -- decoding process.
   -- This has an effect only if Trace is also true.


   Split_Effect : aliased Options.Bool.Option_T (Default => False);
   --
   -- Whether to split the arithmetic effect of a step into one
   -- line per assignment, instead of showing all assignments on
   -- the same line. This option keeps the "-trace effect" output
   -- narrower.


   Trace_Chains : aliased Options.Bool.Option_T (Default => False);
   --
   -- Whether to trace the chaining of "short" instructions/operations
   -- to make up "longer" arithmetic" operations.


end Options.Common;
