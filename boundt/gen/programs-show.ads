-- Programs.Show (decl)
--
-- Display of subprograms, subprogram sets and subprogram lists, for
-- tracing and monitoring the progress of the analysis.
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
-- $Revision: 1.8 $
-- $Date: 2015/10/24 19:36:52 $
--
-- $Log: programs-show.ads,v $
-- Revision 1.8  2015/10/24 19:36:52  niklas
-- Moved to free licence.
--
-- Revision 1.7  2008-11-09 21:41:58  niklas
-- BT-CH-0158: Option "-trace instr".
--
-- Revision 1.6  2007/08/03 17:45:14  niklas
-- Corrected context clauses.
--
-- Revision 1.5  2007/08/03 17:36:41  niklas
-- Recreated to extract the pure Subprogram/Call operations.
--


package Programs.Show is


   procedure Show (Subprogram : in Subprogram_T);
   --
   -- Shows the single Subprogram, with the main properties of the
   -- subprogram: whether the flow-graph is reducible, whether the
   -- subprogram returns to the caller, and all the calls from the
   -- subprogram.
   --
   -- Takes the left margin from the current column.


   procedure Show (Subprograms : in Subprogram_Set_T);
   --
   -- Shows the subprograms in the set, with the main properties
   -- of the subprogram: whether the flow-graph is reducible, whether
   -- the subprogram returns to the caller, and all the calls from the
   -- subprogram.
   --
   -- Takes the left margin from the current column.


   procedure Show (Subprograms : in Subprogram_List_T);
   --
   -- Shows the subprograms in the list, by index, full name and
   -- entry address. No other information is given.
   --
   -- Takes the left margin from the current column.


   procedure Show (Calls : in Call_List_T);
   --
   -- Shows the calls in the list, using Image (Call_T).
   --
   -- Takes the left margin from the current column.


   procedure Trace_Instructions_And_Branches (Subprogram : in Subprogram_T);
   --
   -- Traces the disassembled instructions (steps), their arithmetic
   -- effects and computational efforts, and the arithmetic conditions
   -- and execution times of the edges.
   --
   -- This implements the option "-trace instr".


end Programs.Show;
