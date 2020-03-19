-- Bounds.Looping.Opt (decl)
--
-- Command-line options for arithmetic analysis of loop-bounds.
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
-- $Revision: 1.8 $
-- $Date: 2015/10/24 19:36:47 $
--
-- $Log: bounds-looping-opt.ads,v $
-- Revision 1.8  2015/10/24 19:36:47  niklas
-- Moved to free licence.
--
-- Revision 1.7  2012-02-13 17:52:19  niklas
-- BT-CH-0230: Options -max_loop and -max_stack for spurious bounds.
--
-- Revision 1.6  2011-09-08 08:54:19  niklas
-- Added option "warn sw_neg_step", Warn_Swap_Small_Negative_Step.
--
-- Revision 1.5  2011-09-08 08:46:49  niklas
-- Added option "sw_neg_step", Swap_Small_Negative_Step.
--
-- Revision 1.4  2011-08-31 04:23:33  niklas
-- BT-CH-0222: Option registry. Option -dump. External help files.
--
-- Revision 1.3  2009-11-27 11:28:06  niklas
-- BT-CH-0184: Bit-widths, Word_T, failed modular analysis.
--
-- Revision 1.2  2008/05/03 09:22:06  niklas
-- BT-CH-0126: Combining joint-counter and each-counter analysis.
--
-- Revision 1.1  2008/04/26 19:19:43  niklas
-- BT-CH-0124: Joint loop counters and induction variables.
--


with Options.Bool;
with Options.Limit_Valued;


package Bounds.Looping.Opt is


   Trace_Counters_Opt : aliased Options.Bool.Option_T (Default => False);
   --
   -- Whether to list the loop-counter candidates and the bounds
   -- derived for them.
   --
   Trace_Counters : Boolean renames Trace_Counters_Opt.Value;


   Use_Positive_Form_Opt : aliased Options.Bool.Option_T (Default => True);
   --
   -- Whether to try the "positive" loop-bounds analysis. If so, the
   -- "positive" analysis is applied before the "negative" analysis,
   -- if the latter is applied at all (see Proc_Param_T.Neg).
   --
   -- The "positive" analysis always fails if the loop termination
   -- conditions are modular, because the set of repeating counter
   -- values is then unbounded in both directions.
   --
   Use_Positive_Form : Boolean renames Use_Positive_Form_Opt.Value;


   type Proc_Param_T is record
      Joint : Boolean;
      Each  : Boolean;
      Neg   : Boolean;
      First : Boolean;
   end record;
   --
   -- Defines a procedure for applying the various forms of
   -- loop-bounds analysis, as a specialization of the general
   -- procedure.
   --
   -- Note that the option Use_Positive_Form defines if the
   -- procedure tries the "positive" analysis. If so, the
   -- "positive" analysis is applied before the "negative"
   -- analysis, if the latter is applied at all; see Neg, below.
   -- The "positive" analysis always fails if the loop
   -- termination conditions are modular, because the set
   -- of repeating counter values is then unbounded in
   -- both directions.
   --
   -- Joint
   --    Whether to use "joint counter" analysis at all.
   -- Each
   --    Whether to use "each counter" analysis at all.
   -- Neg
   --    Whether to do the "negative" analysis even if the
   --    "positive" analysis has given loop bounds. Otherwise,
   --    the "negative" analysis is applied only when the "positive"
   --    failed (gave no loop bounds, or was not even attempted).
   --    This choice applies only to the "each counter" analysis,
   --    because the First component can have the same effect for
   --    the "joint counter" analysis.
   -- First
   --    Whether to stop at and return the first loop bound found.


   type Proc_T is (Full, Each, Trad, Joint, First);
   --
   -- The choice of procedures.
   --
   -- Full
   --    Try all forms of analysis, except for the positive
   --    form of the "each counter" analysis because it is
   --    subsumed by the positive "joint counter" analysis.
   --
   -- Each a.k.a. "Full Each"
   --    Try all forms of "each counter" analysis. Do not use
   --    any "joint counter" analysis.
   --
   -- Trad a.k.a. "Traditional"
   --    Try the positive "each counter" analysis for all counter
   --    variables (if Use_Positive_Form is True).
   --    For each counter, try the negative "each counter" analysis
   --    if the positive form fails (or Use_Positive_Form is False).
   --
   -- Joint a.k.a "Joint First"
   --    First try the positive "joint counter" (if Use_Positive_Form
   --    is True). If that fails (or if Use_Positive_Form is False)
   --    try the negative "joint counter" analysis. Do not use any
   --    "each counter" analysis.
   --
   -- First a.k.a. "Full First"
   --    First try the positive "joint counter" (if Use_Positive_Form
   --    is True). If it fails (or if Use_Positive_Form if False) try
   --    the negative "each counter" analysis for the counter variables,
   --    one by one, stopping when a bound is found. If all else fails,
   --    try the negative "joint counter" analysis.


   Param : constant array (Proc_T) of Proc_Param_T := (
      --         Joint   Each    Neg     First     -- Procedure
      --         -----   ----    ---     -----     -- ---------
      Full  =>  (True ,  True ,  True ,  False),   -- Full
      Each  =>  (False,  True ,  True ,  False),   -- Full Each
      Trad  =>  (False,  True ,  False,  False),   -- Traditional
      Joint =>  (True ,  False,  False,  True ),   -- Joint First
      First =>  (True ,  True ,  False,  True ));  -- Full First
   --
   -- The parameters for each choice of procedure.
   -- The "positive" analysis is separately controlled by Use_Positive_Form.


   package Proc_Valued is new Options.Discrete_Valued (
      Value_Type  => Proc_T,
      Value_Image => Proc_T'Image);
   --
   -- Options with values of type Proc_T.


   Proc_Opt : aliased Proc_Valued.Option_T (Default => First);
   --
   -- The chosen loop-analysis procedure.
   --
   Proc : Proc_T renames Proc_Opt.Value;


   Swap_Small_Negative_Step_Opt : aliased Options.Bool.Option_T (
     Default => True);
   --
   -- Whether to convert a deduced large induction-variable step
   -- that represents a small negative step, to that small negative
   -- step. For example, in some cases an 8-bit step of -1 is
   -- represented in the code or intermediate analysis results by
   -- a positive step of 255, relying on wrap-around of two's
   -- complement addition.
   --
   Swap_Small_Negative_Step : Boolean
     renames Swap_Small_Negative_Step_Opt.Value;


   Warn_Swap_Small_Negative_Step_Opt : aliased Options.Bool.Option_T (
     Default => True);
   --
   -- Whether to emit a warning when the action enabled by
   -- Swap_Small_Negative_Step_Opt takes place.
   --
   Warn_Swap_Small_Negative_Step : Boolean
     renames Warn_Swap_Small_Negative_Step_Opt.Value;


   Max_Loop_Bound_Opt : aliased Options.Limit_Valued.Option_T;
   --
   -- An upper limit on the expected number of iterations of any loop.
   -- A deduced loop-bound that is larger than this limit is ignored,
   -- becase we assume that it is a trivial bound due to the finite
   -- number of bits in loop counters. One benefit is that when a
   -- universal (context-independent) analysis of a subprogram finds
   -- such loop-bounds, ignoring these bounds means that the loops are
   -- still unbounded, and better bounds are then sought by context-
   -- dependent analysis.
   --
   -- A finite default limit may be set for some targets, depending
   -- on the bit-width and other properties of the target.
   --
   Max_Loop_Bound : Storage.Bounds.Limit_T renames Max_Loop_Bound_Opt.Value;


   procedure Set_Joint_Counter (State : Boolean);
   --
   -- Legacy support fo the "-[no_]joint_counter" option.


end Bounds.Looping.Opt;
