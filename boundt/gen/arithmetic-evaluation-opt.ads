-- Arithmetic.Evaluation.Opt
--
-- Command-line options for the arithmetic-evaluation operations.
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
-- $Date: 2015/10/24 19:36:46 $
--
-- $Log: arithmetic-evaluation-opt.ads,v $
-- Revision 1.3  2015/10/24 19:36:46  niklas
-- Moved to free licence.
--
-- Revision 1.2  2011-08-31 04:23:33  niklas
-- BT-CH-0222: Option registry. Option -dump. External help files.
--
-- Revision 1.1  2009-11-27 11:28:06  niklas
-- BT-CH-0184: Bit-widths, Word_T, failed modular analysis.
--


with Options.Bool;


package Arithmetic.Evaluation.Opt is


   pragma Elaborate_Body;
   --
   -- To register the options.


   Trace_Refinement_Path_Opt : aliased Options.Bool.Option_T (Default => False);
   --
   -- Whether to trace (display) each refinement of the arithmetic
   -- expressions and sub-expressions during the evaluation of
   -- an arithmetic expression or assignment.
   --
   Trace_Refinement_Path : Boolean renames Trace_Refinement_Path_Opt.Value;


   Warn_Overflow_Opt : aliased Options.Bool.Option_T (Default => False);
   --
   -- Whether to check for and warn when the arithmetic evaluation
   -- of an expression shows that overflow would happen in the
   -- execution of the target program.
   --
   Warn_Overflow : Boolean renames Warn_Overflow_Opt.Value;


end Arithmetic.Evaluation.Opt;
