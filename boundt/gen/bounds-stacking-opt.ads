-- Bounds.Stacking.Opt (decl)
--
-- Command-line options for analysis of stack usage.
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
-- $Revision: 1.2 $
-- $Date: 2015/10/24 19:36:47 $
--
-- $Log: bounds-stacking-opt.ads,v $
-- Revision 1.2  2015/10/24 19:36:47  niklas
-- Moved to free licence.
--
-- Revision 1.1  2012-02-13 17:51:49  niklas
-- First version, for BT-CH-0230.
--


with Programs.Execution;
with Options.Limit_Valued;


package Bounds.Stacking.Opt is


   Max_Abs_Height_Opt : aliased Options.Limit_Valued.Option_T;
   --
   -- An upper limit on the absolute value of the expected local stack
   -- height at any point in the program. A deduced local stack height
   -- that is larger in absolute value than this limit is ignored,
   -- because we assume that it is a trivial bound due to the finite
   -- number of bits in stack pointers.
   --
   -- One benefit is that when a universal (context-independent) analysis
   -- of a subprogram finds such stack-height bounds, ignoring these
   -- bounds means that the stack height is still unbounded, and better
   -- bounds are then sought by context- dependent analysis.
   --
   -- A finite default limit may be set for some targets, depending
   -- on the bit-width and other properties of the target.
   --
   Max_Abs_Height : Storage.Bounds.Limit_T renames Max_Abs_Height_Opt.Value;


   procedure Ignore_Huge_Bounds (
      Limit : in out Programs.Execution.Stack_Limit_T);
   --
   -- Discards any stack-height limit (min and/or max) that is
   -- larger (in absolute value) than Max_Height.


end Bounds.Stacking.Opt;
