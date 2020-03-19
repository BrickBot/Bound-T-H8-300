-- Flow.Origins.Of_Condition_Flags (decl)
--
-- Analyses based on convex linear or affine relationships or simply on
-- value intervals have difficulty with branch conditions using Boolean
-- (1-bit) condition flags, because the condition flags are usually set
-- by comparison or subtraction instructions where the transformation from
-- a linear relation such as x < y into a Boolean value is non-linear, so
-- a linear model (e.g. a polyhedron) cannot represent the relationship
-- between the flag and the variables which were compared.
--
-- Moreover, the comparison instruction that set the flag may be some
-- distance away from the conditional branch which uses the flag.
--
-- The operations in this package attempt to sove this problem by
-- replacing condition flags used in the conditions of flow-graph
-- edges with the relation expressions that defined the values of
-- the condition flags, as found by value-origin analysis.
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
-- $Date: 2015/10/24 19:36:49 $
--
-- $Log: flow-origins-of_condition_flags.ads,v $
-- Revision 1.2  2015/10/24 19:36:49  niklas
-- Moved to free licence.
--
-- Revision 1.1  2013/12/08 22:05:57  niklas
-- BT-CH-0259: Storing value-origin analysis results in execution bounds.
--


package Flow.Origins.Of_Condition_Flags is


   subtype Bit_Cell_Ref is Arithmetic.Expr_Ref;
   --
   -- An expression which is a single 1-bit Cell (i.e. .Kind = Cell and
   -- .Width = 1).


   function Original_Comparison (
      Flag  : Bit_Cell_Ref;
      After : Flow.Step_T;
      Map   : Map_Ref)
   return Arithmetic.Expr_Ref;
   --
   -- The Flag expression occurs in a computation model and is evaluated
   -- immediately After the given step. If the origin of the value of
   -- the Flag (as defined by the Map of value-origins for this computation)
   -- is an expression that represents a numerical comparison (such
   -- as Arithmetic.Lts or Arithmetic.Minus_C), and the comparison expression
   -- is sure to have the same value when evaluated After the given step as
   -- when evaluated in the assignment of the Cell, the comparison expression
   -- is returned. Otherwise, the original Flag expression is returned.


   function With_Flags_Integrated (
      Condition : Arithmetic.Condition_T;
      After     : Flow.Step_T;
      Map       : Map_Ref)
   return Arithmetic.Condition_T;
   --
   -- The Condition occurs in a computation model and is evaluated
   -- immediately After the given step. This function returns a possibly
   -- modified condition in which the uses of condition flags are
   -- replaced by "in-lined" copies of the comparison expressions which
   -- defined the values of the flags, using value-origin Map for this
   -- computation.
   --
   -- To be precise, the Condition is modified as follows: each reference
   -- to a 1-bit cell is replaced by the result of Original_Comparison
   -- applied to that cell subexpression, if different from the original
   -- cell subexpression.
   --
   -- If the Condition does not use any such 1-bit flag cells (i.e. the
   -- function Original_Comparison always returns its Cell parameter
   -- unchanged) the original Condition is returned unchanged.


   procedure Integrate_In_Edge_Conditions (
      Within : in out Flow.Computation.Model_Ref;
      Using  : in     Map_Ref);
   --
   -- Modifies the conditions of edges in the computation model Within
   -- the given computation, Using the results of an earlier value-origin
   -- analysis for this computation, by replacing each condition with a
   -- condition With_Flags_Integrated. (This can make some 1-bit cells
   -- "dead", which may be discovered by a later liveness analysis, but
   -- the liveness analysis is not performed here.)


end Flow.Origins.Of_Condition_Flags;
