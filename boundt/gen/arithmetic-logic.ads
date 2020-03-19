-- Arithmetic.Logic (decl)
--
-- Logical (symbolic) inspection, manipulation and simplification
-- of arithmetic conditions.
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
-- $Revision: 1.4 $
-- $Date: 2015/10/24 19:36:46 $
--
-- $Log: arithmetic-logic.ads,v $
-- Revision 1.4  2015/10/24 19:36:46  niklas
-- Moved to free licence.
--
-- Revision 1.3  2011-09-06 17:42:38  niklas
-- Added the function Complementary and the procedure
-- Decode_Boolean_Bit for use in ALF export.
--
-- Revision 1.2  2007-07-21 18:18:40  niklas
-- BT-CH-0064. Support for AVR/IAR switch-handler analysis.
--
-- Revision 1.1  2006/05/06 06:59:19  niklas
-- BT-CH-0021.
--


with Storage;


package Arithmetic.Logic is


   False_Premise : exception;
   --
   -- Signals that the premise condition for some operation is
   -- found to contain a contradiction, thus to be always false,
   -- and that therefore the operation cannot return a result.


   --
   ---   Cell values implied by a condition
   --


   function Cell_Values_Implied (By : Condition_T)
   return Storage.Cell_Value_List_T;
   --
   -- Analyses the given condition to find if its truth implies
   -- some values for some cells. Returns some cell values implied
   -- By the condition.
   --
   -- For example, the condition "A = 2 and 4 = B" implies the
   -- cell-value list ((A,2),(B,4)).
   --
   -- For another example, the condition "A = 2 or B = 4" implies
   -- no cell values, giving a null list as the result.
   --
   -- There is no quarantee that the result is complete or even
   -- natural. For example, the condition "A = 2 or 1 < 0" will
   -- probably not be seen to imply any cell values, neither will
   -- the condition "A >= 2 and A <= 2".
   --
   -- This function may propagate False_Premise if it finds a
   -- contradiction in the condition. However, it is not guaranteed
   -- to find all contradictions.


   --
   ---   Relations of two conditions
   --


   function Complementary (Cond1, Cond2 : Condition_T)
   return Boolean;
   --
   -- Whether the two Conditions are complementary so that
   -- Cond1 = not Cond2 or vice versa. Another characterisation
   -- of complementary conditions is that (Cond1 and Cond2) is
   -- always false, and (Cond1 or Cond2) is always true.
   --
   -- This is an incomplete, syntactical test. If the function
   -- returns True, then Cond1 and Cond2 are complements; if the
   -- function returns False, they may or may not be complements.


   --
   ---   Simplifications of assignments
   --


   subtype Cond_Assignment_T is Assignment_T (Kind => Conditional);
   --
   -- An assignment of the conditional kind.


   procedure Decode_Boolean_Bit (
      Ass   : in     Cond_Assignment_T;
      Value :    out Condition_T);
   --
   -- If the conditional Assignment implements an encoding of
   -- its condition into the integers 0 and 1, we strip off
   -- the encoding and return the result in Value as a single
   -- expression, relying on an implicit interpretation of "false"
   -- as 0 and "true" as 1.
   --
   -- Partly for historical reasons, Bound-T seldom (never?)
   -- automatically identifies a false Condition_T with the
   -- integer zero or a true Condition_T with the integer 1,
   -- but instead uses conditional assignments to translate
   -- from Booleans to integers. Other tools and languages,
   -- for example ALF/SWEET, identify one-bit integers with
   -- Booleans, and then the encoding as a conditional
   -- assignment is an unnecessary complication.
   --
   -- If Ass is of the form "if Cond then 1 else 0" we
   -- return the Cond as the Value.
   --
   -- If Ass is of the form "if Cond then 0 else 1" we
   -- return "not Cond" as the Value.
   --
   -- If Ass is of any other form, we return Value as
   -- Unknown.


end Arithmetic.Logic;
