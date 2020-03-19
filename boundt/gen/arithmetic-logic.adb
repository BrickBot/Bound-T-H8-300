-- Arithmetic.Logic (body)
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
-- $Revision: 1.5 $
-- $Date: 2015/10/24 20:05:45 $
--
-- $Log: arithmetic-logic.adb,v $
-- Revision 1.5  2015/10/24 20:05:45  niklas
-- Moved to free licence.
--
-- Revision 1.4  2011-09-06 17:42:38  niklas
-- Added the function Complementary and the procedure
-- Decode_Boolean_Bit for use in ALF export.
--
-- Revision 1.3  2009-11-27 11:28:06  niklas
-- BT-CH-0184: Bit-widths, Word_T, failed modular analysis.
--
-- Revision 1.2  2007/07/21 18:18:40  niklas
-- BT-CH-0064. Support for AVR/IAR switch-handler analysis.
--
-- Revision 1.1  2006/05/06 06:59:19  niklas
-- BT-CH-0021.
--


with Output;


package body Arithmetic.Logic is


   --
   ---   Cell values implied by a condition
   --


   function Merge (Left, Right : Storage.Cell_Value_List_T)
   return Storage.Cell_Value_List_T
   --
   -- Merges the two lists of cell values into one list, combining
   -- any duplicate entries for the same cell.
   --
   -- Propagates False_Premise if the same cell is assigned two or
   -- more different values.
   --
   is
      use type Storage.Cell_T;

      Total : Storage.Cell_Value_List_T (1 .. Left'Length + Right'Length);
      Last  : Natural := Left'Length;
      -- The result will be Total(1 .. Last).
      -- The Last value foresees that Total is initalized to Left.

      Cell : Cell_T;
      -- A cell from the Right list.

      Match : Boolean;
      -- The Cell matches a Left entry (or another Right entry).

   begin

      Total(1 .. Last) := Left;

      for R in Right'Range loop

         Cell  := Right(R).Cell;
         Match := False;

         for T in 1 .. Last loop

            Match := Total(T).Cell = Cell;

            if Match then

               if Total(T).Value /= Right(R).Value then
                  -- Absurdum.

                  raise False_Premise;

               end if;

               exit;

            end if;

         end loop;

         if not Match then
            -- A cell we have not seen before.

            Last := Last + 1;
            Total(Last) := Right(R);

         end if;

      end loop;

      return Total(1 .. Last);

   end Merge;


   function Cell_Values_Implied (By : Condition_T)
   return Storage.Cell_Value_List_T
   is

      procedure Report_Non_Logical
      is
      begin

         Output.Fault (
            Location => "Arithmetic.Logic.Cell_Values (Condition)",
            Text     =>
                 "Non-logical operator in "
               & Image (By));

      end Report_Non_Logical;


   begin  -- Cell_Values_Implied

      if By = Always then

         return Storage.No_Cell_Values;

      elsif By = Never then

         raise False_Premise;

      else

         case By.Kind is

         when Unary_Kind =>

            case By.Unary is

            when Notx =>
               -- We do not strive to understand how these logical
               -- operators could imply single values for cells.

               return Storage.No_Cell_Values;

            when EqZero =>
               -- Implies a cell is zero.

               return (1 => (Cell => By.Expr.Cell, Value => 0));

            when EqOne =>
               -- Implies a cell is one (1).

               return (1 => (Cell => By.Expr.Cell, Value => 1));

            when others =>

               Report_Non_Logical;

               return Storage.No_Cell_Values;

            end case;

         when Binary_Kind =>

            case By.Binary is

            when Relation_Op_T =>

               if By.Binary = Eq then
                  -- A possible cell-equality

                  if By.L_Expr.Kind = Cell and By.R_Expr.Kind = Const then

                     return (1 => (
                        Cell  => By.L_Expr.Cell,
                        Value => By.R_Expr.Value));

                  elsif By.L_Expr.Kind = Const and By.R_Expr.Kind = Cell then

                     return (1 => (
                        Cell  => By.R_Expr.Cell,
                        Value => By.L_Expr.Value));

                  else
                     -- The comparands are expressions, not a cell and a const.
                     -- We do not strive to understand how such comparisons
                     -- could imply single values for cells.

                     return Storage.No_Cell_Values;

                  end if;

               else
                  -- We do not strive to understand how other
                  -- relations could imply single values for cells.

                  return Storage.No_Cell_Values;

               end if;

            when Andx =>

               return Merge (
                  Cell_Values_Implied (By => By.L_Expr),
                  Cell_Values_Implied (By => By.R_Expr));
               -- May propagate False_Premise.

            when Orx =>
               -- We do not strive to understand how these logical
               -- operators could imply single values for cells.

               return Storage.No_Cell_Values;

            when others =>

               Report_Non_Logical;

               return Storage.No_Cell_Values;

            end case;

         when others =>

            Report_Non_Logical;

            return Storage.No_Cell_Values;

         end case;

      end if;

   end Cell_Values_Implied;


   --
   ---   Relations of two conditions
   --


   function Complementary (Cond1, Cond2 : Condition_T)
   return Boolean
   is
      use type Width_T;
   begin

      -- As already said, this is an incomplete test that checks
      -- for some syntactically recognisable cases of complementarity
      -- but is not logically complete. The following cases of
      -- complementarity are currently detected as such:
      --
      -- > One condition is EqZero, the other EqOne, with the same
      --   1-bit operand expression.
      --
      -- > One condition is the logical negation of the other.
      --
      -- > Both conditions are relational comparisons with the same
      --   operands but complementary relational operators.
      --
      -- > One condition is an "or", the other an "and", and the operands
      --   of the first condition are complementary with the operands of
      --   the other condition.

      -- Cond1/Cond2 are Eq0/Eq1 of the same bit ?

      if       (Cond1.Kind = Unary_Kind and Cond2.Kind = Unary_Kind)
      and then (Cond1.Expr = Cond2.Expr
      and       Cond1.Unary in Unary_Eq_Bit_T)
      and then (Cond2.Unary = Opposite_Bit_Value(Cond1.Unary)
      and       Cond1.Expr.Width = 1)
      then
         -- One Cond is EqZero, the other EqOne, of the same
         -- one-bit operand.

         return True;

      end if;

      -- Cond1 = not Cond2, or Cond2 = not Cond1 ?

      if (Op_Is (Notx, Cond1) and then Cond1.Expr = Cond2)
      or (Op_Is (Notx, Cond2) and then Cond2.Expr = Cond1)
      then
         -- One Cond is the logical negation of the other.

         return True;

      end if;

      -- Cond1 and Cond2 compare the same two operands
      -- with complementary relational operators ?

      if       (Cond1.Kind = Binary_Kind
      and       Cond2.Kind = Binary_Kind)
      and then  Cond1.Binary in Magnitude_Rel_T
      and then  Cond2.Binary = Opposite_Rel(Cond1.Binary)
      and then (Cond1.L_Expr = Cond2.L_Expr
      and       Cond1.R_Expr = Cond2.R_Expr)
      then
         -- The Conds are complementary relational comparisons
         -- of the same two operands.

         return True;

      end if;

      -- Cond1 and Cond2 are "or" and "and" (or vice versa) of
      -- complementary conditions ?

      if ((Op_Is (Andx, Cond1) and Op_Is (Orx , Cond2))
      or  (Op_Is (Orx , Cond1) and Op_Is (Andx, Cond2)))
      and then (
           Complementary (Cond1.L_Expr, Cond2.L_Expr)
      and  Complementary (Cond1.R_Expr, Cond2.R_Expr))
      then
         -- By DeMorgan's law Cond1 and Cond2 are complementary.

         return True;

       end if; 

      -- And now we are out of ideas.

      return False;

   end Complementary;


   --
   ---   Simplifications of assignments
   --


   procedure Decode_Boolean_Bit (
      Ass   : in     Cond_Assignment_T;
      Value :    out Condition_T)

   is
   begin

      if Ass.Value1 = Word_T'(1) and Ass.Value2 = Word_T'(0) then

         Value := Ass.Cond;

      elsif Ass.Value1 = Word_T'(0) and Ass.Value2 = Word_T'(1) then

         Value := not Ass.Cond;

      else

         Value := Unknown;

      end if;

   end Decode_Boolean_Bit;


end Arithmetic.Logic;
