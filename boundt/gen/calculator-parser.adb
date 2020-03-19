-- Calculator.Parser (body)
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
-- $Revision: 1.15 $
-- $Date: 2015/10/24 20:05:46 $
--
-- $Log: calculator-parser.adb,v $
-- Revision 1.15  2015/10/24 20:05:46  niklas
-- Moved to free licence.
--
-- Revision 1.14  2008-04-26 19:19:44  niklas
-- BT-CH-0124: Joint loop counters and induction variables.
--
-- Revision 1.13  2005/10/20 11:28:29  niklas
-- BT-CH-0015.
--
-- Revision 1.12  2005/04/17 08:00:32  niklas
-- Changed parsing problems to report Fault instead of Error.
--
-- Revision 1.11  2005/02/19 20:34:55  niklas
-- BT-CH-0003.
--
-- Revision 1.10  2005/02/16 21:11:41  niklas
-- BT-CH-0002.
--
-- Revision 1.9  2004/05/01 20:26:13  niklas
-- First Tidorum version.
-- Updated for changes in parent package (no "use Arithmetic").
--
-- Revision 1.8  2000/11/29 09:27:01  holsti
-- Uses Arithmetic.Unbounded instead of local.
--
-- Revision 1.7  2000/11/09 13:38:47  saarinen
-- Added parsing of empty fluxes.
--
-- Revision 1.6  2000/10/06 13:32:38  saarinen
-- Bound handles symbolic constants and negative literals.
--
-- Revision 1.5  2000/09/15 10:12:04  saarinen
-- Accepting '=' as a part of relational expression.
--
-- Revision 1.4  2000/07/12 20:44:04  holsti
-- Added 'with Processor' (removed from Calculator).
--
-- Revision 1.3  2000/06/29 14:10:25  holsti
-- Parser updated.
--
-- Revision 1.2  2000/06/22 10:19:48  saarinen
-- Added functions Parse_Range and Parse_Boolean.
--
-- Revision 1.1  2000/06/17 17:13:33  holsti
-- First version.
--


with Arithmetic;
with Calculator.Lexer;
with Output;

with OpenToken.Text_Feeder.String;


package body Calculator.Parser is

   use Calculator.Lexer;

   use type Arithmetic.Value_T;

   subtype Value_T is Arithmetic.Value_T;


   --
   -- Representing a one-variable affine expression that
   -- may have an unknown constant part:
   --


   type Const_Part_T (Known : Boolean := False) is record
      case Known is
         when False => null;
         when True  => Value : Value_T;
      end case;
   end record;
   --
   -- The constant part of an affine expression, which may
   -- or may not be known.


   type Affine_T is record
      Coeff  : Value_T;
      Offset : Const_Part_T;
   end record;
   --
   -- The expression is Coeff * X + Offset, where X is the
   -- independent variable.


   function Non_Constant (Item : Affine_T) return Boolean
   --
   -- Whether the expression is non-constant.
   --
   is
   begin
      return Item.Coeff /= 0;
   end Non_Constant;


   function Value (Item : Const_Part_T) return Value_T
   --
   -- The value of constant, assumed Known.
   --
   is
   begin
      return Item.Value;
   end Value;


   --
   -- Arithmetic on Const_Part_T values:
   --


   function "+" (Left, Right : Const_Part_T) return Const_Part_T
   is
   begin
      if Left.Known and Right.Known then
         return (Known => True,
                 Value => Left.Value + Right.Value);
      else
         return (Known => False);
      end if;
   end "+";


   function "-" (Item : Const_Part_T) return Const_Part_T
   is
   begin
      if Item.Known then
         return (Known => True, Value => -Item.Value);
      else
         return (Known => False);
      end if;
   end "-";


   function "*" (Left, Right : Const_Part_T) return Const_Part_T
   is
   begin
      if Left.Known and Right.Known then
         return (Known => True,
                 Value => Left.Value * Right.Value);
      else
         return (Known => False);
      end if;
   end "*";



   --
   -- Mixed arithmetic:
   --


   function "*" (
      Left  : Value_T;
      Right : Const_Part_T)
   return Const_Part_T
   is
   begin
      if Right.Known then
         return (Known => True, Value => Left * Right.Value);
      elsif Left = 0 then
         return (Known => True, Value => 0);
      else
         return (Known => False);
      end if;
   end "*";



   --
   -- Arithmetic on Affine_T values:
   --


   function "+" (Left, Right : Affine_T) return Affine_T
   is
   begin
      return (Coeff  => Left.Coeff  + Right.Coeff,
              Offset => Left.Offset + Right.Offset);
   end "+";


   function "-" (Left : Affine_T) return Affine_T
   is
   begin
      return (Coeff  => -Left.Coeff,
              Offset => -Left.Offset);
   end "-";


   function "-" (Left, Right : Affine_T) return Affine_T
   is
   begin
      return Left + (-Right);
   end "-";


   function "*" (Left, Right : Affine_T) return Affine_T
   is
   begin

      if Non_Constant (Left) and Non_Constant (Right) then

         -- Product of two factors, both of which contain
         -- the variable. This is no longer an affine function.

         Output.Fault (
            Location => "Calculator.Parser.""*""",
            Text     => "Expression is super-linear.");

         raise Syntax_Error;

      elsif (Non_Constant (Left ) and not Right.Offset.Known)
         or (Non_Constant (Right) and not Left .Offset.Known)
      then

         -- Product of two factor, one of which contains the
         -- variable and the other has an unknown constant part.
         -- The coefficient of the variable in the product is
         -- not known.

         Output.Fault (
            Location => "Calculator.Parser.""*""",
            Text     => "Expression has an unknown coefficient.");

         raise Syntax_Error;

      else

         return (
            Offset => Left.Offset * Right.Offset,
            Coeff  => Value (Right.Coeff * Left .Offset
                           + Left .Coeff * Right.Offset) );

      end if;

   end "*";


   --
   -- Constructing Affine_T values from scratch:
   --


   function Literal (Value : Value_T) return Affine_T
   is
   --
   -- The Affine_T value that represents a literal value that
   -- occurs in the affine expression.
   --
   begin
      return (Coeff => 0, Offset => (Known => True, Value => Value));
   end Literal;


   function The_Variable return Affine_T
   --
   -- The Affine_T value that represents one occurrence of the
   -- variable in the affine expression.
   --
   is
   begin
      return (Coeff => 1, Offset => (Known => True, Value => 0));
   end The_Variable;


   function Symbolic_Constant return Affine_T
   --
   -- The Affine_T value that represents a symbolic constant
   -- in the affine expression.
   --
   is
   begin
      return (Coeff => 0, Offset => (Known => False));
   end Symbolic_Constant;



   --
   -- Parsing procedures:
   --


   procedure Error (
      From    : Tokenizer.Instance;
      Message : String)
   --
   -- Reports a syntax error (as a Fault) and raises Syntax_Error.
   -- The message should be a period-terminated sentence.
   --
   is
   begin

      Output.Fault (
         Location => "Calculator.Parser.Error",
         Text     =>
              Message
            & " Input token is '"
            & Tokenizer.Lexeme (From)
            & "'.");

      raise Syntax_Error;

   end Error;


   procedure Check_And_Skip (
      From     : in out Tokenizer.Instance;
      Token    : in     Token_T;
      Expected : in     String)
   --
   -- The next token must be Token, and is skipped.
   -- The Expected parameter is for the error message.
   --
   is
   begin

      if Tokenizer.ID (From) /= Token then

         Error (
            From    => From,
            Message => "Expected '" & Expected & "'.");

      end if;

      Tokenizer.Find_Next (From);

   end Check_And_Skip;


   procedure End_Of_File (From : in out Tokenizer.Instance)
   --
   -- The input string shall end here.
   --
   is
   begin

      if Tokenizer.ID (From) /= End_Of_Text then

         Error (
            From    => From,
            Message => "End of string expected.");

      end if;

   end End_Of_File;


   --
   -- The following parsing subprograms are logically functions
   -- that take a Tokenizer.Instance and return an Affine_T.
   -- They must be written as procedures since they need to
   -- update the Tokenizer.Instance.
   --
   -- On entry to any parsing subprogram, the current token is the
   -- first token of the corresponding sentential form.
   -- On exit from any parsing subprogram, the current token is the
   -- first token _after_ the last token of the corresponding
   -- sentential form.
   --
   -- Expression parsing procedures take a parameter "X : in String"
   -- that contains the identifier assigned by Omega for the
   -- independent variable in the affine expression.
   --


   procedure Expression (
      From  : in out Tokenizer.Instance;
      X     : in     String;
      Value :    out Affine_T);


   procedure Variable (
      From  : in out Tokenizer.Instance;
      X     : in     String;
      Value :    out Affine_T)
   --
   -- A variable (identifier) in the affine expression.
   -- This is either the distinguished independent variable X,
   -- or a symbolic constant.
   --
   is
   begin

      if Tokenizer.Lexeme (From) = X then
         Value := The_Variable;
      else
         Value := Symbolic_Constant;
      end if;

   end Variable;


   procedure Primary (
      From  : in out Tokenizer.Instance;
      X     : in     String;
      Value :    out Affine_T)
   --
   -- Primary -> Variable | Literal | '(' Expression ')'
   --
   is
      T : Token_T := Tokenizer.ID (From);
   begin

      case T is

         when Variable =>
            Variable (
               From  => From,
               X     => X,
               Value => Value);
            Tokenizer.Find_Next (From);

         when Literal  =>
            Value := Literal (
               Value_T'Value (
                  Tokenizer.Lexeme (From)));
            Tokenizer.Find_Next (From);

         when Left_Paren =>
            Tokenizer.Find_Next (From);
            Expression (From, X, Value);
            Check_And_Skip (From, Right_Paren, ")");

         when others =>

            Error (
               From    => From,
               Message =>
                  "Primary (variable, literal, subexpression) expected.");

      end case;

   end Primary;


   function First_Of_Primary (Token : Token_T) return Boolean
   --
   -- Whether this token can start a Primary.
   --
   is
   begin
      return Token = Variable
          or Token = Literal
          or Token = Left_Paren;
   end First_Of_Primary;


   procedure Term (
      From  : in out Tokenizer.Instance;
      X     : in     String;
      Value :    out Affine_T)
   --
   -- Term -> Primary | Term Primary | Term '*' Primary
   --
   -- The form "Term Primary" is an implied multiplication.
   -- For example, "2x" is the same as "2*x".
   --
   -- This grammar is larger than the language produced by the
   -- Omega Calculator, because this grammar allows any form of
   -- product, where the Omega Calculator only emits the form
   -- "literal times variable". A restriction in the arithmetic
   -- for Affine_T (operator "*") filters the result, and the
   -- wider grammar does no harm.
   --
   is
      Token : Token_T;
      Next_Factor : Affine_T;

   begin

      Primary (From => From, X => X, Value => Value);

      loop
         Token := Tokenizer.ID (From);

         if Token = Times then
            -- '*' Primary
            Tokenizer.Find_Next (From);
            Primary (From, X, Next_Factor);

         elsif First_Of_Primary (Token) then
            -- Primary (implied '*')
            Primary (From, X, Next_Factor);

         else
            -- End of term.
            exit;
         end if;

         Value := Value * Next_Factor;

      end loop;

   end Term;


   procedure Sum (
      From  : in out Tokenizer.Instance;
      X     : in     String;
      Value :    out Affine_T)
   --
   -- Sum -> Sign Term | Sum '+' Term | Sum '-' Term
   --
   is
      Token : Token_T;
      Next_Term : Affine_T;
   begin

      -- The first term, possibly with an unary sign:

      Token := Tokenizer.ID (From);

      if Token = Plus or Token = Minus then
         Tokenizer.Find_Next (From);
      end if;

      Term (From => From, X => X, Value => Next_Term);

      if Token = Minus then
         -- '-' Term
         Value := - Next_Term;
      else
         -- Term | '+' Term
         Value := Next_Term;
      end if;

      -- The rest of the terms:

      loop
         Token := Tokenizer.ID (From);

         if Token = Plus or Token = Minus then
            Tokenizer.Find_Next (From);
            Term (From, X, Next_Term);
         end if;

         case Token is
            when Plus   => Value := Value + Next_Term;
            when Minus  => Value := Value - Next_Term;
            when others => exit;
         end case;
      end loop;

   end Sum;


   procedure Expression (
      From  : in out Tokenizer.Instance;
      X     : in     String;
      Value :    out Affine_T)
   is
   begin
      Sum (From, X, Value);
   end Expression;



   --
   -- Parsing a one-variable, convex Omega set expression:
   --


   procedure Relation (
      From  : in out Tokenizer.Instance;
      Value :    out Relation_T)
   --
   -- One of the relational operators.
   --
   is
      Token : constant Token_T := Tokenizer.ID (From);
   begin

      if Token in Relation_T then

         Value := Token;
         Tokenizer.Find_Next (From);

      else

         Error (
            From    => From,
            Message => "Relational operator expected.");

      end if;

   end Relation;



   Reversed : constant array (Relation_T) of Relation_T :=
     (Greater_Than  => Less_Than,
      Greater_Equal => Less_Equal,
      Less_Equal    => Greater_Equal,
      Less_Than     => Greater_Than,
      Equal         => Equal);
   --
   -- The "reversed" relation.
   -- "X op Y" is equivalent to "Y Reversed(Op) X".


   procedure Relations (
      From  : in out Tokenizer.Instance;
      X     : in     String;
      Value :    out Storage.Bounds.Interval_T)
   --
   -- Order relations between affine expressions of the
   -- variable X, expressed in one of the following forms,
   -- where Op stands for one of "<", ">", "<=", ">=",
   -- and Left, Middle and Right are affine expressions
   -- of X:
   --
   -- Left Op  Right
   -- Left Op1 Middle Op2 Right
   --
   -- In the second form, the two Op's are not necessarily
   -- the same operator.
   --
   -- As a "semantic" restriction, hopefully obeyed by the
   -- Omega Calculator, at most one of the expressions
   -- surrounding an operator is allowed to contain X.
   --
   is
      Left   : Affine_T;
      Middle : Affine_T;
      Right  : Affine_T;
      Op1    : Token_T;
      Op2    : Token_T;
      -- The syntactical parts.


      procedure Update_Min (
         Expr  : in Affine_T;
         Limit : in Affine_T)
      --
      -- Updates the lower bound of Value using the
      -- relation "Expr >= Limit", where Expr is an
      -- affine, increasing function of X and Limit is
      -- a constant, hopefully of known value.
      --
      is
         use type Storage.Bounds.Limit_Kind_T;

         Min : Storage.Bounds.Limit_T renames Value.Min;

      begin

         if Limit.Offset.Known
         and Expr.Offset.Known then

            Min.Kind := Storage.Bounds.Finite;

            Min.Value := Value_T'Max (
               Min.Value,
               Storage.Bounds.Ceil (
                  Limit.Offset.Value - Expr.Offset.Value,
                  Expr.Coeff));

         end if;

      end Update_Min;


      procedure Update_Max (
         Expr  : in Affine_T;
         Limit : in Affine_T)
      --
      -- Updates the upper bound of Value using the
      -- relation "Expr <= Limit", where Expr is an
      -- affine, increasing function of X and Limit is
      -- a constant, hopefully of known value.
      --
      is
         use type Storage.Bounds.Limit_Kind_T;

         Max : Storage.Bounds.Limit_T renames Value.Max;

      begin

         if Limit.Offset.Known
         and Expr.Offset.Known then

            Max.Kind := Storage.Bounds.Finite;

            Max.Value := Value_T'Min (
               Max.Value,
               Storage.Bounds.Floor (
                  Limit.Offset.Value - Expr.Offset.Value,
                  Expr.Coeff));

         end if;

      end Update_Max;


      procedure Constrain_Expr (
         Expr  : in Affine_T;
         Op    : in Relation_T;
         Limit : in Affine_T)
      --
      -- Updates Value using the relation "Expr Op Limit"
      -- where Expr is an affine function of X, and
      -- Limit is hopefully a constant.
      -- An error is reported if Limit is not a constant.
      --
      is

         C : constant Value_T := Expr.Coeff;

         Pos_Expr : Affine_T;
         -- Expr if C is positive, -Expr if C is negative.

         Pos_Op : Relation_T;
         -- Op if C is positive, reversed Op if C is negative.

         Pos_Limit : Affine_T;
         -- Limit if C is positive, -Limit if C is negative.

         One : constant Affine_T := Literal (1);

      begin

         -- Ensure that the expression is increasing:

         if C > 0 then

            Pos_Expr  := Expr;
            Pos_Op    := Op;
            Pos_Limit := Limit;

         elsif C < 0 then

            Pos_Expr  := -Expr;
            Pos_Op    := Reversed(Op);
            Pos_Limit := -Limit;

         else

            Error (
               From    => From,
               Message =>
                  "Variable " & X & " occurs on neither side of '"
                  & Token_T'Image (Op) & "'.");

         end if;

         -- Check that the limit is constant:

         if Limit.Coeff /= 0 then
            Error (
               From    => From,
               Message =>
                  "Variable " & X & " occurs on both sides of '"
                  & Token_T'Image (Op) & "'.");
         end if;

         -- Update bounds according to the operator:

         case Op is

         when Greater_Than  => Update_Min (Pos_Expr, Pos_Limit + One);
         when Greater_Equal => Update_Min (Pos_Expr, Pos_Limit      );
         when Less_Equal    => Update_Max (Pos_Expr, Pos_Limit      );
         when Less_Than     => Update_Max (Pos_Expr, Pos_Limit - One);
         when Equal         =>
            Update_Min (Pos_Expr, Pos_Limit      );
            Update_Max (Pos_Expr, Pos_Limit      );

         end case;

      end Constrain_Expr;


      procedure Constrain (
         A  : in Affine_T;
         Op : in Relation_T;
         B  : in Affine_T)
      --
      -- Updates Value using the relation "A Op B"
      -- where one of A and B is an affine function
      -- of X, and the other is a constant.
      -- An error is reported otherwise.
      --
      is
      begin
         if A.Coeff /= 0 then
            Constrain_Expr (A, Op, B);
         else
            Constrain_Expr (B, Reversed(Op), A);
         end if;
      end Constrain;

   begin

      Value := Storage.Bounds.Universal_Interval;
      -- Initial value, to be updated.

      if Tokenizer.Lexeme (From) = "FALSE" then

         raise Calculator.Null_Set_Error;

      end if;

      Expression (From, X, Left);
      Relation   (From, Op1);
      Expression (From, X, Right);

      if Tokenizer.ID (From) not in Relation_T then

         -- Just Left Op Right

         Constrain (Left, Op1, Right);

      else

         -- Left Op1 Middle Op2 Right

         Middle := Right;
         Relation   (From, Op2);
         Expression (From, X, Right);

         Constrain (Left  , Op1, Middle);
         Constrain (Middle, Op2, Right);

      end if;

   end Relations;


   procedure Bound (
      From  : in out Tokenizer.Instance;
      Value :    out Storage.Bounds.Interval_T)
     --
     -- Bounds on a one-dimensional, convex Omega set.
     -- The set can be expressed in one of the following forms,
     -- where X is an identifier:
     --
     -- { [ literal ] }
     --
     --    The set contains just the literal number.
     --
     -- { [ - literal ] }
     --
     --    The set contains minus sign and the literal number.
     --
     -- { [ X ] }
     --
     --    The bounds of the set are not known.
     --
     -- { [ X ] : Relations }
     --
     --    The set contains the values represented by the variable
     --    X that satisfy the Relations (where X occurs as a
     --    variable).
     --
   is

      Lit_Value : Value_T;
      -- The value of the "literal".

   begin

      Check_And_Skip (From, Left_Brace, "{");

      Check_And_Skip (From, Left_Bracket, "[");

      case Tokenizer.ID (From) is

      when Literal =>

         Lit_Value := Value_T'Value (Tokenizer.Lexeme (From));

         Value := Storage.Bounds.Singleton (Lit_Value);

         Tokenizer.Find_Next (From);

         Check_And_Skip (From, Right_Bracket, "]");

      when Minus =>

         Tokenizer.Find_Next (From);

         if Tokenizer.ID (From) /= Literal then

            Error (From    => From,
                   Message => "Literal expected.");

         end if;

         Lit_Value := - Value_T'Value (Tokenizer.Lexeme (From));

         Value := Storage.Bounds.Singleton (Lit_Value);

         Tokenizer.Find_Next (From);

         Check_And_Skip (From, Right_Bracket, "]");

      when Variable =>

         declare
            X : constant String := Tokenizer.Lexeme (From);
         begin

            Tokenizer.Find_Next (From);

            Check_And_Skip (From, Right_Bracket, "]");

            if Tokenizer.ID (From) = Colon then

               Tokenizer.Find_Next (From);

               Relations (From, X, Value);

            else

               Value := Storage.Bounds.Universal_Interval;

            end if;
         end;

      when others =>

         Error (
            From    => From,
            Message => "Variable or literal expected.");

      end case;

      Check_And_Skip (From, Right_Brace, "}");

      End_Of_File (From);

   end Bound;


   function Bound (From : String)
   return Storage.Bounds.Interval_T
   is

      Feeder   : aliased OpenToken.Text_Feeder.String.Instance;
      Analyzer : Tokenizer.Instance;

      Value : Storage.Bounds.Interval_T;

   begin

      OpenToken.Text_Feeder.String.Set (
         Feeder => Feeder,
         Value  => From);

      Analyzer := Tokenizer.Initialize (
         Language_Syntax => Calculator.Lexer.Syntax,
         Feeder          => Feeder'Unchecked_Access);
      --
      -- Unchecked_Access is safe because the Feeder and the
      -- Analyzer have the same scope.

      Tokenizer.Find_Next (Analyzer);

      Bound (From => Analyzer, Value => Value);

      return Value;

   end Bound;


   function Boolean_Literal (From : String)
   return Boolean
   is

      Feeder   : aliased OpenToken.Text_Feeder.String.Instance;
      Analyzer : Tokenizer.Instance;
      Value    : Boolean;

   begin

      OpenToken.Text_Feeder.String.Set (
         Feeder => Feeder,
         Value  => From);

      Analyzer := Tokenizer.Initialize (
         Language_Syntax => Calculator.Lexer.Syntax,
         Feeder          => Feeder'Unchecked_Access);
      --
      -- Unchecked_Access is safe because the Feeder and the
      -- Analyzer have the same scope.

      Tokenizer.Find_Next (Analyzer);

      if Tokenizer.ID (Analyzer) /= Variable then

         Error (
            From    => Analyzer,
            Message => "True or False expected.");

      end if;

      -- Interpret the identifier:

      begin

         Value := Boolean'Value (Tokenizer.Lexeme (Analyzer));

      exception

      when Constraint_Error =>

         Error (
            From    => Analyzer,
            Message => "True or False expected.");

      end;

      -- Check that there is no more input:

      Tokenizer.Find_Next (Analyzer);
      End_Of_File (Analyzer);

      -- All OK:

      return Value;

   end Boolean_Literal;


end Calculator.Parser;
