-- Calculator.Lexer (decl)
--
-- Lexical analysis of expressions from a text-i/o calculation engine.
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
-- $Revision: 1.7 $
-- $Date: 2015/10/24 19:36:47 $
--
-- $Log: calculator-lexer.ads,v $
-- Revision 1.7  2015/10/24 19:36:47  niklas
-- Moved to free licence.
--
-- Revision 1.6  2002-12-30 13:12:03  holsti
-- Upgraded to OpenToken_3_0_b.
--
-- Revision 1.5  2000/09/15 10:12:07  saarinen
-- Accepting '=' as a part of relational expression.
--
-- Revision 1.4  2000/06/29 14:10:25  holsti
-- Parser updated.
--
-- Revision 1.3  2000/06/22 10:17:57  saarinen
-- Added symbols colon, brackets and braces.
--
-- Revision 1.2  2000/06/17 16:51:20  holsti
-- End_Of_Text token added.
--
-- Revision 1.1  2000/06/16 20:16:51  holsti
-- First version. Using OpenToken 2.0.
--


with Ada.Strings.Maps.Constants;

with OpenToken.Token.Enumerated;
with OpenToken.Token.Enumerated.Analyzer;
with OpenToken.Recognizer.Character_Set;
with Opentoken.Recognizer.End_Of_File;
with OpenToken.Recognizer.Identifier;
with OpenToken.Recognizer.Integer;
with OpenToken.Recognizer.Separator;


package Calculator.Lexer is


   type Token_T is (
      End_Of_Text,
      Equal,
      Greater_Than,
      Greater_Equal,
      Less_Equal,
      Less_Than,
      Literal,
      Left_Brace,
      Left_Bracket,
      Left_Paren,
      Colon,
      Minus,
      Plus,
      Right_Brace,
      Right_Bracket,
      Right_Paren,
      Times,
      Variable,
      Whitespace);


   subtype Relation_T is Token_T range Equal .. Less_Than;
   --
   -- All the tokens for relational operators.


   package Calc_Token is new OpenToken.Token.Enumerated (Token_T);
   --
   -- The parent of the OpenToken subsystem, specalised to our
   -- set of tokens.


   package Tokenizer is new Calc_Token.Analyzer;
   --
   -- A token analyzer package for our set of tokens.


   use OpenToken.Recognizer;
   use type Ada.Strings.Maps.Character_Set;


   Ident_Start : constant Ada.Strings.Maps.Character_Set :=
      Ada.Strings.Maps.Constants.Letter_Set;
   --
   -- The characters that can start an identifier.

   Ident_Body : constant Ada.Strings.Maps.Character_Set :=
      Ada.Strings.Maps.Constants.Alphanumeric_Set
      or Ada.Strings.Maps.To_Set ("_");
    --
    -- The characters that can occur in an identifier as other than
    -- the first character.


   Syntax : constant Tokenizer.Syntax := (
      End_Of_Text    => Tokenizer.Get (End_Of_File.Get     ),
      Equal          => Tokenizer.Get (Separator.Get ("=" )),
      Greater_Than   => Tokenizer.Get (Separator.Get (">" )),
      Greater_Equal  => Tokenizer.Get (Separator.Get (">=")),
      Less_Equal     => Tokenizer.Get (Separator.Get ("<=")),
      Less_Than      => Tokenizer.Get (Separator.Get ("<" )),
      Literal        =>
         Tokenizer.Get (OpenToken.Recognizer.Integer.Get (
            Allow_Underscores  => False,
            Allow_Exponent     => False,
            Allow_Leading_Zero => True,
            Allow_Signs        => False)),
      Left_Brace     => Tokenizer.Get (Separator.Get ("{" )),
      Left_Bracket   => Tokenizer.Get (Separator.Get ("[" )),
      Left_Paren     => Tokenizer.Get (Separator.Get ("(" )),
      Colon          => Tokenizer.Get (Separator.Get (":" )),
      Minus          => Tokenizer.Get (Separator.Get ("-" )),
      Plus           => Tokenizer.Get (Separator.Get ("+" )),
      Right_Brace    => Tokenizer.Get (Separator.Get ("}" )),
      Right_Bracket  => Tokenizer.Get (Separator.Get ("]" )),
      Right_Paren    => Tokenizer.Get (Separator.Get (")" )),
      Times          => Tokenizer.Get (Separator.Get ("*" )),
      Variable       =>
         Tokenizer.Get (Identifier.Get (
            Start_Chars   => Ident_Start,
            Body_Chars    => Ident_Body,
            Has_Separator => False)),
      Whitespace     =>
        Tokenizer.Get (
            Character_Set.Get (Character_Set.Standard_Whitespace))
      );


end Calculator.Lexer;


