-- HRT.Lexer (decl)
--
-- Lexical analysis of the TPOF file.
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
-- $Date: 2015/10/24 19:36:49 $
--
-- $Log: hrt-lexer.ads,v $
-- Revision 1.4  2015/10/24 19:36:49  niklas
-- Moved to free licence.
--
-- Revision 1.3  2009-01-18 08:12:36  niklas
-- Removed unused context clauses.
--
-- Revision 1.2  2002/12/30 13:12:03  holsti
-- Upgraded to OpenToken_3_0_b.
--
-- Revision 1.1  2001/03/14 14:49:15  ville
-- HRT rework second phase: skeleton package splitted
--
-- Revision 1.2  2000/08/23 09:01:30  saarinen
-- Changed the order of tokens.
--
-- Revision 1.1  2000/08/17 10:00:01  holsti
-- First version.
--

with Ada.Strings.Maps;
with Ada.Strings.Maps.Constants;

with OpenToken.Token.Enumerated;
with OpenToken.Token.Enumerated.Analyzer;
with OpenToken.Recognizer.Character_Set;
with Opentoken.Recognizer.End_Of_File;
with OpenToken.Recognizer.Identifier;
with Opentoken.Recognizer.Keyword;
with Opentoken.Recognizer.Line_Comment;


package HRT.Lexer is


   type Token_T is
      (Barrier_Tok,
       End_Of_Text,
       End_Tok,
       Entry_Tok,
       Program_Tok,
       Protected_Tok,
       Resource_Tok,
       Root_Tok,
       Thread_Tok,
       Type_Tok,
       Cyclic_Tok,
       Sporadic_Tok,
       Interrupt_Tok,
       Synchro_Tok,
       Comment_Tok,
       Whitespace,
       Name_Tok);



   subtype Thread_Type_T is Token_T range Cyclic_Tok .. Interrupt_Tok;
   --
   -- All the tokens for thread type.


   package Hrt_Token is new OpenToken.Token.Enumerated (Token_T);
   --
   -- The parent of the OpenToken subsystem, specalised to our
   -- set of tokens.


   package Tokenizer is new Hrt_Token.Analyzer;
   --
   -- A token analyzer package for our set of tokens.

   use type Ada.Strings.Maps.Character_Set;

   Name_Set : constant Ada.Strings.Maps.Character_Set :=
     Ada.Strings.Maps.Constants.Graphic_Set
     and not Ada.Strings.Maps.To_Set(" ");

   use OpenToken.Recognizer;



   Syntax : constant Tokenizer.Syntax :=
     (Barrier_Tok    => Tokenizer.Get (Keyword.Get("barrier")),
      End_Of_Text    => Tokenizer.Get (End_Of_File.Get       ),
      End_Tok        => Tokenizer.Get (Keyword.Get("end")    ),
      Entry_Tok      => Tokenizer.Get (Keyword.Get("entry")  ),
      Program_Tok    => Tokenizer.Get (Keyword.Get("program")),
      Protected_Tok  => Tokenizer.Get (Keyword.Get("protected")),
      Resource_Tok   => Tokenizer.Get (Keyword.Get("resource")),
      Root_Tok       => Tokenizer.Get (Keyword.Get("root")),
      Thread_Tok     => Tokenizer.Get (Keyword.Get("thread")),
      Type_Tok       => Tokenizer.Get (Keyword.Get("type")),
      Cyclic_Tok     => Tokenizer.Get (Keyword.Get("cyclic")),
      Sporadic_Tok   => Tokenizer.Get (Keyword.Get("sporadic")),
      Interrupt_Tok  => Tokenizer.Get (Keyword.Get("interrupt_sporadic")),
      Synchro_Tok    => Tokenizer.Get (Keyword.Get("synchro")),
      Comment_Tok    => Tokenizer.Get (Line_Comment.Get ("--", True)),
      Whitespace     => Tokenizer.Get
      (Character_Set.Get (Character_Set.Standard_Whitespace)),
      Name_Tok       =>
        Tokenizer.Get (Identifier.Get
                       (Start_Chars   => Name_Set,
                        Body_Chars    => Name_Set,
                        Has_Separator => False))
      );


end HRT.Lexer;


