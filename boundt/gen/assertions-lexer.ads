-- Assertion_Lexer (decl)
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
-- $Revision: 1.29 $
-- $Date: 2015/10/24 19:36:46 $
--
-- $Log: assertions-lexer.ads,v $
-- Revision 1.29  2015/10/24 19:36:46  niklas
-- Moved to free licence.
--
-- Revision 1.28  2013-02-12 08:47:19  niklas
-- BT-CH-0245: Global volatile cells and "volatile" assertions.
--
-- Revision 1.27  2010-01-30 21:13:29  niklas
-- BT-CH-0216: Subprograms have a Return_Method_T attribute.
--
-- Revision 1.26  2009-12-17 14:05:53  niklas
-- BT-CH-0197: Assertions on instruction roles.
--
-- Revision 1.25  2009-03-20 18:19:28  niklas
-- BT-CH-0164: Assertion context identified by source-line number.
--
-- Revision 1.24  2009/01/18 08:11:17  niklas
-- Removed unused context clause.
--
-- Revision 1.23  2008/09/24 08:38:50  niklas
-- BT-CH-0146: Assertions on "loop starts <bound> times".
-- BT-CH-0146: Loop-repeat assertions set both lower and upper bound.
-- BT-CH-0146: Report locations of contradictory "count" assertions.
-- BT-CH-0146: Contradictory "count" assertions imply infeasibility.
--
-- Revision 1.22  2008/09/19 10:34:13  niklas
-- BT-CH-0144: Assertion "populations" work again, and more.
--
-- Revision 1.21  2008/07/14 19:16:54  niklas
-- BT-CH-0135: Assertions on "instructions".
--
-- Revision 1.20  2007/12/17 13:54:33  niklas
-- BT-CH-0098: Assertions on stack usage and final stack height, etc.
--
-- Revision 1.19  2007/03/22 12:58:08  niklas
-- Replaced Plus_Tok ('+') with Offset_Tok ("offset") because Plus_Tok
-- was ambiguous with Integer_Tok which allows signs.
-- Address-offsets are now written "offset <string>", not "+ <string>".
--
-- Revision 1.18  2006/11/26 22:07:23  niklas
-- BT-CH-0039.
--
-- Revision 1.17  2006/10/30 23:09:05  niklas
-- BT-CH-0033.
--
-- Revision 1.16  2006/10/28 19:52:14  niklas
-- BT-CH-0031.
--
-- Revision 1.15  2006/08/22 12:58:53  niklas
-- Added Invalid_Tok for better error messages about invalid text.
-- Removed the Analyzer instance in favour of local instances per
-- assertion file, giving correct line-numbers per file.
--
-- Revision 1.14  2006/05/29 11:22:32  niklas
-- BT-CH-0023.
--
-- Revision 1.13  2006/05/27 21:31:45  niklas
-- Added Equal_Tok for BT-CH-0020.
--
-- Revision 1.12  2005/09/03 11:50:27  niklas
-- BT-CH-0006.
--
-- Revision 1.11  2005/02/04 20:53:07  niklas
-- Added No_Tok and Return_Tok.
--
-- Revision 1.10  2003/03/11 14:18:43  holsti
-- Added plural and singular forms of many keywords. We can now
-- say, for example, "all loops repeat 1 time" instead of "all
-- loop repeats 1 times".
-- Added keywords to let us avoid long keywords with embedded
-- underlines, for example "loop that" instead of "loop_that".
-- Normalized some formatting and type names.
--
-- Revision 1.9  2003/02/17 16:26:20  holsti
-- Added "all" token to specify populations of loops or calls.
--
-- Revision 1.8  2003/02/17 14:22:16  holsti
-- Added the loop-property "executes <code address>" as one more
-- way to identify loops in assertions.
--
-- Revision 1.7  2002/12/30 13:12:03  holsti
-- Upgraded to OpenToken_3_0_b.
--
-- Revision 1.6  2001/02/15 14:17:37  ville
-- Analysis mode assertions enabled
--
-- Revision 1.5  2000/12/28 12:39:55  holsti
-- Added subtype Relation_Token_T (and reordered token enumeration
-- to make these elements contiguous).
-- Allow signs in integer literals.
--
-- Revision 1.4  2000/11/25 16:24:10  holsti
-- Placed each with-clause on its own line.
-- Added use-clauses to shorten the Syntax lines.
--
-- Revision 1.3  2000/11/23 09:07:13  langback
-- Removed register token from syntax definition.
--
-- Revision 1.2  2000/11/22 12:37:02  langback
-- Added Delimiter and Within tokens.
--
-- Revision 1.1  2000/10/30 13:00:10  langback
-- Renamed assertion_lexer.ads to assertion-lexer.ads. The File token is
-- replaced by a Scope token which is associated with the "in" keyword.
--
-- Revision 1.4  2000/10/10 12:28:02  langback
-- Added "Property" token.
--
-- Revision 1.3  2000/06/20 11:17:05  langback
-- Added the possibility to write Ada-like line comments into assertions files.
--
-- Revision 1.2  2000/06/19 10:48:42  langback
-- Version updated for Opentoken v. 2.0
--
-- Revision 1.1  2000/06/14 12:19:42  langback
-- First commit'ed version of the assertions lexer instantiation
--

with Opentoken.Token.Enumerated;
with Opentoken.Token.Enumerated.Analyzer;
with Opentoken.Recognizer.Keyword;
with Opentoken.Recognizer.Separator;
with Opentoken.Recognizer.String;
with Opentoken.Recognizer.Integer;
with Opentoken.Recognizer.Based_Integer_Ada_Style;
with Opentoken.Recognizer.Character_Set;
with Opentoken.Recognizer.Graphic_Character;
with Opentoken.Recognizer.Line_Comment;
with Opentoken.Recognizer.End_Of_File;
with Opentoken.Recognizer.Nothing;


package Assertions.Lexer is

----------------------------------------------------------------------
-- A lexer analyser instantiation for the Bound-T Assertions language.
----------------------------------------------------------------------

  type Token_T is (
     A_Tok,
     An_Tok,
     Address_Tok,
     After_Tok,
     All_Tok,
     And_Tok,
     At_Tok,
     Based_Integer_Tok,
     Before_Tok,
     Cycle_Tok,
     Cycles_Tok,
     Call_Tok,
     Calls_Tok,
     Calling_Tok,
     Call_To_Tok,            -- deprecated
     Comma_Tok,
     Contain_Tok,
     Contains_Tok,
     Containing_Tok,
     Define_Tok,
     Defines_Tok,
     Defining_Tok,
     Delimiter_Tok,
     Do_Tok,
     Does_Tok,
     Double_Dot_Tok,
     Dynamic_Tok,
     End_Tok,
     End_Call_Tok,           -- deprecated
     End_Loop_Tok,           -- deprecated
     Enough_Tok,
     Exactly_Tok,
     Execute_Tok,
     Executes_Tok,
     Executing_Tok,

     -- subtype Relation_Tok_T:

     Equal_Tok,
     Greater_Equal_Tok,
     Greater_Than_Tok,
     Less_Equal_Tok,
     Less_Than_Tok,

     -- end Relation_Tok_T.

     Arithmetic_Tok,
     No_Arithmetic_Tok,
     File_Tok,
     Final_Tok,
     For_Tok,
     Hide_Tok,
     Instruction_Tok,
     Integrate_Tok,
     In_Tok,
     Invariant_Tok,
     Integer_Tok,
     Invalid_Tok,            -- for invalid text
     Are_Tok,
     Is_Tok,
     Is_In_Tok,              -- deprecated
     Is_Labelled_Tok,        -- deprecated
     Labeled_Tok,
     Labelled_Tok,
     Left_Parenthesis_Tok,
     Line_Tok,
     Loop_Tok,
     Loops_Tok,
     Loop_That_Tok,          -- deprecated
     Marked_Tok,
     Marker_Tok,

     -- subtype Nix_Token_T:

     No_Tok,
     Not_Tok,

     -- end Nix_Token_T.

     -- subtype Normal_Token_T:

     Normal_Tok,
     Normally_Tok,

     -- end Normal_Token_T.

     Offset_Tok,
     Omit_Tok,
     On_Tok,
     Or_Tok,
     Performs_Tok,
     Property_Tok,
     Range_Tok,
     Repeat_Tok,
     Repeats_Tok,
     Return_Tok,
     Returns_Tok,
     Right_Parenthesis_Tok,
     Semicolon_Tok,
     Span_Tok,
     Spans_Tok,
     Spanning_Tok,
     Stack_Tok,
     Start_Tok,
     Starts_Tok,
     Subprogram_Tok,
     That_Tok,
     Time_Tok,
     Times_Tok,
     To_Tok,
     Unused_Tok,
     Usage_Tok,
     Used_Tok,
     Use_Tok,
     Uses_Tok,
     Using_Tok,
     Variable_Tok,
     Volatile_Tok,
     String_Tok,
     Character_Tok,
     Comment_Tok,
     End_Of_File_Tok,
     Whitespace_Tok,
     Within_Tok);
   --
   -- The lexical tokens in the assertion language.
   --
   -- Note that several subtypes are defined below, so take care to
   -- insert new literals in places that preserve the meaning of the
   -- subtype ranges.


   subtype Indefinite_Article_Tok_T is Token_T range A_Tok .. An_Tok;
   --
   -- The indefinite articles, "a" and "an".


   subtype Relation_Token_T is Token_T range Equal_Tok .. Less_Than_Tok;
   --
   -- The relational operators.


   subtype Nix_Token_T is Token_T range No_Tok .. Not_Tok;
   --
   -- Two similar forms of denial.


   subtype Normal_Token_T is Token_T range Normal_Tok .. Normally_Tok;
   --
   -- Two similar forms of normality.


   -- Subtypes for singular/plural/verb-form token pairs.
   --
   -- The subtypes are named according to "singular" usage.

   subtype Calls_Tok_T    is Token_T range Call_Tok    .. Calling_Tok;

   subtype Cycle_Tok_T    is Token_T range Cycle_Tok   .. Cycles_Tok;

   subtype Contains_Tok_T is Token_T range Contain_Tok .. Containing_Tok;

   subtype Defines_Tok_T  is Token_T range Define_Tok  .. Defining_Tok;

   subtype Does_Tok_T     is Token_T range Do_Tok      .. Does_Tok;

   subtype Executes_Tok_T is Token_T range Execute_Tok .. Executing_Tok;

   subtype Is_Tok_T       is Token_T range Are_Tok     .. Is_Tok;

   subtype Labelled_Tok_T is Token_T range Labeled_Tok .. Labelled_Tok;

   subtype Loop_Tok_T     is Token_T range Loop_Tok    .. Loops_Tok;

   subtype Repeats_Tok_T  is Token_T range Repeat_Tok  .. Repeats_Tok;

   subtype Returns_Tok_T  is Token_T range Return_Tok  .. Returns_Tok;

   subtype Spans_Tok_T    is Token_T range Span_Tok    .. Spanning_Tok;

   subtype Starts_Tok_T   is Token_T range Start_Tok   .. Starts_Tok;

   subtype Time_Tok_T     is Token_T range Time_Tok    .. Times_Tok;

   subtype Uses_Tok_T     is Token_T range Use_Tok     .. Using_Tok;



   -- OpenToken instantiation:


   package Master_Token is new OpenToken.Token.Enumerated (Token_T);
   package Tokenizer    is new Master_Token.Analyzer;

   use Tokenizer;
   use Opentoken.Recognizer;
   -- The above "use" clauses keep the lines in Syntax (below) reasonably
   -- short, except for components that alias Standard names, such as
   -- Integer and String. Tch tch.


   Syntax : constant Tokenizer.Syntax := (
      A_Tok                 => Get (Keyword.Get ("a")),
      An_Tok                => Get (Keyword.Get ("an")),
      Address_Tok           => Get (Keyword.Get ("address")),
      After_Tok             => Get (Keyword.Get ("after")),
      All_Tok               => Get (Keyword.Get ("all")),
      And_Tok               => Get (Keyword.Get ("and")),
      At_Tok                => Get (Keyword.Get ("at")),
      Based_Integer_Tok     => Get (Based_Integer_Ada_Style.Get),
      Before_Tok            => Get (Keyword.Get ("before")),
      Cycle_Tok             => Get (Keyword.Get ("cycle")),
      Cycles_Tok            => Get (Keyword.Get ("cycles")),
      Call_To_Tok           => Get (Keyword.Get ("call_to")),
      Call_Tok              => Get (Keyword.Get ("call")),
      Calls_Tok             => Get (Keyword.Get ("calls")),
      Calling_Tok           => Get (Keyword.Get ("calling")),
      Comma_Tok             => Get (Separator.Get (",")),
      Contain_Tok           => Get (Keyword.Get ("contain")),
      Contains_Tok          => Get (Keyword.Get ("contains")),
      Containing_Tok        => Get (Keyword.Get ("containing")),
      Define_Tok            => Get (Keyword.Get ("define")),
      Defines_Tok           => Get (Keyword.Get ("defines")),
      Defining_Tok          => Get (Keyword.Get ("defining")),
      Delimiter_Tok         => Get (Keyword.Get ("delimiter")),
      Do_Tok                => Get (Keyword.Get ("do")),
      Does_Tok              => Get (Keyword.Get ("does")),
      Double_Dot_Tok        => Get (Separator.Get ("..")),
      Dynamic_Tok           => Get (Keyword.Get ("dynamic")),
      End_Tok               => Get (Keyword.Get ("end")),
      End_Call_Tok          => Get (Keyword.Get ("end_call")),
      End_Loop_Tok          => Get (Keyword.Get ("end_loop")),
      Enough_Tok            => Get (Keyword.Get ("enough")),
      Execute_Tok           => Get (Keyword.Get ("execute")),
      Executes_Tok          => Get (Keyword.Get ("executes")),
      Executing_Tok         => Get (Keyword.Get ("executing")),
      Exactly_Tok           => Get (Keyword.Get ("exactly")),
      Equal_Tok             => Get (Separator.Get ("=")),
      Greater_Equal_Tok     => Get (Separator.Get (">=")),
      Greater_Than_Tok      => Get (Separator.Get (">")),
      Less_Equal_Tok        => Get (Separator.Get ("<=")),
      Less_Than_Tok         => Get (Separator.Get ("<")),
      Arithmetic_Tok        => Get (Keyword.Get ("arithmetic")),
      No_Arithmetic_Tok     => Get (Keyword.Get ("no_arithmetic")),
      File_Tok              => Get (Keyword.Get ("file")),
      Final_Tok             => Get (Keyword.Get ("final")),
      For_Tok               => Get (Keyword.Get ("for")),
      Hide_Tok              => Get (Keyword.Get ("hide")),
      Instruction_Tok       => Get (Keyword.Get ("instruction")),
      Integrate_Tok         => Get (Keyword.Get ("integrate")),
      Invariant_Tok         => Get (Keyword.Get ("invariant")),
      Integer_Tok           => Get (Opentoken.Recognizer.Integer.Get (
                                      Allow_Signs => True)),
      In_Tok                => Get (Keyword.Get ("in")),
      Are_Tok               => Get (Keyword.Get ("are")),
      Is_Tok                => Get (Keyword.Get ("is")),
      Is_In_Tok             => Get (Keyword.Get ("is_in")),
      Is_Labelled_Tok       => Get (Keyword.Get ("is_labelled")),
      Labeled_Tok           => Get (Keyword.Get ("labeled")),
      Labelled_Tok          => Get (Keyword.Get ("labelled")),
      Left_Parenthesis_Tok  => Get (Separator.Get ("(")),
      Line_Tok              => Get (Keyword.Get ("line")),
      Loop_Tok              => Get (Keyword.Get ("loop")),
      Loops_Tok             => Get (Keyword.Get ("loops")),
      Loop_That_Tok         => Get (Keyword.Get ("loop_that")),
      Marked_Tok            => Get (Keyword.Get ("marked")),
      Marker_Tok            => Get (Keyword.Get ("marker")),
      No_Tok                => Get (Keyword.Get ("no")),
      Not_Tok               => Get (Keyword.Get ("not")),
      Normal_Tok            => Get (Keyword.Get ("normal")),
      Normally_Tok          => Get (Keyword.Get ("normally")),
      Offset_Tok            => Get (Keyword.Get ("offset")),
      Omit_Tok              => Get (Keyword.Get ("omit")),
      On_Tok                => Get (Keyword.Get ("on")),
      Or_Tok                => Get (Keyword.Get ("or")),
      Performs_Tok          => Get (Keyword.Get ("performs")),
      Property_Tok          => Get (Keyword.Get ("property")),
      Range_Tok             => Get (Keyword.Get ("range")),
      Repeat_Tok            => Get (Keyword.Get ("repeat")),
      Repeats_Tok           => Get (Keyword.Get ("repeats")),
      Return_Tok            => Get (Keyword.Get ("return")),
      Returns_Tok           => Get (Keyword.Get ("returns")),
      Right_Parenthesis_Tok => Get (Separator.Get (")")),
      Semicolon_Tok         => Get (Separator.Get (";")),
      Span_Tok              => Get (Keyword.Get ("span")),
      Spans_Tok             => Get (Keyword.Get ("spans")),
      Spanning_Tok          => Get (Keyword.Get ("spanning")),
      Stack_Tok             => Get (Keyword.Get ("stack")),
      Start_Tok             => Get (Keyword.Get ("start")),
      Starts_Tok            => Get (Keyword.Get ("starts")),
      Subprogram_Tok        => Get (Keyword.Get ("subprogram")),
      That_Tok              => Get (Keyword.Get ("that")),
      Time_Tok              => Get (Keyword.Get ("time")),
      Times_Tok             => Get (Keyword.Get ("times")),
      To_Tok                => Get (Keyword.Get ("to")),
      Usage_Tok             => Get (Keyword.Get ("usage")),
      Unused_Tok            => Get (Keyword.Get ("unused")),
      Used_Tok              => Get (Keyword.Get ("used")),
      Use_Tok               => Get (Keyword.Get ("use")),
      Uses_Tok              => Get (Keyword.Get ("uses")),
      Using_Tok             => Get (Keyword.Get ("using")),
      Variable_Tok          => Get (Keyword.Get ("variable")),
      Volatile_Tok          => Get (Keyword.Get ("volatile")),
      Within_Tok            => Get (Keyword.Get ("within")),
      String_Tok            => Get (Opentoken.Recognizer.String.Get),
      Character_Tok         => Get (Graphic_Character.Get),
      Comment_Tok           => Get (Line_Comment.Get ("--")),
      End_Of_File_Tok       => Get (End_Of_File.Get),
      Whitespace_Tok        => Get (Character_Set.Get (
                                      Character_Set.Standard_Whitespace)),
      Invalid_Tok           => Get (Opentoken.Recognizer.Nothing.Get));


end Assertions.Lexer;
