-- Assertions.Parser (body)
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
-- $Revision: 1.42 $
-- $Date: 2015/10/24 20:05:45 $
--
-- $Log: assertions-parser.adb,v $
-- Revision 1.42  2015/10/24 20:05:45  niklas
-- Moved to free licence.
--
-- Revision 1.41  2014/06/11 12:57:44  niklas
-- Updated to use Code_Address_Default instead of 'First.
--
-- Revision 1.40  2013-02-19 09:13:26  niklas
-- BT-CH-0245 clean-up. Only descriptions changed.
--
-- Revision 1.39  2013-02-12 08:47:19  niklas
-- BT-CH-0245: Global volatile cells and "volatile" assertions.
--
-- Revision 1.38  2012-02-04 09:55:23  niklas
-- BT-CH-0227: Avoid GNAT bug re Unbounded_String in discr. record.
--
-- Revision 1.37  2010-01-30 21:13:29  niklas
-- BT-CH-0216: Subprograms have a Return_Method_T attribute.
--
-- Revision 1.36  2009-12-21 14:59:28  niklas
-- BT-CH-0201: Role names with blanks. Option -warn [no_]role.
--
-- Revision 1.35  2009-12-19 13:56:02  niklas
-- BT-CH-0199: Allow all Arithmetic.Value_T values in assertions.
--
-- Revision 1.34  2009-12-17 14:05:54  niklas
-- BT-CH-0197: Assertions on instruction roles.
--
-- Revision 1.33  2009-12-16 12:23:14  niklas
-- BT-CH-0196: Identify subprogram by offset from other subprogram.
--
-- Revision 1.32  2009-12-15 09:13:28  niklas
-- BT-CH-0193: Less alarms for assertions on absent subprograms.
--
-- Revision 1.31  2009-11-27 11:28:06  niklas
-- BT-CH-0184: Bit-widths, Word_T, failed modular analysis.
--
-- Revision 1.30  2009/04/10 11:05:52  niklas
-- Extended Stack_Bound_Clause to List_Stack_Names if the
-- stack name given in the assertion is wrong.
--
-- Revision 1.29  2009/03/24 07:48:35  niklas
-- BT-CH-0166: String_Pool.Item_T for source-file and marker names.
--
-- Revision 1.28  2009/03/20 18:19:29  niklas
-- BT-CH-0164: Assertion context identified by source-line number.
--
-- Revision 1.27  2009/01/18 08:50:44  niklas
-- Removed unused locals.
--
-- Revision 1.26  2009/01/18 08:48:37  niklas
-- Added context clause (removed from parent).
--
-- Revision 1.25  2008/11/03 07:58:12  niklas
-- BT-CH-0155: Ignore assertions on absent subprograms.
--
-- Revision 1.24  2008/09/24 08:38:50  niklas
-- BT-CH-0146: Assertions on "loop starts <bound> times".
-- BT-CH-0146: Loop-repeat assertions set both lower and upper bound.
-- BT-CH-0146: Report locations of contradictory "count" assertions.
-- BT-CH-0146: Contradictory "count" assertions imply infeasibility.
--
-- Revision 1.23  2008/09/19 10:34:14  niklas
-- BT-CH-0144: Assertion "populations" work again, and more.
--
-- Revision 1.22  2008/07/28 19:23:44  niklas
-- BT-CH-0140: Detect contradictory execution-count bounds.
--
-- Revision 1.21  2008/07/14 19:16:54  niklas
-- BT-CH-0135: Assertions on "instructions".
--
-- Revision 1.20  2007/12/17 13:54:33  niklas
-- BT-CH-0098: Assertions on stack usage and final stack height, etc.
--
-- Revision 1.19  2007/08/14 13:18:24  niklas
-- BT-CH-0072: Corrections to handling ambiguous names. More.
--
-- Revision 1.18  2007/08/14 12:36:37  niklas
-- BT-CH-0072: Corrections to handling ambiguous names.
--
-- Revision 1.17  2007/07/10 11:06:28  niklas
-- Improved warning on no assertions found.
--
-- Revision 1.16  2007/03/22 12:58:08  niklas
-- Replaced Plus_Tok ('+') with Offset_Tok ("offset") because Plus_Tok
-- was ambiguous with Integer_Tok which allows signs.
-- Address-offsets are now written "offset <string>", not "+ <string>".
--
-- Revision 1.15  2007/02/13 20:14:05  Niklas
-- BT-CH-0044.
--
-- Revision 1.14  2007/01/25 21:25:12  niklas
-- BT-CH-0043.
--
-- Revision 1.13  2006/11/29 17:35:24  niklas
-- Added Device_Error handling.
--
-- Revision 1.12  2006/11/29 17:04:23  niklas
-- Corrected Parse_File (damaged in rev 1.7) to use Output.Error and
-- set Errors explicitly. Otherwise a missing assertion file causes
-- constraint error when computing Locus of the non-existent token.
--
-- Revision 1.11  2006/11/26 22:07:23  niklas
-- BT-CH-0039.
--
-- Revision 1.10  2006/10/30 23:09:05  niklas
-- BT-CH-0033.
--
-- Revision 1.9  2006/10/28 19:52:15  niklas
-- BT-CH-0031.
--
-- Revision 1.8  2006/10/14 14:36:00  niklas
-- Corrected the following procedures to skip the first token,
-- whether it is written in the singular or the plural form:
-- Is_Maybe_Something, Contains_Call, Contains_Loop, Uses_Variable,
-- Defines_Variable, Calls_To_Subprogram.
--
-- Revision 1.7  2006/09/29 18:07:48  niklas
-- Corrected Find_Label_Address to use the (local) Error
-- procedure (not the one from Output), thus noting that
-- the assertion file has errors.
-- Corrected Parse_File to record the failure to open the
-- assertion file as an error, giving a false Valid result.
-- Updated some messages to match the updated User Manual
-- (version 4).
--
-- Revision 1.6  2006/09/01 09:43:52  niklas
-- Corrected Skip (Token) to use Skip instead of Find_Next,
-- thus raising Invalid_Text for Invalid_Tok.
-- Added function Nots to parse a sequence of "not" tokens
-- as the possible negation of a loop or call property.
-- Corrected procedures Call_Property and Loop_Property to
-- handle negation ("not" tokens) properly, with the new
-- function Nots.
--
-- Revision 1.5  2006/08/31 09:49:36  niklas
-- Corrected procedure Sub_Block to use the new function Match
-- to compare the subprogram names at the start and end of the
-- block. Direct equality comparison of the Part_Names does
-- not work because the Source components naturally differ.
-- Extended to error message for mismatch to include the
-- plain image of the name at the end of the block.
--
-- Revision 1.4  2006/08/30 17:49:13  niklas
-- BT-CH-0027 corrects BT-NC-0160.
--
-- Revision 1.3  2006/08/22 13:07:14  niklas
-- Using Lexer.Invalid_Tok for better error messages about invalid
-- text. Extended procedure Skip to detect Invalid_Tok and to
-- raise the new Invalid_Text exception. Using Skip instead of
-- Find_Next so that this applies to all tokens.
-- Improved Parse_File to create a fresh Analyzer instance for
-- each assertion file, giving correct file-local line-numbers.
--
-- Revision 1.2  2006/05/29 11:22:33  niklas
-- BT-CH-0023.
--
-- Revision 1.1  2006/05/27 21:26:38  niklas
-- First version for BT-CH-0020.
--


with Ada.Strings;
with Ada.Strings.Fixed;
with Ada.Strings.Unbounded;
with Ada.Text_IO;
with Arithmetic;
with Assertions.Lexer;
with Assertions.Opt;
with Assertions.Own.Text;
with Calling;
with OpenToken.Text_Feeder.Text_IO;
with Output;
with Processor.Properties;
with Storage.Volatiles;
with Symbols;
with Symbols.Show;
with Unbounded_Vectors;


package body Assertions.Parser is


   use Ada.Strings.Unbounded;
   use Assertions.Lexer;
   use Assertions.Own;


   --
   ---   Error counting
   --


   Errors : Boolean := False;
   --
   -- Whether we have found errors in the assertion file.
   -- Global so that error handling functions can access it.


   --
   ---   Token lexing
   --


   Token_File_Name : File_Name_Ref;
   --
   -- The name of the assertion file that contains the current token.


   Feeder : aliased OpenToken.Text_Feeder.Text_IO.Instance;
   --
   -- For feeding text from the assertion file to the lexical Analyzer.


   Analyzer : Tokenizer.Instance;
   --
   -- For lexical analysis of the text in the assertion file.


   Invalid_Text : exception;
   --
   -- The current text is not recognized as any (valid) token.
   -- That is, the Tokenizer returns Invalid_Tok for this text.


   function Current_Token return Token_T
   --
   -- The current token as found by the analyzer.
   --
   is
   begin

      return Tokenizer.ID (Analyzer);

   end Current_Token;


   function Token_Source
   return Source_T
   --
   -- The file-name and line-number for the current token.
   --
   is
   begin

      return (
         File_Name   => Token_File_Name,
         Line_Number => Tokenizer.Line (Analyzer));

   end Token_Source;


   function Token_Locus
   return Output.Locus_T
   --
   -- Location of the current token in the assertion file.
   --
   is
   begin

      return Locus (Token_Source);

   end Token_Locus;


   function Token_Column return String
   --
   -- Image of the current column in the assertion file.
   --
   is
   begin

      return Output.Image (Tokenizer.Column (Analyzer));

   end Token_Column;

   function Current_Lexeme return String
   --
   -- The current lexeme (token) as such. No quotes around it.
   --
   is
   begin

      return Tokenizer.Lexeme (Analyzer);

   end Current_Lexeme;


   function Current_Lexeme_Quoted return String
   --
   -- The current lexeme (token) in double quotes.
   --
   is
   begin

      return '"' & Current_Lexeme & '"';

   end Current_Lexeme_Quoted;


   --
   ---  Tracing (optional)
   --


   procedure Trace (Text : in String)
   --
   -- Optional Output.Trace (Text).
   --
   is
   begin

      if Opt.Trace_Parsing then

         Output.Trace (
            Locus => Token_Locus,
            Text  => Text);

      end if;

   end Trace;


   --
   ---  Error handling
   --


   procedure Raise_Error (Text : in String)
   --
   -- Reports a syntax error in the assertion file, giving the
   -- line-number and the current (troublesome) lexeme.
   -- Then raises Input_Error to interrupt the parsing (however,
   -- the calling context may catch the exception and implement
   -- recovery parsing).
   --
   is
   begin

      Output.Error (
         Locus => Token_Locus,
         Text  => Text & ", at " & Current_Lexeme_Quoted);

      Errors := True;
      -- Note the error.

      raise Input_Error;

   end Raise_Error;


   procedure Expected (Text : in String)
   --
   -- Report a syntax error re what was expected in the text.
   -- Otherwise as Raise_Error.
   --
   is
   begin

      Raise_Error (Text => Text & " expected");

   end Expected;


   procedure Error (
      Locus : in Output.Locus_T;
      Text  : in String)
   --
   -- Reports an error in the assertion file that does not
   -- prevent continued parsing, but is associated with the
   -- given Locus (usually in the assertion file).
   --
   is
   begin

      Output.Error (
         Locus => Locus,
         Text  => Text);

      Errors := True;
      -- Note the error.

   end Error;


   procedure Error (
      Source : in Source_T;
      Text   : in String)
   --
   -- Reports an error in the assertion file that does not
   -- prevent continued parsing, but is associated with the
   -- given Source position in the assertion file.
   --
   is
   begin

      Error (
         Locus => Locus (Source),
         Text  => Text);

   end Error;


   procedure Error (Text : in String)
   --
   -- Reports an error in the assertion file that does not
   -- prevent continued parsing, but is associated with the
   -- current line-number in the assertion file.
   --
   is
   begin

      Error (
         Locus => Token_Locus,
         Text  => Text);

   end Error;


   --
   ---   Checking and skipping single tokens
   --


   procedure Skip
   --
   -- Take the next token (perhaps the first token).
   --
   is
   begin

      Tokenizer.Find_Next (Analyzer);

      if Current_Token = Invalid_Tok then

         raise Invalid_Text;

      end if;

   end Skip;


   procedure Skip (Token : in Token_T)
   --
   -- If the current token is the given one, take the next token,
   -- otherwise do nothing. So the Token is an optional one.
   --
   is
   begin

      if Current_Token = Token then

         Skip;

      end if;

   end Skip;


   function If_Skip (Token : Token_T) return Boolean
   --
   -- Whether the currenet token is the given one, and if so
   -- skip to the next token.
   --
   is
   begin

      if Current_Token = Token then

         Skip;

         return True;

      else

         return False;

      end if;

   end If_Skip;


   procedure Expect_And_Skip (
      Token  : in Token_T;
      Expect : in String)
   --
   -- If the current token is the given one, take the next token,
   -- otherwise complain that the token was expected. So the
   -- Token is a required one.
   --
   is
   begin

      if Current_Token = Token then

         Skip;

      else

         Expected (Text => Expect);

      end if;

   end Expect_And_Skip;


   --
   ---   "First" tables for the parser
   --
   --
   -- A set of "First" tables, returning a boolean value which
   -- indicates whether the token input as parameter is a correct
   -- first token of the production rule show by the table-name.


   type Token_Set_T is array (Token_T) of Boolean;
   --
   -- A set of tokens, e.g. all the tokens that can start a
   -- given syntactic construct.
   --
   pragma Pack (Token_Set_T);


   First_Var_Name : constant Token_Set_T := (
      String_Tok  => True,
      Address_Tok => True,
      others      => False);

   First_Part_Name : constant Token_Set_T := (
      String_Tok  => True,
      Address_Tok => True,
      others      => False);

   First_Bound : constant Token_Set_T := (
      Integer_Tok      => True,
      Relation_Token_T => True,
      Double_Dot_Tok   => True,
      others           => False);

   First_Et_Bound : constant Token_Set_T := (
      Time_Tok => True,
      others   => False);

   First_Stack_Bound : constant Token_Set_T := (
      Stack_Tok => True,
      others    => False);

   First_Rep_Bound : constant Token_Set_T := (
      Repeats_Tok_T => True,
      others        => False);

   First_Start_Bound : constant Token_Set_T := (
      Starts_Tok_T => True,
      others       => False);

   First_Var_Bound : constant Token_Set_T := (
      Variable_Tok => True,
      others       => False);

   First_Inv_Bound : constant Token_Set_T := (
      Invariant_Tok => True,
      others        => False);

   First_Prop_Bound : constant Token_Set_T := (
      Property_Tok => True,
      others       => False);

   First_Role_Bound : constant Token_Set_T := (
      Performs_Tok => True,
      others       => False);

   First_Source_Point : constant Token_Set_T := (
      Line_Tok   => True,
      Marker_Tok => True,
      others     => False);

   First_Source_Relation : constant Token_Set_T := (
      After_Tok    => True,
      At_Tok       => True,
      Before_Tok   => True,
      Exactly_Tok  => True,
      On_Tok       => True,
      others       => False);

   First_Source_Position : constant Token_Set_T :=
      First_Source_Relation;

   First_Point_Place : constant Token_Set_T :=
      First_Source_Position
      or Token_Set_T'(
      Labelled_Tok_T => True,
      Marked_Tok     => True,
      In_Tok         => True,
      others         => False);

   First_Property_Prefix : constant Token_Set_T := (
      That_Tok   => True,
      Does_Tok_T => True,
      Is_Tok_T   => True,
      Not_Tok    => True,
      others     => False);

   First_Call_Property : constant Token_Set_T :=
         First_Property_Prefix
      or First_Point_Place
      or Token_Set_T'(
         Defines_Tok_T => True,
         Uses_Tok_T    => True,
         Is_In_Tok     => True,
         others        => False);

   First_Call_Descr : constant Token_Set_T := (
      Dynamic_Tok => True,
      Calls_Tok_T => True,
      Call_To_Tok => True,     -- deprecated
      others      => False);

   First_Loop_Property : constant Token_Set_T :=
         First_Property_Prefix
      or First_Point_Place
      or Token_Set_T'(
         Calls_Tok_T    => True,
         Contains_Tok_T => True,
         Defines_Tok_T  => True,
         Executes_Tok_T => True,
         Spans_Tok_T    => True,
         Uses_Tok_T     => True,
         Is_In_Tok      => True,
         others         => False);

   First_Loop_Properties : constant Token_Set_T :=
      First_Loop_Property
      and not Token_Set_T'(
      Call_Tok  => True,
      Calls_Tok => True,
      others    => False);
   --
   -- Disallows 'call' and 'calls' at the start of a list of
   -- loop properties. This avoids the ambiguity in assertions
   -- like the following:
   --
   --    dynamic call in loop calls "foo" ...
   --
   -- This could mean either of the following:
   --
   --    dynamic call in (loop that calls "foo") ...
   --    dynamic call in (loop) calls "foo" ...
   --
   -- By forbidding 'call' and 'calls' as loop properties when
   -- immediately following 'loop' we select the latter meaning
   -- and avoid the need to place the 'loop' in parentheses.


   First_Loop_Descr : constant Token_Set_T := (
      Loop_Tok_T    => True,
      Loop_That_Tok => True,   -- deprecated
      others        => False);

   First_Clause : constant Token_Set_T :=
         First_Et_Bound
      or First_Stack_Bound
      or First_Rep_Bound
      or First_Start_Bound
      or First_Var_Bound
      or First_Inv_Bound
      or First_Prop_Bound
      or First_Role_Bound;

   First_Population : constant Token_Set_T := (
      All_Tok => True,
      others  => False);

   First_Call_Block : constant Token_Set_T :=
         First_Population
      or First_Call_Descr;

   First_Loop_Block : constant Token_Set_T :=
         First_Population
      or First_Loop_Descr;

   First_Loop_Or_Call : constant Token_Set_T :=
         First_Population
      or First_Loop_Block
      or First_Call_Block;

   First_Instruction_Block : constant Token_Set_T := (
      Instruction_Tok => True,
      others          => False);

   First_Sub_Option : constant Token_Set_T := (
      Arithmetic_Tok    => True,
      Enough_Tok        => True,
      No_Arithmetic_Tok => True,
      Integrate_Tok     => True,
      Hide_Tok          => True,
      Nix_Token_T       => True,
      Omit_Tok          => True,
      Returns_Tok_T     => True,
      Unused_Tok        => True,
      Used_Tok          => True,
      others            => False);

   First_Non_Option_Statement : constant Token_Set_T :=
         First_Loop_Block
      or First_Call_Block
      or First_Instruction_Block
      or First_Clause;

   First_Statement : constant Token_Set_T :=
         First_Sub_Option
      or First_Non_Option_Statement;

   First_Sub_Block : constant Token_Set_T := (
      Subprogram_Tok => True,
      others         => False);

   First_Volatility : constant Token_Set_T := (
      Volatile_Tok => True,
      others       => False);

   First_Volatile_Area : constant Token_Set_T :=
         First_Var_Name
      or Token_Set_T'(Range_Tok => True, others => False);

   First_Scope_Prefix : constant Token_Set_T := (
      Within_Tok => True,
      others     => False);

   First_Scope_Delimiter : constant Token_Set_T := (
      Delimiter_Tok => True,
      others        => False);

   First_Assertion : constant Token_Set_T :=
         First_Scope_Prefix
      or First_Scope_Delimiter
      or First_Var_Bound
      or First_Volatility
      or First_Prop_Bound
      or First_Call_Block
      or First_Loop_Block
      or First_Instruction_Block
      or First_Sub_Block;
   --
   -- A token that can start some kind of assertion.



   --
   ---   Parsing procedures / functions
   --
   --
   -- As typical for recursive-descent parsers, we define a parsing
   -- procedures for (almost) each non-terminal in the assertion
   -- language. The parsing procedure or function parses the text for
   -- this non-terminal and returns an internal representation of the
   -- meaning of the text.


   --
   ---   Literals and near literals
   --


   Just_One : constant Storage.Bounds.Interval_T :=
      Storage.Bounds.Singleton (1);
   --
   -- One, not more, not less.


   At_Least_One : constant Storage.Bounds.Interval_T := (
      Min => Storage.Bounds.Limit (1),
      Max => Storage.Bounds.Not_Limited);
   --
   -- One or more.


   type Time_Unit_T is (Cycles);
   --
   -- Specifies the unit of measurement for an execution-time
   -- assertion.


   function Time_Unit return Time_Unit_T
   --
   -- Time unit symbol, (cycle | cycles).
   --
   is
   begin

      if Current_Token in Cycle_Tok_T then

         Skip;

         return Cycles;

      else

         Expected ("""cycles""");

         return Time_Unit_T'Last;

      end if;

   end Time_Unit;


   generic
      type Integral is range <>;
   package Integral_Literals is

      function Literal return Integral;
      --
      -- The Integral value of the current integer-literal token.
      -- The token is not consumed.

      function Literal_And_Skip return Integral;
      --
      -- The Integral value of the current integer-literal token.
      -- The token is consumed (skipped).

   end Integral_Literals;


   package body Integral_Literals is


      function Literal return Integral
      is

         Literal : constant String := Tokenizer.Lexeme (Analyzer);
         -- The token text.

         Value : Integral;
         -- The result to be.

      begin

         if Current_Token /= Integer_Tok then

             Expected ("Integer value");
             Value := 0;

         else

            Value := Integral'Value (Literal);

         end if;

         return Value;

      exception

      when Constraint_Error =>

         Error (Text =>
             "The integer literal """
            & Literal
            & """ is not a valid number.");

         return 0;

      end Literal;


      function Literal_And_Skip
      return Integral
      is

         Value : constant Integral := Literal;
         -- The value.

      begin

         Skip (Integer_Tok);

         return Value;

      end Literal_And_Skip;

   end Integral_Literals;


   package Integer_Values    is new Integral_Literals (Integer);
   package Arithmetic_Values is new Integral_Literals (Arithmetic.Value_T);


   function Bound (Default_Min : Storage.Bounds.Limit_T)
   return Storage.Bounds.Interval_T
   --
   -- Parses a bound (a value range). If the lower limit on
   -- the range is not given, the Default_Min is used.
   --
   is
      use Storage.Bounds;
      use type Arithmetic.Value_T;

      Source : constant Source_T := Token_Source;
      -- The location of the start of the bound, for a possible
      -- warning message re void bounds or unlimited bounds.

      Relation : Relation_Token_T;
      -- The relation, if the bound is given as <relation> <value>.


      procedure Set (
         Limit   :    out Limit_T;
         Offset  : in     Arithmetic.Value_T := 0)
      --
      -- Set the given Limit to the literal value from the token,
      -- assumed to be an Integer_Tok, modified by the given Offset.
      --
      is
      begin

         Limit := Storage.Bounds.Limit (
            Arithmetic_Values.Literal_And_Skip + Offset);

      end Set;


      Interval : Interval_T := (Min => Default_Min, Max => Not_Limited);
      -- The result.


   begin  -- Bound

      case Current_Token is

      when Double_Dot_Tok =>
         -- A range with unlimited lower bound, (.. max), or
         -- a totally unlimited range ( .. ).

         -- Interval.Min stays as Default_Min.

         Skip;

         if Current_Token = Integer_Tok then

            Set (Interval.Max);

         -- else
         --   Interval_Max stays as Not_Limited.

         end if;

      when Integer_Tok =>
         -- Either one value, or a range (min .. max),
         -- or a range with an unlimited upper bound (min ..).

         Set (Interval.Min);

         if Current_Token = Double_Dot_Tok then
            -- A range of values (min .. max) or a range
            -- with an unlimited upper bound (min ..).

            Skip;

            if Current_Token = Integer_Tok then
               -- A range min .. max.

               Set (Interval.Max);

            -- else
            --    A range with no upper bound (min ..).
            --    Interval_Max stays as Storage.Bounds.Not_Limited.

            end if;

         else
            -- A single value.

            Interval.Max := Interval.Min;

         end if;

      when Relation_Token_T =>

         Relation := Tokenizer.ID (Analyzer);

         Skip (Relation);

         case Relation is

         when Equal_Tok =>

            Set (Interval.Min);

            Interval.Max := Interval.Min;

         when Greater_Equal_Tok =>

            Set (Interval.Min);

         when Greater_Than_Tok =>

            Set (Interval.Min, +1);

         when Less_Equal_Tok =>

            Set (Interval.Max);

         when Less_Than_Tok =>

            Set (Interval.Max, -1);

         end case;

      when others =>

         Expected ("Bound");

      end case;

      if Storage.Bounds.Void (Interval) then
         -- Min > Max: no values are allowed.

         Output.Warning (
            Locus => Own.Locus (Source),
            Text  => "Void interval may create contradiction.");

      elsif Interval = (Min => Default_Min, Max => Not_Limited) then
         -- Neither Min nor Max is limited: all values are allowed.

         Output.Warning (
            Locus => Own.Locus (Source),
            Text  => "Unlimited interval may have no effect.");

      end if;

      return Interval;

   end Bound;


   function Population_Bound
   return Storage.Bounds.Interval_T
   --
   -- A population bound: "all" [ Bound ] or nothing.
   -- The default bound is 1..1.
   --
   is
      use Storage.Bounds;

      Pop : Interval_T;
      -- The result.

   begin

      if Current_Token = All_Tok then
         -- A population is specified.

         Skip;

         if First_Bound (Current_Token) then
            -- The population range is specified.

            Pop := Bound (Default_Min => Non_Negative_Interval.Min);

         else
            -- Any population is accepted.

            Pop := Non_Negative_Interval;

         end if;

      else
         -- No population is specified.

         Pop := Just_One;

      end if;

      if not (Pop <= Non_Negative_Interval) then

         Output.Warning ("Negative populations are meaningless.");

      end if;

      return Pop;

   end Population_Bound;


   function Containee_Count
   return Storage.Bounds.Interval_T
   --
   -- A Count for a "contained part": Bound or nothing.
   -- The default value is 1 .. inf.
   --
   is
      use Storage.Bounds;

      Count : Interval_T;
      -- The result.

   begin

      if First_Bound (Current_Token) then
         -- The count of contained parts is specified.

         Count := Bound (Default_Min => Non_Negative_Interval.Min);

      else
         -- No count is specified.

         Count := At_Least_One;

      end if;

      if not (Count <= Non_Negative_Interval) then

         Output.Warning ("Negative counts are meaningless.");

      end if;

      return Count;

   end Containee_Count;


   function Quoted_String return String
   --
   -- Assuming that the current lexeme is a quoted string, this
   -- function returns the "innards" of the string, skipping the
   -- leading and closing quote.
   --
   -- If the token is not a quoted string, an error message is
   -- emitted and the null string is returned.
   --
   -- Note that we do not consume the token.
   --
   is

      Quoted : constant String := Tokenizer.Lexeme (Analyzer);
      -- The full token, including quotes if any.

   begin

      if Tokenizer.ID (Analyzer) = String_Tok then

         return Quoted (Quoted'First + 1 .. Quoted'Last - 1);

      else

         Expected ("Quoted string");

         return "";

      end if;

   end Quoted_String;


   function Quoted_String_Skip return String
   --
   -- A quoted string, which is then consumed.
   --
   is

      Value : constant String := Quoted_String;

   begin

      Skip (String_Tok);

      return Value;

   end Quoted_String_Skip;


   function Quoted_Character return Character
   --
   -- Assuming that the current lexeme is a quoted character or
   -- a quoted string of length 1, returns the character.
   -- Note that we do not consume the token.
   --
   is

      Quoted : constant String := Tokenizer.Lexeme (Analyzer);
      -- The full token, including quotes if any.

   begin

      if (   Current_Token = Character_Tok
          or Current_Token = String_Tok   )
      and Quoted'Length = 3
      then

         return Quoted(Quoted'First + 1);

      else

         Expected ("Quoted character");

         return '?';

      end if;

   end Quoted_Character;


   function Code_Address (Address : String)
   return Processor.Code_Address_T
   --
   -- The code address denoted by a machine-level address string.
   -- If the string is invalid, assertion parsing is aborted.
   --
   is
   begin

      return Processor.Properties.Subprogram_Address (Address);

   exception

   when Processor.Properties.Address_Error =>

      Raise_Error (Text =>
         '"' & Address & """ is not a valid code address");

      raise;
      -- This statement is never reached.

   end Code_Address;


   function Code_Offset (Address : String)
   return Processor.Code_Offset_T
   --
   -- The code offset denoted by a machine-level offset string.
   -- If the string is invalid, assertion parsing is aborted.
   --
   is
   begin

      return Processor.Properties.Code_Offset (Address);

   exception

   when Processor.Properties.Address_Error =>

      Raise_Error (Text =>
         '"' & Address & """ is not a valid code offset.");

      raise;
      -- This statement is never reached.

   end Code_Offset;


   type Base_Status_T is (Defined, Not_Defined, Useless);
   --
   -- The state of definition of a "base point" for a code-address
   -- offset or a line-number offset.
   --
   -- Defined
   --    The base point is defined.
   -- Not_Defined
   --    A base point is not defined, but is needed, if an
   --    offset is to be parsed correctly.
   -- Useless
   --    A base point is not defined, but an offset can be
   --    parsed well enough without a base point (because the
   --    offset will never be used).


   type Base_Point_T is record
      Address_Status : Base_Status_T := Not_Defined;
      Address        : Processor.Code_Address_T;
      Line_Status    : Base_Status_T := Not_Defined;
      Line           : Line_Number_T;
      File           : Symbols.Source_File_Name_T;
   end record;
   --
   -- A possible base point for specifying code addresses
   -- in the form 'offset <address offset>' and/or source-line
   -- numbers in the form 'line offset <line-number offset>'.


   Undefined_Base_Point : constant Base_Point_T := (
      Address_Status => Not_Defined,
      Address        => Processor.Code_Address_Default,
      Line_Status    => Not_Defined,
      Line           => Line_Number_T'First,
      File           => Symbols.Null_Name);
   --
   -- An undefined base point, but also meaning that a base
   -- point should be defined to avoid errors.


   Useless_Base_Point : constant Base_Point_T := (
      Address_Status => Useless,
      Address        => Processor.Code_Address_Default,
      Line_Status    => Useless,
      Line           => Line_Number_T'First,
      File           => Symbols.Null_Name);
   --
   -- A useless, dummy base point.


   function Base_Point_Of (Subprogram : Programs.Subprogram_T)
   return Base_Point_T
   --
   -- The base point for assertions written within a subprogram
   -- block for this Subprogram.
   --
   is

      Entry_Addr : constant Processor.Code_Address_T :=
         Programs.Entry_Address (Subprogram);
      -- The entry address of the subprogram is the base for
      -- address offsets.

      Base : Base_Point_T;
      -- The result. Initially all "undefined".

      procedure Define_Base_Line (By : in Symbols.Connection_Set_T)
      is
      begin

         if By'Length > 0 then

            Base.Line_Status := Defined;

            Base.Line := Symbols.Line_Number_Of (By(By'First));
            Base.File := Symbols.Source_File_Of (By(By'First));

         end if;

      end Define_Base_Line;

   begin  -- Base_Point_Of

      Base.Address_Status := Defined;
      Base.Address        := Entry_Addr;

      Define_Base_Line (Symbols.Lines_For_Address (
         Address => Entry_Addr,
         Within  => Programs.Symbol_Table (Subprogram)));

      if Base.Line_Status /= Defined then

         Define_Base_Line (Symbols.Line_Before (
            Address => Entry_Addr,
            Within  => Programs.Symbol_Table (Subprogram)));

      end if;

      if Opt.Trace_Parsing
      or Opt.Trace_Map
      then

         declare

            Common : constant String :=
                 "Assertion base for """
               & Programs.Name (Subprogram, Qualified => True)
               & """ is address "
               & Processor.Image (Base.Address);

         begin

            if Base.Line_Status = Defined then

               Output.Trace (
                    Common
                  & ", source-line"
                  & Line_Number_T'Image (Base.Line)
                  & " in """
                  & Symbols.Image (Base.File)
                  & """.");

            else

               Output.Trace (
                    Common
                  & ", source-line undefined.");

            end if;

         end;

      end if;

      return Base;

   end Base_Point_Of;


   --
   ---   Names
   --


   type Names_T is record
      Program      : Programs.Program_T;
      Symbol_Table : Symbols.Symbol_Table_T;
      Prefix       : Unbounded_String;
      Delimiter    : Character;
   end record;
   --
   -- Information for looking up symbolic names (identifiers).
   --
   -- Program
   --    The program under analysis. Reference semantics.
   -- Symbol_Table
   --    The symbol-table of the Program. Reference semantics.
   -- Prefix
   --    A prefix to be applied to symbols such as subprogram or variable
   --    names, to form a qualified symbol for the symbol-table look-up,
   --    unless the symbol already specifies its full scope prefix
   --    (by starting with the Delimiter).
   -- Delimiter
   --    The delimiter to be placed between the prefix and the symbol
   --    given in the assertion file. This will also be used as the
   --    scope-delimiter character for symbol-table look-up.


   function "&" (Names : Names_T; Symbol : String)
   return String
   --
   -- The prefixed Symbol, generally equal to
   --
   --    Prefix.Prefix & Prefix.Delimiter & Symbol.
   --
   -- Special cases are handled as follows:
   --
   -- > if the Symbol starts with the Prefix.Delimiter character
   --   this prevents adding any Prefix; instead, the rest of the
   --   Symbol (skipping the initial Delimiter) is returned;
   --
   -- > otherwise, if the Prefix is empty, the Symbol is returned
   --   as such;
   --
   -- > otherwise, Prefix.Prefix & Prefix.Delimiter & Symbol
   --   is returned.
   --
   is
   begin

      if Symbol(Symbol'First) = Names.Delimiter then
         -- The Symbol contains its own scope prefix, so
         -- the (default) Prefix is not added and instead the
         -- initial delimited is dropped.

         return Symbol(Symbol'First + 1 .. Symbol'Last);


      elsif Length (Names.Prefix) = 0 then
         -- There is no prefix to add.

         return Symbol;

      else
         -- There is a Prefix and the Symbol does not specify
         -- its own prefix, so we combine them:

         return To_String (Names.Prefix)
              & Names.Delimiter
              & Symbol;

      end if;

   end "&";


   function Variable_By_Symbol (
      Symbol : String;
      Source : Source_T;
      Names  : Names_T)
   return Variable_Name_T
   --
   -- Finds the variable identified by a Symbolic name
   -- and returns it as a Variable_Name_T.
   --
   -- If the Symbol is ambiguous when interpreted as a variable name
   -- we emit an error and return a null location is returned.
   --
   is

      Prefixed_Symbol : constant String := Names & Symbol;

      Sym_Scope : constant Symbols.Scope_T :=
         Symbols.Scope_Of (
            Identifier => Prefixed_Symbol,
            Delimiter  => Names.Delimiter);
      -- The scope defined by the given string.

      Var_Name : constant String :=
         Symbols.Name_Of (
            Identifier => Prefixed_Symbol,
            Delimiter  => Names.Delimiter);
      -- The variable name defined by the given string.

      Location : Storage.Location_Ref;
      -- The location map for this variable.

   begin  -- Variable_By_Symbol

      declare

         Connections : constant Symbols.Connection_Set_T :=
            Symbols.Variable_Connections (
               Scope  => Sym_Scope,
               Name   => Var_Name,
               Within => Names.Symbol_Table);
         --
         -- The connections for the given symbol.
         -- May propagate Symbols.Ambiguous_Name.

      begin

         if Connections'Length = 0 then

            Error (
               Source => Source,
               Text   =>
                    "Variable not found"
                  & Output.Field_Separator
                  & Prefixed_Symbol);

            Location := null;

         else

            if Connections'Length > 1 then

               Error (
                  Source => Source,
                  Text   =>
                       "Variable """
                     & Symbol
                     & """ is ambiguous");

            end if;

            for C in Connections'Range loop

               Output.Note (Symbols.Image (Connections(C)));

            end loop;

            Location := Symbols.Location_Of (Connections(Connections'First));

         end if;

      end;

      return (
         Kind     => Name,
         Location => Location,
         Name     => To_Name (Symbol));

   exception

   when Symbols.Ambiguous_Name =>

      Error (
         Source => Source,
         Text   =>
             "Ambiguous variable name"
            & Output.Field_Separator
            & Symbol);

      return (
         Kind     => Name,
         Location => null,
         Name     => To_Name (Symbol));

   end Variable_By_Symbol;


   function Variable_By_Address (
      Address  : String)
   return Variable_Name_T
   --
   -- Finds the variable (cell) identified by a machine address
   -- (in string form) and returns it as a Variable_Name_T.
   --
   is

      Cell : Storage.Cell_T;
      -- The cell for the Address, if valid.

      Location : Storage.Location_Ref;
      -- The location map will be a constant.

   begin

      begin

         Cell := Processor.Properties.Variable_Cell (Address);

         Location := new Storage.Location_T'(
            Storage.Fixed_Location (Cell));

      exception

         when Storage.No_Such_Cell
            | Processor.Properties.Address_Error   =>

            Error (Text =>
               '"' & Address & """ is not a valid cell address");

            Location := null;
      end;

      return (
         Kind     => Own.Address,
         Location => Location,
         Address  => To_Name (Address));

   end Variable_By_Address;


   function Variable_Name (Names : Names_T)
   return Variable_Name_T
   --
   -- Parses a variable name, which can be either a symbolic
   -- identifier (a string) or a machine-address (also given as
   -- a string) prefixed with the "address" keyword.
   --
   -- The symbolic identifier is looked up from the given
   -- symbol table, assisted by the given scope.
   --
   is

      Source : Source_T;
      -- The source location for the variable name.

      Name : Variable_Name_T;
      -- The result.

   begin

      if not First_Var_Name (Current_Token) then

         Expected ("Variable name");

      end if;

      if Current_Token = Address_Tok then

         Skip (Address_Tok);

         Source := Token_Source;

         Name := Variable_By_Address (
            Address => Quoted_String_Skip);

      else

         Source := Token_Source;

         Name := Variable_By_Symbol (
            Symbol => Quoted_String_Skip,
            Source => Source,
            Names  => Names);

      end if;

      return Name;

   end Variable_Name;


   function Property return Processor.Property_T
   --
   -- The name of a processor property, in quotes.
   --
   is

      Text : constant String := Quoted_String_Skip;
      -- The property name stripped of the quotes.

      Value : Processor.Property_T;
      -- The value.

   begin

      begin

         Value := Processor.Property_T'Value (Text);

      exception

      when Constraint_Error =>

         Error (
              "Unrecognized property name"
            & Output.Field_Separator
            & '"' & Text & '"');

         Value := Processor.Property_T'First;

      end;

      return Value;

   end Property;


   function Part_Name (
      Names : in Names_T)
   return Part_Name_T
   --
   -- The name, name-pattern, or address of a program part.
   --
   is

      Source : constant Source_T := Token_Source;
      -- Captures the place at the start.

      Value : Name_String_T;
      -- The name, of whatever interpretation.

      Offset : Processor.Code_Offset_T := Processor.Zero_Code_Offset;
      -- The possible address offset, initialized to none.


      procedure Possible_Offset
      --
      -- An optional 'offset' <code offset>.
      --
      is
      begin

         if If_Skip (Offset_Tok) then

            Offset := Code_Offset (Quoted_String_Skip);

         end if;

      end Possible_Offset;


   begin  -- Part_Name

      if not First_Part_Name (Current_Token) then

         Expected ("Subprogram name");

      end if;

      if Current_Token = Address_Tok then
         -- The address is specified.

         Skip (Address_Tok);

         Value := To_Name (Quoted_String_Skip);

         Possible_Offset;

         return (
            Kind      => Address,
            Delimiter => Names.Delimiter,
            Source    => Source,
            Offset    => Offset,
            Address   => Value);

      -- elsif ...
      --    -- A pattern for the symbolic name is specified. TBA.

      else
         -- The symbolic identifier is specified.

         Value := To_Name (Names & Quoted_String_Skip);

         Possible_Offset;

         return (
            Kind      => Name,
            Delimiter => Names.Delimiter,
            Source    => Source,
            Offset    => Offset,
            Name      => Value);

      end if;

   end Part_Name;


   procedure Find_Named_Subprogram (
      Name        : in     String;
      Delimiter   : in     Character;
      Source      : in     Source_T;
      Program     : in     Programs.Program_T;
      Warn_Absent : in     Boolean;
      Subprogram  :    out Programs.Subprogram_T)
   --
   -- Looks up the Name in the Program's symbol table and returns
   -- the corresponding Subprogram if found.
   --
   -- If the Name does not identify a subprogram, issues a warning
   -- and returns No_Subprogram.
   --
   -- If the Name and Delimiter are ambiguous (could identify several
   -- subprograms) issues an error and returns No_Subprogram.
   --
   -- Name
   --    Fully qualified subprogram identifier.
   -- Delimiter
   --    The scope delimiter character used in Name.
   -- Source
   --    The place of the Name in an assertion file, for the
   --    warning message.
   -- Program
   --    The program that is supposed to contain a subprogram with
   --    this Name.
   -- Warn_Absent
   --    Whether to emit a warning if the Program does not
   --    contain a subprogram with this Name.
   -- Subprogram
   --    The subprogram with this Name, or No_Subprogram.
   --
   is
      use type Output.Locus_T;
   begin

      Programs.Identify (
         Identifier => Name,
         Delimiter  => Delimiter,
         Program    => Program,
         Subprogram => Subprogram);

   exception

   when Symbols.Ambiguous_Name =>

      Error (
         Source => Source,
         Text   =>
              "Ambiguous subprogram name"
            & Output.Field_Separator
            & Name);

      Subprogram := Programs.No_Subprogram;

   when Programs.Subprogram_Not_Found =>

      if Warn_Absent then

         Output.Warning (
            Locus => Locus (Source),
            Text  =>
                 "Subprogram not found"
               & Output.Field_Separator
               & Name);

      end if;

      Subprogram := Programs.No_Subprogram;

   end Find_Named_Subprogram;


   procedure Find_Subprogram_By_Address (
      Address    : in     String;
      Offset     : in     Processor.Code_Offset_T;
      Source     : in     Source_T;
      Program    : in     Programs.Program_T;
      Subprogram :    out Programs.Subprogram_T)
   --
   -- Interprets the Address as a Processor.Code_Address_T (using
   -- Processor.Properties.Sub_Address) and returns the corresponding
   -- Subprogram.
   --
   -- If the Address is not understood, the procedure issues an error
   -- and returns No_Subprogram.
   --
   -- Address
   --    The string that should give the subprogram entry address.
   -- Source
   --    The place of the Name in an assertion file, for the
   --    warning message.
   -- Program
   --    The program that is supposed to contain a subprogram with
   --    this Name.
   -- Subprogram
   --    The subprogram with this Name, or No_Subprogram.
   --
   is
      use type Output.Locus_T;

      Base_Address : Processor.Code_Address_T;
      -- The entry address, if Address is valid.

      Code_Address : Processor.Code_Address_T;
      -- The final address, Base_Address plus Offset.

   begin

      Base_Address := Processor.Properties.Subprogram_Address (Address);
      -- May raise Address_Error.

      Code_Address := Processor.Shift (
         Base   => Base_Address,
         Offset => Offset);

      Programs.Identify (
         Address    => Code_Address,
         Program    => Program,
         Subprogram => Subprogram);

   exception

   when Processor.Properties.Address_Error =>

      Error (
         Locus =>
              Locus (Source)
            & Output.Locus (Call_Path => Address),
         Text => "Subprogram address is invalid.");

      Subprogram := Programs.No_Subprogram;

   end Find_Subprogram_By_Address;


   procedure Find_Subprogram (
      Part_Name   : in     Part_Name_T;
      Names       : in     Names_T;
      Warn_Absent : in     Boolean;
      Subprogram  :    out Programs.Subprogram_T)
   --
   -- Finds the Subprogram identified by the Part_Name.
   --
   is
      use type Processor.Code_Offset_T;
      use type Programs.Subprogram_T;

      Offset_Address : Processor.Code_Address_T;
      -- The offset address for a Named part, if Part_Name.Offset
      -- is not zero.

   begin

      case Part_Name.Kind is

      when Name =>

         Find_Named_Subprogram (
            Name        => To_String (Part_Name.Name),
            Delimiter   => Part_Name.Delimiter,
            Source      => Part_Name.Source,
            Program     => Names.Program,
            Warn_Absent => Warn_Absent,
            Subprogram  => Subprogram);

         if Subprogram /= Programs.No_Subprogram
         and then Part_Name.Offset /= Processor.Zero_Code_Offset
         then

            Offset_Address := Processor.Shift (
               Base   => Programs.Entry_Address (Subprogram),
               Offset => Part_Name.Offset);

            Programs.Identify (
               Address    => Offset_Address,
               Program    => Names.Program,
               Subprogram => Subprogram);

         end if;

      when Pattern =>

         Subprogram := Programs.No_Subprogram;
         -- Will be found by matching names against the pattern.

      when Address =>

         Find_Subprogram_By_Address (
            Address    => To_String (Part_Name.Address),
            Offset     => Part_Name.Offset,
            Source     => Part_Name.Source,
            Program    => Names.Program,
            Subprogram => Subprogram);

      end case;

   end Find_Subprogram;


   procedure Identify_Subprogram (
      Names       : in     Names_T;
      Warn_Absent : in     Boolean;
      Sub_Name    :    out Part_Name_T;
      Feature     :    out Feature_T)
   --
   -- Parses a name, address or name-pattern, giving a Sub_Name that
   -- identifies a (set of) subprogram(s). If the Sub_Name identifies a
   -- single subprogram, a Subprogram_Identity Feature results;
   -- otherwise a Named Feature results.
   --
   is
      use type Programs.Subprogram_T;

      Subprogram : Programs.Subprogram_T;
      -- The subprogram that may be identified.

   begin

      -- The subprogram name, address or name-pattern:

      Sub_Name := Part_Name (Names);

      Find_Subprogram (
         Part_Name   => Sub_Name,
         Names       => Names,
         Warn_Absent => Warn_Absent,
         Subprogram  => Subprogram);

      case Sub_Name.Kind is

      when Name | Address =>

         if Subprogram = Programs.No_Subprogram then
            -- The name (or address) matches no subprogram
            -- in this program.

            Feature := (
               Kind    => Subprogram_Absent,
               Source  => Sub_Name.Source,
               Negated => False,
               Name    => Sub_Name);

         else
            -- The name or address matches exactly one subprogram
            -- in this program.

            Feature := (
               Kind       => Subprogram_Identity,
               Source     => Sub_Name.Source,
               Negated    => False,
               Subprogram => Subprogram);

         end if;

      when Pattern =>
         -- The subprograms that match this Sub_Name are not
         -- yet known.

         Feature := (
            Kind    => Named,
            Source  => Sub_Name.Source,
            Negated => False,
            Name    => Sub_Name);

      end case;

   end Identify_Subprogram;


   procedure Find_Label_Address (
      Symbol  : in     String;
      Source  : in     Source_T;
      Names   : in     Names_T;
      Found   :    out Boolean;
      Address :    out Processor.Code_Address_T)
   --
   -- Finds the label identified by a Symbol and returns its
   -- Address if Found.
   --
   is

      Prefixed_Symbol : constant String := Names & Symbol;

      Conn : Symbols.Connection_T;
      -- The chosen connection, if there are some.

   begin

      Address := Processor.Code_Address_Default;
      -- Dummy value, may be replaced below.

      declare

         Conns : constant Symbols.Connection_Set_T :=
            Symbols.Label_Connections (
               Scope  => Symbols.Scope_Of (
                  Identifier => Prefixed_Symbol,
                  Delimiter  => Names.Delimiter),
               Name   => Symbols.Name_Of (
                  Identifier => Prefixed_Symbol,
                  Delimiter  => Names.Delimiter),
               Within => Names.Symbol_Table);
         --
         -- All label connections for the given symbol in the given scope.
         -- May propagate Symbols.Ambiguous_Name.

      begin

         Found := Conns'Length > 0;

         if Found then
            -- Use the first (or only) connection:

            Conn := Conns(Conns'First);

            Address := Symbols.Address_Of (Conn);

            for C in Conns'First + 1 .. Conns'Last loop

               Output.Warning (
                  Locus => Symbols.Show.Locus (
                     Connection => Conns(C),
                     Source     => Names.Symbol_Table),
                  Text => "Other identifier connection not used.");

            end loop;

            if Conns'Length > 1 then

               Output.Warning (
                  Locus => Symbols.Show.Locus (
                     Connection => Conn,
                     Source     => Names.Symbol_Table),
                  Text => "This identifier connection used.");

            end if;

         else

            Error (
               Source => Source,
               Text   =>
                    "Label not found"
                  & Output.Field_Separator
                  & Prefixed_Symbol);

         end if;

      end;

   exception

   when Symbols.Ambiguous_Name =>

      Error (
         Source => Source,
         Text   =>
              "Ambiguous label name"
            & Output.Field_Separator
            & Prefixed_Symbol);

      Found := False;

   end Find_Label_Address;


   --
   ---   Feature lists
   --


   package Unbounded_Feature_Vectors is new Unbounded_Vectors (
      Element_Type   => Feature_T,
      Vector_Type    => Part_Predicate_T,
      Initial_Size   => 50,
      Size_Increment => 50,
      Deallocate     => Opt.Deallocate);


   type Unbounded_Feature_List_T is
      new Unbounded_Feature_Vectors.Unbounded_Vector;
   --
   -- We must scollect all the features that a certain part has,
   -- before we know the total number of features.


   procedure Add (
      Feature : in     Feature_T;
      To      : in out Unbounded_Feature_List_T)
   is
   begin

      Trace ("Adding feature " & Feature_Kind_T'Image (Feature.Kind));

      Append (To, Feature);

   end Add;


   procedure Set_Predicate (
      Parts : in out Parts_T;
      To    : in out Unbounded_Feature_List_T)
   --
   -- Sets the given feature list as the Predicate of the given Parts.
   -- Erases the feature list To.
   --
   is
   begin

      Trace (
           "Setting "
         & Part_Kind_T'Image (Parts.Kind)
         & " predicate with"
         & Natural'Image (Length (To))
         & " features.");

      if Length (To) > 0 then
         -- Some features are required.

         Parts.Predicate := new Part_Predicate_T'(To_Vector (To));

      else
         -- No features are required.

         Parts.Predicate := True_Predicate;

      end if;

      Erase (To);

   end Set_Predicate;


   --
   ---   Fact assertions
   --


   type Fact_List_T is array (Positive range <>) of Fact_T;
   --
   -- A list of facts.


   package Unbounded_Fact_Vectors is new Unbounded_Vectors (
      Element_Type   => Fact_T,
      Vector_Type    => Fact_List_T,
      Initial_Size   => 100,
      Size_Increment => 100,
      Deallocate     => Opt.Deallocate);


   type Unbounded_Fact_List_T is
      new Unbounded_Fact_Vectors.Unbounded_Vector;
   --
   -- We must sometimes collect all the facts that are asserted
   -- for a given part (eg. a subprogram), before we know all the
   -- features of the part (eg. the loops and calls that it contains)
   -- and thus before we can generate assertions from these facts.
   -- Then we collect the facts in unbounded fact lists.


   function Execution_Time_Bound (
      Part : Actual_Part_Kind_T)
   return Fact_T
   --
   -- An assertion on the execution time of a Part.
   --
   is

      Source : constant Source_T := Token_Source;
      -- Capture the source reference at the start.

      Interval : Storage.Bounds.Interval_T;
      -- The interval of execution time.

      Unit : Time_Unit_T;
      -- The time unit.

      Time : Time_Bound_T := No_Bound;
      -- The result bounds on execution time.

   begin

      Expect_And_Skip (
         Token  => Time_Tok,
         Expect => """time""");

      -- Get the interval of execution time:

      Interval := Bound (Default_Min => Storage.Bounds.Not_Limited);

      if Storage.Bounds.Known (Interval.Min) then

         Time.Min := Processor.Time_T (
            Storage.Bounds.Value (Interval.Min));

      end if;

      if Storage.Bounds.Known (Interval.Max) then

         Time.Max := Processor.Time_T (
            Storage.Bounds.Value (Interval.Max));

      end if;

      -- Check the time unit:

      Unit := Time_Unit;

      -- Check the part for which the bound is asserted:

      case Part is

      when Program =>

         Error ("Execution time bounds for the program are not allowed.");

      when Subprogram  | Call_Static | Call_Dynamic =>
         -- Ok.

         null;

      when Luup =>

         Error ("Execution time bounds for a loop are not allowed.");

      when Instruction =>

         Error ("Execution time bounds for an instruction are not allowed.");

      when others =>

         Error ("Execution time bounds not allowed in this context.");

      end case;

      -- Done:

      return (
         Kind   => Execution_Time,
         Source => Source,
         Time   => Time);

   end Execution_Time_Bound;


   function To_Counts (Interval : Storage.Bounds.Interval_T)
   return Flow.Execution.Bound_T
   --
   -- Converts the Interval to bounds on execution count.
   -- May report warnings and errors if the bounds are negative
   -- or too large.
   --
   is
      use Storage.Bounds;
      use type Arithmetic.Value_T;

      Bound : Flow.Execution.Bound_T := Flow.Execution.Unbounded;
      -- The result.

   begin

      if Interval <= Negative_Interval then

         Error (
              "Must include non-negative numbers"
            & Output.Field_Separator
            & Image (Interval));

      elsif not (Interval <= Non_Negative_Interval) then

         Output.Warning (
              "Negative numbers are meaningless here"
            & Output.Field_Separator
            & Image (Interval));

      end if;

      if Known (Interval.Min)
      and then Value (Interval.Min) >= 0
      then

         Bound.Min := Flow.Execution.Count_T (Value (Interval.Min));

      end if;

      if Known (Interval.Max)
      and then Value (Interval.Max) >= 0
      then

         Bound.Max := Flow.Execution.Count_T (Value (Interval.Max));

      end if;

      return Bound;

   exception

   when Constraint_Error =>
      -- Assume conversion to Count_T failed.

      Output.Error (
           "Numbers are too large"
         & Output.Field_Separator
         & Image (Interval));

      return Flow.Execution.Unbounded;

   end To_Counts;


   function Count_Bound (
      Part : Actual_Part_Kind_T;
      Fact : Count_Fact_Kind_T)
   return Fact_T
   --
   -- Repetition (execution count) bound:
   --    ( repeats | repeat | starts | start ) <bound> ( times | time )
   --
   -- Precondition: Current token is in Repeats_Tok_T or Starts_Tok_T.
   --
   is
      use Storage.Bounds;

      Source : constant Source_T := Token_Source;
      -- Capture the source reference.

      Interval : Interval_T;
      -- The interval of execution counts.

      Counts : Flow.Execution.Bound_T;
      -- The Interval converted to bounds on execution count.

   begin

      Skip;
      -- Skips the "repeats" or "repeat" or "starts" or "start".

      Interval := Bound (Default_Min => Non_Negative_Interval.Min);

      Counts := To_Counts (Interval);

      -- Check trailing keyword:

      if Current_Token not in Time_Tok_T then

         Expected ("""times""");

      else

         Skip;
         -- Skips the "times" or "time".

      end if;

      -- Finish:

      case Fact is

      when Loop_Repetitions =>

         return (
            Kind   => Loop_Repetitions,
            Source => Source,
            Count  => Counts);

      when Loop_Starts =>

         return (
            Kind   => Loop_Starts,
            Source => Source,
            Count  => Counts);

      when Call_Executions =>

         return (
            Kind   => Call_Executions,
            Source => Source,
            Count  => Counts);

      when Instruction_Executions =>

         return (
            Kind   => Instruction_Executions,
            Source => Source,
            Count  => Counts);

      end case;

   end Count_Bound;


   function Variable_Bound (
      Names : Names_T;
      Part  : Actual_Part_Kind_T;
      Span  : Fact_Span_T)
   return Fact_T
   --
   -- Parses a variable bound assertion.
   --
   -- The symbolic variable identifier is looked up from the given
   -- symbol table, assisted by the given scope.
   --
   -- The span of the fact is defined by context.
   --
   is

      Source : constant Source_T := Token_Source;
      -- The source of the variable bound is the "variable" keyword.

      Name : Variable_Name_T;
      -- The variable name.

      Interval : Storage.Bounds.Interval_T;
      -- The asserted range of variable values.

   begin

      Expect_And_Skip (
         Token  => Variable_Tok,
         Expect => """variable""");

      Name := Variable_Name (Names);

      Interval := Bound (Default_Min => Storage.Bounds.Not_Limited);

      -- Check compatibility of Part - Kind:

      case Part is

      when Program
         | Subprogram
         | Luup
         | Call_Static
         | Call_Dynamic
         | Jump
         =>
         -- Acceptable.

         null;

      when Instruction =>

         Error ("Variable bounds for an instruction are not implemented.");

      end case;

      return (
         Kind      => Variable_Value,
         Source    => Source,
         Var_Name  => Name,
         Var_Value => Interval,
         Span      => Span);

   end Variable_Bound;


   function Variable_Invariance (
      Names : Names_T;
      Part  : Actual_Part_Kind_T)
   return Fact_T
   --
   -- A variable-invariance fact.
   --
   is

      Source : constant Source_T := Token_Source;
      -- Capture the source reference at the start.

      Name : Variable_Name_T;
      -- The name of the invariant variable.

   begin

      Expect_And_Skip (
         Token  => Invariant_Tok,
         Expect => """invariant""");

      Name := Variable_Name (Names);

      -- Check compatibility of Part - Kind:

      case Part is

      when Program
         | Subprogram
         | Luup
         | Call_Static
         | Call_Dynamic
         | Jump
         =>
         -- Acceptable.

         null;

      when Instruction =>

         Error ("Variable invariance for an instruction is not implemented.");

      end case;

      return (
         Kind               => Variable_Invariance,
         Source             => Source,
         Invariant_Var_Name => Name);

   end Variable_Invariance;


   procedure Volatile_Variable (Names : in Names_T)
   --
   -- Parses a Volatile_Area that is a Variable_Name. If the parse
   -- succeeds, the corresponding cells are marked volatile.
   --
   is
      use type Storage.Location_Ref;

      Var : constant Variable_Name_T := Variable_Name (Names);
      -- The variable. Natch.

   begin

      if Var.Location /= null then

         for L in Var.Location'Range loop

            Storage.Mark_As_Volatile (Var.Location(L).Cell);

         end loop;

      end if;

   end Volatile_Variable;


   procedure Volatile_Range_Bound (
      Address : in     String;
      Valid   :    out Boolean;
      Bound   :    out Processor.Cell_Spec_T)
   --
   -- Converts the variable_Address into a Cell_Spec to define one
   -- end of a volatile storage range, or returns Valid = False.
   --
   is

      Cell : Storage.Cell_T;
      -- The cell defined by the Address.

   begin

      Cell  := Processor.Properties.Variable_Cell (Address);
      Valid := True;
      Bound := Storage.Spec_Of (Cell);

   exception
   when Processor.Properties.Address_Error =>

      Error (Text =>
         '"' & Address & """ is not a valid cell address");

      Valid := False;

   end Volatile_Range_Bound;


   procedure Volatile_Range
   --
   -- Parses a Volatile_Area that is a range of storage locations,
   -- in the syntax
   --
   --    range variable_address .. variable_address
   --
   -- If the parse succeeds, the area is marked as volatile
   -- in the Storage model.
   --
   is

      From, To : Processor.Cell_Spec_T;
      -- The limits of the range, from the two variable_addresses.

      From_Valid, To_Valid, Range_Valid : Boolean;
      -- Whether the From / To variable_addresses are valid, and
      -- then the range From .. To is valid.

   begin

      Expect_And_Skip (
         Token  => Range_Tok,
         Expect => """range""");

      Volatile_Range_Bound (
         Address => Quoted_String,
         Valid   => From_Valid,
         Bound   => From);

      Skip;
      -- The quoted string.

      Expect_And_Skip (
         Token  => Double_Dot_Tok,
         Expect => """..""");

      Volatile_Range_Bound (
         Address => Quoted_String,
         Valid   => To_Valid,
         Bound   => To);

      if From_Valid and To_Valid then

         Storage.Volatiles.Mark_Range (
            From        => From,
            To          => To,
            Valid_Range => Range_Valid);

         if not Range_Valid then

            Error (Text =>
                 "The cells "
               & Processor.Image (From)
               & " and "
               & Processor.Image (To)
               & " do not define a range.");

         end if;

      end if;

      Skip;
      -- The second variable_address string.

   end Volatile_Range;


   procedure Volatility (
      Names : in     Names_T;
      Valid : in out Boolean)
   --
   -- Parses a volatility assertion and applies it to the Storage model.
   --
   -- The syntax is:
   --
   --    Volatility ::= volatile Volatile_Area [ {, Volatile_Area } ]
   --
   --    Volatile_Area ::= Variable_Name
   --                  |   range variable_address .. variable_address
   --
   -- Volatility assertions are not entered into an assertion set.
   -- They are globally valid and have no specific context.
   --
   -- If the parse succeeds, Valid is returned as True, otherwise
   -- Valid is not changed.
   --
   is

      Tok : Token_T;
      -- The token at the start of a Volatile_Area.

   begin

      Expect_And_Skip (
         Token  => Volatile_Tok,
         Expect => """volatile""");

      loop

         Tok := Current_Token;

         if Tok = Range_Tok then

            Volatile_Range;

         elsif First_Var_Name(Tok) then

            Volatile_Variable (Names);

         else

            Expected ("Variable name or ""range""");

         end if;

         exit when Current_Token /= Comma_Tok;

         Skip;

      end loop;

      Valid := True;

   end Volatility;


   function Property_Bound (
      Part : Actual_Part_Kind_T)
   return Fact_T
   --
   -- Bounds on the value of a processor-specific property.
   --
   is

      Source : constant Source_T := Token_Source;
      -- Capture the source reference at the start.

      Property : Processor.Property_T;
      -- The property that is being bounded.

      Interval : Storage.Bounds.Interval_T;
      -- The bounds on the value of the Property.

   begin

      Expect_And_Skip (
         Token  => Property_Tok,
         Expect => """property""");

      Property := Parser.Property;

      Interval := Bound (Default_Min => Storage.Bounds.Not_Limited);

      -- Check compatibility of Part - Kind:

      case Part is

      when Program
         | Subprogram
         | Luup
         | Call_Static
         | Call_Dynamic
         | Jump
         =>
         -- Acceptable.

         null;

      when Instruction =>

         Error ("Property bounds for an instruction are not implemented.");

      end case;

      return (
         Kind       => Property_Value,
         Source     => Source,
         Property   => Property,
         Prop_Value => Interval);

   end Property_Bound;


   function To_Literal (Name : String) return String
   --
   -- Converts a multi-word instruction-role name into an
   -- Ada-like identifier by trimming off leading and trailing
   -- blanks and replacing each embedded blank or sequence
   -- of blanks by one underscore, '_'.
   --
   is
      use Ada.Strings, Ada.Strings.Fixed;

      Core : constant String := Trim (Name, Both);
      -- The Name without leading and trailing blanks.

      Literal : String(1 .. Core'Length);
      Last    : Natural := 0;
      -- The result is Literal(1 .. Last).

   begin

      for C in Core'Range loop

         if Core(C) /= ' ' then
            -- This shall go in the literal.

            Last          := Last + 1;
            Literal(Last) := Core(C);


         elsif Literal(Last) /= '_' then
            -- This is the first blank in a sequence.
            -- Put an underscore in the Literal.

            Last          := Last + 1;
            Literal(Last) := '_';

         -- else Literal(Last) = '_' which means that
         -- Core(C) is the second or later blank in a
         -- sequence of blanks. Nothing to do for it.

         end if;

      end loop;

      return Literal(1 .. Last);

   end To_Literal;


   procedure Get_Instruction_Role (
      Name  : in     String;
      Valid :    out Boolean;
      Role  :    out Processor.Instruction_Role_T)
   --
   -- Tries to interpret the Name as the name of an instruction
   -- role, in the target-specific set of roles. The Valid parameter
   -- shows whether this succeeds, and the Role the (valid) role.
   --
   is
      use type Processor.Instruction_Role_T;
   begin

      Role := Processor.Instruction_Role_T'Value (Name);
      -- Raises Constraint_Error if Role_Name is not recognized.

      Valid := Role /= Processor.No_Role;

   exception

   when Constraint_Error =>

      Valid := False;
      Role  := Processor.No_Role;

   end Get_Instruction_Role;


   function Role_Bound (
      Part : Actual_Part_Kind_T)
   return Fact_T
   --
   -- Bounds on (or just a single value for) the role that
   -- an instruction performs in the target program:
   --
   --    'performs' ['a' | 'an' ] "<role>"
   --
   is

      Source : constant Source_T := Token_Source;
      -- Capture the source reference at the start.

      Role : Processor.Instruction_Role_T;
      -- The asserted role that this instruction performs.

      Valid : Boolean;
      -- Whether the Role is valid.

   begin

      Expect_And_Skip (
         Token  => Performs_Tok,
         Expect => """performs""");

      if Current_Token in Indefinite_Article_Tok_T then

         Skip;

      end if;

      declare

         Role_Name : constant String := Quoted_String;
         -- The raw role name.

         Literal : constant String := To_Literal (Role_Name);
         -- The role name with leading and trailing blanks removed
         -- and embedded blanks replaced with '_'.

      begin

         Get_Instruction_Role (Literal, Valid, Role);

         if not Valid then
            -- Try with a suffixed "_role".

            Get_Instruction_Role (Literal & "_role", Valid, Role);

         end if;

         if not Valid then

            Error ("Instruction role """
               & Role_Name
               & """ ("""
               & Literal
               & """) not recognized.");

         end if;

         Skip (String_Tok);

      end;

      case Part is

      when Instruction =>
         -- Acceptable.

         null;

      when others =>

         Error ("Roles can be asserted only for instructions.");

      end case;

      return (
         Kind   => Instruction_Role,
         Source => Source,
         Role   => Role);

   end Role_Bound;


   function Subprogram_List (Names : Names_T)
   return Programs.Subprogram_List_T
   --
   -- <subprogram> [ or <subprogram> ... ]
   --
   is
      use type Programs.Subprogram_T;
      use type Programs.Subprogram_List_T;

      Name : Part_Name_T;
      -- The name of the first subprogram.

      Subprogram : Programs.Subprogram_T;
      -- The first subprogram.

      First : Programs.Subprogram_List_T (1 .. 1);
      Num   : Natural := 0;
      -- The list of the first Subprogram is First(1 .. Num).
      -- The list is empty if the first Subprogram is not identified.

   begin

      Name := Part_Name (Names);

      Find_Subprogram (
         Part_Name   => Name,
         Names       => Names,
         Warn_Absent => True,
         Subprogram  => Subprogram);

      if Subprogram /= Programs.No_Subprogram then
         -- The first subprogram was identified.

         Num := Num + 1;

         First(Num) := Subprogram;

      -- else
      --    we leave the First list empty.

      end if;

      if Current_Token = Or_Tok then
         -- There are more subprograms.

         Skip;

         return First(1 .. Num) & Subprogram_List (Names);

      else
         -- This was the last subprogram in the list.

         return First(1 .. Num);

      end if;

   end Subprogram_List;


   function Callee_Bound (Names : Names_T)
   return Fact_T
   --
   -- Bounds on the callees of a dynamic call:
   --
   --   <subprogram> or <subprogram> ...
   --
   is

      Source : constant Source_T := Token_Source;
      -- Capture the place at the start.

      Callees : constant Programs.Subprogram_List_T :=
         Subprogram_List (Names);
      -- The list of (identified) callees.

   begin

      return (
         Kind    => Call_Callees,
         Source  => Source,
         Callees => new Programs.Subprogram_List_T'(Callees));

   end Callee_Bound;


   --
   ---   Scope and prefix controls
   --


   procedure Scope_Prefix (Prefix : out Unbounded_String)
   --
   -- Parse a Scope prefix definition.
   --
   is
   begin

      Skip (Within_Tok);

      Prefix := To_Unbounded_String (Quoted_String);

      Trace ("Scope prefix """ & To_String (Prefix) & '"');

      Skip;
      -- The quoted string.

   end Scope_Prefix;


   procedure Scope_Delimiter (Delimiter : out Character)
   --
   -- Parse a Scope_Delimiter setting.
   --
   is
   begin

      Skip (Delimiter_Tok);

      Delimiter := Quoted_Character;

      Trace ("Scope delimiter '" & Delimiter & ''');

      Skip;
      -- The quoted character.

   end Scope_Delimiter;


   procedure Assert (
      Thing : in     Assertion_T;
      To    : in out Assertion_Bag_T)
   --
   -- Adds, and optionally traces, the Assertion To the bag.
   --
   is
   begin

      if Opt.Trace_Parsing then

         Text.Put (Thing);

      end if;

      Add (Assertion => Thing, To => To);

   end Assert;


   --
   ---   Clauses
   --


   procedure Stack_Bound_Clause (
      Parts : in     Parts_Ref;
      Names : in     Names_T;
      Into  : in out Assertion_Bag_T)
   --
   -- A Stack_Bound Clause that applies to the given Parts,
   -- becoming zero or more assertions that are put Into the bag.
   --
   -- The current position is at the "stack" token. The nominal
   -- exit position is at the semicolon after the clause.
   --
   is
      use type Arithmetic.Value_T;
      use type Programs.Stack_Kind_T;
      use type Storage.Bounds.Interval_T;

      Source : constant Source_T := Token_Source;
      -- Capture the source position at the start.

      Stack : Programs.Stack_T;
      -- The stack named or implied in the assertion.

      Index : Programs.Stack_Index_T := 1;
      -- The index of the Stack, if a Stack is found, else 1.

      Stack_Valid : Boolean := False;
      -- The Stack and Index are valid.

      Set_Usage, Set_Final : Boolean := False;
      -- Whether the Usage or the Final height have been set.

      Fact_Source : Source_T;
      -- The source of a "usage" or "final" fact.

      Value : Storage.Bounds.Interval_T;
      -- The bounds on a "usage" or "final" value.

      Value_Source : Source_T;
      -- The source of the Value.


      procedure Get_Value
      --
      -- Gets the Value and defines Fact_Source and Value_Source.
      -- Called with "usage" or "final" as the current token (which
      -- becomes Fact_Source) and returns after parsing the Bound
      -- (which becomes the Value and Value_Source).
      --
      is
      begin

         Fact_Source := Token_Source;

         Skip;
         -- The "usage" or "final" keyword.

         Value_Source := Token_Source;

         Value := Bound (Default_Min => Storage.Bounds.Not_Limited);

      end Get_Value;


      procedure Assert (
         Kind : in     String;
         Fact : in     Fact_T;
         Set  : in out Boolean)
      is
      begin

         if Set then

            Output.Warning (
               Locus => Locus (Token_Source),
               Text  => "Multiple assertions on stack " & Kind & '.');

         end if;

         Set := True;

         if Storage.Bounds.Void (Fact.Stack_Value) then

            Error (
               Source => Value_Source,
               Text   => "Void bounds on stack " & Kind & " ignored.");

         else

            Assert (
               Thing => (
                  Fact    => Fact,
                  Parts   => Parts,
                  Context => No_Context,
                  Source  => Fact_Source),
               To => Into);

         end if;

      end Assert;


      procedure List_Stack_Names
      --
      -- Lists the names of the stacks in the program, as
      -- separate Error messages.
      --
      is
      begin

         for S in 1 .. Programs.Number_Of_Stacks (Names.Program) loop

            Error (
                 "Stack #"
               & Output.Image (S)
               & Output.Field_Separator
               & Programs.Stack_Name (S, Names.Program));

         end loop;

      end List_Stack_Names;


   begin  -- Stack_Bound_Clause

      Skip (Stack_Tok);

      -- Identify the Stack and its Index:

      if Current_Token = String_Tok then
         -- We have a stack-name.

         declare

            Stack_Name : constant String := Quoted_String;

         begin

            Stack := Programs.Stack_By (
               Name   => Stack_Name,
               Within => Names.Program);
            -- May propagate No_Such_Stack.

            Index := Programs.Index (Stack);

         exception

         when Programs.No_Such_Stack =>

            Error (
                 "The program has no stack named "
               & Current_Lexeme
               & '.');

            -- Index remains at 1.

            List_Stack_Names;

         end;

         Skip (String_Tok);

      else
         -- No stack-name, so there should be only one stack
         -- in the program, with Index = 1 as it already is.

         case Programs.Number_Of_Stacks (Names.Program) is

         when 0 =>

            Error (
               Source => Source,
               Text   => "The program has no stacks.");

         when 1 =>

            Stack := Programs.Stack_By (
               Index  => Index,
               Within => Names.Program);

         when others =>

            Error ("Stack-name required.");

         end case;

      end if;

      -- The asserted facts:

      loop

         if Current_Token = Usage_Tok then

            Get_Value;

            if Storage.Bounds.Unlimited (Value.Min)
            or else Storage.Bounds.Value (Value.Min) < 0
            then

               Output.Warning (
                  Locus => Locus (Value_Source),
                  Text  => "Lower bound on stack usage set to zero.");

            end if;

            Assert (
               Kind => "usage",
               Fact => (
                  Kind        => Stack_Usage,
                  Source      => Value_Source,
                  Stack_Index => Index,
                  Stack_Value => Value),
               Set => Set_Usage);

         elsif Current_Token = Final_Tok then

            Get_Value;

            if       Stack_Valid
            and then Programs.Kind (Stack) = Programs.Stable
            and then Value /= Storage.Bounds.Exactly_Zero
            then

               Error (
                  Source => Value_Source,
                  Text   =>
                       "Final height of stable stack must be zero"
                     & Output.Field_Separator
                     & Programs.Name (Stack));

               Set_Final := True;

            else

               Assert (
                  Kind => "final height",
                  Fact => (
                     Kind        => Stack_Final,
                     Source      => Value_Source,
                     Stack_Index => Index,
                     Stack_Value => Value),
                  Set => Set_Final);

            end if;

         else

            exit;

         end if;

      end loop;

      -- Check the part for which the bound is asserted:

      case Parts.Kind is

      when Program =>

         Error ("Stack bounds for the program are not allowed.");

      when Subprogram  | Call_Static | Call_Dynamic =>
         -- Ok.

         null;

      when Luup =>

         Error ("Stack bounds for a loop are not allowed.");

      when Instruction =>

         Error ("Stack bounds for an instruction are not allowed.");

      when others =>

         Error ("Stack bounds not allowed in this context.");

      end case;

      if Current_Token /= Semicolon_Tok then

         Expected ("""usage"" or ""final""");

      end if;

   end Stack_Bound_Clause;


   procedure Clause (
      Parts : in     Parts_Ref;
      Names : in     Names_T;
      Into  : in out Assertion_Bag_T)
   --
   -- An assertion Clause that applies to the given Parts,
   -- becoming an assertion that is put Into the bag.
   --
   is

      Source : constant Source_T := Token_Source;
      -- Capture the source position at the start.

      Tok : constant Token_T := Current_Token;
      -- The first token of the clause.


      procedure Assert (Fact : in Fact_T)
      is
      begin

         Assert (
            Thing => (
               Fact    => Fact,
               Parts   => Parts,
               Context => No_Context,
               Source  => Source),
            To => Into);

      end Assert;

      Ignored_Fact : Fact_T;
      -- An ignored (erroneous) "starts", "repeats", or "performs"
      -- bound for some part not compatible with this kind of bound.

   begin  -- Clause

      if First_Et_Bound (Tok) then

         Assert (Execution_Time_Bound (Parts.Kind));

      elsif First_Stack_Bound (Tok) then

         Stack_Bound_Clause (
            Parts => Parts,
            Names => Names,
            Into  => Into);

      elsif First_Rep_Bound (Tok) then

         case Parts.Kind is

         when Call_Static | Call_Dynamic =>

            Assert (Count_Bound (
               Part => Parts.Kind,
               Fact => Call_Executions));

         when Instruction =>

            Assert (Count_Bound (
               Part => Parts.Kind,
               Fact => Instruction_Executions));

         when Luup =>

            Assert (Count_Bound (
               Part => Parts.Kind,
               Fact => Loop_Repetitions));

         when others =>

            Error ("Repetition bounds not allowed in this context.");

            Ignored_Fact := Count_Bound (
               Part => Parts.Kind,
               Fact => Loop_Repetitions);

         end case;

      elsif First_Start_Bound (Tok) then

         case Parts.Kind is

         when Luup =>

            Assert (Count_Bound (
               Part => Parts.Kind,
               Fact => Loop_Starts));

         when others =>

            Error ("Start bounds apply only to loops.");

            Ignored_Fact := Count_Bound (
               Part => Parts.Kind,
               Fact => Loop_Starts);

         end case;

      elsif First_Var_Bound (Tok) then

         Assert (Variable_Bound (Names, Parts.Kind, Always));

      elsif First_Inv_Bound (Tok) then

         Assert (Variable_Invariance (Names, Parts.Kind));

      elsif First_Prop_Bound (Tok) then

         Assert (Property_Bound (Parts.Kind));

      elsif First_Role_Bound (Tok) then

         case Parts.Kind is

         when Instruction =>

            Assert (Role_Bound (Parts.Kind));

         when others =>

            Error ("Role bounds (""performs"") apply only to instructions.");

            Ignored_Fact := Role_Bound (Parts.Kind);

         end case;

      else

         Expected ("Clause");

      end if;

      if Current_Token = Semicolon_Tok then

         Skip (Semicolon_Tok);

      else

         Raise_Error ("Semicolon expected after clause");

      end if;

   end Clause;


   procedure Variable_Bounds (
      Parts : in     Parts_Ref;
      Span  : in     Fact_Span_T;
      Names : in     Names_T;
      Into  : in out Assertion_Bag_T)
   --
   -- Parses a semicolon-separated list of variable bound assertions.
   -- The last one may or may not be followed by a semicolon.
   --
   -- The span of these facts is defined by the context.
   --
   is

      Source : Source_T;
      -- The place of a variable bound.

   begin

      while First_Var_Bound (Current_Token) loop

         Source := Token_Source;

         Assert (
            Thing => (
               Fact    => Variable_Bound (Names, Parts.Kind, Span),
               Parts   => Parts,
               Context => No_Context,
               Source  => Source),
            To => Into);

         exit when Current_Token /= Semicolon_Tok;

         Skip;

      end loop;

   end Variable_Bounds;


   --
   ---   Declarations for mutual recursion
   --
   --
   -- See the subprogram bodies for more description.


   procedure Call_Description (
      Names    : in     Names_T;
      Base     : in     Base_Point_T;
      Dynamic  : in     Boolean;
      Can_That : in     Boolean;
      Features : in out Unbounded_Feature_List_T);


   procedure Loop_Description (
      Names    : in     Names_T;
      Base     : in     Base_Point_T;
      Can_That : in     Boolean;
      Features : in out Unbounded_Feature_List_T);


   --
   ---   Explicit features
   --


   function Source_Line (
      Base         : Base_Point_T;
      Default_Fuzz : Source_Fuzz_T)
   return Source_Position_T
   --
   -- A source-code line:
   --
   --    'line' <line number>
   -- or
   --    'line' 'offset' <line offset>
   -- or
   --    'marker' <symbol>
   --
   -- The Default_Fuzz is used for the Fuzz.
   -- The source File is returned as null.
   --
   is

      Offset : Boolean;
      -- Whether an 'offset' is given.

      Base_Line : Line_Number_T;
      -- The Base.Line if defined.

      Number : Integer;
      -- The line number or offset, as given.

      Line_Number : Line_Number_T;
      -- The line number, validated to be in range.

   begin

      if Current_Token = Line_Tok then
         -- 'line' ['offset'] <number>

         Skip;

         Offset := Current_Token = Offset_Tok;

         if Offset then

            case Base.Line_Status is

            when Defined =>

               Base_Line := Base.Line;

            when Not_Defined =>

               Error ("Context provides no base for line-number offset.");

            when Useless =>

               null;

            end case;

            Skip;

         end if;

         Number := Integer_Values.Literal;

         begin

            if not Offset then
               -- The Number is itself the line number.

               Line_Number := Line_Number_T (Number);

            elsif Base.Line_Status = Defined then
               -- The Number is an offset to the Base_Line.

               Line_Number := Line_Number_T (Integer (Base_Line) + Number);

            else
               -- The Number is an offset, but the Base_Line is unknown,
               -- and the Line_Number will not be used.

               Line_Number := Line_Number_T'First;

            end if;

         exception

         when Constraint_Error =>

            Error (
                 "The number"
               & Integer'Image (Number)
               & " is not a valid source-line number or offset.");

            Line_Number := Line_Number_T'First;

         end;

         Skip;
         -- The integer literal.

         return (
            Kind        => Line,
            File        => Symbols.Null_Name,
            Fuzz        => Default_Fuzz,
            Line_Number => Line_Number,
            Offset      => Offset);

      elsif Current_Token = Marker_Tok then
         -- 'marker' <symbol>

         Skip;

         return (
            Kind   => Mark,
            File   => Symbols.Null_Name,
            Fuzz   => Default_Fuzz,
            Marker => To_Item (Quoted_String_Skip));

      else

         Expected ("""line"" or ""marker""");
         -- Propagates Input_Error.

         -- Never reached:

         raise Program_Error;

      end if;

   end Source_Line;


   function Fuzz (Min, Max : Integer) return Source_Fuzz_T
   is
      use Arithmetic;
   begin

      return Interval (Min => Value_T (Min), Max => Value_T (Max));

   end Fuzz;


   procedure Source_File (
      Base     : in     Base_Point_T;
      Position : in out Source_Position_T)
   --
   -- Extends a source-position by a source-file name:
   --
   --    'in' ['file'] <file name as quoted string>
   --
   -- The current token is after the 'in'.
   --
   is
      use type Symbols.Source_File_Name_T;

      Name_Source : Source_T;
      -- The position of the file-name, in the assertion file.

   begin

      Skip (File_Tok);

      Name_Source := Token_Source;

      Position.File := Symbols.To_Source_File_Name (Quoted_String_Skip);

      if       Position.Kind = Line
      and then Position.Offset
      and then Position.File /= Base.File
      then

         Error (
            Source => Name_Source,
            Text   =>
                 "Base-point is in """ & Symbols.Image (Base.File)
               & """, not in """
               & Symbols.Image (Position.File)
               & """.");

      end if;

   end Source_File;


   procedure Source_Context (
      Base     : in     Base_Point_T;
      Relation : in     Source_Relation_T;
      Position : in out Source_Position_T)
   --
   -- Extends a source-position definition by an optional specific
   -- fuzz and and optional specific source-file name:
   --
   --    ['within' <bound>] ['in' <source file>]
   --
   -- The Relation is used only to warn about overriding the fuzz
   -- for an Exactly_On relation.
   --
   is

      File_Present : Boolean;
      -- Whether the 'in' <source file> part is present.

   begin

      -- Is the fuzz specified, and not defaulted?

      if If_Skip (Within_Tok) then
         -- 'within' <bound>

         if Relation = Exactly_On then

            Output.Warning (
               Locus => Token_Locus,
               Text  => "Overriding an ""exactly"" qualifier.");

         end if;

         Position.Fuzz := Source_Fuzz_T (Bound (
            Default_Min => Storage.Bounds.Not_Limited));

      end if;

      -- Comes there a <source file> too?

      File_Present := If_Skip (In_Tok);

      if File_Present then

         Source_File (Base, Position);

      else

         case Position.Kind is

         when Line =>

            if Base.Address_Status = Not_Defined then
               -- This source-position description uses a line-number
               -- in a global context, so it must include a source-file
               -- name, but no name is given.

               Error ("Context defines no source file for line number.");

            end if;

         when Mark =>
            -- All contexts can use markers without a source-file name.

            null;

         when Any =>
            -- Position.Kind cannot be Any at this point.

            Output.Fault (
               Location => "Assertions.Parser.Source_Context",
               Text     => "Position.Kind = Any");

         end case;

      end if;

   end Source_Context;


   procedure Source_Point (
      Base         : in     Base_Point_T;
      Negated      : in     Boolean;
      Relation     : in     Source_Relation_T;
      Default_Fuzz : in     Source_Fuzz_T;
      Features     : in out Unbounded_Feature_List_T)
   --
   -- The property that the source-code for the part has a certain
   -- positional Relationship to some point(s) in the source-code files.
   -- The syntax is:
   --
   --     <source line> <source context>
   --
   -- The current token is the first token of <source line>.
   --
   is

      Source : constant Source_T := Token_Source;
      -- The location in the assertion file.

      Position : Source_Position_T;
      -- The source-code position defined by <source line> <source context>.

   begin

      Position := Source_Line (Base, Default_Fuzz);

      Source_Context (Base, Relation, Position);

      Add (
         Feature => (
            Kind       => Sourced,
            Source     => Source,
            Negated    => Negated,
            Source_Rel => Relation,
            Source_Pos => Position),
         To => Features);

   end Source_Point;


   function Default_Fuzz (Relation : Source_Relation_T)
   return Source_Fuzz_T
   --
   -- The default fuzz implied by the given Relation.
   --
   is

      Z : constant Natural := Opt.Line_Fuzz;

   begin

      case Relation is
      when Before      => return Fuzz (-Z, 0);
      when On          => return Fuzz (-Z, Z);
      when Exactly_On  => return Fuzz ( 0, 0);
      when After       => return Fuzz ( 0, Z);
      when Contains
         | Spans       => return Fuzz ( 0, 0);
      end case;

   end Default_Fuzz;


   procedure Source_Position (
      Base     : in     Base_Point_T;
      Negated  : in     Boolean;
      Features : in out Unbounded_Feature_List_T)
   --
   -- The property that the source-code for the part has a certain
   -- positional Relationship to some point(s) in the source-code files.
   -- The syntax is one of the following:
   --
   --     ['exactly'] ('at'|'on') <source point>
   --     'after'  <source point>
   --     'before' <source point>
   --
   -- The current token is the first token.
   --
   is

      Relation : Point_Relation_T := On;
      -- The relationship that is defined.
      -- Initialized to the most common choice and to avoid
      -- an undefined value in case of syntax error.

   begin

      case Current_Token is

      when Exactly_Tok =>
         -- 'exactly' ('at'|'on')

         Relation := Exactly_On;

         Skip;

         if  Current_Token = At_Tok
         or  Current_Token = On_Tok
         then

            Skip;

         else

            Expected ("""at"" or ""on"" after ""exactly""");

         end if;

      when At_Tok | On_Tok =>

         -- Relation = On already.

         Skip;

      when After_Tok =>

         Relation := After;

         Skip;

      when Before_Tok =>

         Relation := Before;

         Skip;

      when others =>

         Expected ("""exactly"", ""at"", ""on"", ""after"" or ""before""");

      end case;

      Source_Point (
         Base         => Base,
         Negated      => Negated,
         Relation     => Relation,
         Default_Fuzz => Default_Fuzz (Relation),
         Features     => Features);

   end Source_Position;


   function Enclosed_Call_Description (
      Names      : Names_T;
      Base       : Base_Point_T;
      Can_That   : Boolean;
      Population : Storage.Bounds.Interval_T)
   return Parts_Ref
   --
   -- A call description that perhaps Can have "That" features:
   --
   --    [dynamic] <call description that perhaps can have "that" properties>
   --
   is

      Source : constant Source_T := Token_Source;
      -- Captures the place at the start.

      Dynamic : Boolean;
      -- Whether the call is a dynamic one.

      Calls : Parts_Ref;
      -- The result.

      Features : Unbounded_Feature_List_T;
      -- The properties of the calls.

   begin

      Dynamic := If_Skip (Dynamic_Tok);

      if Dynamic then

         Calls := new Parts_T'(
            Kind       => Call_Dynamic,
            Source     => Source,
            Predicate  => True_Predicate,
            Population => Population);
         -- The Predicate will be changed,

      else

         Calls := new Parts_T'(
            Kind       => Call_Static,
            Source     => Source,
            Predicate  => True_Predicate,
            Population => Population);
         -- The Predicate will be changed,

      end if;

      Call_Description (
          Names    => Names,
          Base     => Base,
          Dynamic  => Dynamic,
          Can_That => Can_That,
          Features => Features);

      Set_Predicate (Calls.all, Features);

      return Calls;

   end Enclosed_Call_Description;


   function Enclosed_Loop_Description (
      Names      : Names_T;
      Base       : Base_Point_T;
      Can_That   : Boolean;
      Population : Storage.Bounds.Interval_T)
   return Parts_Ref
   --
   -- A loop description that perhaps Can have "That" properties:
   --
   --    <loop description that perhaps can have "that" properties>
   --
   is

      Loops : Parts_Ref;
      -- The result.

      Features : Unbounded_Feature_List_T;
      -- The properties of the loop.

   begin

      Loops := new Parts_T'(
         Kind       => Luup,
         Source     => Token_Source,
         Predicate  => True_Predicate,
         Population => Population);
      -- The Predicate will be changed.

      Loop_Description (
         Names    => Names,
         Can_That => Can_That,
         Base     => Base,
         Features => Features);

      Set_Predicate (Loops.all, Features);

      return Loops;

   end Enclosed_Loop_Description;


   procedure In_Loop_Or_File (
      Names    : in     Names_T;
      Base     : in     Base_Point_T;
      Negated  : in     Boolean;
      Features : in out Unbounded_Feature_List_T)
   --
   -- The property that the part is in some loop, or in some source file:
   --
   --     ['('] <loop> [')']
   -- or
   --     <source file>
   --
   -- The current token is the word before the ['('] or <source file>,
   -- which is 'in' or 'is_in'; it is skipped, whatever it is.
   --
   -- The parentheses around the loop description are optional, but the
   -- loop can have its own "place" or "that" properties only if the
   -- parentheses are present.
   --
   -- Negated
   --    Whether there is a preceding negation (not).
   --
   is

      In_File : Source_Position_T;
      -- The property of being in a source file.

      File_Source : Source_T;
      -- The position of the <source file> def in the assertion file.

      Enclosed : Boolean;
      -- Whether the loop description is enclosed in parentheses.

      Outer_Loop : Parts_Ref;
      -- Describes the loop that should contain the part.

   begin

      Skip;

      if Current_Token = File_Tok
      or Current_Token = String_Tok
      then
         -- Being in a source file.

         File_Source := Token_Source;

         In_File := (
            Kind => Any,
            File => Symbols.Null_Name,
            Fuzz => Singleton (0));
         -- We will fill in the File momentarily.
         -- The Fuzz is irrelevant.

         Source_File (Base, In_File);

         Add (
            Feature => (
               Kind       => Sourced,
               Source     => File_Source,
               Negated    => Negated,
               Source_Rel => On,
               Source_Pos => In_File),
            To => Features);

      else
         -- Being in a loop.

         Enclosed := If_Skip (Left_Parenthesis_Tok);

         Outer_Loop := Enclosed_Loop_Description (
            Names      => Names,
            Base       => Base,
            Can_That   => Enclosed,
            Population => Just_One);

         if Enclosed then

            Expect_And_Skip (Right_Parenthesis_Tok, "Closing parenthesis");

         end if;

         Add (
            Feature => (
               Kind    => Within,
               Source  => Outer_Loop.Source,
               Negated => Negated,
               Whole   => Outer_Loop),
            To => Features);

      end if;

   end In_Loop_Or_File;


   procedure Is_Labelled (
      Names    : in     Names_T;
      Negated  : in     Boolean;
      Features : in out Unbounded_Feature_List_T)
   --
   -- The property that the part is (contains) a given symbolic
   -- instruction label.
   --     ( is [not] labelled | is_labelled ) "label"
   -- The current token is the one before the "label".
   --
   is

      Source : Source_T;
      -- Captures the source place at the start.

      Name : Name_String_T;
      -- The name of the label.

      Found : Boolean;
      -- Whether the Name is found in the symbol-table.

      Address : Processor.Code_Address_T;
      -- The address connected to the Name.

   begin

      Skip;

      Source := Token_Source;

      Name := To_Name (Quoted_String_Skip);

      Find_Label_Address (
         Symbol  => To_String (Name),
         Source  => Source,
         Names   => Names,
         Found   => Found,
         Address => Address);

      if Found then

         Add (
            Feature => (
               Kind          => Labelled,
               Source        => Source,
               Negated       => Negated,
               Label         => Name,
               Label_Address => Address),
            To => Features);

      -- else
      --     An error was reported in Find_Label_Address.

      end if;

   end Is_Labelled;


   procedure Point_Place (
      Names    : in     Names_T;
      Base     : in     Base_Point_T;
      Negated  : in     Boolean;
      Features : in out Unbounded_Feature_List_T)
   --
   -- The property that the part has a source position, either in
   -- the source code or in the control-flow structure, expressed
   -- as one of the following:
   --
   --    <source position>
   --    'marked' symbol <source context>
   --    'labelled' symbol
   --    'in' <source file>
   --    'in' ['('] <loop> [')']
   --
   -- The current token is at the start.
   --
   is

      Source : constant Source_T := Token_Source;
      -- Captures the source place at the start.

      Position : Source_Position_T;
      -- The 'marked' position.

   begin

      if Current_Token = In_Tok then
         -- 'in' loop or 'in' <source file>

         In_Loop_Or_File (Names, Base, Negated, Features);

      elsif Current_Token in Labelled_Tok_T then
         -- 'labelled' "label"

         Is_Labelled (Names, Negated, Features);

      elsif First_Source_Position (Current_Token) then
         -- 'on' 'line' <number>, etc.

         Source_Position (Base, Negated, Features);

      elsif Current_Token = Marked_Tok then
         -- 'marked' symbol <source context>

         Skip;

         Position := (
            Kind   => Mark,
            File   => Symbols.Null_Name,
            Fuzz   => Default_Fuzz (Opt.Marked_Relation),
            Marker => To_Item (Quoted_String_Skip));

         Source_Context (Base, Opt.Marked_Relation, Position);

         Add (
            Feature => (
               Kind       => Sourced,
               Source     => Source,
               Negated    => Negated,
               Source_Rel => Opt.Marked_Relation,
               Source_Pos => Position),
            To => Features);

      else

         Expected ("Place definition");

      end if;

   end Point_Place;


   procedure Contains_Something (
      Names    : in     Names_T;
      Base     : in     Base_Point_T;
      Negated  : in     Boolean;
      Features : in out Unbounded_Feature_List_T)
   --
   -- The property that the part contains some call(s) or loop(s)
   -- or source point(s):
   --
   --    contain(s) <Count> <possibly parenthesized call/loop description>
   --    contain(s) <source point>
   --
   -- On entry the current token is "contains", "contain", or "containing".
   --
   is

      Count : Storage.Bounds.Interval_T;
      -- The number of loops or calls that the part should contain.

      Enclosed : Boolean;
      -- Whether there are enclosing parentheses.

      Inner_Parts : Parts_Ref;
      -- The inner loop(s) or call(s) that the part should contain.

   begin

      Skip;
      -- The "contains", "contain", or "containing".

      if First_Source_Point (Current_Token) then
         -- contain(s) <source point>

         Source_Point (
            Base         => Base,
            Negated      => Negated,
            Relation     => Contains,
            Default_Fuzz => Singleton (0),
            Features     => Features);

      else
         -- contain(s) <Count> call/loop

         Count := Containee_Count;

         Enclosed := If_Skip (Left_Parenthesis_Tok);
         -- Whether there are enclosing parentheses.

         if First_Call_Descr (Current_Token) then

            Inner_Parts := Enclosed_Call_Description (
               Names      => Names,
               Base       => Base,
               Can_That   => Enclosed,
               Population => Count);

         elsif First_Loop_Descr (Current_Token) then

            Inner_Parts := Enclosed_Loop_Description (
               Names      => Names,
               Base       => Base,
               Can_That   => Enclosed,
               Population => Count);

         else

            Expected ("Call or loop description");

         end if;

         Add (
            Feature => (
               Kind    => Contains,
               Source  => Inner_Parts.Source,
               Negated => Negated,
               Parts   => Inner_Parts),
            To => Features);

         if Enclosed then

            Expect_And_Skip (
               Token  => Right_Parenthesis_Tok,
               Expect => "Closing parenthesis after call or loop");

         end if;

      end if;

   end Contains_Something;


   procedure Uses_Variable (
      Names    : in     Names_T;
      Negated  : in     Boolean;
      Features : in out Unbounded_Feature_List_T)
   --
   -- Parses a Uses feature and adds it to the Features.
   --
   -- On entry the current token is "uses" or "use".
   --
   is

      Source : constant Source_T := Token_Source;
      -- Capture the source ref.

   begin

      Skip;
      -- The "use" or "uses".

      Add (
         Feature => (
            Kind     => Uses,
            Source   => Source,
            Negated  => Negated,
            Variable => Variable_Name (Names)),
         To => Features);

   end Uses_Variable;


   procedure Defines_Variable (
      Names    : in     Names_T;
      Negated  : in     Boolean;
      Features : in out Unbounded_Feature_List_T)
   --
   -- Parses a Defines feature and adds it to the Features.
   --
   -- On entry the current token is "defines" or "define".
   --
   is

      Source : constant Source_T := Token_Source;
      -- Capture the source ref.

   begin

      Skip;
      -- The "defines" or "define".

      Add (
         Feature => (
            Kind     => Defines,
            Source   => Source,
            Negated  => Negated,
            Variable => Variable_Name (Names)),
         To => Features);

   end Defines_Variable;


   function Address_Or_Offset (Base : Base_Point_T)
   return Processor.Code_Address_T
   --
   -- A code address, specified absolutely or as an offset to the Base:
   --
   --    <address> | offset <offset>
   --
   is

      Offset : Processor.Code_Offset_T;
      -- The offset, if an offset is specified.

      Address : Processor.Code_Address_T;
      -- The result.

   begin

      if If_Skip (Offset_Tok) then
         -- offset <offset>

         if Base.Address_Status = Not_Defined then

            Error ("Context provides no base for code offset.");

         end if;

         Offset := Code_Offset (Quoted_String_Skip);

         case Base.Address_Status is

         when Defined =>

            Address := Processor.Shift (
               Base   => Base.Address,
               Offset => Offset);

         when Not_Defined | Useless =>

            Address := Processor.Code_Address_Default;
            -- Just to have some, possibly sensible, value for Address.

         end case;

      else
         -- Absolute address.

         Address := Code_Address (Quoted_String_Skip);

      end if;

      return Address;

   end Address_Or_Offset;


   procedure Executes_Address (
      Base     : in     Base_Point_T;
      Negated  : in     Boolean;
      Features : in out Unbounded_Feature_List_T)
   --
   -- The property that the part executes the instruction at
   -- a given address:
   --
   --     executes ( <address> | offset <offset> )
   --
   -- The current token is "executes" or an equivalent.
   --
   is

      Source : constant Source_T := Token_Source;
      -- Captures the source place at the start.

      Address : Processor.Code_Address_T;
      -- The address to be executed.

   begin

      Skip;
      -- The 'executes'.

      Address := Address_Or_Offset (Base);

      Add (
         Feature => (
            Kind    => Executes,
            Source  => Source,
            Negated => Negated,
            Address => Address),
         To => Features);

   end Executes_Address;


   procedure Property_Prefix (Negated : out Boolean)
   --
   -- A prefix for a loop or call property, containing some noise
   -- words and perhaps a significant number of 'nots':
   --
   --     ['that'] ['do'|'is'] [{'not'}]
   --
   --
   is
   begin

      Skip (That_Tok);

      if Current_Token in Does_Tok_T
      or Current_Token in Is_Tok_T
      then

         Skip;

      end if;

      Negated := False;

      while Current_Token = Not_Tok loop

         Negated := not Negated;

         Skip;

      end loop;

   end Property_Prefix;


   --
   ---   Call blocks
   --


   procedure Call_Property (
      Names    : in     Names_T;
      Base     : in     Base_Point_T;
      Features : in out Unbounded_Feature_List_T)
   --
   -- Parses a call property.
   --
   is

      Negated : Boolean;
      -- The logical sign of the property.

   begin  -- Call_Property

      Property_Prefix (Negated);

      if First_Point_Place (Current_Token) then

         Point_Place (Names, Base, Negated, Features);

      else

         case Current_Token is

         when Is_In_Tok =>    -- deprecated

            In_Loop_Or_File (Names, Base, Negated, Features);

         when Uses_Tok_T =>

            Uses_Variable (Names, Negated, Features);

            Error ("Calls do not have the ""uses"" property.");

         when Defines_Tok_T =>

            Defines_Variable (Names, Negated, Features);

            Error ("Calls do not have the ""defines"" property.");

         when others =>

            Expected ("Call properties");

         end case;

      end if;

   end Call_Property;


   procedure Call_Properties (
      Names    : in     Names_T;
      Base     : in     Base_Point_T;
      Features : in out Unbounded_Feature_List_T)
   --
   -- Parses a conjunction of call properties following "that".
   --
   is
   begin

      loop

         Call_Property (Names, Base, Features);

         exit when Current_Token /= And_Tok;

         Skip;
         -- The 'and'.

      end loop;

   end Call_Properties;


   procedure Call_Description (
      Names    : in     Names_T;
      Base     : in     Base_Point_T;
      Dynamic  : in     Boolean;
      Can_That : in     Boolean;
      Features : in out Unbounded_Feature_List_T)
   --
   -- Parses a call description and records the resulting Features.
   --
   -- For a static call:
   --
   --    ( call [to] | call_to ) "subprogram" [<call properties>]
   --
   -- For a dynamic call:
   --
   --    call [<call properties>]
   --
   -- A <call properties> part is allowed only if Can_That is true,
   -- which means that the call description is the top-level description
   -- of a call, or is enclosed in parentheses when used within the
   -- description of another program part.
   --
   -- Precondition: Current token is "call" or "call_to".
   --
   is

      Expect_Name : Boolean := not Dynamic;
      -- Whether to expect and parse the callee name.

      Callee_Name : Part_Name_T;
      -- The name of the callee.

      Callee_Feature : Feature_T;
      -- The feature of calling Callee_Name, the basic description
      -- of a static call.

   begin

      -- First token(s):

      if Current_Token = Call_To_Tok then
         -- Deprecated form.

         if Dynamic then

            Error ("Dynamic call cannot have ""call_to"".");

            Expect_Name := True;

         end if;

         Skip;
         -- The 'call_to'.

      elsif Current_Token in Calls_Tok_T then

         Skip;

         if Current_Token = To_Tok then
            -- The "to" is optional.

            if Dynamic then

               Error ("Dynamic call cannot have ""call to"".");

               Expect_Name := True;

            end if;

            Skip;
            -- The 'to'.

         end if;

      else

         Output.Fault (
            Location => "Assertions.Parser.Call_Description",
            Text     =>
                 "Current token is """
               & Tokenizer.Lexeme (Analyzer) & '"');

      end if;

      -- Callee name:

      if Expect_Name then

         Identify_Subprogram (
            Names       => Names,
            Warn_Absent => True,
            Sub_Name    => Callee_Name,
            Feature     => Callee_Feature);

         Add (
            Feature => Callee_Feature,
            To      => Features);

      end if;

      -- Features defined by "Place" and/or "That":

      if First_Call_Property (Current_Token) then

         if not Can_That then

            Error ("Properties are not allowed for this call.");

         end if;

         Call_Properties (Names, Base, Features);

      end if;

   end Call_Description;


   procedure Call_Block (
      Container  : in     Parts_Ref;
      Dynamic    : in     Boolean;
      Names      : in     Names_T;
      Base       : in     Base_Point_T;
      Population : in     Storage.Bounds.Interval_T;
      Into       : in out Assertion_Bag_T;
      Call_Part  :    out Parts_Ref)
   --
   -- A call-block, possibly for a Dynamic call, within some
   -- outer Container.
   --
   -- The current token is "call" or "calls". The population, if
   -- any, is handled by the outer parsing level.
   --
   -- The following assertions result:
   --
   -- > An assertion for each Clause contained in the call-block,
   --   applicable to the call(s) matching this call-block, that
   --   is to the Call_Part.
   --
   -- The Call_Part will be a Call_Static or a Call_Dynamic,
   -- depending on the Dynamic parameter.
   --
   -- The features of this Call_Part are:
   --
   -- > The callee name, if any and if not Dynamic.
   -- > Uses and Defines features (TBA implementation).
   -- > Being contained in the Container part if not null.
   -- > To match the given Population of calls.
   --
   -- Container
   --    The part(s) that contain this call-block; null if no
   --    container is specified (a global call-block).
   --    Not necessarily (and not usually) fully defined, because
   --    this call-block may be followed by other contained parts
   --    the contribute to the features of the Container.
   --
   is

      Features : Unbounded_Feature_List_T;
      -- The features of the Call_Part.

      Source : Source_T;
      -- Some place in the call block.

   begin

      if Dynamic then

         Call_Part := new Parts_T'(
            Kind       => Call_Dynamic,
            Source     => Token_Source,
            Predicate  => True_Predicate,
            Population => Population);
         -- The Predicate will be changed.

      else

         Call_Part := new Parts_T'(
            Kind       => Call_Static,
            Source     => Token_Source,
            Predicate  => True_Predicate,
            Population => Population);
         -- The Predicate will be changed.

      end if;

      -- The call is within the Container:

      if Container /= null then

         Add (
            Feature => (
               Kind    => Within,
               Source  => Token_Source,
               Negated => False,
               Whole   => Container),
            To => Features);

      end if;

      -- Other features from the call description:

      Call_Description (
         Names    => Names,
         Base     => Base,
         Dynamic  => Dynamic,
         Can_That => True,
         Features => Features);

      -- Clauses:

      loop

         if Current_Token in Calls_Tok_T then
            -- Asserting the callees of a dynamic call.

            if not Dynamic then

               Error ("Cannot assert callees for a static call.");

            end if;

            Source := Token_Source;

            Skip;
            -- The "call" or "calls".

            Assert (
               Thing => (
                  Fact    => Callee_Bound (Names),
                  Parts   => Call_Part,
                  Context => No_Context,
                  Source  => Source),
               To => Into);

            Expect_And_Skip (
               Token  => Semicolon_Tok,
               Expect => "Semicolon after callees");

         else
            -- Some other kind of fact.

            Clause (
               Parts => Call_Part,
               Names => Names,
               Into  => Into);

         end if;

         exit when Current_Token = End_Tok
            or     Current_Token = End_Call_Tok;  -- deprecated

         if not First_Clause (Current_Token) then

            Expected ("Clause or ""end call""");

         end if;

      end loop;

      -- The end:

      if Current_Token = End_Tok then

         Skip;
         -- The 'end'.

         if Current_Token in Calls_Tok_T then

            Skip;

         else

            Expected ("""call"" after ""end""");

         end if;

      else

         Expect_And_Skip (
            Token  => End_Call_Tok,
            Expect => """end call""");   -- or end_call (deprecated).

      end if;

      Set_Predicate (Call_Part.all, Features);

      Expect_And_Skip (
         Token  => Semicolon_Tok,
         Expect => "Semicolon after ""end call""");

   end Call_Block;


   --
   ---   Loop blocks
   --


   function Calls_To_Subprogram (Names : Names_T)
   return Parts_Ref
   --
   -- A (set of) static call(s) that is defined only by the identifier
   -- of the callee subprograms:
   --
   --   calls/call <subprogram name or address>
   --
   -- On entry the current token is "calls" or "call".
   --
   is

      Source : constant Source_T := Token_Source;
      -- Captures the place at the start.

      Name : Part_Name_T;
      -- The identifier name of the callee(s).

      Identifier : Feature_T;
      -- The identifying feature of the callee(s).
      -- This is either a Subprogram_Identity feature or a Named feature.

   begin

      Skip;
      -- The "calls" or "call".

      Identify_Subprogram (
         Names       => Names,
         Warn_Absent => True,
         Sub_Name    => Name,
         Feature     => Identifier);

      return new Parts_T'(
         Kind       => Call_Static,
         Source     => Source,
         Predicate  => new Part_Predicate_T'(1 => Identifier),
         Population => At_Least_One);

   end Calls_To_Subprogram;


   procedure Loop_Property (
      Names    : in     Names_T;
      Base     : in     Base_Point_T;
      Features : in out Unbounded_Feature_List_T)
   --
   -- Parses a loop property.
   --
   is

      Source : constant Source_T := Token_Source;
      -- Captures the source place at the start.

      Negated : Boolean;
      -- The logical sign of the property.

      Calls : Parts_Ref;
      -- The "calls"

   begin  -- Loop_Property

      Property_Prefix (Negated);

      if First_Point_Place (Current_Token) then

         Point_Place (Names, Base, Negated, Features);

      else

         case Current_Token is

         when Contains_Tok_T =>

            Contains_Something (Names, Base, Negated, Features);

         when Spans_Tok_T =>

            Skip;

            Source_Point (
               Base         => Base,
               Negated      => Negated,
               Relation     => Spans,
               Default_Fuzz => Singleton (0),
               Features     => Features);

         when Is_In_Tok =>    -- deprecated

            In_Loop_Or_File (Names, Base, Negated, Features);

         when Calls_Tok_T =>

            Calls := Calls_To_Subprogram (Names);

            Add (
               Feature => (
                  Kind    => Contains,
                  Source  => Source,
                  Negated => Negated,
                  Parts   => Calls),
               To => Features);

         when Uses_Tok_T =>

            Uses_Variable (Names, Negated, Features);

         when Defines_Tok_T =>

            Defines_Variable (Names, Negated, Features);

         when Is_Labelled_Tok =>   -- deprecated

            Is_Labelled (Names, Negated, Features);

         when Executes_Tok_T =>

            Executes_Address (Base, Negated, Features);

         when others =>

            Expected ("Loop property");

         end case;

      end if;

   end Loop_Property;


   procedure Loop_Properties (
      Names    : in     Names_T;
      Base     : in     Base_Point_T;
      Features : in out Unbounded_Feature_List_T)
   --
   -- Parses a conjunction of loop properties following "that".
   --
   is
   begin

      loop

         Loop_Property (Names, Base, Features);

         exit when Current_Token /= And_Tok;

         Skip;
         -- The 'and'.

      end loop;

   end Loop_Properties;


   procedure Loop_Description (
      Names    : in     Names_T;
      Base     : in     Base_Point_T;
      Can_That : in     Boolean;
      Features : in out Unbounded_Feature_List_T)
   --
   -- Parses a loop description:
   --
   --      loop [<loop properties>]
   --    | loop_that <loop properties>
   --
   -- A <loop properties> part is only allowed if Can_That is true,
   -- which means that the loop description is the top-level description
   -- of a loop, or is enclosed in parentheses when used within the
   -- description of another program part.
   --
   -- Precondition: Current token is loop or loop_that.
   --
   is

      procedure Check_We_Can_That
      is
      begin

         if not Can_That then

            Error ("Properties are not allowed for this loop.");

         end if;

      end Check_We_Can_That;


   begin  -- Loop_Description

      -- Leading keywords:

      if Current_Token = Loop_That_Tok then
         -- Deprecated form of introducing properties.

         Check_We_Can_That;

         Skip;
         -- The Loop_That_Tok.

         Loop_Properties (Names, Base, Features);

      elsif Current_Token not in Loop_Tok_T then

         Expected ("Loop description");

      else

         Skip;
         -- The Loop_Tok_T.

         if First_Loop_Properties (Current_Token) then

            Check_We_Can_That;

            Loop_Properties (Names, Base, Features);

         end if;

      end if;

   end Loop_Description;


   procedure Loop_Block (
      Container  : in     Parts_Ref;
      Names      : in     Names_T;
      Base       : in     Base_Point_T;
      Population : in     Storage.Bounds.Interval_T;
      Into       : in out Assertion_Bag_T;
      Loop_Part  :    out Parts_Ref)
   --
   -- A loop-block within some outer Container.
   --
   -- The current token is "loop" or "loops". The population, if
   -- any, is handled by the outer parsing level.
   --
   -- The following assertions result:
   --
   -- > An assertion for each Clause contained in the loop-block,
   --   applicable to the loop(s) matching this loop-block, that
   --   is to the Loop_Part.
   --
   -- The features of this Loop_Part are:
   --
   -- > Contained and container parts.
   -- > Uses and Defines features.
   -- > Being contained in the Container part if not null.
   -- > To match the given Population of loops.
   --
   -- Container
   --    The part(s) that contain this loop-block; null if no
   --    container is specified (a global loop-block).
   --    Not necessarily (and not usually) fully defined, because
   --    this loop-block may be followed by other contained parts
   --    the contribute to the features of the Container.
   --
   is

      Features : Unbounded_Feature_List_T;
      -- The features of the Loop_Part.

   begin

      Loop_Part := new Parts_T'(
         Kind       => Luup,
         Source     => Token_Source,
         Predicate  => True_Predicate,
         Population => Population);
      -- The Predicate will be changed.

      -- The loop is within the Container:

      if Container /= null then

         Add (
            Feature => (
               Kind    => Within,
               Source  => Token_Source,
               Negated => False,
               Whole   => Container),
            To => Features);

      end if;

      -- Other features from the loop description:

      Loop_Description (
         Names    => Names,
         Base     => Base,
         Can_That => True,
         Features => Features);

      -- Clauses:

      loop

         Clause (
            Parts => Loop_Part,
            Names => Names,
            Into  => Into);

         exit when Current_Token = End_Tok
            or     Current_Token = End_Loop_Tok;  -- deprecated

         if not First_Clause (Current_Token) then

            Expected ("Clause or ""end loop""");

         end if;

      end loop;

      -- The end:

      if Current_Token = End_Tok then

         Skip;
         -- The 'end'.

         if Current_Token in Loop_Tok_T then

            Skip;

         else

            Expected ("""loop"" after ""end""");

         end if;

      else

         Expect_And_Skip (
            Token  => End_Loop_Tok,
            Expect => """end loop""");   -- or end_loop (deprecated).

      end if;

      Set_Predicate (Loop_Part.all, Features);

      Expect_And_Skip (
         Token  => Semicolon_Tok,
         Expect => "Semicolon after ""end loop""");

   end Loop_Block;


   --
   ---   Populated call and loop blocks
   --


   procedure Loop_Or_Call (
      Container          : in     Parts_Ref;
      Names              : in     Names_T;
      Base               : in     Base_Point_T;
      Into               : in out Assertion_Bag_T;
      Container_Features : in out Unbounded_Feature_List_T)
   --
   -- Parses a loop-block or a call-block, perhaps preceded by
   -- a population bound.
   --
   -- Adds the assertion(s) resulting from the statement Into the
   -- assertion bag.
   --
   -- Moreover, if the statement implies some feature(s) of the
   -- containing part(s) and Opt.Implicit_Features is chosen, the
   -- procedure adds these to the Container's Features.
   --
   -- Precondition: Current token begins a Population, a call-block
   -- or a loop-block.
   --
   is

      Source : constant Source_T := Token_Source;
      -- Captures the place at the start.

      Population : Storage.Bounds.Interval_T;
      -- The population specified.

      Tok : Token_T;
      -- The token after the population.

      Dynamic : Boolean;
      -- Whether this is a dynamic call.

      Inner : Parts_Ref;
      -- The part (loop or call block) that will be contained
      -- in the Container.

   begin

      Population := Population_Bound;

      Tok := Current_Token;

      if First_Loop_Block (Tok) then

         Loop_Block (
            Container  => Container,
            Names      => Names,
            Base       => Base,
            Population => Population,
            Into       => Into,
            Loop_Part  => Inner);

      elsif First_Call_Block (Tok) then

         Dynamic := If_Skip (Dynamic_Tok);

         Call_Block (
            Container  => Container,
            Dynamic    => Dynamic,
            Names      => Names,
            Base       => Base,
            Population => Population,
            Into       => Into,
            Call_Part  => Inner);

      else

         Expected ("Loop or call");

         Inner := null;

      end if;

      if  Opt.Implicit_Features
      and Container /= null
      and Inner     /= null
      then

         Add (
            Feature => (
               Kind    => Contains,
               Source  => Source,
               Negated => False,
               Parts   => Inner),
            To => Container_Features);

      end if;

   end Loop_Or_Call;


   --
   ---   Instructions
   --


   procedure Instruction_Block (
      Container          : in     Parts_Ref;
      Names              : in     Names_T;
      Base               : in     Base_Point_T;
      Into               : in out Assertion_Bag_T;
      Container_Features : in out Unbounded_Feature_List_T)
   --
   -- Parses an instruction block.
   --
   -- Adds the assertion(s) resulting from the statement Into the
   -- assertion bag.
   --
   -- Moreover, if the statement implies some feature(s) of the
   -- containing part(s) and Opt.Implicit_Features is chosen, the
   -- procedure adds these to the Container's Features.
   --
   -- Precondition: Current token begins an instruction block.
   --
   is

      Source : constant Source_T := Token_Source;
      -- Captures the place at the start.

      Address : Processor.Code_Address_T;
      -- The address of the instruction.

      Instr_Part : Parts_Ref;
      -- The "instruction" part.

      Features : Unbounded_Feature_List_T;
      -- The features of the Instr_Part.

   begin

      Expect_And_Skip (Instruction_Tok, "instruction");

      Skip (At_Tok);

      Address := Address_Or_Offset (Base);

      Instr_Part := new Parts_T'(
         Kind       => Instruction,
         Source     => Source,
         Predicate  => True_Predicate,
         Population => At_Least_One,
         Address    => Address);

      -- The instruction is within the Container:

      if Container /= null then

         Add (
            Feature => (
               Kind    => Within,
               Source  => Source,
               Negated => False,
               Whole   => Container),
            To => Features);

      end if;

      -- Clauses:

      loop

         Clause (
            Parts => Instr_Part,
            Names => Names,
            Into  => Into);

         exit when Current_Token = End_Tok;

         if not First_Clause (Current_Token) then

            Expected ("Clause or ""end instruction""");

         end if;

      end loop;

      -- The end:

      Skip (End_Tok);

      if Current_Token = Instruction_Tok then

         Skip;

      else

         Expected ("""instruction"" after ""end""");

      end if;

      Set_Predicate (Instr_Part.all, Features);

      Expect_And_Skip (
         Token  => Semicolon_Tok,
         Expect => "Semicolon after ""end instruction""");

      if  Opt.Implicit_Features
      and Container  /= null
      and Instr_Part /= null
      then

         Add (
            Feature => (
               Kind    => Contains,
               Source  => Source,
               Negated => False,
               Parts   => Instr_Part),
            To => Container_Features);

      end if;

   end Instruction_Block;


   --
   ---   Statements
   --


   procedure Statement (
      Container          : in     Parts_Ref;
      Names              : in     Names_T;
      Base               : in     Base_Point_T;
      Into               : in out Assertion_Bag_T;
      Container_Features : in out Unbounded_Feature_List_T)
   --
   -- Parses a statement, being either a loop-block, a call-block,
   -- an instruction-block, or a clause. The loop-block or call-block
   -- can be preceded by a population bound.
   --
   -- Adds the assertion(s) resulting from the statement Into the
   -- assertion bag.
   --
   -- Moreover, if the statement implies some feature(s) of the
   -- containing part(s) and Opt.Implicit_Features is chosen, the
   -- procedure adds these to the Container's Features.
   --
   -- Precondition: Current token begins a Statement.
   --
   is

      Tok : constant Token_T := Current_Token;
      -- The current token.

   begin

      if First_Loop_Or_Call (Tok) then

         Loop_Or_Call (
            Container          => Container,
            Names              => Names,
            Base               => Base,
            Into               => Into,
            Container_Features => Container_Features);

      elsif First_Instruction_Block (Tok) then

         Instruction_Block (
            Container          => Container,
            Names              => Names,
            Base               => Base,
            Into               => Into,
            Container_Features => Container_Features);

      elsif First_Clause (Tok) then

         Clause (
            Parts => Container,
            Names => Names,
            Into  => Into);

      else

         Output.Fault (
            Location => "Assertions.Parser.Statement",
            Text     =>
                 "Current token is """
               & Tokenizer.Lexeme (Analyzer) & '"');

         Skip;

      end if;

   end Statement;


   --
   ---   Subprogram blocks
   --


   procedure Sub_Option (
      Sub  : in     Parts_Ref;
      Into : in out Assertion_Bag_T)
   --
   -- Parses a subprogram option.
   --
   -- Precondition: Current token is the start of a valid option.
   --
   is

      Source : constant Source_T := Token_Source;
      -- Captures the place at the start.

      Value : Boolean := True;
      -- May be changed by "no" tokens.

      Tok : Token_T;
      -- The current token.


      procedure Assert_Option (Fact : in Fact_T)
      --
      -- Asserts an option.
      --
      is
      begin

         Assert (
            Thing => (
               Fact    => Fact,
               Parts   => Sub,
               Context => No_Context,
               Source  => Fact.Source),
            To => Into);

      end Assert_Option;


   begin  -- Sub_option

      -- All the negations if any:

      loop

         Tok := Current_Token;

         exit when Tok not in Nix_Token_T;

         Value := not Value;

         Skip;

      end loop;

      -- Then the option itself:

      case Tok is

      when Arithmetic_Tok =>

         Assert_Option (Fact => (
            Kind           => Subprogram_Arithmetic,
            Source         => Source,
            Use_Arithmetic => Value));

         Skip;

      when Enough_Tok =>

         Skip;

         Expect_And_Skip (
            Token  => For_Tok,
            Expect => """for"" after ""enough""");

         if Current_Token = Time_Tok then

            Assert_Option (Fact => (
               Kind            => Subprogram_Enough_For_Time,
               Source          => Source,
               Enough_For_Time => Value));

            Skip;

         else

            Expected ("""time""");

         end if;

      when Integrate_Tok =>

         if not Value then

            Error ("Integrated analysis cannot be negated.");

         end if;

         Assert_Option (Fact => (
            Kind   => Subprogram_Integrate,
            Source => Source));

         Skip;

      when Hide_Tok =>

         Assert_Option (Fact => (
            Kind   => Subprogram_Hide,
            Source => Source,
            Hide   => Value));

         Skip;

      when Omit_Tok =>

         if Value then
            -- Can assert "omit".

            Assert_Option (Fact => (
               Kind   => Subprogram_Omit,
               Source => Source));

            Skip;

         else
            -- Cannot assert "not omit".

            Error ("The ""omit"" property cannot be negated.");

         end if;

      when Returns_Tok_T =>

         if not Value then
            -- The subprogram does not return.

            Assert_Option (Fact => (
               Kind          => Return_Method,
               Source        => Source,
               Return_Method => (Way => Calling.No_Return)));

            Skip;

         else
            -- The subprogram does return; the question
            -- is how it returns.

            Skip;  -- the Returns_Tok_T.

            Tok := Current_Token;

            case Tok is

            when Semicolon_Tok | Normal_Token_T =>
               -- The subprogram returns normally.

               Assert_Option (Fact => (
                  Kind          => Return_Method,
                  Source        => Source,
                  Return_Method => (Way => Calling.Normal_Return)));

               if Tok in Normal_Token_T then

                  Skip;

               end if;

            when To_Tok | Offset_Tok =>

               Skip (To_Tok);  -- an optional "to".

               case Current_Token is

               when Offset_Tok =>

                  Skip;

                  Assert_Option (Fact => (
                     Kind          => Return_Method,
                     Source        => Source,
                     Return_Method => (
                        Way    => Calling.Offset_Return,
                        Offset => Code_Offset (Quoted_String_Skip))));

               when others =>

                  Expected ("""offset""");

               end case;

            when others =>

               Expected ("""normally"" or ""[to] offset""");

            end case;

         end if;

      when Unused_Tok =>

         if Value then
            -- Can assert "unused".

            Assert_Option (Fact => (
               Kind   => Subprogram_Unused,
               Source => Source));

            Skip;

         else
            -- Cannot assert "not unused".

            Error ("The ""unused"" property cannot be negated.");

         end if;

      when Used_Tok =>

         if not Value then
            -- Can assert "not used", same as "unused".

            Assert_Option (Fact => (
               Kind   => Subprogram_Unused,
               Source => Source));

            Skip;

         else
            -- Cannot assert "used".

            Error ("The ""used"" property cannot be asserted.");

         end if;

      when No_Arithmetic_Tok =>
         -- This form is deprecated.

         Assert_Option (Fact => (
            Kind           => Subprogram_Arithmetic,
            Source         => Source,
            Use_Arithmetic => not Value));

         Skip;

      when others =>

         Output.Fault (
            Location => "Assertions.Parser.Sub_Option",
            Text     =>
                 "Current token is """
               & Tokenizer.Lexeme (Analyzer) & '"');

      end case;

      Expect_And_Skip (
         Token  => Semicolon_Tok,
         Expect => "Semicolon after subprogram option");

   end Sub_Option;


   function Match (This, That : Part_Name_T) return Boolean
   --
   -- Whether the two part-names (eg. subprogram names) match,
   -- that is, are identical except for the Source place in the
   -- assertion file.
   --
   is

      That_At_This : Part_Name_T := That;
      -- The second part-name with Source at the first part-name.

   begin

      That_At_This.Source := This.Source;

      return This = That_At_This;

   end Match;


   procedure Sub_Block (
      Names   : in     Names_T;
      Program : in     Programs.Program_T;
      Into    : in out Assertion_Bag_T)
   --
   -- Parse a subprogram block.
   --
   -- Precondition: Current token is "subprogram".
   --
   is
      use type Programs.Subprogram_T;

      Source : constant Source_T := Token_Source;
      -- Captures the source place at the start.

      Sub_Name : Part_Name_T;
      -- The "name" feature.

      Sub_Identifier : Feature_T;
      -- The identifying feature.

      Subprogram_Mark : Output.Nest_Mark_T;
      -- Marks the default output locus for the subprogram.

      Base : Base_Point_T;
      -- The base address for offsets is the entry point
      -- of the subprogram.

      Sub : Parts_Ref;
      -- The subprogram part.

      Sub_Features : Unbounded_Feature_List_T;
      -- The identifying features of the Sub parts.

      End_Name : Part_Name_T;
      -- The name, if any, after the "end".

   begin

      Expect_And_Skip (
         Token  => Subprogram_Tok,
         Expect => """subprogram""");

      -- The subprogram identifier:

      Identify_Subprogram (
         Names       => Names,
         Warn_Absent => Opt.Warn_Absent_Subprogram,
         Sub_Name    => Sub_Name,
         Feature     => Sub_Identifier);

      case Sub_Identifier.Kind is

      when Subprogram_Identity =>
         -- A single subprogram matches this identifier.

         Subprogram_Mark := Output.Nest (Programs.Locus (
            Sub_Identifier.Subprogram));

         Base := Base_Point_Of (Sub_Identifier.Subprogram);

      when Subprogram_Absent =>
         -- The subprograms that match this identifier are
         -- known not to exist.

         Subprogram_Mark := Output.Nest (Output.Locus (
            Call_Path => Plain_Image (Sub_Identifier.Name)));

         Base := Useless_Base_Point;

      when Named =>
         -- The subprograms that match this identifier are
         -- not yet known.

         Subprogram_Mark := Output.Nest (Output.Locus (
            Call_Path => Plain_Image (Sub_Identifier.Name)));

         Base := Undefined_Base_Point;

      when others =>

         Output.Fault (
            Location => "Assertions.Parser.Sub_Block",
            Text     =>
                 "Sub_Identifier.Kind = "
               & Feature_Kind_T'Image (Sub_Identifier.Kind));

         Base := Undefined_Base_Point;

      end case;

      Add (
         Feature => Sub_Identifier,
         To      => Sub_Features);

      Sub := new Parts_T'(
         Kind       => Own.Subprogram,
         Source     => Source,
         Predicate  => True_Predicate,
         Population => At_Least_One);
      -- The Predicate will be changed.

      -- Optional input bounds:

      if Current_Token = Left_Parenthesis_Tok then

         Skip;

         Variable_Bounds (
            Parts => Sub,
            Span  => Initially,
            Names => Names,
            Into  => Into);

         Expect_And_Skip (
            Token => Right_Parenthesis_Tok,
            Expect => "Closing parenthesis after parameters");

      end if;

      -- Statements and options:

      loop

         if First_Sub_Option (Current_Token) then

            Sub_Option (Sub, Into);

         elsif First_Statement (Current_Token) then

            Statement (
               Container          => Sub,
               Names              => Names,
               Base               => Base,
               Into               => Into,
               Container_Features => Sub_Features);

         else

            exit;

         end if;

      end loop;

      -- The end?
      --    [ end [subprogram] [ <sub_name> ] ';' ]

      if Current_Token = End_Tok then

         Skip;
         -- The 'end'.

         Skip (Subprogram_Tok);
         -- Optional.

         if First_Part_Name (Current_Token) then

            End_Name := Part_Name (Names);

            if not Match (Sub_Name, End_Name) then

               Error (
                    "Mismatched subprogram identifier """
                  & Plain_Image (End_Name)
                  & """ after ""end"".");

               Expected ('"' & Plain_Image (Sub_Name) & '"');

            end if;

         end if;

         Expect_And_Skip (
            Token  => Semicolon_Tok,
            Expect => "Semicolon after ""end subprogram""");

      else

         null;   -- deprecated

      end if;

      Set_Predicate (Sub.all, Sub_Features);

      Output.Unnest (Subprogram_Mark);

   end Sub_Block;


   --
   ---   Assertions as a whole file
   --


   procedure Skip_To_Assertion
   --
   -- Skips text until a token is found that can start an assertion,
   -- or until end of assertion file. This is am error-recovery
   -- action.
   --
   is

      Tok : Token_T;

   begin

      loop

         Tok := Current_Token;

         exit when First_Assertion(Tok) or Tok = End_of_File_Tok;

         Skip;

      end loop;

   end Skip_To_Assertion;


   Whole_Program : constant Parts_Ref := new Parts_T'(
      Kind       => Program,
      Source     => (File_Name => new String'(""), Line_Number => 1),
      Predicate  => True_Predicate,
      Population => At_Least_One);
   --
   -- A part for the whole program.


   procedure Assertions (
      Program  : in     Programs.Program_T;
      Into     : in out Assertion_Bag_T;
      Unbagged :    out Boolean)
   --
   -- The top-level structure of an assertion file is a sequence
   -- of zero or more of the following construct:
   --
   -- > scope delimiter
   -- > scope
   -- > variable bound
   -- > volatility
   -- > property bound
   -- > subprogram block
   -- > loop block
   -- > call block.
   --
   -- Each such construct, except for the scope delimiter, scope,
   -- and volatility, adds zero or more assertions Into the bag.
   -- Some constructs have direct effects, are not put Into the bag,
   -- but make Unbagged return True.
   --
   is

      Names : Names_T := (
         Program      => Program,
         Symbol_Table => Programs.Symbol_Table (Program),
         Prefix       => Null_Unbounded_String,
         Delimiter    => Symbols.Default_Delimiter);
      --
      -- The symbol table, the current value of the symbol prefix,
      -- and the current value of the scope delimiter.

      Global_Features : Unbounded_Feature_List_T;
      -- Implicit features of the Program, implied by global
      -- assertions. Unused and discarded.


      procedure Global (Fact : in Fact_T)
      --
      -- Asserts a global (program-level) Fact.
      --
      is
      begin

         Assert (
            Thing => Assertion_T'(
               Fact    => Fact,
               Parts   => Whole_Program,
               Context => No_Context,
               Source  => Fact.Source),
            To => Into);

         Expect_And_Skip (
            Token  => Semicolon_Tok,
            Expect => "Semicolon after global bound.");

      end Global;


      procedure Assertion
      --
      -- Parse an Assertion.
      --
      is

         Tok : constant Token_T := Current_Token;
         -- The first token of the assertion.

      begin

         Trace ("Parsing Assertion.");

         if First_Scope_Prefix (Tok) then

            Scope_Prefix (Prefix => Names.Prefix);

         elsif First_Scope_Delimiter (Tok) then

            Scope_Delimiter (Delimiter => Names.Delimiter);

         elsif First_Sub_Block (Tok) then

            Sub_Block (
               Names   => Names,
               Program => Program,
               Into    => Into);

         elsif First_Var_Bound (Tok) then

            Global (Variable_Bound (Names, Own.Program, Always));

         elsif First_Volatility (Tok) then

            Volatility (
               Names => Names,
               Valid => Unbagged);

            Expect_And_Skip (
               Token  => Semicolon_Tok,
               Expect => "Semicolon after volatility assertion.");

         elsif First_Prop_Bound (Tok) then

            Global (Property_Bound (Own.Program));

         elsif First_Loop_Or_Call (Tok) then

            Loop_Or_Call (
               Container          => null,
               Names              => Names,
               Base               => Undefined_Base_Point,
               Into               => Into,
               Container_Features => Global_Features);

            Erase (Global_Features);

         elsif First_Instruction_Block (Tok) then

            Instruction_Block (
               Container          => null,
               Names              => Names,
               Base               => Undefined_Base_Point,
               Into               => Into,
               Container_Features => Global_Features);

         else
            -- This token cannot start an assertion.

            Error ( "Assertion expected, at " & Current_Lexeme_Quoted);

            Skip_To_Assertion;

         end if;

      end Assertion;


   begin  -- Assertions

      Unbagged := False;

      begin

         -- Get the first token:

         Skip;

         -- Parse all the assertions:

         loop

            exit when Current_Token = End_of_File_Tok;

            Assertion;

         end loop;

         Trace ("End of assertion file.");

         if Opt.Trace_Parsing then

            Text.Put (Into);

         end if;

      exception

      when Invalid_Text =>
         -- The current "token" is not recognized as any valid
         -- token (it is Invalid_Tok).

         Error (
             "Text at or after column "
            & Token_Column
            & " not understood: "
            & Current_Lexeme_Quoted);

      end;

      if Errors then

         Output.Error ("Assertion file contained errors.");

      end if;

   end Assertions;


   procedure Parse_File (
      File_Name : in     String;
      Program   : in     Programs.Program_T;
      Into      : in out Own.Assertion_Bag_T;
      Valid     :    out Boolean)
   is

      Tally_Before : constant Natural := Length (Into);
      -- The nuber of assertions we have, before parsing this file.

      Unbagged : Boolean := False;
      -- Whether there are valid assertions that are not entered Into
      -- the assertion bag.

      File : aliased Ada.Text_IO.File_Type;
      -- The assertion file.

      File_Mark : Output.Nest_Mark_T;
      -- Marks the default output locus for the source-file name,
      -- which is here set to the assertion-file name.

   begin

      Errors := False;

      Valid := False;
      -- We are very skeptical but may be convinced...

      File_Mark := Output.Nest (Output.Locus (
         Source_File => File_Name));
      --
      -- The "source file" field of basic-output records will
      -- indicate the assertion-file for all messages that result
      -- from parsing the assertions.

      begin

         if Opt.Trace_Parsing then

            Output.Trace ("Opening assertion file.");

         end if;

         Ada.Text_IO.Open (
            File => File,
            Mode => Ada.Text_IO.In_File,
            Name => File_Name);

         -- The File is open and readable.

         Token_File_Name := new String'(File_Name);

         Feeder := OpenToken.Text_Feeder.Text_IO.Create (
            File'Unchecked_Access);

         Analyzer := Tokenizer.Initialize (
            Language_Syntax => Lexer.Syntax,
            Default         => Invalid_Tok,
            Feeder          => Feeder'Access);

         -- Parse:

         Assertions (
            Program  => Program,
            Into     => Into,
            Unbagged => Unbagged);

         -- Check and close:

         if  Length (Into) <= Tally_Before
         and not Unbagged
         then

            Output.Warning ("No valid assertions found in this file.");

         end if;

         -- The Token_File_Name is not discarded because it is
         -- probably referenced from the Source component of
         -- many assertion objects.

         Ada.Text_IO.Close (File);

      exception

      when Ada.Text_IO.Name_Error =>

         Output.Error ("Assertion file was not found.");
         --
         -- We should not use the local procedure Error because it
         -- takes its locus from Token_Locus which is undefined here.

         Errors := True;

      when Ada.Text_IO.Use_Error =>

         Output.Error ("Assertion file could not be read.");
         --
         -- We should not use the local procedure Error because it
         -- takes its locus from Token_Locus which is undefined here.

         Errors := True;

      when Ada.Text_IO.Device_Error =>

         Output.Error ("Assertion file is not a text file.");
         --
         -- We should not use the local procedure Error because it
         -- takes its locus from Token_Locus which is undefined here.

         Errors := True;

      end;

      Valid := not Errors;

      Output.Unnest (File_Mark);

   end Parse_File;


end Assertions.Parser;
