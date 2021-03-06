-------------------------------------------------------------------------------
--
-- Copyright (C) 1999 FlightSafety International and Ted Dennison
--
-- This file is part of the OpenToken package.
--
-- The OpenToken package is free software; you can redistribute it and/or
-- modify it under the terms of the  GNU General Public License as published
-- by the Free Software Foundation; either version 2, or (at your option)
-- any later version. The OpenToken package is distributed in the hope that
-- it will be useful, but WITHOUT ANY WARRANTY; without even the implied
-- warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
-- GNU General Public License for  more details.  You should have received
-- a copy of the GNU General Public License  distributed with the OpenToken
-- package;  see file GPL.txt.  If not, write to  the Free Software Foundation,
-- 59 Temple Place - Suite 330,  Boston, MA 02111-1307, USA.
--
-- As a special exception,  if other files  instantiate  generics from this
-- unit, or you link this unit with other files to produce an executable,
-- this unit does not by itself cause the resulting executable to be
-- covered by the GNU General Public License.  This exception does not
-- however invalidate any other reasons why the executable file might be
-- covered by the GNU Public License.
--
-- Maintainer: Ted Dennison (dennison@telepath.com)
--
-- This software was originally developed by the following company, and was
-- released as open-source software as a service to the community:
--
--           FlightSafety International Simulation Systems Division
--                    Broken Arrow, OK  USA  918-259-4000
--
-- Update History:
-- $Log: opentoken-token-enumerated-analyzer.ads,v $
-- Revision 1.1.1.1  2000-08-12 13:49:25  niklas
-- Import OpenToken 3.0b.
--
-- Revision 1.1  2000/08/12 13:49:25  Ted
-- moved from opentoken-token-analyzer
--
-- Revision 1.3  2000/02/05 04:00:22  Ted
-- Added End_Of_Text to support analyzing binaries.
--
-- Revision 1.2  2000/01/27 20:55:48  Ted
-- A token (lexical) analyzer
--
-- Revision 1.1  1999/12/28 00:57:15  Ted
-- renamed opentoken-analyzer to opentoken-token-analyzer
--
-- Revision 1.2  1999/12/27 19:55:58  Ted
-- fix file contents to work w/ new hierarchy
--
-- Revision 1.1  1999/12/27 17:11:31  Ted
-- renamed everything to new hierarchy
--
-- Revision 1.4  1999/10/08 22:47:33  Ted
-- Add default token functionality
--
-- Revision 1.3  1999/08/17 03:07:54  Ted
-- Add log line
--
-------------------------------------------------------------------------------

with Ada.Characters.Latin_1;
with Ada.Text_IO;
with Ada.Strings.Bounded;
with OpenToken.Recognizer;
with OpenToken.Text_Feeder;
with OpenToken.Text_Feeder.Text_IO;

-------------------------------------------------------------------------------
-- This package implements a mostly full-strength tokenizer (or lexical
-- analyizer).
--
-- To use it, create a function to feed text strings into the Analyzer. Create
-- an enumerated type of all the tokens you want to recognize. Instantiate this
-- package with the function and the enumerated type.
--
-- Next, define a token subclass for each token in Tokens. Then create a Syntax
-- which matches up the tokens to their appropriate token class and pass it
-- into Set_Syntax.
--
-- Once that is done, you may repeatedly call Get_Next to get tokens.
-------------------------------------------------------------------------------
generic

   Last_Terminal : in Token_ID := Token_ID'Last;

package OpenToken.Token.Enumerated.Analyzer is

   -- I'd prefer to use full named notation to get at this type, but Gnat has
   -- some bizzare bug where some instantations of this package can't see the
   -- parent package's name.
   subtype Enumerated_Class is Class;

   subtype Terminal_ID is Token_ID range Token_ID'First..Last_Terminal;

   -- Descriptor for what an individual token in this language looks like.
   type Recognizable_Token is record
      Recognizer   : Recognizer_Handle;
      Token_Handle : Handle;
   end record;

   -- The syntax of a language, which is defined by the set of valid tokens.
   type Syntax is array (Terminal_Id) of Recognizable_Token;

   type Instance is new Source with private;

   -- Need to revisit token definitions or raise Max_String_Length
   Token_Too_Long : exception;

   ----------------------------------------------------------------------------
   -- The Text Feeder is an object that has a function for returning strings
   -- to the analyzer to process.
   -- The default (Input_Feeder) reads from Ada.Text_IO.Current_Input.
   ----------------------------------------------------------------------------
   type Text_Feeder_Ptr is access all OpenToken.Text_Feeder.Instance'Class;

   Input_Feeder : aliased OpenToken.Text_Feeder.Text_IO.Instance;

   ----------------------------------------------------------------------------
   -- Retrieve a new recognizable token, using the given token values.
   -- This is a convienence routine for more easliy creating Syntaxes. It will
   -- dynamicly allocate the memory for the recognizer and token.
   ----------------------------------------------------------------------------
   function Get (Recognizer : in OpenToken.Recognizer.Class;
                 New_Token  : in Enumerated_Class := Get
                ) return Recognizable_Token;


   ----------------------------------------------------------------------------
   -- Return an Analyzer with the given syntax and text feeder.
   ----------------------------------------------------------------------------
   function Initialize (Language_Syntax : in Syntax;
                        Feeder          : in Text_Feeder_Ptr := Input_Feeder'Access
                       ) return Instance;
   function Initialize (Language_Syntax : in Syntax;
                        Default         : in Terminal_Id;
                        Feeder          : in Text_Feeder_Ptr := Input_Feeder'Access
                       ) return Instance;

   ----------------------------------------------------------------------------
   -- Set the Analyzer's syntax to the given value.
   --
   -- Due to the accessability rules of Ada, you cannot create syntax objects
   -- in which the component tokens are declared at a deeper dynamic scope than
   -- the instantiation of this package using 'access on the tokens.
   -- 'Unchecked_Access is safe to use as long as the Analyzer does not have a
   -- longer lifetime than its tokens.
   ----------------------------------------------------------------------------
   procedure Set_Syntax (Analyzer : in out Instance; Language_Syntax : in Syntax);

   ----------------------------------------------------------------------------
   -- Set the analyzer's text feeder.
   ----------------------------------------------------------------------------
   procedure Set_Text_Feeder (Analyzer : in out Instance; Feeder : in Text_Feeder_Ptr);

   ----------------------------------------------------------------------------
   -- Set the analyzer's default token to the given ID.
   --
   -- If Find_Next can't find a matching token, it will set Token to this token
   -- id, instead of raising syntax error. The Lexeme in this situation will
   -- be contain all the contiguous characters that fail to match an token.
   -- In practice this will be much less efficient than an "error" token that
   -- explicitly matches unmatchable strings. But often those are quite
   -- difficult to construct.
   -- The default token will be checked for legitimate matches. If this is not
   -- the behavior you want, it would be best to use a token that can't match
   -- any legitimate string (eg: Opentoken.Recognizer.Nothing)
   ----------------------------------------------------------------------------
   procedure Set_Default (Analyzer : in out Instance;
                          Default  : in     Terminal_Id
                         );

   ----------------------------------------------------------------------------
   -- Reset the analyzer to have *no* default token ID. If Find_Next doesn't
   -- find a matching token, Syntax_Error will be raised.
   ----------------------------------------------------------------------------
   procedure Unset_Default (Analyzer : in out Instance);

   ----------------------------------------------------------------------------
   -- Locate the next token.
   --
   -- The next token will be the token that matches the *longest* sequence of
   -- characters before failing. Ties go to the token with the smallest
   -- Terminal_Id.
   --
   -- If Look_Ahead is set, the next token after the current one will be
   -- returned, but the current one will not be discarded. Subsequent
   -- Look_Ahead calls will return later and later tokens.
   --
   -- Raises Syntax_Error if no token could be found (unless there is a default
   -- token defined).
   ----------------------------------------------------------------------------
   procedure Find_Next (Analyzer   : in out Instance;
                        Look_Ahead : in     Boolean := False);

   ----------------------------------------------------------------------------
   -- Returns the current text line at which processing will resume. This is
   -- particularly useful for printing error messages when syntax errors are
   -- detected.
   ----------------------------------------------------------------------------
   function Line (Analyzer : in Instance) return Natural;

   ----------------------------------------------------------------------------
   -- Returns the current text column at which processing will resume. This is
   -- particularly useful for printing error messages when syntax errors are
   -- detected.
   ----------------------------------------------------------------------------
   function Column (Analyzer : in Instance) return Natural;

   ----------------------------------------------------------------------------
   -- Returns True if the next token will be at the start of its text line.
   -- The main purpose of this routine is to assist in writing recognizers
   -- for tokens that must start a line.
   ----------------------------------------------------------------------------
   function First_Column (Analyzer : in Instance) return Boolean;

   ----------------------------------------------------------------------------
   -- Returns the column at which the the next token starts on its text line.
   -- The main purpose of this routine is to assist in writing recognizers
   -- for tokens that must start on a specific column
   ----------------------------------------------------------------------------
   function Next_Token_Column (Analyzer : in Instance) return Integer;

   ----------------------------------------------------------------------------
   -- Returns the last token that was matched.
   ----------------------------------------------------------------------------
   function Get (Analyzer : in Instance) return OpenToken.Token.Class;

   ----------------------------------------------------------------------------
   -- Returns the last token ID that was matched.
   ----------------------------------------------------------------------------
   function ID (Analyzer : in Instance) return Terminal_Id;

   ----------------------------------------------------------------------------
   -- Returns the actual text of the last token that was matched.
   ----------------------------------------------------------------------------
   function Lexeme (Analyzer : in Instance) return String;

   ----------------------------------------------------------------------------
   -- Returns the recognizer handle of the last token that was matched.
   ----------------------------------------------------------------------------
   function Last_Recognizer (Analyzer : in Instance) return Recognizer_Handle;

private

   type Token_List_Node;
   type Token_List_Node_Pointer is access Token_List_Node;

   -- Put all the Analyzer's state information in here, so there can be several
   -- Analyzers running at once.
   type Instance is new Source with record
      -- User-settable attributes
      Syntax_List   : Syntax;
      Feeder        : Text_Feeder_Ptr := Input_Feeder'access;
      Has_Default   : Boolean := False;
      Default_Token : Terminal_Id;

      -- User-gettable attributes
      Line        : Natural := 1;
      Column      : Natural := 1;
      Lexeme_Head : Natural := 1;
      Lexeme_Tail : Natural := 0;
      Last_Token  : Terminal_Id;

      -- Internal state information
      Buffer       : String (1..Max_String_Length);
      Buffer_Head  : Natural := 1;
      Buffer_Tail  : Natural := 0;
      Buffer_Size  : Natural := 0;

      Next_Line    : Natural := 1;
      Next_Column  : Natural := 1;

      Lookahead_Queue : Token_List_Node_Pointer;
      Lookahead_Tail  : Token_List_Node_Pointer;
   end record;

end OpenToken.Token.Enumerated.Analyzer;
