-------------------------------------------------------------------------------
--
-- Copyright (C) 1999 Ted Dennison
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
-- Update History:
-- $Log: opentoken-token-enumerated.ads,v $
-- Revision 1.1.1.1  2000-08-12 14:11:13  niklas
-- Import OpenToken 3.0b.
--
-- Revision 1.1  2000/08/12 14:11:13  Ted
-- moved from opentoken-token
--
-- Revision 1.2  2000/01/27 20:59:37  Ted
-- Added some common routines.
--
-- Revision 1.1  1999/12/27 21:30:44  Ted
-- Initial Version
--
--
-------------------------------------------------------------------------------

with OpenToken.Recognizer;

-------------------------------------------------------------------------------
-- This package is the top of a generic hierarchy. Based on the list of IDs
-- it is instantiated with, a user can create tokens and token analyzers.
--
-- This package declares an type for designating a single token. It is
-- designed to be created by an instance of the Token.Analyzer class when a
-- particular kind of token is recognized.
--
-- Packages implementing a child of this type need to include a constructor for
-- the token analyzer and any nessecary utility routines their parser may
-- require.
-------------------------------------------------------------------------------
generic

   type Token_Id is (<>);

package OpenToken.Token.Enumerated is

   type Instance is new OpenToken.Token.Instance with private;

   subtype Class is Instance'Class;

   type Handle is access all Class;

   -- Recognizer handle type. Defined here to allow access's of objects
   -- declared at the same level as this package's instantiation.
   type Recognizer_Handle is access all Opentoken.Recognizer.Class;

   ----------------------------------------------------------------------------
   -- Get a token with the given ID.
   ----------------------------------------------------------------------------
   function Get (ID : in Token_ID := Token_ID'First) return Instance'Class;

   ----------------------------------------------------------------------------
   -- This procedure will be called when a token is recognized.
   --
   -- The Token's ID will be set to the given value. The Lexeme and Recognizer
   -- fields aren't used for this instance of the type. But they will be
   -- filled in by the analyzer.
   -- The recognizer is useful in creating tighly coupled pairs of tokens and
   -- recognizers. This allows communication of user-defined information
   -- global to the analyzer instance while maintaining overall re-entrancy.
   ----------------------------------------------------------------------------
   procedure Create (Lexeme     : in     String;
                     ID         : in     Token_ID;
                     Recognizer : in     Recognizer_Handle;
                     New_Token  :    out Instance);

   ----------------------------------------------------------------------------
   -- This function returns the ID of the token.
   -- This is made class-wide so it won't be overridable. That is done because
   -- some child packages access the ID directly, so overriding this routine
   -- would lead to inconsistent results.
   ----------------------------------------------------------------------------
   function ID (Token : in Instance'Class) return Token_ID;

   ----------------------------------------------------------------------------
   -- Set the given token's ID to the given value
   ----------------------------------------------------------------------------
   procedure Set_ID (Token : in out Instance'Class;
                    ID    : in     Token_ID);

   ----------------------------------------------------------------------------
   -- Implementation for a token parse routine.
   --
   -- The default version of this routine checks that the ID of the next token
   -- matches the Match ID.
   --
   -- An active parse consumes the input, where a non active parse does not.
   ----------------------------------------------------------------------------
   procedure Parse
     (Match    : in out Instance;
      Analyzer : in out Source_Class;
      Actively : in     Boolean := True
     );

   ----------------------------------------------------------------------------
   -- This routine should be a quick routine to verify that the given token
   -- can possibly succesfully parse. This routine is meant to be used for
   -- choosing between parsing options, so it should be a *very* quick check
   -- rather than a full parse.
   -- This version just checks against the currently loaded token on in the
   -- analyzer.
   ----------------------------------------------------------------------------
   function Could_Parse_To
     (Match    : in Instance;
      Analyzer : in Source_Class
     ) return Boolean;

   type Source is abstract new OpenToken.Token.Source with null record;

   ----------------------------------------------------------------------------
   -- Returns the actual text of the last token that was matched.
   ----------------------------------------------------------------------------
   function Lexeme (Analyzer : in Source) return String is abstract;

   ----------------------------------------------------------------------------
   -- Returns the recognizer handle of the last token that was matched.
   ----------------------------------------------------------------------------
   function Last_Recognizer (Analyzer : in Source) return Recognizer_Handle is abstract;

private
   type Instance is new OpenToken.Token.Instance with record
      ID : Token_ID;
   end record;

end OpenToken.Token.Enumerated;
