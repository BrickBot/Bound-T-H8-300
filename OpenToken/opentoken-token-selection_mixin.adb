-------------------------------------------------------------------------------
--
-- Copyright (C) 2000 Ted Dennison
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
-- $Log: opentoken-token-selection_mixin.adb,v $
-- Revision 1.1.1.1  2000-08-12 15:05:47  niklas
-- Import OpenToken 3.0b.
--
-- Revision 1.1  2000/08/12 15:05:47  Ted
-- A generic token that consists of a selection of one of several other tokens of the designated type
--
--
-------------------------------------------------------------------------------

with Ada.Exceptions;
with Ada.Tags;

-------------------------------------------------------------------------------
-- This package defines a reusable token for a simple selection between tokens.
-- These a quite easy to create yourself, of course. But having a prebuilt one
-- allows you to easily use it in constructors for other tokens.
-------------------------------------------------------------------------------
package body OpenToken.Token.Selection_Mixin is

   use type Token.Linked_List.List_Iterator;
   use type Token.Linked_List.Instance;

   ----------------------------------------------------------------------------
   -- Internal Helper routines

   ----------------------------------------------------------------------------
   -- Returns with the name of every token in the given list
   ----------------------------------------------------------------------------
   function Names_Of (List : Token.Linked_List.List_Iterator) return String is
      Next_Iteration : Token.Linked_List.List_Iterator := List;

      This_Name : constant String := Ada.Tags.External_Tag
        (Token.Linked_List.Token_Handle (List).all'Tag);
   begin
      -- Find the next token in the list.
      Token.Linked_List.Next_Token (Next_Iteration);

      -- Return with this token's external tag, along with the tags of the rest
      -- of the tokens in the list.
      if Next_Iteration = Token.Linked_List.Null_Iterator then
         return This_Name;
      else
         return This_Name & ", " & Names_Of (Next_Iteration);
      end if;

   end Names_Of;

   ----------------------------------------------------------------------------
   -- Externally visible routines
   --

   ----------------------------------------------------------------------------
   -- This routine is called when none of the sequence's tokens return true for
   -- Could_Parse_To.
   ----------------------------------------------------------------------------
   procedure Raise_Parse_Error
     (Match    : in out Instance;
      Analyzer : in out Source_Class;
      Actively : in     Boolean := True
     ) is
   begin
      if Actively then
         Ada.Exceptions.Raise_Exception
           (Parse_Error'Identity, "Unexpected token found. Expected one of " &
            Names_Of (Token.Linked_List.Initial_Iterator (Match.Members)) & ".");
      else
         -- Don't waste time since this is probably *not* an error condition in
         -- this mode, and it will probably be handled so no one will ever see
         -- the message anyway.
         raise Parse_Error;
      end if;
   end Raise_Parse_Error;

   ----------------------------------------------------------------------------
   -- Retrieve the given selection token from the analyzer.
   -- The private routine Build is called when the entire operation
   -- has been recognized.
   -- An a non active parse does not comsume any input from the analyzer,
   -- and does not call any of the private routines.
   ----------------------------------------------------------------------------
   procedure Parse
     (Match    : in out Instance;
      Analyzer : in out Source_Class;
      Actively : in     Boolean := True
     ) is

      List_Iterator : Token.Linked_List.List_Iterator :=
        Token.Linked_List.Initial_Iterator (Match.Members);
   begin

      while
        not Token.Could_Parse_To
        (Match    => Token.Linked_List.Token_Handle(List_Iterator).all,
         Analyzer => Analyzer)
      loop
         Token.Linked_List.Next_Token(List_Iterator);
         if List_Iterator = Token.Linked_List.Null_Iterator then
            Raise_Parse_Error (Match    => Match,
                               Analyzer => Analyzer,
                               Actively => Actively
                               );
         end if;

      end loop;

      Parse
        (Match    => Token.Linked_List.Token_Handle(List_Iterator).all,
         Analyzer => Analyzer,
         Actively => Actively
         );

      if Actively then
         Build (Match => Instance'Class(Match),
                From  => Component_Token'Class(Token.Linked_List.Token_Handle(List_Iterator).all)
                );
      end if;

   end Parse;

   ----------------------------------------------------------------------------
   -- Create a token selection from a pair of token instances.
   ----------------------------------------------------------------------------
   function "or" (Left  : access Component_Token'Class;
                  Right : access Component_Token'Class) return Instance is
      Result : Instance;
   begin
      Result.Members := OpenToken.Token.Handle(Left) & OpenToken.Token.Handle(Right);
      return Result;
   end "or";

   ----------------------------------------------------------------------------
   -- Create a token selection from a token handle and a token selection.
   ----------------------------------------------------------------------------
   function "or" (Left  : access Component_Token'Class;
                  Right : in     Instance) return Instance is
      Result : Instance;
   begin
      Result.Members := OpenToken.Token.Handle(Left) & Right.Members;
      return Result;
   end "or";
   function "or" (Left  : in     Instance;
                  Right : access Component_Token'Class) return Instance is
      Result : Instance;
   begin
      Result.Members := Left.Members & OpenToken.Token.Handle(Right);
      return Result;
   end "or";

   ----------------------------------------------------------------------------
   -- Create a token selection from a pair of selection tokens
   ----------------------------------------------------------------------------
   function "or" (Left  : in Instance;
                  Right : in Instance) return Instance is
      Result : Instance;
   begin
      Result.Members := Left.Members & Right.Members;
      return Result;
   end "or";

   ----------------------------------------------------------------------------
   -- This routine should is a quick check to verify that the given operation
   -- token can possibly succesfully parse from what's sitting in the analyzer.
   -- This routine is meant to be used for choosing between parsing options.
   -- It simply checks Could_Parse_To for this token's Element token.
   ----------------------------------------------------------------------------
   function Could_Parse_To
     (Match    : in Instance;
      Analyzer : in Source_Class
     ) return Boolean is

      List_Iterator : Token.Linked_List.List_Iterator :=
        Token.Linked_List.Initial_Iterator (Match.Members);
   begin

      while List_Iterator /= Token.Linked_List.Null_Iterator loop
         if Could_Parse_To
           (Match    => Token.Linked_List.Token_Handle(List_Iterator).all,
            Analyzer => Analyzer
            )
         then
            return True;
         end if;
         Token.Linked_List.Next_Token(List_Iterator);
      end loop;
      return False;
   end Could_Parse_To;

   ----------------------------------------------------------------------------
   -- This routine is called when an entire selection has been actively
   -- parsed.
   -- The default implementation does nothing.
   ----------------------------------------------------------------------------
   procedure Build (Match : in out Instance;
                    From  : in     Component_Token'Class) is
   begin
      null;
   end Build;

end OpenToken.Token.Selection_Mixin;
