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
-- $Log: opentoken-recognizer-string.ads,v $
-- Revision 1.1.1.1  2000-02-05 03:58:46  niklas
-- Import OpenToken 3.0b.
--
-- Revision 1.3  2000/02/05 03:58:45  Ted
-- Fix escaped character support to use octal and hex values properly.
--
-- Revision 1.2  1999/12/27 19:56:04  Ted
-- fix file contents to work w/ new hierarchy
--
-- Revision 1.1  1999/12/27 17:11:40  Ted
-- renamed everything to new hierarchy
--
-- Revision 1.1  1999/08/17 02:48:57  Ted
-- Initial version
--
--
-- 1.1 - 8 August 1999  (C.K.W.Grein) Amended functionality
-- 1.0                  Original version
-------------------------------------------------------------------------------

with Ada.Strings.Maps;
with Ada.Characters.Latin_1;

-------------------------------------------------------------------------------
-- This package implements a token recognizer for a string literal. It can
-- optionally use an escape character to introduce special character mappings,
-- and can thus be used to recognize either Ada or C-style strings.
-------------------------------------------------------------------------------
package Opentoken.Recognizer.String is

   -- The following mapping from escape codes (the character after the
   -- backslash) to "escape" characters matches the mapping C uses.

   C_Style_Escape_Code_Map : constant Ada.Strings.Maps.Character_Mapping :=
     Ada.Strings.Maps.To_Mapping
     (From => "abfnrtv",
      To   => Ada.Characters.Latin_1.BEL &
              Ada.Characters.Latin_1.BS &
              Ada.Characters.Latin_1.FF &
              Ada.Characters.Latin_1.LF &
              Ada.Characters.Latin_1.CR &
              Ada.Characters.Latin_1.HT &
              Ada.Characters.Latin_1.VT);
   Java_Style_Escape_Code_Map : constant Ada.Strings.Maps.Character_Mapping :=
     Ada.Strings.Maps.To_Mapping
     (From => "bfnrt""'\",
      To   => Ada.Characters.Latin_1.BS &
              Ada.Characters.Latin_1.FF &
              Ada.Characters.Latin_1.LF &
              Ada.Characters.Latin_1.CR &
              Ada.Characters.Latin_1.HT &
              '"' & ''' & '\');

   type Instance is new Opentoken.Recognizer.Instance with private;

   ----------------------------------------------------------------------------
   -- This procedure will be called to create a String literal token.
   -- If Escapeable is set to False, the string will treat the Escape character
   -- as any other character.
   --
   -- If all parameters are defaulted, an Ada-style string token will be
   -- created.
   -- If Escapeable is set to True and all other parameters are defaulted,
   -- a C-style string token will be created.
   -- If Escapeable is set to True, Double_Delimiter set to false,
   -- Escape_Mapping uses Java_Style_Escape_Code_Map, the remaining parameters
   -- are defaulted, a Java-style string token will be created.
   ----------------------------------------------------------------------------
   function Get (Delimiter       : in Character := '"';
                 Double_Delimiter: in Boolean   := True;
                 Escapeable      : in Boolean   := False;
                 Escape          : in Character := '\';
                 Escape_Mapping  : in Ada.Strings.Maps.Character_Mapping := C_Style_Escape_Code_Map
                ) return Instance;

   ----------------------------------------------------------------------------
   -- Return the translated value of the recognized string. This will not
   -- include any quotation characters. The escape mapping will have been
   -- applied, and internal sets of double quotes will appear as a single
   -- double-quote character.
   ----------------------------------------------------------------------------
   function Value (Recognized_String : in Instance) return Standard.String;

private


   type State_ID is (Delimit, Text, Escaped_Text, Escaped_Octal_Number, First_Hex_Digit, Escaped_Hex_Number, Double_Delimit, Done);

   type Instance is new Opentoken.Recognizer.Instance with record

      -- The finite state machine state
      State           : State_ID := Delimit;
      Esc_Code        : Natural;

      -- The token settings
      Delimiter       : Character;
      Double_Delimiter: Boolean;
      Escapeable      : Boolean;
      Escape          : Character;
      Escape_Mapping  : Ada.Strings.Maps.Character_Mapping;

      -- The translated string value
      Value           : Standard.String (1..Max_String_Length);
      Value_Length    : Natural := 0;
      Good_Length     : Natural := 0;

   end record;

   ----------------------------------------------------------------------------
   -- This procedure will be called when analysis on a new candidate string
   -- is started. The Token needs to clear its state (if any).
   ----------------------------------------------------------------------------
   procedure Clear (The_Token : in out Instance);


   ----------------------------------------------------------------------------
   -- This procedure will be called to perform further analysis on a token
   -- based on the given next character.
   ----------------------------------------------------------------------------
   procedure Analyze (The_Token : in out Instance;
                      Next_Char : in     Character;
                      Verdict   : out    Analysis_Verdict);

end Opentoken.Recognizer.String;

