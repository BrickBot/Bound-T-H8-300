-------------------------------------------------------------------------------
--
-- Copyright (C) 1999 Christoph Karl Walter Grein
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
-- Maintainer: Christoph K. W. Grein (Christ-Usch.Grein@T-Online.de)
--
-- Update History:
--
-- $Log: opentoken-recognizer-based_integer_java_style.ads,v $
-- Revision 1.1.1.1  1999-12-27 19:56:00  niklas
-- Import OpenToken 3.0b.
--
-- Revision 1.2  1999/12/27 19:55:59  Ted
-- fix file contents to work w/ new hierarchy
--
-- Revision 1.1  1999/12/27 17:11:32  Ted
-- renamed everything to new hierarchy
--
-- Revision 1.2  1999/09/02 03:16:43  Ted
-- Add comments
--
-- Revision 1.1  1999/08/17 03:07:26  Christoph Grein
-- Initial Version
--
-------------------------------------------------------------------------------

with Opentoken.Recognizer.Extended_Digits;

-------------------------------------------------------------------------------
-- This package implements a token recognizer for a Java style based integer.
-- Such an integer consists of a base indication, followed by a sequence of
-- extended digits.
-- A base indication is either a leading 0 for an octal integer or leading 0x
-- for a hexadecimal integer.
-- An extended digit is either a decimal digit or a character in the range
-- 'a'..'f' in any letter case. The extended digit must be smaller than the
-- base.
-- When this recognizer and the one for integers are used together in the same
-- syntax, note that the integer recognizer must be created with leading zero
-- disallowed or a string like 07 is ambiguous.
-------------------------------------------------------------------------------
package Opentoken.Recognizer.Based_Integer_Java_Style is

  type Instance is new Opentoken.Recognizer.Instance with private;

  ----------------------------------------------------------------------------
  -- This procedure will be called to create a Based Integer token.
  ----------------------------------------------------------------------------
  function Get return Instance;

private

  type State_ID is (Base_0, Base_X, Numeral, Done);

  type Instance is new Opentoken.Recognizer.Instance with record

    -- The finite state machine state
    Numeral_Recognizer: Extended_Digits.Instance;
    State             : State_ID := Base_0;

  end record;

  ----------------------------------------------------------------------------
  -- This procedure will be called when analysis on a new candidate string
  -- is started. The Token needs to clear its state (if any).
  ----------------------------------------------------------------------------
  procedure Clear (The_Token: in out Instance);

  ----------------------------------------------------------------------------
  -- This procedure will be called to perform further analysis on a token
  -- based on the given next character.
  ----------------------------------------------------------------------------
  procedure Analyze (The_Token: in out Instance;
                     Next_Char: in     Character;
                     Verdict  :    out Analysis_Verdict);

end Opentoken.Recognizer.Based_Integer_Java_Style;
