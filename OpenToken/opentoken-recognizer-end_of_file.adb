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
-- $Log: opentoken-recognizer-end_of_file.adb,v $
-- Revision 1.1.1.1  1999-12-27 19:56:00  niklas
-- Import OpenToken 3.0b.
--
-- Revision 1.2  1999/12/27 19:56:00  Ted
-- fix file contents to work w/ new hierarchy
--
-- Revision 1.1  1999/12/27 17:11:33  Ted
-- renamed everything to new hierarchy
--
-- Revision 1.2  1999/08/17 03:06:40  Ted
-- Add log line
--
-------------------------------------------------------------------------------

-------------------------------------------------------------------------------
-- This package implements a token recognizer for an End of File designator.
-------------------------------------------------------------------------------
package body Opentoken.Recognizer.End_Of_File is

   ----------------------------------------------------------------------------
   -- This procedure will be called when analysis on a new candidate string
   -- is started. The Token needs to clear its state (if any).
   ----------------------------------------------------------------------------
   procedure Clear (The_Token : in out Instance) is
   begin

      The_Token.State := EOF;

   end Clear;


   ----------------------------------------------------------------------------
   -- This procedure will be called to perform further analysis on a token
   -- based on the given next character.
   ----------------------------------------------------------------------------
   procedure Analyze (The_Token : in out Instance;
                      Next_Char : in Character;
                      Verdict   : out Analysis_Verdict) is
   begin

      case The_Token.State is
      when EOF =>
         if Next_Char = EOF_Character then
            Verdict := Matches;
         else
            Verdict := Failed;
         end if;

         The_Token.State := Done;

      when Done =>

         -- We shouldn't get called from here.
         Verdict := Failed;

      end case;

   end Analyze;


   ----------------------------------------------------------------------------
   -- This procedure will be called to create a End_Of_File token
   ----------------------------------------------------------------------------
   function Get (Reportable : in Boolean := True) return Instance is

      New_Token : Instance;
   begin

      return (Report => Reportable,
              State  => EOF);

   end Get;

end Opentoken.Recognizer.End_Of_File;
