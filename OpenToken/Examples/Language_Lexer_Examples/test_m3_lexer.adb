-------------------------------------------------------------------------------
--
-- Copyright (C) 1999 David Starner
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
-- Maintainer: David Starner (dstarner98@aasaa.ofe.org)
--
-- Update History:
-- $Log: test_m3_lexer.adb,v $
-- Revision 1.1.1.1  2000-02-05 04:03:38  niklas
-- Import OpenToken 3.0b.
--
-- Revision 1.1  2000/02/05 04:03:37  Ted
-- Add Modula-3 lexer tester.
--
-------------------------------------------------------------------------------

with Ada.Text_IO;
with Ada.Command_Line;

with OpenToken.Text_Feeder.Text_IO;
with M3_Lexer;
use  M3_Lexer;

procedure Test_M3_Lexer is

   -- Global text file for reading parse data
   File   : aliased Ada.Text_IO.File_Type;
   Feeder : aliased OpenToken.Text_Feeder.Text_IO.Instance :=
     OpenToken.Text_Feeder.Text_IO.Create (File'Unchecked_Access);

begin

   Ada.Text_IO.Open
     (File => File,
      Mode => Ada.Text_IO.In_File,
      Name => Ada.Command_Line.Argument (1));

   Tokenizer.Set_Text_Feeder
     (Analyzer => Analyzer,
      Feeder   => Feeder'Unchecked_Access
      );

   loop

      Tokenizer.Find_Next (Analyzer);

      Ada.Text_IO.Put_Line
        ("Found " & M3_Token'Image (Tokenizer.ID (Analyzer)) &
         ' ' & Tokenizer.Lexeme (Analyzer));

      exit when Tokenizer.ID (Analyzer) = End_of_File_T;

   end loop;

end Test_M3_Lexer;
