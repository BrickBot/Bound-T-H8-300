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
-- $Log: token_analyzer_ctd-run.adb,v $
-- Revision 1.1.1.1  2000-08-12 21:14:12  niklas
-- Import OpenToken 3.0b.
--
-- Revision 1.1  2000/08/12 21:14:12  Ted
-- moved from token_analyzer.adb
--
-- Revision 1.3  2000/01/27 21:19:15  Ted
-- Fix to work with 2.0
--
-- Revision 1.2  1999/12/27 19:56:05  Ted
-- fix file contents to work w/ new hierarchy
--
-- Revision 1.1  1999/10/08 23:24:31  Ted
-- Test driver for token analyzer
--
--
-------------------------------------------------------------------------------
with Ada.Text_IO;
with OpenToken.Text_Feeder.Text_IO;

-------------------------------------------------------------------------------
-- Test driver for the token anlayzer's default token functionality
-------------------------------------------------------------------------------
procedure Token_Analyzer_Ctd.Run is
   Test_File_Name : constant String := "Test.txt";
begin

   ----------------------------------------------------------------------------
   -- Test Case 1
   --
   -- Inputs           : A syntax with a normal and a default token, and a file
   --                    containing a token amidst unmatchable test.
   --
   -- Expected Results : The analyzer should retrieve the default token, the
   --                    normal token, and the default token.
   -- Purpose          : Verify that default tokens are properly read without
   --                    disrupting normal token processing.
   Test_Case_1 : declare

      Text_Line_1 : constant String := "39045trjkjklgr dfsjkl ";
      Text_Line_2 : constant String := "(more garbage)5 689035 6t78905858 9 45:";

      Test_File : Ada.Text_IO.File_Type;

   begin

      Ada.Text_IO.Put ("Testing default token processing...");
      Ada.Text_IO.Flush;

      -- Create the test file
      Ada.Text_IO.Create
        (File => Test_File,
         Mode => Ada.Text_IO.Out_File,
         Name => Test_File_Name
         );
      Ada.Text_IO.Put_Line (File => Test_File, Item =>Text_Line_1 & Normal_Text);
      Ada.Text_IO.Put_Line (File => Test_File, Item =>Text_Line_2);
      Ada.Text_IO.Close (Test_File);

      -- Setup the analyzer
      Ada.Text_IO.Open
        (File => Test_File,
         Mode => Ada.Text_IO.In_File,
         Name => Test_File_Name
         );
      Ada.Text_IO.Set_Input (Test_File);
      Tokenizer.Input_Feeder := OpenToken.Text_Feeder.Text_IO.Create;

      -- Analyze the file
      Tokenizer.Find_Next (Analyzer);

      if Tokenizer.ID(Analyzer) /= Default then
         Ada.Text_IO.Put_Line ("failed.");
         Ada.Text_IO.Put_Line ("First token was not default.");
         return;
      end if;
      if Tokenizer.Lexeme(Analyzer) /= Text_Line_1 then
         Ada.Text_IO.Put_Line ("failed.");
         Ada.Text_IO.Put_Line ("First lexeme was """ & Tokenizer.Lexeme(Analyzer) &
                               """ not """ & Text_Line_1 & """.");
         return;
      end if;

      Tokenizer.Find_Next (Analyzer);

      if Tokenizer.ID(Analyzer) /= Normal then
         Ada.Text_IO.Put_Line ("failed.");
         Ada.Text_IO.Put_Line ("Second token was not Normal.");
         return;
      end if;

      Tokenizer.Find_Next (Analyzer);

      if Tokenizer.ID(Analyzer) /= Default then
         Ada.Text_IO.Put_Line ("failed.");
         Ada.Text_IO.Put_Line ("Third token was not default.");
         return;
      end if;
      if Tokenizer.Lexeme(Analyzer) /= OpenToken.EOL_Character & Text_Line_2 & OpenToken.EOF_Character then
         Ada.Text_IO.Put_Line ("failed.");
         Ada.Text_IO.Put_Line ("Third lexeme was """ & Tokenizer.Lexeme(Analyzer) &
                               """ not """ & OpenToken.EOL_Character & Text_Line_2 & OpenToken.EOF_Character & """.");
         return;
      end if;

      Ada.Text_IO.Close (Test_File);
      Ada.Text_IO.Put_Line ("passed");
   end Test_Case_1;


end Token_Analyzer_Ctd.Run;
