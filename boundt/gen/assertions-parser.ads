-- Assertions.Parser (decl)
--
-- Parsing the assertion language into an internal form.
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
-- $Revision: 1.2 $
-- $Date: 2015/10/24 19:36:46 $
--
-- $Log: assertions-parser.ads,v $
-- Revision 1.2  2015/10/24 19:36:46  niklas
-- Moved to free licence.
--
-- Revision 1.1  2006-05-27 21:26:38  niklas
-- First version for BT-CH-0020.
--


with Assertions.Own;


private
package Assertions.Parser is


   procedure Parse_File (
      File_Name : in     String;
      Program   : in     Programs.Program_T;
      Into      : in out Own.Assertion_Bag_T;
      Valid     :    out Boolean);
   --
   -- Reads and parses the assertions from the text file with the
   -- given File_Name, interpreting them as assertions on the execution
   -- or structure of the given Program, and storing them Into the
   -- given assertion bag (which may already contain other assertions
   -- that will be kept, too).
   --
   -- If the file contains some syntax or semantic errors, the procedure
   -- reports them as Output.Errors and returns Valid as False, otherwise
   -- Valid is True.


end Assertions.Parser;
