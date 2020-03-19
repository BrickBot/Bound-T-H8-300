-- Calculator.Parser (decl)
--
-- Parses expressions returned from a text-i/o calculation engine.
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
-- $Revision: 1.6 $
-- $Date: 2015/10/24 19:36:47 $
--
-- $Log: calculator-parser.ads,v $
-- Revision 1.6  2015/10/24 19:36:47  niklas
-- Moved to free licence.
--
-- Revision 1.5  2009-01-18 08:16:09  niklas
-- Removed unused context clause.
--
-- Revision 1.4  2005/02/16 21:11:41  niklas
-- BT-CH-0002.
--
-- Revision 1.3  2000/06/29 14:10:26  holsti
-- Parser updated.
--
-- Revision 1.2  2000/06/22 10:19:50  saarinen
-- Added functions Parse_Range and Parse_Boolean.
--
-- Revision 1.1  2000/06/17 17:13:35  holsti
-- First version.
--


package Calculator.Parser is


   Syntax_Error : exception;
   --
   -- Signals that one of the following parsing operations
   -- found a syntactical error in its input string.


   function Bound (From : String)
   return Storage.Bounds.Interval_T;
   --
   -- Parses the string as the Omega Calculator's response to
   -- a range query and returns the bounds of the range.


   function Boolean_Literal (From : String)
   return Boolean;
   --
   -- Parses True or False.


end Calculator.Parser;
