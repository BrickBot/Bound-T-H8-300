-- Symbols.Show (decl)
--
-- Types and functions for output of symbolic information itself,
-- and symbolic descriptions of other information, such as cells
-- or program locations.
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
-- $Revision: 1.4 $
-- $Date: 2015/10/24 19:36:53 $
--
-- $Log: symbols-show.ads,v $
-- Revision 1.4  2015/10/24 19:36:53  niklas
-- Moved to free licence.
--
-- Revision 1.3  2004-04-25 11:54:12  niklas
-- First Tidorum version.
-- Provides Statement_Locus_T for source-line connections.
--
-- Revision 1.2  2001/03/26 13:56:28  holsti
-- Locus for connection added.
--
-- Revision 1.1  2001/03/21 20:31:25  holsti
-- First version.
--
--


with Output;


package Symbols.Show is


   function Statement (
      Line   : Connection_T;
      Source : Symbol_Table_T)
   return Output.Statement_Locus_T;
   --
   -- Given a connection for a source-line, returns the
   -- corresponding statement locus.


   function Statements (
      Address : Processor.Code_Address_T;
      Source  : Symbol_Table_T)
   return Output.Statement_Range_T;
   --
   -- Description of the statement(s) connected to the address.
   -- The resulting statement range contains the given address
   -- and the range of source-line numbers (perhaps empty)
   -- connected to this address.


   function Locus (
      Connection : Connection_T;
      Source     : Symbol_Table_T)
   return Output.Locus_T;
   --
   -- The location in the target program described by a subprogram,
   -- label, or line-number connection.


end Symbols.Show;
