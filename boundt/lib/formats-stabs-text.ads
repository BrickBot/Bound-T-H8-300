-- Formats.Stabs.Text (decl)
--
-- Printing symbol tables given in Stabs (or STABS) form.
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
-- $Revision: 1.3 $
-- $Date: 2015/10/24 20:53:54 $
--
-- $Log: formats-stabs-text.ads,v $
-- Revision 1.3  2015/10/24 20:53:54  niklas
-- Moved to free licence.
--
-- Revision 1.2  2004-06-12 11:24:10  niklas
-- Index may be signed (Integer, not Natural).
--
-- Revision 1.1  2004/04/24 17:25:15  niklas
-- First version.
--


package Formats.Stabs.Text is


   -- The following "Put" procedures output their Item
   -- parameter on standard output, usually in multi-line
   -- form.


   procedure Put (
      Item   : in Line_T;
      Index  : in Integer;
      Header : in Boolean := False;
      Name   : in String);


   procedure Put (Item : in Table_T);


end Formats.Stabs.Text;
