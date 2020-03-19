-- Calling.Fully_Fixed (decl)
--
-- A fully static calling protocol with a Fixed mapping for call cells.
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
-- $Date: 2015/10/24 19:36:48 $
--
-- $Log: calling-fully_fixed.ads,v $
-- Revision 1.2  2015/10/24 19:36:48  niklas
-- Moved to free licence.
--
-- Revision 1.1  2008-03-11 22:05:06  niklas
-- First version.
--


with Storage;


package Calling.Fully_Fixed is


   type Protocol_T is new Calling.Static_Protocol_T with null record;
   --
   -- A static calling protocol in which all cells have a Fixed mapping
   -- and may change across calls (are not invariant).


   -- The Static function is inherited; it returns True.


   -- overriding
   function Image (Item : Protocol_T) return String;
   --
   -- Returns "Fully_Fixed".


   -- overriding
   function Map_Kind (
      Callee : Storage.Cell_T;
      Under  : Protocol_T)
   return Map_Kind_T;
   --
   -- Returns Fixed.


   -- The Invariant function is inherited. Thus, no cells
   -- are defined as invariant under this Protocol_T.


   -- overriding
   function Caller_Cell (
      Callee : Storage.Cell_T;
      Under  : Protocol_T)
   return Storage.Cell_T;
   --
   -- Returns the Callee cell, because all cells have Fixed mapping.


   -- overriding
   function Sure_Invariant (
      Caller : Storage.Cell_T;
      Under  : Protocol_T;
      Stub   : Stub_Level_T)
   return Boolean;
   --
   -- Returns False.


   -- The Apply function is inherited and sets Giving to null
   -- and does nothing else.


end Calling.Fully_Fixed;
