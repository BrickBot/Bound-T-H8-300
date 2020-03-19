-- Calling.Plain (decl)
--
-- A fully static and plain-vanilla calling protocol.
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
-- $Date: 2015/10/24 19:36:48 $
--
-- $Log: calling-plain.ads,v $
-- Revision 1.4  2015/10/24 19:36:48  niklas
-- Moved to free licence.
--
-- Revision 1.3  2007-08-20 12:17:26  niklas
-- Made Protocol_T abstract to avoid overriding Sure_Invariant here.
--
-- Revision 1.2  2006/02/27 09:18:46  niklas
-- Changed Protocol_T to derive from Static_Protocol_T, which
-- means that some inherited operations are valid as such and
-- need not be overridden.
--
-- Revision 1.1  2005/02/23 09:05:16  niklas
-- BT-CH-0005.
--


with Programs;
with Storage;
with Storage.Bounds;


package Calling.Plain is


   type Protocol_T is abstract new Calling.Static_Protocol_T
   with record
      Program : Programs.Program_T;
   end record;
   --
   -- A calling protocol that has no dynamically mapped cells and
   -- considers all stack-height cells invariant but others not.
   --
   -- Program
   --    The program in which the protocol occurs.
   --    The Height cells of Programs.Stacks (Program) are
   --    invariant across a call, other cells not.


   -- The Static function is inherited; it returns True.


   function Image (Item : Protocol_T) return String;
   --
   -- Returns "Plain".


   function Map_Kind (
      Callee : Storage.Cell_T;
      Under  : Protocol_T)
   return Map_Kind_T;
   --
   -- Returns Fixed except if Callee is a Stack Height cell, because
   -- the Plain Protocol does not transform cells. Returns Privy for
   -- any Stack Height cell.
   --
   -- Overrides (implements) Map_Kind (Calling.Protocol_T).


   -- The Invariant function is inherited. Thus, only the Stack Height
   -- cells are defined as invariant under the Plain.Protocol_T.


   function Caller_Cell (
      Callee : Storage.Cell_T;
      Under  : Protocol_T)
   return Storage.Cell_T;
   --
   -- Returns the Callee cell, because that's the way the Plain
   -- Protocol works (Fixed mapping).
   --
   -- Overrides (implements) Caller_Cell (Calling.Protocol_T).


   -- The Apply function is inherited and sets Giving to null
   -- and does nothing else.


end Calling.Plain;
