-- Calling.Plain (body)
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
-- $Date: 2015/10/24 20:05:47 $
--
-- $Log: calling-plain.adb,v $
-- Revision 1.3  2015/10/24 20:05:47  niklas
-- Moved to free licence.
--
-- Revision 1.2  2006-02-27 09:18:46  niklas
-- Changed Protocol_T to derive from Static_Protocol_T, which
-- means that some inherited operations are valid as such and
-- need not be overridden.
--
-- Revision 1.1  2005/02/23 09:05:16  niklas
-- BT-CH-0005.
--


package body Calling.Plain is


   function Image (Item : Protocol_T) return String
   is
   begin

      return "Plain";

   end Image;


   function Map_Kind (
      Callee : Storage.Cell_T;
      Under  : Protocol_T)
   return Map_Kind_T
   is
   begin

      if Programs.Is_Stack_Height (Callee, Under.Program) then

         return Privy;

      else

         return Fixed;

      end if;

   end Map_Kind;


   function Caller_Cell (
      Callee : Storage.Cell_T;
      Under  : Protocol_T)
   return Storage.Cell_T
   is
   begin

      return Callee;

   end Caller_Cell;


end Calling.Plain;
