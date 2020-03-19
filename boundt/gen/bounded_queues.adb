-- Bounded_Queues (body)
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
-- $Date: 2015/10/24 20:05:46 $
--
-- $Log: bounded_queues.adb,v $
-- Revision 1.3  2015/10/24 20:05:46  niklas
-- Moved to free licence.
--
-- Revision 1.2  2007-10-26 12:44:34  niklas
-- BT-CH-0091: Reanalyse a boundable edge only if its domain grows.
--
-- Revision 1.1  2001/01/07 21:54:14  holsti
-- First version.
--


package body Bounded_Queues is


   function Length (Item : Queue_Type) return Natural
   is
   begin

      return Item.Length;

   end Length;


   procedure Put (
      Element : in     Element_Type;
      Into    : in out Queue_Type)
   is

      Next : Positive;
      -- The index of the next position, where we will
      -- put the Element.

   begin

      if Into.Length >= Into.Max_Length then

         raise Overflow;

      end if;

      Next := Into.First + Into.Length;

      if Next > Into.Room'Last then

         Next := Next - Into.Max_Length;

      end if;

      Into.Room (Next) := Element;

      Into.Length := Into.Length + 1;

   end Put;


   procedure Get (
      From    : in out Queue_Type;
      Element :    out Element_Type)
   is
   begin

      if From.Length = 0 then

         raise Underflow;

      end if;

      Element := From.Room (From.First);

      if From.First < From.Room'Last then

         From.First := From.First + 1;

      else

         From.First := From.Room'First;

      end if;

      From.Length := From.Length - 1;

   end Get;


end Bounded_Queues;
