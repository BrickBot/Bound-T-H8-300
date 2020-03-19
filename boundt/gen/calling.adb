-- Calling (body)
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
-- $Revision: 1.5 $
-- $Date: 2015/10/24 20:05:47 $
--
-- $Log: calling.adb,v $
-- Revision 1.5  2015/10/24 20:05:47  niklas
-- Moved to free licence.
--
-- Revision 1.4  2010-01-30 21:13:29  niklas
-- BT-CH-0216: Subprograms have a Return_Method_T attribute.
--
-- Revision 1.3  2006-02-27 09:15:51  niklas
-- Added Static_Protocol_T.
--
-- Revision 1.2  2005/02/23 09:05:17  niklas
-- BT-CH-0005.
--
-- Revision 1.1  2005/02/16 21:11:42  niklas
-- BT-CH-0002.
--


with Output;


package body Calling is


   --
   ---   Calling protocols
   --


   function Invariant (
      Callee : Storage.Cell_T;
      Under  : Protocol_T)
   return Boolean
   is
   begin

      return Map_Kind (Callee, Protocol_T'Class (Under)) = Privy;

   end Invariant;


   --
   ---   Static calling protocols
   --


   function Static (Item : Static_Protocol_T) return Boolean
   is
   begin

      return True;

   end Static;


   function Basis (Item : Static_Protocol_T) return Storage.Cell_List_T
   is
   begin

      return Storage.Null_Cell_List;

   end Basis;


   procedure Apply (
      Bounds : in     Storage.Bounds.Bounds_T'Class;
      Upon   : in     Static_Protocol_T;
      Giving :    out Protocol_Ref)
   is
   begin

      Output.Fault (
         Location => "Calling.Apply (Static_Protocol_T)",
         Text     => "Should not be called");

      Giving := null;

   end Apply;


   --
   ---   Return methods
   --


   function Image (Item : Return_Method_T) return String
   is

      Way : constant String := Return_Way_T'Image (Item.Way);

   begin

      case Item.Way is

      when No_Return | Normal_Return =>

         return Way;

      when Offset_Return =>

         return Way
              & ' '
              & Processor.Image (Item.Offset);

      end case;

   end Image;

end Calling;
