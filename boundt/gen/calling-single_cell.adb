-- Calling.Single_Cell (body)
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
-- $Date: 2015/10/24 20:05:47 $
--
-- $Log: calling-single_cell.adb,v $
-- Revision 1.4  2015/10/24 20:05:47  niklas
-- Moved to free licence.
--
-- Revision 1.3  2009-11-27 11:28:07  niklas
-- BT-CH-0184: Bit-widths, Word_T, failed modular analysis.
--
-- Revision 1.2  2005/03/04 09:40:21  niklas
-- Added the new primitive operations Static_Caller_Cell and
-- Dynamic_Caller_Cell, and added a default implementation of
-- Caller_Cell that makes use of them.
--
-- Revision 1.1  2005/02/23 09:05:16  niklas
-- BT-CH-0005.
--


with Output;


package body Calling.Single_Cell is


   function Static (Item : Protocol_T) return Boolean
   is
   begin

      return Storage.Bounds.Singular (Item.Interval);

   end Static;


   function Basis (Item : Protocol_T) return Storage.Cell_List_T
   is
   begin

      if Static (Protocol_T'Class (Item)) then

         return Storage.Null_Cell_List;

      else

         return (1 => Item.Cell);

      end if;

   end Basis;


   function Image (Item : Protocol_T) return String
   is
   begin

      return
           "Single_Cell, "
         & Storage.Bounds.Image (
              Item => Item.Interval,
              Name => Storage.Image (Item.Cell));

   end Image;


   function Static_Caller_Cell (
      Callee : Storage.Cell_T;
      Under  : Protocol_T)
   return Storage.Cell_T
   is
   begin

      Output.Fault (
         Location => "Calling.Single_Cell.Static_Caller_Cell",
         Text     => "This function should be overridden");

      raise Program_Error;

      return Storage.No_Cell;

   end Static_Caller_Cell;


   function Dynamic_Caller_Cell (
      Callee : Storage.Cell_T;
      Under  : Protocol_T;
      Value  : Arithmetic.Value_T)
   return Storage.Cell_T
   is
   begin

      Output.Fault (
         Location => "Calling.Single_Cell.Dynamic_Caller_Cell",
         Text     => "This function should be overridden");

      raise Program_Error;

      return Storage.No_Cell;

   end Dynamic_Caller_Cell;


   function Caller_Cell (
      Callee : Storage.Cell_T;
      Under  : Protocol_T)
   return Storage.Cell_T
   is
   begin

      case Map_Kind (Callee, Protocol_T'Class (Under)) is

      when Fixed =>

         return Callee;

      when Static =>

         return Static_Caller_Cell (
            Callee => Callee,
            Under  => Protocol_T'Class (Under));

      when Dynamic =>

         if Storage.Bounds.Singular (Under.Interval) then
            -- We know the value of Under.Cell.

            return Dynamic_Caller_Cell (
               Callee => Callee,
               Under  => Protocol_T'Class (Under),
               Value  => Storage.Bounds.Single_Value (Under.Interval));

         else
            -- The value of Under.Cell is not (precisely) known.

            return Storage.No_Cell;

         end if;

      when Privy =>
         -- We should never try to find the caller cell for
         -- a Privy callee cell.

         Output.Fault (
            Location => "Calling.Single_Cell.Caller_Cell",
            Text     =>
                 "Callee cell is Privy"
               & Output.Field_Separator
               & Storage.Image (Callee));

         return Callee;

      end case;

   end Caller_Cell;


   procedure Apply (
      Bounds : in     Storage.Bounds.Bounds_T'Class;
      Upon   : in     Protocol_T;
      Giving :    out Calling.Protocol_Ref)
   is
      use type Storage.Bounds.Interval_T;

      New_Bounds : constant Storage.Bounds.Interval_T :=
         Storage.Bounds.Interval (
            Cell  => Upon.Cell,
            Under => Bounds);
      --
      -- The range of Upon.Cell values allowed by the Bounds.

      Result : Protocol_Ref;
      -- The new Single-Cell Protocol, if one is created.

   begin

      if Upon.Interval <= New_Bounds then
         -- The new bounds are no better than the old ones.

         Giving := null;

      else
         -- The new bounds sharpen the old bounds.

         Result := new Protocol_T'Class'(Protocol_T'Class (Upon));

         Result.Interval := Upon.Interval and New_Bounds;

         Giving := Calling.Protocol_Ref (Result);

      end if;

   end Apply;


end Calling.Single_Cell;
