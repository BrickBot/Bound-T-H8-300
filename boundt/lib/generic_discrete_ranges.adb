-- Generic_Discrete_Ranges (body)
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
-- $Date: 2015/10/24 20:53:54 $
--
-- $Log: generic_discrete_ranges.adb,v $
-- Revision 1.2  2015/10/24 20:53:54  niklas
-- Moved to free licence.
--
-- Revision 1.1  2014/06/28 20:07:35  niklas
-- First version.
--


package body Generic_Discrete_Ranges
is


   function In_Range (
      Value  : Base_Type;
      Rainge : Range_Type)
   return Boolean
   is
   begin

      return Value in Rainge.First .. Rainge.Last;

   end In_Range;


   function "<=" (Left, Right : Range_Type)
   return Boolean
   is
   begin

      return
         (Left.Last < Left.First)
         -- The empty range is a subrange of anything.
         or else (
             Right.First <= Left.First
         and Right.Last  >= Left.Last);

   end "<=";


   function Singleton (Value : Base_Type)
   return Range_Type
   is
   begin

      return (First => Value, Last => Value);

   end Singleton;


   procedure Widen (
      Rainge     : in out Range_Type;
      To_Include : in     Base_Type)
   is
   begin

      if Rainge.Last < Rainge.First then
         -- The given range is empty.

         Rainge := Singleton (Value => To_Include);

      else

         if To_Include < Rainge.First then

            Rainge.First := To_Include;

         elsif To_Include > Rainge.Last then

            Rainge.Last := To_Include;

         end if;

      end if;

   end Widen;


   function Image (Item : Range_Type) return String
   is
   begin

      if Item.Last < Item.First then
         -- The empty range.

         return "[]";

      elsif Item = Full_Range then
          -- All possible values.

          return "[all]";

      else

         return '['
               & Image (Item.First)
               & ".."
               & Image (Item.Last)
               & ']';

      end if;

   end Image;


end Generic_Discrete_Ranges;
