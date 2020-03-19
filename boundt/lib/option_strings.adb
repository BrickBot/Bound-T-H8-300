-- Option_Strings (body)
--
-- Author: Niklas Holsti
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
-- $Revision: 1.10 $
-- $Date: 2015/10/24 20:53:55 $
--
-- $Log: option_strings.adb,v $
-- Revision 1.10  2015/10/24 20:53:55  niklas
-- Moved to free licence.
--
-- Revision 1.9  2008-04-22 13:44:43  niklas
-- Added Default_Or_Not.
--
-- Revision 1.8  2007/10/07 19:24:17  niklas
-- Added Item_Or_Not and Yes_Item.
--
-- Revision 1.7  2007/10/05 20:34:42  niklas
-- Added function Image (Boolean, String, String) for BT-CH-0086.
--
-- Revision 1.6  2007/08/10 06:54:17  niklas
-- Added functions Opt_Or_Not and Yes.
--
-- Revision 1.5  2007/05/08 08:29:56  niklas
-- Added function Trim.
--
-- Revision 1.4  2007/03/04 20:17:39  niklas
-- Added the Undashed function.
--
-- Revision 1.3  2005/08/25 13:27:56  niklas
-- Made lower-casing optional in the Value function (to
-- preserve eg. file-name values) and added the function
-- To_Lower to compensate.
--
-- Revision 1.2  2005/01/25 21:37:20  niklas
-- Corrected function Key to omit the leading '-'.
--
-- Revision 1.1  2005/01/25 18:25:32  niklas
-- First version.
--


with Ada.Characters.Handling;
with Ada.Strings.Fixed;


package body Option_Strings is


   function Undashed (Argument : String) return String
   is
      use Ada.Characters.Handling;
   begin

      if       Argument'Length > 1
      and then Argument(Argument'First) = '-'
      then

         return To_Lower (Argument(Argument'First + 1 .. Argument'Last));

      else

         return "";

      end if;

   end Undashed;


   function Key_Part (Argument : in String) return String
   is
      use Ada.Characters.Handling;

      Equal : constant Natural := Ada.Strings.Fixed.Index (
         Source  => Argument,
         Pattern => "=");
      -- The index of the first "=" in Argument, if there is one, else zero.

   begin

      if       Argument'Length > 3
      and then Argument(Argument'First) = '-'
      and then Equal in Argument'First + 2 .. Argument'Last - 1
      then
         -- This looks like a possible -key=value.

         return To_Lower (Argument(Argument'First + 1 .. Equal - 1));

      else
         -- Argument is not of the form -key=value.

         return "";

      end if;

   end Key_Part;


   function Value_Part (
      Argument  : String;
      Lowercase : Boolean := True)
   return String
   is
      use Ada.Characters.Handling;

      Equal : constant Natural := Ada.Strings.Fixed.Index (
         Source  => Argument,
         Pattern => "=");
      -- The index of the first "=" in Argument, if there is one, else zero.

   begin

      if       Argument'Length > 3
      and then Argument(Argument'First) = '-'
      and then Equal in Argument'First + 2 .. Argument'Last - 1
      then
         -- This looks like a possible -key=value.

         if Lowercase then

            return To_Lower (Argument(Equal + 1 .. Argument'Last));

         else

            return Argument(Equal + 1 .. Argument'Last);

         end if;

      else
         -- Argument is not of the form -key=value.

         return "";

      end if;

   end Value_Part;


   Hyp_No : constant String := "-no_";
   --
   -- The "turn off" prefix used in Opt_Or_Not and Yes.


   function Opt_Or_Not (
      Argument : String;
      Option   : String)
   return Boolean
   is
   begin

      return  Argument = '-'    & Option
      or else Argument = Hyp_No & Option;

   end Opt_Or_Not;


   function Yes (Argument : String) return Boolean
   is
      F : constant Positive := Argument'First;
   begin

      return  Argument'Length < Hyp_No'Length
      or else Argument(F .. F + Hyp_No'Length - 1) /= Hyp_No;

   end Yes;


   No : constant String := "no_";
   --
   -- The "turn off" prefix used in Item_Or_Not and Yes_Item.


   function Item_Or_Not (
      Argument : String;
      Option   : String)
   return Boolean
   is
   begin

      return  Argument =      Option
      or else Argument = No & Option;

   end Item_Or_Not;


   function Yes_Item (Argument : String) return Boolean
   is
      F : constant Positive := Argument'First;
   begin

      return  Argument'Length < No'Length
      or else Argument(F .. F + No'Length - 1) /= No;

   end Yes_Item;


   function Image (
      Item       : Boolean;
      When_True  : String;
      When_False : String)
   return String
   is
   begin

      if Item then return When_True;
              else return When_False;
      end if;

   end Image;


   function Default_Or_Not (
      Option : String;
      Yes    : Boolean)
   return String
   is
   begin

      return "The default is -"
         & Image (Yes, "", "no_")
         & Option
         & '.';

   end Default_Or_Not;


   function To_Lower (Item : String) return String
   renames Ada.Characters.Handling.To_Lower;


   function Trim (Item : String) return String
   is
      use Ada.Strings, Ada.Strings.Fixed;
   begin

      return Trim (Item, Both);

   end Trim;


end Option_Strings;
