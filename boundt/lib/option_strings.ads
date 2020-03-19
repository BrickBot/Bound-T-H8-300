-- Option_Strings (decl)
--
-- Operations for inspecting and processing command-line options.
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
-- $Log: option_strings.ads,v $
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
-- Revision 1.3  2006/02/08 19:45:00  niklas
-- Corrected description of Key_Part.
--
-- Revision 1.2  2005/08/25 13:27:57  niklas
-- Made lower-casing optional in the Value function (to
-- preserve eg. file-name values) and added the function
-- To_Lower to compensate.
--
-- Revision 1.1  2005/01/25 18:25:32  niklas
-- First version.
--


package Option_Strings is


   function Undashed (Argument : String) return String;
   --
   -- The "word" part of an Argument of the form "-word".
   -- If the Argument has a different form, a null string is returned.
   -- The string is returned in lower-case.


   function Key_Part (Argument : String) return String;
   --
   -- The "key" part of an Argument of the form "-key=value".
   -- If the Argument has a different form, a null string is returned.
   -- The string is returned in lower-case.


   function Value_Part (
      Argument  : String;
      Lowercase : Boolean := True)
   return String;
   --
   -- The "value" part of an Argument of the form "-key=value".
   -- If the Argument has a different form, a null string is returned.
   -- If the Lowercase parameter is True, the string is returned in
   -- lower-case.


   function Opt_Or_Not (
      Argument : String;
      Option   : String)
   return Boolean;
   --
   -- Whether the Argument is of the form "-<option>" or
   -- of the form "-no_<option>". See also function Yes, below.


   function Yes (Argument : String) return Boolean;
   --
   -- That the Argument does not start with "-no_".
   -- This is meant to be used for an Argument that is of the
   -- form accepted by Opt_Or_Not and to show if the option is
   -- to  be set (True) or unset (False).


   function Item_Or_Not (
      Argument : String;
      Option   : String)
   return Boolean;
   --
   -- Whether the Argument is of the form "<item>" or
   -- of the form "no_<item>". See also function Yes, below.


   function Yes_Item (Argument : String) return Boolean;
   --
   -- That the Argument does not start with "no_".
   -- This is meant to be used for an Argument that is of the
   -- form accepted by Item_Or_Not and to show if the item is
   -- to be set (True) or unset (False).


   function Image (
      Item       : Boolean;
      When_True  : String;
      When_False : String)
   return String;
   --
   -- A simple choice of two strings depending on a Boolean value.


   function Default_Or_Not (
      Option : String;
      Yes    : Boolean)
   return String;
   --
   -- "The default is -Option." or "The default is -no_Option.",
   -- depending on Yes.


   function To_Lower (Item : String) return String;
   --
   -- The given string in lower-case letters.


   function Trim (Item : String) return String;
   --
   -- Removes leading and trailing blank spaces.


end Option_Strings;
