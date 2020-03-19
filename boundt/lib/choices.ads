-- Choices (decl)
--
-- Choice functions, rather like conditional expressions but of
-- course evaluating both alternatives always. However, in most
-- cases any use of these functions can be replaced by a condition
-- expression, when such are included in future Ada.
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
-- $Date: 2015/10/24 20:53:51 $
--
-- $Log: choices.ads,v $
-- Revision 1.2  2015/10/24 20:53:51  niklas
-- Moved to free licence.
--
-- Revision 1.1  2009-12-09 06:23:44  niklas
-- First version.
--


package Choices is


   function If_Then_Else (
      If_This   : Boolean;
      Then_This : String;
      Else_This : String)
   return String;
   --
   -- if If_This then Then_This else Else_This.


   function If_Then (
      If_This   : Boolean;
      Then_This : String)
   return String;
   --
   -- Same as If_Then_Else (If_This, Then_This, "").


end Choices;
