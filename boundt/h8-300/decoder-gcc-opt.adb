-- Decoder.GCC.Opt (body)
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
-- $Date: 2015/10/26 22:19:12 $
--
-- $Log: decoder-gcc-opt.adb,v $
-- Revision 1.2  2015/10/26 22:19:12  niklas
-- Version 4b, and moved to free open-source licence.
--
-- Revision 1.1  2015/10/26 22:01:05  niklas
-- Updated to use current Options services.
--


package body Decoder.GCC.Opt is


   function Size_Image (Item : Int_Size_T) return String
   is
   begin
      case Item is
      when Bits16 => return "16";
      when Bits32 => return "32";
      end case;
   end Size_Image;


   overriding
   function Type_And_Default (Option : access Int_Size_Option_T)
   return String
   is
   begin

      return "Number, 16 or 32, default " & Size_Image (Option.Default);

   end Type_And_Default;


   overriding
   procedure Set (
      Option : access Int_Size_Option_T;
      Value  : in     String)
   is
   begin

      if Value = "16" then

         Option.Value := Bits16;

      elsif Value = "32" then

         Option.Value := Bits32;

      else

         raise Constraint_Error;

      end if;

   end Set;


end Decoder.GCC.Opt;
