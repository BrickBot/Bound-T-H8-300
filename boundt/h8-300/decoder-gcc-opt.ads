-- Decoder.GCC.Opt (decl)
--
-- Command-line options related to the GCC compiler for the H8/300.
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
-- $Date: 2015/10/26 22:19:12 $
--
-- $Log: decoder-gcc-opt.ads,v $
-- Revision 1.3  2015/10/26 22:19:12  niklas
-- Version 4b, and moved to free open-source licence.
--
-- Revision 1.2  2015/10/26 22:01:05  niklas
-- Updated to use current Options services.
--
-- Revision 1.1  2005/07/26 12:00:01  niklas
-- First version.
--


with Options;


package Decoder.GCC.Opt is


   pragma Elaborate_Body;
   --
   -- To register the options here declared.


   type Int_Size_T is (Bits16, Bits32);
   --
   -- The alternative sizes of the "int" type.


   function Size_Image (Item : Int_Size_T) return String;
   --
   -- Returns "16" or "32".


   package Int_Size_Valued is new Options.Discrete_Valued (
      Value_Type             => Int_Size_T,
      Value_Type_Description => "Number",
      Value_Image            => Size_Image,
      Use_Discrete_Image     => False,
      Quote_Image            => False);

   type Int_Size_Option_T is new Int_Size_Valued.Option_T with null record;

   overriding
   function Type_And_Default (Option : access Int_Size_Option_T)
   return String;

   overriding
   procedure Set (
      Option : access Int_Size_Option_T;
      Value  : in     String);

   Int_Size_Opt : aliased Int_Size_Option_T (Default => Bits16);
   --
   -- The size of the "int" type for this target program.

   Int_Size : Int_Size_T renames Int_Size_Opt.Value;


end Decoder.GCC.Opt;
