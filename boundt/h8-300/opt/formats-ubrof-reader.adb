-- Formats.UBROF.Reader (dummy body)
--
-- This file is a dummy, do-nothing body for the package Formats.UBROF.Reader
-- which, in its full, operational form, provides Bound-T/H8-300 with the
-- ability to read and analyse executable files in the UBROF format defined
-- by IAR systems. The full form of the package-body is constrained by NDA
-- with IAR Systems.
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
-- $Revision: 1.1 $
-- $Date: 2015/10/29 14:51:18 $
--
-- $Log: formats-ubrof-reader.adb,v $
-- Revision 1.1  2015/10/29 14:51:18  niklas
-- UBROF support made locally optional.
--


with Format;
with Output;


package body Formats.UBROF.Reader is


   function Accepts (
      Name : String;
      File : IO.File_Type)
   return Boolean
   is
   begin
   
      return False;
      
   end Accepts;


   procedure Load (
      File         : in     IO.File_Type;
      Content      : access Memory.Content_T;
      Symbol_Table : in     Symbols.Symbol_Table_T)
   is
   begin
   
      Output.Fault (
         Location => "Formats.UBROF.Reader.Load",
         Text     => "UBROF is not supported in this Bound-T version.");

      raise Format.Loading_Error;

   end Load;


   procedure Dump (File : in IO.File_Type)
   is
   begin
   
      Output.Fault (
         Location => "Formats.UBROF.Reader.Dump",
         Text     => "UBROF is not supported in this Bound-T version.");

      raise Format.Loading_Error;

   end Dump;


end Formats.UBROF.Reader;
