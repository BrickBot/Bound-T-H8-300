-- Stdio (body)
--
-- Author: Niklas Holsti.
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
-- $Date: 2015/10/24 20:53:55 $
--
-- $Log: stdio.adb,v $
-- Revision 1.2  2015/10/24 20:53:55  niklas
-- Moved to free licence.
--
-- Revision 1.1  2005-03-08 10:19:12  niklas
-- First version.
--


with Ada.Text_IO;


package body Stdio is


   use Interfaces.C;
   use Interfaces.C.Strings;


   function Is_Null (Item : File_Ref) return Boolean
   is
   begin

      return Item = File_Ref (System.Null_Address);

   end Is_Null;


   procedure Open_File (
      Path : in     String;
      Mode : in     String;
      File :    out File_Ref)
   is

      C_Path : chars_ptr := New_String (Path);
      C_Mode : chars_ptr := New_String (Mode);

   begin

      File := Fopen (
         Path => C_Path,
         Mode => C_Mode);

      Free (C_Path);
      Free (C_Mode);

   end Open_File;


   procedure Close_File (File : in File_Ref)
   is

      Result : int;

   begin

      Result := Fclose (File);

      if Result /= 0 then

         Ada.Text_IO.Put_Line (
            Ada.Text_IO.Standard_Error,
            "Stdio.Close: Failed to close file.");

      end if;

   end Close_File;


end Stdio;
