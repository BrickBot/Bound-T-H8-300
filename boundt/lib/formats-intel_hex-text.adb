-- Formats.Intel_Hex.Text (body)
--
-- Author: Niklas Holsti, Tidorum Ltd
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
-- $Date: 2015/10/24 20:53:53 $
--
-- $Log: formats-intel_hex-text.adb,v $
-- Revision 1.3  2015/10/24 20:53:53  niklas
-- Moved to free licence.
--
-- Revision 1.2  2008-10-11 05:56:43  niklas
-- BT-CH-0147: Intel-Hex with linear 32-bit addresses.
--
-- Revision 1.1  2007/09/22 14:19:11  niklas
-- First version.
--


package body Formats.Intel_Hex.Text is


   use Ada.Text_IO;


   procedure Put (Item : Record_T)
   is
   begin

      Put (
           "I-Hex "
         & Rec_Type_T'Image (Item.Rec_Type)
         & ","
         & Natural'Image (Item.Data'Length)
         & " octets at "
         & Hex_Image (Item.Address)
         & ':');

      for D in Item.Data'Range loop

         Put (' ' & Hex_Image (Item.Data(D)));

      end loop;

      New_Line;

   end Put;


   procedure Dump (
      File : in Ada.Text_IO.File_Type;
      Name : in String := "")
   is

      Upper : Unsigned_32_T := 0;
      -- The Upper Linear Base Address.

   begin

      while not End_Of_File (File) loop

         declare

            Rec : constant Record_T := Next_Known_Line (File, Upper, Name);

         begin

            Put (Rec);

            if Rec.Rec_Type = Extended_Linear_Address then

               Set_Upper_Address (Rec, Upper);

            end if;

         end;

      end loop;

   exception

   when Format_Error =>

      Put_Line ("I-Hex dump aborted due to above error.");

   end Dump;


end Formats.Intel_Hex.Text;
