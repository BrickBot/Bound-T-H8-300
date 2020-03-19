-- Formats.S_Record.Text (body)
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
-- $Revision: 1.4 $
-- $Date: 2015/10/24 20:53:54 $
--
-- $Log: formats-s_record-text.adb,v $
-- Revision 1.4  2015/10/24 20:53:54  niklas
-- Moved to free licence.
--
-- Revision 1.3  2007-12-27 22:32:00  niklas
-- BT-CH-0103: Formats.S_Record uses Stream_IO, not Text_IO.
--
-- Revision 1.2  2005/08/25 13:35:01  niklas
-- Changed S_Record_T.Data to be indexed from 0, not 1, for
-- a better match with the procedure Memories.Load.
--
-- Revision 1.1  2005/08/25 12:17:24  niklas
-- First version.
--


package body Formats.S_Record.Text is


   use Ada.Text_IO;


   procedure Put (Item : S_Record_T)
   is
   begin

      Put (
           'S' & S_Type_T'Image (Item.S_Type)
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
      File : in IO.File_Type;
      Name : in String := "")
   is

      Locus : Locus_T;
      -- For tracking the place in the file.

   begin

      Set_To_Start (File, Name, Locus);

      while not IO.End_Of_File (File) loop

         Put (Next_Line (File, Locus));

      end loop;

   exception

   when Format_Error =>

      Put_Line ("Dump aborted due to above error.");

   end Dump;


end Formats.S_Record.Text;
