-- ESF (body)
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
-- $Date: 2015/10/24 20:05:47 $
--
-- $Log: esf.adb,v $
-- Revision 1.4  2015/10/24 20:05:47  niklas
-- Moved to free licence.
--
-- Revision 1.3  2007-02-13 14:56:40  Niklas
-- BT-CH-0044.
--
-- Revision 1.2  2005/10/26 14:09:36  niklas
-- Added the function Is_Open.
--
-- Revision 1.1  2001/04/06 11:06:51  ville
-- First version
--


package body ESF is


   ESF_Is_Open : Boolean := False;
   --
   -- Whether the ESF_File is open.


   function Is_Open return Boolean
   is
   begin

      return ESF_Is_Open;

   end Is_Open;


   procedure Create (File_Name : in String)
   is   
   begin

      Ada.Text_IO.Create (
         File => File,
         Mode => Ada.Text_IO.Out_File,
         Name => File_Name);

      ESF_Is_Open := Ada.Text_IO.Is_Open (File);

   end Create;


   procedure Close
   is
   begin
   
      Ada.Text_IO.Close (File => File);

      ESF_Is_Open := False;

   end Close;


   procedure Put_ESF_Comment (Text : in String)
   is
   begin

      if ESF_Is_Open then

         Ada.Text_IO.Put_Line (
            File => File,
            Item => "  -- " & Text);

         Ada.Text_IO.New_Line (File);

      end if;

   end Put_ESF_Comment;


end ESF;
