-- Basic_Output (body)
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
-- $Date: 2015/10/24 20:05:45 $
--
-- $Log: basic_output.adb,v $
-- Revision 1.4  2015/10/24 20:05:45  niklas
-- Moved to free licence.
--
-- Revision 1.3  2012-01-19 20:13:56  niklas
-- BT-CH-0224: Device.Catalog added. Device options updated.
--
-- Revision 1.2  2007-10-26 12:44:34  niklas
-- BT-CH-0091: Reanalyse a boundable edge only if its domain grows.
--
-- Revision 1.1  2005/10/26 14:09:46  niklas
-- First version.
--


with Ada.Strings;
with Ada.Strings.Fixed;
with ESF;


package body Basic_Output is


   --
   --   Basic output operations
   --


   procedure Flush_Standard_Output
   is
      use Ada.Text_IO;
   begin

      if Col (Standard_Output) > 1 then

         New_Line;

      end if;

      Flush (Standard_Output);

   end Flush_Standard_Output;


   procedure Line (
      Channel      : in Ada.Text_IO.File_Type;
      Key          : in String;
      Program_File : in String;
      Source_File  : in String;
      Call_Path    : in String;
      Statements   : in String;
      Data         : in String)
   is
   begin

      Ada.Text_IO.Set_Col (Channel, 1);
      -- Ends any partially written line, moves to left margin.

      Ada.Text_IO.Put_Line (
           Channel,
           Key          & Field_Separator
         & Program_File & Field_Separator
         & Source_File  & Field_Separator
         & Call_Path    & Field_Separator
         & Statements   & Field_Separator
         & Data);

      if ESF.Is_Open
      and then (   Key = Warning_Key
                or Key = Error_Key  )
      then

         ESF.Put_ESF_Comment(
              Key          & Field_Separator
            & Program_File & Field_Separator
            & Source_File  & Field_Separator
            & Call_Path    & Field_Separator
            & Statements   & Field_Separator
            & Data);

         null;

      end if;

   end Line;


   procedure Fault (
      Location : in String;
      Text     : in String)
   is
   begin

      Flush_Standard_Output;

      Line (
         Channel      => Ada.Text_IO.Standard_Error,
         Key          => Fault_Key,
         Program_File => "",
         Source_File  => "",
         Call_Path    => "",
         Statements   => "",
         Data         => Location
                       & Field_Separator
                       & Text);

   end Fault;

   --
   --   Supporting utility operations
   --


   function Trim (Item : String) return String
   is
   begin

      return Ada.Strings.Fixed.Trim (Item, Ada.Strings.Both);

   end Trim;


   function Image (Item : Integer) return String
   is

      Img : constant String := Integer'Image (Item);

   begin

      if Img (Img'First) /= ' ' then

         return Img;

      else

         return Img (Img'First + 1 .. Img'Last);

      end if;

   end Image;


end Basic_Output;
