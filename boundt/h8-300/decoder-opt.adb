-- Decoder.Opt (body)
--
-- Authors:
--    Samuel Petersson, Mälardalen University
--    Niklas Holsti, Tidorum Ltd
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
-- $Revision: 1.16 $
-- $Date: 2015/10/29 15:35:41 $
--
-- $Log: decoder-opt.adb,v $
-- Revision 1.16  2015/10/29 15:35:41  niklas
-- Added call to Format.Opt.Handle_Wild_Option.
--
-- Revision 1.15  2015/10/26 22:19:13  niklas
-- Version 4b, and moved to free open-source licence.
--
-- Revision 1.14  2015/10/26 22:01:05  niklas
-- Updated to use current Options services.
--
-- Revision 1.13  2008/10/29 14:46:38  niklas
-- Updated for BT-CH-0129: Choose_Device, List_Device_Names.
--
-- Revision 1.12  2008/04/29 20:09:46  niklas
-- Updated to include Set_Warn_Option.State.
--
-- Revision 1.11  2007/12/27 23:12:32  niklas
-- BT-CH-0104: S-record program format for Bound-T/H8-300.
--
-- Revision 1.10  2007/05/03 07:43:27  niklas
-- Extended Set_Trace_Option and List_Trace_Options to use the
-- corresponding operations of Format.Opt. Defined PL as an
-- abbreviation of Put_Line for use in List_Options and others.
--
-- Revision 1.9  2007/03/01 11:55:55  niklas
-- Updated for BT-CH-0046.
--
-- Revision 1.8  2007/02/15 13:16:41  Niklas
-- Updated for BT-CH-0045.
--
-- Revision 1.7  2005/08/25 20:52:01  niklas
-- Added the option -srec to load more code or data from
-- an S-record file, for example the Lego Mindstorms ROM.
-- Added the option -sym to load and define more symbols
-- from a text file, for example the ROM subprograms.
--
-- Revision 1.6  2005/07/26 12:11:17  niklas
-- Added -mint16 and -mint32 for GCC.
--
-- Revision 1.5  2005/05/09 15:56:59  niklas
-- Added support of IAR compiler.
--
-- Revision 1.4  2005/03/22 20:44:05  niklas
-- Added the option Unsigned_Cond, with syntax "-bcc=signed"
-- or "-bcc=unsigned", to support arithmetic analysis.
--
-- Revision 1.3  2005/01/26 21:00:37  niklas
-- Updated the display of the -stack=xxx option in List_Options and
-- added a remark on the default value.
--
-- Revision 1.2  2005/01/26 20:11:05  niklas
-- Added options -device, -stack, -read_ws, -write_ws.
-- Added calls to Format.Set_Option and H8_300.Devices.Set_Option.
-- Added procedure Finish to cross-check and adjust option values.
--
-- Revision 1.1  2004/06/16 07:41:34  niklas
-- First version.
--


with Decoder.GCC.Opt;
with Decoder.IAR.Opt;
with Devices;
with Format.Opt;
with Formats.In_Memory.Opt;
with H8_300.Devices;
with H8_300.Opt;
with Memory_Opt;
with Options.Groups;
with Options.H8_300;
with Output;


package body Decoder.Opt is


   function Unsigned_Cond return Boolean
   is
   begin

      return Cond_Sign_Opt.Value = Unsigned;

   end Unsigned_Cond;


   --
   ---   Setting other options
   --


   procedure Handle_Wild_Option (
      Name  : in     String;
      Valid :    out Boolean)
   is
   begin

      Valid := True;
      -- We are optimistic...

      -- Perhaps a device name?

      begin
            
         Options.Set_Option (
            Name  => H8_300.Opt.Device_Option_Name,
            Value => Name);
         
         return;
         
      exception
      
      when Constraint_Error =>
      
         null; -- Output.Note ("Not a device name: " & Name);
         
      end;
      
      if Name = "-mint16" then

         GCC.Opt.Int_Size := GCC.Opt.Bits16;

      elsif Name = "-mint32" then

         GCC.Opt.Int_Size := GCC.Opt.Bits32;

      else

         -- Perhaps a format name?
      
         Format.Opt.Handle_Wild_Option (
            Name  => Name,
            Valid => Valid);
          
         if not Valid then
            -- Perhaps an IAR compiler option?

            IAR.Opt.Handle_Wild_Option (Name, Valid);
            
         end if;

      end if;

   end Handle_Wild_Option;


   procedure Set_No_Deallocation
   is
   begin

      Formats.In_Memory.Opt.Deallocate := False;
      H8_300.Devices.Deallocate        := False;
      Memory_Opt.Deallocate            := False;

   end Set_No_Deallocation;


   procedure Finish
   is
      use type Devices.Device_Ref;
   begin

      Decoder.IAR.Opt.Finish;

      if H8_300.Opt.Device_Opt.Value = null then

         Output.Error ("No -device was specified.");

         raise Decoder_Error;

      end if;

      H8_300.Devices.Initialize (
         H8_300.Devices.Device_T'Class (H8_300.Opt.Device_Opt.Value.all));

   end Finish;


begin  -- Decoder.Opt

   Options.Register (
      Option => Cond_Sign_Opt'Access,
      Name   => "bcc",
      Groups => (Options.Groups.Arithmetic,
                 Options.Groups.Control_Flow,
                 Options.H8_300.Group));

   Options.Register (
      Option => S_Record_File_Opt'Access,
      Name   => "srec",
      Groups => (Options.Groups.Inputs,
                 Options.H8_300.Group));

end Decoder.Opt;
