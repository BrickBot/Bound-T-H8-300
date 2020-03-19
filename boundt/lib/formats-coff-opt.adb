-- Formats.COFF.Opt (body)
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
-- $Log: formats-coff-opt.adb,v $
-- Revision 1.2  2015/10/24 20:53:51  niklas
-- Moved to free licence.
--
-- Revision 1.1  2013-11-16 19:47:01  niklas
-- BT_CH_0251: 21020/SHARC update: Options and Formats.COFF.
--


with Options;
with Options.Groups;


package body Formats.COFF.Opt is


   procedure Set_Endian (
      Option : Options.Option_Ref;
      Value  : in String)
   --
   -- Actions to be taken after Endian_Opt is set to some value.
   -- The value is used for both "short" and "long" numbers.
   --
   is
   begin

      Short_End := Endian_Opt.Value;
      Long_End  := Endian_Opt.Value;

   end Set_Endian;


begin  -- Formats.COFF.Opt

   Options.Set_Group_Priority (
      Higher => Options.Groups.Inputs,
      Lower  => Group);

   Options.Register (
      Option => Trace_Loading_Opt'Access,
      Name   => Options.Trace_Item ("coff"),
      Groups => (Options.Groups.Inputs, Options.Groups.Trace, Group));

   Options.Register (
      Option => Warn_Storage_Class_Opt'Access,
      Name   => Options.Warn_Item ("coff_sc"),
      Groups => (Options.Groups.Warn, Group));

   Options.Register (
      Option => Endian_Opt'Access,
      Name   => "coff_endian",
      Groups => (Options.Groups.Inputs, Group),
      Set    => Set_Endian'Access);

   Options.Register (
      Option => Symbol_Entry_Length_Opt'Access,
      Name   => "coff_sym_length",
      Groups => (Options.Groups.Inputs, Group));

end Formats.COFF.Opt;
