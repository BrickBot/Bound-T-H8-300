-- Output.Opt (body)
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
-- $Date: 2015/10/24 20:05:50 $
--
-- $Log: output-opt.adb,v $
-- Revision 1.2  2015/10/24 20:05:50  niklas
-- Moved to free licence.
--
-- Revision 1.1  2011-08-31 04:17:13  niklas
-- Added for BT-CH-0222: Option registry. Option -dump. External help files.
--


with Options.Groups;


package body Output.Opt is


   procedure When_Field_Separator_Reset (Option : in Options.Option_Ref)
   --
   -- To be done after the Field_Separator is reset: reset also
   -- the Basic_Output field separator to the same value.
   --
   is
   begin

      Basic_Output.Field_Separator := Field_Separator;

   end When_Field_Separator_Reset;


   procedure When_Field_Separator_Set (
      Option : in Options.Option_Ref;
      Value  : in String)
   --
   -- To be done after the Field_Separator is set: set also
   -- the Basic_Output field separator to the same value.
   --
   is
   begin

      Basic_Output.Field_Separator := Field_Separator;

   end When_Field_Separator_Set;


   procedure When_Show_Notes_Reset (Option : in Options.Option_Ref)
   --
   -- To be done after Show_Notes is reset: reset also
   -- the Basic_Output Show_Notes option to the same value.
   --
   is
   begin

      Basic_Output.Show_Notes := Show_Notes;

   end When_Show_Notes_Reset;


   procedure When_Show_Notes_Set (
      Option : in Options.Option_Ref;
      Value  : in String)
   --
   -- To be done after Show_Notes is set: set also
   -- the Basic_Output Show_Notes option to the same value.
   --
   is
   begin

      Basic_Output.Show_Notes := Show_Notes;

   end When_Show_Notes_Set;


   function Show_Surrounding_Lines return Boolean
   is
   begin

      return Source_Line_Precision.Value = Around;

   end Show_Surrounding_Lines;


   Detail_Group : constant Options.Group_Name_T :=
      Options.Group ("output_details");
   --
   -- The options that control the details of the (basic) output,
   -- such as the field-separator character.


   Output_Detail : constant Options.Groups_T := (
      Options.Groups.Outputs, Detail_Group);
   --
   -- The groups for most of our options.


begin  -- Output.Opt

   Options.Register (
      Option => Field_Separator_Opt'access,
      Name   => "output_sep",
      Groups => Output_Detail,
      Reset  => When_Field_Separator_Reset'access,
      Set    => When_Field_Separator_Set'access);

   Options.Register (
      Option => Show_Notes_Opt'access,
      Name   => "verbose",
      Groups => Output_Detail,
      Reset  => When_Show_Notes_Reset'access,
      Set    => When_Show_Notes_Set'access);

   Options.Register (
      Option => Show_Notes_Opt'access,
      Name   => "v",
      Groups => Output_Detail,
      Reset  => When_Show_Notes_Reset'access,
      Set    => When_Show_Notes_Set'access);

   Options.Register (
      Option => Source_File_Form_Opt'access,
      Name   => "source",
      Groups => Output_Detail);

   Options.Register (
      Option => Show_Code_Addresses_Opt'access,
      Name   => "address",
      Groups => Output_Detail);

   Options.Register (
      Option => Source_Line_Precision'access,
      Name   => "lines",
      Groups => Output_Detail);

   Options.Register (
      Option => Trace_Locus_Nesting_Opt'access,
      Name   => Options.Trace_Item ("locus_nesting"),
      Group  => Options.Groups.Trace);

   Options.Register (
      Option => Trace_Current_Locus_Opt'access,
      Name   => Options.Trace_Item ("locus"),
      Group  => Options.Groups.Trace);

   Options.Register (
      Option => Max_Warnings_Opt'access,
      Name   => "max_warn",
      Group  => Options.Groups.Resource_Limits);

   Options.Register (
      Option => Max_Errors_Opt'access,
      Name   => "max_err",
      Group  => Options.Groups.Resource_Limits);

   Options.Register (
      Option => Max_Faults_Opt'access,
      Name   => "max_fault",
      Group  => Options.Groups.Resource_Limits);

end Output.Opt;
