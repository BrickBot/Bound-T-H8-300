-- Calculator.Opt (body)
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
-- $Date: 2015/10/24 20:05:46 $
--
-- $Log: calculator-opt.adb,v $
-- Revision 1.4  2015/10/24 20:05:46  niklas
-- Moved to free licence.
--
-- Revision 1.3  2013-07-21 18:29:21  niklas
-- Registered calc_join_steps.
--
-- Revision 1.2  2011-09-01 20:02:15  niklas
-- Registered Prog_Calculator as "-calculator".
--
-- Revision 1.1  2011-08-31 04:17:12  niklas
-- Added for BT-CH-0222: Option registry. Option -dump. External help files.
--


with Options.Groups;
with Output;


package body Calculator.Opt is


   function Min_Int_Calc return Arithmetic.Value_T
   is
      use type Arithmetic.Value_T;
   begin

      return - Max_Int_Calc;

   end Min_Int_Calc;


   procedure Set_Calc_Mod_Max (
      Option : in Options.Option_Ref;
      Value  : in String)
   --
   -- Actions after Setting the Option -calc_mod_max to the Value.
   --
   is
      use Arithmetic;
      use type Arithmetic.Value_T;

      New_Width : Arithmetic.Width_T;
      -- The new value of

      Power : Value_T;
      -- Two to the power of the new max width.

   begin

      New_Width := Options.Width.Option_T (Option.all).Value;

      Power := 2 ** Natural (New_Width);

      if Max_Int_Calc < Power then

         Output.Warning (
              "Setting -calc_mod_max to "
            & Value
            & " implies -calc_max "
            & Image (Power)
            & " (was "
            & Image (Max_Int_Calc)
            & ").");

         Max_Int_Calc := Power;

      end if;

   end Set_Calc_Mod_Max;


   Arithmetic_Warn : constant Options.Groups_T := (
      Options.Groups.Arithmetic,
      Options.Groups.Warn);
   --
   -- Those Calculator options that control warnings.


   Arithmetic_Trace : constant Options.Groups_T := (
      Options.Groups.Arithmetic,
      Options.Groups.Trace);
   --
   -- Those Calculator options that control tracing.


   Arithmetic_Outputs : constant Options.Groups_T := (
      Options.Groups.Arithmetic,
      Options.Groups.Outputs);
   --
   -- Those Calculator options that control output files.


   Arithmetic_Imp : constant Options.Groups_T := (
      Options.Groups.Arithmetic,
      Options.Groups.Imp);
   --
   -- Those Calculator options that control implementation choices.


begin  -- Calculator.Opt

   Options.Register (
      Option => Prog_Calculator'access,
      Name   => "calculator",
      Group  => Options.Groups.Arithmetic);

   Options.Register (
      Option => Max_Int_Calc_Opt'access,
      Name   => "calc_max",
      Groups => Arithmetic_Imp);

   Options.Register (
      Option => Warn_Large_Literal_Opt'access,
      Name   => Options.Warn_Item ("large"),
      Groups => Arithmetic_Warn);

   Options.Register (
      Option => Max_Modular_Width_Opt'access,
      Name   => "calc_mod_max",
      Group  => Options.Groups.Arithmetic,
      Set    => Set_Calc_Mod_Max'access);

   Options.Register (
      Option => Warn_Large_Width_Opt'access,
      Name   => Options.Warn_Item ("wide"),
      Groups => Arithmetic_Warn);

   Options.Register (
      Option => Use_Convex_Hull_Opt'access,
      Name   => Options.Imp_Item ("convexhull"),
      Groups => Arithmetic_Imp);

   Options.Register (
      Option => Limit_Method'access,
      Name   => Options.Imp_Item ("limit"),
      Groups => Arithmetic_Imp);

   Options.Register (
      Option => Initial_Limit_Sill_Opt'access,
      Name   => Options.Imp_Item ("limit_sill"),
      Groups => Arithmetic_Imp);

   Options.Register (
      Option => Check_Hull_Limit_Opt'access,
      Name   => Options.Imp_Item ("check_limit"),
      Groups => Arithmetic_Imp);

   Options.Register (
      Option => Warn_Hull_Loose_Opt'access,
      Name   => Options.Warn_Item ("hull"),
      Groups => Arithmetic_Warn);

   Options.Register (
      Option => Trace_IO_Opt'access,
      Name   => Options.Trace_Item ("calc_full"),
      Groups => Arithmetic_Trace);

   Options.Register (
      Option => Trace_Comments_Opt'access,
      Name   => Options.Trace_Item ("calc"),
      Groups => Arithmetic_Trace);

   Options.Register (
      Option => Keep_Files_Opt'access,
      Name   => "keep_om",
      Groups => Arithmetic_Outputs);

   Options.Register (
      Option => Calc_Input_File'access,
      Name   => "calc_input_prefix",
      Groups => Arithmetic_Outputs);

   Options.Register (
      Option => Calc_Output_File'access,
      Name   => "calc_output_prefix",
      Groups => Arithmetic_Outputs);

   Options.Register (
      Option => Join_Steps_Opt'access,
      Name   => "calc_join_steps",
      Groups => Arithmetic_Imp);

   Options.Register (
      Option => Find_Null_Flow_Opt'access,
      Name   => "arith_flow",
      Group  => Options.Groups.Arithmetic);

end Calculator.Opt;
