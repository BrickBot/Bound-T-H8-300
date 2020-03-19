-- Arithmetic.Opt (body)
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
-- $Date: 2015/10/24 20:05:45 $
--
-- $Log: arithmetic-opt.adb,v $
-- Revision 1.3  2015/10/24 20:05:45  niklas
-- Moved to free licence.
--
-- Revision 1.2  2013-02-12 08:47:19  niklas
-- BT-CH-0245: Global volatile cells and "volatile" assertions.
--
-- Revision 1.1  2011-08-31 04:17:12  niklas
-- Added for BT-CH-0222: Option registry. Option -dump. External help files.
--


with Options;
with Options.Groups;


package body Arithmetic.Opt is


   procedure When_Analysis_Reset (Option : in Options.Option_Ref)
   --
   -- To be done after the Analysis_Opt option is reset.
   --
   is
   begin

      case Analysis_Opt.Value is
      when False => Analysis := Disabled;
      when True  => Analysis := Automatic;
      end case;

   end When_Analysis_Reset;


   procedure When_Analysis_Set (
      Option : in Options.Option_Ref;
      Value  : in String)
   --
   -- To be done after the Analysis_Opt option is set.
   -- Reflects the new value in the Analysis option.
   --
   is
   begin

      case Analysis_Opt.Value is
      when False => Analysis := Disabled;
      when True  => Analysis := Enforced;
      end case;

   end When_Analysis_Set;


begin  -- Arithmetic.Opt

   Options.Register (
      Option => Analysis_Opt'access,
      Name   => "arithmetic",
      Groups => (Options.Groups.Analysis,
                 Options.Groups.Arithmetic),
      Reset  => When_Analysis_Reset'access,
      Set    => When_Analysis_Set'access);

   Options.Register (
      Option => Ref_Choice_Opt'access,
      Name   => "arith_ref",
      Group  => Options.Groups.Arithmetic);

   Options.Register (
      Option => Swap_Small_Negatives_Opt'access,
      Name   => Options.Imp_Item ("sw_neg"),
      Groups => (Options.Groups.Imp,
                 Options.Groups.Arithmetic));

   Options.Register (
      Option => Warn_Signing_Literal_Opt'access,
      Name   => Options.Warn_Item ("sign"),
      Groups => (Options.Groups.Arithmetic,
                 Options.Groups.Warn));

   Options.Register (
      Option => Show_Plus_Sign_Opt'access,
      Name   => "show_plus_sign",
      Group  => Options.Groups.Arithmetic);

   Options.Register (
      Option => Bound_Bitwise_Ops_Opt'access,
      Name   => "bitwise_bounds",
      Group  => Options.Groups.Arithmetic);

   Options.Register (
      Option => Max_Shift_Mul_Bits_Opt'access,
      Name   => "calc_shift_mul_max",
      Group  => Options.Groups.Arithmetic);

   Options.Register (
      Option => Warn_Large_Shift_Opt'access,
      Name   => Options.Warn_Item ("shift"),
      Groups => (Options.Groups.Arithmetic,
                 Options.Groups.Warn));

   Options.Register (
      Option => Check_Dup_Target_Opt'access,
      Name   => Options.Imp_Item ("dup_target"),
      Groups => (Options.Groups.Imp,
                 Options.Groups.Arithmetic));

   Options.Register (
      Option => Trace_Alias_Opt'access,
      Name   => Options.Trace_Item ("alias"),
      Groups => (Options.Groups.Arithmetic,
                 Options.Groups.Trace));

end Arithmetic.Opt;
