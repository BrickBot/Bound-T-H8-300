-- Bounds.Looping.Opt (body)
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
-- $Revision: 1.7 $
-- $Date: 2015/10/24 20:05:46 $
--
-- $Log: bounds-looping-opt.adb,v $
-- Revision 1.7  2015/10/24 20:05:46  niklas
-- Moved to free licence.
--
-- Revision 1.6  2012-02-13 17:52:19  niklas
-- BT-CH-0230: Options -max_loop and -max_stack for spurious bounds.
--
-- Revision 1.5  2011-09-08 08:54:19  niklas
-- Added option "warn sw_neg_step", Warn_Swap_Small_Negative_Step.
--
-- Revision 1.4  2011-09-08 08:46:49  niklas
-- Added option "sw_neg_step", Swap_Small_Negative_Step.
--
-- Revision 1.3  2011-08-31 04:23:33  niklas
-- BT-CH-0222: Option registry. Option -dump. External help files.
--
-- Revision 1.2  2009-11-27 11:28:06  niklas
-- BT-CH-0184: Bit-widths, Word_T, failed modular analysis.
--
-- Revision 1.1  2008/05/03 09:22:05  niklas
-- BT-CH-0126: Combining joint-counter and each-counter analysis.
--


with Options;
with Options.Groups;


package body Bounds.Looping.Opt is


   procedure Set_Joint_Counter (State : Boolean)
   is
   begin

      if State then
         -- "-joint_counter" => Joint.

         Proc := Joint;

      else
         -- "-no_joint_counter" => Trad.

         Proc := Trad;

      end if;

   end Set_Joint_Counter;


begin  -- Bounds.Looping.Opt

   -- TBM: Register Use_Positive_Form_Opt under some name.

   Options.Register (
      Option => Trace_Counters_Opt'access,
      Name   => Options.Trace_Item ("counters"),
      Groups => (Options.Groups.Loops, Options.Groups.Trace));

   Options.Register (
      Option => Proc_Opt'access,
      Name   => "loop",
      Group  => Options.Groups.Analysis);

   Options.Register (
      Option => Swap_Small_Negative_Step_Opt'access,
      Name   => "sw_neg_step",
      Group  => Options.Groups.Loops);

   Options.Register (
      Option => Warn_Swap_Small_Negative_Step_Opt'access,
      Name   => Options.Warn_Item ("sw_neg_step"),
      Groups => (Options.Groups.Warn, Options.Groups.Loops));

   Options.Register (
      Option => Max_Loop_Bound_Opt'Access,
      Name   => "max_loop",
      Group  => Options.Groups.Loops);

end Bounds.Looping.Opt;
