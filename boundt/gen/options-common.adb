-- Options.Common (body)
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
-- $Date: 2015/10/24 20:05:49 $
--
-- $Log: options-common.adb,v $
-- Revision 1.3  2015/10/24 20:05:49  niklas
-- Moved to free licence.
--
-- Revision 1.2  2011-09-01 20:00:19  niklas
-- Saying -trace effect automatically sets -trace decode.
-- Saying -trace bref automatically sets -trace decode and -trace effect.
--
-- Revision 1.1  2011-08-31 04:17:13  niklas
-- Added for BT-CH-0222: Option registry. Option -dump. External help files.
--


with Options.Groups;


package body Options.Common is


   procedure Set_Trace_Effect (
      Option : in Option_Ref;
      Value  : in String)
   --
   -- Implications of setting Trace_Effect.
   --
   is
   begin

      if Trace_Effect.Value then

         Trace_Decode.Value := True;

      end if;

   end Set_Trace_Effect;


   procedure Set_Split_Effect (
      Option : in Option_Ref;
      Value  : in String)
   --
   -- Implications of setting Split_Effect.
   --
   is
   begin

      if Split_Effect.Value then

         Trace_Decode.Value := True;
         Trace_Effect.Value := True;

      end if;

   end Set_Split_Effect;


begin  -- Options.Common

   Register (
      Option => Trace_Decode'access,
      Name   => Trace_Item ("decode"),
      Group  => Groups.Trace);

   Register (
      Option => Trace_Effect'access,
      Name   => Trace_Item ("effect"),
      Group  => Groups.Trace,
      Set    => Set_Trace_Effect'access);

   Register (
      Option => Split_Effect'access,
      Name   => Trace_Item ("bref"),
      Group  => Groups.Trace,
      Set    => Set_Split_Effect'access);

   Register (
      Option => Trace_Chains'access,
      Name   => Trace_Item ("chains"),
      Group  => Groups.Trace);

end Options.Common;
