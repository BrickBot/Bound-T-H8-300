-- Flow.Computed.Tags (body)
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
-- $Revision: 1.5 $
-- $Date: 2015/10/24 20:05:47 $
--
-- $Log: flow-computed-tags.adb,v $
-- Revision 1.5  2015/10/24 20:05:47  niklas
-- Moved to free licence.
--
-- Revision 1.4  2009-11-27 11:28:07  niklas
-- BT-CH-0184: Bit-widths, Word_T, failed modular analysis.
--
-- Revision 1.3  2007/08/20 12:19:01  niklas
-- Updated formal parameter names.
--
-- Revision 1.2  2006/06/03 12:16:11  niklas
-- Changed the default implementations of Edge_Cond and Edge_Time
-- to use Flow.Computed.Edge_Cond and Flow.Computed.Edge_Time,
-- redispatching on the Edge and omitting the Tag.
--
-- Revision 1.1  2006/06/03 11:49:11  niklas
-- First version, extracted from the parent, Flow.Computed, but
-- changing the definition of "new" targets to use the list of
-- earlier resolved values of the target expression instead of
-- the existing static successor tags of the source step. Thus,
-- the new type Edge_T corresponds to the old type Values_Edge_T.
--


with Flow.Opt;
with Output;


package body Flow.Computed.Tags is


   --
   --    Edge_T
   --


   function Edge_Cond (
      Edge   : Edge_T;
      Target : Arithmetic.Value_T;
      Tag    : Step_Tag_T;
      Graph  : Graph_T)
   return Arithmetic.Condition_T
   is
   begin

      return Edge_Cond (
         Edge  => Computed.Edge_T'Class (Edge),
         Value => Target,
         Graph => Graph);

   end Edge_Cond;


   function Edge_Time (
      Edge   : Edge_T;
      Target : Arithmetic.Value_T;
      Tag    : Step_Tag_T;
      Graph  : Graph_T)
   return Processor.Time_T
   is
   begin

      return Edge_Time (
         Edge  => Computed.Edge_T'Class (Edge),
         Value => Target,
         Graph => Graph);

   end Edge_Time;


   procedure Add_Target_Tag (
      Edge   : in out Edge_T;
      Target : in     Arithmetic.Value_T;
      Tag    : in     Step_Tag_T;
      Graph  : in     Graph_T)
   is
   begin

      Add_Resolved_Edge (
         To     => Graph,
         Source => Edge,
         Cond   => Edge_Cond (
            Edge   => Edge_T'Class (Edge),
            Target => Target,
            Tag    => Tag,
            Graph  => Graph),
         Time   => Edge_Time (
            Edge   => Edge_T'Class (Edge),
            Target => Target,
            Tag    => Tag,
            Graph  => Graph),
         Target => Tag);

   end Add_Target_Tag;


   procedure Add_Target (
      Edge   : in out Edge_T;
      Target : in     Arithmetic.Value_T;
      Graph  : in     Graph_T)
   is

      Tag : Step_Tag_T;
      -- The Target_Tag for the Target value.

   begin

      Tag := Target_Tag (
         Edge   => Edge_T'Class (Edge),
         Target => Target);
      -- This may raise False_Path.

      if Opt.Trace_Flow_Resolution then

         Output.Trace (
              "New computed target tag"
            & Output.Field_Separator
            & Arithmetic.Image (Target)
            & Output.Field_Separator
            & Image (Tag));

      end if;

      Add_Target_Tag (
         Edge   => Edge_T'Class (Edge),
         Target => Target,
         Tag    => Tag,
         Graph  => Graph);

   exception

   when False_Path =>

      Output.Note (
           "Target_Tag ("
         & Arithmetic.Image (Target)
         & " ) raised False_Path.");

   end Add_Target;


end Flow.Computed.Tags;
