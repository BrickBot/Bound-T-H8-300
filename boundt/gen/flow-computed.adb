-- Flow.Computed (body)
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
-- $Revision: 1.11 $
-- $Date: 2015/10/24 20:05:48 $
--
-- $Log: flow-computed.adb,v $
-- Revision 1.11  2015/10/24 20:05:48  niklas
-- Moved to free licence.
--
-- Revision 1.10  2013/12/20 21:18:29  niklas
-- Extended procedures Apply and Apply_Arithmetic to signal False_Path
-- if the bounds provide no values at all.
--
-- Revision 1.9  2013/12/08 20:27:36  niklas
-- Extended the Apply operation, in the case of non-arithmetic bounds
-- for a dynamic edge where the target is a single cell, to try to derive
-- the list of values of this cell, and thereby resolve the edge. Also,
-- this operation now emits a Note if it does nothing else.
-- Uncommmented the [not] overriding indicators, bringing them into
-- effect.
--
-- Revision 1.8  2009-11-27 11:28:07  niklas
-- BT-CH-0184: Bit-widths, Word_T, failed modular analysis.
--
-- Revision 1.7  2009-10-07 19:26:10  niklas
-- BT-CH-0183: Cell-sets are a tagged-type class.
--
-- Revision 1.6  2007/10/28 09:32:45  niklas
-- BT-CH-0092: Arithmetic analysis of dynamic data refs is optional.
--
-- Revision 1.5  2007/08/25 18:59:56  niklas
-- Corrected Apply_Arithmetic to redispatch on the Edge for the
-- Image in the Trace message. Updated wording in Traces.
--
-- Revision 1.4  2007/07/21 18:18:41  niklas
-- BT-CH-0064. Support for AVR/IAR switch-handler analysis.
--
-- Revision 1.3  2006/06/03 12:14:29  niklas
-- Changed Edge_T to keep track of the earlier resolved values
-- (as in the former Values_Edge_T) and deleted the functions
-- and parameters relating to target-step tags. This makes Edge_T
-- easier to use for dynamic calls. Deleted Values_Edge_T; use
-- instead Edge_T or the new type Flow.Computed.Tags.Edge_T.
-- Moved type Tags_Edge_T to the new child package Tags to form
-- the type Flow.Computed.Tags.Edge_T. Note, however, that the
-- new type keeps track of new resolved targets with a value list,
-- like the old Values_Edge_T, and not by looking at the existing
-- static successors of the source of the boundable edge.
--
-- Revision 1.2  2006/05/06 06:59:20  niklas
-- BT-CH-0021.
--
-- Revision 1.1  2006/04/28 09:53:36  niklas
-- First version.
--


with Ada.Tags;
with Flow.Opt;
with Output;


package body Flow.Computed is


   --
   ---   Edge_T
   --


   function Basis (Item : Edge_T) return Storage.Cell_List_T
   is
   begin

      return Arithmetic.Cells_Used (By => Item.Target);

   end Basis;


   procedure Add_Basis_Cells (
      From : in     Edge_T;
      To   : in out Storage.Cell_Set_T)
   is
   begin

      Arithmetic.Add_Cells_Used (
         By   => From.Target,
         Refs => True,
         To   => To);

   end Add_Basis_Cells;


   procedure Add_Basis_Cells (
      From  : in     Edge_T;
      To    : in out Storage.Cell_Set_T;
      Added : in out Boolean)
   is
   begin

      Arithmetic.Add_Cells_Used (
         By    => From.Target,
         Refs  => True,
         To    => To,
         Added => Added);

   end Add_Basis_Cells;


   function Is_Used (Cell : Storage.Cell_T; By : Edge_T)
   return Boolean
   is
   begin

      return Arithmetic.Is_Used (Cell => Cell, By => By.Target);

   end Is_Used;


   function Image (Item : Edge_T) return String
   is
   begin

      return
           Ada.Tags.Expanded_Name (Edge_T'Class (Item)'Tag)
         & '['
         & Arithmetic.Image (Item.Target)
         & ']';

   end Image;


   procedure Apply (
      Bounds : in     Storage.Bounds.Bounds_T'Class;
      Upon   : in out Edge_T;
      Graph  : in     Graph_T)
   is
      use type Arithmetic.Expr_Kind_T;
   begin

      if Bounds in Arithmetic.Bounds_T'Class then
         -- Goody, we can bound expressions.

         Apply_Arithmetic (
            Bounds => Arithmetic.Bounds_T'Class (Bounds),
            Upon   => Edge_T'Class (Upon),
            Graph  => Graph);

      elsif Upon.Target.Kind = Arithmetic.Cell then
         -- The target expression is a single cell, so we can hope to
         -- get the list of values from the Bounds, for any kind
         -- of Bounds. But the list may be unbounded, of course.

         declare

            Values : constant Storage.Bounds.Value_List_T :=
               Storage.Bounds.Values (
                  Cell  => Upon.Target.Cell,
                  Under => Bounds);
            -- The possible values of the Cell Under these bounds,
            -- if a reasonanbly short list can be produced.

         begin

            if Values'Length > 0 then
               -- Some values are possible.

               Add_New_Targets (
                  Edge   => Edge_T'Class (Upon),
                  Values => Values,
                  Graph  => Graph);

            else
               -- No possible values: infeasible path.

               Output.Note (
                    "Flow.Computed.Apply is infeasible"
                  & Output.Field_Separator
                  & Bounds.Full_Image);

               raise False_Path;

            end if;

         exception

         when Storage.Bounds.Unbounded =>

            Output.Note (
                 "Flow.Computed.Apply not bounded"
               & Output.Field_Separator
               & Bounds.Full_Image);

         end;

      else
         -- Don't know what to do with this...

         Output.Note (
              "Flow.Computed.Apply ignored bounds"
            & Output.Field_Separator
            & Bounds.Full_Image);

      end if;

   end Apply;


   procedure Apply_Arithmetic (
      Bounds : in     Arithmetic.Bounds_T'Class;
      Upon   : in out Edge_T;
      Graph  : in     Graph_T)
   is

      Resolved : constant Natural := Resolvents (Upon);
      -- The number of static edges already resolved
      -- from this computed edge.

      New_Resolved : Natural;
      -- The number of static edges resolved from this computed
      -- edge after we have applied our Bounds.

   begin

      -- Trace:

      if Opt.Trace_Flow_Resolution then

         Output.Trace (
              "Applying "
             & Arithmetic.Image (Bounds)
             & " upon "
             & Image (Edge_T'Class (Upon)));

      end if;

      -- Compute the possible Target values:

      declare

         Values : constant Value_List_T :=
            Arithmetic.Values (
               Expr  => Upon.Target,
               Under => Bounds);
         -- The target values allowed by the Bounds.
         -- This will raise Storage.Bounds.Unbounded if the Bounds
         -- are too loose.

      begin
         -- The Bounds were tight enough to give a Targets list.

         if Opt.Trace_Flow_Resolution then

            Output.Trace (
                 "Number of computed target values"
               & Output.Field_Separator
               & Output.Image (Natural (Values'Length)));

         end if;

         if Values'Length = 0 then
            -- No possible values: infeasible jump.

            Output.Note (
               "Flow.Computed.Apply_Arithmetic is infeasible");

            raise False_Path;

         end if;

         Report_Bounds (
            Edge   => Edge_T'Class (Upon),
            Values => Values);

         -- Filter and add new target values:

         Add_New_Targets (
            Edge   => Edge_T'Class (Upon),
            Values => Values,
            Graph  => Graph);

         -- New edges or stable state?

         New_Resolved := Resolvents (Upon);

         if New_Resolved /= Resolved then
            -- Found some new successors.

            if Opt.Trace_Flow_Resolution then

               Output.Trace (
                    "Computed "
                  & Output.Image (New_Resolved - Resolved)
                  & " new successors.");

            end if;

         else
            -- No new successors; this edge seems stable.

            if Opt.Trace_Flow_Resolution then

               Output.Trace (
                    "Computed edge seems stable with"
                  & Natural'Image (Resolved)
                  & " successors.");

            end if;

            Mark_Stable (Upon);

         end if;

      end;

   exception

   when Storage.Bounds.Unbounded =>

      if Opt.Trace_Flow_Resolution then

         Output.Trace ("Too loose bounds for resolving edge.");

      end if;

   end Apply_Arithmetic;


   procedure Add_New_Targets (
      Edge   : in out Edge_T;
      Values : in     Value_List_T;
      Graph  : in     Graph_T)
   is

      Value : Arithmetic.Value_T;
      -- A possible target value.

   begin

      -- Scan the Targets and add new edges:

      for V in Values'Range loop

         Value := Values(V);

         if not Is_Element (Edge.Traced, Value) then
            -- This is a new target value.

            Append (Edge.Traced, Value);
            -- Remember that we have processed this Target Value.

            if Opt.Trace_Flow_Resolution then

               Output.Trace (
                    "New computed target value"
                  & Output.Field_Separator
                  & Arithmetic.Image (Value));

            end if;

            Add_Target (
               Edge  => Edge_T'Class (Edge),
               Value => Value,
               Graph => Graph);

         end if;

      end loop;

   end Add_New_Targets;


   function Edge_Cond (
      Edge  : Edge_T;
      Value : Arithmetic.Value_T;
      Graph : Graph_T)
   return Arithmetic.Condition_T
   is
      use type Arithmetic.Expr_Ref;
   begin

      return Edge.Target
           = Arithmetic.Const (
                Value  => Value,
                Width  => Arithmetic.Width_Of (Edge.Target),
                Signed => True);

   end Edge_Cond;


   function Edge_Time (
      Edge  : Edge_T;
      Value : Arithmetic.Value_T;
      Graph : Graph_T)
   return Processor.Time_T
   is
   begin

      return 0;

   end Edge_Time;


   procedure Report_Bounds (
      Edge   : in out Edge_T;
      Values : in     Value_List_T)
   is
   begin

      null;

   end Report_Bounds;


end Flow.Computed;
