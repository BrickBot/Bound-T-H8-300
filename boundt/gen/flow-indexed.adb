-- Flow.Indexed (body)
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
-- $Revision: 1.8 $
-- $Date: 2015/10/24 20:05:48 $
--
-- $Log: flow-indexed.adb,v $
-- Revision 1.8  2015/10/24 20:05:48  niklas
-- Moved to free licence.
--
-- Revision 1.7  2013/12/08 20:29:21  niklas
-- Extended the Apply operation to emit a Note if it does nothing else,
-- that is, if the given bounds are not arithmetic.
--
-- Revision 1.6  2008-04-22 13:27:52  niklas
-- Added Edge_T.Allowed, to bound the Index values a priori.
--
-- Revision 1.5  2007/10/28 09:32:45  niklas
-- BT-CH-0092: Arithmetic analysis of dynamic data refs is optional.
--
-- Revision 1.4  2007/03/10 10:48:50  niklas
-- Corrected Apply_Arithmetic to redispatch Image.
--
-- Revision 1.3  2006/02/27 09:20:31  niklas
-- Added Report_Bounds, typically used for warnings about
-- address tables that are assumed to be constant.
--
-- Revision 1.2  2005/06/14 17:07:29  niklas
-- Changed Notes to Traces, conditional on "-trace resolve".
--
-- Revision 1.1  2005/02/16 21:11:44  niklas
-- BT-CH-0002.
--


with Ada.Tags;
with Flow.Opt;
with Output;


package body Flow.Indexed is


   function Basis (Item : Edge_T) return Storage.Cell_List_T
   is
   begin

      return Arithmetic.Cells_Used (By => Item.Index);

   end Basis;


   procedure Add_Basis_Cells (
      From : in     Edge_T;
      To   : in out Storage.Cell_Set_T)
   is
   begin

      Arithmetic.Add_Cells_Used (
         By   => From.Index,
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
         By    => From.Index,
         Refs  => True,
         To    => To,
         Added => Added);

   end Add_Basis_Cells;


   function Is_Used (Cell : Storage.Cell_T; By : Edge_T)
   return Boolean
   is
   begin

      return Arithmetic.Is_Used (Cell => Cell, By => By.Index);

   end Is_Used;


   function Image (Item : Edge_T) return String
   is

      Most : constant String :=
           Ada.Tags.Expanded_Name (Edge_T'Class (Item)'Tag)
         & '['
         & Storage.Bounds.Image (
              Item => Item.Allowed,
              Name => Arithmetic.Image (Item.Index))
         & "] mod"
         & Storage.Bounds.Positive_Value_T'Image (Item.Aligned);
      --
      -- Most of the image, lacking only the Traced part.

   begin

      if Storage.Bounds.Void (Item.Traced) then
         -- No index values traced so far.

         return Most;

      else
         -- Some index values traced.

         return
              Most
            & " less "
            & Storage.Bounds.Image (Item.Traced, "index");

     end if;

   end Image;


   procedure Apply (
      Bounds : in     Storage.Bounds.Bounds_T'Class;
      Upon   : in out Edge_T;
      Graph  : in     Graph_T)
   is
   begin

      if Bounds in Arithmetic.Bounds_T'Class then
         -- Goody, we can bound expressions.

         Apply_Arithmetic (
            Bounds => Arithmetic.Bounds_T'Class (Bounds),
            Upon   => Edge_T'Class (Upon),
            Graph  => Graph);

      else
         -- Some wild type of bounds.

         Output.Note (
              "Flow.Indexed.Apply ignored bounds"
            & Output.Field_Separator
            & Bounds.Full_Image);

      end if;

   end Apply;


   procedure Apply_Arithmetic (
      Bounds : in     Arithmetic.Bounds_T'Class;
      Upon   : in out Edge_T;
      Graph  : in     Graph_T)
   is
      use type Storage.Bounds.Interval_T;

      Bounds_Interval : Storage.Bounds.Interval_T;
      -- The values of Upon.Index allowed by the Bounds.

      Index_Interval : Storage.Bounds.Interval_T;
      -- Bounds_Interval intersected with Upon.Allowed.

   begin

      if Opt.Trace_Flow_Resolution then

         Output.Trace (
              "Applying "
             & Arithmetic.Image (Bounds)
             & " upon "
             & Image (Edge_T'Class (Upon)));

      end if;

      Bounds_Interval := Arithmetic.Interval (Upon.Index, Bounds);
      Index_Interval  := Bounds_Interval and Upon.Allowed;

      if Opt.Trace_Flow_Resolution then

         Output.Trace (
              "Index-bounds interval is "
            & Storage.Bounds.Image (Bounds_Interval, "index"));

         Output.Trace (
              "Allowed Index interval is "
            & Storage.Bounds.Image (Index_Interval, "index"));

      end if;

      declare

         New_Values : constant Storage.Bounds.Value_List_T :=
            Storage.Bounds.Difference (
               Included => Index_Interval,
               Excluded => Upon.Traced,
               Aligned  => Upon.Aligned);
         -- The new index values allowed by the Bounds but not
         -- yet Traced. This will raise Storage.Bounds.Unbounded if
         -- the Bounds are too loose (too many New_Values).

      begin

         Report_Bounds (
            Edge     => Edge_T'Class (Upon),
            Interval => Index_Interval,
            Values   => New_Values);

         if New_Values'Length > 0 then
            -- There are some new index values, so the Graph
            -- seems to be growing by this dynamic edge.

            for N in New_Values'Range loop

               Index (
                  Edge  => Edge_T'Class (Upon),
                  Value => New_Values(N),
                  Graph => Graph);

            end loop;

            Upon.Traced := Index_Interval;

         else
            -- No new index values; this edge seems stable.

            if Opt.Trace_Flow_Resolution then

               Output.Trace ("Indexable edge seems stable.");

            end if;

            Mark_Stable (Upon);

         end if;

      end;

   exception

   when Storage.Bounds.Unbounded =>

      if Opt.Trace_Flow_Resolution then

         Output.Trace ("Too loose bounds for indexing edge.");

      end if;

   end Apply_Arithmetic;


   procedure Report_Bounds (
      Edge     : in out Edge_T;
      Interval : in     Storage.Bounds.Interval_T;
      Values   : in     Storage.Bounds.Value_List_T)
   is
   begin

      null;

   end Report_Bounds;


end Flow.Indexed;
