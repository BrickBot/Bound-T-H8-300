-- Flow.Origins.For_Flow (body)
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
-- $Date: 2015/10/24 20:05:48 $
--
-- $Log: flow-origins-for_flow.adb,v $
-- Revision 1.2  2015/10/24 20:05:48  niklas
-- Moved to free licence.
--
-- Revision 1.1  2013/12/08 22:05:57  niklas
-- BT-CH-0259: Storing value-origin analysis results in execution bounds.
--


with Arithmetic;
with Flow.Opt;
with Flow.Show;
with Output;


package body Flow.Origins.For_Flow is


   --
   ---   Resolving dynamic jumps using "copy initial value"
   ---   transfer functions
   --


   type Origin_Function_T is new Arithmetic.Transfer_Function_T
   with record
      Map  : Map_Ref;
      Step : Step_T;
   end record;
   --
   -- Represents the arithmetic transfer function from the subprogram entry
   -- up to but not including the source Step of a given dynamic edge, in so
   -- far as this function preserves initial values of cells or copies
   -- them to other cells, as shown by the value-origin Map.
   --
   -- Note that the effect of the source Step itself is not included.


   -- overriding
   function Is_Initial_Value (
      Cell : Storage.Cell_T;
      From : Storage.Cell_T;
      Thru : Origin_Function_T)
   return Boolean;


   -- overriding
   function Definition (
      Cell : Storage.Cell_T;
      Thru : Origin_Function_T)
   return Arithmetic.Expr_Ref;
   --
   -- The result is always a single cell (Is_Initial_Value) or Unknown.


   --    Origin_Function_T operation bodies


   -- overriding
   function Is_Initial_Value (
      Cell : Storage.Cell_T;
      From : Storage.Cell_T;
      Thru : Origin_Function_T)
   return Boolean
   is
      use type Storage.Cell_T;

      Origin : Origin_T;
      -- The origin of the Cell.

   begin

      if not Origin_Is_Known (Cell, Thru.Map) then
         -- This cell is not included in our analysis.

         return False;

      else

         Origin := Origin_Before (
            Step  => Thru.Step,
            Cell  => Cell,
            From  => Thru.Map);

         return   Origin.Kind = Initial
         and then Origin.Cell = From;

      end if;

   end Is_Initial_Value;


   function Definition (
      Cell : Storage.Cell_T;
      Thru : Origin_Function_T)
   return Arithmetic.Expr_Ref
   is

      Origin : Origin_T;
      -- The origin of the Cell.

   begin

      if not Origin_Is_Known (Cell, Thru.Map) then
         -- This cell is not included in our analysis.

         return Arithmetic.Unknown;

      else

         Origin := Origin_Before (
            Step  => Thru.Step,
            Cell  => Cell,
            From  => Thru.Map);

         case Origin.Kind is

         when Initial =>

            return Arithmetic.Expr (Origin.Cell);

         when others =>

            return Arithmetic.Unknown;

         end case;

      end if;

   end Definition;


   procedure Resolve_Boundable_Edges (Map : in Map_Ref)
   is

      Model : constant Flow.Computation.Model_Handle_T :=
         Flow.Origins.Computation (Under => Map);
      -- The computation model for this Map.

      Edges : constant Dynamic_Edge_List_T :=
         Flow.Computation.Unstable_Dynamic_Edges (Model.all);
      -- All the feasible dynamic edges that are not yet stably
      -- resolved or stably unresolved.

      Origin_Function : Origin_Function_T := (
         Map  => Map,
         Step => No_Step);
      -- The cell origins on entry to the source-step of a dynamic jump.
      -- Initialized with the components that do not depend on the
      -- particular jump under analysis and null for the other components.

      Edge : Dynamic_Edge_T;
      -- One of the Edges.

      Step : Step_T;
      -- The source of the Edge.

      Step_Mark : Output.Nest_Mark_T;
      -- The locus of the Step.

   begin

      for E in Edges'Range loop

         Edge := Edges(E);

         if Role (Edge.all) = Boundable_Jump then
            -- We look only at dynamic jumps.

            Step := Source (Edge.all);

            Step_Mark := Output.Nest (Show.Locus (
               Step   => Step,
               Source => Symbol_Table (Map)));

            Origin_Function.Step := Step;
            -- This specializes the Origin_Function to consider the
            -- source step of this particular Edge.

            if Flow.Opt.Trace_Flow_Resolution then

               Output.Trace (
                  Locus => Show.Locus (
                     Edge   => Edge.all,
                     Source => Symbol_Table (Map)),
                  Text  =>
                       "Applying value-origin analysis upon "
                     & Image (Edge.all));

            end if;

            Apply (
               Transfer => Origin_Function,
               Upon     => Edge.all,
               Graph    => Flow.Computation.Graph (Computation (Map).all));

            Output.Unnest (Step_Mark);

         end if;

      end loop;

   end Resolve_Boundable_Edges;


end Flow.Origins.For_Flow;
