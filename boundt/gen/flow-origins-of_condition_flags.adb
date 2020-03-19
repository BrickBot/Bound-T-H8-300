-- Flow.Origins.Of_Condition_Flags (body)
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
-- $Date: 2015/10/24 20:05:48 $
--
-- $Log: flow-origins-of_condition_flags.adb,v $
-- Revision 1.3  2015/10/24 20:05:48  niklas
-- Moved to free licence.
--
-- Revision 1.2  2014/06/01 10:31:03  niklas
-- Tracing output depends on Opt.Trace_Flag_Origins.
--
-- Revision 1.1  2013/12/08 22:05:57  niklas
-- BT-CH-0259: Storing value-origin analysis results in execution bounds.
--


with Arithmetic;
with Arithmetic.Logic;
with Flow.Computation;
with Flow.Origins.Opt;
with Flow.Show;
with Output;


package body Flow.Origins.Of_Condition_Flags is


   function Original_Comparison (
      Flag  : Bit_Cell_Ref;
      After : Flow.Step_T;
      Map   : Map_Ref)
   return Arithmetic.Expr_Ref
   is
      use Arithmetic;
      use type Arithmetic.Width_T;

      Result : Expr_Ref := Flag;
      -- The result, by default the same as the Flag.

      Origin : Origin_T;
      -- The origin of the Flag's value.

      Effect : Effect_Ref;
      -- The effect that defines the value of the Flag, if the Origin
      -- is an assignment (which is then part of this effect).

      Assignment : Assignment_T;
      -- The assignment that defines the value of the Flag,
      -- if the Origin is an assignment.

      Flag_Cond : Condition_T := Unknown;
      -- The condition that defines the value of the Flag,
      -- extracted from the Assignment. "Unknown" if the form
      -- of the Assignment does not define such an expression.

   begin

      if Flag.Kind  /= Arithmetic.Cell
      or Flag.Width /= 1
      then

         Output.Fault (
            Location => "Flow.Origins.Of_Condition_Flags.Original_Comparison",
            Text     =>
                 "Not a 1-bit cell"
               & Output.Field_Separator
               & Image (Flag));

      elsif not Origin_Is_Known (Flag.Cell, Map) then

         if Opt.Trace_Flag_Origins then

            Output.Trace (
                 "Origin of flag "
               & Storage.Image (Flag.Cell)
               & " is not known.");

         end if;

      else
         -- The Flag is a 1-bit cell with a known origin.
         -- We look at the origin and try to interpret it
         -- as a numerical comparison.

         Origin := Origin_After (
            Step => After,
            Cell => Flag.Cell,
            From => Map);

         case Origin.Kind is

         when Initial =>
            -- Nothing we can do.

            null;

         when Merged =>

            if Opt.Trace_Flag_Origins then

               Output.Trace (
                    "Origin of flag "
                  & Storage.Image (Flag.Cell)
                  & " is merged"
                  & Output.Field_Separator
                  & Image (Origin));

            end if;

         when Assigned =>

            Effect := Flow.Computation.Effect (
               Step  => Origin.Step,
               Under => Origins.Computation (Map).all);

            Assignment := Definition_Of (
               Target => Origin.Cell,
               Within => Effect.all);

            case Assignment.Kind is

            when Regular =>
               -- Ah, at last we know the expression that
               -- gives the flag its value.

               Flag_Cond := Assignment.Value;

            when Conditional =>
               -- Perhaps this conditional assignment encodes a
               -- condition in the form "if Cond then 1 else 0".

               Arithmetic.Logic.Decode_Boolean_Bit (
                  Ass   => Assignment,
                  Value => Flag_Cond);

               if Flag_Cond = Unknown then
                  -- Nope.

                  if Opt.Trace_Flag_Origins then

                     Output.Trace (
                          "Origin of flag is conditional"
                        & Output.Field_Separator
                        & Image (Assignment));

                  end if;

               end if;

            when others =>
               -- We don't know what to do...

               if Opt.Trace_Flag_Origins then

                  Output.Trace (
                       "Origin of flag is curious"
                     & Output.Field_Separator
                     & Image (Assignment));

               end if;

            end case;

            if Flag_Cond /= Unknown then
               -- We have a candidate expression for the
               -- value of the Flag. However, we must check
               -- that the expression has the same value at
               -- the assignment and use of the Flag.

               if Has_Same_Value (
                  After  => After,
                  Before => Origin.Step,
                  Expr   => Flag_Cond,
                  From   => Map)
               then
                  -- Yes, we can use the defining condition directly
                  -- in place of the Flag.

                  Result := Flag_Cond;

               elsif Opt.Trace_Flag_Origins then

                  Output.Trace (
                       "Origin of flag (at step"
                     & Step_Index_T'Image (Index (Origin.Step))
                     & ") is unstable"
                     & Output.Field_Separator
                     & Image (Flag_Cond));

               end if;

            end if;

         end case;

      end if;

      return Result;

   end Original_Comparison;


   function With_Flags_Integrated (
      Condition : Arithmetic.Condition_T;
      After     : Flow.Step_T;
      Map       : Map_Ref)
   return Arithmetic.Condition_T
   is
      use Arithmetic;
      use type Arithmetic.Width_T;

      Model : Flow.Computation.Model_Handle_T := Computation (Map);
      -- The underlying computation model.


      procedure Integrate_Flags (
         From : in     Condition_T;
         Into :    out Condition_T)
      --
      -- Integrates uses of condition flags Into an expression which is
      -- modified From the given expression. This is the recursive core
      -- of the expression transformer.
      --
      is

         Left, Right, Third : Expr_Ref;
         -- The one, two, or three operands of the From expression
         -- after being subjected to Integrate_Flags.

      begin

         Into := From;
         -- Default. May be changed below.

         case From.Kind is

         when Opaque | Const | Ref =>
            -- Nothing we can do.

            null;
            -- Meaning Into = From.

         when Cell =>

            if From.Width = 1 then
               -- Perhaps a condition flag.

               Into := Original_Comparison (From, After, Map);

            end if;

         when Unary_Kind =>

            -- TBC: recurse only for TBA

            Integrate_Flags (
               From => From.Expr,
               Into => Left);

            if Left /= From.Expr then
               -- Some change.

               Into := Unary (
                  Operation => From.Unary,
                  Operand   => Left,
                  Width     => From.Width);

            end if;

         when Binary_Kind =>

            -- TBC: recurse only for logical and, or.
            -- TBC: i.e. only of the operand are also 1-bit values ?

            Integrate_Flags (From => From.L_Expr, Into => Left);
            Integrate_Flags (From => From.R_Expr, Into => Right);

            if Left  /= From.L_Expr
            or Right /= From.R_Expr
            then
               -- Some change.

               Into := Binary (
                  Operation => From.Binary,
                  Left      => Left,
                  Right     => Right,
                  Width     => From.Width);

            end if;

         when Ternary_Kind =>

            Integrate_Flags (From => From.L3_Expr, Into => Left);
            Integrate_Flags (From => From.R3_Expr, Into => Right);
            Integrate_Flags (From => From.C3_Expr, Into => Third);

            if Left  /= From.L3_Expr
            or Right /= From.R3_Expr
            or Third /= From.C3_Expr
            then
               -- Some change.

               Into := Ternary (
                  Operation => From.Ternary,
                  Left      => Left,
                  Right     => Right,
                  Carry     => Third,
                  Width     => From.Width);

            end if;

         end case;

      end Integrate_Flags;


      Result : Arithmetic.Condition_T;
      -- The result, with flags integrated if possible.


   begin  -- With_Flags_Integrated

      Integrate_Flags (From => Condition, Into => Result);

      return Result;

   end With_Flags_Integrated;


   procedure Integrate_In_Edge_Conditions (
      Within : in out Flow.Computation.Model_Ref;
      Using  : in     Map_Ref)
   is
      use type Arithmetic.Expr_Ref;

      Graph : Graph_T := Flow.Computation.Graph (Under => Within);
      -- The flow-graph of the subprogram in question.

      Conditions_Changed : Natural := 0;
      -- The number of edge conditions changed.

      Step : Step_T;
      -- The source of the edge under work.

      Edge : Step_Edge_T;
      -- The edge under work.

      Step_Mark : Output.Nest_Mark_T;
      -- The locus of the Step.

   begin  -- Integrate_In_Edge_Conditions

      if Opt.Trace_Flag_Origins then

         Output.Trace (
            Locus => Flow.Show.Locus (Graph, Symbol_Table (Using)),
            Text  => "Integrating flags into edge conditions.");

      end if;

      for S in 1 .. Max_Step (Graph) loop

         Step := Step_At (Index => S, Within => Graph);

         Step_Mark := Output.Nest (Flow.Show.Locus (
            Step   => Step,
            Source => Symbol_Table (Using)));

         declare

            Edges : constant Step_Edge_List_T :=
               Flow.Computation.Edges_From (Step => Step, Under => Within);
            -- The edges departing this Step.

            Orig_Cond, New_Cond : Arithmetic.Condition_T;
            -- The original and the possibly updated condition of the Edge.

         begin

            for E in Edges'Range loop

               Edge := Edges(E);

               Orig_Cond := Flow.Computation.Condition (Edge, Within);

               New_Cond := With_Flags_Integrated (
                  Condition => Orig_Cond,
                  After     => Step,
                  Map       => Using);

               if New_Cond /= Orig_Cond then

                  if Opt.Trace_Flag_Origins then

                     Output.Trace (
                          "Integrated flags into condition for edge"
                        & Step_Edge_Index_T'Image (Index (Edge)));

                     Output.Trace (
                          "Original   condition"
                        & Output.Field_Separator
                        & Arithmetic.Image (Orig_Cond));

                     Output.Trace (
                          "Integrated condition"
                        & Output.Field_Separator
                        & Arithmetic.Image (New_Cond));

                  end if;

                  Flow.Computation.Set_Condition (
                     On    => Edge,
                     To    => New_Cond,
                     Under => Within);

                  Conditions_Changed := Conditions_Changed + 1;

               end if;

            end loop;

         end;

         Output.Unnest (Step_Mark);

      end loop;

      if Opt.Trace_Flag_Origins then

         Output.Trace (
            Locus => Flow.Show.Locus (Graph, Symbol_Table (Using)),
            Text  =>
                 "Number of changed conditions"
               & Output.Field_Separator
               & Natural'Image (Conditions_Changed));

      end if;

   end Integrate_In_Edge_Conditions;


end Flow.Origins.Of_Condition_Flags;
