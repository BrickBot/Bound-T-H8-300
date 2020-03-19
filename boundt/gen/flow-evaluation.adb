-- Flow.Evaluation (body)
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
-- $Revision: 1.5 $
-- $Date: 2015/10/24 20:05:48 $
--
-- $Log: flow-evaluation.adb,v $
-- Revision 1.5  2015/10/24 20:05:48  niklas
-- Moved to free licence.
--
-- Revision 1.4  2009-01-18 08:14:38  niklas
-- Removed unused context clauses.
--
-- Revision 1.3  2007/08/20 12:23:36  niklas
-- Added support for Storage.Data by extending Transform_New_Step (both
-- versions) to obey the new precondition on Transformed_Data (effect
-- parameter is not null).
--
-- Revision 1.2  2007/07/21 18:18:41  niklas
-- BT-CH-0064. Support for AVR/IAR switch-handler analysis.
--
-- Revision 1.1  2006/05/06 06:59:21  niklas
-- BT-CH-0021.
--


with Storage;
with Storage.Bounds;


package body Flow.Evaluation is


   -- TBA throughout: aliasing.


   function Is_Bound (
      Cell  :        Storage.Cell_T;
      Under : access Data_Domain_T'Class)
   return Boolean
   is
      use Arithmetic.Evaluation;
   begin

      return Value_Of (Cell, Domain (Under)).Level = Known;

   end Is_Bound;


   procedure Refine_And_Transform (
      Pre         : in     Data_Domain_Ref;
      Raw_Effect  : in     Arithmetic.Effect_T;
      Fine_Effect : in out Arithmetic.Assignment_Set_T;
      Refined     :    out Boolean)
   --
   -- The common part of the two variants of Transform_New_Step.
   --
   -- Pre
   --    The data state on entry to the new step.
   -- Raw_Effect
   --    The raw (unrefined) effect for the new step.
   -- Fine_Effect
   --    The refined Defining assignments and all Range constraint
   --    assignments from the Raw_Effect. Defining assignments that are
   --    refined to identity assignments (cell := cell) are omitted.
   --    This set should be empty on entry.
   -- Refined
   --    Whether some parts of the Raw_Effect really were refined
   --    or omitted, so that the Fine_Effect should be used instead
   --    of the raw Effect.
   --
   -- The procedure redispatches on Pre to compute the evaluation
   -- domain.
   --
   is
      use Arithmetic.Evaluation;
      use type Arithmetic.Cell_T;

      Pre_Domain : constant Arithmetic.Evaluation.Domain_T'Class :=
         Domain (Data_Domain_Ref (Pre));
      --  The data-domain for Arithmetic.Evaluation.

      Result : Assignment_Result_T;
      -- The result of Evaluating a Defining assignment in Raw_Effect.

   begin

      -- Partially evaluate and refine the Raw_Effect:

      Refined := False;

      for R in Raw_Effect'Range loop

         if Raw_Effect(R).Kind in Arithmetic.Defining_Kind_T then
            -- A defining assignment. We try to refine it.

            Result := Eval (
               Assignment => Raw_Effect(R),
               Within     => Pre_Domain,
               Partly     => True);

            if Identity (Result) then
               -- This assignment can be refined away and would
               -- have no effect on the data state.

               Refined := True;

            else
               -- This assignment must be kept.

               if not Same (Result, Raw_Effect(R)) then
                  -- Some real refinement.

                  Refined := True;

               end if;

               Arithmetic.Add (
                  To   => Fine_Effect,
                  More => Residual (Result));

            end if;

         else
            -- Range constraint assignments are currently kept
            -- in their original form.

            -- TBA refine Min, Max, Target
    
            Arithmetic.Add (To => Fine_Effect, More => Raw_Effect(R));

         end if;

      end loop;

   end Refine_And_Transform;


   procedure Transform_New_Step (
      Data   : access Data_Domain_T;
      Step   : in     Step_T;
      Effect : in out Arithmetic.Effect_Ref;
      Post   :    out Step_Data_Ref)
   is

      Fine_Effect : Arithmetic.Assignment_Set_T (
         Max_Size => Arithmetic.Positive_Length (Effect));
      -- The refined Effect.

      Effect_Refined : Boolean;
      -- Whether the Effect was really refined.

   begin

      Refine_And_Transform (
         Pre         => Data_Domain_Ref (Data),
         Raw_Effect  => Effect.all,
         Fine_Effect => Fine_Effect,
         Refined     => Effect_Refined);

      if Effect_Refined then
         -- Return the refined, residual effect:

         Effect := Arithmetic.To_Effect_Ref (Fine_Effect);

      end if;

      if Effect'Length > 0 then
         -- The step has some effect, so the data-state may change.

         Post := Transformed_Data (
            From  => Step_Data_Ref (Data),
            After => Effect.all);

      else
         -- The step has no effect, so the data-state is unchanged.

         Post := Step_Data_Ref (Data);

      end if;

   end Transform_New_Step;


   procedure Transform_New_Step (
      Data    : access Data_Domain_T;
      Step    : in     Step_T;
      Effect  : in     Arithmetic.Effect_T;
      Refined :    out Arithmetic.Effect_Ref;
      Post    :    out Step_Data_Ref)
   is

      Fine_Effect : Arithmetic.Assignment_Set_T (
         Max_Size => Arithmetic.Positive_Length (Effect));
      -- The residual (refined) Effect.

      Effect_Refined : Boolean;
      -- Whether the Effect was really refined.

   begin

      Refine_And_Transform (
         Pre         => Data_Domain_Ref (Data),
         Raw_Effect  => Effect,
         Fine_Effect => Fine_Effect,
         Refined     => Effect_Refined);

      if Effect_Refined then
         -- Return the refined, residal effect:

         Refined := Arithmetic.To_Effect_Ref (Fine_Effect);

      else
         -- The raw Effect is just as fine.

         Refined := Arithmetic.To_Effect_Ref (Effect);

      end if;

      if Refined'Length > 0 then
         -- The refined step has some effect, so the data-state may change.

         Post := Transformed_Data (
            From  => Step_Data_Ref (Data),
            After => Refined.all);

      else
         -- The refined step has no effect, so the data-state is unchanged.

         Post := Step_Data_Ref (Data);

      end if;

   end Transform_New_Step;


   procedure Refine_New_Edge (
      Post   : access Data_Domain_T;
      Source : in     Step_T;
      Target : in     Step_Tag_T;
      Cond   : in out Arithmetic.Condition_T;
      Giving :    out Step_Data_Ref)
   is
      use type Arithmetic.Condition_T;
      use Arithmetic.Evaluation;

      Result : constant Result_T := Eval (
         Expr   => Cond,
         Within => Domain (Data_Domain_Ref (Post)),
         Partly => True);
      -- The partially evaluated Cond.

      New_Cond : constant Arithmetic.Condition_T := To_Condition (Result);
      -- The refined Condition.

   begin

      if New_Cond = Arithmetic.Never
      or New_Cond = Arithmetic.Always
      then
         -- Something impossible or uninteresting.

         Cond   := New_Cond;
         Giving := Step_Data_Ref (Post);

      else
         -- Something interesting and possible.

         Cond := New_Cond;

         Giving := Constrained_Data (
            From => Step_Data_Ref (Post),
            By   => New_Cond);

      end if;

   end Refine_New_Edge;


   procedure Apply (
      Pre   : access Data_Domain_T;
      Post  : access Data_Domain_T;
      Upon  : in out Boundable_Edge_T'Class;
      Graph : in     Graph_T)
   is
   begin

      Flow.Apply (
         Bounds => Storage.Bounds.Bounds_T'Class (
            Domain (Data_Domain_Ref (Pre))),
         Upon   => Upon,
         Graph  => Graph);

   end Apply;


   procedure Apply (
      Data   : access Data_Domain_T;
      Upon   : in     Calling.Protocol_T'Class;
      Giving :    out Calling.Protocol_Ref)
   is
   begin

      Calling.Apply (
         Bounds => Storage.Bounds.Bounds_T'Class (
            Domain (Data_Domain_Ref (Data))),
         Upon   => Upon,
         Giving => Giving);

   end Apply;


end Flow.Evaluation;
