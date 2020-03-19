-- Calculator.Propagator (body)
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
-- $Revision: 1.10 $
-- $Date: 2015/10/24 20:05:47 $
--
-- $Log: calculator-propagator.adb,v $
-- Revision 1.10  2015/10/24 20:05:47  niklas
-- Moved to free licence.
--
-- Revision 1.9  2009-10-07 19:26:09  niklas
-- BT-CH-0183: Cell-sets are a tagged-type class.
--
-- Revision 1.8  2007/10/03 07:27:12  niklas
-- BT-CH-0081: Fix: Size of joint effect from Flow.Life.Join_Effects.
--
-- Revision 1.7  2005/10/20 11:28:29  niklas
-- BT-CH-0015.
--
-- Revision 1.6  2005/09/12 19:02:58  niklas
-- BT-CH-0008.
--
-- Revision 1.5  2005/02/16 21:11:41  niklas
-- BT-CH-0002.
--
-- Revision 1.4  2004/05/01 20:33:02  niklas
-- First Tidorum version.
-- Updated for changes in parent package (no "use Arithmetic", need a "use
-- type" here).
-- Updated for changes in the Calculator.Formulas package, especially the use
-- of the Nil relation to mean a lack of effect, and the use of Transform
-- instead of Relation.
-- Extended the Flow_Out function to consider infeasible edges (Cond = Never)
-- and to use Cond = Arithmetic.Always rather than Cond = null.
--
-- Revision 1.3  2001/01/07 22:06:02  holsti
-- Parameters for live-cell analysis added.
--
-- Revision 1.2  2000/07/17 21:00:08  holsti
-- Cell_Set_T instead of Cell_List_T for domains and ranges.
--
-- Revision 1.1  2000/07/14 20:34:10  holsti
-- First version.
--


with Calculator.Opt;


package body Calculator.Propagator is


   use Formulas;


   function Flow_Into (
      Node  : Flow.Node_Index_T;
      Along : Edge_Index_List_T;
      Root  : Flux_T;
      Calc  : Calc_Handle_T)
   return Formulas.Identifier_T
   is

      Into_Id : constant Identifier_T := Formulas.Into_Id (Node);
      -- The calculator identifier for the flow "into" this node.

      Into : Relation_T;
      -- The "into" relation formula.

   begin

      if Along'Length = 0 then

         -- This is a "root" of the region.

         Into := Id (Root);

      else
         -- Non-root node, use incoming flows.

         Into := Nil;

         for I in Along'Range loop
            Into := Union (Into, Flow_Id (Along(I)));
         end loop;

      end if;

      Assign (
         Target => Into_Id,
         Value  => Into,
         Calc   => Calc);

      return Into_Id;

   end Flow_Into;


   function Pool_Into (
      Node  : Flow.Node_Index_T;
      Along : Edge_Index_List_T;
      Root  : Pool_T;
      Calc  : Calc_Handle_T)
   return Formulas.Identifier_T
   is

      Into_Id : constant Identifier_T := Formulas.Into_Id (Node);
      -- The calculator identifier for the pool "into" this node.

      Into : Set_T;
      -- The "into" relation formula.

   begin

      if Along'Length = 0 then

         -- This is a "root" of the region.

         Into := Id (Root);

      else
         -- Non-root node, use incoming pools.

         Into := Nil;

         for I in Along'Range loop
            Into := Union (Into, Flow_Id (Along(I)));
         end loop;

      end if;

      Assign (
         Target => Into_Id,
         Value  => Into,
         Calc   => Calc);

      return Into_Id;

   end Pool_Into;


   procedure Join_Steps_Separately (
      Steps  : in     Flow.Step_List_T;
      Domain : in     Cell_Set_T;
      Living : in     Flow.Life.Living_T;
      Onto   : in out Relation_T)
   --
   -- Joins the Living effects of the Steps, one by one, Onto the range
   -- end of the given relation.
   --
   -- This procedure does *not* try to join the effects of consecutive
   -- Steps into a single arithmetic effect. See the procedure Join_Steps,
   -- below. This procedure is retained for comparison, but is considered
   -- obsolete.
   --
   is
   begin

      for S in Steps'Range loop

         Onto := Join (
            Onto, 
            Transformation (
               Domain => Domain,
               Effect =>
                  Flow.Life.Live_Effect (
                     Step   => Steps(S),
                     Living => Living  ) ));

      end loop;

   end Join_Steps_Separately;


   procedure Join_Steps (
      Steps  : in     Flow.Step_List_T;
      Domain : in     Cell_Set_T;
      Living : in     Flow.Life.Living_T;
      Onto   : in out Relation_T)
   --
   -- Joins the Living effects of the Steps Onto the range end of
   -- the given relation, but first joins as many Steps as possible
   -- into a joint effect, before translating this effect to a
   -- relation and joining it Onto the given relation.
   --
   is

      Next : Positive := Steps'First;
      -- The index of the next step to be processed in Steps.

      Last : Natural;
      -- The maximal index such that Steps(Next .. Last) can
      -- be joined into one Effect_T.

      Joint_Effect : Arithmetic.Assignment_Set_T (
         Max_Size => 1 + Flow.Life.Max_Joint_Effect_Length (Steps, Living));
      --
      -- The set of assignments from the joined effect of
      -- Steps(Next .. Last). The added 1 makes the Max_Size
      -- positive, as required by the type.

   begin

      loop
         -- Next is in Steps'Range and shows the step for
         -- which Onto is the incoming flux.

         -- Find the longest sequence of steps Steps(Next..)
         -- that we can join into a single effect:

         Flow.Life.Join_Effects (
            Steps  => Steps(Next .. Steps'Last),
            Living => Living,
            Basis  => Domain,
            Into   => Joint_Effect,
            Last   => Last);

         -- Join this effect to Onto:

         Onto := Join (
            Onto,
            Transformation (
               Domain => Domain,
               Effect => Arithmetic.To_Effect (Joint_Effect)));

         exit when Last >= Steps'Last;

         Next := Last + 1;

      end loop;

   end Join_Steps;


   function Effect (
      Node   : Flow.Node_T;
      Domain : Cell_Set_T;
      Living : Flow.Life.Living_T;
      Calc   : Calc_Handle_T)
   return Formulas.Identifier_T
   is

      Steps : constant Flow.Step_List_T := Flow.Steps_In (Node);
      -- The steps in the node. Their effects are joined to
      -- make up the effect of the whole node.

      Eff_Id : constant Identifier_T := Formulas.Eff_Id (Flow.Index (Node));
      -- The identifier for the effect relation of this node.

      Eff : Relation_T;
      -- The effect relation formula.

   begin  -- Effect

      Eff := Nil;

      if Opt.Join_Steps then

         Join_Steps (
            Steps  => Steps,
            Domain => Domain,
            Living => Living,
            Onto   => Eff);

      else

         Join_Steps_Separately (
            Steps  => Steps,
            Domain => Domain,
            Living => Living,
            Onto   => Eff);

      end if;

      if Is_Nil (Eff) then
         -- Oops, this node contained no assignments!
         -- We have to use a dummy identity relation:

         Eff := Identity_Relation (Domain);

      end if;

      Assign (
         Target => Eff_Id,
         Value  => Eff,
         Calc   => Calc);

      return Eff_Id;

   end Effect;


   function Flow_Out (
      From  : Formulas.Identifier_T;
      Cond  : Arithmetic.Condition_T;
      Basis : Formulas.Tuple_T)
   return Formulas.Relation_T
   is
      use type Arithmetic.Expr_Ref;
   begin

      if Cond = Arithmetic.Always
      or Cond = Arithmetic.Unknown
     then
         -- Unconditional edge or an edge with an unknown condition.
         -- No (useful) constraints.

         return From;

      elsif Cond = Arithmetic.Never then
         -- Infeasible edge, empty relation.

         return Null_Relation (Domain => Basis);

      else
         -- Conditional edge implies a range constraint.

         return
            Restrict_Range (
               Relation => From,
               Subrange => Set (Cells => Basis, Cond => Cond));
      end if;

   end Flow_Out;


   function Set_Out (
      From  : Formulas.Identifier_T;
      Cond  : Arithmetic.Condition_T;
      Basis : Formulas.Tuple_T)
   return Formulas.Set_T
   is
      use type Arithmetic.Expr_Ref;
   begin

      if Cond = Arithmetic.Always
      or Cond = Arithmetic.Unknown
      then
         -- Unconditional edge or an edge with an unknown condition.
         -- No (useful) constraints.

         return From;

      elsif Cond = Arithmetic.Never then
         -- Infeasible edge, empty relation.

         return Null_Set (Cells => Basis);

      else
         -- Conditional edge implies an additional constraint.

         return
            Intersection (
               Left  => From,
               Right => Set (Cells => Basis, Cond => Cond));
      end if;

   end Set_Out;


end Calculator.Propagator;
