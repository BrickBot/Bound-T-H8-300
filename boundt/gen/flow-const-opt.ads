-- Flow.Const.Opt (decl)
--
-- Command-line options for the constant-propagation analysis.
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
-- $Date: 2015/10/24 19:36:48 $
--
-- $Log: flow-const-opt.ads,v $
-- Revision 1.7  2015/10/24 19:36:48  niklas
-- Moved to free licence.
--
-- Revision 1.6  2013/12/20 21:09:41  niklas
-- Added Resolve_Opt (option -const_resolve), to control the use of
-- constant propagation for resolving dynamic flow and stack usage.
-- This was useful when testing SWEET for dynamic jump analysis.
--
-- Revision 1.5  2011-08-31 04:23:34  niklas
-- BT-CH-0222: Option registry. Option -dump. External help files.
--
-- Revision 1.4  2008/06/18 20:52:56  niklas
-- BT-CH-0130: Data pointers relative to initial-value cells.
--
-- Revision 1.3  2007/01/25 21:25:14  niklas
-- BT-CH-0043.
--
-- Revision 1.2  2005/02/16 21:11:43  niklas
-- BT-CH-0002.
--
-- Revision 1.1  2004/04/28 18:54:41  niklas
-- First version.
--


with Options.Bool;
with Options.Nat;


package Flow.Const.Opt is


   pragma Elaborate_Body;
   --
   -- To register the options.


   Propagate_Opt : aliased Options.Bool.Option_T (Default => True);
   --
   -- Whether to apply constant-propagation at all.
   -- If this is False, the options below are irrelevant.
   --
   Propagate : Boolean renames Propagate_Opt.Value;


   Aliasing : Storage.Alias_Level_T := Storage.Strict;
   --
   -- The alias-checking level to be used.
   -- This determines when a constant cell value is considered to
   -- pass through a flow-graph step that assigns values to some
   -- cells but not explicitly to this cell.
   --
   -- NOTE this feature is under development and TBD.


   Relative_Values_Opt : aliased Options.Bool.Option_T (Default => True);
   --
   -- Whether to include "relative" values where a cell is modelled
   -- as the initial value of some (same or other) cell plus a
   -- constant offset. Otherwise, only truly constant values are
   -- modelled.
   --
   Relative_Values : Boolean renames Relative_Values_Opt.Value;


   Trace_Iteration_Opt : aliased Options.Bool.Option_T (Default => False);
   --
   -- Whether to trace the steps in the least-fixpoint iteration that
   -- propagates constant values for cells.
   --
   Trace_Iteration : Boolean renames Trace_Iteration_Opt.Value;


   Show_Results_Opt : aliased Options.Bool.Option_T (Default => False);
   --
   -- Whether to display the results of the constant propagation.
   --
   Show_Results : Boolean renames Show_Results_Opt.Value;


   --
   ---   Refinable things
   --


   type Refinable_T is (Effect, Cond);
   --
   -- The things that can be refined (simplified, partially or fully
   -- evaluated) by constant propagation, using the knowledge of the
   -- constant values of cells flowing into the thing. (To be precise,
   -- the refinable things that can be disabled/enabled by options.)
   --
   -- Effect
   --    The effects of the steps. This includes refining the
   --    pointer expressions for dynamically accessed data.
   -- Cond
   --    The preconditions on the conditional edges.


   type Refinables_T is array (Refinable_T) of Boolean;
   --
   -- A selection of things to be refined by constant propagation.


   package Refinable_Valued is new Options.Discrete_Set_Valued (
      Item_Type              => Refinable_T,
      Set_Type               => Refinables_T,
      Value_Type_Description => "Set of Boolean",
      Item_Image             => Refinable_T'Image);


   Refine_Opt : aliased Refinable_Valued.Option_T := (
      Default | Value => (others => True),
      Enumerable      => <>);
   --
   -- What to refine, using the constant-propagation results.
   --
   Refine_Effects         : Boolean renames Refine_Opt.Value(Effect);
   Refine_Edge_Conditions : Boolean renames Refine_Opt.Value(Cond  );


   Trace_Refinements_Opt : aliased Options.Bool.Option_T (Default => False);
   --
   -- Whether to trace (display) each refinement of the arithmetic
   -- model of a flow-graph, due to partial evaluation over the
   -- propagated constant cells.
   --
   Trace_Refinements : Boolean renames Trace_Refinements_Opt.Value;


   --
   ---   Resolvable things
   --


   type Resolvable_T is (
      -- Data,
      Flow_Item,
      Stack);
   --
   -- The things that can be resolved (changed from dynamic to static)
   -- by constant propagation, using the knowledge of the constant
   -- values of cells flowing into the thing. (To be precise, the
   -- resolvablethings that can be disabled/enabled by options.)
   --
   -- Data
   --    Dynamic references to data cells (indexed load/store).
   --    Not implemented. The function is encapsulated in
   --    Arithmetic.Evaluation, not in Flow.Const itself; the
   --    need and benefit must be considered further before
   --    implementing this option.
   -- Flow
   --    Dynamic branches (boundable edges).
   -- Stack
   --    Local stack height.


   type Resolvables_T is array (Resolvable_T) of Boolean;
   --
   -- A selection of things to be resolved by constant propagation.


   package Resolvable_Valued is new Options.Discrete_Set_Valued (
      Item_Type              => Resolvable_T,
      Set_Type               => Resolvables_T,
      Value_Type_Description => "Set of Boolean",
      Item_Image             => Resolvable_T'Image);


   Resolve_Opt : aliased Resolvable_Valued.Option_T := (
      Default | Value => (others => True),
      Enumerable      => <>);
   --
   -- What to refine, using the constant-propagation results.
   --
   -- Resolve_Data  : Boolean renames Resolve_Opt.Value(Data     );
   Resolve_Flow  : Boolean renames Resolve_Opt.Value(Flow_Item);
   Resolve_Stack : Boolean renames Resolve_Opt.Value(Stack    );


   --
   ---   Other options
   --


   Max_Pointer_Iterations_Opt : aliased Options.Nat.Option_T (Default => 10);
   --
   -- An upper bound on the number of times constant propagation is iterated
   -- to make use of resolved or better-bounded dynamic data pointers
   -- from the preceding round. If Propagate = True, at least one
   -- round of constant propagation is used in any case, so the value
   -- zero is the same as the value one.
   --
   Max_Pointer_Iterations : Natural renames Max_Pointer_Iterations_Opt.Value;


   Deallocate : Boolean := True;
   --
   -- Whether to use Unchecked_Deallocation to release unused
   -- heap memory.


end Flow.Const.Opt;
