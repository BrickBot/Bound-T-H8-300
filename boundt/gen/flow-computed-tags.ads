-- Flow.Computed.Tags (decl)
--
-- Dynamic flow edges where the target address is defined by an
-- arithmetic expression and a function that maps a value of this
-- expression to the tag of the corresponding target step.
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
-- $Revision: 1.4 $
-- $Date: 2015/10/24 19:36:48 $
--
-- $Log: flow-computed-tags.ads,v $
-- Revision 1.4  2015/10/24 19:36:48  niklas
-- Moved to free licence.
--
-- Revision 1.3  2009-11-27 11:28:07  niklas
-- BT-CH-0184: Bit-widths, Word_T, failed modular analysis.
--
-- Revision 1.2  2006/06/03 12:16:11  niklas
-- Changed the default implementations of Edge_Cond and Edge_Time
-- to use Flow.Computed.Edge_Cond and Flow.Computed.Edge_Time,
-- redispatching on the Edge and omitting the Tag.
--
-- Revision 1.1  2006/06/03 11:49:12  niklas
-- First version, extracted from the parent, Flow.Computed, but
-- changing the definition of "new" targets to use the list of
-- earlier resolved values of the target expression instead of
-- the existing static successor tags of the source step. Thus,
-- the new type Edge_T corresponds to the old type Values_Edge_T.
--


package Flow.Computed.Tags is


   --
   ---   Edge_T
   --


   type Edge_T is abstract new Computed.Edge_T with null record;
   --
   -- A computed edge provided with functions to map the values
   -- of the bounded target expression to the target step-tags,
   -- edge preconditions and edge execution times.


   type Edge_Ref is access all Edge_T'Class;
   --
   -- A reference to a heap-allocated computed edge object.


   -- not overriding
   function Target_Tag (
      Edge   : Edge_T;
      Target : Arithmetic.Value_T)
   return Step_Tag_T
   is abstract;
   --
   -- The step-tag for the target of the static edge that should
   -- be created when Edge.Target = Target. If such a step-tag
   -- cannot exist (eg. for alignment reasons), the function shall
   -- propagate False_Path.
   --
   -- This function must be implemented in derived types because
   -- there is no generic way to construct a step tag from a
   -- target value.


   -- not overriding
   function Edge_Cond (
      Edge   : Edge_T;
      Target : Arithmetic.Value_T;
      Tag    : Step_Tag_T;
      Graph  : Graph_T)
   return Arithmetic.Condition_T;
   --
   -- The precondition to be assigned to the edge that will (or may)
   -- be added by Add_Target for the same parameters.
   --
   -- The default implementation returns Edge_Cond (Edge, Target, Graph)
   -- (that is, without the Tag parameter) with redispatching on Edge.


   -- not overriding
   function Edge_Time (
      Edge   : Edge_T;
      Target : Arithmetic.Value_T;
      Tag    : Step_Tag_T;
      Graph  : Graph_T)
   return Processor.Time_T;
   --
   -- The execution time to be assigned to the edge that will (or may)
   -- be added by Add_Target for the same parameters.
   --
   -- The default implementation returns Edge_Time (Edge, Target, Graph)
   -- (that is, without the Tag parameter) with redispatching on Edge.


   -- not overriding
   procedure Add_Target_Tag (
      Edge   : in out Edge_T;
      Target : in     Arithmetic.Value_T;
      Tag    : in     Step_Tag_T;
      Graph  : in     Graph_T);
   --
   -- Applies a new Target value and target Tag as a possible target
   -- computed from the Edge, possibly giving new static edges in the
   -- Graph (usually only one new edge, to the Tag).
   --
   -- This operation is usually called from the Apply operation for
   -- arithmetic bounds. The Target value is a new value allowed by
   -- Apply.Bounds for Edge.Target, Tag is the corresponding step-tag
   -- computed by Target_Tag. The Tag may or may not already be the
   -- tag of some succcessor step of Source (Edge).
   --
   -- The default implementation uses Add_Resolved_Edge with the
   -- precondition and time from Edge_Cond and Edge_Time, respectively
   -- (with redispatching on Edge) and no loose-edge information.


   -- overriding
   procedure Add_Target (
      Edge   : in out Edge_T;
      Target : in     Arithmetic.Value_T;
      Graph  : in     Graph_T);
   --
   -- Adds the new static edge resolved from the boundable Edge to
   -- the Graph, when some bounds have produced a new Target.
   --
   -- The default implementation uses Add_Target_Tag (above) with a
   -- Tag value computed by Target_Tag, in both cases redispatching
   -- on Edge.


end Flow.Computed.Tags;
