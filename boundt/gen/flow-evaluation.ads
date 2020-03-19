-- Flow.Evaluation (decl)
--
-- Using Arithmetic.Evaluation for partial evaluation of flow-graph elements
-- during construction of a flow-graph.
--
-- This package provides an abstract implementation (derived type)
-- of Step_Data_T that uses Arithmetic.Evaluation.Domain_T for the
-- partial evaluation of the arithmetic effects, edge preconditions
-- and other flow-graph elements. The implementation (representation)
-- of the data state itself is left open, to be defined in derived
-- types; only the interface to Arithmetic.Evaluation is defined here.
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
-- $Date: 2015/10/24 19:36:48 $
--
-- $Log: flow-evaluation.ads,v $
-- Revision 1.3  2015/10/24 19:36:48  niklas
-- Moved to free licence.
--
-- Revision 1.2  2007-07-21 18:18:42  niklas
-- BT-CH-0064. Support for AVR/IAR switch-handler analysis.
--
-- Revision 1.1  2006/05/06 06:59:21  niklas
-- BT-CH-0021.
--


with Arithmetic;
with Arithmetic.Evaluation;
with Calling;


package Flow.Evaluation is


   type Data_Domain_T is abstract new Step_Data_T with null record;
   --
   -- Models the data state in some way that permits the extraction
   -- of an Arithmetic.Evaluation.Domain_T from the model, thus
   -- letting us use the partial-evaluation services for that kind
   -- of bounds.
   --
   -- Recall the division of the Step_Data_T operations into "low-level"
   -- and "high-level" operations (see the parent package, Flow). Here
   -- we implement the high-level operations for Data_Domain_T using
   -- partial-evaluation functions from Arithmetic.Evaluation and the
   -- new primitive function Domain (see below) that extracts an
   -- Arithmetic.Evaluation.Domain_T from a Data_Domain_T.
   --
   -- The high-level Data_Domain_T operations use the low-level operations
   -- Transformed_Data and Constrained_Data. Thus these low-level
   -- operations are overriden as abstract for Data_Domain_T to force
   -- their implementation in any derived concrete type.
   --
   -- The high-level Data_Domain_T operations do not use the other two
   -- low-level Step_Data_T operations (Refined_Effect and Refined_Condition)
   -- because the corresponding computations are implemented with
   -- Arithmetic.Evaluation. Thus these low-level operations can be
   -- inherited from Step_Data_T (dummy operations).


   type Data_Domain_Ref is access all Data_Domain_T'Class;
   --
   -- A reference to a data-domain on the heap.


   -- not overriding
   function Domain (Data : access Data_Domain_T)
   return Arithmetic.Evaluation.Domain_T'Class
   is abstract;
   --
   -- An evaluation domain that defines a set of data that is a
   -- superset (a possible over-approximation) of the given Data
   -- state. The precision of the domain visavi the Data state
   -- depends on the actual Data_T.


   -- not overriding
   function Is_Bound (
      Cell  :        Storage.Cell_T;
      Under : access Data_Domain_T'Class)
   return Boolean;
   --
   -- Whether the Cell is bound to a known (constant) value Under
   -- the given data-domain.


   -- overriding
   function Transformed_Data (
      From  : access Data_Domain_T;
      After :        Arithmetic.Effect_T)
   return Step_Data_Ref
   is abstract;
   --
   -- This operation is used in Transform_New_Step and Refine_New_Edge
   -- as implemented here for Data_Domain_T. Thus derived types must
   -- implement this operation as defined in its original declaration
   -- for Step_Data_T (unless the derived type overrides Transform_New_Step
   -- and Refine_New_Edge, which would make this package useless)
   -- Therefore this operation is made abstract.


   -- overriding
   function Constrained_Data (
      From : access Data_Domain_T;
      By   :        Arithmetic.Condition_T)
   return Step_Data_Ref
   is abstract;
   --
   -- This operation is used in Transform_New_Step and Refine_New_Edge
   -- as implemented here for Data_Domain_T. Thus derived types must
   -- implement this operation as defined in its original declaration
   -- for Step_Data_T (unless the derived type overrides Transform_New_Step
   -- and Refine_New_Edge, which would make this package useless)
   -- Therefore this operation is made abstract.


   -- overriding
   procedure Transform_New_Step (
      Data   : access Data_Domain_T;
      Step   : in     Step_T;
      Effect : in out Arithmetic.Effect_Ref;
      Post   :    out Step_Data_Ref);


   -- overriding
   procedure Transform_New_Step (
      Data    : access Data_Domain_T;
      Step    : in     Step_T;
      Effect  : in     Arithmetic.Effect_T;
      Refined :    out Arithmetic.Effect_Ref;
      Post    :    out Step_Data_Ref);


   -- overriding
   procedure Refine_New_Edge (
      Post   : access Data_Domain_T;
      Source : in     Step_T;
      Target : in     Step_Tag_T;
      Cond   : in out Arithmetic.Condition_T;
      Giving :    out Step_Data_Ref);


   -- overriding
   procedure Apply (
      Pre   : access Data_Domain_T;
      Post  : access Data_Domain_T;
      Upon  : in out Boundable_Edge_T'Class;
      Graph : in     Graph_T);


   -- overriding
   procedure Apply (
      Data   : access Data_Domain_T;
      Upon   : in     Calling.Protocol_T'Class;
      Giving :    out Calling.Protocol_Ref);


end Flow.Evaluation;
