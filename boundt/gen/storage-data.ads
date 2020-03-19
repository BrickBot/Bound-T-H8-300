-- Storage.Data (decl)
--
-- Data-states for partial evaluation of target programs based on
-- cell-to-value maps.
--
-- Author: Niklas Holsti
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
-- $Date: 2015/10/24 19:36:52 $
--
-- $Log: storage-data.ads,v $
-- Revision 1.4  2015/10/24 19:36:52  niklas
-- Moved to free licence.
--
-- Revision 1.3  2009-11-27 11:28:08  niklas
-- BT-CH-0184: Bit-widths, Word_T, failed modular analysis.
--
-- Revision 1.2  2007/08/25 19:43:38  niklas
-- BT-CH-0075: Storage.Data corrected to store Step_Data_Ref.
--
-- Revision 1.1  2007/08/20 12:24:32  niklas
-- First version.
--


with Arithmetic;
with Arithmetic.Evaluation;
with Flow;
with Flow.Evaluation;
with Hash_G;                    -- LGL components.
with Interfaces;


package Storage.Data is


   --    Principles of operation
   --
   -- We provide the following:
   --
   -- > A type State_T to model a single data-state of the target
   --   processor as an assignment of values to a finite set of cells.
   --   The choice of cells to be modelled (cells for which a value is
   --   recorded in State_T) is defined by an overridable function.
   --
   -- > Operations to update data States by binding or unbinding
   --   data cells to values.
   --
   -- > A type Domain_T to use a State_T as the source of known data
   --   values for partial evalution in Arithmetic.Evaluation.
   --
   -- > A type Step_Data_T, derived from Flow.Evaluation.Data_Domain_T,
   --   for partial evaluation of flow-graphs with respect to a Space
   --   of States.
   --
   -- > A type Space_T to contain some number of Step_Data_T's keyed
   --   by State_T's so that each State_T is represented once only and
   --   coupled with the unique Step_Data_Ref for this State_T.
   --
   -- > Operations to store and retrieve Step_Data/State in a Space_T.
   --
   -- The Step_Data_Ref is our main goal and purpose.


   --
   ---   State_T
   --


   type State_T (Capacity : Natural) is private;
   --
   -- A data-state in the target processor, binding some data cells
   -- to known values and leaving all other data cells unbound.
   --
   -- Th data-state can be mutable; the bindings of cells can be
   -- changed and new bindings for new cells can be added, up to
   -- the defined Capacity of bound cells. However, when a State_T
   -- is stored in a Space then it should be held immutable, and any
   -- necessary changes (transformations) should be made in a copy.
   --
   -- Capacity
   --    The maximum number of cells that can be bound to known
   --    values in this data-state.
   --
   -- The default initial value of a State_T is the "universal" state
   -- that binds no cells to any values.
   --
   -- TBD if "tagged" is useful here.


   type State_Ref is access all State_T;
   --
   -- A reference to a State object on the heap (usually in a Space_T).


   function Image (Item : State_T) return String;
   --
   -- A description of the state, for human understanding.


   procedure Erase (State : in out State_T);
   --
   -- Erases all variable bindings in the given State.


   function Copy (
      State  : in State_T;
      Growth : in Natural)
   return State_T;
   --
   -- A copy of the given State, with a Capacity increased by Growth.
   -- This copy can be changed without any side-effect on the original
   -- State.


   procedure Bind (
      Cell  : in     Storage.Cell_T;
      Value : in     Arithmetic.Word_T;
      Under : in out State_T);
   --
   -- Updates the given state by binding the given Cell to the
   -- given Value, assuming that Under.Capacity is not exceeded.
   -- The width of the Cell defines the width of the Value.


   procedure Unbind (
      Cell  : in     Storage.Cell_T;
      Under : in out State_T);
   --
   -- Updates the given state by removing any value-binding for
   -- the given Cell.


   subtype Eval_Value_T is Arithmetic.Evaluation.Cell_Value_T;
   --
   -- Just an abbreviation. However, we will only use the Level
   -- values Known (meaning the cell is bound to a known value)
   -- and Variable (meaning the cell is not bound, unconstrained).
   -- The value Unknown is not used.


   Unbound : constant Eval_Value_T := (Level => Arithmetic.Evaluation.Variable);
   --
   -- Signifies an unknown value, a cell that is not bound to a
   -- known value in the data-state.


   function Value_Of (
      Cell  : Cell_T;
      Under : State_T)
   return Eval_Value_T;
   --
   -- The Known value to which the Cell is bound, Under the given state,
   -- or Unbound if the Cell is not bound in this state.


   --
   ---   Step_Data_T preview
   --


   type Step_Data_T;
   --
   -- See below...


   type Step_Data_Ref is access all Step_Data_T'Class;


   --
   ---   Space_T
   --


   type Space_T is limited private;
   --
   -- A container for Step_Data_T.
   -- The default initial value of a Space is undefined. You must
   -- use the Prepare procedure to initialize it.


   type Space_Ref is access all Space_T;
   --
   -- A reference to a heap-allocated Space_T.


   procedure Prepare (Space : in out Space_T);
   --
   -- Initializes the Space to an empty set of data States.
   -- Should be called once for each Space object, before any
   -- States are Stored in the Space.


   procedure Store (
      From  : in     Step_Data_Ref;
      State : in     State_T;
      Into  : in     Space_Ref;
      Ref   :    out Step_Data_Ref);
   --
   -- Enters the given State Into the given space and returns a
   -- reference to the Step_Data_T for this State. Of course, if
   -- the State is already in the space, a reference to the existing
   -- Step_Data_T is returned and no new heap block is allocated.
   --
   -- States in a Space should never be mutated by Erase, Bind, or
   -- Unbind. Thus, the resulting Ref should be considered a reference
   -- to an immutable (constant) state.
   --
   -- The From parameter is the Step_Data from which the new State
   -- originates in some way. Its main role is as a prototype so that
   -- the new object under Ref is of the same derived type as From.all.


   procedure Discard (Space : in out Space_T);
   --
   -- Discards the Space and all storage allocated by it.
   -- Does *not* TBC discard storage allocated for the Step_Data_T
   -- or State_T themselves. That is, any Step_Data_Ref values
   -- created by Store into this Space remain valid, only the
   -- collection/container is discarded.


   --
   ---   Domain_T, an evaluation domain based on a State.
   --


   type Domain_T is new Arithmetic.Evaluation.Domain_T
   with record
      State : State_Ref;
   end record;
   --
   -- Using a State to provide cell values for partial evaluation.


   -- overriding
   function Basis (Item : Domain_T)
   return Cell_List_T;


   -- overriding
   function Value_Of (Cell : Cell_T; Within : Domain_T)
   return Eval_Value_T;


   --
   ---   Step_Data_T
   --


   type Step_Data_T is new Flow.Evaluation.Data_Domain_T
   with record
      Space  : Space_Ref;
      State  : State_Ref;
   end record;
   --
   -- A data-state for partial evaluation of a flow-graph with respect
   -- to the data states stored in a given Space.
   --
   -- Space
   --    The space that holds all Step_Data_T for this instance of
   --    partial evaluation.
   -- State
   --    The cell-value bindings in this data-state.


   -- overriding
   function Domain (Data : access Step_Data_T)
   return Arithmetic.Evaluation.Domain_T'Class;


   -- overriding
   function Transformed_Data (
      From  : access Step_Data_T;
      After :        Arithmetic.Effect_T)
   return Flow.Step_Data_Ref;


   -- overriding
   function Constrained_Data (
      From : access Step_Data_T;
      By   :        Arithmetic.Condition_T)
   return Flow.Step_Data_Ref;


   -- overriding
   function Image (Item : Step_Data_T) return String;


   -- not overriding
   procedure Set_Initial_State (
      Values : in Cell_Value_List_T;
      Data   : in Step_Data_Ref);
   --
   -- Initializes the Data-context with a Space that contains
   -- the single State defined by the initial Values of some cells.
   -- The Data object must already be allocated; its Space and State
   -- should be null (default) and will be set here.


   -- not overriding
   function Number_Of_States (Data : Step_Data_T)
   return Natural;
   --
   -- The number of states stored in the Data space.


private


   ---   State_T


   type Hash_Value_T is new Interfaces.Unsigned_16;
   --
   -- A hash value computed from a State_T and used to find the
   -- State in a Space_T.


   Hash_Mod : constant := Hash_Value_T'Modulus;


   type State_T (Capacity : Natural) is record
      Tally  : Natural      := 0;
      Hash   : Hash_Value_T := 0;
      Values : Cell_Value_List_T (1 .. Capacity);
   end record;
   --
   -- A data-state in the target processor, binding some data cells
   -- to known values and leaving all other data cells unbound.
   -- In this crude implementation, the data state is held as a
   -- cell-value list. However, the list is ordered by ascending
   -- cell index.
   --
   -- Capacity
   --    The maximum number of cells that can be bound to known
   --    values in this data-state.
   -- Tally
   --    The current number of bound cells, 0 .. Capacity.
   -- Hash
   --    The hash value computed from the Values(1 .. Tally).
   --    All operations that modify a State_T will update Hash.
   -- Values
   --    The bound values are Values(1 .. Tally), in order of
   --    ascending cell index.


   --    Space_T


   function Hash_Of (Item : State_T) return Natural;
   --
   -- The hash-value of the Item, in Natural form as required by Hash_G.


   function Same_State (Left, Right : State_T) return Boolean;
   --
   -- Strict equality of the cell-value bindings in Left and Right.
   -- The two states must bind exactly the same set of cells to
   -- exactly the same value for each cell.


   package Hashed_States is new Hash_G (
      State_T,        -- Key_Type
      Step_Data_Ref,  -- Data_Type
      Hash_Of,        -- Hash
      Same_State);    -- "=" (Key_Type)
                      -- predefined "=" for Step_Data_Ref (Data_Type).


   type Space_T is new Hashed_States.Hash_Table_Type;


end Storage.Data;
