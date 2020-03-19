-- Programs.Execution (body)
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
-- $Revision: 1.56 $
-- $Date: 2015/10/24 20:05:51 $
--
-- $Log: programs-execution.adb,v $
-- Revision 1.56  2015/10/24 20:05:51  niklas
-- Moved to free licence.
--
-- Revision 1.55  2015/05/22 06:32:22  niklas
-- Corrected indentation.
--
-- Revision 1.54  2013/12/12 22:26:04  niklas
-- BT-CH-0262: Corrections to new value-origin analysis.
--
-- Revision 1.53  2013/12/08 22:05:57  niklas
-- BT-CH-0259: Storing value-origin analysis results in execution bounds.
--
-- Revision 1.52  2009-11-27 11:28:07  niklas
-- BT-CH-0184: Bit-widths, Word_T, failed modular analysis.
--
-- Revision 1.51  2009-10-07 19:26:10  niklas
-- BT-CH-0183: Cell-sets are a tagged-type class.
--
-- Revision 1.50  2009-04-16 16:29:23  niklas
-- BT-CH-0171: Stack pointers in call effects.
--
-- Revision 1.49  2009/04/09 19:24:34  niklas
-- Changed some Warnings to Faults, which they are.
--
-- Revision 1.48  2009/01/18 07:53:05  niklas
-- Removed unused context clauses and locals.
--
-- Revision 1.47  2008/11/09 21:43:05  niklas
-- BT-CH-0158: Output.Image (Time_T) replaces Programs.Execution.Image.
--
-- Revision 1.46  2008/09/24 08:38:53  niklas
-- BT-CH-0146: Assertions on "loop starts <bound> times".
-- BT-CH-0146: Loop-repeat assertions set both lower and upper bound.
-- BT-CH-0146: Report locations of contradictory "count" assertions.
-- BT-CH-0146: Contradictory "count" assertions imply infeasibility.
--
-- Revision 1.45  2008/07/28 19:23:45  niklas
-- BT-CH-0140: Detect contradictory execution-count bounds.
--
-- Revision 1.44  2008/07/23 09:07:16  niklas
-- BT-CH-0139: Fix recursion in Programs.Execution.Paths.
--
-- Revision 1.43  2008/07/20 06:31:38  niklas
-- BT-CH-0137: Cached Locus for subprograms and execution bounds.
--
-- Revision 1.42  2008/07/14 19:16:57  niklas
-- BT-CH-0135: Assertions on "instructions".
--
-- Revision 1.41  2008/02/27 19:25:19  niklas
-- BT-CH-0116: Call-specific time and stack assertions.
--
-- Revision 1.40  2008/02/23 13:34:04  niklas
-- BT-CH-0115: Wcet_Loop output and option -loop_time.
--
-- Revision 1.39  2008/02/18 13:24:10  niklas
-- BT-CH-0111: Processor-specific info in execution bounds.
--
-- Revision 1.38  2008/01/31 21:57:45  niklas
-- BT-CH-0108: Fixes to BT-CH-0098.
--
-- Revision 1.37  2007/12/21 13:31:08  niklas
-- Extended Input_Cells for better Locus in Fault message.
--
-- Revision 1.36  2007/12/17 13:54:40  niklas
-- BT-CH-0098: Assertions on stack usage and final stack height, etc.
--
-- Revision 1.35  2007/11/12 21:37:28  niklas
-- BT-CH-0097: Only arithmetic analysis marks boundable edge domain.
--
-- Revision 1.34  2007/08/27 16:10:14  niklas
-- Extended function Number_Of_Bounds (Bounds_Set) to return zero
-- for an uninitialized (null) bounds-set.
--
-- Revision 1.33  2007/08/17 14:44:00  niklas
-- BT-CH-0074: Stable and Unstable stacks.
--
-- Revision 1.32  2007/08/05 21:07:04  niklas
-- Extended Initialize_Bounds, when the Along context is not null, to
-- check that there are earlier bounds for the subprogram and that the
-- subprogram is the final callee in the context, and else emit Faults.
-- Extended Call_Bounds (Within Bounds_Ref) to check for null callee
-- bounds and emit a warning for such.
--
-- Revision 1.31  2007/08/02 11:23:18  niklas
-- Added the exception Undefined_Stub_Level to understand why
-- the Stub_Level function can be called with null Bounds.
--
-- Revision 1.30  2007/07/09 13:49:01  niklas
-- Added a variant of Add_Inputs_For_Unbounded_Calls that updates
-- a Cell_Set_T, instead of a Small_Cell_Set_T.
-- Extended Bound_Call_Input to trace the action, optionally.
--
-- Revision 1.29  2007/05/02 11:41:13  niklas
-- Added the helper procedure Add_Inputs_For to set the output locus
-- for the call being processed in Add_Inputs_For_Unbounded_Calls.
--
-- Revision 1.28  2007/03/18 12:50:39  niklas
-- BT-CH-0050.
--
-- Revision 1.27  2007/02/13 20:24:50  Niklas
-- BT-CH-0044.
--
-- Revision 1.26  2007/01/25 21:25:17  niklas
-- BT-CH-0043.
--
-- Revision 1.25  2007/01/21 19:31:47  niklas
-- BT-CH-0042.
--
-- Revision 1.24  2007/01/13 13:51:06  niklas
-- BT-CH-0041.
--
-- Revision 1.23  2006/12/05 18:48:37  niklas
-- BT-CH-0040.
--
-- Revision 1.22  2006/11/20 18:59:02  niklas
-- Added the function Bounds_For_Calls to give the universal bounds
-- for all calls from a given Subprogram, itself perhaps not bounded.
--
-- Revision 1.21  2006/11/01 21:27:26  niklas
-- BT-CH-0034.
--
-- Revision 1.20  2006/10/24 21:41:06  niklas
-- BT-CH-0030.
--
-- Revision 1.19  2006/08/22 12:51:23  niklas
-- Extended Set_Input_Cells and Set_Output_Cells to check for
-- No_Cell_Set and signal a fault in that case.
-- Updated function Basis to use Storage.Is_None instead of "=".
--
-- Revision 1.18  2006/05/27 21:39:52  niklas
-- BT-CH-0020.
--
-- Revision 1.17  2006/05/26 13:55:10  niklas
-- Added function Calling_Protocol, for RapiTime export.
--
-- Revision 1.16  2006/03/25 13:34:08  niklas
-- Added query functions Executed_Edges and Executed_Call_Bounds.
-- Added functions to compute the Total_Time of a set of nodes
-- or edges, or of all edges within some execution bounds (based
-- on the earlier private function Total_Time). Modified the
-- Total_Time functions for loops and calls accordingly.
--
-- Revision 1.15  2005/10/09 08:10:23  niklas
-- BT-CH-0013.
--
-- Revision 1.14  2005/09/20 09:57:46  niklas
-- Added function Flow_Graph (Bounds_Ref).
--
-- Revision 1.13  2005/08/24 10:29:51  niklas
-- Added stuff to support the Analysis Workbench, including
-- the functions Link (From Bounds Via Call), No_Links,
-- Counts_Set (Bounds), Count (Node), Count (Edge),
-- Start_Count (Loop), Head_Count (Loop), Neck_Count (Loop),
-- Repeat_Count (Loop), Call_Count (Call), Call_Count (Link),
-- Total_Time (Loop), Total_Time (Call) and Total_Time (Link).
-- Modified Node_Times_With_Call_Times to use the new function
-- Programs.Node (Call), making the local variable Graph useless
-- and removable.
--
-- Revision 1.12  2005/08/08 17:55:39  niklas
-- Added the function Bound_At to access execution bounds using the
-- program-wide unique index Bounds_Index_T. Added the component
-- Bounds_Set_Object_T.Indexed to support this function.
-- Added the functions Loop_Neck_Bound and Loop_Repeat_Bound to
-- query loop-bounds for a given loop (not all loops at once).
--
-- Revision 1.11  2005/06/29 19:35:30  niklas
-- Added Prune_Flow.
--
-- Revision 1.10  2005/06/29 12:54:34  niklas
-- Added the Locus function for loops.
--
-- Revision 1.9  2005/06/28 08:39:14  niklas
-- Extended Assert_Path_Specific_Time to provide the "stub" bounds
-- with a computation model (the "primitive" one). This avoids null
-- pointers in detailed output.
-- Changed function Node_Times to just Note when the times are
-- missing, instead of emitting a Fault. This helps detailed output.
--
-- Revision 1.8  2005/05/09 15:34:06  niklas
-- Added initial support for value-origin analysis, by adding the
-- operation Remove_From_Output.
--
-- Revision 1.7  2005/04/17 12:45:52  niklas
-- Extended Compute_Usage so that, for a callee with an undefined
-- stack-usage bound, it uses instead an unsafe lower bound computed
-- from the initial value of the local stack height for the callee.
-- Changed all Output.Unknown messages to Output.Error.
--
-- Revision 1.6  2005/02/23 09:05:20  niklas
-- BT-CH-0005.
--
-- Revision 1.5  2005/02/20 15:15:36  niklas
-- BT-CH-0004.
--
-- Revision 1.4  2005/02/16 21:11:47  niklas
-- BT-CH-0002.
--
-- Revision 1.3  2004/06/25 14:49:32  niklas
-- Improved Calls_With_Unbounded_Take_Off to work and return an
-- empty list even when Item.Take_Off_Limits is null (as happens
-- for a subprogram with an asserted time).
--
-- Revision 1.2  2004/05/01 09:51:07  niklas
-- First Tidorum version.
-- Taking Cell_T stuff from Storage, not Arithmetic.
-- Added Analysis_Level_T item Universally_Bounded.
-- Added description of analysis level: WCET assertion, constant
-- propagation, sublevels Paths_Bounded, Time_Bounded, Failed.
-- Added operations Assert_Universal_Time and Assert_Path_Specific_Time
-- to indicate that a time-bound is asserted and not computed.
-- Tolerate irreducible subprograms as one kind of unbounded paths.
-- Add concepts of links (Link_T) between execution bounds.
-- Add attributes for Program_T and Assertion_Map_T to Execution Bounds.
-- This reduces the number of parameters that must be passed around.
-- Add edge and node times as Execution Bounds attributes so that they
-- can depend on call path.
-- Major extensions to stack-usage analysis, including definition and
-- safety levels and the concept of take-off stack height. Separate
-- between analysis for time bounds and analysis for space bounds.
-- Warn of an attempt to set a negative stack limit and substitute a
-- zero limit.
-- Cache "boundedness" state for Execution Bounds to avoid repeated
-- traversal of a bounds graph.
-- Removed the function Lowest_WCET. It was used only in ERC32 floating
-- point blocking analysis and is no longer needed there.
-- Added optional Trace output to some operations.
-- Added a variant of the function Bounds_For to get the execution
-- bounds on a root call.
-- Corrected Initialize_Bounds to always create the Call_Bounds and
-- the new Take_Off_Limits even if there are no calls. Thus these
-- components will be valid accesses to null vectors, rather than
-- null accesses.
--
-- Revision 1.1  2003/03/11 08:28:28  holsti
-- First version split off from the parent package Programs.
--


with Arithmetic;
with Bags;   -- From MW Components.
with Flow.Calls;
with Flow.Calls.Opt;
with Flow.Pruning.Opt;
with Loops.Show;
with Output;
with Programs.Execution.Opt;
with Storage.List_Cell_Sets;
with Unbounded_Vectors;


package body Programs.Execution is


   use type Node_Times_Ref;
   use type Step_Edge_Times_Ref;


   --
   ---   More elements for execution bounds objects:
   --


   type Initial_Values_T is access Storage.Bounds.Cell_Interval_List_T;
   --
   -- Initial bounds on cell-values upon entry to a subprogram.


   type Looping_Bounds_T (Number : Loops.Loop_Count_T) is record
      Loop_Starts  : Loop_Bounds_T (1 .. Number);
      Loop_Neck    : Loop_Bounds_T (1 .. Number);
      Loop_Repeats : Loop_Bounds_T (1 .. Number);
   end record;
   --
   -- Bounds on looping, for each loop in the subprogram.
   --
   -- Loop_Starts
   --    Bounds on the number of times the loop-head is entered
   --    from outside the loop, for each execution of the subprogram
   --    that contains the loop.
   --
   -- Loop_Neck
   --    Bounds on the number of times the loop-body is entered
   --    from the loop-head, for each time the loop-head is entered
   --    from outside the loop (= number of times the loop is started).
   --
   -- Loop_Repeats
   --    Bounds on the number of execution of the repeat edges
   --    (back edges) that return to the loop-head from the loop-body,
   --    for each time the loop is started.
   --
   -- A given loop can have a neck bound or a repeat bound or
   -- both or neither; the stricter bound will constrain the
   -- worst-case path.


   type Take_Off_Limits_T is
       array (Call_Index_T range <>, Stack_Index_T range <>)
       of Stack_Limit_T;
   --
   -- Limits on local stack-height (take-off height) for all stacks and
   -- all calls in a subprogram.


   type Stack_Usages_T is array (Stack_Index_T range <>) of Stack_Usage_T;
   --
   -- A usage limit for each stack in the program.


   type Final_Stack_Heights_T is
      array (Stack_Index_T range <>) of Final_Stack_Height_T;
   --
   -- Bounds on the final (local) height for each stack in the program.


   --
   ---   Cell sets, as used here
   --


   package Cell_Sets renames Storage.List_Cell_Sets;
   --
   -- The implementation of cell sets that we use.


   subtype Cell_Set_T is Cell_Sets.Set_T;
   --
   -- For various sets of cells involved in execution bounds.


   --
   ---   Execution bounds objects:
   --


   type Bounds_Object_T (
      Level      : Bounding_Level_T;
      Max_Node   : Flow.Node_Index_T;
      Num_Calls  : Natural;
      Num_Stacks : Natural;
      Num_Loops  : Loops.Loop_Count_T)
   is record

      -- Identification:

      Caller_Bounds    : Bounds_Ref;
      Subprogram       : Subprogram_T;
      Call_Path        : Call_Path_T (1 .. Level);
      Program          : Program_T;
      Index            : Bounds_Index_T;
      Locus            : Output.Locus_T;

      -- Goals:

      For_Time         : Boolean;
      For_Space        : Boolean;

      -- Processor-specific information:

      Info             : Processor.Execution.Info_T;

      -- Computation model:

      Computation      : aliased Flow.Computation.Model_Ref;
      All_Cells        : Cell_Set_T;
      Value_Origins    : Flow.Origins.Map_Ref;

      -- Assertions:

      Assertion_Map    : Assertions.Assertion_Map_T;
      Enough_For_Time  : Boolean := False;

      -- Data flow results:

      Input_Cells      : Cell_Set_T;
      Input_Cell_Tally : Natural;
      Inputs_Defined   : Boolean;
      Initial          : Initial_Values_T;
      Call_Inputs      : Storage.Bounds.Bounds_List_T (1 .. Num_Calls);
      Basis            : Cell_Set_T;
      Output_Cells     : Cell_Set_T;
      Outputs_Defined  : Boolean;

      -- Bounds on callees:

      Call_Bounds      : Call_Bounds_List_T (1 .. Num_Calls);
      Stub_Level       : Calling.Stub_Level_T;

      -- Time per edge and node:

      Step_Edge_Times  : Step_Edge_Times_Ref;
      Node_Times       : Node_Times_Ref;

      -- Execution-flow bounds (asserted or computed):

      Node_Bounds      : Node_Bounds_T (1..Max_Node);
      Loop_Bounds      : Looping_Bounds_T (Num_Loops);
      Void_Flow_Bounds : Boolean;

      -- Execution path results:

      Flow_Counts      : Flow_Counts_Ref;

      -- Execution time results:

      Time_State       : Time_State_T;
      Time_Calculated  : Boolean;
      Time             : Processor.Time_T;

      -- Final stack height results:

      Final_Stack_Height : Final_Stack_Heights_T (1 .. Num_Stacks);

      -- Stack usage results:

      Stack_Height     : Stack_Limits_T (1 .. Num_Stacks);
      Max_Stack_Usage  : Stack_Usages_T (1 .. Num_Stacks);
      Take_Off_Limits  : Take_Off_Limits_T (1 .. Num_Calls, 1 .. Num_Stacks);

      -- Mutability and cached knowledge:

      Frozen           : Boolean := False;

   end record;
   --
   -- Execution bounds with attributes as follows:
   --
   -- Level
   --    The bounding level at which the bounds have been stored.
   --    See the function Level for further explanation.
   --
   -- Max_Node
   --    The number of nodes in the subprogram's flow-graph.
   --
   -- Num_Calls, Num_Stacks, Num_Loops
   --    The number of calls from the Subprogram, the number
   --    of Loops in the Subprogram, and the number of Stacks
   --    in the program.
   --
   -- Caller_Bounds
   --    A reference to the execution bounds on the caller of
   --    this Subprogram, when this Bounds_Object applies to a
   --    specific call in or longer context (call-path).
   --    Null for context-independent (universal) bounds.
   --
   -- Subprogram
   --    The subprogram to which these bounds apply.
   --
   -- Call_Path
   --    The call-path to which the execution bounds apply,
   --    through parameter values and assertions collected from
   --    the calls on the call-path.
   --
   -- Program
   --    The program under analysis, that contains the Subprogram.
   --
   -- Index
   --    A unique sequential number assigned to execution bounds
   --    as they are created, This identifying index is used to
   --    avoid repeated display or processing of bounds that are
   --    referred to from several higher-level bounds. For example,
   --    universal bounds for a subprogram that is called from
   --    several places.
   --
   -- Locus
   --    The output locus, computed from the Subprogram and the
   --    the Call_Path when the bounds are created, and not changed
   --    thereafter, even if the flow-graph of the Subprogram is
   --    extended (because these bounds cannot then apply to the
   --    extended flow-graph).
   --
   -- For_Time
   --    Whether we are seeking bounds on execution time.
   --
   -- For_Space
   --    Whether we are seeking bounds on (stack) memory space,
   --    and the Program under analysis has some stacks.
   --
   -- Computation
   --    The computation model that underlies these bounds. This
   --    model can be updated in-place, through a Model_Handle_T,
   --    wherefore it is aliased.
   --
   -- All_Cells
   --    All the cells statically named in the Computation model
   --    (including cells used or defined in the effect of steps,
   --    cells used in the preconditions of edges, basis cells for
   --    dynamic memory references, dynamic edges and dynamic calling
   --    protocols) and input cells for unbounded calls.
   --
   -- Value_Origins
   --    The results of a value-origin analysis of the Computation,
   --    showing for each cell, at each program point, the origin of
   --    the value(s) that the cell can hold at that point.
   --
   -- Assertion_Map
   --    The connection between user assertions (statements in the
   --    assertion file, structures in the assertion set) and parts
   --    of the Subprogram such as loops or calls.
   --
   -- Enough_For_Time
   --    Whether we shall assume (by assertion) that there are enough
   --    asserted execution-count bounds on parts of the Subprogram
   --    to bound the execution path, thus giving a bound on the
   --    worst-case execution time.
   --
   -- Input_Cells
   --    The subset of input cells for Subprogram that was used to
   --    derive the bounds. If the bounds are not complete, these
   --    cells are needed as inputs to complete the bounding.
   --    Valid only if Inputs_Defined is True.
   --
   -- Input_Cell_Tally
   --    The number of cells in Input_Cells.
   --    Valid only if Inputs_Defined is True.
   --
   -- Inputs_Defined
   --    Whether Input_Cells is a valid cell-set (otherwise,
   --    Input_Cells is an uninitialized variable and should not
   --    be accessed).
   --
   -- Initial
   --    The known initial bounds on cell values on entry.
   --
   -- Call_Inputs
   --    Bounds on the input cells for each call, from the analysis
   --    of the caller or from assertions.
   --
   -- Basis
   --    The cells used for arithmetic analysis. Undefined
   --    (Storage.No_Cell_Set) if arithmetic analysis was not
   --    used to derive these bounds.
   --
   -- Output_Cells
   --    The output cells from the Subprogram's execution under these
   --    bounds. A cell is an output cell if it is the target of a
   --    feasible assignment in the Subprogram's computation model and
   --    is not detected nor asserted to be invariant across the
   --    Subprogram's execution in spite of the assignment.
   --
   -- Call_Bounds
   --    Execution bounds for calls to lower-level subprograms.
   --    For execution bounds on calls that have been created for
   --    the present context (that is, in the context of this
   --    Bounds_Object) the Caller_Bounds of the Call_Bounds refers
   --    to this Bounds_Object. Otherwise, the Call_Bounds were
   --    inherited from a less specific (shallower) context.
   --
   -- Stub_Level
   --    The stub level of these execution bounds. This component
   --    is valid only for Frozen bounds; otherwise the stub level
   --    should be computed from the feasible Call_Bounds.
   --
   -- Step_Edge_Times
   --    The worst-case execution time of each edge in the flow-graph.
   --    This is the single reference to this heap object.
   --
   -- Node_Times
   --    The worst-case execution time of each node in the flow-graph.
   --    This is the single reference to this heap object.
   --
   -- Node_Bounds
   --    Bounds on the flow of execution, expressed as bounds
   --    on the number of times each flow-graph node can be
   --    executed.
   --    Similar bounds on the execution of the flow-graph edges
   --    are not implemented in general, only through the loop-
   --    bounds.
   --
   -- Loop_Bounds
   --    Repetition bounds for the loops in the subprogram.
   --
   -- Void_Flow_Bounds
   --    Whether the Loop_Bounds or Node_Bounds give a void (null,
   --    infeasible, contradictory) set of execution-counts for
   --    some node or edge. If so, there is no feasible execution
   --    path through the subprogram, under these execution bounds.
   --
   -- Flow_Counts
   --    The worst-case execution path(s), represented by the
   --    execution counts of the nodes and edges.
   --
   -- Time_State
   --    The state of knowledge about Time, our upper bound on
   --    the worst-case execution time.
   --
   -- Time_Calculated
   --    Whether the last phase of the execution-time analysis, that
   --    of finding the worst-case path and calculating the worst-case
   --    execution time, has been applied to these execution bounds
   --    (and, by implication, also to all callee bounds linked to
   --    these caller bounds).
   --
   -- Time
   --    An upper bound on the WCET of the subprogram, as
   --    qualified by Time_State.
   --
   -- Final_Stack_Height
   --    Bounds on the final (local) stack height on return from
   --    this subprogram. In other words, bounds on the change in
   --    local stack height over one execution of this subprogram,
   --    from the entry point to any return point, including the
   --    effect, if any, of the return instruction itself.
   --    For a Stable stack the bounds are set to [0,0] when the
   --    stack is created and are never changed thereafter.
   --    For an Unstable stack the bounds are set to the Net_Change
   --    specified when the stack is created and may then be changed
   --    with procedure Bound_Final_Stack_Height.
   --
   -- Stack_Height
   --    Bounds on the stack height local to this subprogram.
   --    The stack-usage of lower-level subprogram is excluded.
   --
   -- Max_Stack_Usage
   --    Total stack usage of this subprogram and lower-level
   --    subprograms called here.
   --
   -- Take_Off_Limits
   --    Limits on the take-off stack-height for all calls from
   --    this subprogram to lower-level subprograms. Some of the
   --    limits may be unknown. The take-off stack-height is the
   --    caller's local stack-height immediately before control
   --    is transferred to the callee.
   --
   -- Frozen
   --    Whether the bounds are frozen (fixing some attributes)
   --    a result of being stored in a bounds-set.


   package Bounds_Vectors is new Unbounded_Vectors (
      Element_Type => Bounds_Ref,
      Vector_Type  => Bounds_List_T,
      Deallocate   => Opt.Deallocate);
   --
   -- For storing the execution bounds for one subprogram.
   -- A linear storage is used, since the number of bounds per
   -- subprogram is not expected to be very large, and the look-up
   -- time thus insignificant.
   -- The key field is the Call_Path.


   type Bounds_Vector_T is new Bounds_Vectors.Unbounded_Vector;
   --
   -- A list of bounds, usually the bounds for one subprogram.


   type Bounds_Store_T is record
      Subprogram : Subprogram_T;
      Bounds     : Bounds_Vector_T;
   end record;
   --
   -- Holds all the bounds found for a given subprogram.


   type Bounds_Store_Ref is access Bounds_Store_T;
   --
   -- An access type makes it easier to update a Bounds_Store that
   -- is held within a Bounds_Set.


   function Entry_Address (Item : Bounds_Store_Ref)
   return Processor.Code_Address_T
   --
   -- The entry address of the subprogram to which the
   -- given bounds apply.
   --
   is
   begin

      return Entry_Address (Item.Subprogram);

   end Entry_Address;


   package Bounds_Bags is new Bags (
      Key_Type  => Processor.Code_Address_T,
      Item_Type => Bounds_Store_Ref,
      Key_Of    => Entry_Address,
      "<"       => Processor."<",  -- for Code_Address_T
      "="       => Processor."=",  -- for Code_Address_T
      Count     => Natural);
   --
   -- For storing bounds in a set of bounds.


   subtype Bounds_Bag_T is
      Bounds_Bags.Bag (Duplicate_Keys_Allowed => False);
   --
   -- A set of execution bounds, grouped by the subprogram to which
   -- they apply (keyed on Entry_Address).


   type Bounds_Set_Object_T is record
      Program    : Program_T;
      For_Time   : Boolean;
      For_Space  : Boolean;
      Bounds     : Bounds_Bag_T;
      Indexed    : Bounds_Vector_T;
      Next_Index : Bounds_Index_T := Bounds_Index_T'First;
      Num_Links  : Natural := 0;
   end record;
   --
   -- A set of execution bounds for some subprograms in a target
   -- program.
   --
   -- Program
   --    The target program that was analyzed to get these bounds.
   --
   -- For_Time, For_Space
   --    Whether we seek bounds on execution time, (stack) memory
   --    space or both. For_Space is False if the Program under
   --    analysis has no stacks.
   --
   -- Bounds
   --    Execution bounds grouped by the subprogram and call-path
   --    to which they apply.
   --
   -- Indexed
   --    Execution bounds accessible by Bounds_Index_T.
   --
   -- Next_Index
   --    A counter for generating unique index values.
   --
   -- Num_Links
   --    The total number of links between bounds, represented by
   --    all the Call_Bounds components of the Bounds.


   function Bounds_Store_For (
      Sub    : Subprogram_T;
      Within : Bounds_Set_T)
   return Bounds_Store_Ref
   --
   -- The store (container) for the execution bounds derived for the
   -- given subprogram within the given bounds set.
   -- If the bound-set has no entry for this subprogram, one is created.
   --
   is

      Store : Bounds_Store_Ref;
      -- The bounds-store for the subprogram, or null.

   begin

       -- Find the bounds for this subprogram, if any:

       begin

          Store :=
             Bounds_Bags.Search (
                Key    => Entry_Address (Sub),
                Within => Within.Bounds);

       exception

       when Bounds_Bags.Nonexistent_Key =>

          Store := new Bounds_Store_T;

          Store.Subprogram := Sub;

          -- The list of bounds is null by default.

          Bounds_Bags.Insert (
             Item => Store,
             Into => Within.Bounds);

       end;

       -- Return the store:

       return Store;

   end Bounds_Store_For;


   function Bounds_For (
      Sub    : Subprogram_T;
      Within : Bounds_Set_T)
   return Bounds_List_T
   --
   -- All the execution bounds derived for the given subprogram
   -- within the given bounds set.
   -- If bound-set has no entry for this subprogram, one is created.
   --
   is
   begin

       return To_Vector (Bounds_Store_For (Sub, Within).Bounds);

   end Bounds_For;


   --
   ---   Stub levels
   --


   Undefined_Stub_Level : exception;
   --
   -- Temporarily defined to understand why the Stub_Level function (below)
   -- may be called with a null Bounds parameter.


   function Stub_Level (Bounds : Bounds_Ref) return Calling.Stub_Level_T
   is
      use type Flow.Edge_Resolution_T;
   begin

      if not Defined (Bounds) then

         Output.Fault (
            Location => "Programs.Execution.Stub_Level (Bounds)",
            Text     => "Null bounds");

         raise Undefined_Stub_Level;

      end if;

      if Bounds.Frozen then
         -- We already know it.

         return Bounds.Stub_Level;

      elsif Stub (Bounds.Subprogram) then
         -- I yam what I yam, and that's all there is.

         return 0;

      elsif Dynamic_Flow (Bounds) /= Flow.Stable then
         -- There may be calls to stubs.

         return 1;

      else
         -- I'm not a stub, but some of my best callees may be.

         return Stub_Level (Call_Bounds (Bounds));
         --
         -- Note that only feasible calls are included here.

      end if;

   end Stub_Level;


   function Unknown_Effect (Bounds : Bounds_Ref) return Boolean
   is
   begin

      return Stub_Level (Bounds) < Calling.Calls_No_Stub;

   exception

   when Undefined_Stub_Level =>

      Output.Fault (
         Location => "Programs.Execution.Unknown_Effect",
         Text     => "Null Bounds");

      raise;

   end Unknown_Effect;


   --
   ---   Call bounds
   --


   function Stub_Level (Call : Call_Bounds_T) return Calling.Stub_Level_T
   is

      Callee_Level : constant Calling.Stub_Level_T := Stub_Level (Call.Bounds);

   begin

      if Callee_Level = Calling.Calls_No_Stub then
         -- No stubs called there -- no stubs called here.

         return Calling.Calls_No_Stub;

      elsif Callee_Level + 1 /= Calling.Calls_No_Stub then
         -- A stub call is further down -- but not infinitely deep.

         return Callee_Level + 1;

      else
         -- A really deep call path.

         Output.Fault (
            Location => "Programs.Execution.Stub_Level (Call_Bounds)",
            Locus    => Locus (Call.Call),
            Text     => "Level overflow.");

         return Callee_Level;

      end if;

   exception

   when Undefined_Stub_Level =>

      Output.Fault (
         Location => "Programs.Execution.Stub_Level (Call)",
         Locus    => Locus (Call.Call),
         Text     => "Null Call.Bounds");

      raise;

   end Stub_Level;


   function Calls_Of (List : Call_Bounds_List_T) return Call_List_T
   is

      Calls : Programs.Call_List_T (List'Range);
      -- The result.

   begin

      for L in List'Range loop

         Calls(L) := List(L).Call;

      end loop;

      return Calls;

   end Calls_Of;


   function Bounds_Of (List : Call_Bounds_List_T)
   return Bounds_List_T
   is

      Bounds : Bounds_List_T (List'Range);
      -- The result.

   begin

      for L in List'Range loop

         Bounds(L) := List(L).Bounds;

      end loop;

      return Bounds;

   end Bounds_Of;


   function Stub_Level (List : Call_Bounds_List_T)
   return Calling.Stub_Level_T
   is

      Min_Level : Calling.Stub_Level_T := Calling.Calls_No_Stub;
      -- The minimum level of the List.

   begin

      for L in List'Range loop

         begin

            Min_Level := Calling.Stub_Level_T'Min (
               Min_Level,
               Stub_Level (List(L)));

         exception

         when Undefined_Stub_Level =>

            Output.Fault (
               Location => "Programs.Execution.Stub_Level (Call_Bounds_List)",
               Locus    => Locus (List(L).Call),
               Text     => "Null bounds");

         end;

      end loop;

      return Min_Level;

   end Stub_Level;



   --
   ---   Links between bounds
   --


   function Link (
      From : Bounds_Ref;
      Via  : Call_T)
   return Link_T
   is
   begin

      return (
         Call  => Via,
         Caller => From,
         Callee => Call_Bounds (On => Via, Within => From));

   end Link;


   function Caller_Bounds (Item : Link_T) return Bounds_Ref
   is
   begin

      return Item.Caller;

   end Caller_Bounds;


   function Callee_Bounds (Item : Link_T) return Bounds_Ref
   is
   begin

      return Item.Callee;

   end Callee_Bounds;


   function Call (Item : Link_T) return Call_T
   is
   begin

      return Item.Call;

   end Call;


   function No_Links return Link_List_T
   is
   begin

      return (1 .. 0 => (No_Call, No_Bounds, No_Bounds));

   end No_Links;


   function Locus (Item : Bounds_Ref)
   return Output.Locus_T
   is
   begin

      return Item.Locus;

   end Locus;


   function Locus (Luup : Loops.Loop_T; Within : Bounds_Ref)
   return Output.Locus_T
   is
   begin

      return Loops.Show.Locus (
         Luup   => Luup,
         Within => Flow_Graph (Subprogram (Within)),
         Source => Symbol_Table (Within));

   end Locus;


   function Locus (Call : Call_T; Within : Bounds_Ref)
   return Output.Locus_T
   is
      use type Output.Locus_T;

      Full_Path : constant Call_Path_T := Call_Path (Within) & Call;
      -- The full call path, including the Call.

   begin

      if Subprogram (Within) /= Caller (Call) then

         Output.Fault (
            Location => "Programs.Execution.Locus (Call, Bounds)",
            Text     =>
                 "Bounds for "
               & Name (Subprogram (Within))
               & ", call from "
               & Name (Caller (Call)));

      end if;

      return Locus (Within) & Locus (Full_Path);

   end Locus;


   --
   ---   Constructing and querying execution bounds sets
   --


   procedure Initialize_Bounds_Set (
      Program   : in     Program_T;
      For_Time  : in     Boolean;
      For_Space : in     Boolean;
      Set       : in out Bounds_Set_T)
   is
   begin

      -- TBA deallocation of old value.

      Set := new Bounds_Set_Object_T;

      Set.Program := Program;

      Set.For_Time  := For_Time;
      Set.For_Space := For_Space
                   and Number_Of_Stacks (Program) > 0;

   end Initialize_Bounds_Set;


   function Program (Item : Bounds_Set_T) return Program_T
   is
   begin

      return Item.Program;

   end Program;


   function Number_Of_Bounds (Within : Bounds_Set_T)
   return Bounds_Count_T
   is
   begin

      if Within = null then
         -- The bounds-set is not initialized.

         return 0;

      else

         return Within.Next_Index - 1;

      end if;

   end Number_Of_Bounds;


   function Number_Of_Bounds (
      Sub    : Subprogram_T;
      Within : Bounds_Set_T)
   return Natural
   is
   begin

      return Last (Bounds_Store_For (Sub, Within).Bounds);

   end Number_Of_Bounds;


   function Bound_At (
      Index  : Bounds_Index_T;
      Within : Bounds_Set_T)
   return Bounds_Ref
   is
   begin

      return Element (Vector => Within.Indexed, Index => Positive (Index));

   end Bound_At;


   procedure Check_Usage_Vs_Height (
      Stack  : in String;
      Usage  : in Stack_Usage_T;
      Height : in Stack_Limit_T)
   --
   -- Checks that the upper bound on the local stack Height is
   -- no larger than the upper bound on the total stack Usage.
   --
   is
      use Storage.Bounds;
      use type Arithmetic.Value_T;
   begin

      if (Bounded (Usage) and Known (Height.Max))
      and then
         Usage.Height < Value (Height.Max)
      then

         Output.Error (
              "Local stack height "
            & Arithmetic.Image (Value (Height.Max))
            & " exceeds total stack usage "
            & Arithmetic.Image (Usage.Height)
            & " for "
            & Stack);

      end if;

   end Check_Usage_Vs_Height;


   procedure Store_Bounds (
      Bounds     : in Bounds_Ref;
      Within     : in Bounds_Set_T)
   is

      Subprogram : constant Subprogram_T := Execution.Subprogram (Bounds);
      -- The subprogram to which the Bounds apply.

      Store : constant Bounds_Store_Ref :=
         Bounds_Store_For (Sub => Subprogram, Within => Within);
      -- The bounds-store for this subprogram.

      Autograph : constant String := "Programs.Execution.Store_Bounds";
      -- For Faults.

      Callee_Bounds : Bounds_Ref;
      -- Bounds on a call.

   begin

      -- Verify that the goals are compatible:

      if Within.For_Time and not Bounds.For_Time then

         Output.Fault (
            Location => Autograph,
            Text     => "New bounds are not for time.");

      end if;

      if Within.For_Space and not Bounds.For_Space then

         Output.Fault (
            Location => Autograph,
            Text     => "New bounds are not for space.");

      end if;

      -- Put the Bounds in the bag:

      Append (
         To    => Store.Bounds,
         Value => Bounds);

      -- Update the number of links to include the calls in Bounds:

      Within.Num_Links := Within.Num_Links + Bounds.Call_Bounds'Length;

      -- Put the Bounds in the Indexed vector, padding unused indices
      -- with nulls:

      for I in Last (Within.Indexed) + 1 .. Positive (Bounds.Index) - 1 loop

         Set (
            Vector => Within.Indexed,
            Index  => I,
            To     => No_Bounds);

      end loop;

      Set (
         Vector => Within.Indexed,
         Index  => Positive (Bounds.Index),
         To     => Bounds);

      -- Check the state of bounding:

      if Bounds.For_Time then

         case Bounds.Time_State is

         when Undefined =>

            Output.Fault (
               Location => Autograph,
               Text     =>
                    "Freezing bounds #"
                  & Bounds_Index_T'Image (Bounds.Index)
                  & " with time-state "
                  & Time_State_T'Image (Bounds.Time_State));

         when others =>

            null;

         end case;

      end if;

      -- TBA Bounds.For_Space.

      for S in 1 .. Bounds.Num_Stacks loop

         Check_Usage_Vs_Height (
            Stack  => Stack_Name (S, Program (Bounds)),
            Usage  => Bounds.Max_Stack_Usage(S),
            Height => Bounds.Stack_Height(S));

      end loop;

      -- Check that linked bounds on calls are stored:

      for C in Bounds.Call_Bounds'Range loop

         Callee_Bounds := Bounds.Call_Bounds(C).Bounds;

         if not Callee_Bounds.Frozen then

            Output.Fault (
               Location => Autograph,
               Text     =>
                    "Freezing bounds #"
                  & Bounds_Index_T'Image (Bounds.Index)
                  & " with unfrozen call-bounds #"
                  & Bounds_Index_T'Image (Callee_Bounds.Index));

         end if;

      end loop;

      -- Freeze the Bounds:

      if Bounds.Frozen then

         Output.Fault (
            Location => Autograph,
            Text     =>
                 "Bounds #"
               & Bounds_Index_T'Image (Bounds.Index)
               & " are already Frozen.");

      end if;

      -- Record the final Stub Level:

      Bounds.Stub_Level := Stub_Level (Bounds);

      Output.Note (
         Locus => Locus (Bounds),
         Text  =>
              "Freezing bounds #"
            & Bounds_Index_T'Image (Bounds.Index));

      Bounds.Frozen := True;

   end Store_Bounds;


   function Bound_At (
      Index  : Positive;
      Sub    : Subprogram_T;
      Within : Bounds_Set_T)
   return Bounds_Ref
   is
   begin

      return Element (Bounds_Store_For (Sub, Within).Bounds, Index);

   end Bound_At;


   function Bounds_For (
      Root   : Call_T;
      Within : Bounds_Set_T)
   return Bounds_Ref
   is
   begin

      return Bounds_For (
         Subprogram => Callee (Root),
         Within     => Within,
         Along      => (1 => Root));

   end Bounds_For;


   function Bounds_For (
      Subprogram : Subprogram_T;
      Within     : Bounds_Set_T;
      Along      : Call_Path_T)
   return Bounds_Ref
   is

      Bounds : constant Bounds_List_T :=
         Bounds_For (
            Sub    => Subprogram,
            Within => Within);
      -- All the bounds known for the subprogram.

      Best : Bounds_Ref := No_Bounds;
      -- The best (most specific) bounds found so far.
      -- Null if no applicable bounds found.

      Best_Full : Boolean := False;
      -- Whether the Best bounds are fully bounded.

      Bounds_Full : Boolean;
      -- Whether the bounds under consideration are fully bounded.

   begin

      for B in Bounds'Range loop

         if Bounds(B).Call_Path <= Along then
            -- These bounds are applicable.

            Bounds_Full := Bounded (Bounds(B));

            if Best = No_Bounds
               -- First applicable bounds found, best so far.

            or else (Bounds_Full and not Best_Full)
               -- Full bounds are better.

            or else (    Bounds_Full     = Best_Full
                     and Bounds(B).Level > Best.Level)
               -- More specific bounds are better.
            then

               Best      := Bounds(B);
               Best_Full := Bounds_Full;

            end if;

         end if;

      end loop;

      return Best;

   end Bounds_For;


   function Bounds_For_Calls (
      From   : Subprogram_T;
      Within : Bounds_Set_T)
   return Call_Bounds_List_T
   is

      Calls : constant Call_List_T := Programs.Calls_From (From);
      -- All the calls from the subprogram.

      Call : Call_T;
      -- One of the Calls.

      Bounds : Bounds_Ref;
      -- Some bounds for the Call.

      Call_Bounds : Call_Bounds_List_T (Calls'Range);
      -- The result to be.

   begin

      for C in Calls'Range loop

         Call := Calls(C);

         Bounds := Bounds_For (
            Subprogram => Callee (Call),
            Within     => Within,
            Along      => (1 => Call));

	 if Bounds = No_Bounds then

            Output.Fault (
               Location => "Programs.Execution.Bounds_For_Calls",
               Locus    => Locus (Call),
               Text     => "No execution bounds known.");

	 end if;

         Call_Bounds(C) := (Call => Call, Bounds => Bounds);

      end loop;

      return Call_Bounds;

   end Bounds_For_Calls;


   function Root_Bounds (Within : Bounds_Set_T)
   return Call_Bounds_List_T
   is

      Roots : constant Call_List_T := Root_Calls (Within.Program);
      -- The root calls.

      Result : Call_Bounds_List_T (Roots'Range);
      -- The result: root calls with universal bounds.

   begin

      for R in Roots'Range loop

         Result(R) := (
            Call   => Roots(R),
            Bounds => Bounds_For (Roots(R), Within));

      end loop;

      return Result;

   end Root_Bounds;


   function Number_Of_Links (Within : Bounds_Set_T)
   return Natural
   is
   begin

      return Within.Num_Links;

   end Number_Of_Links;


   function All_Links (
      From   : Call_List_T;
      Within : Bounds_Set_T)
   return Link_List_T
   is

      Links : Link_List_T (1 .. Within.Num_Links);
      Last  : Natural := 0;
      -- The result will be Links(1 .. Last).

      Done : Bounds_Subset_T (Max_Size => Number_Of_Bounds (Within));
      -- The bounds that have been scanned for links.


      procedure Add_Links (From : in Bounds_Ref)
      --
      -- Adds all links represented by From.Call_Bounds to the
      -- result, and proceeds recursively to lower calls.
      -- No operation if From was already Done.
      --
      is

         CB : Call_Bounds_T;
         -- One of From's call-bounds.

      begin

         if  (not Is_Member (From, Done))
         and From.Call_Bounds'Length > 0
         then
            -- The From bounds need processing.

            Add (Item => From, To => Done);

            for C in From.Call_Bounds'Range loop

               CB := From.Call_Bounds(C);

               Last := Last + 1;

               Links(Last) := (
                  Call   => CB.Call,
                  Caller => From,
                  Callee => CB.Bounds);

               Add_Links (From => CB.Bounds);

            end loop;

         end if;

      end Add_Links;


   begin  -- All_Links

      for F in From'Range loop

         Add_Links (
            From =>
               Bounds_For (
                  Root   => From(F),
                  Within => Within));

      end loop;

      return Links(1 .. Last);

   end All_Links;


   --
   ---   Constructing execution bounds
   --


   function To_Stack_Limit (Item : Storage.Bounds.Interval_T)
   return Stack_Limit_T
   is
   begin

      return Stack_Limit_T (Item);

   end To_Stack_Limit;


   Unlimited : constant Stack_Limit_T :=
      To_Stack_Limit (Storage.Bounds.Universal_Interval);
   --
   -- Default value for all local stack-height limits.


   procedure Create_Blank_Bounds (
      Subprogram : in     Subprogram_T;
      Call_Path  : in     Call_Path_T;
      Caller     : in     Bounds_Ref;
      For_Time   : in     Boolean;
      For_Space  : in     Boolean;
      Within     : in     Bounds_Set_T;
      Bounds     :    out Bounds_Ref)
   --
   -- Creates a blank set of execution bounds for this subprogram
   -- and call-path. The bounds are not yet attached to the subprogram.
   -- However, the bounds-index counter is incremented.
   --
   is
      use type Output.Locus_T;

      Stacks : constant Stacks_T := Programs.Stacks (Program (Within));
      -- All the stacks in the program.

   begin

      Bounds := new Bounds_Object_T (
         Level      => Call_Path'Length,
         Max_Node   => Flow.Max_Node (Flow_Graph (Subprogram)),
         Num_Calls  => Number_Of_Calls_From (Subprogram),
         Num_Loops  => Number_Of_Loops (Subprogram),
         Num_Stacks => Stacks'Length);

      Bounds.Caller_Bounds   := Caller;
      Bounds.Subprogram      := Subprogram;
      Bounds.Call_Path       := Call_Path;
      Bounds.Program         := Program (Within);
      Bounds.Index           := Within.Next_Index;

      if Call_Path'Length = 0 then

         Bounds.Locus := Locus (Subprogram);

      else

         Bounds.Locus :=
              Output.Locus (Call_Path => Image (Call_Path))
            & Locus (Subprogram);

      end if;

      Bounds.For_Time         := For_Time;
      Bounds.For_Space        := For_Space and Bounds.Num_Stacks > 0;

      -- Bounds.Computation is initialized by default to "none".

      Bounds.All_Cells        := Cell_Sets.Empty;

      Bounds.Assertion_Map    := Assertions.No_Map;

      -- Bounds.Enough_For_Time is False by default.

      Bounds.Input_Cells      := Cell_Sets.Empty;
      Bounds.Input_Cell_Tally := 0;
      Bounds.Inputs_Defined   := False;

      -- Bounds.Initial is null by default.

      Bounds.Call_Inputs      := (others => Storage.Bounds.No_Bounds);

      Bounds.Basis            := Cell_Sets.Empty;

      Bounds.Output_Cells     := Cell_Sets.Empty;
      Bounds.Outputs_Defined  := False;

      for C in 1 .. Bounds.Num_Calls loop

         Bounds.Call_Bounds(C) := (
            Call   => Call (From => Subprogram, Index => C),
            Bounds => null);

      end loop;

      Bounds.Stub_Level       := Calling.Calls_No_Stub;
      -- Dummy value, not valid until bounds are frozen.

      Bounds.Step_Edge_Times  := null;
      Bounds.Node_Times       := null;

      Bounds.Node_Bounds      := (
         others => (Min => 0,
                    Max => Flow.Execution.Infinite));

      Bounds.Loop_Bounds.Loop_Starts  := (others => Flow.Execution.Unbounded);
      Bounds.Loop_Bounds.Loop_Neck    := (others => Flow.Execution.Unbounded);
      Bounds.Loop_Bounds.Loop_Repeats := (others => Flow.Execution.Unbounded);

      Bounds.Void_Flow_Bounds := False;

      Bounds.Flow_Counts      := null;

      Bounds.Time_State       := Undefined;
      Bounds.Time_Calculated  := False;
      Bounds.Time             := 0;

      for S in Stacks'Range loop

         Bounds.Final_Stack_Height(S) := To_Stack_Limit (
            Storage.Bounds.One_Or_All (Net_Change (Stacks(S))));

         Bounds.Stack_Height(S) := Unlimited;

      end loop;

      Bounds.Max_Stack_Usage  := (others => (
         State  => Undefined,
         Height => 0,
         Call   => No_Call));

      Bounds.Take_Off_Limits := (others => (others => Unlimited));

      -- Bounds.Frozen is False by default.

      Within.Next_Index := Within.Next_Index + 1;

   end Create_Blank_Bounds;


   function Call_Invariants (
      Call   : Call_T;
      Within : Bounds_Ref)
   return Storage.Cell_List_T
   --
   -- The cells asserted as invariant for this Call, Within the
   -- given execution bounds for the caller, or a null list if
   -- no assertion map is known Within the caller's bounds.
   --
   is
      use type Assertions.Assertion_Map_T;
   begin

      if Within.Assertion_Map = Assertions.No_Map then

         return Storage.Null_Cell_List;

      else

         return Assertions.Call_Invariants (Call, Within.Assertion_Map);

      end if;

   end Call_Invariants;


   procedure Compute_Effect (
      Call_Bounds  : in Call_Bounds_T;
      Within       : in Bounds_Ref;
      Caller_Cells : in Storage.Cell_List_T)
   --
   -- Computes the effect of the Call on the computation Within the
   -- caller's bounds. A Call with (partially) unknown effect (due to
   -- stub callees) may change some of the Caller_Cells. We assume
   -- that Caller_Cells already contains all the caller cells that may
   -- be assigned by the Call, also the known outputs from the Call.
   --
   is
      use type Storage.Cell_List_T;

      Call : constant Call_T := Call_Bounds.Call;
      -- For brevity.

      Model : Flow.Computation.Model_Ref renames Within.Computation;
      -- For brevity.

      Protocol : constant Calling.Protocol_Ref :=
         Flow.Computation.Calling_Protocol (Call, Model);
      -- The calling protocol.

      Effect : Arithmetic.Assignment_Set_T (
         Max_Size => Caller_Cells'Length
                   + Programs.Number_Of_Stacks (
                        Flow.Computation.Program (Model))
                   + 1);
      -- For collecting the effect.
      -- The "+ 1" is to make the size positive.

      Mark : Output.Nest_Mark_T;
      -- Locus for the Call_Bounds.

   begin

      Mark := Output.Nest (Locus (Call_Bounds.Call));

      -- The known outputs, except for invariant cells and
      -- stack-pointer cells:

      Flow.Calls.Add_Call_Effect (
         Call      => Call,
         Protocol  => Protocol.all,
         Outputs   => Output_Cells (Call_Bounds.Bounds),
         Invariant => Call_Invariants (Call, Within)
                    & Stack_Pointer_Cells (Program (Within)),
         To        => Effect);

      -- The net changes in local stack heights and stack pointers:

      Arithmetic.Add (
         To   => Effect,
         More => Final_Stack_Effect (Call_Bounds.Bounds));

      -- The unknown outputs:

      if Unknown_Effect (Call_Bounds.Bounds) then
         -- Check which Caller_Cells may be affected by the
         -- unknown effects of the callee.

         Flow.Calls.Add_Unknown_Call_Effect (
            Call     => Call,
            Protocol => Protocol.all,
            Stub     => Stub_Level (Call_Bounds.Bounds),
            Upon     => Caller_Cells,
            To       => Effect);

      end if;

      -- And there we have it:

      Flow.Computation.Set_Effect (
         Step  => Step (Call),
         To    => Arithmetic.To_Effect_Ref (Effect),
         Under => Model);

      Output.Unnest (Mark);

   exception

   when others =>

      Output.Unnest (Mark);

      raise;

   end Compute_Effect;


   procedure Compute_Call_Effects (
      Calls  : in Call_Bounds_List_T;
      Within : in Bounds_Ref)
   --
   -- Defines the effects of the call steps for the given Calls, Within
   -- the given execution bounds of the caller.
   --
   -- The effect of a call (that is, the effect of the call step) is to
   -- assign opaque values to all those cells, Within.All_Cells, that
   -- may be altered by the execution of the call.
   --
   -- Precondition: Within.All_Cells is up to date.
   --
   is

      All_Cells : constant Storage.Cell_List_T :=
         Cell_Sets.To_List (Within.All_Cells);
      -- All the cells referenced in the caller.

   begin

      for C in Calls'Range loop

         Compute_Effect (
            Call_Bounds  => Calls(C),
            Within       => Within,
            Caller_Cells => All_Cells);

      end loop;

   end Compute_Call_Effects;


   procedure Initialize_Bounds (
      For_Time   : in     Boolean;
      For_Space  : in     Boolean;
      Subprogram : in     Subprogram_T;
      Along      : in     Call_Path_T;
      Caller     : in     Bounds_Ref;
      Within     : in     Bounds_Set_T;
      Bounds     :    out Bounds_Ref)
   is

      Earlier_Bounds : constant Bounds_Ref :=
         Bounds_For (Subprogram, Within, Along);
      -- The most relevant bounds known for this Subprogram and
      -- this call-path. If such bounds exist, they are probably
      -- incomplete (not fully bounded) and apply to a suffix of
      -- the call-path, not to the whole call-path. If Along is
      -- not null, at least "universal" earlier bounds should exist.


      procedure Find_Call_Bounds (Call_Bounds : in out Call_Bounds_T)
      --
      -- Sets Call_Bounds.Bounds to the best bounds currently known
      -- for Call_Bounds.Call in this context. Check if the call
      -- can return to the caller, under these bounds.
      --
      is

         Path : constant Call_Path_T := Along & Call_Bounds.Call;
         -- The calling context.

         Step : constant Flow.Step_T := Programs.Step (Call_Bounds.Call);
         -- The call-step.

      begin

         Call_Bounds.Bounds := Bounds_For (
            Subprogram => Callee (Call_Bounds.Call),
            Within     => Within,
            Along      => Path);

         if not Flow.Computation.Is_Feasible (Step, Bounds.Computation) then
            -- This call is already known to be infeasible, so we will
            -- not worry if its bounds are undefined.

            null;

         elsif Call_Bounds.Bounds = No_Bounds then

            Output.Fault (
               Location =>
                  "Programs.Execution.Initialize_Bounds.Find_Call_Bounds",
               Locus => Locus (Path),
               Text  => "No execution bounds known.");

         else

            if Opt.Trace_Bounds then

               Output.Trace (
                  Locus => Locus (Bounds),
                  Text  =>
                       "Link from bounds #"
                     & Bounds_Index_T'Image (Index (Bounds))
                     & " to bounds #"
                     & Bounds_Index_T'Image (Index (Call_Bounds.Bounds))
                     & " at "
                     & Image (Call_Bounds.Call));

            end if;

            if not Is_Feasible (Call_Bounds.Bounds) then
               -- This callee cannot be executed under those call bounds,
               -- so the call itself must be held infeasible.

               if Flow.Pruning.Opt.Warn_Unreachable then

                  Output.Warning (
                     Locus => Locus (Path),
                     Text  => "Callee is unused or infeasible.");

               end if;

               Flow.Computation.Mark_Infeasible (Step, Bounds.Computation);

            elsif not Returns (Call_Bounds.Bounds) then

               if Flow.Calls.Opt.Warn_No_Return then

                  Output.Warning (
                     Locus => Locus (Path),
                     Text  => "Non-returning call.");

               end if;

               Flow.Computation.Mark_No_Return (
                  From => Call_Bounds.Call,
                  To   => Bounds.Computation);

            end if;

         end if;

      end Find_Call_Bounds;


   begin  -- Initialize_Bounds

      if Along'Length > 0 then
         -- We expect certain things:

         if Callee (Along(Along'Last)) /= Subprogram then

            Output.Fault (
               Location => "Programs.Execution.Initialize_Bounds",
               Locus    => Locus (Along),
               Text     =>
                    "Last callee is not "
                  & Index_And_Name (Subprogram));

         end if;

         if Earlier_Bounds = null then

            Output.Fault (
               Location => "Programs.Execution.Initialize_Bounds",
               Locus    => Locus (Along),
               Text     =>
                    "No earlier bounds exist for "
                  & Index_And_Name (Subprogram));

         end if;

      end if;

      -- Create blank bounds:

      Create_Blank_Bounds (
         For_Time   => For_Time,
         For_Space  => For_Space,
         Subprogram => Subprogram,
         Call_Path  => Along,
         Caller     => Caller,
         Within     => Within,
         Bounds     => Bounds);

      if Opt.Trace_Bounds then

         Output.Trace (
            Locus => Locus (Bounds),
            Text  =>
                 "Created bounds #"
               & Bounds_Index_T'Image (Index (Bounds))
               & " with"
               & Loops.Loop_Count_T'Image (Bounds.Num_Loops)
               & " loops and"
               & Natural'Image (Bounds.Num_Calls)
               & " calls.");

         if Caller /= null then

            Output.Trace (
               Locus => Locus (Bounds),
               Text  =>
                    "Called from bounds #"
                  & Bounds_Index_T'Image (Index (Caller)));

         end if;

         if Earlier_Bounds /= null then

            Output.Trace (
               Locus => Locus (Bounds),
               Text  =>
                    "Based on earlier bounds #"
                  & Bounds_Index_T'Image (Index (Earlier_Bounds))
                  & " for path "
                  & Image (Call_Path (Earlier_Bounds)));

         end if;

      end if;

      -- Initialization depending on the earlier bounds if any:

      if Earlier_Bounds = null then
         -- There are no relevant earlier (more universal) bounds.
         -- We start from the "primitive" computation model.

         Flow.Computation.Refer_To_Primitive_Model (
            Subprogram => Subprogram,
            Model      => Bounds.Computation);
         --
         -- Note that this model considers all calls to have a
         -- new (changed) calling protocol, which means that the
         -- effects of all calls will be computed in the next call
         -- (below) of Note_Updated_Computation.

         Bounds.All_Cells := Cell_Sets.Empty;
         --
         -- We compute this set later, when we know which calls
         -- are unbounded and so can include their input cells.
         -- See call of Note_Updated_Computation below.

         -- We add the effects of the calls later below.

      else
         -- We have some earlier (more universal) bounds, so we
         -- start from the computation model used in those bounds,
         -- including the effects of the calls.

         Flow.Computation.Refer (
            From => Bounds.Computation,
            To   => Earlier_Bounds.Computation);

         -- The same cells are used as in the earlier bounds:

         Bounds.All_Cells := Earlier_Bounds.All_Cells;

         -- Input bounds for calls are reused:

         Bounds.Call_Inputs := Earlier_Bounds.Call_Inputs;

      end if;

      -- If the subprogram has some calls to lower-level subprograms,
      -- we set up the bounds for these calls, using the best bounds
      -- currently known for the callees in this context:

      for C in Bounds.Call_Bounds'Range loop

         Find_Call_Bounds (Bounds.Call_Bounds(C));

      end loop;

      -- If there are no earlier bounds, the effects of the calls
      -- must be computed ab initio:

      if Earlier_Bounds = null then
         -- The Model did not inherit the effects of the Calls, so we
         -- compute them now and add them to the computation model.
         -- This also computes Bounds.All_Cells.

         Note_Updated_Computation (Within => Bounds);

         Flow.Computation.Mark_Clean (Bounds.Computation);

      end if;

      -- TBD inherit loop bounds from Earlier_Bounds?

   end Initialize_Bounds;


   procedure Initialize_Universal_Bounds (
      Subprogram : in     Subprogram_T;
      Within     : in     Bounds_Set_T;
      Bounds     :    out Bounds_Ref)
   is
   begin

      Initialize_Bounds (
         For_Time   => Within.For_Time,
         For_Space  => Within.For_Space,
         Subprogram => Subprogram,
         Along      => Null_Call_Path,
         Caller     => null,
         Within     => Within,
         Bounds     => Bounds);

   end Initialize_Universal_Bounds;


   procedure Adopt_To_Context (
      Call   : in     Call_T;
      Caller : in     Bounds_Ref;
      Within : in     Bounds_Set_T;
      Giving :    out Bounds_Ref)
   is

      Original : constant Bounds_Ref :=
         Call_Bounds (On => Call, Within => Caller);
      -- The current execution bounds on the Call, within the Caller
      -- bounds. Either inherited from a shallower context, or
      -- already specialized to these Caller bounds.

      Deeper : constant Call_List_T := Calls_Of (Call_Bounds (Original));
      -- The calls from the callee to deeper subprograms.

      Stack : Stack_T;
      -- One of the stacks in the program.

      Deep : Call_T;
      -- One of the Deeper calls.

   begin

      if Original.Caller_Bounds = Caller then
         -- Already context-specific to these Caller bounds.

         Giving := Original;

      else
         -- The Original bounds are inherited from another context.
         -- We must copy them before we can enter information
         -- specific to this Caller context.

         if Opt.Trace_Bounds then

            Output.Trace (
                 "Adopting bounds #"
               & Bounds_Index_T'Image (Index (Original))
               & " into caller bounds #"
               & Bounds_Index_T'Image (Index (Caller)));

         end if;

         Initialize_Bounds (
            For_Time   => For_Time   (Original),
            For_Space  => For_Space  (Original),
            Subprogram => Subprogram (Original),
            Along      => Call_Path  (Caller  ) & Call,
            Caller     => Caller,
            Within     => Within,
            Bounds     => Giving);

         -- Copy the path and execution-time bounds, if any:

         case Time_State (Original) is

         when Undefined =>

            if For_Time (Original) then

               Output.Fault (
                  Location => "Programs.Execution.Adopt_To_Context",
                  Locus    => Locus (Call, Caller),
                  Text     => "Callee Time State is UNDEFINED.");

            end if;

         when Depends | Unbounded | Failed =>
            -- We can hope for better success in the new context.
            -- Setting the state to Undefined allows a new start.

            Set_Time_State (
               To     => Undefined,
               Within => Giving);

         when Vague | Infeasible =>
            -- A new context cannot help.

            Set_Time_State (
               To     => Time_State (Original),
               Within => Giving);

         when Time_Boundable_T =>

            Giving.Node_Bounds      := Original.Node_Bounds;
            Giving.Loop_Bounds      := Original.Loop_Bounds;
            Giving.Void_Flow_Bounds := Original.Void_Flow_Bounds;

            if Time_State (Original) in Time_Bounded_T then

               Bound_Time (
                  Time   => Time (Original),
                  State  => Time_State (Original),
                  Within => Giving);

            else

               Set_Time_State (
                  To     => Time_State (Original),
                  Within => Giving);

            end if;

         end case;

         -- Copy the stack bounds, if any:

         for S in 1 .. Original.Num_Stacks loop

            Stack := Stack_By (Index => S, Within => Program (Original));

            -- Local height:

            if Stack_Height_Bounded (Stack, Original) then

               Bound_Stack_Height (
                  Stack  => Stack,
                  To     => Stack_Height (Stack, Original),
                  Within => Giving,
                  Silent => True);

            end if;

            -- Usage:

            Stack := Stack_By (Index => S, Within => Program (Original));

            if Stack_Usage_Bounded (Stack, Original) then

               Bound_Stack_Usage (
                  Stack  => Stack,
                  To     => Stack_Usage (Stack, Original),
                  Within => Giving,
                  Silent => True);

            end if;

            -- Final height:

            if Final_Stack_Height_Known (Stack, Original) then

               Bound_Final_Stack_Height (
                  Stack  => Stack,
                  To     => Final_Stack_Height (Stack, Original),
                  Within => Giving,
                  Silent => True);

            end if;

            -- Take-off height for deeper calls:

            for D in Deeper'Range loop

               Deep := Deeper(D);

               if Take_Off_Height_Bounded (Stack, Deep, Original) then

                  Bound_Take_Off_Height (
                     Stack  => Stack,
                     Before => Deep,
                     To     => Take_Off_Height (Stack, Deep, Original),
                     Within => Giving,
                     Silent => True);

               end if;

            end loop;

         end loop;

         -- Copy the processor-specific info:

         Set_Processor_Info (
            To     => Processor_Info (Original),
            Within => Giving);

         -- Copy the input and output cell-sets:

         Set_Input_Cells (
            To     => Original.Input_Cells,
            Within => Giving);

         Set_Output_Cells (
            To     => Original.Output_Cells,
            Within => Giving);

         -- Copy the (reference to the) assertion map:

         Giving.Assertion_Map := Original.Assertion_Map;

         -- And these are the new bounds on the Call:

         Bound_Call (
            Call   => Call,
            Bounds => Giving,
            Within => Caller);

      end if;

   end Adopt_To_Context;


   procedure Set_Processor_Info (
      To     : in Processor.Execution.Info_T;
      Within : in Bounds_Ref)
   is
   begin

      Within.Info := To;

   end Set_Processor_Info;


   function All_Cells (Within : Bounds_Ref) return Storage.Cell_List_T
   is
   begin

      return Cell_Sets.To_List (Within.All_Cells);

   end All_Cells;


   procedure Check_Mutability (Bounds : in Bounds_Ref)
   --
   -- Checks that the Bounds are not Frozen. Emits a Fault
   -- message if they are Frozen.
   --
   is
   begin

      if Bounds.Frozen then

         Output.Fault (
            Location => "Programs.Execution.Check_Mutability",
            Text     =>
                 "Bounds #"
               & Bounds_Index_T'Image (Bounds.Index)
               & " are Frozen.");

      end if;

   end Check_Mutability;


   function Computation_Changed (Within : Bounds_Ref)
   return Boolean
   is
   begin

      return Flow.Computation.Changed (Within.Computation);

   end Computation_Changed;


   procedure Note_Updated_Computation (Within : in Bounds_Ref)
   is
      use type Cell_Set_T;

      Model : Flow.Computation.Model_Ref renames Within.Computation;
      -- The updated computation model.

      Calls : constant Call_Bounds_List_T := Call_Bounds (Within);
      -- The feasible calls and their bounds, Within these caller bounds.
      -- Some of these calls may become infeasible from later analysis
      -- of the updated computation model.

      Call : Programs.Call_T;
      -- One of the Calls.

      Calls_To_Compute : Call_Bounds_List_T (1 .. Calls'Length);
      Last : Natural := 0;
      -- The calls for which the effect must be recomputed will be
      -- in Calls_To_Compute(1 .. Last).

      New_All_Cells : Cell_Set_T;
      -- The new set of referenced cells.

      New_Inputs, New_Outputs : Cell_Set_T;
      -- The new set of input and output cells for significant calls.

      Cell_Set_Changed : Boolean;
      -- Whether the set of referenced cells has changed.

   begin

      -- Update the set of all referenced cells:

      -- First we find the currently known inputs of all unbounded calls,
      -- using the current (possibly updated) calling protocols:

      Add_Inputs_For_Unbounded_Calls (
         Within => Within,
         To     => New_Inputs);

      -- Next we find the currently known outputs of all (feasible)
      -- calls, using the current (possibly updated) calling protocols:

      for C in Calls'Range loop

         Call := Calls(C).Call;

         Cell_Sets.Add (
            Cells => Flow.Calls.Output_Cells (
               Call      => Call,
               Protocol  => Flow.Computation.Calling_Protocol (Call, Model).all,
               Outputs   => Output_Cells (Calls(C).Bounds),
               Invariant => Call_Invariants (Call, Within)),
            To => New_Outputs);

      end loop;

      -- We then find the cells referenced in the real steps of
      -- the computation, and ignore the existing (old and possibly
      -- irrelevant) effects of call steps. Note that the new protocols
      -- may have created entirely new cells (Cell_T); by initializing
      -- our new Cell_Set_T here, it is sure to have space for these
      -- new cells, too.

      New_All_Cells := Cell_Sets.Copy (
         Flow.Computation.Cells_In (
            Model => Within.Computation,
            Calls => False));

      -- And finally we combine all of the above:

      Cell_Sets.Add (
         Cells => New_Inputs,
         To    => New_All_Cells);

      Cell_Sets.Add (
         Cells => New_Outputs,
         To    => New_All_Cells);

      -- The lists of new inputs and outputs are no longer needed:

      Cell_Sets.Erase (New_Inputs );
      Cell_Sets.Erase (New_Outputs);

      -- Check if the cell-set changed, and take the new cell-set:

      Cell_Set_Changed := New_All_Cells /= Within.All_Cells;

      Within.All_Cells := New_All_Cells;

      -- Update the effects of all calls:
      --
      -- First we find the Calls where the effect should
      -- be recomputed:

      for C in Calls'Range loop

         if Flow.Computation.Calling_Protocol_Changed (Calls(C).Call, Model)
         or else (
            Cell_Set_Changed
            and then Unknown_Effect (Calls(C).Bounds))
         then
            -- The effect of this Call must be recomputed because either
            -- its calling protocol was changed, or because the total set
            -- of cells in the model changed and this Call has a (partially)
            -- unknown effect so it can clobber some of the model cells.

            Last := Last + 1;
            Calls_To_Compute(Last) := Calls(C);

         end if;

      end loop;

      -- Then we recompute the effects of these calls:

      if Last > 0 then

         Compute_Call_Effects (
            Calls  => Calls_To_Compute(1 .. Last),
            Within => Within);

      end if;

   end Note_Updated_Computation;


   procedure Mark_Computation_Clean (Within : in Bounds_Ref)
   is
   begin

      Flow.Computation.Mark_Clean (Within.Computation);

   end Mark_Computation_Clean;


   procedure Bound_Computation (
      Model  : in Flow.Computation.Model_Ref;
      Within : in Bounds_Ref)
   is
   begin

      Check_Mutability (Within);
      -- The computation model defines the feasibility of steps
      -- and edges and thus affects the paths.

      Flow.Computation.Refer (
         From => Within.Computation,
         To   => Model);

      Note_Updated_Computation (Within);

   end Bound_Computation;


   procedure Bound_Value_Origins (Within : in Bounds_Ref)
   --
   -- Computes or recomputes the value-origin map for the computation
   -- model Within the given bounds. If a value-origin map already exists,
   -- it is discarded before the new map is computed.
   is
   begin

      Flow.Origins.Discard (Within.Value_Origins);

      Flow.Origins.Analyse (
         Computation => Within.Computation,
         Giving      => Within.Value_Origins);

   end Bound_Value_Origins;


   procedure Bound_Assertions (
      Map    : in Assertions.Assertion_Map_T;
      Within : in Bounds_Ref)
   is
   begin

      Check_Mutability (Within);
      -- Some loops/calls may be asserted as infeasible.

      -- TBA discard old map if any. Note that it can be referenced
      -- from several Bounds_Objects; Adopt_To_Context copies the
      -- reference and does not make a deep copy.

      Within.Assertion_Map := Map;

      Within.Enough_For_Time :=
         Assertions.Subprogram_Enough_For_Time (
            Subprogram => Within.Subprogram,
            Asserts    => Assertions.Set (Map));

   end Bound_Assertions;


   procedure Initialize_Asserted_Bounds (
      Subprogram : in     Subprogram_T;
      Within     : in     Bounds_Set_T;
      Bounds     :    out Bounds_Ref)
   is
   begin

      Create_Blank_Bounds (
         For_Time   => Within.For_Time,
         For_Space  => Within.For_Space,
         Subprogram => Subprogram,
         Call_Path  => Null_Call_Path,
         Caller     => null,
         Within     => Within,
         Bounds     => Bounds);

      if Opt.Trace_Bounds then

         Output.Trace (
            Locus => Locus (Bounds),
            Text  =>
                 "Created bounds #"
               & Bounds_Index_T'Image (Index (Bounds))
               & " for asserted bounds.");

      end if;

      Flow.Computation.Refer_To_Primitive_Model (
         Subprogram => Subprogram,
         Model      => Bounds.Computation);

      Bounds.All_Cells := Cell_Sets.Empty;

      Set_Input_Cells (
         To     => Cell_Sets.Empty,
         Within => Bounds);

      Set_Output_Cells (
         To     => Cell_Sets.Empty,
         Within => Bounds);

      Bounds.Time_State := Vague;

      for S in Bounds.Max_Stack_Usage'Range loop

         Bounds.Max_Stack_Usage(S).State := Vague;

      end loop;

   end Initialize_Asserted_Bounds;


   procedure Set_Input_Cells (
      To     : in Storage.Cell_Set_T;
      Within : in Bounds_Ref)
   is
   begin

      Check_Mutability (Within);
      -- The input cells do not directly affect the paths,
      -- but there is no reason to change the input cells when
      -- the paths are frozen.

      Within.Input_Cells      := Cell_Sets.Copy (To);
      Within.Input_Cell_Tally := Storage.Card (To);
      Within.Inputs_Defined   := True;

   end Set_Input_Cells;


   procedure Set_Basis_Cells (
      To     : in Storage.Cell_Set_T;
      Within : in Bounds_Ref)
   is
   begin

      Check_Mutability (Within);
      -- The basis cells do not directly affect the paths,
      -- but there is no reason to change the basis cells when
      -- the paths are frozen.

      Within.Basis := Cell_Sets.Copy (To);

   end Set_Basis_Cells;


   procedure Set_Output_Cells (
      To     : in Storage.Cell_Set_T;
      Within : in Bounds_Ref)
   is
   begin

      Check_Mutability (Within);
      -- The output cells do not directly affect the paths,
      -- but there is no reason to change the output cells when
      -- the paths are frozen.

      Within.Output_Cells    := Cell_Sets.Copy (To);
      Within.Outputs_Defined := True;

   end Set_Output_Cells;


   procedure Remove_From_Output (
      Cells  : in Storage.Cell_List_T;
      Within : in Bounds_Ref)
   is
   begin

      Check_Mutability (Within);

      if Within.Outputs_Defined then

         Cell_Sets.Remove (
            Cells => Cells,
            From  => Within.Output_Cells);

      else

         Output.Fault (
            Location => "Programs.Execution.Remove_From_Output",
            Text     => "Output cells not yet defined.");

      end if;

   end Remove_From_Output;


   procedure Bound_Initial_Values (
      To     : in Storage.Bounds.Cell_Interval_List_T;
      Within : in Bounds_Ref)
   is
   begin

      -- TBA discard the old Within.Initial.

      Within.Initial := new Storage.Bounds.Cell_Interval_List_T'(To);

   end Bound_Initial_Values;


   procedure Bound_Call_Inputs (
      To   : in Storage.Bounds.Var_Interval_List_T;
      From : in Bounds_Ref)
   is
   begin

      for C in From.Call_Inputs'Range loop

         From.Call_Inputs(C) :=
            Storage.Bounds.Interval_Bounds (
               From  => To,
               Point => Prime_Address (
                  Call (
                     From  => Subprogram (From),
                     Index => C)));

      end loop;

   end Bound_Call_Inputs;


   procedure Bound_Edge_Times (
      Times  : in Step_Edge_Times_T;
      Within : in Bounds_Ref)
   is
      use type Flow.Step_Edge_Index_T;

      Max_Index : constant Flow.Step_Edge_Count_T :=
         Flow.Max_Step_Edge (Flow_Graph (Within.Subprogram));
      -- The largest step-edge index in the subprogram, or zero if
      -- the subprogram has only one step and no step-edges.

   begin

      if Times'First /= 1 or Times'Last /= Max_Index then

         Output.Fault (
            Location => "Programs.Execution.Bound_Edge_Times",
            Text     =>
                "Times'Range = "
               & Output.Image (Natural (Times'First))
               & " .. "
               & Output.Image (Natural (Times'Last ))
               & " /= 1 .. "
               & Output.Image (Natural (Max_Index  )));

         raise Program_Error;

      end if;

      Flow.Execution.Times.Discard (Within.Step_Edge_Times);

      Within.Step_Edge_Times :=
         new Flow.Execution.Times.Step_Edge_Times_T'(Times);

      Flow.Execution.Times.Discard (Within.Node_Times);
      -- The new edge-times imply new node-times.

      case Within.Time_State is

      when Undefined
         | Vague
         | Depends
         | Computable
         | Asserted
         | Infeasible
         | Unbounded   =>
         -- Changing the edge-times has no effect on the time-state.

         null;

      when Computed =>
         -- The time has to be recomputed for the new edge-times.

         Within.Time_State := Computable;

      when Failed =>
         -- We can hope that a new computation might succeed.

         Within.Time_State := Computable;

      end case;

   end Bound_Edge_Times;


   procedure Bound_Node_Times (
      Times  : in Node_Times_T;
      Within : in Bounds_Ref)
   is
      use type Flow.Node_Index_T;

   begin

      if Times'First /= 1 or Times'Last /= Within.Max_Node then

         Output.Fault (
            Location => "Programs.Execution.Bound_Flow_Times",
            Text     =>
                "Times'Range = "
               & Output.Image (Natural (Times'First))
               & " .. "
               & Output.Image (Natural (Times'Last ))
               & " /= 1 .. "
               & Output.Image (Natural (Within.Max_Node)));

         raise Program_Error;

      end if;

      Flow.Execution.Times.Discard (Within.Node_Times);

      Within.Node_Times := new Flow.Execution.Times.Node_Times_T'(Times);

      Within.Time_State := Undefined;
      -- The new flow-times imply a new total time.

   end Bound_Node_Times;


   procedure Compute_Node_Times (Bounds : in Bounds_Ref)
   is
   begin

      if Opt.Trace_Bounds then

         Output.Trace (
            Locus => Locus (Bounds),
            Text  =>
                 "Computing per-node execution times for bounds #"
               & Bounds_Index_T'Image (Index (Bounds)));

      end if;

      Bound_Node_Times (
         Times =>
            Flow.Execution.Times.Node_Times (
               Subprogram      => Bounds.Subprogram,
               Step_Edge_Times => Bounds.Step_Edge_Times.all,
               Asserts         => Bounds.Assertion_Map),
         Within => Bounds);

   end Compute_Node_Times;


   procedure Compute_Node_Times (
      Calls     : in Call_Bounds_List_T;
      Max_Index : in Bounds_Index_T)
   is

      -- We perform this computation bottom-up in the bounds
      -- hierarchy. At the moment there is no pressing need to do
      -- it this way, but later there may be some additional analysis
      -- that causes a bottom-to-top dependency of node times.
      --
      -- The bottom-up process is implemented by post-order recursion
      -- and a boolean marking of the processed bounds.

      Computed : Bounds_Subset_T (Max_Size => Max_Index);
      --
      -- The subset of bounds objects for which node times have been
      -- Computed. If a bounds object is a member of this set, so
      -- are all its (directly or indirectly) nested callee bounds,
      -- thanks to the bottom-up computation order.
      -- Default initialized to the empty set.


      procedure Compute_Times_For (Bounds : in Bounds_Ref);
      --
      -- Computes node-times for the given bounds and its nested
      -- callee bounds.


      procedure Compute_Times_For (Callee_Bounds : in Call_Bounds_List_T)
      --
      -- Computes node-times for all the lower-level callees.
      --
      is
      begin

         for C in Callee_Bounds'Range loop

            Compute_Times_For (Callee_Bounds(C).Bounds);

         end loop;

      end Compute_Times_For;


      procedure Compute_Times_For (
         Bounds : in Bounds_Ref)
      is
      begin

         if  not Is_Member (Bounds, Computed)
         and not Time_Asserted (Bounds)
         then

            Add (Item => Bounds, To => Computed);

            Compute_Times_For (Call_Bounds (Bounds));

            -- The node times have now been computed for all the
            -- lower-level callees.

            Compute_Node_Times (Bounds);

         end if;

      end Compute_Times_For;


   begin  -- Compute_Node_Times

      for C in Calls'Range loop

         Compute_Times_For (Calls(C).Bounds);

      end loop;

   end Compute_Node_Times;


   procedure Bound_Flow_Counts (
      Counts : in Flow.Execution.Counts_Ref;
      Within : in Bounds_Ref)
   is
   begin

      Within.Flow_Counts := Counts;

   end Bound_Flow_Counts;


   procedure Conjoin (
      Also   : in     Flow.Execution.Bound_T;
      To     : in out Flow.Execution.Bound_T;
      Within : in     Bounds_Ref)
   --
   -- Conjoins a new bound To an existing bound, which means
   -- that To requires Also the new bound. If the conjoined bounds
   -- are void, the execution bounds are marked as having void
   -- (contradictory) flow bounds.
   --
   is
      use Flow.Execution;
   begin

      To := To and Also;

      if Void (To) then

         Within.Void_Flow_Bounds := True;

      end if;

   end Conjoin;


   procedure Bound_Node_Count (
      Node   : in Flow.Node_T;
      Count  : in Flow.Execution.Bound_T;
      Within : in Bounds_Ref)
   is
      use Flow.Execution;

      Bound : Flow.Execution.Bound_T renames
         Within.Node_Bounds(Flow.Index (Node));
      -- The execution-count bounds on this Node.

   begin

      Conjoin (Also => Count, To => Bound, Within => Within);

      if Bound.Max = 0 then

         Flow.Computation.Mark_Infeasible (
            Step  => Flow.First_Step (Node),
            Under => Within.Computation);

      end if;

   end Bound_Node_Count;


   procedure Bound_Node_Counts (
      By     : in Flow.Execution.Node_Count_Bounds_T;
      Within : in Bounds_Ref)
   is
   begin

      for B in By'Range loop

         Bound_Node_Count (
            Node   => By(B).Node,
            Count  => By(B).Count,
            Within => Within);

      end loop;

   end Bound_Node_Counts;


   procedure Bound_Call_Count (
      Call   : in Call_T;
      Count  : in Flow.Execution.Bound_T;
      Within : in Bounds_Ref)
   is

      Caller : constant Subprogram_T := Programs.Caller (Call);
      -- The caller of the call.

   begin

      if Caller = Subprogram (Within) then
         -- Right, this Call is from Within these bounds.

         Bound_Node_Count (
            Node   => Flow.Node_Containing (
                         Step  => Step (Call),
                         Graph => Flow_Graph (Caller)),
            Count  => Count,
            Within => Within);

      else
         -- This call is foreign. Ugh.

         Output.Fault (
            Location => "Programs.Execution.Bound_Call_Count",
            Text     => "Caller is " & Name (Caller));

      end if;

   end Bound_Call_Count;


   procedure Bound_Loop_Starts (
      Luup   : in Loops.Loop_T;
      Starts : in Flow.Execution.Bound_T;
      Within : in Bounds_Ref)
   is
   begin

      Conjoin (
         Also   => Starts,
         To     => Within.Loop_Bounds.Loop_Starts(Loops.Loop_Index (Luup)),
         Within => Within);

   end Bound_Loop_Starts;


   procedure Bound_Loop_Neck (
      Luup   : in Loops.Loop_T;
      Count  : in Flow.Execution.Bound_T;
      Within : in Bounds_Ref)
   is
   begin

      Conjoin (
         Also   => Count,
         To     => Within.Loop_Bounds.Loop_Neck(Loops.Loop_Index (Luup)),
         Within => Within);

   end Bound_Loop_Neck;


   procedure Bound_Loop_Repeats (
      Luup    : in Loops.Loop_T;
      Repeats : in Flow.Execution.Bound_T;
      Within  : in Bounds_Ref)

   is
   begin


      Conjoin (
         Also   => Repeats,
         To     => Within.Loop_Bounds.Loop_Repeats(Loops.Loop_Index (Luup)),
         Within => Within);

   end Bound_Loop_Repeats;


   procedure Bound_Call_Input (
      Call   : in Call_T;
      Bounds : in Storage.Bounds.Bounds_Ref;
      Within : in Bounds_Ref)
   is
   begin

      if Opt.Trace_Call_Input_Bounds then

         Output.Trace (
            Locus => Locus (Call),
            Text  =>
                 "Input bounds"
               & Output.Field_Separator
               & Storage.Bounds.Full_Image (Bounds.all));

      end if;

      Check_Mutability (Within);
      -- There is no reason to change the call-input bounds after
      -- the containing bounds are frozen.

      Within.Call_Inputs(Index (Call)) := Bounds;

      -- TBA discard the old Call_Inputs bounds (carefully...)

   end Bound_Call_Input;


   procedure Bound_Call (
      Call   : in Call_T;
      Bounds : in Bounds_Ref;
      Within : in Bounds_Ref)
   is
   begin

      Check_Mutability (Within);

      if Bounds.Caller_Bounds /= Within and (not Bounds.Frozen) then
         -- If we are linking Bounds from a different (shallower)
         -- context, Within the caller's bounds for a deeper
         -- context, the Bounds on the call must be frozen so
         -- that their essential properties are immutable.

         Output.Fault (
            Location => "Programs.Execution.Bound_Call",
            Text =>
                 "Bounds #"
               & Bounds_Index_T'Image (Bounds.Index)
               & " are not Frozen.");

      end if;

      Within.Call_Bounds(Index (Call)).Bounds := Bounds;

      -- TBA discard the old call-bounds (if not used elsewhere).

   end Bound_Call;


   procedure Bound_Final_Stack_Height (
      Stack  : in Stack_T;
      To     : in Final_Stack_Height_T;
      Within : in Bounds_Ref;
      Silent : in Boolean := False)
   is
      use type Storage.Bounds.Interval_T;

      Stix : constant Stack_Index_T := Index (Stack);

      Intval : Final_Stack_Height_T renames Within.Final_Stack_Height (Stix);

      function Name_And_Bounds (Item : Final_Stack_Height_T)
      return String
      is
      begin

         return Output.Field_Separator
              & Name (Stack)
              & Output.Field_Separator
              & Image (
                   Item => Item,
                   Name => Storage.Image (Height (Stack)));

      end Name_And_Bounds;

   begin  -- Bound_Final_Stack_Height

      Check_Mutability (Within);

      case Stack.Kind is

      when Stable =>

         if To /= To_Stack_Limit (Storage.Bounds.Exactly_Zero) then

            Output.Fault (
               Location => "Programs.Execution.Bound_Final_Stack_Height",
               Text     =>
                    "Non-zero final-height bounds for Stable stack"
                  & Name_And_Bounds (To));

         end if;

      when Unstable =>

         Intval := Intval and To;

         if Opt.Trace_Stack_Bounds and not Silent then

            Output.Trace (
                 "Final stack height"
               & Name_And_Bounds (Intval));

         end if;

         if Void (Intval) then

            Output.Fault (
               Location => "Programs.Execution.Bound_Final_Stack_Height",
               Text     =>
                    "Void final-height bounds for Unstable stack"
                  & Name_And_Bounds (Intval));

         end if;

      end case;

   end Bound_Final_Stack_Height;


   procedure Bound_Stack_Height (
      Stack  : in Stack_T;
      To     : in Stack_Limit_T;
      Within : in Bounds_Ref;
      Silent : in Boolean := False)
   is
   begin

      Check_Mutability (Within);

      Within.Stack_Height(Index (Stack)) := To;

      if Opt.Trace_Stack_Bounds and not Silent then

         Output.Trace (
              "Local stack height"
            & Output.Field_Separator
            & Name (Stack)
            & Output.Field_Separator
            & Image (To));

      end if;

   end Bound_Stack_Height;


   procedure Bound_Take_Off_Height (
      Stack  : in Stack_T;
      Before : in Call_T;
      To     : in Stack_Limit_T;
      Within : in Bounds_Ref;
      Silent : in Boolean := False)
   is
   begin

      Check_Mutability (Within);

      Within.Take_Off_Limits(Index (Before), Index (Stack)) := To;

      if Opt.Trace_Stack_Bounds and not Silent then

         Output.Trace (
            Locus => Locus (Before),
            Text  =>
        	 "Take-off stack height"
               & Output.Field_Separator
               & Name (Stack)
               & Output.Field_Separator
               & Image (To));

      end if;

   end Bound_Take_Off_Height;


   Stack_Key : constant String := "Stack";
   -- Key for "stack usage" in basic output line.


   procedure Bound_Stack_Usage (
      Stack  : in Stack_T;
      To     : in Stack_Usage_T;
      Within : in Bounds_Ref;
      Silent : in Boolean := False)
   is
   begin

      Check_Mutability (Within);

      if To.State = Undefined then

         Output.Fault (
            Location => "Programs.Execution.Bound_Stack_Usage",
            Text     => "To.State is Undefined");

      else

         Within.Max_Stack_Usage(Index (Stack)) := To;

         if Opt.Trace_Stack_Bounds and not Silent then

            Output.Trace (
                 "Total stack usage"
               & Output.Field_Separator
               & Name (Stack)
               & Output.Field_Separator
               & Image (To.Call)
               & Output.Field_Separator
               & Value_Image (To));

         end if;

         if Bounded (To) and not Silent then

            Output.Result (
               Key   => Stack_Key,
               Locus => Locus (Within),
               Text  =>
                    Name (Stack)
                  & Output.Field_Separator
                  & Value_Image (To) );

         end if;

      end if;

   end Bound_Stack_Usage;


   procedure Bound_Time (
      Time   : in Processor.Time_T;
      State  : in Time_Bounded_T;
      Within : in Bounds_Ref)
   is
   begin

      Within.Time_State := State;
      Within.Time       := Time;

   end Bound_Time;


   procedure Set_Time_State (
      To     : in Time_State_T;
      Within : in Bounds_Ref)
   is
   begin

      if Within.Time_State not in Time_Bounded_T
      and To                   in Time_Bounded_T
      then
         -- Claiming to have a time-bound where none exists.

         Output.Fault (
            Location => "Programs.Execution.Set_Time_State",
            Text     =>
                 "Changing Time_State from "
               & Time_State_T'Image (Within.Time_State)
               & " to "
               & Time_State_T'Image (To));

      end if;

      Within.Time_State := To;

   end Set_Time_State;


   procedure Set_Time_Calculated (Within : in Bounds_Ref)
   is
   begin

      if Within.Time_Calculated then

         Output.Fault (
            Location => "Programs.Execution.Set_Time_Calculated",
            Text     => "Already set.");

      end if;

      Within.Time_Calculated := True;

   end Set_Time_Calculated;


   --
   ---   Execution bound query operations:
   --

   -- Queries usually deliver all the accumulated bounds
   -- of a specific types, in one batch.


   function Defined (Item : Bounds_Ref) return Boolean
   is
   begin

      return Item /= No_Bounds;

   end Defined;


   function Index (Item : Bounds_Ref)
   return Bounds_Index_T
   is
   begin

      return Item.Index;

   end Index;


   function Subprogram (Item : Bounds_Ref)
   return Subprogram_T
   is
   begin

      return Item.Subprogram;

   end Subprogram;


   function Flow_Graph (Item : Bounds_Ref)
   return Flow.Graph_T
   is
   begin

      return Flow_Graph (Subprogram (Item));

   end Flow_Graph;


   function Call_Path (Item : Bounds_Ref)
   return Call_Path_T
   is
   begin

      return Item.Call_Path;

   end Call_Path;


   function Level (Item : Bounds_Ref) return Bounding_Level_T
   is
   begin

      if Item = No_Bounds then

         return Indefinite;

      else

         return Item.Level;

      end if;

   end Level;


   function Program (Item : Bounds_Ref) return Program_T
   is
   begin

      if not Defined (Item) then

         Output.Fault (
            Location => "Programs.Execution.Program",
            Text     => "Bounds undefined.");

        raise Program_Error;

      end if;

      return Item.Program;

   end Program;


   function Symbol_Table (Item : Bounds_Ref) return Symbols.Symbol_Table_T
   is
   begin

      return Symbol_Table (Program (Item));

   end Symbol_Table;


   function For_Time (Bounds : Bounds_Ref) return Boolean
   is
   begin

      return Bounds.For_Time;

   end For_Time;


   function For_Space (Bounds : Bounds_Ref) return Boolean
   is
   begin

      return Bounds.For_Space;

   end For_Space;


   function Processor_Info (From : Bounds_Ref)
   return Processor.Execution.Info_T
   is
   begin

      return From.Info;

   end Processor_Info;


   function Computation (Item : Bounds_Ref)
   return Flow.Computation.Model_Handle_T
   is
   begin

      if not Defined (Item) then

         Output.Fault (
            Location => "Programs.Execution.Computation return Handle",
            Text     => "Bounds undefined.");

        raise Program_Error;

      end if;

      return Item.Computation'access;

   end Computation;


   function Value_Origins_Defined (Item : Bounds_Ref)
   return Boolean
   is
   begin

      if not Defined (Item) then

         Output.Fault (
            Location => "Programs.Execution.Value_Origins_Defined",
            Text     => "Bounds undefined.");

        raise Program_Error;

      end if;

      return Flow.Origins.Is_Valid (Item.Value_Origins);

   end Value_Origins_Defined;


   function Value_Origins (Item : Bounds_Ref)
   return Flow.Origins.Map_Ref
   is
   begin

      if not Defined (Item) then

         Output.Fault (
            Location => "Programs.Execution.Value_Origins",
            Text     => "Bounds undefined.");

        raise Program_Error;

      end if;

      return Item.Value_Origins;

   end Value_Origins;


   function Is_Feasible (Item : Bounds_Ref) return Boolean
   is
   begin

      return   (not Unused (Subprogram (Item)))
      and then Time_State (Item) /= Infeasible
      and then Flow.Computation.Is_Feasible (Computation (Item).all);

   end Is_Feasible;


   function Returns (Item : Bounds_Ref) return Boolean
   is
   begin

      return Returns (Subprogram (Item))
      and then Flow.Computation.Returns (Computation (Item).all);

   end Returns;


   function Calling_Protocol (
      Call   : Call_T;
      Within : Bounds_Ref)
   return Calling.Protocol_Ref
   is
   begin

      return Flow.Computation.Calling_Protocol (
         Call  => Call,
         Under => Within.Computation);

   end Calling_Protocol;


   function Dynamic_Flow (Item : Bounds_Ref)
   return Flow.Edge_Resolution_T
   is
   begin

      return Flow.Computation.Dynamic_Flow (Item.Computation);

   end Dynamic_Flow;


   function Unstable_Dynamic_Edges (Item : Bounds_Ref)
   return Flow.Dynamic_Edge_List_T
   is
   begin

      return Flow.Computation.Unstable_Dynamic_Edges (Item.Computation);

   end Unstable_Dynamic_Edges;


   function Assertion_Map (Item : Bounds_Ref)
   return Assertions.Assertion_Map_T
   is
   begin

      if Defined (Item) then

         return Item.Assertion_Map;

      else

         return Assertions.No_Map;

      end if;

   end Assertion_Map;


   function Inputs_Defined (Item : Bounds_Ref)
   return Boolean
   is
   begin

      return Item.Inputs_Defined;

   end Inputs_Defined;


   function Input_Cells (Item : Bounds_Ref)
   return Storage.Cell_List_T
   is
   begin

      if not Item.Inputs_Defined then

         Output.Fault (
            Location => "Programs.Execution.Input_Cells",
            Locus    => Locus (Item),
            Text     => "Input-cell set undefined");

         return Storage.Null_Cell_List;

      else

         return Cell_Sets.To_List (Item.Input_Cells);

      end if;

   end Input_Cells;


   function Number_Of_Input_Cells (Item : Bounds_Ref)
   return Natural
   is
   begin

      if not Item.Inputs_Defined then

         Output.Fault (
            Location => "Programs.Execution.Number_Of_Input_Cells",
            Locus    => Locus (Item),
            Text     => "Input-cell set undefined");

         return 0;

      else

         return Item.Input_Cell_Tally;

      end if;

   end Number_Of_Input_Cells;


   function Input_Cells (
      Call   : Call_T;
      Within : Bounds_Ref)
   return Storage.Cell_List_T
   is
   begin

      return Input_Cells (Call_Bounds (Call, Within));

   end Input_Cells;


   function Basis (Item : Bounds_Ref)
   return Storage.Cell_List_T
   is
   begin

      return Cell_Sets.To_List (Item.Basis);

   end Basis;


   function Initial (Item : Bounds_Ref)
   return Storage.Bounds.Cell_Interval_List_T
   is
   begin

      if Item.Initial /= null then

         return Item.Initial.all;

      else

         return Storage.Bounds.Empty;

      end if;

   end Initial;


   function Outputs_Defined (Item : Bounds_Ref)
   return Boolean
   is
   begin

      return Item.Outputs_Defined;

   end Outputs_Defined;


   function Output_Cells (Item : Bounds_Ref)
   return Storage.Cell_List_T
   is
   begin

      if not Item.Outputs_Defined then

         Output.Fault (
            Location => "Programs.Execution.Output_Cells",
            Text     => "Output-cell set undefined");

         return Storage.Null_Cell_List;

      else

         return Cell_Sets.To_List (Item.Output_Cells);

      end if;

   end Output_Cells;


   function Enough_For_Time (Item : Bounds_Ref) return Boolean
   is
   begin

      return Item.Enough_For_Time;

   end Enough_For_Time;


   function Time_State (Item : Bounds_Ref) return Time_State_T
   is
   begin

      if Defined (Item) then

         return Item.Time_State;

      else

         return Undefined;

      end if;

   end Time_State;


   function Time_Calculated (Within : Bounds_Ref) return Boolean
   is
   begin

      return Within.Time_Calculated;

   end Time_Calculated;


   function Time_Bounded (Item : Bounds_Ref) return Boolean
   is
   begin

      return Item.Time_State in Time_Bounded_T;

   end Time_Bounded;


   function Time_Asserted (Item : Bounds_Ref) return Boolean
   is
   begin

      return Time_State (Item) = Asserted;

   end Time_Asserted;


   function Time (Item : Bounds_Ref) return Processor.Time_T
   is
   begin

      if Time_Bounded (Item) then

         return Item.Time;

      else

         Output.Fault (
            Location => "Programs.Execution.Time",
            Text     =>
                 "Time for bounds #"
               & Bounds_Index_T'Image (Index (Item))
               & " is "
               & Time_State_T'Image (Item.Time_State));

         return 0;

      end if;

   end Time;


   procedure Prune_Flow (Item : in Bounds_Ref)
   is
   begin

      Check_Mutability (Item);

      Flow.Computation.Prune (Item.Computation);

      Mark_Flow_Pruned (Item);

   end Prune_Flow;


   procedure Mark_Flow_Pruned (Item : in Bounds_Ref)
   is
   begin

      Check_Mutability (Item);

   end Mark_Flow_Pruned;


   procedure Mark_Infeasible (
      Step   : in Flow.Step_T;
      Within : in Bounds_Ref)
   is
   begin

      Check_Mutability (Within);

      Flow.Computation.Mark_Infeasible (
         Step  => Step,
         Under => Within.Computation);

      Flow.Computation.Prune (Within.Computation);

   end Mark_Infeasible;


   procedure Mark_Infeasible (
      Edges  : in Flow.Edge_List_T;
      Within : in Bounds_Ref)
   is
   begin

      Check_Mutability (Within);

      Flow.Computation.Mark_Infeasible (
         Edges => Edges,
         Under => Within.Computation);

      Flow.Computation.Prune (Within.Computation);

   end Mark_Infeasible;


   procedure Mark_Infeasible (Bounds : in Bounds_Ref)
   is
   begin

      Mark_Infeasible (
         Step   => Flow.Entry_Step (Flow_Graph (Bounds)),
         Within => Bounds);

   end Mark_Infeasible;


   function Bounded (Bounds : Bounds_Ref)
   return Boolean
   is

      Tied : Boolean := True;
      -- The result. No problems yet.

   begin

      if Bounds.For_Time then
         -- Check that time or paths are bounded:

         Tied := Tied and then Time_State (Bounds) in Time_Boundable_T;

      end if;

      if Bounds.For_Space then
         -- Check that space is bounded:

         Tied := Tied and then Stack_Usage_Bounded (Bounds);

      end if;

      return Tied;

   end Bounded;


   function Bounded (
      Luup   : Loops.Loop_T;
      Within : Bounds_Ref)
   return Boolean
   is
      use Flow.Execution;
      -- The Bounded function.

      Index : constant Loops.Loop_Index_T := Loops.Loop_Index (Luup);
      -- Yes, this is the index of the Luup.

   begin

      return Bounded (Within.Loop_Bounds.Loop_Neck   (Index).Max)
          or Bounded (Within.Loop_Bounds.Loop_Repeats(Index).Max);

   end Bounded;


   function Loop_Start_Bound (
      Luup   : Loops.Loop_T;
      Within : Bounds_Ref)
   return Flow.Execution.Bound_T
   is
   begin

      return Within.Loop_Bounds.Loop_Starts(Loops.Loop_Index (Luup));

   end Loop_Start_Bound;


   function Loop_Neck_Bound (
      Luup   : Loops.Loop_T;
      Within : Bounds_Ref)
   return Flow.Execution.Bound_T
   is
   begin

      return Within.Loop_Bounds.Loop_Neck(Loops.Loop_Index (Luup));

   end Loop_Neck_Bound;


   function Loop_Repeat_Bound (
      Luup   : Loops.Loop_T;
      Within : Bounds_Ref)
   return Flow.Execution.Bound_T
   is
   begin

      return Within.Loop_Bounds.Loop_Repeats(Loops.Loop_Index (Luup));

   end Loop_Repeat_Bound;


   function Loop_Start_Bounds (Item : Bounds_Ref)
   return Loop_Bounds_T
   is
   begin

      return Item.Loop_Bounds.Loop_Starts;

   end Loop_Start_Bounds;


   function Loop_Neck_Bounds (Item : Bounds_Ref)
   return Loop_Bounds_T
   is
   begin

      return Item.Loop_Bounds.Loop_Neck;

   end Loop_Neck_Bounds;


   function Loop_Repeat_Bounds (Item : Bounds_Ref)
   return Loop_Bounds_T
   is
   begin

      return Item.Loop_Bounds.Loop_Repeats;

   end Loop_Repeat_Bounds;


   function Call_Input_Bounds (
      On     : Call_T;
      Within : Bounds_Ref)
   return Storage.Bounds.Bounds_Ref
   is
   begin

      return Within.Call_Inputs(Index (On));

   end Call_Input_Bounds;


   function Call_Bounds (
      On     : Call_T;
      Within : Bounds_Ref)
   return Bounds_Ref
   is
   begin

      -- Check for absence of bounds:

      if not Defined (Within) then

         Output.Fault (
            Location => "Programs.Execution.Call_Bounds",
            Text     => "Undefined call bounds.");

         return No_Bounds;

      end if;

      return Within.Call_Bounds(Index (On)).Bounds;

   end Call_Bounds;


   function Call_Bounds (Within : Bounds_Ref)
   return Call_Bounds_List_T
   is

      List : Call_Bounds_List_T (1 .. Within.Call_Bounds'Length);
      Last : Natural := 0;
      -- The result is List(1 .. Last).

   begin

      for C in Within.Call_Bounds'Range loop

         if Flow.Computation.Is_Feasible (
            Call  => Within.Call_Bounds(C).Call,
            Under => Within.Computation)
         then

            Last := Last + 1;

            List(Last) := Within.Call_Bounds(C);

            if List(Last).Bounds = No_Bounds then

               Output.Fault (
                  Location => "Programs.Execution.Call_Bounds (Bounds_Ref)",
                  Locus    => Locus (List(Last).Call),
                  Text     => "Null bounds on callee.");

               Output.Fault (
                  Location => "Programs.Execution.Call_Bounds (Bounds_Ref)",
                  Locus    => Locus (Within),
                  Text     =>
                       "Null bounds on callee within bounds #"
                     & Bounds_Index_T'Image (Index (Within))
                     & '.');

            end if;

         end if;

      end loop;

      return List(1 .. Last);

   end Call_Bounds;


   function Unbounded_Loops (
      Within  : Bounds_Ref;
      Eternal : Boolean)
   return Loops.Loop_List_T
   is

      use Flow.Execution;
      -- The Bounded function.

      Luups : constant Loops.Loop_List_T :=
         Flow.Computation.Loops_Of (Within.Computation);
      -- Those loops in the subprogram that are feasible and
      -- feasibly repeatable under this computation model.
      -- Note that Luups is not indexed by Loops.Loop_Index_T.

      Wild : Loops.Loop_List_T (1 .. Luups'Length);
      Last : Natural := 0;
      -- The feasible unbounded loops are collected in Wild (1 .. Last).

      Luup : Loops.Loop_T;
      -- One of the Luups.

      X : Loops.Loop_Index_T;
      -- The index of the Luup.

   begin

      for L in Luups'Range loop

         Luup := Luups(L);

         X := Loops.Index (Luup);

         if  not Bounded (Within.Loop_Bounds.Loop_Neck   (X).Max)
         and not Bounded (Within.Loop_Bounds.Loop_Repeats(X).Max)
         and (Eternal
              or else
              not Flow.Computation.Is_Eternal (Luup, Within.Computation))
         then
            -- This loop is feasible but not bounded, and also
            -- finite (not eternal) if eternal loops are excluded.

            Last       := Last + 1;
            Wild(Last) := Luup;

         end if;

      end loop;

      return Wild(1..Last);

   end Unbounded_Loops;


   function Dependent_Calls (
      Within : Bounds_Ref;
      Input  : Boolean)
   return Call_List_T
   --
   -- The Context_Dependent_Cells (when Input => False) or the
   -- Input_Dependent_Cells (when Input => True).
   --
   is

      Calls : constant Call_Bounds_List_T := Call_Bounds (Within);
      -- All the feasible calls with their current execution bounds.

      Time, Space : Boolean;
      -- Whether a given Call has context-dependent execution time
      -- or context-dependent stack space.

      Inp : Boolean := True;
      -- Whether a given Call satisfies the Input condition.
      -- Initialized for Input = False and overridden otherwise.

      Result : Call_List_T (1 .. Calls'Length);
      Last   : Natural := 0;
      -- The result will be Result(1 .. Last).

   begin

      for C in Calls'Range loop

         Time := Within.For_Time
            and then Time_State (Calls(C).Bounds) = Depends;

         Space := Within.For_Space
            and then Space_State (Calls(C).Bounds) = Depends;

         if Input then
            -- Accept only calls with some input cells:

            Inp := Calls(C).Bounds.Input_Cell_Tally > 0;

         end if;

         if (Time or Space) and Inp then

            Last         := Last + 1;
            Result(Last) := Calls(C).Call;

         end if;

      end loop;

      return Result(1 .. Last);

   end Dependent_Calls;


   function Context_Dependent_Calls (Within : Bounds_Ref)
   return Call_List_T
   is
   begin

      return Dependent_Calls (Within => Within, Input => False);

   end Context_Dependent_Calls;


   function Input_Dependent_Calls (Within : Bounds_Ref)
   return Call_List_T
   is
   begin

      return Dependent_Calls (Within => Within, Input => True);

   end Input_Dependent_Calls;


   function Unbounded_Calls (
      Within      : Bounds_Ref;
      Irreducible : Boolean)
   return Call_List_T
   is

      Calls : constant Call_Bounds_List_T := Call_Bounds (Within);
      -- All the feasible calls with their current execution bounds.

      Stacks : constant Stacks_T := Programs.Stacks (Program (Within));
      -- All the stacks in the program (for which a take-off height
      -- may still be unbounded).

      Result : Call_List_T (1 .. Calls'Length);
      Last   : Natural := 0;
      -- The result will be Result(1 .. Last).

      Candidate : Call_Bounds_T;
      -- A candidate from Calls.

      Cand_Callee : Subprogram_T;
      -- The callee of the Candidate.

      Interesting : Boolean;
      -- Whether the current call is interesting.

   begin

      for C in Calls'Range loop

         Candidate := Calls(C);

         Cand_Callee := Callee (Candidate.Call);

         Interesting := False;

         if  (not Stub (Cand_Callee))
         and (Irreducible or else Reducible (Cand_Callee))
         then
            -- This is a possibility.

            if not Bounded (Bounds => Candidate.Bounds) then

               Output.Note (
                  Locus => Locus (Candidate.Call),
                  Text  => "Call is interesting; not fully bounded.");

               Interesting := True;

            end if;

            if Within.For_Space then
               -- Check the take-off heights.

               for S in Stacks'Range loop

                  if not Take_Off_Height_Bounded (
                     Stack  => Stacks(S),
                     Before => Candidate.Call,
                     Within => Within)
                  then

                     Output.Note (
                        Locus => Locus (Candidate.Call),
                        Text  =>
                             "Call is interesting; take-off-height for "
                           & Name (Stacks (S))
                           & " is unbounded.");

                     Interesting := True;

                  end if;

               end loop;

            end if;

         end if;

         if Interesting then

            Last         := Last + 1;
            Result(Last) := Candidate.Call;

         end if;

      end loop;

      return Result(1 .. Last);

   end Unbounded_Calls;


   procedure Add_Caller_Cells (
      From : in     Flow.Calls.Parameter_Map_T;
      To   : in out Storage.Cell_Set_T)
   --
   -- Adds the caller-side cells From the given parameter map
   -- To the given cell-set.
   --
   is
   begin

      for F in From'Range loop

         Storage.Add (From(F).Caller, To);

      end loop;

   end Add_Caller_Cells;


   procedure Add_Inputs_For (
      Call   : in     Call_T;
      Within : in     Bounds_Ref;
      To     : in out Storage.Cell_Set_T)
   --
   -- Adds the caller-side input cells for the Call, Within the
   -- given caller-bounds, To the cell-set.
   --
   is

      Call_Mark : Output.Nest_Mark_T;
      -- Locus for the Call.

      Bounds : Bounds_Ref;
      -- The not fully bounded bounds on the Call.

      Protocol : Calling.Protocol_Ref;
      -- The calling protocol in use for the Call.

   begin

      Call_Mark := Output.Nest (Locus => Locus (Call));

      Protocol := Flow.Computation.Calling_Protocol (
         Call  => Call,
         Under => Within.Computation);

      Bounds := Call_Bounds (
         On     => Call,
         Within => Within);

      Add_Caller_Cells (
         From => Flow.Calls.Input_Parameters (
            Inputs   => Input_Cells (Bounds),
            Protocol => Protocol.all),
         To   => To);

      Output.Unnest (Call_Mark);

   exception

   when X : others =>

      Output.Exception_Info (
         Text       => "Programs.Execution.Add_Inputs_For",
         Occurrence => X);

      Output.Unnest (Call_Mark);

   end Add_Inputs_For;


   procedure Add_Inputs_For_Unbounded_Calls (
      Within : in     Bounds_Ref;
      To     : in out Storage.Cell_Set_T)
   is

      Calls : constant Call_List_T :=
         Unbounded_Calls (Within => Within, Irreducible => True);
      -- The unbounded calls.

   begin

      for C in Calls'Range loop

         Add_Inputs_For (
            Call   => Calls(C),
            Within => Within,
            To     => To);

      end loop;

   end Add_Inputs_For_Unbounded_Calls;


   function Inputs_For_Unbounded_Calls (Within : Bounds_Ref)
   return Storage.Cell_Set_T
   is

      Inputs : Cell_Set_T;
      -- To hold the input cells.
      -- Default initialized to the empty set.

   begin

      Add_Inputs_For_Unbounded_Calls (
         Within => Within,
         To     => Inputs);

      return Inputs;

   end Inputs_For_Unbounded_Calls;


   function Unbounded_Stack_Steps (Within : Bounds_Ref)
   return Flow.Step_List_T
   is

      Stacks : constant Stacks_T := Programs.Stacks (Program (Within));
      -- All the stacks.

      Result : Flow.Bounded_Step_List_T (
         Max_Length => Positive (
            Flow.Computation.Max_Step (Within.Computation)));
      -- To hold the result.

   begin

      if Within.For_Space then
         -- Stacks should be bounded and there are some stacks.

         for S in Stacks'Range loop

            if Stack_Usage_Bounded (Stacks(S), Within)
               -- The total stack usage is already bounded (by an
               -- assertion so we do not need to find bounds on
               -- the local stack height).

            or Stack_Height_Bounded (Stacks(S), Within) then
               -- We already have bounds on the local stack height
               -- (from some earlier analysis), so we can use these
               -- height-bonds to compute usage bounds.

               null;

            else
               -- Stack usage and height bounds not yet known for
               -- this stack, so we need to find bounds on the local
               -- height from which we can compute bounds on the usage.

               Flow.Add (
                  Steps => Flow.Computation.Steps_Defining (
                     Cell  => Height (Stacks(S)),
                     Under => Within.Computation),
                  To => Result);

            end if;

         end loop;

      end if;

      if not Final_Stack_Heights_Known (Within) then
         -- Some (Unstable) stack has no bounds on its final height.

         Flow.Add (
            Steps => Flow.Computation.Final_Steps (Within.Computation),
            To    => Result);

      end if;

      return Flow.To_List (Result);

   end Unbounded_Stack_Steps;


   function Calls_With_Unbounded_Take_Off (Item : Bounds_Ref)
   return Call_List_T
   is

      Stacks : constant Stacks_T := Programs.Stacks (Program (Item));
      -- All the stacks.

      Loose : Call_List_T (1 .. Item.Take_Off_Limits'Length (1));
      Last  : Natural := 0;
      -- Collects the unbounded Take_Off items in Loose(1 .. Last).

      Candidate : Call_T;
      -- One of the calls from the subprogram.

      All_Bounded : Boolean;
      -- Whether all take-off heights for the Candidate are bounded
      -- except possibly for stacks that already have bounded usage.

   begin

      for C in 1 .. Number_Of_Calls_From (Item.Subprogram) loop

         Candidate := Call (From => Item.Subprogram, Index => C);

         if Flow.Computation.Is_Feasible (Candidate, Item.Computation) then
            -- This call is feasible, so we check its take-off heights.

            All_Bounded := True;

            for S in Stacks'Range loop

               if not Stack_Usage_Bounded (Stacks(S), Item) then
                  -- Since the total usage of this stack is not yet
                  -- bounded (by an assertion, for example), we need
                  -- to compute it and thus need the take-off height.

                  All_Bounded := All_Bounded
                             and Bounded (Item.Take_Off_Limits(C,S));

               end if;

            end loop;

            if not All_Bounded then
               -- The take-off height is not bounded, for some stacks
               -- that have unbounded usage.

               Last        := Last + 1;
               Loose(Last) := Candidate;

            end if;

         end if;

      end loop;

      return Loose(1 .. Last);

   end Calls_With_Unbounded_Take_Off;


   function Flow_Bounds_Feasible (Item : in Bounds_Ref)
   return Boolean
   is
   begin

      return not Item.Void_Flow_Bounds;

   end Flow_Bounds_Feasible;


   function Node_Bounds (Item : in Bounds_Ref)
   return Node_Bounds_T
   is
   begin

      return Item.Node_Bounds;

   end Node_Bounds;


   function Edge_Times_Bounded (Item : in Bounds_Ref)
   return Boolean
   is
   begin

      return Defined (Item) and then Item.Step_Edge_Times /= null;

   end Edge_Times_Bounded;


   function Step_Edge_Times (Item : in Bounds_Ref)
   return Step_Edge_Times_T
   is

      Empty : Step_Edge_Times_T (1 .. 0);

   begin

      if not Edge_Times_Bounded (Item) then
         -- No times exist.

         Output.Fault (
            Location => "Programs.Execution.Edge_Times",
            Text     => "Edge times not defined.");

         return Empty;

      else

         return Item.Step_Edge_Times.all;

      end if;

   end Step_Edge_Times;


   function Time (Edge : Flow.Step_Edge_T; From : Bounds_Ref)
   return Processor.Time_T
   is
   begin

      if Edge_Times_Bounded (From) then

         return From.Step_Edge_Times(Flow.Index (Edge));

      else

         return 0;

      end if;

   end Time;


   function Edge_Times (Item : in Bounds_Ref)
   return Edge_Times_T
   is
   begin

      return Flow.Execution.Times.Edge_Times (
         Graph           => Flow_Graph (Subprogram (Item)),
         Step_Edge_Times => Step_Edge_Times (Item));

   end Edge_Times;


   function Time (Edge : Flow.Edge_T; From : Bounds_Ref)
   return Processor.Time_T
   is
   begin

      return Time (
         Edge => Flow.Step_Edge (Edge),
         From => From);

   end Time;


   function Node_Times_Bounded (Item : in Bounds_Ref)
   return Boolean
   is
   begin

      return Defined (Item) and then Item.Node_Times /= null;

   end Node_Times_Bounded;


   function Node_Times_With_Call_Times (From : Bounds_Ref)
   return Node_Times_T
   --
   -- The per-node execution times From the given bounds, with
   -- the time-bound of each callee included.
   --
   -- Preconditions:
   --    Defined (From).
   --    From.Node_Times defined (not null).
   --
   -- Emits a warning for any call that does not have a bound
   -- on execution time but is not Infeasible; uses a zero time
   -- for such calls.
   --
   is
      use type Processor.Time_T;

      Times : Node_Times_T (From.Node_Times'Range) :=
         From.Node_Times.all;
      -- The per-node times without call times.

      Calls : constant Call_Bounds_List_T := Call_Bounds (From);
      -- Bounds on the lower-level calls.

      Call_Node : Flow.Node_Index_T;
      -- A node that contains some lower-level call(s).

      Call_Bounds : Bounds_Ref;
      -- The bounds on the call.

   begin

      -- Add the time of lower-level calls to Times:

      for C in Calls'Range loop

         Call_Node := Flow.Index (Node (Calls(C).Call));

         Call_Bounds := Calls(C).Bounds;

         case Time_State (Call_Bounds) is

         when Time_Bounded_T =>

            Times(Call_Node) := Times(Call_Node) + Time (Call_Bounds);

         when Infeasible =>

            null;

         when others =>

            Output.Fault (
               Location => "Programs.Execution.Node_Times_With_Call_Times",
               Locus    => Locus (From),
               Text     =>
                    "Time is not bounded for "
                  & Image (Calls(C).Call));

         end case;

      end loop;

      return Times;

   end Node_Times_With_Call_Times;


   function Node_Times (
      From       : Bounds_Ref;
      With_Calls : Boolean)
   return Node_Times_T
   is

      Empty : Node_Times_T (1 .. 0);

   begin

      if not Node_Times_Bounded (From) then
         -- No times exist.

         Output.Note ("Node times not defined.");

         return Empty;

      elsif With_Calls then
         -- Include time of calls to lower-level subprograms:

         return Node_Times_With_Call_Times (From);

      else
         -- Just the local execution time:

         return From.Node_Times.all;

      end if;

   end Node_Times;


   function Call_Time (
      Node : Flow.Node_T;
      From : Bounds_Ref)
   return Processor.Time_T
   --
   -- The total worst-case execution time of all calls from the
   -- given Node, From the given bounds.
   --
   -- Emits a warning for any call that does not have a bound
   -- on execution time but is not Infeasible; uses a zero time
   -- for such calls.
   --
   is
      use type Flow.Node_T;
      use type Processor.Time_T;

      Graph : constant Flow.Graph_T := Flow_Graph (From.Subprogram);
      -- The graph of the subprogram in question (the caller).

      Calls : constant Call_Bounds_List_T := Call_Bounds (From);
      -- Bounds on the lower-level calls.

      Total : Processor.Time_T := 0;
      -- The total time of the calls in Node.

      Call_Node : Flow.Node_T;
      -- A node that contains some lower-level call.

      Call_Bounds : Bounds_Ref;
      -- The bounds on the call.

   begin

      -- Add the time of lower-level calls to Times:

      for C in Calls'Range loop

         Call_Node := Flow.Node_Containing (
            Step  => Step (Calls(C).Call),
            Graph => Graph);

         if Call_Node = Node then

            Call_Bounds := Calls(C).Bounds;

            case Time_State (Call_Bounds) is

            when Time_Bounded_T =>

               Total := Total + Time (Call_Bounds);

            when Infeasible =>

               null;

            when others =>

               Output.Fault (
                  Location => "Programs.Execution.Call_Time",
                  Locus    => Locus (From),
                  Text     =>
                       "Time is not bounded for "
                     & Image (Calls(C).Call));

            end case;

         end if;

      end loop;

      return Total;

   end Call_Time;


   function Time (
      Node       : Flow.Node_T;
      From       : Bounds_Ref;
      With_Calls : Boolean)
   return Processor.Time_T
   is
      use type Processor.Time_T;

      Time : Processor.Time_T := 0;
      -- The result.

   begin

      if Node_Times_Bounded (From) then

         Time := From.Node_Times(Flow.Index (Node));

         if With_Calls then

            Time := Time + Call_Time (Node, From);

         end if;

      end if;

      return Time;

   end Time;


   --
   ---   Execution counts of nodes, edges, etc
   --


   function Counts_Set (Item : in Bounds_Ref) return Boolean
   is
      use type Flow.Execution.Counts_Ref;
   begin

      return Defined (Item)
             and then Item.Flow_Counts /= null;

   end Counts_Set;


   function Counts (Item : in Bounds_Ref)
   return Flow_Counts_Ref
   is
   begin

      if Defined (Item) then
         return Item.Flow_Counts;
      else
         return null;
      end if;

   end Counts;


   function Count (Node : Flow.Node_T; Within : Bounds_Ref)
   return Flow.Execution.Count_T
   is
   begin

      if Counts_Set (Within) then

         return Within.Flow_Counts.Node(Flow.Index (Node));

      else

         return 0;

      end if;

   end Count;


   function Count (Edge : Flow.Edge_T; Within : Bounds_Ref)
   return Flow.Execution.Count_T
   is
   begin

      if Counts_Set (Within) then

         return Within.Flow_Counts.Edge(Flow.Index (Edge));

      else

         return 0;

      end if;

   end Count;


   function Total_Count (
      Nodes  : Flow.Node_List_T;
      Within : Bounds_Ref)
   return Flow.Execution.Count_T
   --
   -- The total execution count of the Nodes, in the worst-case
   -- execution path Within the bounds, or zero if no such path
   -- is set.
   --
   is
   begin

      if Counts_Set (Within) then

         return Flow.Execution.Total_Count (
            Nodes  => Nodes,
            Within => Within.Flow_Counts.all);

      else

         return 0;

      end if;

   end Total_Count;


   function Total_Count (
      Edges  : Flow.Edge_List_T;
      Within : Bounds_Ref)
   return Flow.Execution.Count_T
   --
   -- The total execution count of the Edges, in the worst-case
   -- execution path Within the bounds, or zero if no such path
   -- is set.
   --
   is
   begin

      if Counts_Set (Within) then

         return Flow.Execution.Total_Count (
            Edges  => Edges,
            Within => Within.Flow_Counts.all);

      else

         return 0;

      end if;

   end Total_Count;


   function Start_Count (
      Luup   : Loops.Loop_T;
      Within : Bounds_Ref)
   return Flow.Execution.Count_T
   is
   begin

      return Total_Count (
         Edges => Loops.Entry_Edges (
            Into   => Luup,
            Within => Flow_Graph (Subprogram (Within))),
         Within => Within);

   end Start_Count;


   function Head_Count (
      Luup   : Loops.Loop_T;
      Within : Bounds_Ref)
   return Flow.Execution.Count_T
   is
   begin

      return Total_Count (
         Nodes  => (1 => Loops.Head_Node (Luup)),
         Within => Within);

   end Head_Count;


   function Neck_Count (
      Luup   : Loops.Loop_T;
      Within : Bounds_Ref)
   return Flow.Execution.Count_T
   is
   begin

      return Total_Count (
         Edges => Loops.Neck_Edges (
            Into => Luup,
            Within => Flow_Graph (Subprogram (Within))),
         Within => Within);

   end Neck_Count;


   function Repeat_Count (
      Luup   : Loops.Loop_T;
      Within : Bounds_Ref)
   return Flow.Execution.Count_T
   is
   begin

      return Total_Count (
         Edges => Loops.Repeat_Edges (
            Repeating => Luup,
            Within    => Flow_Graph (Subprogram (Within))),
         Within => Within);

   end Repeat_Count;


   function Call_Count (
      Call   : Call_T;
      Within : Bounds_Ref)
   return Flow.Execution.Count_T
   is
   begin

      return Total_Count (
         Nodes  => (1 => Node (Call)),
         Within => Within);

   end Call_Count;


   function Call_Count (Link : Link_T)
   return Flow.Execution.Count_T
   is
   begin

      return Call_Count (
         Call   => Call (Link),
         Within => Caller_Bounds (Link));

   end Call_Count;


   --
   ---   The nodes, edges etc that are executed on the worst-case path
   --


   function Executed_Edges (Within : Bounds_Ref)
   return Flow.Edge_List_T
   is

      Graph : constant Flow.Graph_T := Flow_Graph (Within);
      -- The underlying flow-graph.

      Max_Edge : constant Flow.Edge_Count_T := Flow.Max_Edge (Graph);
      -- The edges are indexed 1 .. Max_Edge.

      List : Flow.Edge_List_T (1 .. Natural (Max_Edge));
      Last : Natural := 0;
      -- The result will be List(1 .. Last).

      Edge : Flow.Edge_T;
      -- The edge under consideration.

   begin

      for E in 1 .. Max_Edge loop

         Edge := Flow.Edge_At (Index => E, Within => Graph);

         if Count (Edge, Within) > 0 then

            Last := Last + 1;
            List(Last) := Edge;

         end if;

      end loop;

      return List(1 .. Last);

   end Executed_Edges;


   function Executed_Call_Bounds (Within : Bounds_Ref)
   return Call_Bounds_List_T
   is

      List : Call_Bounds_List_T (1 .. Within.Call_Bounds'Length);
      Last : Natural := 0;
      -- The result is List(1 .. Last).

   begin

      for C in Within.Call_Bounds'Range loop

         if Call_Count (Within.Call_Bounds(C).Call, Within) > 0 then

            Last := Last + 1;

            List(Last) := Within.Call_Bounds(C);

         end if;

      end loop;

      return List(1 .. Last);

   end Executed_Call_Bounds;


   --
   ---   Total execution time for (sets of) nodes, edges, loops, calls
   --


   function Total_Time (
      Nodes      : Flow.Node_List_T;
      Within     : Bounds_Ref;
      With_Calls : Boolean)
   return Processor.Time_T
   is
      use type Processor.Time_T;

      Sum : Processor.Time_T := 0;
      -- The total to be.

   begin

      if  Counts_Set (Within)
      and Node_Times_Bounded (Within)
      then

         for N in Nodes'Range loop

            Sum := Sum
               + Processor.Time_T (Count (Nodes(N), Within))
                                 * Time  (Nodes(N), Within, With_Calls);

         end loop;

      end if;

      return Sum;

   end Total_Time;


   function Total_Time (
      Edges  : Flow.Edge_List_T;
      Within : Bounds_Ref)
   return Processor.Time_T
   is
      use type Processor.Time_T;

      Sum : Processor.Time_T := 0;
      -- The total to be.

   begin

      if  Counts_Set (Within)
      and Edge_Times_Bounded (Within)
      then

         for E in Edges'Range loop

            Sum := Sum
               + Processor.Time_T (Count (Edges(E), Within))
                                 * Time  (Edges(E), Within);

         end loop;

      end if;

      return Sum;

   end Total_Time;


   function Total_Edge_Time (Within : Bounds_Ref)
   return Time_Sum_T
   is
      use type Flow.Node_T;
      use type Processor.Time_T;

      Graph : constant Flow.Graph_T := Flow_Graph (Within);
      -- The underlying control-flow graph.

      Time : Step_Edge_Times_T renames Within.Step_Edge_Times.all;
      -- The execution time of each step edge.

      Count : Flow_Counts_T renames Within.Flow_Counts.all;
      -- The execution count of nodes and node edges.

      Total : Time_Sum_T := (
         Num => 0,
         Sum => 0,
         Min => Processor.Time_T'Last,
         Max => Processor.Time_T'First);
      -- The result, initially neutral for adding more.

      Edge : Flow.Step_Edge_T;
      -- A step edge under consideration.

      Source, Target : Flow.Node_T;
      -- The source and target nodes of the Edge.


      procedure Include (
         Count : in Flow.Execution.Count_T;
         Time  : in Processor.Time_T)
      --
      -- Includes in the Total result an edge with the given
      -- execution Count and Time, if Count > 0 and Time > 0.
      --
      is
      begin

         if Count > 0 and Time > 0 then
            -- This edge is on the worst-case path and contributes
            -- a nonzero amount to the execution time bound.

            Total.Num := Total.Num + 1;

            Total.Sum := Total.Sum + Count * Time;

            Total.Min := Processor.Time_T'Min (Total.Min, Time);

            Total.Max := Processor.Time_T'Max (Total.Max, Time);

         end if;

      end Include;


   begin  -- Total_Edge_Time

      -- First, we scan all the step-edges, find the step-edges that
      -- are internal to some node, and add up the execution time of
      -- these edges, multiplying by the execution count of the node
      -- in question.
      --
      -- Second, we scan all the node-edges and add up their execution
      -- times, multiplying by the execution count of the edge itself.

      -- Add up the intra-node (step-) edges:

      for E in Time'Range loop

         if Time(E) > 0 then
            -- This edge may contribute to the Sum.

            Edge := Flow.Edge_At (Index => E, Within => Graph);

            Source := Flow.Node_Containing (Flow.Source (Edge), Graph);

            Target := Flow.Node_Containing (Flow.Target (Edge), Graph);

            if Source = Target then
               -- An intra-node edge.

               Include (
                  Count => Count.Node(Flow.Index (Source)),
                  Time  => Time(E));

            end if;

         end if;

      end loop;

      -- Add up the inter-node (node-) edges:

      for E in Count.Edge'Range loop
         -- E is the index of a node-edge.

         if Count.Edge(E) > 0 then
            -- This edge may contribute to the Sum.

            Edge := Flow.Step_Edge (Flow.Edge_At (E, Graph));
            -- The step-edge corresponding to node-edge E.

            Include (
               Count => Count.Edge(E),
               Time  => Time(Flow.Index (Edge)));

         end if;

      end loop;

      if Total.Num = 0 then
         -- No edges contribute to the worst-case execution time bound.

         Total.Min := 0;
         Total.Max := 0;

      end if;

      return Total;

   end Total_Edge_Time;


   function Total_Time (
      Luup       : Loops.Loop_T;
      Within     : Bounds_Ref;
      With_Calls : Boolean)
   return Processor.Time_T
   is
      use type Flow.Edge_List_T;
      use type Processor.Time_T;

      Graph : Flow.Graph_T;
      -- The flow-graph of the subprogram that contains the loop.

   begin

      Graph := Flow_Graph (Subprogram (Within));
      -- Assumes Defined (Within).

      return

           Total_Time (
              Nodes => Flow.To_List (
                 Set  => Loops.Members (Luup).all,
                 From => Graph),
              Within     => Within,
              With_Calls => With_Calls)

         + Total_Time (
              Edges =>
                   Loops.Internal_Edges (Luup, Graph)
                 & Loops.Repeat_Edges   (Luup, Graph),
              Within => Within);

   end Total_Time;


   function Total_Time (
      Call   : Call_T;
      Within : Bounds_Ref)
   return Processor.Time_T
   is
   begin

      return Total_Time (
         Nodes      => (1 => Node (Call)),
         Within     => Within,
         With_Calls => True);

   end Total_Time;


   function Total_Time (Link : Link_T)
   return Processor.Time_T
   is
   begin

      return Total_Time (
         Call   => Call (Link),
         Within => Caller_Bounds (Link));

   end Total_Time;


   function Total_Time (
      Calls  : Call_Bounds_List_T;
      Within : Bounds_Ref)
   return Processor.Time_T
   is
      use type Processor.Time_T;

      Total : Processor.Time_T := 0;
      -- The sum total.

      CB : Call_Bounds_T;
      -- One of the Calls.

   begin

      for C in Calls'Range loop

         CB := Calls(C);

         if Time_Bounded (CB.Bounds) then

            Total := Total + Call_Count (CB.Call, Within) * Time (CB.Bounds);

         end if;

      end loop;

      return Total;

   end Total_Time;


   function Callee_Time (Within : Bounds_Ref)
   return Processor.Time_T
   is
   begin

      return Total_Time (
         Calls  => Within.Call_Bounds,
         Within => Within);

   end Callee_Time;


   --
   --   Final stack height bounds
   --


   function Final_Stack_Height_Known (
      Stack  : Stack_T;
      Within : Bounds_Ref)
   return Boolean
   is
   begin

      return (not Returns (Within))
         or else Singular (Within.Final_Stack_Height(Index (Stack)));

   end Final_Stack_Height_Known;


   function Final_Stack_Heights_Known (Within : Bounds_Ref)
   return Boolean
   is
   begin

      if Returns (Within) then

         for S in Within.Final_Stack_Height'Range loop

            if not Singular (Within.Final_Stack_Height(S)) then

               return False;

            end if;

         end loop;

      end if;

      return True;

   end Final_Stack_Heights_Known;


   function Final_Stack_Height (
      Stack  : Stack_T;
      Within : Bounds_Ref)
   return Final_Stack_Height_T
   is
   begin

      return Within.Final_Stack_Height (Index (Stack));

   end Final_Stack_Height;


   function Loose_Final_Stack_Heights (Within : Bounds_Ref)
   return Storage.Cell_List_T
   is

      Stacks : constant Stacks_T := Programs.Stacks (Program (Within));
      -- All the stacks in this program.

      Result : Storage.Cell_List_T (1 .. Stacks'Length);
      Last   : Natural := 0;
      -- The result will be Result(1 .. Last).

      Stack : Stack_T;
      -- One of the Stacks.

   begin

      for S in Stacks'Range loop

         Stack := Stacks(S);

         if not Final_Stack_Height_Known (Stack, Within) then

            Last := Last + 1;
            Result(Last) := Height (Stack);

         end if;

      end loop;

      return Result(1 .. Last);

   end Loose_Final_Stack_Heights;


   function Final_Stack_Effect (From : Bounds_Ref)
   return Arithmetic.Effect_T
   is
      use Arithmetic;
      use type Arithmetic.Value_T;

      Stacks : constant Stacks_T := Programs.Stacks (Program (From));
      -- All the stacks in the program.

      Effect : Assignment_Set_T (Max_Size => 2 * Stacks'Length);
      -- The assignments to the local-stack-height and stack-pointer
      -- cells. Note that some such cells may not be assigned at all.

      Final : Final_Stack_Height_T;
      -- The bounds on the final stack height for one stack.

      Final_Value : Arithmetic.Value_T;
      -- The final stack height, if bounded to a single value.

      Height_Var : Variable_T;
      -- The variable that holds the local height of the stack.

      Pointer_Var : Variable_T;
      -- The variable that is the pointer for the stack,
      -- or Unknown if the stack has no (known) pointer.

      Set_Height : Assignment_T;
      -- The assignment that updates the Height_Var.

   begin

      for S in Stacks'Range loop

         Final := From.Final_Stack_Height(S);

         Height_Var  := Height  (Stacks(S));
         Pointer_Var := Pointer (Stacks(S));

         if Singular (Final) then
            -- A single known value (change in stack height).

            Final_Value := Single_Value (Final);

            if Final_Value /= 0 then
               -- The stack height is changed.

               Set_Height := Set (
                  Target => Height_Var,
                  Value  => Height_Var
                          + Const (
                               Value  => Final_Value,
                               Width  => Width_Of (Stacks(S)),
                               Signed => True));

               Add (Effect, Set_Height);

               if Pointer_Var /= Unknown then
                  -- The stack pointer changes too.

                  Algebra.Add_Coupled_Update (
                     Absolute => Set_Height,
                     Relative => Pointer_Var,
                     Coupling => Coupling (Stacks(S)),
                     To       => Effect);
                  --
                  -- The roles of "absolute" and "relative" are a
                  -- little inverted here, but never mind, it works.

               end if;

            else
               -- The Height_Var is not changed, but we add the silly
               -- assignment Height_Var := Height_Var in order to shield
               -- Height_Var from being veiled in the operation
               -- Flow.Calls.Add_Unknown_Call_Effect.
               -- TBM to a neater solution.

               Add (Effect, Set (Height_Var, Height_Var));

               if Pointer_Var /= Unknown then

                  Add (Effect, Set (Pointer_Var, Pointer_Var));

               end if;

            end if;

         else
            -- A range of values, perhaps effectively unbounded.

            Add (Effect, Set (Target => Height_Var));

            if Pointer_Var /= Unknown then

               Add (Effect, Set (Target => Pointer_Var));

            end if;

            Output.Warning (
                 "Final effect on stack "
               & Name (Stacks(S))
               & " not exactly known"
               & Output.Field_Separator
               & Image (
                    Item => Final,
                    Name => Image (Height_Var)));

         end if;

      end loop;

      return To_Effect (Effect);

   end Final_Stack_Effect;


   --
   ---   Stack limits
   --


   function Unbounded return Stack_Limit_T
   is
   begin

     return Unlimited;

   end Unbounded;


   function Max (Left, Right : Stack_Limit_T)
   return Stack_Limit_T
   is
   begin

      return Left or Right;

   end Max;


   function Max (Left : Stack_Limit_T; Right : Arithmetic.Value_T)
   return Stack_Limit_T
   is
   begin

      return Left or Singleton (Right);

   end Max;


   function Value_Image (Item : Stack_Usage_T) return String
   is
   begin

      case Item.State is
      when Undefined       => return "undefined";
      when Vague           => return "parts not bounded";
      when Depends         => return "context dependent";
      when Space_Bounded_T => return Arithmetic.Image (Item.Height);
      when Infeasible      => return "infeasible";
      when Unbounded       => return "+inf";
      end case;

   end Value_Image;


   function Brief_Image (Item : Stack_Usage_T) return String
   is
   begin

      case Item.State is
      when Undefined       => return "??";
      when Vague           => return "?";
      when Depends         => return "ctxt";
      when Space_Bounded_T => return Arithmetic.Image (Item.Height);
      when Infeasible      => return "-";
      when Unbounded       => return "inf";
      end case;

   end Brief_Image;


   function Total_Usage (
      Call     : Call_T;
      Take_Off : Stack_Limit_T;
      Callee   : Stack_Usage_T)
   return Stack_Usage_T
   is
      use type Arithmetic.Value_T;
   begin

      case Callee.State is

      when Space_Bounded_T =>

         if Bounded (Take_Off) then
            -- A bounded take-off plus a bounded callee, so
            -- we have a Computed total usage.

            return (
               State  => Computed,
               Height => Max (Take_Off) + Callee.Height,
               Call   => Call);

         else
            -- The take-off is not yet bounded, but we hope that
            -- it Depends on context and can be bounded later.

            return (
               State  => Depends,
               Height => 0,
               Call   => Call);

         end if;

      when Vague | Depends | Infeasible | Unbounded =>

         return (
            State  => Callee.State,
            Height => 0,
            Call   => Call);

      when Undefined =>

         Output.Fault (
            Location => "Programs.Execution.Total_Usage",
            Locus    => Locus (Call),
            Text     => "Callee bounds Undefined.");

         return (
            State  => Undefined,
            Height => 0,
            Call   => Call);

      end case;

   end Total_Usage;


   subtype Space_Fuzzy_T is Space_State_T range Undefined .. Depends;
   --
   -- A space-state that is not (yet) well defined.


   function Max (Left, Right : Stack_Usage_T)
   return Stack_Usage_T
   is
      use Storage;
      use type Arithmetic.Value_T;

      Result : Stack_Usage_T;

   begin

      if  Left.State  in Space_Bounded_T
      and Right.State in Space_Bounded_T
      then
         -- We can compare Left and Right bounds quantitatively.

         if Left.Height >= Right.Height then

            Result := (
               State  => Computed,
               Height => Left.Height,
               Call   => Left.Call);

         else

            Result := (
               State  => Computed,
               Height => Right.Height,
               Call   => Right.Call);

         end if;

      elsif Left.State  = Unbounded
         or Right.State = Unbounded
      then

         Result := (
            State  => Unbounded,
            Height => 0,
            Call   => No_Call);

      elsif Left.State  in Space_Fuzzy_T
         or Right.State in Space_Fuzzy_T
      then

         Result := (
            State  => Space_State_T'Min (Left.State, Right.State),
            Height => 0,
            Call   => No_Call);

      elsif Left.State = Infeasible then
         -- The Left code cannot be executed, so it has no effect
         -- on the overall stack usage.

         Result := Right;

      elsif Right.State = Infeasible then
         -- The Right code cannot be executed, so it has no effect
         -- on the overall stack usage.

         Result := Left;

      else

         Output.Fault (
            Location => "Programs.Execution.Max (Stack_Usage_T)",
            Text     => "Unhandled combination of states.");

         Result := (
            State  => Undefined,
            Height => 0,
            Call   => No_Call);

      end if;

      return Result;

   end Max;


   function Bounded (Item : Stack_Limit_T) return Boolean
   is
   begin

      return Storage.Bounds.Known (Item.Max);

   end Bounded;


   function Exact (Item : Stack_Limit_T) return Boolean
   is
   begin

      return Singular (Item);

   end Exact;


   function Bounded (Item : Stack_Usage_T) return Boolean
   is
   begin

      return Item.State in Space_Bounded_T;

   end Bounded;


   function Space_Bounded (Item : in Bounds_Ref)
   return Boolean
   is
   begin

      return Stack_Usage_Bounded (Item);

   end Space_Bounded;


   function Stack_Height_Bounded (
      Stack  : Stack_T;
      Within : in Bounds_Ref)
   return Boolean
   is
   begin

      if not Defined (Within) then
         -- Huh. Should not happen.

         Output.Fault (
            Location => "Programs.Execution.Stack_Height_Bounded",
            Text     => "Null bounds!");

         return False;

      end if;

      return Bounded (Within.Stack_Height(Index (Stack)));

   end Stack_Height_Bounded;


   function Stack_Height (
      Stack  : Stack_T;
      Within : Bounds_Ref)
   return Stack_Limit_T
   is
   begin

      return Within.Stack_Height(Index (Stack));

   end Stack_Height;


   function Stack_Usage_Bounded (
      Stack  : Stack_T;
      Within : Bounds_Ref)
   return Boolean
   is
   begin

      return Bounded (Within.Max_Stack_Usage(Index (Stack)));

   end Stack_Usage_Bounded;


   function Stack_Usage_Bounded (Item : in Bounds_Ref)
   return Boolean
   is

      All_Bounded : Boolean := True;
      -- Whether all stack usage is bounded.
      -- We ain't seen anything to the contrary yet.

   begin

      if not Defined (Item) then
         -- Huh. Should not happen.

         Output.Fault (
            Location => "Programs.Execution.Stack_Usage_Bounded",
            Text     => "Null bounds!");

         return False;

      end if;

      for S in 1 .. Item.Num_Stacks loop

         All_Bounded := All_Bounded
            and Bounded (Item.Max_Stack_Usage(S));

      end loop;

      return All_Bounded;

   end Stack_Usage_Bounded;


   function Stack_Usage (
      Stack  : Stack_T;
      Within : Bounds_Ref)
   return Stack_Usage_T
   is
   begin

      return Within.Max_Stack_Usage(Index (Stack));

   end Stack_Usage;


   function Space_State (
      Stack  : Stack_T;
      Within : Bounds_Ref)
   return Space_State_T
   is
   begin

      return Within.Max_Stack_Usage(Index (Stack)).State;

   end Space_State;


   function Space_State (Within : Bounds_Ref)
   return Space_State_T
   is

      Any : array (Space_State_T) of Boolean := (others => False);
      -- Whether any stack is a given state.

   begin

      -- Collect all states:

      for M in Within.Max_Stack_Usage'Range loop

         Any(Within.Max_Stack_Usage(M).State) := True;

      end loop;

      -- Check in order:

      if    Any(Infeasible) then return Infeasible;
      elsif Any(Depends   ) then return Depends   ;
      elsif Any(Vague     ) then return Vague     ;
      elsif Any(Undefined ) then return Undefined ;
      elsif Any(Unbounded ) then return Unbounded ;
      elsif Any(Computed  ) then return Computed  ;
      else                       return Asserted  ;
      end if;

   end Space_State;


   function Loose_Stack_Heights (Within : Bounds_Ref)
   return Storage.Cell_List_T
   is

      Stacks : constant Stacks_T := Programs.Stacks (Program (Within));
      -- All the stacks in this program.

      Result : Storage.Cell_List_T (1 .. Stacks'Length);
      Last   : Natural := 0;
      -- The result will be Result(1 .. Last).

      Stack : Stack_T;
      -- One of the Stacks.

   begin

      for S in Stacks'Range loop

         Stack := Stacks(S);

         if not Stack_Usage_Bounded (Stack, Within) then

            Last := Last + 1;
            Result(Last) := Height (Stack);

         end if;

      end loop;

      return Result(1 .. Last);

   end Loose_Stack_Heights;


   function Take_Off_Height_Bounded (
      Stack  : Stack_T;
      Before : Call_T;
      Within : Bounds_Ref)
   return Boolean
   is
   begin

      return Bounded (Take_Off_Height (Stack, Before, Within));

   end Take_Off_Height_Bounded;


   function Take_Off_Height (
      Stack  : Stack_T;
      Before : Call_T;
      Within : Bounds_Ref)
   return Stack_Limit_T
   is
   begin

      if not Defined (Within) then

         Output.Fault (
            Location => "Programs.Execution.Take_Off_Height",
            Text     => "Undefined bounds");

         return Unlimited;

      end if;

      return Within.Take_Off_Limits(Index (Before), Index (Stack));

   end Take_Off_Height;


   function Max_Image (Item : Stack_Limit_T) return String
   is
      use Storage.Bounds;

      Max_Limit : constant Limit_T := Item.Max;
      -- The upper bound.

   begin

      if Known (Max_Limit) then

         return Arithmetic.Image (Value (Max_Limit));

      else

         return "+inf";

      end if;

   end Max_Image;


   --
   ---   Bounds_Subset_T operations
   --


   procedure Erase (Subset : in out Bounds_Subset_T)
   is
   begin

      Subset.Members := (others => False);

   end Erase;


   function Is_Member (
      Index  : Bounds_Index_T;
      Of_Set : Bounds_Subset_T)
   return Boolean
   is
   begin

      return Index in Of_Set.Members'Range and then Of_Set.Members(Index);

   end Is_Member;


   function Is_Member (
      Item   : Bounds_Ref;
      Of_Set : Bounds_Subset_T)
   return Boolean
   is
   begin

      return Is_Member (Index(Item), Of_Set);

   end Is_Member;


   procedure Add (
      Item : in     Bounds_Ref;
      To   : in out Bounds_Subset_T)
   is
   begin

      if Index (Item) in To.Members'Range then

         To.Members(Index (Item)) := True;

      else

         Output.Fault (
            Location => "Programs.Execution.Add (Bounds to Subset)",
            Text =>
                 "Bounds #"
               & Bounds_Index_T'Image (Index (Item))
               & " cannot be added, max subset size is"
               & Bounds_Count_T'Image (To.Max_Size));

      end if;

   end Add;


end Programs.Execution;
