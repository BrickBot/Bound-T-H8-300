-- Assertions (decl)
--
-- Assertions about the structure and executions of the program
-- under analysis. These assertions are usually written by the
-- user of Bound-T (the analyst).
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
-- $Revision: 1.43 $
-- $Date: 2015/10/24 19:36:47 $
--
-- $Log: assertions.ads,v $
-- Revision 1.43  2015/10/24 19:36:47  niklas
-- Moved to free licence.
--
-- Revision 1.42  2011-08-31 04:23:33  niklas
-- BT-CH-0222: Option registry. Option -dump. External help files.
--
-- Revision 1.41  2009-12-17 14:05:54  niklas
-- BT-CH-0197: Assertions on instruction roles.
--
-- Revision 1.40  2009-03-27 13:57:13  niklas
-- BT-CH-0167: Assertion context identified by source-code markers.
--
-- Revision 1.39  2009/03/24 07:48:35  niklas
-- BT-CH-0166: String_Pool.Item_T for source-file and marker names.
--
-- Revision 1.38  2009/03/21 13:09:17  niklas
-- BT-CH-0165: Option -file_match for matching asserted file-names.
--
-- Revision 1.37  2009/03/20 18:19:29  niklas
-- BT-CH-0164: Assertion context identified by source-line number.
--
-- Revision 1.36  2009/01/18 07:51:05  niklas
-- Cleaned up context clauses.
--
-- Revision 1.35  2008/09/24 08:38:51  niklas
-- BT-CH-0146: Assertions on "loop starts <bound> times".
-- BT-CH-0146: Loop-repeat assertions set both lower and upper bound.
-- BT-CH-0146: Report locations of contradictory "count" assertions.
-- BT-CH-0146: Contradictory "count" assertions imply infeasibility.
--
-- Revision 1.34  2008/09/20 12:41:50  niklas
-- BT-CH-0145: No error re too few assertion matches if graph is growing.
--
-- Revision 1.33  2008/07/28 19:23:45  niklas
-- BT-CH-0140: Detect contradictory execution-count bounds.
--
-- Revision 1.32  2008/07/23 09:07:15  niklas
-- BT-CH-0139: Fix recursion in Programs.Execution.Paths.
--
-- Revision 1.31  2008/07/14 19:16:55  niklas
-- BT-CH-0135: Assertions on "instructions".
--
-- Revision 1.30  2008/02/27 14:58:47  niklas
-- BT-CH-0116: Call-specific time and stack assertions.
--
-- Revision 1.29  2007/12/17 13:54:34  niklas
-- BT-CH-0098: Assertions on stack usage and final stack height, etc.
--
-- Revision 1.28  2007/01/25 21:25:13  niklas
-- BT-CH-0043.
--
-- Revision 1.27  2006/11/26 22:07:24  niklas
-- BT-CH-0039.
--
-- Revision 1.26  2006/05/29 11:22:33  niklas
-- BT-CH-0023.
--
-- Revision 1.25  2006/05/27 21:45:43  niklas
-- BT-CH-0020.
--
-- Revision 1.24  2005/02/23 09:05:13  niklas
-- BT-CH-0005.
--
-- Revision 1.23  2005/02/16 21:11:38  niklas
-- BT-CH-0002.
--
-- Revision 1.22  2005/02/04 20:59:45  niklas
-- Added the procedure Apply_Options to enter subprogram options
-- from the assertion set into the Programs data structures.
-- At present this is used only for the "[no] return" option.
--
-- Revision 1.21  2004/05/01 14:57:20  niklas
-- First Tidorum version.
-- Taking Cell_T stuff from Storage, not from Arithmetic.
-- Defined Assertion_Set_T to have reference semantics. Thus, parameters
-- of this type are now always of mode "in" even if the underlying object
-- is updated.
-- Defined Assertion_Map_T objects to have default initial value No_Map.
-- Added Symbol_Table parameter to Check_Mapping. Added Display_Properties
-- to describe the properties of loops when loop-mapping fails.
-- Added support for variables that are held in different cell(s)
-- depending on the code address. Many functions of the form Xxx_Values
-- now have two variants: one taking an assertion set and returning a
-- Var_Bound_List_T, and another taking in addition a Code_Address_T and
-- returning a Cell_Bound_List_T valid at that address. If the program
-- point is implicitly known (eg. Call_Values, Subprogram_Inputs) the
-- second variant takes only the assertion set and no program point.
-- Added assertions on subprogram inputs, valid only on entry to the
-- subprogram, retrieved with the function Subprogram_Inputs (variants
-- as explained above).
-- Using the new package Loops.Cells in the implementation. This supports
-- variables that change location depending on code address.
--
-- Revision 1.20  2003/02/17 16:28:40  holsti
-- Removed unnecessary context clauses.
--
-- Revision 1.19  2001/03/10 00:28:33  holsti
-- Unused Program_T parameters removed.
--
-- Revision 1.18  2001/02/15 14:16:58  ville
-- Analysis mode assertions enabled
--
-- Revision 1.17  2000/12/29 14:38:10  holsti
-- Loop_Invariants added.
--
-- Revision 1.16  2000/12/29 13:20:16  holsti
-- Removed tbas etc. in favour of NCs.
--
-- Revision 1.15  2000/12/28 17:49:38  holsti
-- No_Bound described.
--
-- Revision 1.14  2000/11/29 09:30:05  holsti
-- Property assertions explained.
-- Property_Table_T and Node_Property added.
--
-- Revision 1.13  2000/11/22 12:48:39  langback
-- Intermediate version.
--
-- Revision 1.12  2000/10/31 11:43:28  langback
-- First version after adding abstract syntax tree specific child
-- package. Implemented a number of "tba"s.
--
-- Revision 1.11  2000/10/12 11:12:45  langback
-- First committed version with access functions implemented.
--
-- Revision 1.10  2000/09/01 08:15:25  saarinen
-- Some fixes and other minor changes.
--
-- Revision 1.9  2000/08/18 18:07:32  holsti
-- Unbounded_Vectors Index_Type removed.
--
-- Revision 1.8  2000/08/04 15:00:24  langback
-- Minor changes to some types.
--
-- Revision 1.7  2000/07/25 03:14:50  holsti
-- Call_Count declared.
--
-- Revision 1.6  2000/07/24 22:29:16  holsti
-- Several operations declared (uncommented).
--
-- Revision 1.5  2000/06/13 11:09:57  langback
-- Minor bug fixes - Still a temporary version
--
-- Revision 1.4  2000/06/12 11:21:00  langback
-- Some types added.
--
-- Revision 1.3  2000/04/21 20:01:43  holsti
-- Describe usage if error in arguments.
--
-- Revision 1.2  2000/04/21 19:39:00  holsti
-- Renamed child Options to Opt
--


with Flow;
with Flow.Computation;
with Flow.Execution;
with Loops;
with Processor;
with Programs;
with Storage;
with Storage.Bounds;
with String_Pool;
with Symbols;


package Assertions is


   --
   ---   Assertion sets
   --


   type Assertion_Set_T is private;
   --
   -- A set of assertions, representing the logical conjunction
   -- of all the assertions in the set.
   --
   -- This type has reference semantics. Thus, it should be seen
   -- as a reference to an underlying assertion-set object. The
   -- underlying object can be updated through any such reference,
   -- and the changes will be visible also through any other
   -- reference to the same object.


   Input_Error : exception;
   --
   -- Signals an error in a file of textual assertions.
   -- See procedure Get below.


   procedure Get_Assertions (
      Program       : in     Programs.Program_T;
      Assertion_Set :    out Assertion_Set_T);
   --
   -- Reads and parses the assertions from the assertion files that
   -- are specified in some way (eg. by command-line options),
   -- as they apply to the Program under analysis, and constructs an
   -- assertion set that holds these assertions.
   --
   -- Propagates Input_Error if a syntax or semantic error is found
   -- in an input file.


   procedure Get_Marks;
   --
   -- Reads and parses the source-code mark definitions from the
   -- mark definition files that are specified in some way (eg. by
   -- command-line options) and constructs a mark set for use in
   -- matching assertions to marked program parts.
   --
   -- Propagates Input_Error if a syntax or semantic error is found
   -- in an input file.


   procedure Apply_Options (
      Assertion_Set : in Assertion_Set_T;
      Program       : in Programs.Program_T);
   --
   -- Enters various options, properties, or other facts asserted in
   -- the given assertion set, into the program objects to which the
   -- assertions apply, in the given Program. The options processed
   -- in this way include:
   --
   -- > Whether a subprogram returns to its caller.
   --
   -- > Whether a subprogram is "unused", meaning that no call
   --   to this subprogram is ever executed.
   --
   -- > The computational role to be assumed to the instruction
   --   at a given address.


   procedure Warn_About_Unused_Options (
      Assertion_Set : in Assertion_Set_T;
      Program       : in Programs.Program_T);
   --
   -- To be invoked at the end of the analysis, to check that
   -- all relevant options have been used by the analysis. This
   -- can reveal errors in the option assertions, for example
   -- incorrect offsets for identifying instructions with special
   -- roles.


   --
   ---   Applying assertion sets to various program parts under analysis
   --


   type Time_Bound_T is record
      Min : Processor.Time_T;
      Max : Processor.Time_T;
   end record;
   --
   -- Asserted bounds on the execution time of a subprogram
   -- or one call of a subprogram.
   --
   -- If no minimum time is asserted, Min is zero.
   -- If no maximum time is asserted, Max is Time_T'Last.


   No_Bound : constant Time_Bound_T := (
      Min => 0,
      Max => Processor.Time_T'Last);
   --
   -- An unbounded execution time (absence of bounds on time).


   type Stack_Bounds_T is
      array (Programs.Stack_Index_T range <>) of Storage.Bounds.Interval_T;
   --
   -- Bounds (possibly unlimited) on the usage or local height (depending
   -- on context) for all the stacks in a program.


   type Property_Table_T is
      array (Processor.Property_T) of Storage.Bounds.Interval_T;
   --
   -- Asserted (or defaulted) bounds for all processor-specific properties.
   -- This type is public for use by the target-specific function that
   -- returns the default property bounds. For each property, if there
   -- are no relevant assertions for this property, then the default
   -- bounds are used.


   function Global_Values (
      Asserts : Assertion_Set_T)
   return Storage.Bounds.Var_Interval_List_T;
   --
   -- The set of bounds on variables asserted globally for the
   -- execution of the program, that is, for any program part (any
   -- subprogram, loop, call etc.) and any calling context.


   function Global_Values (
      Asserts : Assertion_Set_T;
      Point   : Processor.Code_Address_T)
   return Storage.Bounds.Cell_Interval_List_T;
   --
   -- Bounds on cells at this Point in the target program, derived from
   -- the bounds on variables asserted globally for the execution of
   -- the program.


   function Subprogram_Inputs (
      Subprogram : Programs.Subprogram_T;
      Asserts    : Assertion_Set_T)
   return Storage.Bounds.Var_Interval_List_T;
   --
   -- Bounds asserted for Subprogram input variables (parameters,
   -- global input variables) valid at the point of entry for any call
   -- of the subprogram.


   function Subprogram_Inputs (
      Subprogram : Programs.Subprogram_T;
      Asserts    : Assertion_Set_T)
   return Storage.Bounds.Cell_Interval_List_T;
   --
   -- Bounds on cells at the entry point of a Subprogram, derived from the
   -- bounds asserted for subprogram input variables (parameters, global input
   -- variables) valid at the point of entry for any call of the subprogram.


   function Subprogram_Values (
      Subprogram : Programs.Subprogram_T;
      Asserts    : Assertion_Set_T)
   return Storage.Bounds.Var_Interval_List_T;
   --
   -- Bounds on variables that are asserted to hold throughout the
   -- Subprogram for any execution of the Subprogram.


   function Subprogram_Values (
      Subprogram : Programs.Subprogram_T;
      Point      : Processor.Code_Address_T;
      Asserts    : Assertion_Set_T)
   return Storage.Bounds.Cell_Interval_List_T;
   --
   -- Bounds on cells at this Point in a given Subprogram, derived from
   -- the bounds on variables asserted for any execution of the subprogram.


   function Subprogram_Invariants (
      Subprogram : Programs.Subprogram_T;
      Asserts    : Assertion_Set_T)
   return Storage.Cell_Set_T;
   --
   -- The set of cells asserted to be invariant in any execution
   -- of this Subprogram.


   function Subprogram_Time_Bounded (
      Subprogram : Programs.Subprogram_T;
      Asserts    : Assertion_Set_T)
   return Boolean;
   --
   -- Whether some finite bounds on the execution time of the given
   -- Subprogram are asserted.


   function Subprogram_Time (
      Subprogram : Programs.Subprogram_T;
      Asserts    : Assertion_Set_T)
   return Time_Bound_T;
   --
   -- Bounds on the execution time of the given Subprogram, asserted to
   -- hold for any call of this Subprogram. The upper bound can be
   -- infinite (unlimited).


   function Stacks_Bounded (
      Subprogram : Programs.Subprogram_T;
      Asserts    : Assertion_Set_T)
   return Boolean;
   --
   -- Whether some finite bounds on the stack usage for all stacks,
   -- and precise (singleton) values on the final stack height for
   -- all (Unstable) stacks, are asserted for the given Subprogram.
   -- For Stable stacks we of course do not require final-height
   -- assertions since the final height is known to be zero.


   function Stack_Usage (
      Subprogram : Programs.Subprogram_T;
      Asserts    : Assertion_Set_T)
   return Stack_Bounds_T;
   --
   -- Bounds on the overall usage of each stack, asserted to hold for
   -- any execution of the given Subprogram. The lower bound for any
   -- stack is usually non-negative. The upper bound can be infinite
   -- (unlimited). The default value (no assertion) has a lower bound
   -- from Programs.Initial_Stack_Height and an unlimited upper bound.


   function Final_Stack_Height (
      Subprogram : Programs.Subprogram_T;
      Asserts    : Assertion_Set_T)
   return Stack_Bounds_T;
   --
   -- Bounds on the final value of the local height of each stack,
   -- asserted to hold for any return from any execution of the given
   -- Subprogram. The upper bound for any stack is always non-negative.
   -- Either or both bounds (lower and/or upper) can be infinite
   -- (unlimited). The default value (no assertion) is unlimited for
   -- an Unstable stack and [0,0] for a Stable stack. (It makes no sense
   -- to assert the final stack height for a Stable stack.)


   function Subprogram_Enough_For_Time (
      Subprogram : Programs.Subprogram_T;
      Asserts    : Assertion_Set_T)
   return Boolean;
   --
   -- Whether this Subprogram should be assumed to have sufficient
   -- execution-count assertions on its parts for us to find bounds
   -- on its execution paths and thus on its execution time, even if
   -- the subprogram contains unbounded loops or is irreducible.


   function Program_Has_Property (
      Property : Processor.Property_T;
      Asserts  : Assertion_Set_T)
   return Boolean;
   --
   -- Whether bounds on the value of the given Property have been
   -- asserted to hold throughout the program under analysis (unless
   -- overridden by more local assertions).


   function Subprogram_Has_Property (
      Property   : Processor.Property_T;
      Subprogram : Programs.Subprogram_T;
      Asserts    : Assertion_Set_T)
   return Boolean;
   --
   -- Whether bounds on the value of the given Property have been
   -- asserted to hold throughout the Subprogram (unless overridden
   -- by more local assertions).


   No_Such_Property : exception;
   --
   -- Signals the absence of an asserted property value.
   -- See Program_Property and similar functions below.


   function Program_Property (
      Property : Processor.Property_T;
      Asserts  : Assertion_Set_T)
   return Storage.Bounds.Interval_T;
   --
   -- The bounds on the value of the given Property that are asserted
   -- to hold throughout the program under analysis (unless overridden
   -- by more local assertions).
   --
   -- Propagates No_Such_Property if there are no such assertions.


   function Subprogram_Property (
      Property   : Processor.Property_T;
      Subprogram : Programs.Subprogram_T;
      Asserts    : Assertion_Set_T)
   return Storage.Bounds.Interval_T;
   --
   -- The bounds on the value of the given Property that are asserted
   -- to hold throughout the given Subprogram (unless overridden by
   -- more local assertions).
   --
   -- Propagates No_Such_Property if there are no such assertions.


   function Callees (
      Call    : Flow.Dynamic_Edge_T;
      From    : Programs.Subprogram_T;
      Model   : Flow.Computation.Model_Handle_T;
      Asserts : Assertion_Set_T)
   return Programs.Subprogram_List_T;
   --
   -- The possible callee subprograms for the given dynamic Call
   -- From a given subprogram under a given computation Model,
   -- or under the primitive computation model if Model is null.


   function Instruction_Counts (
      Subprogram : Programs.Subprogram_T;
      Model      : Flow.Computation.Model_Handle_T;
      Asserts    : Assertion_Set_T)
   return Flow.Execution.Node_Count_Bounds_T;
   --
   -- The bounds on the execution count(s) of flow-graph node(s)
   -- within the Subprogram, deduced from assertions on the execution
   -- count of instructions within this Subprogram or globally.
   --
   -- The Model is used to match the given Subprogram against the
   -- features that the assertions require of the subprogram that is
   -- to contain the relevant instruction.
   --
   -- If some assertions are contradictory, a warning message is
   -- emitted that identifies the last contradictory assertion.


   function Instruction_Role (
      Address : Processor.Code_Address_T;
      Asserts : Assertion_Set_T)
   return Processor.Instruction_Role_T;
   --
   -- The role that the instruction at the given Address performs,
   -- within the computation of the target program, if a role is
   -- asserted for this instruction, otherwise No_Role.
   --
   -- If there are contradicting role assertions, a warning message
   -- is emitted that identifies the last contradictory assertion.
   --
   -- Note that this function includes all assertions on an instruction
   -- at this Address, both global assertions and assertions in the
   -- scope of a (any) subprogram.


   --
   ---   Mapping applicable assertions to subprogram parts
   --


   type Assertion_Map_T is private;
   --
   -- Maps the assertions from an assertion set to those parts of
   -- a particular subprogram (under analysis) to which the assertions
   -- apply. In other words, for each part of the given subprogram
   -- the assertion map shows the assertions that apply to this part.
   -- In particular, for assertions about processor-specific properties
   -- the assertion map shows the value of each property at each node
   -- in the flow-graph of the subprogram.
   --
   -- Contains within itself a reference to the related assertion
   -- set, to the subprogram for which the map was created, and to
   -- access handles to the symbolic information in the target
   -- program (mapping variable identifiers to cells).


   No_Map : constant Assertion_Map_T;
   --
   -- A special value that indicates the absence of a map.
   -- This is also the default initial value of any object of
   -- type Assertion_Map_T.


   procedure Identify_Assertions (
      Model         : in     Flow.Computation.Model_Handle_T;
      Assertion_Set : in     Assertion_Set_T;
      Assertion_Map :    out Assertion_Map_T);
   --
   -- Identifies the assertions from the Assertion_Set that apply to the
   -- several loops and calls in the subprogram that underlies the given
   -- computation Model and stores the results in an Assertion_Map.
   --
   -- Model
   --    The computation model and underlying subprogram that contains
   --    the loops and calls to be identified.
   -- Assertion_Set
   --    The set of all assertions. Some may apply to this subprogram,
   --    some to other subprograms. This is an "in" parameter but of
   --    reference type.
   -- Assertion_Map
   --    The mapping of assertions to the loops and calls within the
   --    Model.
   --
   -- TBA options to include or omit infeasible parts of the Model.


   procedure Verify_Map (
      Map   : in     Assertion_Map_T;
      Valid :    out Boolean);
   --
   -- Verifies that the assertion Map satisfies the requirements
   -- on the population of entities (calls, loops) to which each
   -- such assertion is mapped.
   --
   -- May also emit warnings or other reports when the Map has
   -- some dubious properties, for example when assertions are
   -- mapped to infeasible (unreachable) entities.


   function Set (Map : Assertion_Map_T) return Assertion_Set_T;
   --
   -- The assertion set related to the assertion Map.


   function Subprogram (Map : Assertion_Map_T)
   return Programs.Subprogram_T;
   --
   -- The subprogram to which the Map applies.


   function Stack_By (
      Index : Programs.Stack_Index_T;
      Map   : Assertion_Map_T)
   return Programs.Stack_T;
   --
   -- The stack identified by the given Index, within the program
   -- and subprogram to which the Map applies.


   --
   ---   Applying assertion maps to various program parts under analysis
   --


   function Loop_Values (
      Luup    : Loops.Loop_T;
      Asserts : Assertion_Map_T)
   return Storage.Bounds.Var_Interval_List_T;
   --
   -- Bounds on variables that are asserted to hold throughout the
   -- given Luup for any execution of the Luup.
   --
   -- The result does not include global bounds on variables nor bounds
   -- asserted to hold for the containing program elements (the subprogram
   -- that contains the loop or outer loops, if any).


   function Loop_Values (
      Luup    : Loops.Loop_T;
      Point   : Processor.Code_Address_T;
      Asserts : Assertion_Map_T)
   return Storage.Bounds.Cell_Interval_List_T;
   --
   -- Bounds on cells at this Point in a given Luup, derived from the bounds
   -- on variables asserted for any execution of the loop.
   --
   -- The result does not include global bounds on variables nor bounds
   -- asserted to hold for the containing program elements (the subprogram
   -- that contains the loop or outer loops, if any).


   function Loop_Nest_Values (
      Luups   : Loops.Loop_List_T;
      Point   : Processor.Code_Address_T;
      Asserts : Assertion_Map_T)
   return Storage.Bounds.Cell_Interval_List_T;
   --
   -- Bounds on cells at this Point (possibly) in some nested Luups,
   -- derived from bounds on the variables asserted for any execution
   -- of any of the containing Luups.
   --
   -- The result does not include global bounds on variables nor bounds
   -- asserted to hold for the subprogram in which the loops (if any)
   -- lie.
   --
   -- If there are no Luups the result is a null interval list.


   function Call_Values (
      Call    : Programs.Call_T;
      Asserts : Assertion_Map_T)
   return Storage.Bounds.Var_Interval_List_T;
   --
   -- Bounds on variables that are asserted to hold at the point of
   -- the Call (in the Callee) for any execution of the Call.
   --
   -- The result does not include global bounds on variables nor bounds
   -- asserted to hold for the containing program elements (the caller
   -- subprogram and loops that contain the call, if any).


   function Call_Values (
      Call    : Programs.Call_T;
      Asserts : Assertion_Map_T)
   return Storage.Bounds.Cell_Interval_List_T;
   --
   -- Bounds on cells at a given Call, derived from the bounds on variables
   -- asserted for any execution of this call. The mapping from variables to
   -- cells is applied at the prime address of the call step.
   --
   -- The result does not include global bounds on variables nor bounds
   -- asserted to hold for the containing program elements (the caller
   -- subprogram and loops that contain the call, if any).


   function Local_Call_Values (
      Call    : Programs.Call_T;
      Asserts : Assertion_Map_T)
   return Storage.Bounds.Cell_Interval_List_T;
   --
   -- Bounds on cells at a given Call, derived from the bounds on variables
   -- asserted for any execution of this call, for any execution of the
   -- loops (if any) that contain this call and for any execution of the
   -- subprogram (the caller) that contains this call. The bounds from
   -- each level are combined conjunctively (intersection of intervals).
   --
   -- The result does not include globally asserted variable bounds.
 

   function Loop_Invariants (
      Luup    : Loops.Loop_T;
      Asserts : Assertion_Map_T)
   return Storage.Cell_Set_T;
   --
   -- The set of cells asserted to be invariant in any execution of
   -- this Luup.


   function Call_Invariants (
      Call    : Programs.Call_T;
      Asserts : Assertion_Map_T)
   return Storage.Cell_Set_T;
   --
   -- The set of cells asserted to be invariant across any execution
   -- of this Call. The callee can change the cells as long as they
   -- are restored before returning from the Call.


   function Call_Invariants (
      Call    : Programs.Call_T;
      Asserts : Assertion_Map_T)
   return Storage.Cell_List_T;
   --
   -- The cells that are asserted to be invariant across any execution
   -- of the given call.


   function Loop_Start (
      Luup    : Loops.Loop_T;
      Asserts : Assertion_Map_T)
   return Flow.Execution.Bound_T;
   --
   -- The asserted bounds on the number of times the Luup is started
   -- (entered from outside the loop), per execution of the subprogram
   -- that contains the loop. Can be unbounded at either or both ends.


   function Loop_Count (
      Luup    : Loops.Loop_T;
      Asserts : Assertion_Map_T)
   return Flow.Execution.Bound_T;
   --
   -- The asserted bounds on the number of times the Luup is iterated,
   -- per entry to the loop. Can be unbounded at either or both ends.


   function Call_Count (
      Call    : Programs.Call_T;
      Asserts : Assertion_Map_T)
   return Flow.Execution.Bound_T;
   --
   -- The asserted bounds on the number of times the Call is executed
   -- per execution of the containing subprogram. Either end of the
   -- interval can be bounded or unbounded.


   function Call_Time (
      Call    : Programs.Call_T;
      Asserts : Assertion_Map_T)
   return Time_Bound_T;
   --
   -- The asserted bounds on the execution time of this Call of
   -- this subprogram. The upper bounds is unlimited if there
   -- are no such assertions.


   function Call_Stack_Usage_Asserted (
      Call    : Programs.Call_T;
      Asserts : Assertion_Map_T)
   return Boolean;
   --
   -- Whether there are some effective assertions on the usage of
   -- some stack, for any execution of this Call.


   function Call_Stack_Usage (
      Call    : Programs.Call_T;
      Asserts : Assertion_Map_T)
   return Stack_Bounds_T;
   --
   -- Bounds on the overall usage of each stack, asserted to hold for
   -- any execution of this Call of this subprogram. The lower bound
   -- for any stack is always non-negative. The upper bound can be
   -- infinite (unlimited). The default value (no assertion) has a
   -- lower bound from Programs.Initial_Stack_Height and an unlimited
   -- upper bound.


   function Call_Final_Stack_Height_Asserted (
      Call    : Programs.Call_T;
      Asserts : Assertion_Map_T)
   return Boolean;
   --
   -- Whether there are some effective assertions on the final height
   -- of some unstable stack, for any execution of this Call.
   -- Assertions on the final height of stable stacks are ignored
   -- as ineffective.


   function Call_Final_Stack_Height (
      Call    : Programs.Call_T;
      Asserts : Assertion_Map_T)
   return Stack_Bounds_T;
   --
   -- Bounds on the final value of the local height of each stack,
   -- asserted to hold for any return from any execution of this Call
   -- of this subprogram. The upper bound for any stack is always
   -- non-negative. Either or both bounds (lower and/or upper) can be
   -- infinite (unlimited). The default value (no assertion) is unlimited
   -- for an Unstable stack and [0,0] for a Stable stack. (It makes no
   -- sense to assert the final stack height for a Stable stack.)


   function Loop_Has_Property (
      Property : Processor.Property_T;
      Luup     : Loops.Loop_T;
      Asserts  : Assertion_Map_T)
   return Boolean;
   --
   -- Whether bounds on the value of the given Property have been
   -- asserted to hold troughout the given Luup (unless overridden
   -- by more local assertions).


   function Call_Has_Property (
      Property : Processor.Property_T;
      Call     : Programs.Call_T;
      Asserts  : Assertion_Map_T)
   return Boolean;
   --
   -- Whether bounds on the value of the given Property have been
   -- asserted to hold at the given Call (in the caller).


   function Loop_Property (
      Property : Processor.Property_T;
      Luup     : Loops.Loop_T;
      Asserts  : Assertion_Map_T)
   return Storage.Bounds.Interval_T;
   --
   -- The bounds on the value of the given Property that are
   -- asserted to hold throughout the execution of the given
   -- Luup (unless overridden by more local assertions).
   --
   -- Propagates No_Such_Property if there are no such assertions.


   function Call_Property (
      Property : Processor.Property_T;
      Call     : Programs.Call_T;
      Asserts  : Assertion_Map_T)
   return Storage.Bounds.Interval_T;
   --
   -- The bounds on the value of the given Property that are
   -- asserted to hold at the given Call (in the caller)
   --
   -- Propagates No_Such_Property if there are no such assertions.


   function Node_Property (
      Property : Processor.Property_T;
      Node     : Flow.Node_T;
      Asserts  : Assertion_Map_T)
   return Storage.Bounds.Interval_T;
   --
   -- The bounds on the value of the given Property that are
   -- asserted to hold in the most local context that contains the
   -- given Node, or the default bound, if there are no assertions on
   -- this Property that apply to the given Node.
   --
   -- This function never propagates No_Such_Property.


   --
   ---   For assertion identification and mapping:
   --


   subtype Source_File_Name_T is Symbols.Source_File_Name_T;
   --
   -- The name of a source file. May consist of a directory/folder
   -- path and a base-name.


   subtype Line_Number_T is Symbols.Line_Number_T;
   --
   -- The number of a line in a program source-code file.


   subtype Line_Offset_T is Line_Number_T'Base;
   --
   -- The offset or difference between two source-line numbers.
   -- Vagaries in the compiler-generated source-line-to-code-address
   -- mappings mean that we must use some "fuzz" when matching program
   -- parts to source-line numbers.


   type Marker_Name_T is new String_Pool.Item_T;
   --
   -- The name (identifier) of a source-line marker. Markers can be
   -- embedded in source-code files to mark specific source-code lines
   -- and these marked lines can then be used to identify the contexts
   -- for assertions.


   type Source_Relation_T is (
      Before,
      On,
      Exactly_On,
      After,
      Contains,
      Spans);
   --
   -- The relation a given source-code position is expected to have
   -- to an actual program part that we hope to identify by this
   -- relationship.
   --
   -- Before, On, Exactly_On, After
   --    The program part is placed before, on/at, or after the
   --    source-code position. The program part is a "point-like" part,
   --    that is, the part is represented by a single flow-graph step.
   --    The literals On and Exactly_On differ only in the "fuzz"
   --    allowed (by default).
   -- Contains
   --    The source-code position is contained within the program part,
   --    which is expected to be an "extended" part that comprises
   --    many flow-graph steps, one of which is placed at this
   --    source-code position.
   -- Spans
   --    The source-code position is spanned by the program part (again
   --    expected to be an "extended" part), that is, the line number
   --    of the source-position lies within the range of source-line
   --    numbers of the steps that comprise the part, even if none of
   --    those steps are placed exactly at this line number.


   subtype Point_Relation_T is Source_Relation_T range Before .. After;
   --
   -- The source relations that can be applied to a "point-like" part.
   -- They can generally be applied also to "extended" parts.


   subtype Extension_Relation_T is Source_Relation_T range Contains .. Spans;
   --
   -- The source relations that can be applied only to "extended" parts,
   -- not at all to "point-like" parts.


   type File_Matching_T is (Base, Full);
   --
   -- How to match (compare) actual file names to file names in assertions,
   -- for example the source-file names used in features to identify
   -- target program parts.
   --
   -- Base(_Name)
   --    Use only the "basename" of the files and ignore the directory path.
   -- Full(_Path)
   --    Compare the full file paths, including directories and base name.
   --
   Base_Name : File_Matching_T renames Base;
   Full_Path : File_Matching_T renames Full;


   type File_Casing_T is (CS, CIS);
   --
   -- Whether the matching of actual file names to file names in assertions
   -- is case-sensitive or case-insensitive (oblivious to letter-case).
   --
   Case_Sensitive : File_Casing_T renames CS;
   Case_Oblivious : File_Casing_T renames CIS;


private


   type Assertion_Set_Object_T;

   type Assertion_Set_T is access all Assertion_Set_Object_T;


   type Assertion_Map_Object_T;

   type Assertion_Map_T is access all Assertion_Map_Object_T;

   No_Map : constant Assertion_Map_T := null;


end Assertions;
