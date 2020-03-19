-- Programs (decl)
--
-- Static representation of the target program under analysis in terms
-- of its subprograms and their call relationships.
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
-- $Revision: 1.72 $
-- $Date: 2015/10/24 19:36:52 $
--
-- $Log: programs.ads,v $
-- Revision 1.72  2015/10/24 19:36:52  niklas
-- Moved to free licence.
--
-- Revision 1.71  2014/06/30 19:10:32  niklas
-- Added function Locus (Step, Subprogram) for convenience.
--
-- Revision 1.70  2014/06/11 12:50:01  niklas
-- Added Add_Unstable_Stack_Without_Pointer, for OCL convenience.
--
-- Revision 1.69  2011-09-08 14:14:07  niklas
-- Added Height_Unit for stacks.
--
-- Revision 1.68  2011-09-01 22:12:17  niklas
-- Added function Exists (Program_T).
--
-- Revision 1.67  2010-01-30 21:13:29  niklas
-- BT-CH-0216: Subprograms have a Return_Method_T attribute.
--
-- Revision 1.66  2009-12-17 14:05:54  niklas
-- BT-CH-0197: Assertions on instruction roles.
--
-- Revision 1.65  2009-12-09 06:25:21  niklas
-- Added function Processor_Info to get the processor-specific
-- info for the Program_T that contains a given Subprogram_T.
--
-- Revision 1.64  2009-11-27 11:28:07  niklas
-- BT-CH-0184: Bit-widths, Word_T, failed modular analysis.
--
-- Revision 1.63  2009/04/16 16:29:24  niklas
-- BT-CH-0171: Stack pointers in call effects.
--
-- Revision 1.62  2008/09/07 07:29:09  niklas
-- BT-CH-0142: Conditional calls and option -[no_]tail_calls.
--
-- Revision 1.61  2008/07/23 09:07:17  niklas
-- BT-CH-0139: Fix recursion in Programs.Execution.Paths.
--
-- Revision 1.60  2008/03/11 22:08:07  niklas
-- BT-CH-0121: Delayed calls and other SHARC support.
--
-- Revision 1.59  2008/02/15 20:27:43  niklas
-- BT-CH-0110: Better "&" for call-paths in Output loci.
--
-- Revision 1.58  2007/12/17 13:54:40  niklas
-- BT-CH-0098: Assertions on stack usage and final stack height, etc.
--
-- Revision 1.57  2007/10/18 13:10:23  niklas
-- Added function Root.
--
-- Revision 1.56  2007/08/17 14:44:01  niklas
-- BT-CH-0074: Stable and Unstable stacks.
--
-- Revision 1.55  2007/08/14 12:36:38  niklas
-- BT-CH-0072: Corrections to handling ambiguous names.
--
-- Revision 1.54  2007/08/03 17:52:36  niklas
-- Added function Index_And_Name for tracing purposes.
--
-- Revision 1.53  2007/03/18 12:50:40  niklas
-- BT-CH-0050.
--
-- Revision 1.52  2007/03/09 13:48:32  niklas
-- Added Subprogram_Exists_At and Subprogram_At.
--
-- Revision 1.51  2007/01/13 13:51:07  niklas
-- BT-CH-0041.
--
-- Revision 1.50  2006/11/26 22:07:27  niklas
-- BT-CH-0039.
--
-- Revision 1.49  2006/10/28 19:52:16  niklas
-- BT-CH-0031.
--
-- Revision 1.48  2006/05/27 21:39:52  niklas
-- BT-CH-0020.
--
-- Revision 1.47  2006/05/17 20:06:35  niklas
-- Added the Take_Off_Node functions.
--
-- Revision 1.46  2006/03/25 13:27:37  niklas
-- Added a "name" attribute to Program_T.
--
-- Revision 1.45  2006/02/27 20:06:13  niklas
-- Added support for code patching in the form of the type
-- Code_Address_List_T and an extended description of procedure
-- Initialize.
--
-- Revision 1.44  2005/09/23 10:51:07  niklas
-- Added function Number_Of_All_Calls.
--
-- Revision 1.43  2005/09/05 11:23:39  niklas
-- BT-CH-0007.
--
-- Revision 1.42  2005/09/03 11:50:31  niklas
-- BT-CH-0006.
--
-- Revision 1.41  2005/08/24 10:12:56  niklas
-- Added stuff to support the Analysis Workbench, including
-- the constant No_Calls, the inquiry functions Node (Call),
-- Nodes (Call_List), Non_Looped_Calls (Subprogram),
-- Containing_Loops (Call), and Calls_Within (Loop).
--
-- Revision 1.40  2005/06/12 07:29:21  niklas
-- Added Call_Mark to set the "arrow" string for call images.
-- Changed the display of calls from A->B to A=>B, to avoid
-- confusion with '-' used in approximate line-numbers.
--
-- Revision 1.39  2005/02/23 09:05:20  niklas
-- BT-CH-0005.
--
-- Revision 1.38  2005/02/20 15:15:37  niklas
-- BT-CH-0004.
--
-- Revision 1.37  2005/02/16 21:11:48  niklas
-- BT-CH-0002.
--
-- Revision 1.36  2005/02/04 20:50:12  niklas
-- Added the "Returns" attribute to subprograms, to show if the
-- subprogram can ever return to the caller. In some cases, marking
-- error-handling subprograms as "never returning" can change an
-- irreducible flow-graph to a reducible one, allowing analysis.
--
-- Revision 1.35  2004/05/01 08:44:18  niklas
-- First Tidorum version.
-- Taking Cell_T stuff from Storage, not Arithmetic.
-- Added Reducible attribute to Subprogram_T.
-- Defined public Subprogram_Index_T and Number_Of_Subprograms, to let
-- clients create arrays indexed by subprogram.
-- Renamed the function Calls to Calls_From to reduce ambiguity.
-- Added the analogous function Calls_To.
-- Explicit reference semantics for Subprogram_T means that all
-- such parameters are now mode "in" even if the underlying object
-- is modified.
-- The functions Locus and Entry_Locus take a Qualified option to
-- control the format of the identifiers.
-- Add the private operation Call_Arrow to help output formatting.
-- Added Trace output to some operations.
-- Find_Subprogram now reports multiple (ambiguous) matches when
-- such occur.
--
-- Revision 1.34  2003/03/11 08:28:02  holsti
-- Split execution-bounds stuff from the Programs package to
-- make the child package Programs.Execution.
-- Normalized some lay-out and commenting.
--
-- Revision 1.33  2002/11/29 11:05:19  holsti
-- Added functions Root_Subprograms, Subprograms_Ascending and
-- Subprograms_Descending. Modified the function Subprograms to return
-- only the analyzed subprograms (those called from the roots) and to
-- exclude subprograms that exist only because of assertions.
-- This fixes NC_0146.
--
-- Revision 1.32  2001/12/10 12:58:51  holsti
-- Execution-bound objects are numbered (indexed) with a unique
-- sequential index, Execution_Bounds_Index_T. It is automatically
-- assigned by Initialize_Bounds, Bound_Time and Create_Blank_Bounds
-- (which take a Program parameter to update the bounds counter) and
-- accessed by the new function Index.
--
-- Renamed Max_Bounds to Number_Of_Bounds and added a synonymous
-- function to report the number of execution bounds in a program,
-- counting all subprograms.
--
-- Revision 1.31  2001/11/19 11:22:46  saarinen
-- Modified for Bound-T/ERC32:
-- Added function Root_Call.
-- Added function Lowest_WCET.
-- Added functions Max_Bounds and Bound_At.
-- Modified Processor.Sub_Info_T to Processor.Program.Sub_Info_T
-- Processor.Validate_Infos -> Processor.Program.Validate_Infos.
--
-- Revision 1.30  2001/05/20 13:16:23  holsti
-- Added Set_Processor_Info (NC_117).
--
-- Revision 1.29  2001/04/11 13:22:06  ville
-- Parameter Delimiter added to Programs.Identify
--
-- Revision 1.28  2001/04/04 10:22:26  holsti
-- Add_Root replaces Set_Roots and Make_Root_Call.
--
-- Revision 1.27  2001/03/21 20:29:25  holsti
-- Locus functions added. Other updates to new Output.
--
-- Revision 1.26  2001/03/15 12:03:20  holsti
-- Node_Times optionally include lower-level calls.
--
-- Revision 1.25  2001/03/10 00:43:21  holsti
-- Extensive updates to the handling of calls and their effects.
-- Call_Step_T and Call_Step_Set_T removed.
-- Input and output cell-set attributes added to subprograms and
-- execution bounds.
-- Basis cell-set and initial cell-bounds attributes added to
-- execution bounds.
-- Parameter mapping implemented, using dynamic access.
-- A "launched" attribute is added to calls.
--
-- Revision 1.24  2001/03/06 09:36:41  ville
-- Root-calls excluded from returned calls
--
-- Revision 1.23  2001/02/19 14:49:39  holsti
-- Store_Bounds made public, not always automatic.
--
-- Revision 1.22  2001/02/19 09:29:38  holsti
-- Execution bounds are identified by call path and stored with
-- the subprogram to which they apply. No execution bounds are
-- stored with specific calls. Detailed changes include:
-- Execution_Bounds_T renamed to Execution_Bounds_Ref.
-- Loop bounds with execution bounds no longer contain a copy
-- of the loops of the subprogram, just the bounds; there is
-- no risk of "loop not found".
-- Call_Path_T added.
-- Initialize_Bounds replaces the Erase procedure.
-- Bound_Time replaces the Bound_WCET procedure.
-- All Is_Call_Dependent functions deleted.
-- New_Subprogram encapsulates the creation of new subprograms.
-- Call_Bounds moved to logical position.
-- Functions to get the subprogram and call-path from a set of
-- execution bounds added.
-- WCET_Bounded renamed to Time_Bounded.
-- WCET renamed to Time.
-- Stack_Bounded added, checks if execution bounds include stack.
-- All functions that get execution-bound values are careful to
-- check for null bounds and return without exception.
-- Show_Stack_Path and Show_Stack use call-path-specific bounds.
--
-- Revision 1.21  2000/12/28 17:46:31  holsti
-- Some_Calls added.
--
-- Revision 1.20  2000/12/28 12:33:50  holsti
-- Subprogram_Name_List_T and Not_Implemented removed (not used).
-- Identify takes the Identifier as a String (not Unbounded_String).
-- Identify does not report "already mentioned" when the subprogram exists,
-- because this was relevant only for root subprograms on the command-line.
-- No_Subprogram added.
--
-- Revision 1.19  2000/12/22 11:20:48  saarinen
-- Fixed NC_025, NC_026 and NC_028.
-- Changed Node_Times in Execution_Bounds_Object to be a reference.
--
-- Revision 1.18  2000/12/21 14:39:40  sihvo
-- Added No_Bounds.
--
-- Revision 1.17  2000/12/21 08:24:22  sihvo
-- Minor changes in layout etc.
--
-- Revision 1.16  2000/12/05 15:43:51  holsti
-- The term "loop neck" replaces the overloaded "loop entry".
-- Decoder.Stack_Height_Cell replaces deleted Arithmetic function.
--
-- Revision 1.15  2000/11/24 12:06:01  sihvo
-- Added stack height analysis.
--
-- Revision 1.14  2000/11/24 10:15:47  saarinen
-- Added attribute Var_Cells into Subprogram_Object_T.
-- Added functions to manipulate Var_Cells.
-- Deleted Set_WCET.
--
-- Revision 1.13  2000/11/22 22:32:36  holsti
-- Using Programs.Code_Address_T instead of Programs.Address_T.
-- Added function Calls (From : Subprogram_T; Into : Subprogram_Set_T).
--
-- Revision 1.12  2000/10/26 09:35:53  saarinen
-- Added function Call_Bounds.
--
-- Revision 1.11  2000/09/20 19:30:18  saarinen
-- Added function Processor_Info and modified Calls_Between.
--
-- Revision 1.10  2000/09/05 11:31:28  saarinen
-- Added some operations.
--
-- Revision 1.9  2000/08/04 08:28:12  saarinen
-- Added type Call_List_Ref, and functions Calls(Program) and
-- Make_Root_Call(Subprogram).
--
-- Revision 1.8  2000/07/25 03:12:43  holsti
-- Execution_Bound_T added, other bounds reorganised.
--
-- Revision 1.7  2000/07/13 11:31:02  saarinen
-- Added Call_Step_Set_T, modified Program_T, added support for
-- Call_Graph creation and made some other minor changes.
--
-- Revision 1.6  2000/07/13 10:29:42  parviain
-- Added setters and getters for Call's attributes.
--
-- Revision 1.5  2000/07/04 10:35:23  parviain
-- added one semicolon
--
-- Revision 1.4  2000/07/04 10:21:47  parviain
-- added functions for call information
--
-- Revision 1.3  2000/06/29 07:03:14  saarinen
-- Changed Call_List_T to be array of Call_T
--
-- Revision 1.2  2000/04/27 10:46:35  holsti
-- First implementation.
--


with Arithmetic;
with Arithmetic.Algebra;
with Arithmetic.Opt;
with Calling;
with Flow;
with Loops;
with Output;
with Processor;
with Processor.Program;
with Storage;
with Storage.Bounds;
with Symbols;


package Programs is
--
-- Defines the types that represent the target program under analysis.
-- Most of the analysis results are represented elsewhere (see the child
-- package Programs.Execution).
--
-- There are five main data types:
--
-- > program
-- > subprogram
-- > call
-- > stack
-- > compiler.
--


   --
   ---   Subprograms
   --


   type Subprogram_T is private;
   --
   -- A subprogram under analysis. This can be one of the "root"
   -- subprograms named on the command line or in the HRT model
   -- (TPO file), or it can be a subprogram that was discovered
   -- by the WCET tool itself as a subprogram that is called,
   -- directly or indirectly, by some root subprogram.
   --
   -- The attributes of a subprogram include:
   --
   -- > the program of which the subprogram is a part
   --
   -- > the subprogram's name (identifier, including scope)
   --
   -- > the subprogram's entry-address
   --
   -- > whether the subprogram is known (or considered) to be "unused",
   --   which means that all calls to this subprogram can be considered
   --   infeasible and the subprogram need not be analysed at all
   --
   -- > whether a call to the subprogram should be decoded "integrated"
   --   which means (re-)decoding the subprogram into the caller's
   --   control-flow graph.
   --
   -- For a subprogram that is not decoded "integrated", the following
   -- further attributes are relevant:
   --
   -- > whether the subprogram really is analysed, or has an asserted
   --   resource usage (eg. execution-time bound) and is thus only
   --   handled as a "stub"
   --
   -- > whether "arithmetic analysis" is allowed or denied for this
   --   subprogram
   --
   -- > the control-flow graph, which may be incomplete because it
   --   contains unresolved dynamic jumps
   --
   -- > the loop structure of the flow-graph
   --
   -- > whether the loop-structure is reducible
   --
   -- > whether the subprogram ever returns to its caller, and
   --   if so, whether it returns to the normal return address
   --   (as offered by the caller) or to some other address, for
   --   example offset from the normal address
   --
   -- > the set of calls from this subprogram to other subprograms
   --
   -- > whether the subprogram directly or indirectly calls some
   --   subprogram that is not really analysed and is a stub.
   --
   -- The control-flow graph is provided with the "primitive"
   -- arithmetic effects and edge preconditions. Analysis will create
   -- more refined or context-dependent computation models for the
   -- flow-graph.
   --
   -- A stub subprogram has an unknown arithmetic effect (the effect
   -- in the stub flow-graph is usually null, but is not the real
   -- effect). The effect of a call to a stub subprogram is (or will
   -- be) to make unknown all those cells that are used in the caller
   -- and that may be the targets of assignments in the stub callee.
   --
   -- Similarly, a subprogram that directly or indirectly calls a stub
   -- subprogram also has an unknown (or partially unknown) arithmetic
   -- effect, and again the effect of a call to such a subprogram can
   -- make unknown some of the cells that are used in the caller. We
   -- keep track of the minimum call-depth from a given subprogram to
   -- a stub subprogram. When this depth is large, the possible effects
   -- on the caller's cells may be small because the "call-depth
   -- distance" from the caller to the stub is large.
   --
   -- Our analysis derives bounds on the execution of the subprogram.
   -- The bounds are not attached directly to the subprogram object,
   -- but collected separately; see package Programs.Execution.
   --
   -- Subprogram_T has reference semantics. The values of type
   -- Subprogram_T returned by functions in this package are to
   -- be understood as references to some underlying subprogram
   -- objects; any update applied to such a reference is visible
   -- via all references to the same object. Consequently, most
   -- parameters of type Subprogram_T are of mode "in" even when
   -- the operation modifies the underlying subprogram object.
   --
   -- All Subprogram_T objects created for one Program_T are numbered
   -- (indexed) consecutively, in order of creation, from 1 to the
   -- number of subprogram objects in the program. However, only the
   -- analysed subprograms are numbered in this way.
   --
   -- The index of a Subprogram_T (or the underlying object) is a
   -- readable attribute so the clients can, if desired, set up arrays
   -- of subprograms based on this index. For example, clients can use
   -- Boolean arrays to represent sets of subprograms as bit-vectors.


   type Subprogram_Count_T is new Natural;
   --
   -- A number of subprograms. For example, the total number of
   -- subprograms created in the analysis of one program.


   subtype Subprogram_Index_T is
      Subprogram_Count_T range 1 .. Subprogram_Count_T'Last;
   --
   -- Identifies a specific subprogram object within all subprogram
   -- objects created in the analysis of one program.


   type Subprogram_List_T is array (Positive range <>) of Subprogram_T;
   --
   -- A set (or list) of subprograms.


   type Subprogram_Set_T is private;
   --
   -- An updatable set of subprograms.
   -- The initial value of any variable of type Subprogram_Set_T is
   -- the empty set.


   --
   ---   Calls
   --


   type Call_T is private;
   --
   -- A subprogram call. Attributes include:
   --
   --  > The caller (calling subprogram).
   --  > The callee (called subprogram). This is never a subprogram that
   --    is decoded "integrated" in the caller.
   --  > The call-step in the caller's control-flow graph.
   --  > The primitive calling protocol in use for this call.
   --  > An index to identify this call within the caller.
   --  > The program in which the call occurs.
   --
   -- The primitive calling protocol is the one assigned by the program
   -- decoder when the call is decoded and added to the caller's
   -- control-flow graph. When analysis creates a computation model for
   -- the flow-graph, the model may give a refined or altered calling
   -- protocol for the call, reflecting data-flow information collected
   -- by the analysis from the flow-graph and from the context (call-path).
   --
   -- The index of the call is a sequential unique index that numbers
   -- all the calls with the same caller. The index is not globally
   -- unique. For example, if subprogram Foo contains three calls
   -- (which thus have caller = Foo), these calls are indexed 1, 2, 3.
   --
   -- The program attribute is the same for the call, the caller sub-
   -- program and the callee subprogram (it is in fact inherited from
   -- the caller subprogram).
   --
   -- Our context-dependent analysis derives bounds on the execution
   -- of calls, ie. on the execution of the callee when entered from
   -- a specific call. The bounds are not attached directly to the call
   -- object, but collected separately; see package Programs.Execution.
   --
   -- Call_T has reference semantics (see explanation for Subprogram_T).


   No_Call : constant Call_T;
   --
   -- Indicate a call that is undefined or absent.
   -- This is also the default initial value of any Call_T variable.


   subtype Call_Index_T is Positive;
   --
   -- The calls in (or from) a given subprogram, to other callee subprograms,
   -- are numbered 1, 2, ... using Call_Index_T.
   --
   -- NOTE that this numbering is specific to each caller subprogram, not
   -- global in the whole program.


   type Call_List_T is array (Positive range <>) of Call_T;
   --
   -- A set (or list) of calls.
   --
   -- The index is not generally meaningful, but for some such lists it
   -- may be specified to be the Call_Index_T of the calls.


   type Call_List_Ref is access Call_List_T;


   No_Calls : constant Call_List_T;
   --
   -- An empty (null) list of calls.


   --
   ---   Calling contexts = call paths
   --


   type Call_Path_T is array (Positive range <>) of Call_T;
   --
   -- A call path, which is a nest of calls listed in top-down
   -- order. Thus, the first call invokes a subprogram that
   -- contains the second call, which invokes a subprogram that
   -- contains the third call, and so on.

   function Null_Call_Path return Call_Path_T;
   --
   -- A null call-path (constant).


   function "<=" (Left, Right : Call_Path_T) return Boolean;
   --
   -- Whether one call-path (Left) is a suffix of another (Right).


   function "-" (Left, Right : Call_Path_T) return Call_Path_T;
   --
   -- When the Right call-path is a suffix of the Left call-path,
   -- returns the rest of Left (the prefix of Left that precedes
   -- the shared suffix). Otherwise, raises Constraint_Error.
   --
   -- If P := Left - Right, then Left = P & Right.


   function Image (Path : Call_Path_T)
   return String;
   --
   -- A textual description of the call-path.


   function Locus (Path : Call_Path_T)
   return Output.Locus_T;
   --
   -- Describes the location of the call-path in the target program
   -- under analysis, for display purposes.
   -- The location is within the final called subprogram, the final
   -- callee.


   --
   ---   Stacks
   --


   type Stack_T is private;
   --
   -- Models a stack that is a part of memory that the program under
   -- analysis uses to hold local data for subprograms, return addresses
   -- and parameters passed from the caller to the callee or vice versa.
   --
   -- A given stack can be used for any or all of these purposes, or perhaps
   -- for some special purpose that is none of the above.
   --
   -- A Stack_T has the following attributes:
   --
   -- > A name, which identifies the stack in the output of the analysis.
   --   The name is a string, free format, and has no meaning for the
   --   analysis. There is no check against duplicate names.
   --
   -- > A stack-height cell. This is a Storage.Cell_T that models the local
   --   stack height which means the amount of stack used by the current
   --   subprogram. The value of the stack-height cell at a given point in
   --   the subprogram gives the amount of local stack in use at that point.
   --   A "push" increases the stack height and a "pop" decreases it.
   --
   -- > A constant height-unit, which is the amount of memory, in bits,
   --   represented by one unit in the value of the stack-height cell.
   --
   -- > Possibly a stack-pointer cell. This is a Storage.Cell_T that models
   --   the stack pointer register and changes in step (or in opposite step)
   --   with the stack height.
   --
   -- > The direction of coupling between the stack pointer and the stack
   --   height. If the coupling is Same, a push increases both the stack
   --   height and the stack pointer, by the same amount, and a pop
   --   decreases both by the same amount. If the coupling is Opposite,
   --   a push decreases the stack pointer while increasing the stack
   --   height, but the absolute changes are still the same, and similarly
   --   for pop. Thus, a Same coupling means an "upwards growing" stack,
   --   while an Opposite coupling means a "downwards growing" stack.
   --
   -- > The net change in stack height over a call, if this is a known
   --   constant valid for all subprograms that use this stack.
   --
   -- > An index. All stacks in a given program are indexed sequentially
   --   and uniquely starting from 1. Thus, analysis results such as the
   --   maximum stack usages can be defined as arrays indexed by stack
   --   index.
   --
   -- > The kind of the stack, Stable or Unstable, as explained below.


   type Stack_Kind_T is (Stable, Unstable);
   --
   -- We have two kinds of (models of) stacks: Stable and Unstable.
   -- The Stable model is an older approach that is retained to ease the
   -- transition to the Unstable model which is more general and flexible.
   --
   -- For a Stable stack (model), the local stack height in a caller
   -- subprogram is unchanged (stable) over any call to some other
   -- callee subprogram. The callee may alter the stack and push things
   -- onto it, but must always pop exactly as much as it pushed so that
   -- on return the stack height is restored to its value before the call.
   --
   -- For an Unstable stack (model) a call can have an overall, net effect
   -- on the local stack height in the caller. For example, in some calling
   -- protocols the caller is responsible for pushing stack-located
   -- parameters onto the stack before the call, while the callee must
   -- pop these parameters from the stack before returning to the caller.
   -- In this case the overall (net) effect of the call on the caller's
   -- local stack height is negative: the local stack height is decreased
   -- by the size of the stack-located parameters.
   --
   -- The Stable or Unstable nature of a stack affects our assumptions and
   -- analysis on two points: the initial value of the local stack height
   -- in any subprogram, and the analysis of the overall or net effect of
   -- a call on the local stack height in the caller (also known as the
   -- final local stack height in the callee).
   --
   -- The initial value of the stack-height cell on entry to a subprogram
   -- is defined as follows:
   --
   -- > For a Stable stack the initial local stack height must be defined
   --   by the function Processor.Properties.Entry_Bounds, which must
   --   bound the stack-height cell to exactly one value (singleton).
   --
   -- > For an Unstable stack the initial local stack height is zero.
   --   This is an axiom to which the model of the stack must conform;
   --   any "pushes" caused by the calling sequence (including the
   --   possible push of a return address) is counted in the caller's
   --   stack usage and local stack height. The Entry_Bounds function
   --   should not return any bounds on the stack-height cell.
   --
   -- The overall or net effect of a call on the caller's local stack
   -- height is analysed as follows:
   --
   -- > For a Stable stack the overall effect is zero: the call has no
   --   effect on the (caller's view of the) stack-height cell. This is
   --   an axiom to which the model of the stack must conform. Any "push"
   --   caused by the calling sequence (including the possible push of
   --   a return address) should be counted in the initial value of the
   --   callee's local stack height (from Entry_Bounds). Any "pops"
   --   in the return sequence (including the possible pop of a return
   --   address) should have no effect on the stack-height but may have
   --   an effect on the cells for which values are popped from the stack.
   --   The "net change" attribute of a Stable stack is zero.
   --
   -- > For an Unstable stack a subprogram may push more than it pops, or
   --   vice versa, thus an execution of the subprogram may cause a net
   --   change, positive or negative, in the stack height. If the net
   --   change has a constant value, for all subprograms, it can be defined
   --   as the "net change" attribute of the stack. If the net change is
   --   not known a priori we will try to analyse the computations in
   --   each subprogram to determine the net change (final local stack
   --   height on return).
   --
   -- The set of stacks in a given program is defined in the operation
   -- Decoder.Initialize which loads the program from a binary file.
   -- Thereafter the set of stacks is fixed throughout the analysis of
   -- this program. The nature of each stack (Stable or Unstable) is set
   -- when the stack is defined and cannot be changed.
   --
   -- The stack-height cells should be marked as Calling.Privy cells in
   -- the Map_Kind function of the calling protocol, because each subprogram
   -- activation has its own instance of the stack-height cell. This holds
   -- even if the subprogram causes a net change in the stack height; this
   -- change is transported from the callee's stack-height cell to the
   -- caller's stack-height cell in a dedicated way (see the function
   -- Final_Stack_Effect in Programs.Execution). However, the Sure_Invariant
   -- function of the calling protocol should mark the stack-height cell
   -- as invariant over a call only when the protocol ensures that there is
   -- no net change in the stack height over a call, for example when the
   -- stack is a Stable one.


   subtype Stack_Index_T is Positive;
   --
   -- The index of a stack.


   function Name (Stack : Stack_T) return String;
   --
   -- The name of the Stack.


   function Kind (Stack : Stack_T) return Stack_Kind_T;
   --
   -- The kind of the stack.


   function Height (Stack : Stack_T) return Storage.Cell_T;
   --
   -- The cell that holds the stack height for this Stack.


   function Height (Stack : Stack_T) return Arithmetic.Variable_T;
   --
   -- The cell that holds the stack height for this Stack, as
   -- a variable suitable for use as an assignment target or a
   -- part of an arithmetic expression.
   --
   -- This function does not allocate more memory; it reuses a predefined
   -- Variable_T object.


   function Height_Unit (Stack : Stack_T)
   return Arithmetic.Positive_Value_T;
   --
   -- The number of bits of storage represented by one unit
   -- in the stack height of this Stack.


   function Pointer (Stack : Stack_T) return Storage.Cell_T;
   --
   -- The cell that models the stack pointer for this Stack,
   -- or No_Cell if no stack pointer is modelled.


   function Pointer (Stack : Stack_T) return Arithmetic.Variable_T;
   --
   -- The cell that models the stack pointer for this Stack, as
   -- a variable suitable for use as an assignment target or a
   -- part of an arithmetic expression, or Unknown if no stack
   -- pointer is modelled for this stack.
   --
   -- This function does not allocate more memory; it reuses a
   -- predefined Variable_T object.


   function Coupling (Stack : Stack_T) return Arithmetic.Algebra.Coupling_T;
   --
   -- The coupling between the stack-height and stack-pointer cells.
   -- Irrelevant (to us, at least) if there is no stack pointer.


   function Width_Of (Stack : Stack_T) return Arithmetic.Width_T;
   --
   -- The width of the cell that holds the stack height for this Stack.
   -- Short for Storage.Width_Of (Height (Stack)).


   function Net_Change (Stack : Stack_T) return Storage.Bounds.Limit_T;
   --
   -- The net change in (local) stack height of this Stack, over any
   -- call of any subprogram, when Known; else Unlimited.
   --
   -- For a Stable stack the result is Storage.Bounds.Limit (0).


   function Index (Stack : Stack_T) return Stack_Index_T;
   --
   -- The index of the Stack which uniquely identifies the Stack
   -- within a given program.


   type Stacks_T is array (Stack_Index_T range <>) of Stack_T;
   --
   -- A list of stacks, typically all the stacks in a program.


   --
   ---   Compilers
   --


   type Compiler_T is abstract tagged limited null record;
   --
   -- Represents a compilation tool-chain that generates target
   -- programs for analysis. However, the operations on this type
   -- are trivial because at this point in the dependency structure
   -- we cannot use the types that are needed for the interesting
   -- operations. Target-specific modules will derive types for
   -- specific compilers.


   type Compiler_Ref is access all Compiler_T'Class;
   --
   -- Refers to a Compiler on the heap.


   function Name (Item : Compiler_T) return String
   is abstract;
   --
   -- The name of the compiler. For human consumption only.


   procedure Set_Option (
      Argument : in     String;
      Compiler : in out Compiler_T;
      Valid    :    out Boolean);
   --
   -- Sets a compiler-specific option, if Valid.
   -- The default operation accepts no Arguments as Valid.


   procedure Set_Warn_Option (
      Item     : in      String;
      Compiler : in out Compiler_T;
      Valid    :    out Boolean);
   --
   -- Sets a compiler-specific "-warn" option, if Valid.
   -- The default operation accepts no Items as Valid.


   procedure List_Options (Compiler : in Compiler_T);
   --
   -- List the compiler-specific options.
   -- The default operation does nothing.


   procedure List_Warn_Options (Compiler : in Compiler_T);
   --
   -- List the compiler-specific "-warn" options.
   -- The default operation does nothing.


   --
   ---   A null compiler
   --


   type Null_Compiler_T is new Compiler_T with null record;
   --
   -- An unknown or uninteresting compiler.


   function Name (Item : Null_Compiler_T) return String;


   --
   ---   Programs
   --


   type Program_T is private;
   --
   -- A target program being analysed. Attributes include:
   --
   -- > A name, of no logical significance. Usually equals the
   --   file-name of the executable target program.
   --
   -- > Access handles to the memory image of the program, containing
   --   the code and static (constant) data.
   --
   -- > Set of "root" Subprograms, which are the topmost subprograms
   --   for which analysis is requested.
   --
   -- > Full set of Subprograms under analysis, including the root
   --   subprograms and all lower-level subprograms called by
   --   the roots, directly or indirectly.
   --
   -- > All Calls between the Subprograms under analysis.
   --
   -- > Set of "shoot" subprograms, which are the bottom-most subprograms
   --   for which calls from the other subprograms have been found but
   --   which have not yet been analysed.  This is a working attribute
   --   that forms part of the work-list algorithm for exploring the
   --   call-graph. When the call-graph is complete, the shoot-set is
   --   empty.
   --
   -- > The set of stacks in the program, possibly empty.
   --
   -- > A reference to the compiler that generated the program.
   --
   -- > The Symbol Table for the program, connecting source-code
   --   entities (identifiers, source-code file names and line numbers)
   --   to machine entities (memory addresses, registers).
   --
   -- > The asserted "role" of some instructions, identified by code
   --   address.
   --
   -- Program_T has reference semantics. A value of type Program_T should
   -- be understood as a reference (access) to an underlying program
   -- object. See explanation for Subprogram_T.
   --
   -- The initial value of any Program_T variable is a "null reference"
   -- that is invalid for use as an "in" parameter to any operation in
   -- this package. Every Program_T must be initialized, for example by
   -- the Initialize operation (see below).



   -- PROVIDED OPERATIONS:


   --
   ---   Program operations
   --


   function Exists (Program : Program_T)
   return Boolean;
   --
   -- Whether the Program has been initialized and exists.
   -- See Initialize, below.


   procedure Initialize (
      From_File : in     String;
      Program   :    out Program_T);
   --
   -- Initializes the Program under analysis as follows.
   -- First, the operation sets
   --
   -- > the name to the File_Name (without directory path),
   -- > the set of subprograms to empty,
   -- > the set of root subprograms to empty,
   -- > the set of calls to empty,
   -- > the set of stacks to empty, and
   -- > the symbol table to an empty table.
   --
   -- Then, the operation calls
   --
   -- > Decoder.Initialize
   --
   -- to read the memory image, symbol table and other useful information
   -- From the named File, which is usually a compiled, linked, executable
   -- binary file. Decoder.Initialize can fill and update the Program as
   -- it wishes, adding subprograms, roots, calls, stacks, symbols, etc.
   --
   -- Finally, the operation calls
   --
   -- > Programs.Patching.Apply_Patches, which calls
   -- > Decoder.Patch_Code
   --
   -- to apply any patches that are requested for the memory image of
   -- the target program. If patching fails, the operation propagates
   -- Options.Argument_Error.


   function Name (Program : Program_T) return String;
   --
   -- The name of the program.


   procedure Set_Processor_Info (
      To     : in Processor.Program.Info_T;
      Within : in Program_T);
   --
   -- Sets the processor-specific information Within a given program,
   -- To a given value.


   function Processor_Info (Program : Program_T)
   return Processor.Program.Info_T;
   --
   -- The processor-specific information for the Program.


   function Symbol_Table (Program : Program_T)
   return Symbols.Symbol_Table_T;
   --
   -- The symbol table for the Program.
   -- Note that Symbol_Table_T has reference semantics, so the symbol table
   -- can be updated via the value of this function. Therefore we do not
   -- provide an operation Set_Symbol_Table.


   function Number_Of_Subprograms (Within : Program_T)
   return Subprogram_Count_T;
   --
   -- The number of subprograms (subprogram objects, really) created
   -- within this program. Initially zero, unless the target-specific
   -- operations invoked for the Initialize operation create some
   -- subprograms for their own purposes. For example, they may need
   -- to analyse some trap handlers that are not directly called by
   -- application subprograms but may be invoked indirectly and add
   -- to the execution time of the application subprograms.


   procedure Add_Root (
      Root : in     Subprogram_T;
      To   : in     Program_T;
      Call :    out Call_T);
   --
   -- Defines a subprogram as a root subprogram within a
   -- target program under analysis.
   --
   -- Constructs and returns a virtual call of the root
   -- subprogram, with a null caller. These "root calls" are
   -- returned by Root_Calls (see below).


   function Root_Subprograms (Prog : Program_T)
   return Subprogram_List_T;
   --
   -- All the root subprograms defined by Add_Root.
   -- The order is arbitrary.


   function Root_Calls (Prog : Program_T) return Call_List_T;
   --
   -- Returns a list of artificial calls to given subprograms.


   Subprogram_Not_Found : exception;
   --
   -- Raised when an attempt to Identify a subprogram fails.


   function Root_Call (
      Address : Processor.Code_Address_T;
      Within  : Program_T)
   return Call_T;
   --
   -- Returns the root call to a subprogram which starts at Address.
   --
   -- Raises Subprogram_Not_Found is there is no such root call.


   function Subprogram_Entries (
      Name   : String;
      Within : Program_T)
   return Processor.Code_Address_List_T;
   --
   -- Looks up the given subprogram name (in a global scope)
   -- and returns all the addresses connected to this name.
   -- There may be zero, one, or several addresses.


   procedure Identify (
      Identifier : in     String;
      Delimiter  : in     Character;
      Program    : in     Program_T;
      Subprogram :    out Subprogram_T);
   --
   -- Identifies (locates, finds) a subprogram in the program by
   -- means of a symbolic identifier, nominally the qualified name
   -- of the subprogram.
   --
   -- It is TBD if other forms are accepted, but acceptable identifiers
   -- could include the unqualified name (when unambiguous) or the
   -- entry address (in hexadecimal, for example).
   --
   -- If the subprogram is found, it is added to the set of subprograms
   -- under analysis (if not already there) and returned in the
   -- Subprogram parameter.
   --
   -- A newly created Subprogram is marked as not a stub; irreducible;
   -- not called; not unused; not to be decoded in the integrated way;
   -- and not to be hidden in the call-graph.
   --
   -- If the subprogram is not found, Subprogram_Not_Found is propagated.
   --
   -- If the Identifier and Delimiter give so little "scope" information
   -- that they could match more than one symbol, Symbols.Ambiguous_Name
   -- is propagated.


   procedure Identify (
      Address    : in     Processor.Code_Address_T;
      Program    : in     Program_T;
      Subprogram :    out Subprogram_T);
   --
   -- Identifies (locates, finds) a subprogram in the program by
   -- means of the starting address of the subprogram. The address
   -- can identify a subprogram that also has a symbolic identifier;
   -- if not, a subprogram is created and given a synthetic
   -- identifier that is Processor.Image (Address).
   --
   -- A newly created Subprogram is marked as not a stub; irreducible;
   -- not called; not unused; not to be decoded in the integrated way;
   -- and not to be hidden in the call-graph.
   --
   -- Never propagates Subprogram_Not_Found.


   function Enters_Subprogram (
      Address : Processor.Code_Address_T;
      Program : Program_T)
   return Boolean;
   --
   -- Whether the given Address is the entry address of a subprogram,
   -- defined as follows:
   --
   -- > If a Subprogram_T with this entry address already exists in
   --   the Program model, the answer is True.
   --
   -- > Otherwise, the answer is True iff the symbol-table of the
   --   Program connects the Address to a subprogram, as the entry
   --   address of the subprogram.


   function Subprogram_Exists_At (
      Address : Processor.Code_Address_T;
      Within  : Program_T)
   return Boolean;
   --
   -- Whether there already exists a subprogram with the given entry
   -- Address, Within the given program. The possible existence of
   -- subprogram connections for this Address in the program's Symbol
   -- Table is not a factor.


   function Subprogram_At (
      Address : Processor.Code_Address_T;
      Within  : Program_T)
   return Subprogram_T;
   --
   -- The subprogram with the given entry Address, Within the given
   -- program, or No_Subprogram if there is no such subprogram in
   -- existence yet (even if the program's Symbol Table contains
   -- subprogram connections for this Address).
   --
   -- Never propagates Subprogram_Not_Found.
   -- Never creates a new subprogram.


   function Subprograms (Program : Program_T)
   return Subprogram_List_T;
   --
   -- All the subprograms currently reached in the analysis process
   -- starting from the root subprograms. The subprograms are listed
   -- in an arbitrary order, not necessarily top-down or bottom-up.
   --
   -- Subprograms that have been "identified" only because they are
   -- mentioned in assertions are not included.


   function Subprograms_Descending (Program : Program_T)
   return Subprogram_List_T;
   --
   -- All the subprograms currently reached in the analysis process,
   -- in a top-down calling order, starting from the root subprograms
   -- and ending at the leaf subprograms.
   --
   -- Subprograms that have been "identified" only because they are
   -- mentioned in assertions are not included.


   function Subprograms_Ascending (Program : Program_T)
   return Subprogram_List_T;
   --
   -- All the subprograms currently reached in the analysis process,
   -- in a bottom-up calling order, starting from leaf subprograms and
   -- ending at the root subprograms.
   --
   -- Subprograms that have been "identified" only because they are
   -- mentioned in assertions are not included.


   function Calls (Program : Program_T) return Call_List_T;
   --
   -- All the calls currently reached in the analysis process.
   -- The root-calls (from "no subprogram" to a root subprogram)
   -- are not included in the result. See All_Calls below.


   function All_Calls (Program : Program_T) return Call_List_T;
   --
   -- All the calls currently reached in the analysis process.
   -- The root-calls (from "no subprogram" to a root subprogram)
   -- are included in the result.


   function Number_Of_All_Calls (Program : Program_T) return Natural;
   --
   -- The number of calls currently reached in the analysis process,
   -- including the root-calls, so the result is the same as
   -- All_Calls (Program)'Length.


   procedure Move_Shoots (
      From : in     Program_T;
      To   : in out Subprogram_Set_T);
   --
   -- Moves all the shoot subprograms From the program's shoot set
   -- To the given subprogram set. The shoot set is then empty.
   -- Any subprograms that already were in the To set are still
   -- there; no subprograms are removed from the To set.


   procedure Add_Stable_Stack (
      Name     : in String;
      Pointer  : in Storage.Cell_T;
      Height   : in Storage.Cell_T;
      Unit     : in Arithmetic.Positive_Value_T;
      Coupling : in Arithmetic.Algebra.Coupling_T;
      To       : in Program_T);
   --
   -- Adds a Stable stack with a given Name, stack Pointer cell,
   -- stack Height cell, Height Unit, and Coupling direction, To
   -- a program. Use Pointer => No_Cell if there is no stack pointer.
   --
   -- This has the following consequences and implications:
   --
   -- > Processor.Properties.Entry_Bounds must bound Height to a
   --   single value (but this value may depend on the subprogram)
   --   that is the initial local stack Height for this stack when
   --   entering this subprogram.
   --
   -- > The net effect of any call on the caller's value of Height
   --   is assumed to be zero, and to leave the Pointer cell
   --   unchanged (the Pointer is invariant in any call).
   --
   -- > The final value of Height on return from any subprogram is
   --   irrelevant but is assumed to be zero and is not derived
   --   from the subprogram. (For Stable stacks the models of the
   --   call and return instructions usually omit the push/pop of
   --   the return address, so a derived value would be misleading.)
   --
   -- > The coupling is irrelevant to us, but is preserved and
   --   reported by the Coupling function.
   --
   -- This operation should be used *only* in the operation
   -- Decoder.Initialize, after which the set of stacks in the program
   -- must be held fixed.


   procedure Add_Unstable_Stack (
      Name       : in String;
      Pointer    : in Storage.Cell_T;
      Height     : in Storage.Cell_T;
      Unit       : in Arithmetic.Positive_Value_T;
      Coupling   : in Arithmetic.Algebra.Coupling_T;
      Net_Change : in Storage.Bounds.Limit_T;
      To         : in Program_T);
   --
   -- Adds an Unstable stack with a given Name, stack Pointer cell,
   -- stack Height cell, Coupling, and a given net height change (over
   -- any call, when known) To a program. Use Pointer => No_Cell if
   -- there is no stack pointer.
   --
   -- This has the following consequences and implications:
   --
   -- > Processor.Properties.Entry_Bounds must not set any bounds
   --   on Height (or must bound it to zero).
   --
   -- > The initial local stack Height on entry to any subprogram is
   --   assumed to be zero.
   --
   -- > The final value of Height on return from any subprogram is
   --   derived from the subprogram's effects on Height.
   --
   -- > The change in the Pointer cell value for any call is derived
   --   from the final value of Height for that call and the
   --   given Coupling. For a Same coupling the Pointer increases
   --   by the final Height; for an Opposite coupling the Pointer
   --   decreases by the final Height.
   --
   -- This operation should be used *only* in the operation
   -- Decoder.Initialize, after which the set of stacks in the program
   -- must be held fixed.


   procedure Add_Unstable_Stack_Without_Pointer (
      Name       : in String;
      Height     : in Storage.Cell_T;
      Unit       : in Arithmetic.Positive_Value_T;
      Net_Change : in Storage.Bounds.Limit_T;
      To         : in Program_T);
   --
   -- An abbreviation for the full Add_Unstable_Stack (see above)
   -- when the stack has no Pointer cell, which also means that the
   -- coupling between the (non-existent) pointer and the stack-height
   -- value is irrelevant.


   function Number_Of_Stacks (Within : Program_T) return Natural;
   --
   -- The number of stacks defined by Add_Stack Within the program.


   function Stacks (Within : Program_T) return Stacks_T;
   --
   -- All the stacks Within the program, as defined by Add_Stack.
   -- The index range is 1 .. Number_Of_Stacks (Within).


   No_Such_Stack : exception;
   --
   -- Signals that a given program contains to stack with the
   -- desired properties (index or name).


   function Stack_By (
      Index  : Stack_Index_T;
      Within : Program_T)
   return Stack_T;
   --
   -- The stack with the given Index, Within the given program.
   -- Propagates No_Such_Stack if there is no such stack (eg. the
   -- Index is out of range).


   function Stack_By (
      Name   : String;
      Within : Program_T)
   return Stack_T;
   --
   -- The stack with the given Name, Within the given program.
   -- Propagates No_Such_Stack if there is no such stack.


   function Stack_Name (Index : Stack_Index_T; Within : Program_T)
   return String;
   --
   -- The name of the stack with the given Index, Within the given program.
   -- The Index must be in the range 1 .. Number_Of_Stacks (Within), else
   -- a Constraint_Error results.


   function Stack_Kind (Index : Stack_Index_T; Within : Program_T)
   return Stack_Kind_T;
   --
   -- The kind of the stack with the given Index, Within the given program.
   -- The Index must be in the range 1 .. Number_Of_Stacks (Within), else
   -- a Constraint_Error results.


   function Stack_Height (Index : Stack_Index_T; Within : Program_T)
   return Storage.Cell_T;
   --
   -- The stack-height cell of the stack with the given Index, Within the
   -- given program.
   --
   -- The Index must be in the range 1 .. Number_Of_Stacks (Within), else
   -- a Constraint_Error results.


   function Stack_Height (Index : Stack_Index_T; Within : Program_T)
   return Arithmetic.Variable_T;
   --
   -- The stack-height cell of the stack with the given Index, Within the
   -- given program, as a variable.
   --
   -- The Index must be in the range 1 .. Number_Of_Stacks (Within), else
   -- a Constraint_Error results.
   --
   -- This function does not allocate more memory; it reuses a predefined
   -- Variable_T object.


   function Stack_Height_Cells (Within : Program_T)
   return Storage.Cell_List_T;
   --
   -- The stack-height cells for all the stacks Within the program.
   -- Both Stable and Unstable stacks are included.


   function Unstable_Stack_Height_Cells (Within : Program_T)
   return Storage.Cell_List_T;
   --
   -- The stack-height cells for all the Unstable stacks Within the program.
   -- The Stable stacks are excluded.


   function Unstable_Stack_Heights_Zero (Within : Program_T)
   return Storage.Bounds.Cell_Interval_List_T;
   --
   -- Bounding the stack-height of each of the Unstable stacks Within
   -- the given program to the single value zero.
   --
   -- This is the assumed initial state for these stack-height cells on
   -- entry to any subprogram.
   --
   -- No bounds are placed (here) on the stack-height of Stable stacks.


   function Stack_Pointer_Cells (Within : Program_T)
   return Storage.Cell_List_T;
   --
   -- The stack-pointer cells for all the stacks Within the program.
   -- Both Stable and Unstable stacks are included, but stacks
   -- without stack pointers are (of course) excluded.


   function Is_Stack_Height (
      Cell   : Storage.Cell_T;
      Within : Program_T)
   return Boolean;
   --
   -- Whether the given Cell represents the local height of some
   -- stack in use Within the given program. In other words, whether
   -- Cell = Height (Stacks (Within)(I)) for some I, or in other words
   -- whether the Cell occurs in Stack_Height_Cells (Within). Both Stable
   -- and Unstable stacks are included.


   procedure Set_Compiler (
      Compiler  : in Compiler_Ref;
      Generated : in Program_T);
   --
   -- Defines the Compiler that Generated this program.


   function Compiler (Item : Program_T) return Compiler_Ref;
   --
   -- The compiler that generated the given program.


   procedure Set_Instruction_Role (
      Address  : in     Processor.Code_Address_T;
      Role     : in     Processor.Instruction_Role_T;
      Within   : in     Program_T;
      Conflict :    out Boolean);
   --
   -- Defines the Role performed by the instruction at the given
   -- Address Within the Program. On success the parameter Conflict
   -- is returned as False.
   --
   -- If the instruction at this Address has already been assigned
   -- a different Role, the Conflict parameter is returned as True
   -- and the role of the instruction is not changed.
   --
   -- If the Role is No_Role, and no role has earlier been set for
   -- this Address, the procedure has no effect and Conflict is
   -- returned as False. This means that a "No_Role" setting can
   -- be overridden by some setting some other role, without
   -- causing a conflict.


   function Instruction_Role (
      Address : Processor.Code_Address_T;
      Within  : Program_T)
   return Processor.Instruction_Role_T;
   --
   -- The role that has been set for the instruction at the
   -- given Address Within this program, or No_Role if no
   -- specific role has been set.


   function Instruction_Role_Was_Used (
      Address : Processor.Code_Address_T;
      Within  : Program_T)
   return Boolean;
   --
   -- Whether the specific role asserted for the instruction at
   -- the given Address, Within this program, has been used
   -- (returned by Instruction_Role).


   --
   ---   Subprogram operations
   --


   No_Subprogram : constant Subprogram_T;
   --
   -- A special (constant) value that indicates the absence
   -- of a subprogram. An error will result if any subprogram
   -- operation is applied to a Subprogram parameter that has
   -- the value No_Subprogram.


   function Program (Subprogram : Subprogram_T) return Program_T;
   --
   -- The program of which the Subprogram is a part.


   function Symbol_Table (Subprogram : in Subprogram_T)
   return Symbols.Symbol_Table_T;
   --
   -- The symbol-table for the program that contains the Subprogram.
   -- Same as Symbol_Table (Program (Subprogram)), but often briefer
   -- when Programs is not directly visible.


   function Index (Subprogram : Subprogram_T)
   return Subprogram_Index_T;
   --
   -- The identifying index of the subprogram, unique within the
   -- program that contains the subprogram. In one Program_T, all
   -- created (analysed) subprograms are numbere 1 ..


   function Name (
      Subprogram : Subprogram_T;
      Qualified  : Boolean   := False;
      Delimiter  : Character := Symbols.Default_Delimiter)
   return String;
   --
   -- Returns the name of the subprogram, optionally qualified
   -- with the scope in which the name is defined.
   -- The qualifier consists of the names of the scope levels
   -- prefixed to the subprogram's unqualified name and separated
   -- with the given delimiter character from each other and from
   -- the unqualified name.
   --
   -- Example unqualified name: "Foo".
   -- Example qualified  name : "Init_Module/Main_Proc/Foo",
   -- assuming the delimiter is '/'.


   function Index_And_Name (Subprogram : Subprogram_T)
   return String;
   --
   -- Describes the identity of the Subprogram in the form
   --     # <index> = <Name, qualified> at <Entry_Address>
   --
   -- Used mostly for tracing output.


   function Entry_Address (Subprogram : Subprogram_T)
   return Processor.Code_Address_T;
   --
   -- The entry address of the subprogram - the instruction from
   -- which any call of the subprogram begins execution.


   function Root (Subprogram : Subprogram_T) return Boolean;
   --
   -- Whether the Subprogram is a root subprogram, that is, has
   -- been declared as a root subprogram by Add_Root.


   function Stub (Subprogram : Subprogram_T) return Boolean;
   --
   -- Whether the Subprogram is a "stub" subprogram, that is, a
   -- subprogram that is not really analysed (decoded into a flow-
   -- graph) but for which resource-usage bounds are asserted.
   -- Stub subprograms are given dummy (stub, synthetic) flow-graphs.


   procedure Set_Stub (
      Subprogram : in Subprogram_T;
      To         : in Boolean);
   --
   -- Declares that the Subprogram is (or is not) a stub subprogram.


   function Unused (Subprogram : Subprogram_T) return Boolean;
   --
   -- Whether the subprogram is asserted to be unused in any relevant
   -- execution, meaning that no call to it is ever executed (all
   -- calls can be considered infeasible) and the subprogram does
   -- not have to be analysed at all.
   --
   -- Every unused subprograms is a stub subprogram, but not
   -- vice versa (because stub subprograms can be called).


   procedure Set_Unused (
      Subprogram : in Subprogram_T;
      To         : in Boolean);
   --
   -- Defines the subprogram as never used (To => True) or
   -- possibly used (To => False).


   function Return_Method (Subprogram : Subprogram_T)
   return Calling.Return_Method_T;
   --
   -- If and how the subprogram returns to its caller.
   --
   -- If the subprogram is Unused then the result is No_Return.
   -- Ann Unused subprogram is never called so it can never return.


   function Returns (Subprogram : Subprogram_T) return Boolean;
   --
   -- Whether the subprogram ever returns to its caller.
   -- Equivalent to Return_Method (Subprogram) /= No_Return.
   --
   -- Subprograms that do not return are usually some kind of
   -- fatal-error or abort handlers, or run-time system routines
   -- to raise(throw exceptions.
   --
   -- If the subprogram is Unused then this function returns false.
   -- An Unused subprogram is never called so it can never return.


   procedure Set_Return_Method (
      Subprogram : in Subprogram_T;
      To         : in Calling.Return_Method_T);
   --
   -- Defines if and how the subprogram returns to its caller.


   function Integrate_Calls (Subprogram : Subprogram_T) return Boolean;
   --
   -- Whether the subprogram should be decoded "integrated", as part of
   -- the flow-graph of any caller.
   --
   -- Such subprograms are usually some kind of "helper" or "wrapper"
   -- that a compiler uses to implemented prelude/postlude code, or
   -- that adapts the caller's protocol to the callee's protocol.
   --
   -- If the subprogram is Unused then this function returns false.
   -- The "unused" property overrides the "integrated" property.


   procedure Set_Call_Integration (
      Subprogram : in Subprogram_T;
      To         : in Boolean);
   --
   -- Defines the subprogram as decoded "integrated" (To => True) or
   -- decoded non-integrated (via a call-step) in the normal way
   -- (To => False).


   procedure Set_Arithmetic_Analysis (
      Subprogram : in Subprogram_T;
      Choice     : in Arithmetic.Opt.Choice_T);
   --
   -- Allows or denies "arithmetic analysis" for this subprogram.


   function Arithmetic_Analysis (Subprogram : Subprogram_T)
   return Arithmetic.Opt.Choice_T;
   --
   -- Whether "arithmetic analysis" is allowed or denied for this
   -- subprogram, depending on the last use of Set_Arithmetic_Analysis.


   function Hide_In_Call_Graph_Drawing (Subprogram : Subprogram_T)
   return Boolean;
   --
   -- Whether the Subprogram should be hidden in the gall-graph and
   -- bounds-graph drawings. If so, also all calls to and from this
   -- Subprogram are hidden.
   --
   -- If the subprogram is Unused then this function returns true.
   -- Unused subprograms are never shown in the call-graph.


   procedure Set_Hiding_In_Call_Graph_Drawing (
      Subprogram : in Subprogram_T;
      To         : in Boolean);
   --
   -- Declares that the subprogram should (or should not) be hidden
   -- in the call-graph and bounds-graph drawings.
   -- To => True means "hide", To => False means "show".


   function Flow_Graph (Subprogram : Subprogram_T)
   return Flow.Graph_T;
   --
   -- The flow-graph of the subprogram, or a reference to it.
   -- Note that Flow.Graph_T has reference semantics, so updates
   -- to the returned value will modify the original flow-graph
   -- (as stored for this subprogram).
   --
   -- When a subprogram is first created, it will have an empty
   -- flow-graph (on just created with Flow.Create).


   function Locus (
      Step   : Flow.Step_T;
      Within : Subprogram_T)
   return Output.Locus_T;
   --
   -- The locus of the given Step, Within the given subprogram.


   procedure Set_Loops (
      Within : in Subprogram_T;
      To     : in Loops.Loops_T);
   --
   -- Defines the loop-structure of the subprogram's flow-graph,
   -- for later retrieval with Loops_In. Also defines the subprogram's
   -- flow-graph as "reducible".


   procedure Set_Irreducible (
      Subprogram : in Subprogram_T);
   --
   -- Marks the subprogram as having an irreducible flow-graph, and
   -- consequently as lacking a loop-structure.


   function Reducible (Subprogram : Subprogram_T) return Boolean;
   --
   -- Whether the subprogram has a reducible control-flow graph and
   -- thus a neatly nested loop-structure.


   function Number_Of_Loops (Subprogram : Subprogram_T)
   return Loops.Loop_Count_T;
   --
   -- The number of loops identified in the subprogram's flow-graph,
   -- if the subprogram is reducible, otherwise zero.


   function Loops_Of (Subprogram : Subprogram_T) return Loops.Loops_T;
   --
   -- The loops identified in the subprogram's flow-graph, if the
   -- subprogram is reducible. Otherwise, a null loop set is returned.


   function Containing_Loops (
      Luup   : Loops.Loop_T;
      Within : Subprogram_T)
   return Loops.Loop_List_T;
   --
   -- The loops that contain the given Luup, listed in top-down
   -- containment order from outermost to innermost. The Luup itself
   -- is not included.
   --
   -- If the result is null, the Luup is not contained in any loop.
   -- Otherwise, the 'First element of the result is the outermost
   -- loop and 'Last is the innermost loop, the one that immediately
   -- contains the Luup.


   function Locus (
      Luup   : Loops.Loop_T;
      Within : Subprogram_T)
   return Output.Locus_T;
   --
   -- Describes the location of the Loop Within its subprogram,
   -- for display purposes.


   function Number_Of_Calls_From (Caller : Subprogram_T) return Natural;
   --
   -- The number of calls, so far found, from the given Caller subprogram
   -- to other callee subprograms.


   function Calls_From (Caller : Subprogram_T) return Call_List_T;
   --
   -- All the calls, so far found, from the given Caller subprogram to
   -- other callee subprograms. The result is indexed by the Call_Index_T
   -- indices of the calls, in 1 .. Number_Of_Calls_From (Caller), so
   -- that Index (Calls_From (Caller)(I)) = I.


   function Call (From : Subprogram_T; Index : Call_Index_T) return Call_T;
   --
   -- The call From a given subprogram, to some callee subprogram, with
   -- the given Index number (within all Calls_From this subprogram).
   --
   -- Same as Calls_From (Subprogram)(Index).


   function Calls_To (Callee : Subprogram_T)
   return Call_List_T;
   --
   -- All the calls so far found to the given Callee subprogram, from
   -- other caller subprograms. Root calls (if Callee is a root) are
   -- not included.


   function Calls (
      From : Subprogram_T;
      Into : Subprogram_Set_T)
   return Call_List_T;
   --
   -- All the calls (found so far) from the given subprogram to some
   -- subprogram in the given subprogram set.


   function Some_Calls (
      From : Subprogram_T;
      Into : Subprogram_Set_T)
   return Boolean;
   --
   -- Whether there are any calls (found so far) from the given
   -- subprogram to some subprogram in the given subprogram set.


   function Calls_Between (
      Program     : Program_T;
      Subprograms : Subprogram_Set_T)
   return Call_List_T;
   --
   -- All the calls between the subprograms in the given set.
   -- In other words, all calls where both the caller and the
   -- callee are in the given set.


   function Processor_Info (Sub : Subprogram_T)
   return Processor.Program.Sub_Info_T;
   --
   -- The processor dependent info on this subprogram.


   procedure Set_Processor_Info (
      Sub : in Subprogram_T;
      To  : in Processor.Program.Sub_Info_T);
   --
   -- Defines the processor-dependent info on this subprogram.


   function Processor_Info (Sub : Subprogram_T)
   return Processor.Program.Info_T;
   --
   -- Short for Processor_Info (Program (Sub)).


   function Initial_Stack_Height (
      Stack    : Stack_T;
      Entering : Subprogram_T)
   return Arithmetic.Value_T;
   --
   -- The amount of stuff that has been pushed on a given Stack when
   -- Entering a given subprogram via its call sequence. That is, the
   -- local stack height just before executing the instruction at the
   -- entry of the subprogram (= the entry step of the flow-graph).
   --
   -- The value does not include further stack usage by the subprogram
   -- itself nor by the other subprograms it calls. It may or may not
   -- include the stacked return address and may include other things
   -- depending on the kind of the Stack.
   --
   -- This is the initial value of the local stack-height for this stack
   -- in this subprogram. The unit of memory usage is target-specific
   -- but will often be octets, for example.
   --
   -- The function depends on the kind of the Stack:
   --
   -- > For a Stable stack the value is derived from the bounds that
   --   the function Processor.Properties.Entry_Bounds puts on the
   --   cell Height (Stack). The Entry_Bounds must define a single
   --   initial value for the stack-height cell, else this operation
   --   emits a fault message and raises Program_Error.
   --   TBM to require a single value only for stacks used with dynamic
   --   calling protocols. For stacks where only a stack-usage bound is
   --   required, the Entry_Bounds could give an interval. This function
   --   should then return Interval_T.
   --
   -- > For an Unstable stack the value is zero. Anything pushed on the
   --   stack before entry to the callee is counted in the stack usage
   --   and local stack height of the caller. This includes the return
   --   address (when stacked) and any stacked parameters.


   function Locus (
      Subprogram : Subprogram_T;
      Qualified  : Boolean := False)
   return Output.Locus_T;
   --
   -- Describes the location of the subprogram in the target
   -- program under analysis, for display purposes.


   function Entry_Locus (
      Subprogram : Subprogram_T;
      Qualified  : Boolean := False)
   return Output.Locus_T;
   --
   -- Describes the location of the subprogram's entry point in
   -- the target program under analysis, for display purposes.
   -- This should be a single statement (instruction) within
   -- the full locus.


   --
   ---   Subprogram-set operations
   --


   procedure Erase (Set : in out Subprogram_Set_T);
   --
   -- Makes the set an empty set.


   function Is_Empty (Set : Subprogram_Set_T) return Boolean;
   --
   -- Whether the set is empty.


   function Is_Member (
      Subprogram : Subprogram_T;
      Of_Set     : Subprogram_Set_T) return Boolean;
   --
   -- Whether the subprogram is a member of the set.


   function Cardinality (Set : Subprogram_Set_T) return Natural;
   --
   -- The number of subprograms in the set.


   procedure Add (
      To     : in out Subprogram_Set_T;
      Adding : in     Subprogram_T);
   --
   -- Adds a subprogram to a subprogram set.


   procedure Add (
      To     : in out Subprogram_Set_T;
      Adding : in     Subprogram_Set_T);
   --
   -- Adds all elements in the second set (Adding) to the
   -- first set (To).


   procedure Remove (
      From     : in out Subprogram_Set_T;
      Removing : in     Subprogram_T);
   --
   -- Removes a subprogram from a subprogram set.


   procedure Remove (
      From     : in out Subprogram_Set_T;
      Removing : in     Subprogram_Set_T);
   --
   -- Removes all elements in the second set (Removing) from
   -- the first set (From).


   function To_List (Item : Subprogram_Set_T) return Subprogram_List_T;
   --
   -- Lists all the subprograms in the set, in some arbitrary order.


   --
   ---   Call operations
   --


   procedure Identify_Callee (
      Caller : in     Subprogram_T;
      Target : in     Processor.Code_Address_T;
      Info   : in     Processor.Program.Sub_Info_T;
      Callee :    out Subprogram_T);
   --
   -- Finds or creates the Callee subprogram for the call to the
   -- Target address from the given Caller subprogram.
   --
   -- Caller
   --    The caller subprogram.
   -- Target
   --    The entry address of the callee = the target address of
   --    the call instruction.
   -- Info
   --    The target-specific information for the Callee, derived from
   --    the existence and form of this call instruction.
   --    If the Callee subprogram is a new one and is created here,
   --    the Info is used to initialize the Callee information.
   --    Otherwise, the Info is validated against the existing Callee's
   --    information, to check for conflicts etc in calling protocols.
   --    The existing Callee's information may be updated.
   -- Callee
   --    Returns the callee subprogram. The subprogram (object) is
   --    created if it did not already exist.
   --
   -- A newly created Subprogram is marked as not a stub; irreducible;
   -- not called; not unused; not to be decoded in the integrated way;
   -- and not to be hidden in the call-graph.


   procedure Add_Call (
      Caller   : in     Subprogram_T;
      Callee   : in     Subprogram_T;
      Protocol : in     Calling.Protocol_Ref;
      Step     : in     Flow.Step_T;
      Giving   :    out Call_T);
   --
   -- Creates a new call from the Caller subprogram to a Callee
   -- subprogram using a given primitive calling protocol.
   --
   -- The calling Step in the Caller's flow-graph is also given.
   -- This is the synthetic "call-step", not the step that models
   -- the call instruction itself.
   --
   -- The new call is returned in Giving. It is assigned the next
   -- available index among the calls in the Caller; the first call
   -- gets index 1.
   --
   -- If this is the first call to the Callee, the Callee is added
   -- to the set of "shoot subprograms" in the program.


   function Program (Call : Call_T) return Program_T;
   --
   -- The program in which the Call occurs.


   function Image (Call : Call_T) return String;
   --
   -- Image of the call. Null string for No_Call.


   function Caller (Call : Call_T) return Subprogram_T;
   --
   -- The calling subprogram.


   function Callee (Call : Call_T) return Subprogram_T;
   --
   -- The called subprogram.


   function Unused (Call : Call_T) return Boolean;
   --
   -- Whether the call is known or asserted to be unused, that is
   -- never executed within any execution that should be analysed,
   -- because the callee subprogram has this property. Thus, this
   -- function is equivalent to Unused (Callee (Call)).


   function Index (Call : Call_T) return Call_Index_T;
   --
   -- The unique index of the Call within all calls from Caller (Call).
   -- The value is in the range 1 .. Number_Of_Calls (Caller (Call)).


   function Step (Call : Call_T) return Flow.Step_T;
   --
   -- The step in the caller's flow-graph that contains the call.


   function Steps (Calls : Call_List_T) return Flow.Step_List_T;
   --
   -- The steps in the callers' flow-graphs that contain the calls.
   -- This is probably useful only if all the listed Calls have the
   -- same caller.


   function Node (Call : Call_T) return Flow.Node_T;
   --
   -- The node in the caller's flow-graph that contains the call.


   function Nodes (Calls : Call_List_T) return Flow.Node_List_T;
   --
   -- The nodes in the callers' flow-graphs that contain the calls.
   -- This is probably useful only if all the listed Calls have the
   -- same caller.


   function Take_Off_Node (Call : Call_T) return Flow.Node_T;
   --
   -- The node in the caller's flow-graph that immediately precedes
   -- the execution of the callee; the last node executed in the caller
   -- before control passes to the callee.
   --
   -- Note that this function cannot be applied to No_Call.


   function Take_Off_Node (
      Call   : Flow.Node_T;
      Within : Flow.Graph_T)
   return Flow.Node_T;
   --
   -- The node in the caller's flow-graph that immediately precedes
   -- the execution of the callee for the given Call node; the last
   -- node executed in the caller before control passes to the callee.
   --
   -- Note that this function can be applied even to unresolved
   -- dynamic calls (where the call in the node is No_Call).


   function Prime_Address (Call : Call_T) return Processor.Code_Address_T;
   --
   -- The address of the call instruction.


   function Protocol (Call : Call_T) return Calling.Protocol_Ref;
   --
   -- The primitive calling protocol used by the Call.


   function Locus (Call : Call_T) return Output.Locus_T;
   --
   -- Describes the location of the call in the target program
   -- under analysis, for display purposes.
   -- This location is in the calling subprogram - the caller,
   -- not the callee. Use the corresponding call-path (containing
   -- just this call) for a location in the callee.


   function Non_Looped_Calls (Subprogram : Subprogram_T)
   return Call_List_T;
   --
   -- The calls in the Subprogram that are not contained in any
   -- loops and are therefore executed at most once per execution
   -- of the Subprogram.


   function Containing_Loops (Call : Call_T)
   return Loops.Loop_List_T;
   --
   -- The loops that contain the given Call, listed in top-down
   -- containment order from outermost to innermost.
   --
   -- If the result is null, the Call is not contained in any loop.
   -- Otherwise, the 'First element of the result is the outermost
   -- loop and 'Last is the innermost loop, the one that immediately
   -- contains the Call. However, this loop may contain other loops
   -- that are not listed (because they do not contain the Call).


   function Calls_Within (
      Luup : Loops.Loop_T;
      From : Subprogram_T)
   return Call_List_T;
   --
   -- The calls From the given subprogram that are contained
   -- within the given Luup (one of the loops in this subprogram,
   -- of course).


   --
   ---   Support for code patching
   --


   type Code_Address_List_T is
      array (Positive range <>) of Processor.Code_Address_T;
   --
   -- A list of code addresses. For example, parameters for the
   -- code-patching functions.


private

   -- The Program type is deferred to the body:

   type Program_Object_T;

   type Program_T is access Program_Object_T;


   -- The Subprogram type is deferred to the body:

   type Subprogram_Object_T;

   type Subprogram_T is access Subprogram_Object_T;

   No_Subprogram : constant Subprogram_T := null;


   -- The Call type is deferred to the body:

   type Call_Object_T;

   type Call_T is access Call_Object_T;

   No_Call : constant Call_T := null;

   No_Calls : constant Call_List_T := (1 ..0 => No_Call);


   -- The Subprogram-Set type is deferred to the body:

   type Subprogram_Set_Object_T;

   type Subprogram_Set_T is access Subprogram_Set_Object_T;


   -- The Stack type is defined here:

   type Stack_Name_Ref is access String;
   --
   -- Stack names are stored on the heap.

   type Stack_T is record
      Kind         : Stack_Kind_T;
      Name         : Stack_Name_Ref;
      Height_Unit  : Arithmetic.Positive_Value_T;
      Pointer_Cell : Storage.Cell_T;
      Pointer_Var  : Arithmetic.Variable_T;
      Coupling     : Arithmetic.Algebra.Coupling_T;
      Height_Cell  : Storage.Cell_T;
      Height_Var   : Arithmetic.Variable_T;
      Net_Change   : Storage.Bounds.Limit_T;
      Index        : Stack_Index_T;
   end record;


   --
   ---   Private operations for use in child packages
   --


   function Call_Arrow (Call : Call_T) return String;
   --
   -- The part of a call- or call-path image that shows the locus
   -- of this call and the name of the callee. It has the form
   --
   --     @call_locus=>callee
   --
   -- It is a precondition that Call is not a root call.


end Programs;
