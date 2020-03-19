-- Assertions.Own (decl)
--
-- Private, own data structures for the Assertions subsystem.
--
-- An assertion states a certain fact, about a certain program part or
-- about a set of program parts, in a certain execution context.
--
-- Facts:
--    An (assertion) fact is a self-contained statement; its meaning
--    does not depend on the program part to which it applies. However,
--    some kinds of facts cannot apply to all kinds of program parts.
--
-- Program parts:
--    A program part is identified by some of its features. Some
--    features are intrinsic to the part, eg. the (full) name of a
--    subprogram. Other features express a relationship of this part
--    to other parts, eg. the nesting of loops.
--
--    Relationships between program parts can be recursive. For example.
--    a loop can be (in part) identified by the feature of containing
--    a certain kind of call, while this call is (in part) identified
--    by the feature of being contained in the loop. When a program
--    part is located through its features, such recursive cycles are
--    broken by a depth-first traversal of the relationship graph.
--
--    An assertion can apply to a set of program parts instead of one
--    specific part. This happens simply when the features specified
--    for the program part are so loose that they hold for several
--    program parts. An assertion can put limits on the number of
--    program parts to which it applies (the size of the parts set).
--    If the limit is "one part", the features must be strong enough
--    to select exactly one such part.
--
-- Execution contexts:
--    An execution context specifies the call path, or a suffix of the
--    call path, to the subprogram that contains (or is) the program
--    part in question.
--
-- An assertion set contains a set of assertions. The assertions in a
-- set are in principle independent of each other; in practice they
-- will refer to descriptions of program parts that are partly shared
-- between the assertions in the set. For example, several assertions
-- on loop repetitions may identify the loops as being contained in the
-- same subprogram (subprogram predicate). However, this sharing of part
-- descriptions is relevant only for breaking circles of recursive
-- part relationships.
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
-- $Revision: 1.18 $
-- $Date: 2015/10/24 19:36:46 $
--
-- $Log: assertions-own.ads,v $
-- Revision 1.18  2015/10/24 19:36:46  niklas
-- Moved to free licence.
--
-- Revision 1.17  2012-02-19 18:42:20  niklas
-- BT-CH-0231: Fix Constraint_Error in -trace parse, from BT-CH-0227.
--
-- Revision 1.16  2012-02-04 09:55:23  niklas
-- BT-CH-0227: Avoid GNAT bug re Unbounded_String in discr. record.
--
-- Revision 1.15  2010-01-30 21:13:29  niklas
-- BT-CH-0216: Subprograms have a Return_Method_T attribute.
--
-- Revision 1.14  2009-12-17 14:05:54  niklas
-- BT-CH-0197: Assertions on instruction roles.
--
-- Revision 1.13  2009-12-16 12:23:14  niklas
-- BT-CH-0196: Identify subprogram by offset from other subprogram.
--
-- Revision 1.12  2009-03-24 07:48:34  niklas
-- BT-CH-0166: String_Pool.Item_T for source-file and marker names.
--
-- Revision 1.11  2009/03/20 18:19:29  niklas
-- BT-CH-0164: Assertion context identified by source-line number.
--
-- Revision 1.10  2008/11/03 07:58:12  niklas
-- BT-CH-0155: Ignore assertions on absent subprograms.
--
-- Revision 1.9  2008/09/24 08:38:50  niklas
-- BT-CH-0146: Assertions on "loop starts <bound> times".
-- BT-CH-0146: Loop-repeat assertions set both lower and upper bound.
-- BT-CH-0146: Report locations of contradictory "count" assertions.
-- BT-CH-0146: Contradictory "count" assertions imply infeasibility.
--
-- Revision 1.8  2008/09/19 10:34:14  niklas
-- BT-CH-0144: Assertion "populations" work again, and more.
--
-- Revision 1.7  2008/07/14 19:16:54  niklas
-- BT-CH-0135: Assertions on "instructions".
--
-- Revision 1.6  2007/12/17 13:54:33  niklas
-- BT-CH-0098: Assertions on stack usage and final stack height, etc.
--
-- Revision 1.5  2007/04/24 08:14:39  niklas
-- Added an Image function for Assertion_Subset_T that shows
-- the source location of each assertion in the subset.
--
-- Revision 1.4  2007/01/25 21:25:12  niklas
-- BT-CH-0043.
--
-- Revision 1.3  2006/10/28 19:52:15  niklas
-- BT-CH-0031.
--
-- Revision 1.2  2006/05/29 11:22:33  niklas
-- BT-CH-0023.
--
-- Revision 1.1  2006/05/27 21:26:38  niklas
-- First version for BT-CH-0020.
--


with Assertions.Opt;
with Calling;
with Flow.Computation;
with Flow.Execution;
with Loops;
with Output;
with Processor;
with Programs;
with Storage;
with Storage.Bounds;
with String_Pool;
with Unbounded_Vectors;


private
package Assertions.Own is


   --
   ---   Locations in assertion (source) files
   --


   type File_Name_Ref is access String;
   --
   -- A reference to the name of an assertion file or some
   -- other kind of text file.


   type Source_T is record
      File_Name   : File_Name_Ref;
      Line_Number : Positive;
   end record;
   --
   -- Many elements in the internal data structure are traced back to a
   -- source, which is identified as a specific line in a specific
   -- assertion file.
   --
   -- This type is not used to identify lines in source-code files.
   -- See below, Source_Position_T.


   function Image (Item : Source_T) return String;
   --
   -- The file-name and line-number, separated by the output separator
   -- character (Output.Field_Separator).


   function Locus (Item : Source_T) return Output.Locus_T;
   --
   -- The output locus that identifies the source of an assertion
   -- in terms of the assertion file name and the line number in the
   -- file.


   --
   ---  Facts that can be asserted
   --


   type Fact_Kind_T is (
      Variable_Invariance,
      Variable_Value,
      Property_Value,
      Execution_Time,
      Stack_Final,
      Stack_Usage,
      Loop_Repetitions,
      Loop_Starts,
      Call_Executions,
      Instruction_Executions,
      Instruction_Role,
      Return_Method,
      Subprogram_Arithmetic,
      Subprogram_Enough_For_Time,
      Subprogram_Integrate,
      Subprogram_Omit,
      Subprogram_Unused,
      Subprogram_Hide,
      Call_Callees,
      Jump_Targets);
   --
   -- The kinds of facts that can be asserted.
   --
   -- Variable_Invariance
   --    The fact that a given variable does not change its value
   --    when the program part is executed. This fact can apply
   --    to a call, a loop or a subprogram or to each of a set of
   --    calls, loops or subprograms.
   --
   -- Variable_Value
   --    The fact that the value of a given variable is in a given
   --    range (interval) when the program part is executed.
   --
   -- Property_Value
   --    The fact that the value of a given target-specific property
   --    is in a given range (interval) when the program part is
   --    executed.
   --
   -- Execution_Time
   --    The fact that the execution time of the program part is
   --    in a given range (interval). This fact can apply to a
   --    subprogram (or to each of a set of subprograms) or to a
   --    call (or to each of a set of calls).
   --
   -- Stack_Final
   --    The fact that the final local height of a given stack, on
   --    return from a given subprogram or a given call, is in a
   --    given range (interval). This fact is applicable only to
   --    stacks of the Unstable kind because for Stable stacks any
   --    subprogram/call is assumed to preserve the local stack height
   --    from the caller's point of view.
   --
   -- Stack_Usage
   --    The fact that the amount of space on a given stack that a
   --    program part uses, including the space used by its callees
   --    if any, is in a given range (interval).
   --    This fact can apply to a subprogram (or to each of a set
   --    of subprograms) or to a call (or to each of a set of calls).
   --
   -- Loop_Repetitions
   --    The fact that the number of repetitions of a given loop is
   --    in a given range (interval). This fact can apply only to
   --    a loop, separately to each loop in a set of loops, or to
   --    the total number of repetitions of all loops in a set of
   --    loops.
   --
   -- Loop_Starts
   --    The fact that the number of times a given loop is started
   --    (entered from outside the loop) is in a given range (interval).
   --    This fact can apply to a loop, separately to each loop in a
   --    set of loops, or to the total number of starts of the loops
   --    in a set of loops.
   --
   -- Call_Executions
   --    The fact that the number of executions of a given call is
   --    in a given range (interval). This fact can apply to a call,
   --    separately to each call in a set of calls, or to the total
   --    number of executions of all calls in a set of calls.
   --
   -- Instruction_Executions
   --    The fact that the number of executions of a given instruction
   --    is in a given range (interval). This fact can apply only to
   --    one instruction.
   --
   -- Instruction_Role
   --    The role performed by a given instruction in the computation
   --    of the target program. For example, a call, or a branch, or
   --    a return. This fact can apply only to one instruction.
   --
   -- Return_Method
   --    If and how a given subprogram can return to the caller.
   --    This fact can apply to a subprogram or to each subprogram
   --    in a set of subprograms.
   --    TBD if it can apply to calls.
   --
   -- Subprogram_Arithmetic
   --    This is more an option than a fact; it tells us to use or
   --    not to use arithmetic analysis for some subprogram(s).
   --
   -- Subprogram_Enough_For_Time
   --    This is more an option than a fact; it tells us to attempt
   --    IPET to bound the execution paths in the subprogram even if
   --    the subprogram is irreducible or has unbounded loops, because
   --    it says that other execution-count assertions on the subprogram
   --    are strong enough to bound the paths.
   --
   -- Subprogram_Integrate
   --    This is more an option than a fact; it tells us to model
   --    calls to some subprogram(s) by integrating the subprogram
   --    in the caller's flow-graph rather than making a reference
   --    from the caller's flow-graph to the callee's flow-graph.
   --
   -- Subprogram_Omit
   --    This is more an option than a fact; it tells us to assume
   --    that certain subprogram(s) shall be omitted from the analysis
   --    and replaced by stubs that have whatever properties are
   --    additionally asserted for the subprogram(s).
   --
   -- Subprogram_Unused
   --    This is more an option than a fact; it tells us to assume
   --    that certain subprogram(s) are never used (called) in the
   --    program under analysis. It is equivalent to asserting a zero
   --    execution count for all calls of this (these) subprogram(s)
   --    and the "omit" property for this (these) subprogram(s).
   --
   -- Subprogram_Hide
   --    This is more an option than a fact; it tells us to hide
   --    a certain subprogram in the call-graph and bounds-graph
   --    drawings. All calls to and from this subprogram are also
   --    hidden. Call-graphs of large programs can be significantly
   --    simplified and clarified by hiding widely used but dull
   --    subprograms such as floating-point library functions.
   --
   -- Call_Callees
   --    The possible callees of a dynamic call (a call thru some
   --    form of subprogram pointer).
   --
   -- Jump_Targets
   --    The possible targets of a dynamic jump.


   subtype Count_Fact_Kind_T
      is Fact_Kind_T range Loop_Repetitions .. Instruction_Executions;
   --
   -- A fact relating to an execution count or repetition count.


   type Variable_Name_Kind_T is (Name, Address);
   --
   -- In an assertion, a variable can be named with its symbolic
   -- name (source-level identifier) or with a machine-level
   -- address.


   type Name_String_T is new String_Pool.Item_T;
   --
   -- A string that defines the name or address of a variable.


   function To_String (Item : Name_String_T) return String;
   --
   -- Like String_Pool.To_String, but gives "" for the Null_Item.


   function To_Name (Item : String) return Name_String_T
   renames To_Item;


   type Variable_Name_T (Kind : Variable_Name_Kind_T := Name) is record

      Location : Storage.Location_Ref;

      case Kind is
      when Name    => Name    : Name_String_T;
      when Address => Address : Name_String_T;
      end case;

   end record;
   --
   -- Location
   --    The machine location(s) of the variable.
   -- Name
   --    The symbolic name (source-level identifier) of the variable,
   --    possibly including one or more scope levels, exactly as it was
   --    written in the assertion file.
   --    The Location of the variable is extracted from the symbol-table
   --    (debugging information).
   -- Address
   --    The textual string that identifies a machine-level storage location
   --    exactly as it appeared in the assertion file.
   --    The function Processor.Properties.Variable_Cell computes the
   --    Location defined by the Address. The Location usually does not
   --    depend on the code address (execution point), but may be defined
   --    only for a subset (one or more ranges) of code addresses.


   function Image (Item : Variable_Name_T) return String;
   --
   -- Describes the name or address and location of the Item.


   type Fact_Span_T is (Initially, Always, Finally);
   --
   -- Some facts can be valid Always during execution of a program part,
   -- or Initially on entry to the part, or Finally on exit from the
   -- program part.


   type Subprogram_List_Ref is access Programs.Subprogram_List_T;
   --
   -- A reference to a list of subprograms on the heap.
   -- For example, the callees of a dynamic call.
   -- The list order is usually irrelevant.


   type Fact_T (Kind : Fact_Kind_T := Subprogram_Unused) is
   record

      Source : Source_T;

      case Kind is

      when Variable_Invariance =>

         Invariant_Var_Name : Variable_Name_T;

      when Variable_Value =>

         Var_Name  : Variable_Name_T;
         Var_Value : Storage.Bounds.Interval_T;
         Span      : Fact_Span_T;

      when Property_Value =>

         Property   : Processor.Property_T;
         Prop_Value : Storage.Bounds.Interval_T;

      when Execution_Time =>

         Time : Time_Bound_T;

      when Stack_Final | Stack_Usage =>

         Stack_Index : Programs.Stack_Index_T;
         Stack_Value : Storage.Bounds.Interval_T;

      when Count_Fact_Kind_T =>

         Count : Flow.Execution.Bound_T;

      when Instruction_Role =>

         Role : Processor.Instruction_Role_T;

      when Return_Method =>

         Return_Method : Calling.Return_Method_T;

      when Subprogram_Arithmetic =>

         Use_Arithmetic : Boolean;

      when Subprogram_Enough_For_Time =>

         Enough_For_Time : Boolean;

      when Subprogram_Integrate
         | Subprogram_Omit
         | Subprogram_Unused =>

         null;

      when Subprogram_Hide =>

         Hide : Boolean;

      when Call_Callees =>

         Callees : Subprogram_List_Ref;

      when Jump_Targets =>

         null;  -- TBA targets for dynamic jump.

      end case;

   end record;
   --
   -- An asserted fact.
   --
   -- The Source component shows the (closest) point in some
   -- assertion file where the fact is stated.


   --
   ---   Program parts to which assertions can apply
   --


   type Part_Kind_T is (
      Program,
      Subprogram,
      Luup,
      Call_Static,
      Call_Dynamic,
      Jump,
      Instruction,
      Reading,
      Writing);
   --
   -- The kinds of program parts for which facts can be asserted or
   -- that can be a "contained" part as a feature of the "containing"
   -- part, or vice versa.
   --
   -- Program
   --    The whole program under analysis.
   --    Useful in a global assertion.
   --    Not useful in a part-relationship feature.
   --
   -- Subprogram
   --    A subprogram.
   --
   -- Luup
   --    A loop, including head, body and repetition edges.
   --
   -- Call_Static
   --    A call of a subprogram (callee) from another subprogram (caller).
   --    This part is executed when control passes from the caller to
   --    the callee. This part does not contain any instructions; it
   --    is an atomic point in the execution. The single callee of
   --    a Call_Static is statically known; an assertion on Call_Callees
   --    is not applicable.
   --
   -- Call_Dynamic
   --    A call of a subprogram (callee) from another subprogram (caller).
   --    The caller is known statically; the callee is computed
   --    dynamically (call thru function pointer). An assertion on
   --    Call_Callees may be useful.
   --
   -- Jump
   --    A jump from one place in the program (source) to another
   --    place (target). This part is executed when control passes
   --    from the source to the target. This part does not contain
   --    any instructions; it is an atomic point in the execution.
   --    A jump is usually interesting (for assertions) only if the
   --    target of the jump is computed dynamically (indexed jump,
   --    register-indirect jump) when an assertion on Jump_Targets
   --    may be useful.
   --
   -- Instruction
   --    Any sort of instruction, identified by its address or by
   --    its offset from the start of the subprogram.
   --
   -- Reading
   --    An instruction that really reads (loads) the value of a
   --    given variable, usually a memory location rather than a
   --    register.
   --
   -- Writing
   --    An instruction that really writes (stores) a value into a
   --    given variable, usually a memory location rather than a
   --    register.


   subtype Actual_Part_Kind_T is Part_Kind_T range
      Program .. Instruction;
   --
   -- The kinds of parts that are actually analysed so that
   -- we need to look up assertions that apply to these parts.


   subtype Extended_Part_Kind_T is Actual_Part_Kind_T range
      Program .. Luup;
   --
   -- A part that is "extended" in the sense of usually containing
   -- more that one instruction or flow-graph step.


   subtype Point_Part_Kind_T is Actual_Part_Kind_T range
      Call_Static .. Instruction;
   --
   -- A part that is "pointlike" in the sense that it consists of
   -- just one instruction, thus (usually) of just one flow-graph step.


   subtype Inner_Part_Kind_T is Actual_Part_Kind_T range
      Luup .. Jump;
   --
   -- The kinds of actual parts contained in some outer part and
   -- that can be listed as such (see function Parts_Within below).


   subtype Mappable_Part_Kind_T is Actual_Part_Kind_T range
      Luup .. Call_Static;
   --
   -- The kinds of actual parts contained within a subprogram that
   -- can be the subjects of assertion mapping, that is, for which
   -- the mapping process identifies the subsets of applicable
   -- assertions.
   --
   -- TBA: Include Call_Dynamic.


   type Part_Predicate_T;
   --
   -- A predicate that defines a set of program parts by specifying
   -- some features of the parts. See below for full definition.


   type Part_Predicate_Ref is access Part_Predicate_T;
   --
   -- A reference to a Part_Predicate_T.


   type Parts_T (Kind : Part_Kind_T := Program) is
   record

      Source : Source_T;

      Predicate : Part_Predicate_Ref;

      Population : Storage.Bounds.Interval_T;

      case Kind is

      when Program
         | Subprogram
         | Luup
         | Call_Static
         | Call_Dynamic
         | Jump =>

         null;

      when Reading
         | Writing =>

         Variable : Variable_Name_T;

      when Instruction =>

         Address : Processor.Code_Address_T;

      end case;

   end record;
   --
   -- Denotes a (set of) program parts of a particular Kind and
   -- satisfying a Predicate over this kind of parts. For some
   -- Kinds of parts the obligatory properties of the part are
   -- represented directly as components, eg. the Variable for
   -- Reading or Writing.
   --
   -- Source
   --    Shows the (closest) point in some assertion file where
   --    the part is defined or specified.
   -- Predicate
   --    The features that the part(s) must have.
   --    Never null; if there are no constraining features, use
   --    the True_Predicate (see below). See note below.
   -- Population
   --    The number of parts expected to match the Predicate.
   -- Variable
   --    The variable that is read or written in the instruction
   --    that forms this part.
   -- Address
   --    The (prime) address of the instruction.
   --
   -- Note that when we combine Parts_T and Feature_T (through the
   -- Predicate) mutually recursive, cyclic structures can arise, where
   -- a "containing" part has a Predicate that requires a certain
   -- "contained" part that in turn has a Predicate that requires
   -- the _same_ "containing" part.
   --
   -- Such circular relationships arise when the assertion language
   -- nests assertions on different parts and uses the implicit
   -- container-contained relationships. For example, an assertion
   -- like the following:
   --
   --    subprogram "*"
   --       <subprogram facts>
   --       loop
   --          call to "Bar" <call facts> end call;
   --          <loop facts>
   --       end loop;
   --    end subprogram;
   --
   -- creates three predicates P1, P2 and P3 for the subprogram, the
   -- loop and the call, respectively, and three (sets of) assertions:
   -- the <subprogram facts> that apply to any subprogram satisfying P1,
   -- the <loop facts> that apply to any loop satisfying P2, and the
   -- <call facts> that apply to any call satisfying P3. The predicates
   -- are mutually recursive as follows:
   --
   -- > P1 requires the subprogram to contain a loop that satisfies P2.
   -- > P2 requires the loop to be contained in a subprogram that
   --   satisfies P1 and to contain a call that satisfies P3.
   -- > P3 requires the call to be contained in a loop that satisfies P2.
   --
   -- When we check if a given assertion on <subprogram facts> applies
   -- to a specific subprogram S, we apply P1 to S; as part of this,
   -- if S contains a loop L we apply P2 to this loop; as part of this,
   -- we should check if the containing subprogram S satisfies P1 -- but
   -- we are _already_ doing that, so we do not have to do it again.
   -- Likewise, as part of applying P2 to the loop and if the loop
   -- contains a call we apply P3 to this call; as part of this, we
   -- should check that the containing loop L satisfies P2 -- but we
   -- are _already_ doing that, so we do not do it again. The predicate
   -- checker thus maintains a stack of goals, where a goal is to check
   -- a certain predicate on a certain part, and never pushes a
   -- duplicate goal on the stack.


   type Parts_Ref is access Parts_T;
   --
   -- A reference to a description of a set of program parts.


   --
   ---   Features of program parts
   --


   type Feature_Kind_T is (
      Subprogram_Identity,
      Subprogram_Absent,
      Named,
      Contains,
      Within,
      Uses,
      Defines,
      Eternal,
      Sourced,
      Labelled,
      Executes);
   --
   -- The kinds of features that program parts (may) have.
   --
   -- Subprogram_Identity
   --    The subprogram that identifies the part.
   --    For a subprogram part, this is the subprogram itself.
   --    For a call part, this is the callee subprogram.
   -- Subprogram_Absent
   --    A subprogram that does not exist, because no subprogram
   --    matches the given name or name-pattern.
   -- Named
   --    The part has, or matches, a given name or name-pattern.
   --    For Calls this is the name of the callee subprogram.
   --    For Jumps this is the name (label) of the target.
   -- Contains
   --    The part contains some other part(s).
   -- Within
   --    The part is contained in some other part(s).
   --    TBD directly or indirectly.
   -- Uses
   --    The part uses (reads) a given variable.
   --    Similar to Contains (Reading of this variable).
   -- Defines
   --    The part defines (writes) a given variable.
   --    Similar to Contains (Writing of this variable).
   -- Eternal
   --    The part is an eternal loop.
   -- Sourced
   --    The part contains or is close to the instruction or instructions
   --    that are connected to a given source-code line (source file
   --    name, line number), or connected to the source-code line (source
   --    file name, line number) that holds a given symbolic "marker".
   --    Markers are specially formatted comments in the source-code
   --    files; an auxiliary Bound-T tool finds the markers and records
   --    the source file name and line number for each marker.
   -- Labelled
   --    The part contains the instruction (code address) connected
   --    to a given symbolic label.
   -- Executes
   --    The part contains (executes) the instruction at a given code
   --    address. This is usually a last resort to identify a part.


   type Part_Name_Kind_T is (Name, Pattern, Address);
   --
   -- The "name" feature of a program part can be given in several
   -- ways; see Part_Name_T below.


   type Part_Name_T (Kind : Part_Name_Kind_T := Name)
   is record
      Delimiter : Character;
      Source    : Source_T;
      Offset    : Processor.Code_Offset_T;
      case Kind is
      when Name    => Name    : Name_String_T;
      when Pattern => Pattern : Name_String_T;
      when Address => Address : Name_String_T;
      end case;
   end record;
   --
   -- A predicate for the "name" feature of a program part. The
   -- "name" feature applies to subprograms, labels and the target
   -- of a jump or call.
   --
   -- Delimiter
   --    The scope-delimiter character that was in effect
   --    when this part-name occurred in an assertion file.
   -- Source
   --    The place of the name in an assertion file.
   -- Offset
   --    A possible offset to be added to the address of the program
   --    part identified by the Name, Pattern, or Address, to form
   --    the address of the finally identified program part.
   -- Name
   --    The (single) target that has this Name, which is possibly
   --    qualified identifier using the given scope Delimiter.
   --    However, the Offset may shift the identification to
   --    another, offset target.
   -- Pattern
   --    Any target that has a name that matches this Pattern.
   --    However, the Offset may shift the identification to
   --    another, offset target.
   -- Address
   --    The (single) target that has this Address, plus the Offset.
   --    TBD if the Address can express a set or range of addresses
   --    and thus select several parts.


   function Plain_Image (Item : Part_Name_T) return String;
   --
   -- The name, pattern or address as a string with no information
   -- as to kind or delimiter. For use in output loci, for example.


   type Source_Fuzz_T is new Storage.Bounds.Interval_T;
   --
   -- The allowed fuzz, or error, when comparing the source-line number (P)
   -- of an actual program part, to the source-line number (M) given in
   -- an assertion or a marker definition. The comparison matches if
   -- the difference P - M is in the range Min .. Max.
   --
   -- For example:
   --    > marker at line 23 = M
   --    > call instruction mapped to line 27 = P
   --    > thus P - M = 27 - 23 = 4.
   --
   -- Thus, if the fuzz is 0..6 the call matches the marker.
   -- If the fuzz is 0..3, the call does not match the marker.


   function Match (
      Part : Line_Number_T;
      Mark : Line_Number_T;
      Fuzz : Source_Fuzz_T)
   return Boolean;
   --
   -- Whether the Part line-number, associated with some instruction of
   -- an actual target-program part, matches the asserted or Marked
   -- line-number, to within the given Fuzz.


   type Source_Position_Kind_T is (Line, Mark, Any);
   --
   -- A position in a source-code file can be identified by its Line
   -- number, by its Marker, or not at all, which matches Any point
   -- in some given source-code file.


   type Source_Position_T (Kind : Source_Position_Kind_T := Any)
   is record
      File : Source_File_Name_T;
      Fuzz : Source_Fuzz_T;
      case Kind is
      when Line => Line_Number : Line_Number_T;
                   Offset      : Boolean;
      when Mark => Marker      : Marker_Name_T;
      when Any  => null;
      end case;
   end record;
   --
   -- Identifies zero or more positions in some source-code file(s)
   -- by one of three means: by Line number, or by Marker, or as Any
   -- point in the file(s).
   --
   -- File
   --    The name of the source-code file, if not null.
   --    If null, any file may match.
   --    Must not be null when Kind = Any.
   -- Fuzz
   --    The error allowed in line-number matching.
   --    Irrelevant if Kind = Any.
   -- Line_Number
   --    The (absolute) line number.
   -- Offset
   --    Whether the Line_Number was defined as an absolute number,
   --    or by an offset added to a base line-number.
   --
   -- The combination Kind = Any, File = null is not allowed because
   -- it would given no information. There are thus basically five
   -- different forms, according to the Kind:
   --
   -- Any
   --    Any position in the given File (which is not null).
   -- Line with File not null
   --    This Line in this File
   -- Line with File null
   --    This Line in any file. However, in most cases there will be
   --    only one possible file, the one that contains the subprogram
   --    in which this part is (also) required to lie.
   -- Mark with File not null
   --    Any Marker line in this File.
   -- Mark with File null
   --    Any Marker line in any file.


   type Feature_T (Kind : Feature_Kind_T := Named)
   is record

      Source  : Source_T;
      Negated : Boolean := False;

      case Kind is

      when Subprogram_Identity =>

         Subprogram : Programs.Subprogram_T;

      when Subprogram_Absent | Named =>

         Name : Part_Name_T;

      when Contains =>

         Parts : Parts_Ref;

      when Within =>

         Whole : Parts_Ref;

      when Uses
         | Defines =>

         Variable : Variable_Name_T;

      when Eternal =>

         null;

      when Sourced =>

         Source_Rel : Source_Relation_T;
         Source_Pos : Source_Position_T;

      when Labelled =>

         Label         : Name_String_T;
         Label_Address : Processor.Code_Address_T;

      when Executes =>

         Address : Processor.Code_Address_T;

      end case;

   end record;
   --
   -- A feature of a program part.
   --
   -- Source
   --    Shows the (closest) point in some assertion file where
   --    the feature is defined or specified.
   -- Negated
   --    The feature is logically negatied. For example, the part
   --    is _not_ Named by Name, etc.
   --
   -- In the following we assume Negated is False.
   --
   -- Subprogram
   --    The part is (for a subprogram) or calls (for a call)
   --    this subprogram. Never No_Subprogram.
   -- Name
   --    The part has or matches this Name.
   --    For a Call part, the Name is for the callee.
   --    For a Jump part, the Name is for the jump target.
   --    For a Subprogram_Absent feature, we know that no subprogram
   --    matches this name.
   -- Parts
   --    The parts that must be (directly TBD) contained in this part.
   -- Whole
   --    The part that must (directly TBD) contain this part.
   -- Variable
   --    The variable that must used or defined in this part.
   -- Source_Rel, Source_Pos
   --    A source-code position and the relationship this position
   --    should have to this part.
   -- Label, Label_Address
   --    The symbolic identifier of the label that corresponds to
   --    the Label_Address that should be the address of an
   --    instruction in this part.
   -- Address
   --    The address of the instruction that this part should contain.


   --
   ---   Predicates (descriptions) for (sets of) program parts
   --


   type Part_Predicate_T is array (Positive range <>) of Feature_T;
   --
   -- A predicate is the conjunction of a list of features.


   True_Predicate : constant Part_Predicate_Ref :=
      new Part_Predicate_T (1 .. 0);
   --
   -- A predicate (ref) that is always True.


   --
   ---   Calling contexts
   --


   type Calling_Context_T is new Name_String_T;
   --
   -- Describes one, or a set of calling contexts using an
   -- unspecified format TBA.


   No_Context : constant Calling_Context_T :=
      Calling_Context_T (String_Pool.Null_Item);
   --
   -- A universal context, applies to all calling contexts.
   -- However, this may not be the _only_ representation of
   -- a universal context.


   function Applies (
      Context : Calling_Context_T;
      To      : Programs.Call_Path_T)
   return Boolean;
   --
   -- Whether the Context applies To (covers, includes) the
   -- given call-path.


   --
   ---   Assertions
   --


   type Assertion_T is record
      Fact    : Fact_T;
      Parts   : Parts_Ref;
      Context : Calling_Context_T;
      Source  : Source_T;
   end record;
   --
   -- An assertion that states a certain Fact, for some program Parts,
   -- in a given Context. The Source component shows the (closest)
   -- point in some assertion file where the assertion is made.
   --
   -- Fact
   --    The fact asserted on these Parts in this Context.
   -- Parts
   --    The parts that are constrained by the Fact.
   -- Context
   --    The execution context(s) in which the Fact applies.


   type Assertion_List_T is array (Positive range <>) of Assertion_T;
   --
   -- A list (set) of assertions. Multiple uses.


   --
   ---   Assertion sets and subsets
   --


   package Assertion_Vectors is new Unbounded_Vectors (
      Element_Type   => Assertion_T,
      Vector_Type    => Assertion_List_T,
      Initial_Size   => 300,
      Size_Increment => 300,
      Deallocate     => Opt.Deallocate);


   type Assertion_Bag_T is new Assertion_Vectors.Unbounded_Vector;
   --
   -- A set of assertions, representing the logical conjunction
   -- of all the assertions in the set.
   --
   -- The silly name is chosen to avoid conflict with the
   -- name Assertions.Assertion_Set_T.


   type Assertion_Bag_Ref is access all Assertion_Bag_T;
   --
   -- A reference to a heap-allocated set of assertions.


   procedure Add (
      Assertion : in     Assertion_T;
      To        : in out Assertion_Bag_T);
   --
   -- Adds the Assertion To the set.
   --
   -- The new Assertion can subsume, be subsumed by, override, or
   -- conflict with the assertions already in the set. It is not
   -- specified whether those events are noticed and reported at
   -- the time the assertion is added to the assertion set.


   type Assertion_Subset_T is array (Positive range <>) of Positive;
   --
   -- A subset of the assertions in some Assertion_Bag_T, defined
   -- by listing the indices of the assertions in the set.


   type Assertion_Subset_Ref is access Assertion_Subset_T;
   --
   -- A reference to an assertion subset on the heap.


   Empty_Subset : constant Assertion_Subset_Ref :=
      new Assertion_Subset_T (1 .. 0);
   --
   -- A reference to an empty assertion subset.


   function Image (Item : Assertion_Subset_T) return String;
   --
   -- Describes the subset by listing the indices of the assertions.


   function Image (
      Item   : Assertion_Subset_T;
      Within : Assertion_Bag_T)
   return String;
   --
   -- Describes the subset by listing the indices and source
   -- locations of the assertions.


   function Is_Member (
      Index  : Positive;
      Subset : Assertion_Subset_T)
   return Boolean;
   --
   -- Whether the Subset contains the assertion with the
   -- given Index (in some unspecified base set of assertions).


   --
   ---   Container-Contained relations between parts
   --


   type Actual_Part_T (Kind : Actual_Part_Kind_T := Program) is record

      Subprogram : Programs.Subprogram_T;
      Model      : Flow.Computation.Model_Handle_T;

      case Kind is

      when Program | Own.Subprogram =>

         null;

      when Luup =>

         Luup : Loops.Loop_T;

      when Call_Static =>

         Call : Programs.Call_T;

      when Call_Dynamic =>

         Boundable_Call : Flow.Dynamic_Edge_T;

      when Jump =>

         Jump : Flow.Dynamic_Edge_T;

      when Instruction =>

         Step : Flow.Step_T;

      end case;

   end record;
   --
   -- An actual part from the program under analysis, to which
   -- some assertions may apply.
   --
   -- Subprogram
   --    The subprogram under analysis that is, or contains, the
   --    actual part in question, except when Kind = Program in
   --    which case Subprogram = No_Subprogram.
   -- Model
   --    The current (context-sensitive) computation model for
   --    the Subprogram, if not null.
   --    Model is null when assertions for the subprogram are
   --    sought directly from an assertion set, without the benefit
   --    of an assertion map for the Subprogram/Model combination.
   -- Luup
   --    A loop under analysis in the Subprogram.
   -- Call
   --    A call under analysis. Subprogram is Callee (Call).
   -- Boundable_Call
   --    A boundable edge that represents a dynamic call.
   -- Jump
   --    A dynamic jump under analysis in the Subprogram.
   -- Step
   --    The step that contains (models) an instruction.


   function Image (Item : Actual_Part_T) return String;
   --
   -- A brief description, mainly for tracing purposes.


   function Step (Part : Actual_Part_T) return Flow.Step_T;
   --
   -- For a "pointlike" part, the single step that constitutes
   -- this part in a flow-graph.
   --
   -- Precondition: Part.Kind in Point_Part_Kind_T.


   type Actual_Part_List_T is array (Positive range <>) of Actual_Part_T;
   --
   -- A list (set) of actual parts, for example all the parts of a
   -- certain kind that are contained in an outer part.


   No_Parts : constant Actual_Part_List_T := (
      1 .. 0 => (
         Kind       => Program,
         Subprogram => Programs.No_Subprogram,
         Model      => null));
   --
   -- A list of no parts.


   function Parts_Within (
      Container : Actual_Part_T;
      Kind      : Inner_Part_Kind_T)
   return Actual_Part_List_T;
   --
   -- The parts of the given Kind that are in the Container part.


   --
   ---   Matching program parts against features.
   --


   type Goals_T is array (Positive range <>) of Part_Predicate_Ref;
   --
   -- A stack of goals (predicates being matched to actual parts)
   -- under consideration in the matching process.


   No_Goals : constant Goals_T := (1 ..0 => null);


   function Is_Member (
      Predicate : Part_Predicate_Ref;
      Of_Goals  : Goals_T)
   return Boolean;
   --
   -- Whether the given predicate ref is listed in the given goals.
   -- Note that predidate _references_ are compared.


   function Match (
      Subprogram : Programs.Subprogram_T;
      Model      : Flow.Computation.Model_Handle_T;
      Predicate  : Part_Predicate_Ref;
      Goals      : Goals_T)
   return Boolean;
   --
   -- Whether the given Subprogram, under the given computation Model,
   -- matches the given Predicate as part of the current match Goals.


   function Match (
      Part      : Actual_Part_T;
      Predicate : Part_Predicate_Ref;
      Goals     : Goals_T)
   return Boolean;
   --
   -- Whether the given Part matches the given Predicate as part
   -- of the current match Goals.


   function Match (
      Part  : Actual_Part_T;
      Parts : Parts_T;
      Goals : Goals_T)
   return Boolean;
   --
   -- Whether the given Part matches (is a member of) the given
   -- Parts set, given the current match Goals.


   function To_Be_Mapped (
      Assertion : Assertion_T;
      Onto      : Programs.Subprogram_T)
   return Boolean;
   --
   -- Whether the given Assertion concerns parts that are mapped
   -- to subprogram contents, and should be mapped Onto the given
   -- subprogram.
   --
   -- The mapped parts are loops and calls, and should be mapped
   -- onto the given subprogram if this subprogram matches all
   -- features required of the subprogram-type containers of the
   -- parts (that is, all "Within subprogram" features of the
   -- Assertion's parts, to any containment level).
   --
   -- At present, only Subprogram_Identity features are matched
   -- between the given subprogram and the "Within subprogram"
   -- features. Containment levels are checked only in the "Within"
   -- direction, never descending to "Contains" predicates, even
   -- if this would lead to "Within subprogram" predicates that
   -- might not match the Onto subprogram.


   function To_Be_Mapped (
      From : Assertion_Bag_T;
      Onto : Programs.Subprogram_T)
   return Assertion_Subset_T;
   --
   -- Those assertions From the given assertion set that should
   -- be mapped Onto the contents of the given subprogram.


   type Part_Kinds_T is array (Part_Kind_T) of Boolean;
   --
   -- A set of part kinds, for choosing assertions.


   type Fact_Kinds_T is array (Fact_Kind_T) of Boolean;
   --
   -- A set of fact kinds, for choosing assertions.


   function "+" (Part : Part_Kind_T) return Part_Kinds_T;
   --
   -- The singleton set that contains just the given Part kind.


   function "+" (Fact : Fact_Kind_T) return Fact_Kinds_T;
   --
   -- The singleton set that contains just the given Fact kind.


end Assertions.Own;
