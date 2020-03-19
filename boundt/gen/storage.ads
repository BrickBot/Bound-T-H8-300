-- Storage (decl)
--
-- Storage areas and units of the target processor.
--
-- The Cell Concept
--
-- A storage unit in the target processor is called a "cell". The
-- number of cells is potentially very large; the actual set of cells
-- handled in an analysis depends on the cells accessed by the target
-- program under analysis. Cells may have different sizes, in terms of
-- the range of values that the cell can hold. There may be several
-- different storage areas, each divided into cells. Examples are
-- processor registers, internal RAM memory, external RAM memory.
--
-- A physical storage area may be divided into several logical storage
-- areas, for example the RAM memory may be considered to consist of
-- statically allocated memory, dynamically allocated memory, and
-- stack memory. The stack memory, in turn, could be logically divided
-- from the point of view of one subprogram, into an area of parameters,
-- an area of local variables, and a temporary area for e.g. intermediate
-- results of computations. In these areas, a cell could be identified
-- by its offset relative to the frame pointer or stack pointer, without
-- necessarily knowing the absolute memory address.
--
-- The physical and logical storage areas, their "cell" concepts and
-- the addresses or other specifications that identify cells, are
-- fully defined by the target-specific Processor package.
--
-- For Bound-T, the important attributes of a cell are its identity,
-- the size (width) of the cell in bits (the number of bits it can hold)
-- the value it contains, and how the value is changed by the code of
-- the target program. For example, when Bound-T finds bounds on loop
-- iterations, it identifies a cell that acts as a loop-counter, which
-- means that the target program initialises the cell to a certain
-- value before the loop, increments or decrements the value of the
-- cell in a certain way on every loop iteration, and terminates the
-- loop when the value of the cell meets a certain limit.
--
-- The identity of cells is established by the target-specific Processor
-- package, via the type Cell_Spec_T, and the target-specific Decoder
-- package, which generates the Cell_Spec_T value of each accessed cell.
-- This Storage package creates an internal object for each distinct
-- cell; such objects are referred to via handles of type Cell_T.
-- Each value of type Cell_T corresponds to a unique Cell_Spec_T value,
-- so Cell_T values can be identified by their Cell_Spec_T value, but
-- Cell_T's are not constructed for all possible values of Cell_Spec_T.
--
-- This concept of cell identity is primarily syntactic; in essence, a
-- Cell_Spec_T value corresponds to an addressing mode in the target
-- processor plus the expressions or (preferably) constant values that
-- define the base address, offets, or register numbers. Depending on
-- the target processor architecture, two different Cell_Spec_T values
-- and their corresponding two different Cell_T values may or may not
-- be aliases for the same physical memory location. We have more to
-- say about aliasing later below.
--
--
-- An Instruction Assigns Values to Cells
--
-- The effect of a "step" (~ instruction) in the target program is
-- modelled as a set of assignments, where each assignment computes
-- an arithmetic expression and stores the value in a target cell.
-- The arithmetic expressions use literal values and cell values as
-- operands. The conditions of conditional branches in the target
-- program also use such arithmetic expressions.
--
--
-- Cells are Disjoint
--
-- Bound-T assumes that cells are disjoint and not overlapping or
-- nested. In other words, when the target program stores a value into
-- a cell, all other cells retain their values. It is not possible
-- (currently) to define any kind of cell "containment" hierarchy.
-- However, such overlapping or aliasing can be modeled by the
-- processor-specific instruction decoder.
--
-- For example, target processors often have a "status word" that
-- is considered to contain "condition flag" bits and other program
-- status information. The Processor package cannot explain this nested
-- structure to Bound-T. If it is desired (as is usual) to define the
-- status word as a cell and also the condition flags as cells, then
-- the Decoder package must ensure that an instruction that sets a
-- condition flag is decoded into an assignment to the condition flag
-- cell and the corresponding assignment to the status word cell.
-- In practice, the status cell must be assigned an "unknown" value,
-- because the Decoder usually cannot know what values to assign
-- to the other parts of the status word. Vice versa, an instruction
-- that sets the status word (perhaps by restoring a saved status
-- word) must be decoded into an assignment to the status word and
-- corresponding assignments to all the condition flags.
--
--
-- Dynamic Accesses
--
-- Sometimes a target program accesses a cell for which the identity
-- of the cell (in terms of its Cell_Spec_T or Cell_T) is not defined
-- statically (at decoding time) but depends on some dynamic value,
-- for example the value of a register that is used as the memory
-- address (indirect memory access, indexed memory access). Such
-- accesses are called "dynamic data accesses". Later in the analysis,
-- when bounds on dynamic values are computed, a dynamic data access
-- may be "resolved", that is, we may learn the actual cell or set
-- of cells that it accesses so that the dynamic access is replaced by
-- a static access. On the other hand, a dynamic access may remain
-- "unresolved" because its dependence on dynamic computation is too
-- complex to be analysed.

-- Cells are often taken to be statically (syntactically) identified even
-- if their actual run-time memory address is dynamically defined. For
-- example, a subprogram parameter may be identified by a static offset
-- relative to the subprogram's stack frame, while the actual memory
-- address of the frame and therefore of the parameter depends on the
-- value of the stack pointer when the subprogram is called. To make
-- the analysis as modular (compositional) as possible, it is useful to
-- be able to ignore the actual memory address and work with offsets.
--
-- Another example arises in processors that have multiple register
-- banks so that when an instruction refers to register the static
-- register number is taken as an offset to the current register bank
-- pointer. In such a machine, even register references are dynamically
-- computed accesses. Fortunately, the register bank pointer is rarely
-- changed, so one can often assume that a given subprogram uses the same
-- register bank throughout, which means that register references can be
-- considered static (again, except for aliasing).
--
--
-- Aliasing
--
-- When Bound-T is analysing the data flow in the target program,
-- dynamic data accesses cause two problems: (1) which value results
-- when a dynamically accessed cell is read (for example, within
-- an expression), and (2) which cells may lose their values when
-- a dynamically accessed cell is written by an assignment.
--
-- The first problem is safely (but approximately) solved by
-- considering that an unknown value is read. The second problem
-- is harder: if a dynamic write access could change the value of
-- any cell, the data-flow analysis would be greatly weakened.
-- This is an "aliasing" problem: two storage accesses which look
-- (syntactically) different in the target program may nevertheless
-- be aliases for the same storage cell.
--
-- Fortunately, most target processors and programming languages
-- divide their storage so that a dynamic data access can reach
-- only a subset of the cells, for example only the cells in a certain
-- physical or logical storage area. For example, usually (but not
-- always, alas) the processor registers cannot be accessed by any
-- form of indirect, indexed or dynamic access.
--
-- To make use of such architectural features, we seek a way to define
-- and analyse the possible aliases. We want to have an exact answer
-- when possible (considering also cost) and otherwise an approximate
-- answer, perhaps letting the analyst choose between safe but weak
-- answers or powerful but potentially unsafe answers. Moreover, we
-- want to be able to summarize the effect of subprogram or a loop
-- body so that we can ask if a subprogram call, or a loop iteration,
-- may modify a given cell.
--
-- The current representation of aliases works as follows. We assume that
-- the target-specific Processor package defines an set-like type called
-- Alias_Range_T that show how storage accesses can be aliased. A value of
-- this type defines the (set of) storage areas that can be accessed by a
-- cell or a dynamic memory access (we do not use the term "alias set" for
-- this because it usually has a more detailed meaning as the set of cells
-- that can be aliases). This gives us a rather rough way to check if two
-- cells or dynamic access can be aliases, by checking if their Alias Ranges
-- intersect.
--
-- However, as such this aliasing check would often be too crude. For
-- example, in a processor where registers are mapped to (fixed) memory
-- locations, any register reference can alias with a dynamic memory access
-- (assuming that the dynamically computed address is quite unknown) but two
-- register references with different register numbers are never aliases of
-- each other. To model such relationships between groups of cells, we expect
-- that the target-specific Processor package also defines an enumeration
-- called Alias_Group_T and functions that assign such a group to each
-- Cell_Spec_T. Then, the Alias Range is used only to compare references
-- from different groups and is in fact a function of Alias Group rather
-- than directly a function of Cell Spec.
--
-- Aliasing between cells and dynamic data access is checked as follows:
--
-- > If two cells have the same Alias Group, they are aliases only if
--   they are identical. In other words, within the same Alias Group the
--   static, syntactic attributes of the cells are enough to identify
--   the locations uniquely.
--
-- > If two cells have different Alias Groups, they may be aliases if the
--   Alias Ranges of these groups intersect. Note that this is only a
--   "may", not a "must".
--
-- > A cell and a dynamic data access, or two dynamic data accesses,
--   may be aliases if and only if their Alias Ranges intersect.
--
-- The mapping of cells and dynamic data accesses to Alias Groups and
-- Alias Ranges may be controlled by command-line options or other means.
-- This lets the analyst can choose between strict alias checks, which is
-- safe but reduces the power of the analysis to find loop bounds, or laxer
-- alias checks, which may allow the analysis to find more loop bounds
-- but with a risk of incorrect bounds due to undetected aliasing.
--
-- For example, consider a target processor that has a set of 8-bit
-- registers that cannot be dynamically (indirectly( accessed, a small
-- 256-byte  internal memory that can be accessed with a direct address
-- or indirectly via an 8-bit register, and a 64k external memory
-- that can be accessed indirectly via a 16-bit address given by
-- a pair of 8-bit registers. The Processor package would define
--
--    type Alias_Area_T is (Internal, External);
--
--    type Alias_Range_T is array (Alias_Area_T) of Boolean;
--    -- A set of Alias Areas.
--
--    type Alias_Group_T is (Registers, Internals, Externals);
--
-- Assume, first, that the internal memory can also be accessed
-- indirectly via a 16-bit address when this address is in the
-- range 0 .. 255. Then any 16-bit indirect access could reach either
-- the internal or the external memory, and Processor would assign
-- alias groups as follows:
--
--    Cell type                Group
--    ---------                -----
--    Static register ref      Registers
--    Static internal memory   Internals
--    Static external memory   Externals
--
-- Alias ranges would be assigned as follows:
--
--    Group or Access              Alias Range
--    ---------------              -----------
--    Registers                    {} (empty set)
--    Internals                    {Internal}
--    Externals                    {External}
--    Indirect 8-bit addr          {Internal}
--    Indirect 16-bit addr         {Internal, External}
--
-- Examples of aliasing decisisions:
--
-- > Register R3 and R5 do not alias, because they are in the same
--   group (Registers) but have different specs (assuming that the
--   register number is part of the Cell Spec, as seems natural).
--
-- > Register R3 and the Internal cell with address 112 do not alias,
--   because they are in different groups (Registers, Internals) and
--   the alias ranges of these groups do not intersect.
--
-- > TBA examples.
--
-- Now consider a slightly different processor in which a 16-bit
-- dynamic access always uses the external memory, even for the
-- address range 0 .. 255, but on the other hand an 8-bit dynamic
-- access can alias with a register, because the registers are
-- actually the internal memory cells at addresses 0, 1, ....
-- Then Processor should assign alias groups as follows when a
-- conservative (strict) alias analysis is required:
--
-- > TBA complete example.
--
-- The above defines what we call "syntactic aliasing", because the
-- aliasing condition is based only on the syntax (the instruction
-- addressing modes) and not on the dynamic values of the addresses.
-- Later in the analysis, more precise aliasing checks can be based
-- on bounds derived for the addresses; we call it "dynamic aliasing".
--
--
-- Initial-value cells
--
-- Some cells are known as "initial-value" cells and have a special
-- role. An initial-value cells represents the initial value, on entry
-- to the current subprogram (under analysis), of another cell, known
-- as the "variable cell" for this initial-value cell.
--
-- Initial-value cells are mostly used to model stack-frame pointers,
-- that is, real or virtual pointers to the start of the stack frame
-- of the current subprogram. Typically, the processor (or its ABI)
-- dedicates a certain register as the stack pointer, and this register
-- is modelled as an ordinary (variable) cell. The value of the stack
-- pointer on entry to a subprogram defines the location of the stack
-- frame, and this value is modelled as an initial-value cell.
--
-- Initial-value cells are synthetic in the sense that the real
-- processor does not (necessarily) have a real storage location that
-- holds the value of the initial-value cell.
--
-- The program may or may not dedicate a real register for use as a
-- frame pointer; if there is a frame pointer register, it is usually
-- also modelled as an ordinary cell, but the analysis will discover
-- that its value equals the initial-value cell of the stack pointer
-- (perhaps with a fixed offset).
--
-- The purpose of the initial-value cells is to resolve references to
-- data in the stack frame, when these references are not based on the
-- stack pointer register itself.
--
-- It follows from the definition of initial-value cells that their
-- value is constant (invariant) throughout the relevant subprogram
-- and that there can be no assignments to initial-value cells. The
-- constraint that an initial-value cell equals its variable cell
-- at the start of the subprogram is entered in each analysis by
-- other means, not as an explicit assignment.
--
-- An initial-value cell is its own initial-value cell.
--
--
-- Volatile Cells
--
-- Some cells have non-standard semantics in the sense that the value read
-- from the cell is not the same as the value last written in the cell.
-- These "volatile" cells can represent, for example, memory-mapped I/O
-- registers. Volatile cells are given special treatment in many analyses.
--
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
-- $Revision: 1.24 $
-- $Date: 2015/10/24 19:36:52 $
--
-- $Log: storage.ads,v $
-- Revision 1.24  2015/10/24 19:36:52  niklas
-- Moved to free licence.
--
-- Revision 1.23  2014/06/28 10:01:53  niklas
-- Added function Rainge() return Code_Address_Range_T.
--
-- Revision 1.22  2014/06/11 12:48:55  niklas
-- Moved Code_Address_Range_T and its operations to package Processor,
-- making this type target-specific. Reason: in Bound-T/OCL, Code_Address_T
-- is not scalar, therefore it has no 'First, 'Last, and other
-- necessary operations. Retained renamings of the most important
-- items here, to minimize changes to the interface.
--
-- Revision 1.21  2013-02-12 08:47:20  niklas
-- BT-CH-0245: Global volatile cells and "volatile" assertions.
--
-- Revision 1.20  2011-09-09 14:51:51  niklas
-- Added function Initial_Cell_Variables, for use in ALF export.
--
-- Revision 1.19  2011-09-06 17:39:17  niklas
-- Added Cell_At (Index), for use in ALF export.
--
-- Revision 1.18  2009-11-27 11:28:08  niklas
-- BT-CH-0184: Bit-widths, Word_T, failed modular analysis.
--
-- Revision 1.17  2009-10-07 19:26:10  niklas
-- BT-CH-0183: Cell-sets are a tagged-type class.
--
-- Revision 1.16  2008-06-18 20:52:57  niklas
-- BT-CH-0130: Data pointers relative to initial-value cells.
--
-- Revision 1.15  2007/12/17 13:54:41  niklas
-- BT-CH-0098: Assertions on stack usage and final stack height, etc.
--
-- Revision 1.14  2007/08/20 12:14:07  niklas
-- Support for Storage.Data: Added Image functions for Cell_Value_T and
-- Cell_Value_List_T, and added function Cells_Of (Cell_Value_List_T).
--
-- Revision 1.13  2007/07/21 18:18:43  niklas
-- BT-CH-0064. Support for AVR/IAR switch-handler analysis.
--
-- Revision 1.12  2007/04/18 18:34:40  niklas
-- BT-CH-0057.
--
-- Revision 1.11  2007/03/29 15:18:05  niklas
-- BT-CH-0056.
--
-- Revision 1.10  2007/03/23 21:05:36  niklas
-- Added Small_Cell_Set_T operations Is_Empty, Remove (Cell_T),
-- Remove (Small_Cell_Set_T), Move_Cells, Image, and Discard.
--
-- Revision 1.9  2007/01/25 21:25:20  niklas
-- BT-CH-0043.
--
-- Revision 1.8  2007/01/21 19:31:49  niklas
-- BT-CH-0042.
--
-- Revision 1.7  2006/08/22 12:08:20  niklas
-- Added the function Is_None, to check if a Cell_Set_T is
-- uninitialized or otherwise equal to No_Cell_Set (null).
-- Modified Image (Cell_Set_T) to accept No_Cell_Set and
-- display it as "[not defined]".
--
-- Revision 1.6  2006/05/27 21:27:44  niklas
-- Added Elaborate_All for Unbounded_Vectors.
--
-- Revision 1.5  2005/09/17 06:15:53  niklas
-- Added function Cell_List (From, Except) to list the cells in a
-- difference set without constructing the difference set itself.
--
-- Revision 1.4  2005/09/16 12:57:42  niklas
-- Worked around the GNAT 3.15p problem in predefined "=" for
-- packed bit-vectors, by giving an overriding "=" operation
-- for Cell_Bitset_T.
--
-- Revision 1.3  2005/09/12 19:03:01  niklas
-- BT-CH-0008.
--
-- Revision 1.2  2005/02/16 21:11:49  niklas
-- BT-CH-0002.
--
-- Revision 1.1  2004/04/25 09:33:06  niklas
-- First Tidorum version.
--



with Arithmetic_Base;
with Processor;


package Storage is


   Deallocate : Boolean := True;
   --
   -- Whether to use Unchecked_Deallocation to release unused
   -- heap memory. Declared here to make it usable in instances
   -- of Unbounded_Vectors; meant to be accessed via Storage.Opt.


   --
   ---   Cells
   --


   type Cell_T is private;
   --
   -- A cell is an element of the processor (cpu or memory) that
   -- stores an integral value and takes part in the arithmetical
   -- computations, especially in loop-counter computations.
   -- Typically, the set of cells includes:
   --
   --   > All the processor's integer (fixed-point) registers.
   --   > Processor status flags (0/1-valued cells).
   --   > Local integer variables (memory slots in call frame).
   --   > Global integer variables (at static addresses).
   --
   -- For some processors, it may be convenient to define synthetic
   -- cells that model some aspect of the processor's behaviour
   -- without corresponding to a physical, electronic register.
   --
   -- Address-values (pointers) are included as "integers" in
   -- this model, so a cell can also contain an address.
   --
   -- A Cell_T object has the following attributes:
   --
   -- > its identity, which is used to connect up the data-flow from
   --   cells, to expressions that use these cells, and again to
   --   the cells that are assigned values from expressions.
   --
   -- > its width or size in bits (actually an attribute of the
   --   processor-specific "cell specification" type, Cell_Spec_T).
   --
   -- > its alias group, which determines which dynamic (indexed,
   --   indirect) data accesses can reach this cell.
   --
   -- > whether the cell is volatile (a read does not reflect
   --   the last written value).
   --
   -- > whether the cell can be a loop counter; this is usually
   --   true, except for special cells such as condition flags
   --   or program status words.
   --
   -- > a possible initial-value cell that models the initial
   --   value of this Cell_T on entry to the subprogram.
   --
   -- The implementation of Cell_T is such that if C1 and C2 are
   -- two Cell_T objects, then "C1 = C2" if and only if C1 and
   -- C2 represent the same (identical) cell.
   --
   -- The type Processor.Cell_Spec_T is a specification of
   -- cell identity that can provide more information about the
   -- cell, and is used to create cells in the first place.
   -- This provides cells with a "Spec" attribute, and two cells
   -- are identical if and only if they have the same spec.
   -- Typically, the spec includes a memory-space identifier
   -- (e.g. "register", "data", "code") and an integer number
   -- (register number or memory address).
   --
   -- The alias group and the ability to be a loop counter are
   -- assumed to be functions of the cell identity as given by a
   -- Cell_Spec_T.
   --
   -- The current implemementation also requires a graphic
   -- string name for each cell, which must obey the syntax
   -- of an identifier (as required by the Calculator package)
   -- and be a unique across all cells. The name is created by
   -- the Processor.Image function applied to a Cell_Spec_T, and
   -- can be accessed by Name_Of applied to a Cell_T. Given a
   -- name, the corresponding cell can be found (if it exists)
   -- with Cell_Named.


   No_Cell : constant Cell_T;
   --
   -- A distinguished value that indicates the absence of a cell.
   -- May cause trouble if passed as a parameter to an operation
   -- that really needs a cell.


   No_Such_Cell : exception;
   --
   -- Raised when a cell is sought by name, and there is no
   -- cell by this name.


   --
   ---   Aliasing groups
   --


   -- The processor-specific package Processor.Cells must provide the
   -- following operations:
   --
   --   function Alias_Group (Cell_Spec : Cell_Spec_T) return Alias_Group_T;
   --
   --   function Alias_Range (Group : Alias_Group_T) return Alias_Range_T;
   --
   -- Since Alias_Group_T is expected to be an enumerated type, the latter
   -- function (Alias_Range) could also be implemented as an array.


   subtype Alias_Range_T is Processor.Alias_Range_T;
   --
   -- A renaming for historical reasons.


   Isolated : constant Alias_Range_T := Processor.Isolated;
   --
   -- A renaming for historical reasons.


   function May_Alias (Left, Right : Alias_Range_T) return Boolean
   renames Processor.May_Alias;
   --
   -- A renaming for historical reasons.


   type Alias_Level_T is (
      None,
      Modal,
      Class,
      Strict);
   --
   -- The level of strictness or surety for alias checking.
   --
   -- NOTE: This thing is under development. Ignore for now.
   --
   -- The "None" and "Strict" levels are defined in a general
   -- and identical way for all target processors, but the exact
   -- intepretation of the "Modal" and "Class" levels may depend
   -- on the taret.
   --
   -- None
   --    No alias checks.
   --    > Two cells are aliases if and only if they are identical.
   --    > No dynamic references are aliases with each other nor
   --      with any cell.
   -- Modal
   --    Aliasing depends on the addressing modes used in the target
   --    instructions. The modes are target-dependent but typically
   --    include absolute addressing, register + offset, PC + offset,
   --    and so on.
   --    > Two cells are aliases if and only if they are identical.
   --    > A cell and a dynamic reference may be aliases if they use
   --      the same addressing mode. For example, a local variable cell
   --      that is defined by a static offset to a Frame Pointer
   --      register can alias with a dynamic reference that applies
   --      a dynamic offset to the Frame Pointer register, but not
   --      with a dynamic offset to some other register.
   --    > Two dynamic references may be aliases if and only if they
   --      use the same addressing mode.
   --    This is not a sure check, because two different addressing
   --    modes could result in the same final address. However, this
   --    is a reasonable check in many cases, because the compiler
   --    is likely to use the same addressing mode for all references
   --    to the same variable, unless there really are different access
   --    paths to this variable, for example as a global and as a
   --    call-by-reference parameter. Such aliasing on the logical
   --    level is usually considered bad coding style.
   -- Class
   --    Aliasing takes into account that different dynamic addressing
   --    modes may give the same address if the addressing modes are
   --    in the same "class", where the class is a target-dependent
   --    grouping of addressing modes.
   --    > Two cells are aliases if and only if they are identical.
   --    > A cell and a dynamic reference may be aliases if they
   --      use the same class of addressing mode.
   -- Strict
   --    Aliasing is checked as surely and safely as possible, without
   --    any assumptions on which addressing modes can access the same
   --    cells.
   --    > Two cells are aliases if they are identical, or if they
   --      can access the same physical storage location.
   --    > Two dynamic references may be aliases if they can access
   --      the same physical storage.
   --    The notion of which physical storage locations a cell or a
   --    dynamic reference can access is target-dependent, of course,
   --    but at the Strict level the definition of this notion should
   --    make no assumptions about machine state. For example, it
   --    should not assume that the stack is separate from the heap
   --    or separate from statically allocated storage, or that the
   --    stack-usage follows some calling protocol.


   --
   ---  Creating cells. Attributes of cells.
   --


   subtype Width_T is Arithmetic_Base.Width_T;
   --
   -- An abbreviation for the number of bits in a cell.


   function Cell (
      Spec              : Processor.Cell_Spec_T;
      Initial_Value_For : Cell_T)
   return Cell_T;
   --
   -- Creates a cell with the given specification, or if such
   -- a cell already exists, returns the existing cell.
   --
   -- This cell can be defined as the Initial_Value_For an
   -- existing variable cell, in which case it is an error if
   -- a cell with this Spec already exists or if the variable
   -- cell already has an initial-value cell.


   function Cell (Spec : Processor.Cell_Spec_T) return Cell_T;
   --
   -- Abbreviation for Cell (Spec, Initial_Value_For => No_Cell).


   function Number_Of_Cells return Natural;
   --
   -- The total number of cells that currently exist (i.e.
   -- that have been created, since there is no way to
   -- delete a cell).
   --
   -- The Index of any cell that currently exists is at most
   -- Number_Of_Cells.


   function Spec_Of (Cell : Cell_T) return Processor.Cell_Spec_T;
   --
   -- Returns the specification of the cell.


   function Width_Of (Cell : Cell_T) return Width_T;
   --
   -- The width of the Cell; the number of bits it holds.
   -- This is actually a function of Spec_Of (Cell).


   function Counter_Cell (Cell : Cell_T) return Boolean;
   --
   -- Whether the cell is a potential loop-counter.
   -- This is actually a function of Spec_Of (Cell).


   function Alias_Group (Cell : Cell_T) return Processor.Alias_Group_T;
   --
   -- The alias group of the cell.
   -- This is actually a function of Spec_Of (Cell).


   function Alias_Range (Cell : Cell_T) return Processor.Alias_Range_T;
   --
   -- The alias range of the cell.
   -- This is actually a function of Alias_Group (Cell).


   function May_Alias (This, That : Cell_T) return Boolean;
   --
   -- Whether the This and That cells can be aliases of the same
   -- storage location.


   function Is_Initial (Cell : Cell_T) return Boolean;
   --
   -- Whether this Cell is an initial-value cell, that is, models
   -- the initial value of some other cell, on entry to the
   -- current subprogram.


   function Initial_Cell (Cell : Cell_T) return Cell_T;
   --
   -- The cell that holds the initial value (on entry to the
   -- subprogram) of the given Cell, if such an initial-value
   -- cell is defined, otherwise No_Cell.
   --
   -- If Is_Initial (Cell) then Initial_Cell (Cell) = Cell.


   procedure Mark_As_Volatile (Cell : Cell_T);
   --
   -- Marks the Cell as a volatile cell.
   -- Is it an error (fault) to mark an Initial_Value cell or a
   -- potential counter cell as volatile.


   procedure Mark_Range_As_Volatile (
      From, To    : in     Processor.Cell_Spec_T;
      Valid_Range :    out Boolean);
   --
   -- Defines the range From .. To as volatile storage, in the sense
   -- that any cell, old or new, that lies in or touches that range
   -- should be considered volatile.
   --
   -- If the two cell-specs, From and To, are "compatible" (more or less,
   -- lie in the same address space) and can form a storage range,
   -- Valid_Range is returned as True, the range is defined as volatile,
   -- and any existing cell that lies in or touches the range is marked
   -- as volatile too. Otherwise, Valid_Range is returned as False and
   -- nothing else is done.


   function Is_Volatile (Cell : Cell_T) return Boolean;
   --
   -- Whether the cell is volatile.


   function Number_Of_Volatile_Cells return Natural;
   --
   -- The total number of volatile cells that currently exist,
   -- that is, cells for which Is_Volatile returns True.
   --
   -- The "volatile index" of a cell runs from 1 to this number.


   type Volatile_Cell_Index_T is new Positive;
   --
   -- All volatile cells are given an identifying index number,
   -- running from 1 to Number_Of_Volatile_Cells.


   Not_A_Volatile_Cell : exception;
   --
   -- Signals an attempt to apply an action to a non-volatile cell,
   -- when the action is only appropriate for volatile cells.


   function Volatile_Index (Cell : Cell_T) return Volatile_Cell_Index_T;
   --
   -- The identifying index of the volatile Cell.
   -- Propagates Not_A_Volatile_Cell if the Cell is not volatile.


   function Cell (Name : String) return Cell_T;
   --
   -- Returns the cell with the given name, if it exists,
   -- or else raises No_Such_Cell.


   function Name_Of (Cell : Cell_T) return String;
   --
   -- The graphic name of the cell.


   type Cell_Index_T is new Positive;
   --
   -- Cells are automatically assigned a unique identifying index
   -- which can also be used to make arrays indexed by cells.


   function Index (Cell : Cell_T) return Cell_Index_T;
   --
   -- The index of the cell.


   function Cell_At (Index : Cell_Index_T) return Cell_T;
   --
   -- The cell with the given Index, or No_Cell if there
   -- is no such cell.


   function Image (Item : Cell_T) return String;
   --
   -- Describes the (identity of) the cell in a readable form.


   --
   ---  Cell lists
   --


   type Cell_List_T is array (Positive range <>) of Cell_T;
   --
   -- A list of cells, simply.


   type Cell_List_Ref is access Cell_List_T;
   --
   -- Necessary for some clients, although not used here.


   function Null_Cell_List return Cell_List_T;
   --
   -- A null cell list (length zero).


   function Is_Member (
      Cell    : Cell_T;
      Of_List : Cell_List_T)
   return Boolean;
   --
   -- Whether the cell is present in the list (in one or
   -- more places).


   function Counter_Cells (List : Cell_List_T)
   return Cell_List_T;
   --
   -- Those cells in the List that are marked as Counter_Cell.


   function Image (Item : Cell_List_T) return String;
   --
   -- Describes all the cells in the list.


   function Initial_Cell_Variables
   return Cell_List_T;
   --
   -- All those cells which have an initial-value cell.
   -- That is, for each of these cells C, the function Initial_Cell (C)
   -- returns a cell that is not C and is not No_Cell.


   --
   ---   Cell sets (abstract)
   --


   type Root_Cell_Set_T is abstract tagged null record;
   --
   subtype Cell_Set_T is Root_Cell_Set_T'Class;
   --
   -- A finite set of cells.
   --
   -- Cells can be added to a set and removed from the set.
   -- The type acts like a by-value type but may internally use
   -- references and share storage with other cell-set objects.
   -- Derived types are likely to have controlled components.
   --
   -- Concrete cell-set types with various implementations (bit
   -- vector, unbounded vector, tree, ...) may be implemented
   -- in child packages.
   --
   -- The default initial value of a Cell_Set_T object is the
   -- empty (null) set.


   --    Primitive operations on Cell_Set_T
   --
   -- Some operations have the default implementation via the
   -- corresponding class-wide operation from the Basic_Mixed package,
   -- which see below.


   function Empty return Root_Cell_Set_T
   is abstract;
   --
   -- The empty (null) set.


   procedure Erase (Set : in out Root_Cell_Set_T);
   --
   -- Makes the Set empty. However, may retain storage allocated
   -- to hold elements in the set. See also Discard, below.
   --
   -- The default implementation lists the elements with To_List
   -- (see below) and then Removes all elements from the set.


   function Is_Empty (Item : Root_Cell_Set_T) return Boolean
   is abstract;
   --
   -- Whether the cell-set is empty.


   function Is_Member (
      Cell   : Cell_T;
      Of_Set : Root_Cell_Set_T)
   return Boolean
   is abstract;
   --
   -- Whether the cell is a member of the set.


   function Any_Member (
      Cells  : Cell_List_T;
      Of_Set : Root_Cell_Set_T)
   return Boolean;
   --
   -- Whether any of the listed Cells is a member of the set.
   --
   -- The default implementation uses Is_Member on each of
   -- the Cells until a member is found.


   function Card (Set : Root_Cell_Set_T) return Natural
   is abstract;
   --
   -- The number of cells in the Set.


   function To_Set (List : Cell_List_T) return Root_Cell_Set_T
   is abstract;
   --
   -- The set that contains exactly the cells in the given list.


   function To_List (Set : Root_Cell_Set_T) return Cell_List_T
   is abstract;
   --
   -- Lists the cells in the set in an unspecified order
   -- without duplications.


   function To_List (
      From   : Root_Cell_Set_T;
      Except : Cell_Set_T)
   return Cell_List_T;
   --
   -- The list of cells in the difference set, From - Except.
   -- This is the same as To_List (From - Except) but avoids
   -- constructing the difference set explicitly.
   --
   -- The default implementation constructs To_List (From) and then
   -- filters out those cells that are members of Except.


   procedure Add (
      Cell : in     Cell_T;
      To   : in out Root_Cell_Set_T)
   is abstract;
   --
   -- Adds the Cell To the set.


   procedure Add (
      Cell  : in     Cell_T;
      To    : in out Root_Cell_Set_T;
      Added : in out Boolean);
   --
   -- Adds the given cell to the given set, whether it already
   -- was there or not. If the cell was not already in the set,
   -- Added is returned as True, otherwise Added is not altered.
   --
   -- The default implementation uses Is_Member to set Added and
   -- the Adds the cell.


   procedure Add (
      Cells : in     Cell_List_T;
      To    : in out Root_Cell_Set_T);
   --
   -- Adds the Cells To the set.
   --
   -- The default implementation uses the per-cell Add on each
   -- of the Cells.


   procedure Add (
      Cells : in     Root_Cell_Set_T;
      To    : in out Root_Cell_Set_T);
   --
   -- Adds the Cells To the set.
   --
   -- The default implementation is Basic_Mixed.Add.


   procedure Remove (
      Cell : in     Cell_T;
      From : in out Root_Cell_Set_T)
   is abstract;
   --
   -- Removes the given Cell from the given set, whether it
   -- already was there or not.


   procedure Remove (
      Cells : in     Cell_List_T;
      From  : in out Root_Cell_Set_T);
   --
   -- Removes the given Cells from the given set, whether they
   -- already were there or not.
   --
   -- The default implementation applies the per-cell Remove to
   -- each of the Cells.


   procedure Remove (
      Cells : in     Root_Cell_Set_T;
      From  : in out Root_Cell_Set_T);
   --
   -- Removes the given Cells From the given set, whether they
   -- already were there or not.
   --
   -- The default implementation is Basic_Mixed.Remove.


   procedure Move_Cells (
      From : in out Root_Cell_Set_T;
      To   : in out Root_Cell_Set_T);
   --
   -- Moves all the cells From a given set To another set.
   -- The From set is then empty (unless it was the same variable
   -- as To) and the To set may be larger.
   --
   -- The default implementation is Basic_Mixed.Move_Cells.


   function Copy (Item : Cell_Set_T) return Root_Cell_Set_T
   is abstract;
   --
   -- A copy of the given cell-set, but with a (possibly) different
   -- implementation type (the result type).


   function Image (Item : Root_Cell_Set_T) return String;
   --
   -- The images of the cells in the set, in an unspecified order
   -- and separated by ", ".
   --
   -- The default implementation uses Image (To_List (Item)).


   function "=" (Left, Right : Root_Cell_Set_T) return Boolean;
   --
   -- Whether the two sets contain exactly the same cells.
   --
   -- The default implementation is Basic_Mixed."=".


   function Union (Left, Right : Root_Cell_Set_T) return Cell_Set_T;
   --
   -- The union of the given cell-sets.
   --
   -- The default implementation is Basic_Mixed.Union.


   function "-" (Left, Right : Root_Cell_Set_T) return Cell_Set_T;
   --
   -- The difference of the two cell sets, that is the set
   -- of cells that are in Left but are not in Right.
   --
   -- The default implementation is Basic_Mixed."-".


   function Intersection (Left, Right : Root_Cell_Set_T) return Cell_Set_T;
   --
   -- The intersection of the given cell-sets.
   --
   -- The default implementation is Basic_Mixed.Intersection.


   procedure Discard (Set : in out Root_Cell_Set_T);
   --
   -- Erases the cell-set and releases as much as possible of the
   -- memory used for the cell-set. May finalize controlled
   -- components of the cell-set.
   --
   -- The Set variable may become invalid for use in any other
   -- cell-set operation.
   --
   -- The default implementation simply calls Erase.


   --
   ---   Class-wide operations on sets of cells
   --


   package Basic_Mixed
   --
   -- Operations on cell-sets of mixed class.
   --
   -- These operations take two (or more) parameters of type Cell_Set_T
   -- (that is, Root_Cell_Set_T'Class) and always implement class-wide
   -- procedures. They do not, as the Mixed package, check if the
   -- parameters are of the same class.
   --
   is

      procedure Add (
         Cells : in     Cell_Set_T;
         To    : in out Cell_Set_T);
      --
      -- Adds the Cells To the set.


      procedure Remove (
         Cells : in     Cell_Set_T;
         From  : in out Cell_Set_T);
      --
      -- Removes the given Cells From the given set, whether they
      -- already were there or not.


      procedure Move_Cells (
         From : in out Cell_Set_T;
         To   : in out Cell_Set_T);
      --
      -- Moves all the cells From a given set To another set.
      -- The From set is then empty (unless it was the same variable
      -- as To) and the To set may be larger.

      -- The operation gets the cells from From with To_List (From), To),
      -- then Erases From, and the Adds the cells to To. This means that
      -- the operation can be applied with From and To associated to the
      -- same actual parameter, and the effect is null, as expected, but
      -- in this case the From set is not null on exit.


      function "=" (Left, Right : Cell_Set_T) return Boolean;
      --
      -- Whether the two sets contain exactly the same cells.


      function Union (Left, Right : Cell_Set_T)
      return Cell_Set_T;
      --
      -- The union of the given cell-sets, computed by adding the Right
      -- cells to a copy of the Left cell-set.


      function "-" (Left, Right : Cell_Set_T)
      return Cell_Set_T;
      --
      -- The difference of the two cell sets, that is the set of cells
      -- that are in Left but are not in Right, computed by Removing
      -- all the elements of the Right set from a copy of the Left set.


      function Intersection (Left, Right : Cell_Set_T)
      return Cell_Set_T;
      --
      -- The intersection of the given cell-sets, computed as a copy
      -- of the Left set from which are removed those elements that
      -- are not members of the Right set.


   end Basic_Mixed;


   --
   ---   Class-wide operations on sets of cells that use primitive
   ---   operations when both operand sets are of the same class.
   --


   package Mixed
   --
   -- Operations on cell-sets of mixed class.
   --
   -- These operations take two or more parameters of type Cell_Set_T
   -- (that is, Root_Cell_Set_T'Class). If the parameters are of the
   -- same class, the operation calls the corresponding primitive
   -- operation for this class, otherwise it calls the corresponding
   -- class-wide operation from Basic_Mixed, with the same effect (but
   -- possibly taking more time).
   --
   -- Note that there is a risk of (eternal) recursion if some primitive
   -- operations call these Mixed operations. It is safer for the primitive
   -- to call the corresponding Basic_Mixed operation.
   --
   is

      procedure Add (
         Cells : in     Cell_Set_T;
         To    : in out Cell_Set_T);
      --
      -- Adds the Cells To the set.


      procedure Remove (
         Cells : in     Cell_Set_T;
         From  : in out Cell_Set_T);
      --
      -- Removes the given Cells From the given set, whether they
      -- already were there or not.


      procedure Move_Cells (
         From : in out Cell_Set_T;
         To   : in out Cell_Set_T);
      --
      -- Moves all the cells From a given set To another set.
      -- The From set is then empty (unless it was the same variable
      -- as To) and the To set may be larger.

      -- The operation gets the cells from From with To_List (From), To),
      -- then Erases From, and the Adds the cells to To. This means that
      -- the operation can be applied with From and To associated to the
      -- same actual parameter, and the effect is null, as expected, but
      -- in this case the From set is not null on exit.


      function "=" (Left, Right : Cell_Set_T) return Boolean;
      --
      -- Whether the two sets contain exactly the same cells.


      function Union (Left, Right : Cell_Set_T)
      return Cell_Set_T;
      --
      -- The union of the given cell-sets, computed by adding the Right
      -- cells to a copy of the Left cell-set.


      function "-" (Left, Right : Cell_Set_T)
      return Cell_Set_T;
      --
      -- The difference of the two cell sets, that is the set of cells
      -- that are in Left but are not in Right, computed by Removing
      -- all the elements of the Right set from a copy of the Left set.


      function Intersection (Left, Right : Cell_Set_T)
      return Cell_Set_T;
      --
      -- The intersection of the given cell-sets, computed as a copy
      -- of the Left set from which are removed those elements that
      -- are not members of the Right set.


   end Mixed;


   --
   ---   Locations and location maps
   --


   subtype Code_Address_Range_T is Processor.Code_Address_Range_T;
   --
   -- An interval of code addresses.


   function In_Range (
      Address : Processor.Code_Address_T;
      Rainge  : Code_Address_Range_T)
   return Boolean
   renames Processor.In_Range;


   function "<=" (
      Address : Processor.Code_Address_T;
      Rainge  : Code_Address_Range_T)
   return Boolean
   renames In_Range;


   function "<=" (Left, Right : Code_Address_Range_T)
   return Boolean
   renames Processor."<=";


   function Image (Item : Code_Address_Range_T)
   return String
   renames Processor.Image;


   function Rainge (First, Last : Processor.Code_Address_T)
   return Code_Address_Range_T;
   --
   -- The range of code addresses from First to Last, inclusive.


   type Location_Point_T is record
      Address : Code_Address_Range_T;
      Cell    : Cell_T;
   end record;
   --
   -- A location (cell-spec or cell access path TBA) that is valid for
   -- a certain interval of code addresses. "Valid" here means that the
   -- given location holds the value (or part of the value TBA) of a
   -- certain variable of interest (not represented here).
   --
   -- Address
   --    The interval of code addresses such that the given location is
   --    valid immediately before executing any instruction with an address
   --    in this range, for any execution path that reaches this instruction.
   -- Cell
   --    The cell that holds the value of the interesting variable.


   type Location_T is array (Positive range <>) of Location_Point_T;
   --
   -- A map from code address to zero or more location points. Such a map
   -- shows the locations (cells or cell access paths TBA) that hold the
   -- value (or parts of the value TBA) of a certain variable, depending on
   -- the point of execution (address of instruction about to be executed).
   -- There are no constraints on the address ranges defined by the
   -- elements of the map, so for a given code address A we can have
   -- three qualitatively different cases:
   --
   -- > Exactly one element in the map has an address range that contains A.
   --   The location given in this element holds the value.
   --
   -- > Several elements in the map have address ranges that contain A.
   --   All the locations given in thse elements hold copies of the value.
   --
   -- > No element in the map has an address range that contains A.
   --   This means that the value is not represented in the target
   --   computer when executing this address.


   function Fixed_Location (Cell : in Cell_T)
   return Location_T;
   --
   -- A location that is constant, mapping all code addresses to
   -- the given Cell.


   function Image (Item : Location_Point_T) return String;
   --
   -- Textual description of the location point.


   function Image (Item : Location_T) return String;
   --
   -- Textual description of the location (map).


   type Location_Ref is access Location_T;
   --
   -- A reference to a heap-allocated location map.


   function Image (Item : Location_Ref) return String;
   --
   -- Textual description of the location (map).


   --
   ---   Cells bound to values
   --


   subtype Value_T is Arithmetic_Base.Value_T;
   subtype Word_T  is Arithmetic_Base.Word_T;
   --
   -- Abbreviations.


   type Cell_Value_T is record
      Cell  : Cell_T;
      Value : Word_T;
   end record;
   --
   -- A cell and its single allowed value, which contains
   -- Width_Of (Cell) relevant bits.


   function Image (Item : Cell_Value_T) return String;
   --
   -- Just cell=value.


   type Cell_Value_List_T is array (Positive range <>) of Cell_Value_T;
   --
   -- A list of cells each with a single allowed value.


   No_Cell_Values : constant Cell_Value_List_T (1 .. 0);
   --
   -- An empty list of cell values.


   function Image (Item : Cell_Value_List_T) return String;
   --
   -- Just cell=value, cell=value, cell=value...


   function Cells_Of (Item : Cell_Value_List_T) return Cell_List_T;
   --
   -- All the cells in the list, in the same order.


private


   type Cell_Spec_Ref is access Processor.Cell_Spec_T;
   --
   -- To make sure that we have a constrained type.


   type Cell_Name_Ref is access String;


   type Cell_Object_T is record
      Spec         : Cell_Spec_Ref;
      Name         : Cell_Name_Ref;
      Index        : Cell_Index_T;
      Width        : Width_T;
      Alias_Group  : Processor.Alias_Group_T;
      Alias_Range  : Processor.Alias_Range_T;
      Counter      : Boolean;
      Is_Initial   : Boolean;
      Volatile     : Boolean;
      Vol_Index    : Volatile_Cell_Index_T;
      Initial_Cell : Cell_T;
   end record;
   --
   -- Volatile
   --    Whether the cell is volatile.
   -- Vol_Index
   --    The index of the volatile cell, in the set of all
   --    volatile cells. Defined and relevant only for
   --    volatile cells.
   --
   -- Is_Initial
   --    Whether this is an initial-value cell.
   -- Initial_Cell
   --    The initial-value cell for this variable cell, or
   --    No_Cell if it has no initial-value cell.
   --
   -- An initial-value cell is considered to be its own initial-
   -- value cell, so C.Is_Initial implies C.Initial_Cell = C,
   -- when C is a Cell_T.


   type Cell_T is access Cell_Object_T;


   No_Cell : constant Cell_T := null;


   --
   ---   Cells bound to values
   --


   No_Cell_Values : constant Cell_Value_List_T (1 .. 0) :=
      (others => (Cell => No_Cell, Value => 0));


end Storage;
