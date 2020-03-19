-- Processor (decl)
--
-- Processor dependent definition for the Renesas (ex Hitachi) H8/300 processor.
--
-- Authors:
--    Samuel Petersson, Mälardalen University
--    Niklas Holsti, Tidorum Ltd
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
-- $Date: 2015/10/26 22:19:15 $
--
-- $Log: processor.ads,v $
-- Revision 1.18  2015/10/26 22:19:15  niklas
-- Version 4b, and moved to free open-source licence.
--
-- Revision 1.17  2015/10/26 22:01:42  niklas
-- Updated to current generic/target interface.
--
-- Revision 1.16  2009/12/29 13:37:48  niklas
-- Added Zero_Code_Offset and Image (Code_Offset_T) per BT-CH-0196.
-- This required making Code_Offset_T a type, not a subtype, which
-- requires a separate Shift function for Code addresses and offsets.
-- Added Instruction_Role_T per BT-CH-0197.
--
-- Revision 1.15  2009-12-02 14:20:01  niklas
-- BT-CH-0188: H8/300 updates for bit-widths and Word_T.
--
-- Revision 1.14  2008-02-02 14:52:25  niklas
-- Updated for BT-CH-0108: added Code_Address_List_T and All_Addresses.
--
-- Revision 1.13  2006/11/05 21:19:37  niklas
-- BT-CH-0036: Property BCC_Signed.
--
-- Revision 1.12  2006/10/30 23:09:53  niklas
-- Updated for BT-CH-0033.
--
-- Revision 1.11  2006/08/23 19:32:43  niklas
-- Updates for BT-CH-0025.
--
-- Revision 1.10  2005/09/03 11:52:20  niklas
-- BT-CH-0006.
--
-- Revision 1.9  2005/05/11 18:02:50  niklas
-- Removed the Compose steps.
--
-- Revision 1.8  2005/04/01 12:28:29  niklas
-- Added the Step_Kind_T literals Cmp_Subx and Sub_Subx.
-- Made Cell_Spec_T a definite type (added default Kind).
--
-- Revision 1.7  2005/03/25 16:55:56  niklas
-- Extended for analysing dynamic jumps via address tables as follows.
-- Added the Step_Kind_T literal Jump_Via_Table.
-- Added the Cell_Kind_T literal Jump_Index, for a (single) synthetic
-- cell that holds the index (octet offset) in the address table.
-- Defined Loose_Edge_Info_T as a discriminated record. The variant
-- Jump_Via_Table carries the base address of the table of jump
-- addresses between the two instructions (mov.w and jmp @Rn) that
-- form the jump-via-table pattern.
--
-- Revision 1.6  2005/03/23 20:01:16  niklas
-- Added the Step_Kind_T literal Add_Addx to represent an
-- Add_With_Carry instruction for which the arithmetic effect is
-- combined into the effect of the preceding Add instruction.
--
-- Revision 1.5  2005/03/22 20:38:15  niklas
-- Support arithmetic analysis in the Decoder as follows.
-- Added Step_Kind_T literals Compose and Clobber.
-- Added subtype Address_Value_T.
-- Replaced the Cell_Kind_T literals for the condition flags by
-- two Boolean cells, Equal and Less, which will be defined only
-- to model the relationship between unsigned integer values.
-- Split the Cell_Kind_T literals Param and Local into Param_Byte,
-- Param_Word, Local_Byte and Local_Word and defined their
-- components in Cell_Spec_T.
--
-- Revision 1.4  2005/03/17 07:12:36  niklas
-- Added the Stack_Height cell-kind.
--
-- Revision 1.3  2005/01/26 19:33:15  niklas
-- Changed Effort_T to a count of execution states and Power_T to null.
-- The total time for an instruction / a step is now computed in the
-- Decoder by adding the number of states for each cycle and taking into
-- account the kind of memory access in each cycle, using functions in
-- the new package Processor.Timing and a device-specific memory map.
--
-- Revision 1.2  2004/09/27 08:45:39  niklas
-- Added type Memory_T and its Image and Alias_Range functions, to
-- support the use of memory-reference variables and expressions in
-- the Arithmetic model.
--
-- Revision 1.1  2004/06/16 07:41:40  niklas
-- First version.
--


with Generic_Discrete_Ranges;
with H8_300;


package Processor is
--
-- Processor-dependent constants and types, chiefly the step-address and the
-- data cell.
--
-- This package is highly target-dependent, but the provided interface defined
-- below is required for all targets as it is used by the target-independent
-- parts.
--
-- Additional target-dependent items may be provided for use by other
-- target-dependent parts such as the Decoder package.


   --
   ---   Target-processor identification
   --


   Name : constant String := "Renesas H8/300";
   --
   -- Names the processor architecture.
   -- It is used chiefly to identify the version of the Bound-T tool
   -- in output files.


   --
   ---   Program sequencing model
   --
   -- The program sequencing model separates that part of the processor
   -- state that is statically enumerated in the control-flow graph, from
   -- the rest of the state which is considered a dynamic attribute of
   -- the execution.


   type Address_T is mod 2**16;
   --
   -- A memory address.
   --
   -- The H8/300 is a "von Neumann" architecture with a common address
   -- space for code and data. The memory address unit is an octet and
   -- the maximum memory size is 64 kilo-octets. Octet order is big-endian.


   subtype Code_Address_T is Address_T;
   --
   -- Identifies a location in the code (program) memory.
   -- For example, the entry point of a subprogram, or the instruction
   -- associated with a program label or a source-code line-number.
   --
   -- H8/300 instructions are either 16 or 32 bits long and must be
   -- aligned so that the instruction starts at an even address.
   -- The even address contains the more significant instruction
   -- octet and the next odd address contains the less significant
   -- octet. For 32-bit instructions, the second 16-bit word follows
   -- in the next two octets, again in big-endian order.
   --
   -- Although instructions must be aligned at even addresses, the
   -- program can use an odd address; the hardware automatically overrides
   -- the least significant bit to fetch the instruction from the even
   -- and odd addresses as explained above.
   --
   -- The required operations on Code_Address_T are the predefined attributes
   -- 'First, 'Last, 'Min, 'Max and the relational operators "=" and "<",
   -- for which the predefined operators are good here, and the further
   -- operations declared below.


   Code_Address_Default : constant Code_Address_T := Code_Address_T'First;
   --
   -- A meaningless but formally valid (in the Ada sense) value
   -- of the code address, used for default initializations and
   -- dummy values.


   function Hash (Item : Code_Address_T) return Natural;
   --
   -- A hash function for code addresses is required to implement
   -- various hash tables keyed on code addresses.


   function Image (Item : Code_Address_T) return String;
   --
   -- A textual display of the code address, for use in outputs of
   -- various kinds, mainly for human reading.
   --
   -- If DOT output is used, the result must be embeddable in the
   -- DOT syntax as part of a node label.


   type Address_Offset_T is range -2**16 .. 2**16;
   --
   -- An address offset, measured as a signed number of octets.


   function Shift (Base : Address_T; Offset : Address_Offset_T)
   return Address_T;
   --
   -- The address Base + Offset.


   type Code_Offset_T is new Address_Offset_T;
   --
   -- An offset to a code address. Required for assertions that
   -- identify loops using code offsets from the start of the
   -- containing subprogram.
   --
   -- This is made a new type, rather than a subtype, because we
   -- want to override the Image function with a hexadecimal form.


   Zero_Code_Offset : constant Code_Offset_T := 0;
   --
   -- A zero offset, meaning no offset at all.


   function Image (Item : Code_Offset_T) return String;
   --
   -- Hexadecimal form with a leading '-' for negative values.


   function Shift (Base : Code_Address_T; Offset : Code_Offset_T)
   return Code_Address_T;
   --
   -- The address Base + Offset.


   package Code_Ranges is new Generic_Discrete_Ranges (
      Base_Type => Code_Address_T,
      Image     => Image);
   --
   -- Ranges of Code_Addresses.


   type Code_Address_Range_T is new Code_Ranges.Range_Type;
   --
   -- A range of code addresses First .. Last.
   -- This is used, for example, in the mapping from symbolic variable
   -- to storage location, which can vary from point to point in the
   -- program code.


   Empty_Code_Address_Range : constant Code_Address_Range_T :=
      Code_Address_Range_T (Code_Ranges.Empty_Range);
   --
   -- An empty range of code addresses.


   Full_Code_Address_Range : constant Code_Address_Range_T :=
      Code_Address_Range_T (Code_Ranges.Full_Range);
   --
   -- The universal range, comprising all code addresses.


   type Step_Kind_T is (
      Normal,
      Add_Addx,
      Cmp_Subx,
      Sub_Subx,
      Jump_Via_Table,
      EEPMOV,
      Clobber);
   --
   -- The kinds of steps (flow states) that can occur in the flow-graph.
   --
   -- Normal
   --    A step that represents a normal instruction.
   --
   -- Add_Addx
   --    A step that represents an octet-Add_With_Carry (addx) instruction
   --    but combined with the immediately preceding Add instruction to make
   --    a 16-bit addition. This is used for the following code pattern:
   --
   --        add.b #xx,RnL
   --        addx  #yy,RnH
   --
   --    This instruction pair adds the 16-bit immediate value yy:xx to the
   --    16-bit register Rn. This arithmetic effect is attached to the
   --    Normal step for the "add.b" instruction; the Add_Addx step will
   --    have a null effect (but the normal effort).
   --
   --    Note that if the "addx" instruction can be reached through a jump,
   --    it will *also* be represented by a Normal step with the normal
   --    effect. Even if the jump is preceded by an "add.b" instruction,
   --    the two will not be combined; they are combined only if they
   --    are physically consecutive, adjacent instructions.
   --
   -- Cmp_Subx
   --    A step that represents an octet-Subtract_With_Carry (subx)
   --    instruction, but combined with the immediately preceding Compare
   --    instruction to make a 16-bit compare (with destruction of RdH).
   --    This is used for the following code pattern:
   --
   --       cmp.b #xx,RnL
   --       subx  #yy,RnH
   --
   --    This instruction pair subtracts the 16-bit immediate value yy:xx
   --    from the 16-bit register Rn, altering RnH and leaving RnL unchanged.
   --    The arithmetic effect on the flags is attached to the Normal step
   --    for the "cmp.b" instruction; the effect on RnH (which becomes
   --    opaque) is attached the Cmp_Subx step.
   --
   --    See note for Add_Addx regarding jumps to the second instruction
   --    in the pattern.
   --
   -- Sub_Subx
   --    A step that represents an octet-Subtract_With_Carry (subx)
   --    instruction, but combined with the immediately preceding Subtract
   --    instruction to make a 16-bit subtraction.
   --    This is used for the following code pattern:
   --
   --       sub.b #xx,RnL
   --       subx  #yy,RnH
   --
   --    This instruction pair subtracts the 16-bit immediate value yy:xx
   --    from the 16-bit register Rn. The arithmetic effect is attached
   --    to the Normal step for the "sub.b" instruction; the Sub_Subx step
   --    will have a null effect (but the normal effort).
   --
   --    See note for Add_Addx regarding jumps to the second instruction
   --    in the pattern.
   --
   -- Jump_Via_Table
   --    A step that represents a Jump Register_Indirect (jmp @Rn)
   --    instruction where the value of the register comes from the
   --    immediately preceding Move instruction that reads a table
   --    of addresses (a switch table). The Move instruction has the
   --    form "mov.w @(base,Ri),Rn". The jump is modelled by a dynamic
   --    dynamic edge that depends on the index register Ri. Note that
   --    Ri and Rn may be the same register.
   --
   -- EEPMOV
   --    A synthetic step that is used to model the loop that is intrinsic
   --    to the EEPMOV instruction.
   --
   -- Clobber
   --    A synthetic step that is used to model the clobbering (veiling)
   --    of the sub/super-cells of the EEPMOV counter and pointers, on
   --    exit from the EEPMOV loop.


   type Flow_State_T is record
      Address : Code_Address_T;
      Kind    : Step_Kind_T;
   end record;
   --
   -- Represents the full program-sequencing state of the target processor,
   -- i.e. uniquely identifies a step in the control-flow graph (in the
   -- current generic "context" that is not target-specific).
   --
   -- Address
   --    The address of the instruction that the step represents (perhaps
   --    together with other, synthetic steps).
   -- Kind
   --    The kind of the step.
   --
   -- The required operations are the equality operator "=", for which
   -- the predefined equality is good here, and the operations defined
   -- below.


   function "<" (Left, Right : Flow_State_T) return Boolean;
   --
   -- A complete ordering of control-flow states is required for
   -- two reasons. The first reason is technical: the implementation
   -- of control-flow graphs needs an ordering to make the search for
   -- a step with a given flow-state faster than linear. The second
   -- reason is user-friendliness: when the control-flow graph is
   -- decoded, and several "loose" edges are available from which to
   -- pick the next step to be decoded, it is most understandable
   -- to pick them in order of increasing address.
   --
   -- The "<" operation must obey the ordinary rules for a complete
   -- ordering. If X and Y are two flow states, then:
   --
   --    X <  Y  must imply X /= Y.
   --    X /= Y  must imply X < Y or Y < X.


   function Prime_Address (Item : in Flow_State_T)
   return Code_Address_T;
   --
   -- The "primary" code-address corresponding to this step, for example
   -- the address of the instruction being executed rather than the
   -- address of some other instruction in the pipeline.
   --
   -- This function is used to collect information that describes and
   -- identifies the step, such as source-line numbers. It is not
   -- used for the analysis.


   type Code_Address_List_T is array (Positive range <>) of Code_Address_T;
   --
   -- A list of code addresses, for example all addresses involved in some
   -- flow-state, not necessarily in order.


   function All_Addresses (Item : in Flow_State_T) return Code_Address_List_T;
   --
   -- All code addresses involved in the given flow-state.
   -- For H8/300, this is simply the Prime_Address.


   function Image (Item : Flow_State_T) return String;
   --
   -- A textual display of the flow state, for use in outputs of
   -- various kinds, mainly for human reading.
   --
   -- If DOT output is used, the result must be embeddable in the
   -- DOT syntax as part of a node label.


   --
   ---   Execution time model
   --
   -- The execution time model expresses the amount of processor "work"
   -- represented by instructions and other state transitions, the
   -- concept of processor "speed" and the final execution time.


   type Time_T is new Natural;
   --
   -- Counts execution time in units of execution states, also known
   -- as clock states.


   No_Time : constant Time_T := 0;
   --
   -- Represent a zero execution time.


   type Cycle_Type_T is (
      Fetch,
      Address,
      Stack,
      Byte,
      Word,
      Internal);
   --
   -- The execution time of an instruction is computed from the number
   -- of cycles of various types that the instruction performs:
   --
   -- Fetch
   --    Fetching an instruction from memory.
   -- Address
   --    Reading a branch target address from memory.
   -- Stack
   --    Operating (pushing/popping) the stack.
   -- Byte
   --    Accessing a data byte in memory.
   -- Word
   --    Accessing a data word in memory.
   -- Internal
   --    Internal processor cycles.
   --
   -- Each cycle performed contributes one or more execution states to
   -- the total execution time. The number of execution states per cycle
   -- depends on the cycle type and, for cycles that access memory, on
   -- which memory -- internal, external, register field -- is accessed.
   -- For external memory, the time can depend on a number of wait-states
   -- which may depend on whether the access reads or writes memory.
   --
   -- The H8/300 manuals document the number of cycles of each type
   -- that a given instruction executes. However, for the above reasons
   -- that cycle-vector is not enough to define the execution time.
   -- The instruction decoder must use its analysis of the instruction
   -- to find the number of execution states for each cycle in the
   -- instruction, and add them up to give the total effort for the
   -- instruction as a total number of execution states.


   type Effort_T is new Time_T;
   --
   -- Represents the amount of effort or work the processor must do to
   -- execute an instruction, or an instruction sequence. The effort
   -- is stated as a number of execution states.


   No_Effort : constant Effort_T := 0;
   --
   -- A null effort; the processor is not doing anything.


   function Memory_Reads (Effort : Effort_T) return Natural;
   --
   -- The number of memory read accesses required by a given effort.
   -- This is used only as annotation in the HRT Execution Skeleton,
   -- for later _adjustment_ of the effect of memory wait-states.
   -- Note that the extra time caused by memory wait-states _must_
   -- be included in the basic Effort, Power, and Work computations;
   -- the number of memory accesses reported by this function has no
   -- effect on the worst-case analysis or WCET in Bound-T.
   --
   -- This function is not supported for H8/300 and returns zero.


   function Memory_Writes (Effort : Effort_T) return Natural;
   --
   -- The number of memory write accesses required by a given effort.
   -- See Memory_Reads above.
   --
   -- This function is not supported for H8/300 and returns zero.


   function Image (Item : Effort_T) return String;
   --
   -- A textual display of the effort, for use in outputs of various
   -- kinds, mainly for human reading.
   --
   -- If DOT output is used, the result must be embeddable in the
   -- DOT syntax as part of a node label.


   type Work_T is new Time_T;
   --
   -- The total computational work required for performing a sequence
   -- of instructions (or steps), including the control-flow edges between
   -- the instructions (or steps).
   --
   -- Since the H8/300 is a fully sequential processor, the work can
   -- be modelled simply by the total execution time (number of
   -- execution states).


   No_Work : constant Work_T := Work_T (No_Time);
   --
   -- No work to speak of.


   type Power_T is null record;
   --
   -- The processor power (speed), which can in principle vary from point
   -- to point in the program (down to a granularity of basic blocks).
   -- The power determines the amount of time required to execute a
   -- given effort.
   --
   -- For the H8/300, Power_T is not used (is null) because the final
   -- execution time for each instruction is set by the instruction
   -- decoder as the Effort of that instruction. The conversions from
   -- Effort_T to Work_T and from Work_T to Time_T are simple value-
   -- preserving type conversions with no role for a Power_T.


   procedure Add_Step (
      Taking : in     Effort_T;
      Using  : in     Power_T;
      To     : in out Work_T);
   --
   -- Adds the execution of a step taking a certain effort to a
   -- cumulative amount of work, using a given a amount of computational
   -- power.
   --
   -- If this step is the first step of a basic block, the initial
   -- value of To is No_Work.


   procedure Add_Edge (
      Taking : in     Time_T;
      Using  : in     Power_T;
      To     : in out Work_T);
   --
   -- Adds the execution of an edge (between two steps) taking a certain
   -- time, using a given amount of computational power, to a cumulative
   -- work. The edge is the only edge leaving the source step and the
   -- only edge entering the target step (i.e. it is an internal edge in
   -- a basic block). The time for other kinds of edges (edges between
   -- basic blocks) is included using normal integer addition along the
   -- path in the control-flow graph.


   function Time_To_Finish (
      Work  : Work_T;
      Using : Power_T)
   return Time_T;
   --
   -- The total (and worst-case) execution time taken to finish
   -- the given (cumulative) work using the given computational
   -- power. The given work is the output of a calling sequence of
   -- the form "Add_Step; for zero or more times loop Add_Edge; Add_Step;
   -- end loop;" where the steps and edges form a basic block in flow
   -- order.


   --
   ---   Arithmetic computation model
   --


   type Cell_Kind_T is (
      Equal, Less,
      RnH, RnL, Rn,
      Param_Byte,
      Param_Word,
      Local_Byte,
      Local_Word,
      Mem_Byte,
      Mem_Word,
      Stack_Height,
      Jump_Index);
   --
   -- The kinds of cells that can appear in arithmetic effects.
   --
   -- Equal, Less
   --    Synthetic Boolean cells that model the CCR flags C and Z.
   --    The Equal cell indicates a zero result from a subtraction or an
   --    addition of a negative literal, or an "equal" result from comparison.
   --    The Less cell indicates a negative result from a subtraction or an
   --    addition of a negative literal, or a "less" result from comparison.
   --    The negative result itself, however, is considered unknown.
   --    We do not model directly the CCR flags, but only their role as
   --    indicators of these conditions.
   -- RnH, RnL
   --    The High (RnH) and Low (RnL) 8-bit registers.
   --    There are 8 registers of each kind, numbered 0 .. 7.
   --    The registers are considered to hold unsigned values. If an
   --    operation stores a negative value in the register, the value is
   --    considered unknown.
   -- Rn
   --    The 16-bit registers, each composed of two 8-bit registers.
   --    There are 8 such 16-bit registers, numbered 0 .. 7.
   --    The Rn-cell number i consists of the RnH cell number i, as the
   --    high octet, and the RnL cell number i as the low octet.
   --    The registers are considered to hold unsigned values. If an
   --    operation stores a negative value in the register, the value is
   --    considered unknown.
   -- Param_Byte, Param_Word
   --    A parameter octet or 16-bit word in the stack.
   --    Parameters are pushed by the caller and located in the stack
   --    under (at higher addresses than) the callee's return address.
   --    The return address itself is also considered a parameter.
   --    From the caller's point of view it pushes Locals; the callee
   --    sees them as Params.
   -- Local_Byte, Local_Word
   --    A local octet or 16-bit variable in the stack.
   --    Local variables are pushed by the subprogram itself and located
   --    in the stack above (at lower addresses than) the return address.
   --    Locals pushed before a call may be accessed as Params by the
   --    callee.
   -- Mem_Byte
   --    An octet in memory.
   -- Mem_Word
   --    A 16-bit word in memory (big endian).
   -- Stack_Height
   --    Local stack height.
   --    The number of octets of stack space used by a subprogram for
   --    its return address (2 octets), local variables, and temporary
   --    space.
   -- Jump_Index
   --    A synthetic cell that records the value of the index register
   --    Ri in the jump-via-address-table sequence
   --
   --        mov.w @(base,Ri),Rn
   --        jmp @Rn
   --
   --    The Jump_Index cell is set to Ri (less a possible pre-decrement)
   --    in the first instruction (mov.w) and used in the dynamic edge
   --    originating at the second instruction (jmp). The Jump_Index cell
   --    is necessary when i = n (Ri and Rn are the same register) because
   --    then the index value (value of Ri at the first instruction) is
   --    no longer available at the jump.


   subtype Flag_Kind_T  is Cell_Kind_T range Equal      .. Less;
   subtype Reg_Kind_T   is Cell_Kind_T range RnH        .. Rn;
   subtype Local_Kind_T is Cell_Kind_T range Local_Byte .. Local_Word;
   subtype Param_Kind_T is Cell_Kind_T range Param_Byte .. Param_Word;
   subtype Mem_Kind_T   is Cell_Kind_T range Mem_Byte   .. Mem_Word;
   --
   -- Convenient classification of cell kinds.


   Cell_Width :
      constant array (Cell_Kind_T range RnH .. Mem_Word)
      of H8_300.Width_T := (
      RnH | RnL   => H8_300.Octet,
      Rn          => H8_300.Word,
      Param_Byte  => H8_300.Octet,
      Param_Word  => H8_300.Word,
      Local_Byte  => H8_300.Octet,
      Local_Word  => H8_300.Word,
      Mem_Byte    => H8_300.Octet,
      Mem_Word    => H8_300.Word);
   --
   -- The width of those real cells that are more than one bit wide.


   subtype Param_Offset_T is Address_Offset_T range 0 .. Address_Offset_T'Last;
   --
   -- A parameter cell is located in the stack, under (at larger address
   -- than) the return address (saved PC). The specific parameter is
   -- identified by an offset relative to the SP on entry to the callee,
   -- immediately after the JSR/BSR. The return address (saved PC)
   -- is thus at offset zero; the other (real) parameters are at
   -- offsets >= 2.


   subtype Local_Offset_T is Address_Offset_T range 1 .. Address_Offset_T'Last;
   --
   -- A local variable cell is located in the stack, above (at lower
   -- address than) the return address (saved PC). The specific variable
   -- is identified by an offset relative to the SP on entry to the
   -- callee, but taking the stack-growth direction as positive (so the
   -- difference in addresses is negated). The saved return address itself
   -- is not considered a local or a parameter, so a zero offset is not
   -- accepted.


   type Cell_Spec_T (Kind : Cell_Kind_T := Equal) is record
      case Kind is
      when Flag_Kind_T   => null;
      when Reg_Kind_T    => Num   : H8_300.Register_Number_T;
      when Param_Kind_T  => Param : Param_Offset_T;
      when Local_Kind_T  => Local : Local_Offset_T;
      when Mem_Kind_T    => Addr  : Address_T;
      when Stack_Height  => null;
      when Jump_Index    => null;
      end case;
   end record;
   --
   -- Identifies a storage cell.
   --
   -- Defines an arithmetic cell, including all the components that
   -- distinguish between registers, flags, memory locations etc. as
   -- required by the target processor. An arithmetic cell is any
   -- storage element that can contain a Value_T, even if it cannot
   -- contain all possible Value_T values.
   --
   -- The required operations include the "=" operator, for which the
   -- predefined equality is good here, and the other operations declared
   -- below.
   --
   -- Param
   --    The parameter offset will always be an even number for a
   --    Param_Word but can be even or odd for a Param_Byte.
   -- Local
   --    The local-variable offset will always be an event number for
   --    a Local_Word but can be even or odd for a Local_Byte.
   -- Addr
   --    The memory address will always be an even number for a
   --    Mem_Word but can be even or odd for a Mem_Byte.


   function "<" (Left, Right : Cell_Spec_T) return Boolean;
   --
   -- Cell_Spec_T is used as the key field of some tables.
   -- A comparison operator is required, but may of course be the
   -- predefined "<" when it is suitable.


   function Width_Of (Cell : Cell_Spec_T) return Positive;
   --
   -- The width (number of bits) of the storage Cell.


   function Can_Count (Cell : Cell_Spec_T) return Boolean;
   --
   -- Whether the cell can be a loop counter.


   function Image (Item : Cell_Spec_T) return String;
   --
   -- A graphic representation of the cell specification.
   -- Must be printable, and also must obey the rules imposed
   -- by the Calculator package for cell names (e.g. they may
   -- have to be valid "identifiers" without embedded blanks
   -- or special characters etc.)


   function Valid_Cell_Range (From, To : Cell_Spec_T)
   return Boolean;
   --
   -- Whether the two cells can define a range of cells in the same
   -- address space, From .. To. This is used, for instance, to define
   -- ranges of volatile memory locations.


   function Cell_In_Range (Cell : Cell_Spec_T; From, To : Cell_Spec_T)
   return Boolean;
   --
   -- Whether (some part of) the Cell lies in the range From .. To.
   -- Precondition: Valid_Cell_Range (From, To).
   -- If the Cell is not in the same address space as From .. To,
   -- the function returns False. It is not an error to ask about
   -- cells in different address spaces.


   --
   ---   Memory aliasing model
   --
   -- Two distinct Cell_Spec_Ts can sometimes refer to the same storage
   -- element, if this storage element can be accessed using different
   -- addressing mechanisms. Such cells are called "aliases". Aliasing
   -- is important for the arithmetic model because an assignment to one
   -- of the cells also changes the value in the other cell, although no
   -- assignment to the other cell is explicitly made.
   --
   -- The present aliasing model is a preliminary one and is not fully
   -- implemented in the general Bound-T modules.


   type Alias_Group_T is (TBD);
   --
   -- A grouping of all cell-specs such that two cell-specs in the same
   -- group are aliases if and only if they are identical. For two cell-specs
   -- from different groups, aliasing is possible if they can refer to
   -- intersecting "alias ranges", see below.


   function Alias_Group (Cell : Cell_Spec_T) return Alias_Group_T;
   --
   -- The alias group of the cell.


   type Alias_Range_T is array (Alias_Group_T) of Boolean;
   --
   -- Describes a subset of target-processor storage that can be aliased
   -- with a given kind of cell or given kind of dynamic memory reference.
   -- When two different Cell_Spec_Ts belong to different Alias Groups, they
   -- can be aliased only if the Alias Ranges associated with these Alias
   -- Groups have a non-empty intersection.
   --
   -- The required operations include an "or" operator to represent set
   -- union, for which the predefined "or" is good here, and the other
   -- operations declared below.
   --
   pragma Pack (Alias_Range_T);


   Isolated : constant Alias_Range_T := (others => False);
   --
   -- The empty alias range which means that the associated cell or
   -- storage access cannot be aliased with any other cell or access.


   function May_Alias (Left, Right : Alias_Range_T) return Boolean;
   --
   -- Whether the Left and Right alias ranges intersect, that is
   -- whether the associated cells or storage references can be
   -- aliased with each other.


   function Alias_Range (Group : Alias_Group_T) return Alias_Range_T;
   --
   -- The alias range that can be referenced by cell (specs) of the
   -- given alias group. If Alias_Group_T is a discrete type, this
   -- function can be implemented by an array.


   type Memory_T is (
      Frame,
      Stack,
      Data);
   --
   -- The memory areas that can be addressed dynamically (with a computed
   -- address).
   --
   -- Frame
   --    The subprogram stack frame within the Data memory area.
   --    A Frame cell is accessed using SP (R7) - offset TBC.
   -- Stack
   --    The working stack within the Data memory area.
   --    A Stack cell is accessed using SP (R7) + offset TBC.
   -- Data
   --    Data memory area.
   --    Includes the Frame and Stack areas.
   --    A Data cell is accessed using some general register other
   --    than SP (R7) as the base, plus or minus an offset.
   --
   -- The general registers R0 .. R7 cannot TBC be dynamically addressed.
   --
   -- The Frame and Stack areas are listed separately because an optimistic
   -- aliasing analysis may assume that Frame, Stack and general Data
   -- references do not alias with each other.


   function Image (Item : Memory_T) return String;
   --
   -- A readable description of a Memory_T.


   function Alias_Range (Memory : Memory_T) return Alias_Range_T;
   --
   -- The range of cells (aliases) that a given Memory (reference) can
   -- reach, without considering the possible values of the address
   -- expression.


   --
   ---   Attaching target-specific information to control-flow steps
   --


   type Step_Info_T is record
      Effort : Effort_T;
   end record;
   --
   -- Processor-dependent information associated with a step,
   -- generated and used by the instruction decoder.


   function Effort (Info : Step_Info_T) return Effort_T;
   --
   -- The computational effort for a step is assumed to depend on
   -- (be encoded in) the Step_Info recorded with that step.
   -- Note that if this step calls a subprogram, the effort for
   -- the execution of the callee is _not_ included.


   function Image (Item : Step_Info_T) return String;
   --
   -- A textual display of the step information, for use in outputs
   -- of various kinds, mainly for human reading.
   --
   -- If DOT output is used, the result must be embeddable in the
   -- DOT syntax as part of a node label.


   --
   ---   Attaching target-specific information to "loose" edges
   ---   in control-flow graphs under construction.
   --
   -- A "loose edge" is a control-flow edge in which the source step
   -- is fixed and the step-address of the target step is known, but
   -- the target step itself does not yet exist in the flow-graph.
   --
   -- Loose edges are used in the flow analysis as a "work-list". The
   -- main function of the Decoder module is to "bind" a given loose edge
   -- by creating the target step, and to generate more loose edges that
   -- leave the target step.
   --
   -- For some target processors, it is useful to let the Decoder add
   -- target-specific information to loose edges as it generates them
   -- for use when the Decoder is later called to bind a loose edge.


   type Loose_Edge_Kind_T is (
      Normal,
      Jump_Via_Table);
   --
   -- The kinds of loose edges.
   --
   -- Normal
   --    The normal kind. No special context information for the
   --    decoding of the target instruction.
   -- Jump_Via_Table
   --    The target instruction has already been inspected (peek-ahead)
   --    and found to be a jump-via-register that combines with the
   --    source Move instruction to form a jump-via-address-table.
   --    The base address of the address table is recorded in the
   --    loose edge to help create the dynamic edge for the jump.


   type Loose_Edge_Info_T (Kind : Loose_Edge_Kind_T := Normal)
   is record
      case Kind is
      when Normal         => null;
      when Jump_Via_Table => Base : Code_Address_T;
      end case;
   end record;
   --
   -- Processor-specific information for a loose edge.
   --
   -- Base
   --    The base address of the address-table, for the loose edge
   --    between the two instructions in the jump-via-table sequence:
   --
   --      mov.w @(base,Ri),Rn
   --      jmp @Rn



   No_Loose_Edge_Info : constant Loose_Edge_Info_T := (Kind => Normal);
   --
   -- Default information for a loose edge, for use when the Decoder
   -- creates a new loose edge without specifying the information to
   -- be attached.


   --
   ---   Target-specific assertable properties
   --


   type Property_T is (
      BCC_Signed);
   --
   -- The target-specific "properties" that can be asserted by the user
   -- in the assertion file.
   --
   -- BCC_Signed
   --    The value 1 means that this subprogram should be analysed
   --    as if the command-line option "-bcc=signed" were chosen.
   --
   -- The property-name used in the assertion is the 'Image of one
   -- of these enumeration images (but is not case-sensitive).
   --
   -- Such properties can define the calling protocols for subprograms,
   -- factors affecting processor speed such as wait-states, or any
   -- other information useful to the Decoder.


   --
   ---   Target-specific instruction roles:
   --


   type Instruction_Role_T is (
      No_Role);
   --
   -- The possible special roles of an instruction, when the role
   -- can be in doubt. The role of an instruction can be asserted
   -- and the Decoder can then choose the proper model and analysis.
   --
   -- No_Role
   --    No specific role is asserted.


end Processor;
