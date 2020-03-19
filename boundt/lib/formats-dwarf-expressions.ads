-- Formats.Dwarf.Expressions (decl)
--
-- DWARF expressions are byte-coded programs for a stack-oriented virtual
-- machine that computes with address-sized binary values, usually with
-- unsigned and wrap-around arithmetic. DWARF uses such expressions to
-- encode debugging information that depends on the state of the target
-- program, for example the address of a variable located in the stack
-- or relative to some other base register, or through an indirection
-- pointer chain. Other uses of expressions include dynamically computed
-- array bounds.
--
-- Expressions are not limited to straight-line reverse-Polish operator
-- strings but can contain conditionals, loops and subprogram calls.
--
-- Expressions can occur in DWARF as the values of attributes or as part
-- of special DWARF data-structures in separate sections. For example,
-- the DWARF debugging information entry for a variable can have an
-- attribute called Location with a value that is either directly an
-- expression (block form) or is a reference (loclistptr form) to a set
-- of expressions in the ".debug_loc" section. The latter form is used
-- when the location of the variable depends on the execution point in
-- the target program.
--
-- This package first defines the general structure of expressions and
-- the operations and operators that can occur in expressions, as well as
-- their encoding in a DWARF section.
--
-- This is followed by defining an abstract evaluator of DWARF expressions,
-- using an abstract tagged type to represent the value of the expression.
-- Most expression operators on this abstract value are given default
-- implementations that raise an exception ("Alien_Value") that means that
-- the expression is not evaluable in the present set of values. The
-- intent is that dedicated value types/representations will be derived
-- for specific uses of DWARF expressions, such as as location descriptions.
--
-- As one case, a value type is derived to represent simple locations,
-- where TBA
--
-- This is followed by definitions for specific uses of expressions, for example
-- as variable locators. TBC.
--
-- Finally, the TBA.
--
-- Author: Niklas Holsti.
--
-- A component of the Bound-T Worst-Case Execution Time Tool.
--
-------------------------------------------------------------------------------
-- Copyright (c) 1999 .. 2015 Tidorum Ltd except for text copied verbatim
-- from the DWARF standard.
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
-- $Date: 2015/10/24 20:53:52 $
--
-- $Log: formats-dwarf-expressions.ads,v $
-- Revision 1.7  2015/10/24 20:53:52  niklas
-- Moved to free licence.
--
-- Revision 1.6  2013-07-21 19:15:17  niklas
-- Fix encoding in comment.
--
-- Revision 1.5  2013-02-03 10:50:28  niklas
-- BT-CH-0238: DWARF expression abstract evaluation - start.
--
-- Revision 1.4  2009-01-18 08:36:22  niklas
-- Removed unused context clause.
--
-- Revision 1.3  2008/10/15 12:05:52  niklas
-- BT-CH-0150: DWARF location lists, lexical block scopes, CUBAs.
--
-- Revision 1.2  2007/06/14 11:16:40  niklas
-- BT-CH-0060.
--
-- Revision 1.1  2004/04/24 18:06:19  niklas
-- First version.
--


package Formats.Dwarf.Expressions is


   --
   ---   Operations
   --
   -- An operation consists of an operation code followed by zero or
   -- more (in-line) operands embedded in the expression. The number
   -- and form of the in-line operands depends on the operation in
   -- question. The operations often also use operands from the stack
   -- and update the stack in some way.


   type Operation_T is (

      -- Literal encodings to push a value on the stack.

      Lit,
      Addr,
      Const1u,
      Const1s,
      Const2u,
      Const2s,
      Const4u,
      Const4s,
      Const8u,
      Const8s,
      Constu,
      Consts,

      -- Push current object address

      Push_Object_Address,

      -- Register name operators (only in location expressions)

      Reg,
      Reg_X,

      -- Register based addressing to push a register+offset

      Frame_Base_Reg,
      Base_Reg,
      Base_Reg_X,

      -- Stack operations

      Dup,
      Drop,
      Over,
      Swap,
      Rot,
      Pick,

      -- Dereferencing (memory access) operations

      Deref,
      Deref_Size,
      X_Deref,
      X_Deref_Size,

      -- Unary arithmetic/logical operations

      Abs_Op,
      Neg,
      Not_Op,

      -- Operations with one stack and one literal operand

      Plus_Uconst,

      -- Binary arithmetic/logical operations

      Mul,
      Div,
      Mod_Op,
      Minus,
      Plus,

      And_Op,
      Or_Op,
      Xor_Op,

      Shl,
      Shr,
      Shra,

      -- Control flow operations

      Less_Or_Equal,
      Greater_Or_Equal,
      Equal,
      Less,
      Greater,
      Not_Equal,

      -- Skip, branch and call operations

      Skip,
      Branch,
      Call2,
      Call4,
      Call_Ref,

      -- Special operations

      Nop,

      -- Operations for locations

      Piece,
      Bit_Piece,
      Implicit_Value,
      Stack_Value,

      -- Tut tut

      Invalid_Opcode);
   --
   -- The operations that can occur in a DWARF expression.
   --
   -- A brief explanation of each operator/operation follows.
   -- If there is no mention of the in-line operands, there are none
   -- for the operation in question.
   --
   -- Lit
   --    There are 32 specific operations to push the literals
   --    from 0 to 31, inclusive. We represent all of them with one
   --    entry here; the value of the literal is given separately.
   -- Addr
   --    Pushes an address.
   --    In-line operands: The address (Addr form).
   -- Const1u .. Consts
   --    Introduce (push) signed or unsigned constants of various sizes,
   --    including the general LEB128 form.
   --    In-line operands: The constant value.
   -- Push_Object_Address
   --    Pushes the address of the object currently being evaluated as
   --    part of the evaluation of a user-presented expression.
   --    TBD what the "current object" means in Bound-T.
   -- Reg
   -- Reg_X
   --    These DWARF operations can be used to name a register (in a way,
   --    they "push" the "address" of the register, although registers
   --    are usually not addressable). These operations can be used only
   --    in location expressions. Each register name operator must be used
   --    alone (as a DWARF expression consisting of just that one operation).
   -- Reg
   --    There are 32 specific operations to name the registers number 0
   --    to 31, inclusive. We represent all of them with one entry here;
   --    the register number is given separately.
   -- Reg_X
   --    Names the register having the number given as an in-line operand.
   --    In-line operands: Unsigned LEB128 register number.
   -- Frame_Base_Reg
   --    Pushes the value of the Frame_Base attribute of the current
   --    subprogram, plus a signed literal offset. Since the Frame_Base
   --    attribute is also an expression, this implies a sub-evaluation
   --    of the Frame_Base expression followed by an addition of the
   --    signed literal offset.
   --    In-line operands: Signed LEB128 offset.
   -- Base_Reg
   --    Pushes the value of a given register plus a signed literal
   --    offset. There are 32 specific operations to use the base
   --    registers number 0 to 31, inclusive. We represent all of them
   --    with one entry here; the register number is given separately.
   --    In-line operands: Signed LEB128 offset.
   -- Base_Reg_X
   --    Like Base_Reg, but the number of the base register is given
   --    by an operand.
   --    In-line operands: Unsigned LEB128 register number; signed
   --    LEB128 offset.
   -- Dup
   --    Duplicates the value at the top of the stack.
   -- Drop
   --    Pops the value at the top of the stack.
   -- Over
   --    Pushes a copy of the second-from-top stack entry.
   --    Same as Pick (see below) with index = 1.
   -- Swap
   --    Swaps the two top stack entries.
   -- Rot
   --    Rotates the three topmost stack entries. The entry at the top
   --    of the stack becomes the third stack entry, the second entry
   --    becomes the top of the stack, and the third entry becomes the
   --    second entry.
   -- Pick
   --    Pushes a picked value from the stack onto the stack.
   --    In-line operands: One-byte unsigned pick-index, with zero
   --    meaning top of stack.
   -- Deref
   --    Pops the top stack entry, treats it as an address, retrieves a
   --    value from this address and pushes it. The retrieved value has
   --    the size of an address on the target machine.
   -- Deref_Size
   --    Like Deref but with an in-line operand that specifies the size,
   --    in octets, of the value that is retrieved from the address
   --    given in the top stack entry. The size must not exceed the size
   --    of an address on the target machine. The retrieved value is
   --    zero-extended to address-size before being pushed on the stack.
   --    In-line operands: Unsigned octet giving value-size.
   -- X_Deref
   --    Like Deref, but pops both an address (top of stack) and an
   --    address-space identifier (second from top). Retrieves an
   --    address-sized value from popped address within the popped
   --    address-space (implementation-defined mechanism) and pushes
   --    the retrieved value.
   -- X_Deref_Size
   --    Like Deref_Size with the same address-space extension as in
   --    X_Deref.
   --    In-line operands: Unsigned octet giving value-size.
   -- Abs_Op
   --    Interprets the top stack entry as a signed number and replaces
   --    it with the absolute value of this number.
   -- Neg
   --    Replaces the top stack entry with its arithmetic negation
   --    (change of sign).
   -- Plus_Uconst
   --    Adds an unsigned in-line operand to the top stack entry.
   --    In-line operands: unsigned LEB128 addend.
   -- Mul
   --    Pops the top two stack entries, multiplies them together (TBD
   --    if signed or unsigned), and pushes the result.
   -- Div
   --    Pops the top two stack values, divides the former second entry
   --    by the former top of the stack using signed division, and pushes
   --    the result.
   -- Mod_Op
   --    Pops the top two stack values and pushes the result of the
   --    calculation: former second entry modulo the former top of the
   --    stack.
   -- Minus
   --    Pops the top two stack values, subtracts the former top of the
   --    stack from the former second entry, and pushes the result.
   -- Plus
   --    Pops the top two stack values and pushes their arithmetic sum.
   -- Not_Op
   --    Replaces the top stack entry with its logical negation (bitwise
   --    complement).
   -- And_Op
   --    Pops the top two stack values and pushes their bitwise logical
   --    conjunction.
   -- Or_Op
   --    Pops the top two stack values and pushes their bitwise logical
   --    disjunction.
   -- Xor
   --    Pops the top two stack entries and pushes their bitwise logical
   --    exclusive disjunction.
   -- Shl
   --    Pops the top two stack entries, shifts the former second entry
   --    left by the number of bits specified by the former top of the
   --    stack, and pushes the result.
   -- Shr
   --    Pops the top two stack entries, shifts the former second entry
   --    right logically (filling with zero bits) by the number of bits
   --    specified by the former top of the stack, and pushes the result.
   -- Shra
   --    Pops the top two stack entries, shifts the former second entry
   --    right arithmetically (divide the magnitude by 2, keep the same
   --    sign for the result) by the number of bits specified by the
   --    former top of the stack, and pushes the result.
   -- Less_Or_Equal
   -- Greater_Or_Equal
   -- Equal
   -- Less
   -- Greater
   -- Not_Equal
   --    Each of the six relational operations
   --    > pops the top two stack values,
   --    > compares the former top of the stack with the former second
   --      entry, and
   --    > pushes the constant value 1 onto the stack if the result of
   --      the operation is true or the constant value 0 if the result
   --      of the operation is false.
   --    The comparisons are done as signed operations.
   -- Skip
   --    An unconditional branch. The single operand is a 2-octet signed
   --    integer constant giving the number of octets of the DWARF
   --    expression to skip from the current operation, beginning after
   --    the 2-octet constant. Note that this can be a forward branch or
   --    a backward branch.
   --    In-line operand: 16-bit signed skip count.
   -- Branch
   --    A conditional branch with one in-line operand giving a 2-octet
   --    signed integer constant as for Skip. The operation pops the top
   --    of the stack. If the value popped is not the constant 0, the
   --    branch occurs (octets are skipped), otherwise the branch does
   --    not occur (execution continues as of the skip amount were zero).
   --    In-line operand: 16-bit signed branch count.
   -- Call2
   -- Call4
   -- Call_Ref
   --    There operations  perform subroutine calls, invoking one DWARF
   --    expression (program) as a sub-expression of another. They differ
   --    only in the form and meaning of their single in-line operand:
   --
   --    > For Call2 and Call4, the in-line operand is a 2- or 4-octet
   --      unsigned offset, respectively, of a debugging information entry
   --      (DIE) in the current compilation unit.
   --
   --    > For Call_Ref, the in-line operand is a 32- or 64-bit unsigned
   --      value, depending on the DWARF bit-width. The operand is used
   --      as the offset of a debugging information entry in a .debug_info
   --      section which may be contained in a shared object or executable
   --      other than that containing the operator (relation to be performed
   --      by the consumer).
   --
   --    Operand interpretation for Call2, Call4 and Call_Ref is exactly
   --    like that of attribute values of form Ref2, Ref4 and Ref_Addr,
   --    respectively.
   --
   --    These operations transfer control of the DWARF expression
   --    evaluation to the Location attribute of the referenced DIE.
   --    If there is not such attribute, then there is no effect.
   --    Execution of the Location expression may add values to and/or
   --    remove values from the stack. Execution returns to the point
   --    following the call when the end of the Location attribute is
   --    reached.
   -- Nop
   --    This operation has no effect; it is a place holder.
   --
   -- The following "operstions" create composite location descriptions.
   -- A composite location description describes an object or value which
   -- may be contained in part of a register or stored in more than one
   -- location. Each piece is described by a composition operation, which
   -- does not compute a value nor store any result on the DWARF stack.
   -- There may be one or more composition operations in a single composite
   -- location description. A series of such operations describes the parts
   -- of a value in memory address order. Each composition operation is
   -- immediately preceded by a simple location description which describes
   -- the location where part of the resultant value is contained.
   --
   -- Piece
   --    This operation takes a single in-line operand which describes
   --    the size, in octets, of the piece of the object referenced by
   --    the DWARF expression whose result is at the top of the stack.
   --    If the piece is located in a register, but does not occupy the
   --    entire register, the placement of the piece within that register
   --    is defined by the ABI.
   --    In-line operands: Unsigned LEB128 number giving piece length.
   -- Bit_Piece
   --    This operation takes two in-line operands. The first is an unsigned
   --    LEB128 number that gives the size in bits of the piece. The second
   --    is an unsigned LEB128 number that gives the offset in bits from the
   --    location defined by the preceding DWARF location description.
   --    Interpretation of the offset depends on the kind of location
   --    description. If the location description is empty, the offset does not
   --    matter and the Bit_Piece operation describes a piece consisting of
   --    the given number of bits whose values are undefined. If the location
   --    is a register, the offset is from the least significant bit end of
   --    the register. If the location is a memory address, the Bit_Piece
   --    operation describes a sequence of bits relative to the location whose
   --    address is on the top of the DWARF stack using the bit numbering and
   --    direction conventions that are appropriate to the current language
   --    on the target system. If the location is any implicit value or stack
   --    value, the Bit_Piece operation describes a sequence of bits using the
   --    least significant bits of that value.
   --    Bit_Piece is used instead of Piece when the piece to be assembled into
   --    a value or assigned to is not byte-sized or is not at the start of a
   --    register or addressable unit of memory.
   -- Implicit_Value
   --    In a location expression, describes a variable that has a known
   --    value at compile-time, and is therefore not located in memory.
   --    This operation has two in-line operands. The first operand is
   --    an unsigned LEB128 number giving the length of the known value,
   --    in bytes. The second operand is a block that contains the value
   --    in the memory representation of the target machine.
   -- Stack_Value
   --    In a location expression, describes a variable that has a known
   --    value, or a value computed from other variables and constants.
   --    This variable is therefore not located in memory. The value is
   --    at the top of the DWARF expression stack (note that it is not
   --    necessarily known at compile-time). This operation is the last
   --    operation in its expression.
   -- Invalid_Opcode
   --    A place-holder for an invalid/undefined operation code.
   --
   -- Note that there is no operation for "end of program" or "return
   -- to caller". The expression evaluator/interpreter knowns the number
   -- of octets in the whole expression, and the evaluation ends simply
   -- when it runs off the end of the expression.


   subtype Const_Op_T is Operation_T range Const1u .. Consts;
   --
   -- The operations that push one constant (literal) operand.


   subtype Register_Name_Op_T is Operation_T range Reg .. Reg_X;
   --
   -- The register-name operations. Can be used only in location
   -- expressions and only as the entire location expression.


   subtype Pure_Stack_Op_T is Operation_T range Dup .. Rot;
   --
   -- The operations that only manipulate the stack and have no
   -- in-line or embedded operands.
   -- Pick is the only excluded stack-manipulation operation.


   subtype Unary_Stack_Op_T is Operation_T range Abs_Op .. Not_Op;
   --
   -- The integer-arithmetic and logical operations that use and
   -- update one stack operand.


   subtype Binary_Stack_Op_T is Operation_T range Mul .. Shra;
   --
   -- The integer-arithmetic and logical operations that use two
   -- stack operands and produce one.


   subtype Shift_Op_T is Operation_T range Shl .. Shra;
   --
   -- The shift operations.


   subtype Relation_Op_T is Operation_T range Less_Or_Equal .. Not_Equal;
   --
   -- The relational comparison operations.


   subtype Call_Op_T is Operation_T range Call2 .. Call_Ref;
   --
   -- The subexpression calling operations.


   type Register_Number_T is new Natural;
   --
   -- The number of a register in the target machine. The mapping to
   -- actual target registers is implementation-dependent but should be
   -- defined by the ABI committee for each target architecture.


   procedure Dump_Expression (
      Block  : in Block_T;
      Indent : in String);
   --
   -- Displays the expression in the Block on the standard output
   -- channel, operation by operation.
   --
   -- Indents (prefixes) each output line by the Indent string.
   --
   -- Does not display called expressions, only the call operations.
   --
   -- The position of the Block.Stream is irrelevant on entry and
   -- undefined on return.


   --
   ---   Evaluation of expressions using abstract(ed) value domains
   --


   type Abstract_Value_T is abstract tagged null record;
   --
   -- Abstract root for (abstract) models of the values of DWARF expressions.


   Alien_Value : exception;
   --
   -- When raised during the evaluation of a DWARF expression using a
   -- given value model, signifies that the expression produces a value
   -- that cannot be represented in this value model.


   -- The following primitive operations of Root_Value_T evaluate
   -- various kinds of DWARF expression operations. Most of
   -- them have default implementations that propagate Alien_Value.
   -- A client is expected to derive a new type from Item_T
   -- and to override suitable operations to produce reasonable
   -- results for the client's value model.
   --
   -- Pure stack operations are fixed (see Evaluator, below) and
   -- cannot be overridden, likewise for call operations.


   function Const (
      Op    : Const_Op_T;
      Value : Number_T)
   return Abstract_Value_T
   is abstract;
   --
   -- Evaluates a constant number into an Abstract_Value_T.
   -- Propagates Alien_Value if the domain does not support this.


   function Register_Name (
      Op  : Register_Name_Op_T;
      Num : Register_Number_T)
   return Abstract_Value_T
   is abstract;
   --
   -- Evaluates a register name as part of a location expression.
   -- Propagates Alien_Value if the domain does not support this.


   procedure Unary_Op (
      Op      : in     Unary_Stack_Op_T;
      Operand : in out Abstract_Value_T);
   --
   -- Evaluates a unary operation, Operand := <Op> Operand.
   -- The default implementation propagates Alien_Value.


   procedure Binary_Op (
      Op      : in     Binary_Stack_Op_T;
      Left    : in out Abstract_Value_T;
      Right   : in     Abstract_Value_T);
   --
   -- Evaluates the binary operation Left := Left <Op> Right.
   -- The default implementation propagates Alien_Value.


   procedure Relation (
      Op      : in     Relation_Op_T;
      Left    : in out Abstract_Value_T;
      Right   : in     Abstract_Value_T);
   --
   -- Evaluates the relational operation Operand := Operand <Op> Amount.
   -- The default implementation propagates Alien_Value.


   function To_Boolean (Relation : Abstract_Value_T)
   return Boolean;
   --
   -- Interprets the result of a Relation expression as a Boolean
   -- value, if possible, and otherwise propagates Alien_Value.
   -- The default implementation propagates Alien_Value.


   generic
      type Value_T is new Abstract_Value_T with private;
   procedure Evaluator (
      Expr   : in     Block_T;
      Result :    out Value_T);
   --
   -- Evaluation of a DWARF expression using a given value model.


   --
   ---   Simple location expressions
   --
   -- Some location expressions are simpler (more statically analysable)
   -- than others. We define a dedicated type for them.
   -- TBA to become a type derived from Abstract_Value_T.


   type Simple_Loc_Kind_T is (
      Addr,
      Reg,
      Frame,
      Based,
      Complex);
   --
   -- The kinds of simple locations that we consider, plus a marker
   -- for the Complex (non-simple) locations.
   --
   -- Kind              Expression (Operation_T)
   -- ----              ------------------------
   -- Addr              Addr or Lit or Const
   -- Reg               Reg or Reg_X
   -- Frame             Frame_Base_Reg
   -- Base              Base_Reg or Base_Reg_X
   -- Complex           all other forms (not simple).


   type Simple_Location_T (Kind : Simple_Loc_Kind_T := Addr) is record
      case Kind is
      when Addr =>
         Addr : Address_T;
      when Reg  =>
         Reg : Register_Number_T;
      when Frame =>
         Frame_Offset : Number_T;
      when Based =>
         Base_Reg    : Register_Number_T;
         Base_Offset : Number_T;
      when Complex =>
         Expr : Block_T;
      end case;
   end record;
   --
   -- A simple kind of location, plus a marker for Complex locations.
   --
   -- The offset fields are stored as Numbers, which may not have the
   -- range for extreme cases.
   --
   -- For a Complex location, the Expr block holdsthe original
   -- (non-simple) location expression, for further translation if
   -- possible (eg. target-specific translation).


   function Simple_Location (Expr : Block_T) return Simple_Location_T;
   --
   -- The properties of the location Expression, if it is a simple one,
   -- else a Complex value.
   --
   -- Precondition: Expr.Size_In_Octets > 0.


   --
   ---   Location lists
   --
   -- When the location (memory address or other location) of a variable
   -- object changes during the execution of the target program, the
   -- location may be defined by a location list instead of a single
   -- location expression.
   --
   -- A location list defines a mapping from the code address domain to the
   -- expression domain: address -> expression. The mapping is actually a
   -- relation in the sense that it may be undefined (null-valent) for
   -- some addresses and may associate multiple expressions (be multi-
   -- valent) with some addresses. The purpose of a location list is to
   -- define, for each code address at which the execution of the target
   -- program may be suspended (breakpoint), a location expression by
   -- which the debugger can find the variable. The location expression
   -- defines the location of the variable immediately before the target
   -- program executes the instruction at the suspension address. There
   -- are three cases depending on the number of location expressions
   -- that are associated with the suspension address in the location
   -- list:
   --
   -- > One location expression, this expression gives the location of the
   --   variable's current value, when evaluated with the current values
   --   of the target machine registers and memory content.
   --
   -- > More than one location expression: each expression gives the
   --   location of one copy of the variable's current value, which is
   --   present in as many different places (e.g. in memory and in one
   --   or several registers).
   --
   -- > No location expressions: the variable's current value is not
   --   stored available in any addressable storage unit at this point
   --   in the target program.
   --
   -- A location list is represented as a contiguous sequence of octets
   -- which form a sequence of zero or more true entries terminated by
   -- an end-of-list entry. A true entry is either a location entry or
   -- a base-address selection entry.
   --
   -- A location entry consists of
   --
   -- > A beginning address which gives the first code address at which
   --   this entry becomes valid. The address is relative to the current
   --   base address (see below).
   --
   -- > An ending address which gives the first code address at which
   --   this entry is no longer valid. This must be greater than the
   --   beginning address. The ending address is also relative to the
   --   current base address.
   --
   -- > A location expression describing the location of the variable
   --   object over the range from the beginning address (inclusive) to
   --   the ending address (not inclusive).
   --
   -- A base-address selection entry consists of
   --
   -- > An address that has the largest representable value (all 1's).
   --   (This value can never be the beginning address of an ordinary
   --   location entry because the ending address would have to be larger
   --   still, which is impossible.)
   --
   -- > An address which defines the appropriate base address for use in
   --   interpreting the beginning and ending relative address of
   --   subsequent entries of the location list up to the next base-
   --   address selection entry.
   --
   -- For location entries that come before the first base-address selection
   -- entry in the list, the applicable base address is the base address
   -- (Low_PC attribute) of the compilation unit that owns (refers to) this
   -- location list, the Compilation Unit Base Address (CUBA).
   --
   -- All the above address values are encoded as unsigned binary numbers
   -- in the Addr_Size of the current compilation.
   --
   -- The end-of-list entry is simply a pair of zero addresses.
   --
   -- A location list that has no true entries (consists of just the end-
   -- of-list entry) describes an object that exists in the source code
   -- but not in the executable target program.


   Location_List_Section_Name : constant String := ".debug_loc";
   --
   -- The name of the DWARF section that contains location lists.


   type Location_Entry_T is record
      Beginning : Code_Address_T;
      Ending    : Code_Address_T;
      Expr      : Block_T;
   end record;
   --
   -- A location (list) entry.


   type Location_List_T is array (Positive range <>) of Location_Entry_T;
   --
   -- A location list, usually containing only true entries (not
   -- base-address selection entries nor end-of-list entries).


   function To_Location_List (
      Block : Block_T;
      Base  : Code_Address_T)
   return Location_List_T;
   --
   -- The location list encoded in a given Block, with the given
   -- initial (default) Base address (the CUBA of the containing
   -- compilation unit).
   --
   -- The position of Block.Stream is irrelevant on entry and undefined
   -- on return.


   procedure Dump_Location_List (
      Block  : in Block_T;
      Base   : in Code_Address_T;
      Indent : in String);
   --
   -- Displays the location list Block on the standard output channel,
   -- entry by entry using Dump_Expression to display the location
   -- expressions. The parameter Base is the CUBA of the containing
   -- compilation unit. Each output line is indented (prefixed) with
   -- the Indent parameter.
   --
   -- The position of Block.Stream is irrelevant on entry and undefined
   -- on return.


   --
   ---   Simple location lists
   --
   -- A simple location list is a location list in which each entry
   -- has a simple kind of location (or is marked as Complex).


   type Simple_Location_Entry_T is record
      Beginning : Code_Address_T;
      Ending    : Code_Address_T;
      Location  : Simple_Location_T;
   end record;
   --
   -- An entry in a simple location list.


   type Simple_Location_List_T is
      array (Positive range <>) of Simple_Location_Entry_T;
   --
   -- A simple location list.


   function To_Simple (Item : Location_List_T)
   return Simple_Location_List_T;
   --
   -- The given list of locations converted to simple form, entry
   -- by entry, giving Complex entries where the form is not simple.


end Formats.Dwarf.Expressions;
