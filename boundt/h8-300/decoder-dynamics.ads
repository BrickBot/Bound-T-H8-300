-- Decoder.Dynamics (decl)
--
-- Dynamic accessing aspects of Renesas H8/300 instructions.
--
-- This package defines two kinds of dynamic data references:
--
-- > to the HW stack (via SP = R7 + displacement, push, pop)
--
-- > to data or code memory via some pointer register other than
--   the stack pointer.
--
-- This package also defines dynamic code references (jumps and calls):
--
-- > jump via an address table (eg. a dense switch/case).
--
-- > jump-to-subroutine via a register (that is, the target address
--   is taken from a register) or memory-indirect (vector slot).
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
-- $Revision: 1.8 $
-- $Date: 2015/10/26 22:19:12 $
--
-- $Log: decoder-dynamics.ads,v $
-- Revision 1.8  2015/10/26 22:19:12  niklas
-- Version 4b, and moved to free open-source licence.
--
-- Revision 1.7  2010/02/25 12:18:30  niklas
-- Renamed Call_Via_Register_T to Call_Via_Pointer_T, as it is
-- now used also for memory-indirect dynamic calls (JSR @@aa:8).
-- Added Take_Asserted_Target (.. Call_Via_Pointer_T ..) to let
-- dynamic calls be resolved by assertions.
--
-- Revision 1.6  2009-12-02 14:20:01  niklas
-- BT-CH-0188: H8/300 updates for bit-widths and Word_T.
--
-- Revision 1.5  2005-10-20 15:36:20  niklas
-- Updated to match BT-CH-0014 as follows.
-- Removed the Retur component of Call_Via_Register_T because
-- it is no longer necessary. Ditto the Retur parameter of the
-- function Call_Via_Register.
-- Updated the operations Index and Close for Call_Via_Register_T.
-- The Index operation now uses no Return point, and the Close
-- operation now adds no edge.
--
-- Revision 1.4  2005/09/05 11:37:18  niklas
-- Added Call_Via_Register_T as allowed by BT-CH-0007.
--
-- Revision 1.3  2005/05/11 18:18:08  niklas
-- Added the type Reference_T for the common parts (the components
-- Displacement and Width) of Stack_Reference_T and Mem_Reference_T.
-- This means that Stack_Reference_T is now also derived from
-- Interval_Pointer_T and has a Reduced_Alias_Range function.
-- Added functions that create a reference to the Low_Octet or
-- High_Octet of a referenced word, or to the Whole_Word that contains
-- a referenced octet. Added the function Width to show if a reference
-- points to an octet or a word.
--
-- Revision 1.2  2005/03/25 16:57:21  niklas
-- Added Jump_Via_Table_T.
--
-- Revision 1.1  2005/03/22 20:47:38  niklas
-- First version.
--


with Arithmetic;
with Arithmetic.Pointers;
with Flow;
with Flow.Indexed;
with H8_300;
with Processor.Program;
with Storage;
with Storage.Bounds;
with Storage.References;


package Decoder.Dynamics is


   --
   ---   Dynamic references with a Displacement and a Width.
   --


   type Reference_T
   is abstract new Arithmetic.Pointers.Interval_Pointer_T with record
      Displacement : Arithmetic.Value_T;
   end record;
   --
   -- A dynamic reference with the additional attributes of a static
   -- Displacement that modifies the dynamic address. The inherited
   -- component Width shows whether the reference refers to a word or
   -- an octet.
   --
   -- Inherited from Arithmetic.Pointers.Interval_Pointer_T:
   --    function  Alias_Range (for no Bounds)
   --    procedure Constrain
   --    function  Basis
   --    procedure Add_Basis_Cells (two of them)
   --    function  Is_Used
   --    function  Image
   --    function  Alias_Range (for general Bounds)
   --    function  Arithmetic_Alias_Range
   --    function  Referent (for general Bounds)
   --    function  Arithmetic_Referent (for Arithmetic Bounds)
   --    procedure Apply (two of them, general and Arithmetic Bounds).


   type Reference_Ref is access all Reference_T'Class;
   --
   -- A reference to a dynamic memory reference of Reference_T class.


   --
   ---   Dynamic references to Stack data:
   --


   type Stack_Reference_T is new Reference_T with null record;
   --
   -- A dynamic reference to a cell on the hardware stack, using
   -- the SP register (R7) plus a displacement.
   --
   -- Displacement (inherited)
   --    The reference is to SP + Displacement where SP refers to the
   --    value of the Stack Pointer (R7) before the relevant instruction
   --    is executed. In other words, a possible automatic Pre_Decrement
   --    is included in the Displacement value (and explains why it
   --    is a Value_T instead of H8_300.Displacement_T).
   -- Width (inherited)
   --    The width (octet or 16-bit word) of the referenced cell.
   --
   -- The reference can be resolved to a Local or a Param cell, depending
   -- on the value of the Displacement relative to the local stack height
   -- at the point of reference (before amy updates to SP that the
   -- containing instruction may cause).
   --


   type Stack_Reference_Ref is access all Stack_Reference_T'Class;
   --
   -- A reference to a dynamic memory reference of the stack-ref class.


   function Image (Item : Stack_Reference_T) return String;
   --
   -- Overrides Arithmetic.Pointers.Image.


   function Reduced_Alias_Range (
      Pointer  : Stack_Reference_T;
      Interval : Storage.Bounds.Interval_List_T)
   return Storage.Alias_Range_T;
   --
   -- Overrides (implements) Arithmetic.Pointers.Reduced_Alias_Range:


   function Referent (
      Pointer : Stack_Reference_T;
      Value   : Storage.Bounds.Value_List_T)
   return Storage.Cell_T;
   --
   -- Overrides (implements) Arithmetic.Pointers.Referent.


   --
   ---   Dynamic data references via some other Pointer register:
   --


   type Mem_Reference_T is new Reference_T
   with record
      Pointer      : H8_300.Register_Number_T;
   end record;
   --
   -- A dynamic reference via a Pointer register to data in memory.
   -- The inherited Expr component contains just the Pointer; the
   -- constant Displacement (which contains the possible Pre_Decrement)
   -- is kept separate to allow reuse of the same Expr (= the Pointer
   -- expression) for all references using the same Pointer.


   type Mem_Reference_Ref is access all Mem_Reference_T'Class;
   --
   -- A reference to a dynamic memory reference of the stack-ref class.


   function Image (Item : Mem_Reference_T) return String;
   --
   -- Overrides Arithmetic.Pointers.Image.


   function Reduced_Alias_Range (
      Pointer  : Mem_Reference_T;
      Interval : Storage.Bounds.Interval_List_T)
   return Storage.Alias_Range_T;
   --
   -- Overrides (implements) Arithmetic.Pointers.Reduced_Alias_Range:


   function Referent (
      Pointer : Mem_Reference_T;
      Value   : Storage.Bounds.Value_List_T)
   return Storage.Cell_T;
   --
   -- Overrides (implements) Arithmetic.Pointers.Referent.


   --
   ---   Creating dynamic references to data:
   --


   subtype Register_Indirect_Operand_T is 
      H8_300.Operand_T (Kind => H8_300.Register_Indirect);
   --
   -- An operand accessed indirectly via a register.


   function Reference (
      Operand : Register_Indirect_Operand_T;
      Width   : H8_300.Width_T)
   return Storage.References.Boundable_Ref;
   --
   -- The boundable reference that models a register-indirect Operand
   -- with a given Width.
   --
   -- This is either a Stack_Reference_T or a Mem_Reference_T,
   -- depending on Operand.Pointer.


   function Low_Octet (Word : Storage.References.Boundable_Ref)
   return Storage.References.Boundable_Ref;
   --
   -- Creates a dynamic reference to the low octet of the memory
   -- word at Word. Word must be a Reference_T to an even (aligned)
   -- address.


   function High_Octet (Word : Storage.References.Boundable_Ref)
   return Storage.References.Boundable_Ref;
   --
   -- Creates a dynamic reference to the high octet of the memory
   -- word at Word. Word must be a Reference_T to an even (aligned)
   -- address.


   function Whole_Word (Octet : Storage.References.Boundable_Ref)
   return Storage.References.Boundable_Ref;
   --
   -- Creates a dynamic reference to the memory word that contains
   -- the referent Octet. The Octet must be a Reference_T to an
   -- octet.


   --
   ---   Properties of dynamic references to data:
   --


   function Width (Ref : Storage.References.Boundable_Ref)
   return H8_300.Width_T;
   --
   -- The width of the referent.
   -- Ref must be a Reference_T.


   --
   ---   Jump via a table of addresses:
   --


   type Jump_Via_Table_T is new Flow.Indexed.Edge_T with record
      Base : Processor.Code_Address_T;
      Code : Processor.Program.Info_T;
   end record;
   --
   -- A dynamic jump of the form "jmp @Rn" were Rn has been read
   -- from an address-table using an instruction of the form
   -- "mov.w @(Base,Ri),Rn". The Index expression is thus the 16-bit
   -- register Ri which must be even (Aligned => 2).
   --
   -- The Code component provides access to the memory image of
   -- the target program, for reading the table of addresses.


   type Jump_Via_Table_Ref is access all Jump_Via_Table_T'Class;
   --
   -- A reference to a heap-allocated jump-via-table object.


   procedure Index (
      Edge  : in out Jump_Via_Table_T;
      Value : in     Arithmetic.Value_T;
      Graph : in     Flow.Graph_T);
   --
   -- Applies a new Value as an index to the indexable Edge, possibly
   -- giving new static edges in the Graph (usually only one new edge).
   --
   -- Overrides (implements) Flow.Indexed.Index.


   function Jump_Via_Table (
      Base  : Processor.Code_Address_T;
      Index : Arithmetic.Expr_Ref;
      Code  : Processor.Program.Info_T)
   return Flow.Dynamic_Edge_T;
   --
   -- A new jump-via-table object.


   --
   ---   Jump-to-subroutine via a register or memory vector
   --


   type Call_Via_Pointer_T is new Flow.Indexed.Edge_T with record
      Caller : Programs.Subprogram_T;
   end record;
   --
   -- A dynamic call of the form "jsr @Rn" or "jsr @@aa" were Rn is
   -- computed in some unspecified way (initially, we expect that Rn
   -- will be bounded to a single value), and likewise the memory vector
   -- at the address "aa". The Index expression is Rn which must be even
   -- (Aligned => 2) or Memory(aa, Word).
   --
   -- The other component specifies the Caller subprogram, which is
   -- needed to construct the call when a target address is resolved.


   type Call_Via_Pointer_Ref is access all Call_Via_Pointer_T'Class;
   --
   -- A reference to a heap-allocated call-via-pointer object.


   overriding
   procedure Index (
      Edge  : in out Call_Via_Pointer_T;
      Value : in     Arithmetic.Value_T;
      Graph : in     Flow.Graph_T);
   --
   -- Applies a new Value as an index to the indexable Edge, possibly
   -- giving new static edges in the Graph (usually only one new edge).
   -- For Call_Via_Register, the new edge usually leads to a synthetic
   -- call-step (also created here). However, for "integrated" callees
   -- the new edge is a loose edge to the entry point of the callee,
   -- provided with a new "integrated" context.


   overriding
   procedure Take_Asserted_Target (
      Edge   : in out Call_Via_Pointer_T;
      Target : in     Processor.Code_Address_T;
      Graph  : in     Flow.Graph_T);


   overriding
   procedure Close (
      Edge  : in out Call_Via_Pointer_T;
      Graph : in     Flow.Graph_T);
   --
   -- Takes note that the Edge will be closed (no more resolvents).
   -- If no calls were resolved for the Edge, emits a Warning that the
   -- call-via-pointer instruction is modelled as a no-operation.


   function Call_Via_Pointer (
      Index  : Arithmetic.Expr_Ref;
      Caller : Programs.Subprogram_T)
   return Flow.Dynamic_Edge_T;
   --
   -- A new call-via-pointer object.
   --
   -- For "jsr @Rn", the Index should be the expression "Rn".
   -- For "jsr @@aa", the Index should be the Memory cell at address "aa",
   -- of word width.


end Decoder.Dynamics;
