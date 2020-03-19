-- Formats.ELF.Parsing (decl)
--
-- Parsing the hierarchical and sequential structure of symbol
-- tables in the ELF form.
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
-- $Revision: 1.9 $
-- $Date: 2015/10/24 20:53:53 $
--
-- $Log: formats-elf-parsing.ads,v $
-- Revision 1.9  2015/10/24 20:53:53  niklas
-- Moved to free licence.
--
-- Revision 1.8  2013/12/13 12:05:59  niklas
-- BT-CH-0263: Fix Formats.ELF.Parsing to redispatch on Action.
--
-- Revision 1.7  2010-02-20 07:49:34  niklas
-- Added Action_T.Function_Kinds, to let the ARM7 parser include
-- Loproc and Hiproc function symbols. GCC uses them for Thumb.
--
-- Revision 1.6  2008-10-28 10:13:37  niklas
-- Described Symbol_Denotation_T.Size.
--
-- Revision 1.5  2007/04/26 11:27:52  niklas
-- BT-CH-0059.
--
-- Revision 1.4  2006/04/13 19:16:47  niklas
-- Added choice of symbols based on global or non-global
-- binding, by the component Action_T.Take_Bindings and
-- the function Is_Included.
-- Corrected Function_Symbol to obey Action.Take_Subprograms.
-- Corrected Object_Symbol and Other_Symbol to obey Action.Take_Cells
-- and modified them to emit Notes only if they would otherwise have
-- connected the symbol to a cell.
--
-- Revision 1.3  2006/04/12 19:49:55  niklas
-- Added the "out" parameter Parse_Symbols.Found to show if
-- an ELF symbol table was found.
--
-- Revision 1.2  2005/02/25 20:58:16  niklas
-- Changed all Symbol_Table_T parameters from "in out" to "in" as
-- befits a reference type. Related to BT-CH-0004.
--
-- Revision 1.1  2004/04/24 18:04:32  niklas
-- First version.
--


with Processor;
with Symbols;


package Formats.ELF.Parsing is
--
-- An ELF symbol-table is a sequence of symbol records (ELF.Symbol_T).
-- There are a number of different kinds of symbol records; some
-- records define program entities (subprograms, variables) while other
-- records define the context (module, source file) of the entities.
--
-- This package provides an operation to traverse an ELF symbol table,
-- interpret the symbol records and enter them in a Bound-T symbol-table
-- structure. The operation is parametrized by an "action" class so that
-- specific steps in the parsing process can be modified by creating
-- a subclass that overrides specific operations of the action class.


   --
   ---   Symbols and bindings
   --


   subtype Symbol_Kind_T is Symbol_Info_T range 0 .. 15;
   --
   -- Symbol kind (from St_Info).
   -- Values listed as Formats.Stt_xxx.


   type Symbol_Kind_Set_T is array (Symbol_Kind_T) of Boolean;
   --
   -- A set of symbol kinds.


   subtype Symbol_Binding_T is Symbol_Info_T range 0 .. 15;
   --
   -- Symbol binding (from St_Info).
   -- Values listed as Formats.Stb_xxx.


   type Bindings_T is (Global, Non_Global, Any);
   --
   -- Chooses symbols for processing depending on their binding level.
   --
   -- Global
   --    Only symbols with global binding.
   -- Non_Global
   --    Only symbols with non-global binding.
   -- Any
   --    All symbols.


   function Is_Included (
      Binding : Symbol_Binding_T;
      Within  : Bindings_T)
   return Boolean;
   --
   -- Whether the given Binding is included Within the given
   -- set of bindings.


   type Address_Space_T is (Code, Data, Unknown);
   --
   -- The kind of memory (object) that a symbol's address references.
   --
   -- Code
   --    An address in a code (instruction, text) area.
   -- Data
   --    An address in a data (operand) area.
   -- Unknown
   --    An address of unknown target type.


   type Symbol_Denotation_T is record
      Kind    : Symbol_Kind_T;
      Binding : Symbol_Binding_T;
      Size    : Uword_T;
      Value   : Addr_T;
      Space   : Address_Space_T;
   end record;
   --
   -- The essential machine denotation of an ELF symbol record.
   --
   -- Size
   --    The size associated with the symbol, in octets ("bytes").
   --    For the symbol of a data object (variable or constant) this
   --    is the size of the data object.
   --    Zero for an unknown or non-existent size.


   --
   ---   Control over parsing actions
   --


   type Parser_T is private;
   --
   -- Private internal state of an "action" object.


   Default_Function_Kinds : constant Symbol_Kind_Set_T := (
      Stt_Func   => True,
      Stt_Notype => True,
      others     => False);
   --
   -- The default set of symbol kinds that indicate a "function"
   -- (subprogram) symbol, when the symbol lies in Code space.


   type Action_T is abstract tagged limited record
      Take_Subprograms : Boolean           := True;
      Take_Cells       : Boolean           := True;
      Take_Bindings    : Bindings_T        := Any;
      Function_Kinds   : Symbol_Kind_Set_T := Default_Function_Kinds;
      Scope            : Symbols.Scope_Stack_T;
      Parser           : Parser_T;
   end record;
   --
   -- The "action" class that translates parsing steps to actions
   -- on the Bound-T context and symbol table.
   --
   -- Take_xxx
   --    These components specify whether to take into the resulting
   --    symbol-table the ELF definitions for subprograms and data
   --    cells (variables, parameters), respectively.
   -- Take_Binding
   --    Specifies inclusion or exclusion of symbols based on the
   --    binding level of the symbol. The default value (Any)
   --    includes all symbols.
   -- Function_Kinds
   --    The set of symbol-kinds that indicate a "function" symbol
   --    if the symbol lies in the Code space.
   -- Scope
   --    The current scope (context), updated during parsing.
   -- Parser
   --    Parsing state that may be maintained by the parsing
   --    process itself.


   --
   ---   Cell mapping actions
   --
   -- One of the overridable parsing actions is the mapping of ELF
   -- register/variable specifications to Bound-T processor-specific
   -- cell specs:


   No_Such_Cell : exception;
   --
   -- Raised by an Action_T operation if it cannot map the given
   -- ELF Stt_Object symbol-denotation to a cell-spec.


   function Object_Cell (
      Object : Symbol_Denotation_T;
      Action : Action_T)
   return Processor.Cell_Spec_T
   is abstract;
   --
   -- The (processor-specific) cell specified by an Stt_Object record
   -- or a Global symbol mapped to a data address.
   --
   -- Raises No_Such_Cell if the Object does not correspond to a
   -- (tracked) cell for the present target processor.
   --
   -- This function is abstract, because Processor.Cell_Spec_T is so
   -- target-specific that a general default implement is impossible.


   --
   ---   Parsing actions
   --
   -- Most of the primitive operations on an Action object have
   -- the following form:
   --
   --    procedure <ELF_Type_Name> (
   --       <Denotation> : in     Symbol_Denotation_T;
   --       Name         : in     String;
   --       Action       : in out Action_T;
   --       Symbol_Table : in     Symbols.Symbol_Table_T)
   --
   -- Such an operation represents the action to be taken for parsing
   -- (handling) an ELF symbol record of the type indicated by the
   -- procedure name (ELF_Type_Name) and perhaps the name of the first
   -- parameter (Denotation), with Name being the associated string.
   -- The action may have an effect on the Action state and sometimes
   -- results in adding some symbols to the Bound-T Symbol_Table.
   -- Note that Symbol_Table_T is a reference type so the "in" mode
   -- allows updates to the underlying symbol table object.


   procedure Source_File (
      File         : in     Symbol_Denotation_T;
      Name         : in     String;
      Action       : in out Action_T;
      Symbol_Table : in     Symbols.Symbol_Table_T);
   --
   -- Acts on an Stt_File record.
   --
   -- The default action (for the base type Action_T) is to first
   -- reset Action.Scope to the global scope, and then add a scope
   -- for Name to Action.Scope to make a one-level scope.


   procedure Function_Symbol (
      Fun          : in     Symbol_Denotation_T;
      Name         : in     String;
      Action       : in out Action_T;
      Symbol_Table : in     Symbols.Symbol_Table_T);
   --
   -- Acts on an Stt_Func record and also on an Stt_Notype record that
   -- maps to a code address.
   --
   -- The default action (for the base type Action_T) is the following:
   --
   -- > Pop the innermost level from Action.Scope, if Action.Scope has
   --   more than one level.
   --
   -- > If Action.Take_Subprograms is True and Action.Take_Bindings
   --   includes Fun.Binding, connect the identifier part of the Name
   --   string to the associated code address as a subprogram symbol
   --   in the Symbol_Table in the current Action.Scope.
   --
   -- > If the symbol binding is not global, add the identifier part of
   --   the Name string to Action.Scope as the new innermost level
   --   (replacing the popped level if any).
   --
   -- These actions are taken only if the symbol maps to a code address;
   -- otherwise a Note is emitted and no other action is taken.


   procedure Object_Symbol (
      Object       : in     Symbol_Denotation_T;
      Name         : in     String;
      Action       : in out Action_T;
      Symbol_Table : in     Symbols.Symbol_Table_T);
   --
   -- Acts on an Stt_Object record.
   --
   -- The default action (for the base type Action_T) is null unless
   -- Action.Take_Cells is True and Action.Take_Bindings includes
   -- Object.Binding. If the symbol maps to a data address and has a
   -- positive size (St_Size not zero), the operation connects the
   -- Name string to the processor cell defined by Object_Cell;
   -- otherwise, the operation emits a Note and takes no other action.


   procedure Other_Symbol (
      Denotation   : in     Symbol_Denotation_T;
      Name         : in     String;
      Symbol       : in     Symbol_T;
      Action       : in out Action_T;
      Symbol_Table : in     Symbols.Symbol_Table_T);
   --
   -- Acts on any other type of ELF symbol record.
   --
   -- The default action (for the base type Action_T) is null unless
   -- Action.Take_Cells is True, the symbol has a Global binding and
   -- Action.Take_Bindings includes global symbols. If the symbol maps
   -- to a data address, the operation operation connects the Name string
   -- to the processor cell defined by Object_Cell; otherwise, the
   -- operation emits a Note and takes no other action.
   --
   -- The Symbol parameter provides the full ELF symbol record in case
   -- an overriding operation needs it, in addition to the Denotation.


   procedure Parse_Symbols (
      From         : in     File_Ref;
      Trace        : in     Boolean := False;
      Action       : in out Action_T'Class;
      Symbol_Table : in     Symbols.Symbol_Table_T;
      Found        :    out Boolean);
   --
   -- Reads and parses an ELF symbol-table from a loaded ELF file,
   -- if the file contains one.
   --
   -- From
   --    The loaded ELF file.
   -- Trace
   --    If true, the symbol records are displayed on standard output
   --    one by one as they are processed.
   -- Action
   --    Defines the actions to be taken for each symbol record.
   -- Symbol_Table
   --    The internal symbol-table to which (selected) ELF symbols
   --    are added.
   -- Found
   --    Whether the ELF symbol-table was found or not.
   --
   -- The operation traverses the ELF symbol records in sequential order
   -- while maintaining Action.Scope, which is initially set to empty
   -- (global scope). For each ELF symbol record, the operation calls the
   -- primitive Action operation corresponding to the type of the
   -- record, which may update the current Scope and the Symbol_Table.
   -- However, for any symbol record with global binding, the operation
   -- first makes Action.Scope global, before calling the Action operation
   -- for this record.
   --
   -- If Action.Take_Bindings is Global, the operation skips all symbols
   -- with non-Global binding (the primitive Action operations are not
   -- called for such symbols).


private


   type Parser_T is null record;
   --
   -- The private internal state for parsing an ELF symbol table.


end Formats.ELF.Parsing;
