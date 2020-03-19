-- Formats.COFF.Parsing (decl)
--
-- Parsing the hierarchical and sequential structure of symbol
-- tables in the COFF form.
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
-- $Date: 2015/10/24 20:53:51 $
--
-- $Log: formats-coff-parsing.ads,v $
-- Revision 1.8  2015/10/24 20:53:51  niklas
-- Moved to free licence.
--
-- Revision 1.7  2007-04-26 11:27:51  niklas
-- BT-CH-0059.
--
-- Revision 1.6  2005/07/26 20:24:50  niklas
-- Extended Object_Kind to classify as a subprogram any symbol
-- with underived null type in a text segment (some GCC asm
-- library routines have these attributes).
--
-- Revision 1.5  2005/07/26 11:58:36  niklas
-- Added a variant of the Object_Kind function for symbols that
-- are not in a "real" section but have a section number -2 .. 0.
--
-- Revision 1.4  2005/07/25 19:35:01  niklas
-- Corrected the function Object_Kind to let the type derivation
-- chain of a Subprogram symbol have more derivations after the
-- initial DT_Fcn.
--
-- Revision 1.3  2005/03/25 18:28:44  niklas
-- Modified the default implementation of Object_Kind to require,
-- for the Subprogram kind, a type derivation of just DT_Fcn, but
-- to allow the section to contain data as well as code (text).
--
-- Revision 1.2  2005/03/16 21:27:43  niklas
-- Changed Symbol_Table parameters to "in" as befits a reference type.
--
-- Revision 1.1  2004/05/31 20:20:03  niklas
-- First version.
--


with Processor;
with Symbols;


package Formats.COFF.Parsing is
--
-- A COFF symbol-table is a sequence of symbol records (COFF.Symbol_T).
-- There are a number of different kinds of symbol records; some
-- records define program entities (subprograms, variables) while other
-- records define the context (module, source file, subprogram, block)
-- of the entities.
--
-- The are two kinds of "primary" symbol records (Kind = Symbol_Entry and
-- Kind = Symbol_Entry2). The other kinds of symbol records are called
-- "auxiliary" symbol records. Some primary symbol records may be followed
-- by zero or more auxiliary records that belong to the primary record.
-- Thus, the symbol-table is logically a list of primary symbol records
-- and each primary symbol record has a sub-list of its auxiliary symbol
-- records.
--
-- Some of the primary symbol records are "special" symbols that do not
-- represent single source-program entities like variables or subprograms,
-- but define context information such as source-file boundaries,
-- subprogram boundaries and block boundaries.
--
-- This package provides an operation to traverse a COFF symbol table,
-- interpret the symbol records and enter them in a Bound-T symbol-table
-- structure. The operation is parametrized by an "action" class so that
-- specific steps in the parsing process can be modified by creating
-- a subclass that overrides specific operations of the action class.
--
-- Overridable operations are provided to handle the primary symbol records,
-- whether special or normal. When a primary symbol record owns auxiliary
-- symbol records, the information from the auxiliary symbols is passed
-- as an input parameter to the operation that handles the owning primary
-- symbol.
--
-- The root operations for the special symbols are probably reusable as such
-- for many targets and uses of COFF files, because they simply build up or
-- tear down the Bound-T context (Scope_Stack_T) for the normal symbols.
-- The root operations for the normal symbols must probably be overridden
-- for each target, because they should create the corresponding "connections"
-- in the Bound-T symbol table, and these connections use target-specific
-- types such as Cell_Spec_T.


   type Parser_T is private;
   --
   -- Private internal state of an "action" object.


   type Action_T is abstract tagged limited record
      Take_Subprograms   : Boolean := True;
      Take_Cells         : Boolean := True;
      Block_Nesting      : Natural := 0;
      Scope              : Symbols.Scope_Stack_T;
      Parser             : Parser_T;
   end record;
   --
   -- The "action" class that translates parsing steps to actions
   -- on the Bound-T context and symbol table.
   --
   -- Take_xxx
   --    These components specify whether to take into the resulting
   --    symbol-table the COFF definitions for subprograms and data
   --    cells (variables, parameters), respectively.
   -- Block_Nesting
   --    The block nesting level = how many blocks have been entered
   --    by a ".bb" symbol record without a matching ".eb" symbol.
   -- Scope
   --    The current scope (context), updated during parsing.
   -- Parser
   --    Parsing state that may be maintained by the parsing
   --    process itself.
   --
   -- The "action" class is abstract because some of its primitive
   -- operations must be overridden in a derived type, specifically
   -- for each target processors.


   type Symbol_Denotation_T is record
      Sym_Type : Symbol_Type_T;
      Class    : Storage_Class_T;
      Value    : Ulong_T;
   end record;
   --
   -- The essential machine denotation of a COFF primary symbol record.


   --
   --    Object classification
   --
   -- One of the overridable parsing actions classifies a COFF symbol
   -- as representing a subprogram or a cell or something else.



   type Object_Kind_T is (Subprogram, Label, Data, Other);
   --
   -- The kind of object represented by a COFF primary symbol.
   --
   -- Subprogram
   --    A subprogram ("function" in COFF terms).
   -- Label
   --    A statement/instruction label.
   -- Data
   --    A data-storage cell (variable), simple or composite.
   -- Other
   --    Something else (type, component, ...)


   Subprogram_Class : constant array (Storage_Class_T) of Boolean := (
      C_Efcn    |
      C_Ext     |
      C_Stat    |
      C_Extdef  |
      C_Shadow  |
      C_Weakext => True,
      others    => False);
   --
   -- Whether a Storage_Class can be associated with a subprogram.
   --
   -- Some functions (_exit, _srand, _rand) seem to have storage class
   -- C_Shadow, but most are C_Ext or C_Stat.
   -- C_Fcn is exluded since it is used for the ".bf" and ".ef"
   -- marker symbols.


   Data_Class : constant array (Storage_Class_T) of Boolean := (
      C_Auto    |
      C_Reg     |
      C_Arg     |
      C_Regparm => True,
      others    => False);
   --
   -- Whether a Storage_Class can definitely be associated with a data cell.
   --
   -- In some cases (see Object_Kind below) other storage classes can
   -- also represent data cells.


   function Object_Kind (
      Object  : Symbol_Denotation_T;
      Section : Section_Header_T;
      Action  : Action_T)
   return Object_Kind_T;
   --
   -- The kind of object represented by the symbol denotation within
   -- a given COFF section.
   --
   -- The default classification is as follows:
   --
   -- Subprogram
   --    If  the storage class is in Subprogram_Class
   --    and either
   --       - the type derivation starts with DT_Fcn and
   --         the section is not a data-only section, or
   --       - the type is T_Null and the section contains code.
   --
   -- Label
   --    If the storage class is C_Label.
   --
   -- Data
   --    If the storage class is in Data_Class
   --    or (the storage class is C_Ext or C_Stat
   --        and the derived type does not start with DT_Fcn).
   --
   -- Other
   --    Otherwise.


   function Object_Kind (
      Object  : Symbol_Denotation_T;
      Section : Section_Number_T;
      Action  : Action_T)
   return Object_Kind_T;
   --
   -- The kind of object represented by the symbol denotation that is
   -- not in any (real) COFF section. That is, the Section number is
   -- in the range -2 .. 0.
   --
   -- The default classification is as follows:
   --
   -- Subprogram
   --    Never.
   --
   -- Label
   --    Never.
   --
   -- Data
   --    If the storage class is in Data_Class.
   --
   -- Other
   --    Otherwise.


   --
   --    Cell mapping actions
   --
   -- One of the overridable parsing actions is the mapping of COFF
   -- register/variable specifications to Bound-T processor-specific
   -- cell specs:


   No_Such_Cell : exception;
   --
   -- Raised by an Action_T operation if it cannot map the given
   -- symbol-denotation to a cell-spec.


   function Data_Cell (
      Object : Symbol_Denotation_T;
      Action : Action_T)
   return Processor.Cell_Spec_T
   is abstract;
   --
   -- The (processor-specific) cell specified by a Symbol_Entry or
   -- Symbol_Entry_2 symbol record that represents a data cell.
   --
   -- Precondition: Object_Kind (Object) = Data.
   --
   -- Raises No_Such_Cell if the Object does not correspond to a
   -- (tracked) cell for the present target processor.
   --
   -- There is no default action because this is an abstract operation
   -- that must be overridden.


   --
   --    Parsing actions
   --
   -- Most of the primitive operations on an Action object have
   -- the following form:
   --
   --    procedure <COFF_Type_Name> (
   --       <Denotation> : in     Symbol_Denotation_T;
   --       Name         : in     String;
   --       Action       : in out Action_T;
   --       Symbol_Table : in out Symbols.Symbol_Table_T)
   --
   -- Such an operation represents the action to be taken for parsing
   -- (handling) a COFF symbol record of the type indicated by the
   -- procedure name (COFF_Type_Name) and perhaps the name of the first
   -- parameter (Denotation), with Name being the associated string.
   -- The action may have an effect on the Action state and sometimes
   -- results in adding some symbols to the Bound-T Symbol_Table.


   procedure Source_File (
      Name         : in     String;
      Action       : in out Action_T;
      Symbol_Table : in     Symbols.Symbol_Table_T);
   --
   -- Acts on a ".file" special symbol. Together with the associated
   -- auxiliary Aux_File_Name symbol record, this special symbol marks the
   -- start of the symbols beloning to a new source file and supplies the
   -- name of the source file.
   --
   -- The default action (for the base type Action_T) is to first
   -- reset Action.Scope to the global scope, and then add a scope
   -- for Name to Action.Scope to make a one-level scope.


   procedure Function_Symbol (
      Fun          : in     Symbol_Denotation_T;
      Name         : in     String;
      Aux_Func     : in     Symbol_Ref;
      Begins       : in     Boolean;
      Aux_Begin    : in     Symbol_Ref;
      Action       : in out Action_T;
      Symbol_Table : in     Symbols.Symbol_Table_T);
   --
   -- Acts on a primary symbol record for a subprogram (function), that
   -- is a symbol denotation of the Subprogram kind.
   --
   -- Together with the associated auxiliary Aux_Function symbol record,
   -- it defines a subprogram and may also stand at the start of the symbol
   -- records for the parameters and locals of this subprogram.
   --
   -- Fun
   --    The subprogram entry address and other information.
   --    Precondition: Object_Kind (Fun) = Subprogram.
   -- Name
   --    The symbolic name of the subprogram.
   -- Aux_Func
   --    The Aux_Function symbol record if there is one, else null.
   -- Begins
   --    Whether there are symbol records for the parameters and/or locals
   --    of this subprogram (that is, whether there is a ".bf" symbol).
   -- Aux_Begin
   --    The Aux_BF symbol record if there is one, else null.
   --    Always null if Begins is False.
   --
   -- The default action (for the base type Action_T) is to connect the
   -- identifier part of the Name string to the associated code address as a
   -- subprogram symbol in the Symbol_Table in the current Action.Scope (if the
   -- component Action.Take_Subprograms is True). Finally, if the symbol
   -- binding is not global, the operation adds the identifier part of
   -- the string to Action.Scope as the new innermost level (replacing
   -- the popped level if any). Moreover, the source-code line-number
   -- in the Aux_Begin symbol, if any, is connected to the entry address
   -- of the subprogram.


   procedure End_Function (
      Fun          : in     Symbol_Denotation_T;
      Name         : in     String;
      Aux_End      : in     Symbol_Ref;
      Action       : in out Action_T;
      Symbol_Table : in     Symbols.Symbol_Table_T);
   --
   -- Acts to close the list of symbols for the parameters and local variables
   -- of a subprogram. The list was opened by an earlier call of Function_Symbol
   -- for this subprogram, with Begins => True.
   --
   -- Fun
   --    The subprogram entry address and other information.
   --    Precondition: Object_Kind (Fun) = Subprogram.
   -- Name
   --    The symbolic name of the subprogram.
   -- Aux_End
   --    The Aux_EF symbol record if there is one, else null.
   --
   -- The default action (for the base type Action_T) is to pop the
   -- innermost level from Action.Scope.


   procedure Begin_Block (
      Aux_Begin    : in     Symbol_Ref;
      Action       : in out Action_T;
      Symbol_Table : in     Symbols.Symbol_Table_T);
   --
   -- Acts on the beginning of a lexical block, nested within a subprogram and
   -- possibly within outer blocks of that subprogram.
   --
   -- Aux_Begin
   --    The Aux_BB symbol record if there is one, else null.
   --
   -- The default action (for the base type Action_T) is null. This means
   -- that blocks do not affect the scope of symbols; if the same symbol is
   -- defined in several blocks, the symbol will be ambiguous.


   procedure End_Block (
      Aux_End      : in     Symbol_Ref;
      Action       : in out Action_T;
      Symbol_Table : in     Symbols.Symbol_Table_T);
   --
   -- Acts on the ending of a lexical block, nested within a subprogram and
   -- possibly within outer blocks of that subprogram.
   --
   -- Aux_End
   --    The Aux_EB symbol record if there is one, else null.
   --
   -- The default action (for the base type Action_T) is null.


   procedure Label_Symbol (
      Object       : in     Symbol_Denotation_T;
      Name         : in     String;
      Action       : in out Action_T;
      Symbol_Table : in     Symbols.Symbol_Table_T);
   --
   -- Acts on a primary symbol record for a statement lable,
   -- that is a symbol-denotation of the Label kind.
   --
   -- Label objects have no auxiliary symbol records.
   --
   -- The default action (for the base type Action_T) is to connect
   -- the Name string to the code address defined by Object.Value.


   procedure Data_Symbol (
      Object       : in     Symbol_Denotation_T;
      Name         : in     String;
      Aux          : in     Symbol_Ref;
      Action       : in out Action_T;
      Symbol_Table : in     Symbols.Symbol_Table_T);
   --
   -- Acts on a primary symbol record for a data cell of some kind,
   -- that is a symbol-denotation of the Data kind.
   --
   -- If the object is an array, there may (and should) be an Aux_Array
   -- symbol record. Together, the primary and possible auxiliary symbol
   -- record define the type and location of the data object.
   --
   -- The default action (for the base type Action_T) is to check that
   -- the symbol maps to a data address. If so, the operation connects
   -- the Name string to the processor cell defined by Object_Cell;
   -- otherwise, the operation emits a Note and takes no other action.


   procedure Other_Symbol (
      Denotation   : in     Symbol_Denotation_T;
      Name         : in     String;
      Symbol       : in     Symbol_T;
      Aux          : in     Symbol_Ref;
      Action       : in out Action_T;
      Symbol_Table : in     Symbols.Symbol_Table_T);
   --
   -- Acts on any primary and normal (not special) COFF symbol record that is
   -- neither of the Subprogram kind nor of the Data kind. Thus, a symbol of
   -- the Other kind.
   --
   -- Precondition: Object_Kind (Denotation) = Other.
   --
   -- Denotation
   --    The denotation of the symbol.
   -- Name
   --    The name (identifier) of the symbol.
   -- Symbol
   --    The full COFF symbol record in case an overriding operation needs it,
   --    in addition to the Denotation.
   -- Aux
   --    The first auxiliary symbol owned by the primay Symbol, or null if it
   --    owns no auxiliary symbols.
   --
   -- The default action (for the base type Action_T) is null.


   procedure Special_Symbol (
      Nature       : in     Symbol_Nature_T;
      Symbol       : in     Symbol_T;
      Aux          : in     Symbol_Ref;
      Action       : in out Action_T;
      Symbol_Table : in     Symbols.Symbol_Table_T);
   --
   -- Acts on any primary, special COFF symbol record other than ".file",
   -- ".bb" or ".eb" and on the (first) associated auxiliary symbol record
   -- if any.
   --
   -- The default action (for the base type Action_T) is null.


   --
   --    Parsing procedure (class-wide in Action):
   --


   procedure Parse_Symbols (
      From         : in     File_Ref;
      Trace        : in     Boolean := False;
      Action       : in out Action_T'Class;
      Symbol_Table : in     Symbols.Symbol_Table_T);
   --
   -- Reads and parses a COFF symbol-table from a loaded COFF file.
   --
   -- From
   --    The loaded COFF file.
   -- Trace
   --    If true, the symbol records are displayed on standard output
   --    one by one as they are processed.
   -- Action
   --    Defines the actions to be taken for each symbol record.
   -- Symbol_Table
   --    The internal symbol-table to which (selected) COFF symbols
   --    are added.
   --
   -- The operation traverses the COFF symbol records in sequential order
   -- while maintaining Action.Scope, which is initially set to empty
   -- (global scope). For each COFF symbol record, the operation calls the
   -- primitive Action operation corresponding to the type of the
   -- record, which may update the current Scope and the Symbol_Table.
   -- However, for any symbol record with global binding, the operation
   -- first makes Action.Scope global, before calling the Action operation
   -- for this record.


private


   type Parser_T is null record;
   --
   -- The private internal state for parsing a COFF symbol table.


end Formats.COFF.Parsing;
