-- Formats.SDCC.CDB (decl)
--
-- The "CDB" file format for the Small Device C Compiler (SDCC).
--
-- Author: Niklas Holsti, Tidorum Ltd, 2007.
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
-- $Revision: 1.4 $
-- $Date: 2015/10/24 20:53:54 $
--
-- $Log: formats-sdcc-cdb.ads,v $
-- Revision 1.4  2015/10/24 20:53:54  niklas
-- Moved to free licence.
--
-- Revision 1.3  2007-10-07 12:54:07  niklas
-- Extended Memory_Symbol to assume that symbols for which no SDCC.Symbol
-- is found represent unsigned octet variables in external RAM.
--
-- Revision 1.2  2007/09/23 20:55:58  niklas
-- Improved the default Register_Symbol operation to ignore symbols
-- that map to a null register list.
--
-- Revision 1.1  2007/09/22 14:19:25  niklas
-- First version.
--


with Processor;
with Symbols;


package Formats.SDCC.CDB is


   -- The references for this package are:
   --
   -- 1. "CDB File Format"
   --    Lenny Story, 2003-03-21, SDCC Development Team.
   --
   -- A CDB file is a text file with one line per record.
   -- There are several kinds of records with some similarities
   -- in form. The order of records is generally not significant.
   -- When records refer to shared information they do so by name,
   -- for example by type-name. We do assume that all such references
   -- are satisfied (defined) in earlier records. Thus, all linker
   -- memory-address records (L:L and L:G records) must follow the
   -- corresponding symbol records (S, T, and F records).
   --
   -- The records can:
   -- > Name the module.
   -- > Define a function (subprogram).
   -- > Define a symbol (for a subprogram, variable, etc.)
   -- > Define a structured type.
   -- > Report memory layout information from the linker.
   --
   -- The linker information can:
   -- > Define the address of a symbol.
   -- > Connect source-code lines to code addresses.
   --
   -- This package provides an operation to traverse a CDB file
   -- and enter the information in a Bound-T symbol-table.
   -- The operation is parametrized by an "action" class so that
   -- specific steps in the parsing process can be modified by
   -- creating a subclass that overrides specific operations of
   -- the action class.


   type Parser_T is private;
   --
   -- Private internal state of an "action" object.


   type Action_T is abstract tagged limited record
      Take_Subprograms : Boolean    := True;
      Take_Cells       : Boolean    := True;
      Take_Lines       : Boolean    := True;
      Scope            : Symbols.Scope_Stack_T;
      Symbols          : Symbol_Table_T;
      Parser           : Parser_T;
   end record;
   --
   -- The "action" class that translates parsing steps to actions
   -- on the Bound-T context and symbol table.
   --
   -- Take_xxx
   --    These components specify whether to take into the resulting
   --    symbol-table the CDB definitions for subprograms, data
   --    cells (variables, parameters), and source-lines, respectively.
   -- Scope
   --    The symbol-definition scope.
   -- Symbols
   --    The CDB Compiler Symbol Records so far parsed.
   -- Parser
   --    Parsing state that may be maintained by the parsing
   --    process itself.
   --
   -- Note that each CDB line contains the necessary scope information
   -- in itself. The Scope component is mainly a working area in which
   -- to build the scope that the CDB line defines; no scope is retained
   -- from one CDB line to the next.


   function To_Subprogram_Address (
      Address : String;
      Action  : Action_T)
   return Processor.Code_Address_T
   is abstract;
   --
   -- The entry address of the subprogram that has the given Address
   -- as given on a CDB Linker record.


   Not_A_Cell : exception;
   --
   -- Raised (propagated) in the To_Cell_Spec functions when the
   -- variable in question is not modelled as a processor cell.


   function To_Cell_Spec (
      Typ      : Type_Def_T;
      Register : String;
      Action   : Action_T)
   return Processor.Cell_Spec_T
   is abstract;
   --
   -- The processor-specific location (cell) of the variable of the
   -- given Type when stored in the given Register (just one register).
   --
   -- Propagates Not_A_Cell if this variable is not modelled as a cell.


   function To_Cell_Spec (
      Typ       : Type_Def_T;
      Registers : Reg_List_T;
      Action    : Action_T)
   return Processor.Cell_Spec_T
   is abstract;
   --
   -- The processor-specific location (cell) of the variable of the
   -- given Type when stored in the given Registers (more than one).
   --
   -- Propagates Not_A_Cell if this variable is not modelled as a cell.


   function To_Cell_Spec (
      Typ     : Type_Def_T;
      Space   : Stack_Space_T;
      Offset  : Frame_Offset_T;
      Action  : Action_T)
   return Processor.Cell_Spec_T
   is abstract;
   --
   -- The processor-specific location (cell) of the variable of the
   -- given Type when stored in the given stack Space at the given
   -- Offset with respect to the frame pointer.
   --
   -- Propagates Not_A_Cell if this variable is not modelled as a cell.


   function To_Cell_Spec (
      Typ     : Type_Def_T;
      Space   : Memory_Space_T;
      Address : String;
      Action  : Action_T)
   return Processor.Cell_Spec_T
   is abstract;
   --
   -- The processor-specific location (cell) of the variable of the
   -- given Type when stored in the given memory Space at the given
   -- Address.
   --
   -- Propagates Not_A_Cell if this variable is not modelled as a cell.


   procedure Build_Symbol_Scope (
      Scope  : in     Scope_T;
      Action : in out Action_T);
   --
   -- Makes Action.Scope represent the given CDB Scope.
   --
   -- The default operation maps scopes as follows:
   --
   -- CDB Scope             Symbols Scope (Action.Scope)
   -- ---------------       ----------------------------
   -- Global                No levels    : Global
   -- File                  One level    : File-name
   -- Local                 Three levels : Func-name | level# | block#


   procedure Register_Symbol (
      Name         : in     String;
      Scope        : in     Scope_T;
      Typ          : in     Type_Def_T;
      Registers    : in     Reg_List_T;
      Action       : in out Action_T;
      Symbol_Table : in     Symbols.Symbol_Table_T);
   --
   -- Enters a symbol that is located in some Registers into the
   -- symbol-table, in response to a Compiler Symbol record that defines
   -- the Name, Scope, Type, and Registers of the symbol.
   --
   -- The default operation uses the To_Cell function (with redispatch)
   -- to translate the Type and Register(s) to a Cell_Spec as follows:
   --
   -- > If the register list is empty, the symbol was optimized away
   --   and the operation does nothing (except a Note).
   --
   -- > If the register list contains one register, the operation uses
   --   the To_Cell function with one register parameter.
   --
   -- > If the register list contains two or more registers, the operation
   --   uses the To_Cell function with a register-list parameter.
   --
   -- If the register list is not empty and the chosen To_Cell function
   -- returns a cell-spec, the default operation enters the result in
   -- the Symbol_Table.


   procedure Stack_Symbol (
      Name         : in     String;
      Scope        : in     Scope_T;
      Typ          : in     Type_Def_T;
      Space        : in     Stack_Space_T;
      Offset       : in     Frame_Offset_T;
      Action       : in out Action_T;
      Symbol_Table : in     Symbols.Symbol_Table_T);
   --
   -- Enters a symbol that is located in a stack Space, at a given Offset,
   -- into the symbol-table, in response to a Compiler Symbol record that
   -- specifies the Name, Scope, Type, Space, and Offset of the symbol.
   --
   -- The default operation uses the To_Cell function (with redispatch) to
   -- translate the Type, Space, and Offset to a Cell_Spec and enters the
   -- result in the Symbol_Table.


   procedure Memory_Symbol (
      Name         : in     String;
      Scope        : in     Scope_T;
      Address      : in     String;
      Action       : in out Action_T;
      Symbol_Table : in     Symbols.Symbol_Table_T);
   --
   -- Enters a symbol and its memory location in the symbol-table, in
   -- response to a Linker location record that specifies the Name,
   -- Scope, and Address of the symbol.
   --
   -- The default operation looks up the Name and Scope in Action.Symbols
   -- to find the Compiler Symbol record for this symbol; this record
   -- defines the Type and Location of the symbol. The Location is expected
   -- to be in a Memory Space. The default operation then uses the relevant
   -- To_Cell_Spec function (with redispatch) to translate the Type, Space
   -- and Address to a Cell_Spec and enters the result in the Symbol_Table.
   --
   -- If Action.Symbols has no symbol for this Name and Scope, the symbol
   -- is assumed to represent a variable of unsigned octet type, located
   -- in External RAM.


   procedure Assembler_Line (
      File         : in     String;
      Line         : in     Symbols.Line_Number_T;
      Address      : in     Processor.Code_Address_T;
      Action       : in out Action_T;
      Symbol_Table : in     Symbols.Symbol_Table_T);
   --
   -- Enters a source-line/address connection from an assembly-language
   -- file into the symbol-table.
   --
   -- The default operation enters the Line/Address connection in the
   -- scope of the File name.


   procedure C_Line (
      File         : in     String;
      Line         : in     Symbols.Line_Number_T;
      Level        : in     Natural;
      Block        : in     Natural;
      Address      : in     Processor.Code_Address_T;
      Action       : in out Action_T;
      Symbol_Table : in     Symbols.Symbol_Table_T);
   --
   -- Enters a source-line/address connection from 'C' source-code
   -- file into the symbol-table.
   --
   -- The default operation enters the Line/Address connection in the
   -- scope of the File name.


   procedure Parse (
      File_Name    : in     String;
      Trace        : in     Boolean := False;
      Action       : in out Action_T'Class;
      Symbol_Table : in     Symbols.Symbol_Table_T;
      Found        :    out Boolean);
   --
   -- Reads and parses the CDB file with the given File_Name.
   --
   -- File_Name
   --    The name of the CDB file. Here opened, read, and closed.
   -- Trace
   --    If true, the symbol records are displayed on standard output
   --    one by one as they are processed.
   -- Action
   --    Defines the actions to be taken for each symbol record.
   -- Symbol_Table
   --    The internal symbol-table to which (selected) ELF symbols
   --    are added.
   -- Found
   --    Whether a valid, useful CDB file was found at File_Name.
   --
   -- The operation traverses the CDB records in sequential order.
   -- For each CDB record, the operation calls the primitive Action
   -- operation corresponding to the type of the record, which may
   -- update the Symbol_Table. The Scope for each CDB record is
   -- constructed from the record itself; there is no scope information
   -- in the order of records.


private


   type Parser_T is null record;
   --
   -- The private internal state for parsing a CDB file.


end Formats.SDCC.CDB;
