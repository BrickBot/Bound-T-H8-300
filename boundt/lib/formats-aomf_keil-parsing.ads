-- Formats.AOMF_Keil.Parsing (decl)
--
-- Parsing and loading AOMF files, including extensions defined by
-- Keil Elektronik GmbH.
-- 
-- Author: Niklas Holsti, Tidorum Ltd, 2004.
--
-- A component of the Bound-T Worst-Case Execution Time Tool.
--
-------------------------------------------------------------------------------
-- Copyright (c) 2004 .. 2015 Tidorum Ltd
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
-- $Revision: 1.6 $
-- $Date: 2015/10/24 20:53:51 $
--
-- $Log: formats-aomf_keil-parsing.ads,v $
-- Revision 1.6  2015/10/24 20:53:51  niklas
-- Moved to free licence.
--
-- Revision 1.5  2009-12-07 11:06:06  niklas
-- BT-CH-0184: Arithmetic.Value_T replaces Processor.Value_T.
--
-- Revision 1.4  2007/04/26 11:27:51  niklas
-- BT-CH-0059.
--
-- Revision 1.3  2006/04/28 09:15:59  niklas
-- Adapted to new Keil BL51 (V6.00) output format where the
-- Source_Name record seems to be missing. Added Parser components
-- to record the last source-file name, module name and procedure
-- name, for use in the source-file scope as available.
-- Updated to use mode "in" for Symbol_Table_T (reference semantics)
-- and to not erase the Symbol_Table in Parse_And_Load.
--
-- Revision 1.2  2005/01/02 22:37:33  niklas
-- Extended Define_Symbol to treat untyped or void Code symbols as
-- subprogram symbols.
--
-- Revision 1.1  2004/10/10 10:03:29  niklas
-- First version.
--


with Processor;
with String_Pool;
with Symbols;


package Formats.AOMF_Keil.Parsing is
--
-- An (Extended) AOMF file is a sequence of records of several types that
-- contain the binary machine-code of the target program and may also
-- describe the program in terms of source-level entities (modules, source
-- files, subprograms, data objects, types). The record sequence follows a
-- well-defined syntax which controls the context and sequence in which
-- different tags (different record types) can occur.
--
-- This package provides an operation to traverse the record sequence, using
-- this specific syntax, to load the code (and initial data) from the code
-- records into the simulated target memory, and to load the source-level
-- information into the Bound-T symbol-table. The operation is parametrized
-- by an "action" class so that specific steps in this traversal (parsing)
-- process can be modified be creating a subclass that overrides specific
-- operations of the action class. The overriding operations typically
-- map AOMF machine-level information to Bound-T representations as
-- required for a specific target processor.
--
-- Since AOMF files are used almost exclusively with the Intel-8051
-- (a.k.a. MCS-51) target processor, the operation for the root class
-- are fairly complete and there may be little need for overriding.


   type Parser_T is private;
   --
   -- Private internal state of an "action" object.


   function Source_File_Name (Parser : Parser_T) return String;
   --
   -- The source-file named in the last Source_Name record.
   -- If no Source_Name record has been parsed yet, a null string
   -- is returned.


   function Module_Name (Parser : Parser_T) return String;
   --
   -- The name of the current or last module that is being parsed or
   -- that was parsed.
   --
   -- This the name in the last Scope_Definition (Kind = Begin_Module).
   -- If no Scope_Definition (Kind = Begin_Module) has been parsed
   -- yet a null string is returned. Note that a Scope_Definition
   -- (Kind = End_Module) does not change the name.


   function Procedure_Name (Parser : Parser_T) return String;
   --
   -- The name of the current or last procedure that is being parsed
   -- or that was parsed.
   --
   -- This the name in the last Scope_Definition (Kind = Begin_Procedure).
   -- If no Scope_Definition (Kind = Begin_Procedure) has been parsed
   -- yet a null string is returned. Note that a Scope_Definition
   -- (Kind = End_Procedure) does not change the name.


   type Action_T is abstract tagged limited record
      Scope  : Symbols.Scope_Stack_T;
      Types  : Type_Table_T;
      Parser : Parser_T;
   end record;
   --
   -- The "action" class that connects the parsing process to the Bound-T
   -- context and symbol-table in a configurable way.
   --
   -- This root type is abstract because some of its operations cannot be
   -- given default definitions. For example, there is no default way to
   -- map a Word16_T offset in Code space to Processor.Code_Address_T.
   --
   -- Nevertheless, some components are concrete and even visible:
   --
   -- Scope
   --    The symbol-definition scope.
   --    This is meant to be defined and maintained by the general
   --    parsing routines, and only used (read) in the specialized,
   --    overridden operations.
   -- Types
   --    The table of types, as defined in the most recent preceding
   --    Type Definition AOMF record.
   -- Parser
   --    Private parsing state.


   --
   --    Parsing actions (overridable)
   --


   procedure Load_Memory (
      Seg_ID  : in     Octet_T;
      Offset  : in     Word16_T;
      Data    : in     Data_T;
      Action  : in out Action_T)
   is abstract;
   --
   -- Loads absolute octets into the program's memory image.
   -- Data'Length octets are loaded into the segment defined by
   -- Seg_ID starting from the given Offset address.


   Not_Located : exception;
   --
   -- Raised (propagated) in the To_Cell_Spec functions when the
   -- variable in question is not modelled as a processor cell.


   function To_Cell_Spec (
      Seg_ID   : Octet_T;
      Storage  : Storage_T;
      Index    : Type_Index_T;
      Defining : Compound_Type_T;
      Offset   : Word16_T;
      Action   : Action_T)
   return Processor.Cell_Spec_T
   is abstract;
   --
   -- The processor-specific location (in processor cells) of the
   -- variable with the given type Index and the underlying Defining
   -- type, when the variable is stored in the given Storage at the
   -- given Offset.
   --
   -- If the location is not modelled as a cell, the function shall
   -- propagate the Not_Located exception.


   procedure Define_Variable (
      Symbol : in     String;
      Cell   : in     Processor.Cell_Spec_T;
      Action : in out Action_T;
      Into   : in     Symbols.Symbol_Table_T);
   --
   -- Enters a variable symbol and its location (cell) in the
   -- symbol-table.
   --
   -- The default operation is to Connect the symbol as a variable
   -- in Action.Scope with the given Cell location.


   function To_Code_Address (
      Seg_ID  : Octet_T;
      Address : Word16_T;
      Action  : Action_T)
   return Processor.Code_Address_T
   is abstract;
   --
   -- Converts a numerical code-memory address to the internal form for the
   -- current target Processor, perhaps depending on the Segment ID in
   -- which the Address lies.


   procedure Define_Subprogram (
      Symbol  : in     String;
      Address : in     Processor.Code_Address_T;
      Action  : in out Action_T;
      Into    : in     Symbols.Symbol_Table_T);
   --
   -- Enters a subprogram symbol and its entry address in the symbol-
   -- table.
   --
   -- The default operation is to Connect the symbol as a subprogram
   -- in Action.Scope with the given entry Address.


   procedure Define_Symbol (
      Symbol    : in     String;
      Seg_ID    : in     Octet_T;
      Storage   : in     Storage_T;
      Index     : in     Type_Index_T;
      Defining  : in     Compound_Type_T;
      Offset    : in     Word16_T;
      Action    : in out Action_T;
      Into      : in     Symbols.Symbol_Table_T);
   --
   -- Enters a symbol and its storage location in the symbol-table
   -- as a subprogram, label or cell, depending on the type of
   -- symbol, and under the current symbol-context.
   --
   -- The default operation is to:
   --
   --   > For a variable Symbol, call Define_Variable using the
   --     To_Cell_Spec function.
   --
   --   > For a subprogram Symbol, call Define_Subprogram using the
   --     To_Code_Address function.
   --
   --   > For a code-label Symbol, call Symbols.Connect_Label using
   --     the To_Code_Address function.
   --
   --   > For an untyped or void Symbol, located in Code space, call
   --     Define_Subprogram as above.
   -- 
   --   > do nothing TBM for other symbols.


   --
   --    Top-level parsing operations (class-wide)
   --


   Data_Error : exception;
   --
   -- Unrecoverable error while parsing and loading an AOMF file.
   -- This is usually a syntax or semantic error in the file.


   procedure Parse_And_Load (
      File         : in     IO.File_Type;
      Action       : in out Action_T'Class;
      Symbol_Table : in     Symbols.Symbol_Table_T);
   --
   -- Reads and parses the (open) AOMF file identified, using the given
   -- Actions object to translate and load the contents into a Bound-T
   -- representation. The memory image (code and constant / initialized data)
   -- is loaded into some memory simulation defined by the Action. The symbols
   -- are added to the given Symbol_Table, but using operations defined
   -- by the Action.
   --
   -- Raises Data_Error upon any (unrecoverable) problem, after reporting the
   -- error (using Output).


private


   No_Name : constant String_Pool.Item_T := String_Pool.Null_Item;
   --
   -- Represents the absence of a source-file, module or procedure name.


   type Parser_T is record
      Source_Name    : String_Pool.Item_T := No_Name;
      Module_Name    : String_Pool.Item_T := No_Name;
      Procedure_Name : String_Pool.Item_T := No_Name;
   end record;
   --
   -- The private internal state for parsing an AOMF file.


end Formats.AOMF_Keil.Parsing;
