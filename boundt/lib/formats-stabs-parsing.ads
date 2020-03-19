-- Formats.Stabs.Parsing (decl)
--
-- Parsing the hierarchical and sequential structure of symbol
-- tables in the Stabs (or STABS) form.
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
-- $Revision: 1.6 $
-- $Date: 2015/10/24 20:53:54 $
--
-- $Log: formats-stabs-parsing.ads,v $
-- Revision 1.6  2015/10/24 20:53:54  niklas
-- Moved to free licence.
--
-- Revision 1.5  2009-12-07 11:06:06  niklas
-- BT-CH-0184: Arithmetic.Value_T replaces Processor.Value_T.
--
-- Revision 1.4  2007/08/18 20:29:10  niklas
-- Added procedure Nest_Scope to wrap Symbols.Nest.
-- Added procedures Begin_Block and End_Block for anonymous blocks.
-- Added Parser_T.Next_Block_Number to number anonymous blocks.
--
-- Revision 1.3  2007/04/26 11:27:52  niklas
-- BT-CH-0059.
--
-- Revision 1.2  2005/02/25 20:59:06  niklas
-- Changed all Symbol_Table_T parameters from "in out" to "in" as
-- befits a reference type. Related to BT-CH-0004.
--
-- Revision 1.1  2004/04/24 17:26:05  niklas
-- First version.
--


with Arithmetic_Base;
with Processor;
with Symbols;
with Twos_Complement_Mod;


package Formats.Stabs.Parsing is
--
-- A Stabs symbol-table is a sequence of records or "lines". There are
-- a great number of different kinds of Stabs lines; some lines define
-- program entities (symbols, line numbers) while other lines define
-- the context (module, source file) of the entities, sometimes in a
-- hierarchical way with separate, bracketing lines for opening a
-- context and closing a context. Thus, the sequence of Stabs lines
-- is an important feature of a Stabs table, and it is necessary to
-- "parse" the sequence to deduce the meaning of each line.
--
-- This package provides an operation to traverse a Stabs table,
-- interpret the Stabs lines and enter them in a Bound-T symbol-table
-- structure. The operation is parametrized by an "action" class so that
-- specific steps in the parsing process can be modified by creating
-- a subclass that overrides specific operations of the action class.
--
-- This package assumes the existence of a processor-specific type
-- called Processor.Code_Offset_T which represents an address offset.
-- This type is needed because the Value component of an N_Sline
-- record is (by default) interpreted as an offset to the starting
-- address of the enclosing source-file (N_So) or subprogram (N_Fun).


   package Twos_Complement_32 is
   new Twos_Complement_Mod (
      Unsigned_Type => Unsigned_32_T,
      Signed_Type   => N_Value_T,
      Abs_Type      => Arithmetic_Base.Value_T);
   --
   -- For handling Stabs N_Value components as either signed or
   -- unsigned.


   function To_Code_Address (Value : N_Value_T)
   return Processor.Code_Address_T;
   --
   -- The Stabs symbol-value interpreted as an unsigned code address.


   function To_Code_Offset (Value : N_Value_T)
   return Processor.Code_Offset_T;
   --
   -- The Stabs symbol-value interpreted as an unsigned code offset.


   type Parser_T is private;
   --
   -- Private internal state of an "action" object.


   type Action_T is tagged limited record
      Take_Subprograms   : Boolean := True;
      Take_Cells         : Boolean := True;
      Take_Source_Lines  : Boolean := True;
      Sline_Base_Defined : Boolean := False;
      Sline_Base         : Processor.Code_Address_T;
      Scope              : Symbols.Scope_Stack_T;
      Parser             : Parser_T;
   end record;
   --
   -- The "action" class that translates parsing steps to actions
   -- on the Bound-T context and symbol table.
   --
   -- Take_xxx
   --    These components specify whether to take into the resulting
   --    symbol-table the Stabs definitions for subprograms, data cells
   --    (variables, parameters) and source-line numbers, respectively.
   -- Sline_Base_Defined
   --    An Stabs line that defines a code base address for the following
   --    N_Sline lines has been parsed, and thus the Sline_Base address
   --    is defined.
   -- Sline_Base
   --    The code address that is the base address for the N_Sline
   --    Values, which are offsets relative to this base address.
   --    This is defined only if Sline_Base_Defined is True.
   -- Scope
   --    The current scope (context), updated during parsing.
   -- Parser
   --    Parsing state that may be maintained by the parsing
   --    process itself.


   procedure Nest_Scope (
      Level  : in     String;
      Action : in out Action_T'Class);
   --
   -- Nests a new Level on Action.Scope. Parsers should use this
   -- operation, instead of using Symbols.Nest directly, because
   -- there is some internal state in Action.Parser that needs to
   -- be initialized for the new level.


   type Line_Info_T is record
      N_Other : Octet_T;
      N_Desc  : N_Desc_T;
      N_Value : N_Value_T;
   end record;
   --
   -- The essential info of a Stabs line, with the type and
   -- string to be provided or assumed separately.


   --
   --    Cell mapping actions
   --
   -- One of the overridable parsing actions is the mapping of Stabs
   -- register/variable specifications to Bound-T processor-specific
   -- cell specs:


   No_Such_Cell : exception;
   --
   -- Raised by an Action_T operation if it cannot map the given
   -- Stabs register/variable specification to a cell-spec.


   function Register_Cell (
      Rsym   : Line_Info_T;
      Action : Action_T)
   return Processor.Cell_Spec_T;
   --
   -- The (processor-specific) cell specified by an N_Rsym line.
   -- This is usually some kind of processor register.
   -- Raises No_Such_Cell if the Rsym does not correspond to a
   -- (tracked) cell for the present target processor.
   --
   -- The default action (for the base type Action_T) is to
   -- raise No_Such_Cell.


   function Local_Cell (
      Lsym   : Line_Info_T;
      Action : Action_T)
   return Processor.Cell_Spec_T;
   --
   -- The (processor-specific) cell specified by an N_Lsym line.
   -- This is usually a cell in the local call frame, addressed
   -- relative to a frame pointer.
   -- Raises No_Such_Cell if the Lsym does not correspond to a
   -- (tracked) cell for the present target processor.
   --
   -- The default action (for the base type Action_T) is to
   -- raise No_Such_Cell.


   function Parameter_Cell (
      Psym   : Line_Info_T;
      Action : Action_T)
   return Processor.Cell_Spec_T;
   --
   -- The (processor-specific) cell specified by an N_Psym line.
   -- This is usually a cell in the local call frame, addressed
   -- relative to frame pointer, but often in the other direction
   -- compared to Local_Cell.
   -- Raises No_Such_Cell if the Psym does not correspond to a
   -- (tracked) cell for the present target processor.
   --
   -- The default action (for the base type Action_T) is to
   -- raise No_Such_Cell.


   --
   --    Parsing actions
   --
   -- Most of the primitive operations on an Action object have
   -- the following form:
   --
   --    procedure <Stabs_Type_Name> (
   --       <Line>       : in     Line_Info_T;
   --       Symbol       : in     String;
   --       Action       : in out Action_T;
   --       Symbol_Table : in     Symbols.Symbol_Table_T)
   --
   -- Such an operation represents the action to be taken for parsing
   -- (handling) a Stabs line of the type indicated by the procedure
   -- name (Stabs_Type_Name) and perhaps the name of the first
   -- parameter (Line), with Symbol being the associated string.
   -- The action may have an effect on the Action state and sometimes
   -- results in adding some symbols to the Bound-T Symbol_Table. Note
   -- that Symbol_Table_T is a reference type so the "in" mode allows
   -- updates to the underlying symbol-table object.


   procedure Source_File (
      So           : in     Line_Info_T;
      Name         : in     String;
      Action       : in out Action_T;
      Symbol_Table : in     Symbols.Symbol_Table_T);
   --
   -- Acts on an N_So line.
   --
   -- The default action (for the base type Action_T) is to first
   -- reset Action.Scope to the global scope. Then, if the string
   -- associated with the So line is not null (not empty), it is
   -- added to Action.Scope to make a one-level scope; moreover the
   -- associated machine (code) address is recorded internally
   -- (in the Action) as the base for any following N_Sline lines.


   procedure Source_File_L (
      Sol          : in     Line_Info_T;
      Name         : in     String;
      Action       : in out Action_T;
      Symbol_Table : in     Symbols.Symbol_Table_T);
   --
   -- Acts on an N_Sol line.
   --
   -- The default action (for the base type Action_T) is to set
   -- Action.Scope to a one-level scope consisting of the source-file
   -- name (the string associated with the So line).


   procedure Function_Symbol (
      Fun          : in     Line_Info_T;
      Symbol       : in     String;
      Action       : in out Action_T;
      Symbol_Table : in     Symbols.Symbol_Table_T);
   --
   -- Acts on an N_Fun line.
   --
   -- The default action (for the base type Action_T) is to pop the
   -- innermost level from Action.Scope, if Action.Scope has more than
   -- one level. The machine (code) address associated with the Fun
   -- line is also recorded internally (in the Action) as the base for
   -- any following N_Sline lines. Finally, if the string associated
   -- with Fun is not null, the operation connects the identifier part
   -- of the string to the associated code address as a subprogram symbol
   -- in the Symbol_Table in the current Action.Scope (if the
   -- component Action.Take_Subprograms is True) and adds the identifier
   -- part of the string to Action.Scope as the new innermost level
   -- (replacing the popped level).


   procedure Source_Line (
      Sline        : in     Line_Info_T;
      Symbol       : in     String;
      Action       : in out Action_T;
      Symbol_Table : in     Symbols.Symbol_Table_T);
   --
   -- Acts on an N_Sline line.
   --
   -- The default action (for the base type Action_T) is to connect
   -- the source-line number Sline.N_Desc to the code address that
   -- is computed by adding abs(Sline.N_Value) to the last recorded
   -- Sline base address, Action.Sline_Base, in Action.Scope.
   -- However, this is done only if Action.Take_Source_Lines is True.
   -- The Symbol parameter has no role in the default action.


   procedure Register_Symbol (
      Rsym         : in     Line_Info_T;
      Symbol       : in     String;
      Action       : in out Action_T;
      Symbol_Table : in     Symbols.Symbol_Table_T);
   --
   -- Acts on an N_Rsym line, using the Register_Cell function.
   --
   -- The default action (for the base type Action_T) is to
   -- connect the cell defined by Register_Cell (Rsym, Action)
   -- to the identifier part of the string assocated with the Rsym in
   -- Action.Scope. However, this is done only if Action.Take_Cells
   -- is True and if Register_Cell succeeds without raising No_Such_Cell.


   procedure Local_Symbol (
      Lsym         : in     Line_Info_T;
      Symbol       : in     String;
      Action       : in out Action_T;
      Symbol_Table : in     Symbols.Symbol_Table_T);
   --
   -- Acts on an N_Lsym line.
   --
   -- The default action (for the base type Action_T) is to
   -- connect the cell defined by Local_Cell (Lsym, Action)
   -- to the identifier part of the string assocated with the Lsym in
   -- Action.Scope. However, this is done only if Action.Take_Cells
   -- is True and if Local_Cell succeeds without raising No_Such_Cell.


   procedure Parameter_Symbol (
      Psym         : in     Line_Info_T;
      Symbol       : in     String;
      Action       : in out Action_T;
      Symbol_Table : in     Symbols.Symbol_Table_T);
   --
   -- Acts on an N_Psym line.
   --
   -- The default action (for the base type Action_T) is to
   -- connect the cell defined by Parameter_Cell (Psym, Action)
   -- to the identifier part of the string assocated with the Psym
   -- in Action.Scope. However, this is done only if Action.Take_Cells
   -- is True and if Parameter_Cell succeeds without raising No_Such_Cell.


   procedure Global_Symbol (
      Gsym         : in     Line_Info_T;
      Symbol       : in     String;
      Action       : in out Action_T;
      Symbol_Table : in     Symbols.Symbol_Table_T);
   --
   -- Acts on an N_Gsym line.
   --
   -- The default action (for the base type Action_T) is null.


   procedure Begin_Block (
      Lbrac        : in     Line_Info_T;
      Symbol       : in     String;
      Action       : in out Action_T;
      Symbol_Table : in     Symbols.Symbol_Table_T);
   --
   -- Acts on an N_Lbrac line. Such lines open anonymous block scopes.
   --
   -- The default action (for the base type Action_T) is to open
   -- a new scope level for this block on Action.Scope, using a
   -- scope name of the form "{n" where "n" is a sequential
   -- number 0, 1, ... Nested blocks thus get scopes of the form
   -- "{n|{m|...".
      

   procedure End_Block (
      Rbrac        : in     Line_Info_T;
      Symbol       : in     String;
      Action       : in out Action_T;
      Symbol_Table : in     Symbols.Symbol_Table_T);
   --
   -- Acts on an N_Rbrac line. Such lines close anonymous block scopes.
   --
   -- The default action (for the base type Action_T) is to close
   -- (remove) the top scope level from Action.Scope, after checking
   -- that the name of this level begins with "{". The rest of the
   -- name is not checked.


   procedure Other_Line (
      Line         : in     Line_T;
      Symbol       : in     String;
      Action       : in out Action_T;
      Symbol_Table : in     Symbols.Symbol_Table_T);
   --
   -- Acts on any other type of line.
   --
   -- The default action (for the base type Action_T) is null.


   procedure Parse (
      From         : in     IO.File_Type;
      Lines        : in     Segment_Desc_T;
      Strings      : in     Segment_Desc_T;
      Trace        : in     Boolean := False;
      Action       : in out Action_T'Class;
      Symbol_Table : in     Symbols.Symbol_Table_T);
   --
   -- Reads and parses a Stabs symbol-table from a program binary file.
   --
   -- From
   --    The file from which to load.
   -- Lines
   --    The location and length of the ".stab" segment.
   -- Strings
   --    The location and length of the ".stabstr" segment.
   -- Trace
   --    If true, the Stabs lines are displayed on standard output
   --    one by one as they are read and processed.
   -- Action
   --    Defines the actions to be taken for each Stabs line.
   -- Symbol_Table
   --    The internal symbol-table to which (selected) Stabs symbols
   --    are added.
   --
   -- The operation traverses the Stabs lines in sequential order
   -- while maintaining Action.Scope, which is initially set to empty
   -- (global scope). For each Stabs line, the operation calls the
   -- primitive Action operation corresponding to the type of the
   -- line, which may update the current Scope and the Symbol_Table.


private


   type Next_Block_Numbers_T is
      array (Natural range 0 .. 100)
      of Positive;
   --
   -- For each scope level, the number to be given to the
   -- next anonymous block on this level.
   -- Index 0 is for the global scope.
   -- An arbitrary upper bound is set on the number of levels.


   type Parser_T is record
      Next_Block_Number : Next_Block_Numbers_T := (others => 1);
   end record;
   --
   -- The private internal state for parsing an Stabs table.
   --
   -- Next_Block_Number
   --    The next sequential number for an unnamed block (N_Lbrac)
   --    at each active scope level, plus index 0 for the global level.
   --    Only the slice 0 .. Depth (Action.Scope) is defined.


end Formats.Stabs.Parsing;
