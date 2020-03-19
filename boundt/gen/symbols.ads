-- Symbols (decl)
--
-- Symbolic information associated with a target program.
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
-- $Revision: 1.27 $
-- $Date: 2015/10/24 19:36:53 $
--
-- $Log: symbols.ads,v $
-- Revision 1.27  2015/10/24 19:36:53  niklas
-- Moved to free licence.
--
-- Revision 1.26  2009-03-24 07:48:36  niklas
-- BT-CH-0166: String_Pool.Item_T for source-file and marker names.
--
-- Revision 1.25  2009/03/20 18:19:30  niklas
-- BT-CH-0164: Assertion context identified by source-line number.
--
-- Revision 1.24  2008/10/11 08:16:15  niklas
-- BT-CH-0148: Symbols from text files and the -symbols option.
--
-- Revision 1.23  2007/08/14 12:36:38  niklas
-- BT-CH-0072: Corrections to handling ambiguous names.
--
-- Revision 1.22  2007/05/03 07:26:27  niklas
-- Added two variants of Connect_Subprogram using Scope_T.
--
-- Revision 1.21  2007/04/26 11:28:05  niklas
-- BT-CH-0059.
--
-- Revision 1.20  2007/02/24 09:51:53  niklas
-- BT-CH-0046.
--
-- Revision 1.19  2005/10/09 08:10:24  niklas
-- BT-CH-0013.
--
-- Revision 1.18  2005/02/20 15:15:37  niklas
-- BT-CH-0004.
--
-- Revision 1.17  2004/04/25 13:37:54  niklas
-- First Tidorum version.
-- Replaced "Cell" connections by "Variable" connections that use
-- Storage.Location_T and thus allow the mapping from variable to cell
-- to depend on code address.
-- Added operations to find the source lines that surround a given code
-- address (approximate source-line identification).
--
-- Revision 1.16  2001/06/09 16:32:01  holsti
-- Added Connect_Subprogram variant with Giving parameter.
--
-- Revision 1.15  2001/04/10 12:58:32  ville
-- Added subroutines Connect_Label and Label_Connections
--
-- Revision 1.14  2001/03/21 20:30:36  holsti
-- Name_Of scope-level added. Symbols.Lines omitted.
--
-- Revision 1.13  2001/03/02 09:01:18  holsti
-- Default scope delimiter changed to vertical bar.
--
-- Revision 1.12  2000/12/28 19:45:21  holsti
-- Connections_For_Line removed (not used and not implemented).
-- Operation descriptions updated.
--
-- Revision 1.11  2000/12/22 13:37:30  sihvo
-- Added line numbers.
--
-- Revision 1.10  2000/12/04 08:09:28  sihvo
-- Changed Hash to Bags. Added variables to symbol table.
--
-- Revision 1.9  2000/11/22 22:30:24  holsti
-- Using Processor.Code_Address_T instead of Processor.Address_T.
-- Comments updated re Connection_T attributes for various symbols.
--
-- Revision 1.8  2000/11/06 12:37:17  langback
-- Replaced Connect with separate routines Connect_Subprogram and
-- Connect_Cell. No routine for Label's yet.
--
-- Revision 1.7  2000/10/25 14:21:39  langback
-- Added spec for function "Variable_Cell".
--
-- Revision 1.6  2000/07/13 11:24:18  saarinen
-- Added hash-table for Code_Addresses and implemented
-- Connections_For_Address.
--
-- Revision 1.5  2000/07/04 11:23:08  saarinen
-- Moved functions Name_Of and Scope_Of from Programs to here.
--
-- Revision 1.4  2000/05/05 12:11:23  holsti
-- Symbols.Opt controls tracing output.
--
-- Revision 1.3  2000/05/02 19:41:30  holsti
-- String_Pool added, Symbols partly implemented.
--
-- Revision 1.2  2000/04/24 14:28:40  holsti
-- Symbol scopes added.
--


with Processor;
with Storage;
with String_Pool;
with Unbounded_Vectors;


package Symbols is
--
-- Symbolic information associated with a target program.
--
-- This information allows one to map symbolic identifiers, such
-- as subprogram names, variable names or source-code line numbers,
-- to machine-level information, mainly memory addresses or
-- cell specifiers.
--
-- These symbol tables are loaded from the target program executable
-- file, and remain constant throughout the analysis.
--


   --
   ---   Scopes
   --


   type Scope_T is private;
   --
   -- The scopes form a nested hierarchy, usually starting from
   -- the module at the top level, then subprogram, then block.
   -- Each level is identified by a name, and the "completely
   -- qualified" name of a symbol is the concatenation of the
   -- scope level names with the symbol name, for example
   -- "Init_Module|Help_Procedure|Counter_Variable".
   --
   -- The default initial value of any Scope_T variable is the
   -- special value No_Scope which represents an undefined scope.
   --
   -- If the scope is related to a line number connection, it
   -- is assumed that the first level of the scope is the name of 
   -- the source file and the second level (if there is one) is
   -- the name of the subprogram. The meaning of the third or
   -- higher scope levels is not defined.
   --
   -- A Scope_T value other than No_Scope represents a fully
   -- defined, fixed scope. It is not possible to add levels nor
   -- to remove levels. There is another type, Scope_Stack_T, that
   -- is dynamic and does allow adding and removing levels.
   --
   -- Scope_T has reference semantics but this hardly matters since
   -- it is immutable (no mutation operations provided).


   No_Scope : constant Scope_T;
   --
   -- An undefined scope. The default initial value of all Scope_T
   -- variables.


   Global_Scope : constant Scope_T;
   --
   -- The global scope, with no scope levels.


   function Depth (Scope : Scope_T) return Natural;
   --
   -- The nesting depth of the scope.
   -- Zero for the global scope.


   function Name_Of (
      Level  : Positive;
      Within : Scope_T)
   return String;
   --
   -- The name of the chosen level within the given scope.
   -- The Level must be in 1 .. Depth, else a Fault is reported
   -- and the null string "" is returned.


   function Deepest (Scope : Scope_T) return String;
   --
   -- The name of the deepest level nested in the scope.
   -- Raises Constraint_Error if the scope is null (global).


   Default_Delimiter: constant Character := '|';
   --
   -- The default character used to separate levels in a
   -- nested scope.


   function Image (
      Scope     : Scope_T;
      Delimiter : Character := Default_Delimiter)
   return String;
   --
   -- The concatenated names of the levels of the scope, in order
   -- from outermost to innermost, separated with the delimiter
   -- character.


   function Scope (Identifier : String) return Scope_T;
   --
   -- The scope that consists of one level, the given Identifier.


   function Scope_Of (
      Identifier : String;
      Delimiter  : Character := Default_Delimiter)
   return Scope_T;
   --
   -- Parses the Identifier as a sequence of names separated
   -- by delimiters. Returns a scope that consists of all but
   -- the last name, listed from top to bottom order.


   function Name_Of (
      Identifier : String;
      Delimiter  : Character := Default_Delimiter)
   return String;
   --
   -- Parses the Identifier as a sequence of names separated
   -- by delimiters. Returns the last name, or the full Identifier
   -- if there are no delimiters.


   --
   ---   Scope stacks
   --


   type Scope_Stack_T is limited private;
   --
   -- A scope that is dynamic; new levels can be added and existing
   -- levels can be removed in a stack fashion (last in - first out).
   -- There is no fixed upper bound on the number of levels that can
   -- be added.
   --
   -- The default initial value of any Scope_Stack_T variable is the
   -- global scope, with no scope levels.


   procedure Make_Global (Scope : in out Scope_Stack_T);
   --
   -- Makes the scope null = global.


   function Depth (Scope : Scope_Stack_T) return Natural;
   --
   -- The current nesting depth of the scope.


   procedure Nest (
      Level : in     String;
      Scope : in out Scope_Stack_T);
   --
   -- Adds an inner scope level to the scope, making it more
   -- deeply nested.


   procedure Unnest (Scope : in out Scope_Stack_T);
   --
   -- Removes the innermost (most recently Nested) level from the
   -- scope.
   -- Raises Constraint_Error if the Scope is already null (= global).


   procedure Unnest (
      Level : in     String;
      Scope : in out Scope_Stack_T);
   --
   -- Just as the Nest without a Level parameter, but this one also
   -- checks that the name of the unnested (deepest) level is "Level".


   -- function Name_Of (
   --   Level  : Positive;
   --   Within : Scope_Stack_T)
   -- return String;
   --
   -- The name of the chosen level within the given scope.
   -- The Level must be in 1 .. Depth, else Constraint Error results.


   function Deepest (Scope : Scope_Stack_T) return String;
   --
   -- The name of the deepest level nested in the scope.
   -- Raises Constraint_Error if the scope is null (global).


   function Image (
      Scope     : Scope_Stack_T;
      Delimiter : Character := Default_Delimiter)
   return String;
   --
   -- The concatenated names of the levels of the scope, in order
   -- from outermost to innermost, separated with the delimiter
   -- character.


   function To_Scope (Stack : Scope_Stack_T) return Scope_T;
   --
   -- The (fixed, fully defined) scope that is the current value
   -- of the given scope Stack.


   procedure Discard (Item : in out Scope_Stack_T);
   --
   -- Deallocates the storage for this scope-stack, returning
   -- it to the default initial value (global scope).


   --
   ---   Connections of symbols to addresses etc.
   --


   type Connection_T is private;

   -- Represents the connections between a symbolic identifier
   -- and a machine-level entity (usually an address).
   -- Four kinds of connection exist:
   --
   --  > Subprogram symbol connection.
   --  > Variable symbol connection.
   --  > Label symbol connection.
   --  > Line-number symbol connection.
   --
   -- Attributes common to all kinds of connection are the symbolic
   -- identifier (string or number), and a "scope" that separates,
   -- for example, global variables and local variables of the same
   -- name.
   --
   -- The machine-level item connected to an identifier depends
   -- on the type of identifier:
   --
   -- > A code address, for a subprogram, label or line-number.
   --   Additional attributes may be connected to the subprogram.
   --
   -- > A data location (cell specifier or cell access path TBA), for
   --   a variable or other kind of data object.
   --   The location may be valid only for a certain range or set of
   --   code addresses (execution points) and there may be different
   --   locations for different code addresses.


   type Connection_Kind_T is (
      Subprogram,
      Label,
      Variable,
      Line_Number);
   --
   -- The kinds of connections that can exist.


   Cell : constant Connection_Kind_T := Variable;
   --
   -- Old and deprecated name for a Variable connection.


   type Connection_Set_T is array (Positive range <>) of Connection_T;

   -- Zero or more elementary connections, usually all for
   -- the same symbolic identifier and all of the same kind.


   type Source_File_Name_T is new String_Pool.Item_T;
   --
   -- The name of a source-code file, as defined in the connections
   -- of source-code lines to machine-code addresses.


   Null_Name : constant Source_File_Name_T :=
      Source_File_Name_T (String_Pool.Null_Item);
   --
   -- Represents the absence of a source-file name.


   function To_Source_File_Name (Name : String) return Source_File_Name_T;
   --
   -- The given Name as a Source_File_Name_T, or Null_Name if the
   -- given Name is a null string ("").


   function Image (Name : Source_File_Name_T) return String;
   --
   -- The string that the Name represents, or "" for Null_Name.


   type Line_Number_T is new Natural;
   --
   -- Type for line numbers. 


   function Image (Item : Connection_T) return String;
   --
   -- Describes the connection for human understanding.
   -- Represents three fields of standard output.


   function Kind_Of (Connection : Connection_T) return Connection_Kind_T;
   --
   -- The kind of item to which the connection applies.


   function Scope_Of (Connection : Connection_T) return Scope_T;
   --
   -- The (full) scope of a connection.


   function Name_Of (Connection : Connection_T) return String;
   --
   -- The name (identifier) of a connection.
   --
   -- Deprecated usage: If the connection represents a line number,
   -- returns the line number as string. Use instead the Line_Number_Of
   -- functions, below.


   function Scoped_Name_Of (
      Connection : Connection_T;
      Delimiter  : Character := Default_Delimiter)
   return String;
   --
   -- The full, scoped named of the Connection, defined as the Image
   -- of the Scope_Of the connection followed by the Name_Of the
   -- connection, separated by the given Delimiter.


   function Source_File_Of (Connection : Connection_T)
   return Source_File_Name_T;
   --
   -- The source-file name of a connection that represents a
   -- line number.


   function Source_File_Of (Connection : Connection_T)
   return String;
   --
   -- The source-file name of a connection that represents a
   -- line number. Same as To_String (Source_File_Of (Connection)),
   -- using the preceding definition of Source_File_Of.


   function Line_Number_Of (Connection : Connection_T) 
   return Line_Number_T;
   --
   -- The line number of a connection which represents a line number.


   function Line_Number_Of (Connection : Connection_T)
   return String;
   --
   -- The line number, formatted as a string, of a connection which
   -- represents a line number.


   function Address_Of (Connection : Connection_T)
   return Processor.Code_Address_T;
   --
   -- The address of a connection which represents a subprogram,
   -- a label or a line number.


   type Code_Address_List_T is
      array (Positive range <>) of Processor.Code_Address_T;
   --
   -- A list (set) of code addresses.


   function No_Code_Addresses return Code_Address_List_T;
   --
   -- A null list of code addresses.


   function Addresses_Of (Connections : Connection_Set_T)
   return Code_Address_List_T;
   --
   -- The addresses of all the Connections (subprograms, labels or
   -- line numbers), with duplicate addresses removed.


   function Location_Of (Connection : Connection_T)
   return Processor.Cell_Spec_T;
   --
   -- The location of a connection which represents a variable,
   -- assuming that this (1) connection does not depend on the point
   -- of execution of the target program, and (2) the location is a
   -- simple cell-spec rather than TBA a cell access path.


   function Spec_Of (Connection : Connection_T)
   return Processor.Cell_Spec_T
   renames Location_Of;
   --
   -- Obsolete and deprecated synonym.


   function Location_Of (Connection : Connection_T)
   return Storage.Location_Ref;
   --
   -- The location(s) of a connection which represents a variable.


   --
   ---   Symbol tables
   --


   type Symbol_Table_T is private;

   -- An instance of the symbolic information, associated with
   -- a particular target executable file.
   --
   -- Several (at least two) instances are needed if the tool
   -- will use an unoptimised executable to help analyse an
   -- optimised executable of the same program.
   --
   -- A Symbol Table is included in the "access handles to the
   -- target program", used in the analysis algorithms.
   --
   -- Reference semantics apply to this type. Thus, a value of type
   -- Symbol_Table_T should be understood as a reference to an underlying
   -- symbol-table object. An update to this object via any reference to
   -- it is equally visible through any other reference to the same object.
   -- Parameters of type Symbol_Table_T are declared with mode "in"
   -- even when the underlying symbol-table object will be updated by
   -- the operation.
   --
   -- Each variable of this type must be initialised by the
   -- Erase procedure before use (else Constraint_Error may be
   -- raised).


   No_Symbol_Table : constant Symbol_Table_T;
   --
   -- A special value to show the absence of a symbol-table.
   -- This is also the default initial value of any variable
   -- object of type Symbol_Table_T.


   procedure Erase (Table : in out Symbol_Table_T);
   --
   -- Erases all the entries from the table, making is empty.
   -- This procedure must also be used to initialise every
   -- symbol-table object before it is used for the first time.


   procedure Connect_Subprogram (
      Scope   : in Scope_Stack_T;
      Name    : in String;
      Address : in Processor.Code_Address_T;
      Within  : in Symbol_Table_T);
   --
   -- Enters the name of a subprogram in the symbol table, given 
   -- the (full) scope and name of the subprogram, and the
   -- subprogram's entry address.


   procedure Connect_Subprogram (
      Scope   : in Scope_T;
      Name    : in String;
      Address : in Processor.Code_Address_T;
      Within  : in Symbol_Table_T);
   --
   -- Same as above, but with a Scope_T instead of a Scope_Stack_T.


   procedure Connect_Subprogram (
      Scope   : in     Scope_Stack_T;
      Name    : in     String;
      Address : in     Processor.Code_Address_T;
      Within  : in     Symbol_Table_T;
      Giving  :    out Connection_T);
   --
   -- As Connect_Subprogram above, but also returns the new
   -- connection that was created.


   procedure Connect_Subprogram (
      Scope   : in     Scope_T;
      Name    : in     String;
      Address : in     Processor.Code_Address_T;
      Within  : in     Symbol_Table_T;
      Giving  :    out Connection_T);
   --
   -- Same as above, but with a Scope_T instead of a Scope_Stack_T.


   procedure Connect_Label (
      Scope   : in Scope_Stack_T;
      Name    : in String;
      Address : in Processor.Code_Address_T;
      Within  : in Symbol_Table_T);
   --
   -- Enters the name of a label in the symbol table, given 
   -- the (full) scope and name of the label, and the
   -- label's entry address.


   procedure Connect_Line (
      Scope   : in Scope_Stack_T;
      Number  : in Line_Number_T;
      Address : in Processor.Code_Address_T;
      Within  : in Symbol_Table_T);
   --
   -- Enters source file line number in the symbol table,
   -- given the line number, associated machine address
   -- and scope (name of source-file and subprogram).


   procedure Connect_Line (
      Scope   : in Scope_T;
      Number  : in Line_Number_T;
      Address : in Processor.Code_Address_T;
      Within  : in Symbol_Table_T);
   --
   -- Same as above, but with a Scope_T instead of a Scope_Stack_T.


   procedure Connect_Variable (
      Scope    : in Scope_Stack_T;
      Name     : in String;
      Location : in Processor.Cell_Spec_T;
      Within   : in Symbol_Table_T);
   --
   -- Enters a variable in the symbol table, given the (full) scope
   -- and name of the variable and the associated location map,
   -- which is here an address-independent cell-spec.


   procedure Connect_Variable (
      Scope    : in Scope_T;
      Name     : in String;
      Location : in Processor.Cell_Spec_T;
      Within   : in Symbol_Table_T);
   --
   -- Same as above, but with a Scope_T instead of a Scope_Stack_T.


   procedure Connect_Cell (
      Scope  : in Scope_Stack_T;
      Name   : in String;
      Spec   : in Processor.Cell_Spec_T;
      Within : in Symbol_Table_T)
   renames Connect_Variable;
   --
   -- Deprecated name for Connect_Variable.


   procedure Connect_Variable (
      Scope    : in Scope_Stack_T;
      Name     : in String;
      Location : in Storage.Location_T;
      Within   : in Symbol_Table_T);
   --
   -- Enters a variable in the symbol table, given the (full) scope
   -- and name of the variable and the associated location map.


   function No_Connections return Connection_Set_T;
   --
   -- The empty set of connections.


   Ambiguous_Name : exception;
   --
   -- Indicates that a Scope:Name combination is ambiguous and therefore
   -- the connections for this Scope:Name are not resolved and not returned.
   -- The ambiguity arises because the Scope is too short (shallow); a
   -- longer (deeper, more complete) Scope for the same Name will resolve
   -- a single symbol and its connections.


   function Subprogram_Connections (
      Scope  : Scope_T;
      Name   : String;
      Within : Symbol_Table_T)
   return Connection_Set_T;
   --
   -- Using :
   --    identifier string,
   --    scope (perhaps partial),
   --    access to symbol table
   -- Giving:
   --    subprogram connection(s) for this identifier in this scope.
   --
   -- The given scope can be a suffix of the real scope, if this
   -- is enough to identify the subprogram unambiguously.
   -- For example, if S is a subprogram in the scope A:B:C,
   -- and there is no other subprogram named S in any scope that
   -- ends with B:C, then the scope B:C is sufficient.
   -- The full scope will be returned.
   --
   -- Propagates Ambiguous_Name if the given scope is too shallow to
   -- resolve the identifier.


   function Variable_Connections (
      Scope  : Scope_T;
      Name   : String;
      Within : Symbol_Table_T)
   return Connection_Set_T;
   --
   -- Using :
   --    identifier string,
   --    scope (perhaps partial),
   --    access to symbol table
   -- Giving:
   --    variable connection(s) for this identifier in this scope.
   --
   -- The given scope can be a suffix of the real scope, if this
   -- is enough to identify the subprogram unambiguously.
   -- For example, if V is a variable in the scope A:B:C,
   -- and there is no other variable named V in any scope that
   -- ends with B:C, then the scope B:C is sufficient.
   -- The full scope will be returned.
   --
   -- Propagates Ambiguous_Name if the given scope is too shallow to
   -- resolve the identifier.


   function Label_Connections (
      Scope  : Scope_T;
      Name   : String;
      Within : Symbol_Table_T)
   return Connection_Set_T;
   --
   -- Using :
   --    identifier string,
   --    scope (perhaps partial),
   --    access to symbol table
   -- Giving:
   --    label connection(s) for this identifier in this scope.
   --
   -- The given scope can be a suffix of the real scope, if this
   -- is enough to identify the subprogram unambiguously.
   -- For example, if L is a label in the scope A:B:C,
   -- and there is no other lable named L in any scope that
   -- ends with B:C, then the scope B:C is sufficient.
   -- The full scope will be returned.
   --
   -- Propagates Ambiguous_Name if the given scope is too shallow to
   -- resolve the identifier.


   function Connections_For_Address (
      Address : Processor.Code_Address_T;
      Within  : Symbol_Table_T)
   return Connection_Set_T;
   --
   -- All the connections for this address. These can be subprograms,
   -- labels, or line-numbers).


   function Lines_For_Address (
      Address : Processor.Code_Address_T;
      Within  : Symbol_Table_T)
   return Connection_Set_T;
   --
   -- The line-number connections for the given Address.
   -- This is a subset of Connections_For_Address.


   function Line_Before (
      Address : Processor.Code_Address_T;
      Within  : Symbol_Table_T)
   return Connection_Set_T;
   --
   -- The line-number connection with the largest code address
   -- that is before or at the given address, if there is one.
   -- Otherwise, an empty set is returned.


   function Line_After (
      Address : Processor.Code_Address_T;
      Within  : Symbol_Table_T)
   return Connection_Set_T;
   --
   -- The line-number connection with the smallest code address
   -- that is after or at the given address, if there is one.
   -- Otherwise, an empty set is returned.


   function Connections_For_Location (
      Location : Processor.Cell_Spec_T;
      Scope    : Scope_T;
      Within   : Symbol_Table_T)
   return Connection_Set_T;
   --
   -- The variable connections for the given Cell_Spec with the given
   -- Scope, Within a given symbol-table. A variable connection is "for"
   -- a given cell-spec if there are some code adresses at which the
   -- variable's value is stored in that cell.
   --
   -- This function returns only the connections where scope matches
   -- exactly. Connections with shallower or deeper scopes are not included
   -- even if the scopes are hiearchically related to the given Scope.
   --
   -- This operation is obsolete and deprecated.


   function Connections_For_Cell_Spec (
      Cell_Spec : Processor.Cell_Spec_T;
      Scope     : Scope_T;
      Within    : Symbol_Table_T)
   return Connection_Set_T
   renames Connections_For_Location;
   --
   -- Obsolete and deprecated synonym.


   --
   ---   Semi-private stuff
   --


   Deallocate_In_Stacks : Boolean := True;
   --
   -- Whether to use Unchecked_Deallocation to release unused
   -- heap memory in Scope_Stacks.


private


   type Level_List_T is array (Positive range <>) of String_Pool.Item_T;
   --
   -- The names of the levels in a scope, listed from outermost to
   -- innermost.


   package Level_Vectors is new Unbounded_Vectors (
      Element_Type   => String_Pool.Item_T,
      Vector_Type    => Level_List_T,
      Initial_Size   => 30,
      Size_Increment => 100,
      Deallocate     => Deallocate_In_Stacks);


   type Scope_Stack_T is limited record
      Stack : Level_Vectors.Unbounded_Vector;
   end record;
   --
   -- An unbounded stack of levels = string-pool items.


   type Scope_T is access Level_List_T;
   --
   -- A (canonical) reference to a scope.
   -- As in the string pool, two Scope_T values are equal if
   -- and only if the corresponding scopes (nests of level names)
   -- are equal.


   No_Scope : constant Scope_T := null;


   Global_Scope : constant Scope_T :=
      new Level_List_T'(1 .. 0 => String_Pool.Null_Item);


   type Value_Kind_T is (Unresolved, Suffix_Key, Full_Key);
   --
   -- The kinds of key-value associations that can occur in
   -- a table of symbolic names, with the following meaning:
   --
   -- Unresolved
   --    The key K does not uniquely identify a symbol-value connection,
   --    but there are two or more longer keys with K as a true suffix
   --    that do.
   --
   -- Suffix_Key
   --    The key K uniquely identifies a symbol-value connection, where
   --    K is a (true) suffix of the key of the symbol.
   --
   -- Full_Key
   --    The key K uniquely identifies a symbol-value connection, where
   --    K is the full key of the symbol.


   type Connection_T (Kind : Connection_Kind_T := Subprogram) is record

      Scope : Scope_T;

      case Kind is

      when Subprogram | Label =>
         Name    : String_Pool.Item_T;
         Address : Processor.Code_Address_T;

      when Variable =>
         Location : Storage.Location_Ref;
         Var_Name : String_Pool.Item_T;

      when Line_Number =>
         Line_Number  : Line_Number_T;
         Line_Address : Processor.Code_Address_T;

      end case;

   end record;


   type Symbol_Table_Object_T;

   type Symbol_Table_T is access Symbol_Table_Object_T;

   No_Symbol_Table : constant Symbol_Table_T := null;


end Symbols;
