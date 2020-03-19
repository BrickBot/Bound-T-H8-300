-- Formats.Dwarf.Info.Parsing (decl)
--
-- Parsing DWARF debugging information trees into a Bound-T symbol table.
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
-- $Date: 2015/10/24 20:53:52 $
--
-- $Log: formats-dwarf-info-parsing.ads,v $
-- Revision 1.6  2015/10/24 20:53:52  niklas
-- Moved to free licence.
--
-- Revision 1.5  2008-11-10 15:50:30  niklas
-- BT-CH-0159: DWARF label symbols.
--
-- Revision 1.4  2008/10/15 12:05:53  niklas
-- BT-CH-0150: DWARF location lists, lexical block scopes, CUBAs.
--
-- Revision 1.3  2007/06/14 11:16:40  niklas
-- BT-CH-0060.
--
-- Revision 1.2  2007/04/26 11:27:52  niklas
-- BT-CH-0059.
--
-- Revision 1.1  2004/04/24 18:06:19  niklas
-- First version.
--


with Formats.Dwarf.Expressions;
with Formats.Dwarf.Line_Numbers;
with Processor;
with Symbols;


package Formats.Dwarf.Info.Parsing is


   --
   --    Visitor type for traversing DWARF tree
   --


   Maximum_Scope_Depth : constant := 100;
   --
   -- A rather large maximum for the scope nesting depth.


   type Scope_Index_T is
      array (Positive range 1 .. Maximum_Scope_Depth)
      of Node_Index_T;
   --
   -- Labels each level of a scope with the index of the DWARF node
   -- that owns (defines) the scope.


   type Kit_T is limited record
      Table : Symbols.Symbol_Table_T;
      Scope : Symbols.Scope_Stack_T;
   end record;
   --
   -- The things we need to define a symbol, in addition to the symbol
   -- itself:
   --
   -- Table
   --    The symbol-table (reference type) into which new symbols
   --    are defined.
   -- Scope
   --    The lexical scope for any symbols defined.


   type Action_T is abstract new Visitor_T
   with record
      Kit   : Kit_T;
      Index : Scope_Index_T;
      Lex   : Natural := 0;
      Lines : Line_Numbers.Compilation_Set_T;
   end record;
   --
   -- Visits DWARF entries to define them as Symbols symbols.
   -- To be extended into a processor-specific Visitor type that
   -- is a parameter to the Traverse operation.
   --
   -- Kit
   --    The symbol-table and current scope.
   -- Index
   --    The index of the DWARF node that defines each level of Scope.
   --    The valid slice is 1 .. Depth(Scope).
   -- Lex
   --    A sequential numbering of lexical blocks, to provide scope
   --    names for the blocks. The numbering restarts within each
   --    subprogram.
   -- Lines
   --    Line-number information for a set of compilation units.
   --    The default initial value is a null object which means that
   --    no line-numbers are inserted in the Table.
   --
   -- This type must be extended and any of its operations can be
   -- overridden to adapt the parsing to specific needs.


   --    Tree traversing actions:


   -- overriding
   procedure Enter_Compilation (
      Start   : in     Point_T;
      Visitor : in out Action_T);
   --
   -- Erases the current scope.


   -- overriding
   procedure Visit (
      Point   : in out Point_T;
      Visitor : in out Action_T);
   --
   -- If the Point represents an interesting symbol, defines it
   -- into Visitor.Table under Visitor.Scope.


   -- overriding
   procedure Descend (
      From    : in     Point_T;
      Visitor : in out Action_T);
   --
   -- If the From point defines a new scope, extends Visitor.Scope.


   -- overriding
   procedure Ascend (
      To      : in     Point_T;
      Visitor : in out Action_T);
   --
   -- If the To point defines a new scope, pops it from Visitor.Scope.


   --    Code address mapping actions:


   Invalid_Address : exception;
   --
   -- Raised by an Action_T operation if it cannot map a DWARF
   -- address to an address in the target processor.


   -- not overriding
   function To_Code_Address (
      Address : Address_T;
      Visitor : Action_T)
   return Processor.Code_Address_T;
   --
   -- The target-specific code address that corresponds to the 
   -- given DWARF Address. May propagate Invalid_Address.
   --
   -- The default implementation simply converts the type of
   -- the Address, preserving the numerical value, and converting
   -- a possible Constraint_Error into Invalid_Address.


   --    Data object to cell mapping actions:


   No_Such_Cell : exception;
   --
   -- Raised by an Action_T operation if it cannot map the
   -- DWARF data node to a cell-spec.


   -- not overriding
   function Is_Modelled (
      Typ     : Types.Type_T;
      Visitor : Action_T)
   return Boolean
   is abstract;
   --
   -- Whether the given Type is modelled as a cell type for the
   -- present target processor.


   -- not overriding
   function Cell_Spec (
      Datum    : Point_T;
      Typ      : Types.Type_T;
      Location : Block_T;
      Visitor  : Action_T)
   return Processor.Cell_Spec_T;
   --
   -- The cell-spec that holds the Datum, a node with a Data_Tag_T
   -- and the given Type (Type_Ref attribute). The cell-spec is
   -- typically computed from the Type and the Location attribute
   -- of the Datum.
   --
   -- The function propagates No_Such_Cell if this Datum is not
   -- located in a cell as modelled for the current target processor.
   --
   -- The default implementation computes the Simple_Location_T
   -- for the Location and then delegates to the Cell_Spec function
   -- defined below for such locations.
   --
   -- Precondition: Is_Modelled (Typ) and Location.Octets > 0.


   -- not overriding
   function Cell_Spec (
      Datum    : Point_T;
      Typ      : Types.Type_T;
      Location : Expressions.Simple_Location_T;
      Visitor  : Action_T)
   return Processor.Cell_Spec_T;
   --
   -- The cell-spec that holds the Datum, a node with a Data_Tag_T
   -- and the given Type (Type_Ref attribute). The cell-spec is
   -- typically computed from the Type and the Location attribute
   -- of the Datum.
   --
   -- The function propagates No_Such_Cell if this Datum is not
   -- located in a cell as modelled for the current target processor.
   --
   -- The default implementation is the following:
   --
   -- > If the Location is an Addr, calls the primitive function
   --   Addressed_Cell (see below) to create the cell-spec.
   --
   -- > If the Location is a Register_Name_Op_T, calls the primitive
   --   function Register_Cell (see below) to create the cell-spec.
   --
   -- > If the Location is a Frame_Base_Reg, calls the primitive
   --   function Frame_Cell (see below) to create the cell-spec.
   --
   -- > If the Location is a Base_Reg(_X), calls the primitive
   --   function Based_Cell (see below) to create the cell-spec.
   --
   -- > Otherwise calls the primitive function Complex_Cell (see
   --   below) to create the cell-spec.
   --
   -- Precondition: Is_Modelled (Typ).


   -- not overriding
   function Addressed_Cell (
      Datum   : Point_T;
      Typ     : Types.Type_T;
      Addr    : Address_T;
      Visitor : Action_T)
   return Processor.Cell_Spec_T
   is abstract;
   --
   -- Like function Cell_Spec, above, but for the case where the Location
   -- of the Datum is a static Addr.


   -- not overriding
   function Register_Cell (
      Datum    : Point_T;
      Typ      : Types.Type_T;
      Register : Expressions.Register_Number_T;
      Visitor  : Action_T)
   return Processor.Cell_Spec_T
   is abstract;
   --
   -- Like function Cell_Spec, above, but for the case where the Location
   -- of the Datum is a Register_Name_Op.


   -- not overriding
   function Frame_Cell (
      Datum   : Point_T;
      Typ     : Types.Type_T;
      Offset  : Number_T;
      Visitor : Action_T)
   return Processor.Cell_Spec_T
   is abstract;
   --
   -- Like function Cell_Spec, above, but for the case where the Location
   -- of the Datum is a Frame_Base_Reg.


   -- not overriding
   function Based_Cell (
      Datum    : Point_T;
      Typ      : Types.Type_T;
      Register : Expressions.Register_Number_T;
      Offset   : Number_T;
      Visitor  : Action_T)
   return Processor.Cell_Spec_T
   is abstract;
   --
   -- Like function Cell_Spec, above, but for the case where the Location
   -- of the Datum is a Base_Reg or Base_Reg_X.


   -- not overriding
   function Complex_Cell (
      Datum    : Point_T;
      Typ      : Types.Type_T;
      Location : Block_T;
      Visitor  : Action_T)
   return Processor.Cell_Spec_T;
   --
   -- Like function Cell_Spec, above, but for the case where the Location
   -- of the Datum is none of the above simple cases.
   --
   -- The default implementation propagates No_Such_Cell.


   -- not overriding
   procedure Define_Fixed_Variable (
      Datum    : in     Point_T;
      Name     : in     String;
      Typ      : in     Types.Type_T;
      Location : in     Block_T;
      Visitor  : in out Action_T);
   --
   -- Defines a Datum of a given Name, Type, and fixed Location,
   -- in the symbol-table, under the current scope.
   -- May propagate No_Such_Cell.
   --
   -- The default implementation checks that the Location is not null
   -- and then connects this Name to the cell for this Location as
   -- defined by the function Cell_Spec above.


   -- not overriding
   procedure Define_Mobile_Variable (
      Datum    : in     Point_T;
      Name     : in     String;
      Typ      : in     Types.Type_T;
      Loc_List : in     Block_T;
      Base     : in     Address_T;
      Visitor  : in out Action_T);
   --
   -- Defines a Datum of the given Name, Type, and Location List,
   -- in the symbol-table, under the current scope. The Base gives
   -- the default base address for the Location List (CUBA).
   -- May propagate No_Such_Cell or Invalid_Address.
   --
   -- The default implementation uses the function Cell_Spec, above,
   -- to find the cell for each entry in the Location List and then
   -- connects this Name to the resulting Storage.Location_T.


   --
   --    Default tag sets for opening and visiting
   --
   -- The following tag-sets may be suitable as the Opening and
   -- Visiting parameters of the operation Traverse (Tree). They
   -- are compatible with the primitive operations of the visitor
   -- type Action_T; if larger sets are used, these operations
   -- may have to be extended (overridden) too.


   Tags_To_Open : constant Tag_Set_T := (
      Module_Tag_T    |
      Callable_Tag_T  |
      Block_Tag_T     => True,
      others          => False);
   --
   -- Tags for entries that should be opened and their children
   -- traversed.


   Tags_To_Visit : constant Tag_Set_T := (
      Compile_Unit   |
      Callable_Tag_T |
      Label          |
      Data_Tag_T     => True,
      others         => False);
   --
   -- Tags for entries that should be visited.


   --
   --    Line-number parsing
   --
   -- The following operations are provided in case the automatic line-number
   -- handling by Symbol_Visitor_T is not suitable.


   procedure Define_Line_Numbers (
      From : in     Line_Numbers.Compilation_T;
      Into : in out Symbols.Symbol_Table_T);
   --
   -- Parses the DWARF line-number information From a given compilation
   -- unit and inserts the result Into a symbol-table.


end Formats.Dwarf.Info.Parsing;
