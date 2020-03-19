-- Formats.Dwarf.Info.Parsing (body)
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
-- $Revision: 1.10 $
-- $Date: 2015/10/24 20:53:52 $
--
-- $Log: formats-dwarf-info-parsing.adb,v $
-- Revision 1.10  2015/10/24 20:53:52  niklas
-- Moved to free licence.
--
-- Revision 1.9  2014/06/28 10:00:40  niklas
-- Updated to use Storage.Code_Address_T as a private type.
--
-- Revision 1.8  2014/06/20 19:50:11  niklas
-- Extended Define_Mobile_Variable to handle illegal addresses.
--
-- Revision 1.7  2009-01-18 08:34:28  niklas
-- Removed unused context clause.
--
-- Revision 1.6  2008/11/10 15:50:30  niklas
-- BT-CH-0159: DWARF label symbols.
--
-- Revision 1.5  2008/10/28 09:48:33  niklas
-- Corrected Define_Mobile_Variable to supply only the defined
-- Storage_Locations to Connect_Variable (error in BT-CH-0150).
--
-- Revision 1.4  2008/10/15 12:05:52  niklas
-- BT-CH-0150: DWARF location lists, lexical block scopes, CUBAs.
--
-- Revision 1.3  2007/06/14 11:16:40  niklas
-- BT-CH-0060.
--
-- Revision 1.2  2006/04/12 19:29:12  niklas
-- Changed and extended procedure Visit (Point) to enter a
-- subprogram in the symbol-table only if it has a name and
-- an entry point and is not inlined.
--
-- Revision 1.1  2004/04/24 18:06:19  niklas
-- First version.
--


with Formats.Dwarf;
with Formats.Dwarf.Info;
with Formats.Dwarf.Line_Numbers;
with Formats.Dwarf.Opt;
with Output;
with Processor;
with Storage;


package body Formats.Dwarf.Info.Parsing is


   --    Tree traversing actions:


   procedure Enter_Compilation (
      Start   : in     Point_T;
      Visitor : in out Action_T)
   is
      Node : constant Node_T           := Node_At (Start);
      VDL  : constant Value_Def_List_T := Values (Node);
   begin

      Output.Note (
           "Entering DWARF compilation unit for "
         & String'(Value_Of (Name, VDL, Node, "[no name]")));

      Symbols.Make_Global (Visitor.Kit.Scope);

   end Enter_Compilation;


   function Type_Image (
      Node   : Node_T;
      Values : Value_Def_List_T;
      Tree   : Tree_T)
   return String
   --
   -- A description of the type of the data object represented
   -- by the Node in the Tree, from the Type_Ref attribute within
   -- the attribute Values of the Node.
   --
   -- Handles Attribute_Absent, Wrong_Form, No_Such_Type.
   --
   is
   begin

      return Types.Image (Type_Of (Node, Values, Tree));

   exception

   when Attribute_Absent   => return "(no Type_Ref)";
   when Wrong_Form         => return "(Type_Ref has wrong Form)";
   when Types.No_Such_Type => return "(no such type)";

   end Type_Image;


   procedure Visit (
      Point   : in out Point_T;
      Visitor : in out Action_T)
   is
      use type In_Memory.Count_T;

      Node : constant Node_T           := Node_At (Point);
      Tag  : constant Tag_T            := Info.Tag (Point);
      VDL  : constant Value_Def_List_T := Values (Node);


      procedure Get_Line_Numbers
      --
      -- Gets and defines the line-numbers from a Compile_Unit node.
      --
      is

         Stmt_List_Offset : Section_Offset_T;
         -- The value of the Stmt_List attribute, which is the
         -- offset, in the ".debug_line" section, of the line-number
         -- block for this compilation unit.

      begin

         Stmt_List_Offset := Value_Of (Stmt_List, VDL, Node);

         Define_Line_Numbers (
            From =>
               Line_Numbers.Compilation (
                  At_Offset => Stmt_List_Offset,
                  Within    => Visitor.Lines),
            Into => Visitor.Kit.Table);

      exception

      when Attribute_Absent =>
         -- The Node has no value for the Stmt_List attribute.

         if Opt.Trace_Loading then

            Output.Trace (
               Locus => Output.Locus (
                  Source_File => Value_Of (Name, VDL, Node, "")),
               Text => "No DWARF line-number information.");

         end if;

      end Get_Line_Numbers;


      procedure Visit_Subprogram
      --
      -- Visiting a Subprogram node.
      --
      is

         Name_Found : Boolean;
         -- Whether the Subprogram Node has a Name attribute.

         Name_Def : Value_Def_T;
         -- The Name attribute of the Subprogram Node.
         -- Valid if Name_Found.

         Entry_Point_Found : Boolean;
         -- Whether the entry-point of the Subprogram Node was found.

         Entry_Address : Address_T;
         -- The entry point of the Subprogram Node.
         -- Valid if Entry_Point_Found.

         Target_Entry : Processor.Code_Address_T;
         -- The target-specific address corresponding to Entry_Address.

      begin

         Find (
            Attribute => Name,
            Within    => VDL,
            Found     => Name_Found,
            Value_Def => Name_Def);

         if not Name_Found then

            if Opt.Trace_Loading then

               Output.Trace ("DWARF defines no Name for this Subprogram.");

            end if;

         else

            Find_Entry_Point (
               Subprogram => Node,
               Values     => VDL,
               Found      => Entry_Point_Found,
               Address    => Entry_Address);

            if not Entry_Point_Found then

               if Opt.Trace_Loading then

                  Output.Trace (
                     Text  => "DWARF defines no entry point.",
                     Locus => Output.Locus (
                        Call_Path => Value_Of (Name_Def, Node)));

               end if;

            else

               Target_Entry := To_Code_Address (
                  Address => Entry_Address,
                  Visitor => Action_T'Class (Visitor));
               -- May raise Invalid_Address.

               -- Good, Target_Entry is valid:

               Symbols.Connect_Subprogram (
                  Scope   => Visitor.Kit.Scope,
                  Name    => Value_Of (Name_Def, Node),
                  Address => Target_Entry,
                  Within  => Visitor.Kit.Table);

            end if;

         end if;

      end Visit_Subprogram;


      procedure Visit_Label
      --
      -- Visiting a Label node.
      --
      is

         Name_Found : Boolean;
         -- Whether the Label node has a Name attribute.

         Name_Def : Value_Def_T;
         -- The Name attribute of the Label node.
         -- Valid if Name_Found.

         Address_Found : Boolean;
         -- Whether the machine address of a Label node was found.

         Address_Def : Value_Def_T;
         -- The Low_PC attribute of the Label node.
         -- Valid if Address_Found.

         Address : Address_T;
         -- The machine address property of the Label node.
         -- Valid if Address_Found.

         Target_Address : Processor.Code_Address_T;
         -- The target-specific address corresponding to Address.

      begin

         Find (
            Attribute => Name,
            Within    => VDL,
            Found     => Name_Found,
            Value_Def => Name_Def);

         if not Name_Found then

            if Opt.Trace_Loading then

               Output.Trace ("DWARF defines no Name for this Label.");

            end if;

         else

            Find (
               Attribute => Low_PC,
               Within    => VDL,
               Found     => Address_Found,
               Value_Def => Address_Def);

            if not Address_Found then

               if Opt.Trace_Loading then

                  Output.Trace (
                     Text  => "DWARF defines no address.",
                     Locus => Output.Locus (
                        Call_Path => Value_Of (Name_Def, Node)));

               end if;

            else

               Address := Value_Of (Address_Def, Node);

               Target_Address := To_Code_Address (
                  Address => Address,
                  Visitor => Action_T'Class (Visitor));
               -- May raise Invalid_Address.

               -- Good, Target_Address is valid:

               Symbols.Connect_Label (
                  Scope   => Visitor.Kit.Scope,
                  Name    => Value_Of (Name_Def, Node),
                  Address => Target_Address,
                  Within  => Visitor.Kit.Table);

            end if;

         end if;

      end Visit_Label;


      procedure Visit_Datum
      --
      -- Visiting a Variable node or a Formal Parameter node.
      --
      is

         Typ : Types.Type_T;
         -- The type of the data object.

         Loc_Def : Value_Def_T;
         -- The value and form of the Location attribute of the data object.

         Loc_Base : Poss_Code_Address_T;
         -- The CUBA for a Location list.

      begin

         if Opt.Trace_Var_Par then

            Output.Trace (
                 "DWARF "
               & Tag_T'Image (Tag)
               & " """
               & Value_Of (Name, VDL, Node)
               & """, type "
               & Type_Image (Node, VDL, Tree (Point)));

         end if;

         Typ := Type_Of (Node, VDL, Tree (Point));

         if not Is_Modelled (Typ, Action_T'Class (Visitor)) then

            if Opt.Trace_Var_Par then

               Output.Trace ("DWARF type is not modelled.");

            end if;

         else
            -- This Type is modelled, good.

            Loc_Def := Value_Def (Location, VDL);

            case Loc_Def.Form is

            when Block_Form_T =>
               -- A block with a single Location expression.

               Define_Fixed_Variable (
                  Datum    => Point,
                  Typ      => Typ,
                  Name     => Value_Of (Name, VDL, Node),
                  Location => Value_Of (Loc_Def, Node),
                  Visitor  => Action_T'Class (Visitor));

            when Section_Offset_Form_T =>
               -- A loclistpr to a Location List block.

               Loc_Base := CUBA (Node);

               if Loc_Base.Defined then

                  Define_Mobile_Variable (
                     Datum    => Point,
                     Name     => Value_Of (Name, VDL, Node),
                     Typ      => Typ,
                     Loc_List => Location_List (
                        Offset => Value_Of (Loc_Def, Node),
                        From   => Node),
                     Base     => Loc_Base.Address,
                     Visitor  => Action_T'Class (Visitor));

               elsif Opt.Trace_Var_Par then

                  Output.Trace (
                     "DWARF Compilation Unit Base Address unknown.");

               end if;

            when others =>

               if Opt.Trace_Var_Par then

                  Output.Trace (
                       "DWARF Location attribute in "
                     & Image (Loc_Def.Form)
                     & " form is not understood.");

               end if;

            end case;

         end if;

      end Visit_Datum;


   begin  -- Visit

      case Tag is

      when Subprogram =>

         if not Is_Inlined (Node, VDL) then

            Visit_Subprogram;

         end if;

      when Label =>

         Visit_Label;

      when Compile_Unit =>

         if not Line_Numbers.Is_Null (Visitor.Lines) then
            -- We should try to get line-number information.

            Get_Line_Numbers;

         end if;

      when Variable
         | Formal_Parameter =>

         Visit_Datum;

      when others =>

         null;

      end case;

   exception

   when Attribute_Absent =>

      Output.Note ("DWARF node visit aborted; missing attribute.");
      --
      -- If Opt.Trace_Var_Par, a Trace has been issued earlier.

   when Wrong_Form =>

      if Opt.Trace_Var_Par then

         Output.Trace ("DWARF node visit aborted; attribute has wrong form.");

      end if;

   when Invalid_Address =>

      if Opt.Trace_Var_Par then

         Output.Trace ("DWARF address not valid for target.");

      end if;

   when No_Such_Cell =>

      if Opt.Trace_Var_Par then

         Output.Trace ("DWARF data object not mapped to any cell.");

      end if;

   end Visit;


   procedure Descend (
      From    : in     Point_T;
      Visitor : in out Action_T)
   is
      Node : constant Node_T           := Node_At (From);
      VDL  : constant Value_Def_List_T := Values (Node);
   begin

      case Tag (Node) is

      when Compile_Unit
         | Subprogram   =>

         Symbols.Nest (
            Level => Value_Of (Name, VDL, Node, Default => ""),
            Scope => Visitor.Kit.Scope);

         Visitor.Index (Symbols.Depth (Visitor.Kit.Scope)) := Index (Node);

         Visitor.Lex := 0;

      when Lexical_Block =>

         Visitor.Lex := Visitor.Lex + 1;

         Symbols.Nest (
            Level => "block" & Output.Image (Visitor.Lex),
            Scope => Visitor.Kit.Scope);

         Visitor.Index (Symbols.Depth (Visitor.Kit.Scope)) := Index (Node);

      when others =>

         null;

      end case;

   end Descend;


   procedure Ascend (
      To      : in     Point_T;
      Visitor : in out Action_T)
   is
      Node  : constant Node_T  := Node_At (To);
      Depth : constant Natural := Symbols.Depth (Visitor.Kit.Scope);
   begin

      if       Depth in Visitor.Index'Range
      and then Visitor.Index(Depth) = Index (Node)
      then
         -- This is the Node that defined the current scope.

         Symbols.Unnest (Visitor.Kit.Scope);

      end if;

   end Ascend;


   --    Code address mapping actions:


   -- not overriding
   function To_Code_Address (
      Address : Address_T;
      Visitor : Action_T)
   return Processor.Code_Address_T
   is
   begin

      return Processor.Code_Address_T (Address);

   exception

   when Constraint_Error =>

      raise Invalid_Address;

   end To_Code_Address;


   --    Data object to cell mapping actions:


   function Cell_Spec (
      Datum    : Point_T;
      Typ      : Types.Type_T;
      Location : Block_T;
      Visitor  : Action_T)
   return Processor.Cell_Spec_T
   is
   begin

      return Cell_Spec (
         Datum    => Datum,
         Typ      => Typ,
         Location => Expressions.Simple_Location (Location),
         Visitor  => Action_T'Class (Visitor));

   end Cell_Spec;


   function Cell_Spec (
      Datum    : Point_T;
      Typ      : Types.Type_T;
      Location : Expressions.Simple_Location_T;
      Visitor  : Action_T)
   return Processor.Cell_Spec_T
   is
      use Expressions;
   begin

      case Location.Kind is

      when Addr =>

         return Addressed_Cell (
            Datum   => Datum,
            Typ     => Typ,
            Addr    => Location.Addr,
            Visitor => Action_T'Class (Visitor));

      when Reg =>

         return Register_Cell (
            Datum    => Datum,
            Typ      => Typ,
            Register => Location.Reg,
            Visitor  => Action_T'Class (Visitor));

      when Frame =>

         return Frame_Cell (
            Datum   => Datum,
            Typ     => Typ,
            Offset  => Location.Frame_Offset,
            Visitor => Action_T'Class (Visitor));

      when Based =>

         return Based_Cell (
            Datum    => Datum,
            Typ      => Typ,
            Register => Location.Base_Reg,
            Offset   => Location.Base_Offset,
            Visitor  => Action_T'Class (Visitor));

      when Complex =>

         return Complex_Cell (
            Datum    => Datum,
            Typ      => Typ,
            Location => Location.Expr,
            Visitor  => Action_T'Class (Visitor));

      end case;

   end Cell_Spec;


   function Complex_Cell (
      Datum    : Point_T;
      Typ      : Types.Type_T;
      Location : Block_T;
      Visitor  : Action_T)
   return Processor.Cell_Spec_T
   is

      Dummy : Processor.Cell_Spec_T;
      -- Prevent compiler error re absent return statement.

   begin

      raise No_Such_Cell;

      return Dummy;

   end Complex_Cell;


   -- not overriding
   procedure Define_Fixed_Variable (
      Datum    : in     Point_T;
      Name     : in     String;
      Typ      : in     Types.Type_T;
      Location : in     Block_T;
      Visitor  : in out Action_T)
   is
      use type In_Memory.Count_T;
   begin

      if Location.Octets = 0 then

         if Opt.Trace_Var_Par then

            Output.Trace (
                 "DWARF entity """
               & Name
               & """ has null location.");

         end if;

      else

         Symbols.Connect_Variable (
            Scope    => Visitor.Kit.Scope,
            Name     => Name,
            Location => Cell_Spec (
               Datum    => Datum,
               Typ      => Typ,
               Location => Location,
               Visitor  => Action_T'Class (Visitor)),
            Within   => Visitor.Kit.Table);

      end if;

   end Define_Fixed_Variable;


   -- not overriding
   procedure Define_Mobile_Variable (
      Datum    : in     Point_T;
      Name     : in     String;
      Typ      : in     Types.Type_T;
      Loc_List : in     Block_T;
      Base     : in     Address_T;
      Visitor  : in out Action_T)
   is
      use Expressions;
      use type In_Memory.Count_T;

      Locations : constant Location_List_T := To_Location_List (
         Block => Loc_List,
         Base  => Base);
      -- The DWARF location list.

      Loc : Location_Entry_T;
      -- One of the Locations.

      Storage_Locations : Storage.Location_T (1 .. Locations'Length);
      -- The corresponding Storage locations.

      Last : Natural := 0;
      -- The number of accepted Storage_Locations.

      Rainge : Storage.Code_Address_Range_T;
      -- The range of code address for Loc.

      Cell : Storage.Cell_T;
      -- The cell for Loc.

   begin

      -- Translate the Locations to Storage_Locations:

      for L in Locations'Range loop

         Loc := Locations(L);

         if Loc.Expr.Octets > 0 then

            begin

               -- Convert the address range (may raise Invalid_Address
               -- or Constraint_Error):

               Rainge := Storage.Rainge (
                  First => To_Code_Address (
                     Address => Loc.Beginning,
                     Visitor => Action_T'Class (Visitor)),
                  Last => To_Code_Address (
                     Address => Loc.Ending - 1,
                     Visitor => Action_T'Class (Visitor)));

               -- Find the cell (may raise No_Such_Cell):

               Cell := Storage.Cell (Cell_Spec (
                  Datum    => Datum,
                  Typ      => Typ,
                  Location => Loc.Expr,
                  Visitor  => Action_T'Class (Visitor)));

               -- Successful translation:

               Last := Last + 1;

               Storage_Locations(Last) := (
                  Address => Rainge,
                  Cell    => Cell);

            exception

            when Invalid_Address
               | Constraint_Error =>

               if Opt.Trace_Var_Par then

                  Output.Trace (
                       "DWARF entity """
                     & Name
                     & """ has unacceptable location range"
                     & Code_Address_T'Image (Loc.Beginning)
                     & " .."
                     & Code_Address_T'Image (Loc.Ending));

               end if;

            when No_Such_Cell =>

               if Opt.Trace_Var_Par then

                  Output.Trace (
                       "DWARF entity """
                     & Name
                     & """ not located in cell for "
                     & Storage.Image (Rainge));

               end if;

            end;

         end if;

      end loop;

      -- Did we get any valid locations?

      if Last = 0 then

         if Opt.Trace_Var_Par then

            Output.Trace (
                 "DWARF entity """
               & Name
               & """ has no locations.");

         end if;

      else

         Symbols.Connect_Variable (
            Scope    => Visitor.Kit.Scope,
            Name     => Name,
            Location => Storage_Locations(1 .. Last),
            Within   => Visitor.Kit.Table);

      end if;

   end Define_Mobile_Variable;


   --
   ---   Line-number parsing
   --


   procedure Insert_Row (
      Row    : in     Line_Numbers.Row_T;
      Prev   : in     Line_Numbers.Row_T;
      First  : in     Boolean;
      Last   : in     Boolean;
      From   : in     Line_Numbers.Compilation_T;
      Done   : in out Boolean;
      Result : in out Kit_T)
   --
   -- Inserts a row from the DWARF line-number table into the
   -- given Symbols table.
   --
   is
   begin

      if First or Row.File /= Prev.File then
         -- Switch scope.
         -- For line-numbers, the Scope has two levels:
         --  > source-file name, and
         --  > subprogram name.
         -- Here we only know the source-file name; we hope that
         -- Bound-T knows the subprogram from other sources.

         Symbols.Make_Global (Result.Scope);

         Symbols.Nest (
            Level => Line_Numbers.Source_File_Name (
               File   => Row.File,
               Within => From),
            Scope => Result.Scope);

         Symbols.Nest (Level => "", Scope => Result.Scope);

      end if;

      if not Row.End_Sequence then
         -- A real row rather than a terminator.

         Symbols.Connect_Line (
            Scope   => Result.Scope,
            Number  => Symbols.Line_Number_T (Row.Line),
            Address => Processor.Code_Address_T (Row.Address),
            Within  => Result.Table);

      -- else
      --    Rows with the End_Sequence flag tend to have dummy
      --    line-number components (e.g. 1) so we ignore them.

      end if;

   end Insert_Row;


   procedure Insert_Rows_In_Table
   is new Line_Numbers.Scan_Table (
      Result_Type => Kit_T,
      Action      => Insert_Row);
   --
   -- Scans the line-number table and inserts selected rows
   -- in a Symbols symbol-table.


   procedure Define_Line_Numbers (
      From : in     Line_Numbers.Compilation_T;
      Into : in out Symbols.Symbol_Table_T)
   is

      Kit : Kit_T;
      -- Combines the symbol-table (Into) with a working Scope.

   begin

      Symbols.Make_Global (Kit.Scope);

      Kit.Table := Into;
      -- This is a by-reference type so any updated through
      -- Kit.Table are also visible through Into.

      Insert_Rows_In_Table (
         From   => From,
         Result => Kit);

   end Define_Line_Numbers;


end Formats.Dwarf.Info.Parsing;
