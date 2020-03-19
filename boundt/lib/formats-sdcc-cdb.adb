-- Formats.SDCC.CDB (body)
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
-- $Revision: 1.6 $
-- $Date: 2015/10/24 20:53:54 $
--
-- $Log: formats-sdcc-cdb.adb,v $
-- Revision 1.6  2015/10/24 20:53:54  niklas
-- Moved to free licence.
--
-- Revision 1.5  2007-10-07 19:36:28  niklas
-- Extended procedure Memory_Symbol to obey the new options
-- Assume_Ext_RAM_U8 and Warn_Assumed_Type_Loc.
--
-- Revision 1.4  2007/10/07 12:51:09  niklas
-- Extended Memory_Symbol to assume that symbols for which no SDCC.Symbol
-- is found represent unsigned octet variables in external RAM.
--
-- Revision 1.3  2007/09/23 20:55:58  niklas
-- Improved the default Register_Symbol operation to ignore symbols
-- that map to a null register list.
--
-- Revision 1.2  2007/09/23 20:26:44  niklas
-- For function records, translate the address-space code 'Z' to
-- the Code space; for other records, to Undefined.
--
-- Revision 1.1  2007/09/22 14:19:25  niklas
-- First version.
--


with Ada.Text_IO;
with Formats.SDCC.Opt;
with Output;


package body Formats.SDCC.CDB is


   Max_Line_Length : constant := 5_000;
   --
   -- The maximum length of a CDB line, assumed.


   type Line_T is record
      Num  : Output.Line_Number_T;
      Cur  : Positive;
      Last : Natural;
      Text : String (1 .. Max_Line_Length);
   end record;
   --
   -- A line from a CDB file, with a current-position pointer.
   --
   -- Num
   --    The sequential number of the line, it its file.
   -- Cur
   --    The current position is Text(Cur).
   --    If Cur <= Last, there is something left to parse.
   --    If Cur >  Last, the whole line is parsed.
   -- Last
   --    The line is Text(1 .. Last).
   -- Text
   --    The characters in the line, Text(1 .. Last).


   type Field_T is record
      First, Last : Positive;
   end record;
   --
   -- A field within a line, comprising Text(First .. Last).


   function Left (Within : Line_T) return Natural
   --
   -- The number of characters left to be parsed.
   --
   is
   begin

      return Integer'Max (0, Within.Last - Within.Cur + 1);

   end Left;


   function Nothing_Left (Within : Line_T) return Boolean
   --
   -- Whether all the line text has been parsed.
   --
   is
   begin

      return Within.Cur > Within.Last;

   end Nothing_Left;


   Syntax_Error : exception;
   --
   -- Something wrong in the form of a line.


   End_Line_Error : exception;
   --
   -- Scanning past the end of a Line.


   procedure Error (What : in String; Line : in Line_T)
   --
   -- Report an error in the Line and propagate Syntax_Error.
   --
   is
   begin

      Output.Error (
           "CDB"
         & Output.Field_Separator
         & What
         & Output.Field_Separator
         & '"' & Line.Text(Line.Cur .. Line.Last) & '"');

      raise Syntax_Error;

   end Error;


   function Current_Char (Line : Line_T) return Character
   --
   -- The current character, if it exists, otherwise propagates
   -- End_Line_Error.
   --
   is
   begin

      if Line.Cur > Line.Last then

         raise End_Line_Error;

      end if;

      return Line.Text(Line.Cur);

   end Current_Char;


   function Here (Char : Character; Within : Line_T)
   return Boolean
   --
   -- Whether the current character (exists and) is Char.
   -- Never propagates End_Line_Error.
   --
   is
   begin

      return   Within.Cur <= Within.Last
      and then Within.Text(Within.Cur) = Char;

   end Here;


   procedure Next (Within : in out Line_T)
   --
   -- Advances one character Within the line.
   -- Propagates End_Line_Error if already at the end of the line.
   --
   is
   begin

      if Nothing_Left (Within) then

         raise End_Line_Error;

      end if;

      Within.Cur := Within.Cur + 1;

   end Next;


   procedure Skip (Char : in Character; Within : in out Line_T)
   --
   -- Consumes the Char, which is expected to be the current
   -- character Within the line. Otherwise propagates Syntax_Error
   -- or End_Line_Error.
   --
   is
   begin

      if Current_Char (Within) /= Char then

         Error ("Expected '" & Char & ''', Within);

      end if;

      Next (Within);

   end Skip;


   procedure Get_Field (
      Field :    out Field_T;
      From  : in out Line_T;
      Stop  : in     Character;
      Skip  : in     Boolean)
   --
   -- Sets the Field to the Text from the current position up
   -- to but not including the next Stop character.
   -- If Skip is true the stop character is also consumed (skipped)
   -- and in this case the operation propagates End_Line_Error if
   -- there is no Stop character in the rest of the line.
   --
   is
   begin

      Field.First := From.Cur;

      loop

         Next (From);

         exit when Nothing_Left (From);

         exit when Current_Char (From) = Stop;

      end loop;

      Field.Last := From.Cur - 1;

      if Skip then

         CDB.Skip (Stop, From);

      end if;

   end Get_Field;


   procedure Get_Field (
      Field :    out Field_T;
      From  : in out Line_T;
      Stop1 : in     Character;
      Stop2 : in     Character;
      Skip  : in     Boolean)
   --
   -- Sets the Field to the Text from the current position up
   -- to but not including the next Stop1 or Stop2 character.
   -- The stop character is also consumed if Skip is true and
   -- in this case the operation propagates End_Line_Error if
   -- there is no Stop1/2 character in the rest of the line.
   --
   is

      Char : Character;
      -- The current character, possibly a Stop.

   begin

      Field.First := From.Cur;

      loop

         Next (From);

         exit when Nothing_Left (From);

         Char := Current_Char (From);

         exit when Char = Stop1 or Char = Stop2;

      end loop;

      Field.Last := From.Cur - 1;

      if Skip then

         CDB.Skip (Current_Char (From), From);

      end if;

   end Get_Field;


   procedure Get_Rest (
      From  : in out Line_T;
      Field :    out Field_T)
   --
   -- Sets the Field to the rests of the Text from the current
   -- position up to the end of the line. Propagates End_Line_Error
   -- if there is nothing left on the line.
   --
   is
   begin

      if Nothing_Left (From) then

         raise End_Line_Error;

      end if;

      Field.First := From.Cur;
      Field.Last  := From.Last;

      From.Cur := From.Last + 1;

   end Get_Rest;


   function Value (
      Field : Field_T;
      Line  : Line_T)
   return String
   --
   -- The contents of the field.
   --
   is
   begin

      return Line.Text(Field.First .. Field.Last);

   end Value;


   function Value (
      Field : Field_T;
      Line  : Line_T)
   return String_Pool.Item_T
   --
   -- The contents of the field.
   --
   is
   begin

      return String_Pool.To_Item (Value (Field, Line));

   end Value;


   function Number (
      Field : Field_T;
      Line  : Line_T)
   return Symbols.Line_Number_T
   --
   -- The contents of the field.
   --
   is
   begin

      return Symbols.Line_Number_T'Value (Value (Field, Line));

   end Number;


   function Number (
      Field : Field_T;
      Line  : Line_T)
   return Natural
   --
   -- The contents of the field.
   --
   is
   begin

      return Natural'Value (Value (Field, Line));

   end Number;


   function Frame_Offset (
      Field : Field_T;
      Line  : Line_T)
   return Frame_Offset_T
   --
   -- The contents of the field.
   --
   is
   begin

      return Frame_Offset_T'Value (Value (Field, Line));

   end Frame_Offset;


   function Address (
      Field : Field_T;
      Line  : Line_T)
   return Processor.Code_Address_T
   --
   -- The contents of the field.
   --
   is
   begin

      return Processor.Code_Address_T'Value (
           "16#"
         & Value (Field, Line)
         & '#');

   end Address;


   procedure Get (
      From : in     Ada.Text_IO.File_Type;
      Line :    out Line_T)
   --
   -- Reads the next Line From the file.
   -- May propagate End_Error.
   --
   is
   begin

      Line.Num := Output.Line_Number_T (Ada.Text_IO.Line (From));

      Ada.Text_IO.Get_Line (
         File => From,
         Item => Line.Text,
         Last => Line.Last);

      Line.Cur := 1;

   end Get;


   procedure Build_Symbol_Scope (
      Scope  : in     Scope_T;
      Action : in out Action_T)
   is
   begin

      Symbols.Make_Global (Action.Scope);

      case Scope.Kind is

      when Global =>

         null;

      when File =>

         Symbols.Nest (
            Level => String_Pool.To_String (Scope.File_Name),
            Scope => Action.Scope);

      when Local =>

         Symbols.Nest (
            Level => String_Pool.To_String (Scope.Func_Name),
            Scope => Action.Scope);

         Symbols.Nest (
            Level => Output.Image (Scope.Level),
            Scope => Action.Scope);

         Symbols.Nest (
            Level => Output.Image (Scope.Block),
            Scope => Action.Scope);

      end case;

   end Build_Symbol_Scope;


   procedure Register_Symbol (
      Name         : in     String;
      Scope        : in     Scope_T;
      Typ          : in     Type_Def_T;
      Registers    : in     Reg_List_T;
      Action       : in out Action_T;
      Symbol_Table : in     Symbols.Symbol_Table_T)
   is

      Spec : Processor.Cell_Spec_T;
      -- The cell-spec, if located.

   begin

      if Registers'Length = 0 then

         Output.Note (
              "CDB symbol optimized away (no registers)"
            & Output.Field_Separator
            & Image (Scope)
            & Output.Field_Separator
            & Name);

         raise Not_A_Cell;

      elsif Registers'Length = 1 then

         Spec := To_Cell_Spec (
            Typ      => Typ,
            Register => String_Pool.To_String (Registers(Registers'First)),
            Action   => Action_T'Class (Action));

      else

         Spec := To_Cell_Spec (
            Typ       => Typ,
            Registers => Registers,
            Action    => Action_T'Class (Action));

      end if;

      Build_Symbol_Scope (
         Scope  => Scope,
         Action => Action_T'Class (Action));

      Symbols.Connect_Variable (
         Scope    => Action.Scope,
         Name     => Name,
         Location => Spec,
         Within   => Symbol_Table);

   exception

   when Not_A_Cell =>

      null;

   end Register_Symbol;


   procedure Stack_Symbol (
      Name         : in     String;
      Scope        : in     Scope_T;
      Typ          : in     Type_Def_T;
      Space        : in     Stack_Space_T;
      Offset       : in     Frame_Offset_T;
      Action       : in out Action_T;
      Symbol_Table : in     Symbols.Symbol_Table_T)
   is

      Spec : Processor.Cell_Spec_T;
      -- The cell-spec, if located.

   begin

      Spec := To_Cell_Spec (
         Typ    => Typ,
         Space  => Space,
         Offset => Offset,
         Action => Action_T'Class (Action));

      Build_Symbol_Scope (
         Scope  => Scope,
         Action => Action_T'Class (Action));

      Symbols.Connect_Variable (
         Scope    => Action.Scope,
         Name     => Name,
         Location => Spec,
         Within   => Symbol_Table);

   exception

   when Not_A_Cell =>

      null;

   end Stack_Symbol;


   Char_Chain : constant Type_Chain_Ref :=
      new Type_Chain_T'(1 => (Kind => Char));
   --
   -- A type-chain for "char". To be used when better information
   -- is not available (see Memory_Symbol).


   procedure Memory_Symbol (
      Name         : in     String;
      Scope        : in     Scope_T;
      Address      : in     String;
      Action       : in out Action_T;
      Symbol_Table : in     Symbols.Symbol_Table_T)
   is

      Symbol : Symbol_T;
      -- The Compiler symbol for this Name & Scope.

      Spec : Processor.Cell_Spec_T;
      -- The cell-spec, if located.

   begin

      begin

         Symbol := SDCC.Symbol (
            Name   => Name,
            Scope  => Scope,
            Within => Action.Symbols);

      exception

      when No_Such_Symbol =>

         Output.Note (
              "CDB Symbol record not found"
            & Output.Field_Separator
            & Image (Scope)
            & Output.Field_Separator
            & Name
            & Output.Field_Separator
            & Address);

         if Opt.Assume_Ext_RAM_U8 then
            -- Let us guess that the symbol is in external RAM and
            -- is an unsigned 8-bit variable.

            Symbol := (
               Name  => String_Pool.To_Item (Name),
               Scope => Scope,
               Typ   => (
                  Size  => 1,
                  Chain => Char_Chain,
                  Sign  => Unsigned),
               Location => (Space => Ext_RAM));

            if Opt.Warn_Assumed_Type_Loc then

               Output.Warning (
                    "CDB symbol """
                  & Name
                  & """ ("
                  & Image (Scope)
                  & ") assumed to be unsigned octet in External RAM.");

            end if;

         else
            -- We shall not assume. Anything.

            raise Not_A_Cell;

         end if;

      end;

      if Is_Function (Symbol) then
         -- This Symbol represents a subprogram.

         Build_Symbol_Scope (
            Scope  => Scope,
            Action => Action_T'Class (Action));

         Symbols.Connect_Subprogram (
            Scope   => Action.Scope,
            Name    => Name,
            Address => To_Subprogram_Address (
               Address,
               Action_T'Class (Action)),
            Within  => Symbol_Table);

      else
         -- This Symbol represents a data variable.

         case Symbol.Location.Space is

         when Stack_Space_T
            | Register
            | Undefined =>

            Output.Warning (
                 "CDB Linker symbol in "
               & Address_Space_T'Image (Symbol.Location.Space)
               & Output.Field_Separator
               & Image (Scope)
               & Output.Field_Separator
               & Name);

         when Memory_Space_T =>

            Spec := To_Cell_Spec (
               Typ     => Symbol.Typ,
               Space   => Symbol.Location.Space,
               Address => Address,
               Action  => Action_T'Class (Action));

            Build_Symbol_Scope (
               Scope  => Scope,
               Action => Action_T'Class (Action));

            Symbols.Connect_Variable (
               Scope    => Action.Scope,
               Name     => Name,
               Location => Spec,
               Within   => Symbol_Table);

         end case;

      end if;

   exception

   when Not_A_Cell =>

      null;

   end Memory_Symbol;


   procedure Assembler_Line (
      File         : in     String;
      Line         : in     Symbols.Line_Number_T;
      Address      : in     Processor.Code_Address_T;
      Action       : in out Action_T;
      Symbol_Table : in     Symbols.Symbol_Table_T)
   is
   begin

      Symbols.Make_Global (Action.Scope);

      Symbols.Nest (
         Level => File,
         Scope => Action.Scope);

      Symbols.Connect_Line (
         Scope   => Action.Scope,
         Number  => Line,
         Address => Address,
         Within  => Symbol_Table);

   end Assembler_Line;
 

   procedure C_Line (
      File         : in     String;
      Line         : in     Symbols.Line_Number_T;
      Level        : in     Natural;
      Block        : in     Natural;
      Address      : in     Processor.Code_Address_T;
      Action       : in out Action_T;
      Symbol_Table : in     Symbols.Symbol_Table_T)
   is
   begin

      Symbols.Make_Global (Action.Scope);

      Symbols.Nest (
         Level => File,
         Scope => Action.Scope);

      Symbols.Connect_Line (
         Scope   => Action.Scope,
         Number  => Line,
         Address => Address,
         Within  => Symbol_Table);

   end C_Line;


   procedure Parse_Type_Layer (
      Line : in out Line_T;
      Typ  :    out Type_Layer_T)
   --
   -- Parses a type layer (DCLType).
   -- Leaves the line at the separator after the DCLType.
   --
   is

      C1, C2 : Character;
      -- The two characters of the type code.

      Spec_Field : Field_T;
      -- A field that specifies something about the type.

      Bit_Pos_Field : Field_T;
      -- The starting bit position of a bit-field type.


      procedure Unknown_Type
      is
      begin

         Error ("Unknown type """ & C1 & C2 & '"', Line);

      end Unknown_Type;


      procedure Get_Spec
      is
      begin

         Get_Field (
            Field => Spec_Field,
            From  => Line,
            Stop1 => ',',
            Stop2 => ':',
            Skip  => False);

      end Get_Spec;


   begin  -- Parse_Type_Layer

      C1 := Current_Char (Line); Next (Line);
      C2 := Current_Char (Line); Next (Line);

      case C1 is

      when 'D' =>

         case C2 is

         when 'A' =>
            -- DA = array.

            Get_Spec;

            Typ := (
               Kind   => Aray,
               Length => Number (Spec_Field, Line));

         when 'F' =>

            Typ := (
               Kind         => Func,
               Is_Interrupt => False,
               Interrupt    => 0,
               Reg_Bank     => 0);
            --
            -- All but Kind are dummy.

         when 'G' => Typ := (Kind => Generic_Pointer);
         when 'C' => Typ := (Kind => Code_Pointer);
         when 'X' => Typ := (Kind => Ext_RAM_Pointer);
         when 'D' => Typ := (Kind => Int_RAM_Pointer);
         when 'P' => Typ := (Kind => Paged_Pointer);
         when 'I' => Typ := (Kind => Upper_128_Pointer);

         when others =>

            Unknown_Type;

         end case;

      when 'S' =>

         case C2 is

         when 'L' => Typ := (Kind => Long);
         when 'I' => Typ := (Kind => Int);
         when 'C' => Typ := (Kind => Char);
         when 'S' => Typ := (Kind => Short);
         when 'V' => Typ := (Kind => Void);
         when 'F' => Typ := (Kind => Flowt);

         when 'T' =>

            Get_Spec;

            Typ := (
               Kind        => Struct,
               Struct_Name => Value (Spec_Field, Line));

         when 'X' => Typ := (Kind => Sbit);

         when 'B' =>

            Get_Field (
               Field => Bit_Pos_Field,
               From  => Line,
               Stop  => '$',
               Skip  => True);

            Get_Spec;

            Typ := (
               Kind      => Bit_Field,
               Start_Bit => Number (Bit_Pos_Field, Line),
               Bits      => Number (Spec_Field, Line));

         when others =>

            Unknown_Type;

         end case;

      when others =>

         Unknown_Type;

      end case;

   end Parse_Type_Layer;


   procedure Parse_Type_Def (
      Line : in out Line_T;
      Top  : in     Type_Chain_T;
      Typ  :    out Type_Def_T)
   --
   -- Parses a type definition enclosed in '(' ')'.
   -- The leading '(' is already consumed and we consume the trailing ')'.
   -- The Top parameter defines the outermost layers of the type chain.
   --
   is

      Size_Field : Field_T;
      -- The size field.

      Chain : Type_Chain_T (1 .. Top'Length + Left (Line) / 3 + 1);
      -- Each layer in the type chain takes at least 3 characters
      -- of the line, counting the separator character. This is
      -- an over-estimate of the maximum number of layers.

      Last : Natural := Top'Length;
      -- The last Chain layer defined.
      -- The initial value corresponds to the Top layers.

      Sign : Sign_T;
      -- The signedness.

   begin

      -- The size:

      Skip ('{', Line);

      Get_Field (
         Field => Size_Field,
         From  => Line,
         Stop  => '}',
         Skip  => True);

      -- The type layers:

      Chain(1 .. Last) := Top;

      loop

         Last := Last + 1;

         Parse_Type_Layer (Line, Chain(Last));

         exit when Here (':', Line);

         Skip (',', Line);

      end loop;

      -- The sign:

      Skip (':', Line);

      case Current_Char (Line) is
      when 'U'    => Sign := Unsigned;
      when 'S'    => Sign := Signed;
      when others => Error ("Unknown signedness code", Line);
      end case;

      Next (Line);

      Skip (')', Line);

      Typ := (
         Size  => Number (Size_Field, Line),
         Chain => new Type_Chain_T'(Chain(1 .. Last)),
         Sign  => Sign);

   end Parse_Type_Def;


   procedure Parse_Register_List (
      Line : in out Line_T;
      List :    out Reg_List_Ref)
   --
   -- Parses a register list enclosed in '[' ']'.
   -- The leading '[' is already consumed and we consume the trailing ']'.
   --
   is

      Regs : Reg_List_T (1 .. Left (Line) / 2 + 1);
      -- Each register in the list takes at least 2 characters.

      Last : Natural := 0;
      -- The last Regs defined.

      Name : Field_T;
      -- The register name field.

   begin

      while not Here (']', Line) loop

         Get_Field (
            Field => Name,
            From  => Line,
            Stop1 => ',',
            Stop2 => ']',
            Skip  => False);

         Last := Last + 1;

         Regs(Last) := String_Pool.To_Item (Value (Name, Line));

         if Here (',', Line) then

            Skip (',', Line);

         end if;

      end loop;

      Skip (']', Line);

      List := new Reg_List_T'(Regs(1 .. Last));

   end Parse_Register_List;


   Top_Func_Layer : constant Type_Layer_T := (
      Kind         => Func,
      Is_Interrupt => False,
      Interrupt    => 0,
      Reg_Bank     => 0);
   --
   -- For use as the top type layer in function Symbols.
   -- All components except Kind are dummies and will be replaced
   -- in real function Symbols.


   procedure Parse (
      File_Name    : in     String;
      Trace        : in     Boolean := False;
      Action       : in out Action_T'Class;
      Symbol_Table : in     Symbols.Symbol_Table_T;
      Found        :    out Boolean)
   is

      File : Ada.Text_IO.File_Type;
      -- The CDB file.

      Line : Line_T;
      -- A line from the File.

      File_Mark : Output.Nest_Mark_T;
      -- Output context showing the File_Name.

      Line_Mark : Output.Nest_Mark_T;
      -- Output context showing the File_Name and a line number.


      procedure Parse_Symbol_Record (Is_Func : in Boolean)
      --
      -- The Line is a compiler symbol record "S:" or a function
      -- record "F:" and what is Left. The parameter Is_Func shows
      -- which kind of record Line is.
      --
      -- In case of success the symbol is entered in the CDB symbol-
      -- table (note, not in the Symbol_Table).
      --
      is

         Scope_Mark : Character;
         -- The character that shows the scope-kind.

         Space_Mark : Character;
         -- The character that shows the address space.

         On_Stack : Boolean;
         -- Whether the symbol represents a stack cell.

         Regs : Reg_List_Ref;
         -- A possible register list.

         Scope_Field    : Field_T;
         Name_Field     : Field_T;
         Level_Field    : Field_T;
         Block_Field    : Field_T;
         Offset_Field   : Field_T;
         IT_Num_Field   : Field_T;
         Reg_Bank_Field : Field_T;
         -- Some fields of the CDB line.

         Scope : Scope_T;
         -- The scope of the symbol.

         Typ : Type_Def_T;
         -- The type of the symbol.

         Loc : Location_T;
         -- The location of the symbol.

         Symbol : Symbol_T;
         -- The symbol to be defined.


      begin  -- Parse_Symbol_Record

         -- The scope and name:

         Scope_Mark := Current_Char (Line);

         Next (Line);

         case Scope_Mark is

         when 'G' =>

            Scope := (Kind => Global);

            Skip ('$', Line);

         when 'F' =>

            Get_Field (
               Field => Scope_Field,
               From  => Line,
               Stop  => '$',
               Skip  => True);

            Scope := (
               Kind      => SDCC.File,
               File_Name => Value (Scope_Field, Line));

         when 'L' =>

            Get_Field (
               Field => Scope_Field,
               From  => Line,
               Stop  => '$',
               Skip  => True);

            Scope := (
               Kind      => Local,
               Func_Name => Value (Scope_Field, Line),
               Level     => 1,
               Block     => 1);
            -- Level and Block to be filled in below.

         when others =>

            Error ("Unknown scope code '" & Scope_Mark & ''', Line);

         end case;

         -- Symbol name:

         Get_Field (
            Field => Name_Field,
            From  => Line,
            Stop  => '$',
            Skip  => True);

         -- Level and Block (always present but useful only
         -- for Local symbols):

         Get_Field (
            Field => Level_Field,
            From  => Line,
            Stop  => '$',
            Skip  => True);

         Get_Field (
            Field => Block_Field,
            From  => Line,
            Stop  => '(',
            Skip  => True);

         -- Type:

         if Is_Func then
            -- Function symbols get a Func top layer.

            Parse_Type_Def (
               Line => Line,
               Top  => (1 => Top_Func_Layer),
               Typ  => Typ);

         else
            -- Data symbols get just the type-chain specified.

            Parse_Type_Def (
               Line => Line,
               Top  => No_Types,
               Typ  => Typ);

         end if;

         -- Address space:

         Skip (',', Line);

         Space_Mark := Current_Char (Line);

         Next (Line);

         -- Stack stuff:

         Skip (',', Line);

         case Current_Char (Line) is
         when '0'    => On_Stack := False;
         when '1'    => On_Stack := True;
         when others => Error ("Unknown OnStack code", Line);
         end case;

         Next (Line);

         Skip (',', Line);

         Get_Field (
            Field => Offset_Field,
            From  => Line, 
            Stop  => ',',
            Skip  => False);

         if On_Stack /= (Space_Mark = 'A' or Space_Mark = 'B') then
            -- Conflict between On_Stack and Space_Mark.

            Error ("AddressSpace conflicts with OnStack", Line);

         end if;

         -- The rest is different for Symbol and Func:

         if Is_Func then
            -- This is a Function line.

            -- Check that it is in the Code address space:

            if not (   Space_Mark = 'C'
                    or Space_Mark = 'D'
                    or Space_Mark = 'Z')
            then

               Error ("Function has non-code AddressSpace", Line);

            end if;

            -- Interrupt and register-bank properties:

            Skip (',', Line);

            declare

               Top : Type_Layer_T renames Typ.Chain(Typ.Chain'First);
               -- The outermost Func layer. Here we set its interrupt
               -- and register-bank components.

            begin

               case Current_Char (Line) is
               when '0'    => Top.Is_Interrupt := False;
               when '1'    => Top.Is_Interrupt := True;
               when others => Error ("Unknown Is_Interrupt code", Line);
               end case;

               Next (Line); Skip (',', Line);

               Get_Field (
                  Field => IT_Num_Field,
                  From  => Line,
                  Stop  => ',',
                  Skip  => True);

               Top.Interrupt := Number (IT_Num_Field, Line);

               Get_Rest (
                  Field => Reg_Bank_Field,
                  From  => Line);

               Top.Reg_Bank := Number (Reg_Bank_Field, Line);

            end;

         else
            -- This is a Symbol (data variable) line.

            -- Possible register list:

            if Space_Mark = 'R' then

               Skip (',', Line);
               Skip ('[', Line);

               Parse_Register_List (
                  Line => Line,
                  List => Regs);

            end if;

         end if;

         -- That's all folks:

         if not Nothing_Left (Line) then

            Error ("Expected end of line", Line);

         end if;

         -- Construct the location:

         case Space_Mark is

         when 'A' =>

            Loc := (
               Space  => Ext_Stack,
               Offset => Frame_Offset (Offset_Field, Line));

         when 'B' =>

            Loc := (
               Space  => Int_Stack,
               Offset => Frame_Offset (Offset_Field, Line));

         when 'C' => Loc := (Space => Code);
         when 'D' => Loc := (Space => Code_Static);
         when 'E' => Loc := (Space => Int_RAM_Lower_128);
         when 'F' => Loc := (Space => Ext_RAM);
         when 'G' => Loc := (Space => Int_RAM);
         when 'H' => Loc := (Space => Bit);
         when 'I' => Loc := (Space => SFR);
         when 'J' => Loc := (Space => SBit);

         when 'R' =>

            Loc := (
               Space     => Register,
               Registers => Regs);

         when 'Z' =>

            if Is_Func then

               Loc := (Space => Code);

            else

               Loc := (Space => Undefined);

            end if;

         when others =>

            Error ("Unknown AddressSpace code '" & Space_Mark & ''', Line);

         end case;

         -- Put it all together:

         Symbol := (
            Name     => Value (Name_Field, Line),
            Scope    => Scope,
            Typ      => Typ,
            Location => Loc);

         -- Table the symbol:

         Insert (
            Symbol => Symbol,
            Into   => Action.Symbols);

         -- If the data symbol is located in registers or in the stack,
         -- we can enter it directly in the Symbol_Table:

         case Loc.Space is

         when Stack_Space_T =>

            Stack_Symbol (
               Name         => String_Pool.To_String (Symbol.Name),
               Scope        => Symbol.Scope,
               Typ          => Symbol.Typ,
               Space        => Loc.Space,
               Offset       => Loc.Offset,
               Action       => Action_T'Class (Action),
               Symbol_Table => Symbol_Table);

         when Register =>

            Register_Symbol (
               Name         => String_Pool.To_String (Symbol.Name),
               Scope        => Symbol.Scope,
               Typ          => Symbol.Typ,
               Registers    => Loc.Registers.all,
               Action       => Action_T'Class (Action),
               Symbol_Table => Symbol_Table);

         when Memory_Space_T
            | Undefined =>
            -- Wait for a Linker record.

            null;

         end case;

      exception

      when Duplicate_Symbol =>

         Output.Note (
              "CDB symbol duplicated"
            & Output.Field_Separator
            & Line.Text(1 .. Line.Last));

      end Parse_Symbol_Record;


      procedure Parse_Linker_Record
      --
      -- The Line is a linker record "L:" and what is Left.
      --
      is

         Is_End_Address : Boolean := Here ('X', Line);
         -- Whether this marks the end address.

         Linker_Type : Character;
         -- The linker record type.

         Scope_Field       : Field_T;
         Name_Field        : Field_T;
         Line_Number_Field : Field_T;
         Address_Field     : Field_T;
         Level_Field       : Field_T;
         Block_Field       : Field_T;
         -- Some fields of the CDB line.


         procedure Get_Level_Block_Address
         is
         begin

           Get_Field (
              Field => Level_Field,
              From  => Line,
              Stop  => '$',
              Skip  => True);

           Get_Field (
              Field => Block_Field,
              From  => Line,
              Stop  => ':',
              Skip  => True);

           Get_Rest (
              From  => Line,
              Field => Address_Field);

         end Get_Level_Block_Address;


      begin  -- Parse_Linker_Record

         if Is_End_Address then

            Next (Line);

         end if;

         Linker_Type := Current_Char (Line);

         Next (Line);

         case Linker_Type is

         when 'G' =>

            Skip ('$', Line);

            Get_Field (
               Field => Name_Field,
               From  => Line,
               Stop  => '$',
               Skip  => True);

            Get_Level_Block_Address;

            if not Is_End_Address then

               Memory_Symbol (
                  Name         => Value (Name_Field, Line),
                  Scope        => (Kind => Global),
                  Address      => Value (Address_Field, Line),
                  Action       => Action_T'Class (Action),
                  Symbol_Table => Symbol_Table);

            end if;

         when 'F' =>

            Get_Field (
               Field => Scope_Field,
               From  => Line,
               Stop  => '$',
               Skip  => True);

            Get_Field (
               Field => Name_Field,
               From  => Line,
               Stop  => '$',
               Skip  => True);

            Get_Level_Block_Address;

            if not Is_End_Address then

               Memory_Symbol (
                  Name  => Value (Name_Field, Line),
                  Scope => (
                     Kind      => SDCC.File,
                     File_Name => Value (Scope_Field, Line)),
                  Address      => Value (Address_Field, Line),
                  Action       => Action_T'Class (Action),
                  Symbol_Table => Symbol_Table);

            end if;

         when 'L' =>

            Get_Field (
               Field => Scope_Field,
               From  => Line,
               Stop  => '$',
               Skip  => True);

            Get_Field (
               Field => Name_Field,
               From  => Line,
               Stop  => '$',
               Skip  => True);

            Get_Level_Block_Address;

            if not Is_End_Address then

               Memory_Symbol (
                  Name  => Value (Name_Field, Line),
                  Scope => (
                     Kind      => Local,
                     Func_Name => Value (Scope_Field, Line),
                     Level     => Number (Level_Field, Line),
                     Block     => Number (Block_Field, Line)),
                  Address      => Value (Address_Field, Line),
                  Action       => Action_T'Class (Action),
                  Symbol_Table => Symbol_Table);

            end if;

         when 'A' =>
            -- A source-line-address record from the assembler.

            if Opt.Assembler_Lines then

               Skip ('$', Line);

               Get_Field (
                  Field => Name_Field,
                  From  => Line,
                  Stop  => '$',
                  Skip  => True);

               Get_Field (
                  Field => Line_Number_Field,
                  From  => Line,
                  Stop  => ':',
                  Skip  => True);

               Get_Rest (
                  From  => Line,
                  Field => Address_Field);

               Assembler_Line (
                  File         => Value   (Name_Field       , Line) & ".asm",
                  Line         => Number  (Line_Number_Field, Line),
                  Address      => Address (Address_Field    , Line),
                  Action       => Action_T'Class (Action),
                  Symbol_Table => Symbol_Table);

            end if;

         when 'C' =>
            -- A source-line-address record from the 'C' compiler.

            Skip ('$', Line);

            Get_Field (
               Field => Name_Field,
               From  => Line,
               Stop  => '$',
               Skip  => True);

            Get_Field (
               Field => Line_Number_Field,
               From  => Line,
               Stop  => '$',
               Skip  => True);

            Get_Level_Block_Address;

            C_Line (
               File         => Value   (Name_Field       , Line),
               Line         => Number  (Line_Number_Field, Line),
               Address      => Address (Address_Field    , Line),
               Level        => Number  (Level_Field      , Line),
               Block        => Number  (Block_Field      , Line),
               Action       => Action_T'Class (Action),
               Symbol_Table => Symbol_Table);

         when others =>

            Error ("Unknown Linker record code '" & Linker_Type & ''', Line);

         end case;

      end Parse_Linker_Record;


      procedure Parse_Line
      --
      -- Parses the Line.
      --
      is

         Rec_Type : Character;
         -- The main type of the Line = Line.Text(1).

      begin

         Rec_Type := Current_Char (Line);

         Next (Line);

         Skip (':', Line);

         case Rec_Type is

         when 'M' =>

            Output.Note (
                 "CDB module"
               & Output.Field_Separator
               & Line.Text(3 .. Line.Last));

         when 'F' =>

            Parse_Symbol_Record (Is_Func => True);

         when 'S' =>

            Parse_Symbol_Record (Is_Func => False);

         when 'T' =>

            null;  -- TBA Type record

         when 'L' =>

            Parse_Linker_Record;

         when others =>

            Error ("Unknown record-type code '" & Rec_Type & ''', Line);

         end case;

      exception

      when Syntax_Error
         | Constraint_Error =>

         Output.Error (
              "CDB line not understood"
            & Output.Field_Separator
            & Line.Text(1 .. Line.Last));

      when End_Line_Error =>

         Output.Error (
              "CDB line ends short"
            & Output.Field_Separator
            & Line.Text(1 .. Line.Last));

      end Parse_Line;


   begin  -- Parse

      File_Mark := Output.Nest (Output.Locus (Source_File => File_Name));

      Found := False;

      if Opt.Trace_Loading then

         Output.Trace ("Loading CDB file.");

      end if;

      begin

         Ada.Text_IO.Open (
            File => File,
            Mode => Ada.Text_IO.In_File,
            Name => File_Name);

         loop

            Get (File, Line);
            -- May propagate End_Error.

            Line_Mark := Output.Nest (
               Output.Locus (
                  Statements =>
                     Output."+" (Output.Locus (
                          Source_File => File_Name,
                          Line_Number => Output.Line_Number_T (Line.Num)))));

            Parse_Line;

            Output.Unnest (Line_Mark);

         end loop;

      exception

      when Ada.Text_IO.Name_Error =>

         Output.Error (
              "CDB file not found"
            & Output.Field_Separator
            & File_Name);

      when Ada.Text_IO.Use_Error =>

         Output.Error (
              "CDB file not readable"
            & Output.Field_Separator
            & File_Name);

      when Ada.Text_IO.End_Error =>
         -- Normal termination.

         if Opt.Trace_Loading then

            Output.Trace ("CDB file ends.");

         end if;

         Found := True;

      end;

      if Ada.Text_IO.Is_Open (File) then

         Ada.Text_IO.Close (File);

      end if;

      Symbols.Discard (Action.Scope);

      Output.Unnest (File_Mark);

   end Parse;


end Formats.SDCC.CDB;
