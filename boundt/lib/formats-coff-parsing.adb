-- Formats.COFF.Parsing (body)
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
-- $Revision: 1.12 $
-- $Date: 2015/10/24 20:53:51 $
--
-- $Log: formats-coff-parsing.adb,v $
-- Revision 1.12  2015/10/24 20:53:51  niklas
-- Moved to free licence.
--
-- Revision 1.11  2007-04-26 11:27:51  niklas
-- BT-CH-0059.
--
-- Revision 1.10  2007/03/06 11:09:33  niklas
-- Extended Parse_File to concatenate the strings from multiple
-- successive Aux_File_Name symbols, unless prevented by a
-- negated Opt.Catenate_File_Names.
--
-- Revision 1.9  2005/07/26 20:25:22  niklas
-- Extended Object_Kind to classify as a subprogram any symbol
-- with underived null type in a text segment (some GCC asm
-- library routines have these attributes).
-- Corrected Parse_Function to check for a null Symbol (no more
-- symbols) when it checks for Begin_Function.
--
-- Revision 1.8  2005/07/26 12:02:58  niklas
-- Fixed format of trace line.
--
-- Revision 1.7  2005/07/26 11:58:58  niklas
-- Added a variant of the Object_Kind function for symbols that
-- are not in a "real" section but have a section number -2 .. 0.
-- Added a trace for the classification of each parsed symbol.
--
-- Revision 1.6  2005/07/25 19:35:01  niklas
-- Corrected the function Object_Kind to let the type derivation
-- chain of a Subprogram symbol have more derivations after the
-- initial DT_Fcn.
--
-- Revision 1.5  2005/03/25 18:30:19  niklas
-- Modified the default implementation of Object_Kind to require,
-- for the Subprogram kind, a type derivation of just DT_Fcn, but
-- to allow the section to contain data as well as code (text).
-- Added trace output to Get_Line_Numbers (Section).
--
-- Revision 1.4  2005/03/24 22:24:03  niklas
-- Corrected line-numbers by subtracting one (apparently line-
-- number 1 means "the first line of the subprogram").
--
-- Revision 1.3  2005/03/24 22:09:52  niklas
-- Corrected line-number parsing to use the line-number of the
-- Aux_BF entry for the relevant subprogram as a base and adding
-- the line-numbers as offsets.
--
-- Revision 1.2  2005/03/16 21:27:43  niklas
-- Changed Symbol_Table parameters to "in" as befits a reference type.
--
-- Revision 1.1  2004/05/31 20:20:02  niklas
-- First version.
--


with Ada.Strings.Unbounded;
with Formats.COFF.Opt;
with Formats.COFF.Text;
with Storage;
with Output;


package body Formats.COFF.Parsing is

   use Ada.Strings.Unbounded;


   --    Overall COFF symbol-table syntax
   --
   -- The symbol table (= list of Symbol_T's) is assumed to obey the
   -- following overall syntax:
   --
   -- <symbol table> ::= <symbols from a file>+
   --
   -- <symbols from a file> ::= <file start> <global or function>+
   --
   -- <file start> ::= ".file" [ <aux file name> ]
   --
   -- <global or function> ::= <global symbol> | <function>
   --
   -- <global symbol> ::= <any Normal symbol that can be global>
   --
   -- <function> ::=
   --    <func symbol> [ <begin func> <local or block>+ <end func> ]
   --
   -- <func symbol> ::= <symbol of type DT_Fcn> [ <aux function> ]
   --
   -- <begin func> ::= ".bf" [ <aux begin function> ]
   --
   -- <local or block> ::= <local symbol> | <block>
   --
   -- <local symbol> ::= <any normal symbol that can be parameter or local>
   --
   -- <block> ::= <begin block> <local or block>+ <end block>
   --
   -- <begin block> ::= ".bb" [ <aux begin block> ]
   --
   -- <end block> ::= ".eb" [ <aux end block> ]
   --    
   -- <end func> ::= ".ef" [ <aux end function> ]
   --
   -- This syntax ignores many of the special symbols, the parser just
   -- skips them.
   --
   -- The parser seldom checks if a Normal symbol is suitable for its
   -- context. For example, it will accept in a global context a Normal
   -- symbol with storage class C_Arg, although this storage class is
   -- meant only for subprogram parameters. 


   function To_String (Name : in String) return Unbounded_String
   --
   -- The null-padded Name with null paddings removed.
   --
   is
   begin

      return To_Unbounded_String (To_String (Name));

   end To_String;


   procedure Get_Primary_Info (
      From       : in     Symbol_T;
      Within     : in     File_Ref;
      Name       :    out Unbounded_String;
      Denotation :    out Symbol_Denotation_T;
      Section    :    out Section_Number_T)
   --
   -- Extracts the Name, Denotation and Section number From a primary Symbol.
   --
   -- Precondition: Symbol.Kind in Primary_Symbol_T.
   --
   is
   begin

      case Primary_Symbol_T (From.Kind) is

      when Symbol_Entry =>

         Name := To_String (From.S1_Name);

         Denotation := (
            Sym_Type => From.S1_Symbol_Type,
            Class    => Storage_Class(From.S1_Storage_Class),
            Value    => From.S1_Value);

         Section := From.S1_Section_Number;

      when Symbol_Entry_2 =>

         Name := To_Unbounded_String (
            String_At (
              Offset => From.S2_Offset,
              Within => Within));

         Denotation := (
           Sym_Type => From.S2_Symbol_Type,
           Class    => Storage_Class(From.S2_Storage_Class),
           Value    => From.S2_Value);

         Section := From.S2_Section_Number;

      end case;

   end Get_Primary_Info;


   --
   --    Primitive operations for Action_T:
   --


   function Object_Kind (
      Object  : Symbol_Denotation_T;
      Section : Section_Header_T;
      Action  : Action_T)
   return Object_Kind_T
   is

      Fund : constant Fundamental_Type_T :=
         COFF.Fundamental_Type (Object.Sym_Type);
      -- The fundamental type of the symbol.

      Deriv : constant Type_Derivation_T :=
         COFF.Derivation (Object.Sym_Type);
      -- The derivation of the symbol's type.

      Is_Func : constant Boolean :=
         Deriv'Length > 0
         and then Deriv(Deriv'First) = DT_Fcn;
      -- Whether it is a function (subprogram).

   begin

      if  Subprogram_Class(Object.Class)
      and (
            (    Is_Func
             and not Section.Flags.Only_Data)
         or (    Deriv'Length = 0
             and Fund = T_Null
             and Section.Flags.Only_Text))
      then

         return Subprogram;

      elsif Data_Class(Object.Class)
      or  (    (not Is_Func)
           and (Object.Class = C_Ext or Object.Class = C_Stat))
      then

         return Data;

      elsif Object.Class = C_Label then

         return Label;

      else

         return Other;

      end if;

   end Object_Kind;


   function Object_Kind (
      Object  : Symbol_Denotation_T;
      Section : Section_Number_T;
      Action  : Action_T)
   return Object_Kind_T
   is
   begin

      if Data_Class(Object.Class) then

         return Data;

      else

         return Other;

      end if;

   end Object_Kind;


   procedure Source_File (
      Name         : in     String;
      Action       : in out Action_T;
      Symbol_Table : in     Symbols.Symbol_Table_T)
   is
   begin

      Symbols.Make_Global (Action.Scope);

      Symbols.Nest (
         Level => Name,
         Scope => Action.Scope);

   end Source_File;


   procedure Function_Symbol (
      Fun          : in     Symbol_Denotation_T;
      Name         : in     String;
      Aux_Func     : in     Symbol_Ref;
      Begins       : in     Boolean;
      Aux_Begin    : in     Symbol_Ref;
      Action       : in out Action_T;
      Symbol_Table : in     Symbols.Symbol_Table_T)
   is

      Func_Connection : Symbols.Connection_T;
      -- The function connection. Unused output.

   begin

      -- Enter the subprogram symbol in the current (containing) scope:

      if Action.Take_Subprograms then

         Symbols.Connect_Subprogram (
            Scope   => Action.Scope,
            Name    => Name,
            Address => Processor.Code_Address_T (Fun.Value),
            Within  => Symbol_Table,
            Giving  => Func_Connection);

      end if;

      if Aux_Begin /= null then
         -- The line number of the subprogram entry point is found here:

         Symbols.Connect_Line (
            Scope   => Action.Scope,
            Number  => Symbols.Line_Number_T (Aux_Begin.BF_Line_Number),
            Address => Processor.Code_Address_T (Fun.Value),
            Within  => Symbol_Table);

      end if;

      if Begins then
         -- Symbols for parameters and locals are listed.

         -- Add the subprogram that begins here to the scope:

         Symbols.Nest (
            Level => Name,
            Scope => Action.Scope);

      end if;

   end Function_Symbol;


   procedure End_Function (
      Fun          : in     Symbol_Denotation_T;
      Name         : in     String;
      Aux_End      : in     Symbol_Ref;
      Action       : in out Action_T;
      Symbol_Table : in     Symbols.Symbol_Table_T)
   is
   begin

      Symbols.Unnest (Action.Scope);

   end End_Function;


   procedure Begin_Block (
      Aux_Begin    : in     Symbol_Ref;
      Action       : in out Action_T;
      Symbol_Table : in     Symbols.Symbol_Table_T)
   is
   begin

      null;

   end Begin_Block;


   procedure End_Block (
      Aux_End      : in     Symbol_Ref;
      Action       : in out Action_T;
      Symbol_Table : in     Symbols.Symbol_Table_T)
   is
   begin

      null;

   end End_Block;


   procedure Label_Symbol (
      Object       : in     Symbol_Denotation_T;
      Name         : in     String;
      Action       : in out Action_T;
      Symbol_Table : in     Symbols.Symbol_Table_T)
   is
   begin

      Symbols.Connect_Label (
         Scope   => Action.Scope,
         Name    => Name,
         Address => Processor.Code_Address_T (Object.Value),
         Within  => Symbol_Table);

   end Label_Symbol;


   procedure Data_Symbol (
      Object       : in     Symbol_Denotation_T;
      Name         : in     String;
      Aux          : in     Symbol_Ref;
      Action       : in out Action_T;
      Symbol_Table : in     Symbols.Symbol_Table_T)
   is
   begin

      Symbols.Connect_Cell (
         Scope  => Action.Scope,
         Name   => Name,
         Spec   => Data_Cell (Object, Action_T'Class (Action)),
         Within => Symbol_Table);

   exception

      when No_Such_Cell =>

         Output.Note (
              "Object """
            & Name
            & """ ignored, no such cell");

   end Data_Symbol;


   procedure Other_Symbol (
      Denotation   : in     Symbol_Denotation_T;
      Name         : in     String;
      Symbol       : in     Symbol_T;
      Aux          : in     Symbol_Ref;
      Action       : in out Action_T;
      Symbol_Table : in     Symbols.Symbol_Table_T)
   is
   begin

      null;

   end Other_Symbol;


   procedure Special_Symbol (
      Nature       : in     Symbol_Nature_T;
      Symbol       : in     Symbol_T;
      Aux          : in     Symbol_Ref;
      Action       : in out Action_T;
      Symbol_Table : in     Symbols.Symbol_Table_T)
   is
   begin

      null;

   end Special_Symbol;


   --
   --    Parsing procedure:
   --


   Important : constant array (Symbol_Nature_T) of Boolean := (
      Normal_Nature_T => True,
      File_Name       => True,
      Begin_Function  => True,
      End_Function    => True,
      Begin_Block     => True,
      End_Block       => True,
      End_Struct      => False,
      Text_Section    => False,
      Data_Section    => False,
      Bss_Section     => False,
      Other_Section   => False,
      Target          => False,
      Text_End        => False,
      Data_End        => False,
      Bss_End         => False,
      Auxiliary       => True);
   --
   -- Whether a symbol of a given Nature is important to us, or not.
   -- In the future, more symbols may become important.


   function Number_Aux (Symbol : Symbol_Ref) return Byte_T
   --
   -- The number of auxiliary symbols for the Symbol, or zero if
   -- Symbol.Kind is not in Primary_Symbol_T.
   --
   is
   begin

      case Symbol.Kind is

      when Symbol_Entry =>

         return Symbol.S1_Number_Aux;

      when Symbol_Entry_2 =>

         return Symbol.S2_Number_Aux;

      when others =>

         return 0;

      end case;

   end Number_Aux;


   procedure Parse_Symbols (
      From         : in     File_Ref;
      Trace        : in     Boolean := False;
      Action       : in out Action_T'Class;
      Symbol_Table : in     Symbols.Symbol_Table_T)
   is

      -- The scope (source-file name and subprogram name) of line-number
      -- entries is discovered by referring to the scope of the subprogram
      -- that contains the group of line-number entries. The scopes are
      -- recorded here so that they can be easily accessed by the index of
      -- the symbol of the subprogram, which the COFF file supplies at the
      -- start of each line-number entry group:

      Subprogram_Scopes : array (From.Symbols'Range) of Symbols.Scope_T;
      -- The scope that was associated with the internal symbols of a
      -- subprogram symbol.
      --
      -- Subprogram_Scopes(S) is defined only if Scopes_Used(S) is True.

      Scopes_Used : array (From.Symbols'Range) of Boolean :=
         (others => False);
      -- Whether an element of Subprogram_Scopes is defined.

      Sym_Index : Natural;
      -- The index of the current symbol being scanned.
      -- The current symbol is From.Symbols(Sym_Index).

      Symbol : Symbol_Ref;
      -- The current symbol, From.Symbols(Sym_Index).
      -- Null if the end of the symbol list is reached.


      procedure Next (Number : in Byte_T := 1)
      --
      -- Advances to the next symbol in the list, or to an even
      -- later symbol in the list by skipping the given Number of
      -- symbols.
      --
      is
      begin

         if Number = 0 then
            -- Status quo.

            null;

         elsif Sym_Index + Natural (Number) <= From.Symbols'Last then

            Sym_Index := Sym_Index + Natural (Number);

            Symbol := From.Symbols(Sym_Index);

         else

            Symbol := null;

         end if;

      end Next;


      procedure Next_Important (Number : in Byte_T := 1)
      --
      -- Advances to the next symbol in the list, or to an even
      -- later symbol in the list by skipping the given Number of
      -- symbols, then finds the next Important symbol.
      --
      is

         Skip_Aux : Byte_T;
         -- The number of unimportant auxiliary symbols to skip.

      begin

         Next (Number);

         loop

            exit when Symbol = null;

            exit when Important(Nature (Symbol.all));

            Skip_Aux := Number_Aux (Symbol);

            Next (Number => 1);
            -- Skip the unimportant symbol.

            Next (Number => Skip_Aux);
            -- Skip any auxiliary symbols owned by the unimportant symbol.

         end loop;

      end Next_Important;


      procedure Parse_Auxiliaries (
         Number : out Byte_T;
         First  : out Symbol_Ref)
      --
      -- Assuming that the current Symbol is a primary symbol, this operation
      -- returns the Number of auxiliary symbols that the current Symbol
      -- owns, and a reference to the First such symbol, or null if there
      -- are none. The procedure also advances over the current Symbol and
      -- all auxiliary symbols that it owns.
      --
      is
      begin

         Number := Number_Aux (Symbol);

         Next;

         if Number > 0 then

            First := Symbol;

         else

            First := null;

         end if;

         Next_Important (Number);

      end Parse_Auxiliaries;


      procedure Find_Func_Line (
         Func  : in     Natural;
         Valid :    out Boolean;
         Line  :    out Ulong_T)
      --
      -- Find the first source-line number for the function symbol
      -- with the given Func index. This is the BF_Line_Number component
      -- of the Aux_BF auxiliary entry for the ".bf" entry that follows
      -- the function symbol.
      --
      -- If an Aux_BF record is found, Valid is returned as True and the
      -- line-number is returned in Line, otherwise Valid is returned
      -- as False.
      --
      is

         Func_Sym : Symbol_Ref renames From.Symbols(Func);
         -- The function symbol.

         BF_Index : Natural := Func + Natural (Number_Aux (Func_Sym)) + 1;
         -- The index of the ".bf" entry.

         BF_Sym : Symbol_Ref renames From.Symbols(BF_Index);
         -- The ".bf" entry.

         Aux : Symbol_Ref;
         -- One of the auxiliary entries for the ".bf".

      begin

         Valid := False;
         Line  := 0;
         -- Pessimistic initial values.

         if Nature (BF_Sym.all) /= Begin_Function then

            Output.Error (
                 "Function symbol at index"
               & Natural'Image (Func)
               & " is not followed by a .bf entry.");

         else

            for K in 1 .. Number_Aux (BF_Sym) loop

               Aux := From.Symbols(BF_Index + Natural(K));

               if Aux.Kind = Aux_BF then
                  -- Found it!

                  Line  := Ulong_T (Aux.BF_Line_Number);
                  Valid := True;

                  exit;

               end if;

            end loop;

            if not Valid then

               Output.Error (
                    "Function symbol at index"
                  & Natural'Image (Func)
                  & " has no auxiliary ""Begin Function"" entry.");

            end if;

         end if;

      end Find_Func_Line;


      procedure Parse_Normal_Symbol;
      --
      -- Parses a Normal primary symbol (Symbol_Entry or Symbol_Entry_2).
      -- See further description in the implementation, below.


      procedure Parse_Local_Or_Block;
      --
      -- Parses a Normal, primary (local) symbol, or a block.
      -- See description in the body, below.


      procedure Parse_Block
      --
      -- Parses a block, the symbols local to the block, and perhaps deeper
      -- nested blocks, according to the syntax
      --
      --    <block> ::= <begin block> <local or block>+ <end block>
      --
      --    <begin block> ::= ".bb" [ <aux begin block> ]
      --
      --    <end block> ::= ".eb" [ <aux end block> ]
      --
      -- On call, the current Symbol is the first one in this syntax,
      -- that is, a ".bb" special symbol.
      --
      -- On return, the current Symbol is the one after this syntax.
      --
      is

         Num_Begin_Aux : Byte_T;
         -- Number of auxiliary symbols owned by the <begin block>.

         Aux_Begin : Symbol_Ref;
         -- The (first) auxiliary symbol owned by the <begin block>, or
         -- null if it owns no auxiliary symbols.

         Num_End_Aux : Byte_T;
         -- The number of auxiliary symbols owned by the <end block>.

         Aux_End : Symbol_Ref := null;
         -- The (first) auxiliary symbol owned by <end block>, or null
         -- if <end block> owns no auxiliary symbols.

      begin

         -- <begin block>:

         Parse_Auxiliaries (
            Number => Num_Begin_Aux,
            First  => Aux_Begin);

         Begin_Block (
            Aux_Begin    => Aux_Begin,
            Action       => Action,
            Symbol_Table => Symbol_Table);

         -- <local or block>+ :

         loop

            exit when Symbol = null;

            exit when Nature (Symbol.all) = End_Block;

            Parse_Local_Or_Block;

         end loop;

         -- <end block>:

         if Symbol = null
         or else Nature(Symbol.all) /= End_Block
         then

            Output.Error ("COFF block has no "".eb"" symbol");

         else

            Parse_Auxiliaries (
               Number => Num_End_Aux,
               First  => Aux_End);

         end if;

         End_Block (
            Aux_End      => Aux_End,
            Action       => Action,
            Symbol_Table => Symbol_Table);

      end Parse_Block;


      procedure Parse_Local_Or_Block
      --
      -- Parses a local symbol or a block containing local symbols and
      -- perhaps deeper blocks, according to the syntax
      --
      --    <local or block> ::= <local symbol> | <block>
      --
      --    <local symbol> ::= <any normal symbol that can be parameter or local>
      --
      --    <block> ::= <begin block> ...
      --
      -- On call, the current Symbol is the first one in this syntax.
      -- On return, the current Symbol is the one after this syntax,
      -- usually an <end func>.
      --   
      is
      begin

         case Nature (Symbol.all) is

         when Begin_Block =>
            -- An inner block.

            Parse_Block;

         when others =>
            -- A normal symbol, we hope.

            Parse_Normal_Symbol;

         end case;

      end Parse_Local_Or_Block;


      procedure Parse_Function (
         Func_Index : in Natural;
         Denotation : in Symbol_Denotation_T;
         Name       : in String;
         Aux_Func   : in Symbol_Ref)
      --
      -- Parses a function symbol and possibly symbols contained in the
      -- function, according to the syntax
      --
      --    <function> ::=
      --       <func symbol> [ <begin func> <local or block>+ <end func> ]
      --
      --    <func symbol> ::= <symbol of type DT_Fcn> [ <aux function> ]
      --
      --    <begin func> ::= ".bf" [ <aux begin function> ]
      --
      --    <end func> ::= ".ef" [ <aux end function> ]
      --
      -- The parameters are as follows:
      --
      -- Func_Index
      --    The index in From.Symbols of the <func symbol>.
      -- Denotation
      --    The denotation of the <func symbol>.
      -- Name
      --    The identifier of the subprogram.
      -- Aux_Func
      --    The (first) auxiliary symbol owned by the <func symbol>, or
      --    null if it owns no auxiliary symbols.
      --
      -- On call, the current Symbol is the one after the <func symbol> and
      -- its auxiliary symbols, if any.
      --
      -- On return, the current Symbol is the next symbol after the
      -- above syntax, that is, after the <end func>.
      --
      is

         Begins : Boolean;
         -- Whether there is a <begin func> .. <end func> list.

         Num_Begin_Aux : Byte_T;
         -- Number of auxiliary symbols owned by the <begin func>.

         Aux_Begin : Symbol_Ref := null;
         -- The (first) auxiliary symbol owned by the <begin func>, or
         -- null if it owns no auxiliary symbols.

         Num_End_Aux : Byte_T;
         -- Number of auxiliary symbols owned by the <end func>.

         Aux_End : Symbol_Ref := null;
         -- The (first) auxiliary symbol owned by the <end func>, or
         -- null if it owns no auxiliary symbols.

      begin

         -- <func symbol>:

         -- Already parsed.

         -- <begin func> if any:

         Begins :=
            Symbol /= null and then Nature (Symbol.all) = Begin_Function;

         if Begins then
            -- Special symbol Begin_Func (".bf").

            Parse_Auxiliaries (
               Number => Num_Begin_Aux,
               First  => Aux_Begin);

         end if;

         -- Process the subprogram itself:

         Function_Symbol (
            Fun          => Denotation,
            Name         => Name,
            Aux_Func     => Aux_Func,
            Begins       => Begins,
            Aux_Begin    => Aux_Begin,
            Action       => Action,
            Symbol_Table => Symbol_Table);

         if Begins then
            -- There are symbols for parameters and/or locals.

            -- Remember the scope (needed by line numbers):

            Subprogram_Scopes(Func_Index) := Symbols.To_Scope (Action.Scope);

            Scopes_Used(Func_Index) := True;

            -- <local or block>+

            loop

               exit when Symbol = null;

               exit when Nature(Symbol.all) = End_Function;

               Parse_Local_Or_Block;

            end loop;          

            -- <end func>:

            if Symbol = null
            or else Nature (Symbol.all) /= End_Function
            then

               Output.Error (
                   "COFF function has no "".ef"" symbol"
                  & Output.Field_Separator
                  & Name);

            else

               Parse_Auxiliaries (
                  Number => Num_End_Aux,
                  First  => Aux_End);

            end if;

            End_Function (
               Fun          => Denotation,
               Name         => Name,
               Aux_End      => Aux_End,
               Action       => Action,
               Symbol_Table => Symbol_Table);

         end if;

      end Parse_Function;


      procedure Parse_Normal_Symbol
      --
      -- Parses a Normal symbol and, for function symbols, the list of
      -- enclosed symbols, according to the syntax
      --
      --    <global or function> ::= <data or function>
      --
      --    <local symbol> := <data or function>
      --
      --    <data or function> ::= <data/other symbol> | <function>
      --
      --    <data/other symbol> ::= <any Normal symbol of Data or Other kind>
      --
      --    <function> ::= <func symbol> ...
      --
      --    <func symbol> ::= <any normal symbol of Subprogram kind>
      --
      -- The current Symbol is the first data or function symbol.
      -- Returns with the current Symbol that follows the above syntax.
      --
      is

         Main_Index : constant Natural := Sym_Index;
         -- The index of the current Symbol.

         Main : constant Symbol_T := Symbol.all;
         -- The current Symbol.

         Sym_Name : Unbounded_String;
         -- The name of the Symbol.

         Denotation : Symbol_Denotation_T;
         -- The denotation of the Symbol.

         Section : Section_Number_T;
         -- The number of the section that contains the referent.

         Num_Aux : Byte_T;
         -- The number of auxiliary symbols owned by the Main Symbol.

         First_Aux : Symbol_Ref;
         -- The (first) auxiliary symbol, or null if there are none.

         Kind : Object_Kind_T;
         -- The kind of the denoted object.

      begin

         case Nature (Main) is

         when Normal_Nature_T =>
            -- We have a symbol denotation of some sort.

            -- Get the symbol name and denotation:

            Get_Primary_Info (
               From       => Main,
               Within     => From,
               Name       => Sym_Name,
               Denotation => Denotation,
               Section    => Section);

            -- Parse the auxiliary symbols:

            Parse_Auxiliaries (
               Number => Num_Aux,
               First  => First_Aux);

            -- Find out what kind of object the symbol denotes:

            if Section in From.Sections'Range then

               Kind := Object_Kind (
                  Object  => Denotation,
                  Section => From.Sections(Section).Header,
                  Action  => Action);

            elsif Section in -2 .. 0 then

               Kind := Object_Kind (
                  Object  => Denotation,
                  Section => Section,
                  Action  => Action);

            else

               Output.Warning (
                    "Invalid COFF symbol section number"
                  & Output.Field_Separator
                  & To_String (Sym_Name)
                  & Output.Field_Separator
                  & Section_Number_T'Image (Section));

               Kind := Other;

            end if;

            if Trace then

               Output.Trace (
                    "COFF symbol """
                  & To_String (Sym_Name)
                  & """ in section "
                  & Section_Number_T'Image (Section)
                  & " classed as "
                  & Object_Kind_T'Image (Kind));

            end if;

            -- Handle the symbol according to its denoted kind:

            case Kind is

            when Subprogram =>

               Parse_Function (
                  Func_Index => Main_Index,
                  Denotation => Denotation,
                  Name       => To_String (Sym_Name),
                  Aux_Func   => First_Aux);

            when Label =>

               Label_Symbol (
                  Object       => Denotation,
                  Name         => To_String (Sym_Name),
                  Action       => Action,
                  Symbol_Table => Symbol_Table);

            when Data =>

               Data_Symbol (
                  Object       => Denotation,
                  Name         => To_String (Sym_Name),
                  Aux          => First_Aux,
                  Action       => Action,
                  Symbol_Table => Symbol_Table);

            when Other =>

               Other_Symbol (
                  Denotation   => Denotation,
                  Name         => To_String (Sym_Name),
                  Symbol       => Main,
                  Aux          => First_Aux,
                  Action       => Action,
                  Symbol_Table => Symbol_Table);

            end case;

         when Special_Nature_T =>

            -- Parse the auxiliary symbols:

            Parse_Auxiliaries (
               Number => Num_Aux,
               First  => First_Aux);

            -- Act on the special symbol:

            Special_Symbol (
               Nature       => Nature (Main),
               Symbol       => Main,
               Aux          => First_Aux,
               Action       => Action,
               Symbol_Table => Symbol_Table);

         when Auxiliary =>

            Output.Warning (
                 "Skipped misplaced COFF auxiliary symbol, "
               & Symbol_Kind_T'Image (Symbol.Kind));

            Next_Important;

         end case;

      end Parse_Normal_Symbol;


      procedure Parse_File
      --
      -- Parses the symbols from one source file, according to the
      -- syntax
      --
      --    <symbols from a file> ::= <file start> <global or function>+
      --
      --    <file start> ::= ".file" [ <aux file name>+ ]
      --
      -- On call, the current Symbol is a File_Name.
      -- On return, the current Symbol is the next File_Name, or null.
      --
      -- When there are several <aux file names>, the full source-file
      -- name is the concatenation of the strings from all <aux file names>,
      -- unless Opt.Catenate_File_Names is False in which case only the
      -- string from the first <aux file name> is used.
      --
      is

         Num_Aux : Byte_T;
         -- The number of auxiliary symbols after the File_Name,
         -- decremented as they are processed.

         Name : Unbounded_String := Null_Unbounded_String;
         -- The file-name, or null if there is no Aux_File.

      begin

         -- <file start> :

         Num_Aux := Number_Aux (Symbol);

         Next;

         -- Possible Aux File Names:

         if Num_Aux > 0 and Symbol.Kind = Aux_File_Name then
            -- There is at least one Aux_File_Name symbol.

            loop

               Append (Name, COFF.To_String (Symbol.File_Name));

               Num_Aux := Num_Aux - 1;

               Next;

               exit when Num_Aux = 0 or not Opt.Catenate_File_Names;

            end loop;

            if Opt.Trace_Loading then

               Output.Trace (
                    "Start of COFF source-file "
                  & To_String (Name));

            end if;

         else

            Output.Warning ("COFF "".file"" entry with no file-name.");

         end if;

         Source_File (
            Name         => To_String (Name),
            Action       => Action,
            Symbol_Table => Symbol_Table);

         -- Skip any left-over auxiliary symbols:

         Next_Important (Num_Aux);

         -- <global or function>+ :

         loop

            exit when Symbol = null;
            -- End of the symbol list.

            exit when Nature (Symbol.all) = File_Name;
            -- End of this file, start of next file.

            Parse_Normal_Symbol;

         end loop;

      end Parse_File;


      procedure Parse_All_Symbols
      --
      -- Parses the whole COFF symbol-table according to the syntax
      --
      --    <symbol table> ::= <symbols from a file>+
      --
      -- where <symbols from a file> starts with a ".file" special symbol.
      --
      is
      begin

         File_Loop :
         loop

            -- Find the next File_Name symbol:

            Skip_Loop :
            loop

               exit Skip_Loop when Nature (Symbol.all) = File_Name;

               -- A Symbol that does not fit in the syntax.

               Output.Warning (
                    "Skipping COFF symbol, Kind = "
                  & Symbol_Kind_T'Image (Symbol.Kind)
                  & ", Nature = "
                  & Symbol_Nature_T'Image (Nature (Symbol.all)));

               Next_Important;

               exit File_Loop when Symbol = null;

            end loop Skip_Loop;

            -- This is a File_Name.

            Parse_File;

            exit File_Loop when Symbol = null;
            -- No more symbols.

         end loop File_Loop;

      end Parse_All_Symbols;


      procedure Get_Line_Numbers (Section : Section_T)
      -- 
      -- Add line numbers of the given section to symbol table.
      --
      is

         Item : Line_Number_T;
         -- An item in the line-number table.

         Func_Index : Natural;
         -- The COFF symbol index of a line-number entry that
         -- is a subprogram reference.

         Subprogram_Scope : Symbols.Scope_T := Symbols.Global_Scope;
         -- Scope of each subprogram the line numbers belong to.

         Func_Line : Ulong_T;
         -- The line number of the function Func_Index.
         -- The Line_Number fields of the line-number entries are
         -- offsets relative to Func_Line.

         Func_Line_Known : Boolean := False;
         -- Whether Func_Line is defined.

      begin

         if Opt.Trace_Loading then

            Output.Trace (
                 "Loading COFF line-numbers from section "
               & To_String (Section.Header.Name));

         end if;

         -- The array of line numbers should start with a subprogram
         -- reference ( line number 0 ) but if it does not, global
         -- scope is included to the line numbers.

         for L in Section.Line_Numbers'Range loop

            Item := Section.Line_Numbers(L);

            -- If the line number is a subprogram reference, the scope
            -- of the subprogram should have been saved in 
            -- Subprogram_Scope(Line_Address), which is indicated by  
            -- Scopes_Used(Line_Address) = True. 

            if Item.Line_Number = 0 then

               Func_Index := Natural (Item.Line_Address);
               -- This should refer to the subprogram symbol.

               if       Func_Index in Scopes_Used'Range
               and then Scopes_Used(Func_Index)
               then 

                  Subprogram_Scope := Subprogram_Scopes (Func_Index);

                  if Opt.Trace_Loading then

                     Output.Trace (
                         "Loading COFF line-numbers for symbol index"
                        & Natural'Image (Func_Index)
                        & ", scope "
                        & Symbols.Image (Subprogram_Scope));

                  end if; 

               else

                  Output.Warning (Text =>
                       "COFF line-number table refers to symbol index"
                     & Natural'Image (Func_Index)
                     & " which has no subprogram scope.");

                  Subprogram_Scope := Symbols.Global_Scope;

               end if;

               Find_Func_Line (
                  Func  => Func_Index,
                  Valid => Func_Line_Known,
                  Line  => Func_Line);

               if not Func_Line_Known then

                  Output.Error ("Line numbers unknown for this subprogram.");

               elsif Opt.Trace_Loading then

                  Output.Trace (
                       "This subprogram starts on line"
                     & Ulong_T'Image (Func_Line));

               end if;

            elsif Func_Line_Known then

               Symbols.Connect_Line (
                  Number  => Symbols.Line_Number_T (
                     Func_Line + Item.Line_Number - 1),
                  Scope   => Subprogram_Scope,
                  Address => Processor.Code_Address_T (Item.Line_Address),
                  Within  => Symbol_Table);

            end if;

         end loop;

      end Get_Line_Numbers;


   begin  -- Parse_Symbols

      if Opt.Trace_Loading then

         Output.Trace ("Starting to parse COFF symbols.");

      end if;

      -- Initialize the parsing state:

      Symbols.Make_Global (Action.Scope);

      -- Scan the symbols in order and parse them according to
      -- the overall symbol syntax:

      Sym_Index := From.Symbols'First;

      if Sym_Index <= From.Symbols'Last then
         -- There are some symbols.

         Symbol := From.Symbols(Sym_Index);

         Parse_All_Symbols;

      else

         Output.Warning ("The COFF symbol-table is empty.");

      end if;

      -- Fill the line-number table:

      if Opt.Trace_Loading then

         Output.Trace ("Starting to load COFF source-line numbers.");

      end if;

      for S in From.Sections'Range loop

         Get_Line_Numbers (From.Sections(S));

      end loop;

   end Parse_Symbols;


end Formats.COFF.Parsing;
