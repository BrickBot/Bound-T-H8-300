-- Formats.Stabs.Parsing (body)
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
-- $Revision: 1.7 $
-- $Date: 2015/10/24 20:53:54 $
--
-- $Log: formats-stabs-parsing.adb,v $
-- Revision 1.7  2015/10/24 20:53:54  niklas
-- Moved to free licence.
--
-- Revision 1.6  2007-08-18 20:31:13  niklas
-- Added procedure Nest_Scope for use also externally.
-- Added tracing of scope enter/leave, under Opt.Trace_Loading.
-- Added procedures Begin_Block and End_Block for anonymous blocks.
--
-- Revision 1.5  2007/06/28 18:49:19  niklas
-- Corrected spelling in a warning message.
--
-- Revision 1.4  2007/01/25 21:25:37  niklas
-- BT-CH-0043.
--
-- Revision 1.3  2006/04/12 19:52:59  niklas
-- Corrected procedure Source_Line to enter the line in the
-- line-number table only if the Sline_Base is defined.
-- Added Note to procedure Parse.
--
-- Revision 1.2  2005/02/25 20:59:06  niklas
-- Changed all Symbol_Table_T parameters from "in out" to "in" as
-- befits a reference type. Related to BT-CH-0004.
--
-- Revision 1.1  2004/04/24 17:26:03  niklas
-- First version.
--


with Formats.Stabs.Opt;
with Output;
with Unchecked_Deallocation;


package body Formats.Stabs.Parsing is


   use Twos_Complement_32;


   procedure Nest_Scope (
      Level  : in     String;
      Action : in out Action_T'Class)
   is
   begin

      -- Nest the new Level on the Scope:

      Symbols.Nest (
         Level => Level,
         Scope => Action.Scope);

      if Opt.Trace_Loading then

         Output.Trace (
              "Enter STABS scope"
            & Output.Field_Separator
            & Symbols.Image (Action.Scope));

      end if;

      -- Initialize the count of anonymous blocks for this level:

      Action.Parser.Next_Block_Number(Symbols.Depth (Action.Scope)) := 1;

   end Nest_Scope;


   procedure Trace_Unnest_Scope (Action : in Action_T'Class)
   --
   -- Optional tracing of scope unnesting.
   --
   is
   begin

      if Opt.Trace_Loading then

         Output.Trace (
              "Leave STABS scope"
            & Output.Field_Separator
            & Symbols.Image (Action.Scope));

      end if;

   end Trace_Unnest_Scope;


   procedure Trace_Make_Global (Action : in Action_T'Class)
   --
   -- Optional tracing of making the scope global.
   --
   is
   begin

      if       Opt.Trace_Loading
      and then Symbols.Depth (Action.Scope) > 0
      then

         Output.Trace ("Leaving all STABS scopes.");

      end if;

   end Trace_Make_Global;


   procedure Unchecked_Discard
   is new Unchecked_Deallocation (
      Name   => Table_Ref,
      Object => Table_T);


   procedure Discard (Item : in out Table_Ref)
   is
   begin

      if Formats.Stabs.Opt.Deallocate then

         Unchecked_Discard (Item);

      else

         Item := null;

      end if;

   end Discard;


   function To_Code_Address (Value : N_Value_T)
   return Processor.Code_Address_T
   is
   begin

      return Processor.Code_Address_T (abs Value);

   end To_Code_Address;


   function To_Code_Offset (Value : N_Value_T)
   return Processor.Code_Offset_T
   is
   begin

      return Processor.Code_Offset_T (abs Value);

   end To_Code_Offset;


   function Register_Cell (
      Rsym   : Line_Info_T;
      Action : Action_T)
   return Processor.Cell_Spec_T
   is
      C : Processor.Cell_Spec_T;
   begin

      raise No_Such_Cell;

      return C;

   end Register_Cell;


   function Local_Cell (
      Lsym   : Line_Info_T;
      Action : Action_T)
   return Processor.Cell_Spec_T
   is
      C : Processor.Cell_Spec_T;
   begin

      raise No_Such_Cell;

      return C;

   end Local_Cell;


   function Parameter_Cell (
      Psym   : Line_Info_T;
      Action : Action_T)
   return Processor.Cell_Spec_T
   is
      C : Processor.Cell_Spec_T;
   begin

      raise No_Such_Cell;

      return C;

   end Parameter_Cell;


   procedure Set_Sline_Base (
      To     : in     Processor.Code_Address_T;
      Within : in out Action_T)
   --
   -- Sets the Sline_Base.
   --
   is
   begin

      Within.Sline_Base         := To;
      Within.Sline_Base_Defined := True;

   end Set_Sline_Base;


   procedure Source_File (
      So           : in     Line_Info_T;
      Name         : in     String;
      Action       : in out Action_T;
      Symbol_Table : in     Symbols.Symbol_Table_T)
   is
   begin

      Trace_Make_Global (Action);

      Symbols.Make_Global (Action.Scope);

      if Name /= "" then

         Set_Sline_Base (
            To     => To_Code_Address (So.N_Value),
            Within => Action);

         Nest_Scope (
            Level  => Name,
            Action => Action);

      end if;

   end Source_File;


   procedure Source_File_L (
      Sol          : in     Line_Info_T;
      Name         : in     String;
      Action       : in out Action_T;
      Symbol_Table : in     Symbols.Symbol_Table_T)
   is
   begin

      Trace_Make_Global (Action);

      Symbols.Make_Global (Action.Scope);

      Nest_Scope (
         Level  => Name,
         Action => Action);

   end Source_File_L;


   procedure Function_Symbol (
      Fun          : in     Line_Info_T;
      Symbol       : in     String;
      Action       : in out Action_T;
      Symbol_Table : in     Symbols.Symbol_Table_T)
   is

      Value : constant Processor.Code_Address_T :=
         To_Code_Address (Fun.N_Value);
      -- The code address associated with the N_Fun line.

      Ident : constant String := Identifier (Symbol);
      -- The identifier part of the symbol string.

   begin

      Set_Sline_Base (
         To     => Value,
         Within => Action);

      if Symbols.Depth (Action.Scope) > 1 then
         -- Pop off the old subprogram name.
         -- TBM to check rather the type of the innermost level?

         Trace_Unnest_Scope (Action);

         Symbols.Unnest (Action.Scope);

      end if;

      if Ident /= "" then

         if Action.Take_Subprograms then

            Symbols.Connect_Subprogram (
               Scope   => Action.Scope,
               Name    => Ident,
               Address => Value,
               Within  => Symbol_Table);

         end if;

         Nest_Scope (
            Level  => Ident,
            Action => Action);

      end if;

   end Function_Symbol;


   procedure Source_Line (
      Sline        : in     Line_Info_T;
      Symbol       : in     String;
      Action       : in out Action_T;
      Symbol_Table : in     Symbols.Symbol_Table_T)
   is
      use type Processor.Code_Address_T;
   begin

      if Action.Take_Source_Lines then

         if Action.Sline_Base_Defined then

            Symbols.Connect_Line (
               Number  => Symbols.Line_Number_T (Sline.N_Desc),
               Scope   => Action.Scope,
               Address =>
                    Action.Sline_Base
                  + To_Code_Offset (Sline.N_Value),
               Within  => Symbol_Table);

         else

            Output.Warning (
               "STABS N_Sline record with no base address, ignored.");

         end if;

      end if;

   end Source_Line;


   procedure Register_Symbol (
      Rsym         : in     Line_Info_T;
      Symbol      : in     String;
      Action       : in out Action_T;
      Symbol_Table : in     Symbols.Symbol_Table_T)
   is
   begin

      if Action.Take_Cells then

         Symbols.Connect_Cell (
            Scope  => Action.Scope,
            Name   => Identifier (Symbol),
            Spec   => Register_Cell (Rsym, Action_T'Class (Action)),
            Within => Symbol_Table);

      end if;

   exception

      when No_Such_Cell =>

         Output.Note (
            "Rsym """ & Symbol & """ ignored, no such cell");

   end Register_Symbol;


   procedure Local_Symbol (
      Lsym         : in     Line_Info_T;
      Symbol       : in     String;
      Action       : in out Action_T;
      Symbol_Table : in     Symbols.Symbol_Table_T)
   is
   begin

      if Action.Take_Cells then

         Symbols.Connect_Cell (
            Scope  => Action.Scope,
            Name   => Identifier (Symbol),
            Spec   => Local_Cell (Lsym, Action_T'Class (Action)),
            Within => Symbol_Table);

      end if;

   exception

      when No_Such_Cell =>

         Output.Note (
            "Lsym """ & Symbol & """ ignored, no such cell");

   end Local_Symbol;


   procedure Parameter_Symbol (
      Psym         : in     Line_Info_T;
      Symbol       : in     String;
      Action       : in out Action_T;
      Symbol_Table : in     Symbols.Symbol_Table_T)
   is
   begin

      if Action.Take_Cells then

         Symbols.Connect_Cell (
            Scope  => Action.Scope,
            Name   => Identifier (Symbol),
            Spec   => Parameter_Cell (Psym, Action_T'Class (Action)),
            Within => Symbol_Table);

      end if;

   exception

      when No_Such_Cell =>

         Output.Note (
            "Psym """ & Symbol & """ ignored, no such cell");

   end Parameter_Symbol;


   procedure Global_Symbol (
      Gsym         : in     Line_Info_T;
      Symbol       : in     String;
      Action       : in out Action_T;
      Symbol_Table : in     Symbols.Symbol_Table_T)
   is
   begin

      null;

   end Global_Symbol;


   procedure Begin_Block (
      Lbrac        : in     Line_Info_T;
      Symbol       : in     String;
      Action       : in out Action_T;
      Symbol_Table : in     Symbols.Symbol_Table_T)
   is

      Outer : constant Natural := Symbols.Depth (Action.Scope);
      -- The depth of the containing scope level.

      Number : Positive renames Action.Parser.Next_Block_Number(Outer);
      -- The number for the new anonymous block.

   begin

      -- Open the anonymous block scope:

      Nest_Scope (
         Level  => '{' & Output.Image (Number),
         Action => Action);

      -- Count the anonymous blocks on the Outer level:

      Number := Number + 1;

   end Begin_Block;
      

   procedure End_Block (
      Rbrac        : in     Line_Info_T;
      Symbol       : in     String;
      Action       : in out Action_T;
      Symbol_Table : in     Symbols.Symbol_Table_T)
   is

      Level : constant String := Symbols.Deepest (Action.Scope);
      -- The name of the scope level to be closed.

   begin

      if Level'Length = 0 then

         Output.Warning ("STABS N_Rbrac closes nameless scope.");

      elsif Level(Level'First) /= '{' then

         Output.Warning (
              "STABS N_Rbrac closes scope """
            & Level
            & """.");

      end if;

      Trace_Unnest_Scope (Action);

      Symbols.Unnest (Action.Scope);

   end End_Block;


   procedure Other_Line (
      Line         : in     Line_T;
      Symbol       : in     String;
      Action       : in out Action_T;
      Symbol_Table : in     Symbols.Symbol_Table_T)
   is
   begin

      null;

   end Other_Line;


   procedure Parse (
      From         : in     IO.File_Type;
      Lines        : in     Segment_Desc_T;
      Strings      : in     Segment_Desc_T;
      Trace        : in     Boolean := False;
      Action       : in out Action_T'Class;
      Symbol_Table : in     Symbols.Symbol_Table_T)
   is

      Table : Table_Ref;
      -- The Stabs symbol-table, as loaded form the file.


      procedure Process (Line : in Line_T)
      --
      -- Processes one Stabs line.
      --
      is

         Sym : constant String := Symbol (Line, Table.all);
         -- The symbolic string associated with the Line.

         Info : constant Line_Info_T := (
            N_Other => Line.N_Other,
            N_Desc  => Line.N_Desc,
            N_Value => Line.N_Value);
         -- The info associated with the Line.

      begin

         case Line.N_Type is

            when N_So    => Source_File     (Info, Sym, Action, Symbol_Table);
            when N_Sol   => Source_File_L   (Info, Sym, Action, Symbol_Table);
            when N_Fun   => Function_Symbol (Info, Sym, Action, Symbol_Table);
            when N_Sline => Source_Line     (Info, Sym, Action, Symbol_Table);
            when N_Rsym  => Register_Symbol (Info, Sym, Action, Symbol_Table);
            when N_Lsym  => Local_Symbol    (Info, Sym, Action, Symbol_Table);
            when N_PSym  => Parameter_Symbol(Info, Sym, Action, Symbol_Table);
            when N_Gsym  => Global_Symbol   (Info, Sym, Action, Symbol_Table);
            when N_Lbrac => Begin_Block     (Info, Sym, Action, Symbol_Table);
            when N_Rbrac => End_Block       (Info, Sym, Action, Symbol_Table);
            when others  => Other_Line      (Line, Sym, Action, Symbol_Table);

         end case;

      end Process;


   begin  -- Parse

      Output.Note ("Parsing STABS symbols.");

      -- Initialize the parsing state:

      Action.Sline_Base_Defined := False;
      Action.Sline_Base         := Processor.Code_Address_T'First;

      Symbols.Make_Global (Action.Scope);

      -- Load the Stabs table from the file:

      Load (
         From    => From,
         Lines   => Lines,
         Strings => Strings,
         Trace   => Trace,
         Giving  => Table);

      -- Process the Stabs lines one by one:

      for L in Table.Lines'Range loop

         Process (Line => Table.Lines(L));

      end loop;

      -- Discard the Stabs table:

      Discard (Table);

   end Parse;


end Formats.Stabs.Parsing;
