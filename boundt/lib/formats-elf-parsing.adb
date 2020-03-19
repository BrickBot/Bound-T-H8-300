-- Formats.ELF.Parsing (body)
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
-- $Revision: 1.11 $
-- $Date: 2015/10/24 20:53:53 $
--
-- $Log: formats-elf-parsing.adb,v $
-- Revision 1.11  2015/10/24 20:53:53  niklas
-- Moved to free licence.
--
-- Revision 1.10  2013/12/13 12:05:59  niklas
-- BT-CH-0263: Fix Formats.ELF.Parsing to redispatch on Action.
--
-- Revision 1.9  2010-02-20 07:50:32  niklas
-- Extended Parse_Symbols.Process to use Action_T.Function_Kinds.
--
-- Revision 1.8  2010-01-06 14:58:46  niklas
-- Changed Function_Symbol to emit an (optional) Warning instead of
-- an Error when the address is out of range.
--
-- Revision 1.7  2010-01-06 13:30:47  niklas
-- Extended Function_Symbol to check that the function's address
-- is in range for Processor.Code_Address_T.
--
-- Revision 1.6  2009-01-18 08:35:34  niklas
-- Removed unused context clause.
--
-- Revision 1.5  2008/10/28 10:15:45  niklas
-- Changed all Notes to Traces, optional on Opt.Trace_Loading.
-- Added Traces at the start and end of ELF symbol parsing.
--
-- Revision 1.4  2006/04/13 19:16:47  niklas
-- Added choice of symbols based on global or non-global
-- binding, by the component Action_T.Take_Bindings and
-- the function Is_Included.
-- Corrected Function_Symbol to obey Action.Take_Subprograms.
-- Corrected Object_Symbol and Other_Symbol to obey Action.Take_Cells
-- and modified them to emit Notes only if they would otherwise have
-- connected the symbol to a cell.
--
-- Revision 1.3  2006/04/12 19:49:55  niklas
-- Added the "out" parameter Parse_Symbols.Found to show if
-- an ELF symbol table was found.
--
-- Revision 1.2  2005/02/25 20:58:16  niklas
-- Changed all Symbol_Table_T parameters from "in out" to "in" as
-- befits a reference type. Related to BT-CH-0004.
--
-- Revision 1.1  2004/04/24 18:04:32  niklas
-- First version.
--


with Formats.ELF.Opt;
with Formats.ELF.Text;
with Output;


package body Formats.ELF.Parsing is


   function Is_Included (
      Binding : Symbol_Binding_T;
      Within  : Bindings_T)
   return Boolean
   is
   begin

      if Within = Any then

         return True;

      elsif Binding = Stb_Global then

         return Within = Global;

      else

         return Within = Non_Global;

      end if;

   end Is_Included;


   procedure Source_File (
      File         : in     Symbol_Denotation_T;
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
      Action       : in out Action_T;
      Symbol_Table : in     Symbols.Symbol_Table_T)
   is

      Fun_Address : Processor.Code_Address_T;
      -- The entry address of the function, if in range.

      Address_Valid : Boolean := False;
      -- Whether Fun_Address is valid.

   begin

      case Fun.Space is

      when Code =>
         -- The symbol refers to code instructions.

         if Symbols.Depth (Action.Scope) > 1 then
            -- Pop off the old subprogram name (There is no
            -- subprogram-level nesting in ELF symbol table)

            Symbols.Unnest (Action.Scope);

         end if;

         if  Action.Take_Subprograms
         and Is_Included (Fun.Binding, Action.Take_Bindings)
         then

            begin

               Fun_Address := Processor.Code_Address_T (Fun.Value);

               Address_Valid := True;

            exception

            when Constraint_Error =>

               if Opt.Warn then

                  Output.Warning (
                       "ELF symbol """
                     & Name
                     & """ ignored because its address is out of range"
                     & Output.Field_Separator
                     & Hex_Image (Fun.Value));

               end if;

            end;

            if Address_Valid then

               Symbols.Connect_Subprogram (
                  Scope   => Action.Scope,
                  Name    => Name,
                  Address => Fun_Address,
                  Within  => Symbol_Table);

            end if;

         end if;

         if Fun.Binding /= Stb_Global then
            -- Push on the new subprogram name:

            Symbols.Nest (
                Level => Name,
                Scope => Action.Scope);

         -- else
         --    The scope is not stored for global symbols, since
         --    after this there will only be other globals.

         end if;

      when others =>

         if Opt.Trace_Loading then

            Output.Trace (
                 "Function """
               & Name
               & """ not in code section, not recorded");

         end if;

      end case;

   end Function_Symbol;


   procedure Object_Symbol (
      Object       : in     Symbol_Denotation_T;
      Name         : in     String;
      Action       : in out Action_T;
      Symbol_Table : in     Symbols.Symbol_Table_T)
   is
   begin

      if Action.Take_Cells
      and then Is_Included (Object.Binding, Action.Take_Bindings)
      then

         if Object.Size > 0 and Object.Space = Data then

            Symbols.Connect_Cell (
               Scope  => Action.Scope,
               Name   => Name,
               Spec   => Object_Cell (Object, Action_T'Class (Action)),
               Within => Symbol_Table);

         elsif Opt.Trace_Loading then

            Output.Trace (
                 "Object """
               & Name
               & """ not in data section, or has size 0, not recorded");

         end if;

      end if;

   exception

   when No_Such_Cell =>

      if Opt.Trace_Loading then

         Output.Trace (
              "Object """
            & Name
            & """ ignored, no such cell");

      end if;

   end Object_Symbol;


   procedure Other_Symbol (
      Denotation   : in     Symbol_Denotation_T;
      Name         : in     String;
      Symbol       : in     Symbol_T;
      Action       : in out Action_T;
      Symbol_Table : in     Symbols.Symbol_Table_T)
   is
   begin

      if Action.Take_Cells
      and then (Denotation.Binding = Stb_Global
      and       Is_Included (Denotation.Binding, Action.Take_Bindings))
      then

         if Denotation.Space = Data then

            Symbols.Connect_Cell (
               Scope  => Action.Scope,
               Name   => Name,
               Spec   => Object_Cell (Denotation, Action_T'Class (Action)),
               Within => Symbol_Table);

         elsif Opt.Trace_Loading then

            Output.Trace (
                 "Global object """
               & Name
               & """ not in data section, not recorded");

         end if;

      end if;

   exception

   when No_Such_Cell =>

      if Opt.Trace_Loading then

         Output.Trace (
              "Global object """
            & Name
            & """ ignored, no such cell");

      end if;

   end Other_Symbol;


   procedure Parse_Symbols (
      From         : in     File_Ref;
      Trace        : in     Boolean := False;
      Action       : in out Action_T'Class;
      Symbol_Table : in     Symbols.Symbol_Table_T;
      Found        :    out Boolean)
   is


      procedure Process (
         Symbol : in Symbol_T;
         Index  : in Natural)
      --
      -- Processes the current symbol record.
      --
      is

         Denotation : Symbol_Denotation_T := (
            Kind    => Symbol_Kind (Symbol.St_Info),
            Binding => Symbol_Bind (Symbol.St_Info),
            Size    => Symbol.St_Size,
            Value   => Symbol.St_Value,
            Space   => Unknown);
         --
         -- What this symbol denotes on the machine level.
         -- The Space component will be filled in later.

         Name : constant String := Name_Of (Symbol, From.all);
         -- The name of the symbol.

      begin

         if Trace then

            Text.Put (
               Item   => Symbol,
               Index  => Index,
               Header => False,
               Name   => Name);

         end if;

         -- Find the Space:

         if       Symbol.St_Shndx in From.Code_Sections'Range
         and then From.Code_Sections(Symbol.St_Shndx)
         then

            Denotation.Space := Code;

         elsif    Symbol.St_Shndx in From.Data_Sections'Range
         and then From.Data_Sections(Symbol.St_Shndx)
         then

            Denotation.Space := Data;

         -- else
         --    Space remains Unknown.

         end if;

         -- Possibly return to global scope:

         if Denotation.Binding = Stb_Global then

            Symbols.Make_Global (Action.Scope);

         end if;

         -- Act on the symbol:

         if Action.Take_Bindings = Global
         and then not Is_Included (Denotation.Binding, Global)
         then
            -- Skip non-Global symbol if we are taking only Globals.

            null;

         elsif Denotation.Kind = Stt_File then

            Source_File (
               File         => Denotation,
               Name         => Name,
               Action       => Action,
               Symbol_Table => Symbol_Table);

         elsif Denotation.Kind = Stt_Func
         or   (Action.Function_Kinds(Denotation.Kind)
               and
               Denotation.Space = Code)
         then

            Function_Symbol (
               Fun          => Denotation,
               Name         => Name,
               Action       => Action,
               Symbol_Table => Symbol_Table);

         elsif Denotation.Kind = Stt_Object then

            Object_Symbol (
               Object       => Denotation,
               Name         => Name,
               Action       => Action,
               Symbol_Table => Symbol_Table);

         else

            Other_Symbol (
               Denotation   => Denotation,
               Name         => Name,
               Symbol       => Symbol,
               Action       => Action,
               Symbol_Table => Symbol_Table);

         end if;

      end Process;


      Syms : Symbols_Ref;
      -- The ELF symbol table.


   begin  -- Parse_Symbols

      -- Initialize the parsing state:

      Symbols.Make_Global (Action.Scope);

      Action.Parser := (null record);

      -- Check the existence of the symbol table:

      Found := From.Symbols /= Shn_Undef;

      if not Found then

         Output.Warning ("No ELF symbol table found.");

      else
         -- Process the ELF symbol records one by one:

         if Opt.Trace_Loading then

            Output.Trace ("Parsing ELF symbols:");

         end if;

         Syms := From.Sections(From.Symbols).Data.Symbols;

         for S in Syms'Range loop

            Process (
               Symbol => Syms(S),
               Index  => S);

         end loop;

         if Opt.Trace_Loading then

            Output.Trace ("End of ELF symbols.");

         end if;

     end if;

   end Parse_Symbols;


end Formats.ELF.Parsing;
