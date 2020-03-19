-- Formats.AOMF_Keil.Parsing (body)
--
-- Author: Niklas Holsti, Tidorum Ltd, 2004.
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
-- $Log: formats-aomf_keil-parsing.adb,v $
-- Revision 1.12  2015/10/24 20:53:51  niklas
-- Moved to free licence.
--
-- Revision 1.11  2009-12-07 11:06:05  niklas
-- BT-CH-0184: Arithmetic.Value_T replaces Processor.Value_T.
--
-- Revision 1.10  2009-07-08 17:13:02  niklas
-- Corrected Load_Extended_Debug_Items to obey Opt.Define_Segment_Symbols,
-- thus (by default) not defining Segment_Symbols as Bound-T symbols.
--
-- Revision 1.9  2008-02-27 19:39:51  niklas
-- Added the option Define_Segment_Symbols (default False) to control
-- whether Segment_Symbols are entered in the Bound-T symbol table.
--
-- Revision 1.8  2007/09/27 07:06:43  niklas
-- Added "AOMF" to some warnings/errors, for TR-TN-AOMF-001.
--
-- Revision 1.7  2007/08/31 11:30:24  niklas
-- Applying Opt.Warn_Line_No_Sub.
--
-- Revision 1.6  2007/08/31 09:04:48  niklas
-- Extended Define_Symbol and Synthesize_Type to obey Opt.Trace_Symbols.
-- Extended Load_Extended_Debug_Items to synthesize types for symbols
-- that would be Untyped otherwise. (This happens if Keil C51 is not
-- given the OBJECTEXTEND directive in addition to DEBUG.)
--
-- Revision 1.5  2007/04/26 11:27:51  niklas
-- BT-CH-0059.
--
-- Revision 1.4  2006/04/28 09:15:29  niklas
-- Adapted to new Keil BL51 (V6.00) output format where the
-- Source_Name record seems to be missing. Added Parser components
-- to record the last source-file name, module name and procedure
-- name, for use in the source-file scope as available.
-- Updated to use mode "in" for Symbol_Table_T (reference semantics)
-- and to not erase the Symbol_Table in Parse_And_Load.
--
-- Revision 1.3  2005/01/02 22:38:01  niklas
-- Extended Define_Symbol to treat untyped or void Code symbols as
-- subprogram symbols.
-- Added record type-code to note re skipped or ignored record.
--
-- Revision 1.2  2005/01/01 20:24:41  niklas
-- Changed Skip_Wrong_Record to emit a warning instead of an error
-- and to show the hex code of the record instead of the mnemonic.
--
-- Revision 1.1  2004/10/10 10:03:29  niklas
-- First version.
--


with Formats.AOMF_Keil.Opt;
with Formats.AOMF_Keil.Text;
with Output;


package body Formats.AOMF_Keil.Parsing is


   --
   --    Operations on Parser_T
   --


   use type String_Pool.Item_T;


   function To_String (Item : String_Pool.Item_T) return String
   --
   -- The string, or a null string if No_Name.
   --
   is
   begin

      if Item = No_Name then

         return "";

      else

         return String_Pool.To_String (Item);

      end if;

   end To_String;


   function Source_File_Name (Parser : Parser_T) return String
   is
   begin

      return To_String (Parser.Source_Name);

   end Source_File_Name;


   function Module_Name (Parser : Parser_T) return String
   is
   begin

      return To_String (Parser.Module_Name);

   end Module_Name;


   function Procedure_Name (Parser : Parser_T) return String
   is
   begin

      return To_String (Parser.Procedure_Name);

   end Procedure_Name;


   --
   --    Parsing actions (overridable)
   --


   procedure Define_Variable (
      Symbol : in     String;
      Cell   : in     Processor.Cell_Spec_T;
      Action : in out Action_T;
      Into   : in     Symbols.Symbol_Table_T)
   is
   begin

      Symbols.Connect_Variable (
         Scope    => Action.Scope,
         Name     => Symbol,
         Location => Cell,
         Within   => Into);

   end Define_Variable;


   procedure Define_Subprogram (
      Symbol  : in     String;
      Address : in     Processor.Code_Address_T;
      Action  : in out Action_T;
      Into    : in     Symbols.Symbol_Table_T)
   is
   begin

      Symbols.Connect_Subprogram (
         Scope   => Action.Scope,
         Name    => Symbol,
         Address => Address,
         Within  => Into);

   end Define_Subprogram;


   procedure Define_Symbol (
      Symbol    : in     String;
      Seg_ID    : in     Octet_T;
      Storage   : in     Storage_T;
      Index     : in     Type_Index_T;
      Defining  : in     Compound_Type_T;
      Offset    : in     Word16_T;
      Action    : in out Action_T;
      Into      : in     Symbols.Symbol_Table_T)
   is

      type Species_T is (Variable, Subprogram, Label, Other);

      Species : Species_T := Other;

   begin

      -- Perhaps trace:

      if Opt.Trace_Symbols then

         Output.Trace (
              "AOMF symbol"""
            & Symbol
            & """, seg"   & Octet_T'Image (Seg_ID)
            & ", "        & Text.Image (Storage)
            & ", type "   & Text.Image (Index)
            & ", offset " & Hex_Image (Offset));

         Text.Put (Defining);

      end if;

      -- Decide the Species:

      if Defining.Kind = Funktion
      and (   Storage.Kind = Code
           or Storage.Kind = Code_Bank
           or Storage.Kind = Unknown)
      then

         Species := Subprogram;

      elsif Defining.Kind = Predefined then

         case Basic_Type (Defining.Predefined.Basic) is

         when Untyped | Void =>

            if Storage.Kind = Code then
               -- Here we have for example the Keil library routines.

               Species := Subprogram;

            else

               Species := Other;

            end if;

         when Bit | Char | UChar => Species := Variable;

         when Code_Label         => Species := Label;

         when others             => Species := Other;

         end case;

      end if;

      -- Act according to the Species:

      case Species is

      when Variable =>

         begin

            Define_Variable (
               Symbol => Symbol,
               Cell   => To_Cell_Spec (
                  Seg_ID   => Seg_ID,
                  Storage  => Storage,
                  Index    => Index,
                  Defining => Defining,
                  Offset   => Offset,
                  Action   => Action_T'Class (Action)),
               Action => Action_T'Class (Action),
               Into   => Into);

         exception

         when Not_Located =>
            -- No Cell for this variable.

            null;

         end;

      when Subprogram =>

         Define_Subprogram (
            Symbol  => Symbol,
            Address => To_Code_Address (
               Seg_ID  => Seg_ID,
               Address => Offset,
               Action  => Action_T'Class (Action)),
            Action => Action_T'Class (Action),
            Into   => Into);

      when Label =>

         Symbols.Connect_Label (
            Scope   => Action.Scope,
            Name    => Symbol,
            Address => To_Code_Address (
               Seg_ID  => Seg_ID,
               Address => Offset,
               Action  => Action_T'Class (Action)),
            Within  => Into);

      when Other =>

         null;

      end case;

   end Define_Symbol;


   --
   --    Parsing operations (class-wide)
   --
   -- The sequence of records in an AOMF file obeys the following overall
   -- syntax:
   --
   --    <AOMF file> = <module>
   --
   --    <module> = <header> <data or debug>* <Module End>
   --
   --    <header = [ <BL51 Bank Head> ] <Module Header>
   --
   --    <data or debug> = <Content> | <debug>
   --
   --    <debug> = <scope>
   --            | <Type Definition>
   --            | <Debug Items>
   --            | <Extended Debug Items>
   --
   --    <scope> = <Scope Definition> [ <Source Name> ]
   --
   -- The Scope Definition records occur in pairs: Begin_X and End_X.
   -- Each such pair brackets the Type Definitions, (Extended) Debug
   -- Items and Content for a part of the program (X = module, procedure
   -- or do-block). Such scopes can be nested. Typically there is a
   -- sequence of module scopes, each of which may contain procedure
   -- scopes, which may contain do-block scopes, which may contain nested
   -- do-block scopes. A module-scope typically corresponds to one 'C'
   -- language source file.
   --
   -- The syntax does not mention the records of type Segment Definition,
   -- Public Definition or External Definition, because they will be
   -- ignored where ever they occur.


   Block_Ender : constant array (Block_Beginner_T) of Block_Ender_T := (
      Begin_Module    => End_Module,
      Begin_Procedure => End_Procedure,
      Begin_Do        => End_Do);
   --
   -- The "end" Scope Definition Block Type for a given Block Type of
   -- a "begin" Scope Definition


   procedure Parse_And_Load (
      File         : in     IO.File_Type;
      Action       : in out Action_T'Class;
      Symbol_Table : in     Symbols.Symbol_Table_T)
   is
      use type IO.Count;

      Stream : constant IO.Stream_Access := IO.Stream (File);

      Rec_Count : Natural := 0;
      -- The number of AOMF records read.

      Rec : Record_Info_T;
      -- Information on the current record.
      -- This is used as a look-ahead in parsing.

      Line_Scope : Symbols.Scope_Stack_T;
      -- The scope for source-line-number items.
      -- Contains the source-file-name and the subprogram name.


      procedure Syntax_Error (Text : in String)
      --
      -- The current Rec.Kind should not be at this place in the
      -- AOMF file. Complain and abort.
      --
      is
      begin

         Output.Error (Text =>
              "AOMF "
            & Record_Kind_T'Image (Rec.Kind)
            & " record invalid or out of context. "
            & Text);

         raise Data_Error;

      end Syntax_Error;


      procedure Get_Record
      --
      -- Reads the info (Rec) for the next record, checks the check-sum
      -- of the current record and raises Data_Error if invalid.
      --
      is

         Valid_Sum : Boolean;
         -- Whether the record has as valid check-sum.

      begin

         if Rec_Count > 0 then
            -- We have read an earlier record but perhaps not all of
            -- it; especially perhaps not the check-sum octet.

            Skip_To_Next_Record (Rec, File);

         -- else
         --    The Stream is at the start of the file.

         end if;

         Read_Record_Info (File, Stream, Rec);

         Rec_Count := Rec_Count + 1;

         if Opt.Trace_Loading then

            Output.Trace (
                 "AOMF record #"
               & Output.Image (Rec_Count)
               & ' '
               & Record_Kind_T'Image (Rec.Kind)
               & Word16_T'Image (Rec.Length)
               & " octets.");

         end if;

         Check_Sum (Rec, File, Stream, Valid_Sum);

         if not Valid_Sum then

            Output.Error ("AOMF record has wrong check-sum.");

            raise Data_Error;

         end if;

      end Get_Record;


      procedure Skip_Nulls
      --
      -- Skips records that are allowed but ignored.
      --
      is
      begin

         while Rec.Kind = Unknown
            or Rec.Kind = Source_Browse_TBC
         loop

            Output.Note (
                 "Skipping AOMF record of unknown or ignored type ("
               & Hex_Image (Rec.Kind_Code)
               & " hex)");

            Skip_To_Next_Record (Rec, File);

            Get_Record;

         end loop;

      end Skip_Nulls;


      procedure Skip_Record
      --
      -- Skips the current record and any following "null" records,
      -- then reads Rec for next (non-null) record.
      --
      is
      begin

         Skip_To_Next_Record (Rec, File);

         Skip_Nulls;

      end Skip_Record;


      procedure Skip_Wrong_Record (Why : in String)
      --
      -- Reports the current record as "wrong" in some way, and skips it.
      -- Parsing is not aborted.
      --
      is
      begin

         Output.Warning (
              "AOMF record with code "
            & Hex_Image (Rec.Kind_Code)
            & " (hex) skipped; "
            & Why);

         Skip_Record;

      end Skip_Wrong_Record;


      procedure Load_Content
      --
      -- <Content>
      --
      is
         Content : Content_T;
      begin

         Read_Content (Stream, Rec.Length, Content);

         Load_Memory (
            Seg_ID => Content.Seg_ID,
            Offset => Content.Offset,
            Data   => Content.Data.all,
            Action => Action);

         Get_Record;

      end Load_Content;


      procedure Load_Types
      --
      -- <Type Definition>
      --
      -- Reads the type definitions into Actions.Types.
      --
      is
      begin

         Read_Type_Definitions (
            Rec    => Rec,
            File   => File,
            Stream => Stream,
            Table  => Action.Types);

         Get_Record;

      end Load_Types;


      procedure Load_Line_Number
      --
      -- Reads a Line_Number_Item from an (Extended) Debug Items record
      -- and enters it in Symbol_Table under Line_Scope.
      --
      is
         Item : Line_Item_T;
      begin

         Line_Item_T'Read (Stream, Item);

         Symbols.Connect_Line (
            Scope   => Line_Scope,
            Number  => Symbols.Line_Number_T (Item.Line),
            Address => To_Code_Address (
               Seg_ID  => Item.Seg_ID,
               Address => Item.Offset,
               Action  => Action),
            Within => Symbol_Table);

      end Load_Line_Number;


      procedure Synthesize_Type (
         Storage    : in     Storage_T;
         Variable   : in     Boolean;
         Type_Index :    out Type_Index_T;
         Defining   :    out Compound_Type_T)
      --
      -- Synthesizes (makes up, invents) a Type Index and a Defining
      -- type descriptor for a Debug Items (non-extended) symbol, from
      -- the Storage of the symbol and the Symbol_Info.Variable flag.
      --
      is
      begin

         if Opt.Trace_Symbols then

            if Variable then

               Output.Trace (
                    "AOMF type synthesized for variable in "
                  & Text.Image (Storage));

            else

               Output.Trace (
                    "AOMF type synthesized for non-variable in "
                  & Text.Image (Storage));

            end if;

         end if;

         case Storage.Kind is
         when XData | Data | IData => Type_Index := 3;  -- UChar
         when Bit                  => Type_Index := 1;  -- Bit
         when others               => Type_Index := 0;  -- Untyped
         end case;

         if  (   Storage.Kind = Code
              or Storage.Kind = Code_Bank)
         and not Variable
         then
            -- Might be a subprogram.

            Defining := (
               Kind     => Funktion,
               Funktion => (Returns => 0, Formals => 0));

         else
            -- Seems to be a variable.

            Defining := (
               Kind       => Predefined,
               Predefined => (Basic => Type_Index));

         end if;

      end Synthesize_Type;


      procedure Make_Line_Scope
      --
      -- Builds the Line_Scope for source-line items, from
      -- the recorded Source_File_Name, Module_Name, and/or
      -- Procedure_Name.
      --
      is
      begin

         Symbols.Make_Global (Line_Scope);

         -- Source-file name:

         if Action.Parser.Source_Name /= No_Name then
            -- We prefer to use the Source_Name, if available.

            Symbols.Nest (
               Level => Source_File_Name (Action.Parser),
               Scope => Line_Scope);

         elsif Action.Parser.Module_Name /= No_Name then
            -- If we have no Source_Name, use the Module_Name
            -- if available. TBM to use Source Browser records.

            Symbols.Nest (
               Level => Module_Name (Action.Parser),
               Scope => Line_Scope);

         else
            -- Argh, no idea of the source file, but put in a
            -- null string as a placeholder.

            Output.Warning (
               "AOMF source-file name unknown for line numbers.");

            Symbols.Nest (
               Level => "",
               Scope => Line_Scope);

         end if;

         -- Subprogram name:

         if Action.Parser.Procedure_Name /= No_Name then

            Symbols.Nest (
               Level => Procedure_Name (Action.Parser),
               Scope => Line_Scope);

         else

            if Opt.Warn_Line_No_Sub then

               Output.Warning (
                  "AOMF subprogram name unknown for line numbers.");

            end if;

            Symbols.Nest (
               Level => "",
               Scope => Line_Scope);

         end if;

      end Make_Line_Scope;


      procedure Load_Debug_Items
      --
      -- <Debug Items>
      --
      is

         Def_Kind : Def_Kind_T;
         -- The kind of items in the record.

         Symbol         : Symbol_Item_T;
         Segment_Symbol : Segment_Symbol_Item_T;
         -- Content items.

         Storage : Storage_T;
         -- Storage, for Segment Symbols.

         Type_Index : Type_Index_T;
         -- A synthetic type-index for symbols.

         Defining : Compound_Type_T;
         -- A synthetic "defining type" for symbols.

      begin

         Def_Kind_T'Read (Stream, Def_Kind);

         if Opt.Trace_Loading then

            Output.Trace (
                 "AOMF definitions for "
               & Def_Kind_T'Image (Def_Kind));

         end if;

         if Def_Kind = Line_Numbers then
            -- Construct the scope for line numbers.

            Make_Line_Scope;

         end if;

         -- Parse the items:

         while Data_Left (Rec, File) loop

            case Def_Kind is

            when Local_Symbols | Public_Symbols =>

               Symbol_Item_T'Read (Stream, Symbol);

               Synthesize_Type (
                  Storage    => Symbol.Info.Usage,
                  Variable   => Symbol.Info.Variable,
                  Type_Index => Type_Index,
                  Defining   => Defining);

               Define_Symbol (
                  Symbol   => Symbol.Name.all,
                  Seg_ID   => Symbol.Seg_ID,
                  Storage  => Symbol.Info.Usage,
                  Index    => Type_Index,
                  Defining => Defining,
                  Offset   => Symbol.Offset,
                  Action   => Action,
                  Into     => Symbol_Table);

            when Segment_Symbols =>

               Segment_Symbol_Item_T'Read (Stream, Segment_Symbol);

               if Opt.Define_Segment_Symbols then

                  Storage := Segment_Symbol.Info.Segment_Type;

                  Synthesize_Type (
                     Storage    => Storage,
                     Variable   =>
                        Storage.Kind /= Code and Storage.Kind /= Code_Bank,
                     Type_Index => Type_Index,
                     Defining   => Defining);

                  Define_Symbol (
                     Symbol   => Segment_Symbol.Name.all,
                     Seg_ID   => Segment_Symbol.Seg_ID,
                     Storage  => Storage,
                     Index    => Type_Index,
                     Defining => Defining,
                     Offset   => Segment_Symbol.Offset,
                     Action   => Action,
                     Into     => Symbol_Table);

               else

                  Output.Note (
                       "Skipped segment symbol """
                     & Segment_Symbol.Name.all
                     & """.");

               end if;

            when Line_Numbers =>

               Load_Line_Number;

            when Unknown =>

               Skip_Wrong_Record ("unknown content.");
 
            end case;

         end loop;

         Get_Record;

      end Load_Debug_Items;


      procedure Load_Extended_Debug_Items
      --
      -- <Extended Debug Items>
      --
      is

         Def_Kind : Def_Kind_T;
         -- The kind of items in the record.

         Symbol : Symbol_Item_Extended_T;
         -- Extended debug symbol, if Def_Kind /= Line_Numbers.

         Storage : Storage_T;
         -- Storage, for symbols with Undefined type.

         Type_Index : Type_Index_T;
         -- A synthetic type-index for symbols with Undefined type.

         Defining : Compound_Type_T;
         -- A synthetic "defining type" for symbols with Undefined type.

      begin

         Def_Kind_T'Read (Stream, Def_Kind);

         if Opt.Trace_Loading then

            Output.Trace (
                 "AOMF definitions for "
               & Def_Kind_T'Image (Def_Kind));

         end if;

         if Def_Kind = Line_Numbers then
            -- Construct the scope for line numbers.

            Make_Line_Scope;

         end if;

         -- Parse the items:

         while Data_Left (Rec, File) loop

            case Def_Kind is

            when Local_Symbols | Public_Symbols | Segment_Symbols =>

               Symbol_Item_Extended_T'Read (Stream, Symbol);

               if Def_Kind = Segment_Symbols
               and not Opt.Define_Segment_Symbols
               then

                  Output.Note (
                       "Skipped (extended) segment symbol """
                     & Symbol.Name.all
                     & """.");

               elsif Symbol.Type_Index /= Untyped_Index then
                  -- Nice and fine.

                  Define_Symbol (
                     Symbol   => Symbol.Name.all,
                     Seg_ID   => Symbol.Seg_ID,
                     Storage  => Symbol.Storage,
                     Index    => Symbol.Type_Index,
                     Defining => Defining_Type (
                        Index => Symbol.Type_Index,
                        Table => Action.Types),
                     Offset   => Symbol.Offset,
                     Action   => Action,
                     Into     => Symbol_Table);

               else
                  -- Hah. We try to work around by inventing.

                  Storage := Symbol.Storage;

                  Synthesize_Type (
                     Storage    => Storage,
                     Variable   =>
                        Storage.Kind /= Code and Storage.Kind /= Code_Bank,
                     Type_Index => Type_Index,
                     Defining   => Defining);

                  Define_Symbol (
                     Symbol   => Symbol.Name.all,
                     Seg_ID   => Symbol.Seg_ID,
                     Storage  => Storage,
                     Index    => Type_Index,
                     Defining => Defining,
                     Offset   => Symbol.Offset,
                     Action   => Action,
                     Into     => Symbol_Table);

               end if;

            when Line_Numbers =>

               Load_Line_Number;

            when Unknown =>

               Skip_Wrong_Record ("unknown content.");
 
            end case;

         end loop;

         Get_Record;

      end Load_Extended_Debug_Items;


      procedure Check_Match (Beginner, Ender : in Scope_Definition_T)
      --
      -- Checks that the Ender is a proper end-scope record for the
      -- scope begun by the Beginner.
      --
      is

         Expected : constant Block_Ender_T := Block_Ender(Beginner.Block_Type);
         -- The expected value of Ender.Block_Type.

      begin

         if Ender.Block_Type /= Expected then

            Syntax_Error (
                 "Expected "
               & Block_Type_T'Image (Expected)
               & " """
               & Beginner.Block_Name.all);

         elsif Beginner.Block_Name.all /= Ender.Block_Name.all then

            Syntax_Error (
                 "Expected Block_Name """
               & Beginner.Block_Name.all
               & """, not """
               & Ender.Block_Name.all);

         end if;

      end Check_Match;


      procedure Scope (
         Beginner : in Scope_Definition_T;
         Level    : in Natural)
      --
      -- <scope begin> <data or debug>* <scope end>
      --
      -- where
      --
      -- <scope begin> = <Scope Definition> [ <Source Name> ]
      -- <scope end>   = <Scope Definition>
      --
      -- In <scope begin>, the <Scope Definition> is a Block_Beginner_T.
      -- In <scope end>, the <Scope Definition> is a Block_Ender_T and
      -- must Match.
      --
      -- On entry, the <Scope Definition> for <scope begin> has been
      -- read and is the Beginner parameter.
      --
      -- The scope nesting should be:
      --   - level 0 : Module
      --   - level 1 : Procedure
      --   - level 2 : Do
      --   - level 3 : Do
      --   - ...
      --
      -- Only level 0 (Module) should have a <Source Name> record in
      -- the <scope begin>.
      --
      is

         Level_Block : Block_Type_T;
         -- The expected Beginner.Block_Type.

         Source : Source_Name_T;
         -- The possible Source_Name.

         Inner : Scope_Definition_T;
         -- A scope record, maybe begins, maybe ends.

      begin

         -- Check Beginner.Block_Type against Level:

         case Level is
         when 0      => Level_Block := Begin_Module;
         when 1      => Level_Block := Begin_Procedure;
         when others => Level_Block := Begin_Do;
         end case;

         if Beginner.Block_Type /= Level_Block then

            Output.Error (
                 "AOMF level"
               & Natural'Image (Level)
               & " scope should be "
               & Block_Type_T'Image (Level_Block)
               & ", not "
               & Block_Type_T'Image (Beginner.Block_Type));

         end if;

         -- Set the Parser Names for modules and procedures:

         case Beginner.Block_Type is

         when Begin_Module =>

            Action.Parser.Module_Name :=
               String_Pool.To_Item (Beginner.Block_Name.all);

         when Begin_Procedure =>

            Action.Parser.Procedure_Name :=
               String_Pool.To_Item (Beginner.Block_Name.all);

         when others =>

            null;

         end case;

         -- Source Name, maybe:

         Get_Record;

         if Rec.Kind = Source_Name then
            -- Entering a new source file.

            Source_Name_T'Read (Stream, Source);

            if Opt.Trace_Loading then

               Output.Trace (
                    "Enter AOMF scope for source file """
                  & Source.File_Name.all
                  & '"');

            end if;

            Action.Parser.Source_Name :=
               String_Pool.To_Item (Source.File_Name.all);

            Get_Record;

         end if;

         -- Entering a scope:

         if Beginner.Block_Name'Length > 0 then

            if Opt.Trace_Loading then

               Output.Trace (
                    "Enter AOMF scope """
                  & Beginner.Block_Name.all
                  & '"');

            end if;

            Symbols.Nest (
               Level => Beginner.Block_Name.all,
               Scope => Action.Scope);

         end if;

         -- Innards of the scope:

         loop

            case Rec.Kind is

            when Content =>

               Load_Content;

            when Scope_Definition =>

               Scope_Definition_T'Read (Stream, Inner);

               case Inner.Block_Type is

               when Unknown => Skip_Record;

               when Block_Beginner_T =>

                  Scope (Beginner => Inner, Level => Level + 1);

               when Block_Ender_T =>

                  exit;

               end case;

            when Type_Definition =>

               Load_Types;

            when Debug_Items =>

               Load_Debug_Items;

            when Extended_Debug_Items =>

               Load_Extended_Debug_Items;

            when Source_Browse_TBC
               | BL51_Bank_Head
               | Module_Header
               | Module_End
               | Source_Name
               | Unknown =>

               Skip_Wrong_Record ("invalid in a scope.");

            end case;

         end loop;

         -- End of the scope:

         Check_Match (Beginner => Beginner, Ender => Inner);

         if Beginner.Block_Name'Length > 0 then

            Symbols.Unnest (
               Level => Beginner.Block_Name.all,
               Scope => Action.Scope);

         end if;

         Get_Record;

      end Scope;


      procedure Module
      --
      -- <header> <data or debug>* <Module_End>
      --
      -- where <header> = [ <BL51 Bank Head> ] <Module Header>
      --
      is

         Header : Module_Header_T;
         Ender  : Module_End_T;
         -- The header and ender records.

         Scoper : Scope_Definition_T;
         -- Begins a scope.

      begin

         -- Header of the module:

         if Rec.Kind = BL51_Bank_Head then

            Output.Warning (
               "Skipping AOMF BL51 Bank Head record; not implemented.");

            Skip_Record;

         end if;

         if Rec.Kind /= Module_Header then

            Syntax_Error ("Module_Header expected.");

         end if;

         Module_Header_T'Read (Stream, Header);

         Get_Record;

         -- Innards of the module:

         loop

            case Rec.Kind is

            when Content =>

               Load_Content;

            when Scope_Definition =>

               Scope_Definition_T'Read (Stream, Scoper);

               case Scoper.Block_Type is

               when Unknown => Skip_Record;

               when Block_Beginner_T =>

                  Scope (Beginner => Scoper, Level => 0);

               when Block_Ender_T =>

                  Skip_Wrong_Record ("end-of-scope out of context.");

               end case;

            when Type_Definition =>

               Load_Types;

            when Debug_Items =>

               Load_Debug_Items;

            when Extended_Debug_Items =>

               Load_Extended_Debug_Items;

            when Module_End =>

               exit;

            when Source_Browse_TBC
               | BL51_Bank_Head
               | Module_Header
               | Source_Name
               | Unknown =>

               Skip_Wrong_Record ("invalid in a module.");

            end case;

         end loop;

         -- End of the module:

         Module_End_T'Read (Stream, Ender);

         if Header.Name.all /= Ender.Name.all then

            Output.Error (
               "AOMF Module_Header.Name /= Module_End.Name");

         end if;

      end Module;
         

   begin  -- Parse_And_Load

      Get_Record;

      Skip_Nulls;

      Module;

      Clear (Action.Types);

      Symbols.Discard (Line_Scope);

   end Parse_And_Load;


end Formats.AOMF_Keil.Parsing;
