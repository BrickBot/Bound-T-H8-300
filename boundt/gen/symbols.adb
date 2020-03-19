-- Symbols (body)
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
-- $Revision: 1.36 $
-- $Date: 2015/10/24 20:05:52 $
--
-- $Log: symbols.adb,v $
-- Revision 1.36  2015/10/24 20:05:52  niklas
-- Moved to free licence.
--
-- Revision 1.35  2009-03-24 07:48:35  niklas
-- BT-CH-0166: String_Pool.Item_T for source-file and marker names.
--
-- Revision 1.34  2009/03/20 18:19:30  niklas
-- BT-CH-0164: Assertion context identified by source-line number.
--
-- Revision 1.33  2008/10/11 08:16:14  niklas
-- BT-CH-0148: Symbols from text files and the -symbols option.
--
-- Revision 1.32  2007/08/17 21:30:34  niklas
-- Added function Equal for Connection_T.
-- Extended procedure Insert to issue "Duplicated symbol" warnings
-- only when the same symbol is defined with a non-Equal value, and
-- to issue just a Note when the new value Equals the old one.
--
-- Revision 1.31  2007/08/14 12:36:38  niklas
-- BT-CH-0072: Corrections to handling ambiguous names.
--
-- Revision 1.30  2007/08/13 09:01:34  niklas
-- BT-CH-0070: Tracing symbol scopes. Warn re duplicated symbol.
--
-- Revision 1.29  2007/05/03 07:26:26  niklas
-- Added two variants of Connect_Subprogram using Scope_T.
--
-- Revision 1.28  2007/04/26 11:28:05  niklas
-- BT-CH-0059.
--
-- Revision 1.27  2007/02/24 09:51:53  niklas
-- BT-CH-0046.
--
-- Revision 1.26  2006/05/08 13:12:19  niklas
-- Changed Connect_Line to accept Scopes with depth >= 1 and emit
-- only a Note (not a Fault) if Scope < 2. This still happens for
-- 8051/IAR programs, where it would be a bit difficult to get the
-- subprogram name for level 2, and Output usually knows the
-- subprogram name anyway.
--
-- Revision 1.25  2006/04/28 09:19:46  niklas
-- Changed function Name_Of (Level, Scope) to emit a Fault
-- instead of raising Constraint_Error for an invalid level.
-- Extended Connect_Line to emit a fault if the scope has
-- less than two levels (source file, subprogram).
--
-- Revision 1.24  2005/10/09 08:10:24  niklas
-- BT-CH-0013.
--
-- Revision 1.23  2005/06/28 14:19:23  niklas
-- Fixed function Variable_Connections to use the "variable" Kind
-- instead of the "label" Kind.
--
-- Revision 1.22  2005/04/17 08:05:13  niklas
-- Changed problems with wrongly or too deeply nested scopes
-- to report Fault instead of Error.
--
-- Revision 1.21  2005/02/20 15:15:37  niklas
-- BT-CH-0004.
--
-- Revision 1.20  2004/04/25 13:37:54  niklas
-- First Tidorum version.
-- Replaced "Cell" connections by "Variable" connections that use
-- Storage.Location_T and thus allow the mapping from variable to cell
-- to depend on code address.
-- Added operations to find the source lines that surround a given code
-- address (approximate source-line identification).
--
-- Revision 1.19  2003/02/27 14:40:13  holsti
-- Duplicated symbols are reported as Warnings (not Notes) but the
-- warnings are conditional on Opt.Warn_Duplicated_Symbol.
--
-- Revision 1.18  2001/11/19 11:24:44  saarinen
-- Modified "Duplicate Symbol" Errors into Notes to reduce the number
-- of error messages.
--
-- Revision 1.17  2001/06/09 16:32:01  holsti
-- Added Connect_Subprogram variant with Giving parameter.
--
-- Revision 1.16  2001/04/10 12:59:10  ville
-- Added subroutines Connect_Label and Label_Connections
--
-- Revision 1.15  2001/03/21 20:30:33  holsti
-- Name_Of scope-level added. Symbols.Lines omitted.
--
-- Revision 1.14  2001/03/02 09:01:15  holsti
-- Default scope delimiter changed to vertical bar.
--
-- Revision 1.13  2000/12/28 19:47:00  holsti
-- Scope_Of allows empty scope (NC_043).
-- Connections_For_Symbol and Connections_For_Line removed (unused).
--
-- Revision 1.12  2000/12/22 13:37:02  sihvo
-- Added line numbers.
--
-- Revision 1.11  2000/12/04 08:09:27  sihvo
-- Changed Hash to Bags. Added variables to symbol table.
--
-- Revision 1.10  2000/11/22 22:30:21  holsti
-- Using Processor.Code_Address_T instead of Processor.Address_T.
-- Comments updated re Connection_T attributes for various symbols.
--
-- Revision 1.9  2000/11/07 11:40:54  ville
-- Add_Address call deleted from Connect_Cell
--
-- Revision 1.8  2000/11/06 12:37:12  langback
-- Replaced Connect with separate routines Connect_Subprogram and
-- Connect_Cell. No routine for Label's yet.
--
-- Revision 1.7  2000/10/25 14:22:29  langback
-- Added stub for function "Variable_Cell". Proper implementation needed.
--
-- Revision 1.6  2000/08/25 06:44:51  parviain
-- Fixed function Address_Less.
--
-- Revision 1.5  2000/07/13 11:24:16  saarinen
-- Added hash-table for Code_Addresses and implemented
-- Connections_For_Address.
--
-- Revision 1.4  2000/07/04 11:23:06  saarinen
-- Moved functions Name_Of and Scope_Of from Programs to here.
--
-- Revision 1.3  2000/05/05 12:11:22  holsti
-- Symbols.Opt controls tracing output.
--
-- Revision 1.2  2000/05/02 19:41:30  holsti
-- String_Pool added, Symbols partly implemented.
--
-- Revision 1.1  2000/04/24 14:28:39  holsti
-- Symbol scopes added.
--


with Ada.Strings.Fixed;
with Ada.Strings.Unbounded;
with Bags;                                     -- MW components.
with Bags.Bounded_Operations;                  -- MW components.
with Output;
with Symbols.Opt;
with Table_Of_Strings_And_Static_Values_G;     -- LGL components.


package body Symbols is


   use Ada.Strings.Unbounded;

   use type String_Pool.Item_T;


   --
   ---   Equality of Connection_T
   --


   function Equal (Left, Right : Connection_T)
   return Boolean
   --
   -- Whether the two connections are equivalent, that is, of
   -- equal meaning after accesses are dereferenced.
   --
   is
      use type Processor.Code_Address_T;
      use type Storage.Location_T;
   begin

      -- The following uses the reference-uniqueness of Scope_T:

      if Left.Kind  /= Right.Kind
      or Left.Scope /= Right.Scope
      then

         return False;

      else

         -- The following uses the reference-uniqueness
         -- of String_Pool.Item_T, for Name and Var_Name:

         case Left.Kind is

         when Subprogram | Label =>

            return Left.Name    = Right.Name
            and    Left.Address = Right.Address;

         when Variable =>

            return Left.Var_Name     = Right.Var_Name
            and    Left.Location.all = Right.Location.all;

         when Line_Number =>

            return Left.Line_Number  = Right.Line_Number
            and    Left.Line_Address = Right.Line_Address;

         end case;

      end if;

   end Equal;


   --
   ---   String / level lists
   --


   function Image (
      Strings   : Level_List_T;
      Delimiter : Character := Default_Delimiter)
   return String
   --
   -- The Strings concatenated in index order, separated
   -- by the Delimiter.
   --
   is

      Result : Unbounded_String;
      -- The result, initially null.

   begin

      for S in Strings'Range loop

         Append (Result, String_Pool.To_String (Strings(S)));

         if S < Strings'Last then

            Append (Result, Delimiter);

         end if;

      end loop;

      return To_String (Result);

   end Image;


   type Relation_T is (Less, Equal, Greater);
   --
   -- The result of a relational operator.


   function Lex_Relation (Left, Right : Level_List_T) return Relation_T
   --
   -- Lexicographic relation on string (Level) lists.
   --
   is
      L : Positive := Left'First;
      R : Positive := Right'First;
   begin

      loop
         -- Invariant: for all indices I < L and J < R,
         -- Left(I) = Right(J).

         if L > Left'Last and R > Right'Last then
            return Equal;

         elsif L > Left'Last then
            -- Left is true prefix of Right.
            return Less;

         elsif R > Right'Last then
            -- Right is a true prefix of Left.
            return Greater;

         elsif Left (L) < Right(R) then return Less;
         elsif Right(R) < Left (L) then return Greater;
         else
            -- Left(L) = Right(R). Undecided still.
            L := L + 1;
            R := R + 1;
         end if;
      end loop;

   end Lex_Relation;


   function Lex_Less (Left, Right : Level_List_T) return Boolean
   --
   -- Lexicographic "<".
   --
   is
   begin

      return Lex_Relation (Left, Right) = Less;

   end Lex_Less;


   function Shared_Suffix_Length (Left, Right : Level_List_T)
   return Natural
   --
   -- Return the largest value L such that the length-L suffixes of
   -- Left and Right are equal.
   --
   is
      L : Natural := Left'Last;
      R : Natural := Right'Last;
   begin

      loop

         if (L < Left'First or R < Right'First)
         or else Left(L) /= Right(R) then

            return Left'Last - L;

         end if;

         L := L - 1;
         R := R - 1;

      end loop;

   end Shared_Suffix_Length;


   function Is_Suffix (Suffix, Whole : Level_List_T) return Boolean
   --
   -- Whether one string-list (Suffix) is a suffix of another (Whole)
   -- or is equal to it.
   --
   is
      F : Positive;
   begin

      if Suffix'Length > Whole'Length then

         return False;

      else

         F := Whole'Last - Suffix'Length + 1;

         return Whole(F .. Whole'Last) = Suffix;

      end if;

   end Is_Suffix;


   --
   --    Forward declarations
   --


   function To_Scope (Nest : Level_List_T) return Scope_T;
   --
   -- The Scope for the given Nest of levels.


   --
   ---   Scopes
   --


   function Depth (Scope : Scope_T) return Natural
   is
   begin

      return Scope'Length;

   end Depth;


   function Name_Of (
      Level  : Positive;
      Within : Scope_T)
   return String_Pool.Item_T
   --
   -- The name of the chosen level within the given scope.
   -- The Level must be in 1 .. Depth, else a Fault is reported
   -- and the null string "" returned.
   --
   is
   begin

      if Level in Within'Range then

         return Within(Level);

      else

         Output.Fault (
            Location => "Symbols.Name_Of (Level, Scope)",
            Text     =>
                 "Level"
               & Positive'Image (Level)
               & " > depth"
               & Natural'Image (Within'Length)
               & Output.Field_Separator
               & Image (Within));

         return String_Pool.To_Item ("");

      end if;

   end Name_Of;


   function Name_Of (
      Level  : Positive;
      Within : Scope_T)
   return String
   is
   begin

      return String_Pool.To_String (Name_Of (Level, Within));

   end Name_Of;


   function Deepest (Scope : Scope_T) return String
   is
   begin

      return String_Pool.To_String (Scope(Scope'Last));

   exception

   when Constraint_Error =>

      Output.Fault (
         Location => "Symbols.Deepest",
         Text     =>
              "Accessing Deepest level of null scope with depth ="
            & Natural'Image (Scope'Length));

      raise;

   end Deepest;


   function Image (
      Scope     : Scope_T;
      Delimiter : Character := Default_Delimiter)
   return String
   is
   begin

      return Image (
         Strings   => Scope.all,
         Delimiter => Delimiter);

   end Image;


   function Scope (Identifier : String) return Scope_T
   is
   begin

      return To_Scope (Nest => (1 => String_Pool.To_Item (Identifier)));

   end Scope;


   function Scope_Of (
      Identifier : String;
      Delimiter  : Character := Default_Delimiter)
   return Scope_T
   is
      use Ada.Strings.Fixed;

      Delim : constant String := (1 => Delimiter);

      Stack : Scope_Stack_T;

      First  : Positive := Identifier'First;
      Last   : constant Natural := Identifier'Last;
      Del    : Natural;

   begin

      loop

         -- Find next delimiter, exit if there is none:

         Del := Index (Identifier(First .. Last), Delim);
         exit when Del = 0;

         -- Push the delimited name on the scope stack:

         Nest (
            Level => Identifier (First .. Del - 1),
            Scope => Stack);

         First := Del + 1;

      end loop;

      return To_Scope (Stack);

   end Scope_Of;


   function Name_Of (
      Identifier : String;
      Delimiter  : Character := Default_Delimiter)
   return String
   is
      use Ada.Strings.Fixed;
      Del : Natural;
   begin

      Del := Index (
         Source  => Identifier,
         Pattern => (1 => Delimiter),
         Going   => Ada.Strings.Backward);

      if Del = 0 then
         return Identifier;
      else
         return Identifier (Del + 1 .. Identifier'Last);
      end if;

   end Name_Of;


   --
   ---   Scope stacks
   --


   use Level_Vectors;


   procedure Make_Global (Scope : in out Scope_Stack_T)
   is
   begin

      Truncate_Length (Vector => Scope.Stack, To => 0);

   end Make_Global;


   function Depth (Scope : Scope_Stack_T) return Natural
   is
   begin

      return Length (Scope.Stack);

   end Depth;


   procedure Nest (
      Level : in     String;
      Scope : in out Scope_Stack_T)
   is
   begin

      Append (To => Scope.Stack, Value => String_Pool.To_Item (Level));

   end Nest;


   procedure Unnest (Scope : in out Scope_Stack_T)
   is
   begin

      if Length (Scope.Stack) > 0 then

         Drop (Index => Last (Scope.Stack), From => Scope.Stack);

      else

         Output.Fault (
            Location => "Symbols.Unnest",
            Text     =>
                 "Tried to unnest a null scope with depth ="
               & Natural'Image (Depth (Scope)));

         raise Constraint_Error;

      end if;

   end Unnest;


   procedure Unnest (
      Level : in     String;
      Scope : in out Scope_Stack_T)
   is
   begin

      if Element (Scope.Stack, Last (Scope.Stack)) /= Level then

         Output.Warning (Text =>
              "Closing scope " & Level
            & " but current scope is " & Deepest (Scope));

      end if;

      Unnest (Scope);

   end Unnest;


   function Deepest (Scope : Scope_Stack_T) return String
   is
   begin

      return String_Pool.To_String (Element (Scope.Stack, Last (Scope.Stack)));

   end Deepest;


   function Image (
      Scope     : Scope_Stack_T;
      Delimiter : Character := Default_Delimiter)
   return String
   is
   begin

      return Image (To_Vector (Scope.Stack), Delimiter);

   end Image;


   package Scope_Table is new Table_Of_Strings_And_Static_Values_G (
      Character_Type => String_Pool.Item_T,
      String_Type    => Level_List_T,
      Less           => Lex_Less,
      Equals         => "=",
      Value_Type     => Scope_T);
   --
   -- All scopes associated with symbols are stored in the Scope_Table,
   -- so that each distinct scope is represented by a unique Scope_T
   -- value.


   Scopes : Scope_Table.Table_Type;
   --
   -- The table of scopes, used to assign unique, compact identifiers
   -- to each scope nest that is used.
   --
   -- The default initial value is the empty table.


   function To_Scope (Nest : Level_List_T) return Scope_T
   --
   -- The Scope for the given Nest of levels.
   --
   is

      Scope : Scope_T;
      -- The return value.

   begin

      -- Look up the level-nest in the scope table:

      Scope_Table.Get_Value (
         Table => Scopes,
         Key   => Nest,
         Value => Scope);

      -- This nest is already in the table.

      return Scope;

   exception

      when Scope_Table.Missing_Item_Error =>

         -- New scope. Insert in table.

         if Opt.Trace_Scopes then

            Output.Trace (
                 "New scope """
               & Image (Nest)
               & '"');

         end if;

         Scope := new Level_List_T'(Nest);

         Scope_Table.Insert (
            Table => Scopes,
            Key   => Scope.all,
            Value => Scope);

         return Scope;

   end To_Scope;


   function To_Scope (Stack : Scope_Stack_T) return Scope_T
   is
   begin

      return To_Scope (Nest => To_Vector (Stack.Stack));

   end To_Scope;


   procedure Discard (Item : in out Scope_Stack_T)
   is
   begin

      Erase (Item.Stack);

   end Discard;


   --
   ---   Connections of symbols to addresses etc.
   --


   function To_Source_File_Name (Name : String) return Source_File_Name_T
   is
   begin

      if Name'Length = 0 then

         return Null_Name;

      else

         return To_Item (Name);

      end if;

   end To_Source_File_Name;


   function Image (Name : Source_File_Name_T) return String
   is
   begin

      if Name = Null_Name then

         return "";

      else

         return To_String (Name);

      end if;

   end Image;


   function Image (Item : Line_Number_T) return String
   renames Output.Image;


   function Image (Item : Connection_T) return String
   is

      Name : constant String :=
           Output.Field_Separator
         & Scoped_Name_Of (Item)
         & Output.Field_Separator;
      -- The middle field, with both its separators.

   begin

      case Item.Kind is

      when Subprogram =>

         return "Subprogram"
              & Name
              & Processor.Image (Item.Address);

      when Label =>

         return "Label"
              & Name
              & Processor.Image (Item.Address);

      when Variable =>

         return "Variable"
              & Name
              & Storage.Image (Item.Location);

      when Line_Number =>

         return "Source line"
              & Name
              & Processor.Image (Item.Line_Address);

      end case;

   end Image;


   function Full_Key (Item : Connection_T) return Level_List_T
   --
   -- The full key consists of the scope levels (top down)
   -- followed by the identifier name.
   --
   is
   begin

      return Item.Scope.all
          & (1 => String_Pool.To_Item (Name_Of (Item)));

   end Full_Key;


   function Kind_Of (Connection : Connection_T) return Connection_Kind_T
   is
   begin

      return Connection.Kind;

   end Kind_Of;


   function Scope_Of (Connection : Connection_T) return Scope_T
   is
   begin

      return Connection.Scope;

   end Scope_Of;


   function Name_Of (Connection : Connection_T) return String
   is
   begin

      case Connection.Kind is

      when Subprogram | Label =>

         return String_Pool.To_String (Connection.Name);

      when Variable =>

         return String_Pool.To_String (Connection.Var_Name);

      when Line_Number =>

         return Image (Connection.Line_Number);

      end case;

   end Name_Of;


   function Scoped_Name_Of (
      Connection : Connection_T;
      Delimiter  : Character := Default_Delimiter)
   return String
   is

      Scope : constant String := Image (Scope_Of (Connection), Delimiter);
      -- The scope part.

   begin

      if Scope'Length = 0 then
         -- There is no scope.

         return Name_Of (Connection);

      else

         return Scope & Delimiter & Name_Of (Connection);

      end if;

   end Scoped_Name_Of;


   function Source_File_Of (Connection : Connection_T)
   return Source_File_Name_T
   is
   begin

      return Source_File_Name_T (String_Pool.Item_T'(Name_Of (
         Level  => 1,
         Within => Connection.Scope)));

   end Source_File_Of;


   function Source_File_Of (Connection : Connection_T)
   return String
   is
   begin

      return Name_Of (Level => 1, Within => Connection.Scope);

   end Source_File_Of;


   function Line_Number_Of (Connection : Connection_T)
   return Line_Number_T
   is
   begin

      return Connection.Line_Number;

   end Line_Number_Of;


   function Line_Number_Of (Connection : Connection_T)
   return String
   is
   begin

      return Image (Connection.Line_Number);

   end Line_Number_Of;


   function Address_Of (Connection : Connection_T)
   return Processor.Code_Address_T
   is
   begin

      if Connection.Kind = Line_Number then

         return Connection.Line_Address;

      else

         return Connection.Address;

      end if;

   end Address_Of;


   function No_Code_Addresses return Code_Address_List_T
   is

      None : Code_Address_List_T (1 .. 0);

   begin

      return None;

   end No_Code_Addresses;


   procedure Swap (This, That : in out Processor.Code_Address_T)
   --
   -- A straight swap.
   --
   is
      Holder : Processor.Code_Address_T;
   begin

      Holder := This;
                This := That;
                        That := Holder;

   end Swap;


   function Addresses_Of (Connections : Connection_Set_T)
   return Code_Address_List_T
   is
      use type Processor.Code_Address_T;

      List : Code_Address_List_T (1 .. Connections'Length);
      Last : Natural := 0;
      -- The result will be List(1 .. Last), in increasing order.

      Addr : Processor.Code_Address_T;
      -- The address of a connection.

      Dup : Boolean;
      -- Whether Addr duplicates an address already in the List.

   begin

      -- This is a "square" implementation. Blushes.

      for C in Connections'Range loop

         Addr := Address_Of (Connections(C));

         Dup := False;

         Insertion : for L in 1 .. Last loop

            -- Addr is a code-address to be inserted in the list.
            -- It can be the address of Connections(C) or an address
            -- from the List that was displaced when Connections(C)
            -- or the earlier Addr value was put in the List at the
            -- suitable place, according to "<".

            Dup := Addr = List(L);

            exit Insertion when Dup;

            if Addr < List(L) then
               -- Addr goes here.

               Swap (Addr, List(L));

            end if;

         end loop Insertion;

         if not Dup then
            -- The List lengthens.

            Last := Last + 1;

            List(Last) := Addr;

         end if;

      end loop;

      return List(1 .. Last);

   end Addresses_Of;


   function Location_Of (Connection : Connection_T)
   return Processor.Cell_Spec_T
   is
   begin

      return Storage.Spec_Of (
         Connection.Location(Connection.Location'First).Cell);

   end Location_Of;
      

   function Location_Of (Connection : Connection_T)
   return Storage.Location_Ref
   is
   begin

      return Connection.Location;

   end Location_Of;


   --
   ---   Symbol tables
   --


   --    Symbolic name -->  Machine address
   --
   -- Tables to map symbolic names (in scopes) to machine address are
   -- defined here below.
   --
   -- Each symbol is tabled with a key containin the least number of
   -- scope levels (from deepest upwards) that are needed to identify
   -- the symbol unambiguously.
   --
   -- For example, consider the symbol S in scope A:B:C.
   -- If S is a unique name even without scope qualification, the
   -- symbol is stored with the key S (the stored value contains the
   -- scope A:B:C, too). Uniqueness means that there is no other
   -- symbol with name S, in any scope. If S is not unique in this
   -- sense, the symbol is stored with the key C:S, or B:C:S, or A:B:C:S,
   -- whichever is the shortest unique key. For shorter (ambiguous) keys,
   -- an "unresolved" entry is tabled.
   --
   -- When a symbol is looked up from the table, the same sequence
   -- of keys is used: a first look-up uses just the symbol name, and
   -- if an "unresolved" entry is found, successive scope levels are
   -- included until the symbol is resolved.


   type Table_Value_T (Kind : Value_Kind_T := Unresolved) is record
      case Kind is
         when Unresolved =>
            null;
         when Suffix_Key | Full_Key =>
            Connection : Connection_T;
      end case;
   end record;


   function Image (Item : Table_Value_T) return String
   --
   -- For human understanding.
   --
   is

      Kind : constant String := Value_Kind_T'Image (Item.Kind);

   begin

      case Item.Kind is

      when Unresolved =>

         return Kind;

      when Suffix_Key | Full_Key =>

         return Kind & '(' & Image (Item.Connection) & ')';

      end case;

   end Image;


   package Connection_Table is new Table_Of_Strings_And_Static_Values_G (
      Character_Type => String_Pool.Item_T,
      String_Type    => Level_List_T,
      Less           => Lex_Less,
      Equals         => "=",
      Value_Type     => Table_Value_T);
   --
   -- Tables to map (Scope, Name) -> Connection.


   package Address_Bags is new Bags (
      Key_Type  => Processor.Code_Address_T,
      Item_Type => Connection_T,
      Key_Of    => Address_Of,
      "<"       => Processor."<",
      "="       => Processor."=",
      Count     => Natural);
   --
   -- Table to map Address -> Connection.


   package Address_Bags_Bounded_Operations
   is new Address_Bags.Bounded_Operations;
   --
   -- Traversal of Address-orded connections in selected
   -- bounded ranges of Address.


   package Cell_Spec_Bags is new Bags (
      Key_Type  => Processor.Cell_Spec_T,
      Item_Type => Connection_T,
      Key_Of    => Spec_Of,
      "<"       => Processor."<",
      "="       => Processor."=",
      Count     => Natural);
   --
   -- Table to map Cell_Spec -> Connection.
   --
   -- TBM to structure (automaton) for tracking the connections reached
   -- by access paths, depending on the execution point.


   function Shared_Suffix_Length (Left, Right : Scope_T)
   return Natural
   --
   -- Return the largest value L such that the length-L suffixes of
   -- the level-name sequences of Left and Right are equal.
   --
   is
   begin

      return Shared_Suffix_Length (
         Left  => Left.all,
         Right => Right.all);

   end Shared_Suffix_Length;


   type Connection_Tables_T is
      array (Connection_Kind_T) of Connection_Table.Table_Type;
   --
   -- A set of tables symbol->connection, for the several
   -- kinds of connections.


   type Symbol_Table_Object_T is record
      Tables          : Connection_Tables_T;
      Address_Table   : Address_Bags.Bag (Duplicate_Keys_Allowed => True);
      Cell_Spec_Table : Cell_Spec_Bags.Bag (Duplicate_Keys_Allowed => True);
      --TBA: types etc.
   end record;
   --
   -- The symbol tables for one program under analysis.


   Null_Connections : Connection_Set_T (1..0);
   --
   -- The empty set of connections.


   procedure Erase (Table : in out Symbol_Table_T)
   is
   begin

      if Table /= null then

         for T in Table.Tables'range loop

            Connection_Table.Destroy (Table.Tables(T));

         end loop;

         Address_Bags.Destroy (Table.Address_Table);

         Cell_Spec_Bags.Destroy (Table.Cell_Spec_Table);

      else

         Table := new Symbol_Table_Object_T;

      end if;

   end Erase;


   procedure Insert_Qualified (
      Into       : in out Connection_Table.Table_Type;
      Key_A      : in     Level_List_T;
      Conn_A     : in     Connection_T;
      Key_B      : in     Level_List_T;
      Conn_B     : in     Connection_T;
      Min_Suffix : in     Positive);
   --
   -- Handles the collision of two connection-table entries with
   -- some amount of shared key suffix.
   --
   -- It is assumed that the older entry (type Suffix_Key) has already
   -- been removed from the table, and the task is to insert both the
   -- old and the new entry with keys long enough to distinguish them,
   -- and to add Unresolved entries for shorter keys, down to a key
   -- length of Min_Suffix.


   procedure Insert (
      Key        : in     Level_List_T;
      Connection : in     Connection_T;
      Min_Suffix : in     Positive := 1;
      Into       : in out Connection_Table.Table_Type)
   --
   -- Insert a Key and Connection in a table, using the shortest suffix
   -- of the key that unambiguously identifies the entry, but is
   -- at least of length Min_Suffix.
   -- The full Key is used if Min_Suffix >= Key'Length.
   --
   -- If there already is an entry that shares a key suffix with the
   -- new entry, the old entry is updated (reinserted with a longer
   -- suffix).
   --
   -- If the table already contains an entry with the same Key, an
   -- error message is emitted and the table is unchanged.
   --
   is

      First_Suffix_Length : constant Positive :=
         Positive'Min (Min_Suffix, Key'Length);
      -- The first (perhaps only) suffix-length to be tried.

      First : Positive range Key'Range;
      Last  : constant Positive range Key'Range := Key'Last;
      -- The key suffix under consideration is Key(First .. Last).

      Value : Table_Value_T;
      -- The value to be inserted.

      Duplicate : Boolean;
      -- The table already contains an entry with this key (suffix).

      Dup_Value : Table_Value_T;
      -- The value of the old entry.


      procedure Report_Duplicate
      --
      -- Reports that the new Key:Connection defines a symbol with
      -- the same Key as the Dup_Value, which is a Full_Key or
      -- a Suffix_Key.
      --
      is
      begin

         if Equal (Connection, Dup_Value.Connection) then
            -- Duplicated definition with same value.

            Output.Note (
                 "Duplicated symbol and value"
               & Output.Field_Separator
               & Image (Connection));

         elsif Opt.Warn_Duplicated_Symbol then
            -- Duplicated definition with different value, and
            -- user wants to be warned about such.

            Output.Warning (
                 "Duplicated symbol"
               & Output.Field_Separator
               & Image (Connection)
               & Output.Field_Separator
               & Image (Dup_Value.Connection));

         end if;

      end Report_Duplicate;


   begin  -- Insert

      -- The insertion proceeds incrementally, trying to insert
      -- the connection with increasing suffixes of the full key,
      -- by default starting with a minimum suffix length of 1.
      --
      -- When trying to insert with a suffix Ks, and there already
      -- is a table entry E with the key Ks, the following happens:
      --
      -- > If E is "Unresolved", and Ks is not the full K,
      --   the insertion continues with the next longer suffix.
      --   If Ks = K, the "Unresolved" entry is removed and the
      --   connection is inserted with key K as "Full_Key".
      --
      -- > If E is "Full_Key", and Ks is not the full K,
      --   the insertion continues with the next longer suffix.
      --   If Ks = K, there is a key collision; an error
      --   message is emitted and the connection is not inserted.
      --
      -- > If E is "Suffix_Key", let Ke be the full key of E as
      --   computed from its scope and name.
      --   If Ke = K, there is a key collision; an error
      --   message is emitted and the connection is not inserted.
      --   If Ke /= K, the old entry E is removed and the two
      --   entries (old and new) are (re)inserted with sufficiently
      --   long keys by Insert_Qualified, which see.

      if Opt.Trace_Insertion then

         Output.Trace (
              "Inserting symbol """
            & Image (Key)
            & """ for "
            & Image (Connection));

      end if;


      Suffix_Increments:

      for Suffix_Length in First_Suffix_Length .. Key'Length loop

         First := Key'Last - Suffix_Length + 1;

         -- See if we have the full key or a (true) suffix of it:

         if First = Key'First then
            Value := (Kind       => Full_Key,
                      Connection => Connection);
         else
            Value := (Kind       => Suffix_Key,
                      Connection => Connection);
         end if;

         if Opt.Trace_Insertion then

            Output.Trace ("Key suffix " & Image (Key(First..Last)));

         end if;

         -- Try to insert with this part of the key:

         Connection_Table.Insert (
            Table          => Into,
            Key            => Key (First .. Last),
            Value          => Value,
            Duplicate_Item => Duplicate);

         if not Duplicate then

            -- The new connection was inserted, no problem.

            if Opt.Trace_Insertion then

               Output.Trace ("Inserted " & Image (Value));

            end if;

            exit Suffix_Increments;

         else
            -- Inspect the existing connection:

            Dup_Value := Connection_Table.Value (
               Table => Into,
               Key   => Key (First .. Last));

            if Opt.Trace_Insertion then

               Output.Trace ("Duplicate key " & Image (Dup_Value));

            end if;

            case Dup_Value.Kind is

               when Unresolved =>

                  if Value.Kind = Full_Key then

                     -- Replace Unresolved entry with Full_Key entry.

                     Connection_Table.Replace_Value (
                        Table => Into,
                        Key   => Key,
                        Value => Value);

                     exit Suffix_Increments;

                  end if;

               when Full_Key =>

                  if Value.Kind = Full_Key then
                     -- Report duplicate key and quit.

                     Report_Duplicate;

                     return;

                   end if;

               when Suffix_Key =>

                  if Dup_Value.Connection.Scope = Connection.Scope then

                     -- Same scopes, thus same full keys.
                     -- Report duplicate key and quit.

                     Report_Duplicate;

                     return;

                  else

                     -- Keys are distinct but have a common suffix.
                     -- Remove the old entry and (re)insert both the
                     -- old and new entries.

                     Connection_Table.Remove (
                        Table => Into,
                        Key   => Key (First .. Last));

                     Insert_Qualified (
                        Into       => Into,
                        Key_A      => Full_Key (Dup_Value.Connection),
                        Conn_A     => Dup_Value.Connection,
                        Key_B      => Key,
                        Conn_B     => Connection,
                        Min_Suffix => Suffix_Length);

                     exit Suffix_Increments;

                  end if;

            end case;

         end if;

      end loop Suffix_Increments;

   end Insert;


   procedure Insert_Qualified (
      Into       : in out Connection_Table.Table_Type;
      Key_A      : in     Level_List_T;
      Conn_A     : in     Connection_T;
      Key_B      : in     Level_List_T;
      Conn_B     : in     Connection_T;
      Min_Suffix : in     Positive)
   is

      Shared_Len : constant Positive :=
                   Shared_Suffix_Length (Key_A, Key_B);
      --
      -- The length of the shared key suffix.

      Max_Unresolved : constant Natural :=
         Positive'Min (Shared_Len,
         Positive'Min (Key_A'Length - 1,
                       Key_B'Length - 1));
      --
      -- The longest suffix length for which an Unresolved
      -- entry is created (if the key is unused).
      -- The key lengths (less 1) are placed as ceilings to
      -- include the case where one key is a suffix of the
      -- other.

      First : Positive;
      -- The shared suffix under consideration is
      -- Key_B (First .. Key_B'Last).

      Duplicate : Boolean;
      -- An entry already exists for the suffix under consideration.

   begin

      -- Fill in new Unresolved entries:

      for Suffix_Length in Min_Suffix .. Max_Unresolved loop

         First := Key_B'Last - Suffix_Length + 1;

         Connection_Table.Insert (
            Table => Into,
            Key   => Key_B (First .. Key_B'Last),
            Value => (Kind => Unresolved),
            Duplicate_Item => Duplicate);

         -- A value may already exist for this key suffix.
         -- This is acceptable; the purpose of the Unresolved
         -- entries is just be place-holders.

         -- Put ("   Make Unresolved = " & Image(Key_B(First..Key_B'Last)));
         -- if Duplicate then
         --    Put (" (duplicate)");
         -- end if;
         -- New_Line;

      end loop;

      -- Insert the two connections, resolved on the next
      -- scope level:

      Insert (
         Key        => Key_A,
         Connection => Conn_A,
         Min_Suffix => Shared_Len + 1,
         Into       => Into);

      Insert (
         Key        => Key_B,
         Connection => Conn_B,
         Min_Suffix => Shared_Len + 1,
         Into       => Into);

      -- Put_Line ("End of Insert_Qualified.");

   end Insert_Qualified;


   procedure Connect_Subprogram (
      Scope   : in Scope_Stack_T;
      Name    : in String;
      Address : in Processor.Code_Address_T;
      Within  : in Symbol_Table_T)
   is

      Connection : Connection_T (Subprogram);
      -- The new connection (not used).

   begin

      Connect_Subprogram (
         Scope   => To_Scope (Scope),
         Name    => Name,
         Address => Address,
         Within  => Within,
         Giving  => Connection);

   end Connect_Subprogram;


   procedure Connect_Subprogram (
      Scope   : in Scope_T;
      Name    : in String;
      Address : in Processor.Code_Address_T;
      Within  : in Symbol_Table_T)
   is

      Connection : Connection_T (Subprogram);
      -- The new connection (not used).

   begin

      Connect_Subprogram (
         Scope   => Scope,
         Name    => Name,
         Address => Address,
         Within  => Within,
         Giving  => Connection);

   end Connect_Subprogram;


   procedure Connect_Subprogram (
      Scope   : in     Scope_Stack_T;
      Name    : in     String;
      Address : in     Processor.Code_Address_T;
      Within  : in     Symbol_Table_T;
      Giving  :    out Connection_T)
   is
   begin

      Connect_Subprogram (
         Scope   => To_Scope (Scope),
         Name    => Name,
         Address => Address,
         Within  => Within,
         Giving  => Giving);

   end Connect_Subprogram;


   procedure Connect_Subprogram (
      Scope   : in     Scope_T;
      Name    : in     String;
      Address : in     Processor.Code_Address_T;
      Within  : in     Symbol_Table_T;
      Giving  :    out Connection_T)
   is
      use Address_Bags;

   begin

      Giving := (
         Kind    => Subprogram,
         Scope   => Scope,
         Name    => String_Pool.To_Item (Name),
         Address => Address);

      if Opt.Trace then

         Output.Trace (Text =>
              Connection_Kind_T'Image (Subprogram)
            & Output.Field_Separator
            & Image (Full_Key (Giving))
            & Output.Field_Separator
            & Processor.Image (Address));

      end if;

      Insert (
         Key        => Full_Key (Giving),
         Connection => Giving,
         Min_Suffix => 1,
         Into       => Within.Tables(Subprogram));

      Insert (
         Item => Giving,
         Into => Within.Address_Table);

   end Connect_Subprogram;


   procedure Connect_Label (
      Scope   : in Scope_Stack_T;
      Name    : in String;
      Address : in Processor.Code_Address_T;
      Within  : in Symbol_Table_T)
   is

      use Address_Bags;

      Connection : constant Connection_T (Label) := (
         Kind    => Label,
         Scope   => To_Scope (Scope),
         Name    => String_Pool.To_Item (Name),
         Address => Address);

   begin

      if Opt.Trace then

         Output.Trace (Text =>
              Connection_Kind_T'Image (Label)
            & Output.Field_Separator
            & Image (Full_Key (Connection))
            & Output.Field_Separator
            & Processor.Image (Address));

      end if;

      Insert (
         Key        => Full_Key (Connection),
         Connection => Connection,
         Min_Suffix => 1,
         Into       => Within.Tables(Label));

      Insert (
         Item => Connection,
         Into => Within.Address_Table);

   end Connect_Label;


   procedure Connect_Line (
      Scope   : in Scope_Stack_T;
      Number  : in Line_Number_T;
      Address : in Processor.Code_Address_T;
      Within  : in Symbol_Table_T)
   is
   begin

      Connect_Line (
         Scope   => To_Scope (Scope),
         Number  => Number,
         Address => Address,
         Within  => Within);

   end Connect_Line;


   procedure Connect_Line (
      Scope   : in Scope_T;
      Number  : in Line_Number_T;
      Address : in Processor.Code_Address_T;
      Within  : in Symbol_Table_T)
   is

      use Address_Bags;

      Scope_Depth : constant Natural := Scope'Length;

      Connection : constant Connection_T (Line_Number) := (
         Kind         => Line_Number,
         Scope        => Scope,
         Line_Number  => Number,
         Line_Address => Address);

   begin

      if Opt.Trace then

         Output.Trace (Text =>
              Connection_Kind_T'Image (Line_Number)
            & Output.Field_Separator
            & Image (Full_Key (Connection))
            & Output.Field_Separator
            & Processor.Image (Address));

      end if;

      if  Scope_Depth < 1
      or (Scope_Depth < 2 and Opt.Warn_Shallow_Line_Scope)
      then
         -- Short on scope.

         Output.Warning (
             "Shallow scope for line-number "
            & Image (Number)
            & Output.Field_Separator
            & Image (Scope));

      end if;

      if Scope_Depth > 0 then
         -- Some scope.

         Insert (
            Item => Connection,
            Into => Within.Address_Table);

      end if;

   end Connect_Line;


   procedure Connect_Variable (
      Scope    : in Scope_Stack_T;
      Name     : in String;
      Location : in Processor.Cell_Spec_T;
      Within   : in Symbol_Table_T)
   is
   begin

      Connect_Variable (
         Scope    => To_Scope (Scope),
         Name     => Name,
         Location => Location,
         Within   => Within);

   end Connect_Variable;


   procedure Connect_Variable (
      Scope    : in Scope_T;
      Name     : in String;
      Location : in Processor.Cell_Spec_T;
      Within   : in Symbol_Table_T)
   is
      use Cell_Spec_Bags;

      Connection : constant Connection_T (Cell) := (
         Kind     => Cell,
         Scope    => Scope,
         Var_Name => String_Pool.To_Item (Name),
         Location => new Storage.Location_T'(
            Storage.Fixed_Location (Cell => Storage.Cell (Location))));

   begin

      if Opt.Trace then

         Output.Trace (Text =>
              Connection_Kind_T'Image (Cell)
            & Output.Field_Separator
            & Image (Full_Key (Connection))
            & Output.Field_Separator
            & Processor.Image (Location));

      end if;

      -- Insert (Scope, Name) -> Connection.

      Insert (
         Key        => Full_Key (Connection),
         Connection => Connection,
         Min_Suffix => 1,
         Into       => Within.Tables(Cell));

      -- Insert Cell_Spec -> Connection.

      Insert (
          Item => Connection,
          Into => Within.Cell_Spec_Table);

   end Connect_Variable;


   procedure Connect_Variable (
      Scope    : in Scope_Stack_T;
      Name     : in String;
      Location : in Storage.Location_T;
      Within   : in Symbol_Table_T)
   is
      use Cell_Spec_Bags;

      Connection : constant Connection_T (Cell) := (
         Kind     => Cell,
         Scope    => To_Scope (Scope),
         Var_Name => String_Pool.To_Item (Name),
         Location => new Storage.Location_T'(Location));

   begin

      if Opt.Trace then

         Output.Trace (Text =>
              Connection_Kind_T'Image (Cell)
            & Output.Field_Separator
            & Image (Full_Key (Connection))
            & Output.Field_Separator
            & Storage.Image (Location));

      end if;

      -- Insert (Scope, Name) -> Connection.

      Insert (
         Key        => Full_Key (Connection),
         Connection => Connection,
         Min_Suffix => 1,
         Into       => Within.Tables(Cell));

      -- Insert Cell_Spec -> Connection.

      Insert (
          Item => Connection,
          Into => Within.Cell_Spec_Table);

   end Connect_Variable;


   function No_Connections return Connection_Set_T
   is
   begin

      return Null_Connections;

   end No_Connections;


   function Connections (
      Key    : Level_List_T;
      Within : Connection_Table.Table_Type;
      Kind   : String)
   return Connection_Set_T
   --
   -- Look up and return the connections for the Key.
   -- In case of problems, use Kind in messages to report
   -- the kind of element being sought (e.g. "subprogram").
   --
   is

      Value : Table_Value_T;

   begin

      for K in reverse Key'range loop

         Value := Connection_Table.Value (
            Key   => Key (K .. Key'Last),
            Table => Within);

         if Value.Kind = Unresolved then
            -- Repeat loop to use more of the key.

            null;

         else
            -- Resolved entry. Check keys.

            declare
               Value_Key : constant Level_List_T :=
                  Full_Key (Value.Connection);
            begin

               if Is_Suffix (Suffix => Key, Whole => Value_Key) then
                  -- Found it.

                  if Key'Length < Value_Key'Length then

                     -- Note use of abbreviated name.

                     Output.Note (
                          "Full " & Kind & " name"
                        & Output.Field_Separator
                        & Image (Value_Key));

                  end if;

                  return (1 => Value.Connection);

               elsif Value.Kind = Full_Key then
                  -- Repeat loop to use more of the key.

                  null;

               else
                  -- Not there.

                  return Null_Connections;

               end if;

            end;

         end if;

      end loop;

      raise Ambiguous_Name;

   exception

   when Connection_Table.Missing_Item_Error =>

      return Null_Connections;

   end Connections;


   function Subprogram_Connections (
      Scope  : Scope_T;
      Name   : String;
      Within : Symbol_Table_T)
   return Connection_Set_T
   is
   begin

      return Connections (
         Key    => Scope.all
                   & (1 => String_Pool.To_Item (Name)),
         Within => Within.Tables(Subprogram),
         Kind   => "subprogram");

   end Subprogram_Connections;


   function Variable_Connections (
      Scope  : Scope_T;
      Name   : String;
      Within : Symbol_Table_T)
   return Connection_Set_T
   is
   begin

      return Connections (
         Key    => Scope.all
                   & (1 => String_Pool.To_Item (Name)),
         Within => Within.Tables(Variable),
         Kind   => "variable");

   end Variable_Connections;


   function Label_Connections (
      Scope  : Scope_T;
      Name   : String;
      Within : Symbol_Table_T)
   return Connection_Set_T
   is
   begin

      return Connections (
         Key    => Scope.all
                   & (1 => String_Pool.To_Item (Name)),
         Within => Within.Tables(Label),
         Kind   => "label");

   end Label_Connections;


   function Connections_For_Address (
      Address : Processor.Code_Address_T;
      Within  : Symbol_Table_T)
   return Connection_Set_T
   is
      use Address_Bags;

      Connection_List : constant List :=
         Search (
            Key    => Address,
            Within => Within.Address_Table);
   begin

      return Connection_Set_T ( Connection_List );

   end Connections_For_Address;


   function Lines_For_Address (
      Address : Processor.Code_Address_T;
      Within  : Symbol_Table_T)
   return Connection_Set_T
   is

      Conns : constant Connection_Set_T :=
         Connections_For_Address (Address, Within);
      -- All the connections, of any kind.

      Lines : Connection_Set_T (1 .. Conns'Length);
      Last  : Natural := 0;
      -- The line-number connections from Conns, listed
      -- in the same order as Lines(1..Last).

   begin

      for C in Conns'Range loop

         if Conns(C).Kind = Line_Number then

            Last := Last + 1;

            Lines(Last) := Conns(C);

         end if;

      end loop;

      return Lines(1 .. Last);

   end Lines_For_Address;


   function Line_Before (
      Address : Processor.Code_Address_T;
      Within  : Symbol_Table_T)
   return Connection_Set_T
   is

      Line : Connection_T;
      -- The result, if a line connection is found.

      Found_One : exception;
      -- Terminates the traversal when a line connection is found.


      procedure Take_First_Line (Item : in Connection_T)
      --
      -- Traversal action: stores the first line connection in
      -- Line and terminates the traversal.
      --
      is
      begin

         if Item.Kind = Line_Number then
            -- Found one.

            Line := Item;

            raise Found_One;

         end if;

      end Take_First_Line;


      procedure Find_Line_Before
      is new Address_Bags_Bounded_Operations.Upper_Bounded_Traversal (
         Action => Take_First_Line);
      --
      -- Traverses connections in address order, for all connections
      -- with address less or equal to an upper bound, and stores
      -- the first Line_Number connection in Line and terminates
      -- traversal by Found_One. Returns normally if no Line_Number
      -- connection was found.


   begin  -- Line_Before

      Find_Line_Before (
         On_Bag => Within.Address_Table,
         Last   => Address,
         Order  => Address_Bags.Descending);

      -- Found no line at or before Address.

      return Null_Connections;

   exception

   when Found_One =>
      -- Found a Line.

      return (1 => Line);

   end Line_Before;


   function Line_After (
      Address : Processor.Code_Address_T;
      Within  : Symbol_Table_T)
   return Connection_Set_T
   is

      Line : Connection_T;
      -- The result, if a line connection is found.

      Found_One : exception;
      -- Terminates the traversal when a line connection is found.


      procedure Take_First_Line (Item : in Connection_T)
      --
      -- Traversal action: stores the first line connection in
      -- Line and terminates the traversal.
      --
      is
      begin

         if Item.Kind = Line_Number then
            -- Found one.

            Line := Item;

            raise Found_One;

         end if;

      end Take_First_Line;


      procedure Find_Line_After
      is new Address_Bags_Bounded_Operations.Lower_Bounded_Traversal (
         Action => Take_First_Line);
      --
      -- Traverses connections in address order, for all connections
      -- with address greater or equal to a lower bound, and stores
      -- the first Line_Number connection in Line and terminates
      -- traversal by Found_One. Returns normally if no Line_Number
      -- connection was found.


   begin  -- Line_After

      Find_Line_After (
         On_Bag => Within.Address_Table,
         First  => Address,
         Order  => Address_Bags.Ascending);

      -- Found no line at or after Address.

      return Null_Connections;

   exception

   when Found_One =>
      -- Found a Line.

      return (1 => Line);

   end Line_After;


   function Connections_For_Location (
      Location : Processor.Cell_Spec_T;
      Scope    : Scope_T;
      Within   : Symbol_Table_T)
   return Connection_Set_T
   is
      use Cell_Spec_Bags;

      Connections : constant List :=
         Search (
            Key    => Location,
            Within => Within.Cell_Spec_Table);
      -- All connections with the given cell spec.

      Correct_Specs : Connection_Set_T ( 1 .. Connections'Last);
      --
      -- The connections for which scope and cell spec are the same
      -- as the parameters.

      Index : Natural := 0;

   begin

      for C in Connections'Range loop

         if Scope_Of (Connections(C)) = Scope then
            Index := Index + 1;
            Correct_Specs(Index) := Connections(C);
         end if;

      end loop;

      return Correct_Specs (1..Index);

   end Connections_For_Location;


end Symbols;
