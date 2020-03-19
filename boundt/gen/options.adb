-- Options (body)
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
-- $Revision: 1.141 $
-- $Date: 2015/10/27 22:34:32 $
--
-- $Log: options.adb,v $
-- Revision 1.141  2015/10/27 22:34:32  niklas
-- Use Value_Image, not 'Image, for enumerating discrete option values.
--
-- Revision 1.140  2015/10/25 19:03:41  niklas
-- Added procedure Set_Option, which also calls When_Set.
--
-- Revision 1.139  2015/10/24 20:05:50  niklas
-- Moved to free licence.
--
-- Revision 1.138  2014/06/01 10:28:21  niklas
-- Added the Refused exception to signal serious errors
-- in option values.
-- Added the image function Modular_Valued.Type_Alone for
-- better display of certain option values.
-- -
--
-- Revision 1.137  2012-02-12 14:13:19  niklas
-- Added Integer_Valued.Type_Alone, for BT-CH-0229.
--
-- Revision 1.136  2012-01-19 21:33:36  niklas
-- Added Base parameter to Integer_Valued.
-- Added Modular_Valued, also with Base parameter.
--
-- Revision 1.135  2011-09-06 17:40:58  niklas
-- Added "lib" as a default help directory.
--
-- Revision 1.134  2011-09-01 21:29:53  niklas
-- Changed the image of a Discret_Set_Valued option, when an empty
-- set, from "" to "empty by default".
--
-- Revision 1.133  2011-08-31 04:23:34  niklas
-- BT-CH-0222: Option registry. Option -dump. External help files.
--

with Ada.Text_IO;


with Ada.Characters.Handling;
with Ada.Command_Line;
with Ada.Environment_Variables;
with Ada.Strings;
with Ada.Strings.Fixed;
with Ada.Strings.Unbounded;
with File_System;
with Topo_Sort;


package body Options is


   use type Group_Name_T;


   function Trim (Item : String) return String
   is
   begin

      return Ada.Strings.Fixed.Trim (Item, Ada.Strings.Both);

   end Trim;


   --
   ---   Option-value enumerators
   --


   overriding
   function Current_Value (Enum : Not_Enumerable_T) return String
   is
   begin

      return "";

   end Current_Value;


   overriding
   procedure Next (
      Enum  : in out Not_Enumerable_T;
      Ended :    out Boolean)
   is
   begin

      Ended := True;

   end Next;


   --
   ---   Option objects
   --


   function Enumerator (Option : Option_T) return Enumerator_T'Class
   is
   begin

      return Not_Enumerable;

   end Enumerator;


   --
   ---   Common functions
   --


   function To_Internal (Value : String) return String
   --
   -- Removes all leading and trailing blanks and hyphens from Value, and
   -- replaces in the remainder all embedded strings of spaces or hyphens
   -- with single underscores.
   --
   -- This converts the external (user-written) form of an option value
   -- to its internal (Ada enumeration literal) form.
   --
   is

      Result : String (1 .. Value'Length);
      Last   : Natural := 0;
      -- The result is Result(1 .. Last).

   begin

      for V in Value'Range loop

         case Value(V) is

         when ' ' | '-' =>

            if Last > 0 and then Result(Last) /= ' ' then
               -- The first space or hyphen in an embedded (or trailing)
               -- string of spaces or hyphens.

               Last := Last + 1;
               Result(Last) := ' ';
               -- Will later be replaced by '_', or deleted.

            end if;

         when others =>

            if Last > 0 and then Result(Last) = ' ' then
               -- The first non-space, non-hyphen character after
               -- an embedded string of spaces and hyphens.

               Result(Last) := '_';

            end if;

            Last := Last + 1;
            Result(Last) := Value(V);

         end case;

      end loop;

      if Last > 0 and then Result(Last) = ' ' then
         -- This represents a trailing sequence of spaces and hyphens,
         -- to be ignored.

         Last := Last - 1;

      end if;

      return Result(1 .. Last);

   end To_Internal;


   Item_Suffix : constant String := "_item";
   --
   -- A suffix that may be present in the Ada enumeration literals
   -- for option values. This is necessary if the literal would
   -- otherwise be an Ada reserved word.


   function To_External (Value : String) return String
   --
   -- Converts the Value to lower-case and then removes a
   -- possible trailing Item_Suffix.
   --
   -- This converts the internal (Ada enumeration literal) form
   -- of an option value to its (canonical) external form.
   --
   is
      use Ada.Characters.Handling;

      Result : String (1 .. Value'Length) := To_Lower (Value);
      -- The Value in lower-case letters.

      Suffix_Start : constant Integer := Result'Last - Item_Suffix'Length + 1;
      -- The starting index in Result of a possible Item_Suffix.

   begin

      if       Suffix_Start >= Result'First
      and then Result(Suffix_Start .. Result'Last) = Item_Suffix
      then
         -- There is a suffix, so we remove it:

         return Result(1 .. Suffix_Start - 1);

      else
         -- No suffix.

         return Result;

      end if;

   end To_External;


   --   Generic discrete-valued options


   function Discrete_Image (Item : String) return String
   is

      Low_Item : constant String := Ada.Characters.Handling.To_Lower (Item);
      -- The item in lowercase letters.

   begin

      if       Low_Item'Length >= 5
      and then Low_Item(Low_Item'Last - 4 .. Low_Item'Last) = "_item"
      then

         return Low_Item(Low_Item'First .. Low_Item'Last - 5);

      else

         return Low_Item;

      end if;

   end Discrete_Image;


   function Quoted (
      Item : String;
      Quote : Boolean)
   return String
   --
   -- Possible Quote the given Item.
   --
   is
   begin

      if Quote then return '"' & Item & '"';
               else return       Item;
      end if;

   end Quoted;


   package body Discrete_Valued
   is
   
   
      function Image (Item : Value_Type) return String
      is
      begin
      
         if Use_Discrete_Image then
         
            return Discrete_Image (Value_Image (Item));
         
         else
         
            return Value_Image (Item);
            
         end if;
         
      end Image;
   

      overriding
      function Type_And_Default (Option : access Option_T)
      return String
      is
      begin
      
         return Value_Type_Description & ", default "
              & Image (Option.Default);

      end Type_And_Default;

      overriding
      procedure Reset (Option : access Option_T)
      is
      begin

         Option.Value := Option.Default;

      end Reset;


      overriding
      procedure Set (
         Option : access Option_T;
         Value  : in     String)
      is
         Can_Value : constant String := To_Internal (Value);
      begin

         Option.Value := Value_Type'Value (Can_Value);

      exception

      when Constraint_Error =>

         Option.Value := Value_Type'Value (Can_Value & "_Item");

      end Set;

      overriding
      function Enumerator (Option : Option_T) return Enumerator_T'Class
      is
      begin

         if Option.Enumerable then

            return Enumerator_T'Class (Enum_T'(Current => Value_Type'First));

         else

            return Not_Enumerable;

         end if;

      end Enumerator;


      overriding
      function Current_Value (Enum : Enum_T) return String
      is
      begin

         return To_External (Image (Enum.Current));

      end Current_Value;


      overriding
      procedure Next (Enum : in out Enum_T; Ended : out Boolean)
      is
      begin

         Ended := Enum.Current = Value_Type'Last;

         if not Ended then

            Enum.Current := Value_Type'Succ (Enum.Current);

         end if;

      end Next;

   end Discrete_Valued;


   package body Discrete_Set_Valued
   is


      function Image (Item : Item_Type)
      return String
      --
      -- The Item_Image, perhaps thru Discrete_Image.
      --
      is
      begin

         if Use_Discrete_Image then

            return Discrete_Image (Item_Image (Item));

         else

            return Item_Image (Item);

         end if;

      end Image;


      function Image (Set : Set_Type) return String
      --
      -- The images of the items in the Set, separated by commas.
      --
      is
         use Ada.Strings.Unbounded;

         Result : Unbounded_String;
         -- The result to be, initially null.

      begin

         for I in Item_Type loop

            if Set(I) then

               if Length (Result) > 0 then

                  Append (Result, ',');

               end if;

               Append (Result, Image (I));

            end if;

         end loop;

         return To_String (Result);

      end Image;


      overriding
      function Type_And_Default (Option : access Option_T)
      return String
      is

         Default_Items : constant String := Image (Option.Default);
         -- The images of the items in the default set,
         -- separated by commas.

      begin

         if Default_Items'Length > 0 then

            return Value_Type_Description
                 & ", default "
                 & Quoted (Default_Items, Quote_Image);

         else

            return Value_Type_Description
                 & ", empty by default";

         end if;

      end Type_And_Default;


      overriding
      procedure Reset (Option : access Option_T)
      is
      begin

         Option.Value := Option.Default;

      end Reset;


      procedure Update (
         Option  : access Option_T;
         Literal : in     String;
         Member  : in     Boolean)
      is

         Item : Item_Type;
         -- The item identified by the literal.

      begin

         begin

            Item := Item_Type'Value (Literal);

         exception

         when Constraint_Error =>

            Item := Item_Type'Value (Literal & "_Item");

         end;

         Option.Value(Item) := Member;

      end Update;


      overriding
      procedure Set (
         Option : access Option_T;
         Value  : in     String)
      is

         Can_Value : constant String := To_Internal (Value);

         First : Positive := Can_Value'First;
         Comma : Natural;
         -- One of the "item" literals is Can_Value(First .. Comma - 1),
         -- perhaps with a "no_" prefix and without an "_Item" suffix.

      begin

         while First <= Can_Value'Last loop

            Comma := First;

            while Comma <= Can_Value'Last and then Can_Value(Comma) /= ',' loop
               Comma := Comma + 1;
            end loop;

            if First + 2 < Comma
            and then Can_Value(First .. First + 2) = "no_"
            then
               -- An item to be removed from the option value.

               Update (
                  Option  => Option_Class_Ref (Option),
                  Literal => Can_Value(First + 3 .. Comma - 1),
                  Member  => False);

            elsif First < Comma then
               -- An item to be added to the option value.

               Update (
                  Option  => Option_Class_Ref (Option),
                  Literal => Can_Value(First .. Comma - 1),
                  Member  => True);

            -- else
            --    Two consecutive commas, or a comma at the start
            --    of Can_Value. Ignore it.

            end if;

            First := Comma + 1;

         end loop;

      end Set;

      overriding
      function Enumerator (Option : Option_T) return Enumerator_T'Class
      is
      begin

         if Option.Enumerable then

            return Enumerator_T'Class (Enum_T'(Current => Item_Type'First));

         else

            return Not_Enumerable;

         end if;

      end Enumerator;


      overriding
      function Current_Value (Enum : Enum_T) return String
      is
      begin

         return To_External (Image (Enum.Current));

      end Current_Value;


      overriding
      procedure Next (Enum : in out Enum_T; Ended : out Boolean)
      is
      begin

         Ended := Enum.Current = Item_Type'Last;

         if not Ended then

            Enum.Current := Item_Type'Succ (Enum.Current);

         end if;

      end Next;

   end Discrete_Set_Valued;


   Base_Digits : constant array (Natural range 0 .. 15) of Character :=
      "0123456789abcdef";
   --
   -- Numerical digits for bases up to 16.


   package body Integer_Valued
   is

      function Image (Item : Value_Type) return String
      is
      begin

         if Base = 10 then

            return Trim (Value_Type'Image (Item));

         elsif Item < 0 then

            return '-' & Image (abs Item);

         elsif Item < Value_Type'Base (Base) then

            return (1 => Base_Digits(Natural (Item)));

         else

            return Image (Item / Value_Type (Base))
                 & Base_Digits(Natural (Item mod Value_Type (Base)));

         end if;

      end Image;


      function Type_Alone (Option : access Option_T)
      return String
      is
         use Ada.Strings.Unbounded;

         Result : Unbounded_String;
         -- The result, constructed piece by piece.

         Low_Bound, High_Bound : Boolean;
         -- Whether Value_Type is constrained on its low/high
         -- end with respect to its base type.

      begin

         Append (Result, "Integer");

         Low_Bound  := Value_Type'First /= Value_Type'Base'First;
         High_Bound := Value_Type'Last  /= Value_Type'Base'Last ;

         if Low_Bound and High_Bound then
            -- The type is constrained at both ends.

            Append (Result,
                 ' '
               & Image (Value_Type'First)
               & " .. "
               & Image (Value_Type'Last ));

         elsif Low_Bound then
            -- Only the lower bound is constrained.

            Append (Result,
                 " >= "
               & Image (Value_Type'First));

         elsif High_Bound then
            -- Only the upper bound is constrained.

            Append (Result,
                 " <= "
               & Image (Value_Type'Last));

         end if;

         if Base /= 10 then

            Append (Result,
                 ", base"
               & Number_Base_T'Image (Base));

         end if;

         return To_String (Result);

      end Type_Alone;


      overriding
      function Type_And_Default (Option : access Option_T)
      return String
      is
      begin

         return Type_Alone (Option)
              & ", default "
              & Image (Option.Default);

      end Type_And_Default;


      overriding
      procedure Reset (Option : access Option_T)
      is
      begin

         Option.Value := Option.Default;

      end Reset;


      overriding
      procedure Set (
         Option : access Option_T;
         Value  : in     String)
      is
      begin

         if Base = 10 then

            Option.Value := Value_Type'Value (Value);

         else

            Option.Value := Value_Type'Value (
                  Trim (Number_Base_T'Image (Base))
                & '#'
                & Value
                & '#');

         end if;

      end Set;


   end Integer_Valued;


   package body Modular_Valued
   is

      function Image (Item : Value_Type) return String
      is
      begin

         if Base = 10 then

            return Trim (Value_Type'Image (Item));

         elsif Item < 0 then

            return '-' & Image (abs Item);

         elsif Item < Value_Type'Base (Base) then

            return (1 => Base_Digits(Natural (Item)));

         else

            return Image (Item / Value_Type (Base))
                 & Base_Digits(Natural (Item mod Value_Type (Base)));

         end if;

      end Image;


      function Type_Alone (Option : access Option_T)
      return String
      is
         use Ada.Strings.Unbounded;

         Result : Unbounded_String;
         -- The result, constructed piece by piece.

         Low_Bound, High_Bound : Boolean;
         -- Whether Value_Type is constrained on its low/high
         -- end with respect to its base type.

      begin

         Append (Result, "Unsigned integer");

         Low_Bound  := Value_Type'First /= Value_Type'Base'First;
         High_Bound := Value_Type'Last  /= Value_Type'Base'Last ;

         if Low_Bound and High_Bound then
            -- The type is constrained at both ends.

            Append (Result,
                 ' '
               & Image (Value_Type'First)
               & " .. "
               & Image (Value_Type'Last ));

         elsif Low_Bound then
            -- Only the lower bound is constrained.

            Append (Result,
                 " >= "
               & Image (Value_Type'First));

         elsif High_Bound then
            -- Only the upper bound is constrained.

            Append (Result,
                 " <= "
               & Image (Value_Type'Last));

         end if;

         if Base /= 10 then

            Append (Result,
                 ", base"
               & Number_Base_T'Image (Base));

         end if;

         return To_String (Result);

      end Type_Alone;


      overriding
      function Type_And_Default (Option : access Option_T)
      return String
      is
         use Ada.Strings.Unbounded;

         Result : Unbounded_String;
         -- The result, constructed piece by piece.

         Low_Bound, High_Bound : Boolean;
         -- Whether Value_Type is constrained on its low/high
         -- end with respect to its base type.

      begin

         Append (Result, "Integer");

         Low_Bound  := Value_Type'First /= Value_Type'Base'First;
         High_Bound := Value_Type'Last  /= Value_Type'Base'Last ;

         if Low_Bound and High_Bound then
            -- The type is constrained at both ends.

            Append (Result,
                 ' '
               & Image (Value_Type'First)
               & " .. "
               & Image (Value_Type'Last ));

         elsif Low_Bound then
            -- Only the lower bound is constrained.

            Append (Result,
                 " >= "
               & Image (Value_Type'First));

         elsif High_Bound then
            -- Only the upper bound is constrained.

            Append (Result,
                 " <= "
               & Image (Value_Type'Last));

         end if;

         if Base /= 10 then

            Append (Result,
                 ", base"
               & Number_Base_T'Image (Base));

         end if;

         Append (Result,
                ", default "
              & Image (Option.Default));

         return To_String (Result);

      end Type_And_Default;


      overriding
      procedure Reset (Option : access Option_T)
      is
      begin

         Option.Value := Option.Default;

      end Reset;


      overriding
      procedure Set (
         Option : access Option_T;
         Value  : in     String)
      is
      begin

         if Base = 10 then

            Option.Value := Value_Type'Value (Value);

         else

            Option.Value := Value_Type'Value (
                  Trim (Number_Base_T'Image (Base))
                & '#'
                & Value
                & '#');

         end if;

      end Set;


   end Modular_Valued;


   package body Valued
   is

      function Default (Value : Value_Type) return Option_T
      is
      begin

         return (Default => Value, Value => Value);

      end Default;


      overriding
      function Type_And_Default (Option : access Option_T)
      return String
      is
      begin

         return Value_Type_Description
              & ", default "
              & Image (Option.Default);

      end Type_And_Default;


      overriding
      procedure Reset (Option : access Option_T)
      is
      begin

         Option.Value := Option.Default;

      end Reset;


      overriding
      procedure Set (
         Option : access Option_T;
         Value  : in     String)
      is
      begin

         Option.Value := Valued.Value (Value);

      end Set;


   end Valued;


   --
   ---   Option groups
   --


   function Group (S : String) return Group_Name_T
   is
   begin

      return String_Pool.To_Item (S);

   end Group;


   function Name_Of (Group : Group_T) return Group_Name_T
   is
   begin

      return Group.Name;

   end Name_Of;


   function Name_Of (Group : Group_T) return String
   is
   begin

      return String_Pool.To_String (Group.Name);

   end Name_Of;


   function Name_Of (Group : Group_Index_T) return String
   is
   begin

      return String_Pool.To_String (Group_Table(Group).Name);

   end Name_Of;


   function Names_Group (Name : String) return Boolean
   is

      Group_Name : constant Group_Name_T := Group (Name);
      -- The given Name, formatted as an option-group name.

   begin

      for G in First_Group .. Last_Group loop

         if Group_Name_T'(Name_Of (Group_Table(G))) = Group_Name then

            return True;

         end if;

      end loop;

      return False;

   end Names_Group;


   procedure Find_Group (
      Name  : in     String;
      Found :    out Boolean;
      Group :    out Group_T)
   is

      Group_Name : constant Group_Name_T := Options.Group (Name);
      -- The given Name, formatted as an option-group name.

   begin

      Found := False;

      for G in First_Group .. Last_Group loop

         if Group_Name_T'(Name_Of (Group_Table(G))) = Group_Name then

            Found := True;

            Group := Group_Table(G);

            return;

         end if;

      end loop;

   end Find_Group;


   function Priority_Of (Group : Group_Index_T) return Group_Priority_T
   is
   begin

      return Group_Table(Group).Priority;

   end Priority_Of;


   function Image (Item : Group_Set_T) return String
   is
      use Ada.Strings.Unbounded;

      Result : Unbounded_String;
      -- The result, which is the comma-separated list of group names.

   begin

      for I in Item'Range loop

         if Item(I) then

            if Length (Result) > 0 then

               Append (Result, ", ");

            end if;

            Append (Result, Name_Of (Group => I));

         end if;

      end loop;

      return To_String (Result);

   end Image;


   procedure Register_Group (
      Name  : in     Group_Name_T;
      Index :    out Group_Index_T)
   --
   -- Registers a group by this Name in the Group_Table, and
   -- returns its Index.
   --
   is
   begin

      -- Is the group already registered?

      for G in Group_Index_T'First .. Last_Group loop

         if Group_Table(G).Name = Name then
            -- Yes, the group is registered.

            Index := G;

            return;

         end if;

      end loop;

      -- A new group.

      Last_Group := Last_Group + 1;

      Index := Last_Group;

      Group_Table(Index) := (
         Name     => Name,
         Index    => Last_Group,
         Priority => Index);
      --
      -- The Priority will be recomputed later, when the
      -- groups are sorted.

   end Register_Group;


   type Group_Order_T is record
      Higher : Group_Index_T;
      Lower  : Group_Index_T;
   end record;
   --
   -- Expresses the priority order between two groups.


   function Higher (Item : Group_Order_T) return Group_Index_T
   is
   begin

      return Item.Higher;

   end Higher;


   function Lower (Item : Group_Order_T) return Group_Index_T
   is
   begin

      return Item.Lower;

   end Lower;


   type Group_List_T is array (Positive range <>) of Group_Index_T;
   --
   -- A list (sequence) of groups, possibly in a significant order.


   type Group_Order_List_T is array (Positive range <>) of Group_Order_T;
   --
   -- A set of pairwise group priority orderings.


   Max_Group_Orders : constant := 100;
   --
   -- An upper bound on the number of group-order pairs
   -- that can be defined, in order to sort the groups by
   -- priority.


   Num_Group_Orders : Natural := 0;
   --
   -- The actual number of group-order pairs defined so far.


   Group_Orders : Group_Order_List_T (1 .. Max_Group_Orders);
   --
   -- The group-order pairs defined so far are
   -- Group_Orders (1 .. Num_Group_Orders).


   procedure Set_Group_Priority (
      Higher : in Group_Name_T;
      Lower  : in Group_Name_T)
   is

      High, Low : Group_Index_T;
      -- The indices of the two groups.

   begin

      Register_Group (Name => Higher, Index => High);
      Register_Group (Name => Lower , Index => Low );

      Num_Group_Orders := Num_Group_Orders + 1;

      if Num_Group_Orders <= Max_Group_Orders then

         Group_Orders(Num_Group_Orders) := (
            Higher => High,
            Lower  => Low );

      end if;

   end Set_Group_Priority;


   function Sort_Groups is new Topo_Sort (
      Element      => Group_Index_T,
      Pair         => Group_Order_T,
      Element_List => Group_List_T,
      Pair_List    => Group_Order_List_T,
      Lesser       => Higher,
      Greater      => Lower);
   --
   -- Sorts a group list into a linear order that satisfies all
   -- of a given set of group-orders.


   Groups_By_Prio : Group_List_T (1 .. Max_Groups);
   --
   -- The registered groups listed in an order that satisfies all
   -- given pairwise group-orders.
   --
   -- Defined by Finish_Group_Definition. The defined index
   -- range is 1 .. Last_Group - First_Group + 1, or less if there
   -- is a priority cycle.


   procedure Finish_Group_Definition
   --
   -- To be called once after all option groups and their pairwise
   -- priorities are defined.
   --
   -- Computes Groups_By_Prio and sets the Priority of all registered
   -- groups accordingly.
   --
   is

      All_Groups : Group_List_T (1 .. Last_Group - First_Group + 1);
      Num_Groups : Natural := 0;
      -- The list of indices of the registered groups, in order, is
      -- All_Groups(1 .. Num_Groups).

      Num_Orders : Natural;
      -- The number of registered pairwise group orders.

   begin

      -- Check and set Num_Orders:

      if Num_Group_Orders <= Max_Group_Orders then
         -- Good, we have registered all the pairwise orderings.

         Num_Orders := Num_Group_Orders;

      else
         -- Oops, more pairwise orderings than we have room for.

         Num_Orders := Max_Group_Orders;

         Ada.Text_IO.Put_Line (Ada.Text_IO.Standard_Error,
              "Defined"
            & Natural'Image (Num_Group_Orders)
            & " pairwise group orders, using only"
            & Natural'Image (Num_Orders));

      end if;

      -- Get the indices of the registered groups:

      for G in First_Group .. Last_Group loop

         Num_Groups := Num_Groups + 1;
         All_Groups(Num_Groups) := G;

      end loop;

      -- Sort the groups (indices) topologically by the group-orders:

      declare

         Sorted_Groups : constant Group_List_T := Sort_Groups (
            Elements => All_Groups,
            Pairs    => Group_Orders(1 .. Num_Orders));
         -- The indices of all registered groups, in priority order,
         -- unless the group-orders defined a cycle, in which case
         -- some groups are missing from Sorted_Groups.

      begin

         if Sorted_Groups'Length < Num_Groups then

            Ada.Text_IO.Put_Line (Ada.Text_IO.Standard_Error,
                 "Options.Finish_Group_Definition:"
               & "Group priority cycle, only"
               & Natural'Image (Sorted_Groups'Length)
               & " groups sorted, of"
               & Natural'Image (Num_Groups)
               & " total.");

         end if;

         Groups_By_Prio(1 .. Sorted_Groups'Length) := Sorted_Groups;

         -- Define the priority of the groups by their position (index)
         -- in Sorted_Groups:

         for G in 1 .. Sorted_Groups'Length loop

            Group_Table(Sorted_Groups(G)).Priority := G;

            --Ada.Text_IO.Put_Line (
            --    "Group "
            --   & Name_Of (Sorted_Groups(G))
            --   & " has priority"
            --   & Group_Priority_T'Image (G));

         end loop;

      end;

   end Finish_Group_Definition;


   --
   ---   Option table
   --


   function Option (Element : Option_Element_T) return Option_Ref
   is
   begin

      return Element.Option;

   end Option;


   function Name_Of (Element : Option_Element_T) return String
   is
   begin

      return String_Pool.To_String (Element.Real_Name);

   end Name_Of;


   function Is_Synonym (Element : Option_Element_T) return Boolean
   is
   begin

      return Element.Name /= Element.Real_Name;

   end Is_Synonym;


   function Prefixed (
      Prefix : String;
      Item   : String)
   return String
   is
   begin

      if Prefix'Length > 0 then

         return Prefix & Prefix_Sep & Item;

      else

         return Item;

      end if;

   end Prefixed;


   function Imp_Item (Item : String) return String
   is
   begin

      return Prefixed ("imp", Item);

   end Imp_Item;


   function Trace_Item (Item : String) return String
   is
   begin

      return Prefixed ("trace", Item);

   end Trace_Item;


   function Warn_Item (Item : String) return String
   is
   begin

      return Prefixed ("warn", Item);

   end Warn_Item;


   function Prefix_Of (Prefixed_Name : String) return String
   is

      Sep : constant Natural := Ada.Strings.Fixed.Index (
         Source  => Prefixed_Name,
         Pattern => (1 => Prefix_Sep));
      -- The index of the first Prefix_Sep in Prefixed_Name,
      -- or zero if there is no Prefix_Sep there.

   begin

      return Prefixed_Name(Prefixed_Name'First .. Sep - 1);

   end Prefix_Of;


   function Item_Of (Prefixed_Name : String) return String
   is

      Sep : constant Positive := Ada.Strings.Fixed.Index (
         Source  => Prefixed_Name,
         Pattern => (1 => Prefix_Sep));
      -- The index of the first Prefix_Sep in Prefixed_Name,
      -- or Constraint_Error if there is no Prefix_Sep there.

   begin

      return Prefixed_Name(Sep + 1 .. Prefixed_Name'Last);

   end Item_Of;


   procedure Register (
      Option  : in Option_Ref;
      Name    : in String;
      Synonym : in String         := "";
      Groups  : in Groups_T       := No_Groups;
      Reset   : in Reset_Action_T := No_Action'access;
      Set     : in Set_Action_T   := No_Action'access)
   is

      Option_Name : constant Option_Name_T := Options.Option_Name (Name);
      -- The internal form of the Name.

      Synonym_Name : Option_Name_T;
      -- The internal form of the Synonym, if not null.

      Group_Set : Group_Set_T := (others => False);
      -- The set of Groups.

      Group_Index : Group_Index_T;
      -- The index of one of the Groups.

   begin

      for G in Groups'Range loop

         Register_Group (
            Name  => Groups(G),
            Index => Group_Index);

         Group_Set(Group_Index) := True;

      end loop;

      Option_Maps.Insert (
         Container => Option_Table,
         Key       => Option_Name,
         New_Item  => (
            Option     => Option,
            Name       => Option_Name,
            Real_Name  => Option_Name,
            Groups     => Group_Set,
            Enabled    => True,            -- TBM!
            When_Reset => Reset,
            When_Set   => Set));

      if Synonym /= "" then

         Synonym_Name := Options.Option_Name (Synonym);

         Option_Maps.Insert (
            Container => Option_Table,
            Key       => Synonym_Name,
            New_Item  => (
               Option     => Option,
               Name       => Synonym_Name,
               Real_Name  => Option_Name,
               Groups     => Group_Set,
               Enabled    => True,            -- TBM!
               When_Reset => Reset,
               When_Set   => Set));

      end if;

   exception

   when Constraint_Error =>
      -- Duplicated Name of Option.

      Ada.Text_IO.Put_Line (Ada.Text_IO.Standard_Error,
           "Duplicated option name "
          & Name);

   end Register;


   procedure Register (
      Option  : in Option_Ref;
      Name    : in String;
      Synonym : in String         := "";
      Group   : in Group_Name_T;
      Reset   : in Reset_Action_T := No_Action'access;
      Set     : in Set_Action_T   := No_Action'access)
   is
   begin

      Register (
         Option  => Option,
         Name    => Name,
         Synonym => Synonym,
         Groups  => (1 => Group),
         Reset   => Reset,
         Set     => Set);

   end Register;


   procedure Find_Option (
      Name   : in     String;
      Found  :    out Boolean;
      Option :    out Option_Element_T)
   is
      use Option_Maps;

      Here : constant Cursor := Find (Option_Table, Option_Name (Name));
      -- Designates the table element for this option, if any.

   begin

      Found := Here /= No_Element;

      if Found then

         Option := Element (Here);

      end if;

   end Find_Option;


   function Option (Name : String) return Option_Ref
   is
      use Option_Maps;

      Here : constant Cursor := Find (Option_Table, Option_Name (Name));
      -- Designates the table element for this option, if any.

   begin

      if Here = No_Element then

         return null;

      else

         return Element (Here).Option;

      end if;

   end Option;


   function Name_Of (Option : access Option_T) return String
   is
      use Option_Maps;

      Here : Cursor := First (Option_Table);

   begin

      loop
         exit when Here = No_Element;
         exit when Element (Here).Option = Option;
         Next (Here);
      end loop;

      return Name_Of (Element (Here));

   end Name_Of;


   procedure Reset (Option : in Option_Element_T)
   is
   begin

      Reset (Option.Option);

      Option.When_Reset (Option.Option);

   end Reset;


   procedure Set (
      Option : in Option_Element_T;
      Value  : in String)
   is
   begin
               
      Set (Option.Option, Value);

      Option.When_Set (Option.Option, Value);

   end Set;


   procedure Set_Option (
      Name  : in String;
      Value : in String)
   is
   
      Found  : Boolean;
      Option : Option_Element_T;
      -- The option with this Name, if Found.
   
   begin
   
      Find_Option (
         Name   => Name,
         Found  => Found,
         Option => Option);
         
      if not Found then
         -- Oh oh.
         
         raise Program_Error;
         
      else
      
         Set (
            Option => Option,
            Value  => Value);
            
      end if;
   
   end Set_Option;


   procedure Finish_Definition
   is
   begin

      Finish_Group_Definition;

   end Finish_Definition;


   --
   ---   Usage and option descriptions ("help" system)
   --


   Description_Root : Ada.Strings.Unbounded.Unbounded_String;
   --
   -- The root of the directory path to the directories that
   -- contain option description files ("help" files). Defined
   -- by an environment variable.


   Bound_T_Help : constant String := "BOUNDT_HELP";
   --
   -- The (name of the) environment variable that defines the
   -- value of Description_Root.


   Description_Paths : Ada.Strings.Unbounded.Unbounded_String;
   --
   -- The list of paths to directories that contain option description
   -- files ("help" files). There may be zero, one, or several paths,
   -- separated by the Path_Separator character.


   Path_Separator : constant Character := ';';
   --
   -- The character that separates paths within Description_Paths.


   procedure Add_Help (
      Path     : in String;
      Relative : in Boolean := True)
   is
      use Ada.Strings.Unbounded;

      New_Path : Unbounded_String;
      -- The path to be added.

   begin

      if Relative then

         New_Path := To_Unbounded_String (File_System.Path_Name (
            Path => To_String (Description_Root),
            File => Path));

      else

         New_Path := To_Unbounded_String (Path);

      end if;

      if Length (Description_Paths) = 0 then
         -- This is the first Path.

         Description_Paths := New_Path;

      else
         -- This is an additional Path.
         -- We add it at the front of the paths so that a general
         -- option can be given a more specific description for a
         -- known target or host system.

         Description_Paths := New_Path & Path_Separator & Description_Paths;

      end if;

   end Add_Help;


   procedure Add_Help (
      Path, Subpath : in String;
      Relative      : in Boolean := True)
   is
   begin

      Add_Help (File_System.Path_Name (Path, Subpath), Relative);

   end Add_Help;


   function All_Options return Option_List_T
   is
      use Option_Maps;

      List : Option_List_T (1 .. Integer (Length (Option_Table)));
      Last : Natural := 0;
      -- The result is List(1 .. Last).

      Opt : Cursor := First (Option_Table);
      -- Traversing the option table in alphabetical order.

   begin

      while Opt /= No_Element loop

         Last := Last + 1;
         List(Last) := Element (Opt);

         Next (Opt);

      end loop;

      return List(1 .. Last);

   end All_Options;


   function First_Option (With_Prefix : in String)
   return Option_Maps.Cursor
   is
      use Option_Maps;

      Cur : Cursor := Ceiling (
         Container => Option_Table,
         Key       => Option_Name (Prefixed (
            Prefix => With_Prefix,
            Item   => "")));
      -- The first option with a name >= With_Prefix & '.' & "".

   begin

      if Prefix_Of (Name_Of (Element (Cur))) = With_Prefix then

         return Cur;

      else

         return Option_Maps.No_Element;

      end if;

   end First_Option;


   function Last_Option (With_Prefix : in String)
   return Option_Maps.Cursor
   is
      use Option_Maps;

      Cur : Cursor := Floor (
         Container => Option_Table,
         Key       => Option_Name (Prefixed (
            Prefix => With_Prefix,
            Item   => (1 => Character'Last))));
      -- The last option with a name <= With_Prefix & '.' & some item
      -- where the first character of the item is not Character'Last.

   begin

      if Prefix_Of (Name_Of (Element (Cur))) = With_Prefix then

         return Cur;

      else

         return Option_Maps.No_Element;

      end if;

   end Last_Option;


   function All_Options (With_Prefix : in String)
   return Option_List_T
   is
      use Option_Maps;

      List : Option_List_T (1 .. Integer (Length (Option_Table)));
      Last : Natural := 0;
      -- The result is List(1 .. Last).

      Cur : Cursor := First_Option (With_Prefix);
      -- The first option with the given prefix.

      Fin : Cursor := Last_Option (With_Prefix);
      -- The last option with the given prefix.

   begin

      loop

         exit when Cur = No_Element;

         Last := Last + 1;
         List(Last) := Element (Cur);

         exit when Cur = Fin;

         Next (Cur);

      end loop;

      return List(1 .. Last);

   end All_Options;


   function Options_In (Group : Group_T)
   return Option_List_T
   is
      use Option_Maps;

      List : Option_List_T (1 .. Integer (Length (Option_Table)));
      Last : Natural := 0;
      -- The result is List(1 .. Last).

      Opt : Cursor := First (Option_Table);
      -- Traversing the option table in alphabetical order.

   begin

      while Opt /= No_Element loop

         if Element(Opt).Groups(Group.Index) then

            Last := Last + 1;
            List(Last) := Element (Opt);

         end if;

         Next (Opt);

      end loop;

      return List(1 .. Last);

   end Options_In;


   --
   ---   Help files describing options and option groups
   --


   procedure Open_Description (
      Name : in     String;
      Form : in     String;
      File : in out Ada.Text_IO.File_Type)
   is
      use Ada.Strings.Fixed;
      use Ada.Strings.Unbounded;

      Very_Last : constant Natural := Length (Description_Paths);
      -- The end of the Description Paths.

      First : Positive := 1;
      -- The start of the next path in Description_Paths.

   begin

      loop

         declare

            Sep : constant Natural := Index (
               Source  => Description_Paths,
               Pattern => (1 => Path_Separator),
               From    => First);
            -- The index in Description_Paths of the separator that
            -- terminates the present path within Description_Paths,
            -- or zero if there is none.

            Last : Natural;
            -- The end of the present path in Description_Paths.

         begin

            if Sep = 0 then
               -- This is the last path in Description_Paths.

               Last := Very_Last;

            else

               Last := Sep - 1;

            end if;

            Ada.Text_IO.Open (
               File => File,
               Mode => Ada.Text_IO.In_File,
               Name => File_System.Path_Name (
                  Path => Slice (Description_Paths, First, Last),
                  File => Name & '.' & Form));

            -- Success!

            exit;

         exception

         when Ada.Text_IO.Name_Error
            | Ada.Text_IO.Use_Error =>

            -- File not found with this path.
            -- Look for the next path in Path.

            First := Last + 2;

            while    First <= Very_Last
            and then Element (Description_Paths, First) = Path_Separator
            loop
               First := First + 1;
            end loop;

            if First > Very_Last then
               -- No more paths to try.

               raise;

            end if;

         end;

      end loop;

   end Open_Description;


   function Group_Description_File (Group : Group_T) return String
   is
   begin

      return String'(Name_Of (Group)) & "-group";

   end Group_Description_File;


begin  -- Options

   if Ada.Environment_Variables.Exists (Bound_T_Help) then

      Description_Root := Ada.Strings.Unbounded.To_Unbounded_String (
         Ada.Environment_Variables.Value (Bound_T_Help));

      Add_Help ("gen");
      -- The subdirectory for general options.

      Add_Help ("lib");
      -- The subdirectory for options defined in various
      -- libraries that may or may not be included in a
      -- given version of Bound-T.

   else

      Ada.Text_IO.Put_Line (Ada.Text_IO.Standard_Error,
           "Environment variable "
         & Bound_T_Help
         & " is not defined, option -help will not work.");

   end if;

end Options;
