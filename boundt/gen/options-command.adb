-- Options.Command (body)
--
-- Command option handling.
-- Author: WCET team, Space Systems Finland, 1999.
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
-- $Revision: 1.4 $
-- $Date: 2015/10/24 20:05:49 $
--
-- $Log: options-command.adb,v $
-- Revision 1.4  2015/10/24 20:05:49  niklas
-- Moved to free licence.
--
-- Revision 1.3  2012-02-25 19:42:51  niklas
-- Extended Describe (Option, Value) to compare the Values in a
-- case-insensitive way.
--
-- Revision 1.2  2011-09-01 13:05:48  niklas
-- Decoder.Opt.Handle_Wild_Option replaces Choose_Device.
-- Added Version_Id to "All option groups" header.
--
-- Revision 1.1  2011-08-31 04:17:13  niklas
-- Added for BT-CH-0222: Option registry. Option -dump. External help files.
--


with Ada.Characters.Handling;
with Ada.Command_Line;
with Ada.Text_IO;
with Ada.Strings.Fixed;
with Ada.Strings.Unbounded;
with Decoder.Opt;
with Help_Opt;
with Option_Strings;
with Options.Bool;
with Options.Groups;
with Options.Traversal;
with Output;
with Version_Id;

-- For legacy options:

with Calculator.Opt;
with ILP.Opt;


package body Options.Command is

   use Ada.Command_Line;
   use Ada.Text_IO;


   --
   ---  Legacy option combinations
   --


   Keep_Files : aliased Options.Bool.Option_T (Default => False);
   --
   -- The option "-keep_files" is a combination of "-keep_lp" and "-keep_om".


   procedure When_Keep_Files_Reset (Option : in Option_Ref)
   --
   -- Implements post-Reset actions for "-keep_files".
   --
   is
   begin

      Calculator.Opt.Keep_Files := Calculator.Opt.Keep_Files_Opt.Default;

      ILP.Opt.Keep_Files := ILP.Opt.Keep_Files_Opt.Default;

      Keep_Files.Value := Keep_Files.Default;

   end When_Keep_Files_Reset;


   procedure When_Keep_Files_Set (
      Option : in Option_Ref;
      Value  : in String)
   --
   -- Implements post-Set actions for "-keep_files".
   --
   is
   begin

      Calculator.Opt.Keep_Files := Keep_Files.Value;

      ILP.Opt.Keep_Files := Keep_Files.Value;

   end When_Keep_Files_Set;


   --
   ---   Handling command-line options
   --


   function Without_Leading_Hyphen (Item : String) return String
   --
   -- The given string without a possible leading '-'.
   --
   is
   begin

      if Item(Item'First) = '-' then

         return Item(Item'First + 1 .. Item'Last);

      else

         return Item;

      end if;

   end Without_Leading_Hyphen;


   function Is_Known_Prefix (Item : String) return Boolean
   --
   -- Whether this is a known option prefix.
   --
   is
   begin

      return Item = "draw"
          or Item = "imp"
          or Item = "trace"
          or Item = "warn";

   end Is_Known_Prefix;


   function Begins_With (Prefix, Whole : String) return Boolean
   --
   -- Whether the Whole string begins with the Prefix.
   --
   is
   begin

      return Whole'Length >= Prefix'Length
      and then Whole(Whole'First .. Whole'First + Prefix'Length - 1) = Prefix;

   end Begins_With;


   function Is_Negated (Arg : String) return Boolean
   --
   -- Whether the Arg is (or seems to be) a negated Boolean option
   -- or a negated item in the value of a prefixed option.
   --
   is
   begin

      if Arg(Arg'First) = '-' then

         return  Begins_With ("-no_", Arg)
         or else Begins_With ("-no-", Arg);

      else

         return  Begins_With ("no_", Arg)
         or else Begins_With ("no-", Arg);

      end if;

   end Is_Negated;


   function Post_Negation (Arg : String) return String
   --
   -- Assuming that the Arg Is_Negated, the rest of the Arg,
   -- after the negation.
   --
   is
   begin

      if Arg(Arg'First) = '-' then

         return Ada.Strings.Fixed.Tail (Arg, Arg'Length - 4);

      else

         return Ada.Strings.Fixed.Tail (Arg, Arg'Length - 3);

      end if;

   end Post_Negation;


   function Is_Boolean (Element : Option_Element_T) return Boolean
   --
   -- Whether this is a Boolean-valued option.
   --
   is
   begin

      return Element.Option.all in Bool.Option_T'Class;

   end Is_Boolean;


   procedure Set_Option (
      Element : in Options.Option_Element_T;
      Name    : in Ada.Strings.Unbounded.Unbounded_String;
      Value   : in Ada.Strings.Unbounded.Unbounded_String)
   --
   -- Sets the option Element, with the given Name, to the given Value.
   -- If the Value is not valid for this option, reports an Error
   -- and propagates Argument_Error.
   --
   is
      use Ada.Strings.Unbounded;
   begin

      Options.Set (
         Option => Element,
         Value  => To_String (Value));
      -- May propagate Constraint_Error.

   exception

   when Constraint_Error =>

      Output.Error (
           "Illegal option value"
         & Output.Field_Separator
         & To_String (Name)
         & Output.Field_Separator
         & To_String (Value));

      raise Argument_Error;

   end Set_Option;


   procedure Handle_Atomic_Option (
      Prefix : in String;
      Item   : in String)
   --
   -- Handles an option that is contained entirely in one Item and
   -- may be a Prefixed option (if not, the Prefix is "").
   -- Thus, the option (Item) has one of the forms:
   --
   --   1.  name         (for a Boolean option, set True)
   --   2.  no_name      (for a Boolean option, set False)
   --   3.  name=value   (for any kind of option, set to "value").
   --
   -- If the option is Prefixed, the name of the option is
   -- really Prefix & Prefix_Set & "name", otherwise the name
   -- is just the "name".
   is
      use Ada.Strings.Fixed;
      use Ada.Strings.Unbounded;

      Equal : constant Natural := Index (Source => Item, Pattern => "=");
      -- The index of the first "=" in the Item, else zero.

      Name, Value : Unbounded_String;
      -- The (putative) name and value of the option.

      Found : Boolean;
      -- Whether an option with the prefixed name was found.

      Element : Options.Option_Element_T;
      -- The option, if Found.

   begin

      if Equal > 0 then
         -- Form 3.

         Name := To_Unbounded_String (Options.Prefixed (
            Prefix => Prefix,
            Item   => Item(Item'First .. Equal - 1)));

         Value := To_Unbounded_String (Item(Equal + 1 .. Item'Last));

      else
         -- Form 1 or 2.

         Name := To_Unbounded_String (Options.Prefixed (Prefix, Item));

      end if;

      Options.Find_Option (
         Name   => To_String (Name),
         Found  => Found,
         Option => Element);

      if Equal = 0 then

         if Found then
            -- Form 1.

            Value := To_Unbounded_String ("True");

         elsif Is_Negated (Item) then
            -- Perhaps Form 2.

            Name := To_Unbounded_String (
               Options.Prefixed (Prefix, Post_Negation (Item)));

            Options.Find_Option (
               Name   => To_String (Name),
               Found  => Found,
               Option => Element);

            if Found then

               Value := To_Unbounded_String ("False");

            end if;

         end if;

      end if;

      if Found then

         Set_Option (
            Element => Element,
            Name    => Name,
            Value   => Value);

      else

         Output.Error (
              "Unrecognized ""-"
            & Prefix
            & """ item"
            & Output.Field_Separator
            & Item);

         raise Argument_Error;

      end if;

   end Handle_Atomic_Option;


   procedure Handle_Prefixed_Option (
      Prefix : in String;
      Value  : in String)
   --
   -- Handles a set of Boolean options with a syntax that
   -- consists of a Prefix name, such as "trace", followed by
   -- a Value that is a comma-separated list of "item names",
   -- possibly prefixed with "no_" or "no-".
   --
   -- The actual options are named as the prefix & Prefix_Sep & item.
   --
   is
      use Ada.Strings.Fixed;
      use Ada.Strings.Unbounded;

      Next : Positive := Value'First;
      -- The next part of Value to be inspected in Value(Next..).

      Comma : Natural;
      -- The position of the first comma in Value(Next..).

   begin

      while Next <= Value'Last loop

         Comma := Index (Source => Value(Next .. Value'Last), Pattern => ",");

         if Comma = 0 then
            -- No more commas in Value.

            Comma := Value'Last + 1;

         end if;

         if Next < Comma then
            -- A non-null item.

            Handle_Atomic_Option (
               Prefix => Prefix,
               Item   => Value(Next .. Comma - 1));

         end if;

         Next := Comma + 1;

      end loop;

   end Handle_Prefixed_Option;


   procedure Handle_Option (
      Number      : in     Positive;
      Arg         : in     String;
      Took_Action : in out Boolean;
      Next_Number :    out Positive)
   --
   -- New, general form of Handle_Argument.
   -- Handles Arg, which is argument number Number and seems to
   -- be an option because it starts with '-' and is not just "-".
   -- Processes the Arg (including perhaps following arguments giving
   -- option values) and returns Next_Number > Number as the number
   -- of the next argument (not part of this option Arg).
   -- If the option causes some real action to be taken (for example,
   -- some output) sets Took_Action to True.
   -- Raises Argument_Error for option syntax errors.
   --
   is
      use Ada.Strings.Fixed;
      use Ada.Strings.Unbounded;

      Equal : constant Natural := Index (Source => Arg, Pattern => "=");
      -- The index of the first "=" in the Arg, else zero.

      Element : Options.Option_Element_T;
      -- The option table element for this option, if found.

      Found : Boolean;
      -- Whether the Element was found.

      Valid : Boolean;
      -- Whether the Arg is a valid option, in some subordinate
      -- parser's opinion.

      Name, Value : Unbounded_String;
      -- The (putative) name and value of the option.


      procedure Next_Is_Value
      --
      -- The next argument is the Value of the option.
      --
      is
      begin

         if Next_Number <= Argument_Count then

            Value := To_Unbounded_String (Argument (Next_Number));

            Next_Number := Next_Number + 1;

         else
            -- Missing value.

            Output.Error (
                 "Missing value for option"
               & Output.Field_Separator
               & Arg);

            raise Argument_Error;

         end if;

      end Next_Is_Value;


   begin  -- Handle_Option

      Next_Number := Number + 1;

      -- We consider eight cases (forms of option) to find
      -- the Name and Value of the option:
      --
      -- 1. -<name of Boolean option>
      --
      --    This is a Boolean option to be turned on (True value).
      --
      -- 2. -no_<name of Boolean option>
      --
      --    This is a Boolean option to be turned off (False value).
      --
      -- 3. -<name of non-Boolean option>   <value>
      --
      --    The value of the option is the next argument after Arg.
      --
      -- 4. -<name of option>=<value>
      --
      --    The Arg argument gives both the name and the value of
      --    the option, separated by an equals sign. The option can
      --    have any type, including Boolean.
      --
      -- 5. -<prefix>   <value>
      --
      --    The Arg argument is a "prefix", such as "-trace", to be
      --    followed in the next argument by a value that is a list
      --    of items that combine with the prefix to give the names
      --    of the (Boolean) options. The prefix itself is not the
      --    name of any option.
      --
      -- 6. -<prefix>=<value>
      --
      --    As in case 5, but here the Arg gives both the prefix
      --    and the value (list of items).
      --
      -- 7. -<something>
      --
      --    Special cases, for example when Arg is directly the
      --    name of a target device, but the user was lazy and
      --    omitted the "-device=" part.


      -- Perhaps the entire Arg (after the leading '-') is an option?

      Name := To_Unbounded_String (Tail (Arg, Arg'Length - 1));

      Options.Find_Option (To_String (Name), Found, Element);

      if Found then
         -- The whole Arg (except the leading '-') is an option name.

         if Is_Boolean (Element) then
            -- Case 1: a Boolean option, to be turned on (enabled).

            Value := To_Unbounded_String ("True");

         else
            -- Case 3: the next argument sets the value of the option.

            Next_Is_Value;

         end if;

      else
         -- The whole Arg (after the leading '-') is not an option name.
         -- Perhaps it is a Boolean option with a "-no" prefix?

         if Is_Negated (Arg) then

            Name := To_Unbounded_String (Post_Negation (Arg));

            Options.Find_Option (To_String (Name), Found, Element);

            if Found and then Is_Boolean (Element) then
               -- Case 2: a Boolean option, to be turned off (disabled).

               Value := To_Unbounded_String ("False");

            end if;

         else
            -- The Arg does not have a "-no" prefix.
            -- Perhaps it is of the form "-name=value"?

            if Equal > Arg'First + 1 then
               -- The Arg can be divided into name=value.

               Name  := To_Unbounded_String (Arg(Arg'First + 1 .. Equal - 1));
               Value := To_Unbounded_String (Arg(Equal + 1 .. Arg'Last));

               Options.Find_Option (To_String (Name), Found, Element);

            elsif Equal > 0 then
               -- The Arg begins "-=...". Barf.

               Output.Error (
                    "Missing option name"
                  & Output.Field_Separator
                  & Arg);

               raise Argument_Error;

            end if;

         end if;

      end if;

      -- Did we succeed?

      if Found then
         -- Yep, we have an option and a value.

         Set_Option (
            Element => Element,
            Name    => Name,
            Value   => Value);

      else
         -- Nope, this option is not recognized.

         -- Perhaps a "prefixed" option? It can be a single
         -- argument (-prefix=value) or two arguments (-prefix value).

         if Equal = 0 then
            -- The value (if any) follows in the next argument.
            -- This argument is the prefix only.

            Name := To_Unbounded_String (Arg(Arg'First + 1 .. Arg'Last));

         else
            -- This argument is "-prefix=value".

            Name  := To_Unbounded_String (Arg(Arg'First + 1 .. Equal - 1));

         end if;

         if Is_Known_Prefix (To_String (Name)) then
            -- A prefix option.

            if Equal = 0 then
               -- The value is in the next argument (if any).
               -- Case 5.

               Next_Is_Value;

            else
               -- The value follows the equal sign in this argument.
               -- Case 6.

               Value := To_Unbounded_String (Arg(Equal + 1 .. Arg'Last));

            end if;

            Handle_Prefixed_Option (
               Prefix => To_String (Name),
               Value  => To_String (Value));

         else
            -- Not a prefix option, nor any general option.
            -- Case 7.

            Decoder.Opt.Handle_Wild_Option (
               Name  => Option_Strings.Undashed (Arg),
               Valid => Valid);

            if not Valid then

               Output.Error (
                    "Unrecognized option"
                  & Output.Field_Separator
                  & Arg);

               raise Argument_Error;

            end if;

         end if;

      end if;

   end Handle_Option;


   procedure Handle_Help (Next : in Positive);
   --
   -- Handles the arguments to a "-help" argument.
   -- The first argument after the "-help" is number Next.


   procedure Get_From_Command (
      Took_Action   : out Boolean;
      Next_Argument : out Positive)
   is

      N : Positive;
      -- The number of the current argument (option).

   begin

      -- Scan the command options:

      Took_Action := False;

      Next_Argument := 1;

      Option_Arguments : loop

         exit Option_Arguments when Next_Argument > Argument_Count;

         N := Next_Argument;

         declare

            Arg : constant String := Argument (N);

         begin

            if Arg = "-help" then
               -- This will consume (or reject) all the
               -- rest of the arguments.

               Help_Opt.Add_Help_Dirs;

               Handle_Help (Next => N + 1);

               Next_Argument := Argument_Count + 1;
               Took_Action   := True;

               exit Option_Arguments;

            elsif Arg'Length > 1 and then Arg(Arg'First) = '-' then
               -- This seems to be an option.

               Handle_Option (
                  Number      => N,
                  Arg         => Arg,
                  Took_Action => Took_Action,
                  Next_Number => Next_Argument);

            else
               -- Seems that the options have ended.

               exit Option_Arguments;

            end if;

         end;

      end loop Option_Arguments;

   exception

   when Argument_Error =>

      Advise_Help;

      raise;

   end Get_From_Command;


   --
   ---   Describing command-line options
   --


   procedure Advise_Help
   is
   begin

      Output.Error ("Use -help for help.");

   end Advise_Help;


   procedure Begin_Col (Column : in Positive_Count)
   --
   -- Sets the output cursor to the given Column, but also
   -- ensures that there is at least on blank space before
   -- the following text, if not at the start of the line.
   --
   is
   begin

      if Column > 1 then

         Set_Col (Column - 1);
         Put (' ');

      else

         Set_Col (Column);

      end if;

   end Begin_Col;


   procedure Describe (
      Stuff  : in String;
      Margin : in Positive_Count)
   --
   -- Prints the description of the Stuff on standard output,
   -- taking it from the "description" file Stuff & ".txt".
   -- Each line of text is started from the given left Margin.
   -- Any sequence of null lines is printed as one null line,
   -- except that trailing null lines are omitted completely.
   --
   is

      Description : File_Type;
      -- The description file (text).

      Line : String (1 .. 1000);
      Last : Natural;
      -- A line from the description file.

      Null_Line : Boolean := False;
      -- Whether we last saw one or several null lines.

   begin

      Open_Description (
         Name => Stuff,
         Form => "txt",
         File => Description);

      loop

         Get_Line (Description, Line, Last);

         if Last < 1 then

            Null_Line := True;

         else

            if Null_Line then

               New_Line;

               Null_Line := False;

            end if;

            Begin_Col (Margin);

            Put_Line (Line(1 .. Last));

         end if;

      end loop;

   exception

   when Name_Error =>

      Begin_Col (Margin);

      Put_Line ("(no description of """ & Stuff & """ found)");

   when End_Error =>

      if Is_Open (Description) then

         Close (Description);

      end if;

   end Describe;


   --
   ---   Describing all the options on standard output
   --


   -- The options are traversed and described in alphabetical order,
   -- without regard to groups, except that for the prefixed options
   -- the options (items) for each prefix are traversed and described
   -- hierarchically by groups.


    package Margins
    -- Left margins (start columns) for the output.
    is

       First : constant Positive_Count := 1;
       -- The first column on a line.

       Indent : constant Positive_Count := 3;
       -- Indentation between levels.

       Basic : constant Positive_Count := First + Indent;
       -- Descriptions of basic (non-prefixed) options.

       Value_Text_Offset : constant Positive_Count := 10;
       -- Additional offset from the (display of) an option's value,
       -- to the description of this value.

   end Margins;


   Value_Separator : constant Character := ',';
   --
   -- The text file that describes the value V of an option A is
   -- named A & Value_Separator & V & ".txt".
   --
   -- It would be nicer to use an equals sign '=' but this interacts
   -- badly with "bash" file-name completion, for some reason not
   -- known to me.


   procedure Describe_Values (
      Option : in Option_Element_T;
      Margin : in Positive_Count)
   --
   -- If the Option has enumerable values, describes each value.
   --
   is

      Full_Name : constant String := Name_Of (Option);
      -- The full name of the option, including possible prefixes.

      Enum : Enumerator_T'Class := Enumerator (Option.Option.all);
      -- For enumerating the values of the Option.

      Enum_Ended : Boolean;
      -- The enumeration of the Option values has ended.

   begin

      if Enum not in Not_Enumerable_T'Class then
         -- The values can be enumerated.

         Begin_Col (Margin);

         Put_Line ("The values are:");

         loop

            declare

               Value : constant String := Current_Value (Enum);
               -- The current value, in the enumeration sequence.

            begin

               Begin_Col (Margin);

               Put (Value);

               Describe (
                  Stuff  => Full_Name & Value_Separator & Value,
                  Margin => Margin + Margins.Value_Text_Offset);

               Next (Enum, Enum_Ended);

            end;

            exit when Enum_Ended;

         end loop;

      end if;

   end Describe_Values;


   procedure Describe_Value (
      Option : in     Option_Element_T;
      Value  : in     String;
      Found  :    out Boolean)
   --
   -- If the Option has enumerable values, and the given Value
   -- represents one or more of the Option's values (using a case
   -- insensitive comparison), these values are described and Found
   -- is returned as True. Otherwise, Found is returned as False.
   --
   is
      use Ada.Characters.Handling;

      Value_Up : constant String := To_Upper (Value);
      -- The Value in all upper-case letters.

      Full_Name : constant String := Name_Of (Option);
      -- The full name of the option, including possible prefixes.

      Enum : Enumerator_T'Class := Enumerator (Option.Option.all);
      -- For enumerating the values of the Option.

      Enum_Ended : Boolean;
      -- The enumeration of the Option values has ended.

   begin

      Found := False;
      -- Unless we find a match for Value below.

      if Enum not in Not_Enumerable_T'Class then
         -- The values can be enumerated.

         loop

            declare

               This_Value : constant String := Current_Value (Enum);
               -- The current value, in the enumeration sequence.

            begin

               if To_Upper (This_Value) = Value_Up then
                  -- This is the value to be described.

                  if not Found then
                     -- The first match.

                     Put_Line ('-' & Full_Name);

                     Describe (
                        Stuff  => Full_Name,
                        Margin => Margins.Basic);

                  end if;

                  Put ('-' & Full_Name & '=' & Value);

                  Describe (
                     Stuff  => Full_Name & Value_Separator & This_Value,
                     Margin => Margins.Basic);

                  Found := True;

               end if;

               Next (Enum, Enum_Ended);

            end;

            exit when Enum_Ended;

         end loop;

      end if;

   end Describe_Value;


   procedure Put_Name_Type_Default (
      Hyphen : in Boolean := True;
      Name   : in String;
      Option : in Option_Element_T)
   --
   -- Writes a line that shows the Full Name of an option
   -- and describes the type and default value of the option.
   --
   is
   begin

      if Hyphen then Put ('-'); end if;

      Put_Line (
           Name
         & " ("
         & Type_And_Default (Options.Option(Option))
         & ')');

   end Put_Name_Type_Default;


   procedure Describe (Option : in Option_Element_T)
   --
   -- Describes the Option on standard output.
   -- The option may be prefixed or not.
   --
   is

      Opt_Name : constant String := Name_Of (Option);
      -- The full name (perhaps prefixed) of the option.

      Opt_Prefix : constant String := Prefix_Of (Opt_Name);
      -- The prefix of this option, or the null string if
      -- this option has not prefixed.

   begin

      if Opt_Prefix = "" then
         -- This option is not prefixed.

         Put_Name_Type_Default (
            Name   => Opt_Name,
            Option => Option);

         Describe (
            Stuff  => Opt_Name,
            Margin => Margins.Basic);

         Describe_Values (
            Option => Option,
            Margin => Margins.Basic);

      else
         -- This option is prefixed.

         Put_Name_Type_Default (
            Name   => Opt_Prefix & ' ' & Item_Of (Opt_Name),
            Option => Option);

         Describe (
            Stuff  => Opt_Name,
            Margin => Margins.Basic);

         Describe_Values (
            Option => Option,
            Margin => Margins.Basic);

      end if;

   end Describe;


   type Grouped_Visitor_T is new Traversal.Visitor_T with record
      Group_Level : Natural := 0;
      Margin      : Positive_Count;
      Show_Prefix : Boolean := False;
   end record;
   --
   -- Describes the options in a list of options (that may
   -- be prefixed or not), grouping the options by their
   -- membership in option groups.
   --
   -- Group_Level
   --    The level in the group tree, which is the same as the
   --    number of groups that have been entered but not left.
   -- Margin
   --    The current left margin for descriptive text.
   -- Show_Prefix
   --    Whether to show the prefix of a prefixed option.
   --    Example with prefix    : "-draw symbol"
   --    Example without prefix : "symbol"


   overriding
   procedure Visit (
      Visitor : in out Grouped_Visitor_T;
      Option  : in     Option_Element_T);

   overriding
   procedure Enter_Group (
      Visitor : in out Grouped_Visitor_T;
      Group   : in     Group_T);

   overriding
   procedure Leave_Group (
      Visitor : in out Grouped_Visitor_T;
      Group   : in     Group_T);


   overriding
   procedure Visit (
      Visitor : in out Grouped_Visitor_T;
      Option  : in     Option_Element_T)
   is

      Full_Name : constant String := Name_Of (Option);
      -- The full name of the option, including possible prefixes.

      Prefix : constant String := Prefix_Of (Full_Name);
      -- The prefix, or "" if none.

   begin

      Begin_Col (Visitor.Margin);

      if Prefix = "" then
         -- This option is not prefixed.

         Put_Name_Type_Default (
            Name   => Full_Name,
            Option => Option);

      else
         -- This option is prefixed.

         if Visitor.Show_Prefix then
            -- Show "-<prefix> <item>":

            Put_Name_Type_Default (
               Name   => Prefix & ' ' & Item_Of (Full_Name),
               Option => Option);

         else
            --Show only "<item>":

            Put_Name_Type_Default (
               Hyphen => False,
               Name   => Item_Of (Full_Name),
               Option => Option);

         end if;

      end if;

      Describe (
         Stuff  => Full_Name,
         Margin => Visitor.Margin + Margins.Indent);

      Describe_Values (
         Option => Option,
         Margin => Visitor.Margin + Margins.Indent);

   end Visit;


   overriding
   procedure Enter_Group (
      Visitor : in out Grouped_Visitor_T;
      Group   : in     Group_T)
   is
   begin

      Describe (
         Stuff  => Group_Description_File (Group),
         Margin => Visitor.Margin);

      Visitor.Group_Level := Visitor.Group_Level + 1;

      Visitor.Margin := Visitor.Margin + Margins.Indent;

   end Enter_Group;


   overriding
   procedure Leave_Group (
      Visitor : in out Grouped_Visitor_T;
      Group   : in     Group_T)
   is
   begin

      Visitor.Group_Level := Visitor.Group_Level - 1;

      Visitor.Margin := Visitor.Margin - Margins.Indent;

   end Leave_Group;


   procedure Describe_Options (
      List   : in Option_List_T;
      Margin : in Positive_Count;
      Prefix : in Boolean)
   --
   -- Describes all the options in the List, starting at
   -- the given Margin, grouping (and indenting) the options
   -- by their option-group membership. The Prefix of prefixed
   -- options may be displayed or elided.
   --
   is

      Common : Group_Set_T := Full_Group_Set;
      -- The groups that are common to all these options.

      Visitor : Grouped_Visitor_T;
      -- To visit and describe each option in the List.

   begin

      -- Find the common set of groups:

      for L in List'Range loop

         Common := Common and List(L).Groups;

      end loop;

      -- Describe this option set, organized by groups:

      Visitor.Margin      := Margin;
      Visitor.Show_Prefix := Prefix;

      Traversal.Traverse (
         List     => List,
         Within   => Common,
         Synonyms => False,
         Visitor  => Visitor);

   end Describe_Options;


   procedure Describe_Prefixed_Options (Prefix : in String)
   --
   -- Describes the set of prefixed options.
   --
   is
   begin

      Put_Line ('-' & Prefix);

      Describe (Stuff => Prefix, Margin => Margins.Basic);

      Describe_Options (
         List   => All_Options (With_Prefix => Prefix),
         Margin => Margins.Basic,
         Prefix => True);

   end Describe_Prefixed_Options;


   procedure Introduce_Options
   --
   -- Describes the general form and usage of Bound-T options.
   --
   is
   begin

      Describe (
         Stuff  => "options_intro",
         Margin => 1);

   end Introduce_Options;


   procedure Describe_Usage
   --
   -- Describes the general usage of Bound-T.
   --
   is
   begin

      Put_Line (Version_Id);

      New_Line;

      Describe (
         Stuff  => "usage_and_arguments",
         Margin => 1);

      New_Line;

   end Describe_Usage;


   procedure Describe_Group (
      Group   : in Group_T;
      Members : in Boolean)
   --
   -- Describes the given option-Group, optionally describing
   -- also the Members of the group.
   --
   is
   begin

      Put_Line ("Option group: " & Name_Of (Group) & '.');

      Describe (
         Stuff  => Group_Description_File (Group),
         Margin => 1);

      if Members then

         New_Line;

         Describe_Options (
            List   => Options_In (Group),
            Margin => 1,
            Prefix => True);

      end if;

   end Describe_Group;


   procedure Describe_All_Groups
   --
   -- Describes all the option groups, but not the options
   -- in each group.
   --
   is
      use type String_Pool.Item_T;

      Sorted : array (1 .. Last_Group - First_Group + 1) of Group_T;
      -- All the groups, in alphabetical order by Name.

      Last_Sorted : Natural := 0;
      -- The groups so far sorted are Sorted(1 .. Last_Sorted).

      Group : Group_T;
      -- One of the groups.

      Max_Name_Length : Natural := 0;
      -- The longest name of an option group.
      -- For setting an output margin for the option descriptions.

   begin

      -- Sort the groups alphabetically:

      for G in First_Group ..  Last_Group loop

         Group := Group_Table(G);

         Max_Name_Length := Natural'Max (
            Max_Name_Length,
            String_Pool.Length (Group.Name));

         -- Insert this Group in Sorted(1 .. ):

         for L in reverse 1 .. Last_Sorted + 1 loop
            -- Sorted(L) is a vacant place in which the Group
            -- may be inserted, if acceptable to the alphabetical
            -- order with respect to Sorted(L - 1).

            if L > 1 and then Group.Name < Sorted(L - 1).Name
            then
               -- Group must be inserted before Sorted(L - 1).

               Sorted(L) := Sorted(L - 1);

            else
               -- Group.Name >= Sorted(L - 1).Name, so this
               -- is a good place for this Group.

               Sorted(L) := Group;

               exit;

            end if;

         end loop;

         Last_Sorted := Last_Sorted + 1;

      end loop;

      -- Describe the groups:

      Put_Line ("All option groups in " & Version_Id & ":");

      New_Line;

      for S in 1 .. Last_Sorted loop

         Begin_Col (1);

         Put (Name_Of (Sorted(S)));

         Describe (
            Stuff  => Group_Description_File (Sorted(S)),
            Margin => Positive_Count (Max_Name_Length + 2));

      end loop;

   end Describe_All_Groups;



   procedure Describe
   is
      use Option_Maps;


      procedure PL (Item : in String) renames Put_Line;
      -- Abbreviation for frequent use.

      Opt : Cursor := First (Option_Table);
      -- Traversing the option table in alphabetical order.

      Elem : Option_Element_T;
      -- The Opt element.

   begin  -- Describe

      Describe_Usage;

      Introduce_Options;

      New_Line;

      PL ("Other options by name in alphabetical order:");
      --
      -- The usage description (briefly) describes the "-help" option,
      -- which is not included in the list of other options, below.

      New_Line;

      while Opt /= No_Element loop

         Elem := Element (Opt);

         declare

            Opt_Name : constant String := Name_Of (Elem);
            -- The full name (perhaps prefixed) of the option.

            Opt_Prefix : constant String := Prefix_Of (Opt_Name);
            -- The prefix of this option, or the null string if
            -- this option has not prefixed.

         begin

            if Opt_Prefix = "" then
               -- This option is not prefixed.

               Put_Name_Type_Default (
                  Name   => Opt_Name,
                  Option => Elem);

               Describe (
                  Stuff  => Opt_Name,
                  Margin => Margins.Basic);

               Describe_Values (
                  Option => Elem,
                  Margin => Margins.Basic);

               Next (Opt);

            else
               -- This is the first of a sequence of prefixed options.

               Describe_Prefixed_Options (Opt_Prefix);

               Opt := Last_Option (With_Prefix => Opt_Prefix);

               Next (Opt);

            end if;

         end;

      end loop;

   exception

   when Device_Error =>
      -- This can happen, for example, when the output from this
      -- operation is viewed through "less" and the user aborts the
      -- viewer before we have emitted all the above output, closing
      -- the Standard_Output channel.

      null;

   end Describe;


   procedure Help_For_Single_Word (Word : in String)
   --
   -- Handles "-help <word>".
   --
   is

      Plain_Word : constant String := Without_Leading_Hyphen (Word);
      -- The <word> without its possible leading '-'.

      Found : Boolean := True;
      -- Whether this Word has been found in some set
      -- of words, eg. option names.

      Option : Option_Element_T;
      -- The option possibly named by this Word.

      Group : Group_T;
      -- The option group possibly named by this Word.

   begin

      if Plain_Word = "all" then
         -- All available help.

         Describe;

      elsif Plain_Word = "help" then
         -- Just help on "-help" itself.

         Put_Line ("-help");

         Describe (Stuff => "help", Margin => Margins.Basic);

      elsif Plain_Word = "options" then

         Introduce_Options;

      elsif Plain_Word = "groups" then

         Describe_All_Groups;

      elsif Is_Known_Prefix (Plain_Word) then
         -- A set of prefixed options.

         Describe_Prefixed_Options (Plain_Word);

      else
         -- Is it the name of an option?

         Find_Option (
            Name   => Plain_Word,
            Found  => Found,
            Option => Option);

         if Found then

            Describe (Option);

         else

            Find_Group (
               Name  => Plain_Word,
               Found => Found,
               Group => Group);

            if Found then

               Describe_Group (
                  Group   => Group,
                  Members => True);

            end if;

         end if;

      end if;

      if not Found then

         Output.Error (
              "Unknown option """
            & Word
            & """.");

      end if;

   end Help_For_Single_Word;


   procedure Help_For_Two_Words (Word1, Word2 : in String)
   --
   -- Handles "-help <word1> <word2>".
   --
   is

      Plain_Word : constant String := Without_Leading_Hyphen (Word1);
      -- The possible option name or option prefix.

      Found : Boolean := False;
      -- Whether there is an option or option value identified
      -- by Word1 and Word2 in some way.

      Option : Option_Element_T;
      -- The option possibly named by Plain_Word.

      Group : Group_T;
      -- The option group possibly named by Word2.

   begin

      if Plain_Word = "group" then

         Find_Group (
            Name  => Word2,
            Found => Found,
            Group => Group);

      end if;

      if Found then
         -- This is "-help group <group-name>".

         Describe_Group (
            Group   => Group,
            Members => True);

      elsif Is_Known_Prefix (Plain_Word) then
         -- Probably "-help <prefix> <item>".

         Find_Option (
            Name   => Prefixed (Prefix => Plain_Word, Item => Word2),
            Found  => Found,
            Option => Option);

         if Found then

            Put_Line ('-' & Plain_Word);

            Describe (Stuff => Plain_Word, Margin => Margins.Basic);

            Describe (Option);

         end if;

      else
         -- Perhaps "-help <option> <value>".

         Find_Option (
            Name   => Plain_Word,
            Found  => Found,
            Option => Option);

         if Found then

            Describe_Value (
               Option => Option,
               Value  => Word2,
               Found  => Found);

         end if;

      end if;

      if not Found then

         Output.Error (
              "Unknown option """
            & Word1
            & ' '
            & Word2
            & """.");

      end if;

   end Help_For_Two_Words;


   procedure Handle_Help (
      Next : in Positive)
   --
   -- Handles the arguments to a "-help" argument.
   -- The first argument after the "-help" is number Next.
   --
   is

      Args : constant Natural := Argument_Count - Next + 1;

   begin

      case Args is

      when 0 =>
         -- No arguments, just general help on Bound-T usage.

         Describe_Usage;

      when 1 =>
         -- Perhaps the name of an option.

         Help_For_Single_Word (Word => Argument (Next));

      when 2 =>
         -- Perhaps a prefix and an item.

         Help_For_Two_Words (
            Word1 => Argument(Next),
            Word2 => Argument(Next + 1));

      when others =>

         Output.Error ("Too many arguments for ""-help"".");

         raise Argument_Error;

      end case;

   end Handle_Help;


begin  -- Options.Command

   Register (
      Option => Keep_Files'access,
      Name   => "keep_files",
      Groups => (1 => Options.Groups.Outputs),
      Reset  => When_Keep_Files_Reset'access,
      Set    => When_Keep_Files_Set'access);

end Options.Command;
