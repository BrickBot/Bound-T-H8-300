-- Options (decl)
--
-- Handling options (small inputs) from the command-line
-- and possibly other sources.
--
-- An "option" is a variable that has a name (an identifier) and
-- a value of some type. Options can be created in various places
-- (modules) and "registered" in a central table of options that
-- the present package provides. The option table is indexed by
-- the option name, but options are also collected into groups
-- by zero or more "group names" associated with each option.
-- The option table can be queried to yield all the options in
-- a given group. Sets of options that contain options from several
-- groups can be sorted into groups for presentation and description
-- purposes.
--
-- The options fall generally into four classes:
--
-- > General options, not related to the target processor, target
--   device, or target cross-compiler.
--
-- > Options specific to the target processor architecture, but
--   applicable to all devices and cross-compilers. This set of
--   options is also defined statically at program start (because
--   each Bound-T executable handles one target architecture).
--   One of these options is the choice of target device. Another
--   is the choice of cross-compiler.
--
-- > Options for a specific target device. This set of options
--   depends dynamically on the chosen (or detected) target device.
--
-- > Options for a specific target cross-compiler. This set of
--   options depends dynamically on the chosen (or detected)
--   cross-compiler.
--
-- To allow such dynamic dependencies, the option table lets you
-- enable or disable particular options and groups dynamically.
--
-- An option group can contain options of one or several of the
-- above classes. For example, the group of "trace" options
-- typically contains options from at least the first three
-- classes, and perhaps also from the fourth class.
--
-- The same set of options can be presented and set via different
-- interfaces such as the command line, some sort of graphical
-- interface, and perhaps others, perhaps environment variables.
-- The "group" concept can be important for some interfaces (eg.
-- graphical ones) and irrelevant for others (eg. the command line).
-- Therefore the option name must be an unambiguous identifier and
-- it is not possible for one group or two different groups to
-- contain options with the same name.
--
-- An action that "sets" an option can set the complete value,
-- totally replacing the earlier value, or it can update the value,
-- leaving some part of the original value unchanged. For example,
-- an option that has, as value, a subset of some finite collection,
-- can be "set" by adding some element(s) of the collection or by
-- deleting some element(s) of the collection.
--
-- The user can set option values on the command line or through
-- some other kind of interface, for example a graphical one.
-- The "option" data types provide enough information to let us
-- parse command lines, but do not provide in themselves provide
-- descriptive information for the labels on GUI buttons or for
-- "help" output. Such information is collected from external files
-- specific to each option and (possibly) to each value of this
-- option. Different versions of the external files can exist for
-- different environments, for example for different languages.
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
-- $Revision: 1.17 $
-- $Date: 2015/10/25 19:03:41 $
--
-- $Log: options.ads,v $
-- Revision 1.17  2015/10/25 19:03:41  niklas
-- Added procedure Set_Option, which also calls When_Set.
--
-- Revision 1.16  2015/10/24 19:36:51  niklas
-- Moved to free licence.
--
-- Revision 1.15  2014/06/01 10:28:22  niklas
-- Added the Refused exception to signal serious errors
-- in option values.
-- Added the image function Modular_Valued.Type_Alone for
-- better display of certain option values.
-- -
--
-- Revision 1.14  2013-02-03 12:30:41  niklas
-- Fixed comment typos.
--
-- Revision 1.13  2012-02-12 14:13:19  niklas
-- Added Integer_Valued.Type_Alone, for BT-CH-0229.
--
-- Revision 1.12  2012-01-19 21:33:36  niklas
-- Added Base parameter to Integer_Valued.
-- Added Modular_Valued, also with Base parameter.
--
-- Revision 1.11  2011-09-06 17:40:24  niklas
-- Increased Max_Groups from 32 to 64.
--
-- Revision 1.10  2011-08-31 04:23:34  niklas
-- BT-CH-0222: Option registry. Option -dump. External help files.
--


with Ada.Containers.Ordered_Maps;
with Ada.Text_IO;
with String_Pool;


package Options is


   Refused : exception;
   --
   -- Signals that some processing cannot be done because some of
   -- the option values for that process are wrong and refused.
   -- An appropriate error message must be emitted before this
   -- exception is raised.


   --
   ---   Option-value enumerators
   --


   type Enumerator_T is abstract tagged null record;
   --
   -- An object that can enumerate (iterate over) all the values of
   -- a particular (type of) option and produce a string "image"
   -- of the value. The main purpose here is to use the value
   -- to find the external file that describes the meaning of
   -- this value for this option.
   --
   -- An Enumerator_T object has a "current" value and can be
   -- told to advance to the next value in the list.
   --
   -- Some types of options have values for which an enumeration
   -- in this way is not useful, for example floating-point values.
   -- A special subclass of Enumerator_T, Not_Enumerable_T, is
   -- provided for such options.


   function Current_Value (Enum : Enumerator_T) return String
   is abstract;
   --
   -- A readable denotation of the current value.


   procedure Next (
      Enum  : in out Enumerator_T;
      Ended :    out Boolean)
   is abstract;
   --
   -- Advances to the next value in the Enumeration of values, or
   -- returns Ended as True if there are no more values in the
   -- enumeration.


   type Not_Enumerable_T is new Enumerator_T with null record;
   --
   -- Represents a type of value for which it is not useful to
   -- enumerate all values.


   overriding
   function Current_Value (Enum : Not_Enumerable_T) return String;
   --
   -- Returns a null string.


   overriding
   procedure Next (
      Enum  : in out Not_Enumerable_T;
      Ended :    out Boolean);
   --
   -- Returns Ended as True.


   Not_Enumerable : Not_Enumerable_T;
   --
   -- The single necessary object of this type.


   --
   ---   Option objects
   --
   -- An internal representation of options, of various kinds, as data
   -- objects, allowing general handling and translations to other
   -- formats, for example XML or HTML forms.
   --
   -- The kinds of "options" we consider are variables that (usually)
   -- exist in one static instance, are identified by a name, and
   -- have a specific (fairly simple) type,
   --
   -- The option objects themselves do *not* contain any descriptive text.
   -- All descriptions are retrieved from a known directory of (text)
   -- files, based on the name of the option and possibly on the value of
   -- the option. This will allow easier evolution of the descriptions,
   -- better formatted and structured descriptions, and descriptions in
   -- various languages.
   --
   -- An option object can belong to zero or several groups of options.
   -- An option group is identified by a name.
   --
   -- Options fall into four classes:
   --
   -- > General and standard options that apply equally to all target
   --   processors, target devices, and cross-compilers. These options
   --   are statically defined (by the present version of Bound-T) and
   --   do not depend (visibly) on the values of other options.
   --
   -- > Options that are specific to the target processor (architecture)
   --   but apply equally to all target devices and all cross-compilers.
   --   These options are also defined statically (by the present version
   --   of Bound-T) and do not depend (visibly) on the values of other
   --   options. One of these options is the choice of target device (the
   --   "-device" option); another is the choice of cross-compiler.
   --
   -- > Options that are specific to one target device. These options
   --   are available (settable) only when the "device" option is set
   --   to this device, or when Bound-T automatically detects that the
   --   target program was compiled for this device.
   --
   -- > Options that are specific to one cross-compiler. These options
   --   are available (settable) only when the cross-compiler option is
   --   set to this cross-compiler, or when Bound-T detects automatically
   --   that this cross-compiler generated the target program.
   --
   -- Option objects can be created anywhere (in any module) but must be
   -- registered in the option table to make them visible and settable
   -- thru this package.
   --
   -- Options can be disabled or enabled individually. Option groups
   -- can also be enabled and disabled. An option is effectively enabled
   -- (visible and settable) if the option itself is enabled, and if
   -- at least one of its groups is enabled (if the option is a member
   -- of some groups).


   type Option_T is abstract tagged null record;
   --
   -- An option of some kind.
   --
   -- The properties of options are defined later in two ways:
   --
   -- > The current value and default value are defined in
   --   derived types. The default value is usually a discriminant
   --   and the current value is an ordinary component.
   --
   -- > The name, group membership, and option-specific enabling
   --   are held in the option table and created when the option
   --   is registered in the table.


   type Option_Ref is access all Option_T'Class;
   --
   -- Option objects must be "aliased" so that their values can be
   -- accessed directly (by modules that know of these options) without
   -- going through the option table. The option table contains references
   -- to the option objects.


   function Type_And_Default (Option : access Option_T)
   return String
   is abstract;
   --
   -- A brief textual description of the type of the option (that is,
   -- what type of values the option can have) and its default value.
   -- The current value is not included TBC.


   procedure Reset (Option : access Option_T)
   is abstract;
   --
   -- Resets the Option to its default value, whatever that is.


   procedure Set (
      Option : access Option_T;
      Value  : in     String)
   is abstract;
   --
   -- Sets (or updates) the Option to (or by) the Value, if the Value
   -- is a valid description of the option (or an update).
   -- Otherwise propagates Constraint_Error with no change to
   -- the Option's value.


   function Enumerator (Option : Option_T) return Enumerator_T'Class;
   --
   -- In general, an enumerator for the values of this Option,
   -- initialised to the first value, or Not_Enumerable for Options
   -- that do not have enumerable values.
   --
   -- The root-class implementation returns Not_Enumerable. Thus, it
   -- is necessary to override this function only for options that
   -- are enumerable.


   --   Generic discrete-valued options


   function Discrete_Image (Item : String) return String;
   --
   -- A utility function for generating the command-line forms of
   -- the 'Images of discrete types. The result is the given Item
   -- converted to lower-case letters and with a possible "_item"
   -- suffix removed.


   generic
      type Value_Type is (<>);
      Default_Enumerable : in Boolean := True;
      Value_Type_Description : in String := "Keyword";
      with function Value_Image (Item : Value_Type) return String;
      Use_Discrete_Image : in Boolean := True;
      Quote_Image        : in Boolean := True;
   package Discrete_Valued
   --
   -- Options with values of a discrete type.
   -- The "enumerability" can be controlled per option, with a
   -- default value given generically.
   -- Value_Type_Description describes the Value_Type and is used
   -- in the function Type_And_Default, as is Value_Image.
   -- If Use_Discrete_Image is True, the result of Value_Image
   -- is passed thru the Discrete_Image function.
   -- If Quote_Image is True, the result of Value_Image (possibly
   -- passed thru Discrete_Image) is enclosed in quotes (").
   --
   is

      type Option_T (Default : Value_Type)
      is new Options.Option_T
      with record
         Value      : Value_Type := Default;
         Enumerable : Boolean    := Default_Enumerable;
      end record;
      --
      -- An option that has a Default value and a current Value.

      overriding
      function Type_And_Default (Option : access Option_T)
      return String;

      overriding
      procedure Reset (Option : access Option_T);

      overriding
      procedure Set (
         Option : access Option_T;
         Value  : in     String);
      --
      -- Sets the Option's value to Value_Type'Value (Value). If this
      -- fails, retries with Value_Type'Value (Value & "_Item"). Thus,
      -- if Value_Type is an enumeration, it can contain a reserved
      -- word, for example "function", as a literal suffixed with
      -- "_Item", in this example "function_Item".
      --
      -- First, however, the operation replaces all strings of blanks
      -- and hyphens in Value with underscores. Leading and trailing
      -- blanks and hyphens are ignored.

      overriding
      function Enumerator (Option : Option_T) return Enumerator_T'Class;
      --
      -- Lists Value_Type'First to Value_Type'Last if Option.Enumerable,
      -- otherwise returns Not_Enumerable.

      type Enum_T is new Enumerator_T with record
         Current : Value_Type;
      end record;

      overriding
      function Current_Value (Enum : Enum_T) return String;

      overriding
      procedure Next (Enum : in out Enum_T; Ended : out Boolean);

   end Discrete_Valued;


   generic
      type Item_Type is (<>);
      type Set_Type is array (Item_Type) of Boolean;
      Default_Enumerable : in Boolean := True;
      Value_Type_Description : in String := "Set of keywords";
      with function Item_Image (Item : Item_Type) return String;
      Use_Discrete_Image : in Boolean := True;
      Quote_Image        : in Boolean := True;
   package Discrete_Set_Valued
   --
   -- Options with values that are sets of values of a discrete type.
   -- The "enumerability" can be controlled per option, with a
   -- default value given generically.
   -- Value_Type_Description describes the value type (which is a
   -- combination of Item_Type and Set_Type) and is used in the
   -- function Type_And_Default, as is Item_Image.
   -- If Use_Discrete_Image is True, the result of Item_Image
   -- is passed thru the Discrete_Image function.
   -- If Quote_Image is True, the result of Value_Image (possible
   -- passed thru Discrete_Image) is enclodes in quotes (").
   --
   is

      type Option_T is new Options.Option_T with record
         Default    : Set_Type;
         Value      : Set_Type;
         Enumerable : Boolean := Default_Enumerable;
      end record;
      --
      -- An option that has a Default value and a current Value,
      -- both of which are sets of Item_Type elements.

      type Option_Ref       is access all Option_T;
      type Option_Class_Ref is access all Option_T'Class;

      overriding
      function Type_And_Default (Option : access Option_T)
      return String;

      overriding
      procedure Reset (Option : access Option_T);

      not overriding
      procedure Update (
         Option  : access Option_T;
         Literal : in     String;
         Member  : in     Boolean);
      --
      -- Updates Option.Value to include or exclude the item defined by
      -- the Literal, depending on whether the item should be a Member,
      -- or not. If the Literal does not define an Item_Type value, the
      -- operation propagates Constraint_Error.

      overriding
      procedure Set (
         Option : access Option_T;
         Value  : in     String);
      --
      -- The Value string is expected to be a comma-separated sequence
      -- of Item_Type literals, possibly prefixed with "no_".
      --
      -- For literals not prefixed with "no_", the operation adds the
      -- denoted item to the Option's value. For literals prefixed with
      -- "no_", the operation removes the denoted item from the Option's
      -- value.
      --
      -- If the literal as such is not a valid Item_Type, the operation
      -- attempts to use an "_Item" suffix. Thus, if Value is "no_function",
      -- the operation sets Option.Value(Item_Type'Value("function_Item"))
      -- to False.
      --
      -- If the Value string does not have this form, the operation
      -- propagates Constraint_Error. However, some updates may have been
      -- applied to the Option, if passed by reference.
      --
      -- First, however, the operation replaces all strings of blanks
      -- and hyphens in Value with underscores. Leading and trailing blanks
      -- and hyphens are ignored. Thus, the above example can also be
      -- written in Value as "no-function" or "no function".

      overriding
      function Enumerator (Option : Option_T) return Enumerator_T'Class;
      --
      -- Lists Item_Type'First to Item_Type'Last, thus only the singleton
      -- values of the option, and only if Option.Enumerable, otherwise
      -- returns Not_Enumerable.

      type Enum_T is new Enumerator_T with record
         Current : Item_Type;
      end record;

      overriding
      function Current_Value (Enum : Enum_T) return String;

      overriding
      procedure Next (Enum : in out Enum_T; Ended : out Boolean);

   end Discrete_Set_Valued;


   subtype Number_Base_T is Positive range 2 .. 16;


   generic
      type Value_Type is range <>;
      Base : in Number_Base_T := 10;
   package Integer_Valued
   --
   -- Options with values of an integral type.
   -- These options are not enumerable.
   -- The Base specifies the numerical base for input and
   -- output strings.
   --
   is

      type Option_T (Default : Value_Type) is new Options.Option_T
      with record
         Value : Value_Type := Default;
      end record;
      --
      -- An option that has a Default value and a current Value.

      function Image (Item : Value_Type) return String;
      --
      -- The Item, as a string of digits in the given Base.

      function Type_Alone (Option : access Option_T)
      return String;
      --
      -- Like Type_And_Default, but without the default part.
      -- Intended for use in derived types.

      overriding
      function Type_And_Default (Option : access Option_T)
      return String;

      overriding
      procedure Reset (Option : access Option_T);

      overriding
      procedure Set (
         Option : access Option_T;
         Value  : in     String);
      --
      -- Sets the Option's value to Value_Type'Value(Value), if Base = 10,
      -- of Value_Type'Value (<Base>#Value#), if Base /= 10.
      -- Propagates Constraint_Error if Value is not an integer literal
      -- in some acceptable (to 'Value) form.

   end Integer_Valued;


   generic
      type Value_Type is mod <>;
      Base : in Number_Base_T := 10;
   package Modular_Valued
   --
   -- Options with values of a modular integral type.
   -- These options are not enumerable.
   -- The Base specifies the numerical base for input and
   -- output strings.
   --
   is

      type Option_T (Default : Value_Type) is new Options.Option_T
      with record
         Value : Value_Type := Default;
      end record;
      --
      -- An option that has a Default value and a current Value.

      function Image (Item : Value_Type) return String;
      --
      -- The Item, as a string of digits in the given Base.

      function Type_Alone (Option : access Option_T)
      return String;
      --
      -- Like Type_And_Default, but without the default part.
      -- Intended for use in derived types.

      overriding
      function Type_And_Default (Option : access Option_T)
      return String;

      overriding
      procedure Reset (Option : access Option_T);

      overriding
      procedure Set (
         Option : access Option_T;
         Value  : in     String);
      --
      -- Sets the Option's value to Value_Type'Value(Value), if Base = 10,
      -- of Value_Type'Value (<Base>#Value#), if Base /= 10.
      -- Propagates Constraint_Error if Value is not an integer literal
      -- in some acceptable (to 'Value) form.

   end Modular_Valued;


   generic
      type Value_Type is private;
      Value_Type_Description : in String;
      with function Value (Item : String) return Value_Type;
      with function Image (Item : Value_Type) return String;
   package Valued
   --
   -- Options with values of some private type.
   -- These options are not enumerable.
   -- Value_Type_Description describes the Value_Type and is used
   -- in the function Type_And_Default, as is Image.
   --
   is

      type Option_T is new Options.Option_T
      with record
         Default : Value_Type;
         Value   : Value_Type;
      end record;
      --
      -- An option that has a Default value and a current Value.

      function Default (Value : Value_Type) return Option_T;
      --
      -- An Option_T with a given default Value, which is also
      -- the current value.

      overriding
      function Type_And_Default (Option : access Option_T)
      return String;

      overriding
      procedure Reset (Option : access Option_T);

      overriding
      procedure Set (
         Option : access Option_T;
         Value  : in     String);
      --
      -- Sets the Option's value to Valued.Value (Value), where the Value
      -- function is assumed to propagate Constraint_Error if the Value
      -- parameter is not an acceptable definition of a Value_Type value.

   end Valued;


   --
   ---   Hooks for special action during Set or Reset.
   --


   type Reset_Action_T is access procedure (Option : in Option_Ref);
   --
   -- An action to be taken after an Option it Reset.


   procedure No_Action (Option : in Option_Ref)
   is null;


   type Set_Action_T is access procedure (
      Option : in Option_Ref;
      Value  : in String);
   --
   -- An action to be taken after an Option is successfully Set to a Value.


   procedure No_Action (
      Option : in Option_Ref;
      Value  : in String)
   is null;


   --
   ---   Option groups
   --


   type Group_T is private;
   --
   -- A group of options, to which a given option can belong or
   -- not belong. Can also be considered a Boolean property of options.


   subtype Group_Name_T is String_Pool.Item_T;
   --
   -- The identifying name of an option group.


   function Group (S : String) return Group_Name_T;
   --
   -- Converts a string to a group-name, possibly adding some
   -- conventional prefix/suffix in order to separate group
   -- description files from option description files when a
   -- group and an option have the same name.


   function Name_Of (Group : Group_T) return Group_Name_T;
   --
   -- The name of the group.


   function Name_Of (Group : Group_T) return String;
   --
   -- The name of the group.


   function Names_Group (Name : String) return Boolean;
   --
   -- Whether this is the Name of some known group.


   procedure Find_Group (
      Name  : in     String;
      Found :    out Boolean;
      Group :    out Group_T);
   --
   -- Searches the group table for the group with the given Name
   -- and returns the registered Group if Found.


   type Groups_T is array (Positive range <>) of Group_Name_T;
   --
   -- A list of groups, typically all the groups to which an option
   -- belongs.


   No_Groups : constant Groups_T (1 .. 0) := (others => String_Pool.Null_Item);
   --
   -- No groups at all.


   type Group_Set_T is private;
   --
   -- A set of groups, equivalent to Groups_T.


   function Image (Item : Group_Set_T) return String;
   --
   -- Shows the names of the groups in the set.


   procedure Set_Group_Priority (
      Higher : in Group_Name_T;
      Lower  : in Group_Name_T);
   --
   -- Defines a part of the priority ordering between option groups,
   -- by requiring that one group shall have a Higher priority with
   -- respect to another group of a Lower priority.


   --
   ---   Option table
   --


   type Option_Element_T is private;
   --
   -- An option as registered in the option table.
   -- Among other attributes, it contains a reference to
   -- the actual Option_T'Class object.


   function Option (Element : Option_Element_T) return Option_Ref;
   --
   -- The (reference to the) option object described by this Element.


   function Name_Of (Element : Option_Element_T) return String;
   --
   -- The real name for this option. This Element may be registered
   -- under a different synonym.


   function Is_Synonym (Element : Option_Element_T) return Boolean;
   --
   -- Whether this Element is registered as a synonym (a different
   -- name) for an option that is also registered under its real
   -- name.


   function Prefixed (
      Prefix : String;
      Item   : String)
   return String;
   --
   -- The name of an option that consists of a Prefix and an Item,
   -- separated in a standard way. Such options are given special
   -- treatment in the command-line argument scanning, which can use
   -- the syntax "-prefix[=][no_]item,[no_]item,...".
   --
   -- If Prefix = "", the result is just the Item.


   function Imp_Item (Item : String) return String;
   --
   -- The name of an option (item) for the "-imp" prefix.


   function Trace_Item (Item : String) return String;
   --
   -- The name of an option (item) for the "-trace" prefix.


   function Warn_Item (Item : String) return String;
   --
   -- The name of an option (item) for the "-warn" prefix.


   function Prefix_Of (Prefixed_Name : String) return String;
   --
   -- The part of the Name before the (first) Prefix_Sep in Prefixed_Name.
   -- The null string if there is no prefix.


   function Item_Of (Prefixed_Name : String) return String;
   --
   -- The part of the Name after the (first) Prefix_Sep in Prefixed_Name.
   -- Propagates Constraint_Error if there is no prefix.


   procedure Register (
      Option  : in Option_Ref;
      Name    : in String;
      Synonym : in String         := "";
      Groups  : in Groups_T       := No_Groups;
      Reset   : in Reset_Action_T := No_Action'access;
      Set     : in Set_Action_T   := No_Action'access);
   --
   -- Registers the Option in the option table, under its Name
   -- and its Groups, with possible actions for Reset and Set,
   -- and a possible Synonym name.


   procedure Register (
      Option  : in Option_Ref;
      Name    : in String;
      Synonym : in String         := "";
      Group   : in Group_Name_T;
      Reset   : in Reset_Action_T := No_Action'access;
      Set     : in Set_Action_T   := No_Action'access);
   --
   -- Registers the Option in the option table, under its Name
   -- and its single Group, with possible actions for Reset and Set,
   -- and a possible Synonym name.


   procedure Find_Option (
      Name   : in     String;
      Found  :    out Boolean;
      Option :    out Option_Element_T);
   --
   -- Searches the option table for the option with the given Name
   -- and returns the registered Option if Found.


   function Option (Name : String) return Option_Ref;
   --
   -- The option registered under the given Name, or null
   -- if no option is registered with this Name.


   function Name_Of (Option : access Option_T) return String;
   --
   -- The name under which this Option is registered. If the Option
   -- is not registered, Constraint_Error is propagated.


   procedure Reset (Option : in Option_Element_T);
   --
   -- Applies the Reset operation, and its related actions, to
   -- the given Option.


   procedure Set (
      Option : in Option_Element_T;
      Value  : in String);
   --
   -- Applies the Set operation, and its related actions, to
   -- the given Option with the given Value.
   
   
   procedure Set_Option (
      Name  : in String;
      Value : in String);
   --
   -- Finds the option registered with the given Name, and then
   -- applies Set to change this option's value to Value, and if that
   -- succeeds, invokes the When_Set operation on this option and
   -- the new value.
   --
   -- If there is no option with this Name, propagates Program_Error;
   -- If there is such an option, but the Value is not valid for this
   -- option, propagates Constraint_Error.
   --
   -- A typical use for this operation is in Decoder.Handle_Wild_Option,
   -- for example to accept a device name without the "-device" prefix.


   procedure Finish_Definition;
   --
   -- To be called once, after all options and option groups are
   -- registered all all pairwise priorities between option groups
   -- are set.
   --
   -- Computes the final priorities of the option groups.


   --
   ---   Usage and option descriptions ("help" system)
   --


   procedure Add_Help (
      Path     : in String;
      Relative : in Boolean := True);
   --
   -- Adds a file-system path to a directory/folder that contains
   -- option-description files ("help" files).
   --
   -- Most help files are stored under one root directory (there may be
   -- different root directories for different languages or for other
   -- reasons). Such Paths are Relative to the root directory. If the
   -- Relative parameter is False, the Path is used as such, without
   -- prefixing it with the help-root path.
   --
   -- All generic help files (that apply to all versions of Bound-T,
   -- whatever the target processor, host system, and optional parts)
   -- are stored in the subdirectory "gen" under the root directory.
   --
   -- Options for a certain target processor, a certain host system,
   -- or a certain optional Bound-T function are stored in subdirectories
   -- identified by the name of the target processor, host system, or
   -- optional function. The relevant target-specific *.Opt packages
   -- shall call Add_Help to include these subdirectories in the search
   -- for help files.
   --
   -- More help directories can be specified with the command-line
   -- option "-help_dir", in which case the argument specifies the
   -- directory path as such, not relative to the help-root.


   procedure Add_Help (
      Path, Subpath : in String;
      Relative      : in Boolean := True);
   --
   -- Same as Add_Help (File_System.Path_Name (Path, Subpath), Relative).


private


   subtype Option_Name_T is String_Pool.Item_T;
   --
   -- The identifying name of an option.


   function Option_Name (S : String) return Option_Name_T
   renames String_Pool.To_Item;
   --
   -- Converts a string to an option-name.


   Max_Groups : constant := 64;
   --
   -- An upper bound on the number of option groups.
   -- Required only to make Group_Set_T a definite type.


   subtype Group_Index_T is Positive range 1 .. Max_Groups;
   --
   -- An index that identifies an option group.


   First_Group : constant Group_Index_T := Group_Index_T'First;
   Last_Group  : Natural := First_Group - 1;
   --
   -- The defined groups have the indices First_Group .. Last_Group.


   type Group_Set_T is array (Group_Index_T) of Boolean;
   --
   -- A set of option groups.
   -- Group members are marked by True values.


   Null_Group_Set : constant Group_Set_T := (others => False);
   --
   -- The empty set of groups.


   Full_Group_Set : constant Group_Set_T := (others => True);
   --
   -- The set that contains all groups.


   type Option_Element_T is record
      Option     : Option_Ref;
      Name       : Option_Name_T;
      Real_Name  : Option_Name_T;
      Groups     : Group_Set_T;
      Enabled    : Boolean;
      When_Reset : Reset_Action_T;
      When_Set   : Set_Action_T;
   end record;
   --
   -- An option as registered in the option table, possibly
   -- as a synonym Name for an Option that is also registered under
   -- a different Real Name (in the registration under Real_Name
   -- we have Name = Real_Name).
   --
   -- When_Reset
   --    Called when a "reset value" is applied to this option-table element.
   --    Note that the primitive Reset operation for Option_T does not call it.
   -- When_Set
   --    Called when a "set value" is applied to this option-table element.
   --    Note that the primivite Set operation for Option_T does not call it.
   --
   -- In effect, When_Reset and When_Set are called only when the reset/set
   -- is called only when the option is identified by name and looked up from
   -- the option table.
   

   type Option_List_T is array (Positive range <>) of Option_Element_T;
   --
   -- A list or subset of options, to be traversed for presentation.


   package Option_Maps is new Ada.Containers.Ordered_Maps (
      Key_Type     => Option_Name_T,
      Element_Type => Option_Element_T,
      "<"          => String_Pool."<",
      "="          => "=");
   --
   -- The option table maps Option_Name_T to Option_Element_T.


   Option_Table : Option_Maps.Map;
   --
   -- The option table, initially empty.


   Prefix_Sep : constant Character := '.';
   --
   -- The character that is used to separate the "prefix" and "item"
   -- parts in the name of a "prefixed" option.


   subtype Group_Priority_T is Positive;
   --
   -- The "priority" of a group guides the tree-like traversal
   -- of sets of options, ordered by their containing groups.
   -- Groups with higher (numerically smaller) priorities define
   -- the higher (closer to root) branches in the traversal tree.


   type Group_T is record
      Name     : Group_Name_T;
      Index    : Group_Index_T;
      Priority : Group_Priority_T;
   end record;


   Group_Table : array (Group_Index_T) of Group_T;
   --
   -- The defined groups are Group_Table (First_Group .. Last_Group).


   function Name_Of (Group : Group_Index_T) return String;
   --
   -- The name of the given Group.


   function Priority_Of (Group : Group_Index_T) return Group_Priority_T;
   --
   -- The priority of the given Group.


   function All_Options return Option_List_T;
   --
   -- All the options, in alphabetical order by full name.


   function First_Option (With_Prefix : in String)
   return Option_Maps.Cursor;
   --
   -- Refers to the alphabetically first option With the given Prefix.


   function Last_Option (With_Prefix : in String)
   return Option_Maps.Cursor;
   --
   -- Refers to the alphabetically last option With the given Prefix.


   function All_Options (With_Prefix : in String)
   return Option_List_T;
   --
   -- All the options With the given Prefix.


   function Options_In (Group : Group_T)
   return Option_List_T;
   --
   -- All the options in the given Group.


   --
   ---   Help files describing options and option groups
   --


   procedure Open_Description (
      Name : in     String;
      Form : in     String;
      File : in out Ada.Text_IO.File_Type);
   --
   -- Opens the option/group description File with the
   -- given Name, of the given Form (txt, html, etc.).
   -- May propagate Name_Error, Use_Error, etc.


   function Group_Description_File (Group : Group_T) return String;
   --
   -- The name of the file that describes the given Group.
   -- A group can have the same name as an option, and therefore
   -- the file-names must be separated in some way, implemented
   -- in this function.


end Options;
