-- Formats.Dwarf.Info (body)
--
-- Notes on peculiar features seen in actual DWARF information:
--
-- 1. Although the DWARF standard says that a list of sibling debugging
--    information entries is always terminated by a null entry (one that
--    consists only of the abbreviation code zero), the null entry seems
--    to be omitted sometimes when the list ends at the end of the
--    ".debug_info" section.
--    Producers:
--       ARM compiler for ARM7.
--
-- 2. Although the DWARF standard says that the declaration-coordinate
--    attributes Decl_File, Decl_Line and Decl_Col show the source-code
--    point at which the first character of the identifier of the
--    declared object appears, for Subprogram entries in C they instead
--    refer to the point of the curly bracket '{' that enclose the
--    function body.
--    Producers:
--       ARM compiler for ARM7.
--
-- 3. Similarly, although the Decl_File and Decl_line may correctly show
--    the line that holds the subprogram identifier, Decl_Col may be 1
--    instead of the starting column of the identifier.
--    Producers:
--       IAR compiler for ARM7.
--
-- 4. A Typedef entry for the "void" type may not have any Type_Ref
--    attribute to show the underlying type, just the Name "void".
--    Producers:
--       GNAT GAP 2006 for the ERC32.
--
-- 5. Separate compilation-units for the same subprogram, one giving
--    source-level information (Name, Type) and the other giving
--    machine-level information (Location). Ugh, no work-around
--    implemented.
--    Producers:
--       ARM ADS1.2 [build 805], armcc for ARM7.
--
-- Author: Niklas Holsti.
--
-- A component of the Bound-T Worst-Case Execution Time Tool.
--
-------------------------------------------------------------------------------
-- Copyright (c) 1999 .. 2015 Tidorum Ltd except for text copied verbatim
-- from the DWARF standard.
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
-- $Revision: 1.8 $
-- $Date: 2015/10/24 20:53:52 $
--
-- $Log: formats-dwarf-info.adb,v $
-- Revision 1.8  2015/10/24 20:53:52  niklas
-- Moved to free licence.
--
-- Revision 1.7  2011-05-28 12:45:36  niklas
-- Handle exceptions to skip invalid compilation units.
--
-- Revision 1.6  2008-10-15 12:05:53  niklas
-- BT-CH-0150: DWARF location lists, lexical block scopes, CUBAs.
--
-- Revision 1.5  2007/06/14 11:16:40  niklas
-- BT-CH-0060.
--
-- Revision 1.4  2007/01/25 21:25:35  niklas
-- BT-CH-0043.
--
-- Revision 1.3  2006/04/12 19:31:32  niklas
-- Added Value_Of attribute functions that take a Value_Def_T,
-- not a whole Value_Def_List_T.
-- Added Value_Of attribute functions for integer values.
-- Added specific operations for subprogram attributes in the
-- form of function Is_Inlined and procedure Find_Entry_Point.
-- Added the components Strings and Locations to Compilation_T,
-- to provide access to the .debug_str and .debug_loc sections
-- when a Node is constructed from an Element. Extended and
-- corrected function Node_For accordingly.
-- Added the parameter Scan_Entries.Str to provide the source
-- for View.Str.
-- Added the parameters Scan.Str, Load.Str and Dump.Str to provide
-- access to the .debug_str section.
--
-- Revision 1.2  2004/06/26 09:50:32  niklas
-- Improved Scan_Entries to stop scanning when the End_Index of the
-- enclosing compilation unit is reached, even in the absence of a
-- terminating null entry at that point.
--
-- Revision 1.1  2004/04/24 18:06:19  niklas
-- First version.
--


with Ada.Strings.Fixed;
with Ada.Text_IO;
with Ada.Unchecked_Deallocation;
with Formats.Dwarf.Expressions;
with Formats.Dwarf.Opt;
with Formats.Dwarf.Strings;
with Formats.Dwarf.Text;
with Formats.In_Memory;
with Formats.In_Memory.Text;
with Output;
with Unbounded_Vectors;


package body Formats.Dwarf.Info is


   subtype Supported_Version_T is Version_T range 2 .. 3;
   --
   -- The Unit_Header.Versions supported by this implementation.


   --
   --    DWARF tree elements
   --
   -- An element represents a DWARF debugging information entry (DIE).
   -- The elements are organized in a family tree. This tree lets us
   -- quickly determine the ancestors of any given DIE from the
   -- section-index of the given DIE.
   --
   -- An element does not directly contain any DWARF information such
   -- as tags or attributes. Instead, it contains a pointer (index) to
   -- the DIE as it resides in an in-memory copy of the DWARF info
   -- section (the ".debug_info" section). To interpret this information
   -- one must also have on hand the compilation-unit header (bit-width
   -- and address-size numbers), the abbrevation table for this
   -- compilation unit, and sometimes the DWARF string section (the
   -- ".debug_str" section) and/or the DWARF location-list section (the
   -- ".debug_loc" section).


   subtype Element_Index_T is In_Memory.Index_T;
   --
   -- Identifies the place of a DIE within an in-memory copy of the
   -- DWARF info section.


   use type Element_Index_T;


   type Element_T is record
      Index   : Element_Index_T;
      Child   : Element_Ref;
      Sibling : Element_Ref;
   end record;
   --
   -- An element in the DWARF tree.
   --
   -- Index
   --    The index, in some in-memory copy of the DWARF ".debug_info"
   --    section, of the start of the DIE that this element represents.
   --    This is the index of the first octet of the unsigned LEB128
   --    encoding of the abbrevation code for the DIE.
   -- Child
   --    The element for the first child DIE. Null if there are no
   --    child elements. The children are listed by increasing Index.
   -- Sibling
   --    The element for the next sibling DIE. Null if there are no
   --    more siblings. The siblings are listed by increasing Index.


   --
   --    Compilation Unit info
   --


   type Compilation_Unit_Header_T is record
      Unit_Length   : Initial_Length_T;
      Version       : Version_T;
      Abbrev_Offset : Section_Offset_T;
      Addr_Size     : Addr_Size_T;
   end record;
   --
   -- Header for the series of debugging information entries contributed
   -- by a single compilation unit.
   --
   -- Unit_Length
   --    The total length, in octets, of the .debug_info contribution
   --    for this compilation unit, not including this length field
   --    itself.
   -- Version
   --    DWARF version (1, 2 or 3).
   -- Abbrev_Offset
   --    An offset into the .debug_abbrev section that associates this
   --    compilation unit with a particular set of debugging information
   --    entry abbreviations (templates).
   -- Addr_Size
   --    The size in octets of an address on the target architecture.
   --    An attribute value in the "Addr" form is represented by this
   --    number of octets for all debugging information entries in this
   --    compilation unit. If the system uses segmented addressing, this
   --    value represents the size of the offset portion of an address.


   procedure Read_Compilation_Unit_Header (
      Stream : access Ada.Streams.Root_Stream_Type'Class;
      Item   : out    Compilation_Unit_Header_T);
   --
   -- Reads a Compilation Unit Header from a DWARF section.
   --
   for Compilation_Unit_Header_T'Read use Read_Compilation_Unit_Header;


   procedure Read_Compilation_Unit_Header (
      Stream : access Ada.Streams.Root_Stream_Type'Class;
      Item   : out    Compilation_Unit_Header_T)
   is
   begin

      Initial_Length_T'Read (Stream, Item.Unit_Length);
      Version_T'Read        (Stream, Item.Version);

     if Item.Version not in Supported_Version_T then

         Output.Warning (
              "DWARF debug-info unit header version ="
            & Version_T'Image (Item.Version)
            & ", supported versions are"
            & Version_T'Image (Supported_Version_T'First)
            & " .."
            & Version_T'Image (Supported_Version_T'Last));

      end if;

      Read_Variant_Unsigned (
         Stream => Stream,
         Bits   => Item.Unit_Length.Bits,
         Item   => Item.Abbrev_Offset);

      Addr_Size_T'Read (Stream, Item.Addr_Size);

   end Read_Compilation_Unit_Header;


   procedure Put (Item : in Compilation_Unit_Header_T)
   --
   -- Displays the compilation unit header on standard output.
   --
   is
      use Ada.Text_IO;
   begin

      Put_Line ("DWARF .debug_info Compilation Unit Header:");

      Put_Line (
           "   Unit Length  :"
         & Image (Item.Unit_Length));

      Put_Line (
           "   Version      :"
         & Version_T'Image (Item.Version));

      Put_Line (
           "   Abbrev Offset:"
         & Section_Offset_T'Image (Item.Abbrev_Offset));

      Put_Line (
           "   Address Size :"
         & Addr_Size_T'Image (Item.Addr_Size));

   end Put;


   type Compilation_T is record
      Sections      : Sections_T;
      Header_Index  : In_Memory.Index_T;
      Header        : Compilation_Unit_Header_T;
      Abbrevs       : Abbrev.Set_Ref;
      Info_Index    : In_Memory.Index_T;
      End_Index     : In_Memory.Index_T;
      First_Element : Element_Ref;
      CUBA          : Poss_Code_Address_T;
   end record;
   --
   -- Summary information for a compilation unit.
   --
   -- Sections
   --    Access to the in-memory copies of the DWARF sections.
   -- Header_Index
   --    The index of the first octet of the compilation header for
   --    this compilation unit, in some (not here given) in-memory
   --    copy of the ".debug_info" section. Attributes of an intra-
   --    unit reference class are non-negative offsets to this index.
   -- Header
   --    The Compilation Unit Header.
   -- Abbrevs
   --    The abbreviation set.
   -- Info_Index
   --    The index of the first debugging information entry for this
   --    compilation unit, in Sections.Info.
   --    TBC if this is useful.
   -- End_Index
   --    The index of the first octet after the end of the entries for
   --    this compilation unit. This is the Header_Index of the next
   --    compilation unit if any.
   -- First_Element
   --    The first element of the DWARF tree for this compilation unit.
   --    Nominally this should have Tag = Compile_Unit and be the parent
   --    of all other elements within this compilation unit.
   --    First_Element.Index = Info_Index.
   -- CUBA
   --    Compilation Unit Base Address, as possibly defined by the
   --    Low_PC attribute of the First_Element.


   procedure Unchecked_Discard
   is new Ada.Unchecked_Deallocation (
      Name   => Compilation_Ref,
      Object => Compilation_T);


   type Compilation_Ref_List_T is array (Positive range <>) of Compilation_Ref;
   --
   -- A list of (references to) compilation units.
   -- Typically the list contains all the compilation units from one
   -- DWARF info section, in file (index) order.


   package Unbounded_Compilation_Vectors
   is new Unbounded_Vectors (
      Element_Type => Compilation_Ref,
      Vector_Type  => Compilation_Ref_List_T,
      Deallocate   => Opt.Deallocate);


   type Compilations_T is new Unbounded_Compilation_Vectors.Unbounded_Vector;
   --
   -- An unbounded list of (references to) compilation units.


   --
   --    Tree object
   --


   type Tree_Object_T is record
      Info         : In_Memory.Stream_Ref;
      Compilations : Compilations_T;
      Types        : Dwarf.Types.Indexed_Type_Set_T;
   end record;
   --
   -- An entire DWARF info section parsed into compilation units
   -- with family trees.
   --
   -- Info
   --    An in-memory copy of the info section, accessible as a stream.
   -- Compilations
   --    An indexable array of the compilation units, in the same order
   --    as in the info section.
   -- Types
   --    Type definitions.


   --    Loading a DWARF tree : Principle of operation
   --
   -- We load a DWARF tree by first reading the DWARF sections into
   -- In_Memory copies and then scanning the info section (.debug_info)
   -- from start to end, thus constructing the list of compilations
   -- and the tree of elements for each compilation. For each compilation
   -- we read the required abbrevation set from the abbrevation section
   -- (.dwarf_abbrev) and make an expanded representation of the
   -- abbreviations (Dwarf.Abbrev.Set_Ref). On this pass of the info
   -- section we do not read any of the attributes of the entries, except
   -- for the dumping/tracing option.
   --
   -- After this point the in-memory copy of the abbreviation section is
   -- no longer needed and is discarded.
   --
   -- The next step is to build the set of types represented in the
   -- DWARF info. We did not do this on the fly during the first scan
   -- of the info section because the type entries very often refer to
   -- other type entries that can be before or after the referring entry
   -- and even in another compilation unit; the interpretation of an
   -- entry sometimes depends on properties of the compilation unit so
   -- it could be hard to interpret the referenced type entry before we
   -- have loaded the header of the relevant compilation unit. After the
   -- first scan on the info we know all compilation units which makes it
   -- easier to follow the Type_Ref chains in the type definitions. We
   -- traverse the element tress of all compilation units (the order is
   -- arbitrary) and enter type definitions in the Types set. When a type
   -- entry refers to another type entry we recursively enter the
   -- referenced entries in Tree.Types if not already entered. However,
   -- we do not represent the full type information but only features
   -- relevant to Bound-T, as chosen in Formats.Dwarf.Types. Some type
   -- entries may be skipped completely.
   --
   -- The result is Tree.Types, a set of type definitions keyed by the
   -- index of the type entry in the DWARF info section. A type definition
   -- (Formats.Dwarf.Types.Type_T) may be stand-alone or may refer to
   -- other type definitions. In any case, all relevant properties of the
   -- types can be extracted from Tree.Types without further use of the
   -- info section.


   function View_Current_Entry (Within : Compilation_T)
   return Abbrev.View_T
   --
   -- A view of the entry Within the given compilation, at the
   -- position of Within.Sections.Info.
   --
   is
   begin

      return Abbrev.View (
         Sect => Within.Sections,
         Set  => Within.Abbrevs,
         Refz => Within.Header_Index);

   end View_Current_Entry;


   function View_Entry_At (
      Index  : In_Memory.Index_T;
      Within : Compilation_Ref)
   return Abbrev.View_T
   --
   -- A view of the entry Within the given compilation, at the
   -- given Index for Within.Sections.Info. The position of the
   -- Info stream is altered.
   --
   is
   begin

      In_Memory.Set_Index (
         Stream => Within.Sections.Info.all,
         To     => Index);

      return View_Current_Entry (Within.all);

   end View_Entry_At;


   function Node_For (
      Element : Element_Ref;
      Within  : Compilation_Ref)
   return Node_T
   --
   -- The node that describes the given Element residing Within a
   -- given compilation. The position of the Info stream is altered.
   --
   is
   begin

      return (
         Compilation => Within,
         Index       => Node_Index_T (Element.Index),
         View        => View_Entry_At (Element.Index, Within));

   end Node_For;


   function Contains (
      Index : Element_Index_T;
      Comp  : Compilation_Ref)
   return Boolean
   --
   -- Whether this Index is within the given Compilation.
   --
   is
   begin

      return Index in Comp.Info_Index .. Comp.End_Index - 1;

   end Contains;


   function Compilation_Containing (
      Index : Element_Index_T;
      Tree  : Tree_Object_T)
   return Compilation_Ref
   --
   -- The compilation unit that contains a given debugging information entry
   -- element which is identified by its Index (the index of its first octet)
   -- within a DWARF tree.
   --
   is

      Comp : Compilation_Ref;
      -- The current compilation.

   begin

      -- TBA change to binary search algorithm.

      for C in 1 .. Last (Tree.Compilations) loop

         Comp := Element (Tree.Compilations, C);

         if Contains (Index, Comp) then
            -- This compilation contains the entry.

            return Comp;

         end if;

      end loop;

      Output.Fault (
         Location => "Formats.Dwarf.Info.Compilation_Containing",
         Text     =>
              "No compilation unit contains index"
            & Element_Index_T'Image (Index));

      raise Format_Error;

   end Compilation_Containing;


   Max_Descent_Depth : constant := 100;
   --
   -- Maximum length of a descent path.


   function Path_To (
      Element : Element_Index_T;
      Tree    : Tree_Object_T)
   return Node_List_T
   --
   -- The descent path within a given DWARF tree to an Element identified
   -- by its index (the index of its first octet). The descent path starts
   -- from the First_Element of the containing compilation unit (or one of
   -- its siblings, if it unusually has some) and continues down to the
   -- element itself.
   --
   is

      Path : Node_List_T (1 .. Max_Descent_Depth);
      Last : Natural := 0;
      -- The descent path will be Path(1 .. Last).

     Compilation : constant Compilation_Ref :=
        Compilation_Containing (Element, Tree);
      -- The compilation that contains the Element.

      Parent : Element_Ref;
      -- One of the set of sibling elements where the set contains
      -- an ancestor of the Element or the Element itself. Then, the
      -- ancestor element or the Element itself.

   begin

      -- Find the descent path in the compilation:

      Parent := Compilation.First_Element;

      Vertical:
      loop
         -- Assuming that Parent is the first of a set of sibling
         -- elements that must contain an ancestor of the Element.

         -- Find the sibling that is the ancestor:

         Horizontal:
         loop

            exit Horizontal when Parent.Sibling = null;
            -- Since there are no more siblings, this Parent must
            -- be the next in-line ancestor.

            exit Horizontal when Parent.Sibling.Index > Element;
            -- The next sibling is past the Element, so this
            -- Parent must be the next in-line ancestor.

            Parent := Parent.Sibling;
            -- The Element is further along in this sibling list.

         end loop Horizontal;

         if Parent.Index > Element then
            -- This should not happen; it means that Element
            -- cannot be a descendant of Root.

            Output.Fault (
               Location => "Formats.Dwarf.Info.Path_To",
               Text     =>
                    "Parent index"
                  & Element_Index_T'Image (Parent.Index)
                  & " > Element index"
                  & Element_Index_T'Image (Element));

            exit Vertical;

         end if;

         -- Parent is an ancestor or is the Element itself, so
         -- it should go on the Path:

         if Last = Path'Last then
            -- This path is too deep for us.

            Output.Fault (
               Location => "Formats.Dwarf.Info.Path_To",
               Text     =>
                    "Path to element at index"
                  & Element_Index_T'Image (Element)
                  & " is deeper than"
                  & Natural'Image (Last)
                  & " levels.");

            raise Constraint_Error;

         end if;

         Last := Last + 1;

         Path(Last) := Node_For (
            Element => Parent,
            Within  => Compilation);

         exit Vertical when Parent.Index = Element;
         --
         -- The Path ends at the Element.
         -- This is the normal exit from the loop.

         if Parent.Child = null then
            -- This should not happen; it means that Parent covers
            -- the Element index, but claims to have no children.

            Output.Fault (
               Location => "Formats.Dwarf.Info.Path_To",
               Text     =>
                    "Parent at index"
                  & Element_Index_T'Image (Parent.Index)
                  & " cannot have a descendant at index"
                  & Element_Index_T'Image (Element));

            exit Vertical;

         end if;

         -- Look at the next generation:

         Parent := Parent.Child;

      end loop Vertical;

      return Path(1 .. Last);

   end Path_To;


   procedure Put_Index (Index : In_Memory.Index_T)
   --
   -- Emits the stream Index as [Index] on standard output,
   -- followed by one blank.
   --
   is
   begin

      Ada.Text_IO.Put (
           '['
         & Text.Trim (In_Memory.Index_T'Image (Index))
         & "] ");

   end Put_Index;


   procedure Set_CUBA (CU : in Node_T)
   --
   -- Given a Compile_Unit node CU, we set the Compilation Unit Base
   -- Address (CUBA) for CU.Compilation from the Low_PC attribute of
   -- this node, if this attribute is defined. Otherwise, CUBA is
   -- set to "not defined".
   --
   is

      Base_Address : Code_Address_T;
      -- The CUBA, if defined.

   begin

      Base_Address := Value_Of (
         Attribute => Low_PC,
         Within    => Values (CU),
         From      => CU);

      -- The Low_PC = CUBA is defined.

      CU.Compilation.CUBA := (Defined => True, Address => Base_Address);

   exception

   when Attribute_Absent =>

      CU.Compilation.CUBA := (Defined => False);

   when Wrong_Form =>

      Output.Warning ("DWARF attribute Low_PC is not an address.");

      CU.Compilation.CUBA := (Defined => False);

   end Set_CUBA;


   procedure Scan_Entries (
      Comp  : in     Compilation_Ref;
      Level : in     Natural;
      Load  : in     Boolean;
      Dump  : in     Integer;
      First :    out Element_Ref)
   --
   -- Scans a tree of DWARF debugging information entries (DIEs)
   -- sequentially, starting at the current position of the Info stream,
   -- and optionally loads the entries into a tree structure and/or dumps
   -- the entries on standard output.
   --
   -- Info
   --    The in-memory copy of the ".debug_info" section.
   --    On call, it should be positioned at the start of a debugging
   --    information entry that is the first in a list of sibling entries.
   --    The entry may be a null entry. On return, positioned after the
   --    last entry in this list of sibling entries.
   -- Comp
   --    The compilation unit to be scanned. This provides access to
   --    the in-memory copies of the DWARF sections and defines the end-point
   --    in the Info stream, the base index for reference attributes (when
   --    intra-compilation), the base address for location lists (CUBA, if
   --    defined), and the abbreviation set. The CUBA is updated if we
   --    scan a Compile_Unit element with a Low_PC attribute.
   -- Level
   --    The tree level of the first entry (and its siblings).
   --    Level is zero for the top level (compilation unit entries).
   --    The Level is needed only for dump indentation.
   -- Load
   --    Enables loading the entries as a tree of Elements, with the
   --    first entry returned in the First parameter. Otherwise the
   --    entries are not loaded and First is returned as null.
   -- Dump
   --    The maximum depth of dumping. A negative value prevents display.
   --    A zero value allows display of the first entry and its siblings
   --    but not of their children, because the children are scanned with
   --    a recursive call with Dump => Dump - 1.
   -- First
   --    Returned as a reference to the element corresponding to the
   --    first entry, if Load is True. Otherwise returned as null.
   --
   is
      use Ada.Text_IO;
      use Ada.Strings.Fixed;

      View : Abbrev.View_T;
      -- A view of the current entry.

      Element : Element_T;
      -- The element for the current entry.
      -- Defined even if Load is False.

      Prev : Element_Ref := null;
      -- The preceding sibling element, null if none.

      This : Element_Ref;
      -- The loaded copy of the current Element.

      Next : In_Memory.Index_T;
      -- The index of the next entry.

   begin

      First := null;
      -- A good default value, valid when Load is False
      -- or the entry list is empty.

      Siblings:
      loop

         -- The standard says that each sibling list is terminated by
         -- a null entry, but some producers omit the null entry at
         -- the end of the info section.

         if In_Memory.End_Of_Data (Comp.Sections.Info.all) then

            Output.Note (
               "DWARF debug info ends without null entry.");

            exit Siblings;

         end if;

         -- Read the next entry:

         Element := (
            Index   => In_Memory.Index (Comp.Sections.Info.all),
            Child   => null,
            Sibling => null);

         -- Some producers omit the null entry at the end of a compilation
         -- unit, and thus we must check also End_Index:

         if Element.Index >= Comp.End_Index then

            Output.Note (
               "DWARF debug info ends at End_Index without null entry.");

            exit Siblings;

         end if;

         View := View_Current_Entry (Within => Comp.all);

         exit Siblings when Abbrev.Is_Null (View);
         -- Stop when a null entry is found.

         if Abbrev.Tag (View) = Compile_Unit then
            -- Check for Low_PC to define Comp.CUBA.

            Set_CUBA (Node_T'(
               Compilation => Comp,
               Index       => Node_Index_T (Element.Index),
               View        => View));

         end if;

         -- Display or skip this entry:

         if Dump >= 0 then
            -- Entry to be displayed.

            Put_Index (Element.Index);

            Abbrev.Put (
               View   => View,
               Indent => Level * "   ",
               CUBA   => Comp.CUBA,
               Next   => Next);

         else

            Next := Abbrev.Next_Entry (View);

         end if;

         In_Memory.Set_Index (
            Stream => Comp.Sections.Info.all,
            To     => Next);

         -- Scan the children if any:

         if Abbrev.Children (View) then
            -- This entry may have children.

            Scan_Entries (
               Comp  => Comp,
               Level => Level + 1,
               Load  => Load,
               Dump  => Dump - 1,
               First => Element.Child);

         end if;

         -- Possibly load the element:

         if Load then
            -- Load the Element into heap memory and
            -- insert it in the tree:

            This := new Element_T'(Element);

            if Prev = null then
               -- This is the first element among siblings.

               First := This;

            else

               Prev.Sibling := This;

            end if;

            Prev := This;

         end if;

      end loop Siblings;

   end Scan_Entries;


   procedure Enter_Type (
      Node   : in     Node_T;
      Tree   : in     Tree_T;
      Giving :    out Types.Type_T)
   --
   -- Enters the type defined by the given Node into Tree.Types, unless
   -- it is already there. If the type refers to other types, these other
   -- types are also entered, and so on recursively. However, this
   -- recursion is limited to type references that are relevant for our
   -- purposes (integral types).
   --
   -- The Giving parameter returns the type definition for Index, whether
   -- here created or pre-existing.
   --
   -- Precondition: the Tag at Index is in Type_Tag_T.
   --
   is
      use type Types.Kind_T;

      Index : constant Node_Index_T := Info.Index (Node);
      -- The index of the node (element, entry) in the DWARF info.

      Type_Index : constant Types.Type_Index_T :=
         Types.Type_Index_T (Index);
      -- The index of the type to be defined.

      Values : constant Value_Def_List_T := Info.Values (Node);
      -- Provides access to the attributes of the Node.

      Tag : Type_Tag_T;
      -- The tag of the type def or a referenced type def.

      Base_Encoding : Octet_T;
      -- The encoding of a (new) Base Type.


      function Type_Name return String
      --
      -- The name of the type, if defined, else "<no name>".
      --
      is
      begin

         return Value_Of (
            Attribute => Name,
            Within    => Values,
            From      => Node,
            Default   => "<no name>");

      end Type_Name;


      procedure Skip (Reason : in String)
      --
      -- Skip this type for some reason.
      --
      is
      begin

         Output.Note (
              "DWARF type """
            & Type_Name
            & """ at"
            & Node_Index_T'Image (Index)
            & " skipped because "
            & Reason
            & '.');

      end Skip;


      procedure Is_Integral (Signed : in Boolean)
      --
      -- Yes, this is an integral type.
      --
      is
      begin

         Giving := (
            Kind           => Types.Integral,
            Index          => Type_Index,
            Signed         => Signed,
            Size_In_Octets => Positive (Number_T'(
               Value_Of (Byte_Size, Values, Node))));

      end Is_Integral;


      procedure Enter_Type_Ref
      --
      -- The present Node should have a Type_Ref attribute for
      -- another (more basic) type, which becomes Giving and is
      -- entered if not already in the set.
      --
      is

         Ref_Index : Node_Index_T;
         -- The Type_Ref.

         Ref_Elem : Element_Index_T;
         -- The Type_Ref as an element index.

         Ref_Comp : Compilation_Ref;
         -- The compilation that contains Ref_Index if that type
         -- is not yet entered.

      begin

         Ref_Index := Value_Of (Type_Ref, Values, Node);

         Ref_Elem := Element_Index_T (Ref_Index);

         Giving := Types.Type_At (Ref_Elem, Tree.Types);

         -- The referenced type (Giving) was already defined, but
         -- we are now defining the same type for the new index:

         Giving.Index := Type_Index;

      exception

      when Types.No_Such_Type =>
         -- Ha, the Type_Ref type is not yet entered. We do so now.

         if Contains (Ref_Elem, Node.Compilation) then
            -- Reference within the same compilation.

            Ref_Comp := Node.Compilation;

         else
            -- Reference to another compilation.

            Ref_Comp := Compilation_Containing (
               Index => Ref_Elem,
               Tree  => Tree.all);

         end if;

         Enter_Type (
            Node => (
               Compilation => Ref_Comp,
               Index       => Ref_Index,
               View        => View_Entry_At (Ref_Elem, Ref_Comp)),
            Tree   => Tree,
            Giving => Giving);

         -- And again we will define the same type for the new index:

         Giving.Index := Type_Index;

      end Enter_Type_Ref;


   begin  -- Enter_Type

      Giving := Types.Type_At (Type_Index, Tree.Types);
      --
      -- If this does not raise No_Such_Type the type is already
      -- entered. Lucky us.

   exception

   when Types.No_Such_Type =>
      -- This is a new type for us.

      begin

         Tag := Abbrev.Tag (Node.View);

         Giving := (Kind => Types.Non_Integral, Index => Type_Index);
         -- Pessimistic initial value.

         case Tag is

         when Base_Type =>

            Base_Encoding := Octet_T (Number_T'(
               Value_Of (Encoding, Values, Node)));

            case Base_Encoding is

            when 1 | 2 | 7 | 8 =>
               -- An unsigned integral value.

               Is_Integral (Signed => False);

            when 5 | 6 =>
               -- A signed integral value.

               Is_Integral (Signed => True);

            when others =>
               -- Not an integral value.

               null;

            end case;

         when Enumeration_Type =>

            -- Enumerated types are represented as integer values.

            Is_Integral (Signed => True);    -- Signedness TBC.

         when Const_Type
            | Mutable_Type
            | Restrict_Type
            | Volatile_Type =>

            -- For our needs a type that is a modified form of a
            -- parent type, for these modifications, is the same as
            -- the parent type.
            -- TBA use of the Volatile modification.

            Enter_Type_Ref;

         when Packed_Type =>

            -- Packed types are not integral.

            null;

         when Pointer_Type
            | Reference_Type
            | Ptr_To_Member_Type =>

            Is_Integral (Signed => False);

         when Subrange_Type =>

            -- For our needs a type that is a subrange of a parent
            -- type (Type_Ref) is the same as the parent type.
            -- TBA use of range constraints.

            Enter_Type_Ref;

         when Typedef =>

            -- For our needs the definition of a new name for a parent
            -- type (Type_Ref) is irrelevant; we use the parent type.

            Enter_Type_Ref;

         when Unspecified_Type
            | Array_Type
            | Set_Type
            | String_Type
            | File_Type
            | Class_Type
            | Interface_Type
            | Structure_Type
            | Union_Type
            | Subroutine_Type =>

            -- Not an integral type.

            null;

         end case;

      exception

      when Attribute_Absent =>

         Skip (Reason => "an attribute is absent");

      when Wrong_Form =>

         Skip (Reason => "an attribute has the wrong form");

      when Constraint_Error =>

         Skip (Reason => "an attribute has an out-of-range value");

      end;

      -- We know what's what:

      if Opt.Trace_Loading then

         Output.Trace (
              "DWARF type """
            & Type_Name
            & """ defined as "
            & Types.Image (Giving));

      end if;

      Types.Insert (
         Giving,
         Tree.Types);

   end Enter_Type;


   procedure Load_Types (
      First : in Element_Ref;
      Comp  : in Compilation_Ref;
      Tree  : in Tree_T)
   --
   -- Loads type definitions from the DWARF tree forest at the First
   -- element and its siblings (if any), within the given Compilation
   -- unit and the given Tree, into Tree.Types.
   --
   is

      Elem : Element_Ref := First;
      -- The first of the siblings.

      View : Abbrev.View_T;
      -- A view of the Elem and its attributes.

      Typ : Types.Type_T;
      -- A type definition, not used.

   begin

      while Elem /= null loop

         View := View_Entry_At (Index => Elem.Index, Within => Comp);

         if Abbrev.Tag (View) in Type_Tag_T then

             Enter_Type (
                Node   => (
                   Compilation => Comp,
                   Index       => Node_Index_T (Elem.Index),
                   View        => View),
                Tree   => Tree,
                Giving => Typ);

         end if;

         if Elem.Child /= null then
            -- Descend into the deeper and darker forest.

            Load_Types (
               First => Elem.Child,
               Comp  => Comp,
               Tree  => Tree);

         end if;

         Elem := Elem.Sibling;

      end loop;

   end Load_Types;


   procedure Scan (
      Info   : in     Segment_Desc_T;
      Abbrev : in     Segment_Desc_T;
      Str    : in     Segment_Desc_T;
      Loc    : in     Segment_Desc_T;
      From   : in     IO.File_Type;
      Load   : in     Boolean;
      Dump   : in     Integer;
      Tree   :    out Tree_T)
   --
   -- Scans the DWARF information sequentially and optionally loads parts
   -- into in-memory data structures and optionally dumps (displays) the
   -- information on standard output. The Dump parameter is similar to
   -- the Dump parameter of Scan_Entries, which see.
   --
   -- The Str section is optional and absent if Str.Octets = 0.
   -- The Loc section is optional and absent if Loc.Octets = 0.
   --
   is
      use type In_Memory.Index_T;
      use type IO.Count;

      Some_Dump : constant Boolean := Dump >= 0;
      -- At least the top level will be dumped.

      Sections : Sections_T;
      -- Access to in-memory copies of the DWARF info, strings
      -- and locations sections.

      Abbrev_Stream : In_Memory.Stream_Ref;
      -- Access to an in-memory copy of the DWARF abbreviations section.

      Abbrev_Sets : Formats.Dwarf.Abbrev.Sets_T;
      -- The abbreviation sets.

      Compilation : Compilation_Ref;
      -- Summary information for a compilation unit.

      Novel_Abbrevs : Boolean;
      -- Whether the current compilation unit defined a new
      -- abbreviation table.

      End_Index : In_Memory.Index_T;
      -- Same as Compilation.End_Index.

      Comp_Ref : Compilation_Ref;
      -- A reference to a compilation unit, for defining types
      -- in that unit.

      Compilation_Valid : Boolean;
      -- Whether the Compilation was read successfully, as far as we can tell.


      procedure Load_Section (
         Name    : in     String;
         Section : in     Segment_Desc_T;
         Giving  :    out In_Memory.Stream_Ref)
      --
      -- Loads and optionally dumps the given section.
      --
      is
      begin

         if Some_Dump then

            Ada.Text_IO.Put_Line (
                 "DWARF " & Name & " section starts at"
               & IO.Positive_Count'Image (Section.Start)
               & " and contains"
               & IO.Count'Image (Section.Octets)
            & " octets.");

         end if;

         In_Memory.Load (
            From   => From,
            Part   => Section,
            Giving => Giving);

         if Some_Dump then

            In_Memory.Text.Dump (Giving);

         end if;

      end Load_Section;


   begin  -- Scan

      -- Load the DWARF sections into in-memory buffers:

      Load_Section (
         Name    => Section_Name,
         Section => Info,
         Giving  => Sections.Info);

      Load_Section (
         Name    => Formats.Dwarf.Abbrev.Section_Name,
         Section => Abbrev,
         Giving  => Abbrev_Stream);

      if Is_Null (Str) then
         -- There is no string-values section.

         Sections.Strings := null;

      else
         -- The string-values section exists.

         Load_Section (
            Name    => Formats.Dwarf.Strings.Section_Name,
            Section => Str,
            Giving  => Sections.Strings);

      end if;

      if Is_Null (Loc) then
         -- There is no location-list section.

         Sections.Locations := null;

      else
         -- The location-list section exists.

         Load_Section (
            Name    => Expressions.Location_List_Section_Name,
            Section => Loc,
            Giving  => Sections.Locations);

      end if;

      -- Create the Tree if chosen:

      if Load then

         Tree := new Tree_Object_T;

         Tree.Info := Sections.Info;

      else

         Tree := null;

      end if;

      -- Scan the Info section and load abbreviation sets as
      -- necessary:

      loop

         -- Read the compilation unit header:

         Compilation := new Compilation_T;

         Compilation.Sections := Sections;

         Compilation.Header_Index := In_Memory.Index (Sections.Info.all);

         Compilation_Unit_Header_T'Read (Sections.Info, Compilation.Header);

         Compilation.Info_Index := In_Memory.Index (Sections.Info.all);

         End_Index :=
              Compilation.Header_Index
            + In_Memory.Count_T (Total_Length (
                 Compilation.Header.Unit_Length));

         Compilation.End_Index := End_Index;

         if Some_Dump then

            Ada.Text_IO.New_Line;

            Put (Compilation.Header);

         end if;

         -- Read the compilation unit, trapping exceptions:

         Read_Compilation:
         begin

            -- Read the set of abbreviation entries:

            Formats.Dwarf.Abbrev.Find_Or_Load_Set (
               Key => (
                  Offset    => Compilation.Header.Abbrev_Offset,
                  Addr_Size => Compilation.Header.Addr_Size,
                  Bits      => Compilation.Header.Unit_Length.Bits),
               Stream => Abbrev_Stream,
               Sets   => Abbrev_Sets,
               Ref    => Compilation.Abbrevs,
               Novel  => Novel_Abbrevs);

            -- Perhaps dump the abbreviations:

            if not Some_Dump then

               null;

            elsif Novel_Abbrevs then

               Formats.Dwarf.Abbrev.Put (Compilation.Abbrevs);

            else

               Ada.Text_IO.Put_Line ("   Abbreviations already displayed.");

            end if;

            -- Scan the debugging information entries:

            if Load or Some_Dump then

               Scan_Entries (
                  Comp  => Compilation,
                  Level => 0,
                  Load  => Load,
                  Dump  => Dump,
                  First => Compilation.First_Element);

            end if;

            -- Finish this compilation unit:

            if Some_Dump then

               Ada.Text_IO.Put_Line ("end of DWARF compilation unit.");

            end if;

            Compilation_Valid := True;

         exception

         when X : others =>

            Compilation_Valid := False;

            Output.Exception_Info (
               Text       => "Exception while reading DWARF compilation unit",
               Occurrence => X);

            Output.Warning ("Skipping invalid DWARF compilation unit.");

         end Read_Compilation;

         if Compilation_Valid and Load then

            Append (
               To    => Tree.Compilations,
               Value => Compilation);

         elsif Opt.Deallocate then

            Unchecked_Discard (Compilation);

         end if;

         -- Advance to next compilation unit:

         In_Memory.Set_Index (
            Stream => Sections.Info.all,
            To     => End_Index);

         exit when In_Memory.End_Of_Data (Sections.Info.all);

      end loop;

      In_Memory.Discard (Abbrev_Stream);

      if Load then
         -- Load the type definitions into Tree.Types:

         for C in 1 .. Last (Tree.Compilations) loop

            Output.Note (
                 "Loading DWARF types for compilation unit"
               & Positive'Image (C));

            Comp_Ref := Element (Tree.Compilations, C);

            Load_Types (
               First => Comp_Ref.First_Element,
               Comp  => Comp_Ref,
               Tree  => Tree);

         end loop;

      else
         -- Discard unnecessary stuff:

         In_Memory.Discard (Sections.Info);
         In_Memory.Discard (Sections.Strings);
         In_Memory.Discard (Sections.Locations);

      end if;

   end Scan;


   procedure Load (
      Info   : in     Segment_Desc_T;
      Abbrev : in     Segment_Desc_T;
      Str    : in     Segment_Desc_T;
      Loc    : in     Segment_Desc_T;
      From   : in     IO.File_Type;
      Tree   :    out Tree_T)
   is

      Dump : Integer;
      -- The level of dumping.

   begin

      if Opt.Trace_Loading then
         -- Dump all levels.

         Dump := Integer'Last;

      else
         -- Dump nothing.

         Dump := -1;

      end if;

      Scan (
         Info   => Info,
         Abbrev => Abbrev,
         Str    => Str,
         Loc    => Loc,
         From   => From,
         Load   => True,
         Dump   => Dump,
         Tree   => Tree);

   end Load;


   procedure Dump (
      Info   : in Segment_Desc_T;
      Abbrev : in Segment_Desc_T;
      Str    : in Segment_Desc_T;
      Loc    : in Segment_Desc_T;
      From   : in IO.File_Type)
   is

      Not_Loaded : Tree_T;
      -- Unused output from Scan. Will be null.

   begin

      Scan (
         Info   => Info,
         Abbrev => Abbrev,
         Str    => Str,
         Loc    => Loc,
         From   => From,
         Load   => False,
         Dump   => Integer'Last,
         Tree   => Not_Loaded);

   end Dump;


   --
   --    Nodes in the DWARF tree
   --


   function Index (Node : Node_T) return Node_Index_T
   is
   begin

      return Node_Index_T (Node.Index);

   end Index;


   function Abbrev_Code (Node : Node_T) return Abbrev.Code_T
   is
   begin

      return Abbrev.Code (Node.View);

   end Abbrev_Code;


   function Tag (Node : Node_T) return Tag_T
   is
   begin

      return Abbrev.Tag (Node.View);

   end Tag;


   function Address_Size (Node : Node_T) return Addr_Size_T
   is
   begin

      return Node.Compilation.Header.Addr_Size;

   end Address_Size;


   function CUBA (Node : Node_T) return Poss_Code_Address_T
   is
   begin

      return Node.Compilation.CUBA;

   end CUBA;


   --
   --    Attribute values from DWARF tree nodes
   --


   function Values (Node : Node_T) return Value_Def_List_T
   is

      List : Value_Def_List_T (1 .. Abbrev.Number_Of_Attributes (Node.View));
      -- For the result.

      Unused_Next : In_Memory.Index_T;
      -- Unused index of next octet after entry.

   begin

      Abbrev.Digest (
         View   => Node.View,
         Giving => List,
         Next   => Unused_Next);

      return List;

   end Values;


   --    Integer numbers ("constants")


   procedure Raise_Wrong_Form (
      Actual : in Value_Def_T;
      Expect : in String)
   --
   -- Raises Wrong_Form to signal that the Actual form of an
   -- attribute is not the Expected one.
   --
   is
   begin

      if Opt.Trace_Loading then

         Output.Trace (
              "DWARF attribute "
            & Attribute_T'Image (Actual.Attribute)
            & " has form "
            & True_Form_T'Image (Actual.Form)
            & ", not "
            & Expect);

      end if;

      raise Wrong_Form;

   end Raise_Wrong_Form;


   function Value_Of (
      Item : Value_Def_T;
      From : Node_T)
   return Number_T
   is

      Info : constant In_Memory.Stream_Ref := From.View.Sect.Info;
      -- Abbreviation.

   begin

      In_Memory.Set_Index (Info.all, Item.Index);

      case Item.Form is

      when Constant_Form_T =>

         return Number (Info, Item.Form);

      when others =>

         Raise_Wrong_Form (Item, "constant");

         return 0;   -- Dummy, not reached.

      end case;

   end Value_Of;


   function Value_Of (
      Attribute : Attribute_T;
      Within    : Value_Def_List_T;
      From      : Node_T)
   return Number_T
   is
   begin

      return Value_Of (
         Item => Value_Def (Attribute, Within),
         From => From);

   end Value_Of;


   --    Strings


   function Value_Of (
      Item : Value_Def_T;
      From : Node_T)
   return String
   is
      use type In_Memory.Stream_Ref;

      Str_Offset : In_Memory.Offset_T;
      -- The offset in the .debug_str section, when Form = Strp.

   begin

      case Item.Form is

      when String_Form =>

         In_Memory.Set_Index (From.View.Sect.Info.all, Item.Index);

         return String_To_Null (From.View.Sect.Info);

      when Strp =>

         In_Memory.Set_Index (From.View.Sect.Info.all, Item.Index);

         Read_Variant_Unsigned (
            Stream => From.View.Sect.Info,
            Bits   => From.Compilation.Header.Unit_Length.Bits,
            Item   => Variant_Unsigned_T (Str_Offset));

         if From.View.Sect.Strings /= null then
            -- We have a Strings section.

            In_Memory.Set_Index (
               Stream => From.View.Sect.Strings.all,
               To     => In_Memory.Index_T (Str_Offset + 1));

            return String_To_Null (From.View.Sect.Strings);

         else

            Output.Error (
                 "DWARF value for Strp attribute "
               & Attribute_T'Image (Item.Attribute)
               & " at offset"
               & In_Memory.Offset_T'Image (Str_Offset)
               & " not available.");

            return "";

         end if;

      when others =>

         Raise_Wrong_Form (Item, "string or string pointer");

         return "";   -- Dummy, not reached.

      end case;

   end Value_Of;


   function Value_Of (
      Attribute : Attribute_T;
      Within    : Value_Def_List_T;
      From      : Node_T)
   return String
   is
   begin

      return Value_Of (
         Item => Value_Def (Attribute, Within),
         From => From);

   end Value_Of;


   function Value_Of (
      Attribute : Attribute_T;
      Within    : Value_Def_List_T;
      From      : Node_T;
      Default   : String)
   return String
   is
   begin

      return Value_Of (Attribute, Within, From);

   exception

   when Attribute_Absent =>

      return Default;

   end Value_Of;


   --    Addresses


   function Value_Of (
      Item : Value_Def_T;
      From : Node_T)
   return Address_T
   is

      Address : Address_T;
      -- The result.

   begin

      case Item.Form is

      when Addr =>

         In_Memory.Set_Index (From.View.Sect.Info.all, Item.Index);

         Read_Address (
            Stream => From.View.Sect.Info,
            Size   => Abbrev.Key (From.View.Set).Addr_Size,
            Item   => Address);

         return Address;

      when others =>

         Raise_Wrong_Form (Item, "address");

         return Address_T'First;  -- Dummy, not reached.

      end case;

   end Value_Of;


   function Value_Of (
      Attribute : Attribute_T;
      Within    : Value_Def_List_T;
      From      : Node_T)
   return Address_T
   is
   begin

      return Value_Of (
         Item => Value_Def (Attribute, Within),
         From => From);

   end Value_Of;


   function Value_Of (
      Attribute : Attribute_T;
      Within    : Value_Def_List_T;
      From      : Node_T;
      Default   : Address_T)
   return Address_T
   is
   begin

      return Value_Of (Attribute, Within, From);

   exception

   when Attribute_Absent =>

      return Default;

   end Value_Of;


   --    Blocks


   function Value_Of (
      Item : Value_Def_T;
      From : Node_T)
   return Block_T
   is

      Info : constant In_Memory.Stream_Ref := From.View.Sect.Info;
      -- An abbreviation.

      Block_Len : In_Memory.Count_T;
      -- The length of the block.

      Key : constant Abbrev.Set_Key_T := Abbrev.Key (From.View.Set);
      -- The key of the abbreviation set.

   begin

      case Item.Form is

      when Block_Form_T =>

         In_Memory.Set_Index (Info.all, Item.Index);

         Block_Len := Block_Length (
            Form   => Item.Form,
            Stream => Info);

         return (
            Stream    => Info,
            Start     => In_Memory.Index (Info.all),
            Octets    => Block_Len,
            Addr_Size => Key.Addr_Size,
            Bits      => Key.Bits);

      when others =>

         Raise_Wrong_Form (Item, "block");

         -- The following is a dummy, not reached:

         return (
            Stream    => Info,
            Start     => In_Memory.Index_T'First,
            Octets    => 0,
            Addr_Size => 0,
            Bits      => Key.Bits);

      end case;

   end Value_Of;


   function Value_Of (
      Attribute : Attribute_T;
      Within    : Value_Def_List_T;
      From      : Node_T)
   return Block_T
   is
   begin

      return Value_Of (
         Item => Value_Def (Attribute, Within),
         From => From);

   end Value_Of;


   function Location_List (
      Offset : Section_Offset_T;
      From   : Node_T)
   return Block_T
   is
      use type In_Memory.Stream_Ref;
      use type In_Memory.Index_T;

      View : Abbrev.View_T    renames From.View;
      Key  : Abbrev.Set_Key_T renames Abbrev.Key (View.Set);

   begin

      if View.Sect.Locations = null then

         raise No_Location_Section;

      end if;

      if Offset <  0
      or Offset >= Section_Offset_T (View.Sect.Locations.Length)
      then

         raise Invalid_Location_Offset;

      end if;

      return (
         Stream    => View.Sect.Locations,
         Start     => In_Memory.Index_T'First + In_Memory.Offset_T (Offset),
         Octets    => 0,    -- not known.
         Addr_Size => Key.Addr_Size,
         Bits      => Key.Bits);

   end Location_List;


   --    References to other DWARF entries


   function Value_Of (
      Item : Value_Def_T;
      From : Node_T)
   return Node_Index_T
   is
   begin

      case Item.Form is

      when Reference_Form_T =>

         In_Memory.Set_Index (From.View.Sect.Info.all, Item.Index);

         return Node_Index_T (Reference (
            Info => From.View.Sect.Info,
            Form => Item.Form,
            Bits => From.Compilation.Header.Unit_Length.Bits,
            Base => From.View.Refz));

      when others =>

         Raise_Wrong_Form (Item, "reference");

         return Node_Index_T'First;   -- Dummy, not reached.

      end case;

   end Value_Of;


   function Value_Of (
      Attribute : Attribute_T;
      Within    : Value_Def_List_T;
      From      : Node_T)
   return Node_Index_T
   is
   begin

      return Value_Of (
         Item => Value_Def (Attribute, Within),
         From => From);

   end Value_Of;


   --    Types of data objects


   function Type_Of (
       Node   : Node_T;
       Values : Value_Def_List_T;
       Tree   : Tree_T)
   return Types.Type_T
   is

      Index : Node_Index_T;
      -- The index of the type entry.

   begin

      Index := Value_Of (Type_Ref, Values, Node);
      -- May raise Attribute_Absent or Wrong_Form.

      return Types.Type_At (
         Index  => Types.Type_Index_T (Index),
         Within => Tree.Types);

   end Type_Of;


   --    Pointers to other DWARF information


   function Value_Of (
      Item : Value_Def_T;
      From : Node_T)
   return Section_Offset_T
   is

      Offset : Section_Offset_T;
      -- The result.

   begin

      In_Memory.Set_Index (From.View.Sect.Info.all, Item.Index);

      case Item.Form is

      when Data4 =>

         Unsigned_32_T'Read (From.View.Sect.Info, Unsigned_32_T (Offset));

      when Data8 =>

         Unsigned_64_T'Read (From.View.Sect.Info, Unsigned_64_T (Offset));

      when others =>

         Raise_Wrong_Form (Item, "data4 or data8");

         return Section_Offset_T'First;  -- Dummy, not reached.

      end case;

      return Offset;

   end Value_Of;


   function Value_Of (
      Attribute : Attribute_T;
      Within    : Value_Def_List_T;
      From      : Node_T)
   return Section_Offset_T
   is
   begin

      return Value_Of (
         Item => Value_Def (Attribute, Within),
         From => From);

   end Value_Of;


   function Value_Of (
      Attribute : Attribute_T;
      Within    : Value_Def_List_T;
      From      : Node_T;
      Default   : Section_Offset_T)
   return Section_Offset_T
   is
   begin

      return Value_Of (Attribute, Within, From);

   exception

   when Attribute_Absent =>

      return Default;

   end Value_Of;


   --
   --    Subprogram attributes
   --


   function Is_Inlined (
      Subprogram : Node_T;
      Values     : Value_Def_List_T)
   return Boolean
   is

      Inline_Found : Boolean;
      -- Whether the Inline attribute is present.

      Inline_Def : Value_Def_T;
      -- The format and index of the Inline value, if Found.

      Inline_Value : Number_T;
      -- The value of the Inline attribute, if Found.

   begin

      Find (
         Attribute => Inline,
         Within    => Values,
         Found     => Inline_Found,
         Value_Def => Inline_Def);

      if not Inline_Found then
         -- This subprogram is not inlined.

         return False;

      else
         -- The Inline value shows the answer.

         Inline_Value := Value_Of (Inline_Def, Subprogram);

         case Inline_Value is

         when 0 =>
            -- Neither declared inline nor inlined by the compiler.

            return False;

         when 1 =>
            -- Not declared inline but inlined by the compiler.

            return True;

         when 2 =>
            -- Declared inline but not inlined by the compiler.

            return False;

         when 3 =>
            -- Declared inline and inlined by the compiler.

            return True;

         when others =>

            Output.Error (
                 "DWARF Inline attribute invalid value"
               & Number_T'Image (Inline_Value));

            return True;
            -- Assume it's inlined, since Inline is present.

         end case;

      end if;

   end Is_Inlined;


   procedure Find_Entry_Point (
      Subprogram : in     Node_T ;
      Values     : in     Value_Def_List_T;
      Found      :    out Boolean;
      Address    :    out Address_T)
   is

      VD : Value_Def_T;
      -- The format and index of the entry point value, if Found.

   begin

      Find (
         Attribute => Entry_PC,
         Within    => Values,
         Found     => Found,
         Value_Def => VD);

      if not Found then

         Find (
            Attribute => Low_PC,
            Within    => Values,
            Found     => Found,
            Value_Def => VD);

      end if;

      if Found then

         Address := Value_Of (Item => VD, From => Subprogram);

      else

         Address := Address_T'First;

      end if;

   end Find_Entry_Point;


   --
   --    Points in a traversal
   --


   type Level_T is record
      Element : Element_Ref;
      Node    : Node_T;
   end record;
   --
   -- A level in the path to a point.


   type Level_List_T is array (Positive range <>) of Level_T;
   --
   -- A list of levels, usually a path from the root of a DWARF tree
   -- to some point within the tree.


   type Point_Object_T (Max_Depth : Positive) is record
      Tree        : Tree_T;
      Comp_Index  : Positive := 1;
      Compilation : Compilation_Ref;
      Depth       : Natural := 0;
      Gone_To     : Boolean;
      Stop        : Boolean := False;
      Path        : Level_List_T (1 .. Max_Depth);
   end record;
   --
   -- A point in the traversal of a DWARF tree.
   --
   -- Max_Depth
   --    The maximum depth of the traversal.
   --    Max_Depth = 1 will visit only the first (top) node in each
   --    compilation unit, which should nominally be a Compile_Unit node.
   -- Tree
   --    The tree being traversed.
   -- Comp_Index
   --    The index of the current compilation unit, in Root.Compilations.
   -- Compilation
   --    The current compilation unit.
   -- Depth
   --    The current family descent depth.
   -- Gone_To
   --    Whether the Visit operation has applied some Go_To_Xxx
   --    operation(s) to this Point, overriding the default traversal
   --    order.
   -- Stop
   --    Whether the Visit operation has applied Stop_Traversal.
   -- Path
   --    The current family descent path is Path(1 .. Depth).
   --    The current node being visited is Path(Depth).
   --
   -- The Go_To_Xxx operations can alter all components except Root.


   procedure Unchecked_Discard
   is new Ada.Unchecked_Deallocation (
      Name   => Point_T,
      Object => Point_Object_T);


   procedure Discard (Item : in out Point_T)
   --
   -- Unchecked deallocation depending on Opt.Deallocate.
   --
   is
   begin

      if Opt.Deallocate then

         Unchecked_Discard (Item);

      end if;

   end Discard;


   function Tree (Point : Point_T) return Tree_T
   is
   begin

      return Point.Tree;

   end Tree;


   function Node_At (Point : Point_T) return Node_T
   is
   begin

      return Point.Path(Point.Depth).Node;

   end Node_At;


   function Tag (Point : Point_T) return Tag_T
   is
   begin

      return Tag (Point.Path(Point.Depth).Node);

   end Tag;


   function Depth (Point : Point_T) return Positive
   is
   begin

      return Point.Depth;

   end Depth;


   function Has_Children (Point : Point_T) return Boolean
   is
   begin

      return Point.Path(Point.Depth).Element.Child /= null;

   end Has_Children;


   function Can_Open (Point : Point_T) return Boolean
   is
   begin

      return Point.Depth < Point.Max_Depth
      and then Has_Children (Point);

   end Can_Open;


   function Has_More_Siblings (Point : Point_T) return Boolean
   is
   begin

      return Point.Path(Point.Depth).Element.Sibling /= null;

   end Has_More_Siblings;


   function Has_More_Parent_Siblings (
      Point : Point_T;
      Level : Positive := 1)
   return Boolean
   is
   begin

      return
         Level < Point.Depth
         and then
            Point.Path(Point.Depth - Level).Element.Sibling /= null;

   end Has_More_Parent_Siblings;


   procedure Change_Path (
      Remove : in     Natural;
      Add    : in     Element_Ref;
      Point  : in out Point_T)
   --
   -- Removes a given number of elements from the path of the Point and
   -- then Adds a given element to the path as the new deepest level.
   -- Sets Point.Gone_To to True.
   --
   -- Assumes that Element is in the current compilation of Point.
   --
   -- Propagates No_Such_Point if the maximum depth would be exceeded and
   -- does not modify Point in such case.
   --
   is
   begin

      if Point.Depth - Remove >= Point.Max_Depth then
         -- Exceeding maximum depth!

         raise No_Such_Point;

      else

         Point.Depth := Point.Depth - Remove + 1;

         Point.Path(Point.Depth) := (
            Element => Add,
            Node    => Node_For (
               Element => Add,
               Within  => Point.Compilation));

         Point.Gone_To := True;

      end if;

   end Change_Path;


   procedure Go_To_First_Child (Point : in out Point_T)
   is
   begin

      if not Has_Children (Point) then
         -- No children, cannot go.

         raise No_Such_Point;

      elsif Point.Depth >= Point.Max_Depth then
         -- Exceeding maximum depth, cannot go.

         raise No_Such_Point;

      else
         -- There are children, and they are not too deep.

         Change_Path (
            Remove => 0,
            Add    => Point.Path(Point.Depth).Element.Child,
            Point  => Point);

      end if;

   end Go_To_First_Child;


   procedure Go_To_Next_Sibling (Point : in out Point_T)
   is
   begin

      if not Has_More_Siblings (Point) then
         -- No more siblings, cannot go.

         raise No_Such_Point;

      else

         Change_Path (
            Remove => 1,
            Add    => Point.Path(Point.Depth).Element.Sibling,
            Point  => Point);

      end if;

   end Go_To_Next_Sibling;


   procedure Go_To_Next_Parent_Sibling (
      Point : in out Point_T;
      Level : in     Positive := 1)
   is
   begin

      if not Has_More_Parent_Siblings (Point, Level) then
          -- No more siblings at that ancestor Level, cannot go.

          raise No_Such_Point;

      else

         Change_Path (
            Remove => Level + 1,
            Add    => Point.Path(Point.Depth - Level).Element.Sibling,
            Point  => Point);

      end if;

   end Go_To_Next_Parent_Sibling;


   procedure Stop_Traversal (Point : in out Point_T)
   is
   begin

      Point.Stop := True;

   end Stop_Traversal;


   function Ancestors (
      Point : Point_T;
      Depth : Positive)
   return Ancestors_T
   is

      Ancs : Node_List_T (1 .. Positive'Min (Depth, Point.Depth));

   begin

      for A in Ancs'Range loop

        Ancs(A) := Point.Path(A).Node;

      end loop;

      return Ancs;

   end Ancestors;


   procedure Trace_Traversal (
      Text   : in String;
      Point  : in Point_T)
   --
   -- Traces a traversal action connected to the given Point.
   -- The Text parameter is for Output.Trace.
   -- The node at Point is displayed with Abbrev.Put (View).
   --
   is

      Node : Node_T renames Point.Path(Point.Depth).Node;
      -- The node at this Point.

      Dummy_Next : In_Memory.Index_T;
      -- Unused output from Abbrev.Put.

   begin

      Output.Trace ("DWARF traversal " & Text);

      Abbrev.Put (
         View   => Node.View,
         Indent => "   ",
         CUBA   => Node.Compilation.CUBA,
         Next   => Dummy_Next);

   end Trace_Traversal;


   procedure Go_To_Compilation (
      Index   : in     Positive;
      Point   : in out Point_T;
      Visitor : in out Visitor_T'Class)
   --
   -- Moves the traversal Point to the first element in the
   -- compilation unit with the given Index.
   --
   is
   begin

      if Index > Last (Point.Tree.Compilations) then
         -- No more compilations, cannot go.

         if Opt.Trace_Traversal then

            Output.Trace ("ends after last compilation.");

         end if;

         raise No_Such_Point;

      else

         Point.Comp_Index := Index;

         Point.Compilation :=
            Element (Point.Tree.Compilations, Index);

         Change_Path (
            Remove => Point.Depth,
            Add    => Point.Compilation.First_Element,
            Point  => Point);

         if Opt.Trace_Traversal then

            Trace_Traversal (
               Text  => "enters compilation #" & Positive'Image (Index),
               Point => Point);

         end if;

         Enter_Compilation (Start => Point, Visitor => Visitor);

         Point.Gone_To := True;

      end if;

   end Go_To_Compilation;


   procedure Enter_Compilation (
      Start   : in     Point_T;
      Visitor : in out Visitor_T)
   is
   begin

      null;

   end Enter_Compilation;


   procedure Descend (
      From    : in     Point_T;
      Visitor : in out Visitor_T)
   is
   begin

      null;

   end Descend;


   procedure Ascend (
      To      : in     Point_T;
      Visitor : in out Visitor_T)
   is
   begin

      null;

   end Ascend;


   procedure Go_To_Next_Point (
      Point   : in out Point_T;
      Opening : in     Tag_Set_T;
      Visitor : in out Visitor_T'Class)
   --
   -- Moves the traversal point to the next point in the default
   -- traversal order. Point.Gone_To is returned as True if the
   -- traversal continues and False otherwise.
   --
   is
   begin

      Point.Gone_To := False;

      if Can_Open (Point)
      and then Opening (Tag (Point))
      then

         if Opt.Trace_Traversal then

            Trace_Traversal (
               Text  => "opening subtree at node:",
               Point => Point);

         end if;

         Descend (From => Point, Visitor => Visitor);

         Go_To_First_Child (Point);

      elsif Has_More_Siblings (Point) then

         Go_To_Next_Sibling (Point);

      else
         -- No (reachable) children, no own siblings left.

         -- Find a parent with more siblings:

         while Point.Depth > 1 loop

            Point.Depth := Point.Depth - 1;

            if Opt.Trace_Traversal then

               Trace_Traversal (
                  Text  => "closing subtree at node:",
                  Point => Point);

            end if;

            Ascend (To => Point, Visitor => Visitor);

            if Has_More_Siblings (Point) then

               Go_To_Next_Sibling (Point);

               exit;

            end if;

         end loop;

      end if;

      if not Point.Gone_To
      and then
         Point.Comp_Index < Last (Point.Tree.Compilations)
      then
         -- Go to the next compilation unit.

         Go_To_Compilation (
            Index   => Point.Comp_Index + 1,
            Point   => Point,
            Visitor => Visitor);

      end if;

   end Go_To_Next_Point;


   procedure Traverse (
      Tree     : in     Tree_T;
      Opening  : in     Tag_Set_T;
      Visiting : in     Tag_Set_T;
      Depth    : in     Positive;
      Visitor  : in out Visitor_T'Class)
   is

      Point : Point_T;
      -- The traversal point.

   begin

      -- Start from the first element of the first compilation unit:

      Point := new Point_Object_T (Max_Depth => Depth);

      Point.Tree := Tree;

      Go_To_Compilation (Index => 1, Point => Point, Visitor => Visitor);

      -- Traverse to the end or to Stop:

      Traversal:
      loop

         Point.Gone_To := False;

         if Visiting(Tag (Point)) then
            -- This Tag is interesting to the Visitor.

            if Opt.Trace_Traversal then

               Trace_Traversal (
                  Text  => "visiting node:",
                  Point => Point);

            end if;

            Visit (
               Point   => Point,
               Visitor => Visitor);

            if Point.Stop then

               if Opt.Trace_Traversal then

                  Output.Trace ("DWARF traversal Stopped.");

               end if;

               exit Traversal;

            end if;

         end if;

         if not Point.Gone_To then
            -- The Visitor was not called or did not use Go_To_Xxx,
            -- so we apply the default traversal order:

            Go_To_Next_Point (Point, Opening, Visitor);

            exit when not Point.Gone_To;
            -- End of traversal after last compilation unit.

         end if;

      end loop Traversal;

      Discard (Point);

   exception

      when others =>

         Discard (Point);

         raise;

   end Traverse;


end Formats.Dwarf.Info;
