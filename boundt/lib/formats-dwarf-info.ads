-- Formats.Dwarf.Info (decl)
--
-- The DWARF debugging information tree.
--
-- The DWARF information has several parts of facets which are implemented
-- by dedicated child packages of Formats.Dwarf. The whole information is
-- structured as a tree of debugging information entries (DIEs).
--
-- The debugging information entries are stored in the section called
-- ".debug_info" using a set of abbreviations (really entry templates)
-- defined in the section called ".debug_abbrev". The present package,
-- Formats.Dwarf.Info, provides access to the entries in the ".debug_info"
-- section with assistance from the sibling package Formats.Dwarf.Abbrev
-- that understands the ".debug_abbrev" section. These entries contain
-- references to other DWARF facets such as call-frame descriptions and
-- source-code line-number tables.
--
-- Two kinds of access to the DWARF information are provided:
--
-- > Tree traversal can traverse the whole DWARF tree, or selected
--   parts of the tree, under the control of a client-supplied visitor
--   object.
--
-- > Keyed access to information about a specific item identified by
--   DWARF reference, symbolic name (identifier), or machine address.
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
-- $Log: formats-dwarf-info.ads,v $
-- Revision 1.8  2015/10/24 20:53:52  niklas
-- Moved to free licence.
--
-- Revision 1.7  2008-10-15 12:05:53  niklas
-- BT-CH-0150: DWARF location lists, lexical block scopes, CUBAs.
--
-- Revision 1.6  2007/06/14 11:16:40  niklas
-- BT-CH-0060.
--
-- Revision 1.5  2007/04/26 11:27:52  niklas
-- BT-CH-0059.
--
-- Revision 1.4  2007/04/18 18:35:49  niklas
-- BT-CH-0057.
--
-- Revision 1.3  2007/03/29 12:51:48  niklas
-- BT-CH-0056.
--
-- Revision 1.2  2006/04/12 19:32:22  niklas
-- Added "in" parameters Load.Str and Dump.Str to reference the
-- section .debug_str that holds attribute values for Form = Strp.
-- Added Value_Of attribute functions that take a Value_Def_T,
-- not a whole Value_Def_List_T.
-- Added Value_Of attribute functions for integer values.
-- Added specific operations for subprogram attributes in the
-- form of function Is_Inlined and procedure Find_Entry_Point.
--
-- Revision 1.1  2004/04/24 18:06:20  niklas
-- First version.
--

--:dbpool with GNAT.Debug_Pools;

with Formats.Dwarf.Abbrev;
with Formats.Dwarf.Types;
with Formats.In_Memory;


package Formats.Dwarf.Info is


   Section_Name : constant String := ".debug_info";
   --
   -- The name of the DWARF section that contains the debugging
   -- information entries.


   type Tree_T is private;
   --
   -- A DWARF tree as represented in a ".debug_info" section.
   -- The tree is structured as
   --
   -- > A sequence of compilations (or compilation units), each of which
   --   contains
   --
   -- >> A sequence of top-level debugging information entries (usually
   --    only one Compile_Unit entry), each of which contains
   --
   -- >>> A list of attribute values, and possibly (nominally)
   --
   -- >>>> A hierarchy of lower-level debugging information entries (for
   --      entities such as modules, subprograms, types, variables ...)
   --      each of which contains attribute values and possibly child
   --      entries.
   --
   -- Each debugging information entry is characterized by a tag (Tag_T)
   -- that shows the kind of program entity the entry represents.
   -- The list of attributes included in an entry is variable, even
   -- within entries with the same tag. Each entry conforms to some
   -- entry abbreviation (template) defined in the ".debug_abbrev"
   -- section.
   --
   -- The abbreviation defines the tag, the list of attributes provided,
   -- for each attribute the form in which the attribute value is encoded
   -- in the ".debug_info" section, and whether the entry can own child
   -- entries.
   --
   -- In the ".debug_info" section, a debugging information entry is
   -- represented merely by the code of the abbreviation entry followed
   -- by the encodings of the attribute values.
   --
   -- Each compilation unit refers (via an offset) to the set of
   -- abbreviations used by its debugging information entries. Several
   -- compilation units can use the same set.
   --
   -- The physical subdivision of the ".debug_info" section into sub-
   -- sections for each  compilation unit is not visible through an
   -- object of type Tree_T. The top-level nodes in the tree (below the
   -- notional root node) are the debugging information entries tagged
   -- Compile_Unit. There is no real root node above the Compile_Unit
   -- nodes so a Tree_T is really a forest of Compile_Unit trees.
   --
   -- In addition to the tree structure, some entries refer to other
   -- entries thru DWARF "reference" attributes. For example, an entry
   -- that represents a variable usually has a Type_Ref attribute that
   -- refers to the entry that defines the type of the variable. A Tree
   -- object includes an indexed structure for quick access to type
   -- descriptions (Formats.Dwarf.Types.Type_T).


   procedure Load (
      Info   : in     Segment_Desc_T;
      Abbrev : in     Segment_Desc_T;
      Str    : in     Segment_Desc_T;
      Loc    : in     Segment_Desc_T;
      From   : in     IO.File_Type;
      Tree   :    out Tree_T);
   --
   -- Loads the DWARF Info and Abbrev sections From the given file,
   -- giving a DWARF Tree.
   --
   -- Optionally, a Str (Strings) section may be present and may be
   -- referenced through "Strp" attributes. The absence of a Str
   -- section should be indicated by setting Str.Octets to zero.
   --
   -- Optionally, a Loc (Locations) section may be present and may be
   -- referenced through "locptr" attributes. The absence of a Loc
   -- section should be indicated by setting Loc.Octets to zero.


   procedure Dump (
      Info   : in Segment_Desc_T;
      Abbrev : in Segment_Desc_T;
      Str    : in Segment_Desc_T;
      Loc    : in Segment_Desc_T;
      From   : in IO.File_Type);
   --
   -- Displays the DWARF Info and Abbrev sections From the given
   -- file on standard output.
   --
   -- Optionally, a Str (Strings) section may be present and may be
   -- referenced through "Strp" attributes. The absence of a Str
   -- section should be indicated by setting Str.Octets to zero.
   --
   -- Optionally, a Loc (Locations) section may be present and may be
   -- referenced through "locptr" attributes. The absence of a Loc
   -- section should be indicated by setting Loc.Octets to zero.


   --
   --    Nodes in the DWARF tree
   --


   type Node_T is private;
   --
   -- A node in the DWARF tree. A "debugging information entry" in the
   -- DWARF terminology. The attributes (in the Ada sense) of a node
   -- include:
   --
   -- > The DWARF tag, e.g. DW_TAG_subprogram, that identifies the kind
   --   of program item the node represents.
   --
   -- > The DWARF attributes that provide information about the program
   --   item the node represents.
   --
   -- Implementation-oriented attributes (in the Ada sense) include
   --
   -- > The abbreviation code that identifies the abbrevation entry that
   --   lists the DWARF attributes attached to the node and the form of
   --   each attribute value.
   --
   -- > The compilation unit that contains this node.


   type Node_Index_T is private;
   --
   -- An index (number) that uniquely identifies each node in a
   -- DWARF tree. Not necessarily a dense, consecutive numbering, so
   -- should not be used as an array index.


   type Node_List_T is array (Positive range <>) of Node_T;
   --
   -- A list of nodes. The order is usually significant, but the index
   -- values usually not.


   subtype Ancestors_T is Node_List_T;
   --
   -- The list of ancestors of a given node, in top-down order (parents
   -- before children) starting (nominally) from a compilation unit node.
   -- The ancestor list _of_ a compilation-unit node is nominally empty.


   function Index (Node : Node_T) return Node_Index_T;
   --
   -- The unique identifying index of the Node.


   function Abbrev_Code (Node : Node_T) return Abbrev.Code_T;
   --
   -- The abbreviation code of the Node.


   function Tag (Node : Node_T) return Tag_T;
   --
   -- The tag of the Node, showing the kind of item the Node represents.


   function Address_Size (Node : Node_T) return Addr_Size_T;
   --
   -- The address size of the compilation unit that contains this Node.


   function CUBA (Node : Node_T) return Poss_Code_Address_T;
   --
   -- The Compilation Unit Base Address of the compilation unit
   -- that contains this Node.


   --
   --    Attribute values from DWARF tree nodes
   --


   function Values (Node : Node_T) return Value_Def_List_T;
   --
   -- The attributes in the node, and the form of each attribute value.
   -- This list must be retrieved from the node before one can get
   -- the value of any selected attribute.


   --    Integer numbers ("constants")


   function Value_Of (
      Item : Value_Def_T;
      From : Node_T)
   return Number_T;
   --
   -- The value of an attribute Item of integer type, From a
   -- given node. Propagates Wrong_Form if the attribute value
   -- is not an integer number.


   function Value_Of (
      Attribute : Attribute_T;
      Within    : Value_Def_List_T;
      From      : Node_T)
   return Number_T;
   --
   -- The value of an attribute of integer type.
   -- Propagates Attribute_Absent if the node does not define
   -- this Attribute, or Wrong_Form if the attribute is defined
   -- but not as an integer number.


   --    Strings


   function Value_Of (
      Item : Value_Def_T;
      From : Node_T)
   return String;
   --
   -- The value of an attribute Item of string type, From a
   -- given node. Propagates Wrong_Form if the attribute value
   -- is not a string.


   function Value_Of (
      Attribute : Attribute_T;
      Within    : Value_Def_List_T;
      From      : Node_T)
   return String;
   --
   -- The value of an attribute of string type.
   -- Propagates Attribute_Absent if the node does not define
   -- this Attribute, or Wrong_Form if the attribute is defined
   -- but not as a string.


   function Value_Of (
      Attribute : Attribute_T;
      Within    : Value_Def_List_T;
      From      : Node_T;
      Default   : String)
   return String;
   --
   -- The value of an attribute of string type.
   -- Returns the Default if the node does not define this Attribute.
   -- Propagates Wrong_From of the attribute is defined but not as
   -- a string.


   --    Addresses


   function Value_Of (
      Item : Value_Def_T;
      From : Node_T)
   return Address_T;
   --
   -- The value of an attribute Item of address type, From a
   -- given node. Propagates Wrong_Form if the attribute value
   -- is not in a form that yields an address.


   function Value_Of (
      Attribute : Attribute_T;
      Within    : Value_Def_List_T;
      From      : Node_T)
   return Address_T;
   --
   -- The value of an attribute of address type.
   -- Propagates Attribute_Absent if the node does not define
   -- this Attribute, or Wrong_Form if the attribute is defined
   -- but not in a form that yields an address.


   function Value_Of (
      Attribute : Attribute_T;
      Within    : Value_Def_List_T;
      From      : Node_T;
      Default   : Address_T)
   return Address_T;
   --
   -- The value of an attribute of address type.
   -- Returns the Default if the node does not define this Attribute.
   -- Propagates Wrong_From if the attribute is defined but not in
   -- a form that yields an address.


   --    Blocks


   function Value_Of (
      Item : Value_Def_T;
      From : Node_T)
   return Block_T;
   --
   -- The value (description) of an attribute of a block type.
   -- Propagates Wrong_Form if the attribute value is not in a
   -- block form.


   function Value_Of (
      Attribute : Attribute_T;
      Within    : Value_Def_List_T;
      From      : Node_T)
   return Block_T;
   --
   -- The value (description) of an attribute of a block type.
   -- Propagates Attribute_Absent if the node does not define
   -- this Attribute, or Wrong_Form if the attribute is defined
   -- but not in a block form.


   No_Location_Section : exception;
   --
   -- Signals an attempt to find a location-list block when
   -- the DWARF file has no location-list section.


   Invalid_Location_Offset : exception;
   --
   -- Signals an attempt to use a location-list offset (loclistptr)
   -- that points outside the location-list section.


   function Location_List (
      Offset : Section_Offset_T;
      From   : Node_T)
   return Block_T;
   --
   -- The block that contains a location list for an attribute
   -- of class loclistptr with the value Offset.
   --
   -- Propagates No_Location_Section if the DWARF file has no
   -- location-list section. Propagates Invalid_Location_Offset
   -- if the Number is not a valid offset into the location-list
   -- section.
   --
   -- The number of octets in the location-list block is unknown
   -- and is returned as zero.


   --    References to other DWARF entries


   function Value_Of (
      Item : Value_Def_T;
      From : Node_T)
   return Node_Index_T;
   --
   -- The value of an attribute that refers to another DWARF entry.
   -- Propagates Wrong_Form if the attribute value is not in a form
   -- that yields an entry reference.


   function Value_Of (
      Attribute : Attribute_T;
      Within    : Value_Def_List_T;
      From      : Node_T)
   return Node_Index_T;
   --
   -- The value of an attribute that refers to another DWARF entry.
   -- Propagates Attribute_Absent if the node does not define
   -- this Attribute, or Wrong_Form if the attribute is defined
   -- but not in a form that yields an entry reference.


   --    Types of data objects


   function Type_Of (
       Node   : Node_T;
       Values : Value_Def_List_T;
       Tree   : Tree_T)
   return Types.Type_T;
   --
   -- The type of the data object represented by the given Node
   -- in the Tree, from the Type_Ref attribute within the attribute
   -- Values of the Node.
   --
   -- Propagates exceptions as follows:
   --
   -- > Attribute_Absent if the node does not have a Type_Ref attribute,
   --
   -- > Wrong_Form if the attribute is defined but not in a form that
   --   yields an entry reference,
   --
   -- > Types.No_Such_Type if the Tree has no record of the type
   --   referenced by the Type_Ref value.


   --    Pointers to other DWARF information
   --
   -- This covers the following DWARF classes, all of which stand for
   -- offsets into other DWARF sections and point to additional or
   -- related information:
   --
   --   Class          Offset into section
   --   -----          -------------------
   --   lineptr        .debug_line
   --   loclistptr     .debug_loc
   --   macptr         .debug_macinfo
   --   rangelistptr   .debug_ranges
   --
   -- In the DWARF info, values of these classes are encoded using the
   -- forms Data4 or Data8. We use type Section_Offset_T for them.


   function Value_Of (
      Item : Value_Def_T;
      From : Node_T)
   return Section_Offset_T;
   --
   -- The value of an attribute Item of pointer (section offset)
   -- class, From a given node. Propagates Wrong_Form if the attribute
   -- value is not in a form that yields a pointer.


   function Value_Of (
      Attribute : Attribute_T;
      Within    : Value_Def_List_T;
      From      : Node_T)
   return Section_Offset_T;
   --
   -- The value of an attribute of pointer (section offset) class.
   -- Propagates Attribute_Absent if the node does not define
   -- this Attribute, or Wrong_Form if the attribute is defined
   -- but not in a form that yields a pointer.


   function Value_Of (
      Attribute : Attribute_T;
      Within    : Value_Def_List_T;
      From      : Node_T;
      Default   : Section_Offset_T)
   return Section_Offset_T;
   --
   -- The value of an attribute of pointer (section offset) class.
   -- Returns the Default if the node does not define this Attribute.
   -- Propagates Wrong_From of the attribute is defined but not in
   -- a form that yields a pointer.


   --
   --    Subprogram attributes
   --


   function Is_Inlined (
      Subprogram : Node_T;
      Values     : Value_Def_List_T)
   return Boolean;
   --
   -- Whether this Subprogram is inlined, either by specific
   -- declaration (pragma inline) or by the compiler's decision.
   -- The function checks for the presence and value of the
   -- Inline attribute.


   procedure Find_Entry_Point (
      Subprogram : in     Node_T ;
      Values     : in     Value_Def_List_T;
      Found      :    out Boolean;
      Address    :    out Address_T);
   --
   -- Finds the entry point of the Subprogram represented by
   -- a given DWARF node, with the given attribute Values.
   -- The entry point is at the returned Address, if Found.
   --
   -- The procedure checks the relevant attributes in the
   -- following order and uses the first available one:
   --
   -- > Entry_PC
   -- > Low_PC


   --
   --    Traversal points in the DWARF tree
   --


   type Point_T is limited private;
   --
   -- A point in a traversal of a DWARF tree. This is basically a Node
   -- with additional information about the family relationships such
   -- as ancestry. The attributes of a Point include:
   --
   -- > The node at this point.
   --
   -- > The ancestry of the point/node.
   --
   -- > The tree that contains this point/node.
   --
   -- > Means to access the first child node, or the next sibling node,
   --   or the parent node.
   --
   -- > Whether the traversal can descend to deeper levels (as
   --   limited by the maximum Depth parameter to the Traverse
   --   operation, see below).


   function Tree (Point : Point_T) return Tree_T;
   --
   -- The DWARF tree that contains the Point.


   function Node_At (Point : Point_T) return Node_T;
   --
   -- The node at the given Point.


   function Tag (Point : Point_T) return Tag_T;
   --
   -- The same as Tag (Node (Point)).


   function Depth (Point : Point_T) return Positive;
   --
   -- The family depth of the point, with 1 standard for the top-most
   -- level of the DWARF tree (nominally Compile_Unit entries).


   function Has_Children (Point : Point_T) return Boolean;
   --
   -- Whether the Point has children, that is, whether the node of
   -- this Point owns other nodes. Same as Has_Children (Node (Point)).


   function Can_Open (Point : Point_T) return Boolean;
   --
   -- Whether the Point has children and the traversal can descend
   -- to these children without exceeding the maximum traversal depth.


   function Has_More_Siblings (Point : Point_T) return Boolean;
   --
   -- Whether the Point has more (younger) siblings among all the
   -- the children of the Point's parent node. Same as
   -- Has_More_Siblings (Node (Point)).


   function Has_More_Parent_Siblings (
      Point : Point_T;
      Level : Positive := 1)
   return Boolean;
   --
   -- Whether the Point's parent point, or ancestor point at the given
   -- Level of descent, has more siblings. The levels are counted
   -- bottom-up, so Level = 1 for parent, Level = 2 for grand-parent
   -- and so on. False is returned if Level >= Depth(Point).


   No_Such_Point : exception;
   --
   -- Raised when one of the following Go_To procedures is called
   -- when the target point does not exist or is nested too deeply
   -- in the DWARF tree.


   procedure Go_To_First_Child (Point : in out Point_T);
   --
   -- Moves the traversal point to the first child node of the given Point
   -- or propagates No_Such_Point if the Point has no children or if
   -- the maximum depth of the traversal would be exceeded.


   procedure Go_To_Next_Sibling (Point : in out Point_T);
   --
   -- Moves the traversal point to the next (younger) sibling point of
   -- the given Point, or propagates No_Such_Point if the Point has no more
   -- (younger) siblings.


   procedure Go_To_Next_Parent_Sibling (
      Point : in out Point_T;
      Level : in     Positive := 1);
   --
   -- Moves the traversal point to the next (younger) sibling point of the
   -- given Point's parent point, or ancestor point at the given Level
   -- of descent, or propagates No_Such_Point if the parent/ancestor
   -- point has no more (younger) siblings. The levels are counted
   -- bottom-up, so Level = 1 for parent, Level = 2 for grand-parent
   -- and so on. No_Such_Point if Level >= Depth (Point).


   procedure Stop_Traversal (Point : in out Point_T);
   --
   -- Stops the traversal. Overrides any Go_To_Xxx operation.


   function Ancestors (
      Point : Point_T;
      Depth : Positive)
   return Ancestors_T;
   --
   -- The ancestors of the given Point, from the oldest (root) ancestor
   -- down to the immediate parent of the Point, or for at most the
   -- given Depth of generations, whichever is less.


   --
   --    Tree traversal
   --


   type Visitor_T is abstract tagged limited private;
   --
   -- Represents a client-specified "visitor" that travels over the
   -- DWARF tree, visits selected points and uses their information.


   procedure Enter_Compilation (
      Start   : in     Point_T;
      Visitor : in out Visitor_T);
   --
   -- Enters a new compilation unit at the starting point of the unit.
   -- This is nominally a Compile_Unit node which owns all other nodes
   -- in the unit (it has children but no siblings).
   --
   -- Since Start is the starting point, Depth (Start) will be 1 (no
   -- ancestors).
   --
   -- This operation is called even if the tag of the starting point
   -- is not selected for visiting or opening.
   --
   -- The Visitor cannot here control the traversal (the Start point
   -- is of mode "in" only). However, if the tag of the Start point is
   -- selected for visiting, the call of Enter_Compilation is at once
   -- followed by a call of Visit (Point => Start), and there the
   -- Visitor can affect the future of the traversal.
   --
   -- This is by default a null operation which does nothing.


   procedure Visit (
      Point   : in out Point_T;
      Visitor : in out Visitor_T)
   is abstract;
   --
   -- Visits a Point in a DWARF tree.
   --
   -- The Visitor can decide which point to visit next by applying some
   -- Go_To_Xxx procedures to the Point, or can stop the traversal
   -- by applying the Stop_Traversal procedure to the Point. Otherwise,
   -- the default is to go to the first child if the Point has children,
   -- otherwise to the next sibling if there is one, and otherwise to
   -- the first sibling of the parent, etc. in pre-order.
   --
   -- This operation is abstract and thus must be overridden by derived
   -- Visitor types.


   procedure Descend (
      From    : in     Point_T;
      Visitor : in out Visitor_T);
   --
   -- Descends From the root of a DWARF sub-tree to traverse its children.
   --
   -- The Visitor is informed by this operation when the traversal is
   -- about to examine the first child of the tree node at the given
   -- point (which has already been visited, if allowed by its tag).
   --
   -- In the default traversal order, every Descend call pairs with a
   -- later Ascend call with the same Point parameter.
   --
   -- The Descend operation is not called when the Visitor uses a
   -- Go_To_Xxx operation to control the traversal.
   --
   -- This is by default a null operation which does nothing.


   procedure Ascend (
      To      : in     Point_T;
      Visitor : in out Visitor_T);
   --
   -- Ascends back To the root of a DWARF sub-tree after traversing
   -- its children.
   --
   -- The Visitor is informed by this operation when the traversal has
   -- finished with all the children (and grandchildren etc.) of the
   -- tree node at the given point and is about to continue to the
   -- next sibling of this point.
   --
   -- In the default traversal order, every Ascend call pairs with an
   -- earlier Descend call with the same Point parameter.
   --
   -- The Ascend operation is not called when the Visitor uses a
   -- Go_To_Xxx operation to control the traversal, nor if the
   -- Visitor uses Stop_Traversal to abort the traversal.
   --
   -- This is by default a null operation which does nothing.


   procedure Traverse (
      Tree     : in     Tree_T;
      Opening  : in     Tag_Set_T;
      Visiting : in     Tag_Set_T;
      Depth    : in     Positive;
      Visitor  : in out Visitor_T'Class);
   --
   -- Traverses the Tree under the control of the Visitor.
   --
   -- The Tree is logically the parent of all the top-level points
   -- in the tree. The traversal starts at the first top-level point
   -- which is the first DWARF element in the first compilation unit,
   -- and ends when after visiting the last DWARF element of the last
   -- compilation unit. The Visitor can stop the traversal at any point
   -- by applying the Stop_Traversal operation.
   --
   -- During the default traversal order, only points for nodes with tags
   -- in the Opening set are "opened" for traversal in the sense that the
   -- sub-tree rooted at such a node is traversed, bracketed by calls of
   -- the Open and Close operations. Thus, all the ancestors of any
   -- visited point will have tags in the Opening set. The Opening set
   -- must usually contain the tag Compile_Unit for the traversal
   -- to be useful.
   --
   -- During the traversal, only points for nodes with tags in the Visiting
   -- set are visited. The effect is the same as if the Visitor's Visit
   -- operation were enclosed in a conditional statement of the form
   -- "if Visiting (Tag(Point)) then .. end if". However, the ancestors
   -- of a visited point can have tags that are not in the Tags set.
   --
   -- The default traversal never descends to more than the given Depth
   -- in the family (descent, ownership) tree. Thus, if a visited point
   -- is at this depth, the traversal proceeds to its siblings and not
   -- to its children, if any, just as if the tag of the visited point
   -- were not a member of the Opening set.


private


   type Tree_Object_T;

   type Tree_T is access Tree_Object_T;


   type Compilation_T;

   type Compilation_Ref is access Compilation_T;


   type Element_T;

   type Element_Ref is access Element_T;


   type Node_T is record
      Compilation : Compilation_Ref;
      Index       : Node_Index_T;
      View        : Abbrev.View_T;
   end record;
   --
   -- Compilation
   --    The compilation unit that contains the DWARF entry.
   -- Index
   --    The index of the node in the DWARF info section.
   -- View
   --    A view of the the entry as it resides in some in-memory
   --    copy of the ".debug_info" section. The view shows the
   --    location and true form of each attribute value and also
   --    provides the tag, the abbreviation code, and a reference
   --    to the in-memory copy of the info section.

   type Node_Index_T is new In_Memory.Index_T;


   type Point_Object_T (Max_Depth : Positive);

   type Point_T is access all Point_Object_T;

   --:dbpool Point_Pool : GNAT.Debug_Pools.Debug_Pool;

   --:dbpool for Point_T'Storage_Pool use Point_Pool;


   type Visitor_T is abstract tagged limited null record;


end Formats.Dwarf.Info;
