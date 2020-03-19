-- Formats.Dwarf.Abbrev (decl)
--
-- Abbreviations (templates) for DWARF debugging information entries.
--
-- The DWARF information has several parts of facets which are
-- implemented by dedicated child packages of Formats.Dwarf. The whole
-- information is structured as a tree of debugging information entries.
-- The entries are stored in the section called ".debug_info" using
-- a set of abbreviations (really entry templates) defined in the
-- section called ".debug_abbrev". This package, Formats.Dwarf.Abbrev,
-- provides data types for the abbreviations, operations to load them
-- from the executable file, and operations to access data in debugging
-- information entries as defined by the abbreviation or templates.
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
-- $Revision: 1.6 $
-- $Date: 2015/10/24 20:53:52 $
--
-- $Log: formats-dwarf-abbrev.ads,v $
-- Revision 1.6  2015/10/24 20:53:52  niklas
-- Moved to free licence.
--
-- Revision 1.5  2008-10-15 12:05:52  niklas
-- BT-CH-0150: DWARF location lists, lexical block scopes, CUBAs.
--
-- Revision 1.4  2007/06/14 11:16:39  niklas
-- BT-CH-0060.
--
-- Revision 1.3  2007/01/25 21:25:35  niklas
-- BT-CH-0043.
--
-- Revision 1.2  2006/04/12 19:28:16  niklas
-- Added the component View_T.Str for getting attribute values
-- of Form = Strp from the .debug_str section.
-- Added corresponding "in" parameter View.Str.
--
-- Revision 1.1  2004/04/24 18:06:18  niklas
-- First version.
--


with Ada.Streams;
with Formats.Dwarf.Opt;
with Formats.In_Memory;
with Unbounded_Vectors;


package Formats.Dwarf.Abbrev is


   Section_Name : constant String := ".debug_abbrev";
   --
   -- The name of the DWARF section that contains the abbreviations
   -- (templates) for debugging information entries.


   --
   --    Abbreviation codes
   --


   type Code_T is new Natural;
   --
   -- The abbreviation code that identifies the template (abbreviation)
   -- of a given debugging information entry, within a set of abbreviations.
   -- The value zero defines a null entry.


   procedure Read_Code (
      Stream : access Ada.Streams.Root_Stream_Type'Class;
      Item   :    out Code_T);
   --
   -- Reads an abbreviation code from the Stream.
   --
   for Code_T'Read use Read_Code;


   --
   --    Attribute identifiers and encoding forms
   --


   type Att_Form_T is record
      Attribute  : Attribute_T;
      Form       : Form_T;
   end record;
   --
   -- Defines the identity and form of an attribute in a debugging
   -- information entry (DIE), as part of a DIE abbreviation template.
   --
   -- Attribute
   --    The identity of the attribute.
   -- Form
   --    The form in which the attribute's value is encoded.


   procedure Read_Att_Form (
      Stream : access Ada.Streams.Root_Stream_Type'Class;
      Item   : out    Att_Form_T);
   --
   -- Reads an Att_Form_T from the Stream.
   --
   for Att_Form_T'Read use Read_Att_Form;


   function Image (Item : Att_Form_T) return String;
   --
   -- Readable presentation of the attribute-form pair.


   subtype Att_Form_Index_T is Natural;
   --
   -- Identifies an attribute-form pair in the list of such pairs
   -- that defines the attributes in an abbreviation. Normal values
   -- start from 1, zero has a special (context-dependent) meaning.


   type Att_Form_List_T is array (Att_Form_Index_T range <>) of Att_Form_T;
   --
   -- A list of attributes and forms, usually specifying exactly the
   -- attribute values present in a given info entry.
   -- The index range usually starts at 1.


   --
   --   Abbreviation entries
   --


   type Enty_Ref is private;
   --
   -- A (reference to) an abbreviation entry which is identified by
   -- a code (Code_T) and defines the following things about each and
   -- every debugging information entry (DIE) that refers to this code:
   --
   -- > The tag of the DIE (via the tag-code).
   -- > The number of attributes in the DIE.
   -- > For each attribute, the identity of the attribute (Attribute_T)
   --   and the form of encoding for the value (Form_T).
   -- > Whether the DIE can have (own) child DIEs.
   --
   -- In addition, the entry caches some information about the layout
   -- of the DIE which is useful if some or all of the attributes have
   -- a fixed-length encoding. This layout information depends on some
   -- properties of the compilation unit, so we will store distinct
   -- copies of abbreviation entries for compilation units that have
   -- different values of these properties (the address size and the
   -- DWARF bit-width).


   No_Enty : constant Enty_Ref;
   --
   -- A special value that means the absence of an abbreviation entry.


   function Code (Enty : Enty_Ref) return Code_T;
   --
   -- The abbreviation code for this abbreviation entry.


   function Tag_Code (Enty : Enty_Ref) return Tag_Code_T;
   --
   -- The tag-code defined by the abbreviation.


   function Number_Of_Attributes (Enty : Enty_Ref) return Natural;
   --
   -- The number of attributes defined by the abbreviation.


   function Attributes (Enty : Enty_Ref) return Att_Form_List_T;
   --
   -- The attributes and encoding forms defined by the abbreviation.


   --
   --    Sets of abbreviation entries
   --


   type Set_Key_T is record
      Offset    : Section_Offset_T;
      Addr_Size : Addr_Size_T;
      Bits      : Bits_T;
   end record;
   --
   -- A DWARF ".debug_abbrev" section can contain several different
   -- abbreviation sets, identified by their offset from the start of
   -- the section. Each compilation unit header specifies, by its offset,
   -- the abbreviation set used in this compilation unit. However, we
   -- also store in the abbreviation tables form-length and value-offset
   -- information that depends on the properties of the compilation
   -- unit, specifically the address size (Addr_Size) and the bit-width
   -- (Unit_Length.Bits). Therefore we identify the loaded abbreviation
   -- sets by the three key values offset, address-size, bit-width. If two
   -- compilation units use the same abbreviation set but have different
   -- address size or bit-width (which is unlikely) we load two copies
   -- of the abbreviation set and store them under the corresponding
   -- different keys.


   type Set_Ref is private;
   --
   -- A (reference to) a set of info-entry abbreviations (templates) as
   -- read from the ".debug_abbrev" section. The properties include
   --
   -- > A key of type Set_Key_T which identifies the set uniquely.
   --
   -- > A table of abbreviations, indexed by abbrevation code, loaded
   --   from the ".debug_abbrev" section at the Offset given in the key.
   --
   -- > Fixed layout information that is derived from the abbreviations
   --   and the Addr_Size and Bits of the key.
   --
   -- Each compilation unit that uses this set refers to it by the Offset
   -- value; we add the Addr_Size and Bits to make up the whole key.


   function Key (Set : Set_Ref) return Set_Key_T;
   --
   -- The Key of a given abbrevation Set.


   function Enty (Code : Code_T; Set : Set_Ref) return Enty_Ref;
   --
   -- A reference to the abbreviation entry with the given Code, within the
   -- given Set of abbreviations, or No_Enty if the Set has no definition
   -- for this Code.


   --
   --    Locating the values of chosen attributes within a DIE
   --


   type View_T is record
      Sect : Sections_T;
      Base : In_Memory.Index_T;
      Enty : Enty_Ref;
      Set  : Set_Ref;
      Refz : In_Memory.Index_T;
   end record;
   --
   -- All the stuff needed to locate attribute values of a debugging
   -- information entry (DIE).
   --
   -- Sect
   --    Stream access to an in-memory copy of the .debug_info section
   --    that contains the DIE and possibly the Strings and Locations
   --    sections (.debug_str and .debug_loc) if present.
   -- Base
   --    The index, for Info, of the first octet of the encoding of the
   --    value of the first attribute in the DIE. This is the octet
   --    that follows the abbreviation code of the DIE.
   -- Enty
   --    The abbreviation entry for this DIE.
   --    No_Enty means that the DIE is a null DIE (consisting only of
   --    an abbreviation code with the value zero).
   -- Set
   --    The abbreviation set that contains the Enty. Some properties
   --    of the Set are needed to interpret the abbreviation.
   -- Refz
   --    Base index for attributes that refer to entries in the same
   --    compilation through stream offsets to this base.


   function Is_Null (View : View_T) return Boolean;
   --
   -- Whether the View shows a null debugging information entry.


   function Code (View : View_T) return Code_T;
   --
   -- The abbreviation code used by this entry.


   function Tag_Code (View : View_T) return Tag_Code_T;
   --
   -- The tag-code of this entry.


   function Tag (View : View_T) return Tag_T;
   --
   -- The tag of this entry.


   function Number_Of_Attributes (View : View_T) return Natural;
   --
   -- The number of attributes in this entry.


   function Attributes (View : View_T) return Att_Form_List_T;
   --
   -- The attributes and encoding forms defined by the abbreviation
   -- template for this entry. Note that some forms may be Indirect,
   -- meaning that the true form is defined in the debugging information
   -- entry itself and not in the abbreviation (see the procedure Digest
   -- below).


   function Children (View : View_T) return Boolean;
   --
   -- Whether this entry can have (own) child entries, as defined in
   -- the abbreviation template. If so, the first child entry is the
   -- one that follows the viewed entry. However, the first child entry
   -- may be a null entry, showing that the entry is actually childless.


   function View (
      Sect : Sections_T;
      Set  : Set_Ref;
      Refz : In_Memory.Index_T)
   return View_T;
   --
   -- Decodes enough of the debugging information entry at the current
   -- Sect.Info stream position to construct a view of this entry using
   -- the proper abbreviation (template) from the given abbreviation Set,
   -- the base Refz of reference attributes, and the (possibly null)
   -- streams Sect.Strings and Sect.Locations.
   --
   -- For a null debugging information entry, the resulting view will
   -- satisfy Is_Null.


   procedure Locate (
      Attribute : in     Attribute_T;
      Thru      : in     View_T;
      Form      :    out True_Form_T);
   --
   -- Determines the true Form and location of the value of the given
   -- Attribute within a debugging information entry (DIE) Thru the
   -- given view of the DIE. The location is returned in the position
   -- of the stream Thru.Info and the true form in the Form parameter,
   -- with indirection resolved.
   --
   -- The Thru.Info stream will point at the first octet of the value
   -- of the desired Attribute. If the Attribute uses an Indirect form,
   -- the stream will point at the first octet after the true Form_Code,
   -- so it will be in the same position as if the abbreviation entry
   -- had specified the true Form instead of Indirect.
   --
   -- No other (visible) components of the view are modified.
   --
   -- If the DIE does not contain a value for this Attribute, the
   -- operation propagates Attribute_Absent.


   --
   --    Locating the values and forms of all the attributes in a DIE
   --


   procedure Digest (
      View   : in     View_T;
      Giving :    out Value_Def_List_T;
      Next   :    out In_Memory.Index_T);
   --
   -- Creates a digest of the contents of the debugging information entry
   -- under View, Giving a list of all the attributes, their true forms
   -- and positions (stream indices) in the entry, and the Next index
   -- after the whole entry.
   --
   -- The results are in Giving(1 .. Number_Of_Attributes(View)).


   function Next_Entry (After : View_T) return In_Memory.Index_T;
   --
   -- The index of the first debugging information entry that comes
   -- After the entry under view. If the entry under view can have
   -- children, this is the index of the first child; otherwise it
   -- is the index of the next sibling of the entry under view.
   -- In either case the entry at this index can be a null entry.
   --
   -- The value returned is the same as from the Next parameter in
   -- the Digest procedure, but without bothering to construct the
   -- value definition list.


   procedure Put (
      View   : in     View_T;
      Indent : in     String;
      CUBA   : in     Poss_Code_Address_T;
      Next   :    out In_Memory.Index_T);
   --
   -- Displays the entry under view on standard output using a
   -- given indentation prefix string and a (possibly undefined)
   -- Compilation Unit Base Address.
   --
   -- Returns in Next the index of the next entry, as in Next_Entry.
   --
   -- TBD display of referenced entries.


   --
   --    Collections of abbreviation sets
   --


   type Sets_T is private;
   --
   -- A collection of abbreviation sets, read from the same
   -- ".debug_abbrev" section, identified and separated by
   -- the Key value of each set.
   --
   -- The default initial value is the empty set.


   procedure Find_Or_Load_Set (
      Key    : in     Set_Key_T;
      Stream : in     In_Memory.Stream_Ref;
      Sets   : in out Sets_T;
      Ref    :    out Set_Ref;
      Novel  :    out Boolean);
   --
   -- Finds the abbreviation set with the given Key value in the
   -- given collection of such Sets, and returns a reference to it.
   --
   -- If the collection does not yet contain such a set, this operation
   -- loads the set from the given Key.Offset within an in-memory copy of
   -- a ".debug_abbrev" section, through the associated Stream, adds the
   -- set to the collection for later use, and returns a reference to the
   -- newly loaded abbreviation set. The output parameter Novel shows
   -- whether Ref was loaded now (True) or earlier (False).
   --
   -- When this operation loads an abbrevation set it also sets up the
   -- fixed layout information (the Length and Layout tables, see the
   -- package body).


   procedure Put (Item : Set_Ref);
   --
   -- Displays the abbreviation set on standard output.


   procedure Put (Item : Sets_T);
   --
   -- Displays the collection of abbreviation sets on standard output.


private


   type Enty_T;
   --
   -- An abbreviation entry.


   type Enty_Ref is access Enty_T;
   --
   -- All abbreviation entries are stored on the heap.


   procedure Read_Enty (
      Stream : access Ada.Streams.Root_Stream_Type'Class;
      Item   : out    Enty_Ref);
   --
   -- Reads an abbreviation entry from the Stream and stores it in
   -- the heap, or returns null in Item if the entry is a null entry
   -- that consists of just a zero abbreviation code.
   --
   -- The Layout component is not represented in the stream and is just
   -- initialized to all zero.
   --
   for Enty_Ref'Read use Read_Enty;


   No_Enty : constant Enty_Ref := null;


   type Set_T;
   --
   -- A set of abbreviations.
   -- Details defined in the package body.


   type Set_Ref is access Set_T;
   --
   -- Refers to a heap-allocated abbreviation set.


   type Set_Ref_List_T is array (Positive range <>) of Set_Ref;
   --
   -- A list or set of (references to) abbreviation sets.
   -- The order or index usually has no logical significance.


   package Unbounded_Set_Vectors
   is new Unbounded_Vectors (
      Element_Type => Set_Ref,
      Vector_Type  => Set_Ref_List_T,
      Deallocate   => Opt.Deallocate);


   type Sets_T is
      new Unbounded_Set_Vectors.Unbounded_Vector;
   --
   -- A collection of abbreviation sets, read from the same
   -- ".debug_abbrev" section, identified and separated by
   -- the Key value of each set.


end Formats.Dwarf.Abbrev;
