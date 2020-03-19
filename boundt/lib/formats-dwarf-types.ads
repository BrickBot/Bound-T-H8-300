-- Formats.Dwarf.Types (decl)
--
-- Type structures in DWARF form.
--
-- The DWARF debugging information entry (DIE) for a data object (a
-- variable, a parameter, or a function return value) specifies the
-- data type by referring to a DIE that represents the type. There
-- are several forms of DIEs that represent types, starting from the
-- base (unstructured, unmodified) types and going on to create new
-- or modified types such as records/structures, arrays, pointers
-- and references.
--
-- This package provides an Ada model of the DWARF type structures
-- but limited to the aspects that are important to Bound-T.
--
-- Author: Niklas Holsti.
--
-- A component of the Bound-T Worst-Case Execution Time Tool.
--
-------------------------------------------------------------------------------
-- Copyright (c) 1999 .. 2015 Tidorum Ltd, except for text copied verbatim
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
-- $Revision: 1.2 $
-- $Date: 2015/10/24 20:53:52 $
--
-- $Log: formats-dwarf-types.ads,v $
-- Revision 1.2  2015/10/24 20:53:52  niklas
-- Moved to free licence.
--
-- Revision 1.1  2007-06-13 16:46:35  niklas
-- First version, for BT-CH-0060.
--


with Bags;                               -- MW_Components
with Formats.In_Memory;


package Formats.Dwarf.Types is


   type Kind_T is (
      Integral,
      Non_Integral);
   --
   -- The kind of a type, as far as Bound-T needs to know.
   --
   -- Integral
   --    The type's values are integers, or are encoded as integers.
   -- Non_Integral
   --    Other kinds of types.


   subtype Type_Index_T is In_Memory.Index_T;
   --
   -- The types (type definitions) are identified by their positions
   -- in the DWARF "info", for compatibility with the Type_Ref attribute.


   use type In_Memory.Index_T;


   type Type_T (Kind : Kind_T := Non_Integral) is record
      Index : Type_Index_T;
      case Kind is
      when Integral =>
         Signed         : Boolean;
         Size_In_Octets : Positive;
      when Non_Integral =>
         null;
      end case;
   end record;
   --
   -- The properties of a type.
   --
   -- Index
   --    The index of the type entry in some in-memory copy
   --    of the DWARF "info" section. Uniquely identifies this
   --    type (description). There may be other type entries that
   --    describe fully equivalent types, or even the same type
   --    (as decided by the name of the type).
   -- Signed
   --    Whether the type represents signed integers.
   --    Otherwise it represents unsigned integers.
   -- Size_In_Octets
   --    The size of the integer value, in octets (8 bits).
   --    The type may or may not have a more restricted value range.


   function Image (Item : Type_T) return String;
   --
   -- A readable description of the type.


   function Index_Of (Item : Types.Type_T) return Type_Index_T;
   --
   -- The "Key_Of" function for Type_Bags, see below.


   package Type_Bags is new Bags (
      Key_Type  => Type_Index_T,
      Item_Type => Types.Type_T,
      Key_Of    => Index_Of,
      Count     => Natural);


   type Indexed_Type_Set_T is new Type_Bags.Bag (
      Duplicate_Keys_Allowed => False);
   --
   -- A set of Types indexed by their Index value.


   No_Such_Type : exception;
   --
   -- When looking for a type with a given index and failing.


   function Type_At (
      Index  : Type_Index_T;
      Within : Indexed_Type_Set_T)
   return Type_T;
   --
   -- The type with the given Index, Within the given set.
   -- Propagates No_Such_Type if the set does not contain
   -- a type with this Index.


end Formats.Dwarf.Types;
