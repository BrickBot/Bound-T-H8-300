-- Formats.Dwarf.Types (body)
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
-- $Revision: 1.2 $
-- $Date: 2015/10/24 20:53:52 $
--
-- $Log: formats-dwarf-types.adb,v $
-- Revision 1.2  2015/10/24 20:53:52  niklas
-- Moved to free licence.
--
-- Revision 1.1  2007-06-13 16:46:35  niklas
-- First version, for BT-CH-0060.
--


package body Formats.Dwarf.Types is


   function Signedness (Signed : Boolean) return String
   is
   begin

      if Signed then return "signed";
                else return "unsigned";
      end if;

   end Signedness;


   function Image (Item : Type_T) return String
   is

      Index : constant String :=
         '[' & Type_Index_T'Image (Item.Index) & " ]";

   begin

      case Item.Kind is

      when Integral =>

         return Index
            & ' '
            & Signedness (Item.Signed)
            & Positive'Image (Item.Size_In_Octets)
            & " octets";

      when Non_Integral =>

         return Index & " non-integral";

      end case;

   end Image;


   function Index_Of (Item : Types.Type_T) return Type_Index_T
   is
   begin

      return Item.Index;

   end Index_Of;


   function Type_At (
      Index  : Type_Index_T;
      Within : Indexed_Type_Set_T)
   return Type_T
   is
   begin

      return Search (Key => Index, Within => Within);

   exception

   when Type_Bags.Nonexistent_Key =>

      raise No_Such_Type;

   end Type_At;


end Formats.Dwarf.Types;
