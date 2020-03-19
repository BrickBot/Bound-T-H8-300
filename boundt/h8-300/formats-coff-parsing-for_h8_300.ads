-- Formats.COFF.Parsing.For_H8_300 (decl)
--
-- Reading H8/300 symbols from COFF symbol-tables.
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
-- $Revision: 1.2 $
-- $Date: 2015/10/26 22:19:14 $
--
-- $Log: formats-coff-parsing-for_h8_300.ads,v $
-- Revision 1.2  2015/10/26 22:19:14  niklas
-- Version 4b, and moved to free open-source licence.
--
-- Revision 1.1  2004/05/31 20:25:05  niklas
-- First, incomplete version.
--


with Processor;


package Formats.COFF.Parsing.For_H8_300 is


   type Action_T is new Parsing.Action_T
      with null record;
   --
   -- Defines how to parse a COFF symbol table from GNU H8/300.
   --
   -- The following primitive operations are inherited with their
   -- default implementation from Formats.COFF.Parsing:
   --
   --   > Object_Kind
   --   > Source_File
   --   > Function_Symbol
   --   > End_Function
   --   > Begin_Block
   --   > End_Block
   --   > Data_Symbol
   --   > Other_Symbol
   --   > Special_Symbol


   function Data_Cell (
      Object : Symbol_Denotation_T;
      Action : Action_T)
   return Processor.Cell_Spec_T;
   --
   -- The (processor-specific) cell specified by a Symbol_Entry or
   -- Symbol_Entry_2 symbol record that represents a data cell.
   --
   -- Precondition: Object_Kind (Object) = Data.
   --
   -- Raises No_Such_Cell if the Object does not correspond to a
   -- (tracked) cell for the present target processor.


end Formats.COFF.Parsing.For_H8_300;
