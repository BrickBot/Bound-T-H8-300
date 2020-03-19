-- Formats.Dwarf.Opt (decl)
--
-- Options for accessing DWARF information.
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
-- $Revision: 1.6 $
-- $Date: 2015/10/24 20:53:52 $
--
-- $Log: formats-dwarf-opt.ads,v $
-- Revision 1.6  2015/10/24 20:53:52  niklas
-- Moved to free licence.
--
-- Revision 1.5  2011-09-01 13:03:30  niklas
-- Updated for BT-CH-0222: Option registry.
--
-- Revision 1.4  2008-10-15 12:05:53  niklas
-- BT-CH-0150: DWARF location lists, lexical block scopes, CUBAs.
--
-- Revision 1.3  2007/01/25 21:25:36  niklas
-- BT-CH-0043.
--
-- Revision 1.2  2006/04/12 19:35:08  niklas
-- Added Block_Alignment option.
--
-- Revision 1.1  2004/04/24 18:06:21  niklas
-- First version.
--


with Options;
with Options.Bool;
with Options.Pos;


package Formats.Dwarf.Opt is


   pragma Elaborate_Body;
   --
   -- To register the options and set the priority for the
   -- option groups, here defined.


   Group : constant Options.Group_Name_T := Options.Group ("dwarf");
   --
   -- All the DWARF options.


   package IO_Pos_Count_Valued is new Options.Integer_Valued (
      Value_Type => IO.Positive_Count);
   --
   Block_Alignment_Opt : aliased IO_Pos_Count_Valued.Option_T (Default => 1);
   --
   -- The "alignment" for DWARF information blocks, in octets.
   -- The offset from a block to the next block is the length of
   -- the "initial length" field of the first block, plus the
   -- length of the first block rounded up to a multiple of
   -- Block_Alignment.
   --
   -- The need to round the length up to a multiple of Block_Alignment
   -- has been discovered empirically in DWARF V2 files from the
   -- ARM and IAR compilers for the ARM7; I can find no mention
   -- of this in the DWARF standard.
   --
   Block_Alignment : IO.Positive_Count renames Block_Alignment_Opt.Value;


   Location_List_Max_Length_Opt : aliased Options.Pos.Option_T (
      Default => 300);
   -- 
   -- Maximum number of location entries in a location list.
   -- If there are more entries they are ignored (except in dump).
   --
   Location_List_Max_Length : Positive
      renames Location_List_Max_Length_Opt.Value;


   Trace_Loading_Opt : aliased Options.Bool.Option_T (Default => False);
   --
   -- Whether to trace on standard output the process of accessing
   -- and interpreting DWARF information.
   --
   Trace_Loading : Boolean renames Trace_Loading_Opt.Value;


   Trace_Traversal_Opt : aliased Options.Bool.Option_T (Default => False);
   --
   -- Whether to trace on standard output the process of traversing
   -- the DWARF information tree.
   --
   Trace_Traversal : Boolean renames Trace_Traversal_Opt.Value;


   Trace_Var_Par_Opt : aliased Options.Bool.Option_T (Default => False);
   --
   -- Whether to trace on standard output the process of translating
   -- symbols for variables and formal parameters.
   --
   Trace_Var_Par : Boolean renames Trace_Var_Par_Opt.Value;


   Trace_Line_Number_Program_Opt : aliased Options.Bool.Option_T (
      Default => False);
   --
   -- Whether to trace on standard output the operations executed in
   -- a line-number program.
   --
   Trace_Line_Number_Program : Boolean
      renames Trace_Line_Number_Program_Opt.Value;


   Deallocate : Boolean := True;
   --
   -- Whether to use Unchecked_Deallocation to release unused
   -- heap memory.


end Formats.Dwarf.Opt;
