-- Symbols.Opt (decl)
--
-- Command-line options that control the handling of symbol tables
-- that connect the target-program source code and the object code.
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
-- $Revision: 1.9 $
-- $Date: 2015/10/24 19:36:53 $
--
-- $Log: symbols-opt.ads,v $
-- Revision 1.9  2015/10/24 19:36:53  niklas
-- Moved to free licence.
--
-- Revision 1.8  2011-09-01 21:30:42  niklas
-- Using File_Sets for -symbols, instread of String_Sets.
--
-- Revision 1.7  2011-08-31 04:23:34  niklas
-- BT-CH-0222: Option registry. Option -dump. External help files.
--
-- Revision 1.6  2008/10/11 08:16:14  niklas
-- BT-CH-0148: Symbols from text files and the -symbols option.
--
-- Revision 1.5  2007/08/13 09:01:34  niklas
-- BT-CH-0070: Tracing symbol scopes. Warn re duplicated symbol.
--
-- Revision 1.4  2007/04/26 11:28:05  niklas
-- BT-CH-0059.
--
-- Revision 1.3  2007/02/24 09:51:52  niklas
-- BT-CH-0046.
--
-- Revision 1.2  2003/02/27 14:38:09  holsti
-- Added option Warn_Duplicated_Symbol.
--
-- Revision 1.1  2000/05/05 12:09:48  holsti
-- Option -trace and Symbols.Opt added.
--


with Options.Bool;
with Options.File_Sets;
with Options.String_Sets;


package Symbols.Opt is

   pragma Elaborate_Body;


   Symbol_File_Names_Opt : aliased Options.File_Sets.Option_T;
   --
   -- The names of the symbol files, if given.
   --
   Symbol_File_Names : Options.String_Sets.String_Set_T
      renames Symbol_File_Names_Opt.Value;


   Trace_Opt : aliased Options.Bool.Option_T (Default => False);
   --
   -- Whether to trace on standard output certain symbol table
   -- actions. The traced actions are defined in Symbols, but
   -- include e.g. declaration (insertion) of symbols.
   --
   Trace : Boolean renames Trace_Opt.Value;


   Trace_Scopes_Opt : aliased Options.Bool.Option_T (Default => False);
   --
   -- Whether to trace on standard output the creation of permanent
   -- symbol scope objects (Scope_T). The transient creation of dynamic
   -- scopes (Scope_Stack_T) is not traced.
   --
   Trace_Scopes : Boolean renames Trace_Scopes_Opt.Value;


   Trace_Insertion_Opt : aliased Options.Bool.Option_T (Default => False);
   --
   -- Whether to trace on standard output the detailed steps for
   -- insert a symbol in the symbol table (the resolution of the
   -- optional prefixes).
   --
   Trace_Insertion : Boolean renames Trace_Insertion_Opt.Value;


   Warn_Duplicated_Symbol_Opt : aliased Options.Bool.Option_T (Default => True);
   --
   -- Whether to emit a warning message when a multiple declarations
   -- of the same symbol are found in the target program.
   --
   Warn_Duplicated_Symbol : Boolean renames Warn_Duplicated_Symbol_Opt.Value;


   Warn_Shallow_Line_Scope_Opt : aliased Options.Bool.Option_T (
      Default => False);
   --
   -- Whether to emit a warning message when a Line Number connection
   -- is defined with only one scope level.
   --
   Warn_Shallow_Line_Scope : Boolean renames Warn_Shallow_Line_Scope_Opt.Value;


   Deallocate_In_Stacks : Boolean renames Symbols.Deallocate_In_Stacks;
   --
   -- Whether to use Unchecked_Deallocation to release unused
   -- heap memory in Scope Stacks.


end Symbols.Opt;
