-- Formats.Stabs (decl)
--
-- Symbol tables in the Stabs (or STABS) form.
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
-- $Date: 2015/10/24 20:53:54 $
--
-- $Log: formats-stabs.ads,v $
-- Revision 1.4  2015/10/24 20:53:54  niklas
-- Moved to free licence.
--
-- Revision 1.3  2007-04-18 18:35:49  niklas
-- BT-CH-0057.
--
-- Revision 1.2  2007/03/29 12:51:48  niklas
-- BT-CH-0056.
--
-- Revision 1.1  2004/04/24 17:24:38  niklas
-- First version.
--

--:dbpool with GNAT.Debug_Pools;


package Formats.Stabs is
--
-- A Stabs symbol-table is a hierarchically structured set of "symbol"
-- definitions. A symbol definition correlates an item of the source-
-- code program, such as a subprogram or variable identifier, with the
-- corresponding machine-code entity assigned by the compiler and linker,
-- such as the machine memory address of the subprogram or variable.
--
-- In a program file (binary file, executable file) the symbol-table
-- is usually represented by two "segments" or "sections" of data, the
-- ".stab" segment and the ".stabstr" segment. Each segment is physically
-- an octet string. The contets of this string are interpreted as follows:
--
--   > The ".stab" segment is a sequence of "Stabs lines". Each line
--     holds one symbol definition or one "structuring" definition
--     that divides the sequence in a way that corresponds to the
--     modules or source-code files of the program. Thus, the context
--     or scope of a symbol definition is established by the preceding
--     structuring definitions.
--
--   > The ".stabstr" segment is a sequence of null-terminated strings
--     that are the symbolic (source-code) identifiers of the symbols
--     or that provide source-level data for the structuring definitions,
--     such as source-code file names. Each Stabs-line points (by an
--     index) to the corresponding string (if any) in the ".stabstr".
--     Some of the strings are null (empty) strings (the index points
--     to a null octet in the ".stabstr").
--
-- The ".stabstr" strings are either directory paths, file names, or
-- identifiers. For identifiers, the string is composed of the actual
-- identifier perhaps followed by a colon (:) and type/range information.


   --
   --    Stabs record types
   --
   -- See www.informatik.uni-frankfurt.de/doc/texi/stabs_8.html
   -- and include/aout/stab.def


   type N_Type_T is new Octet_T;
   --
   -- The "type" or kind of a Stabs line.
   -- See list below.

   -- Non-Stabs symbol types:

   N_HdrSym   : constant N_Type_T := 16#00#;
   N_Abs      : constant N_Type_T := 16#02#;
   N_Ext_Abs  : constant N_Type_T := 16#03#;
   N_Text     : constant N_Type_T := 16#04#;
   N_Ext_Text : constant N_Type_T := 16#05#;
   N_Data     : constant N_Type_T := 16#06#;
   N_Ext_Data : constant N_Type_T := 16#07#;
   N_Bss      : constant N_Type_T := 16#08#;
   N_Ext_Bss  : constant N_Type_T := 16#09#;
   N_Fn_Seq   : constant N_Type_T := 16#0c#;
   N_Indr     : constant N_Type_T := 16#0a#;
   N_Comm     : constant N_Type_T := 16#12#;
   N_Seta     : constant N_Type_T := 16#14#;
   N_Sett     : constant N_Type_T := 16#16#;
   N_Setd     : constant N_Type_T := 16#18#;
   N_Setb     : constant N_Type_T := 16#1a#;
   N_Setv     : constant N_Type_T := 16#1c#;
   N_Warning  : constant N_Type_T := 16#1e#;
   N_Fn       : constant N_Type_T := 16#1f#;

   -- Stabs symbol types:

   N_Gsym     : constant N_Type_T := 16#20#;
   N_Fname    : constant N_Type_T := 16#22#;
   N_Fun      : constant N_Type_T := 16#24#;
   N_Stsym    : constant N_Type_T := 16#26#;
   N_Lcsym    : constant N_Type_T := 16#28#;
   N_Main     : constant N_Type_T := 16#2a#;
   N_Rosym    : constant N_Type_T := 16#2c#;
   N_Pc       : constant N_Type_T := 16#30#;
   N_Nsyms    : constant N_Type_T := 16#32#;
   N_Nomap    : constant N_Type_T := 16#34#;
   N_Obj      : constant N_Type_T := 16#38#;
   N_Opt      : constant N_Type_T := 16#3c#;
   N_Rsym     : constant N_Type_T := 16#40#;
   N_M2c      : constant N_Type_T := 16#42#;
   N_Sline    : constant N_Type_T := 16#44#;
   N_Dsline   : constant N_Type_T := 16#46#;
   N_Bsline   : constant N_Type_T := 16#47#;  -- 48? TBC
   N_Brows    : constant N_Type_T := 16#48#;
   N_Defd     : constant N_Type_T := 16#4a#;
   N_Fline    : constant N_Type_T := 16#4c#;
   N_Ehdecl   : constant N_Type_T := 16#50#;
   N_Mod2     : constant N_Type_T := 16#52#;  -- 50? TBC
   N_Catch    : constant N_Type_T := 16#54#;
   N_Ssym     : constant N_Type_T := 16#60#;
   N_Endm     : constant N_Type_T := 16#62#;
   N_So       : constant N_Type_T := 16#64#;
   N_Lsym     : constant N_Type_T := 16#80#;
   N_Bincl    : constant N_Type_T := 16#82#;
   N_Sol      : constant N_Type_T := 16#84#;
   N_Psym     : constant N_Type_T := 16#a0#;
   N_Eincl    : constant N_Type_T := 16#a2#;
   N_Entry    : constant N_Type_T := 16#a4#;
   N_Lbrac    : constant N_Type_T := 16#c0#;
   N_Excl     : constant N_Type_T := 16#c2#;
   N_Scope    : constant N_Type_T := 16#c4#;
   N_Rbrac    : constant N_Type_T := 16#e0#;
   N_Bcomm    : constant N_Type_T := 16#e2#;
   N_EComm    : constant N_Type_T := 16#e4#;
   N_Ecoml    : constant N_Type_T := 16#e8#;
   N_With     : constant N_Type_T := 16#ea#;
   N_Nbtext   : constant N_Type_T := 16#f0#;
   N_Nbdata   : constant N_Type_T := 16#f2#;
   N_Nbbss    : constant N_Type_T := 16#f4#;
   N_Nbsts    : constant N_Type_T := 16#f6#;
   N_Nblcs    : constant N_Type_T := 16#f8#;


   --
   --    Stabs lines (records)
   --


   type N_Strx_T is new Unsigned_32_T;
   --
   -- Index or offset from a Stabs line to a string in ".stabstr".


   type N_Desc_T is new Unsigned_16_T;
   --
   -- Description of a symbol.
   -- Interpretation TBD.


   type N_Value_T is new Signed_32_T;
   --
   -- The machine value (e.g. address) of a symbol.


   type Line_T is record
      N_Strx    : N_Strx_T;
      N_Type    : N_Type_T;
      N_Other   : Octet_T;
      N_Desc    : N_Desc_T;
      N_Value   : N_Value_T;
   end record;
   --
   -- A "Stabs line", an element of the ".stab" segment.
   --
   -- N_Strx
   --    The offset, to the string name for this symbol, in the
   --    ".stabstr" string, relative to the start of the module
   --    (= the NUL character corresponding to the preceding Hdr_Sym
   --    entry).
   --    After the symbol table is loaded from the file, this is
   --    the absolute offset to string name in .stabstr, relative
   --    to the start of the .stabstr. (In other words, the loading
   --    process converts the module-relative offsetst to absolute
   --    offsets).
   -- N_Type
   --    Type of symbol (see list below).
   -- N_Other
   --    Misc info (usually empty).
   -- N_Desc
   --    Description field.
   -- N_Value
   --    Value of the symbol.
   --
   -- Some type-specific meanings:
   --
   -- N_HdrSym type
   --    N_Strx in the file is always 1 (offset to first symbol TBC).
   --    N_Desc is the number of symbols in the module (including Hdr_Sym).
   --    N_Value is the total length of the strings for this module in
   --    the .stabstr. The base offset for N_Strx is the sum of the
   --    N_Value's of all but the last preceding Hdr_Syms.


   Octets_Per_Line : constant := 12;
   --
   -- The size of one Line_T, as it appears in a segment, in octets.
   --
   --   N_Strx   N_Strx_T     4
   --   N_Type   N_Type_T     1
   --   N_Other  Octet_T      1
   --   N_Desc   N_Desc_T     2
   --   N_Value  N_Value_T    4
   ----------------------------
   --   Total                12



   type Lines_T is array (Integer range <>) of Line_T;
   --
   -- The Stabs lines (content of ".stab" segment).


   type Table_T (
      Max_Line  : Integer;
      Num_Chars : Natural)
   is record
      Lines   : Lines_T (-1 .. Max_Line);
      Strings : String (1 .. Num_Chars);
   end record;
   --
   -- A Stabs symbol-table, consisting of a number of Stabs lines
   -- and the associated string list (concatenated, null-separated).
   -- Using -1 as the first line-index is traditional for some
   -- reason unknown to the writer.


   type Table_Ref is access Table_T;
   --
   -- A Stabs symbol table, dynamically allocated.

   --:dbpool Table_Pool : GNAT.Debug_Pools.Debug_Pool;

   --:dbpool for Table_Ref'Storage_Pool use Table_Pool;


   function Symbol (
      Line   : Line_T;
      Within : Table_T;
      Cut    : Character := Character'Val (0))
   return String;
   --
   -- The symbolic string associated with the given Stabs line
   -- (symbol), in the given Stabs table.
   --
   -- The optional Cut parameter specifies a character that terminates
   -- the string, so the full identifier string is returned only up to
   -- but excluding the first occurrence of this character. For example,
   -- in variable symbols a colon is used to separate the variable
   -- identifier from the type/range information.


   function Identifier (Symbol : String) return String;
   --
   -- The "identifier" part of a symbolic string from Stabs string
   -- table. This is the part before the first colon (:).


   procedure Load (
      From    : in     IO.File_Type;
      Lines   : in     Segment_Desc_T;
      Strings : in     Segment_Desc_T;
      Trace   : in     Boolean := False;
      Giving  :    out Table_Ref);
   --
   -- Loads a Stabs symbol-table from a program binary file.
   --
   -- From
   --    The file from which to load.
   -- Lines
   --    The location and length of the ".stab" segment.
   -- Strings
   --    The location and length of the ".stabstr" segment.
   -- Trace
   --    Whether to trace (display) the Stabs lines one by one as
   --    they are read from the file.
   -- Giving
   --    The loaded Stabs table.


end Formats.Stabs;
