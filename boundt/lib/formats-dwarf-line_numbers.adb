-- Formats.Dwarf.Line_Numbers (body)
--
-- Notes on peculiar features seen in actual DWARF information:
--
-- 1. The length of the line-number info for a compilation unit must
--    be rounded up to a multiple of 4 octets to find the start of
--    the info for the next compilation unit.
--    Producers: ARM compiler for ARM7; IAR compiler for ARM7.
--
-- 2. The file-length component in a compilation-unit header seems to
--    show the number of lines in the file, rather than the number of
--    octets (bytes) as the standard says.
--    Producers: IAR compiler for ARM7.
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
-- $Revision: 1.4 $
-- $Date: 2015/10/24 20:53:52 $
--
-- $Log: formats-dwarf-line_numbers.adb,v $
-- Revision 1.4  2015/10/24 20:53:52  niklas
-- Moved to free licence.
--
-- Revision 1.3  2013-02-03 10:46:54  niklas
-- BT-CH-0233: Dwarf 4 "Set Discriminant (4)" line-number operation.
--
-- Revision 1.2  2006-04-12 19:34:26  niklas
-- Corrected procedure Scan_Table to do nothing if the
-- line-number program is empty.
--
-- Revision 1.1  2004/04/24 18:06:20  niklas
-- First version.
--


with Ada.Text_IO;
with Formats.Dwarf.Opt;
with Formats.In_Memory.Text;
with Hex;
with Output;


package body Formats.Dwarf.Line_Numbers is


   subtype Supported_Version_T is Version_T range 2 .. 3;
   --
   -- The Unit_Header.Versions supported by this implementation.


   --
   --    Stream reading:
   --


   procedure Read_String (
      Stream : access Ada.Streams.Root_Stream_Type'Class;
      Item   : out    String_Ref)
   is

      Str : constant String := String_To_Null (From => Stream);
      -- The string value.

   begin

      -- Return string ref or null:

      if Str'Length > 0 then

         Item := new String'(Str);

      else

         Item := null;

      end if;

   end Read_String;


   procedure Read_String_Vector (
      Stream : access Ada.Streams.Root_Stream_Type'Class;
      Item   : out    String_Vector_T)
   is

      Ref : String_Ref;
      -- One of the strings.

   begin

      Erase (Item);

      loop

         String_Ref'Read (Stream, Ref);

         exit when Ref = null;

         Append (To => Item, Value => Ref);

      end loop;

   end Read_String_Vector;


   procedure Read_File_Info (
      Stream : access Ada.Streams.Root_Stream_Type'Class;
      Item   : out File_Info_T)
   is
   begin

      String_Ref'Read (Stream, Item.Name);

      if Item.Name = null then
         -- A leading null octet means there are no more files.

         Item.Dir_Index   := 0;
         Item.Timestamp   := 0;
         Item.File_Length := 0;

      else

         Read_LEB128_Natural (Stream, Item.Dir_Index);
         Timestamp_T'Read    (Stream, Item.Timestamp);
         File_Length_T'Read  (Stream, Item.File_Length);

      end if;

   end Read_File_Info;


   procedure Read_File_Vector (
      Stream : access Ada.Streams.Root_Stream_Type'Class;
      Item   : out File_Vector_Ref)
   is

      File : File_Info_T;
      -- Info for one file.

   begin

      Item := new File_Vector_T;

      Erase (Item.all);

      loop

         File_Info_T'Read (Stream, File);

         exit when File.Name = null;
         -- A leading null octet means there are no more files.

         Append (To => Item.all, Value => File);

      end loop;

   end Read_File_Vector;


   procedure Read_Unit_Header (
      Stream : access Ada.Streams.Root_Stream_Type'Class;
      Item   : out    Unit_Header_T)
   is
   begin

      Initial_Length_T'Read (Stream, Item.Unit_Length);
      Version_T'Read        (Stream, Item.Version);

      if Item.Version not in Supported_Version_T then

         Output.Warning (
              "DWARF line-number unit header version ="
            & Version_T'Image (Item.Version)
            & ", supported versions are"
            & Version_T'Image (Supported_Version_T'First)
            & " .."
            & Version_T'Image (Supported_Version_T'Last));

      end if;

      Read_Variant_Unsigned (
         Stream => Stream,
         Bits   => Item.Unit_Length.Bits,
         Item   => Item.Header_Length);

      Unsigned_8_T'Read (Stream, Item.Minimum_Instruction_Length);
      Boolean_T'Read    (Stream, Item.Default_Is_Stmt);
      Signed_8_T'Read   (Stream, Item.Line_Base);
      Unsigned_8_T'Read (Stream, Item.Line_Range);
      Opcode_T'Read     (Stream, Item.Opcode_Base);

      for S in 1 .. Item.Opcode_Base - 1 loop
         Unsigned_8_T'Read (Stream, Item.Standard_Opcode_Lengths(S));
      end loop;

      String_Vector_T'Read (Stream, Item.Include_Directories);
      File_Vector_Ref'Read (Stream, Item.Files);

      Item.Num_Files := Length (Item.Files.all);

   end Read_Unit_Header;


   --
   --    Line-number program opcodes
   --


   procedure Split_Special (
      Opcode         : in     Opcode_T;
      Unit           : in     Unit_Header_T;
      Code_Increment :    out Code_Offset_T;
      Line_Increment :    out Integer)
   --
   -- Splits a special Opcode into the code-address increment and
   -- line-number increment that are encoded in the Opcode using the
   -- encoding parameters for this compilation Unit.
   --
   is

      Adjusted_Opcode : constant Unsigned_8_T := Opcode - Unit.Opcode_Base;

   begin

      if Opcode < Unit.Opcode_Base then

         Output.Error (
              "Special opcode"
            & Opcode_T'Image (Opcode)
            & " is less than Opcode_Base ="
            & Opcode_T'Image (Unit.Opcode_Base));

         Code_Increment := 0;
         Line_Increment := 0;

      else

         Code_Increment :=
              Code_Offset_T (Adjusted_Opcode / Unit.Line_Range)
            * Code_Offset_T (Unit.Minimum_Instruction_Length);

         Line_Increment :=
              Integer (Unit.Line_Base)
            + Integer (Adjusted_Opcode mod Unit.Line_Range);

      end if;

   end Split_Special;


   --
   --    Compilation_T access operations:
   --


   function Section_Offset (Item : Compilation_T)
   return Section_Offset_T
   is
   begin

      return Item.Offset;

   end Section_Offset;


   function Source_File_Name (
      File   : Positive;
      Within : Compilation_T)
   return String
   is
   begin

      return Element (Within.Header.Files.all, File).Name.all;

   exception

      when Constraint_Error => raise Format_Error;

   end Source_File_Name;


   function Initial_State (Within : Unit_Header_T)
   return Row_T
   --
   -- The initial state of the Row registers for instruction sequences
   -- Within a given compilation unit.
   --
   is
   begin

      return (
         Address        => 0,
         File           => 1,
         Line           => 1,
         Column         => 0,
         Is_Stmt        => Boolean (Within.Default_Is_Stmt),
         Basic_Block    => False,
         End_Sequence   => False,
         Prologue_End   => False,
         Epilogue_Begin => False,
         ISA            => 0);   -- TBA "architecturally defined default".

   end Initial_State;


   type Operation_T is (
      Extended,
      Copy,
      Advance_PC,
      Advance_Line,
      Set_File,
      Set_Column,
      Negate_Stmt,
      Set_Basic_Block,
      Const_Add_PC,
      Fixed_Advance_PC,
      Set_Prologue_End,
      Set_Epilogue_Begin,
      Set_ISA,
      Vendor_Extension,
      Special);
   --
   -- The line-number program operations, listed in order
   -- of Opcode or Opcode-range.
   --
   -- Extended
   --    A zero opcode introduces an extende opcode => Extended.
   -- Advance_PC .. Set_ISA
   --    The standard opcodes.
   -- Vendor_Extension
   --    Opcodes 13 .. Opcode_Base - 1 are vendor extensions to the
   --    standard opcodes.
   -- Special
   --    Opcodes Opcode_Base .. 255 are special opcodes with embedded
   --    operands (increments to address and line-number).


   subtype Standard_Operation_T is Operation_T range Advance_PC .. Set_ISA;
   --
   -- The "standard" operations.


   function Operation (
      Opcode : Opcode_T;
      Base   : Opcode_T)
   return Operation_T
   --
   -- Classifies the Opcode as an operation or operation class.
   --
   is
   begin

      if Opcode >= Base then

         return Special;

      elsif Opcode = 0 then

         return Extended;

      elsif Opcode <= Operation_T'Pos (Standard_Operation_T'Last) then

         return Operation_T'Val (Opcode);

      else

         return Vendor_Extension;

      end if;

   end Operation;


   type Extended_Operation_T is (
      End_Sequence,
      Set_Address,
      Define_File,
      Set_Discriminant,
      Unknown_Extended_Op);
   --
   -- The extended operations listed in order of extended opcode.
   --
   -- Set_Discriminant is a Dwarf 4 addition.


   function Extended_Operation (Extcode : Unsigned_8_T)
   return Extended_Operation_T
   --
   -- The extended operation identified by the Extcode.
   --
   is
   begin

      case Extcode is
      when 1      => return End_Sequence;
      when 2      => return Set_Address;
      when 3      => return Define_File;
      when 4      => return Set_Discriminant;
      when others => return Unknown_Extended_Op;
      end case;

   end Extended_Operation;


   procedure Put_Trace (Text : in String)
   --
   -- Emits a trace of the line-number program's execution.
   --
   is
   begin

      Ada.Text_IO.Set_Col (15);
      Ada.Text_IO.Put_Line (Text);

   end Put_Trace;


   Ends_Without_End_Sequence : exception;
   --
   -- Raised when the line-number program ends (or runs off the end
   -- of the operation sequence) without a final End_Sequence operation.
   -- This is considered a non-fatal error in the DWARF info.


   procedure Run_To_Next_Row (
      Unit      : in     Unit_Header_T;
      Opcodes   : in     In_Memory.Stream_Ref;
      Op_End    : in     In_Memory.Index_T;
      Trace     : in     Boolean;
      Num_Files : in out Natural;
      State     : in out Row_T;
      Row       :    out Row_T)
   --
   -- Runs (interprets, executes) line-number-program Opcodes from
   -- a given stream, modifying the row State, until the program
   -- creates the next Row in the line-number table. The created
   -- Row is returned. Note that the State may be updated after the
   -- Row is created, so Row and State may not be equal on return.
   --
   -- Unit
   --    The compilation-unit header defines several parameters that
   --    are necessary for interpreting the line-number program.
   -- Opcodes
   --    The stream of line-number opcodes.
   -- Op_End
   --    The index in Opcodes of the first octet after the end of this
   --    line-number program. We will of course not run past this point.
   --    If we reach this point and the last operation before this point
   --    does not create a row (nominally the operation should be
   --    End_Sequence), we stop and propagate Ends_Without_End_Sequence.
   -- Trace
   --    Whether to trace instructions on standard output as they are
   --    executed.
   -- Num_Files
   --    The number of source-code files so far defined. When the
   --    line-number program is started, this should be set to
   --    Unit.Num_Files, the number of files defined in the header.
   --    Each DW_LNE_define_file instruction increments the number;
   --    if the number is then larger than Length(Unit.Files), the
   --    new defined file is appended to Unit.Files.
   -- State
   --    The row-register state; updated by opcodes.
   -- Row
   --    The created row in the line-number table.
   --
   is
      use type In_Memory.Index_T;

      Opcode : Opcode_T;
      -- An operation code in the LNP (line-number program).

      Op : Operation_T;
      -- The operation (class) identified by Opcode.

      MIL : constant Code_Offset_T :=
         Code_Offset_T (Unit.Minimum_Instruction_Length);
      -- The minimum instruction length which is often a multiplier
      -- in the increment to be added to State.Address.

      Code_Incr : Code_Offset_T;
      -- The increment to State.Address from a special opcode.

      Line_Incr : Integer;
      -- The increment to State.Line from a special opcode.

      Ext_Len : Natural;
      -- The length of an extended instruction.

      Row_Created : Boolean := False;
      -- Whether a Row was created by the last LNP instruction.


      function Unsigned_Operand
      return LEB128.Longest_Unsigned_T
      --
      -- An unsigned operand from the Opcode stream.
      --
      is
         Operand : LEB128.Longest_Unsigned_T;
      begin

         Operand := LEB128.Longest_Unsigned_T'Input (Opcodes);

         if Trace then

            Put_Trace (
                "   unsigned operand ="
              & LEB128.Longest_Unsigned_T'Image (Operand));

         end if;

         return Operand;

      end Unsigned_Operand;


      function Signed_Operand
      return LEB128.Longest_Signed_T
      --
      -- A signed operand from the Opcode stream.
      --
      is
         Operand : LEB128.Longest_Signed_T;
      begin

         Operand := LEB128.Longest_Signed_T'Input (Opcodes);

         if Trace then

            Put_Trace (
                "   signed operand ="
              & LEB128.Longest_Signed_T'Image (Operand));

         end if;

         return Operand;

      end Signed_Operand;


      procedure Create_Row
      --
      -- Creates a new row with the current State and then resets
      -- the basic-block, prologue and epilogue flags.
      --
      is
      begin

         Row := State;

         Row_Created := True;

         State.Basic_Block    := False;
         State.Prologue_End   := False;
         State.Epilogue_Begin := False;

      end Create_Row;


      procedure Run_Extended_Opcode (Instr_Length : in Positive)
      --
      -- Executes an extended Opcode (Opcode is the null octet
      -- that prefixes the extended instruction).
      -- The Instr_Length parameter is the length of the instruction
      -- in octets, not including this length field itself nor the
      -- null-octet prefix.
      --
      is

         Extcode : Unsigned_8_T;
         -- The extended opcode.

         Rest : constant Natural := Instr_Length -1;
         -- The total length of the operands after the extended opcode.

         Extop : Extended_Operation_T;
         -- The extended operation identified by Extcode.

         File_Info : File_Info_T;
         -- Info re a new source-code file.

      begin

         Unsigned_8_T'Read (Opcodes, Extcode);

         Extop := Extended_Operation (Extcode);

         if Trace then

            Put_Trace (
                 "   Extended "
               & Extended_Operation_T'Image (Extop)
               & ", opcode ="
               & Unsigned_8_T'Image (Extcode)
               & ", length ="
               & Positive'Image (Instr_Length));

         end if;

         case Extop is

         when End_Sequence =>

            State.End_Sequence := True;

            Create_Row;

            State := Initial_State (Unit);

         when Set_Address =>

            case Rest is
            -- This is the length of the address operand.

            when 2 =>

               Unsigned_16_T'Read (
                  Opcodes,
                  Unsigned_16_T (State.Address));

            when 4 =>

               Unsigned_32_T'Read (
                  Opcodes,
                  Unsigned_32_T (State.Address));

            when others =>

               Output.Warning (
                    "DWARF DW_LNE_set_address address size"
                  & Natural'Image (Rest)
                  & "octets: not implemented.");

               raise Format_Error;

            end case;

         when Define_File =>

            Num_Files := Num_Files + 1;

            if Num_Files > Length (Unit.Files.all) then
               -- New file should be added to Unit.Files.

               File_Info_T'Read (Opcodes, File_Info);

               Append (To => Unit.Files.all, Value => File_Info);

           else
              -- Skip new file definition; already added to Unit.Files
              -- in an earlier execution.

              In_Memory.Skip (
                 Stream => Opcodes.all,
                 Count  => In_Memory.Count_T (Rest));

            end if;

         when Set_Discriminant =>
            -- We don't use discriminant numbers.

            In_Memory.Skip (
               Stream => Opcodes.all,
               Count  => In_Memory.Count_T (Rest));

         when Unknown_Extended_Op =>

            Output.Note (
                 "DWARF extended line-number opcode"
               & Unsigned_8_T'Image (Extcode)
               & " unknown; skipped"
               & Natural'Image (Rest)
               & " octets of operands");

            In_Memory.Skip (
               Stream => Opcodes.all,
               Count  => In_Memory.Count_T (Rest));

         end case;

      end Run_Extended_Opcode;


      procedure Skip_Vendor_Extension
      --
      -- Skips an instruction that starts with a non-special Opcode
      -- (Opcode in 1 .. Opcode_Base) but which is non-standard and is
      -- therefore some type of vendor extension that we do not know
      -- more about than the number of operands.
      --
      is

         Dummy : LEB128.Longest_Unsigned_T;
         -- One of the operands. We do nothing with the value.
         -- In fact, we don't known if it should be understood as
         -- signed or unsigned.

      begin

         Output.Warning (
              "DWARF line-number opcode"
            & Opcode_T'Image (Opcode)
            & " is an unknown vendor extension with"
            & Unsigned_8_T'Image (Unit.Standard_Opcode_Lengths(Opcode))
            & " operands; skipped.");

         for K in 1 .. Unit.Standard_Opcode_Lengths(Opcode) loop

            Dummy := Unsigned_Operand;

         end loop;

      end Skip_Vendor_Extension;


   begin  -- Run_To_Next_Row

      loop
         -- Execute LNP instructions one by one, updating State,
         -- until Row_Created.

         -- Check for end of program:

         if In_Memory.Index (Opcodes.all) >= Op_End then
            -- Oops, running out of the LNP.

            raise Ends_Without_End_Sequence;

         end if;

         -- Fetch the opcode and classify it:

         Opcode_T'Read (Opcodes, Opcode);

         Op := Operation (Opcode => Opcode, Base => Unit.Opcode_Base);

         if Trace then

            Put_Trace (
                 Operation_T'Image (Op)
               & ", opcode ="
               & Unsigned_8_T'Image (Opcode));

         end if;

         -- Execute the operation:

         case Op is

         when Special =>
            -- A "special" opcode.

            Split_Special (
               Opcode         => Opcode,
               Unit           => Unit,
               Code_Increment => Code_Incr,
               Line_Increment => Line_Incr);

            if Trace then

               Put_Trace (
                    "   address +"
                  & Code_Offset_T'Image (Code_Incr)
                  & ", line +"
                  & Integer'Image (Line_Incr));

            end if;

            State.Address := State.Address + Code_Incr;
            State.Line    := State.Line    + Line_Incr;

            Create_Row;

         when Extended =>
            -- A null octet begins an "extended" opcode.

            Ext_Len := Natural (Unsigned_Operand);

            if Ext_Len > 0 then

               Run_Extended_Opcode (Ext_Len);

            elsif Trace then

               Put_Trace ("   Extended instruction of zero length.");

            end if;

         when Copy =>

            Create_Row;

         when Advance_PC =>

            State.Address :=
                 State.Address
               + MIL * Code_Offset_T (Unsigned_Operand);

         when Advance_Line =>

            State.Line := State.Line + Integer (Signed_Operand);

         when Set_File =>

            State.File := Positive (Unsigned_Operand);

         when Set_Column =>

            State.Column := Natural (Unsigned_Operand);

         when Negate_Stmt =>

            State.Is_Stmt := not State.Is_Stmt;

         when Set_Basic_Block =>

            State.Basic_Block := True;

         when Const_Add_PC =>

            Split_Special (
               Opcode         => 255,
               Unit           => Unit,
               Code_Increment => Code_Incr,
               Line_Increment => Line_Incr);

            if Trace then

               Put_Trace (
                    "   address +"
                  & Code_Offset_T'Image (Code_Incr));

            end if;

            State.Address := State.Address + Code_Incr;

         when Fixed_Advance_PC =>

            Unsigned_16_T'Read (Opcodes, Unsigned_16_T (Code_Incr));

            State.Address := State.Address + Code_Incr;

         when Set_Prologue_End =>

            State.Prologue_End := True;

         when Set_Epilogue_Begin =>

            State.Epilogue_Begin := True;

         when Set_ISA =>

            State.ISA := Natural (Unsigned_Operand);

         when Vendor_Extension =>
            -- Vendor-specific extension to standard opcodes.
            -- 13 .. Opcode_Base - 1.

            Skip_Vendor_Extension;

         end case;

         exit when Row_Created;

      end loop;

   end Run_To_Next_Row;


   procedure Scan_Table (
      From      : in     Compilation_T;
      Result    : in out Result_Type)
   is
      use type In_Memory.Index_T;

      Num_Files : Natural := From.Header.Num_Files;
      -- The number of source files defined so far.
      -- Initialized to the number defined in the compilation unit
      -- header; incremented by every DW_LNE_define_file.

      State : Row_T := Initial_State (Within => From.Header);
      -- The state of the row registers.

      Row : Row_T := State;
      -- The current (created) row.

      Prev : Row_T;
      -- The previous row.

      First : Boolean := True;
      -- Whether Row is the first (true) row.

      Last : Boolean;
      -- Whether Row is the last row in the table.

      Done : Boolean := False;
      -- "All done" indication from the Action operation.

   begin

      if From.Prog_Start < From.Prog_End then
         -- The line-number program is not empty.

         -- Start at the start of the line-number program:

         In_Memory.Set_Index (
            Stream => From.Stream.all,
            To     => From.Prog_Start);

         loop

            Prev := Row;

            Run_To_Next_Row (
               Unit      => From.Header,
               Opcodes   => From.Stream,
               Op_End    => From.Prog_End,
               Trace     => Opt.Trace_Line_Number_Program,
               Num_Files => Num_Files,
               State     => State,
               Row       => Row);

            Last := In_Memory.Index (From.Stream.all) >= From.Prog_End;

            Action (
               Row    => Row,
               Prev   => Prev,
               First  => First,
               Last   => Last,
               From   => From,
               Done   => Done,
               Result => Result);

            exit when Last or Done;

            First := False;

         end loop;

      end if;

   exception

   when Ends_Without_End_Sequence =>

      Output.Warning (
           "DWARF line-number program "
         & "does not end with DW_LNE_end_sequence.");

   end Scan_Table;


   procedure Put (
      Item   : in File_Info_T;
      Indent : in Ada.Text_IO.Positive_Count)
   --
   -- Displays the source-file information on standard output.
   --
   is
      use Ada.Text_IO;
   begin

      Set_Col (Indent);
      Put_Line ("File Name   : " & Item.Name.all);
      Set_Col (Indent);
      Put_Line ("Dir Index   :"  & Natural'Image (Item.Dir_Index));
      Set_Col (Indent);
      Put_Line ("Timestamp   :"  & Timestamp_T'Image (Item.Timestamp));
      Set_Col (Indent);
      Put_Line ("File Length :"  & File_Length_T'Image (Item.File_Length));

   end Put;


   procedure Put (Item : in Unit_Header_T)
   --
   -- Displays the Compilation Unit Header on standard output.
   --
   is
      use Ada.Text_IO;
   begin

      Put_Line (
      "   Unit Length    :" & Image (Item.Unit_Length));

      Put_Line (
      "   Version        :" & Version_T'Image (Item.Version));

      Put_Line (
      "   Header Length  :" & Variant_Unsigned_T'Image (Item.Header_Length));

      Put_Line (
      "   Min. Instr. Len:"
      & Unsigned_8_T'Image (Item.Minimum_Instruction_Length));

      Put_Line (
      "   Default Is Stmt: " & Boolean_T'Image (Item.Default_Is_Stmt));

      Put_Line (
      "   Line Base      :" & Signed_8_T'Image (Item.Line_Base));

      Put_Line (
      "   Line Range     :" & Unsigned_8_T'Image (Item.Line_Range));

      Put_Line (
      "   Opcode Base    :" & Unsigned_8_T'Image (Item.Opcode_Base));

      Put_Line (
      "   Standard Opcode Lengths:");

      for S in 1 .. Item.Opcode_Base - 1 loop

         Put_Line (
              "      opcode"
            & Unsigned_8_T'Image (S)
            & " has"
            & Unsigned_8_T'Image (Item.Standard_Opcode_Lengths(S))
            & " operands");

      end loop;

      Put_Line (
      "   Include directories:");

      for I in 1 .. Length (Item.Include_Directories) loop

         Put_Line (
              "      "
            & Positive'Image (I)
            & ": "
            & Element (Item.Include_Directories, I).all);

      end loop;

      Put_Line (
      "   Files:");

      for F in 1 .. Length (Item.Files.all) loop

         Put_Line (
              "      "
            & "File #" & Positive'Image (F));

         Put (Item => Element (Item.Files.all, F), Indent => 9);

      end loop;

   end Put;


   --
   --    Displaying each row from the line-number table
   --


   type Nothing_T is null record;
   --
   -- The table-scan for row display has no result value.


   procedure Put_Row (
      Row    : in     Row_T;
      Prev   : in     Row_T;
      First  : in     Boolean;
      Last   : in     Boolean;
      From   : in     Compilation_T;
      Done   : in out Boolean;
      Result : in out Nothing_T)
   --
   -- Displays the Row on standard output.
   --
   is
      use Ada.Text_IO;

      Line_Col : constant := 10;
      Flag_Col : constant := 20;
      ISA_Col  : constant := 27;
      File_Col : constant := 33;

      Is_Stmt        : constant Character := 'S';
      Basic_Block    : constant Character := 'B';
      End_Sequence   : constant Character := 'T';
      Prologue_End   : constant Character := 'P';
      Epilogue_Begin : constant Character := 'E';
      Last_Row       : constant Character := 'L';

   begin

      -- Display table header for first row:

      if First then

         Put_Line ("DWARF line-number table");
         New_Line;

         Put ("Address");
         Set_Col (Line_Col); Put ("Line:Col");
         Set_Col (Flag_Col); Put ("Flags");
         Set_Col (ISA_Col);  Put ("ISA");
         Set_Col (File_Col); Put ("File");
         New_Line;

      end if;

      -- Display the Row:

      Put (Hex.Image (Hex.Word32_T (Row.Address)));

      Set_Col (Line_Col);
      Put (Output.Image (Row.Line));
      Put (':');
      Put (Output.Image (Row.Column));

      Set_Col (Flag_Col);
      if Row.Is_Stmt         then Put (Is_Stmt       ); end if;
      if Row.Basic_Block     then Put (Basic_Block   ); end if;
      if Row.End_Sequence    then Put (End_Sequence  ); end if;
      if Row.Prologue_End    then Put (Prologue_End  ); end if;
      if Row.Epilogue_Begin  then Put (Epilogue_Begin); end if;
      if Last                then Put (Last_Row      ); end if;

      Set_Col (ISA_Col);
      Put (Output.Image (Row.ISA));

      Set_Col (File_Col);
      Put_Line (Source_File_Name (File => Row.File, Within => From));

      -- Display legend after last row:

      if Last then

         New_Line;
         Put_Line ("Flags:");
         Put_Line ("   " & Is_Stmt        & " = Is_Stmt");
         Put_Line ("   " & Basic_Block    & " = Basic_Block");
         Put_Line ("   " & End_Sequence   & " = End_Sequence");
         Put_Line ("   " & Prologue_End   & " = Prologue_End");
         Put_Line ("   " & Epilogue_Begin & " = Epilogue_Begin");
         Put_Line ("   " & Last_Row       & " = Last row.");
         New_Line;

      end if;

   end Put_Row;


   procedure Put_Table
   is new Scan_Table (Result_Type => Nothing_T, Action => Put_Row);
   --
   -- Displays the line-number table on standard output.


   procedure Put_Line_Number_Program (
      Compilation : in  Compilation_T)
   --
   -- Displays the line-number program for a given compilation unit
   -- on the standard output.
   --
   -- Compilation
   --    The header information for the compilation unit.
   --    We assume that the line-number section has been loaded
   --    into an In_Memory buffer, accesible via Compilation.Stream.
   --
   is
      use Ada.Text_IO;

      Nothing : Nothing_T;
      -- Unused result from the table scan.

   begin

      Put_Line ("   Compilation Unit Line Number Program");
      Put_Line (
           "      Mem index, start:"
         & In_Memory.Index_T'Image (Compilation.Prog_Start));

      Put_Line (
           "      Mem index, end  :"
         & In_Memory.Index_T'Image (Compilation.Prog_End));

      Put_Table (
         From   => Compilation,
         Result => Nothing);

   exception

      when X : others =>

         Output.Exception_Info (
            Text       => "Exception while displaying DWARF line-numbers",
            Occurrence => X);

   end Put_Line_Number_Program;


   --
   --    Compilation_Set access operations
   --


   function Is_Null (Item : Compilation_Set_T) return Boolean
   is
      use type In_Memory.Stream_Ref;
   begin

      return Item.Stream = null;

   end Is_Null;


   --
   --    Load and Dump operations:
   --


   procedure Load (
      From    : in     IO.File_Type;
      Section : in     Segment_Desc_T;
      Giving  :    out Compilation_Set_T)
   is
   begin

      In_Memory.Load (
         From   => From,
         Part   => Section,
         Giving => Giving.Stream);

   end Load;


   function Compilation (
      At_Offset : Section_Offset_T;
      Within    : Compilation_Set_T)
   return Compilation_T
   is
      use type IO.Positive_Count;
      use type In_Memory.Count_T;

      Stream : In_Memory.Stream_Ref renames Within.Stream;
      -- Abbreviation.

      Org_Index : constant In_Memory.Index_T :=
        In_Memory.Index_T'First + In_Memory.Count_T (At_Offset);
      -- The Stream index where the compilation unit starts.

      Comp : Compilation_T;
      -- The result.

      Bits : Bits_T;
      -- The DWARF bit-width for Comp.

   begin

      Comp.Stream := Stream;

      Comp.Offset := At_Offset;

      In_Memory.Set_Index (
         Stream => Stream.all,
         To     => Org_Index);

      Unit_Header_T'Read (Stream, Comp.Header);

      Bits := Comp.Header.Unit_Length.Bits;

      Comp.Prog_Start :=
           Org_Index
         + In_Memory.Count_T (
              Initial_Length_Length (Bits)
            + Version_Length
            + Variant_Unsigned_Length (Bits))
         + In_Memory.Count_T (Comp.Header.Header_Length);
      --
      -- The additions to Org_Index are, in order, the lengths of
      -- the header fields:
      --    Unit_Length
      --    Version
      --    Header_Length
      --    all of the rest of the header after Header_Length.

      Comp.Prog_End :=
           Org_Index
         + In_Memory.Count_T (Initial_Length_Length (Bits))
         + In_Memory.Count_T (Comp.Header.Unit_Length.Length);

      return Comp;

   end Compilation;


   procedure Dump (
      Section : in Segment_Desc_T;
      From    : in IO.File_Type)
   is
      use Ada.Text_IO;
      use type IO.Positive_Count;
      use type In_Memory.Count_T;

      Offset :Section_Offset_T := 0;
      -- The current offset (of the next compilation unit).

      Comp : Compilation_T;
      -- Compilation unit info header.

   begin

      New_Line;

      Put_Line ("Dumping DWARF line-number info.");

      Put_Line (
           "Section starts at index"
         & IO.Positive_Count'Image (Section.Start)
         & " and contains"
         & IO.Count'Image (Section.Octets)
         & " octets.");

      In_Memory.Load (
         From   => From,
         Part   => Section,
         Giving => Comp.Stream);

      In_Memory.Text.Dump (Stream => Comp.Stream);

      while Offset < Section_Offset_T (Section.Octets) loop

         New_Line;

         Put_Line (
              "   Compilation unit at offset"
            & Section_Offset_T'Image (Offset));

         Comp := Compilation (
            At_Offset => Offset,
            Within    => Compilation_Set_T'(Stream => Comp.Stream));

         Put (Item => Comp.Header);

         Put_Line_Number_Program (Comp);

         Offset :=
              Offset
            + Section_Offset_T (Rounded_Length (Comp.Header.Unit_Length));

      end loop;

      New_Line;

      Put_Line ("End of DWARF line-number info.");

      New_Line;

   exception

      when X : others =>

         Output.Exception_Info (
            Text       => "Exception while dumping DWARF line-numbers",
            Occurrence => X);

   end Dump;


end Formats.Dwarf.Line_Numbers;
