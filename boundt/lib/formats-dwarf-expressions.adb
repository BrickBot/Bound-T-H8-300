-- Formats.Dwarf.Expressions (body)
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
-- $Revision: 1.7 $
-- $Date: 2015/10/24 20:53:52 $
--
-- $Log: formats-dwarf-expressions.adb,v $
-- Revision 1.7  2015/10/24 20:53:52  niklas
-- Moved to free licence.
--
-- Revision 1.6  2013-02-03 10:50:28  niklas
-- BT-CH-0238: DWARF expression abstract evaluation - start.
--
-- Revision 1.5  2013-02-01 12:42:30  niklas
-- Simple_Expression handles over/underflows.
--
-- Revision 1.4  2008-10-29 10:40:59  niklas
-- Corrected Dump_Expression to output a line even for a null
-- expression block (displayed as "null").
--
-- Revision 1.3  2008/10/15 12:05:52  niklas
-- BT-CH-0150: DWARF location lists, lexical block scopes, CUBAs.
--
-- Revision 1.2  2007/06/14 11:16:40  niklas
-- BT-CH-0060.
--
-- Revision 1.1  2004/04/24 18:06:18  niklas
-- First version.
--


with Ada.Strings.Fixed;
with Ada.Text_IO;
with Formats.Dwarf.Opt;
with Formats.Dwarf.Text;
with Output;


package body Formats.Dwarf.Expressions is


   --
   ---   Operations
   --


   type Opcode_T is new Unsigned_8_T;
   --
   -- The code that identifies an operation, and also supplies the
   -- embedded operands for the operations Lit, Reg and Base_Reg.


   subtype Lit_Code_T is Opcode_T range 16#30# .. 16#4f#;
   --
   -- The Opcodes for Lit operations. The embedded operand is
   -- the Opcode minus Lit_Code_T'First.


   subtype Reg_Code_T is Opcode_T range 16#50# .. 16#6f#;
   --
   -- The Opcodes for Reg operations. The embedded operand is
   -- the Opcode minus Reg_Code_T'First.


   subtype Base_Reg_Code_T is Opcode_T range 16#70# .. 16#8f#;
   --
   -- The Opcodes for Bsae_Reg operations. The embedded operand
   -- is the Opcode minus Reg_Code_T'First.


   Operation_For_Code :
      constant array (Opcode_T range 16#03# .. 16#9f#)
      of Operation_T := (
      16#03#           => Addr,
      16#04#           => Invalid_Opcode,
      16#05#           => Invalid_Opcode,
      16#06#           => Deref,
      16#07#           => Invalid_Opcode,
      16#08#           => Const1u,
      16#09#           => Const1s,
      16#0a#           => Const2u,
      16#0b#           => Const2s,
      16#0c#           => Const4u,
      16#0d#           => Const4s,
      16#0e#           => Const8u,
      16#0f#           => Const8s,
      16#10#           => Constu,
      16#11#           => Consts,
      16#12#           => Dup,
      16#13#           => Drop,
      16#14#           => Over,
      16#15#           => Pick,
      16#16#           => Swap,
      16#17#           => Rot,
      16#18#           => X_Deref,
      16#19#           => Abs_Op,
      16#1a#           => And_Op,
      16#1b#           => Div,
      16#1c#           => Minus,
      16#1d#           => Mod_Op,
      16#1e#           => Mul,
      16#1f#           => Neg,
      16#20#           => Not_Op,
      16#21#           => Or_Op,
      16#22#           => Plus,
      16#23#           => Plus_Uconst,
      16#24#           => Shl,
      16#25#           => Shr,
      16#26#           => Shra,
      16#27#           => Xor_Op,
      16#28#           => Branch,
      16#29#           => Equal,
      16#2a#           => Greater_Or_Equal,
      16#2b#           => Greater,
      16#2c#           => Less_Or_Equal,
      16#2d#           => Less,
      16#2e#           => Not_Equal,
      16#2f#           => Skip,

      Lit_Code_T       => Lit,
      Reg_Code_T       => Reg,
      Base_Reg_Code_T  => Base_Reg,

      16#90#           => Reg_X,
      16#91#           => Frame_Base_Reg,
      16#92#           => Base_Reg_X,
      16#93#           => Piece,
      16#94#           => Deref_Size,
      16#95#           => X_Deref_Size,
      16#96#           => Nop,
      16#97#           => Push_Object_Address,
      16#98#           => Call2,
      16#99#           => Call4,
      16#9a#           => Call_Ref,
      16#9b#           => Invalid_Opcode,  -- form_tls_address
      16#9c#           => Invalid_Opcode,  -- call_frame_cfa
      16#9d#           => Bit_Piece,
      16#9e#           => Implicit_Value,
      16#9f#           => Stack_Value);


   function Operation (Opcode : Opcode_T) return Operation_T
   --
   -- Decodes the operation-code to an operation.
   --
   is
   begin

      if Opcode in Operation_For_Code'Range then

         return Operation_For_Code(Opcode);

      else

         return Invalid_Opcode;

      end if;

   end Operation;


   function Literal (Opcode : Lit_Code_T)
   return Address_T
   --
   -- The embedded literal constant operand of a Lit operation.
   --
   is
   begin

      return Address_T (Opcode - Lit_Code_T'First);

   end Literal;


   function Register (Opcode : Reg_Code_T)
   return Register_Number_T
   --
   -- The embedded literal register number of a Reg operation.
   --
   is
   begin

      return Register_Number_T (Opcode - Reg_Code_T'First);

   end Register;


   function Base_Register (Opcode : Base_Reg_Code_T)
   return Register_Number_T
   --
   -- The embedded literal base register number of a Base_Reg operation.
   --
   is
   begin

      return Register_Number_T (Opcode - Base_Reg_Code_T'First);

   end Base_Register;


   Const_Form : constant array (Const_Op_T) of Form_T := (
      Const1u => Data1,
      Const1s => Data1s,
      Const2u => Data2,
      Const2s => Data2s,
      Const4u => Data4,
      Const4s => Data4s,
      Const8u => Data8,
      Const8s => Data8s,
      Constu  => Udata,
      Consts  => Sdata);
   --
   -- The DWARF Form for the in-line operand of the Const operations.


   Call_Form : constant array (Call_Op_T) of Form_T := (
      Call2    => Ref2,
      Call4    => Ref4,
      Call_Ref => Ref_Addr);
   --
   -- The DWARF Form for the in-line operand of the Call operations.


   procedure Dump_Expression (
      Block  : in Block_T;
      Indent : in String)
   is
      use Ada.Text_IO;
      use type In_Memory.Index_T;

      From : constant In_Memory.Stream_Ref := Block.Stream;
      -- An abbreviation.

      End_Index : constant In_Memory.Index_T := Block.Start + Block.Octets;
      -- The index of the octet after the expression.

      First_Line : Boolean := True;
      -- Whether the next output line is the first one.

      Offset : In_Memory.Count_T;
      -- The offset from the start of the From to the current opcode.

      Relative_Offset : In_Memory.Count_T;
      -- The skip/branch count for a Skip or Branch, relative to the
      -- octet following the Skip or Branch operation.

      Target_Offset : In_Memory.Count_T;
      -- The skip/branch target offset, relative to the start of From.

      Opcode : Opcode_T;
      -- The code for the current operation.

      Op : Operation_T;
      -- The current operation.

   begin

      --Put_Line (
      --     "DWARF expression starts at index"
      --   & In_Memory.Index_T'Image (Start)
      --   & " and contains"
      --   & In_Memory.Count_T'Image (Octets)
      --   & " octets.");

      if Block.Octets = 0 then
         -- The expression is null.

         Put_Line ("null");

      else
         -- The expression has some contents.

         In_Memory.Set_Index (Stream => From.all, To => Block.Start);

         while In_Memory.Index (From.all) < End_Index loop

            -- Display the offset, opcode and operation:

            Offset := In_Memory.Index (From.all) - In_Memory.Index_T'First;

            if not First_Line then

               Text.Begin_Line (Indent);

            end if;

            First_Line := False;

            Put ("Opcode(offset");
            Put (In_Memory.Count_T'Image (Offset));

            Opcode_T'Read (From, Opcode);

            Put (") = ");
            Put (Image (Opcode));
            Put (" = ");

            Op := Operation (Opcode);

            Put (Operation_T'Image (Op));
            Put (" (");

            -- Display the operands if any:

            case Op is

            when Lit =>

               Put (Address_T'Image (Literal (Opcode)));

            when Addr =>

               Put (Text.Addr_Image (From, Block.Addr_Size));

            when Const_Op_T =>

               Put (Text.Constant_Image (From, Const_Form(Op)));

            when Reg =>

               Put (Register_Number_T'Image (Register(Opcode)));

            when Reg_X
               | Plus_Uconst
               | Piece
               =>

               Put (Text.Constant_Image (From, Udata));

            when Bit_Piece =>

               Put (Text.Constant_Image (From, Udata));
               Put (" bits from offset ");
               Put (Text.Constant_Image (From, Udata));

            when Frame_Base_Reg =>

               Put (Text.Constant_Image (From, Sdata));

            when Base_Reg =>

               Put (Register_Number_T'Image (Base_Register(Opcode)));

               Put (" + " & Text.Constant_Image (From, Sdata));

            when Base_Reg_X =>

               Put (Text.Constant_Image (From, Udata));

               Put (" + " & Text.Constant_Image (From, Sdata));

            when Pick
               | Deref_Size
               | X_Deref_Size
               =>

               Put (Text.Constant_Image (From, Data1));

            when Skip | Branch =>

               Relative_Offset := In_Memory.Count_T (Signed_16_T'Input (From));

               Target_Offset := In_Memory.Index (From.all) + Relative_Offset;

               Put (
                    In_Memory.Count_T'Image (Relative_Offset)
                  & ", to offset"
                  & In_Memory.Count_T'Image (Target_Offset));

            when Call_Op_T =>

               Put (Text.Reference_Image (
                  Info => From,
                  Form => Call_Form(Op),
                  Bits => Block.Bits,
                  Base => In_Memory.Index_T'First));   -- TBC.

            when Push_Object_Address
               | Pure_Stack_Op_T
               | Deref
               | X_Deref
               | Unary_Stack_Op_T
               | Binary_Stack_Op_T
               | Relation_Op_T
               | Stack_Value
               | Nop
               =>

               null;
               -- These operations have no in-line or embedded operands.

            when Implicit_Value =>

               Put ("implicit value ");

               Text.Dump_Block (
                  Info   => From,
                  Form   => Dwarf.Block,
                  Indent => Indent & "   ");

            when Invalid_Opcode =>

               Put_Line ("abort display)");

               exit;

            end case;

            Put_Line (" )");

         end loop;

      end if;

   end Dump_Expression;


   --
   ---   Evaluation of expressions using abstract(ed) value domains
   --


   procedure Unary_Op (
      Op      : in     Unary_Stack_Op_T;
      Operand : in out Abstract_Value_T)
   is
   begin
      raise Alien_Value;
   end Unary_Op;


   procedure Binary_Op (
      Op      : in     Binary_Stack_Op_T;
      Left    : in out Abstract_Value_T;
      Right   : in     Abstract_Value_T)
   is
   begin
      raise Alien_Value;
   end Binary_Op;


   procedure Relation (
      Op      : in     Relation_Op_T;
      Left    : in out Abstract_Value_T;
      Right   : in     Abstract_Value_T)
   is
   begin
      raise Alien_Value;
   end Relation;


   function To_Boolean (Relation : Abstract_Value_T)
   return Boolean
   is
   begin
      raise Alien_Value;
      return False;
   end To_Boolean;


   procedure Evaluator (
      Expr   : in     Block_T;
      Result :    out Value_T)
   is
   begin

      null;  -- TBA

   end Evaluator;


   --
   ---   Simple location expressions
   --


   function Simple_Location (Expr : Block_T) return Simple_Location_T
   is
      use type In_Memory.Count_T;

      From : constant In_Memory.Stream_Ref := Expr.Stream;
      -- An abbreviation.

      Opcode : Opcode_T;
      -- The code for the first operation.

      Op : Operation_T;
      -- The first operation.

      Num : Number_T := 0;
      -- Some number from the stream.
      -- Meaning depends on Opcode.

      Base_X_Reg    : Register_Number_T;
      Base_X_Offset : Number_T;
      -- For a Base_Reg_X location.

   begin

      In_Memory.Set_Index (Stream => From.all, To => Expr.Start);

      if Expr.Octets = 0 then
         -- Eh? a null expression.

         Output.Fault (
            Location => "Formats.Dwarf.Expressions.Simple_Location",
            Text     => "Null expression.");

         return (Kind => Complex, Expr => Expr);

      else

         begin

            Opcode_T'Read (From, Opcode);

            Op := Operation (Opcode);

            case Op is

            when Lit =>

               return (
                  Kind => Addr,
                  Addr => Literal (Opcode));

            when Addr =>

               return (
                  Kind => Addr,
                  Addr => Address (From, Expr.Addr_Size));

            when Const_Op_T =>

               begin

                  Num := Number (From, Const_Form(Op));

                  return (
                     Kind => Addr,
                     Addr => Address_T (Num));

               exception

               when Constraint_Error =>

                  Output.Note (
                       "Constant Dwarf operand "
                     & Number_T'Image (Num)
                     & " out of range, taken mod 2 ^"
                     & Natural'Image (Address_T'Size));

                  return (
                     Kind => Addr,
                     Addr => Address_T'Mod (Num));

               end;

            when Reg =>

               return (
                  Kind => Reg,
                  Reg  => Register (Opcode));

            when Reg_X =>

               Num := Number (From, Udata);

               return (
                  Kind => Reg,
                  Reg  => Register_Number_T (Num));

            when Frame_Base_Reg =>

               Num := Number (From, Sdata);

               return (
                  Kind         => Frame,
                  Frame_Offset => Num);

            when Base_Reg =>

               Num := Number (From, Sdata);

               return (
                  Kind        => Based,
                  Base_Reg    => Base_Register (Opcode),
                  Base_Offset => Num);

            when Base_Reg_X =>

               -- Read the components in order:

               Base_X_Reg    := Register_Number_T (Number (From, Udata));
               Base_X_Offset := Number (From, Sdata);
               Num           := Base_X_Offset;

               -- Compose the aggregate result:

               return (
                  Kind        => Based,
                  Base_Reg    => Base_X_Reg,
                  Base_Offset => Base_X_Offset);

            when others =>

               return (Kind => Complex, Expr => Expr);

            end case;

         exception

         when X : Constraint_Error =>

            Output.Exception_Info (
               Text =>
                    "Evaluating DWARF "
                  & Operation_T'Image (Op)
                  & ", Num ="
                  & Number_T'Image (Num),
               Occurrence => X);

            Output.Warning ("DWARF expression evaluated as ""complex"".");

            return (Kind => Complex, Expr => Expr);

         end;

      end if;

   end Simple_Location;


   --
   ---   Location lists
   --


   function To_Location_List (
      Block : Block_T;
      Base  : Code_Address_T)
   return Location_List_T
   is
      use type In_Memory.Count_T;

      From : constant In_Memory.Stream_Ref := Block.Stream;
      -- An abbreviation.

      List : Location_List_T (1 .. Opt.Location_List_Max_Length);
      Last : Natural := 0;
      -- The result will be List(1 .. Last), or List(1 .. List'Last)
      -- if there are too many entries.

      App_Base : Code_Address_T := Base;
      -- The applicable base address.

      Beginning, Ending : Code_Address_T;
      -- The address range of the current entry.

      Expr_Start : In_Memory.Index_T;
      -- The starting index of the location expression for the
      -- current location entry.

      Expr_Len : In_Memory.Count_T;
      -- The length, in octets, of the location expression for the
      -- current location entry.

   begin

      In_Memory.Set_Index (Stream => From.all, To => Block.Start);

      loop

         Read_Address (
            Stream => From,
            Size   => Block.Addr_Size,
            Item   => Beginning);

         Read_Address (
            Stream => From,
            Size   => Block.Addr_Size,
            Item   => Ending);

         exit when Beginning = 0 and Ending = 0;
         -- The end-of-list entry.

         if Beginning = Last_Address(Block.Addr_Size) then
            -- A base-address selection entry.

            App_Base := Ending;

         else
            -- A location entry.

            Unsigned_16_T'Read (From, Unsigned_16_T (Expr_Len));

            Expr_Start := In_Memory.Index (From.all);

            Last := Last + 1;

            if Last <= List'Last then

               List(Last) := (
                  Beginning => App_Base + Beginning,
                  Ending    => App_Base + Ending,
                  Expr      => Block_T'(
                     Stream    => From,
                     Start     => Expr_Start,
                     Octets    => Expr_Len,
                     Addr_Size => Block.Addr_Size,
                     Bits      => Block.Bits));

            end if;

            In_Memory.Set_Index (
               Stream => From.all,
               To     => Expr_Start + Expr_Len);

         end if;

      end loop;

      if Last > List'Last then

         Output.Warning (
              "DWARF location list has"
            & Natural'Image (Last)
            & " entries. Only the first"
            & Natural'Image (List'Length)
            & " will be used.");

         Last := List'Last;

      end if;

      return List(1 .. Last);

   end To_Location_List;


   procedure Dump_Location_List (
      Block  : in Block_T;
      Base   : in Code_Address_T;
      Indent : in String)
   is
      use Ada.Strings.Fixed;
      use Ada.Text_IO;
      use type In_Memory.Index_T;

      Expr_Indent : constant String :=
         Indent & ((4 * Natural (Block.Addr_Size) + 6) * " ");
      -- The indentation prefix for the expressions in the location
      -- list. The added indentation is for the beginning and ending
      -- addresses expressed in hexadecimal.

      From : constant In_Memory.Stream_Ref := Block.Stream;
      -- An abbreviation.

      App_Base : Code_Address_T := Base;
      -- The applicable base address.

      Beginning, Ending : Code_Address_T;
      -- The address range of the current entry.

      Expr_Start : In_Memory.Index_T;
      -- The starting index of the location expression for the
      -- current location entry.

      Expr_Len : In_Memory.Count_T;
      -- The length, in octets, of the location expression for the
      -- current location entry.

   begin

      Text.Put_Line (Indent,
           "DWARF location list starts at index ="
         & In_Memory.Index_T'Image (Block.Start)
         & ", base = " & Image (Base, Block.Addr_Size));

      In_Memory.Set_Index (Stream => From.all, To => Block.Start);

      loop

         Read_Address (
            Stream => From,
            Size   => Block.Addr_Size,
            Item   => Beginning);

         Read_Address (
            Stream => From,
            Size   => Block.Addr_Size,
            Item   => Ending);

         exit when Beginning = 0 and Ending = 0;
         -- The end-of-list entry.

         Text.Begin_Line (Indent);

         if Beginning = Last_Address(Block.Addr_Size) then
            -- A base-address selection entry.

            App_Base := Ending;

            Put_Line (
                 "Base address = "
               & Image (App_Base, Block.Addr_Size));

         else
            -- A location entry.

            Put (
                 Image (App_Base + Beginning, Block.Addr_Size)
               & " .. "
               & Image (App_Base + Ending, Block.Addr_Size)
               & "  ");

            Unsigned_16_T'Read (From, Unsigned_16_T (Expr_Len));

            Expr_Start := In_Memory.Index (From.all);

            Dump_Expression (
               Block => (
                  Stream    => From,
                  Start     => Expr_Start,
                  Octets    => Expr_Len,
                  Addr_Size => Block.Addr_Size,
                  Bits      => Block.Bits),
               Indent    => Expr_Indent);

            In_Memory.Set_Index (
               Stream => From.all,
               To     => Expr_Start + Expr_Len);

         end if;

      end loop;

   end Dump_Location_List;


   --
   ---   Simple location lists
   --


   function To_Simple (Item : Location_List_T)
   return Simple_Location_List_T
   is

      Result : Simple_Location_List_T (Item'Range);

   begin

      for I in Item'Range loop

         Result(I) := (
            Beginning => Item(I).Beginning,
            Ending    => Item(I).Ending,
            Location  => Simple_Location (Item(I).Expr));

      end loop;

      return Result;

   end To_Simple;


end Formats.Dwarf.Expressions;
