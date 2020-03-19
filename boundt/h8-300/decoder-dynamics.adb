-- Decoder.Dynamics (body)
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
-- $Revision: 1.11 $
-- $Date: 2015/10/26 22:19:12 $
--
-- $Log: decoder-dynamics.adb,v $
-- Revision 1.11  2015/10/26 22:19:12  niklas
-- Version 4b, and moved to free open-source licence.
--
-- Revision 1.10  2010/02/25 12:18:30  niklas
-- Renamed Call_Via_Register_T to Call_Via_Pointer_T, as it is
-- now used also for memory-indirect dynamic calls (JSR @@aa:8).
-- Added Take_Asserted_Target (.. Call_Via_Pointer_T ..) to let
-- dynamic calls be resolved by assertions.
--
-- Revision 1.9  2009-12-02 14:20:01  niklas
-- BT-CH-0188: H8/300 updates for bit-widths and Word_T.
--
-- Revision 1.8  2007-03-22 18:51:34  niklas
-- BT-CH-0053.
--
-- Revision 1.7  2006/11/05 21:19:36  niklas
-- BT-CH-0036: Property BCC_Signed.
--
-- Revision 1.6  2005/10/20 15:36:20  niklas
-- Updated to match BT-CH-0014 as follows.
-- Removed the Retur component of Call_Via_Register_T because
-- it is no longer necessary. Ditto the Retur parameter of the
-- function Call_Via_Register.
-- Updated the operations Index and Close for Call_Via_Register_T.
-- The Index operation now uses no Return point, and the Close
-- operation now adds no edge.
--
-- Revision 1.5  2005/09/05 11:37:18  niklas
-- Added Call_Via_Register_T as allowed by BT-CH-0007.
--
-- Revision 1.4  2005/09/03 11:59:34  niklas
-- BT-CH-0006.
--
-- Revision 1.3  2005/05/11 18:18:08  niklas
-- Added the type Reference_T for the common parts (the components
-- Displacement and Width) of Stack_Reference_T and Mem_Reference_T.
-- This means that Stack_Reference_T is now also derived from
-- Interval_Pointer_T and has a Reduced_Alias_Range function.
-- Added functions that create a reference to the Low_Octet or
-- High_Octet of a referenced word, or to the Whole_Word that contains
-- a referenced octet. Added the function Width to show if a reference
-- points to an octet or a word.
--
-- Revision 1.2  2005/03/25 16:57:21  niklas
-- Added Jump_Via_Table_T.
--
-- Revision 1.1  2005/03/22 20:47:38  niklas
-- First version.
--
-- Revision 1.1  2005/03/04 14:22:29  niklas
-- First version, matching BT-CH-0002.
--


with Arithmetic;
with Arithmetic.Opt;
with Flow.Calls;
with Format;
with Output;
with Processor.Cells;


package body Decoder.Dynamics is


   use Processor;
   use type Arithmetic.Value_T;


   --
   ---   Dynamic references to Stack data:
   --


   Width_Mark : constant array (H8_300.Width_T) of Character := (
      H8_300.Octet => 'b',
      H8_300.Word  => 'w');
   --
   -- For use in the Image (Stack_Reference_T) and Image (Mem_Reference_T).



   function Width (Ref : Reference_T'Class)
   return H8_300.Width_T
   --
   -- The width of the referenced cell, which is either
   -- an Octet or a Word.
   --
   is
   begin

      case Ref.Width is
      when H8_300.Octet_Bits => return H8_300.Octet;
      when H8_300.Word_Bits  => return H8_300.Word;
      when others =>

         Output.Fault (
            Location => "Decoder.Dynamics.Width_Mark",
            Text     =>
                 "The width is"
               & Arithmetic.Width_T'Image (Ref.Width)
               & " bits.");

         return H8_300.Octet;

      end case;

   end Width;


   function Image (Item : Stack_Reference_T) return String
   is
   begin

      return
           Width_Mark(Width (Item))
         & "@(sp, "
         & Arithmetic.Image (Item.Displacement)
         & ')';

   end Image;


   function Reduced_Alias_Range (
      Pointer  : Stack_Reference_T;
      Interval : Storage.Bounds.Interval_List_T)
   return Storage.Alias_Range_T
   is
   begin

      return (others => False);  -- TBM

   end Reduced_Alias_Range;


   function Referent (
      Pointer : Stack_Reference_T;
      Value   : Storage.Bounds.Value_List_T)
   return Storage.Cell_T
   is

      Frame_Offset : Arithmetic.Value_T;
      -- The offset of the referenced cell, from the SP on entry to
      -- the subprogram, counted positive in the stack-growth direction.

   begin

      Frame_Offset := Value(Value'First) - Pointer.Displacement;

      if Frame_Offset >= Arithmetic.Value_T (Local_Offset_T'First) then
         -- A local cell.

         return Cells.Local (
            Offset => Local_Offset_T (Frame_Offset),
            Width  => Width (Pointer));

      elsif -Frame_Offset >= Arithmetic.Value_T (Param_Offset_T'First) then
         -- A parameter cell (including the return address as a
         -- special case).

         return Cells.Param (
            Offset => Param_Offset_T (-Frame_Offset),
            Width  => Width (Pointer));

      else
         -- Eh?

         Output.Fault (
            Location => "Decoder.Dynamics.Referent (Stack_Reference_T)",
            Text     =>
                 "Invalid Frame_Offset = "
               & Arithmetic.Image (Frame_Offset));

         return Storage.No_cell;

      end if;

   end Referent;


   --
   ---   Dynamic data references via some other Pointer register:
   --


   function Image (Item : Mem_Reference_T) return String
   is
   begin

      return
           Width_Mark(Width (Item))
         & "@("
         & Arithmetic.Image (Item.Expr(Item.Expr'First))
         & ", "
         & Arithmetic.Image (Item.Displacement)
         & ')';

   end Image;


   function Reduced_Alias_Range (
      Pointer  : Mem_Reference_T;
      Interval : Storage.Bounds.Interval_List_T)
   return Storage.Alias_Range_T
   is
   begin

      return (others => False);  -- TBM

   end Reduced_Alias_Range;


   function Referent (
      Pointer : Mem_Reference_T;
      Value   : Storage.Bounds.Value_List_T)
   return Storage.Cell_T
   is
      use type Arithmetic.Word_T;

      Base : constant Arithmetic.Value_T := Value(Value'First);
      -- The calculated value of the Pointer register.

      Signed_Address : constant Arithmetic.Value_T :=
         Base + Pointer.Displacement;
      -- The full memory address, but considered as a signed value
      -- in 2's complement.

      Address : constant Arithmetic.Word_T :=
         Arithmetic.Unsigned_Word (
            Value => Signed_Address,
            Width => H8_300.Address_Bits);
      -- The full memory address as an unsigned value.

   begin

      if Address <= Arithmetic.Word_T (Address_T'Last) then
         -- A valid address.

         return Cells.Memory (
            Address => Address_T (Address),
            Width   => Width (Pointer));

      else
         -- Address value out of range. Did we miss a wrap-around?

         Output.Fault (
            Location => "Decoder.Dynamic.Referent (Mem_Reference_T)",
            Text     =>
                 "Resolved value "
               & Arithmetic.Word_T'Image (Address)
               & " = "
               & Arithmetic.Image (Value(Value'First))
               & " + "
               & Arithmetic.Image (Pointer.Displacement)
               & " is not a valid address.");

         return Storage.No_Cell;

      end if;

   end Referent;


   --
   ---   Creating dynamic references to data:
   --


   Max_Positive_Displacement : constant := 2**15 - 1;
   --
   -- The largest displacement that we consider a positive displacement.
   -- Larger values are converted to negative displacements using 2's
   -- complement.


   Displacement_Range : constant := 2**16;
   --
   -- The total range of displacement values.


   function Reference (
      Operand : Register_Indirect_Operand_T;
      Width   : H8_300.Width_T)
   return Storage.References.Boundable_Ref
   is
      use type H8_300.Register_Number_T;

      Modified_Displacement : Arithmetic.Value_T;
      -- The given Displacement including possible Pre_Decrement.

      Signed_Displacement : Arithmetic.Value_T;
      -- The modified displacement with large positive values translated
      -- to smaller negative values.

      Stack_Ref : Stack_Reference_Ref;
      Mem_Ref   : Mem_Reference_Ref;
      -- The two possibilities.

   begin

      -- Compute the real displacement relative to the Pointer
      -- register before the instruction:

      case Operand.Auto_Mod is

      when H8_300.None | H8_300.Post_Increment =>

         Modified_Displacement := Arithmetic.Value_T (Operand.Displacement);

      when H8_300.Pre_Decrement =>

         Modified_Displacement :=
              Arithmetic.Value_T (Operand.Displacement)
            - Arithmetic.Value_T (H8_300.Operand_Octets(Width));

      end case;

      if Modified_Displacement <= Max_Positive_Displacement then
         -- We can believe that this is a positive displacement.

         Signed_Displacement := Modified_Displacement;

      elsif Operand.Pointer /= H8_300.SP then
         -- This displacement is so large that it probably means
         -- a (smaller) negative displacement. Negative displacements
         -- are not considered for SP-relative references because they
         -- would be unsafe in the presence of interrupts.

         Signed_Displacement := Modified_Displacement - Displacement_Range;

         if Arithmetic.Opt.Warn_Signing_Literal then

            Output.Warning (
                 "Displacement "
               & Arithmetic.Image (Modified_Displacement)
               & " taken as signed = "
               & Arithmetic.Image (Signed_Displacement));

         end if;

      end if;

      -- Create the relevant sort of dynamic reference:

      case Operand.Pointer is

      when 0 .. 6 =>

         Mem_Ref := new Mem_Reference_T (Dim => 1);

         Mem_Ref.Expr := (1 =>
            Cells.Register_Var(Operand.Pointer, H8_300.Word));

         Mem_Ref.Interval := (1 =>
            Storage.Bounds.Interval (
               Min => Arithmetic.Value_T (H8_300.Unsigned_Word_T'First),
               Max => Arithmetic.Value_T (H8_300.Unsigned_Word_T'Last)));

         Mem_Ref.Pointer      := Operand.Pointer;
         Mem_Ref.Displacement := Signed_Displacement;
         Mem_Ref.Width        := Cells.Width_In_Bits (Width);

         return Storage.References.Boundable_Ref (Mem_Ref);

      when H8_300.SP =>

         Stack_Ref := new Stack_Reference_T (Dim => 1);

         Stack_Ref.Expr         := (1 => Cells.SH);
         Stack_Ref.Displacement := Signed_Displacement;
         Stack_Ref.Width        := Cells.Width_In_Bits (Width);

         return Storage.References.Boundable_Ref (Stack_Ref);

      end case;

   end Reference;


   function Octet_Part (
      Word   : Storage.References.Boundable_Ref;
      Offset : Arithmetic.Value_T)
   return Storage.References.Boundable_Ref
   --
   -- Creates a dynamic reference to the octet that is offset from
   -- the given Ref by the given (octet) Offset.
   -- Ref must be a Reference_T to an even (aligned) memory access.
   --
   -- TBA extend octet Refs to show if they should point to the low or
   -- high octet of a referenced word, so that the octet is referenced
   -- correctly even if the word address is odd (the octet offset should
   -- be added to the word address rounded down to an even number).
   --
   is

      Part : Reference_Ref;
      -- The result.

   begin

      if Word.all in Reference_T'Class then

         Part := new Reference_T'Class'(Reference_T'Class (Word.all));

         Part.Displacement := Part.Displacement + Offset;
         Part.Width        := Cells.Width_In_Bits (H8_300.Octet);

         return Storage.References.Boundable_Ref (Part);

      else

         Output.Fault (
            Location => "Decoder.Dynamics.Octet_Part",
            Text     =>
                 "The Word is not in Reference_T'Class"
               & Output.Field_Separator
               & Storage.References.Image (Word.all));

        return Word;

      end if;

   end Octet_Part;


   function Low_Octet (Word : Storage.References.Boundable_Ref)
   return Storage.References.Boundable_Ref
   is
   begin

      return Octet_Part (Word => Word, Offset => 1);

   end Low_Octet;


   function High_Octet (Word : Storage.References.Boundable_Ref)
   return Storage.References.Boundable_Ref
   is
   begin

      return Octet_Part (Word => Word, Offset => 0);

   end High_Octet;


   function Whole_Word (Octet : Storage.References.Boundable_Ref)
   return Storage.References.Boundable_Ref
   is

      Whole : Reference_Ref;
      -- The result.

   begin

      if Octet.all in Reference_T'Class then

         Whole := new Reference_T'Class'(Reference_T'Class (Octet.all));

         Whole.Width := Cells.Width_In_Bits (H8_300.Word);

         return Storage.References.Boundable_Ref (Whole);

      else

         Output.Fault (
            Location => "Decoder.Dynamics.Whole_Word",
            Text     =>
                 "The Octet is not in Reference_T'Class"
               & Output.Field_Separator
               & Storage.References.Image (Octet.all));

        return Octet;

      end if;

   end Whole_Word;


   --
   ---   Properties of dynamic references to data:
   --


   function Width (Ref : Storage.References.Boundable_Ref)
   return H8_300.Width_T
   is
   begin

      if Ref.all in Reference_T'Class then

         return Width (Reference_T'Class (Ref.all));

      else

         Output.Fault (
            Location => "Decoder.Dynamics.Width",
            Text     =>
                 "The Ref is not in Reference_T'Class"
               & Output.Field_Separator
               & Storage.References.Image (Ref.all));

        return H8_300.Word;

      end if;

   end Width;


   --
   ---   Jump via a table of addresses:
   --


   procedure Index (
      Edge  : in out Jump_Via_Table_T;
      Value : in     Arithmetic.Value_T;
      Graph : in     Flow.Graph_T)
   is
      use type Arithmetic.Expr_Ref;

      Target : Processor.Code_Address_T;
      -- The target address, from the address-table.

   begin

      Target := Processor.Code_Address_T (
         Format.Code (
            Address => Processor.Shift (
               Base   => Edge.Base,
               Offset => Processor.Address_Offset_T (Value)),
            From => Edge.Code.all));

      Flow.Add_Resolved_Edge (
         To     => Graph,
         Source => Edge,
         Cond   => Edge.Index = Arithmetic.Const (
                      Value  => Value,
                      Width  => Edge.Index.Width,
                      Signed => True),
         Time   => 0,
         Target => Flow.Transit (
            From => Flow.Source (Edge),
            To   => (
               Kind    => Processor.Normal,
               Address => Target)));

   end Index;


   function Jump_Via_Table (
      Base  : Processor.Code_Address_T;
      Index : Arithmetic.Expr_Ref;
      Code  : Processor.Program.Info_T)
   return Flow.Dynamic_Edge_T
   is

      Edge : Jump_Via_Table_Ref;
      -- The new jump-via-table object.

   begin

      Edge := new Jump_Via_Table_T;

      Edge.Index   := Index;
      Edge.Aligned := 2;
      Edge.Base    := Base;
      Edge.Code    := Code;

      return Flow.Dynamic_Edge_T  (Edge);

   end Jump_Via_Table;


   --
   ---   Jump-to-subroutine via a register or memory vector
   --


   overriding
   procedure Index (
      Edge  : in out Call_Via_Pointer_T;
      Value : in     Arithmetic.Value_T;
      Graph : in     Flow.Graph_T)
   --
   -- Applies a new Value as an index to the indexable Edge, possibly
   -- giving new static edges in the Graph. For Call_Via_Pointer_T, which
   -- represents a dynamic call, each new Value Adds a Resolved Call to
   -- Graph.
   --
   -- Overrides (implements) Flow.Indexed.Index.
   is
      use type Arithmetic.Expr_Ref;

      Target : Processor.Code_Address_T;
      -- The target address, directly from the Value.

      Call : Programs.Call_T;
      -- The new call, not used here.

   begin

      begin

         Target := Processor.Code_Address_T (Value);

      exception

      when Constraint_Error =>

         Output.Warning (
              "Resolved callee address"
            & Arithmetic.Image (Value)
            & " is not a valid code address.");

         Output.Warning (
            "This dynamic call is not resolved further.");

         Flow.Mark_Stable (Edge);

         return;

      end;

      Flow.Calls.Add_Resolved_Call (
         To        => Graph,
         Source    => Edge,
         Caller    => Edge.Caller,
         Target    => Target,
         Cond      => Edge.Index = Arithmetic.Const (
                         Value  => Value,
                         Width  => Edge.Index.Width,
                         Signed => True),
         Time      => 0,
         Protocol  => Calling_Protocol (Programs.Program (Edge.Caller)),
         Call_Info => Processor.Program.Initial_Sub_Info,
         Step_Info => (Effort => Processor.No_Effort),
         Giving    => Call);

   end Index;


   overriding
   procedure Take_Asserted_Target (
      Edge   : in out Call_Via_Pointer_T;
      Target : in     Processor.Code_Address_T;
      Graph  : in     Flow.Graph_T)
   is

      Call : Programs.Call_T;
      -- The new call, not used here.

   begin

      Flow.Calls.Add_Resolved_Call (
         To        => Graph,
         Source    => Edge,
         Caller    => Edge.Caller,
         Target    => Target,
         Time      => 0,
         Protocol  => Calling_Protocol (Programs.Program (Edge.Caller)),
         Call_Info => Processor.Program.Initial_Sub_Info,
         Step_Info => (Effort => Processor.No_Effort),
         Giving    => Call);

   end Take_Asserted_Target;


   overriding
   procedure Close (
      Edge  : in out Call_Via_Pointer_T;
      Graph : in     Flow.Graph_T)
   is
   begin

      -- Check if the call was resolved at all:

      if Flow.Resolvents (Edge) = 0 then

         Output.Warning (
            "Unresolved JSR @Rn/@@aa taken as no-operation.");

      end if;

      -- Parent actions:

      Flow.Indexed.Close (
         Edge  => Flow.Indexed.Edge_T (Edge),
         Graph => Graph);

   end Close;


   function Call_Via_Pointer (
      Index  : Arithmetic.Expr_Ref;
      Caller : Programs.Subprogram_T)
   return Flow.Dynamic_Edge_T
   is

      Edge : Call_Via_Pointer_Ref;
      -- The new call-via-pointer object.

   begin

      Edge := new Call_Via_Pointer_T;

      Edge.Index   := Index;
      Edge.Aligned := 2;
      Edge.Caller  := Caller;

      return Flow.Dynamic_Edge_T  (Edge);

   end Call_Via_Pointer;


end Decoder.Dynamics;
