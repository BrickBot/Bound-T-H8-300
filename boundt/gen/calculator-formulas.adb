-- Calculator.Formulas (body)
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
-- $Revision: 1.42 $
-- $Date: 2015/10/24 20:05:46 $
--
-- $Log: calculator-formulas.adb,v $
-- Revision 1.42  2015/10/24 20:05:46  niklas
-- Moved to free licence.
--
-- Revision 1.41  2013/12/22 20:14:56  niklas
-- Value enumerators obey Storage.Bounds.Opt.Max_Listed_Values and raise
-- Unbounded_Set or Storage.Bounds.Unbounded if more values are offered;
-- they do not return a truncated list.
--
-- Revision 1.40  2013-02-12 08:47:19  niklas
-- BT-CH-0245: Global volatile cells and "volatile" assertions.
--
-- Revision 1.39  2011-08-31 04:23:33  niklas
-- BT-CH-0222: Option registry. Option -dump. External help files.
--
-- Revision 1.38  2010-11-28 15:36:33  niklas
-- BT-CH-0221: Translate Exts as identity for Omega Calculator.
--
-- Revision 1.37  2009-12-14 14:49:05  niklas
-- Corrected (extended) Conjoin_Assignment (Target, Expr, To) to handle
-- the Ternary operation Minus_C by calling Conjoin_Carry_From_Minus.
--
-- Revision 1.36  2009-11-27 11:28:07  niklas
-- BT-CH-0184: Bit-widths, Word_T, failed modular analysis.
--
-- Revision 1.35  2009-10-07 19:26:09  niklas
-- BT-CH-0183: Cell-sets are a tagged-type class.
--
-- Revision 1.34  2009-01-18 09:01:16  niklas
-- Removed unused local functions.
--
-- Revision 1.33  2008/04/26 19:19:43  niklas
-- BT-CH-0124: Joint loop counters and induction variables.
--
-- Revision 1.32  2008/04/22 12:43:30  niklas
-- Added function Is_Null (Set).
--
-- Revision 1.31  2007/10/05 07:23:54  niklas
-- BT-CH-0085: Multi-bit "xor" as equality test.
--
-- Revision 1.30  2007/10/02 20:37:58  niklas
-- BT-CH-0080: One-bit bitwise Boolean arithmetic operators.
--
-- Revision 1.29  2007/07/26 11:08:53  niklas
-- BT-CH-0066. Fracture assignments.
--
-- Revision 1.28  2007/05/02 09:29:44  niklas
-- Added Parens() to function Join to correct precedence errors
-- happening in function Apply. Added Parens () to function Apply
-- on general principles. Updated function Adapted (Flux) to
-- remove unnecessary Parens (); the ones around the Intersection
-- call were already unnecessary.
--
-- Revision 1.27  2006/09/04 15:05:54  niklas
-- Extended Write_Sliced to allow line breaks at '(' characters, too.
-- The Omega formula for the union of flows into the node after a large
-- switch/case can start with many consecutive '('. See, for example,
-- the test program tp_cover.
--
-- Revision 1.26  2006/04/28 09:40:02  niklas
-- Added function Interval (Interval_T) return Set.
-- Corrected parameter Hull_Bound.Set to be type Set_T.
-- Added function Values (Set) giving Value_List_T or
-- the new exception Unbounded_Set.
-- Changed Processor.Value_T to Arithmetic.Value_T for
-- better isolation (they are the same type, still).
-- Added two new Conjoin procedures, one comparing a
-- variable to a Limit_T and another to Interval_T, to
-- encapsulate some common actions in the new function
-- Interval (Interval_T) and the existing function
-- Set (Cell_Interval_List_T).
--
-- Revision 1.25  2006/04/10 08:20:44  niklas
-- Changed function Is_Calculable to consider "not calculable"
-- any constant that exceeds the calculator number range, taking
-- over this check from the function Arithmetic.Const.
--
-- Revision 1.24  2005/10/20 11:28:29  niklas
-- BT-CH-0015.
--
-- Revision 1.23  2005/09/16 13:05:24  niklas
-- Added surrounding Parens() to the union, intersection, range
-- constraint and domain constraint formulae. The Omega manual
-- does not really explain the precedences, so parentheses are
-- safer.
--
-- Revision 1.22  2005/09/06 12:15:37  niklas
-- Extended Conjoin_Range_Constraint to simplify the Omega encoding
-- to one "=" constraint when the Min and Max bounds of the range
-- assignment are the same expression structure (same Expr_Ref).
--
-- Revision 1.21  2005/04/17 08:02:18  niklas
-- Changed caclulator problems to report Fault instead of Error.
--
-- Revision 1.20  2005/02/19 20:34:55  niklas
-- BT-CH-0003.
--
-- Revision 1.19  2005/02/16 21:11:40  niklas
-- BT-CH-0002.
--
-- Revision 1.18  2004/08/09 19:51:29  niklas
-- BT-CH-0001.
--
-- Revision 1.17  2004/05/01 20:12:17  niklas
-- First Tidorum version.
-- Taking Cell_T stuff from Storage, not from Arithmetic. Removed "use" for
-- Arithmetic, added some "use type" for Storage types.
-- Added predicates Is_Nil and Not_Nil.
-- Added function Set to formulate Range_Pre constraints.
-- Added Null_Relation to model false conditions.
-- Replaced the function Relation (Effect) by Transformation that maps
-- a null Effect to a Nil relation, for use in Join or Apply.
-- Changed the Apply function to treat the Nil relation as a neutral
-- element (no change in the set).
-- Replaced the Convert function by two variants of the Adapted function,
-- for pools and fluxes respectively. The new functions detect when no real
-- adaptation is required (no change in the cell set) and return the
-- input without any further calculation.
-- Added some support for unsigned expressions.
-- Operators that are not "calculable", such as bit-wise logical operators,
-- are mapped to inequalities or to unknown values (unconstrained).
-- A conditional assignment with an unknown (or not calculable) condition
-- can give valid constraints because the true/false cases can imply some
-- common constraints.
-- A bound that admits only a single value is now expressed as an equality
-- constraint, not two inequalities.
-- Added a set of Conjoin operations to build formulas from conjuncts.
-- Added support for range assignment constraints.
-- Added support for the option Keep_Files.
--
-- Revision 1.16  2001/09/28 09:15:05  holsti
-- Platform-dependent parts moved to Calculator.Platform.
--
-- Revision 1.15  2001/03/14 17:38:32  holsti
-- Write_Sliced can break comments anywhere.
--
-- Revision 1.14  2001/03/10 00:23:34  holsti
-- Symbolic constant cells no longer used.
--
-- Revision 1.13  2001/02/20 10:43:43  holsti
-- Fixed NC_104: Deadlock in Omega pipe i/o.
--
-- Revision 1.12  2001/01/07 21:58:09  holsti
-- Comment operation added.
--
-- Revision 1.11  2001/01/04 16:09:23  saarinen
-- Moved function Convert for calculator to here.
--
-- Revision 1.10  2000/12/28 13:28:17  saarinen
-- Added function Complement.
--
-- Revision 1.9  2000/11/23 12:49:22  saarinen
-- Deleted procedure Symbolic and variables
-- Next_Pool_ID and Next_Flux_ID.
--
-- Revision 1.8  2000/09/20 18:46:23  saarinen
-- Added function Symbolic.
-- Made little changes regarding modified Calc_Handle_T.
--
-- Revision 1.7  2000/08/20 21:04:00  holsti
-- Calculator I/O tracing added. Null reply caught.
--
-- Revision 1.6  2000/08/17 13:00:14  holsti
-- Mapping domain to expression added.
--
-- Revision 1.5  2000/07/17 21:00:06  holsti
-- Cell_Set_T instead of Cell_List_T for domains and ranges.
--
-- Revision 1.4  2000/07/16 18:40:32  holsti
-- Transitive_Closure added.
--
-- Revision 1.3  2000/07/14 20:32:19  holsti
-- Include creation of new pools and fluxes.
--
-- Revision 1.2  2000/07/12 20:47:40  holsti
-- Start/Stop calculator implemented here.
--
-- Revision 1.1  2000/07/12 12:26:12  holsti
-- Calculator.Formulas added and used.
--


with Arithmetic.Opt;
with Calculator.Opt;
with Calculator.Parser;
with Calculator.Platform;
with Exec_Cmd;
with Options.Strings;
with Output;
with Storage.Bounds.Opt;
with Storage.Reserved_Names;

with Ada.Strings.Maps;
with Ada.Strings.Fixed;
with Ada.Strings.Unbounded;


package body Calculator.Formulas is


   use Ada.Strings.Unbounded;

   use type Arithmetic.Assignment_Kind_T;
   use type Arithmetic.Expr_Kind_T;
   use type Arithmetic.Expr_Ref;

   use type Cell_Set_T;
   use type Storage.Bounds.Limit_Kind_T;


   --
   ---   Formula-construction operations
   --


   function Nil return Formula_T
   is
   begin
      return Null_Unbounded_String;
   end Nil;


   function Is_Nil (Item : Formula_T) return Boolean
   is
   begin

      return Length (Item) = 0;

   end Is_Nil;


   function Not_Nil (Item : Formula_T) return Boolean
   is
   begin

      return Length (Item) > 0;

   end Not_Nil;


   function Parens (Item : Formula_T) return Formula_T
   is
   begin
      return '(' & Item & ')';
   end Parens;


   function To_Formula (Item : String) return Formula_T
   --
   -- Just a conversion from a standard string to a formula.
   --
   renames Ada.Strings.Unbounded.To_Unbounded_String;


   function Id (Item : Pool_T'Class) return Identifier_T
   is
      S : String := Pool_Id_T'Image (Item.Id);
   begin

      return To_Formula ("p_" & S(2..S'Last));

   end Id;


   function Id (Item : Flux_T'Class) return Identifier_T
   is
      S : String := Flux_Id_T'Image (Item.Id);
   begin

      return To_Formula ("f_" & S(2..S'Last));

   end Id;


   function Variables (Cells : Cell_Set_T)
   return Formula_T
   --
   -- The names of the Cells, as in a tuple, but without the
   -- bracketing [ ].
   --
   is

      List : constant Storage.Cell_List_T := Cell_Sets.To_List (Cells);
      -- The cells, listed.

      Result : Formula_T;
      -- The result, under construction.

   begin

      for C in List'Range loop

         Append (Result, Storage.Name_Of (List(C)));

         if C < List'Last then
            Append (Result, ',');
         end if;

      end loop;

      return Result;

   end Variables;


   function Tuple (Contents : String) return Tuple_T
   --
   -- The tuple defined by the Contents, normally a list of
   -- variable names, some perhaps with dashes.
   --
   is
   begin

      return To_Formula ("[")
           & Contents
           & ']';

   end Tuple;


   function Tuple (Cells : Cell_Set_T) return Tuple_T
   is
   begin

      return '[' & Variables (Cells) & ']';

   end Tuple;


   function Tuple (Cell : Storage.Cell_T) return Tuple_T
   is
   begin

      return Tuple (Storage.Name_Of (Cell));

   end Tuple;


   function Name (Cell : Storage.Cell_T; Dash : Boolean)
   return String
   --
   -- The name of the cell, perhaps with a dash to choose the
   -- new (post) value of the cell rather than the old (pre) value.
   --
   is
   begin

      if Dash then

         return Storage.Name_Of (Cell) & ''';

      else

         return Storage.Name_Of (Cell);

      end if;

   end Name;


   generic

      with function Dash (Cell : Storage.Cell_T) return Boolean;

   function Tuple_With_Dashes (Cells : Cell_Set_T)
   return Tuple_T;
   --
   -- The tuple that contains the given cells, with dashes placed
   -- on those cells for which Dash returns True.


   function Tuple_With_Dashes (Cells : Cell_Set_T)
   return Tuple_T
   is

      List : constant Storage.Cell_List_T := Cell_Sets.To_List (Cells);
      -- The cells, listed.

      Result : Tuple_T := To_Formula ("[");
      -- The result, under construction.

   begin

      for V in List'range loop

         Append (
            Result,
            Name (
               Cell => List(V),
               Dash => Dash (List(V))));

         if V < List'last then
            Append (Result, ',');
         end if;

      end loop;

      Append (Result,']');

      return Result;

   end Tuple_With_Dashes;


   function Tuple (
      Cells         : Cell_Set_T;
      Dashed        : Cell_Set_T;
      Dash_Volatile : Boolean)
   return Tuple_T
   is

      function Dash (Cell : Storage.Cell_T) return Boolean
      is
      begin

         return Cell_Sets.Is_Member (Cell, Dashed)
         or else (Dash_Volatile
                  and then Storage.Is_Volatile (Cell));

      end Dash;

      function Dashed_Tuple is new Tuple_With_Dashes (Dash);

   begin

      return Dashed_Tuple (Cells);

   end Tuple;


   function Tuple (
      Cells  : Cell_Set_T;
      Dashed : Arithmetic.Effect_T)
   return Tuple_T
   --
   -- The tuple that contains the given cells, with a dash mark
   -- for the cells that are targets of assignments in Dashed, or
   -- are volatile cells.
   -- Note that this Dashed set does not have to be a subset of Cells.
   --
   is

      function Dash (Cell : Storage.Cell_T) return Boolean
      is
      begin

         return  Storage.Is_Volatile (Cell)
         or else Arithmetic.Is_Defined (Cell => Cell, By => Dashed);

      end Dash;

      function Dashed_Tuple is new Tuple_With_Dashes (Dash);

   begin

      return Dashed_Tuple (Cells);

   end Tuple;


   --
   ---   Conversion of Arithmetic.Expr_T to calculator Formulae
   --


   Calculable_Unary_Op :
      constant array (Arithmetic.Unary_Op_T) of Boolean := (
         Arithmetic.Notw     => False,
         Arithmetic.Notx     => True,
         Arithmetic.EqZero   => True,
         Arithmetic.EqOne    => True,
         Arithmetic.Signw    => True,
         Arithmetic.Exts     => True,
         Arithmetic.Extz     => True,
         Arithmetic.Trunl    => False,
         Arithmetic.Trunh    => False);
   --
   Calculable_Binary_Op :
      constant array (Arithmetic.Binary_Op_T) of Boolean := (
         Arithmetic.Plus     => True,
         Arithmetic.Minus    => True,
         Arithmetic.Mulu     => False,
         Arithmetic.Muls     => False,

         Arithmetic.Plus_C   => False,
         Arithmetic.Plus_N   => False,
         Arithmetic.Plus_V   => False,

         Arithmetic.Minus_B  => True,
         Arithmetic.Minus_C  => True,
         Arithmetic.Minus_N  => True,
         Arithmetic.Minus_V  => False,

         Arithmetic.Eq       => True,

         Arithmetic.Lts      => True,
         Arithmetic.Gts      => True,
         Arithmetic.Les      => True,
         Arithmetic.Ges      => True,

         Arithmetic.Ltu      => True,
         Arithmetic.Gtu      => True,
         Arithmetic.Leu      => True,
         Arithmetic.Geu      => True,

         Arithmetic.Andx     => True,
         Arithmetic.Orx      => True,

         Arithmetic.Andw     => False,
         Arithmetic.Orw      => False,
         Arithmetic.Xorw     => False,

         Arithmetic.Slz      => False,
         Arithmetic.Srz      => False,
         Arithmetic.Sra      => False,
         Arithmetic.Rotl     => False,
         Arithmetic.Rotr     => False,

         Arithmetic.Conc     => True);
   --
   Calculable_Ternary_Op :
      constant array (Arithmetic.Ternary_Op_T) of Boolean := (
         Arithmetic.Plus         => True,
         Arithmetic.Minus_With_B => True,
         Arithmetic.Minus_With_C => True,

         Arithmetic.Plus_C       => False,
         Arithmetic.Plus_N       => False,
         Arithmetic.Plus_V       => False,

         Arithmetic.Minus_B      => True,
         Arithmetic.Minus_C      => True,
         Arithmetic.Minus_BN     => True,
         Arithmetic.Minus_CN     => True,
         Arithmetic.Minus_BV     => False,
         Arithmetic.Minus_CV     => False);
   --
   -- Those operators in expressions that can be faithfully translated
   -- into calculator formulas by the Arithmetic.Image function are
   -- marked by True values.
   --
   -- The remaining operators must either receive special translation
   -- or will make Unknown any expression that contains them.
   --
   -- The multiplication operators are calculable only if one of the
   -- operands is a constant.
   --
   -- The bitwise operators are given special translations when the
   -- operator is the topmost (outermost) one in the whole expression
   -- that is assigned to a variable.
   --
   -- The Slz operator is subjected to an additional check: the
   -- amount of shift (2nd, right, operand) must be a constant and must
   -- be in the interval 0 .. Opt.Max_Shift_Mul_Bits.
   --
   -- The Conc operator is subjected to an additional check: the
   -- amount of shift (width of the 2nd, right, operand) must be
   -- in the interval 0 .. Opt.Max_Shift_Mul_Bits.
   --
   -- The Notw operator is subjected to an additional check: for
   -- single-bit operands (Width = 1) Notw is calculable as the
   -- arithmetic function x -> 1 - x.


   function Is_Calculable (Expr : Arithmetic.Expr_Ref)
   return Boolean
   --
   -- Whether the expression contains only calculable operators.
   -- An Unknown expression is not calculable.
   -- A dynamic memory Ref is not calculable.
   --
   is
      use type Arithmetic.Width_T;

      Value : Arithmetic.Value_T;
      -- The value of a Constant Expr.

   begin

      if Expr = Arithmetic.Unknown then

         return False;

      else

         case Expr.Kind is

         when Arithmetic.Opaque
            | Arithmetic.Ref =>

            return False;

         when Arithmetic.Cell =>

            return True;

         when Arithmetic.Const =>

            Value := Arithmetic.Const_Value (Expr);

            if Value in Opt.Min_Int_Calc .. Opt.Max_Int_Calc then

               return True;

            else
               -- The value is too large.

               if Opt.Warn_Large_Literal then

                  Output.Warning (
                       "Constant "
                     & Arithmetic.Value_T'Image (Value)
                     & " exceeds calculator range, considered unknown.");

               end if;

               return False;

            end if;

         when Arithmetic.Unary_Kind =>

            case Expr.Unary is

            when Arithmetic.Notw =>
               -- Bit-wise negation (logical complement).

               return Expr.Width = 1
                  and then Is_Calculable (Expr.Expr);

            when others =>

               return
                  Calculable_Unary_Op(Expr.Unary)
                  and then Is_Calculable (Expr.Expr);

            end case;

         when Arithmetic.Binary_Kind =>

            case Expr.Binary is

            when Arithmetic.Mulu | Arithmetic.Muls =>

               return Arithmetic.Is_Constant (Expr.L_Expr)
                   or Arithmetic.Is_Constant (Expr.R_Expr);

            when Arithmetic.Slz =>
               -- The Left value, shifted left by Right bits.

               return
                  Arithmetic.Can_Shift_By_Mul (Expr)
                  and then Is_Calculable (Expr.L_Expr);
                  --
                  -- Can_Shift_By_Mul (Expr) implies that Expr.R_Expr
                  -- is a known constant, thus calculable.

            when Arithmetic.Conc =>
               -- The Left value, shifted left by Width_Of (Right) bits
               -- and combined (inclusive-or) with the Right value.

               return
                  Arithmetic.Can_Shift_By_Mul (
                     Arithmetic.Word_T (Arithmetic.Width_Of (Expr.R_Expr)))
                  and then Is_Calculable (Expr.L_Expr)
                  and then Is_Calculable (Expr.R_Expr);

            when others =>

               return
                  Calculable_Binary_Op(Expr.Binary)
                  and then Is_Calculable (Expr.L_Expr)
                  and then Is_Calculable (Expr.R_Expr);

            end case;

         when Arithmetic.Ternary_Kind =>

            return
               Calculable_Ternary_Op(Expr.Ternary)
               and then Is_Calculable (Expr.L3_Expr)
               and then Is_Calculable (Expr.R3_Expr)
               and then Is_Calculable (Expr.C3_Expr);

         end case;

      end if;

   end Is_Calculable;


   Mod_Factor1 : String renames
      Storage.Reserved_Names.Name(Storage.Reserved_Names.Mod_Factor1).all;
   --
   Mod_Factor2 : String renames
      Storage.Reserved_Names.Name(Storage.Reserved_Names.Mod_Factor2).all;
   --
   -- Identify the modular factors that are existentially quantified in
   -- the modular model of comparison operations.


   function Modulus (Width : Arithmetic.Width_T) return String
   --
   -- The modulus (2**Width) of this Width.
   --
   is
      use Arithmetic;
      use type Arithmetic.Value_T;
   begin

      return Image (Value_T (Max_Word (Width)) + 1);
      --
      -- We convert Max_Word to Value_T, before adding 1, as Word_T
      -- may be too narrow for the full Modulus.

   end Modulus;


   function Signed_Relation (
      Left  : String;
      Op    : String;
      Right : String;
      Width : Arithmetic.Width_T)
   return String
   --
   -- The image of a signed relation Operation, Left Op Right,
   -- interpreted modulo 2**Width. The Op must be one of "<"
   -- or "<=", not ">" or ">=".
   --
   is
      use Arithmetic;
   begin

      return
           "(exists " & Mod_Factor1 & ',' & Mod_Factor2 & ':'
         &         Image (Min_Negative (Width))
         &  "<=" & Left  & '-' & Modulus (Width) & Mod_Factor1
         &  Op   & Right & '-' & Modulus (Width) & Mod_Factor2
         &  "<=" & Image (Max_Positive (Width))
         &  ')';

   end Signed_Relation;


   function Unsigned_Relation (
      Left    : String;
      Op      : String;
      Right   : String;
      Modulus : String)
   return String
   --
   -- The image of an unsigned relation Operation, Left Op Right,
   -- interpreted modulo the Modulus. The Op must be one of "<"
   -- or "<=", not ">" or ">=".
   --
   is
   begin

      return
           "(exists " & Mod_Factor1 & ',' & Mod_Factor2 & ':'
         & "0<=" & Left  & '-' & Modulus & Mod_Factor1
         &  Op   & Right & '-' & Modulus & Mod_Factor2
         &  '<'  & Modulus
         &  ')';

   end Unsigned_Relation;


   function Modular_Relation (
      Left  : String;
      Op    : Arithmetic.Relation_Op_T;
      Right : String;
      Width : Arithmetic.Width_T)
   return String
   --
   -- The image of a relational Operation, Left Op Right, interpreted
   -- modulo 2**Width.
   --
   is
      use Arithmetic;
   begin

      case Op is

      when Eq =>
         -- Left = Right

         return
              "(exists " & Mod_Factor1 & ':'
            & Left
            & '-' & Modulus (Width) & Mod_Factor1
            & '='
            & Right
            & ')';

      when Lts =>
         -- Left < Right, signed.

         return Signed_Relation (Left, "<", Right, Width);

      when Gts =>
         -- Left > Right, signed.

         return Signed_Relation (Right, "<", Left, Width);

      when Les =>
         -- Left <= Right, signed.

         return Signed_Relation (Left, "<=", Right, Width);

      when Ges =>
         -- Left >= Right, signed.

         return Signed_Relation (Right, "<=", Left, Width);

      when Ltu =>
         -- Left < Right, unsigned.

         return Unsigned_Relation (Left, "<", Right, Modulus (Width));

      when Gtu =>
         -- Left > Right, unsigned.

         return Unsigned_Relation (Right, "<", Left, Modulus (Width));

      when Leu =>
         -- Left <= Right, unsigned.

         return Unsigned_Relation (Left, "<=", Right, Modulus (Width));

      when Geu =>
         -- Left >= Right, unsigned.

         return Unsigned_Relation (Right, "<=", Left, Modulus (Width));

      end case;

   end Modular_Relation;


   function Use_Modular_Model (
      Width : Arithmetic.Width_T;
      Expr  : Arithmetic.Expr_T)
   return Boolean
   --
   -- Whether a modular-arithmetic model should be used for
   -- comparisons of operands of this Width. If not, this function
   -- optionally emits a warning that non-modular comparison is used
   -- for this Expr.
   --
   is
      use type Arithmetic.Width_T;
   begin

      if Width <= Opt.Max_Modular_Width then
         -- Use modular (bounded-integer) formula.

         return True;

      else
         -- Use non-modular (unbounded-integer) formula.

         if Opt.Warn_Large_Width then

            Output.Warning (
                 "Non-modular comparison of"
               & Arithmetic.Width_T'Image (Width)
               & "-bit numbers"
               & Output.Field_Separator
               & Arithmetic.Image (Expr));

         end if;

         return False;

      end if;

   end Use_Modular_Model;


   type Calc_Imager_T is new Arithmetic.Expr_Imager_T with null record;
   --
   -- An imager object, to override some of default Arithmetic.Images
   -- for use in calculator formulae.
   --
   -- The overrides are the following:
   --
   -- >  Mulu and Muls are modelled only if one of the operands
   --                         is a constant.
   -- >  Comparisons are modelled using "mod 2**width" if the
   --                         width is not too large.
   -- >  EqZero (Item)        is modelled as Item = 0 (possibly modular).
   -- >  EqOne  (Item)        is modelled as Item = 1 (possibly modular).
   -- >  Exts   (Item)        is modelled as Item. TBC.
   -- >  Extz   (Item)        is modelled as Item.
   -- >  Slz    (Left, Right) is modelled as Left * 2**Right.
   -- >  Conc   (Left, Right) is modelled as Left*Width_Of(Right) + Right.
   -- >  Constants            are modelled as signed values when the
   --                         Signed flag is set.
   --
   -- TBA: Modular formulas for Exts,Extz, Slz, Conc.


   -- overriding
   function Const_Image (
      Item   : Arithmetic.Const_Expr_T;
      Imager : Calc_Imager_T)
   return String;


   -- overriding
   function Unary_Image (
      Item   : Arithmetic.Unary_Expr_T;
      Imager : Calc_Imager_T)
   return String;


   -- overriding
   function Binary_Op_Image (
      Item   : Arithmetic.Binary_Op_T;
      Imager : Calc_Imager_T)
   return String;


   -- overriding
   function Binary_Image (
      Item   : Arithmetic.Binary_Expr_T;
      Imager : Calc_Imager_T)
   return String;


   -- overriding
   function Ternary_Image (
      Item   : Arithmetic.Ternary_Expr_T;
      Imager : Calc_Imager_T)
   return String;


   -- Calc_Imager_T operation bodies:


   subtype One_Char_String is String(1..1);
   --
   -- A one-character string, for Eq_Value below.


   Eq_Value : constant array (
      Arithmetic.Unary_Op_T range Arithmetic.EqZero .. Arithmetic.EqOne)
      of One_Char_String := (
         Arithmetic.EqZero => "0",
         Arithmetic.EqOne  => "1");
   --
   -- The value to which the operand of EqZero/EqOne is compared,
   -- represented as a string for Image purposes.


   -- overriding
   function Const_Image (
      Item   : Arithmetic.Const_Expr_T;
      Imager : Calc_Imager_T)
   return String
   is
      use Arithmetic;
      use type Arithmetic.Width_T;
   begin

      if Item.Signed then

         return Image (Signed_Value (Item.Value, Item.Width));

      else

         return Image (Item.Value);

      end if;

   end Const_Image;


   -- overriding
   function Unary_Image (
      Item   : Arithmetic.Unary_Expr_T;
      Imager : Calc_Imager_T)
   return String
   is
      use Arithmetic;
      use type Arithmetic.Width_T;
   begin

      if Item.Unary in Eq_Value'Range then
         -- To be modelled as Item = Eq_Value(Item.Unary).

         if Use_Modular_Model (Item.Expr.Width, Item) then
            -- Modular formula.

            return Modular_Relation (
               Op    => Arithmetic.Eq,
               Left  => Image (Item.Expr, Calc_Imager_T'Class (Imager)),
               Right => Eq_Value(Item.Unary),
               Width => Item.Expr.Width);

         else
            -- Non-modular (unbounded-integer) formula.

            return
                  '('
               & Image (Item.Expr, Calc_Imager_T'Class (Imager))
               & "="
               & Eq_Value(Item.Unary)
               & ')';

         end if;

      elsif Item.Unary = Notw
      and   Item.Width = 1
      then
         -- To be modelled as 1 - Item.

         return "(1-" & Image (Item.Expr, Calc_Imager_T'Class (Imager)) & ')';

      elsif Item.Unary = Exts
      or    Item.Unary = Extz
      then
         -- To be modelled as the identity function.

         return Image (Item.Expr, Calc_Imager_T'Class (Imager));

         -- TBA modularity.

      else
         -- All other calculable operations are imaged normally.

         return Unary_Image (
            Item   => Item,
            Imager => Expr_Imager_T (Imager));

         -- TBA modularity for some?

      end if;

   end Unary_Image;


   -- overriding
   function Binary_Op_Image (
      Item   : Arithmetic.Binary_Op_T;
      Imager : Calc_Imager_T)
   return String
   is
      use Arithmetic;
   begin

      case Item is

      when Mulu | Muls => return "*" ;
      when Ltu         => return "<" ;
      when Gtu         => return ">" ;
      when Leu         => return "<=";
      when Geu         => return ">=";

      when others =>

         return Binary_Op_Image (Item, Expr_Imager_T (Imager));

      end case;

   end Binary_Op_Image;


   function Signed_Factor (
      Item   : Arithmetic.Expr_Ref;
      Imager : Arithmetic.Expr_Imager_T'Class)
   return String
   --
   -- The image of one operand (factor) in Muls. If it is constant
   -- we use a signed interpretation; otherwise the normal image.
   --
   is
      use Arithmetic;
      use type Value_T;

      Value : Value_T;
      -- The signed value, if constant.

   begin

      if Is_Constant (Item) then

         Value := Signed_Value (Item);

         if Value >= 0 then
            -- A positive value (a zero value will have been
            -- removed by earlier constant propagation).

            return Image (Value);

         else
            -- A negative value.

            return '(' & Image (Value) & ')';

         end if;

      else
         -- A non-constant factor.

         return Image (Item, Imager);

      end if;

   end Signed_Factor;


   -- overriding
   function Binary_Image (
      Item   : Arithmetic.Binary_Expr_T;
      Imager : Calc_Imager_T)
   return String
   is
      use Arithmetic;
      use type Arithmetic.Width_T, Arithmetic.Word_T;

      Width : Width_T;
      -- The width of the operands in a comparison.

   begin

      case Item.Binary is

      when Muls =>
         -- We know that (at least) one of the operands is a constant.
         -- We must give it a signed interpretation.

         return Signed_Factor (Item.L_Expr, Imager)
              & '*'
              & Signed_Factor (Item.R_Expr, Imager);

      when Relation_Op_T =>

         Width := Item.L_Expr.Width;

         if Use_Modular_Model (Width, Item) then
            -- Modular formula.

            return Modular_Relation (
               Op    => Item.Binary,
               Left  => Image (Item.L_Expr, Calc_Imager_T'Class (Imager)),
               Right => Image (Item.R_Expr, Calc_Imager_T'Class (Imager)),
               Width => Width);

         else
            -- Non-modular (unbounded-integer) formula.

            return Binary_Image (
               Item   => Item,
               Imager => Expr_Imager_T (Imager));

         end if;

      when Slz =>
         -- To be modelled as Left * 2**Right.
         -- Thanks to the checks in Is_Calculable we know that the Right
         -- sub-expression is a non-negative constant in a relatively
         -- limited range (0 .. Arithmetic.Opt.Max_Shift_Mul_Bits).

         if Item.R_Expr.Value = 0 then
            -- No need to multiply with 1.

            return Image (Item.L_Expr, Calc_Imager_T'Class (Imager));

         else

            return "(("
               &   Image (Item.L_Expr, Calc_Imager_T'Class (Imager))
               &   ")*"
               &   Image (Word_T'(2) ** Natural (Item.R_Expr.Value))
               &   ')';

         end if;

      when Conc =>
         -- To be modelled as Left * 2**Width_Of (Right) + Right.
         -- Thanks to the checks in Is_Calculable we know that the
         -- constant factor Width_Of (Right) is in a relatively
         -- limited range (0 .. Arithmetic.Opt.Max_Shift_Mul_Bits).

         return "(("
            &   Image (Item.L_Expr, Calc_Imager_T'Class (Imager))
            &   ")*"
            &   Image (Max_Word (Width_Of (Item.R_Expr)) + 1)
            &   "+"
            &   Image (Item.R_Expr, Calc_Imager_T'Class (Imager))
            &   ')';


      when others =>
         -- All other calculable operations are imaged normally.

         return Binary_Image (
            Item   => Item,
            Imager => Expr_Imager_T (Imager));

      end case;

   end Binary_Image;


   -- overriding
   function Ternary_Image (
      Item   : Arithmetic.Ternary_Expr_T;
      Imager : Calc_Imager_T)
   return String
   is
      use Arithmetic;

      Img : Calc_Imager_T'Class renames Calc_Imager_T'Class (Imager);

   begin

      case Item.Ternary is

      when Minus_With_B =>

         return '('
            & Image (Item.L3_Expr.all, Img)
            & '-'
            & Image (Item.R3_Expr.all, Img)
            & '-'
            & Image (Item.C3_Expr.all, Img)
            & ')';

      when Minus_With_C =>

         return '('
            & Image (Item.L3_Expr.all, Img)
            & '-'
            & Image (Item.R3_Expr.all, Img)
            & '+'
            & Image (Item.C3_Expr.all, Img)
            & "-1"
            & ')';

      when others =>

         return Ternary_Image (Item, Expr_Imager_T (Imager));

      end case;

   end Ternary_Image;


   Calc_Imager : Calc_Imager_T;
   --
   -- Our normal imager.


   function Image (Item : Arithmetic.Expr_Ref)
   return String
   --
   -- Our normal image.
   --
   is
   begin

      return Arithmetic.Image (Item, Calc_Imager_T'Class (Calc_Imager));

   end Image;


   function Null_Set (Cells : Tuple_T)
   return Set_T
   is
   begin

      return '{' & Cells & " : 0 = 1}";
      --
      -- The condition "0 = 1" is just a way of writing "False".

   end Null_Set;


   function Set (
      Cells : Tuple_T;
      Cond  : Arithmetic.Condition_T)
   return Set_T
   is
   begin

      if Is_Calculable (Cond) then

         return '{'
            & Cells & ':'
            & Image (Cond)
            & '}';

      else

         return '{' & Cells & '}';

      end if;

   end Set;


   function "or" (Left, Right : Formula_T) return String
   --
   -- The disjunction (logical inclusive "or"), Left or Right.
   -- Either Left or Right can be nil and the result is then
   -- the other one. If both Left and Right are nil the result
   -- is nil (a null string).
   --
   is
   begin

      if Not_Nil (Left) and Not_Nil (Right) then

         return To_String (Parens (Parens (Left) & " or " & Parens (Right)));

      else

         return To_String (Left) & To_String (Right);

      end if;

   end "or";


   procedure Conjoin (
      Conjunct : in     String;
      To       : in out Formula_T)
   --
   -- Adds (conjoins) a conjunct (logical expression) To the
   -- formula, using the "and" operator if the formula already
   -- contains something. Does nothing if the Conjunct is null.
   --
   is
   begin

      if Conjunct'Length > 0 then

         if Not_Nil (To) then

            Append (To, " and ");

         end if;

         Append (To, Conjunct);

      end if;

   end Conjoin;


   procedure Conjoin (
      Conjunct : in     Formula_T;
      To       : in out Formula_T)
   --
   -- Same as Conjoin (To_String (Conjunct, To)).
   --
   is
   begin

      Conjoin (Conjunct => To_String (Conjunct), To => To);

   end Conjoin;


   function Not_Image (Item : Arithmetic.Condition_T)
   return String
   --
   -- The negated condition.
   --
   is
   begin

      return "(not " & Image (Item) & ')';

   end Not_Image;


   procedure Conjoin (
      Conjunct : in     Arithmetic.Condition_T;
      Negated  : in     Boolean;
      To       : in out Formula_T)
   --
   -- Adds (conjoins) a conjunct (logical expression) To the
   -- formula, if the conjunct is calculable.
   -- The conjunct can optionally be negated in the formula.
   --
   is
   begin

      if Is_Calculable (Conjunct) then

         if Negated then

            Conjoin (
               Conjunct => Not_Image (Conjunct),
               To       => To);

         else

            Conjoin (
               Conjunct => Image (Conjunct),
               To       => To);

         end if;

      end if;

   end Conjoin;


   function Numeric_Image (Item : Boolean) return String
   --
   -- The image of the numeric equivalent of a Boolean/bit value.
   --
   is
   begin

      if Item then return "1";
              else return "0";
      end if;

   end Numeric_Image;


   procedure Conjoin_Bit (
      Item  : in     Arithmetic.Expr_Ref;
      Value : in     Boolean;
      To    : in out Formula_T)
   --
   -- Conjoins an Omega equality expression that constrains the Item
   -- to have the numeric equivalent of the given Boolean Value (zero
   -- for False, one for True), if the Item is calculable; otherwise
   -- returns To unchanged.
   --
   is
   begin

      if Is_Calculable (Item) then

         Conjoin (
            Conjunct => '(' & Image (Item)
                      & '=' & Numeric_Image (Value)
                      & ')',
            To => To);

      end if;

   end Conjoin_Bit;


   type Dash_Imager_T is new Calc_Imager_T with null record;
   --
   -- An expression imager that places a dash (') on all cells,
   -- to denote the new value of the cell in a calculator relation.


   -- overriding
   function Cell_Image (
      Item   : Arithmetic.Cell_Expr_T;
      Imager : Dash_Imager_T)
   return String;


   -- overriding
   function Cell_Image (
      Item   : Arithmetic.Cell_Expr_T;
      Imager : Dash_Imager_T)
   return String
   is
   begin

      return Storage.Name_Of (Item.Cell) & ''';

   end Cell_Image;


   Dash_Imager : Dash_Imager_T;
   --
   -- Our dashed imager.


   function Imager (Dashed : Boolean)
   return Calc_Imager_T'Class
   --
   -- A possibly Dashed imager.
   --
   is
   begin

      if Dashed then return Calc_Imager_T'Class (Dash_Imager);
                else return Calc_Imager_T'Class (Calc_Imager);
      end if;

   end Imager;


   procedure Conjoin (
      Item     : in     String;
      Relation : in     String;
      Bound    : in     String;
      To       : in out Formula_T)
   --
   -- Conjoins an Omega relation expression that constrains the Item
   -- by the given Relation with the Bound.
   --
   is
   begin

      Conjoin (
         Conjunct => '(' & Item & Relation & Bound & ')',
         To       => To);

   end Conjoin;


   procedure Conjoin (
      Item     : in     String;
      Relation : in     String;
      Bound    : in     Arithmetic.Expr_Ref;
      Dash     : in     Boolean := False;
      To       : in out Formula_T)
   --
   -- Conjoins an Omega relation expression that constrains the Item
   -- by the given Relation with the Bound expression, if the Bound is
   -- calculable; otherwise returns To unchanged.
   -- The cell names in Bound can optionally be Dashed so that they
   -- refer to the new (post) values rather than the old (pre) values.
   --
   is
   begin

      if Is_Calculable (Bound) then

         Conjoin (
            Item     => Item,
            Relation => Relation,
            Bound    => Arithmetic.Image (Bound, Imager (Dash)),
            To       => To);

      end if;

   end Conjoin;


   procedure Conjoin (
      Var   : in     String;
      Op    : in     String;
      Limit : in     Storage.Bounds.Limit_T;
      To    : in out Formula_T)
   --
   -- Adds the constraint "Var Op Limit" To the formula, if the
   -- limit is bounded.
   --
   is
   begin

      if Limit.Kind = Storage.Bounds.Finite then

         Conjoin (
            Conjunct => Var & Op & Arithmetic.Value_T'Image (Limit.Value),
            To       => To);

      end if;

   end Conjoin;


   procedure Conjoin (
      Var    : in     String;
      Within : in     Storage.Bounds.Interval_T;
      To     : in out Formula_T)
   --
   -- Adds the constraint "Var in Interval" To the formula.
   --
   is
   begin

      if Storage.Bounds.Singular (Within) then
         -- Only a single value Within this interval.

         Conjoin (
            Conjunct => Var & '=' & Arithmetic.Value_T'Image (Within.Min.Value),
            To       => To);
         --
         -- Could as well use Within.Max, they are equal.

      else

         Conjoin (Var, ">=", Within.Min, To);
         Conjoin (Var, "<=", Within.Max, To);

      end if;

   end Conjoin;


   function Interval (Within : Storage.Bounds.Interval_T)
   return Set_T
   is

      Var : constant String := "x";
      -- The name for the only variable. Any name will do.

      Constraints : Formula_T;
      -- The constraints from the interval.

      Result : Set_T;
      -- The result to be constructed, using Var and Constraints.

   begin  -- Set

      Conjoin (Var => Var, Within => Within, To => Constraints);

      if Not_Nil (Constraints) then
         -- Some constraints resulted, so we construct the Result.

         Result := "{[" & To_Formula (Var) & "]:";

         Append (Result, Constraints);

         Append (Result, '}');

      end if;

      return Result;

   end Interval;


   function Set (
      Cells  : Tuple_T;
      Bounds : Storage.Bounds.Cell_Interval_List_T)
   return Set_T
   is

      Result : Formula_T;
      -- The result to be constructed.

      Constraints : Formula_T;
      -- The constraints.

   begin

      Result := '{' & Cells;

      for B in Bounds'Range loop

         Conjoin (
            Var    => Storage.Name_Of (Bounds(B).Cell),
            Within => Bounds(B).Interval,
            To     => Constraints);

      end loop;

      if Not_Nil (Constraints) then
         -- Some constraints resulted.

         Append (Result, ':');
         Append (Result, Constraints);

      end if;

      Append (Result, '}');

      return Result;

   end Set;


   function Is_Mask (Operand : Arithmetic.Expr_Ref)
   return Boolean
   --
   -- Whether the given Operand is a constant of the form 2**(n-1) for
   -- some n.
   --
   -- False if the Operand is not a constant, is not of this form, or if
   -- the constant value is negative or out of range for Arithmetic.Word_T.
   --
   is
      use type Arithmetic.Word_T;

      Word : Arithmetic.Word_T;
      -- The Operand Value as a word.

   begin

      if Operand.Kind /= Arithmetic.Const then

         return False;

      else

         Word := Arithmetic.Word_T (Operand.Value);
         -- May raise constraint error.

         return Arithmetic.Mask (Word) = Word;

      end if;

   exception

      when Constraint_Error =>
         -- Probably from the conversion Value => Word.

         return False;

   end Is_Mask;


   procedure Conjoin_Unsigned (
      Item     : in     String;
      Relation : in     String;
      Bound    : in     Arithmetic.Expr_Ref;
      To       : in out Formula_T)
   --
   -- Conjoins To a formula the relation expression that constrains
   -- the Item by the given Relation with the Bound expression, where
   -- the Bound is interpreted as an unsigned value.
   --
   is
   begin

      -- TBM call Conjoin directly.

      Conjoin (
         Item     => Item,
         Relation => Relation,
         Bound    => Bound,
         To       => To);

   end Conjoin_Unsigned;


   procedure Conjoin_Conditional_Equality (
      Target : in     String;
      Cond1  : in     Formula_T;
      Cond2  : in     Formula_T;
      Value1 : in     Arithmetic.Expr_Ref;
      Value2 : in     Arithmetic.Expr_Ref;
      To     : in out Formula_T);
   --
   -- See description below.


   procedure Conjoin_Boolean (
      Target   : in     String;
      Yes_Cond : in     String;
      No_Cond  : in     String;
      To       : in out Formula_T)
   --
   -- Conjoins To a formula the Omega relation expressions that
   -- constrain the Target cell (already provided with a dash to show
   -- update, if needed) to be equal to the Boolean value of the
   -- Omega condition Yes_Cond. That is, Yes_Cond implies Target = 1.
   -- The Omega condition No_Cond is assumed to be the logical
   -- negation of Yes_Cond, or at least a condition that implies
   -- Target = 0. If Yes_Cond and No_Cond are not logical complements
   -- the Target is unconstrained when both Yes_Cond and No_Cond are false.
   --
   -- If Yes_Cond or No_Cond or both are null, those conditions are
   -- omitted from the result, but the constraint (1 or 0) on the
   -- Target remain.
   --
   is
   begin

      Conjoin_Conditional_Equality (
         Target => Target,
         Cond1  => To_Formula (Yes_Cond),
         Cond2  => To_Formula (No_Cond),
         Value1 => Arithmetic.One_Bit,
         Value2 => Arithmetic.Zero_Bit,
         To     => To);

   end Conjoin_Boolean;


   procedure Conjoin_Boolean_Rel (
      Target  : in     String;
      Operand : in     Arithmetic.Expr_Ref;
      Yes_Rel : in     String;
      No_Rel  : in     String;
      To      : in out Formula_T)
   --
   -- Conjoins To a formula the Omega relation expressions that
   -- constrain the Target cell (already provided with a dash to show
   -- update, if needed) to be equal to the Boolean value of the
   -- Omega relation "Operand Yes_Rel", where Operand is assumed
   -- to be an integer expression, for example "a+b", and Yes_Rel
   -- is the rest of the relation, for example ">0". In this example,
   -- Target is assigned the boolean value of the relation (a+b)>0,
   -- mapped to integers using true=>1, false=>0.
   -- The parameter No_Rel is assumed to be the logical negation
   -- of Yes_Rel, for example "<=0", or at least a relation that
   -- implies Target = 0. If Yes_Rel and No_Rel are not logical
   -- complements when applied to the same Operand, the Target is
   -- unconstrained when both Yes_Rel and No_Rel are false.
   --
   is
   begin

      if Is_Calculable (Operand) then
         -- With a calculable Operand we can define when
         -- the Target is 1 or 0, depending on Yes/No_Rel.

         declare

            Operand_Image : constant String :=
               Arithmetic.Image (Operand, Calc_Imager);

         begin

            Conjoin_Boolean (
               Target   => Target,
               Yes_Cond => Operand_Image & Yes_Rel,
               No_Cond  => Operand_Image & No_Rel,
               To       => To);

         end;

      else
         -- The Operand is not calculable, so we cannot say
         -- when the Target is 0 or 1, but we can say that
         -- it is one of the two.

         Conjoin_Boolean (
            Target   => Target,
            Yes_Cond => "",
            No_Cond  => "",
            To       => To);

      end if;

   end Conjoin_Boolean_Rel;


   procedure Conjoin_Bitwise_And (
      Target : in     String;
      Left   : in     Arithmetic.Expr_Ref;
      Right  : in     Arithmetic.Expr_Ref;
      To     : in out Formula_T)
   --
   -- Conjoins To a formula zero or more Omega relation expressions that
   -- constrain the Target cell (already provided with a dash to show update)
   -- to be equal to, or bounded by, the given bitwise "and" operation
   -- applied to the the given Left and Right operands. The result depends
   -- on the (equal) width of the operands.
   --
   is

      -- Principles of Operation:
      --
      -- There are three cases:
      -- 1. One-bit "and".
      -- 2. Masking multi-bit "ands".
      -- 3. Other multi-bit "ands".
      --
      -- One-bit "and" is translated like a conditional assignment of
      -- the form "Target := if Left = 1 then Right else 0".
      -- This is an exact translation. Note, however, that the roles
      -- of Left and Right are not quite symmetric: if Left = 1 the
      -- formula refines to the equality Target = Right, but if Right = 1
      -- it does not refine (as easily) to the equality Target = Left.
      --
      -- A multi-bit "and" is a masking operation if one operand is a
      -- constant of the form 2**(n-1). Such an "and" has the effect
      -- of choosing the "n" lowest bits of the other operand.
      --
      -- A masking operation like "T := A and M", where M is of the
      -- form 2**n - 1, is translated into the constraints:
      --
      --    0 <= T and ( (T = A and M >= A) or (T <= M and M < A) ).
      --
      -- Other multi-bit "and" operations like "T := A and B" where
      -- neither A nor B is a constant of the form 2**(n-1) are translated
      -- into the constraints:
      --
      --    0 <= T and T <= A and T <= B.
      --
      -- In the latter or non-masking form, if A or B is a negative
      -- constant it is translated into the corresponding unsigned
      -- value (reversing the two's complement interpretation).
      --
      -- The crucial reason for handling masking constraints separately
      -- is that some Ada compilers (e.g. XGC ERC32 Ada) use them when
      -- a loop is counted by an enumerated type, and the part "T = A"
      -- is necessary to bound such loops.

      use type Arithmetic.Width_T;

      Is_Masking : Boolean := False;
      -- Whether this is a masking operation.

      Mask : Arithmetic.Expr_Ref;
      -- The operand that is a mask value of the form 2**(n-1), if
      -- Is_Masking is True.

      Masked : Arithmetic.Expr_Ref;
      -- The operand that is masked (the other operand than Mask),
      -- if Is_Masking is True.

      Case1, Case2 : Formula_T;
      -- The two cases (disjuncts) in the translation for a one-bit
      -- operation or a masking operation.

   begin

      if Left.Width = 1 then
         -- One-bit "and".

         Conjoin_Bit (
            Item  => Left,
            Value => True,
            To    => Case1);

         Conjoin_Bit (
            Item  => Left,
            Value => False,
            To    => Case2);

         Conjoin_Conditional_Equality (
            Target => Target,
            Cond1  => Case1,
            Cond2  => Case2,
            Value1 => Right,
            Value2 => Arithmetic.Zero_Bit,
            To     => To);

      else
         -- Multi-bit "and".

         if Arithmetic.Opt.Bound_Bitwise_Ops then

            -- Check if this is a masking operation:

            if Is_Mask (Left) then

               Is_Masking := True;
               Mask       := Left;
               Masked     := Right;

            elsif Is_Mask (Right) then

               Is_Masking := True;
               Mask       := Right;
               Masked     := Left;

            end if;

            -- All translations start with 0 <= Target:

            Conjoin (
               Conjunct => "(0 <=" & Target & ')',
               To       => To);

            if Is_Masking then
               -- Translate the masking operation:

               -- The case where the masked value is <= Mask:

               Conjoin (
                  Item     => Target,
                  Relation => "=",
                  Bound    => Masked,
                  To       => Case1);

               Conjoin (
                  Item     => Arithmetic.Image (Mask),
                  Relation => ">=",
                  Bound    => Masked,
                  To       => Case1);

               -- The case where the masked value is > Mask:

               Conjoin (
                  Item     => Target,
                  Relation => "<=",
                  Bound    => Mask,
                  To       => Case2);

               Conjoin (
                  Item     => Arithmetic.Image (Mask),
                  Relation => "<",
                  Bound    => Masked,
                  To       => Case2);

               -- Combine the cases:

               Conjoin (
                  Conjunct => Case1 or Case2,
                  To       => To);

            else
               -- Not a masking operation.
               -- Translate it as such:

               Conjoin_Unsigned (
                  Item     => Target,
                  Relation => "<=",
                  Bound    => Left,
                  To       => To);

               Conjoin_Unsigned (
                  Item     => Target,
                  Relation => "<=",
                  Bound    => Right,
                  To       => To);

            end if;

         end if;

      end if;

   end Conjoin_Bitwise_And;


   procedure Conjoin_Bitwise_Or (
      Target : in     String;
      Left   : in     Arithmetic.Expr_Ref;
      Right  : in     Arithmetic.Expr_Ref;
      To     : in out Formula_T)
   --
   -- Conjoins To a formula zero or more Omega relation expressions that
   -- constrain the Target cell (already provided with a dash to show update)
   -- to be equal to, or bounded by, the given bitwise "or" operation
   -- applied to the the given Left and Right operands. The result depends on
   -- the (equal) width of the operands.
   --
   is

      -- Principles of Operation:
      --
      -- There are two cases:
      -- 1. One-bit "or".
      -- 3. Multi-bit "ors".
      --
      -- One-bit "or" is translated like a conditional assignment of
      -- the form "Target := if Left = 1 then 1 else Right".
      -- This is an exact translation. Note, however, that the roles
      -- of Left and Right are not quite symmetric: if Left = 0 the
      -- formula refines to the equality Target = Right, but if Right = 0
      -- it does not refine (as easily) to the equality Target = Left.
      --
      -- A multi-bit "or" operation like "T := A or B" is translated
      -- into the constraints
      --
      --    0 <= T and A <= T and B <= T and T <= A + B.

      use type Arithmetic.Width_T;

      Case1, Case2 : Formula_T;
      -- The two cases (disjuncts) in the translation for a one-bit
      -- operation.

   begin

      if Left.Width = 1 then
         -- One-bit "or".

         Conjoin_Bit (
            Item  => Left,
            Value => True,
            To    => Case1);

         Conjoin_Bit (
            Item  => Left,
            Value => False,
            To    => Case2);

         Conjoin_Conditional_Equality (
            Target => Target,
            Cond1  => Case1,
            Cond2  => Case2,
            Value1 => Arithmetic.One_Bit,
            Value2 => Right,
            To     => To);

      else
         -- Multi-bit "or".

         if Arithmetic.Opt.Bound_Bitwise_Ops then

            Conjoin (
               Conjunct => "(0 <=" & Target & ')',
               To       => To);

            Conjoin_Unsigned (
               Item     => Target,
               Relation => ">=",
               Bound    => Left,
               To       => To);

            Conjoin_Unsigned (
               Item     => Target,
               Relation => ">=",
               Bound    => Right,
               To       => To);

            Conjoin_Unsigned (
               Item     => Target,
               Relation => "<=",
               Bound    => Left + Right,
               To       => To);
            --
            -- TBM: The above expression for Bound creates
            -- junk Expr_T's on every evaluation, leaking
            -- memory.

         end if;

      end if;

   end Conjoin_Bitwise_Or;


   procedure Conjoin_Bitwise_Xor (
      Target : in     String;
      Left   : in     Arithmetic.Expr_Ref;
      Right  : in     Arithmetic.Expr_Ref;
      To     : in out Formula_T)
   --
   -- Conjoins To a formula zero or more Omega relation expressions that
   -- constrain the Target cell (already provided with a dash to show update)
   -- to be equal to, or bounded by, the given bitwise exclusive-or operation
   -- applied to the given Left and Right operands. The result depends on
   -- the (equal) width of the operands.
   --
   is

      -- Principles of Operation:
      --
      -- There are two case:
      -- 1. One-bit "xor".
      -- 2. Multi-bit "xor".
      --
      -- One-bit "xor" is translated like a conditional assignment of
      -- the form "Target := if Left = Right then 0 else 1", assuming
      -- that Left and Right are calculable. This is an exact translation.
      -- If either Left or Right is not calculable the translation is the
      -- constraint that Target is in 0 .. 1.
      --
      -- Multi-bit "xor" operations are sometimes used to compare two
      -- operands for equality, the result being zero if and only if
      -- the operands are equal. Therefore, we translate the operation
      -- as a conditional assignment of the form
      -- "Target := if Left = Right then 1 else <some unknown nonzero value>"
      -- assuming that Left and Right are calculable; otherwise Target is
      -- left unconstrained.

      use type Arithmetic.Width_T;

      Calculable : constant Boolean :=
         Is_Calculable (Left) and Is_Calculable (Right);
      -- Whether both operands are calculable.

      Left_Im  : constant String := Arithmetic.Image (Left);
      Right_Im : constant String := Arithmetic.Image (Right);
      -- Hoping that they are calculable.

      Equal   : constant String := '(' & Left_Im &  '=' & Right_Im & ')';
      Unequal : constant String := '(' & Left_Im & "!=" & Right_Im & ')';
      -- The two cases, Left = Right and Left /= Right.

      Case1, Case2 : Formula_T;
      -- The two cases (disjuncts).

   begin

      if Left.Width = 1 then
         -- One-bit "xor".

         if Calculable then

            Conjoin (
               Conjunct => Equal,
               To       => Case1);

            Conjoin (
               Conjunct => Unequal,
               To       => Case2);

            Conjoin_Boolean (
               Target   => Target,
               Yes_Cond => To_String (Case1),
               No_Cond  => To_String (Case2),
               To       => To);

         else
            -- One or both operands are incalculable. Make do
            -- with a range constraint.

            Conjoin (
               Var    => Target,
               Within => Storage.Bounds.Interval (Min => 0, Max => 1),
               To     => To);

         end if;

      else
         -- Multi-bit "xor".

         if Calculable then

            -- When Left = Right:

            Conjoin (
               Conjunct => Equal,
               To       => Case1);

            Conjoin (
               Conjunct => Target & "=0",
               To       => Case1);

            -- When Left /= Right:

            Conjoin (
               Conjunct => Unequal,
               To       => Case2);

            Conjoin (
               Conjunct => Target & "!=0",
               To       => Case2);

            -- One or the other:

            Conjoin (
               Conjunct => Case1 or Case2,
               To       => To);

         end if;

      end if;

   end Conjoin_Bitwise_Xor;


   procedure Conjoin_Borrow_From_Minus (
      Target : in     String;
      Left   : in     Arithmetic.Expr_Ref;
      Right  : in     Arithmetic.Expr_Ref;
      Borrow : in     String;
      To     : in out Formula_T)
   --
   -- Conjoins To a formula the Omega relation expressions that
   -- constrain the Target cell (already provided with a dash to show
   -- show update) to equal the borrow from the subtraction
   -- Left - (Right & Borrow), where the "&" means that the
   -- expression to be subtracted is formed by appending Borrow
   -- to the Right expression.
   --
   -- Precondition: All subexpressions are calculable.
   --
   is

      L : constant String := Arithmetic.Image (Left  , Calc_Imager);
      R : constant String := Arithmetic.Image (Right , Calc_Imager)
                           & Borrow;

   begin

      -- Borrow from L - (R & B) is modelled as the Boolean L < R & B.
      -- This is a heuristic approximation and not exact for overflows.

      Conjoin_Boolean (
         Target   => Target,
         Yes_Cond => L & "<"  & R,
         No_Cond  => L & ">=" & R,
         To       => To);

   end Conjoin_Borrow_From_Minus;


   procedure Conjoin_Carry_From_Minus (
      Target : in     String;
      Left   : in     Arithmetic.Expr_Ref;
      Right  : in     Arithmetic.Expr_Ref;
      Carry  : in     String;
      To     : in out Formula_T)
   --
   -- Conjoins To a formula the Omega relation expressions that
   -- constrain the Target cell (already provided with a dash to show
   -- show update) to equal the carry from the subtraction
   -- Left - (Right & Carry), where the "&" means that the
   -- expression to be subtracted is formed by appending Carry
   -- to the Right expression.
   --
   -- Precondition: All subexpressions are calculable.
   --
   is

      L : constant String := Arithmetic.Image (Left  , Calc_Imager);
      R : constant String := Arithmetic.Image (Right , Calc_Imager)
                           & Carry;

   begin

      -- Carry from L - (R & B) is modelled as the Boolean L >= R & B.
      -- This is a heuristic approximation and not exact for overflows.

      Conjoin_Boolean (
         Target   => Target,
         Yes_Cond => L & ">=" & R,
         No_Cond  => L & "<"  & R,
         To       => To);

   end Conjoin_Carry_From_Minus;


   procedure Conjoin_Negative_From_Minus (
      Target : in     String;
      Left   : in     Arithmetic.Expr_Ref;
      Right  : in     Arithmetic.Expr_Ref;
      Carry  : in     String;
      To     : in out Formula_T)
   --
   -- Conjoins To a formula the Omega relation expressions that
   -- constrain the Target cell (already provided with a dash to show
   -- show update) to equal the negative sign-bit from the subtraction
   -- Left - (Right & Carry), where the "&" means that the
   -- expression to be subtracted is formed by appending Carry
   -- to the Right expression.
   --
   -- Precondition: All subexpressions are calculable.
   --
   is

      L : constant String := Arithmetic.Image (Left  , Calc_Imager);
      R : constant String := Arithmetic.Image (Right , Calc_Imager)
                           & Carry;

   begin

      -- Negative from L - (R & B) is modelled as the Boolean L < R & B.
      -- This is a heuristic approximation and not exact for overflows.

      Conjoin_Boolean (
         Target   => Target,
         Yes_Cond => L & "<" & R,
         No_Cond  => L & ">="  & R,
         To       => To);

   end Conjoin_Negative_From_Minus;


   procedure Conjoin_Relation (
      Target  : in     String;
      Expr    : in     Arithmetic.Expr_Ref;
      Yes_Rel : in     String;
      No_Rel  : in     String;
      To      : in out Formula_T)
   --
   -- Conjoins To a formula the Omega relation expressions that
   -- constrain the 1-bit Target cell (already provided with a dash to
   -- show update) to equal "Expr.L_Expr Yes_Rel Expr.R_Expr", assuming
   -- that No_Rel is the opposite relation to Yes_Rel, and that
   -- Expr.L_Expr and Expr.R_Expr are calculable.
   --
   -- If one or both of Expr.L_Expr and Expr.R_Expr is not calculable,
   -- the result constrains the Target to be 0 or 1, non-deterministically
   -- and independently of Expr.
   --
   -- Precondition: Expr.Kind = Binary_Kind.
   --
   is
   begin

      if       Is_Calculable (Expr.L_Expr)
      and then Is_Calculable (Expr.R_Expr)
      then

         declare

            L : constant String := Arithmetic.Image (Expr.L_Expr, Calc_Imager);
            R : constant String := Arithmetic.Image (Expr.R_Expr, Calc_Imager);

         begin

            Conjoin_Boolean (
               Target   => Target,
               Yes_Cond => L & Yes_Rel & R,
               No_Cond  => L & No_Rel  & R,
               To       => To);

         end;

      else

         Conjoin_Boolean (
           Target   => Target,
           Yes_Cond => "",
           No_Cond  => "",
           To       => To);

      end if;

   end Conjoin_Relation;


   procedure Conjoin_Assignment (
      Target : in     String;
      Expr   : in     Arithmetic.Expr_Ref;
      To     : in out Formula_T)
   --
   -- Conjoins To a formula zero or more Omega relation expressions that
   -- constrain the Target cell (already provided with a dash to show update)
   -- to be equal to, or bounded by, the Expression that is assigned to it.
   --
   -- For some Expressions (e.g. Unknown) no constraint can be created
   -- and then the To parameter is returned unchanged.
   --
   -- For some Expressions (e.g. bitwise "and", "or") more than one
   -- constraint may be created, since the assigned value is not uniquely
   -- expressible by arithmetic but is boundable.
   --
   is
   begin

      if Expr /= Arithmetic.Unknown then

         case Expr.Kind is

         when Arithmetic.Unary_Kind =>

            case Expr.Unary is

            when Arithmetic.EqZero
               | Arithmetic.EqOne =>

               Conjoin_Boolean_Rel (
                  Target  => Target,
                  Operand => Expr.Expr,
                  Yes_Rel => "="  & Eq_Value(Expr.Unary),
                  No_Rel  => "!=" & Eq_Value(Expr.Unary),
                  To      => To);

            when Arithmetic.Signw =>

               Conjoin_Boolean_Rel (
                  Target  => Target,
                  Operand => Expr.Expr,
                  Yes_Rel => "<0",
                  No_Rel  => ">=0",
                  To      => To);

            when others =>
               -- Nothing special to do.

               Conjoin (
                  Item     => Target,
                  Relation => "=",
                  Bound    => Expr,
                  To       => To);

            end case;

         when Arithmetic.Binary_Kind =>

            case Expr.Binary is

            when Arithmetic.Minus_B =>

               if Is_Calculable (Expr) then

                  Conjoin_Borrow_From_Minus (
                     Target => Target,
                     Left   => Expr.L_Expr,
                     Right  => Expr.R_Expr,
                     Borrow => "",
                     To     => To);

               end if;

            when Arithmetic.Minus_C =>

               if Is_Calculable (Expr) then

                  Conjoin_Carry_From_Minus (
                     Target => Target,
                     Left   => Expr.L_Expr,
                     Right  => Expr.R_Expr,
                     Carry  => "",
                     To     => To);

               end if;

            when Arithmetic.Minus_N =>

               if Is_Calculable (Expr) then

                  Conjoin_Negative_From_Minus (
                     Target => Target,
                     Left   => Expr.L_Expr,
                     Right  => Expr.R_Expr,
                     Carry  => "",
                     To     => To);

               end if;

            -- TBA Plus_C/N/V, Minus_V.

            when Arithmetic.Eq =>

               Conjoin_Relation (
                  Target  => Target,
                  Expr    => Expr,
                  Yes_Rel => "=",
                  No_Rel  => "!=",
                  To      => To);

            when Arithmetic.Lts
               | Arithmetic.Ltu =>

               Conjoin_Relation (
                  Target  => Target,
                  Expr    => Expr,
                  Yes_Rel => "<",
                  No_Rel  => ">=",
                  To      => To);

            when Arithmetic.Gts
               | Arithmetic.Gtu =>

               Conjoin_Relation (
                  Target  => Target,
                  Expr    => Expr,
                  Yes_Rel => ">",
                  No_Rel  => "<=",
                  To      => To);

            when Arithmetic.Les
               | Arithmetic.Leu =>

               Conjoin_Relation (
                  Target  => Target,
                  Expr    => Expr,
                  Yes_Rel => "<=",
                  No_Rel  => ">",
                  To      => To);

            when Arithmetic.Ges
               | Arithmetic.Geu =>

               Conjoin_Relation (
                  Target  => Target,
                  Expr    => Expr,
                  Yes_Rel => ">=",
                  No_Rel  => "<",
                  To      => To);

            when Arithmetic.Andw =>

               Conjoin_Bitwise_And (
                  Target => Target,
                  Left   => Expr.L_Expr,
                  Right  => Expr.R_Expr,
                  To     => To);

            when Arithmetic.Orw =>

               Conjoin_Bitwise_Or (
                  Target => Target,
                  Left   => Expr.L_Expr,
                  Right  => Expr.R_Expr,
                  To     => To);

            when Arithmetic.Xorw =>

               Conjoin_Bitwise_Xor (
                  Target => Target,
                  Left   => Expr.L_Expr,
                  Right  => Expr.R_Expr,
                  To     => To);

            when others =>

               Conjoin (
                  Item     => Target,
                  Relation => "=",
                  Bound    => Expr,
                  To       => To);

            end case;

         when Arithmetic.Ternary_Kind =>

            case Expr.Ternary is

            when Arithmetic.Minus_B =>

               if Is_Calculable (Expr) then

                  Conjoin_Borrow_From_Minus (
                     Target => Target,
                     Left   => Expr.L3_Expr,
                     Right  => Expr.R3_Expr,
                     Borrow => '+'
                             & Arithmetic.Image (Expr.C3_Expr, Calc_Imager),
                     To     => To);

               end if;

            when Arithmetic.Minus_C =>

               if Is_Calculable (Expr) then

                  Conjoin_Carry_From_Minus (
                     Target => Target,
                     Left   => Expr.L3_Expr,
                     Right  => Expr.R3_Expr,
                     Carry  => "+1-"
                             & Arithmetic.Image (Expr.C3_Expr, Calc_Imager),
                     To     => To);

               end if;

            when Arithmetic.Minus_BN =>

               if Is_Calculable (Expr) then

                  Conjoin_Negative_From_Minus (
                     Target => Target,
                     Left   => Expr.L3_Expr,
                     Right  => Expr.R3_Expr,
                     Carry  => '+'
                             & Arithmetic.Image (Expr.C3_Expr, Calc_Imager),
                     To     => To);

               end if;

            when Arithmetic.Minus_CN =>

               if Is_Calculable (Expr) then

                  Conjoin_Negative_From_Minus (
                     Target => Target,
                     Left   => Expr.L3_Expr,
                     Right  => Expr.R3_Expr,
                     Carry  => "+1-"
                             & Arithmetic.Image (Expr.C3_Expr, Calc_Imager),
                     To     => To);

               end if;

            -- TBA Plus_C/N/V, Minus_V.

            when others =>

               Conjoin (
                  Item     => Target,
                  Relation => "=",
                  Bound    => Expr,
                  To       => To);

            end case;

         when Arithmetic.Const
            | Arithmetic.Cell
            | Arithmetic.Ref
            =>

            Conjoin (
               Item     => Target,
               Relation => "=",
               Bound    => Expr,
               To       => To);

        when Arithmetic.Opaque =>
           -- No known constraints on Target.

           null;

        end case;

      end if;

   end Conjoin_Assignment;


   procedure Conjoin_Range_Constraint (
      Target : in     String;
      Ass    : in     Arithmetic.Assignment_T;
      To     : in out Formula_T)
   --
   -- Conjoins To a formula the Omega relation expressions that
   -- constrain the Target cell by and of a given range constraint
   -- Assignment.
   --
   is

      Dash : constant Boolean := Ass.Kind = Arithmetic.Range_Post;
      -- Whether the cells in the Min and Max expressions refer
      -- to the new (post) values rather than the old (pre) values.

   begin

      if Ass.Min /= Ass.Max then
         -- Min and Max bounds may be different, so we conjoin them
         -- separately as inequalities.

         Conjoin (
            Item     => Target,
            Relation => ">=",
            Bound    => Ass.Min,
            Dash     => Dash,
            To       => To);

         Conjoin (
           Item     => Target,
           Relation => "<=",
           Bound    => Ass.Max,
           Dash     => Dash,
           To       => To);

      else
         -- Min and Max are the same expression so they must have
         -- the same value. This is a single equality constraint.

         Conjoin (
            Item     => Target,
            Relation => "=",
            Bound    => Ass.Min,  -- = Max.
            Dash     => Dash,
            To       => To);

      end if;

   end Conjoin_Range_Constraint;


   function Set (
      Cells : Tuple_T;
      Pre   : Arithmetic.Effect_T)
   return Set_T
   is

      Result : Formula_T;
      -- The result to be constructed.

      Constraints : Formula_T;
      -- The constraints.

      Ass : Arithmetic.Assignment_T;
      -- One assignment in Pre.

   begin

      -- The tuple:

      Result := '{' & Cells;

      -- The constraints if any:

      for P in Pre'Range loop

         Ass := Pre(P);

         if Ass.Kind = Arithmetic.Range_Pre
         and then Ass.Target.Kind = Arithmetic.Cell
         then

            Conjoin_Range_Constraint (
               Target => Storage.Name_Of (Ass.Target.Cell),
               Ass    => Ass,
               To     => Constraints);

         end if;

      end loop;

      -- Combine tuple and constraints:

      if Not_Nil (Constraints) then
         -- Some constraints resulted.

         Append (Result, ':');
         Append (Result, Constraints);

      end if;

      Append (Result, '}');

      return Result;

   end Set;


   function Singleton (Value : Arithmetic.Value_T)
   return Set_T
   is
   begin

      return Singleton (Values => (1=> Value));

   end Singleton;


   function Singleton (Values : Storage.Bounds.Value_List_T)
   return Set_T
   is

      Tuple : Formula_T;
      -- The single tuple of values.

   begin

      for V in Values'Range loop

         Append (Tuple, Arithmetic.Value_T'Image (Values(V)));

         if V < Values'Last then

            Append (Tuple, ',');

         end if;

      end loop;

      return "{[" & Tuple & "]}";

   end Singleton;


   procedure Conjoin_Conditional_Equality (
      Target : in     String;
      Cond1  : in     Formula_T;
      Cond2  : in     Formula_T;
      Value1 : in     Arithmetic.Expr_Ref;
      Value2 : in     Arithmetic.Expr_Ref;
      To     : in out Formula_T)
   --
   -- Conjoins To a formula the Omega relation expressions that constrain
   -- the (statically addressed) Target variable to model the conditional
   -- constraints:
   --   > if Cond1 then Target = Value1, and
   --   > if Cond2 then Target = Value2.
   -- with the Target value left unconstrained when neither Cond1 nor
   -- Cond2 holds.
   --
   -- The result is usually a disjunct of the form
   --
   --     (Target = Value1 and Cond1) or (Target = Value2 and Cond2)
   --
   -- but there are special cases for unknown (incalculable) values
   -- and conditions.
   --
   -- Cond1 or Cond2 are Nil if the respective conditions are not
   -- calculable. In this case the conjunction with the Nil Cond is
   -- omitted but the possible Target = Value constraint remains.
   --
   -- If Value1 or Value2 is not calculable then the corresponding
   -- disjunct (case) is omitted together with its Cond.
   --
   is

      Case1, Case2 : Formula_T;
      -- The constraints from the two possible assignments
      -- Target := Value1 and Target := Value2.

   begin

      -- Note that constraints can result from a conditional
      -- assignment even if the condition (Cond) is not
      -- calculable, because some constraints may hold for
      -- either value of the condition.

      -- Case Cond1, Target := Value1:

      Conjoin_Assignment (
         Target => Target,
         Expr   => Value1,
         To     => Case1);

      if Not_Nil (Case1) then
         -- Value1 is calculable.

         Conjoin (
            Conjunct => Cond1,
            To       => Case1);

      end if;

      -- Case Cond2, Target := Value2:

      Conjoin_Assignment (
         Target => Target,
         Expr   => Value2,
         To     => Case2);

      if Not_Nil (Case2) then
         -- Value2 is calculable.

         Conjoin (
            Conjunct => Cond2,
            To       => Case2);

      end if;

      -- Combine the cases:

      Conjoin (
         Conjunct => Case1 or Case2,
         To       => To);

   end Conjoin_Conditional_Equality;


   procedure Conjoin_Assignment (
      Ass : in     Arithmetic.Assignment_T;
      To  : in out Formula_T)
   --
   -- Conjoins To a formula the Omega relation expressions that constrain
   -- the (statically addressed) target cell of the given assignment.
   -- The result is usually of the form "target' = expression"
   -- but there are special cases for an unknown new value
   -- and a conditional assignment.
   --
   -- For an unknown new value, no relation expression is added.
   -- We assume that the fact that the target cell is updated in
   -- this step is already shown by the apostrophe (dash) in the
   -- range tuple of the Omega relation built for this step.
   --
   -- Precondition: Ass.Target.Kind = Cell (not Ref).
   --
   is

      Target : constant String :=
         Name  (
            Cell => Ass.Target.Cell,
            Dash => Ass.Kind /= Arithmetic.Range_Pre);
      -- The target cell, perhaps with a dash to show update.

      Cond1, Cond2 : Formula_T;
      -- The conditions for a conditional assignment.

   begin

      case Ass.Kind is

      when Arithmetic.Regular =>

         Conjoin_Assignment (
            Target => Target,
            Expr   => Ass.Value,
            To     => To);

      when Arithmetic.Conditional =>

         Conjoin (
            Conjunct => Ass.Cond,
            Negated  => False,
            To       => Cond1);

         Conjoin (
            Conjunct => Ass.Cond,
            Negated  => True,
            To       => Cond2);

         Conjoin_Conditional_Equality (
            Target => Target,
            Cond1  => Cond1,
            Cond2  => Cond2,
            Value1 => Ass.Value1,
            Value2 => Ass.Value2,
            To     => To);

      when Arithmetic.Fracture =>

         Conjoin_Assignment (
            Target => Target,
            Expr   => Arithmetic.Unknown,
            To     => To);

      when Arithmetic.Range_Kind_T =>

         Conjoin_Range_Constraint (
            Target => Target,
            Ass    => Ass,
            To     => To);

      end case;

   end Conjoin_Assignment;


   function Constraints (Eff : Arithmetic.Effect_T) return String
   --
   -- Returns the Omega constraints that represent the
   -- assignments in the given effect, as part of an Omega
   -- relation expression.
   --
   -- Asignments to volatile target cells create no constraints.
   --
   is

      Conses : Formula_T;
      -- Accumulates the constraints for each assignment (with a
      -- target cell that is not volatile) joined by "and" operators.

   begin

      for I in Eff'Range loop

         if Eff(I).Target.Kind = Arithmetic.Cell then

            if not Storage.Is_Volatile (Eff(I).Target.Cell) then

               Conjoin_Assignment (
                  Ass => Eff(I),
                  To  => Conses);

            end if;

         -- else
         --    TBA handle Target.Kind = Ref, dynamically addressed Target.

         end if;

      end loop;

      return To_String (Conses);

   end Constraints;


   function Identity_Relation (Domain : Tuple_T)
   return Relation_T
   is
   begin
      return Relation (Domain => Domain, Rainge => Domain);
   end Identity_Relation;


   function Identity_Relation (Domain : Cell_Set_T)
   return Relation_T
   is
      Tup : constant Tuple_T := Tuple (Domain);
   begin
      return Relation (Domain => Tup, Rainge => Tup);
   end Identity_Relation;


   function Null_Relation (Domain : Tuple_T)
   return Relation_T
   is
   begin

      return '{' & Domain & "->" & Domain & " : 0 = 1}";
      --
      -- The condition "0 = 1" is just a way of writing "False".

   end Null_Relation;


   function Relation (
      Domain : Tuple_T;
      Rainge : Tuple_T)
   return Relation_T
   is
   begin

      return '{' & Domain & "->" & Rainge & '}';

   end Relation;


   function Relation (
      Domain : Tuple_T;
      Rainge : Tuple_T;
      Where  : Formula_T)
   return Relation_T
   is
   begin

      return '{' & Domain & "->" & Rainge & ':' & Where & '}';

   end Relation;


   function Warp (
      From : Cell_Set_T;
      To   : Cell_Set_T)
   return Relation_T
   is
   begin

      if From = To then

         return Nil;

      else

         return Relation (
            Domain => Tuple (From),
            Rainge => Tuple (To  ));

      end if;

   end Warp;


   function Transformation (
      Domain : Cell_Set_T;
      Effect : Arithmetic.Effect_T)
   return Relation_T
   is

      Map : Formula_T;
      -- The part [domain vars] -> [range vars].

      Conses : constant String := Constraints (Effect);
      -- The constraints on range vars induced by the Effect.

   begin

      if Effect'Length = 0 then
         -- There are no assignments.

         return Nil;

      else
         -- There are assignments, which either constrain some
         -- target cells or leave them unknown, opaque.

         Map :=
              Tuple (Domain)
            & "->"
            & Tuple (Cells => Domain, Dashed => Effect);

         if Conses'Length > 0 then
            -- There are some constraints.

            return '{' & Map & ':' & Conses & '}';

         else
            -- The updated cells are unconstrained, and this is
            -- already indicated by the dashes in Map. No further
            -- constraints need be added:

            return '{' & Map & '}';

         end if;

      end if;

   end Transformation;


   function Projection (
      Domain : Cell_Set_T;
      To     : Storage.Cell_T)
   return Relation_T
   is
      use type Arithmetic.Width_T;

      Width : constant Arithmetic.Width_T := Storage.Width_Of (To);
      -- The width of the target cell.

      Var : constant String := "v'";
      -- The formal variable that represents "To mod 2**Width" for
      -- the purpose of expressing the constraints on this value.
      --
      -- The dash (') serves to separate this variable identifier
      -- from the otherwise synonymous identifier that may be in
      -- the domain tuple.

   begin

      if Width <= Opt.Max_Modular_Width then
         -- Use modular (bounded-integer) projection.

         return Relation (
            Domain => Tuple (Domain),
            Rainge => Tuple (Var),
            Where  => To_Formula (
                 "(exists " & Mod_Factor1 & ':'
               & Var & '='
               & Storage.Name_Of (To) & '-' & Modulus (Width) & Mod_Factor1
               & " and 0 <=" & Var & '<' & Modulus (Width)
               & ')'));

      else
         -- Use non-modular (unbounded-integer) formula.

         if Opt.Warn_Large_Width then

            Output.Warning (
                 "Non-modular bounds of"
               & Arithmetic.Width_T'Image (Width)
               & "-bit cell "
               & Storage.Image (To));

         end if;

         return Relation (
            Domain => Tuple (Domain),
            Rainge => Tuple (To));

      end if;

   end Projection;


   function Mapping (
      Domain : Cell_Set_T;
      Value  : Arithmetic.Expr_Ref)
   return Relation_T
   is

      Var : constant String := "v'";
      -- The formal variable that is considered to be the
      -- target of the assignment "Var := Value", for the purpose
      -- of expressing the constraints on the value of Value.
      -- There may be zero, one or several constraints and they
      -- may be inequalities as well as equalities.
      --
      -- The dash (') serves to separate this variable identifier
      -- from the otherwise synonymous identifier that may be in
      -- the domain tuple.

      Prefix : Relation_T :=
         '{' & Tuple (Domain) & "->[" & Var & ']';
      --
      -- The initial part of the result, establishing the domain
      -- tuple and the formal range variable.

      Constraints : Formula_T;
      -- The constraints on the value Var of Value.

   begin

      Conjoin_Assignment (
         Target => Var,
         Expr   => Value,
         To     => Constraints);

      if Not_Nil (Constraints) then
         -- The Value is subject to some constraints.
         -- Include them in the result relation:

         Append (Prefix, ':' & Constraints);

      end if;

      return Prefix & '}';

      -- TBA modularity

   end Mapping;


   function First_Of_Two return Relation_T
   is
   begin
      return To_Formula ("{[a,b]->[a]}");
   end First_Of_Two;


   function Second_Of_Two return Relation_T
   is
   begin
      return To_Formula ("{[a,b]->[b]}");
   end Second_Of_Two;


   function Sum_Of_Two return Relation_T
   is
   begin
      return To_Formula ("{[a,b]->[a+b]}");
   end Sum_Of_Two;


   function Union (Left, Right : Set_T) return Set_T
   is
   begin

      if    Is_Nil (Left ) then return Right;

      elsif Is_Nil (Right) then return Left;

      else

         return Parens (Left & " union " & Right);

      end if;

   end Union;


   function Intersection (Left, Right : Set_T) return Set_T
   is
   begin

      if    Is_Nil (Left ) then return Right;

      elsif Is_Nil (Right) then return Left;

      else

         return Parens (Left & " intersection " & Right);

      end if;

   end Intersection;


   function Difference (Left, Right : Set_T) return Set_T
   is
   begin

      if Is_Nil (Left ) or Is_Nil (Right) then

         return Left;

      else

         return Parens (Left & " - " & Right);

      end if;

   end Difference;


   function Join (Left, Right : Relation_T) return Relation_T
   is
   begin
      if    Is_Nil (Left ) then return Right;
      elsif Is_Nil (Right) then return Left;
      else
         return Parens (Left & " . " & Right);
      end if;
   end Join;


   function Transitive_Closure (Item : Relation_T)
   return Relation_T
   is
   begin
      return '(' & Item & "+)";
   end Transitive_Closure;


   function Domain (Item : Relation_T) return Set_T
   is
   begin
      return "domain(" & Item & ')';
   end Domain;


   function Rainge (Item : Relation_T) return Set_T
   is
   begin
      return "range(" & Item & ')';
   end Rainge;


   function Complement (Item : Set_T) return Set_T
   is
   begin
      return "complement(" & Item & ')';
   end Complement;


   function Apply (
      Relation : Relation_T;
      To_Set   : Set_T)
   return Set_T
   is
   begin

      if Is_Nil (Relation) then

         return To_Set;

      else

         return Parens (Relation & '(' & To_Set & ')');

      end if;

   end Apply;


   function Restrict_Range (
      Relation : Relation_T;
      Subrange : Set_T)
   return Relation_T
   is
   begin

      return Parens (Relation & '/' & Subrange);

   end Restrict_Range;


   function Restrict_Domain
     (Relation  : Relation_T;
      Subdomain : Set_T)
      return Relation_T
   is
   begin

      return Parens (Relation & '\' & Subdomain);

   end Restrict_Domain;


   function Adapted (
      Pool : Pool_T'Class;
      To   : Cell_Set_T)
   return Set_T
   is
   begin

      return Apply (
         Relation =>
            Warp (
               From => Pool.Cells,
               To   => To),
            To_Set =>
               Id (Pool));

   end Adapted;


   function Adapted (
      Flux : Flux_T;
      To   : Cell_Set_T)
   return Relation_T
   is

      Fx_Tuple : Tuple_T;
      -- The tuple for Flux.Cells.

      To_Tuple : Tuple_T;
      -- The tuple for To.

      Wrapped : Relation_T;
      -- The Flux relation, wrapped between projections in the
      -- form To -> Flux.Cells -> To.

      Bypass : Relation_T;
      -- A relation that keeps To - Flux.Cells invariant (identity
      -- relation) but allows the cells in Flux.Cells to vary freely.


   begin

      if Storage.Mixed."=" (Flux.Cells, To) then
         -- No adaptation necessary.

         return Id (Flux);

      else
         -- Adaptation is necessary.

         To_Tuple := Tuple (To);
         Fx_Tuple := Tuple (Flux.Cells);

         Wrapped :=
            Join (Join (
               Relation (Domain => To_Tuple, Rainge => Fx_Tuple),
               Id (Flux)),
               Relation (Domain => Fx_Tuple, Rainge => To_Tuple));

         Bypass :=
            Relation (
               Domain => To_Tuple,
               Rainge => Tuple (
                  Cells         => To,
                  Dashed        => Flux.Cells,
                  Dash_Volatile => False));

         return Intersection (Wrapped, Bypass);

      end if;

   end Adapted;


   --
   --    Induction variables and iteration counters
   --


   Iter_Count : String renames
      Storage.Reserved_Names.Name(Storage.Reserved_Names.Iter_Count).all;
   --
   -- Identifies the joint iteration counter.


   function Counter_Tuple return Tuple_T
   is
   begin

      return To_Formula ('[' & Iter_Count & ']');

   end Counter_Tuple;


   function Tuple_With_Counter (Cells : Cell_Set_T)
   return Tuple_T
   is

      Vars : Formula_T := Variables (Cells);
      -- The Cells as a variable list.

   begin

      if Length (Vars)= 0 then
         -- There are no Cells.

         return Counter_Tuple;

      else

         return To_Formula ("[")
              & Vars
              & "," & Iter_Count
              & ']';

      end if;

   end Tuple_With_Counter;


   procedure Conjoin_Induct (
      Var   : in     String;
      Op    : in     String;
      Limit : in     Storage.Bounds.Limit_T;
      To    : in out Formula_T)
   --
   -- Adds the constraint "(Var'-Var) Op (Limit Iter_Count)" To
   -- the formula, if the limit is bounded. The parentheses are
   -- not actually emitted.
   --
   is
   begin

      if Limit.Kind = Storage.Bounds.Finite then

         Conjoin (
            Conjunct =>
                 Var & '''
               & '-'
               & Var
               & Op
               & Arithmetic.Value_T'Image (Limit.Value)
               & Iter_Count,
            To => To);

      end if;

   end Conjoin_Induct;


   procedure Conjoin_Induct (
      Var    : in     String;
      Within : in     Storage.Bounds.Interval_T;
      To     : in out Formula_T)
   --
   -- Adds the constraint "(Var'-Var) in (Within times Iter_Count)"
   -- To the formula. The parentheses are not actually emitted.
   -- Depending on the nature of Within, this can add zero, one, or
   -- two actual relations To the formula.
   --
   is
   begin

      if Storage.Bounds.Singular (Within) then
         -- Only a single value Within this interval.

         Conjoin_Induct (Var, "=", Within.Min, To);
         --
         -- Could as well use Within.Max, they are equal.

      else

         Conjoin_Induct (Var, ">=", Within.Min, To);
         Conjoin_Induct (Var, "<=", Within.Max, To);

      end if;

   end Conjoin_Induct;


   function Induction_Model (
      Cells     : Cell_Set_T;
      Invariant : Cell_Set_T;
      Induction : Storage.Cell_List_T;
      Step      : Storage.Bounds.Interval_List_T)
   return Relation_T
   is

      function Dash (Cell : Storage.Cell_T) return Boolean
      is
      begin

         return  (not Cell_Sets.Is_Member (Cell, Invariant))
         or else        Storage.Is_Member (Cell, Induction);

      end Dash;

      function Dashed_Tuple is new Tuple_With_Dashes (Dash);

      Induct_Cons : Formula_T;
      -- The induction constraints on the Variables.

   begin

      for I in Induction'Range loop

         Conjoin_Induct (
            Var    => Storage.Name_Of (Induction(I)),
            Within => Step(I),
            To     => Induct_Cons);

      end loop;

      return Relation (
         Domain => Tuple_With_Counter (Cells),
         Rainge => Dashed_Tuple (Cells),
         Where  => Induct_Cons);

   end Induction_Model;


   --
   -- Execution of formulas and queries:
   --


   procedure Get_Line (
      From : in  Calc_Handle_T;
      Line : out Unbounded_String)
   --
   -- Reads a line of output from the calculator instance, with
   -- optional tracing.
   --
   is
   begin

      Exec_Cmd.Read_Line (From.Exec, Line);

      if Opt.Trace_IO then
         Output.Trace (Text => "Reply   """ & To_String (Line) & '"');
      end if;

   end Get_Line;


   procedure Check_Echo (
      Calc        : in Calc_Handle_T;
      Line_Number : in Positive);


   Slicers : constant Ada.Strings.Maps.Character_Set :=
      Ada.Strings.Maps.To_Set (",.}]() ");
   --
   -- The characters at which Write_Sliced (below) can split
   -- a calculator line.


   procedure Write_Sliced (
      Calc    : in     Calc_Handle_T;
      Text    : in     String;
      Comment : in     Boolean;
      Lines   :    out Natural)
   --
   -- Writes Text to calculator, sliced into lines that are less than 70
   -- characters long.
   -- If Comment is True, each slice is prefixed "# " as a comment marker.
   -- After writing each line, checks for an echo line.
   -- Returns in Lines the number of lines written.
   -- NOTE Expects every 70 characters long substring of Text to contain
   -- at least one of ',' '.' '}' ']' '(' ')' or ' '.
   --
   is
      use Ada.Strings;
      use Ada.Strings.Fixed;
      use Ada.Strings.Maps;

      Max_Slice : constant := 70;
      -- Maximum length of a slice.

      First : Positive := Text'First;
      -- The first unwritten position of Text.

      Cut : Positive;
      -- Last possible Text position to include in a slice
      -- starting at First.

      Last : Natural;
      -- The slice to write is First .. Last.


      procedure Write (Prefixed_Slice : in String)
      --
      -- Write the slice, which perhaps included a comment prefix.
      -- Count lines and check echo.
      --
      is
      begin

         if Opt.Trace_IO then
            Output.Trace (Text => "Command """ & Prefixed_Slice & '"');
         end if;

         Exec_Cmd.Write_Line (Calc.Exec, Prefixed_Slice);

         Lines := Lines + 1;

         Check_Echo (Calc => Calc, Line_Number => Lines);

      end Write;


   begin  -- Write_Sliced

      Lines := 0;

      while First <= Text'Last loop

         Cut := First + Max_Slice - 1;

         if Cut >= Text'Last then
            -- All remaining text fits in one slice.

            Last := Text'Last;

         else

            Last := Index (
               Text(First..Cut),
               Slicers,
               Inside, Backward);

            if Last = 0 then

               if Comment then
                  -- Can be sliced anywhere, in fact.

                  Last := Cut;

               else
                  -- Not a comment, don't know how to slice it.

                  Output.Fault (
                     Location =>
                        "Calculator.Formulas.Write_Sliced",
                     Text =>
                          "Too long atomic formula"
                        & Output.Field_Separator
                        & Text(First..Cut));

                  raise Calculator_Error;

               end if;

            end if;

         end if;

         if Comment then
            Write ("# " & Text(First .. Last));
         else
            Write (Text(First .. Last));
         end if;

         First := Last + 1;

      end loop;

   end Write_Sliced;


   procedure Check_Echo (
      Calc        : in Calc_Handle_T;
      Line_Number : in Positive)
   --
   -- Read a calculator echo-reply line and check its form.
   --
   is
      Reply : Unbounded_String;
      -- The reply line.
   begin

      Get_Line (From => Calc, Line => Reply);

      if Length(Reply) < 1
      or else Element (Reply, 1) /= '#' then

         Output.Fault (
            Location => "Calculator.Formulas.Check_Echo",
            Text     => "Calculator did not give the expected echo line.");

         Output.Fault (
            Location => "Calculator.Formulas.Check_Echo",
            Text     =>
                 "Response line" & Positive'Image(Line_Number)
               & ": '" & To_String (Reply) & ''');

         raise Calculator_Error;

      end if;

   end Check_Echo;


   procedure Check_Empty_Line (
      Calc        : in Calc_Handle_T;
      Line_Number : in Positive)
   --
   -- Reads a line from the calculator and checks that it is empty.
   --
   is
      Reply : Unbounded_String;
      -- The reply line.
   begin

      Get_Line (From => Calc,
                Line => Reply);

      if Length(Reply) > 0 then

         Output.Fault (
            Location => "Calculator.Formulas.Check_Empty_Line",
            Text     => "Calculator did not give the expected empty line.");

         Output.Fault (
            Location => "Calculator.Formulas.Check_Empty_Line",
            Text     =>
                "Response line" & Positive'Image(Line_Number)
               & ':' & To_String (Reply));

         raise Calculator_Error;

      end if;

   end Check_Empty_Line;



   Next_Calc_Id : Calc_Id_T := 1;
   --
   -- Used as a sequential identifier for calculators.


   --
   -- Calculation operations:
   --


   function Start return Calc_Handle_T
   is
      use Ada.Strings;
      use Ada.Strings.Fixed;

      Calc_Tag : constant String :=
        Trim (Calc_Id_T'Image (Next_Calc_Id), Both);

      Calc_Exe : Exec_Cmd.Execution_T;
      -- The execution of the calculator.

      Calc : Calc_Handle_T;
      -- A handle for the calculator.

      Hello : Unbounded_String;
      -- The first greeting from the calculator.


      function Log_File (Prefix : Options.Strings.Option_T)
      return String
      --
      -- The name for the input or output log-file for the next
      -- calculator, with the given prefix, or the null string
      -- if files are not kept.
      --
      is
      begin

         if Opt.Keep_Files then

            return Options.Strings.Value_Of (Prefix) & '_' & Calc_Tag;

         else

            return "";

         end if;

      end Log_File;


   begin  -- Start

      if Opt.Trace_IO then
         Output.Trace (Text =>
              "Starting calculator '"
            & Options.Strings.Value_Of (Opt.Prog_Calculator)
            & "' number " & Calc_Tag);
      end if;

      Calc_Exe := Calculator.Platform.Start (
         Program    => Options.Strings.Value_Of (Opt.Prog_Calculator),
         Input_Log  => Log_File (Opt.Calc_Input_File),
         Output_Log => Log_File (Opt.Calc_Output_File));

      Calc := new Calc_Object_T'(
         Exec         => Calc_Exe,
         Calc_ID      => Next_Calc_Id,
         Next_Pool_Id => 1,
         Next_Flux_Id => 1);

      Get_Line (From => Calc, Line => Hello);

      if Index (Hello, "Omega") = 0 then

         Output.Fault (
            Location => "Calculator.Formulas.Start",
            Text     =>
                 "Calculator starting message unexpected: "
               & To_String (Hello));

         raise Calculator_Error;

      end if;

      Next_Calc_Id := Next_Calc_Id + 1;

      return Calc;

   end Start;


   procedure Stop (Calc : in out Calc_Handle_T)
   is
   begin

      if Opt.Trace_IO then
         Output.Trace (Text => "Stopping calculator");
      end if;

      Exec_Cmd.End_Execution (Calc.Exec);

      Calc := null;

   end Stop;


   procedure Comment (
      Text : in String;
      Calc : in Calc_Handle_T)
   is
      Lines : Natural;
      -- The number of lines written to the calculator.
   begin

      if Opt.Trace_Comments then
         Output.Trace (Text => Text);
      end if;

      Write_Sliced (
         Calc    => Calc,
         Text    => Text,
         Comment => True,
         Lines   => Lines);

   end Comment;


   procedure Assign (
      Target : in Identifier_T;
      Value  : in Formula_T;
      Calc   : in Calc_Handle_T)
   is
      Lines : Natural;
      -- The number of lines written to the calculator.
   begin

      Write_Sliced (
         Calc    => Calc,
         Text    => To_String (Target & ":=" & Value & ';'),
         Comment => False,
         Lines   => Lines);

      -- Check for the prompt:

      Check_Echo (Calc => Calc, Line_Number => Lines + 1);

   end Assign;


   function Value (
      Formula : Formula_T;
      Calc    : Calc_Handle_T)
   return Formula_T
   is

      Lines_Written : Natural;
      -- The number of lines written for Formula.

      Line_Number : Positive;
      -- The number of the response line.

      Reply_Line : Unbounded_String;
      -- One line of response from the calculator.

      Reply : Unbounded_String;
      -- The whole reply, edited for echos and null lines.

   begin

      Reply := Null_Unbounded_String;

      Write_Sliced (
         Calc    => Calc,
         Text    => To_String (Formula & ';'),
         Comment => False,
         Lines   => Lines_Written);

      Line_Number := Lines_Written + 1;

      Check_Empty_Line (
         Calc        => Calc,
         Line_Number => Line_Number);

      loop
         -- Textual response, which ends with an empty line.

         Get_Line (From => Calc, Line => Reply_Line);

         Line_Number := Line_Number + 1;

         exit when Length (Reply_Line) = 0;

         Append (Reply, Reply_Line);

         -- In True/False responses there is no empty line,
         -- so match the text to find the last line:

         exit when Index (Reply_Line, "True" ) = 1;
         exit when Index (Reply_Line, "False") = 1;

      end loop;

      -- And a last echo line, with only "# ".

      Check_Echo (Calc => Calc, Line_Number => Line_Number + 1);

      return Reply;

   end Value;


   function Subset (
      Left  : Set_T;
      Right : Set_T;
      Calc  : Calc_Handle_T)
   return Boolean
   is
      Reply : Formula_T;
      -- The reply for the subset query.
   begin

      Reply := Value (
         Formula => Left & " subset " & Right,
         Calc    => Calc);

      return Parser.Boolean_Literal (To_String (Reply));

   end Subset;


   function Is_Null (
      Set   : Set_T;
      Cells : Tuple_T;
      Calc  : Calc_Handle_T)
   return Boolean
   is
   begin

      return Subset (
         Left  => Set,
         Right => Null_Set (Cells),
         Calc  => Calc);

   end Is_Null;


   function Is_Null (
      Set  : Set_T;
      Calc : Calc_Handle_T)
   return Boolean
   is
   begin

      return Subset (
         Left  => Set,
         Right => Null_Set (To_Formula ("[x]")),
         Calc  => Calc);

   end Is_Null;


   function Superset (
      Set         : Set_T;
      Of_Interval : Storage.Bounds.Interval_T;
      Calc        : Calc_Handle_T)
   return Boolean
   is
   begin

      return Subset (
         Left  => Interval (Of_Interval),
         Right => Set,
         Calc  => Calc);

   end Superset;


   function Hull_Bound (
      Set  : Set_T;
      Calc : Calc_Handle_T)
   return Storage.Bounds.Interval_T
   is
      Reply : Formula_T;
      -- The reply for the hull-range query.
   begin

      if Opt.Use_Convex_Hull then

	 Reply := Value (
            Formula => "convexhull(" & Set & ')',
            Calc    => Calc);

      else

	 Reply := Value (
            Formula => "hull(" & Set & ')',
            Calc    => Calc);

      end if;

      return Parser.Bound (To_String (Reply));

   end Hull_Bound;


   function Values (
      Set  : Set_T;
      Calc : Calc_Handle_T)
   return Storage.Bounds.Value_List_T
   is
      use type Arithmetic.Value_T;

      List : Storage.Bounds.Value_List_T (
         1 .. Storage.Bounds.Opt.Max_Listed_Values);
      -- The values, listed in increasing order.

      Last : Natural := 0;
      -- The result will be List(1 .. Last).

      Left : Storage.Bounds.Interval_T := Storage.Bounds.Universal_Interval;
      -- The interval we have left. Initially everything.

      Bound : Storage.Bounds.Interval_T;
      -- The bounds on the values Left in the Set.

      Value : Arithmetic.Value_T;
      -- The least value Left in the Set, from Bound.Min.

   begin

      -- Iteratively find the least value Left in the Set, add
      -- it to the List and increase Left.Min to exclude it.

      Enumeration :
      loop

         -- Find the bounds of the values Left in the Set:

         begin

            Bound := Hull_Bound (
               Set  => Intersection (Set, Interval (Left)),
               Calc => Calc);

         exception

         when Null_Set_Error =>
            -- We already found and listed all values.

            exit Enumeration;

         end;

         -- There is something Left in the Set.

         if not Storage.Bounds.Bounded (Bound) then
            -- We cannot compute the bounds on the Set (Left).

            Output.Note ("Unbounded set of values to list.");

            raise Unbounded_Set;

         end if;

         -- Add the smallest value left to the List:

         Value := Storage.Bounds.Value (Bound.Min);

         if Last >= List'Last then

            Output.Warning (
                 "Arithmetic set has over"
               & Natural'Image (List'Length)
               & " values, considered unbounded.");

            raise Unbounded_Set;

         end if;

         Last := Last + 1;

         List(Last) := Value;

         -- Exclude the Value from those Left:

         Left.Min := Storage.Bounds.Limit (Value + 1);

      end loop Enumeration;

      -- Return what we have.

      return List(1 .. Last);

   end Values;


   --
   -- Creation of new pools and fluxes:
   --


   function New_Pool (
      Cells : Cell_Set_T;
      Value : Set_T;
      Calc  : Calc_Handle_T)
   return Pool_T
   --
   -- Creates a new pool, with the given cells as the domain
   -- tuple, and assigns it the given set value using the given
   -- calculator.
   --
   is
      Pool : constant Pool_T := (
         Id    => Calc.Next_Pool_Id,
         Cells => Cells,
         Calc  => Calc);
   begin

      Assign (
         Target => Id (Pool),
         Value  => Value,
         Calc   => Calc);

      Calc.Next_Pool_Id := Calc.Next_Pool_Id + 1;

      return Pool;

   end New_Pool;


   function New_Flux (
      Cells : Cell_Set_T;
      Value : Relation_T;
      Calc  : Calc_Handle_T)
   return Flux_T
   --
   -- Creates a new flux, with the given cells as the domain
   -- and range tuples, and assigns it the given relation value
   -- using the given calculator.
   --
   is
      Flux : constant Flux_T := (
         Id    => Calc.Next_Flux_Id,
         Cells => Cells,
         Calc  => Calc);
   begin

      Assign (
         Target => Id (Flux),
         Value  => Value,
         Calc   => Calc);

      Calc.Next_Flux_Id := Calc.Next_Flux_Id + 1;

      return Flux;

   end New_Flux;


   function Identity_Flux (
      Domain : Cell_Set_T;
      Calc   : Calc_Handle_T)
   return Flux_T
   is
   begin

      return New_Flux (
         Cells  => Domain,
         Value  => Identity_Relation (Domain),
         Calc   => Calc);

   end Identity_Flux;


   --
   -- Identifiers specific to Bound-T algorithms:
   --


   function Number (Step : Flow.Step_T) return String
   --
   -- The step-specific part of an identifier.
   --
   is
      S : String := Flow.Step_Index_T'Image(Flow.Index(Step));
   begin
      S(S'First) := 's';
      return S;
   end Number;


   function Number (Node : Flow.Node_Index_T) return String
   --
   -- The node-specific part of an identifier.
   --
   is
      S : String := Flow.Node_Index_T'Image (Node);
   begin
      S(S'First) := 'n';
      return S;
   end Number;


   function Into_Id (Step : Flow.Step_T) return Identifier_T
   is
   begin
      return To_Formula ("into_" & Number (Step));
   end Into_Id;


   function Into_Id (Node : Flow.Node_Index_T) return Identifier_T
   is
   begin
      return To_Formula ("into_" & Number (Node));
   end Into_Id;


   function Eff_Id (Step : Flow.Step_T) return Identifier_T
   is
   begin
      return To_Formula ("eff_" & Number (Step));
   end Eff_Id;


   function Eff_Id (Node : Flow.Node_Index_T) return Identifier_T
   is
   begin
      return To_Formula ("eff_" & Number (Node));
   end Eff_Id;


   function From_Id (Step : Flow.Step_T) return Identifier_T
   is
   begin
      return To_Formula ("from_" & Number (Step));
   end From_Id;


   function From_Id (Node : Flow.Node_Index_T) return Identifier_T
   is
   begin
      return To_Formula ("from_" & Number (Node));
   end From_Id;


   function Flow_Id (Edge : Flow.Step_Edge_T ) return Identifier_T
   is
   begin
      return To_Formula (
           "flow_"
         & Number (Flow.Source (Edge))
         & "_"
         & Number (Flow.Target (Edge)) );
   end Flow_Id;


   function Flow_Id (Edge : Flow.Edge_Index_T) return Identifier_T
   is
      E : String := Flow.Edge_Index_T'Image (Edge);
   begin

      E(E'First) := 'e';

      return To_Formula ("flow_" & E);

   end Flow_Id;


end Calculator.Formulas;
