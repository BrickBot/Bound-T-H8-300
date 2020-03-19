-- Storage.Bounds (body)
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
-- $Revision: 1.17 $
-- $Date: 2015/10/24 20:05:51 $
--
-- $Log: storage-bounds.adb,v $
-- Revision 1.17  2015/10/24 20:05:51  niklas
-- Moved to free licence.
--
-- Revision 1.16  2014/07/01 22:07:01  niklas
-- Added Image functions for Var_Interval(_List)_T.
--
-- Revision 1.15  2013/12/08 20:08:23  niklas
-- Added the function Number_Of_Values.
--
-- Revision 1.14  2012-02-13 17:52:20  niklas
-- BT-CH-0230: Options -max_loop and -max_stack for spurious bounds.
--
-- Revision 1.13  2009-11-27 11:28:07  niklas
-- BT-CH-0184: Bit-widths, Word_T, failed modular analysis.
--
-- Revision 1.12  2008/06/18 20:52:56  niklas
-- BT-CH-0130: Data pointers relative to initial-value cells.
--
-- Revision 1.11  2008/03/11 22:08:07  niklas
-- BT-CH-0121: Delayed calls and other SHARC support.
--
-- Revision 1.10  2007/12/17 13:54:41  niklas
-- BT-CH-0098: Assertions on stack usage and final stack height, etc.
--
-- Revision 1.9  2007/07/09 13:42:51  niklas
-- Added the Bounds_T primitive functions Intervals and Full_Image.
--
-- Revision 1.8  2007/03/29 15:18:04  niklas
-- BT-CH-0056.
--
-- Revision 1.7  2007/03/18 12:50:41  niklas
-- BT-CH-0050.
--
-- Revision 1.6  2007/01/25 21:25:18  niklas
-- BT-CH-0043.
--
-- Revision 1.5  2006/05/27 21:33:23  niklas
-- Added function Is_In (Value, Interval) for BT-CH-0020.
--
-- Revision 1.4  2006/02/27 10:04:14  niklas
-- Added functions Min (Interval_T) and Max (Interval_T).
--
-- Revision 1.3  2005/03/04 09:32:34  niklas
-- Added functions "+" and "-" to shift limits and intervals
-- up or down by adding a constant value to the bounds.
-- Added function Intersects to show if two intervals meet.
--
-- Revision 1.2  2005/02/19 20:34:56  niklas
-- BT-CH-0003.
--
-- Revision 1.1  2005/02/16 21:11:48  niklas
-- BT-CH-0002.
--


with Ada.Tags;
with Ada.Unchecked_Deallocation;
with Arithmetic;
with Faults;
with Output;
with Storage.Bounds.Opt;


package body Storage.Bounds is


   use type Value_T;
   use type Word_T;
   use type Width_T;


   --
   ---   Limits, intervals, and lists for signed integers of indefinite width
   --


   --
   ---   Division with ceiling/floor:
   --


   function Floor (Left, Right : Value_T)
   return Value_T
   is
   begin

      if (Left > 0 and Right > 0)
      or (Left < 0 and Right < 0)
      or (Left mod Right = 0)
      then
         return Left / Right;
      else
         return Left / Right - 1;
      end if;

   end Floor;


   function Ceil (Left, Right : Value_T)
   return Value_T
   is
   begin

      return -Floor (Left, -Right);

   end Ceil;



   --
   ---    Limits (one-sided or on a single value) on signed integers
   --


   function Image (Item : Limit_T) return String
   is
   begin

      return Image (Item => Item, Unknown => "?");

   end Image;


   function Image (
      Item    : Limit_T;
      Unknown : String)
   return String
   is
   begin

      case Item.Kind is
      when Unlimited => return Unknown;
      when Finite    => return Arithmetic.Image (Item.Value);
      end case;

   end Image;


   function Over (Value : Value_T; Max : Limit_T) return Boolean
   is
   begin

      case Max.Kind is
      when Unlimited => return False;
      when Finite    => return Value > Max.Value;
      end case;

   end Over;


   function Under (Value : Value_T; Min : Limit_T) return Boolean
   is
   begin

      case Min.Kind is
      when Unlimited => return False;
      when Finite    => return Value < Min.Value;
      end case;

   end Under;


   function Limit (Value : Value_T) return Limit_T
   is
   begin

      return (Kind => Finite, Value => Value);

   end Limit;


   function Unlimited (L : Limit_T) return Boolean
   is
   begin

      return L.Kind = Unlimited;

   end Unlimited;


   function Known (L : Limit_T) return Boolean
   is
   begin

      return L.Kind = Finite;

   end Known;


   function Value (L : Limit_T) return Value_T
   is
   begin

      if L.Kind /= Finite then

         raise Unbounded;

      end if;

      return L.Value;

   end Value;


   procedure Limit_Value (
      Limit    : in     Limit_T;
      Value    :    out Value_T;
      Is_Const :    out Boolean)
   is
   begin

      Is_Const := Limit.Kind = Finite;

      if Is_Const  then

         Value := Limit.Value;

      else

         Value := 0;

      end if;

   end Limit_Value;


   function And_Max (Left, Right : Limit_T) return Limit_T
   is
   begin

      if Right.Kind = Unlimited then

         return Left;

      elsif Left.Kind = Unlimited then

         return Right;

      else
         -- Two Finite limits, the smaller Value is better:

         return (
            Kind  => Finite,
            Value => Value_T'Min (Left.Value, Right.Value));

      end if;

   end And_Max;


   function And_Min (Left, Right : Limit_T) return Limit_T
   is
   begin

      if Right.Kind = Unlimited then

         return Left;

      elsif Left.Kind = Unlimited then

         return Right;

      else
         -- Two Finite limits, the larger Value is better:

         return (
            Kind  => Finite,
            Value => Value_T'Max (Left.Value, Right.Value));

      end if;

   end And_Min;


   function Or_Min (Left, Right : Limit_T) return Limit_T
   is
   begin

      if Right.Kind = Unlimited or Left.Kind = Unlimited then

         return Not_Limited;

      else
         -- Two Finite limits, the smaller Value is better:

         return (
            Kind  => Finite,
            Value => Value_T'Min (Left.Value, Right.Value));

      end if;

   end Or_Min;


   function Or_Max (Left, Right : Limit_T) return Limit_T
   is
   begin

      if Right.Kind = Unlimited or Left.Kind = Unlimited then

         return Not_Limited;

      else
         -- Two Finite limits, the larger Value is better:

         return (
            Kind  => Finite,
            Value => Value_T'Max (Left.Value, Right.Value));

      end if;

   end Or_Max;


   procedure Increase (
      Limit    : in out Limit_T;
      To_Allow : in     Value_T)
   is
   begin

      if not Unlimited (Limit) and then Limit.Value < To_Allow then

         Limit.Value := To_Allow;

      end if;

   end Increase;


   procedure Decrease (
      Limit    : in out Limit_T;
      To_Allow : in     Value_T)
   is
   begin

      if not Unlimited (Limit) and then Limit.Value > To_Allow then

         Limit.Value := To_Allow;

      end if;

   end Decrease;


   function "+" (Left : Limit_T; Right : Value_T)
   return Limit_T
   is
   begin

      case Left.Kind is
      when Unlimited => return Left;
      when Finite    => return (Finite, Left.Value + Right);
      end case;

   end "+";


   function "-" (Left : Limit_T; Right : Value_T)
   return Limit_T
   is
   begin

      case Left.Kind is
      when Unlimited => return Left;
      when Finite    => return (Finite, Left.Value - Right);
      end case;

   end "-";


   function "-" (Item : Limit_T) return Limit_T
   is

      Opp : Value_T;
      -- The opposite of Item.Value.

   begin

      if Item.Value = Value_T'First then

         Opp := Value_T'Last;

      elsif Item.Value = Value_T'Last then

         Opp := Value_T'First;

      else

         Opp := - Item.Value;

      end if;

      return (Kind => Item.Kind, Value => Opp);

   end "-";


   --
   ---   Intervals of signed integers
   --


   function Interval (Min, Max : Value_T)
   return Interval_T
   is
   begin

      return (
         Min => (Kind => Finite, Value => Min),
         Max => (Kind => Finite, Value => Max));

   end Interval;


   function Min (Item : Interval_T) return Value_T
   is
   begin

      return Value (Item.Min);

   end Min;


   function Max (Item : Interval_T) return Value_T
   is
   begin

      return Value (Item.Max);

   end Max;


   function Singleton (Value : Value_T)
   return Interval_T
   is
   begin

      return Interval (Min => Value, Max => Value);

   end Singleton;


   function One_Or_All (Value : Limit_T)
   return Interval_T
   is
   begin

      return (Min => Value, Max => Value);

   end One_Or_All;


   function Void (Item : Interval_T) return Boolean
   is
   begin

      return     (Known (Item.Min) and Known (Item.Max))
         and then Value (Item.Min)  >  Value (Item.Max);

   end Void;


   function Known (Interval : Interval_T) return Boolean
   is
   begin

      return Known (Interval.Min) or Known (Interval.Max);

   end Known;


   function Bounded (Interval : Interval_T) return Boolean
   is
   begin

      return Known (Interval.Min) and Known (Interval.Max);

   end Bounded;


   function Universal (Interval : Interval_T) return Boolean
   is
   begin

      return not (Known (Interval.Min) or Known (Interval.Max));

   end Universal;


   function Number_Of_Values (Within : Interval_T) return Value_T
   is
   begin

      if Bounded (Within) then

         return Within.Max.Value - Within.Min.Value + 1;

      else

         raise Unbounded;

      end if;

   exception

   when Constraint_Error =>
      -- The number is too large for Value_T.

      Output.Warning (
           "Interval too large to be counted"
         & Output.Field_Separator
         & Image (Within));

      raise Unbounded;
      -- Well, effectively it is unbounded.

   end Number_Of_Values;


   function Is_In (
      Value    : Value_T;
      Interval : Interval_T)
   return Boolean
   is
   begin

      return
         (not Known (Interval.Min) or else Value >= Interval.Min.Value)
      and
         (not Known (Interval.Max) or else Value <= Interval.Max.Value);

   end Is_In;


   function "<" (Value : Value_T; Interval : Interval_T)
   return Boolean
   is
   begin

      return Known (Interval.Min) and then Value < Interval.Min.Value;

   end "<";


   function ">" (Value : Value_T; Interval : Interval_T)
   return Boolean
   is
   begin

      return Known (Interval.Max) and then Value > Interval.Max.Value;

   end ">";


   function Singular (Interval : Interval_T)
   return Boolean
   is
   begin

      return   Bounded (Interval)
      and then Interval.Min.Value = Interval.Max.Value;

   end Singular;


   function Single_Value (Interval : Interval_T)
   return Value_T
   is
   begin

      if Singular (Interval) then

         return Interval.Min.Value;
         -- Same as Interval.Max.Value.

      else

         raise Multiple_Values;

      end if;

   end Single_Value;


   function "and" (Left, Right : Interval_T) return Interval_T
   is
   begin

      return (Min => And_Min (Left.Min, Right.Min),
              Max => And_Max (Left.Max, Right.Max));

   end "and";


   function "or" (Left, Right : Interval_T) return Interval_T
   is
   begin

      if Void (Left) then

         return Right;

      elsif Void (Right) then

         return Left;

      else

         return (
            Min => Or_Min (Left.Min, Right.Min),
            Max => Or_Max (Left.Max, Right.Max));

      end if;

   end "or";


   procedure Widen (
      Interval   : in out Interval_T;
      To_Contain : in     Value_T)
   is
   begin

      if Void (Interval) then

         Interval := Singleton (To_Contain);

      else

         Decrease (Interval.Min, To_Contain);

         Increase (Interval.Max, To_Contain);

      end if;

   end Widen;


   function "<=" (Left, Right : Interval_T) return Boolean
   is

      Subset : Boolean := True;
      -- The result, optimistically initialized.

   begin

      -- Compare lower bounds: Right.Min <= Left.Min

      if Right.Min.Kind = Finite then
         -- The Right interval limits the low end.

         if      Left.Min.Kind = Unlimited
         or else Left.Min.Value < Right.Min.Value
         then
            -- The Left interval extends to smaller values than Right,
            -- unless the Left interval is Void.

            Subset := False;

         end if;

      end if;

      -- Compare upper bounds: Left.Max <= Right.Max

      if  Subset
      and Right.Max.Kind = Finite then
         -- The low end was good, but the Right interval limits the
         -- high end, so it must be checked too.

         if      Left.Max.Kind = Unlimited
         or else Left.Max.Value > Right.Max.Value
         then
            -- The Left interval extends to larger values than Right,
            -- unless the Left interval is Void.

            Subset := False;

         end if;

      end if;

      if not Subset then
         -- Last chance: Left interval may be Void.

         Subset := Void (Left);

      end if;

      return Subset;

   end "<=";


   function Intersects (Left, Right : Interval_T) return Boolean
   is
   begin

      return not Void (Left and Right);

   end Intersects;


   function "+" (Left : Interval_T; Right : Value_T)
   return Interval_T
   is
   begin

      return (Min => Left.Min + Right, Max => Left.Max + Right);

   end "+";


   function "-" (Left : Interval_T; Right : Value_T)
   return Interval_T
   is
   begin

      return (Min => Left.Min - Right, Max => Left.Max - Right);

   end "-";


   function "+" (Left, Right : Interval_T) return Interval_T
   is

      Sum : Interval_T := Universal_Interval;
      -- The interval sum, possibly constrained below.

   begin

      if Known (Left.Min) and Known (Right.Min) then

         Sum.Min := Limit (Value (Left.Min) + Value (Right.Min));

      end if;

      if Known (Left.Max) and Known (Right.Max) then

         Sum.Max := Limit (Value (Left.Max) + Value (Right.Max));

      end if;

      return Sum;

   end "+";


   function "-" (Item : Interval_T) return Interval_T
   is
   begin

      return (Min => - Item.Max, Max => - Item.Min);

   end "-";


   function "-" (Left, Right : Interval_T) return Interval_T
   is
   begin

      return Left + (- Right);

   end "-";


   function Fixed_Image (
      Item   : Limit_T;
      Prefix : String := "";
      Suffix : String := "")
   return String
   --
   -- If the limit is known, returns the limiting value,
   -- between Prefix and Suffix.
   -- Otherwise returns the null string.
   --
   is
   begin

      case Item.Kind is

      when Finite =>

         return Prefix & Arithmetic.Image (Item.Value) & Suffix;

      when Unlimited =>

         return "";

      end case;

   end Fixed_Image;


   function Image (Item : Interval_T) return String
   is
   begin

      if Singular (Item) then

         return Image (Item.Min);

      else

	 return Image (Item => Item.Min, Unknown => "-inf")
              & " .. "
              & Image (Item => Item.Max, Unknown => "+inf");

      end if;

   end Image;


   function Image (
      Item : Interval_T;
      Name : String)
   return String
   is
   begin

      if Singular (Item) then

         return
              Name
            & Fixed_Image (Item.Min, Prefix => "=");

      else

         return
              Fixed_Image (Item.Min, Suffix => "<=")
            & Name
            & Fixed_Image (Item.Max, Prefix => "<=");

      end if;

   end Image;


   function Image (
      Item : Interval_T;
      Cell : Cell_T)
   return String
   is
   begin

      return Image (Item, Image (Cell));

   end Image;


   function Single_Value (Intervals : Interval_List_T)
   return Value_List_T
   is

      Values : Value_List_T (Intervals'Range);
      -- Holds the result.

   begin

      for I in Intervals'Range loop

         Values(I) := Single_Value (Intervals(I));

      end loop;

      return Values;

   end Single_Value;


   function Image (
      Item   : Interval_List_T;
      Prefix : String)
   return String
   is
   begin

      if Item'Length = 0 then

         return "";

      elsif Item'Length = 1 then

         return
            Image (
               Item => Item(Item'First),
               Name => Prefix & Output.Image (Item'First));

      else

         return
            Image (
               Item => Item(Item'First),
               Name => Prefix & Output.Image (Item'First))
            & ", "
            & Image (Item(Item'First + 1 .. Item'Last), Prefix);

      end if;

   end Image;


   --
   ---   Intervals of signed integers for cells
   --


   function Empty return Cell_Interval_List_T
   is

      None : Cell_Interval_List_T (1..0);

   begin

      return None;

   end Empty;


   function Interval (
      Cell : Storage.Cell_T;
      From : Cell_Interval_List_T)
   return Interval_T
   is

      Inter : Interval_T := Universal_Interval;
      -- The bounds on the cell, known so far.

   begin

      for F in From'Range loop

         if From(F).Cell = Cell then

            Inter := Inter and From(F).Interval;

         end if;

      end loop;

      return Inter;

   end Interval;


   function Intervals_For_Cells (
      Cells : Cell_Set_T;
      From  : Cell_Interval_List_T)
   return Cell_Interval_List_T
   is

      List : Cell_Interval_List_T (1 .. From'Length);
      Last : Natural := 0;
      -- The result will be List(1 .. Last).

   begin

      for F in From'Range loop

         if Storage.Is_Member (From(F).Cell, Cells) then

            Last := Last + 1;
            List(Last) := From(F);

         end if;

      end loop;

      return List(1..Last);

   end Intervals_For_Cells;


   function "and" (Left, Right : Cell_Interval_List_T)
   return Cell_Interval_List_T
   is

      Conj : Cell_Interval_List_T (1 .. Left'Length + Right'Length);
      Last : Natural := 0;
      -- The conjoined bounds are returned in Conj(1..Last).


      procedure Conjoin (Item : Cell_Interval_T)
      --
      -- Adds a bound to Conj, conjoining it with any existing
      -- bound on the same cell.
      --
      is
      begin

         for C in 1 .. Last loop

            if Conj(C).Cell = Item.Cell then

               Conj(C).Interval := Conj(C).Interval and Item.Interval;

               return;

            end if;

         end loop;

         -- It is a new cell.

         Last := Last + 1;
         Conj(Last) := Item;

      end Conjoin;


   begin  -- "and"

      for L in Left'Range loop
         Conjoin (Left(L));
      end loop;

      for R in Right'Range loop
         Conjoin (Right(R));
      end loop;

      return Conj(1..Last);

   end "and";


   function Image (Item : Cell_Interval_T) return String
   is
   begin

      return Image (Item => Item.Interval, Name => Storage.Image (Item.Cell));

   end Image;


   function Image (Item : Cell_Interval_List_T) return String
   is
   begin

      if Item'Length = 0 then

         return "none";

      elsif Item'Length = 1 then

         return Image (Item(Item'First));

      else

         return
              Image (Item(Item'First))
            & ", "
            & Image (Item(Item'First + 1 .. Item'Last));

      end if;

   end Image;


   function Cells_Of (Item : Cell_Interval_List_T) return Cell_List_T
   is

      Cells : Cell_List_T (Item'Range);

   begin

      for C in Cells'Range loop

         Cells(C) := Item(C).Cell;

      end loop;

      return Cells;

   end Cells_Of;


   --
   ---   Intervals of signed integers for variables with moving locations
   --


   function Image (Item : Var_Interval_T)
   return String
   is
   begin

      return Image (Item.Location)
           & " in "
           & Image (Item.Interval);

   end Image;


   function Image (Item : Var_Interval_List_T)
   return String
   is
   begin

      case Item'Length is

      when 0 => return "(none)";

      when 1 => return Image (Item(Item'First));

      when others =>
            return '('
          & Image (Item(Item'First))
          & ", "
          & Image (Item(Item'First + 1 .. Item'Last))
          & ')';

      end case;

   end Image;


   function "and" (Left, Right : Var_Interval_List_T)
   return Var_Interval_List_T
   is
      use type Storage.Location_Ref;

      Conj : Var_Interval_List_T (1 .. Left'Length + Right'Length);
      Last : Natural := 0;
      -- The conjoined bounds are returned in Conj(1..Last).


      procedure Conjoin (Item : Var_Interval_T)
      --
      -- Adds a bound to Conj, conjoining it with any existing
      -- bound on the same variable.
      --
      is
      begin

         for C in 1 .. Last loop

            if Conj(C).Location = Item.Location then

               Conj(C).Interval := Conj(C).Interval and Item.Interval;

               return;

            end if;

         end loop;

         -- It is a new cell.

         Last := Last + 1;
         Conj(Last) := Item;

      end Conjoin;


   begin  -- "and"

      for L in Left'Range loop
         Conjoin (Left(L));
      end loop;

      for R in Right'Range loop
         Conjoin (Right(R));
      end loop;

      return Conj(1..Last);

   end "and";


   function Cell_Intervals (
      From  : Var_Interval_List_T;
      Point : Processor.Code_Address_T)
   return Cell_Interval_List_T
   is

      Result : Cell_Interval_List_T (1 .. Storage.Number_Of_Cells);
      -- To hold the resulting Cell Interval List.

      Last : Natural := 0;
      -- The defined Cell Intervals are Result(1 .. Last).

      Loc : Storage.Location_Ref;
      -- The location of one of the variables in From.

      Rebound : Boolean;
      -- Whether the current cell was already bounded in the Result.

   begin

      for F in From'Range loop

         Loc := From(F).Location;

         for L in Loc'Range loop

            if Storage.In_Range (
               Address => Point,
               Rainge  => Loc(L).Address)
            then
               -- Loc(L) locates the bounded value in Loc(L).Cell.

               Rebound := False;

               Find_Earlier_Bound:
               for R in Result'First .. Last loop

                  Rebound := Result(R).Cell = Loc(L).Cell;

                  if Rebound then
                     -- This cell is already bounded. Conjoin the bounds:

                     Result(R).Interval :=
                        Result(R).Interval and From(F).Interval;

                     exit Find_Earlier_Bound;

                  end if;

               end loop Find_Earlier_Bound;

               if not Rebound then
                  -- This is the first bound on this cell.

                  Last := Last + 1;

                  Result(Last) := (
                     Cell     => Loc(L).Cell,
                     Interval => From(F).Interval);

               end if;

            end if;

         end loop;

      end loop;

      return Result(1 .. Last);

   end Cell_Intervals;


   --
   ---  Lists (enumerations) of signed integer values
   --


   function Values (Within : Interval_T) return Value_List_T
   is

      Min : constant Value_T := Value (Within.Min);
      Max : constant Value_T := Value (Within.Max);
      -- The limits of the interval.
      -- This raises (and propagates) Unbounded if the limits
      -- are not known.

      Full_Num : constant Natural :=
         Natural'Max (0, Integer (Max - Min + 1));
      -- The full number of values in the interval.

      Num : constant Natural :=
         Natural'Min (Opt.Max_Listed_Values, Full_Num);
      -- The number that we are allowed to list.

      List : Value_List_T (1 .. Num);
      -- The result.

   begin

      if Num < Full_Num then
         -- Too many values in the interval.

         raise Unbounded;

      end if;

      for L in List'Range loop

         List(L) := Min + Value_T (L - List'First);

      end loop;

      return List;

   end Values;


   function "-" (Left, Right : Interval_T) return Value_List_T
   is
   begin

      return Difference (
         Included => Left,
         Excluded => Right,
         Aligned  => 1);

   end "-";


   function Align_Up (
      Value   : Value_T;
      Aligned : Positive_Value_T)
   return Value_T
   --
   -- Value rounded up to the nearest multiple of Aligned.
   --
   is
   begin

      if Value mod Aligned = 0 then

         return Value;

      else

         return Value + Aligned - (Value mod Aligned);

      end if;

   end Align_Up;


   function Align_Down (
      Value   : Value_T;
      Aligned : Positive_Value_T)
   return Value_T
   --
   -- Value rounded down to the nearest multiple of Aligned.
   --
   is
   begin

      return Value - (Value mod Aligned);

   end Align_Down;


   function Values (Within : Interval_T; Aligned : Positive_Value_T)
   return Value_List_T
   is
      use Processor;

      Min : constant Value_T := Value (Within.Min);
      Max : constant Value_T := Value (Within.Max);
      -- The limits of the interval.
      -- This raises (and propagates) Unbounded if the limits
      -- are not known.

      First : constant Value_T := Align_Up   (Min, Aligned);
      Last  : constant Value_T := Align_Down (Max, Aligned);
      -- The first and last aligned values within the limits.

      Full_Num : constant Natural :=
         Natural'Max (0, Integer ((Last + Aligned - First) / Aligned));
      -- The full number of aligned values in First .. Last.

      Num : constant Natural :=
         Natural'Min (Opt.Max_Listed_Values, Full_Num);
      -- The number that we are allowed to list.

      Values : Value_List_T (1 .. Num);
      -- The values to be returned.

   begin

      if Num < Full_Num then
         -- Too many values in the interval.

         raise Unbounded;

      end if;

      for V in Values'Range loop

         Values(V) := First + Value_T (V - 1) * Aligned;

      end loop;

      return Values;

   end Values;


   function Difference (
      Included : in Interval_T;
      Excluded : in Interval_T;
      Aligned  : in Positive_Value_T)
   return Value_List_T
   is
      use Processor;

      I_Min : constant Value_T := Value (Included.Min);
      I_Max : constant Value_T := Value (Included.Max);
      E_Min : constant Value_T := Value (Excluded.Min);
      E_Max : constant Value_T := Value (Excluded.Max);
      -- The limits involved.

      Lesser : constant Interval_T := (
         Min => Included.Min,
         Max => (Kind  => Finite,
                 Value => Value_T'Min (I_Max, E_Min - 1)));
      --
      -- The interval consisting of the Included values that
      -- are less than the Excluded interval, if any.

      Greater : constant Interval_T := (
         Min => (Kind  => Finite,
                 Value => Value_T'Max (I_Min, E_Max + 1)),
         Max => Included.Max);
      --
      -- The interval consisting of the Included values that
      -- are greater than the Excluded interval, if any.

   begin

      return
           Values (Within => Lesser , Aligned => Aligned)
         & Values (Within => Greater, Aligned => Aligned);

   end Difference;


   --
   ---   Limits, intervals, and lists for binary words of specific width
   --


   --
   ---   Limits (one-sided or on a single value) for binary words
   --
   -- These are not yet "bounds", but can be derived from "bounds" and
   -- can be used to construct "bounds".


   function Image (Item : Word_Limit_T) return String
   is
   begin

      return Image (Item => Item, Unknown => "?");

   end Image;


   function Image (
      Item    : Word_Limit_T;
      Unknown : String)
   return String
   is
   begin

      case Item.Kind is
      when Unlimited => return Unknown;
      when Finite    => return Arithmetic_Base.Image (Item.Value);
      end case;

   end Image;


   function Limit (Value : Word_T) return Word_Limit_T
   is
   begin

      return (Kind => Finite, Value => Value);

   end Limit;


   function Unlimited (L : Word_Limit_T) return Boolean
   is
   begin

      return L.Kind = Unlimited;

   end Unlimited;


   function Known (L : Word_Limit_T) return Boolean
   is
   begin

      return L.Kind = Finite;

   end Known;


   function Value (L : Word_Limit_T) return Word_T
   is
   begin

      if L.Kind /= Finite then

         raise Unbounded;

      end if;

      return L.Value;

   end Value;


   function And_Max (Left, Right : Word_Limit_T) return Word_Limit_T
   is
   begin

      if Right.Kind = Unlimited then

         return Left;

      elsif Left.Kind = Unlimited then

         return Right;

      else
         -- Two Finite limits, the smaller Value is better:

         return (
            Kind  => Finite,
            Value => Word_T'Min (Left.Value, Right.Value));

      end if;

   end And_Max;


   function And_Min (Left, Right : Word_Limit_T) return Word_Limit_T
   is
   begin

      if Right.Kind = Unlimited then

         return Left;

      elsif Left.Kind = Unlimited then

         return Right;

      else
         -- Two Finite limits, the larger Value is better:

         return (
            Kind  => Finite,
            Value => Word_T'Max (Left.Value, Right.Value));

      end if;

   end And_Min;


   function Or_Min (Left, Right : Word_Limit_T) return Word_Limit_T
   is
   begin

      if Right.Kind = Unlimited or Left.Kind = Unlimited then

         return Word_Not_Limited;

      else
         -- Two Finite limits, the smaller Value is better:

         return (
            Kind  => Finite,
            Value => Word_T'Min (Left.Value, Right.Value));

      end if;

   end Or_Min;


   function Or_Max (Left, Right : Word_Limit_T) return Word_Limit_T
   is
   begin

      if Right.Kind = Unlimited or Left.Kind = Unlimited then

         return Word_Not_Limited;

      else
         -- Two Finite limits, the larger Value is better:

         return (
            Kind  => Finite,
            Value => Word_T'Max (Left.Value, Right.Value));

      end if;

   end Or_Max;


   procedure Increase (
      Limit    : in out Word_Limit_T;
      To_Allow : in     Word_T)
   is
   begin

      if not Unlimited (Limit) and then Limit.Value < To_Allow then

         Limit.Value := To_Allow;

      end if;

   end Increase;


   procedure Decrease (
      Limit    : in out Word_Limit_T;
      To_Allow : in     Word_T)
   is
   begin

      if not Unlimited (Limit) and then Limit.Value > To_Allow then

         Limit.Value := To_Allow;

      end if;

   end Decrease;


   --
   ---   Intervals of binary words
   --
   -- These are not yet "bounds", but can be derived from bounds and
   -- used to construct bounds.


   function To_Normal (Item : Word_Interval_T) return Word_Interval_T
   is

      Result : Word_Interval_T := Item;
      -- The interval to be normalized and returned.

      No_Min, No_Max : Boolean;
      -- Whether the Min/Max are unlimited.

   begin

      if not Result.Inside then
         -- An "outside" cointerval can perhaps be normalized to Inside.

         No_Min := Result.Min.Kind = Unlimited
                   or else Result.Min.Value = 0;

         No_Max := Result.Max.Kind = Unlimited
                   or else
                      Result.Max.Value = Arithmetic.Max_Word (Result.Width);

         if No_Min and No_Max then
            -- The outside of the universal interval.

            Result := Void_Word_Interval (Result.Width);

         elsif No_Max then
            -- All values less than Result.Min.

            Result.Max    := (Finite, Result.Min.Value - 1);
            Result.Min    := (Unlimited, 0);
            Result.Inside := True;

         elsif No_Min then
            -- All values greater than Result.Max.

            Result.Min    := (Finite, Result.Max.Value + 1);
            Result.Max    := (Unlimited, Arithmetic.Max_Word (Result.Width));
            Result.Inside := True;

         elsif Result.Min.Value > Result.Max.Value then
            -- All values, period.

            Result := Universal_Word_Interval (Result.Width);

         -- else
         --    Both Min and Max are Finite and 0 < Min <= Max < Max_Word,
         --    so the Result remains "outside".

         end if;

      end if;

      return Result;

   end To_Normal;


   function Void_Word_Interval (Width : Width_T)
   return Word_Interval_T
   is
   begin

      return (
         Min    => (Finite, 1),
         Max    => (Finite, 0),
         Width  => Width,
         Inside => True);

   end Void_Word_Interval;


   function Universal_Word_Interval (Width : Width_T)
   return Word_Interval_T
   is
   begin

      return (
         Min    => (Unlimited, 0),
         Max    => (Unlimited, Arithmetic.Max_Word (Width)),
         Width  => Width,
         Inside => True);

   end Universal_Word_Interval;


   function Exactly_Zero_Word (Width : Width_T)
   return Word_Interval_T
   is
   begin

      return Interval (Min => 0, Max => 0, Width => Width);

   end Exactly_Zero_Word;


   function Positive_Interval (Width : Width_T)
   return Word_Interval_T
   is
   begin

      return Interval (
         Min => 1,
         Max => Arithmetic.Max_Positive (Width),
         Width => Width);

   end Positive_Interval;


   function Eu_Interval (Min, Max : Word_T; Width : Width_T)
   return Word_Interval_T
   is
   begin

      return (
         Min    => (Kind => Finite, Value => Min),
         Max    => (Kind => Finite, Value => Max),
         Width  => Width,
         Inside => True);

   end Eu_Interval;


   function Co_Interval (Min, Max : Word_T; Width : Width_T)
   return Word_Interval_T
   is
   begin

      return To_Normal ((
         Min    => (Finite, Min),
         Max    => (Finite, Max),
         Width  => Width,
         Inside => False));

   end Co_Interval;


   function Unsigned_Limit (Item : Limit_T)
   return Word_Limit_T
   is
   begin

      if Item.Kind = Unlimited then

         return (Unlimited, 0);

      elsif Item.Value >= 0 then

         return (Finite, Word_T (Item.Value));

      end if;

      raise Negative_Limit;

   end Unsigned_Limit;


   function Unsigned_Limit (Value : Value_T)
   return Word_Limit_T
   is
   begin

      if Value >= 0 then

         return (Finite, Word_T (Value));

      end if;

      raise Negative_Limit;

   end Unsigned_Limit;


   function To_Word_Interval (
      Item  : Interval_T;
      Width : Width_T)
   return Word_Interval_T
   is

     Min : Limit_T renames Item.Min;
     Max : Limit_T renames Item.Max;

     MinU, MaxU : Word_T;
     -- The unsigned words for Min.Value and Max.Value.

   begin

      if  Min.Kind = Finite
      and Max.Kind = Finite
      then
         -- The interval Min .. Max.

         MinU := Arithmetic.Unsigned_Word (Min.Value, Width);
         MaxU := Arithmetic.Unsigned_Word (Max.Value, Width);

         if Min.Value >= 0 then

            if Max.Value >= 0 then
               -- The unsigned view agrees with the signed view.

               return (
                  Min    => (Finite, MinU),
                  Max    => (Finite, MaxU),
                  Inside => True,
                  Width  => Width);

            else
               -- Min >= 0 > Max: null interval.

               return Void_Word_Interval (Width);

            end if;

         else
            -- Min < 0.

            if Max.Value >= 0 then
               -- The negative values become large unsigned values
               -- so we get an "outside" interval.

               return (
                  Min    => (Finite, MaxU + 1),
                  Max    => (Finite, MinU - 1),
                  Inside => False,
                  Width  => Width);

            else
               -- Min and Max both negative.

               return (
                  Min    => (Finite, MinU),
                  Max    => (Finite, MaxU),
                  Inside => True,
                  Width  => Width);

            end if;

         end if;

      elsif Min.Kind = Finite then
         -- The interval Min .. inf.

         MinU := Arithmetic.Unsigned_Word (Min.Value, Width);

         if Min.Value >= 0 then
            -- All the non-negative signed values from Min.Value on.

            return (
               Min    => (Finite, MinU),
               Max    => (Finite, Arithmetic.Max_Positive (Width)),
               Inside => True,
               Width  => Width);

         else
            -- Min < 0: The negative values become large unsigned
            -- values.

            return (
               Min    => (Finite, Arithmetic.Max_Positive (Width) + 1),
               Max    => (Finite, MinU - 1),
               Inside => False,
               Width  => Width);

         end if;

      elsif Max.Kind = Finite then
         -- The interval -inf .. Max.

         MaxU := Arithmetic.Unsigned_Word (Max.Value, Width);

         if Max.Value < 0 then
            -- All the negative signed values up to Max.Value.

            return (
               Min    => (Finite, Arithmetic.Max_Positive (Width) + 1),
               Max    => (Finite, MaxU),
               Inside => True,
               Width  => Width);

         else
            -- Max >= 0: All negative signed values and some positive ones.

            return (
               Min    => (Finite, MaxU + 1),
               Max    => (Finite, Arithmetic.Max_Positive (Width)),
               Inside => False,
               Width  => Width);

         end if;

      else
         -- The unlimited interval.

         return Universal_Word_Interval (Width);

      end if;

   end To_Word_Interval;


   function Rest_Below (Limit : Word_T; From : Word_Interval_T)
   return Word_Interval_T
   --
   -- The interval that contains those values From a given
   -- interval that are less than a given Limit.
   --
   is
   begin

      if Limit = 0 then
         -- Nothing can be below.

         return Void_Word_Interval (From.Width);

      else

         return From and Interval (0, Limit - 1, From.Width);

      end if;

   end Rest_Below;


   function Rest_Above (Limit : Word_T; From : Word_Interval_T)
   return Word_Interval_T
   --
   -- The interval that contains those values From a given
   -- interval that are greater than a given Limit.
   --
   is

      MaxW : constant Word_T := Arithmetic.Max_Word (From.Width);

   begin

      if Limit = MaxW then
         -- Nothing can be above.

         return Void_Word_Interval (From.Width);

      else

         return From and Interval (Limit + 1, MaxW, From.Width);

      end if;

   end Rest_Above;


   function Low_End (Cointerval : Word_Interval_T)
   return Word_Interval_T
   --
   -- The low end 0 .. Cointerval.Min - 1.
   --
   -- Precondition (not checked): not Cointerval.Inside.
   --
   is
   begin

      return (
         Min    => (Unlimited, 0),
         Max    => (Finite, Cointerval.Min.Value - 1),
         Width  => Cointerval.Width,
         Inside => True);

   end Low_End;


   function High_End (Cointerval : Word_Interval_T)
   return Word_Interval_T
   --
   -- The high end Cointerval.Max + 1 .. Max_Word (Width).
   --
   -- Precondition (not checked): not Cointerval.Inside.
   --
   is
   begin

      return (
         Min    => (Finite   , Cointerval.Max.Value + 1),
         Max    => (Unlimited, Arithmetic.Max_Word (Cointerval.Width)),
         Width  => Cointerval.Width,
         Inside => True);

   end High_End;


   function Min (Item : Word_Interval_T) return Word_T
   is
   begin

      return Value (Item.Min);

   end Min;


   function Max (Item : Word_Interval_T) return Word_T
   is
   begin

      return Value (Item.Max);

   end Max;


   function Signed_Min (Item : Word_Interval_T) return Value_T
   is
   begin

      return Arithmetic.Signed_Value (
         Word  => Value (Item.Min),
         Width => Item.Width);

   end Signed_Min;


   function Signed_Max (Item : Word_Interval_T) return Value_T
   is
   begin

      return Arithmetic.Signed_Value (
         Word  => Value (Item.Max),
         Width => Item.Width);

   end Signed_Max;


   function Singleton (Value : Word_T; Width : Width_T)
   return Word_Interval_T
   is
   begin

      return Interval (Min => Value, Max => Value, Width => Width);

   end Singleton;


   function One_Or_All (Value : Word_Limit_T; Width : Width_T)
   return Word_Interval_T
   is
   begin

      return (Min => Value, Max => Value, Width => Width, Inside => True);

   end One_Or_All;


   function Void (Item : Word_Interval_T) return Boolean
   is
   begin

      return     (Item.Min.Kind = Finite and Item.Max.Kind = Finite)
         and then Item.Min.Value          >  Item.Max.Value;

   end Void;


   function Known (Interval : Word_Interval_T) return Boolean
   is
   begin

      return Known (Interval.Min) or Known (Interval.Max);

   end Known;


   function Bounded (Interval : Word_Interval_T) return Boolean
   is
   begin

      return Known (Interval.Min) and Known (Interval.Max);

   end Bounded;


   function Universal (Interval : Word_Interval_T) return Boolean
   is
   begin

      return Interval.Inside
      and then (Unlimited (Interval.Min)
         or else Interval.Min.Value = 0)
      and then (Unlimited (Interval.Max)
         or else Interval.Max.Value = Arithmetic.Max_Word (Interval.Width));

   end Universal;


   procedure Get_Limits (
      Item : in     Word_Interval_T;
      Lo   :    out Word_T;
      Hi   :    out Word_T)
   --
   -- The limits Lo .. Hi of the given non-void interval.
   --
   -- > For an eu-interval, Lo and Hi are just the Min and Max if
   --   finite, or 0 and Max_Word (Item.Width) if unlimited.
   --
   -- > For a co-interval, Lo and Hi are such that the co-interval
   --   is equivalent to eu-interval Lo .. Hi + Max_Word + 1,
   --   modulo Max_Word + 1.
   --
   -- Going the other way, given Lo and Hi, the corresponding
   -- Interval_T is derived as follows (see function Limited_Interval):
   --
   -- > If Lo <= Hi, it is the eu-interval Lo .. Hi.
   --
   -- > If Lo > Hi, there are two cases:
   --   o  if Lo - Hi > 1, it is the co-interval "not Hi + 1 .. Lo - 1".
   --   o  if Lo - Hi = 1, it is the universal interval.
   --
   is
   begin

     if Item.Inside then
         -- An eu-interval; Min and Max may be finite or unlimited.
         -- However, Min <= Max because the interval is not void.

         case Item.Min.Kind is
         when Unlimited => Lo := 0;
         when Finite    => Lo := Item.Min.Value;
         end case;

         case Item.Max.Kind is
         when Unlimited => Hi := Arithmetic.Max_Word (Item.Width);
         when Finite    => Hi := Item.Max.Value;
         end case;

      else
         -- A co-interval; Min and Max are finite and
         -- moreover Min > 0 and Max < Max_Word.

         Lo := Item.Max.Value + 1;
         Hi := Item.Min.Value - 1;

      end if;

   end Get_Limits;


   function Limited_Interval (
      Lo, Hi : Word_T;
      Width  : Width_T)
   return Word_Interval_T
   --
   -- The eu-interval or co-interval that corresponds to the
   -- non-modular interval Lo .. Hi, if Lo <= Hi, or the
   -- non-modular interval Lo .. Hi + 2**Width, if Lo > Hi.
   --
   is
   begin

      if Lo <= Hi then

         return (
            Min    => (Finite, Lo),
            Max    => (Finite, Hi),
            Width  => Width,
            Inside => True);

      elsif Lo - Hi > 1 then

         return To_Normal ((
            Min    => (Finite, Hi + 1),
            Max    => (Finite, Lo - 1),
            Width  => Width,
            Inside => False));

      else
         -- Lo = Hi + 1.

         return Universal_Word_Interval (Width);

      end if;

   end Limited_Interval;


   function Length (Item : Word_Interval_T) return Word_T
   is

      Lo, Hi : Word_T;
      -- The finite limits of the (eu-) interval.

   begin

      Get_Limits (Item => Item, Lo => Lo, Hi => Hi);

      return (Hi - Lo) and Arithmetic.Max_Word (Item.Width);

   end Length;


   function Is_In (
      Value    : Word_T;
      Interval : Word_Interval_T)
   return Boolean
   is
   begin

      return (not Interval.Inside) xor (
         (not Known (Interval.Min) or else Value >= Interval.Min.Value)
         and then
         (not Known (Interval.Max) or else Value <= Interval.Max.Value));

   end Is_In;


   function "<" (Value : Word_T; Interval : Word_Interval_T)
   return Boolean
   is
   begin

      return   Interval.Inside
      and then Interval.Min.Kind = Finite
      and then Interval.Min.Value > Value;

   end "<";


   function Singular (Interval : Word_Interval_T)
   return Boolean
   is
   begin

      return   Interval.Inside
      and then Bounded (Interval)
      and then Interval.Min.Value = Interval.Max.Value;

   end Singular;


   function Single_Value (Interval : Word_Interval_T)
   return Word_T
   is
   begin

      if Singular (Interval) then

         return Interval.Min.Value;
         -- Same as Interval.Max.Value.

      else

         raise Multiple_Values;

      end if;

   end Single_Value;


   function Signed_Single_Value (Interval : Word_Interval_T)
   return Value_T
   is
   begin

      return Arithmetic.Signed_Value (
         Word  => Single_Value (Interval),
         Width => Interval.Width);

   end Signed_Single_Value;


   function Is_Only (Value : Word_T; Interval : Word_Interval_T)
   return Boolean
   is
   begin

      return (Interval.Inside
         and  Interval.Min.Kind = Finite
         and  Interval.Max.Kind = Finite)
         and then (
             Interval.Min.Value = Value
         and Interval.Max.Value = Value);

   end Is_Only;


   function Is_Only_Zero (Item : Word_Interval_T) return Boolean
   is
   begin

      return Is_Only (Value => 0, Interval => Item);

   end Is_Only_Zero;


   procedure Report_Width_Mismatch (
      Left, Right : in Word_Interval_T;
      Where       : in String)
   --
   -- Reports the fault the Left.Width /= Right.Width.
   --
   is
   begin

      Output.Fault (
         Location => "Storage.Bounds." & Where,
         Text     =>
              "Width mismatch,"
            & Width_T'Image (Left.Width)
            & " /="
            & Width_T'Image (Right.Width));

   end Report_Width_Mismatch;


   function Inside_And_Outside (Left, Right : Word_Interval_T)
   return Word_Interval_T
   --
   -- The intersection of an eu-interval Left and a co-interval Right.
   -- The result is an eu-interval unless Left is the universal interval.
   -- If Left is not universal, but intersects both the low and high
   -- ends of Right, the result is an overestimate because it equals Left
   -- and thus includes the complement of Right.
   --
   -- Preconditions (not checked):
   --    Left.Inside
   --    not Right.Inside
   --    Left.Width = Right.Width
   --
   is

      Result : Word_Interval_T;
      -- The result.

   begin

      if Universal (Left) then
         -- Left is universal; the result is the Right cointerval.

         Result := Right;

      else

         Result := Left;
         -- The Right constraints will be added below.

         if Known (Left.Min) and then Left.Min.Value >= Right.Min.Value then
            -- The low end of the Right cointerval falls outside
            -- the Left interval. Only the high end of the Right
            -- cointerval can contribute.

            Result.Min := (
               Kind  => Finite,
               Value => Word_T'Max (Left.Min.Value, Right.Max.Value + 1));

         -- else
         --    The Left interval shares some values with the low end
         --    of the Right cointerval. Result.Min is Left.Min.

         end if;

         if Known (Left.Max) and then Left.Max.Value <= Right.Max.Value then
            -- The high end of the Right cointerval falls outside
            -- the Left interval. Only the low end of the Right
            -- cointerval can contribute.

            Result.Max := (
               Kind  => Finite,
               Value => Word_T'Min (Left.Max.Value, Right.Min.Value - 1));

         -- else
         --    The Left interval shares some values with the high end
         --    of the Right cointerval. Result.Max is Left.Max.

         end if;

      end if;

      return Result;

   end Inside_And_Outside;


   function "and" (Left, Right : Word_Interval_T) return Word_Interval_T
   is
   begin

      if Left.Width /= Right.Width then

         Report_Width_Mismatch (Left, Right, """and""");

      end if;

      if Left.Inside then

         if Right.Inside then
            -- Inside "and" Inside.

            return (
               Min    => And_Min (Left.Min, Right.Min),
               Max    => And_Max (Left.Max, Right.Max),
               Width  => Left.Width,
               Inside => True);

         else
            -- Inside "and" outside.

            return Inside_And_Outside (Left, Right);

         end if;

      else

         if Right.Inside then
            -- Outside "and" Inside.

            return Inside_And_Outside (Right, Left);

         else
            -- Outside "and" outside.
            -- There are two cases: whether the Left and Right gaps
            -- intersect or not.

            if Left.Max.Value  < Right.Min.Value
            or Right.Max.Value < Left.Min.Value
            then
               -- The gaps do not intersect, and thus the exact
               -- intersection has two (both) gaps. This cannot be
               -- represented exactly as an Interval_T; the contents
               -- of one gap must be included in the Interval_T.
               -- Including the smaller gap minimizes the over-estimation.

               if    Left.Max.Value  - Left.Min.Value
                  <  Right.Max.Value - Right.Min.Value
               then
                  -- The Left gap is smaller.

                  return Right;

               else
                  -- The Right is smaller, or no larger.

                  return Left;

               end if;

            else
               -- The Left and Right gaps intersect, and thus become
               -- only one gap in the intersection, which can be
               -- represented exactly as a co-interval.

               return (
                  Min => (
                     Kind  => Finite,
                     Value => Word_T'Min (Left.Min.Value, Right.Min.Value)),
                  Max => (
                     Kind  => Finite,
                     Value => Word_T'Max (Left.Max.Value, Right.Max.Value)),
                  Width  => Left.Width,
                  Inside => False);
               --
               -- The result is a cointerval that contains both zero
               -- and Max_Word (Width), because Left and Right are such.
               -- Thus there is no need to normalize the result.

            end if;

         end if;

      end if;

   end "and";


   function Inside_Or_Outside (Left, Right : Word_Interval_T)
   return Word_Interval_T
   --
   -- The union of an Inside interval, Left, and an "outside"
   -- cointerval, Right. The result is an "outside" cointerval unless
   -- it is the universal interval.
   --
   -- Preconditions (not checked):
   --    Left.Inside
   --    not Void (Left)
   --    not Right.Inside
   --    Left.Width = Right.Width
   --
   is

      Min_In : constant Boolean := Is_In (Right.Min.Value, Left);
      Max_In : constant Boolean := Is_In (RIght.Max.Value, Left);

      Result : Word_Interval_T;
      -- The result.

   begin

      if Min_In and Max_In then
         -- The Left interval covers the gap in the Right cointerval.

         Result := Universal_Word_Interval (Left.Width);

      elsif    Known (Left.Min) and then Left.Min.Value > Right.Min.Value
      and then Known (Left.Max) and then Left.Max.Value < Right.Max.Value
      then
         -- The Left interval lies inside the Right gap, without
         -- filling either end of the gap. The exact union has two
         -- gaps; we must return an over-estimation.

         Result := Universal_Word_Interval (Left.Width);

      else
         -- The Left interval lies entirely in the low or high part
         -- of the Right cointerval, or covers part of the gap but
         -- not all of the gap.

         Result := Right;
         -- The Left constraints will be added below.

         if Min_In then
            -- The Left interval fills the low end of the Right gap,
            -- but not the whole gap.

            Result.Min := (
               Kind  => Finite,
               Value => Left.Max.Value + 1);

         elsif Max_In then
            -- The Left interval fills the high end of the Right gap,
            -- but not the whole gap.

            Result.Max := (
               Kind  => Finite,
               Value => Left.Min.Value - 1);

         end if;

      end if;

      return Result;

   end Inside_Or_Outside;


   function Or_Disjoint (Left, Right : Word_Interval_T)
   return Word_Interval_T
   --
   -- The union of two disjoint eu-intervals Left and Right, where
   -- Left.Max < Right.Min. The result is usually an over-estimate,
   -- including the extra values Left.Max + 1 .. Right.Min - 1, but
   -- one special case may be represented as an exact eu-interval
   -- and another as an exact co-interval.
   --
   -- Preconditions (not checked):
   --    Left.Inside and Right.Inside
   --    Left.Max.Kind = Right.Min.Kind = Finite
   --    Left.Max.Value < Right.Min.Value
   --    Left.Width = Right.Width.
   --
   is
   begin

      if  (Left.Min.Kind = Unlimited
           or else Left.Min.Value = 0)
      and (Right.Max.Kind = Unlimited
           or else Right.Max.Value = Arithmetic.Max_Word (Right.Width))
      and  Left.Max.Value + 1 < Right.Min.Value
      then
         -- The union of Left and Right consists of all values except
         -- Left.Max + 1 .. Right.Min - 1. This is a co-interval.

         return (
            Min    => (Finite, Left.Max.Value + 1),
            Max    => (Finite, Right.Min.Value - 1),
            Width  => Left.Width,
            Inside => False);

      else
         -- The closest approximation to the union is the eu-interval
         -- Left.Min .. Right.Max, which is usually an over-estimate
         -- but is exact if Left.Max + 1 = Right.Min.

         return (
            Min    => Left.Min,
            Max    => Right.Max,
            Width  => Left.Width,
            Inside => True);

      end if;

   end Or_Disjoint;


   function "or" (Left, Right : Word_Interval_T) return Word_Interval_T
   is
   begin

      if Left.Width /= Right.Width then

         Report_Width_Mismatch (Left, Right, """or""");

      end if;

      if Left.Inside then

         if Void (Left) then

            return Right;

         elsif Right.Inside then
            -- Inside (non-void) "or" inside.

            if Void (Right) then

               return Left;

            else
               -- Inside (non-void) "or" inside (non-void).

               if (Left.Max.Kind = Finite and Right.Min.Kind = Finite)
               and then Left.Max.Value < Right.Min.Value
               then
                  -- Left is entirely < Right.

                  return Or_Disjoint (Left, Right);

               elsif (Right.Max.Kind = Finite and Left.Min.Kind = Finite)
               and then Right.Max.Value < Left.Min.Value
               then
                  -- Right is entirely < Left.

                  return Or_Disjoint (Right, Left);

               else
                  -- Left and Right intersect.

                  return (
                     Min    => Or_Min (Left.Min, Right.Min),
                     Max    => Or_Max (Left.Max, Right.Max),
                     Width  => Left.Width,
                     Inside => True);

               end if;

            end if;

         else
            -- Inside (non-void) "or" outside.

            return Inside_Or_Outside (Left, Right);

         end if;

      else

         if Right.Inside then
            -- Outside "or" inside.

            if Void (Right) then

               return Left;

            else
               -- Outside "or" inside (non-void).

               return Inside_Or_Outside (Right, Left);

            end if;

         else
            -- Outside "or" outside.

            return To_Normal ((
               Min => (
                  Kind  => Finite,
                  Value => Word_T'Max (Left.Min.Value, Right.Min.Value)),
               Max => (
                  Kind  => Finite,
                  Value => Word_T'Min (Left.Max.Value, Right.Max.Value)),
               Width  => Left.Width,
               Inside => False));
            --
            -- The result may be the universal interval, which To_Normal
            -- will detect and change to an Inside universal interval.

         end if;

      end if;

   end "or";


   function "not" (Item : Word_Interval_T) return Word_Interval_T
   is
   begin

      return (
         Min    => Item.Min,
         Max    => Item.Max,
         Width  => Item.Width,
         Inside => not Item.Inside);

   end "not";


   procedure Widen (
      Interval   : in out Word_Interval_T;
      To_Contain : in     Word_T)
   is
   begin

      if Is_In (To_Contain, Interval) then
         -- Not a thing to do.

         null;

      elsif Void (Interval) then

         Interval := Singleton (Value => To_Contain, Width => Interval.Width);

      elsif Interval.Inside then

         Decrease (Interval.Min, To_Contain);

         Increase (Interval.Max, To_Contain);

      else
         -- The Interval is an "outside" cointerval and To_Contain
         -- lies in the gap, Min .. Max.

         if To_Contain - Interval.Min.Value <= Interval.Max.Value - To_Contain
         then
            -- The new value is closer to the Min end, so we extend that.

            Interval.Min.Value := To_Contain + 1;

         else
            -- The new value is closer to the Max end, so we extend that.

            Interval.Max.Value := To_Contain - 1;

         end if;

         -- Perhaps the gap was closed?

         if Interval.Min.Value > Interval.Max.Value then

            Interval := Universal_Word_Interval (Interval.Width);

         end if;

      end if;

   end Widen;


   function "<=" (Left, Right : Word_Interval_T) return Boolean
   is
   begin

      if Void (Left) then

         return True;

      elsif Left.Inside then

         if Right.Inside then
            -- Inside (non-void) "<=" Inside.

            return
               (Right.Min.Kind = Unlimited
                or else (Left.Min.Kind = Finite
                         and then Left.Min.Value >= Right.Min.Value))
            and
               (Right.Max.Kind = Unlimited
                or else (Left.Max.Kind = Finite
                         and then Left.Max.Value <= Right.Max.Value));

         else
            -- Inside (non-void) "<=" outside.
            -- Left is a subset of Right iff Left is a subset of the
            -- low end of Right or a subset of the high end of Right.
            -- The two disjuncts below check these two cases.

            return
               (Left.Max.Kind = Finite
                and then Left.Max.Value < Right.Min.Value)
            or
               (Left.Min.Kind = Finite
                and then Left.Min.Value > Right.Max.Value);

         end if;

      else

         if Right.Inside then
            -- Outside "<=" Inside.
            -- This happens iff the Inside interval is universal.

            return Universal (Right);

         else
            -- Outside "<=" outside.

            return Left.Min.Value <= Right.Min.Value
            and    Left.Max.Value >= Right.Max.Value;

         end if;

      end if;

   end "<=";


   function Intersects (Left, Right : Word_Interval_T) return Boolean
   is
   begin

      return not Void (Left and Right);

   end Intersects;


   function "+" (Left : Word_Interval_T; Right : Word_T)
   return Word_Interval_T
   is

      MaxW : constant Word_T := Arithmetic.Max_Word (Left.Width);
      -- The maximum value of this width.

      Lo, Hi : Word_T;
      -- The finite limits of the Left interval.

      Sum : Word_Interval_T;
      -- The result.

   begin

      if Void (Left) or Right = 0 then
         -- No change.

         Sum := Left;

      else
         -- The sum is not void.

         Get_Limits (Item => Left, Lo => Lo, Hi => Hi);

         Sum := Limited_Interval (
            Lo    => (Lo + Right) and MaxW,
            Hi    => (Hi + Right) and MaxW,
            Width => Left.Width);

         -- Transfer Unlimited ends to the Sum:

         if Sum.Inside then

            if Left.Min.Kind = Unlimited then

               Sum.Min := (Unlimited, 0);

            end if;

            if Left.Max.Kind = Unlimited then

               Sum.Max := (Unlimited, MaxW);

            end if;

         -- else
         --    A co-interval is always returned in the Finite form.
         --    TBD normalisation to an unlimited eu-interval if indicated.

         end if;

      end if;

      return Sum;

   end "+";


   function "-" (Left : Word_Interval_T; Right : Word_T)
   return Word_Interval_T
   is
   begin

      return Left + Arithmetic.Opposite_Sign (Right, Left.Width);

   end "-";


   function "+" (Left, Right : Word_Interval_T) return Word_Interval_T
   is

      MaxW : constant Word_T := Arithmetic.Max_Word (Left.Width);
      -- The maximum value of this width.

      Left_Lo, Left_Hi, Right_Lo, Right_Hi : Word_T;
      -- Left and Right converted to Lo .. Hi eu-intervals.

      Sum : Word_Interval_T;
      -- The result.

   begin

      if Left.Width /= Right.Width then

         Report_Width_Mismatch (Left, Right, """+""");

      end if;

      -- Compute the finite Sum:

      if Void (Left) or Void (Right) then
         -- A sum with an empty component is empty.

         Sum := Void_Word_Interval (Left.Width);

      elsif Length (Left) >= MaxW - Length (Right) then
         -- The length of the sum interval is >= MaxW, so
         -- it contains all numbers, thus is universal.

         Sum := (
            Min    => (Finite, 0),
            Max    => (Finite, MaxW),
            Width  => Left.Width,
            Inside => True);

      else
         -- The sum interval is neither void nor universal.

         Get_Limits (Item => Left , Lo => Left_Lo , Hi => Left_Hi );
         Get_Limits (Item => Right, Lo => Right_Lo, Hi => Right_Hi);

         Sum := Limited_Interval (
            Lo    => (Left_Lo + Right_Lo) and MaxW,
            Hi    => (Left_Hi + Right_Hi) and MaxW,
            Width => Left.Width);

      end if;

      -- Transfer Unlimited ends to the Sum:

      if Sum.Inside then

        if Left.Min.Kind = Unlimited or Right.Min.Kind = Unlimited then

           Sum.Min := (Unlimited, 0);

        end if;

        if Left.Max.Kind = Unlimited or Right.Max.Kind = Unlimited then

           Sum.Max := (Unlimited, MaxW);

        end if;

      -- else
      --    A co-interval is always returned in the Finite form.
      --    TBD normalisation to an unlimited eu-interval if indicated.

      end if;

      return Sum;

   end "+";


   function "-" (Item : Word_Interval_T) return Word_Interval_T
   is

      Lo, Hi : Word_T;
      -- The finite limits of the interval.

   begin

      if Void (Item) then

         return Void_Word_Interval (Item.Width);

      else

         Get_Limits (Item => Item, Lo => Lo, Hi => Hi);

         return Limited_Interval (
            Lo    => Arithmetic.Opposite_Sign (Hi, Item.Width),
            Hi    => Arithmetic.Opposite_Sign (Lo, Item.Width),
            Width => Item.Width);

      end if;

   end "-";


   function "-" (Left, Right : Word_Interval_T) return Word_Interval_T
   is
   begin

      return Left + (-Right);

   end "-";


   function Fixed_Image (
      Item   : Word_Limit_T;
      Prefix : String := "";
      Suffix : String := "")
   return String
   --
   -- If the limit is known, returns the limiting value,
   -- between Prefix and Suffix.
   -- Otherwise returns the null string.
   --
   is
   begin

      case Item.Kind is

      when Finite =>

         return Prefix & Arithmetic_Base.Image (Item.Value) & Suffix;

      when Unlimited =>

         return "";

      end case;

   end Fixed_Image;


   function Image (Item : Word_Interval_T) return String
   is
   begin

      if Item.Inside then

         if Singular (Item) then

            return Image (Item.Min);

         else

	    return Image (Item => Item.Min, Unknown => "-inf")
                 & " .. "
                 & Image (Item => Item.Max, Unknown => "+inf");

         end if;

      else
         -- An "outside" cointerval.

         if Item.Min.Value = Item.Max.Value then

            return "not " & Image (Item.Min);

         else

	    return "not "
                 & Image (Item => Item.Min)
                 & " .. "
                 & Image (Item => Item.Max);

         end if;

     end if;

   end Image;


   function Image (
      Item : Word_Interval_T;
      Name : String)
   return String
   is
   begin

      if Item.Inside then

         if Singular (Item) then

            return
                 Name
               & Fixed_Image (Item.Min, Prefix => "=");

         else

            return
                 Fixed_Image (Item.Min, Suffix => "<=")
               & Name
               & Fixed_Image (Item.Max, Prefix => "<=");

         end if;

      else
         -- An "outside" cointerval.

         if Item.Min.Value = Item.Max.Value then

            return Name & "/=" & Image (Item.Min);

         else

	    return Name
                 & " not "
                 & Image (Item => Item.Min)
                 & " .. "
                 & Image (Item => Item.Max);

         end if;

      end if;

   end Image;


   function Image (
      Item : Word_Interval_T;
      Cell : Cell_T)
   return String
   is
   begin

      return Image (Item, Image (Cell));

   end Image;


   function Single_Value (Intervals : Word_Interval_List_T)
   return Word_List_T
   is

      Values : Word_List_T (Intervals'Range);
      -- Holds the result.

   begin

      for I in Intervals'Range loop

         Values(I) := Single_Value (Intervals(I));

      end loop;

      return Values;

   end Single_Value;


   function Image (
      Item   : Word_Interval_List_T;
      Prefix : String)
   return String
   is
   begin

      if Item'Length = 0 then

         return "";

      elsif Item'Length = 1 then

         return
            Image (
               Item => Item(Item'First),
               Name => Prefix & Output.Image (Item'First));

      else

         return
            Image (
               Item => Item(Item'First),
               Name => Prefix & Output.Image (Item'First))
            & ", "
            & Image (Item(Item'First + 1 .. Item'Last), Prefix);

      end if;

   end Image;


   --
   ---   Intervals of binary words for cells
   --
   -- These are not yet "bounds", but can be derived from bounds and can
   -- be used to construct bounds.


   function Empty return Cell_Word_Interval_List_T
   is

      None : Cell_Word_Interval_List_T (1..0);

   begin

      return None;

   end Empty;


   function Interval (
      Cell : Storage.Cell_T;
      From : Cell_Word_Interval_List_T)
   return Word_Interval_T
   is

      Inter : Word_Interval_T := Universal_Word_Interval (Width_Of (Cell));
      -- The bounds on the cell, known so far.

   begin

      for F in From'Range loop

         if From(F).Cell = Cell then

            Inter := Inter and From(F).Interval;

         end if;

      end loop;

      return Inter;

   end Interval;


   function Intervals_For_Cells (
      Cells : Cell_Set_T;
      From  : Cell_Word_Interval_List_T)
   return Cell_Word_Interval_List_T
   is

      List : Cell_Word_Interval_List_T (1 .. From'Length);
      Last : Natural := 0;
      -- The result will be List(1 .. Last).

   begin

      for F in From'Range loop

         if Storage.Is_Member (From(F).Cell, Cells) then

            Last := Last + 1;
            List(Last) := From(F);

         end if;

      end loop;

      return List(1..Last);

   end Intervals_For_Cells;


   function "and" (Left, Right : Cell_Word_Interval_List_T)
   return Cell_Word_Interval_List_T
   is

      Conj : Cell_Word_Interval_List_T (1 .. Left'Length + Right'Length);
      Last : Natural := 0;
      -- The conjoined bounds are returned in Conj(1..Last).


      procedure Conjoin (Item : Cell_Word_Interval_T)
      --
      -- Adds a bound to Conj, conjoining it with any existing
      -- bound on the same cell.
      --
      is
      begin

         for C in 1 .. Last loop

            if Conj(C).Cell = Item.Cell then

               Conj(C).Interval := Conj(C).Interval and Item.Interval;

               return;

            end if;

         end loop;

         -- It is a new cell.

         Last := Last + 1;
         Conj(Last) := Item;

      end Conjoin;


   begin  -- "and"

      for L in Left'Range loop
         Conjoin (Left(L));
      end loop;

      for R in Right'Range loop
         Conjoin (Right(R));
      end loop;

      return Conj(1..Last);

   end "and";


   function Image (Item : Cell_Word_Interval_T) return String
   is
   begin

      return Image (
         Item => Item.Interval,
         Name => Storage.Image (Item.Cell));

   end Image;


   function Image (Item : Cell_Word_Interval_List_T) return String
   is
   begin

      if Item'Length = 0 then

         return "none";

      elsif Item'Length = 1 then

         return Image (Item(Item'First));

      else

         return
              Image (Item(Item'First))
            & ", "
            & Image (Item(Item'First + 1 .. Item'Last));

      end if;

   end Image;


   function Cells_Of (Item : Cell_Word_Interval_List_T) return Cell_List_T
   is

      Cells : Cell_List_T (Item'Range);

   begin

      for C in Cells'Range loop

         Cells(C) := Item(C).Cell;

      end loop;

      return Cells;

   end Cells_Of;


   --
   ---   Intervals of binary words for variables with moving locations
   --


   function "and" (Left, Right : Var_Word_Interval_List_T)
   return Var_Word_Interval_List_T
   is
      use type Storage.Location_Ref;

      Conj : Var_Word_Interval_List_T (1 .. Left'Length + Right'Length);
      Last : Natural := 0;
      -- The conjoined bounds are returned in Conj(1..Last).


      procedure Conjoin (Item : Var_Word_Interval_T)
      --
      -- Adds a bound to Conj, conjoining it with any existing
      -- bound on the same variable.
      --
      is
      begin

         for C in 1 .. Last loop

            if Conj(C).Location = Item.Location then

               Conj(C).Interval := Conj(C).Interval and Item.Interval;

               return;

            end if;

         end loop;

         -- It is a new cell.

         Last := Last + 1;
         Conj(Last) := Item;

      end Conjoin;


   begin  -- "and"

      for L in Left'Range loop
         Conjoin (Left(L));
      end loop;

      for R in Right'Range loop
         Conjoin (Right(R));
      end loop;

      return Conj(1..Last);

   end "and";


   function Cell_Intervals (
      From  : Var_Word_Interval_List_T;
      Point : Processor.Code_Address_T)
   return Cell_Word_Interval_List_T
   is

      Result : Cell_Word_Interval_List_T (1 .. Storage.Number_Of_Cells);
      -- To hold the resulting Cell Interval List.

      Last : Natural := 0;
      -- The defined Cell Intervals are Result(1 .. Last).

      Loc : Storage.Location_Ref;
      -- The location of one of the variables in From.

      Rebound : Boolean;
      -- Whether the current cell was already bounded in the Result.

   begin

      for F in From'Range loop

         Loc := From(F).Location;

         for L in Loc'Range loop

            if Storage.In_Range (
               Address => Point,
               Rainge  => Loc(L).Address)
            then
               -- Loc(L) locates the bounded value in Loc(L).Cell.

               Rebound := False;

               Find_Earlier_Bound:
               for R in Result'First .. Last loop

                  Rebound := Result(R).Cell = Loc(L).Cell;

                  if Rebound then
                     -- This cell is already bounded. Conjoin the bounds:

                     Result(R).Interval :=
                        Result(R).Interval and From(F).Interval;

                     exit Find_Earlier_Bound;

                  end if;

               end loop Find_Earlier_Bound;

               if not Rebound then
                  -- This is the first bound on this cell.

                  Last := Last + 1;

                  Result(Last) := (
                     Cell     => Loc(L).Cell,
                     Interval => From(F).Interval);

               end if;

            end if;

         end loop;

      end loop;

      return Result(1 .. Last);

   end Cell_Intervals;


   --
   ---  Lists (enumerations) of binary word values
   --


   function Inside_Values (Within : Word_Interval_T) return Word_List_T
   --
   -- Implements Values (Within) for an Inside interval.
   --
   -- Precondition (not checked): Within.Inside.
   --
   is

      Min : constant Word_T := Value (Within.Min);
      Max : constant Word_T := Value (Within.Max);
      -- The limits of the interval.
      -- This raises (and propagates) Unbounded if the limits
      -- are not known.

      Full_Num : constant Natural :=
         Natural'Max (0, Integer (Max - Min + 1));
      -- The full number of values in the interval.

      Num : constant Natural :=
         Natural'Min (Opt.Max_Listed_Values, Full_Num);
      -- The number that we are allowed to list.

      List : Word_List_T (1 .. Num);
      -- The result.

   begin

      if Num < Full_Num then
         -- Too many values in the interval.

         raise Unbounded;

      end if;

      for L in List'Range loop

         List(L) := Min + Word_T (L - List'First);

      end loop;

      return List;

   end Inside_Values;


   function Outside_Values (Within : Word_Interval_T) return Word_List_T
   --
   -- Implements Values (Within) for an "outside" cointerval.
   --
   -- Precondition (not checked): not Within.Inside.
   --
   is
   begin

     return
        Inside_Values ((
           Min    => (Finite, 0),
           Max    => (Finite, Within.Min.Value - 1),
           Width  => Within.Width,
           Inside => True))
        &
        Inside_Values ((
           Min    => (Finite, Within.Max.Value + 1),
           Max    => (Finite, Arithmetic.Max_Word (Within.Width)),
           Width  => Within.Width,
           Inside => True));

   end Outside_Values;


   function Values (Within : Word_Interval_T) return Word_List_T
   is
   begin

      if Within.Inside then

         return Inside_Values (Within);

      else

         return Outside_Values (Within);

      end if;

   end Values;


   function "-" (Left, Right : Word_Interval_T) return Word_List_T
   is
   begin

      return Difference (
         Included => Left,
         Excluded => Right,
         Aligned  => 1);

   end "-";


   function Align_Up (
      Value   : Word_T;
      Aligned : Positive_Word_T)
   return Word_T
   --
   -- Value rounded up to the nearest multiple of Aligned.
   --
   is

      Rest : constant Word_T := Value mod Aligned;

   begin

      if Rest = 0 then

         return Value;

      else

         return Value + Aligned - Rest;

      end if;

   end Align_Up;


   function Align_Down (
      Value   : Word_T;
      Aligned : Positive_Word_T)
   return Word_T
   --
   -- Value rounded down to the nearest multiple of Aligned.
   --
   is
   begin

      return Value - (Value mod Aligned);

   end Align_Down;


   function Inside_Values (
      Within  : Word_Interval_T;
      Aligned : Positive_Word_T)
   return Word_List_T
   --
   -- Implements Values (Within, Aligned) for an Inside interval.
   --
   -- Precondition (not checked): Within.Inside.
   --
   is
      use Arithmetic_Base;

      Min : constant Word_T := Value (Within.Min);
      Max : constant Word_T := Value (Within.Max);
      -- The limits of the interval.
      -- This raises (and propagates) Unbounded if the limits
      -- are not known.

      First : constant Word_T := Align_Up   (Min, Aligned);
      Last  : constant Word_T := Align_Down (Max, Aligned);
      -- The first and last aligned values within the limits.

      Full_Num : constant Natural :=
         Natural'Max (0, Integer ((Last + Aligned - First) / Aligned));
      -- The full number of aligned values in First .. Last.

      Num : constant Natural :=
         Natural'Min (Opt.Max_Listed_Values, Full_Num);
      -- The number that we are allowed to list.

      Values : Word_List_T (1 .. Num);
      -- The values to be returned.

   begin

      if Num < Full_Num then
         -- Too many values in the interval.

         raise Unbounded;

      end if;

      for V in Values'Range loop

         Values(V) := First + Word_T (V - 1) * Aligned;

      end loop;

      return Values;

   end Inside_Values;


   function Outside_Values (
      Within  : Word_Interval_T;
      Aligned : Positive_Word_T)
   return Word_List_T
   --
   -- Implements Values (Within, Aligned) for an "outside" cointerval.
   --
   -- Precondition (not checked): not Within.Inside.
   --
   is
   begin

     return
        Inside_Values (
           Within => (
              Min    => (Finite, 0),
              Max    => (Finite, Within.Min.Value - 1),
              Width  => Within.Width,
              Inside => True),
           Aligned => Aligned)
        &
        Inside_Values (
           Within => (
              Min    => (Finite, Within.Max.Value + 1),
              Max    => (Finite, Arithmetic.Max_Word (Within.Width)),
              Width  => Within.Width,
              Inside => True),
           Aligned => Aligned);

   end Outside_Values;


   function Values (Within : Word_Interval_T; Aligned : Positive_Word_T)
   return Word_List_T
   is
   begin

      if Within.Inside then

         return Inside_Values (Within, Aligned);

      else

         return Outside_Values (Within, Aligned);

      end if;

   end Values;


   function Inside_Difference (
      Included : in Word_Interval_T;
      Excluded : in Word_Interval_T;
      Aligned  : in Positive_Word_T)
   return Word_List_T
   --
   -- Implements the Difference function (see below) when the
   -- two intervals, Included and Excluded, are both eu-intervals.
   --
   -- Precondition (not checked): Included.Inside and Excluded.Inside.
   --
   is
   begin

      return
           Values (
              Within  => Rest_Below (Value (Excluded.Min), Included),
              Aligned => Aligned)
         & Values (
              Within  => Rest_Above (Value (Excluded.Max), Included),
              Aligned => Aligned);

   end Inside_Difference;


   function Difference (
      Included : Word_Interval_T;
      Excluded : Word_Interval_T;
      Aligned  : Positive_Word_T)
   return Word_List_T
   is
   begin

      if Included.Width /= Excluded.Width then

         Report_Width_Mismatch (Included, Excluded, "Difference");

      end if;

      if Included.Inside then

         if Excluded.Inside then
            -- Inside "less" Inside.

            return Inside_Difference (
               Included => Included,
               Excluded => Excluded,
               Aligned  => Aligned);

         else
            -- Inside "less" outside.

            return Values (
               Within  => Included and not Excluded,
               Aligned => Aligned);

         end if;

      else

         if Excluded.Inside then
            -- Outside "less" Inside.

            return
                 Inside_Difference (
                    Included => Low_End (Included),
                    Excluded => Excluded,
                    Aligned  => Aligned)
               & Inside_Difference (
                    Included => High_End (Included),
                    Excluded => Excluded,
                    Aligned  => Aligned);

         else
            -- Outside "less" outside.

            return
                 Values (
                    Within  => Low_End (Included) and not Excluded,
                    Aligned => Aligned)
               & Values (
                    Within  => High_End (Included) and not Excluded,
                    Aligned => Aligned);

         end if;

      end if;

   end Difference;


   --
   ---   Bounds on variables in the abstract
   --


   function Difference (To, From : Cell_T; Under : Bounds_T)
   return Interval_T
   is
   begin

      return Interval (To  , Bounds_T'Class (Under))
           - Interval (From, Bounds_T'Class (Under));

   end Difference;


   function Intervals (Under : Bounds_T)
   return Cell_Interval_List_T
   is

      Cells : constant Cell_List_T := Basis (Bounds_T'Class (Under));
      -- The basis cells.

      Vals : Cell_Interval_List_T (Cells'Range);
      -- The result to be.

   begin

      for V in Vals'Range loop

         Vals(V) := (
            Cell     => Cells(V),
            Interval => Interval (Cells(V), Bounds_T'Class (Under)));

      end loop;

      return Vals;

   end Intervals;


   function Value (Cell : Cell_T; Under : Bounds_T)
   return Value_T
   is

      Inter : constant Interval_T := Interval (Cell, Bounds_T'Class (Under));
      -- The interval allowed for the Cell.

   begin

      if Singular (Inter) then

         return Single_Value (Inter);

      else

         raise Unbounded;

      end if;

   end Value;


   function Values (Cell : Cell_T; Under : Bounds_T)
   return Value_List_T
   is
   begin

      return Values (Interval (Cell, Bounds_T'Class (Under)));

   end Values;


   function Image (Item : Bounds_T) return String
   is
   begin

      return Ada.Tags.Expanded_Name (Bounds_T'Class (Item)'Tag);

   end Image;


   function Full_Image (Item : Bounds_T) return String
   is
   begin

      return Image (Bounds_T'Class (Item))
           & '('
           & Image (Intervals (Bounds_T'Class (Item)))
           & ')';

   end Full_Image;


   procedure Unchecked_Discard
   is new Ada.Unchecked_Deallocation (
      Name   => Bounds_Ref,
      Object => Bounds_T'Class);


   procedure Discard (Item : in out Bounds_Ref)
   is
   begin

      if Opt.Deallocate then

         Unchecked_Discard (Item);

      else

         Item := null;

      end if;

   exception when others => Faults.Deallocation;

   end Discard;


   procedure Discard (List : in out Bounds_List_T)
   is
   begin

      for L in List'Range loop

         Discard (List(L));

      end loop;

   end Discard;


   --
   --    The bottom: no bounds at all.
   --


   function Basis (Item : No_Bounds_T) return Cell_List_T
   is
   begin

      return Null_Cell_List;

   end Basis;


   function Interval (Cell : Cell_T; Under : No_Bounds_T)
   return Interval_T
   is
   begin

      return Universal_Interval;

   end Interval;


   Ur_No_Bounds : constant Bounds_Ref := new No_Bounds_T;
   --
   -- The one and only (suggested) No_Bounds object,


   function No_Bounds return Bounds_Ref
   is
   begin

      return Ur_No_Bounds;

   end No_Bounds;


   --
   ---   Bounds composed of single values
   --


   function Basis (Item : Singular_Bounds_T) return Cell_List_T
   is
   begin

      return Cells_Of (Item.List);

   end Basis;


   function Interval (Cell : Cell_T; Under : Singular_Bounds_T)
   return Interval_T
   is
   begin

      for L in Under.List'Range loop

         if Under.List(L).Cell = Cell then

            return Singleton (Arithmetic.Signed_Value (
                      Word  => Under.List(L).Value,
                      Width => Width_Of (Cell)));

         end if;

      end loop;

      -- This Cell is not in the list.

      return Universal_Interval;

   end Interval;


   function Value (Cell : Cell_T; Under : Singular_Bounds_T)
   return Value_T
   is
   begin

      for L in Under.List'Range loop

         if Under.List(L).Cell = Cell then

            return Arithmetic.Signed_Value (
               Word  => Under.List(L).Value,
               Width => Width_Of (Cell));

         end if;

      end loop;

      -- This Cell is not in the list.

      raise Unbounded;

   end Value;


   function Singular_Bounds (List : Cell_Value_List_T)
   return Singular_Bounds_T
   is
   begin

      return (
         Length => List'Length,
         List   => List);

   end Singular_Bounds;


   --
   --    Bounds composed of intervals
   --


   function Basis (Item : Interval_Bounds_T) return Cell_List_T
   is

      Cells : Cell_List_T (1 .. Item.Length);
      -- The result.

   begin

      for C in Cells'Range loop

         Cells(C) := Item.List(C).Cell;

      end loop;

      return Cells;

   end Basis;


   function Interval (Cell : Cell_T; Under : Interval_Bounds_T)
   return Interval_T
   is
   begin

      return Interval (Cell, Under.List);

   end Interval;


   function Interval_Bounds (List : Cell_Interval_List_T)
   return Interval_Bounds_T
   is
   begin

      return (
         Length => List'Length,
         List   => List);

   end Interval_Bounds;


   function Interval_Bounds (
      From  : Var_Interval_List_T;
      Point : Processor.Code_Address_T)
   return Bounds_Ref
   is

      Cell_Bounds : constant Cell_Interval_List_T :=
         Cell_Intervals (From, Point);
      -- The bounds at Point on the cells corresponding to the
      -- variables in From.

   begin

      if Cell_Bounds'Length > 0 then
         -- We have some bounds at the Point.

         return new Interval_Bounds_T'(Interval_Bounds (Cell_Bounds));

      else
         -- Nothing. Either From is empty or no variable
         -- in From is mapped to a cell at the given Point.

         return No_Bounds;

      end if;

   end Interval_Bounds;


   --
   --    Boundable things
   --


   procedure Add_Basis_Cells (
      From : in     Boundable_T;
      To   : in out Cell_Set_T)
   is
   begin

      Add (
         Cells => Basis (Boundable_T'Class (From)),
         To    => To);

   end Add_Basis_Cells;


   procedure Add_Basis_Cells (
      From  : in     Boundable_T;
      To    : in out Cell_Set_T;
      Added : in out Boolean)
   is

      Cells : constant Cell_List_T := Basis (Boundable_T'Class (From));
      -- The basis cells.

   begin

      for C in Cells'Range loop

         Add (Cell => Cells(C), To => To, Added => Added);

      end loop;

   end Add_Basis_Cells;


   function Is_Used (Cell : Cell_T; By : Boundable_T) return Boolean
   is
   begin

      return Is_Member (
         Cell    => Cell,
         Of_List => Basis (Boundable_T'Class (By)));


   end Is_Used;


   function Image (Item : Boundable_T) return String
   is
   begin

      return Ada.Tags.Expanded_Name (Boundable_T'Class (Item)'Tag);

   end Image;


   procedure Apply (
      Bounds : in     Bounds_T'Class;
      Upon   : in out Boundable_T;
      Result :    out Resolution_T)
   is
   begin

      Result := Stable;

   end Apply;


end Storage.Bounds;
