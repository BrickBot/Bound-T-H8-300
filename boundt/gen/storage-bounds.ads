-- Storage.Bounds (decl)
--
-- Representing bounds (constraints) on variables and expression values.
--
-- The term "bounds" has here a very general (perhaps even vague) meaning
-- as any constraint on the joint values of a set of program or processor
-- state elements (variables or expressions) valid at some point(s) in
-- the execution of the program.
--
-- Various kinds of data-flow / data-range analysis yield such bounds
-- with different form and precision. For example, a constant-propagation
-- analysis yields bounds that give, for each variable in question, either
-- a single possible value (the constant) or a "not constant" value, while
-- interval analysis provides an interval (min, max) for each variable,
-- perhaps with one or both ends unlimited (infinite).
--
-- The above examples are forms of "bounds" that apply separately to each
-- variable, although the analysis that derived the bounds will take
-- into account the flow of data between variables. There are also more
-- complex forms of bounds that define how variable values are related,
-- for example by stating that the value of variable X is the sum of the
-- values of variables Y and Z.
--
-- This package defines some concrete kinds of bounds, and also a
-- general type of bounds as an extensible type (class), thus
-- allowing several representations of the type, simple or complex.
--
-- Two groups of concrete bounds are defined. The first group applies
-- to signed integer numbers of indefinite width (number of bits), as
-- represented by the type Arithmetic.Value_T. The second group applies
-- to binary words of a specific width, as represented by the types
-- Arithmetic.Word_T and Arithmetic.Width_T. Such words are normally
-- considered unsigned but can be viewed as signed.
--
-- For both groups of concrete bounds (integers/words) the bounds are
-- constructed in two steps. The first step defines a one-sided bound
-- or limit that defines a limiting value that can be used as a lower
-- bound or an upper bound. The second step combines two limits to
-- make an interval with both a lower and an upper bound.
--
-- In the second group, the "interval" type for binary words includes
-- "complement intervals" (co-intervals) that comprise all values less
-- than a first limit or greater than a second (larger) limit. Co-intervals
-- are needed to represent intervals of word values in the signed view.
-- They also close the word-interval domain with respect to the operations
-- of logical (set) complement and modular addition and subtraction.
--
-- In addition to limits and intervals, both groups of concrete bounds
-- (integers/words) include a third type of bound that consists of a
-- list of the possible values of the bounded variable.
--
-- Our main use of bounds (of various forms) is to use them to constrain
-- or refine "boundable" things. The boundable things are elements or
-- parts of our model of a program or an execution of a program. The
-- result of applying some "bounds" to a "boundable" thing is more
-- knowledge (less options) for the nature and effects of the thing,
-- in the limit (best case) a full static characterization of the thing.
--
-- The type "boundable" is defined as an extensible type (class) since
-- we want to bound many different things: dynamic jumps, dynamic data
-- accesses, loop iterations, stack usage, etc.
--
-- The main operation, that of applying "bounds" to a "boundable", is
-- thus doubly dispatching.
--
-- At this top level, not much that is concrete can be said or defined
-- about "bounds" and "boundables", but these top-level definitions
-- provide an (access) type for "boundables" that can be embedded in
-- program and execution models in a uniform way.
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
-- $Revision: 1.22 $
-- $Date: 2015/10/24 19:36:52 $
--
-- $Log: storage-bounds.ads,v $
-- Revision 1.22  2015/10/24 19:36:52  niklas
-- Moved to free licence.
--
-- Revision 1.21  2014/07/01 22:07:01  niklas
-- Added Image functions for Var_Interval(_List)_T.
--
-- Revision 1.20  2013/12/08 20:08:23  niklas
-- Added the function Number_Of_Values.
--
-- Revision 1.19  2012-02-13 17:52:20  niklas
-- BT-CH-0230: Options -max_loop and -max_stack for spurious bounds.
--
-- Revision 1.18  2009-11-27 11:28:07  niklas
-- BT-CH-0184: Bit-widths, Word_T, failed modular analysis.
--
-- Revision 1.17  2008/09/24 08:38:53  niklas
-- BT-CH-0146: Assertions on "loop starts <bound> times".
-- BT-CH-0146: Loop-repeat assertions set both lower and upper bound.
-- BT-CH-0146: Report locations of contradictory "count" assertions.
-- BT-CH-0146: Contradictory "count" assertions imply infeasibility.
--
-- Revision 1.16  2008/06/18 20:52:56  niklas
-- BT-CH-0130: Data pointers relative to initial-value cells.
--
-- Revision 1.15  2008/03/11 22:08:07  niklas
-- BT-CH-0121: Delayed calls and other SHARC support.
--
-- Revision 1.14  2007/12/17 13:54:41  niklas
-- BT-CH-0098: Assertions on stack usage and final stack height, etc.
--
-- Revision 1.13  2007/07/21 18:18:43  niklas
-- BT-CH-0064. Support for AVR/IAR switch-handler analysis.
--
-- Revision 1.12  2007/07/09 13:42:51  niklas
-- Added the Bounds_T primitive functions Intervals and Full_Image.
--
-- Revision 1.11  2007/05/02 09:30:04  niklas
-- Added Exactly_Zero.
--
-- Revision 1.10  2007/04/18 18:34:39  niklas
-- BT-CH-0057.
--
-- Revision 1.9  2007/03/29 15:18:04  niklas
-- BT-CH-0056.
--
-- Revision 1.8  2007/03/18 12:50:41  niklas
-- BT-CH-0050.
--
-- Revision 1.7  2007/01/25 21:25:19  niklas
-- BT-CH-0043.
--
-- Revision 1.6  2006/05/27 21:33:23  niklas
-- Added function Is_In (Value, Interval) for BT-CH-0020.
--
-- Revision 1.5  2006/02/27 10:04:14  niklas
-- Added functions Min (Interval_T) and Max (Interval_T).
--
-- Revision 1.4  2005/03/04 09:32:35  niklas
-- Added functions "+" and "-" to shift limits and intervals
-- up or down by adding a constant value to the bounds.
-- Added function Intersects to show if two intervals meet.
--
-- Revision 1.3  2005/02/23 09:05:21  niklas
-- BT-CH-0005.
--
-- Revision 1.2  2005/02/19 20:34:56  niklas
-- BT-CH-0003.
--
-- Revision 1.1  2005/02/16 21:11:49  niklas
-- BT-CH-0002.
--

--:dbpool with GNAT.Debug_Pools;


with Processor;


package Storage.Bounds is


   Unbounded : exception;
   --
   -- Signals that a variable (cell) is not bounded, by some given
   -- bounds, to the extent required for the current operation to
   -- return the desired result.


   --
   ---   Limits, intervals, and lists for signed integers of indefinite width
   --


   --
   ---   Division with ceiling/floor
   --
   -- Our computations need "floor" and "ceiling" functions, which may
   -- as well be provided in public.


   function Floor (Left, Right : Value_T)
   return Value_T;
   --
   -- Floor (Left / Right), when Right /= 0.
   -- In other words, the largest integer <= Left/Right.


   function Ceil (Left, Right : Value_T)
   return Value_T;
   --
   -- Ceiling (Left / Right), when Right /= 0.
   -- In other words, the smallest integer >= Left/Right.


   --
   ---   Limits (one-sided or on a single value) on signed integers
   --
   -- These are not yet "bounds", but can be derived from "bounds" and
   -- can be used to construct "bounds".


   type Limit_Kind_T is (Unlimited, Finite);
   --
   -- An upper or lower limit on a variable, or a limit that defines
   -- a single value for the variable (depending on context) can be
   -- of two kinds:
   --
   -- Unlimited
   --    No limit is deduced. As far as we know the limit is
   --    minus infinity for a lower limit and plus infinity for
   --    an upper limit, or an unknown value for a single-value limit.
   --
   -- Finite
   --    A limiting (literal) value is deduced.


   type Limit_T is record
      Kind  : Limit_Kind_T := Unlimited;
      Value : Value_T      := 0;
   end record;
   --
   -- One end of a bounded interval, or a possibly defined single value
   -- for a variable.
   --
   -- Whether this is an upper or lower limit or a single-value limit
   -- must be known from context.
   --
   -- The Value is relevant mainly if Kind = Finite, but may also be
   -- important if Kind = Unlimited because it is often set to be the
   -- corresponding infinity (plus or minus).


   Not_Limited : constant Limit_T := (
      Kind  => Unlimited,
      Value => 0);
   --
   -- An unknown limit.


   function Image (Item : Limit_T) return String;
   --
   -- The value for a known limit, "?" otherwise.


   function Image (
      Item    : Limit_T;
      Unknown : String)
   return String;
   --
   -- The value for a known limit, the Unknown string otherwise.


   function Over (Value : Value_T; Max : Limit_T) return Boolean;
   --
   -- Whether the Value is over the (upper) limit Max, which is true
   -- if and only if the Max is known and Value > Max.Value.


   function Under (Value : Value_T; Min : Limit_T) return Boolean;
   --
   -- Whether the Value is under the (lower) limit Min, which is true
   -- if and only if the Min is known and Value < Min.Value.


   function Limit (Value : Value_T) return Limit_T;
   --
   -- The finite limit at Value.


   function Unlimited (L : Limit_T) return Boolean;
   --
   -- Whether the limit is unlimited (infinity).


   function Known (L : Limit_T) return Boolean;
   --
   -- Whether the limit is finite and known (a constant).


   function Value (L : Limit_T) return Value_T;
   --
   -- The value of the limit, assumed to be Known.
   -- If the limit-value is not Known, Unbounded is raised.


   procedure Limit_Value (
      Limit    : in     Limit_T;
      Value    :    out Value_T;
      Is_Const :    out Boolean);
   --
   -- Returns the limiting value if constant, as shown by a True return
   -- value in Is_Const. Otherwise Is_Const is False and Value is
   -- undefined.


   function And_Max (Left, Right : Limit_T) return Limit_T;
   --
   -- Given two upper limits, returns their logical conjunction
   -- or, in other words, the tighter (lesser) limit.


   function And_Min (Left, Right : Limit_T) return Limit_T;
   --
   -- Given two lower limits, returns their logical conjunction
   -- or, in other words, the tighter (greater) limit.


   function Or_Min (Left, Right : Limit_T) return Limit_T;
   --
   -- Given two lower limits, returns their logical disjunction
   -- or, in other words, the looser (lesser) limit.


   function Or_Max (Left, Right : Limit_T) return Limit_T;
   --
   -- Given two upper limits, returns their logical disjunction
   -- or, in other words, the looser (greater) limit.


   procedure Increase (
      Limit    : in out Limit_T;
      To_Allow : in     Value_T);
   --
   -- Increases the upper Limit To_Allow the given value, if the
   -- value is not already within (less or equal to) the Limit.


   procedure Decrease (
      Limit    : in out Limit_T;
      To_Allow : in     Value_T);
   --
   -- Decreases the lower Limit To_Allow the given value, if the
   -- value is not already within (greater or equal to) the Limit.


   function "+" (Left : Limit_T; Right : Value_T)
   return Limit_T;
   --
   -- The Left limit shifted by adding Right to the Value.
   -- If Left is unlimited, so is the result.


   function "-" (Left : Limit_T; Right : Value_T)
   return Limit_T;
   --
   -- The Left limit shifted by subtracting Right from the Value.
   -- If Left is unlimited, so is the result.


   function "-" (Item : Limit_T) return Limit_T;
   --
   -- The limit with the opposite sign to the given limit.


   --
   ---   Intervals of signed integers
   --
   -- These are not yet "bounds", but can be derived from bounds and
   -- used to construct bounds.


   type Interval_T is record
      Min : Limit_T;
      Max : Limit_T;
   end record;
   --
   -- Bounds the value of some value (such as a cell, or an expression)
   -- to an interval Min .. Max.
   -- The value (cell or expression) to which this bound applies
   -- must be known from context.


   Void_Interval : constant Interval_T := (
      Min => (Kind => Finite, Value => 1),
      Max => (Kind => Finite, Value => 0));
   --
   -- A bound that represents the empty set (since Min > Max).


   Universal_Interval : constant Interval_T := (
      Min => (Kind => Unlimited, Value => Value_T'First),
      Max => (Kind => Unlimited, Value => Value_T'Last ));
   --
   -- An interval that accepts any value.
   -- The Min and Max values are defined to make it easier
   -- to intersect this interval with other (bounded) intervals.


   Non_Negative_Interval : constant Interval_T := (
      Min => (Kind => Finite, Value => 0),
      Max => Not_Limited);
   --
   -- An interval that contains all values >= 0.


   Minus_One : constant := -1;
   --
   -- For use in Negative_Interval below.


   Negative_Interval : constant Interval_T := (
      Min => Not_Limited,
      Max => (Kind => Finite, Value => Minus_One));
   --
   -- An interval that contains all values < 0.


   Exactly_Zero : constant Interval_T := (
      Min => (Kind => Finite, Value => 0),
      Max => (Kind => Finite, Value => 0));
   --
   -- The singleton interval that contains only the value 0.


   function Interval (Min, Max : Value_T)
   return Interval_T;
   --
   -- The interval Min .. Max.


   function Min (Item : Interval_T) return Value_T;
   --
   -- Short for Value (Interval.Min). Can raise Unbounded.


   function Max (Item : Interval_T) return Value_T;
   --
   -- Short for Value (Interval.Max). Can raise Unbounded.


   function Singleton (Value : Value_T)
   return Interval_T;
   --
   -- The one-point interval Value .. Value.


   function One_Or_All (Value : Limit_T)
   return Interval_T;
   --
   -- The one-point interval Value .. Value if Value is Known, else
   -- the universal (unlimited) interval.


   function Void (Item : Interval_T) return Boolean;
   --
   -- Whether the interval represents the empty (void) set.
   -- In other words, no value can satisfy both the lower-limit
   -- and the upper-limit.
   -- False is returned for indeterminate cases.


   function Known (Interval : Interval_T) return Boolean;
   --
   -- Whether the interval has finite and known (constant) components
   -- at one or both ends. Note that we do not require both ends
   -- (min and max) to be finite.


   function Bounded (Interval : Interval_T) return Boolean;
   --
   -- Whether both ends of the Interval are known and finite.


   function Universal (Interval : Interval_T) return Boolean;
   --
   -- Whether the Interval is the universal interval. This is the
   -- case if both ends are Unlimited.


   function Number_Of_Values (Within : Interval_T) return Value_T;
   --
   -- The number of values Within the interval. If the interval is
   -- unbounded, or the number of values is too large to fit in Value_T,
   -- the function propagates Unbounded.


   function Is_In (
      Value    : Value_T;
      Interval : Interval_T)
   return Boolean;
   --
   -- Whether the Value lies in the Interval.


   function "<" (Value : Value_T; Interval : Interval_T)
   return Boolean;
   --
   -- Whether the Value is less than the lower bound, Interval.Min.
   -- False if Interval.Min is Unlimited.


   function ">" (Value : Value_T; Interval : Interval_T)
   return Boolean;
   --
   -- Whether the Value is greater than the upper bound, Interval.Max.
   -- False if Interval.Max is unlimited.


   Multiple_Values : exception;
   --
   -- Raised by Single_Value (below) if the given interval does not
   -- define a single value.


   function Singular (Interval : Interval_T)
   return Boolean;
   --
   -- Whether the Interval allows only a single value (Min and Max
   -- are constants and equal). The single value in question can
   -- be retrieved with Single_Value, see below.


   function Single_Value (Interval : Interval_T)
   return Value_T;
   --
   -- If the Interval allows a single value, that is if Singular (Interval)
   -- is True (which means that Min and Max are constants and equal),
   -- returns this value.
   -- Otherwise, that is if Singular (Interval) is False, raises
   -- Multiple_Values.


   function "and" (Left, Right : Interval_T) return Interval_T;
   --
   -- The conjunction (intersection) of two intervals.


   function "or" (Left, Right : Interval_T) return Interval_T;
   --
   -- The disjunction (union) of two intervals.


   procedure Widen (
      Interval   : in out Interval_T;
      To_Contain : in     Value_T);
   --
   -- Widens the Interval To_Contain the given value, if it does
   -- not already contain this value.


   function "<=" (Left, Right : Interval_T) return Boolean;
   --
   -- Whether the Left interval is a subset of the Right interval.
   -- In other words, the Left interval is narrower than (or the
   -- same as) the Right interval. Always True if the Left interval
   -- is Void.


   function Intersects (Left, Right : Interval_T) return Boolean;
   --
   -- Whether the intersection of the Left and Right intervals is
   -- non-empty.


   function "+" (Left : Interval_T; Right : Value_T)
   return Interval_T;
   --
   -- The Left interval shifted by adding Right to the Min and Max limits.
   -- If either Left limit is unlimited, so is the corresponding limit in
   -- the result.


   function "-" (Left : Interval_T; Right : Value_T)
   return Interval_T;
   --
   -- The Left interval shifted by subtracting Right from the Min and
   -- Max limits. If either Left limit is unlimited, so is the
   -- corresponding limit in the result.


   function "+" (Left, Right : Interval_T) return Interval_T;
   --
   -- The interval sum of Left and Right. If either operand is
   -- void, so is the sum. If one end of either operand is unlimited,
   -- so is the corresponding end of the sum.


   function "-" (Item : Interval_T) return Interval_T;
   --
   -- The interval of the opposite sign, converting min .. max
   -- to -max .. -min.


   function "-" (Left, Right : Interval_T) return Interval_T;
   --
   -- The interval difference of Left - Right. If either operand is
   -- void, so is the sum. If one end of either operand is unlimited,
   -- so is the corresponding end of the difference.
   -- Equivalent to Left + (- Right).


   function Image (Item : Interval_T) return String;
   --
   -- The interval as (low limit) .. (high limit).
   -- An unknown (unbounded) limit is written as "-inf" or "+inf".
   -- If the interval is singular, just the single value is shown.


   function Image (
      Item : Interval_T;
      Name : String)
   return String;
   --
   -- The Name is used to denote the value (cell or expression)
   -- to which the bound applies. The result has the form
   --
   --    min <= Name <= max
   --
   -- where "min <= " and/or "<= max" is missing if unbounded.


   function Image (
      Item : Interval_T;
      Cell : Cell_T)
   return String;
   --
   -- Same as Image (Item, Image (Cell)).


   type Interval_List_T is array (Positive range <>) of Interval_T;
   --
   -- A set of interval bounds on values.
   -- The value (cell or expression) to which each interval applies
   -- must be known from context.


   type Value_List_T is array (Positive range <>) of Value_T;
   --
   -- A list of values. The indexing and meaning depend on usage.


   function Single_Value (Intervals : Interval_List_T)
   return Value_List_T;
   --
   -- The list of single values allowed by each of the listed singular
   -- Intervals. If some of the Intervals are not singular the function
   -- propagates Multiple_Values.


   function Image (
      Item   : Interval_List_T;
      Prefix : String)
   return String;
   --
   -- Describes each interval in the list, using a Name for the
   -- bounded value composed from the Prefix and the index of
   -- the interval in the list.


   --
   ---   Intervals of signed integers for cells
   --
   -- These are not yet "bounds", but can be derived from bounds and can
   -- be used to construct bounds.


   type Cell_Interval_T is record
      Cell     : Cell_T;
      Interval : Interval_T;
   end record;
   --
   -- An interval that bounds the value of a cell.


   type Cell_Interval_List_T is array (Positive range <>) of Cell_Interval_T;
   --
   -- A set of interval bounds on cell values, giving both the cells
   -- and the value-interval for each cell.


   function Empty return Cell_Interval_List_T;
   --
   -- An empty (null) list of cell-interval bounds.


   function Interval (
      Cell : Storage.Cell_T;
      From : Cell_Interval_List_T)
   return Interval_T;
   --
   -- The interval bounds on the given Cell, taken From the given list.
   -- If the list has several interval bounds for the cell, their
   -- conjunction (intersection) is returned. If the list has no interval
   -- bounds for the cell, Universal_Interval is returned.


   function Intervals_For_Cells (
      Cells : Cell_Set_T;
      From  : Cell_Interval_List_T)
   return Cell_Interval_List_T;
   --
   -- Returns the interval bounds From the list on the given Cells.
   -- If a cell from Cells occurs several times in the From list, it
   -- will occur as many times in the result.


   function "and" (Left, Right : Cell_Interval_List_T)
   return Cell_Interval_List_T;
   --
   -- The conjunction of the two lists of interval bounds on cells.
   -- If the same cell is mentioned in both lists, it occurs
   -- but once in the result, with the conjunction (intersection)
   -- of the intervals from all its ocurrences.
   -- If a cell is mentioned only in one of the two lists, that list
   -- defines the interval for the cell in the result.


   function Image (Item : Cell_Interval_T) return String;
   --
   -- As in Image (Item => Item.Bound, Name => Image (Item.Cell)).


   function Image (Item : Cell_Interval_List_T) return String;
   --
   -- Describes the bounds as a list of Image (Bound, Image(Cell)).


   function Cells_Of (Item : Cell_Interval_List_T) return Cell_List_T;
   --
   -- All the cells in the given list of cells and intervals, in the
   -- same order and with possible duplicates.


   --
   ---   Intervals of signed integers for variables with moving locations
   --
   -- These are not yet "bounds", nor can they be derived from bounds, but
   -- they can be used to construct bounds when the point in the program is
   -- known so that the variable can be associated with a cell.


   type Var_Interval_T is record
      Location : Location_Ref;
      Interval : Interval_T;
   end record;
   --
   -- Interval bounds on the value of a variable that may occupy different
   -- storage locations at different points in the program.


   function Image (Item : Var_Interval_T)
   return String;
   --
   -- A readable description of the variable and its interval bound.


   type Var_Interval_List_T is array (Positive range <>) of Var_Interval_T;
   --
   -- A set of interval bounds on variable values, giving both the location
   -- of each variable (possibly varying with the execution point) and
   -- the interval that bounds each variable.


   function Image (Item : Var_Interval_List_T)
   return String;
   --
   -- A readable description of the list of variables and their
   -- interval bounds.


   function "and" (Left, Right : Var_Interval_List_T)
   return Var_Interval_List_T;
   --
   -- The conjunction of the two lists of interval bounds in variables.
   -- If the same variable (same location-ref) is mentioned in both lists,
   -- it occurs but once in the result, with the conjunction (intersection)
   -- of the intervals from all its ocurrences.
   -- If a variable is mentioned only in one of the two lists, that list
   -- defines the interval for the variable in the result.
   -- Variables are identified by the locations (reference equality).


   function Cell_Intervals (
      From  : Var_Interval_List_T;
      Point : Processor.Code_Address_T)
   return Cell_Interval_List_T;
   --
   -- Interval bounds on cell values derived From interval bounds on
   -- variables by mapping the location of each variable to a cell at
   -- the given Point in the target program.
   --
   -- If a variable maps to several cells at this point, the bounds for
   -- the variable are applied to each cell in the result.
   --
   -- If a variable maps to no cells at this point, the bounds for the
   -- variable are not used in the result.
   --
   -- If several variables map to the same cell at this point, the bounds
   -- for these variables are conjoined (intersected) to give the bounds
   -- on this cell in the result.


   --
   ---   Lists (enumerations) of signed integer values
   --
   -- These are not yet "bounds" but can be derived from bounds.
   --
   -- Some "bounds" can yield a list of possible values for the
   -- bounded variable(s). This will usually be an ordered list
   -- (ascending order) without duplicates.


   function Values (Within : Interval_T) return Value_List_T;
   --
   -- All the values Within the interval, assuming that the interval
   -- is fully Bounded, else Unbounded is raised.
   --
   -- If the number of values would exceed Opt.Max_Listed_Values the
   -- function raises Unbounded.


   function "-" (Left, Right : Interval_T) return Value_List_T;
   --
   -- All the values allowed by Left, except the values also
   -- allowed by Right. In other words, the difference between
   -- the set defined by Left and the set defined by Right.
   -- All limits involved must be known constants, otherwise
   -- Unbounded is raised.
   --
   -- If the number of values would exceed Opt.Max_Listed_Values the
   -- function raises Unbounded.


   subtype Positive_Value_T is
      Value_T range 1 .. Value_T'Last;
   --
   -- The non-negative values.


   function Values (Within : Interval_T; Aligned : Positive_Value_T)
   return Value_List_T;
   --
   -- All the values that are Within the given interval and are
   -- also integral multiples of Aligned, assuming that the interval
   -- is fully Bounded, else Unbounded is raised.
   -- TBD if the limits must be multiples of Aligned.
   --
   -- If the number of values would exceed Opt.Max_Listed_Values the
   -- function raises Unbounded.


   function Difference (
      Included : in Interval_T;
      Excluded : in Interval_T;
      Aligned  : in Positive_Value_T)
   return Value_List_T;
   --
   -- All the values allowed by the Included interval, except the
   -- values also allowed by the Excluded interval, and considering
   -- only those values that are integral multiples of Aligned.
   -- All limits involved must be known constants, otherwise the
   -- Unbounded exception is raised.
   -- TBD if the limits must be multiples of Aligned.
   --
   -- If the number of values would exceed Opt.Max_Listed_Values the
   -- function raises Unbounded.


   --
   ---   Limits, intervals, and lists for binary words of specific width
   --


   --
   ---   Limits (one-sided or on a single value) for binary words
   --
   -- These are not yet "bounds", but can be derived from "bounds" and
   -- can be used to construct "bounds".


   type Word_Limit_T is record
      Kind  : Limit_Kind_T := Unlimited;
      Value : Word_T       := 0;
   end record;
   --
   -- One end of a bounded interval, or a possibly defined single value
   -- for a variable.
   --
   -- Whether this is an upper or lower limit or a single-value limit
   -- must be known from context.
   --
   -- The Value is relevant mainly if Kind = Finite. The significant width
   -- of the Value is not defined here, nor is its signedness, so they
   -- must be taken from context. The excess (high) bits in Value must
   -- be zero.


   Word_Not_Limited : constant Word_Limit_T := (
      Kind  => Unlimited,
      Value => 0);
   --
   -- An unknown limit.


   function Image (Item : Word_Limit_T) return String;
   --
   -- The value for a known limit, "?" otherwise.


   function Image (
      Item    : Word_Limit_T;
      Unknown : String)
   return String;
   --
   -- The value for a known limit, the Unknown string otherwise.


   function Limit (Value : Word_T) return Word_Limit_T;
   --
   -- The finite limit at Value.


   function Unlimited (L : Word_Limit_T) return Boolean;
   --
   -- Whether the limit is unlimited (infinity).


   function Known (L : Word_Limit_T) return Boolean;
   --
   -- Whether the limit is finite and known (a constant).


   function Value (L : Word_Limit_T) return Word_T;
   --
   -- The value of the limit, assumed to be Known.
   -- If the limit-value is not Known, Unbounded is raised.


   function And_Max (Left, Right : Word_Limit_T) return Word_Limit_T;
   --
   -- Given two upper limits, returns their logical conjunction
   -- or, in other words, the tighter (lesser) limit, in the
   -- unsigned view.


   function And_Min (Left, Right : Word_Limit_T) return Word_Limit_T;
   --
   -- Given two lower limits, returns their logical conjunction
   -- or, in other words, the tighter (greater) limit, in the
   -- unsigned view.


   function Or_Min (Left, Right : Word_Limit_T) return Word_Limit_T;
   --
   -- Given two lower limits, returns their logical disjunction
   -- or, in other words, the looser (lesser) limit, in the
   -- unsigned view.


   function Or_Max (Left, Right : Word_Limit_T) return Word_Limit_T;
   --
   -- Given two upper limits, returns their logical disjunction
   -- or, in other words, the looser (greater) limit, in the
   -- unsigned view.


   procedure Increase (
      Limit    : in out Word_Limit_T;
      To_Allow : in     Word_T);
   --
   -- Increases the upper Limit To_Allow the given value, if the
   -- value is not already within (less or equal to) the Limit,
   -- in the unsigned view.


   procedure Decrease (
      Limit    : in out Word_Limit_T;
      To_Allow : in     Word_T);
   --
   -- Decreases the lower Limit To_Allow the given value, if the
   -- value is not already within (greater or equal to) the Limit,
   -- in the unsigned view.


   --
   ---   Intervals of binary words
   --
   -- These are not yet "bounds", but can be derived from bounds and
   -- used to construct bounds.


   type Word_Interval_T is record
      Min    : Word_Limit_T;
      Max    : Word_Limit_T;
      Width  : Width_T;
      Inside : Boolean;
   end record;
   --
   -- Bounds the value of some value (such as a cell, or an expression)
   -- by the Min and Max limits, either or both of which may be Finite
   -- or Unlimited. The bounds can require the value to be Inside the
   -- interval Min .. Max, or outside this interval (< Min or > Max).
   --
   -- The former case is called a true interval or "eu-interval", and
   -- the latter case is called a complement interval or "co-interval".
   -- Co-intervals are useful mainly for representing interval bounds
   -- on signed values. For a co-interval the interval Min .. Max is
   -- called the "gap" of the co-interval. It is the set complement of
   -- the co-interval. Of course, it is also useful that co-intervals
   -- close the domain with respect to logical negation (set complement).
   --
   -- The Width of the values is also given. The value (cell or
   -- expression) to which this bound applies must be known from context.
   --
   -- The empty (void) interval is represented as an eu-interval with
   -- finite Min and Max such that Min.Value > Max.Value.
   --
   -- Intervals with one or both ends Unlimited are always defined as
   -- eu-intervals with Inside = True. Thus, a co-interval always has
   -- both ends Finite, and moreover Min.Value is greater than zero,
   -- Max.Value is less than Max_Word (Width), and Min.Value is less or
   -- equal to Max.Value. See the To_Normal function below.
   --
   -- Thus, a (normalized) co-interval always contains both the value zero
   -- and the value Max_Word (Width) but never contains all values. Thus,
   -- a (normalized) co-interval is never "void" nor "universal".
   --
   -- Operations on intervals include intersection ("and"), union ("or"),
   -- and complement ("not"). Some operations can return an approximate
   -- result, which is always an over-estimate, in other words, a superset
   -- of the exact result. Details:
   --
   -- > Intersection ("and") is exact for eu-intervals and for co-intervals
   --   with intersecting gaps (so that the exact intersection has only
   --   one gap). This includes all co-intervals that represent signed
   --   intervals. The intersection of an eu-interval with a co-interval
   --   can be inexact. The intersection of two co-intervals with non-
   --   intersecting gaps is always an over-estimate (only one gap is left).
   --
   -- > Union ("or") is exact for all co-intervals and for intersecting
   --   eu-intervals. The union of an eu-interval with a co-interval can
   --   be inexact. The union of two non-intersecting eu-intervals is
   --   always an over-estimate (the values between the two eu-intervals
   --   are included in the union). (TBA/TBC presenting the union as a
   --   co-interval, which may be exact or at least more precise.)
   --
   -- > Complement ("not") is always exact (it simply complements Inside).
   --
   -- The presence of an exact complement operation ("not") lets us use the
   -- De Morgan rules to compute under-estimates of the intersection and
   -- union of intervals. To compute an under-estimate of the intersection
   -- "I and J" of two intervals, compute not ((not I) or (not J)).
   -- Similarly, an under-estimate of the union "I or J" is computed by
   -- not ((not I) and (not J)).


   function To_Normal (Item : Word_Interval_T) return Word_Interval_T;
   --
   -- Normalises an "outside" interval with one or both ends Unlimited
   -- or with Min > Max to the corresponding Inside interval.
   -- Returns Inside intervals unchanged. If the result is an "outside"
   -- interval (Inside = False) its Min and Max are both Finite, Min.Value
   -- is greater than zero, Max.Value is less than Max_Word (Item.Width),
   -- and Min.Value <= Max.Value.


   function Void_Word_Interval (Width : Width_T)
   return Word_Interval_T;
   --
   -- An interval that represents the empty set, with the given Width
   -- of values.


   function Universal_Word_Interval (Width : Width_T)
   return Word_Interval_T;
   --
   -- An interval that contains all values of the given Width.
   -- It is not defined if the ends are unlimited or limited to
   -- the extreme values of words of this Width.


   function Exactly_Zero_Word (Width : Width_T)
   return Word_Interval_T;
   --
   -- The singleton interval that contains only the value 0, of
   -- the given Width.


   function Positive_Interval (Width : Width_T)
   return Word_Interval_T;
   --
   -- The interval that contains all the positive words of the
   -- given width, from 1 to Max_Positive (Width).


   function Eu_Interval (Min, Max : Word_T; Width : Width_T)
   return Word_Interval_T;
   --
   -- The eu-interval Min .. Max, for values of the given Width.


   function Interval (Min, Max : Word_T; Width : Width_T)
   return Word_Interval_T
   renames Eu_Interval;


   function Co_Interval (Min, Max : Word_T; Width : Width_T)
   return Word_Interval_T;
   --
   -- The complement of the interval Min .. Max, for values of
   -- the given Width. That is, the set of values < Min or > Max.


   Negative_Limit : exception;
   --
   -- Signals an attempt to convert a negative, signed limit
   -- to an unsigned limit.


   function Unsigned_Limit (Item : Limit_T)
   return Word_Limit_T;
   --
   -- The corresponding unsigned limit, if the given signed
   -- limit has a non-negative value, or is unlimited.
   -- If the signed limit has a negative value, the function
   -- propagates Negative_Limit.


   function Unsigned_Limit (Value : Value_T)
   return Word_Limit_T;
   --
   -- The limit (Finite, Word_T (Value)), if Value >= 0,
   -- else Negative_Limit is propagated.


   function To_Word_Interval (
      Item  : Interval_T;
      Width : Width_T)
   return Word_Interval_T;
   --
   -- The finite-word interval corresponding to the given
   -- signed interval, when limited to the given Width.


   function Min (Item : Word_Interval_T) return Word_T;
   --
   -- Short for Value (Interval.Min). Can raise Unbounded.


   function Max (Item : Word_Interval_T) return Word_T;
   --
   -- Short for Value (Interval.Max). Can raise Unbounded.


   function Signed_Min (Item : Word_Interval_T) return Value_T;
   --
   -- Short for Signed_Value (Min (Item), Item.Width).
   -- Can raise Unbounded.


   function Signed_Max (Item : Word_Interval_T) return Value_T;
   --
   -- Short for Signed_Value (Max (Item), Item.Width).
   -- Can raise Unbounded.


   function Singleton (Value : Word_T; Width : Width_T)
   return Word_Interval_T;
   --
   -- The one-point interval Value .. Value.


   function One_Or_All (Value : Word_Limit_T; Width : Width_T)
   return Word_Interval_T;
   --
   -- The one-point interval Value .. Value if Value is Known, else
   -- the universal (unlimited) interval.


   function Void (Item : Word_Interval_T) return Boolean;
   --
   -- Whether the interval represents the empty (void) set.
   -- In other words, no value can satisfy both the lower-limit
   -- and the upper-limit.


   function Known (Interval : Word_Interval_T) return Boolean;
   --
   -- Whether the interval has finite and known (constant) components
   -- at one or both ends. Note that we do not require both ends
   -- (min and max) to be finite.


   function Bounded (Interval : Word_Interval_T) return Boolean;
   --
   -- Whether both ends of the Interval are known and finite.


   function Universal (Interval : Word_Interval_T) return Boolean;
   --
   -- Whether the Interval contains all values of its width (is
   -- the universal interval). This is the case if both ends are
   -- Unlimited, or limited to the minimum or maximum word value
   -- (0 or Max_Word).


   function Length (Item : Word_Interval_T) return Word_T;
   --
   -- The maximum difference between any two elements of the
   -- given non-void interval. For a co-interval, we consider
   -- the wrapped interval Max + 1 .. Min - 1 + 2**Width.
   -- The number of values in the interval (the cardinality of
   -- the interval as a set) is Length + 1.
   --
   -- Precondition: not Void (Item).


   function Is_In (
      Value    : Word_T;
      Interval : Word_Interval_T)
   return Boolean;
   --
   -- Whether the Value lies in the Interval, using the unsigned view.


   function "<" (Value : Word_T; Interval : Word_Interval_T)
   return Boolean;
   --
   -- Whether the Value is less than all values in the Interval.


   function Singular (Interval : Word_Interval_T)
   return Boolean;
   --
   -- Whether the Interval allows only a single value (Min and Max
   -- are constants and equal). The single value in question can
   -- be retrieved with Single_Value, see below.


   function Single_Value (Interval : Word_Interval_T)
   return Word_T;
   --
   -- If the Interval allows a single value, that is if Singular (Interval)
   -- is True (which means that Min and Max are constants and equal),
   -- returns this value.
   -- Otherwise, that is if Singular (Interval) is False, raises
   -- Multiple_Values.


   function Signed_Single_Value (Interval : Word_Interval_T)
   return Value_T;
   --
   -- The signed view of Single_Value (Interval), using the Width
   -- of the Interval.


   function Is_Only (Value : Word_T; Interval : Word_Interval_T)
   return Boolean;
   --
   -- Whether the Interval contains exactly the given Value and
   -- nothing more.


   function Is_Only_Zero (Item : Word_Interval_T) return Boolean;
   --
   -- Whether the interval contains only zero and nothing more.


   function "and" (Left, Right : Word_Interval_T) return Word_Interval_T;
   --
   -- The conjunction (intersection) of two intervals.
   -- The result may be an over-estimate (superset).


   function "or" (Left, Right : Word_Interval_T) return Word_Interval_T;
   --
   -- The disjunction (union) of two intervals.
   -- The result may be an over-estimate (superset).


   function "not" (Item : Word_Interval_T) return Word_Interval_T;
   --
   -- The complement interval. This just inverts the Item.Inside, so
   -- it is always an exact complement.


   procedure Widen (
      Interval   : in out Word_Interval_T;
      To_Contain : in     Word_T);
   --
   -- Widens the Interval To_Contain the given value, if it does
   -- not already contain this value.


   function "<=" (Left, Right : Word_Interval_T) return Boolean;
   --
   -- Whether the Left interval is a subset of the Right interval.
   -- In other words, the Left interval is narrower than (or the
   -- same as) the Right interval. Always True if the Left interval
   -- is Void.


   function Intersects (Left, Right : Word_Interval_T) return Boolean;
   --
   -- Whether the intersection of the Left and Right intervals is
   -- non-empty.


   function "+" (Left : Word_Interval_T; Right : Word_T)
   return Word_Interval_T;
   --
   -- The Left interval shifted by adding Right to the Min and Max limits.
   -- If Right is zero, the result is the Left interval unchanged.
   -- Otherwise, the result is an interval with both ends limited by the
   -- sum of the Left ends and Right. If the limits wrap around in the
   -- range of Left.Width, the operation may turn an eu-interval into a
   -- co-interval or vice versa.


   function "-" (Left : Word_Interval_T; Right : Word_T)
   return Word_Interval_T;
   --
   -- The Left interval shifted by subtracting Right from the Min and
   -- Max limits. The same as Left + Opposite_Sign (Right, Left.Width).


   function "+" (Left, Right : Word_Interval_T) return Word_Interval_T;
   --
   -- The interval sum of Left and Right. If either operand is
   -- void, so is the sum.


   function "-" (Item : Word_Interval_T) return Word_Interval_T;
   --
   -- The interval that contains the values of Opposite_Sign to
   -- the values in the given interval.


   function "-" (Left, Right : Word_Interval_T) return Word_Interval_T;
   --
   -- The interval difference of Left - Right. If either operand is
   -- void, so is the sum. Equivalent to Left + (- Right).


   function Image (Item : Word_Interval_T) return String;
   --
   -- The interval as "(low limit) .. (high limit)" for an eu-interval,
   -- or "not (low limit) .. (high limit)" for a co-interval.
   -- An unknown (unbounded) limit is written as "-inf" or "+inf".
   -- If the interval is singular, or the complement of a singular
   -- eu-interval, just the single value is shown (preceded by "not"
   -- for a co-interval).


   function Image (
      Item : Word_Interval_T;
      Name : String)
   return String;
   --
   -- The Name is used to denote the value (cell or expression)
   -- to which the bound applies. The result has one of the forms:
   --
   --    min <= Name <= max     for a bounded eu-interval
   --    min <= Name            for an eu-interval with only lower bound
   --    Name <= max            for an eu-interval with only upper bound
   --    Name                   for an unbounded eu-interval
   --    Name = value           for a singular eu-interval
   --    Name /= value          for the co-interval of a singular eu-interval
   --    Name not min .. max    for other co-intervals.


   function Image (
      Item : Word_Interval_T;
      Cell : Cell_T)
   return String;
   --
   -- Same as Image (Item, Image (Cell)).


   type Word_Interval_List_T is array (Positive range <>) of Word_Interval_T;
   --
   -- A set of interval bounds on word values.
   -- The value (cell or expression) to which each interval applies
   -- must be known from context.


   type Word_List_T is array (Positive range <>) of Word_T;
   --
   -- A list of word values. The indexing and meaning depend on usage.
   -- The width of the words must be known from context.


   function Single_Value (Intervals : Word_Interval_List_T)
   return Word_List_T;
   --
   -- The list of single values allowed by each of the listed singular
   -- Intervals. If some of the Intervals are not singular the function
   -- propagates Multiple_Values.


   function Image (
      Item   : Word_Interval_List_T;
      Prefix : String)
   return String;
   --
   -- Describes each interval in the list, using a Name for the
   -- bounded value composed from the Prefix and the index of
   -- the interval in the list.


   --
   ---   Intervals of binary words for cells
   --
   -- These are not yet "bounds", but can be derived from bounds and can
   -- be used to construct bounds.


   type Cell_Word_Interval_T is record
      Cell     : Cell_T;
      Interval : Word_Interval_T;
   end record;
   --
   -- An interval that bounds the value of a cell, viewed as a binary
   -- word. Normally Interval.Width = Width_Of (Cell).


   type Cell_Word_Interval_List_T is
      array (Positive range <>) of Cell_Word_Interval_T;
   --
   -- A set of interval bounds on cell values, giving both the cells
   -- and the value-interval for each cell, viewed as a binary word.


   function Empty return Cell_Word_Interval_List_T;
   --
   -- An empty (null) list of cell-interval bounds.


   function Interval (
      Cell : Storage.Cell_T;
      From : Cell_Word_Interval_List_T)
   return Word_Interval_T;
   --
   -- The interval bounds on the given Cell, taken From the given list.
   -- If the list has several interval bounds for the cell, their
   -- conjunction (intersection) is returned. If the list has no interval
   -- bounds for the cell, Universal_Interval is returned.


   function Intervals_For_Cells (
      Cells : Cell_Set_T;
      From  : Cell_Word_Interval_List_T)
   return Cell_Word_Interval_List_T;
   --
   -- Returns the interval bounds From the list on the given Cells.
   -- If a cell from Cells occurs several times in the From list, it
   -- will occur as many times in the result.


   function "and" (Left, Right : Cell_Word_Interval_List_T)
   return Cell_Word_Interval_List_T;
   --
   -- The conjunction of the two lists of interval bounds on cells.
   -- If the same cell is mentioned in both lists, it occurs
   -- but once in the result, with the conjunction (intersection)
   -- of the intervals from all its ocurrences.
   -- If a cell is mentioned only in one of the two lists, that list
   -- defines the interval for the cell in the result.


   function Image (Item : Cell_Word_Interval_T) return String;
   --
   -- As in Image (Item => Item.Bound, Name => Image (Item.Cell)).


   function Image (Item : Cell_Word_Interval_List_T) return String;
   --
   -- Describes the bounds as a list of Image (Bound, Image(Cell)).


   function Cells_Of (Item : Cell_Word_Interval_List_T) return Cell_List_T;
   --
   -- All the cells in the given list of cells and intervals, in the
   -- same order and with possible duplicates.


   --
   ---   Intervals of binary words for variables with moving locations
   --
   -- These are not yet "bounds", nor can they be derived from bounds, but
   -- they can be used to construct bounds when the point in the program is
   -- known so that the variable can be associated with a cell.


   type Var_Word_Interval_T is record
      Location : Location_Ref;
      Interval : Word_Interval_T;
   end record;
   --
   -- Interval bounds on the value of a variable that may occupy different
   -- storage locations at different points in the program, when the value
   -- is viewed as a binary word.


   type Var_Word_Interval_List_T
      is array (Positive range <>) of Var_Word_Interval_T;
   --
   -- A set of interval bounds on variable values, giving both the location
   -- of each variable (possibly varying with the execution point) and
   -- the interval that bounds each variable, when the values are viewed
   -- as binary words.


   function "and" (Left, Right : Var_Word_Interval_List_T)
   return Var_Word_Interval_List_T;
   --
   -- The conjunction of the two lists of interval bounds in variables.
   -- If the same variable (same location-ref) is mentioned in both lists,
   -- it occurs but once in the result, with the conjunction (intersection)
   -- of the intervals from all its ocurrences.
   -- If a variable is mentioned only in one of the two lists, that list
   -- defines the interval for the variable in the result.
   -- Variables are identified by the locations (reference equality).


   function Cell_Intervals (
      From  : Var_Word_Interval_List_T;
      Point : Processor.Code_Address_T)
   return Cell_Word_Interval_List_T;
   --
   -- Interval bounds on cell values derived From interval bounds on
   -- variables by mapping the location of each variable to a cell at
   -- the given Point in the target program.
   --
   -- If a variable maps to several cells at this point, the bounds for
   -- the variable are applied to each cell in the result.
   --
   -- If a variable maps to no cells at this point, the bounds for the
   -- variable are not used in the result.
   --
   -- If several variables map to the same cell at this point, the bounds
   -- for these variables are conjoined (intersected) to give the bounds
   -- on this cell in the result.


   --
   ---  Lists (enumerations) of binary word values
   --
   -- These are not yet "bounds" but can be derived from bounds.
   --
   -- Some "bounds" can yield a list of possible values for the
   -- bounded variable(s). This will usually be an ordered list
   -- (ascending order) without duplicates.


   function Values (Within : Word_Interval_T) return Word_List_T;
   --
   -- All the values Within the interval, assuming that the interval
   -- is fully Bounded, else Unbounded is raised.
   --
   -- If the number of values would exceed Opt.Max_Listed_Values the
   -- function raises Unbounded. For an "outside" interval the result
   -- can contain twice this number of values, because the low end
   -- and high end are handled separately as two Inside intervals.


   function "-" (Left, Right : Word_Interval_T) return Word_List_T;
   --
   -- All the values allowed by Left, except the values also
   -- allowed by Right. In other words, the difference between
   -- the set defined by Left and the set defined by Right.
   -- All limits involved must be known constants, otherwise
   -- Unbounded is raised.
   --
   -- If the number of values would exceed Opt.Max_Listed_Values the
   -- function raises Unbounded.


   subtype Positive_Word_T is Word_T range 1 .. Word_T'Last;
   --
   -- The non-zero values of binary words.


   function Values (Within : Word_Interval_T; Aligned : Positive_Word_T)
   return Word_List_T;
   --
   -- All the values that are Within the given interval and are
   -- also integral multiples of Aligned, assuming that the interval
   -- is fully Bounded, else Unbounded is raised.
   -- TBD if the limits must be multiples of Aligned.
   --
   -- If the number of values would exceed Opt.Max_Listed_Values the
   -- function raises Unbounded.


   function Difference (
      Included : Word_Interval_T;
      Excluded : Word_Interval_T;
      Aligned  : Positive_Word_T)
   return Word_List_T;
   --
   -- All the values allowed by the Included interval, except the
   -- values also allowed by the Excluded interval, and considering
   -- only those values that are integral multiples of Aligned.
   -- All limits involved must be known constants, otherwise the
   -- Unbounded exception is raised.
   --
   -- The result equals Values (Included and not Excluded, Aligned) if
   -- the intersection ("and") is computed exactly. Otherwise, the
   -- result from Difference is more exact that from the Values call.
   --
   -- TBD if the limits must be multiples of Aligned.
   --
   -- If the number of values would exceed Opt.Max_Listed_Values the
   -- function raises Unbounded.


   --
   ---   Bounds on variables in the abstract
   --


   type Bounds_T is abstract tagged null record;
   --
   -- Bounds (constraints, limits, restrictions) on the (joint or
   -- separate) values of a set of variables at some point(s) in a
   -- program or an execution.


   function Basis (Item : Bounds_T) return Cell_List_T
   is abstract;
   --
   -- The cells that the Item bounds. The list may be empty, although
   -- this is a degenerate case as the Item is then useless; it does
   -- not constrain any cell values.
   --
   -- Depending on the type of Item, the bounds may or may not imply
   -- bounds (eg. an interval) for each cell in the basis. Some types
   -- of bounds may only imply relationships between the values of
   -- the cells without constraining the value of an individual cell.


   function Interval (Cell : Cell_T; Under : Bounds_T)
   return Interval_T
   is abstract;
   --
   -- The interval (range) of values of the Cell, permitted Under the
   -- given Bounds. Note that the Cell does not necessarily take on
   -- every value in this interval. The function shall not propagate
   -- Unbounded but may return an interval where either or both the
   -- Min and Max are unlimited.


   function Difference (To, From : Cell_T; Under : Bounds_T)
   return Interval_T;
   --
   -- The interval (range) of values of the difference To - From
   -- between two cells, permitted Under the given Bounds. Note that
   -- the difference does not necessarily take on every value in
   -- this interval. The function shall not propagate Unbounded but
   -- may return an interval where either or both the Min and Max
   -- are unlimited.
   --
   -- The default operation computes the interval-arithmetic
   -- difference Interval (To, Under) - Interval (From, Under),
   -- with (of course) redispatching on Under.


   function Intervals (Under : Bounds_T)
   return Cell_Interval_List_T;
   --
   -- The list of Basis cells and the Interval permitted for each
   -- basis cell, Under the given bounds.
   --
   -- The default operation applies the Basis and Interval functions.


   function Value (Cell : Cell_T; Under : Bounds_T)
   return Value_T;
   --
   -- The single value of the Cell permitted Under the given bounds.
   -- Propagates Unbounded if the bounds do not constrain the cell
   -- to a single value.
   --
   -- The default operation is Single_Value (Interval (Cell, Under)).


   function Values (Cell : Cell_T; Under : Bounds_T)
   return Value_List_T;
   --
   -- The list of possible values of the Cell, permitted Under the
   -- given Bounds. This list should contain all and only the possible
   -- values of the Cell. If such a list cannot be derived from the
   -- bounds, the function should propagate Unbounded.
   --
   -- The maximum length of the returned list may be bounded by a
   -- command-line option.
   --
   -- The default operation is Values (Interval (Cell, Under)).


   function Image (Item : Bounds_T) return String;
   --
   -- Textual description of the bounds object.
   --
   -- The default implementation displays the Expanded_Name
   -- of the Item's tag.


   function Full_Image (Item : Bounds_T) return String;
   --
   -- Textual description of the bounds object including the
   -- basis cells and the bounds placed on the basis cells.
   --
   -- The default implementation displays Image followed by the
   -- Interval bounds on each of the Basis cells.


   type Bounds_Ref is access all Bounds_T'Class;
   --
   -- A reference to some kind of bounds.

   --:dbpool Bounds_Pool : GNAT.Debug_Pools.Debug_Pool;

   --:dbpool for Bounds_Ref'Storage_Pool use Bounds_Pool;


   procedure Discard (Item : in out Bounds_Ref);
   --
   -- Discards (deallocates) a heap-allocated bounds Item.
   -- As for all unchecked-deallocation, the user must be careful
   -- with dangling references.


   type Bounds_List_T is array (Positive range <>) of Bounds_Ref;
   --
   -- A list of references to some kind(s) of bounds.
   -- The significance, if any, of the indices and the order
   -- depends on the context and usage.


   procedure Discard (List : in out Bounds_List_T);
   --
   -- Discards all bounds objects in the List using unchecked
   -- deallocation.


   --
   --    The bottom: no bounds at all.
   --


   type No_Bounds_T is new Bounds_T with null record;
   --
   -- A class of bounds that places no bounds on any cells at all.


   function Basis (Item : No_Bounds_T) return Cell_List_T;
   --
   -- The empty list.
   -- Overrides (implements) Basis (Bounds_T).


   function Interval (Cell : Cell_T; Under : No_Bounds_T)
   return Interval_T;
   --
   -- The universal interval.
   -- Overrides (implements) Interval (Bounds_T).


   function No_Bounds return Bounds_Ref;
   --
   -- A reference to a specific No_Bounds object that is created
   -- at package elaboration, so the returned value is constant.
   -- There is really no need for any other objects of this type
   -- since they would all behave the same way.


   --
   --    Bounds composed of single values
   --
   -- The simplest type of "bounds" defines a single value for each cell
   -- in the basis. The set of allowed cell-tuple values is the single
   -- tuple where each basis cell has the single allowed value.


   type Singular_Bounds_T (Length : Positive)
   is new Bounds_T with record
      List : Cell_Value_List_T (1 .. Length);
   end record;
   --
   -- Bounds each List.Cell to the single corresponding List.Value and
   -- no other.


   function Basis (Item : Singular_Bounds_T) return Cell_List_T;
   --
   -- Overrides (implements) Basis for Bounds.


   function Interval (Cell : Cell_T; Under : Singular_Bounds_T)
   return Interval_T;
   --
   -- Overrides (implements) Interval for Bounds.


   function Value (Cell : Cell_T; Under : Singular_Bounds_T)
   return Value_T;
   --
   -- Overrides Value for Bounds.


   function Singular_Bounds (List : Cell_Value_List_T)
   return Singular_Bounds_T;
   --
   -- Converts a list of cells and their values to a Bounds_T'Class.


   --
   --    Bounds composed of intervals
   --
   -- One (rather simple) type of "bounds" defines an interval for
   -- each cell in the basis. Thus, no interaction between cells is
   -- modelled. The set of allowed cell-tuple values is the cartesian
   -- product of the intervals for each cell.


   type Interval_Bounds_T (Length : Positive)
   is new Bounds_T with record
      List : Cell_Interval_List_T (1 .. Length);
   end record;
   --
   -- Bounds each List.Cell to the corresponding List.Interval.


   function Basis (Item : Interval_Bounds_T) return Cell_List_T;
   --
   -- Overrides (implements) Basis for Bounds.


   function Interval (Cell : Cell_T; Under : Interval_Bounds_T)
   return Interval_T;
   --
   -- Overrides (implements) Interval for Bounds.


   function Interval_Bounds (List : Cell_Interval_List_T)
   return Interval_Bounds_T;
   --
   -- Converts a list of interval bounds on cell values to
   -- a Bounds_T'Class.


   function Interval_Bounds (
      From  : Var_Interval_List_T;
      Point : Processor.Code_Address_T)
   return Bounds_Ref;
   --
   -- The Bounds_T object created From a list of interval bounds on
   -- variables, mapped to the corresponding cells at a given Point,
   -- and stored in the heap. The object is of type Interval_Bounds_T
   -- or No_Bounds_T.


   --
   --    Boundable things
   --


   type Boundable_T is abstract tagged null record;
   --
   -- A thing that can be bounded (constrained, refined) by some bounds
   -- because the meaning (nature, result, effect) of the thing depends on
   -- the values of some variables (cells) at the point in the program
   -- (or execution) where the boundable thing resides (or is executed).


   function Basis (Item : Boundable_T) return Cell_List_T
   is abstract;
   --
   -- The cells on which the boundable Item depends. The list is usually
   -- non-empty, as otherwise the Item must be statically known and is
   -- not really "boundable".
   --
   -- The main purpose of this function is to tell various analysis
   -- algorithms which cells should be bounded in order to bound this
   -- Item.


   procedure Add_Basis_Cells (
      From : in     Boundable_T;
      To   : in out Cell_Set_T);
   --
   -- Adds all the cells From the boundable Item's Basis set To the
   -- given set of cells.
   --
   -- The default implementation uses the Basis function, which may be
   -- inefficient.


   procedure Add_Basis_Cells (
      From  : in     Boundable_T;
      To    : in out Cell_Set_T;
      Added : in out Boolean);
   --
   -- Adds all the cells From the boundable Item's Basis set To the
   -- given set of cells. If some of the added cells were not already
   -- in the set, Added is set to True, otherwise (no change in the
   -- set) Added is not changed.
   --
   -- The default implementation uses the Basis function, which may be
   -- inefficient.


   function Is_Used (Cell : Cell_T; By : Boundable_T) return Boolean;
   --
   -- Whether the given Cell is used By the boundable, in other words
   -- whether the Cell belongs to the Basis of the boundable.
   --
   -- The default implementation uses the Basis function, which is
   -- likely to be inefficient.


   function Image (Item : Boundable_T) return String;
   --
   -- Textual description of the boundable object.
   --
   -- The default implementation displays the Expanded_Name
   -- of the Item's tag.


   type Resolution_T is (
      Fail_Changed,
      Changed,
      Fail_Stable,
      Stable);
   --
   -- The overall result of applying some "bounds" to a "boundable"
   -- object.
   --
   -- Fail_Changed
   --    The boundable object was changed (elaborated or constrained)
   --    by the bounds, but at the same time some sort of failure was
   --    detected which means that the boundable object cannot be
   --    analyzed further but should be deleted, perhaps leaving some
   --    space-holder or dummy object. The details of the deletion and
   --    the possible dummy object depend on the specific kind of
   --    boundable object and its role. However, since the object was
   --    changed, the program model should be re-analyzed or the
   --    analysis should be updated to include this change.
   --
   -- Changed
   --    The boundable object was successfully changed by the bounds,
   --    so that the program model must be re-analyzed to include
   --    the change. Moreover, the boundable object retains some
   --    dynamic characteristics, so if the program re-analysis leads
   --    to sharper bounds the new bounds should again be applied to
   --    this boundable object.
   --
   -- Fail_Stable
   --    Some sort of failure was detected which means that the
   --    boundable object cannot be analyzed further. The object was
   --    not changed but should be deleted as for Fail_Changed.
   --
   -- Stable
   --    The bounds were successfully applied to the boundable object
   --    but did not change the object. The analysis of this object
   --    has reached a stable point. When all boundable objects in
   --    the program model are stable, the analysis of the model
   --    has converged (as far as dynamic bounding is concerned).
   --
   -- The elements are listed in such an order that the combined
   -- result of two Resolution_T values is the "lesser" (earlier)
   -- of the two. For example Changed+Stable => Changed,
   -- and Fail_Changed+anything => Fail_Changed.


   procedure Apply (
      Bounds : in     Bounds_T'Class;
      Upon   : in out Boundable_T;
      Result :    out Resolution_T);
   --
   -- Applies the Bounds Upon a thing, perhaps modifying the thing,
   -- as shown by the Result value.
   --
   -- This operation is primitive (and thus overridable and dispatchable)
   -- for Boundable_T, but class-wide for Bounds_T. Thus, the general
   -- parts of Bound-T are transparent to whatever sorts of boundable
   -- things the processor-specific parts define, while the processor-
   -- specific parts must depend on the sort of "bounds" that the
   -- general analysis can compute.
   --
   -- The Apply operation at this level is probably too general to
   -- be useful, because it can only affect the boundable thing itself
   -- and not, for example, the program-model of which this thing is a
   -- part. Therefore, we provide a default, do-nothing implementation
   -- that simply returns Result as Stable.
   --
   -- The next derivation level of Boundable_T will define more useful
   -- derived types such as dynamic memory references and dynamic jumps.


   type Boundable_Ref is access Boundable_T'Class;
   --
   -- A reference to some kind of boundable thing.


end Storage.Bounds;
