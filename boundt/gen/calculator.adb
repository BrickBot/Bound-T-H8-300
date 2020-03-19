-- Calculator (body)
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
-- $Revision: 1.59 $
-- $Date: 2015/10/24 20:05:47 $
--
-- $Log: calculator.adb,v $
-- Revision 1.59  2015/10/24 20:05:47  niklas
-- Moved to free licence.
--
-- Revision 1.58  2013/12/22 20:14:56  niklas
-- Value enumerators obey Storage.Bounds.Opt.Max_Listed_Values and raise
-- Unbounded_Set or Storage.Bounds.Unbounded if more values are offered;
-- they do not return a truncated list.
--
-- Revision 1.57  2013-02-19 09:13:55  niklas
-- BT-CH-0245 clean-up. Only descriptions and lay-out changed.
--
-- Revision 1.56  2013-02-12 08:47:19  niklas
-- BT-CH-0245: Global volatile cells and "volatile" assertions.
--
-- Revision 1.55  2011-08-31 04:23:34  niklas
-- BT-CH-0222: Option registry. Option -dump. External help files.
--
-- Revision 1.54  2009-11-27 11:28:07  niklas
-- BT-CH-0184: Bit-widths, Word_T, failed modular analysis.
--
-- Revision 1.53  2009-10-07 19:26:10  niklas
-- BT-CH-0183: Cell-sets are a tagged-type class.
--
-- Revision 1.52  2009-01-18 08:03:38  niklas
-- Moved context clause to body. Removed unused locals.
--
-- Revision 1.51  2008/06/20 10:11:53  niklas
-- BT-CH-0132: Data pointers using Difference (Expr, Cell, Bounds).
--
-- Revision 1.50  2008/06/18 20:52:55  niklas
-- BT-CH-0130: Data pointers relative to initial-value cells.
--
-- Revision 1.49  2008/05/03 18:04:54  niklas
-- Corrected Bounds_Of_Complement to use the Within parameter.
--
-- Revision 1.48  2008/05/03 09:22:06  niklas
-- BT-CH-0126: Combining joint-counter and each-counter analysis.
--
-- Revision 1.47  2008/04/28 08:40:10  niklas
-- BT-CH-0125: Fix loop-until-equal analysis.
--
-- Revision 1.46  2008/04/26 19:19:44  niklas
-- BT-CH-0124: Joint loop counters and induction variables.
--
-- Revision 1.45  2008/04/22 12:51:27  niklas
-- Extended Pool_To_Steps to optionally find edges that have a null
-- (empty) data pool, and to mark those edges infeasible. Added the
-- nested procedures Mark_Infeasible and Check_Null_Flow.
--
-- Revision 1.44  2007/10/03 07:27:12  niklas
-- BT-CH-0081: Fix: Size of joint effect from Flow.Life.Join_Effects.
--
-- Revision 1.43  2007/08/27 16:22:07  niklas
-- Added nicer Image for Pool_T.
--
-- Revision 1.42  2007/02/13 20:15:03  Niklas
-- BT-CH-0044.
--
-- Revision 1.41  2006/04/28 09:45:49  niklas
-- Added functions Values (Expr, Pool or Flux) to override the
-- inherited functions Values (Expr, Arithmetic.Bounds_T).
-- Corrected function Values (Pool) to require the Pool to be
-- bounded both from below and from above (but note that this
-- function is probably obsolete and not used).
--
-- Revision 1.40  2005/10/20 19:34:01  niklas
-- BT-CH-0016.
--
-- Revision 1.39  2005/10/20 11:28:30  niklas
-- BT-CH-0015.
--
-- Revision 1.38  2005/09/17 14:42:05  niklas
-- BT-CH-0009.
--
-- Revision 1.37  2005/09/16 14:19:10  niklas
-- Added Calc_Image functions for Pool_T and Flux_T, for easier
-- cross-referencing to Omega files. Note that Flux_T already
-- inherits an Image function from Arithmetic.Bounds_T but this has
-- a different purpose.
--
-- Revision 1.36  2005/09/12 19:02:58  niklas
-- BT-CH-0008.
--
-- Revision 1.35  2005/07/01 10:58:01  niklas
-- Changed the Interval functions for Flux_T to propagate
-- Flow.False_Path instead of Empty_Flux, since these functions
-- may be called through dynamic dispatch in contexts that are not
-- aware of the actual (derived) type of the bounds.
--
-- Revision 1.34  2005/02/16 21:11:41  niklas
-- BT-CH-0002.
--
-- Revision 1.33  2004/08/09 19:51:30  niklas
-- BT-CH-0001.
--
-- Revision 1.32  2004/05/01 20:23:01  niklas
-- First Tidorum version.
-- Taking Cell_T stuff from Storage, not from Arithmetic. Removed "use" for
-- Arithmetic, added some "use type" for Storage types.
-- Added variant of Range_Bounded_Flux that takes the bounds from Range_Pre
-- assignment constraints.
-- Changed Constrain_Range and Restrict_Domain_Of_Cells to use the new
-- functions Adapted and Warp and the modified Apply function from
-- Calculator.Formulas, for simpler code and calculator expressions.
-- Corrected Propagate.Handle_Loop_Head to make variant cells unknown.
-- Changed Bounds_After_Step to use the new function Transformation from
-- Calculator.Formulas.
--
-- Revision 1.31  2001/04/17 13:46:12  holsti
-- Uses Flow.Sort.
--
-- Revision 1.30  2001/03/15 07:25:58  holsti
-- Bounds_After_Step uses effect on given cell only.
--
-- Revision 1.29  2001/01/19 08:50:17  saarinen
-- NC_041 Special handling of loop-exit edges.
-- NC_084 Flux_To_Steps review issues.
--
-- Revision 1.28  2001/01/13 11:10:37  holsti
-- Removed Limits_From_Flux in favour of Bounds_From_Flux (with
-- no "Inv" parameter, which was unused anyway).
-- Renamed Limits_From_Complement to Bounds_From_Complement (and
-- removed the unused "Inv" parameter).
-- Removed to-be's in favour of NC's.
--
-- Revision 1.27  2001/01/07 22:05:07  holsti
-- Comment parameter added to Start.
-- Comments on calculation steps added.
-- Parameters for live-cell analysis added.
-- Owner_Of operations added to support commenting.
--
-- Revision 1.26  2001/01/04 16:11:36  saarinen
-- Moved function Convert to Calculator.Formulas.
-- Added missing parenthesis into an omega formula
-- in Flux_To_Steps.
--
-- Revision 1.25  2000/12/28 13:29:26  saarinen
-- Added function Limits_From_Complement.
--
-- Revision 1.24  2000/12/21 10:14:42  sihvo
-- Changes in layout.
--
-- Revision 1.23  2000/11/24 12:06:00  sihvo
-- Added stack height analysis.
--
-- Revision 1.22  2000/11/23 12:46:56  saarinen
-- Deleted Flux_To_Steps, Flux_Of_Region, Flux_To_Nodes.
-- Fixed joining of fluxes with different bases (NC_029).
--
-- Revision 1.21  2000/11/22 22:22:41  holsti
-- Call layout prettified.
--
-- Revision 1.20  2000/10/14 16:36:20  saarinen
-- Fixed Flux_To_Steps.
--
-- Revision 1.19  2000/10/06 14:03:15  saarinen
-- Added function Restrict_Domain_Of_Cells, and
-- fixed Flux_To_Steps.
--
-- Revision 1.18  2000/09/20 18:58:54  saarinen
-- Modified Calc_Handle_T.
-- Fixed some problems with different Cell_Sets in some procedures.
-- Changed Flux_To_Step not to use transitive closure.
--
-- Revision 1.17  2000/08/17 13:00:53  holsti
-- Bounds_From_Flux for expression added.
--
-- Revision 1.16  2000/07/24 22:42:36  holsti
-- Flux_To_Steps over acyclic region added.
--
-- Revision 1.15  2000/07/17 21:00:09  holsti
-- Cell_Set_T instead of Cell_List_T for domains and ranges.
--
-- Revision 1.14  2000/07/16 18:42:32  holsti
-- Flux_To_Nodes uses loop-repeat fluxes as designed.
--
-- Revision 1.13  2000/07/16 13:10:56  holsti
-- Cells_Of (Flux) added.
--
-- Revision 1.12  2000/07/14 20:37:13  holsti
-- Using Calculator.Propagator and List_Filters.
--
-- Revision 1.11  2000/07/12 20:46:00  holsti
-- Data-flow in regions uses only region edges.
--
-- Revision 1.10  2000/07/12 12:26:14  holsti
-- Calculator.Formulas added and used.
--
-- Revision 1.9  2000/06/29 14:10:26  holsti
-- Parser updated.
--
-- Revision 1.8  2000/06/26 10:10:37  saarinen
-- Removed un-needed with-clauses.
--
-- Revision 1.7  2000/06/22 10:24:07  saarinen
-- Added the use of calculator.parser.
--
-- Revision 1.6  2000/06/16 10:45:45  saarinen
-- Implemented rest of the operations without parser.
--
-- Revision 1.4  2000/06/02 08:38:33  saarinen
-- Added many local procedures and implemented some of the visible ones
--
-- Revision 1.3  2000/05/18 10:27:05  saarinen
-- Added expression and cell_set types
--
-- Revision 1.2  2000/05/12 11:06:36  saarinen
-- first implementations
--


with Calculator.Formulas;
with Calculator.Opt;
with Calculator.Propagator;
with Flow.Computation;
with Flow.Pruning.Opt;
with Flow.Show;
with Flow.Sort;
with List_Filters;
with Output;
with Programs;
with Storage.Bounds.Opt;
with Symbols;


package body Calculator is


   use Calculator.Formulas;

   use type Storage.Cell_T;


   --
   -- Filters to pick the edges relevant to a node:
   --

   package Edge_Filters is new List_Filters (
      Item_Type  => Flow.Edge_T,
      List_Type  => Flow.Edge_List_T,
      Value_Type => Flow.Node_T,
      "="        => Flow."=");

   function Edges_Into is new Edge_Filters.Chosen (Flow.Target);
   function Edges_From is new Edge_Filters.Chosen (Flow.Source);



   function Indices (Edges : Flow.Edge_List_T)
   return Propagator.Edge_Index_List_T
   --
   -- The indices of the edges in the list.
   --
   is
      Ind : Propagator.Edge_Index_List_T (Edges'Range);
      -- The result.
   begin

      for E in Edges'Range loop
         Ind(E) := Flow.Index (Edges(E));
      end loop;

      return Ind;

   end Indices;



   procedure Report_Calc_Mixing (
      Location : in String)
   --
   -- When an operation has more than one pool or flux parameter,
   -- it is an internal fault if they reside on different
   -- calculator instances. Here, this error is reported.
   --
   is
   begin

      Output.Fault (
         Location => Location,
         Text     => "Operands from different calculators");

      raise Calculator_Error;

   end Report_Calc_Mixing;


   procedure Check_Same_Cells (
      Left, Right : in Storage.Cell_Set_T;
      Location    : in String)
   --
   -- For some operations with more than one pool or flux
   -- parameter, the cells involved will be the same, unless
   -- an internal fault has occurred. Here, this is checked
   -- and the fault, if any, is reported.
   --
   -- This is a fairly expensive check and should perhaps
   -- be controlled by an option.
   --
   is
      use type Storage.Cell_Set_T;

   begin

      if Left /= Right then

         Output.Fault (
            Location => Location,
            Text     => "Operands with different cell lists");

         raise Calculator_Error;

      end if;

   end Check_Same_Cells;


   --
   --    Common properties of data pools and data fluxes
   --


   -- overriding
   function Basis (Item : Bounds_T) return Storage.Cell_List_T
   is
   begin

      return Storage.To_List (Cells_Of (Item));

   end Basis;


   -- overriding
   function Difference (To, From : Storage.Cell_T; Under : Bounds_T)
   return Storage.Bounds.Interval_T
   is
      use Arithmetic;
   begin

      if  Cell_Sets.Is_Member (To  , Under.Cells)
      and Cell_Sets.Is_Member (From, Under.Cells)
      then
         -- We can calculate with these cells.

         return Interval (
            Expr  => Expr (To) - Expr (From),
            Under => Bounds_T'Class (Under));
         --
         -- TBM: The above leaks an Expr_T object.

      else
         -- One or both cells are foreign to this calculation
         -- and so the difference is unbounded.

         return Storage.Bounds.Universal_Interval;

      end if;

   end Difference;


   -- overriding
   function Difference (
      To    : Arithmetic.Expr_Ref;
      From  : Storage.Cell_T;
      Under : Bounds_T)
   return Storage.Bounds.Interval_T
   is
      use Arithmetic;
   begin

      if Cell_Sets.Is_Member (From, Under.Cells) then
         -- We can calculate with this cell.
         -- We assume that the To expression contains only
         -- cells from Under.Cells.

         return Interval (
            Expr  => To - Expr (From),
            Under => Bounds_T'Class (Under));
         --
         -- TBM: The above leaks an Expr_T object.

      else
         -- The From cell is foreign to this calculation
         -- and so the difference is unbounded.

         return Storage.Bounds.Universal_Interval;

      end if;

   end Difference;


   function Cells_Of (Item : Bounds_T) return Storage.Cell_Set_T
   is
   begin

      return Item.Cells;

   end Cells_Of;


   function Owner_Of (Item : Bounds_T) return Calc_Handle_T
   is
   begin

      return Item.Calc;

   end Owner_Of;


   --
   --   OPERATION IMPLEMENTATIONS:
   --


   function Image (Item : Pool_T) return String
   is
   begin

      return "arithmetic-pool#"
           & Output.Image (Natural (Item.Id));

   end Image;


   function Interval (Cell : Storage.Cell_T; Under : Pool_T)
   return Storage.Bounds.Interval_T
   is
   begin

      return Bounds_From_Pool (Pool => Under, Cell => Cell);

   exception

   when Null_Set_Error =>

      Output.Note ("Calculator.Interval (Pool, Cell) finds an empty pool.");

      raise Flow.False_Path;

   end Interval;


   function Interval (Expr : Arithmetic.Expr_Ref; Under : Pool_T)
   return Storage.Bounds.Interval_T
   is
   begin

      return Bounds_From_Pool (Pool => Under, Expr => Expr);

   exception

   when Null_Set_Error =>

      Output.Note ("Calculator.Interval (Pool, Expr) finds an empty pool.");

      raise Flow.False_Path;

   end Interval;


   function Values (Expr : Arithmetic.Expr_Ref; Under : Pool_T)
   return Storage.Bounds.Value_List_T
   is
   begin

      return
         Formulas.Values (
            Set => Apply (
               Relation => Mapping (
                  Domain => Under.Cells,
                  Value  => Expr),
               To_Set => Id (Under)),
            Calc => Under.Calc);

   exception

   when Formulas.Unbounded_Set =>

      raise Storage.Bounds.Unbounded;

   end Values;


   function Calc_Image (Item : Pool_T) return String
   is
   begin

      return
           "pool"
         & Pool_Id_T'Image (Item.Id)
         & " {"
         & Cell_Sets.Image (Item.Cells)
         & '}';

   end Calc_Image;


   -- Delegate Start, Stop and Comment to Calculator.Formulas,
   -- where other Exec_Cmd activities take place:


   function Start (Comment_Text : in String)
   return Calc_Handle_T
   is
      Calc : Calc_Handle_T;
      -- The calculator, here started.
   begin

      Calc := Calculator.Formulas.Start;

      if Comment_Text'Length > 0 then
         Comment (
            Text => Comment_Text,
            Calc => Calc);
      end if;

      return Calc;

   end Start;


   procedure Stop (Calc : in out Calc_Handle_T)
   renames Calculator.Formulas.Stop;


   procedure Comment (
      Text : in String;
      Calc : in Calc_Handle_T)
   renames Calculator.Formulas.Comment;


   function Interval (Cell : Storage.Cell_T; Under : Flux_T)
   return Storage.Bounds.Interval_T
   is
   begin

      return Bounds_From_Flux (Flux => Under, Cell => Cell);

   exception

   when Null_Set_Error =>

      Output.Note ("Calculator.Interval (Flux, Cell) finds an empty flux.");

      raise Flow.False_Path;

   end Interval;


   function Interval (Expr : Arithmetic.Expr_Ref; Under : Flux_T)
   return Storage.Bounds.Interval_T
   is
   begin

      return Bounds_From_Flux (Flux => Under, Expr => Expr);

   exception

   when Null_Set_Error =>

      Output.Note ("Calculator.Interval (Flux, Expr) finds an empty flux.");

      raise Flow.False_Path;

   end Interval;


   function Values (Expr : Arithmetic.Expr_Ref; Under : Flux_T)
   return Storage.Bounds.Value_List_T
   is
   begin

      return
         Formulas.Values (
            Set => Rainge (Join (
               Id (Under),
               Mapping (
                  Domain => Under.Cells,
                  Value  => Expr))),
            Calc => Under.Calc);

   exception

   when Formulas.Unbounded_Set =>

      raise Storage.Bounds.Unbounded;

   end Values;


   function Calc_Image (Item : Flux_T) return String
   is
   begin

      return
           "flux"
         & Flux_Id_T'Image (Item.Id)
         & " {"
         & Cell_Sets.Image (Item.Cells)
         & '}';

   end Calc_Image;


   function Bounded_Pool (
      Cells  : Cell_Set_T'Class;
      Bounds : Storage.Bounds.Cell_Interval_List_T;
      Calc   : Calc_Handle_T)
   return Pool_T
   is

      Valid_Bounds : Storage.Bounds.Cell_Interval_List_T :=
         Storage.Bounds.Intervals_For_Cells (Cells, Bounds);

   begin

      return New_Pool (
         Cells => Cell_Set_T (Cells),
         Value => Set (
            Cells  => Tuple (Cell_Set_T (Cells)),
            Bounds => Valid_Bounds),
         Calc => Calc);

   end Bounded_Pool;


   function Bounded_Pool (
      Pool   : Pool_T;
      Bounds : Storage.Bounds.Cell_Interval_List_T)
   return Pool_T
   is

      Valid_Bounds : Storage.Bounds.Cell_Interval_List_T :=
         Storage.Bounds.Intervals_For_Cells (Pool.Cells, Bounds);

   begin

      if Valid_Bounds'Length = 0 then
         -- Free and easy, remain the same:

         return Pool;

      else
         -- Bound and constrained, change:

         return New_Pool (
            Cells => Pool.Cells,
            Value => Intersection (
               Id (Pool),
               Set (
                  Cells  => Tuple (Pool.Cells),
                  Bounds => Valid_Bounds)),
            Calc => Pool.Calc);

      end if;

   end Bounded_Pool;


   function Bounded_Pool (
      Pool : Pool_T;
      Pre  : Arithmetic.Effect_T)
   return Pool_T
   is

      Ranges : constant Arithmetic.Effect_T :=
         Arithmetic.Pick (
            Kind => Arithmetic.Range_Pre,
            From => Pre);
      -- All the Range_Pre constraints in Pre. Perhaps none.

   begin

      if Ranges'Length = 0 then
         -- Free and easy, remain the same:

         return Pool;

      else
         -- Bound and constrained, change:

         return New_Pool (
            Cells => Pool.Cells,
            Value => Intersection (
               Id (Pool),
               Set (
                  Cells => Tuple (Pool.Cells),
                  Pre   => Ranges)),
            Calc => Pool.Calc);

      end if;

   end Bounded_Pool;


   function Intersection (Left, Right : Pool_T) return Pool_T
   is
   begin

      if Left.Calc /= Right.Calc then

         Report_Calc_Mixing ("Calculator.Intersection");

      end if;

      return New_Pool (
         Cells => Left.Cells,
         Value => Intersection (Id (Left), Id (Right)),
         Calc  => Left.Calc);

   end Intersection;


   function Complement (Pool : Pool_T) return Pool_T
   is
   begin

      return New_Pool (
         Cells => Pool.Cells,
         Value => Complement (Id (Pool)),
         Calc  => Pool.Calc);

   end Complement;


   function Range_Bounded_Flux (
      Flux   : Flux_T;
      Bounds : Storage.Bounds.Cell_Interval_List_T)
   return Flux_T
   is

      Valid_Bounds : Storage.Bounds.Cell_Interval_List_T :=
         Storage.Bounds.Intervals_For_Cells (Flux.Cells, Bounds);

   begin

      if Valid_Bounds'Length = 0 then
         -- Free and easy, remain the same:

         return Flux;

      else
         -- Bound and constrained, change:

         return New_Flux (
            Cells => Flux.Cells,
            Value => Restrict_Range (
               Relation => Id (Flux),
               Subrange => Set (
                  Cells  => Tuple (Flux.Cells),
                  Bounds => Valid_Bounds)),
            Calc => Flux.Calc);

      end if;

   end Range_Bounded_Flux;


   function Range_Bounded_Flux (
      Flux : Flux_T;
      Pre  : Arithmetic.Effect_T)
   return Flux_T
   is

      Ranges : constant Arithmetic.Effect_T :=
         Arithmetic.Pick (
            Kind => Arithmetic.Range_Pre,
            From => Pre);
      -- All the Range_Pre constraints in Pre. Perhaps none.

   begin

      if Ranges'Length = 0 then
         -- Free and easy, remain the same:

         return Flux;

      else
         -- Bound and constrained, change:

         return New_Flux (
            Cells => Flux.Cells,
            Value => Restrict_Range (
               Relation => Id (Flux),
               Subrange => Set (
                  Cells => Tuple (Flux.Cells),
                  Pre   => Ranges)),
            Calc => Flux.Calc);

      end if;

   end Range_Bounded_Flux;


   function Identity_Flux (Pool : Pool_T'Class) return Flux_T
   is
   begin

      return New_Flux (
         Cells => Pool.Cells,
         Value =>
            Restrict_Domain (
               Relation  => Identity_Relation (Pool.Cells),
               Subdomain => Id (Pool)),
         Calc => Pool.Calc);

   end Identity_Flux;


   function Domain_Of (Flux : Flux_T'Class)
   return Pool_T
   is
   begin

      return New_Pool (
         Cells => Flux.Cells,
         Value => Domain (Id (Flux)),
         Calc  => Flux.Calc);

   end Domain_Of;


   function Constrain_Range (
      Flux : Flux_T;
      Pool : Pool_T'Class)
   return Flux_T
   is
   begin

      if Flux.Calc /= Pool.Calc then
         Report_Calc_Mixing ("Calculator.Constrain_Range");
      end if;

      Calculator.Comment (
         Text => "Constrain_Range of flux to pool",
         Calc => Flux.Calc);

      return New_Flux (
         Cells => Flux.Cells,
         Value => Restrict_Range (
            Relation => Id (Flux),
            Subrange => Adapted (
               Pool => Pool,
               To   => Flux.Cells)),
            Calc  => Flux.Calc);

   end Constrain_Range;


   procedure Model_Looping_Flux (
      Luup       : in     Loops.Loop_T;
      Summary    : in     Loop_Summary_T;
      Entry_Flux : in     Flux_T;
      Full_Into  :    out Relation_T;
      Repeat     :    out Flux_T)
   --
   -- Models the data-flow into the head of a Luup and around the Luup,
   -- as part of the propagation of data flow along a flow-graph where
   -- loop summaries have been computed earlier.
   --
   -- Luup
   --    The loop under analysis.
   -- Summary
   --    The summary of this Luup.
   -- Entry_Flux
   --    The flux into the loop-head along forward edges, in other
   --    words the data-state that initializes the Luup for its first
   --    execution.
   -- Full_Into
   --    A model of the full data-flux into the loop-head, combining
   --    forward and repeat edges.
   -- Repeat
   --    An improved, input-sensitive model of the repeat-flux of
   --    the Luup (a single execution of the Luup up to a repeat
   --    edge). This is Summary.Repeat with domain constrained to
   --    the range of Full_Into.
   --
   is

      Basis : Cell_Set_T renames Entry_Flux.Cells;
      -- The basis cells for the analysis.

      Calc : Calc_Handle_T renames Entry_Flux.Calc;
      -- The calculator we use.

      Basic_Repeat : Flux_T;
      -- Summary.Repeat adapted (widened) to the Basis_Cells.

      Fuzz_Var : Relation_T;
      -- A relation that makes the loop-variant cells unknown and
      -- leaves the rest of the Basis cells unchanged (except for
      -- volatile cells which are made unkown). This models the
      -- effect of an unknown number of earlier loop iterations on
      -- the loop-variant cells, before the latest iteration (which
      -- is modelled by Basic_Repeat).

      Iterated_Repeat : Flux_T;
      -- The approximated iterated repetition relation for this loop,
      -- representing entry into the loop, an unknown number (zero
      -- or more) of loop iterations which fuzz the loop-variant
      -- cells, and a final loop-body execution that reaches (again
      -- or for the first time) a repeat edge.
      -- This is Entry_Flux join Fuzz_Var join Basic_Repeat.

      United_Into : Flux_T;
      -- The full flux into the loop-head, uniting the Entry_Flux
      -- with Iterated_Repeat.

      Head_Set : Set_T;
      -- The data-set that models the state at the start of the
      -- loop-head, after any number (zero or more) of loop
      -- repetitions. This models the "loop invariant" relations
      -- between cells, at the start of the loop head, but limited
      -- to the Summary.Repeat.Cells (because we need only those).
      -- The improved Repeat relation is Summary.Repeat with
      -- the domain constrained to Head_Set.

   begin

      Calculator.Comment (
         Text =>
              "Model_Looping_Flux for loop"
            & Loops.Loop_Index_T'Image (Loops.Loop_Index (Luup)),
         Calc => Calc);

      -- Adapt (widen) the summary (local) repeat-flux to handle
      -- the full Basis:

      Calculator.Comment (
         Text => "Summary repeat adapted to full Basis",
         Calc => Calc);

      Basic_Repeat := New_Flux (
         Cells => Basis,
         Value => Adapted (
            Flux => Summary.Repeat,
            To   => Basis),
         Calc  => Calc);

      -- Model the iterated repeat relation:

      Calculator.Comment (
         Text => "Iterated repeat relation",
         Calc => Calc);

      Fuzz_Var :=
         Relation (
            Domain => Tuple (Cells         => Basis),
            Rainge => Tuple (Cells         => Basis,
                             Dashed        => Summary.Variant,
                             Dash_Volatile => True));

      Iterated_Repeat := New_Flux (
         Cells => Basis,
         Value => Join (Join (
            Id (Entry_Flux),
            Fuzz_Var          ),
            Id (Basic_Repeat) ),
         Calc  => Calc);

      -- The combined flux into the loop-head is the union of
      -- the entry flux and the iterated repeat flux:

      Calculator.Comment (
         Text => "Combined entry + iteration flux to loop head",
         Calc => Calc);

      United_Into := New_Flux (
         Cells => Basis,
         Value => Union (Id (Entry_Flux), Id (Iterated_Repeat)),
         Calc  => Calc);

      Full_Into := Id (United_Into);

      -- Model the data-set at the start of the loop head as
      -- resulting either from the initial entry or from some
      -- number of iterations:

      Head_Set := Rainge (Join (
         Full_Into,
         Warp (From => Basis, To => Summary.Repeat.Cells)));

      -- Compute an improved single-repeat flux by restricting
      -- the domain of Summary.Repeat to the Head_Set:

      Calculator.Comment (
         Text => "Restrict repeat-domain to head-set",
         Calc => Calc);

      Repeat := New_Flux (
         Cells => Summary.Repeat.Cells,
         Value => Restrict_Domain (
            Relation  => Id (Summary.Repeat),
            Subdomain => Head_Set),
         Calc  => Calc);

   end Model_Looping_Flux;


   procedure Flux_To_Steps (
      Nodes      : in     Flow.Node_List_T;
      Edges      : in     Flow.Edge_List_T;
      Living     : in     Flow.Life.Living_T;
      Root       : in     Flux_T;
      Luups      : in     Loops.Loop_List_T;
      Summaries  : in     Loop_Summary_List_T;
      Steps      : in     Flow.Step_List_T;
      Into_Luups :    out Flux_List_T;
      Repeats    :    out Flux_List_T;
      Into_Steps :    out Flux_List_T;
      Exit_Flux  :    out Flux_T)
   is
      use type Calculator.Formulas.Formula_T;
      use type Flow.Step_Index_T;
      use type Loops.Loop_Index_T;


      Max_Step : constant Flow.Step_Index_T :=
         Flow.Max_Step (Flow.Life.Graph (Living));
      -- The maximum step-index in the flow-graph.

      Max_Node : constant Flow.Node_Index_T :=
         Flow.Max_Node (Flow.Life.Graph (Living));
      -- The maximum node-index in the flow-graph.

      Region : constant Flow.Node_List_T := Flow.Sort.By_Flow (Nodes, Edges);
      -- The nodes listed in precedence order using topological
      -- sorting of the edges in the acyclic region.

      Basis_Cells : Cell_Set_T renames Root.Cells;
      -- The cells to be treated as variables.

      Basis_Tuple : constant Tuple_T := Tuple (Basis_Cells);
      -- The "basis" tuple for the Omega relations is the set of
      -- cells to be treated as variables.

      Calc : constant Calc_Handle_T := Root.Calc;
      -- The calculator we use (for short).

      Not_A_Head : constant Natural := Luups'Last + 1;
      -- Placeholder to indicate that a given node is not
      -- a loop-head node.

      subtype Luups_Index_T is Natural range Luups'First .. Not_A_Head;
      -- Identifies one of the Luups, or "none of them".

      Luup : array (1 .. Max_Node) of Luups_Index_T :=
         (others => Not_A_Head);
      -- For a node N that is a loop-head, Luup(Index(N)) will be
      -- the index of the loop in Luups of which N is the head.
      -- For a node N that is not a loop-head, Luup(Index(N)) will
      -- be Not_A_Head, as in the initial value.

      Interest : array (1 .. Max_Step) of Boolean :=
        (others => False);
      -- For an interesting step S, Interest(Index(S)) will be true.

      Defined : array (Steps'Range) of Boolean := (others => False);
      -- Whether the flux for an interesting step was defined.
      -- If some False elements remain at the conclusion of
      -- data-flow propagation, then some of the interesting
      -- steps were not contained in the given region.

      Exit_Rel : Formula_T := Calculator.Formulas.Nil;
      -- The formula for the exit flux will be collected to here.


      procedure Define_Flux (
         For_Step : in     Flow.Step_T;
         Relation : in out Relation_T)
      --
      -- Defines the flux for an interesting step to be the flux
      -- defined by the given Relation, setting those elements in
      -- Into_Steps that refer to this step. Note that there can be
      -- several such elements in Into_Steps.
      --
      -- Updates the Relation to refer to this flux. Thus, the Relation
      -- keeps the same meaning but has a simpler form.
      --
      is
         use type Flow.Step_T;

         Flux : Flux_T;
         -- The flux defined by the Relation.

      begin

         -- Compute the Flux:

         Comment (
            Text =>
                 "Flux_To_Steps.Define_Flux for step"
               & Flow.Step_Index_T'Image (Flow.Index (For_Step)),
            Calc => Calc);

         Flux := New_Flux (
            Cells => Basis_Cells,
            Value => Relation,
            Calc  => Calc);

         -- Put this Flux in the relevant places in Into_Steps:

         for S in Steps'Range loop

            if Steps(S) = For_Step then

               Into_Steps(S) := Flux;

            end if;

            Defined(S) := True;

         end loop;

         -- Update the Relation to refer to this Flux:

         Relation := Id (Flux);

      end Define_Flux;


      function From_Relation_Unjoined (
         Into : Relation_T;
         Node : Flow.Node_T)
      return Relation_T
      --
      -- Computes the join of the flux into a node and the effects
      -- of the steps in a node, giving the "from" relation for the
      -- node, which is returned as a calculator formula (not yet
      -- assigned to a flux).
      --
      -- On the fly, computes and assigns fluxes for any interesting
      -- steps in the node. This is why we cannot use the function
      -- Calculator.Propagator.Effect, which would otherwise contain
      -- the core of this computation.
      --
      -- This procedure does *not* try to join the effects of consecutive
      -- steps into a single arithmetic effect. See the procedure
      -- From_Relation_Joined, below. This procedure is retained for
      -- comparison, but is considered obsolete.
      --
      is

         Node_Steps : constant Flow.Step_List_T := Flow.Steps_In (Node);
         -- The steps in the node. Their effects are joined to
         -- make up the effect of the whole node.

         From_Rel : Relation_T;
         -- The "from" relation formula.

      begin

         From_Rel := Into;

         for I in Node_Steps'Range loop

            if Interest(Flow.Index (Node_Steps(I))) then
               -- This step is interesting, so grab the flux
               -- to every place it is required.

               Define_Flux (
                  For_Step => Node_Steps(I),
                  Relation => From_Rel);

            end if;

            From_Rel := Join (
               From_Rel,
               Transformation (
                  Domain => Basis_Cells,
                  Effect => Flow.Life.Live_Effect (
                     Step   => Node_Steps(I),
                     Living => Living)));

         end loop;

         return From_Rel;

      end From_Relation_Unjoined;


      function From_Relation_Joined (
         Into : Relation_T;
         Node : Flow.Node_T)
      return Relation_T
      --
      -- Computes the join of the flux into a node and the effects
      -- of the steps in a node, giving the "from" relation for the
      -- node, which is returned as a calculator formula (not yet
      -- assigned to a flux).
      --
      -- On the fly, computes and assigns fluxes for any interesting
      -- steps in the node. This is why we cannot use the function
      -- Calculator.Propagator.Effect, which would otherwise contain
      -- the core of this computation. Another reason is that we use
      -- Flow.Life.Join_Effects to simplify the resulting relation
      -- expression.
      --
      is

         Node_Steps : constant Flow.Step_List_T := Flow.Steps_In (Node);
         -- The steps in the node. Their effects are joined to
         -- make up the effect of the whole node.

         Next : Positive := Node_Steps'First;
         -- The index of the next step to be processed in Node_Steps.

         Last : Natural;
         -- The maximal index such that Node_Steps(Next .. Last) can
         -- be joined into one Effect_T.

         Joint_Effect : Arithmetic.Assignment_Set_T (Max_Size =>
            1 + Flow.Life.Max_Joint_Effect_Length (Node_Steps, Living));
         -- The set of assignments from the joined effect of
         -- Node_Steps(Next .. Last).

         From_Rel : Relation_T;
         -- The "from" relation formula.

      begin  -- From_Relation_Joined

         From_Rel := Into;

         loop
            -- Next is in Node_Steps'Range and shows the step for
            -- which From_Rel is the incoming flux.

            -- Check if Next is an interesting step:

            if Interest(Flow.Index (Node_Steps(Next))) then
               -- This step is interesting, so grab the flux
               -- to every place it is required.

               Define_Flux (
                  For_Step => Node_Steps(Next),
                  Relation => From_Rel);

            end if;

            -- Find the longest sequence of steps Node_Steps(Next..)
            -- that we can to join into a single effect:

            Last := Next;

            while Last < Node_Steps'Last
            and then not Interest(Flow.Index(Node_Steps(Last + 1)))
            loop
               -- We can try to join Node_Steps(Next .. Last + 1) into
               -- a single effect.

               Last := Last + 1;

            end loop;

            Flow.Life.Join_Effects (
               Steps  => Node_Steps(Next .. Last),
               Living => Living,
               Basis  => Basis_Cells,
               Into   => Joint_Effect,
               Last   => Last);

            -- Join this effect to From_Rel:

            From_Rel := Join (
               From_Rel,
               Transformation (
                  Domain => Basis_Cells,
                  Effect => Arithmetic.To_Effect (Joint_Effect)));

            exit when Last >= Node_Steps'Last;

            Next := Last + 1;

         end loop;

         return From_Rel;

      end From_Relation_Joined;


      procedure Propagate (
         Node : in Flow.Node_T;
         Into : in Flow.Edge_List_T;
         From : in Flow.Edge_List_T)
      --
      -- Propagate data-flow over the given Node using the
      -- given lists of edges into the node and from the node.
      -- If the Node contains any interesting steps, assign
      -- the Fluxes for these steps on the fly.
      --
      is
         use type Flow.Node_Set_Ref;

         Nodex : constant Flow.Node_Index_T := Flow.Index (Node);
         -- The index of this node.

         Luups_Index : constant Luups_Index_T := Luup(Nodex);
         -- Shows the loop (if any) of which Node is the head.

         Into_Id : Identifier_T;
         -- The calculator identifier of the "into" relation
         -- of the node (the "effect" relation is not computed
         -- separately).

         Into_Rel : Relation_T;
         -- The total "into" flux of a node, considering also the
         -- case of a loop head, where repeat-edges are included.

         From_Id : constant Identifier_T := Formulas.From_Id (Nodex);
         -- The calculator identifier for the "from" relation.

         From_Rel : Relation_T;
         -- The formula for the "from" relation, depending on
         -- whether the node is a loop-head or not.

         Flow_Rel : Relation_T;
         -- A "flow" relation on an out-edge.

      begin  -- Propagate

         Comment (
            Text =>
                 "Flux_To_Steps.Propagate for node"
               & Flow.Node_Index_T'Image (Nodex),
            Calc => Calc);

         -- "Into" relation by union of incoming flows:

         Into_Id := Propagator.Flow_Into (
            Node  => Nodex,
            Along => Indices (Into),
            Root  => Root,
            Calc  => Calc);

         -- The full incoming flow is computed differently for
         -- loop-heads and normal nodes:

         if Luups_Index in Luups'Range then
            -- This is a loop-head node, so it receives data
            -- also from the loop's repeat-edges.

            -- Save the flux that initializes the loop, that is,
            -- the flux along forward edges to the loop-head:

            Calculator.Comment (
               Text => "Flux along forward edges into loop-head",
               Calc => Calc);

            Into_Luups(Luups_Index) := New_Flux (
               Cells => Basis_Cells,
               Value => Into_Id,
               Calc  => Calc);

            -- Model the loop repetitions (approximately) to
            -- compute the full flux into the loop-head and
            -- an improved repeat-relation:

            Model_Looping_Flux (
               Luup       => Luups(Luups_Index),
               Summary    => Summaries(Luups_Index),
               Entry_Flux => Into_Luups(Luups_Index),
               Full_Into  => Into_Rel,
               Repeat     => Repeats(Luups_Index));

         else
            -- This is not a loop-head node, so it receives
            -- data only from the forward Into edges.

            Into_Rel := Into_Id;

         end if;

         -- The "from" relation of the node joins the "into" relation
         -- to the effects of the steps in the node:

         if Opt.Join_Steps then

            From_Rel := From_Relation_Joined (
               Into => Into_Rel,
               Node => Node);

         else

            From_Rel := From_Relation_Unjoined (
               Into => Into_Rel,
               Node => Node);

         end if;
         --
         -- The flux into the interesting steps in Node is computed on
         -- the fly in From_Relation_(Joined|Unjoined).

         -- Note that the "effect" relation (flux) is not computed
         -- or stored separately.

         -- "From" flux:

         Assign (
            Target => From_Id,
            Value  => From_Rel,
            Calc   => Calc);


         if From'Length = 0 then
            -- A returning node.

            Exit_Rel := Union (Exit_Rel, From_Id);

         end if;

         -- "Flow" relations as "From" restricted by edge precondition:

         for E in From'Range loop

            Flow_Rel := Propagator.Flow_Out (
               From  => From_Id,
               Cond  => Flow.Computation.Condition (
                  Edge  => From(E),
                  Under => Flow.Life.Model (Living).all),
               Basis => Basis_Tuple);

            Assign (
               Target => Flow_Id (Flow.Index (From(E))),
               Value  => Flow_Rel,
               Calc   => Calc);

         end loop;

      end Propagate;


   begin  -- Flux_To_Steps

      -- Mark the loop heads:

      for L in Luups'range loop

         Luup(Flow.Index (Loops.Head_Node (Luups(L)))) := L;

      end loop;

      -- Mark the interesting steps:

      for S in Steps'Range loop

         Interest(Flow.Index (Steps(S))) := True;

      end loop;

      -- For each node in precedence order, compute the
      -- relations "into", "effect", "from", and the "flow"
      -- relations on the out-edges:

      for I in Region'Range loop

         Propagate (
            Node => Region(I),
            Into => Edges_Into (Pick => Region(I), From => Edges),
            From => Edges_From (Pick => Region(I), From => Edges));

      end loop;

      if Exit_Rel /= Nil then

         Exit_Flux := New_Flux (
            Cells => Basis_Cells,
            Value => Exit_Rel,
            Calc  => Calc);
      else

         Exit_Flux := New_Flux (
            Cells => Basis_Cells,
            Value => Relation (
               Domain => Basis_Tuple,
               Rainge =>
                  Tuple (
                     Cells         => Basis_Cells,
                     Dashed        => Basis_Cells,
                     Dash_Volatile => True)),
            Calc  => Calc);

      end if;

   end Flux_To_Steps;


   procedure Model_Looping_Pool (
      Luup       : in     Loops.Loop_T;
      Summary    : in     Loop_Summary_T;
      Entry_Pool : in     Pool_T;
      Full_Into  :    out Set_T;
      Repeat     :    out Flux_T)
   --
   -- Models the data-flow into the head of a Luup and around the Luup,
   -- as part of the propagation of data flow along a flow-graph where
   -- loop summaries have been computed earlier.
   --
   -- Luup
   --    The loop under analysis.
   -- Summary
   --    The summary of this Luup.
   -- Entry_Pool
   --    The pool into the loop-head from forward edges, in other
   --    words the data-state that initializes the Luup for its first
   --    execution.
   -- Full_Into
   --    A model of the full data-pool into the loop-head, combining
   --    forward and repeat edges.
   -- Repeat
   --    An improved, input-sensitive model of the repeat-flux of
   --    the Luup (a single execution of the Luup up to a repeat
   --    edge). This is Summary.Repeat with domain constrained to
   --    Full_Into.
   --
   is

      Basis : Cell_Set_T renames Entry_Pool.Cells;
      -- The basis cells for the analysis.

      Calc : Calc_Handle_T renames Entry_Pool.Calc;
      -- The calculator we use.

      Basic_Repeat : Flux_T;
      -- Summary.Repeat adapted (widened) to the Basis_Cells.

      Fuzz_Var : Relation_T;
      -- A relation that makes the loop-variant cells unknown and
      -- leaves the rest of the Basis cells unchanged (except for
      -- volatile cells which are made unknown). This models the
      -- effect of an unknown number of earlier loop iterations on
      -- the loop-variant cells, before the latest iteration (which
      -- is modelled by Basic_Repeat).

      Iterated_Repeat : Flux_T;
      -- The approximated iterated repetition relation for this loop,
      -- representing entry into the loop, an unknown number (zero
      -- or more) of loop iterations which fuzz the loop-variant
      -- cells, and a final loop-body execution that reaches (again
      -- or for the first time) a repeat edge.
      -- This is Fuzz_Var join Basic_Repeat, with domain constrained
      -- to the Entry_Pool.

      United_Into : Pool_T;
      -- The full pool into the loop-head, uniting the Entry_Pool
      -- with the range of Iterated_Repeat.

      Head_Set : Set_T;
      -- The data-set that models the state at the start of the
      -- loop-head, after any number (zero or more) of loop
      -- repetitions. This models the "loop invariant" relations
      -- between cells, at the start of the loop head, but limited
      -- to the Summary.Repeat.Cells (because we need only those).
      -- The improved Repeat relation is Summary.Repeat with
      -- the domain constrained to Head_Set.

   begin

      Calculator.Comment (
         Text =>
              "Model_Looping_Pool for loop"
            & Loops.Loop_Index_T'Image (Loops.Loop_Index (Luup)),
         Calc => Calc);

      -- Adapt (widen) the summary (local) repeat-flux to handle
      -- the full Basis:

      Calculator.Comment (
         Text => "Summary repeat adapted to full Basis",
         Calc => Calc);

      Basic_Repeat := New_Flux (
         Cells => Basis,
         Value => Adapted (
            Flux => Summary.Repeat,
            To   => Basis),
         Calc  => Calc);

      -- Model the iterated repeat relation:

      Calculator.Comment (
         Text => "Iterated repeat relation",
         Calc => Calc);

      Fuzz_Var :=
         Relation (
            Domain => Tuple (Cells         => Basis),
            Rainge => Tuple (Cells         => Basis,
                             Dashed        => Summary.Variant,
                             Dash_Volatile => True));

      Iterated_Repeat := New_Flux (
         Cells => Basis,
         Value => Join (
            Restrict_Domain (
               Relation  => Fuzz_Var,
               Subdomain => Id (Entry_Pool)),
            Id (Basic_Repeat)),
         Calc  => Calc);

      -- The combined pool into the loop-head is the union of
      -- the entry pool and the range of the iterated repeat flux:

      Calculator.Comment (
         Text => "Combined entry + iteration pool to loop head",
         Calc => Calc);

      United_Into := New_Pool (
         Cells => Basis,
         Value => Union (Id (Entry_Pool), Rainge (Id (Iterated_Repeat))),
         Calc  => Calc);

      Full_Into := Id (United_Into);

      -- Model the data-set at the start of the loop head as
      -- resulting either from the initial entry or from some
      -- number of iterations:

      Head_Set := Apply (
         Relation => Warp (From => Basis, To => Summary.Repeat.Cells),
         To_Set   => Full_Into);

      -- Compute an improved single-repeat flux by restricting
      -- the domain of Summary.Repeat to the Head_Set:

      Calculator.Comment (
         Text => "Restrict repeat-domain to head-set",
         Calc => Calc);

      Repeat := New_Flux (
         Cells => Summary.Repeat.Cells,
         Value => Restrict_Domain (
            Relation  => Id (Summary.Repeat),
            Subdomain => Head_Set),
         Calc  => Calc);

   end Model_Looping_Pool;


   procedure Pool_To_Steps (
      Nodes      : in     Flow.Node_List_T;
      Edges      : in     Flow.Edge_List_T;
      Living     : in     Flow.Life.Living_T;
      Root       : in     Pool_T;
      Luups      : in     Loops.Loop_List_T;
      Summaries  : in     Loop_Summary_List_T;
      Steps      : in     Flow.Step_List_T;
      Into_Luups :    out Pool_List_T;
      Repeats    :    out Flux_List_T;
      Into_Steps :    out Pool_List_T;
      Exit_Pool  :    out Pool_T)
   is
      use type Calculator.Formulas.Formula_T;
      use type Flow.Step_Index_T;
      use type Loops.Loop_Index_T;


      Max_Step : constant Flow.Step_Index_T :=
         Flow.Max_Step (Flow.Life.Graph (Living));
      -- The maximum step-index in the flow-graph.

      Max_Node : constant Flow.Node_Index_T :=
         Flow.Max_Node (Flow.Life.Graph (Living));
      -- The maximum node-index in the flow-graph.

      Region : constant Flow.Node_List_T := Flow.Sort.By_Flow (Nodes, Edges);
      -- The nodes listed in precedence order using topological
      -- sorting of the edges in the acyclic region.

      Basis_Cells : Cell_Set_T renames Root.Cells;
      -- The cells to be treated as variables.

      Basis_Tuple : constant Tuple_T := Tuple (Basis_Cells);
      -- The "basis" tuple for the Omega relations is the set of
      -- cells to be treated as variables.

      Calc : constant Calc_Handle_T := Root.Calc;
      -- The calculator we use (for short).

      Not_A_Head : constant Natural := Luups'Last + 1;
      -- Placeholder to indicate that a given node is not
      -- a loop-head node.

      subtype Luups_Index_T is Natural range Luups'First .. Not_A_Head;
      -- Identifies one of the Luups, or "none of them".

      Luup : array (1 .. Max_Node) of Luups_Index_T :=
         (others => Not_A_Head);
      -- For a node N that is a loop-head, Luup(Index(N)) will be
      -- the index of the loop in Luups of which N is the head.
      -- For a node N that is not a loop-head, Luup(Index(N)) will
      -- be Not_A_Head, as in the initial value.

      Interest : array (1 .. Max_Step) of Boolean :=
        (others => False);
      -- For an interesting step S, Interest(Index(S)) will be true.

      Defined : array (Steps'Range) of Boolean := (others => False);
      -- Whether the pool for an interesting step was defined.
      -- If some False elements remain at the conclusion of
      -- data-flow propagation, then some of the interesting
      -- steps were not contained in the given region.

      Exit_Set : Set_T := Calculator.Formulas.Nil;
      -- The formula for the exit pool will be collected here.

      Some_Infeasible_Edges : Boolean := False;
      -- Whether some edges were found to be infeasible, because
      -- their flow-sets are null.


      procedure Define_Pool (
         For_Step : in     Flow.Step_T;
         Set      : in out Set_T)
      --
      -- Defines the pool for an interesting step to be the pool
      -- defined by the given Set, setting those elements in
      -- Into_Steps that refer to this step. Note that there can be
      -- several such elements in Into_Steps.
      --
      -- Updates the Set to refer to this pool. Thus, the Set
      -- keeps the same meaning but has a simpler form.
      --
      is
         use type Flow.Step_T;

         Pool : Pool_T;
         -- The pool defined by the Set.

      begin

         -- Compute the Pool:

         Comment (
            Text =>
                 "Pool_To_Steps.Define_Pool for step"
               & Flow.Step_Index_T'Image (Flow.Index (For_Step)),
            Calc => Calc);

         Pool := New_Pool (
            Cells => Basis_Cells,
            Value => Set,
            Calc  => Calc);

         -- Put this Pool in the relevant places in Into_Steps:

         for S in Steps'Range loop

            if Steps(S) = For_Step then

               Into_Steps(S) := Pool;

            end if;

            Defined(S) := True;

         end loop;

         -- Update the Set to refer to this Pool:

         Set := Id (Pool);

      end Define_Pool;


      function From_Set_Unjoined (
         Into : Set_T;
         Node : Flow.Node_T)
      return Set_T
      --
      -- Applies the relation defined by the effect of the steps in a
      -- Node, to the data-set flowing Into the Node, giving the "from"
      -- set for the Node, which is returned as a calculator formula
      -- (not yet assigned to a pool).
      --
      -- On the fly, computes and assigns data-pools for any interesting
      -- steps in the Node. This is why we cannot use the function
      -- Calculator.Propagator.Effect, which would otherwise contain
      -- the core of this computation.
      --
      -- This procedure does *not* try to join the effects of consecutive
      -- steps into a single arithmetic effect. See the procedure
      -- From_Set_Joined, below. This procedure is retained for
      -- comparison, but is considered obsolete.
      --
      is

         Node_Steps : constant Flow.Step_List_T := Flow.Steps_In (Node);
         -- The steps in the node. Their effects are joined to
         -- make up the effect of the whole node.

         Trans : Relation_T := Nil;
         -- The transformation relation of the steps in the Node,
         -- built (joined) incrementally step by step. Reset to Nil
         -- at each Define_Pool.

         Trans_Flux : Flux_T;
         -- The Trans relation as a flux.

         Set : Set_T := Into;
         -- The current (transformed) data-set. Updated at each Define_Pool.

      begin

         for I in Node_Steps'Range loop

            if Interest(Flow.Index (Node_Steps(I))) then
               -- This step is interesting, so grab the pool
               -- to every place where it is required.

               if Trans /= Nil then
                  -- The Set must be transformed for this step.

                  Trans_Flux := New_Flux (
                     Cells => Basis_Cells,
                     Value => Trans,
                     Calc  => Calc);

                  Set := Apply (
                     Relation => Id (Trans_Flux),
                     To_Set   => Set);

                  Trans := Nil;

               end if;

               Define_Pool (
                  For_Step => Node_Steps(I),
                  Set      => Set);

            end if;

            Trans := Join (
               Trans,
               Transformation (
                  Domain => Basis_Cells,
                  Effect => Flow.Life.Live_Effect (
                     Step   => Node_Steps(I),
                     Living => Living)));

         end loop;

         if Trans /= Nil then
            -- The Set must be transformed for the final result.

            Trans_Flux := New_Flux (
               Cells => Basis_Cells,
               Value => Trans,
               Calc  => Calc);

            Set := Apply (
               Relation => Id (Trans_Flux),
               To_Set   => Set);

         end if;

         return Set;

      end From_Set_Unjoined;


      function From_Set_Joined (
         Into : Relation_T;
         Node : Flow.Node_T)
      return Set_T
      --
      -- Applies the relation defined by the effect of the steps in a
      -- Node, to the data-set flowing Into the Node, giving the "from"
      -- set for the Node, which is returned as a calculator formula
      -- (not yet assigned to a pool).
      --
      -- On the fly, computes and assigns data-pools for any interesting
      -- steps in the Node. This is why we cannot use the function
      -- Calculator.Propagator.Effect, which would otherwise contain
      -- the core of this computation. Another reason is that we use
      -- Flow.Life.Join_Effects to simplify the resulting relation
      -- expression.
      --
      is

         Node_Steps : constant Flow.Step_List_T := Flow.Steps_In (Node);
         -- The steps in the node. Their effects are joined to
         -- make up the effect of the whole node.

         Next : Positive := Node_Steps'First;
         -- The index of the next step to be processed in Node_Steps.

         Last : Natural;
         -- The maximal index such that Node_Steps(Next .. Last) can
         -- be joined into one Effect_T.

         Joint_Effect : Arithmetic.Assignment_Set_T (Max_Size =>
            1 + Flow.Life.Max_Joint_Effect_Length (Node_Steps, Living));
         -- The set of assignments from the joined effect of
         -- Node_Steps(Next .. Last).

         Trans : Relation_T := Nil;
         -- The transformation relation of the steps in the Node,
         -- built (joined) incrementally step by step. Reset to Nil
         -- at each Define_Pool.

         Trans_Flux : Flux_T;
         -- The Trans relation as a flux.

         Set : Set_T := Into;
         -- The current (transformed) data-set. Updated at each Define_Pool.

      begin

         loop
            -- Next is in Node_Steps'Range and shows the step for
            -- which Apply (Trans, Set) is the incoming pool.

            -- Check if Next is an interesting step:

            if Interest(Flow.Index (Node_Steps(Next))) then
               -- This step is interesting, so grab the pool
               -- to every place it is required.

               if Trans /= Nil then
                  -- The Set must be transformed for this step.

                  Trans_Flux := New_Flux (
                     Cells => Basis_Cells,
                     Value => Trans,
                     Calc  => Calc);

                  Set := Apply (
                     Relation => Id (Trans_Flux),
                     To_Set   => Set);

                  Trans := Nil;

               end if;

               Define_Pool (
                  For_Step => Node_Steps(Next),
                  Set      => Set);

            end if;

            -- Find the longest sequence of steps Node_Steps(Next..)
            -- that we can to join into a single effect:

            Last := Next;

            while Last < Node_Steps'Last
            and then not Interest(Flow.Index(Node_Steps(Last + 1)))
            loop
               -- We can try to join Node_Steps(Next .. Last + 1) into
               -- a single effect.

               Last := Last + 1;

            end loop;

            Flow.Life.Join_Effects (
               Steps  => Node_Steps(Next .. Last),
               Living => Living,
               Basis  => Basis_Cells,
               Into   => Joint_Effect,
               Last   => Last);

            -- Join this effect to Trans:

            Trans := Join (
               Trans,
               Transformation (
                  Domain => Basis_Cells,
                  Effect => Arithmetic.To_Effect (Joint_Effect)));

            exit when Last >= Node_Steps'Last;

            Next := Last + 1;

         end loop;

         if Trans /= Nil then
            -- The Set must be transformed for the final result.

            Trans_Flux := New_Flux (
               Cells => Basis_Cells,
               Value => Trans,
               Calc  => Calc);

            Set := Apply (
               Relation => Id (Trans_Flux),
               To_Set   => Set);

         end if;

         return Set;

      end From_Set_Joined;


      procedure Mark_Infeasible (Edge : Flow.Step_Edge_T)
      --
      -- Marks this Edge as infeasible.
      --
      is

         Symbol_Table : constant Symbols.Symbol_Table_T :=
            Programs.Symbol_Table (Flow.Life.Program (Living));
         -- The symbol-table, for locus.

      begin

         if Flow.Pruning.Opt.Warn_Unreachable then

            Output.Warning (
               Locus => Flow.Show.Locus (Flow.Source (Edge), Symbol_Table),
               Text  =>
                    "Unreachable flow to instruction at "
                  & Output.Image (
                       Item   => Flow.Show.All_Statements (
                          Step    => Flow.Target (Edge),
                          Source  => Symbol_Table),
                          Address => True));

         end if;

         Flow.Computation.Mark_Infeasible (
            Edge  => Edge,
            Under => Flow.Life.Model (Living).all);

         Some_Infeasible_Edges := True;

      end Mark_Infeasible;


      procedure Check_Null_Flow (
         Edge : Flow.Edge_T;
         Set  : Set_T)
      --
      -- Check if the flow Set over this Edge is null (infeasible)
      -- and in that case mark the Edge as infeasible.
      --
      is
      begin

         Calculator.Comment (
            Text => "Check for null flow",
            Calc => Calc);

         if Is_Null (Set => Set, Cells => Basis_Tuple, Calc => Calc) then

            Mark_Infeasible (Flow.Step_Edge (Edge));

         end if;

      end Check_Null_Flow;


      procedure Propagate (
         Node : in Flow.Node_T;
         Into : in Flow.Edge_List_T;
         From : in Flow.Edge_List_T)
      --
      -- Propagate data-flow over the given Node using the
      -- given lists of edges into the node and from the node.
      -- If the Node contains any interesting steps, assign
      -- the pools for these steps on the fly.
      --
      is
         use type Flow.Node_Set_Ref;

         Nodex : constant Flow.Node_Index_T := Flow.Index (Node);
         -- The index of this node.

         Luups_Index : constant Luups_Index_T := Luup(Nodex);
         -- Shows the loop (if any) of which Node is the head.

         Into_Id : Identifier_T;
         -- The calculator identifier of the "into" pool of the node,
         -- considering only the forward edges to the node.

         Into_Set : Set_T;
         -- The total "into" pool of a node, considering also the
         -- case of a loop head, where repeat-edges are included.

         From_Id : constant Identifier_T := Formulas.From_Id (Nodex);
         -- The calculator identifier for the "from" pool.

         From_Set : Set_T;
         -- The formula for the "from" set, depending on
         -- whether the node is a loop-head or not.

         Flow_Set : Set_T;
         -- A "flow" set on an out-edge.

      begin  -- Propagate

         Comment (
            Text =>
                 "Pool_To_Steps.Propagate for node"
               & Flow.Node_Index_T'Image (Nodex),
            Calc => Calc);

         -- "Into" set by union of incoming pools:

         Into_Id := Propagator.Pool_Into (
            Node  => Nodex,
            Along => Indices (Into),
            Root  => Root,
            Calc  => Calc);

         -- The full incoming pool is computed differently for
         -- loop-heads and normal nodes:

         if Luups_Index in Luups'Range then
            -- This is a loop-head node, so it receives data
            -- also from the loop's repeat-edges.

            -- Save the pool that initializes the loop, that is,
            -- the pool from forward edges to the loop-head:

            Calculator.Comment (
               Text => "Pool from forward edges into loop-head",
               Calc => Calc);

            Into_Luups(Luups_Index) := New_Pool (
               Cells => Basis_Cells,
               Value => Into_Id,
               Calc  => Calc);

            -- Model the loop repetitions (approximately) to
            -- compute the full pool into the loop-head and
            -- an improved repeat-relation:

            Model_Looping_Pool (
               Luup       => Luups(Luups_Index),
               Summary    => Summaries(Luups_Index),
               Entry_Pool => Into_Luups(Luups_Index),
               Full_Into  => Into_Set,
               Repeat     => Repeats(Luups_Index));

         else
            -- This is not a loop-head node, so it receives
            -- data only from the forward Into edges.

            Into_Set := Into_Id;

         end if;

         -- The "from" set of the node maps the "into" relation
         -- through the effects of the steps in the node:

         if Opt.Join_Steps then

            From_Set := From_Set_Joined (
               Into => Into_Set,
               Node => Node);

         else

            From_Set := From_Set_Unjoined (
               Into => Into_Set,
               Node => Node);

         end if;
         --
         -- The pools into the interesting steps in Node are computed
         -- on the fly in From_Set_(Joined|Unjoined).

         -- Note that the "effect" relation (flux) is not computed
         -- or stored separately.

         -- "From" pool:

         Assign (
            Target => From_Id,
            Value  => From_Set,
            Calc   => Calc);

         if From'Length = 0 then
            -- A returning node.

            Exit_Set := Union (Exit_Set, From_Id);

         end if;

         -- Compute the "Flow" sets as "From" restricted by the
         -- edge preconditions:

         for E in From'Range loop

            Flow_Set := Propagator.Set_Out (
               From  => From_Id,
               Cond  => Flow.Computation.Condition (
                  Edge  => From(E),
                  Under => Flow.Life.Model (Living).all),
               Basis => Basis_Tuple);

            Assign (
               Target => Flow_Id (Flow.Index (From(E))),
               Value  => Flow_Set,
               Calc   => Calc);

            if Opt.Find_Null_Flow then

               Check_Null_Flow (Edge => From(E), Set => Flow_Set);

            end if;

         end loop;

      end Propagate;


   begin  -- Pool_To_Steps

      -- Mark the loop heads:

      for L in Luups'range loop

         Luup(Flow.Index (Loops.Head_Node (Luups(L)))) := L;

      end loop;

      -- Mark the interesting steps:

      for S in Steps'Range loop

         Interest(Flow.Index (Steps(S))) := True;

      end loop;

      -- For each node in precedence order, compute the "into"
      -- pool, the "effect" relation, the "from" pool, and the
      -- "flow" pools on the out-edges:

      for I in Region'Range loop

         Propagate (
            Node => Region(I),
            Into => Edges_Into (Pick => Region(I), From => Edges),
            From => Edges_From (Pick => Region(I), From => Edges));

      end loop;

      if Exit_Set /= Nil then
         -- There were some exit edges.

         Exit_Pool := New_Pool (
            Cells => Basis_Cells,
            Value => Exit_Set,
            Calc  => Calc);
      else
         -- There are no exit edges TBC.

         Exit_Pool := New_Pool (
            Cells => Basis_Cells,
            Value => Set (
               Cells => Basis_Tuple,
               Cond  => Arithmetic.Always),
            Calc  => Calc);

      end if;

      if Some_Infeasible_Edges then

         Flow.Computation.Prune (Flow.Life.Model (Living).all);

      end if;

   end Pool_To_Steps;


   function Union (Fluxes : Flux_List_T) return Flux_T
   is

      Autograph : constant String := "Calculator.Union";
      -- For fault messages.

      First : constant Positive := Fluxes'First;
      -- For short.

      Uniting : Relation_T;
      -- The formula that unites the given fluxes.

   begin

      -- There must be something to unite:

      if Fluxes'Length = 0 then

         Output.Fault (
            Location => Autograph,
            Text     => "Flux list is empty");

         raise Calculator_Error;

      end if;

      -- Build the formula that unites the given fluxes:

      Uniting := Nil;

      for F in Fluxes'Range loop

         -- Check that the given fluxes all come from the same
         -- calculator instance:

         if Fluxes(F).Calc /= Fluxes(First).Calc then
            Report_Calc_Mixing (Autograph);
         end if;

         -- Check that the given fluxes all have the same basis
         -- set of cells:

         Check_Same_Cells (
            Left     => Fluxes(First).Cells,
            Right    => Fluxes(F    ).Cells,
            Location => Autograph);

         Uniting := Union (Uniting, Id (Fluxes(F)));

      end loop;

      -- Create the new flux and assign it the uniting:

      return New_Flux (
         Cells => Fluxes(First).Cells,
         Value => Uniting,
         Calc  => Fluxes(First).Calc);

   end Union;


   function Invariants_In_Flux (Flux : Flux_T)
   return Storage.Cell_List_T
   is

      Cells  : constant Storage.Cell_List_T := Cell_Sets.To_List (Flux.Cells);
      -- Cells in basis set.

      Basis : constant Tuple_T := Tuple (Flux.Cells);
      -- The basis tuple.

      Cell_I : Tuple_T;
      -- The singleton tuple containing the cell being studied.

      Widen  : Relation_T;
      Narrow : Relation_T;
      Joined : Relation_T;
      Ident  : Relation_T;
      -- Subexpressions of the invariance query.

      Invariant : Boolean;
      -- The result of the invariance query for a cell.

      Inv : Storage.Cell_List_T (1 .. Cells'Length);
      -- Collects the cells to return, initially none.

      Last : Natural := 0;
      -- The collected cells are Inv (1 .. Last).

   begin

      for I in Cells'Range loop

         -- Compute the "invariance query" for this cell as the
         -- Omega expression:
         --
         --   ({[C]->[basis]} . F . {[basis]->[C]}) subset {[C]->[C]}
         --
         -- where F is the given flux, with "basis" the set of
         -- basis cells and C the cell being tested.

         Cell_I := Tuple (Cells(I));

         Comment (
            Text =>
                 "Checking invariance of "
               & Storage.Name_Of (Cells(I)),
            Calc => Flux.Calc);

         Widen := Relation (
            Domain => Cell_I,
            Rainge => Basis);
         -- {[C]->[basis]}.

         Narrow := Relation (
            Domain => Basis,
            Rainge => Cell_I);
         -- {[basis]->[C]}.

         Joined := Parens (Join (Widen, Join (Id (Flux), Narrow)));
         -- ({[C]->[basis]} . F . {[basis]->[C]}).

         Ident := Identity_Relation (Cell_I);
         -- {[C]->[C]}.

         -- Evaluate the subset query:

         Invariant := Subset (
            Left  => Joined,
            Right => Ident,
            Calc  => Flux.Calc);

         if Invariant then
            -- Add the cell as invariant.
            Last := Last + 1;
            Inv(Last) := Cells(I);
         end if;

      end loop;

      return Inv(1..Last);

   end Invariants_In_Flux;


   function Flux_To_Vary (
      Basis : Cell_Set_T'Class;
      Var   : Cell_Set_T'Class;
      Calc  : Calc_Handle_T)
   return Flux_T
   is
   begin

      -- Return the Omega relation {[B]->[C]}, where
      -- "B" is the list of basis variables and "C" is the list
      -- of basis variables with a tick (') placed on every "varying"
      -- variable:

      return New_Flux (
         Cells => Cell_Set_T (Basis),
         Value =>
            Relation (
               Domain => Tuple (Cells         => Cell_Set_T (Basis)),
               Rainge => Tuple (Cells         => Cell_Set_T (Basis),
                                Dashed        => Cell_Set_T (Var  ),
                                Dash_Volatile => True)),
         Calc => Calc);

   end Flux_To_Vary;


   function Repeat_With_Induction (
      Initial   : Pool_T'Class;
      Invariant : Cell_Set_T'Class;
      Induction : Storage.Cell_List_T;
      Step      : Storage.Bounds.Interval_List_T;
      Repeat    : Flux_T'Class)
   return Flux_T
   is

      Repeat_Count : constant Tuple_T :=
         Tuple_With_Counter (Repeat.Cells);
      -- The tuple of the Repeat cells, plus the synthetic joint
      -- iteration counter.

      Induct : constant Relation_T :=
         Formulas.Induction_Model (
            Cells     => Repeat.Cells,
            Invariant => Cell_Set_T (Invariant),
            Induction => Induction,
            Step      => Step);
      -- The relation between the initial values of the Induction
      -- variables, and their values at the start of a given
      -- iteration of the loop. The domain is Repeat_Count and
      -- the range is Repeat.Cells.

   begin

      return New_Flux (
         Cells => Repeat.Cells,
         Value => Join (Join (
            Relation (
               Domain => Counter_Tuple,
               Rainge => Repeat_Count),
            Parens (
               Restrict_Domain (
                  Relation  => Induct,
                  Subdomain => Join (
                     Id (Initial),
                     Relation (
                        Domain => Tuple (Initial.Cells),
                        Rainge => Repeat_Count))))),
            Id (Repeat)),
         Calc => Repeat.Calc);

   end Repeat_With_Induction;


   function Is_In (Value : Arithmetic.Value_T; Pool : Pool_T)
   return Boolean
   is
   begin

      return Subset (
         Left  => Singleton (Value),
         Right => Id (Pool),
         Calc  => Pool.Calc);

   end Is_In;


   function Smallest_Value_By_Search (
      Pool : Pool_T;
      Min  : Arithmetic.Value_T)
   return Arithmetic.Value_T
   --
   -- The smallest value in the given one-dimensional Pool, no less
   -- than Min, discovered by binary search. Propagates Null_Set_Error
   -- if no such value is found, perhaps because the actual value is
   -- too large.
   --
   is
      use type Arithmetic.Value_T;

      Abs_Max : constant Arithmetic.Value_T := Opt.Max_Int_Calc;
      -- The absolutely largest value considered.

      First : Arithmetic.Value_T := Min;
      Last  : Arithmetic.Value_T :=
         Arithmetic.Value_T'Max (Min, 0) + Opt.Initial_Limit_Sill;
      -- The current range of the search.
      -- The Last value is forced to be positive (and reasonably large)
      -- to ensure that repeated doubling reaches Abs_Max soon.

      Sill : Arithmetic.Value_T;
      -- The current sill point (cut-off) in the binary-search phase.

      None_Here : Boolean;
      -- There are no candidates in the current interval.

   begin

      -- Find a range that contains a candidate:

      loop

         None_Here := Is_Null (
            Set => Intersection (
               Id (Pool),
               Interval (Storage.Bounds.Interval (
                  Min => First,
                  Max => Last))),
            Calc => Pool.Calc);

         exit when not None_Here;
         -- Some values found in First .. Last.

         -- There are no candidates in the interval First .. Last.

         if Last >= Abs_Max then
            -- Cannot check larger values.

            if not Is_Null (
               Set => Intersection (
        	  Id (Pool),
        	  Interval ((
                     Min => Storage.Bounds.Limit (Min),
                     Max => Storage.Bounds.Not_Limited))),
               Calc => Pool.Calc)

            then

               Output.Warning (
                    "Non-null set appears null; smallest value may be > "
                  & Arithmetic.Image (Abs_Max));

            end if;

            raise Null_Set_Error;

         end if;

         -- Look at larger values:

         First := Last + 1;

         if Last <= Abs_Max / 2 then

            Last := 2 * Last;

         else

            Last := Abs_Max;

         end if;

      end loop;

      -- Now we know that there is a candidate in First .. Last.
      -- Locate the least value by binary search:

      while First < Last loop

         Sill := First + (Last - First) / 2;

         None_Here := Is_Null (
            Set => Intersection (
               Id (Pool),
               Interval (Storage.Bounds.Interval (
                  Min => First,
                  Max => Sill))),
            Calc => Pool.Calc);

         if None_Here then
            -- No values in First .. Sill.

            First := Sill + 1;

         else
            -- Some values in First .. Sill.

            Last := Sill;

         end if;

      end loop;

      return First;

   end Smallest_Value_By_Search;


   function Smallest_Value (
      Pool : Pool_T;
      Min  : Arithmetic.Value_T)
   return Arithmetic.Value_T
   is
      use Storage.Bounds;

      Hull : Interval_T;
      -- The bounds from the hull, if optionally used.

      Smallest : Arithmetic.Value_T;
      -- The smallest value found.

   begin

      case Opt.Limit_Method.Value is

      when Opt.Search =>
         -- Binary search for the smallest value >= Min.

         Smallest := Smallest_Value_By_Search (Pool, Min);

      when Opt.Hull =>
         -- Use the lower bound of the hull set.

         Hull := Hull_Bound (
            Set  => Intersection (
                       Id (Pool),
                       Interval ((
                          Min => Limit (Min),
                          Max => Not_Limited))),
            Calc => Pool.Calc);

         begin

            Smallest := Storage.Bounds.Min (Hull);

         exception

         when Unbounded =>

            raise Null_Set_Error;

         end;

         -- We know the Smallest value of the Hull.

         if Opt.Check_Hull_Limit
         and then not Is_In (Smallest, Pool)
         then

            if Opt.Warn_Hull_Loose then

               Output.Warning (
                    "Smallest value "
        	  & Arithmetic.Image (Smallest)
        	  & " of hull is not in the set. Using binary search.");

            end if;

            Smallest := Smallest_Value_By_Search (Pool, Smallest);

         end if;

      end case;

      return Smallest;

   end Smallest_Value;


   function Largest_Value_By_Search (
      Pool : Pool_T;
      Max  : Arithmetic.Value_T)
   return Arithmetic.Value_T
   --
   -- The largest value in the given one-dimensional Pool, no greater
   -- than Max, discovered by binary search. Propagates Null_Set_Error
   -- if no such value is found, perhaps because the actual value is
   -- too small (a large negative number).
   --
   is
      use type Arithmetic.Value_T;

      Abs_Min : constant Arithmetic.Value_T := Opt.Min_Int_Calc;
      -- The absolutely smallest value considered.

      First : Arithmetic.Value_T :=
         Arithmetic.Value_T'Min (Max, 0) - Opt.Initial_Limit_Sill;
      Last  : Arithmetic.Value_T := Max;
      -- The current range of the search.
      -- The First value is forced to be negative (and reasonably large)
      -- to ensure that repeated doubling reaches Abs_Min soon.

      Sill : Arithmetic.Value_T;
      -- The current sill point (cut-off) in the binary-search phase.

      None_Here : Boolean;
      -- There are no candidates in the current interval.

   begin

      -- Find a range that contains a candidate:

      loop

         None_Here := Is_Null (
            Set => Intersection (
               Id (Pool),
               Interval (Storage.Bounds.Interval (
                  Min => First,
                  Max => Last))),
            Calc => Pool.Calc);

         exit when not None_Here;
         -- Some values found in First .. Last.

         -- There are no candidates in the interval First .. Last.

         if First <= Abs_Min then
            -- Cannot check larger values.

            if not Is_Null (
               Set => Intersection (
        	  Id (Pool),
        	  Interval ((
                     Min => Storage.Bounds.Not_Limited,
                     Max => Storage.Bounds.Limit (Max)))),
               Calc => Pool.Calc)

            then

               Output.Warning (
                    "Non-null set appears null; largest value may be < "
                  & Arithmetic.Image (Abs_Min));

            end if;

            raise Null_Set_Error;

         end if;

         -- Look at smaller values:

         Last := First - 1;

         if First >= Abs_Min / 2 then

            First := 2 * First;

         else

            First := Abs_Min;

         end if;

      end loop;

      -- Now we know that there is a candidate in First .. Last.
      -- Locate the least value by binary search:

      while First < Last loop

         Sill := Last - (Last - First) / 2;

         None_Here := Is_Null (
            Set => Intersection (
               Id (Pool),
               Interval (Storage.Bounds.Interval (
                  Min => Sill,
                  Max => Last))),
            Calc => Pool.Calc);

         if None_Here then
            -- No values in Sill .. Last.

            Last := Sill - 1;

         else
            -- Some values in Sill .. Last.

            First := Sill;

         end if;

      end loop;

      return Last;

   end Largest_Value_By_Search;


   function Largest_Value (
      Pool : Pool_T;
      Max  : Arithmetic.Value_T)
   return Arithmetic.Value_T
   is

      use Storage.Bounds;

      Hull : Interval_T;
      -- The bounds from the hull, if optionally used.

      Largest : Arithmetic.Value_T;
      -- The largest value found.

   begin

      case Opt.Limit_Method.Value is

      when Opt.Search =>
         -- Binary search for the largest value <= Max.

         Largest := Largest_Value_By_Search (Pool, Max);

      when Opt.Hull =>
         -- Use the lower bound of the hull set.

         Hull := Hull_Bound (
            Set  => Intersection (
                       Id (Pool),
                       Interval ((
                          Min => Not_Limited,
                          Max => Limit (Max)))),
            Calc => Pool.Calc);

         begin

            Largest := Storage.Bounds.Max (Hull);

         exception

         when Unbounded =>

            raise Null_Set_Error;

         end;

         -- We know the Largest value of the Hull.

         if Opt.Check_Hull_Limit
         and then not Is_In (Largest, Pool)
         then

            if Opt.Warn_Hull_Loose then

               Output.Warning (
                    "Largest value "
        	  & Arithmetic.Image (Largest)
        	  & " of hull is not in the set. Using binary search.");

            end if;

            Largest := Largest_Value_By_Search (Pool, Largest);

         end if;

      end case;

      return Largest;

   end Largest_Value;


   function Smallest_Hole (
      Pool : Pool_T;
      Min  : Arithmetic.Value_T)
   return Arithmetic.Value_T
   is
      use type Arithmetic.Value_T;

      Abs_Max : constant Arithmetic.Value_T := Opt.Max_Int_Calc;
      -- The absolutely largest value considered.

      First : Arithmetic.Value_T := Min;
      Last  : Arithmetic.Value_T :=
         Arithmetic.Value_T'Max (Min, 0) + Opt.Initial_Limit_Sill;
      -- The current range of the search.
      -- The Last value is forced to be positive (and reasonably large)
      -- to ensure that repeated doubling reaches Abs_Max soon.

      Sill : Arithmetic.Value_T;
      -- The current sill point (cut-off) in the binary-search phase.

      None_Here : Boolean;
      -- There are no candidates in the current interval.

   begin

      -- Find a range that contains a candidate:

      loop

         None_Here := Superset (
            Set         => Id (Pool),
            Of_Interval => Storage.Bounds.Interval (
               Min => First,
               Max => Last),
            Calc => Pool.Calc);

         exit when not None_Here;
         -- Some holes found in First .. Last.

         -- There are no candidates in the interval First .. Last
         -- because this interval is a subset of the Pool.

         if Last >= Abs_Max then
            -- Cannot check larger values.

            if not Superset (
               Set         => Id (Pool),
               Of_Interval => (
                  Min => Storage.Bounds.Limit (Min),
                  Max => Storage.Bounds.Not_Limited),
               Calc => Pool.Calc)

            then

               Output.Warning (
                    "Non-full set appears full; smallest hole may be > "
                  & Arithmetic.Image (Abs_Max));

            end if;

            raise Null_Set_Error;

         end if;

         -- Look at larger values:

         First := Last + 1;

         if Last <= Abs_Max / 2 then

            Last := 2 * Last;

         else

            Last := Abs_Max;

         end if;

      end loop;

      -- Now we know that there is a candidate (a hole) in First .. Last.
      -- Locate the least hole by binary search:

      while First < Last loop

         Sill := First + (Last - First) / 2;

         None_Here := Superset (
            Set         => Id (Pool),
            Of_Interval => Storage.Bounds.Interval (
               Min => First,
               Max => Sill),
            Calc => Pool.Calc);

         if None_Here then
            -- No holes in First .. Sill.

            First := Sill + 1;

         else
            -- Some holes in First .. Sill.

            Last := Sill;

         end if;

      end loop;

      return First;

   end Smallest_Hole;


   function Largest_Hole (
      Pool : Pool_T;
      Max  : Arithmetic.Value_T)
   return Arithmetic.Value_T
   is
   begin

      Output.Fault (
         Location => "Calculator.Largest_Hole",
         Text     => "Operation not implemented.");

      raise Program_Error;

      return 0; -- TBA, TBC

   end Largest_Hole;


   function Bounds_Of (Pool : Pool_T)
   return Storage.Bounds.Interval_T
   is
   begin

      return Hull_Bound (
         Set  => Id (Pool),
         Calc => Pool.Calc);

   end Bounds_Of;


   function Bounds_Of_Complement (
      Pool   : Pool_T;
      Within : Storage.Bounds.Interval_T)
   return Storage.Bounds.Interval_T
   is
   begin

      return Hull_Bound (
         Set  => Intersection (Complement (Id (Pool)), Interval (Within)),
         Calc => Pool.Calc);

   end Bounds_Of_Complement;


   function Bounds_Of_Domain (Flux : Flux_T)
   return Storage.Bounds.Interval_T
   is
   begin

      return Hull_Bound (
         Set  => Domain (Id (Flux)),
         Calc => Flux.Calc);

   end Bounds_Of_Domain;


   function Is_In_Domain (Value : Arithmetic.Value_T; Flux : Flux_T)
   return Boolean
   is
   begin

      return Subset (
         Left  => Singleton (Value),
         Right => Domain (Id (Flux)),
         Calc  => Flux.Calc);

   end Is_In_Domain;


   function Project (
      Bounds : Bounds_T'Class;
      To     : Storage.Cell_T)
   return Relation_T
   --
   -- The relation that projects the Cells_Of Bounds, To the
   -- one-dimensional space consisting of the single cell.
   --
   -- If the Width of the Cell is not too large, the projection
   -- also applies the "mod 2**Width" function.
   --
   is
   begin

      return Projection (
         Domain => Bounds.Cells,
         To     => To);

   end Project;


   function Map (
      Bounds : Bounds_T'Class;
      To     : Arithmetic.Expr_Ref)
   return Relation_T
   --
   -- The relation that maps the Cells_Of Bounds, To the one-
   -- dimensional space consisting of the values of the expression.
   --
   -- If the Width of the Cell is not too large, the projection
   -- also applies the "mod 2**Width" function.
   --
   is
   begin

      return Mapping (
         Domain => Bounds.Cells,
         Value  => To);

   end Map;


   function Bounds_From_Pool (
      Pool : Pool_T;
      Cell : Storage.Cell_T)
   return Storage.Bounds.Interval_T
   is
   begin

      return Hull_Bound (
         Set  => Apply (
            Relation => Project (Pool, Cell),
            To_Set   => Id (Pool)),
         Calc => Pool.Calc);

   end Bounds_From_Pool;


   function Bounds_From_Pool (
      Pool : Pool_T;
      Expr : Arithmetic.Expr_Ref)
   return Storage.Bounds.Interval_T
   is
   begin

      return Hull_Bound (
         Set  => Apply (
            Relation => Map (Pool, Expr),
            To_Set   => Id (Pool)),
         Calc => Pool.Calc);

   end Bounds_From_Pool;


   function Bounds_From_Flux (
      Flux : Flux_T;
      Cell : Storage.Cell_T)
   return Storage.Bounds.Interval_T
   is
   begin

      return Hull_Bound (
         Set  => Rainge (Join (Id (Flux), Project (Flux, Cell))),
         Calc => Flux.Calc);

   end Bounds_From_Flux;


   function Bounds_From_Complement (
      Flux : Flux_T;
      Cell : Storage.Cell_T)
   return Storage.Bounds.Interval_T
   is
   begin

      return Hull_Bound (
         Set  => Complement (Rainge (Join (Id (Flux), Project (Flux, Cell)))),
         Calc => Flux.Calc);

   end Bounds_From_Complement;


   function Bounds_From_Flux (
      Flux : Flux_T;
      Expr : Arithmetic.Expr_Ref)
   return Storage.Bounds.Interval_T
   is
   begin

      return Hull_Bound (
         Set  => Rainge (Join (Id (Flux), Map (Flux, Expr))),
         Calc => Flux.Calc);

   end Bounds_From_Flux;


   function Is_In_Range (
      Value : Arithmetic.Value_T;
      Flux  : Flux_T;
      Cell  : Storage.Cell_T)
   return Boolean
   is
   begin

      return Subset (
         Left  => Singleton (Value),
         Right => Rainge (Join (Id (Flux), Project (Flux, Cell))),
         Calc  => Flux.Calc);

   end Is_In_Range;


   function Step_From_Flux (
      Flux : Flux_T;
      Cell : Storage.Cell_T)
   return Storage.Bounds.Interval_T
   is

      -- With F standing for the given flux, and C standing for the
      -- cell, compute the Omega expression
      --
      --   hull (
      --      {[a,b]->[b]} (
      --         domain ( ({[C,i]->[basis]}.F.{[basis]->[C]})
      --                  intersection
      --                  sum)));
      --
      -- where "sum" is the Omega relation {[a,b]->[a+b]}.

      Basis : constant Tuple_T := Tuple (Flux.Cells);
      -- The basis tuple.

      C_Tuple : constant Tuple_T := Tuple (Cell);
      -- The tuple containing just the single Cell.

      C_Domain : constant Relation_T :=
         Relation (Domain => C_Tuple, Rainge => Basis);
      --
      -- The relation {[C]->[basis]}, to isolate this Cell as
      -- the interesting component of the domain of the flux.

      C_Range : constant Relation_T :=
         Relation (Domain => Basis, Rainge => C_Tuple);
      --
      -- The relation {[basis]->[C]}, to isolate this Cell as
      -- the interesting component of the range of the flux.

      C_Inc_to_C : constant Relation_T :=
         Parens (
            Join (First_Of_Two,
            Join (C_Domain    ,
            Join (Id (Flux)   ,
                  C_Range     ))));
      --
      -- {[C,i]->[C]} . {[C]->[basis]} . F . {[basis]->[C]}.
      --
      -- The component {[C,i]->[basis]} is here divided into two
      -- joined parts to avoid mixing names of cells (Cell) and
      -- entirely synthetic names like "i", with possible
      -- unintended name clashes.

   begin

      return Hull_Bound (
         Set  =>
            Apply (
               Relation => Second_Of_Two,
               To_Set   =>
                  Domain (Intersection (C_Inc_to_C, Sum_Of_Two))),
         Calc => Flux.Calc);

   end Step_From_Flux;


   function Effect_Only (
      On : Storage.Cell_T;
      By : Arithmetic.Effect_T)
   return Arithmetic.Effect_T
   --
   -- The effect on the given cell, by the given effect.
   -- Usually at most one assignment is returned, since any
   -- cell should occur at most once as a target in an effect.
   --
   is
      use type Arithmetic.Expr_Kind_T;

      Only : Arithmetic.Effect_T (By'Range);
      Last : Natural := Only'First - 1;
      -- The effects on the cell are collected in Only(..Last).

   begin

      for B in By'Range loop

         if       By(B).Target.Kind = Arithmetic.Cell
         and then By(B).Target.Cell = On
         then

            Last := Last + 1;
            Only(Last) := By(B);

         end if;

      end loop;

      return Only(Only'First .. Last);

   end Effect_Only;


   function Pool_After_Effect (
      Pool   : Pool_T;
      Effect : Arithmetic.Effect_T)
   return Pool_T
   is
   begin

      return New_Pool (
         Cells => Pool.Cells,
         Value => Apply (
            Relation => Transformation (
               Domain => Pool.Cells,
               Effect => Effect),
            To_Set => Id (Pool)),
         Calc => Pool.Calc);

   end Pool_After_Effect;


   function Flux_After_Effect (
      Flux   : Flux_T;
      Effect : Arithmetic.Effect_T)
   return Flux_T
   is
   begin

      return New_Flux (
         Cells => Flux.Cells,
         Value => Join (
            Id (Flux),
            Transformation (
               Domain => Flux.Cells,
               Effect => Effect)),
         Calc => Flux.Calc);

   end Flux_After_Effect;


   function Bounds_After_Step (
      Into_Step : Pool_T;
      Effect    : Arithmetic.Effect_T;
      Cell      : Storage.Cell_T)
   return Storage.Bounds.Interval_T
   is

      From_Set : Set_T;
      -- The "from" set (data-pool) of the step.

   begin

      From_Set := Apply (
         Relation => Join (
            Transformation (
               Domain => Into_Step.Cells,
               Effect => Effect_Only (On => Cell, By => Effect)),
            Project (Into_Step, Cell)),
         To_Set => Id (Into_Step));

      return
         Hull_Bound (
            Set  => From_Set,
            Calc => Into_Step.Calc);

   end Bounds_After_Step;


   function Bounds_After_Step (
      Into_Step : Flux_T;
      Effect    : Arithmetic.Effect_T;
      Cell      : Storage.Cell_T)
   return Storage.Bounds.Interval_T
   is

      From_Rel : Relation_T;
      -- The "from" relation of the step.

   begin

      From_Rel :=
         Join (
            Id (Into_Step),
            Transformation (
               Domain => Into_Step.Cells,
               Effect => Effect_Only (On => Cell, By => Effect)));

      return
         Hull_Bound (
            Set  => Rainge (Join (From_Rel, Project (Into_Step, Cell))),
            Calc => Into_Step.Calc);

   end Bounds_After_Step;


   function Values_In_Domain (
      Flux : Flux_T'Class;
      Cell : Storage.Cell_T)
   return Pool_T
   is
   begin

      return New_Pool (
         Cells => Cell_Sets.To_Set ((1 => Cell)),
         Value => Apply (
            Relation => Project (Flux, Cell),
            To_Set   => Domain (Id (Flux))),
         Calc  => Flux.Calc);

   end Values_In_Domain;


   function Value_Pool (
      Flux : Flux_T'Class;
      Expr : Arithmetic.Expr_Ref;
      Var  : Storage.Cell_T)
   return Pool_T
   is
   begin

      return New_Pool (
         Cells => Cell_Sets.To_Set ((1 => Var)),
         Value => Rainge (Join (Id (Flux), Map (Flux, Expr))),
         Calc  => Flux.Calc);

   end Value_Pool;


   function Values (Within : Pool_T) return Storage.Bounds.Value_List_T
   is

      List : Storage.Bounds.Value_List_T (
         1 .. Storage.Bounds.Opt.Max_Listed_Values);
      -- The values listed in increasing order.

      Last : Natural := 0;
      -- The result will be List(1 .. Last).

      Residual : Pool_T := Within;
      -- The pool, or what remains of it.

      Bound : Storage.Bounds.Interval_T;
      -- The bounds on the remaining values in the pool.

      Value : Arithmetic.Value_T;
      -- The minimum value remaining, from Bound.Min.

   begin

      -- Iteratively find the least value in the Residual, add
      -- it to the List and remove it from the Residual:

      Enumeration :
      loop

         -- Find the bounds of the Residual:

         begin

            Bound := Hull_Bound (
               Set  => Id (Residual),
               Calc => Within.Calc);

         exception

         when Null_Set_Error =>
            -- We already found and listed all values.

            exit Enumeration;

         end;

         -- Residual is not empty.

         if not Storage.Bounds.Bounded (Bound) then
            -- We cannot compute the bounds on Residual.

            Output.Warning ("Arithmetic pool is unbounded");

            raise Storage.Bounds.Unbounded;

         end if;

         -- Add min (Residual) to the List:

         Value := Storage.Bounds.Value (Bound.Min);

         if Last >= List'Last then

            Output.Warning (
                 "Arithmetic pool has over"
               & Natural'Image (List'Length)
               & " values, considered unbounded.");

            raise Storage.Bounds.Unbounded;

         end if;

         Last := Last + 1;

         List(Last) := Value;

         -- Remove min (Residual) from Residual:

         Residual := New_Pool (
           Cells => Within.Cells,
           Value => Id (Residual) - Singleton ((1 => Value)),
           Calc  => Within.Calc);

      end loop Enumeration;

      -- Return what we have.

      return List(1 .. Last);

   end Values;


end Calculator;
