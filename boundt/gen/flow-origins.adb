-- Flow.Origins (body)
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
-- $Revision: 1.18 $
-- $Date: 2015/10/24 20:05:48 $
--
-- $Log: flow-origins.adb,v $
-- Revision 1.18  2015/10/24 20:05:48  niklas
-- Moved to free licence.
--
-- Revision 1.17  2013/12/08 22:05:57  niklas
-- BT-CH-0259: Storing value-origin analysis results in execution bounds.
--
-- Revision 1.16  2013-02-19 09:17:03  niklas
-- BT-CH-0245 clean-up. Only descriptions and tracing changed.
--
-- Revision 1.15  2013-02-12 08:47:19  niklas
-- BT-CH-0245: Global volatile cells and "volatile" assertions.
--
-- Revision 1.14  2009-11-27 11:28:07  niklas
-- BT-CH-0184: Bit-widths, Word_T, failed modular analysis.
--
-- Revision 1.13  2009/04/16 10:06:18  niklas
-- BT-CH-0170: Corrected Flow.Origins.Is_Identical to Is_Initial_Value.
--
-- Revision 1.12  2008/12/25 09:02:26  niklas
-- Moved context clause for Storage.Cell_Numbering from spec to body.
-- Changed Propagate_Value_Origins.Feasible_Successors to use its
-- parameter Within instead of the global Model (they are the same).
--
-- Revision 1.11  2007/11/12 21:37:27  niklas
-- BT-CH-0097: Only arithmetic analysis marks boundable edge domain.
--
-- Revision 1.10  2007/10/26 12:44:35  niklas
-- BT-CH-0091: Reanalyse a boundable edge only if its domain grows.
--
-- Revision 1.9  2007/08/25 18:57:17  niklas
-- Updated to define an Is_Identical function for Origin_Function_T.
--
-- Revision 1.8  2007/04/18 18:34:39  niklas
-- BT-CH-0057.
--
-- Revision 1.7  2007/03/29 15:18:02  niklas
-- BT-CH-0056.
--
-- Revision 1.6  2007/02/13 20:20:41  Niklas
-- BT-CH-0044.
--
-- Revision 1.5  2007/01/25 21:25:15  niklas
-- BT-CH-0043.
--
-- Revision 1.4  2007/01/13 13:51:04  niklas
-- BT-CH-0041.
--
-- Revision 1.3  2006/11/20 20:20:18  niklas
-- BT-CH-0037.
--
-- Revision 1.2  2006/10/24 08:44:31  niklas
-- BT-CH-0028.
--
-- Revision 1.1  2005/05/09 15:24:23  niklas
-- First version.
--


with Ada.Text_IO;
with Ada.Unchecked_Deallocation;
with Arithmetic;
with Faults;
with Flow.Opt;
with Flow.Origins.Opt;
with Flow.Show;
with Least_Fixpoint;
with Output;
with Programs;
with Storage.Cell_Numbering;


package body Flow.Origins is


   --
   ---   Principles of operation
   --
   -- The process finds the least fixpoint for a data-flow problem where
   -- the value domain associates each cell with the single "origin" or
   -- definition of the cell's value, if the origin is known.
   --
   -- A state in the abstract domain is thus a mapping that assigns to
   -- each cell C either a known origin or a null (unknown) origin.
   -- The value can be refined (made more precise) in two ways:
   --
   -- > by assigning an origin to a cell that had none before, or
   --
   -- > by changing a cell's origin to a "closer" origin, that is,
   --   one that intercepts the flow from the previous origin.
   --
   -- The bottom (least defined) value is the one where no cell has a
   -- known origin.
   --
   -- In the least fixpoint every cell is assigned an origin at
   -- every feasible point in the subprogram; no undefined origins
   -- remain in the solution.
   --
   -- There are three kinds of value origins:
   --
   -- > The initial state, that is the cell values at the start of the
   --   entry step.
   --
   -- > Any assignment where the target is a cell and the assigned value
   --   is an expression that is _not_ just a cell. Thus, "copy"
   --   assignments of the form c1 := c2 are not origins of values;
   --   instead, they propagate the origin of cell c2 to be the
   --   origin of c1 after the assignment.
   --
   -- > Merge points where the several edges into a step carry multiple
   --   origins for a cell. These origins are merged into a single
   --   origin at the start of the step.
   --
   -- > TBA aliases from assignments to dynamic targets.
   --
   -- The memory consumption of the algorithm could be reduced by
   -- computing the solution over the node graph (basic block graph)
   -- instead of the step graph. One would first compute a summary
   -- effect for each node that shows if the final value of a cell
   -- is a copy of some initial cell value, or is assigned in the
   -- node itself. TBD if this improvement is useful.
   --
   --
   --    Data structure
   --
   -- The result of the value-origin analysis is represented in
   -- a Map_Object_T data structure with the following components:
   --
   -- > A reference to the computation model to which analysis
   --   applies.
   --
   -- > A Storage.Cell_Numbering_Map_T to map cells to numbers
   --   for indexing the other components (specifically the type
   --   Numbered_Cell_Origins_T) by cells.
   --
   -- > An array (Origin_List_T) holding all the Origin_Ts that are
   --   created by the analysis and referenced in the result of the
   --   analysis. The array is indexed by Origin_Index_T.
   --
   -- > An array (Origin_Map_T) indexed by step-index and containing
   --   the origins of all numbered cells after this step.
   --
   -- > For each step (each element of Origin_Map_T) we have an
   --   array (Numbered_Cell_Origins_T) indexed by cell number and
   --   giving the Origin_T of the cell, at this step, through its
   --   Origin_Index_T. The origin is valid after the step, so if
   --   the step assigns the cell the origin is at this step itself.
   --
   -- We put a lot of the components in the heap because the structure
   -- may be large and stack-space is limited on some platforms.
   --
   -- The top type, Map_Ref, is an access type to make it easy
   -- to use for eg. computing transfer functions; we can put a
   -- component of type Map_Ref in some type derived from
   -- Arithmetic.Transfer_Function_T.


   type Inverse_Cell_Numbering_Ref is access Storage.Cell_Numbering.List_T;
   --
   -- An inverse cell-numbering (mapping from the number to the cell)
   -- on the heap.

   --:dbpool Inverse_Cell_Numbering_Pool : GNAT.Debug_Pools.Debug_Pool;

   --:dbpool for Inverse_Cell_Numbering_Ref'Storage_Pool use Inverse_Cell_Numbering_Pool;


   subtype Origin_Index_T is Natural;
   --
   -- All the origins for an analysis are numbered sequentially
   -- from 1. The abstract state at a point in the flow-graph refers
   -- to origins via this index.


   No_Origin : constant Origin_Index_T := 0;
   --
   -- A special value for the origin-index that means that no origin
   -- is yet known.


   type Origin_List_T is array (Origin_Index_T range <>) of Origin_T;
   --
   -- A list of origins, for example all origins defined in a subprogram.
   -- To avoid confusing No_Origin with a real origin, the 'First of
   -- the list should be > 0.


   type Origin_List_Ref is access Origin_List_T;
   --
   -- A list of origins on the heap.

   --:dbpool Origin_List_Pool : GNAT.Debug_Pools.Debug_Pool;

   --:dbpool for Origin_List_Ref'Storage_Pool use Origin_List_Pool;


   type Numbered_Cell_Origins_T is
      array (Storage.Cell_Numbering.Number_T range <>)
      of Origin_Index_T;
   --
   -- The abstract value is a mapping from cells to origins.
   -- The mapping uses a dense numbering of all the relevant cells
   -- (those that are assigned in the flow-graph). Volatile cells
   -- are included, although their origins are not analysed.


   type Numbered_Cell_Origins_Ref is access Numbered_Cell_Origins_T;
   --
   -- The vectors of cell origins are stored in the heap for
   -- two reasons: firstly, to make it easier to refer to the
   -- correct vector when using TBD to evaluate
   -- the expressions in a step, and secondly because such a
   -- vector is needed for each step in the flow-graph and the
   -- whole set of vectors would use a lot of stack space, which
   -- is limited on some platforms.

   --:dbpool Numbered_Cell_Origins_Pool : GNAT.Debug_Pools.Debug_Pool;

   --:dbpool for Numbered_Cell_Origins_Ref'Storage_Pool use Numbered_Cell_Origins_Pool;


   subtype Step_Number_T is Positive;
   --
   -- The number of a step in the flow-graph, as required for
   -- Least_Fixpoint. The value is the same as the Step_Index,
   -- only the type is different.


   type Origin_Map_T is
      array (Step_Number_T range <>) of Numbered_Cell_Origins_Ref;
   --
   -- The cell-value origins after each step in a flow-graph.


   type Map_Object_T (Steps : Positive) is limited record
      Computation     : aliased Flow.Computation.Model_Ref;
      Number_Of_Cell  : Storage.Cell_Numbering.Map_T;
      Cell_For_Number : Inverse_Cell_Numbering_Ref;
      Num_Origins     : Natural;
      Origin_Set      : Origin_List_Ref;
      Origin_Map      : Origin_Map_T (1 .. Steps);
   end record;
   --
   -- The results of value-origin analysis for a given computation
   -- model of a given subprogram. The subprogram and model are not
   -- explicitly identified and must be known in other ways.
   --
   -- Steps
   --    The number of steps in the flow-graph that was analysed.
   -- Computation
   --    A reference to the computation model on which the analysis
   --    was done and to which following components apply.
   -- Number_Of_Cell
   --    A dense numbering of all the (statically) named cells
   --    in the computation. Volatile cells are included, although
   --    their origins are not analysed.
   -- Cell_For_Number
   --    The inverse of Numbers: a mapping from the number of a
   --    cell to the cell itself.
   -- Num_Origins
   --    The number of origins created in the analysis.
   -- Origin_Set
   --    The origins created in this analysis.
   --    The list is heap-allocated because it may be quite large and
   --    stack-space is limited on some platforms. The index range in
   --    use is 1 .. Num_Origins; the array is usually larger because
   --    space was reserved for the maximum amount of possible origins
   --    estimated in a rough way from the structure of graph and the
   --    computation.
   -- Origin_Map
   --    For each step, the propagated cell-value origins after
   --    the step. This is the solution (least fixpoint) of the
   --    data-flow problem.


   function Is_Valid (Item : Map_Ref) return Boolean
   is
   begin

      return Item /= null;

   end Is_Valid;


   procedure Unchecked_Discard
   is new Ada.Unchecked_Deallocation (
      Object => Storage.Cell_Numbering.List_T,
      Name   => Inverse_Cell_Numbering_Ref);


   procedure Discard (Item : in out Inverse_Cell_Numbering_Ref)
   --
   -- Discards the Item with optional Deallocate.
   --
   is
   begin

      if Opt.Deallocate then

         Unchecked_Discard (Item);

      else

         Item := null;

      end if;

   exception when others => Faults.Deallocation;

   end Discard;


   procedure Unchecked_Discard
   is new Ada.Unchecked_Deallocation (
      Object => Origin_List_T,
      Name   => Origin_List_Ref);


   procedure Discard (Item : in out Origin_List_Ref)
   --
   -- Discards the Item with optional Deallocate.
   --
   is
   begin

      if Opt.Deallocate then

         Unchecked_Discard (Item);

      else

         Item := null;

      end if;

   exception when others => Faults.Deallocation;

   end Discard;


   procedure Unchecked_Discard
   is new Ada.Unchecked_Deallocation (
      Name   => Numbered_Cell_Origins_Ref,
      Object => Numbered_Cell_Origins_T);


   procedure Discard (Item : in out Numbered_Cell_Origins_Ref)
   --
   -- Discards the Item with optional Deallocate.
   --
   is
   begin

      if Opt.Deallocate then

         Unchecked_Discard (Item);

      else

         Item := null;

      end if;

   exception when others => Faults.Deallocation;

   end Discard;


   procedure Unchecked_Discard
   is new Ada.Unchecked_Deallocation (
      Object => Map_Object_T,
      Name   => Map_Ref);


   procedure Discard (Item : in out Map_Ref)
   --
   -- Discards the Item and its parts with optional Deallocate.
   --
   is
   begin

      if Item /= null then

         Flow.Computation.Discard (Item.Computation);
         Storage.Cell_Numbering.Discard (Item.Number_Of_Cell);
         Discard (Item.Cell_For_Number);
         Discard (Item.Origin_Set);

         for M in Item.Origin_Map'Range loop

            Discard (Item.Origin_Map(M));

         end loop;

         if Opt.Deallocate then

            Unchecked_Discard (Item);

         else

            Item := null;

         end if;

      end if;

   exception when others => Faults.Deallocation;

   end Discard;


   function Computation (Under : Map_Ref)
   return Flow.Computation.Model_Handle_T
   is
   begin

      return Under.Computation'Access;

   end Computation;


   function Symbol_Table (Under : Map_Ref)
   return Symbols.Symbol_Table_T
   is
   begin

      return Programs.Symbol_Table (
         Flow.Computation.Subprogram (Under.Computation));

   end Symbol_Table;


   function Number (Step : Step_T) return Step_Number_T
   --
   -- The index of a step, as a Step_Number_T for Least_Fixpoint.
   --
   is
   begin

      return Step_Number_T (Index (Step));

   end Number;


   --
   ---   Image functions
   --


   Kind_Mark : constant array (Origin_Kind_T) of Character := (
      Initial  => 'I',
      Assigned => 'A',
      Merged   => 'M');
   --
   -- The mark for the kind of an origin, in Image (Origin).


   function Image (Item : Origin_T) return String
   is
   begin

      return
           Kind_Mark(Item.Kind)
         & '/'
         & Storage.Image (Item.Cell)
         & '@'
         & Output.Image (Natural (Index (Item.Step)));

   end Image;


   function Image (
      Item    : Origin_Index_T;
      Origins : Origin_List_T)
   return String
   --
   -- Describes one cell-value origin.
   --
   is

      Index : constant String := Output.Image (Natural (Item));

   begin

      if Item = No_Origin then

         return Index;

      else

         return Index & '=' & Image (Origins(Item));

      end if;

   end Image;


   function Image (
      Item    : Numbered_Cell_Origins_T;
      Origins : Origin_List_T)
   return String
   --
   -- Describes the cell-value origins.
   --
   is
      use type Storage.Cell_Numbering.Number_T;
   begin

      if Item'Length = 0 then

         return "";

      else

         return
              Storage.Cell_Numbering.Number_T'Image (Item'First)
            & " =>"
            & Image (Item(Item'First), Origins)
            & ','
            & Image (Item(Item'First + 1 .. Item'Last), Origins);

      end if;

   end Image;


   --
   ---   The basic results
   --


   function All_Cells (Under : Map_Ref)
   return Storage.Cell_List_T
   is
   begin

      return Storage.Cell_List_T (Under.Cell_For_Number.all);

   end All_Cells;


   function Origin_Is_Known (
      Cell : Storage.Cell_T;
      From : Map_Ref)
   return Boolean
   is
   begin

      return Storage.Cell_Numbering.Numbered (Cell, From.Number_Of_Cell)
      and    not Storage.Is_Volatile (Cell);

   end Origin_Is_Known;


   function Origin_After (
      Step : Step_T;
      Cell : Storage.Cell_T;
      From : Map_Ref)
   return Origin_T
   is
      use Storage.Cell_Numbering;

      Cell_Num : Number_T;
      -- The number of the Cell in this analysis.

      Org_Index : Origin_Index_T;
      -- The index of the origin of this Cell.

   begin

      Cell_Num := Number (Cell, From.Number_Of_Cell);

      Org_Index := From.Origin_Map(Number (Step)) (Cell_Num);

      return From.Origin_Set(Org_Index);

   exception

   when Not_Numbered =>
      -- The Cell is not one of the cells that we analysed
      -- and numbered in From.Number_Of_Cell.

      Output.Fault (
         Location => "Flow.Origins.Origin_After",
         Text     =>
              "Cell "
            & Storage.Image (Cell)
            & " is not numbered.");

      raise;

   end Origin_After;


   function Origin_Before (
      Step  : Step_T;
      Cell  : Storage.Cell_T;
      From  : Map_Ref)
   return Origin_T
   is

      Preds : constant Step_List_T :=
         Flow.Computation.Predecessors (
            Step  => Step,
            Under => From.Computation);
      -- The feasible predecessors of the Step.

      Origin : Origin_T := (
         Kind => Initial,
         Cell => Cell,
         Step => Step);
      -- For the result to be.
      -- The initial value applies if Step is the entry step
      -- and so Preds is empty.

      Other_Origin : Origin_T;
      -- Another origin from a predecessor.

   begin

      if Preds'Length > 0 then
         -- The Step is not the entry step of its flow-graph.
         -- We look at all the incoming origins; if they are all the
         -- same this is the origin, otherwise the origin is Merged.

         Origin := Origin_After (Preds(Preds'First), Cell, From);

         for P in Preds'First + 1 .. Preds'Last loop

            Other_Origin := Origin_After (Preds(P), Cell, From);

            if Other_Origin /= Origin then
               -- Mixed origins. Merge.

               Origin := (
                  Kind => Merged,
                  Cell => Cell,
                  Step => Step);

               exit;

            end if;

         end loop;

      end if;

      return Origin;

   exception

   when Storage.Cell_Numbering.Not_Numbered =>

      Output.Fault (
         Location => "Flow.Origins.Origin_Before",
         Text     => "Passing problem from Origins_After.");

      raise;

   end Origin_Before;


   function Has_Same_Value (
      After  : Step_T;
      Before : Step_T;
      Expr   : Arithmetic.Expr_Ref;
      From   : Map_Ref)
   return Boolean
   is
      use Arithmetic;
   begin

      case Expr.Kind is

      when Opaque =>

         return False;
         -- The concrete values may differ. The abstract values
         -- are the same (opaque), but are not useful.

      when Const =>

         return True;

      when Cell =>

         return Origin_After  (After , Expr.Cell, From)
              = Origin_Before (Before, Expr.Cell, From);

      when Ref =>

         return False;
         -- We do not know which cell is referenced, so we cannot
         -- know if its value is the same Here and There.

      when Unary_Kind =>

         return Has_Same_Value (After, Before, Expr.Expr, From);

      when Binary_Kind =>

         return   Has_Same_Value (After, Before, Expr.L_Expr, From)
         and then Has_Same_Value (After, Before, Expr.R_Expr, From);

      when Ternary_Kind =>

         return   Has_Same_Value (After, Before, Expr.L3_Expr, From)
         and then Has_Same_Value (After, Before, Expr.R3_Expr, From)
         and then Has_Same_Value (After, Before, Expr.C3_Expr, From);

      end case;

   end Has_Same_Value;


   --
   ---   Tracing the process of origin analysis
   --


   procedure Trace (
      Action  : in String;
      Step    : in Step_T;
      Source  : in Step_T;
      Input   : in Numbered_Cell_Origins_Ref;
      Output  : in Numbered_Cell_Origins_Ref;
      Origins : in Origin_List_Ref;
      Grew    : in Boolean)
   --
   -- Report a "transform" or "merging" action.
   --
   is
      use Ada.Text_IO;
   begin

      Put (
          "Value Origin Analysis:"
        & Action
        & ":Step:" & Step_Index_T'Image (Index (Step)));

      if Source /= null then
         Put (":Source_Step:" & Step_Index_T'Image (Index (Source)));
      end if;

      New_Line;

      Put_Line ("- Input  : " & Image (Input.all , Origins.all));
      Put_Line ("- Output : " & Image (Output.all, Origins.all));
      Put_Line ("- Grew   : " & Boolean'Image (Grew));

   end Trace;


   procedure Show_Origins (
      Model      : in Flow.Computation.Model_Ref;
      Origin_Map : in Origin_Map_T;
      Origin_Set : in Origin_List_T;
      Cells      : in Storage.Cell_Numbering.List_T)
   --
   -- Displays the map of cell-value origins.
   --
   is
      use Ada.Text_IO;

      First_Origin_Col : constant := 10;
      -- Column for the first cell origin, in the cell origin table.

      Origin_Width : constant := 12;
      -- The width of each column in the cell origin table.

      Origin_Col : Positive_Count;
      -- Current column in cell value table.

      Index : Origin_Index_T;
      -- Indicates the original of the current cell in the current step.

      No_Orig : constant String := "-";
      -- The mark for "no origin."

   begin

      New_Line;

      Put_Line (
           "Value-origin results for "
         & Programs.Name (Flow.Computation.Subprogram (Model)));

      New_Line;
      Put_Line (Kind_Mark(Initial)  & " means an initial value.");
      Put_Line (Kind_Mark(Assigned) & " means a computed value.");
      Put_Line (Kind_Mark(Merged)   & " means a merge of various values.");
      Put_Line (No_Orig             & " means no origin (volatile cell).");
      New_Line;

      Set_Col (First_Origin_Col);
      Put_Line ("Cell-value origins after the step, by Cell #");

      Put ("Step#");

      Origin_Col := First_Origin_Col;

      for C in Cells'Range loop

         Set_Col (Origin_Col);
         Put (Storage.Cell_Numbering.Number_T'Image (C));

         Origin_Col := Origin_Col + Origin_Width;

      end loop;

      New_Line;

      for S in Origin_Map'Range loop

         Put (Step_Number_T'Image (S));

         Origin_Col := First_Origin_Col;

         for C in Cells'Range loop

            Set_Col (Origin_Col);

            Put (' ');
            -- To ensure separation from the preceding value.

            Index := Origin_Map(S)(C);

            if Index /= No_Origin then

               Put (Image (Origin_Set(Index)));

            else

               Put (No_Orig);

            end if;

            Origin_Col := Origin_Col + Origin_Width;

         end loop;

         New_Line;

      end loop;

      New_Line;

      Put_Line (
           "End of value-origin results for "
         & Programs.Name (Flow.Computation.Subprogram (Model)));

      New_Line (2);

   end Show_Origins;



   --
   ---   Solving the origin analysis
   --


   procedure Analyse_Value_Origins (
      Model  : in     Flow.Computation.Model_Ref;
      Result :    out Map_Ref)
   --
   -- Implements the value-origin analysis as described for the
   -- public operation Analyse, but assuming that Opt.Propagate
   -- is True.
   --
   is

      -- Cell-value origin propagation is implemented by least-fixpoint
      -- iteration of data-flow equations along the flow (forward
      -- direction). The results are the cell value origins after each
      -- step.

      use type Storage.Cell_T;

      Graph : constant Graph_T := Flow.Computation.Graph (Model);
      --
      -- The flow-graph underlying the computation model.
      -- Only the step-level graph will be used.

      Steps : constant Step_List_T := All_Steps (Graph);
      --
      -- All the steps of the graph (including infeasible steps).
      -- Note that the indices in this vector are equal to the
      -- Step_Index_T values of the steps, although of Positive type.
      -- In other words, this array can be indexed by Step_Number_T.
      -- Infeasible steps are not processed, but they are included in
      -- the list to have this indexing property.

      Is_Merge_Point : array (Steps'Range) of Boolean;
      --
      -- Whether a Step is a merge point where several paths join.
      -- Merged origins are created only for merge points.

      Named_Cells : Storage.Cell_Set_T := Flow.Computation.Cells_In (
         Model => Model,
         Calls => True);
      --
      -- All the cells (statically) named in the given computation model,
      -- including both assigned cells and used cells, and including
      -- the effects (assignments to model cells) of calls. We are mainly
      -- interested in the assigned cells, but the other cells may
      -- originate values for the assigned cells and so it is simpler
      -- to include them in the analysis. (The set could be reduced to
      -- include only the assigned cells and the source cells of copy
      -- assignments. Cells used in other ways could be ignored.)
      --
      -- Volatile cells are included, for simplicity, but will have an
      -- unknown origin.

      Target_Cells : Storage.Cell_Set_T :=
         Flow.Computation.Cells_Defined (Model);
      --
      -- All the cells (statically) assigned in the given computation model.
      -- This is a subset of Named_Cells.
      --
      -- The Target_Cells may have Merged or Assigned origins. The other
      -- Named_Cells can have only Initial origins.

      Number_Of_Cell : Storage.Cell_Numbering.Map_T :=
         Storage.Cell_Numbering.Map (Named_Cells);
      --
      -- A dense numbering of all the (statically) named cells in the
      -- computation. This will also be accessible as Result.Number_Of_Cell,
      -- see below, but is created here because we need it to create
      -- some of the following constants etc.

      Cell_For_Number : constant Inverse_Cell_Numbering_Ref :=
         new Storage.Cell_Numbering.List_T'(
            Storage.Cell_Numbering.Inverse (Number_Of_Cell));
      --
      -- The cell for each cell-number. This will also be accessible
      -- as Result.Cell_For_Number.

      Total_Named_Cells : constant Natural := Cell_For_Number'Length;
      --
      -- The total number of cells in this analysis.

      subtype Cell_Number_T is
         Storage.Cell_Numbering.Number_T range Cell_For_Number'Range;
      --
      -- The number of a cell in the consecutive numbering of
      -- of all the (statically) assigned cells in the computation.

      subtype Cell_Origins_T is Numbered_Cell_Origins_T (Cell_Number_T);
      --
      -- The origin for each cell's value, indexed by the consecutive
      -- numbers assigned to cells (statically) assigned in the computation.

      subtype Cell_Origins_Ref is Numbered_Cell_Origins_Ref;
      --
      -- The accessed object will always be of subtype Cell_Origins_T.

      Max_Initial_Origins : constant Natural := Total_Named_Cells;
      --
      -- The maximum (and actual) number of Initial origins.

      Max_Assigned_Origins : constant Natural :=
         Flow.Computation.Total_Defining_Assignments (Model);
      --
      -- The maximum number of Assigned origins. This number could
      -- be reduced to the number of non-copy assignments.

      Max_Merged_Origins : constant Natural :=
         Storage.Card (Target_Cells) * Positive (Max_Node (Graph));
      --
      -- The maximum number of Merged origins. This could be reduced
      -- to use the number of _feasible_ nodes.

      Max_Origins : constant Natural :=
           Max_Initial_Origins
         + Max_Assigned_Origins
         + Max_Merged_Origins;
      --
      -- An upper bound on the number of "origins" that can be created.


      function Locus (Step : Step_T) return Output.Locus_T
      is
      begin

         return Flow.Show.Locus (
            Step   => Step,
            Source => Programs.Symbol_Table (Flow.Computation.Subprogram (Model)));

      end Locus;


      function Is_Copy (
         Item : Arithmetic.Assignment_T;
         Step : Step_T)
      return Boolean
      --
      -- Whether this assignment is a "copy" assignment, that is, the
      -- target is a cell and the value expression is also a cell, and
      -- a non-volatile one.
      --
      -- The Step is only to provide output locus.
      --
      is
         use Arithmetic;

         Copy : Boolean := False;
         -- Let us be pessimistic, initially.

      begin

         if        Item.Kind        = Regular
         and then (Item.Target.Kind = Cell
         and       Item.Value.Kind  = Cell)
         then
            -- Looking good...

            Copy := not Storage.Is_Volatile (Item.Value.Cell);

            if (not Copy) and Opt.Trace_Volatile then

               Output.Trace (
                  Text =>
                       "Value-origin copy-chain ends at volatile cell"
                     & Output.Field_Separator
                     & Arithmetic.Image (Item),
                  Locus => Locus (Step));

            end if;

         end if;

         return Copy;

      end Is_Copy;


      procedure Mark_Merge_Points
      --
      -- Marks the merge-point steps in Is_Merge_Point.
      --
      is

         Incoming : Natural;
         -- The number of edges into a step.

      begin

         for S in Steps'Range loop

            Incoming := Flow.Computation.Number_Into (Steps(S), Model);

            if Steps(S) = Entry_Step (Graph) then
               -- There is also an implicit entry edge.

               Incoming := Incoming + 1;

            end if;

            Is_Merge_Point(S) := Incoming > 1;

         end loop;

      end Mark_Merge_Points;


      procedure Create (
         Origin : in     Origin_T;
         Giving :    out Origin_Index_T)
      --
      -- Creates a new Origin in the Origin_Set and returns its
      -- index in Giving.
      --
      is
      begin

         Result.Num_Origins := Result.Num_Origins + 1;

         Giving := Result.Num_Origins;

         if Opt.Trace_Iteration then

            Output.Trace (
                "Origin"
               & Natural'Image (Giving)
               & " = "
               & Image (Origin));

         end if;

         Result.Origin_Set(Giving) := Origin;

      end Create;


      function Feasible_Successors (
         After  : Step_T;
         Post   : Cell_Origins_Ref;
         Within : Flow.Computation.Model_Ref)
      return Step_List_T
      --
      -- The "successors" operation for Least_Fixpoint for finding
      -- cell value origins.
      --
      is
      begin

         return Flow.Computation.Successors (Step => After, Under => Within);

      end Feasible_Successors;


      procedure Copy_Origins (
         Pre  : in     Cell_Origins_Ref;
         Via  : in     Step_T;
         Post : in out Cell_Origins_Ref;
         Grew :    out Boolean)
      --
      -- The "transform" operation for Least_Fixpoint used for finding
      -- cell value origins.
      --
      -- Each copy assignment in the step's effect propagates the origin
      -- of the source cell in Pre, if it has one, to become the origin
      -- of the target cell in Post (this will never cause a conflict or
      -- merge of origins).
      --
      -- Each non-copy assignment in the step's effect defines the
      -- origin of its target cell in Post. However, this was already
      -- done in Initialize, and cannot be improved upon, so non-copy
      -- assignments are not considered here; they are, however, detected
      -- as Post-origins of the Assign kind in the Via step.
      --
      -- For the cells that are not assigned in the step's effect, the
      -- Pre-origin, if any, is passed as the Post-origin of the cell.
      --
      -- Targets that are volatile cells are ignored.
      --
      is

         Effect : constant Arithmetic.Effect_Ref :=
            Flow.Computation.Effect (Step => Via, Under => Model);
         -- The effect of the step.

         Assign : Arithmetic.Assignment_T;
         -- One of the assignments in the Effect.

         Source, Target : Cell_Number_T;
         -- The numbers of the source and target cells of a copy
         -- assignment.

         Effected : array (Cell_Number_T) of Boolean;
         -- The cells to which the Effect assigns a value.

      begin

         Grew := False;
         -- No growth seen so far.

         -- Mark the (non-copy) Assigned cells:

         for C in Cell_Number_T loop

            Effected(C) :=
                        Post(C) /= No_Origin
               and then Result.Origin_Set(Post(C)).Kind = Assigned
               and then Result.Origin_Set(Post(C)).Step = Via;

         end loop;

         -- Inspect the Effect for copy assignments that now have
         -- an origin for the source:

         for E in Effect'Range loop

            Assign := Effect(E);

            if Is_Copy (Assign, Via)
            and then not Storage.Is_Volatile (Assign.Target.Cell)
            then
               -- This assignment copies the value of a non-volatile
               -- cell into another (or the same) non-volatile cell,
               -- and is therefore a link in a copy chain.

               Source :=
                  Storage.Cell_Numbering.Number (
                     Cell  => Assign.Value.Cell,
                     Under => Result.Number_Of_Cell);

               Target :=
                  Storage.Cell_Numbering.Number (
                     Cell  => Assign.Target.Cell,
                     Under => Result.Number_Of_Cell);

               Effected(Target) := True;

               if  Pre(Source) /= No_Origin
               and Pre(Source) /= Post(Target)
               then
                  -- The copy refines Post.

                  Post(Target) := Pre(Source);

                  Grew := True;

               end if;

            end if;

         end loop;

         -- Pass on Pre-origins to Post-origins when not assigned here:

         for C in Cell_Number_T loop

            if  not Effected(C)
            and Pre(C) /= No_Origin
            and Pre(C) /= Post(C)
            then
               -- The Effect does not assign to cell C, and we know a
               -- Pre-origin for C that should become the new Post-origin.

               Post(C) := Pre(C);

               Grew := True;

            end if;

         end loop;


         if Opt.Trace_Iteration then

             Trace (
                Action  => "Trans",
                Step    => Via,
                Source  => null,
                Input   => Pre,
                Output  => Post,
                Origins => Result.Origin_Set,
                Grew    => Grew);

         end if;

      end Copy_Origins;


      procedure Initialize_Origins (
         Via   : in     Step_T;
         Pre   :    out Cell_Origins_Ref;
         Post  :    out Cell_Origins_Ref;
         Grew  :    out Boolean)
      --
      -- The "initialize" operation for Least_Fixpoint for finding
      -- cell value origins.
      --
      -- Initializes the origins that flow into the step (Pre) to
      -- nothing, except when Via is the entry step in which case Pre
      -- is set to assign the Initial origin to each non-volatile cell
      -- and nothing to each volatile cell.
      --
      -- Initializes the origins after the step (Post) to reflect the
      -- assignments in the Via step as follows:
      --
      -- > For cells that are not assigned in the step, the Post
      --   origin of the cell is the same as the Pre origin of the cell.
      --
      -- > For cells that are the target of a copy assignment, the
      --   Post origin of the cell is the same as the Pre origin of
      --   the source (copied) cell.
      --
      -- > For cells that are the target of a non-copy assignment, the
      --   Post origin of the cell is Assigned in this step.
      --
      -- However, if the Via step is infeasible then the Post state
      -- is simply set to nothing.
      --
      is
         use type Arithmetic.Expr_Kind_T;

         Feasible : constant Boolean :=
            Flow.Computation.Is_Feasible (Step => Via, Under => Model);
         -- Whether the Via step is feasible.

         Non_Volatile_Cells : Natural := 0;
         -- The number of non-volatile cells in the analysis.

         Effect : constant Arithmetic.Effect_Ref :=
            Flow.Computation.Effect (Step => Via, Under => Model);
         -- The effect of the step.

         Assign : Arithmetic.Assignment_T;
         -- One of the assignments in the Effect.

         Source, Target : Cell_Number_T;
         -- The numbers of the source and target cells of an
         -- assignment.

      begin

         -- Initialize Pre to nothing, or the Initial state:

         if Via = Entry_Step (Graph) then
            -- All cell values before this step originate in the
            -- initial state (except for volatile cells, which have
            -- no origin).

            Pre := new Cell_Origins_T;

            for C in Cell_Number_T loop

               if Storage.Is_Volatile (Cell_For_Number(C)) then

                  Pre(C) := No_Origin;

               else

                  Non_Volatile_Cells := Non_Volatile_Cells + 1;

                  Create (
                     Origin => (
                        Kind => Initial,
                        Cell => Cell_For_Number(C),
                        Step => Via),
                     Giving => Pre(C));

               end if;

            end loop;

            Grew := Feasible and Non_Volatile_Cells > 0;
            -- The Post state will have some non-null origins
            -- if the Via step is feasible and there are some
            -- non-volatile cells.

         else

            Pre := new Cell_Origins_T'(others => No_Origin);

            Grew := False;
            -- The Post state will initially be nothing.

         end if;

         -- Initialize Post:

         if not Feasible then
            -- The Via step is infeasible so no origins will
            -- flow out from it.

            Post := new Cell_Origins_T'(others => No_Origin);

         else
            -- The Via step is feasible so it will pass on some
            -- of the Pre origins and may define new origins.

            Post := new Cell_Origins_T'(Pre.all);
            --
            -- Some of these origins will be overridden by assignments
            -- in the Effect, but this is not a conflict or merge of
            -- origins.

            for E in Effect'Range loop

               Assign := Effect(E);

               if Assign.Kind in Arithmetic.Defining_Kind_T then

                  if Assign.Target.Kind = Arithmetic.Cell then
                     -- We can model this assignment in Origins.

                     Target :=
                        Storage.Cell_Numbering.Number (
                           Cell  => Assign.Target.Cell,
                           Under => Result.Number_Of_Cell);

                     if Storage.Is_Volatile (Assign.Target.Cell) then
                        -- Assignments to volatile cells are ignored because
                        -- the origins of volatile cells are not tracked.

                        if Opt.Trace_Volatile then

                           Output.Trace (
                              Text =>
                                   "Value-origin analysis ignores assignment "
                                 & "to volatile cell"
                                 & Output.Field_Separator
                                 & Arithmetic.Image (Assign),
                              Locus => Locus (Via));

                        end if;

                     elsif Is_Copy (Assign, Via) then
                        -- A copy assignment passes the Pre-origin of the
                        -- source cell to the Post-origin of the target cell.

                        Source :=
                           Storage.Cell_Numbering.Number (
                              Cell  => Assign.Value.Cell,
                              Under => Result.Number_Of_Cell);

                        Post(Target) := Pre(Source);
                        --
                        -- This does not imply a change in Grew, because
                        -- either Pre(Source) is No_Origin (Via is not
                        -- the entry step) or we already set Grew to True
                        -- (Via is the entry step).

                     else
                        -- A non-copy assignment is an origin in itself.

                        Create (
                           Origin => (
                              Kind => Assigned,
                              Cell => Cell_For_Number(Target),
                              Step => Via),
                           Giving => Post(Target));

                        Grew := True;

                      end if;

                  -- elsif
                  --    TBA assignment with dynamic target: aliasing.

                  end if;  -- Assign.Target.Kind

               end if;  -- Assign.Kind

            end loop;  -- E in Effect

         end if;  -- Feasible

         if Opt.Trace_Iteration then

             Trace (
                Action  => "Init",
                Step    => Via,
                Source  => null,
                Input   => Pre,
                Output  => Post,
                Origins => Result.Origin_Set,
                Grew    => Grew);

         end if;

      end Initialize_Origins;


      procedure Merge_Origins (
         Source : in     Step_T;
         Post   : in     Cell_Origins_Ref;
         Target : in     Step_T;
         Pre    : in out Cell_Origins_Ref;
         Grew   :    out Boolean)
      --
      -- The "merge" operation for Least_Fixpoint for finding cell
      -- value origins.
      --
      -- The origins for all cells in Post are merged into the Pre
      -- origins as follows:
      --
      -- > If the cell has a Post origin but no Pre origin, the Post
      --   origin is propagated to Pre.
      --
      -- > If the cell has a different origin in Post and Pre then
      --
      --   - if the Target is not a merge point, the Post origin is
      --     propagated to Pre, replacing the earlier Pre origin.
      --
      --   - If the Target is a merge point and the Pre origin is not
      --     a Merge at Target, a new Merge origin for this cell is
      --     defined at the start of the Target step and becomes the
      --     Pre origin.
      --
      is
      begin

         Grew := False;
         -- No growth in Pre seen so far.

         for C in Cell_Number_T loop

            if Post(C) = No_Origin
            or Post(C) = Pre(C)
            then
               -- Nothing new to merge into Pre.

               null;

            elsif  Pre(C) = No_Origin
            or not Is_Merge_Point(Number (Target))
            then
               -- This defines the first origin for C in Pre or just
               -- overrides (refines) the previous origin because this
               -- is not a merge point.

               Pre(C) := Post(C);

               Grew := True;

            elsif Result.Origin_Set(Pre(C)).Kind /= Merged
               or Result.Origin_Set(Pre(C)).Step /= Target
            then
               -- This is a merge point for which the C cell has multiple
               -- origins that have not yet been Merged. So we Merge
               -- them now.

               Create (
                  Origin => (
                     Kind => Merged,
                     Cell => Cell_For_Number(C),
                     Step => Target),
                  Giving => Pre(C));

               Grew := True;

            end if;

         end loop;

         if Opt.Trace_Iteration then

            Trace (
               Action  => "Merge",
               Step    => Target,
               Source  => Source,
               Input   => Post,
               Output  => Pre,
               Origins => Result.Origin_Set,
               Grew    => Grew);

         end if;

      end Merge_Origins;


      procedure Finalize (Item : in out Cell_Origins_Ref)
      --
      -- The "Finalize" operation for Least_Fixpoint.
      --
      is
      begin

         Discard (Item);

      end Finalize;


      function Value_Origins
      is new Least_Fixpoint (
         Graph      => Flow.Computation.Model_Ref,
         Node       => Step_T,
         Node_List  => Step_List_T,
         Index_Of   => Number,
         Value      => Cell_Origins_Ref,
         Value_List => Origin_Map_T,
         Successors => Feasible_Successors,
         Initialize => Initialize_Origins,
         Transform  => Copy_Origins,
         Merge      => Merge_Origins,
         Finalize   => Finalize);


   begin  -- Analyse_Value_Origins

      -- Create the analysis results structure:

      Result := new Map_Object_T (Steps => Steps'Last);

      Flow.Computation.Refer (
         From => Result.Computation,
         To   => Model);

      Result.Number_Of_Cell  := Number_Of_Cell;
      Result.Cell_For_Number := Cell_For_Number;
      Result.Num_Origins     := 0;
      Result.Origin_Set      := new Origin_List_T (1 .. Max_Origins);
      --
      -- The Origin_Map will be set later.


      -- Start the trace if chosen:

      if Opt.Trace_Iteration
      or Opt.Show_Results
      then

         Output.Trace ("Value-origin analysis starts");

         Ada.Text_IO.New_Line;
         Ada.Text_IO.Put_Line ("Cell numbering for value origins:");

         Storage.Cell_Numbering.Show (Result.Number_Of_Cell);

         Ada.Text_IO.New_Line;

         Ada.Text_IO.Put_Line (
              "Maximum number of origins is"
            & Natural'Image (Max_Origins)
            & " ="
            & Natural'Image (Max_Initial_Origins)
            & " initial +"
            & Natural'Image (Max_Assigned_Origins)
            & " assigned +"
            & Natural'Image (Max_Merged_Origins)
            & " merged.");

      end if;

      -- Mark the steps that are merge points:

      Mark_Merge_Points;


      -- Map the flow of values and copies of values:

      Result.Origin_Map := Value_Origins (
         Nodes  => Steps,
         Edges  => Natural (Max_Step_Edge (Graph)),
         Within => Model,
         Afters => True);

      if Opt.Show_Results then

         Show_Origins (
            Model      => Model,
            Origin_Map => Result.Origin_Map,
            Origin_Set => Result.Origin_Set.all,
            Cells      => Cell_For_Number.all);

      end if;

      -- Some working data are no longer needed:

      Storage.Discard (Target_Cells);

      Storage.Discard (Named_Cells);

   end Analyse_Value_Origins;


   procedure Analyse (
      Computation : in     Flow.Computation.Model_Ref;
      Giving      :    out Map_Ref)
   is
   begin

      if Opt.Propagate then

         Analyse_Value_Origins (Computation, Giving);

      else

         Giving := null;

      end if;

   end Analyse;


end Flow.Origins;
