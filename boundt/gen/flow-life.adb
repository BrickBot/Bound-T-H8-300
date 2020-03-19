-- Flow.Life (body)
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
-- $Revision: 1.37 $
-- $Date: 2015/10/24 20:05:48 $
--
-- $Log: flow-life.adb,v $
-- Revision 1.37  2015/10/24 20:05:48  niklas
-- Moved to free licence.
--
-- Revision 1.36  2013-02-19 09:15:55  niklas
-- BT-CH-0245 clean-up. Only trace messages changed.
--
-- Revision 1.35  2013-02-12 08:47:19  niklas
-- BT-CH-0245: Global volatile cells and "volatile" assertions.
--
-- Revision 1.34  2011-08-31 04:23:34  niklas
-- BT-CH-0222: Option registry. Option -dump. External help files.
--
-- Revision 1.33  2009-10-07 19:26:10  niklas
-- BT-CH-0183: Cell-sets are a tagged-type class.
--
-- Revision 1.32  2009-05-15 12:16:47  niklas
-- BT-CH-0173: Handling cells used by Range_Post constraints.
--
-- Revision 1.31  2009/01/18 08:56:31  niklas
-- Removed unused local subtype.
--
-- Revision 1.30  2008/04/22 12:40:08  niklas
-- Added function Program, for convenience.
--
-- Revision 1.29  2007/12/21 10:09:39  niklas
-- BT-CH-0100: Fix Flow.Life re edge preconditions.
--
-- Revision 1.28  2007/12/17 13:54:36  niklas
-- BT-CH-0098: Assertions on stack usage and final stack height, etc.
--
-- Revision 1.27  2007/11/10 15:53:11  niklas
-- BT-CH-0096: Fix overestimated Max_Joint_Effect_Length.
--
-- Revision 1.26  2007/10/31 12:16:01  niklas
-- BT-CH-0095: Arithmetic analysis of "live" dynamic data refs.
--
-- Revision 1.25  2007/10/28 09:32:45  niklas
-- BT-CH-0092: Arithmetic analysis of dynamic data refs is optional.
--
-- Revision 1.24  2007/10/26 12:44:35  niklas
-- BT-CH-0091: Reanalyse a boundable edge only if its domain grows.
--
-- Revision 1.23  2007/10/03 07:26:34  niklas
-- BT-CH-0081: Fix: Size of joint effect from Flow.Life.Join_Effects.
-- BT-CH-0081: Fix: Use given computation model in Local_Demand.
--
-- Revision 1.22  2007/08/17 14:44:00  niklas
-- BT-CH-0074: Stable and Unstable stacks.
--
-- Revision 1.21  2007/08/02 11:18:15  niklas
-- Extended Join_Effects to implement Opt.Trace_Joining.
--
-- Revision 1.20  2007/07/26 11:08:53  niklas
-- BT-CH-0066. Fracture assignments.
--
-- Revision 1.19  2007/03/29 15:18:02  niklas
-- BT-CH-0056.
--
-- Revision 1.18  2007/02/13 20:18:35  Niklas
-- BT-CH-0044.
--
-- Revision 1.17  2007/01/25 21:25:15  niklas
-- BT-CH-0043.
--
-- Revision 1.16  2007/01/13 13:51:04  niklas
-- BT-CH-0041.
--
-- Revision 1.15  2005/10/20 19:34:01  niklas
-- BT-CH-0016.
--
-- Revision 1.14  2005/09/12 19:02:59  niklas
-- BT-CH-0008.
--
-- Revision 1.13  2005/04/20 12:23:35  niklas
-- Corrected Local_Demand to include the basis cells of all dynamically
-- referenced operands (dynamic read accesses). Added the procedure
-- Add_Dynamic_Operand_Bases to support this. We need this now, but
-- when aliasing is implemented it should be done only for assignments
-- where the target is a live cell or aliases with some live cell(s).
--
-- Revision 1.12  2005/03/16 21:47:11  niklas
-- Corrected the setting of Max_Offset to work for the strange
-- case of a subprogram with no arithmetic assignments at all.
--
-- Revision 1.11  2005/02/23 09:05:19  niklas
-- BT-CH-0005.
--
-- Revision 1.10  2005/02/16 21:11:44  niklas
-- BT-CH-0002.
--
-- Revision 1.9  2004/08/09 19:51:32  niklas
-- BT-CH-0001.
--
-- Revision 1.8  2004/04/27 20:05:51  niklas
-- First Tidorum version.
-- Take Cell_T stuff from Storage instead of Arithmetic.
-- Support Range constraint assignments. The target of a Range assignment
-- depends on all cells used in the range constraint expressions. A Range
-- assignment is live if the constrained target cell is live. Live_Effect
-- deletes dead Range assignments from its result.
-- Allow options For_Time and For_Space in Find_Live_Cells.
-- Take into account feasibility of flow-graph edges.
-- Collect live-assignment map on the fly during iteration.
--
-- Revision 1.7  2003/03/11 08:23:57  holsti
-- Split execution-bounds stuff from the Programs package to
-- make the child package Programs.Execution.
--
-- Revision 1.6  2001/03/21 20:20:17  holsti
-- Updated for changes in Output.Image.
--
-- Revision 1.5  2001/03/15 07:17:39  holsti
-- Basis includes input cells.
--
-- Revision 1.4  2001/03/10 22:37:41  holsti
-- Stack cell added to Post_Live only for return steps.
--
-- Revision 1.3  2001/03/10 00:48:05  holsti
-- Computes also input, output and relevant cells.
--
-- Revision 1.2  2001/02/01 10:50:44  ville
-- Removed unused cells from live cells
--
-- Revision 1.1  2001/01/07 21:54:17  holsti
-- First version.
--


with Ada.Unchecked_Deallocation;
with Ada.Text_IO;

with Calling;
with Faults;
with Flow.Life.Opt;
with Flow.Life.Show;
with Flow.Show;
with Flow.Calls;
with Least_Fixpoint;
with Output;
with Storage.Bitvec_Cell_Sets;
with Storage.References;
with Storage.List_Cell_Sets;


package body Flow.Life is


   package Live_Cell_Sets renames Storage.Bitvec_Cell_Sets;
   --
   -- The kind of cell-sets that we (mostly) use.

   use Live_Cell_Sets;


   subtype Live_Cell_Set_T is Live_Cell_Sets.Set_T;
   --
   -- A set of cells, as used here.


   use type Arithmetic.Opt.Ref_Choice_T;


   subtype Assignment_Offset_T is Natural;
   --
   -- Identifies one of the assignments in the effect of a step,
   -- as an offset to the 'First assignment-index for this step.


   type Life_Map_T is
      array (Step_Index_T range <>,
             Assignment_Offset_T range <>)
      of Boolean;
   --
   -- Whether, in a given step, a given assignment is live.
   -- Thus, a True value means "live" and a False value means "dead".
   --
   pragma Pack (Life_Map_T);


   type Living_Object_T (
      Max_Step   : Step_Index_T;
      Max_Offset : Assignment_Offset_T)
   is record
      Model      : Computation.Model_Handle_T;
      Inputs     : Live_Cell_Set_T;
      Basis      : Live_Cell_Set_T;
      Outputs    : Live_Cell_Set_T;
      Basis_Size : Natural;
      Range_Ass  : Natural;
      For_Data   : Arithmetic.Opt.Ref_Choice_T;
      Live       : Life_Map_T (1 .. Max_Step, 0 .. Max_Offset);
   end record;
   --
   -- Defines the "live" assignments.
   --
   -- Max_Step
   --    Defines the range of step indices, 1 .. Max_Step. Comes from
   --    the underlying flow-graph.
   -- Max_Offset
   --    Defines the range of index offsets in the Effects of each step,
   --    which is one less than the maximum length of any Effect in the
   --    underlying computation Model.
   -- Model
   --    The underlying computation model.
   -- Inputs
   --    The set of input cells, that is, the cells that are "live"
   --    on entry to the subprogram.
   -- Basis
   --    The set of basis cells, that is, all the cells that are "live"
   --    at some point in the subprogram. It is not necessary for all
   --    basis cells to be simultaneously "live" at the same point.
   -- Outputs
   --    The set of output cells, that is, all cells that are assigned
   --    at some point in the subprogram, as far as we know. Unresolved
   --    referents of boundable targets are not included.
   -- Basis_Size
   --    The total number of "live" cells = Card (Basis).
   -- Range_Ass
   --    The total number of "live" range-constraint assignments in
   --    all feasible steps in the flow-graph.
   --    This is used in Max_Joint_Effect_Length.
   -- For_Data
   --    Whether the basis cells for boundable data references are
   --    included as "live" cells, depending on where the reference
   --    occurs.
   -- Live
   --    A bit-map that shows the "live" assignments (indexed by offset
   --    from the start of the Effect in the Model) for each step
   --    (indexed by step-index).


   function Model (Item : Living_T)
   return Flow.Computation.Model_Handle_T
   is
   begin

      return Item.Model;

   end Model;


   function Graph (Item : Living_T) return Graph_T
   is
   begin

      return Flow.Computation.Graph (Item.Model.all);

   end Graph;


   function Program (Item : Living_T) return Programs.Program_T
   is
   begin

      return Computation.Program (Item.Model.all);

   end Program;


   function Inputs (Item : Living_T) return Storage.Cell_Set_T
   is
   begin

      return Item.Inputs;

   end Inputs;


   function Basis (Item : Living_T) return Storage.Cell_Set_T
   is
   begin

      return Item.Basis;

   end Basis;


   function Outputs (Item : Living_T) return Storage.Cell_Set_T
   is
   begin

      return Item.Outputs;

   end Outputs;


   procedure Trace (
      Action  : in String;
      Step    : in Step_T;
      Source  : in Step_T;
      Input   : in Storage.Cell_Set_T;
      Output  : in Storage.Cell_Set_T;
      Grew    : in Boolean)
   --
   -- Report a "transform" or "merging" action.
   --
   is
      use Ada.Text_IO;
   begin

      Put (
          "Liveness:"
        & Action
        & ":Step:" & Step_Index_T'Image (Index (Step)));

      if Source /= null then
         Put (":Source_Step:" & Step_Index_T'Image (Index (Source)));
      end if;

      New_Line;

      Put_Line ("- Input  : " & Storage.Image (Input));
      Put_Line ("- Output : " & Storage.Image (Output));
      Put_Line ("- Grew   : " & Boolean'Image (Grew));

   end Trace;


   --
   -- Components for Least_Fixpoint for liveness analysis:
   --


   function Positive_Index (Step : Step_T) return Positive
   --
   -- The index of a step, as a Positive.
   --
   is
   begin
      return Positive (Index (Step));
   end Positive_Index;


   function Feasible_Predecessors (
      Step   : Step_T;
      Cells  : Live_Cell_Set_T;
      Within : Flow.Computation.Model_Ref)
   return Step_List_T
   --
   -- All the feasible predecessors of the given Step, whatever the
   -- currently considered Cells are. For liveness analysis, the "values"
   -- (cell sets) flowing around the graph do not influence the edge
   -- preconditions.
   --
   is
   begin

      return Flow.Computation.Predecessors (Step, Within);

   end Feasible_Predecessors;


   type Cell_Set_List_T is array (Positive range <>) of Live_Cell_Set_T;
   --
   -- The values computed by liveness analysis are cell-sets, indicating
   -- the live cells after each step.


   function Max_Population (Alive : Cell_Set_List_T) return Natural
   --
   -- The maximum population of cells that is "live" after any step.
   -- In other words, the maximum cardinality of the elements in Alive.
   -- Another way to think of it is as the maximum width of the stream
   -- of life, at any step in the flow-graph.
   --
   is

      Max : Natural := 0;
      -- The result.

   begin

      for A in Alive'Range loop

         Max := Natural'Max (Max, Card (Alive(A)));

      end loop;

      return Max;

   end Max_Population;


   type Call_Index_Map_T is array (Step_Index_T range <>) of Natural;
   --
   -- Associates a step with an index to a vector of calls or
   -- call-bounds, to show the call or call-bounds that is contained
   -- in a step. Zero means that the step does not contain a call.


   procedure Mark_Call_Steps (
      Steps      : in     Step_List_T;
      Calls      : in     Programs.Execution.Call_Bounds_List_T;
      Call_Index :    out Call_Index_Map_T)
   --
   -- Sets up the Call_Index vector to refer to the call for each
   -- call-step.
   --
   is

      Step_Index : Step_Index_T;
      -- The index of a call step.

   begin

      Call_Index := (others => 0);

      for C in Calls'Range loop

         Step_Index := Index (Programs.Step (Calls(C).Call));

         if Call_Index(Step_Index) /= 0 then

            Output.Fault (
               Location => "Flow.Life.Mark_Call_Steps",
               Text     =>
                    "More than one call in step #"
                  & Step_Index_T'Image (Step_Index));

         end if;

         Call_Index(Step_Index) := C;

      end loop;

   end Mark_Call_Steps;


   procedure Add_Input_Cells (
      Call     : in     Programs.Execution.Call_Bounds_T;
      Protocol : in     Calling.Protocol_Ref;
      From     : in     Storage.Cell_Set_T;
      To       : in out Storage.Cell_Set_T)
   --
   -- Adds To a given cell-set those cells From another cell-set that
   -- are input cells needed to bound a given Call using a given
   -- calling Protocol.
   --
   is
      use Storage;

      Inputs : constant Flow.Calls.Parameter_Map_T :=
         Flow.Calls.Input_Parameters (
            Inputs   => Programs.Execution.Input_Cells (Call.Bounds),
            Protocol => Protocol.all);
      -- The input parameters of the call (still required for finishing
      -- the bounding of the call), as associations between caller and
      -- callee cells. We need only the caller cells since we are
      -- analysing only the caller at this point.

   begin

      for I in Inputs'Range loop

         if Is_Member (Cell => Inputs(I).Caller, Of_Set => From) then

            Add (Cell => Inputs(I).Caller, To => To);

         end if;

      end loop;

   end Add_Input_Cells;


   procedure Add_Dynamic_Target_Bases (
      Effect : in     Arithmetic.Effect_Ref;
      Live   : in out Storage.Cell_Set_T)
   --
   -- Adds to the Live set the basis cells for any dynamically addressed
   -- assignment target in the effect of the step.
   --
   is
      use type Arithmetic.Expr_Kind_T;

      Target : Arithmetic.Variable_T;
      -- The target of an assignment in the Effect.

   begin

      for E in Effect'Range loop

         Target := Effect(E).Target;

         if Target.Kind = Arithmetic.Ref then

            Storage.References.Add_Basis_Cells (
               From => Target.Ref.all,
               To   => Live);

         end if;

      end loop;

   end Add_Dynamic_Target_Bases;


   function Assigns_Volatile_Cell (Assignment : Arithmetic.Assignment_T)
   return Boolean
   --
   -- Whether the target of the assignment is a volatile cell.
   -- If the target is an unbounded dynamic reference, we cannot
   -- know if the actual target cell is volatile, and return False.
   --
   is
      use Arithmetic;
   begin

      return Assignment.Target.Kind = Cell
      and then Storage.Is_Volatile (Assignment.Target.Cell);

   end Assigns_Volatile_Cell;


   procedure Add_Target_Sources (
      Step   : in     Step_T;
      Kind   : in     Arithmetic.Assignment_Kind_T;
      Live   : in out Storage.Cell_Set_T;
      Living : in out Living_Object_T)
   --
   -- Adds cells to the Live set to ensure that for any assignment
   -- of the given Kind in the effect of the step, if the Target cell
   -- is Live then so are all the cells used in that assignment. This
   -- includes cells whose post-value is used when Kind = Range_Post.
   --
   -- Marks as Living any assignment of this Kind with a Live Target.
   -- Assignments to volatile target cells are ignored (considered dead).
   --
   is
      use type Arithmetic.Assignment_Kind_T;
      use type Arithmetic.Expr_Kind_T;

      Effect : constant Arithmetic.Effect_Ref :=
         Computation.Effect (Step, Living.Model.all);
      -- The effect of the step.

      Cells_Added : Boolean;
      -- Whether some cells were added to Live in the last
      -- pass, so another pass is needed.

      Added : array (Effect'Range) of Boolean := (others => False);
      -- Whether the cells used in a given assignment have been
      -- added to Live.

   begin

      for N in 0 .. Effect'Length loop
         -- The N counter bounds the number of passes.

         Cells_Added := False;

         for E in Effect'Range loop

            if       Effect(E).Kind = Kind
            and then Effect(E).Target.Kind = Arithmetic.Cell
            and then Storage.Is_Member (Effect(E).Target.Cell, Live)
            then
               -- This assignment is live, unless the target is volatile.

               if not Storage.Is_Volatile (Effect(E).Target.Cell) then
                  -- Live assignment to regular (non-volatile) cell.

                  Living.Live(Index(Step), E - Effect'First) := True;

                  if not Added(E) then
                     -- We have not yet added to Live the cells used in
                     -- this assignment.

                     Arithmetic.Add_Cells_Used (
                        By    => Effect(E),
                        Refs  => Living.For_Data /= Arithmetic.Opt.None,
                        Post  => True,
                        To    => Live,
                        Added => Cells_Added);

                     Added(E) := True;

                  end if;

               elsif Opt.Trace_Volatile then

                  Output.Trace (
                     Text =>
                          "Liveness analysis kills assignment to "
                        & "volatile cell"
                        & Output.Field_Separator
                        & Arithmetic.Image (Effect(E)),
                     Locus => Flow.Show.Locus (
                        Step   => Step,
                        Source => Programs.Symbol_Table (
                           Flow.Computation.Program (Living.Model.all))));

               end if;

            -- TBA handling dynamically addressed Targets.

            end if;

         end loop;

         exit when not Cells_Added;

      end loop;

      -- Check that transitive closure converged:

      if Cells_Added then

         Output.Fault (
            Location => "Flow.Life.Add_Target_Sources",
            Text     => "Transitive closure did not converge.");

      end if;

   end Add_Target_Sources;


   procedure Pass_Undefined_Cells (
      Post_Live : in     Storage.Cell_List_T;
      Effect    : in     Arithmetic.Effect_Ref;
      Pre_Live  : in out Storage.Cell_Set_T;
      Grew      : in out Boolean)
   --
   -- For any cell in Post_Live that is not defined by a Defining
   -- assignment in Effect, adds this cell to Pre_Live. If this
   -- adds new elements to Pre_Live, Grew is set to True, otherwise
   -- Grew is unchanged.
   --
   -- For volatile cells in Post_Live, the presence of an Effect
   -- assignment to this cell does not matter; the cell is added
   -- to Pre_Live anyway (and Grow set to True) if the cell is not
   -- already a member of Pre_Live.
   --
   is
      use Storage;

      Post : Cell_T;
      -- One of the Post_Live cells.

   begin

      for P in Post_Live'Range loop

         Post := Post_Live(P);

         if not Is_Member (Post, Pre_Live)
         and then (
            Storage.Is_Volatile (Post)
            or else not Arithmetic.Is_Defined (Post, Effect.all))
         then
            -- This cell, too, must be demanded from the predecessors
            -- of the step with this Effect, since the cell is demanded
            -- by successors and is not defined by the step itself.

            Add (Cell => Post, To => Pre_Live);

            Grew := True;

         end if;

      end loop;

   end Pass_Undefined_Cells;


   procedure Show_Statistics (
      Living  : in Living_T;
      Max_Pop : in Natural)
   --
   -- Displays the number of live and dead assignments, in response
   -- to the option Opt.Show_Statistics. Also shows the maximum live
   -- population.
   --
   is

      Model : Computation.Model_Ref renames Living.Model.all;
      -- The computation model that was used.

      Graph : constant Graph_T := Computation.Graph (Model);
      -- The underlying control-flow graph.

      Step : Step_T;
      -- One of the steps.

      Eff : Arithmetic.Effect_Ref;
      -- The effect of Step.

      Total_Assignments : Natural := 0;
      -- Number of assignments in the graph.

      Live_Assignments : Natural := 0;
      -- Number of live assignments in the graph.

      Infeasible_Steps : Natural := 0;
      -- The number of infeasible steps that are not included
      -- in the other counts.

   begin

      -- Compute statistics on the live assignments:

      for S in Living.Live'Range(1) loop

         Step := Step_At (Index => S, Within => Graph);

         if Computation.Is_Feasible (Step, Model) then

            -- Count the live and dead assignments in this step:

            Eff := Computation.Effect (Step, Model);

            Total_Assignments := Total_Assignments + Eff'Length;

            for E in Eff'Range loop

               if Living.Live(S, E - Eff'First) then

                  Live_Assignments := Live_Assignments + 1;

               end if;

            end loop;

         else

            Infeasible_Steps := Infeasible_Steps + 1;

         end if;

      end loop;

      Output.Trace (Text =>
            "Assignment statistics"
          & Output.Field_Separator
          & "Total"
          & Output.Field_Separator
          & Output.Image (Total_Assignments)
          & Output.Field_Separator
          & "Live"
          & Output.Field_Separator
          & Output.Image (Live_Assignments)
          & Output.Field_Separator
          & "Range"
          & Output.Field_Separator
          & Output.Image (Living.Range_Ass)
          & Output.Field_Separator
          & "Steps"
          & Output.Field_Separator
          & Output.Image (Natural'(Living.Live'Length(1)))
          & Output.Field_Separator
          & "Max_Per_Step"
          & Output.Field_Separator
          & Output.Image (Natural'(Living.Live'Length(2)))
          & Output.Field_Separator
          & "Basis_Size"
          & Output.Field_Separator
          & Output.Image (Living.Basis_Size)
          & Output.Field_Separator
          & "Max_Live_Pop"
          & Output.Field_Separator
          & Output.Image (Max_Pop)
          & Output.Field_Separator
          & "Infeasible_Steps"
          & Output.Field_Separator
          & Output.Image (Infeasible_Steps));

   end Show_Statistics;


   procedure Show_Dead_Assignments (
      Step  : in Step_T;
      Model : in Computation.Model_Ref;
      Live  : in Storage.Cell_Set_T)
   --
   -- Display the dead assignments of the step, in response to
   -- the choice Opt.Show_Dead.
   --
   is
      use Arithmetic;

      Eff : constant Effect_Ref := Computation.Effect (Step, Model);
      -- The effect of the step under the given Model.

   begin

      for E in Eff'Range loop

         if Eff(E).Target.Kind = Cell
         and then not Storage.Is_Member (
            Cell   => Eff(E).Target.Cell,
            Of_Set => Live)
         then

            Output.Trace (
               Text =>
                    "Dead in step"
                  & Step_Index_T'Image (Index (Step))
                  & Output.Field_Separator
                  & Image (Eff(E)),
               Locus => Flow.Show.Locus (
                  Step   => Step,
                  Source => Programs.Symbol_Table (
                     Flow.Computation.Program (Model))));

         end if;

      end loop;

   end Show_Dead_Assignments;


   function Live_Computation (
      Model     : Computation.Model_Handle_T;
      Calls     : Programs.Execution.Call_Bounds_List_T;
      Heights   : Storage.Cell_List_T;
      Finals    : Storage.Cell_List_T;
      Asserts   : Assertions.Assertion_Set_T;
      For_Time  : Boolean;
      For_Space : Boolean;
      For_Data  : Arithmetic.Opt.Ref_Choice_T)
   return Living_T
   is
      use Storage;

      Graph : constant Graph_T := Computation.Graph (Model.all);
      --
      -- The subprogram's flow-graph.
      -- Only the step-level graph will be used.

      Steps : constant Step_List_T := All_Steps (Graph);
      --
      -- All the steps of the graph.
      -- Note that the indices in this vector are equal to the
      -- Step_Index_T values of the steps (but of Positive type).

      -- Liveness analysis is implemented by a least-fixpoint
      -- computation that propagates _against_ the execution flow.
      -- The goal is to compute the set of live cells _after_ each step.
      -- During the iteration, any assignment that can define or
      -- constrain live cells is marked as a live assignment.

      subtype Call_Index_T is Natural range 0 .. Calls'Last;
      --
      -- The index of a call in Calls, or zero if there is no call.

      Call_Index : Call_Index_Map_T (1 .. Max_Step (Graph));
      --
      -- If Steps(S) is a call step, Call_Index(S) is the corresponding
      -- index of the call in Calls, else it is zero.

      Alive : Cell_Set_List_T (Steps'Range);
      --
      -- For each step, the set of live cells after the step.

      Living : Living_T;
      --
      -- The result.


      function Locus (Step : Step_T) return Output.Locus_T
      is
      begin

         return Flow.Show.Locus (
            Step   => Step,
            Source => Programs.Symbol_Table (
               Flow.Computation.Program (Model.all)));

      end Locus;


      procedure Add_Dynamic_Operand_Bases (
         Step   : in     Step_T;
         Effect : in     Arithmetic.Effect_Ref;
         Live   : in out Storage.Cell_Set_T)
      --
      -- Adds to the Live set the basis cells for any dynamically addressed
      -- operands in the effect of the step, except in assignments that
      -- target a volatile cell.
      --
      -- The Step is only for trace output locus.
      --
      is
         use Arithmetic;

         Assignment : Assignment_T;
         -- One of the assignments in the Effect.

      begin

         for E in Effect'Range loop

            Assignment := Effect(E);

            if not Assigns_Volatile_Cell (Assignment) then
               -- Assignment to regular (non-volatile) cell, thus
               -- perhaps passing values to later computations.

               case Assignment.Kind is

               when Regular =>

                  Add_Reference_Bases (
                     From => Assignment.Value,
                     To   => Live);

               when Conditional =>

                  Add_Reference_Bases (
                     From => Assignment.Cond,
                     To   => Live);

                  Add_Reference_Bases (
                     From => Assignment.Value1,
                     To   => Live);

                  Add_Reference_Bases (
                     From => Assignment.Value2,
                     To   => Live);

               when Fracture =>

                  null;

               when Range_Kind_T =>

                  Add_Reference_Bases (
                     From => Assignment.Min,
                     To   => Live);

                  Add_Reference_Bases (
                     From => Assignment.Max,
                     To   => Live);

               end case;

            else
               -- The target is a volatile cell, therefore the assignment
               -- is useless for the analysis of later computations.

               if Opt.Trace_Volatile then

                  Output.Trace (
                     Text =>
                          "Liveness analysis ignores basis cells"
                        & "in assignment to volatile cell"
                        & Output.Field_Separator
                        & Arithmetic.Image (Assignment),
                     Locus => Locus (Step));

               end if;

            end if;

         end loop;

      end Add_Dynamic_Operand_Bases;


      procedure Deliver_And_Demand (
         Post_Live : in     Live_Cell_Set_T;
         Step      : in     Step_T;
         Pre_Live  : in out Live_Cell_Set_T;
         Grew      :    out Boolean)
      --
      -- The "transform" operation for Least_Fixpoint used for liveness.
      -- Abstractly executes the step, in reversed flow order, using the
      -- live-set after the step (Post_Live) and updating the live-set
      -- before the step (Pre_Live).
      --
      -- Marks as Living any assignments in the effect of the Step that
      -- define or constrain live cells.
      --
      -- Note that the parameters are named according to the normal flow
      -- order, not according to the reverse order as used in the fixpoint
      -- analysis and in the formal generic Transform parameter.
      --
      is
         use Storage;
         use type Arithmetic.Assignment_Kind_T;
         use type Arithmetic.Expr_Kind_T;

         Step_Index : constant Step_Index_T := Index (Step);
         -- The index of the step, in Living.

         Eff : constant Arithmetic.Effect_Ref :=
            Computation.Effect (Step, Model.all);
         -- The effect of the step under the given Model.

         Post_Demand : Live_Cell_Set_T := Post_Live;
         -- The cells demanded from the step. Initialized to the cells
         -- demanded by successor steps. Will contain also cells demanded
         -- by the Range_Post constraints on the former cells.

      begin

         Grew := False;
         -- Unless growth is detected later.

         -- For Range_Post constraints on target cells that are live
         -- after the step, demand from this step any cell used in
         -- the constraint:

         Add_Target_Sources (
            Step   => Step,
            Kind   => Arithmetic.Range_Post,
            Live   => Post_Demand,
            Living => Living.all);

         -- If a cell is assigned in this step, is live after this
         -- step, and is not volatile, demand from the predecessor steps
         -- the cells used in the assignment, and also the cells used
         -- in any Range_Rel constraint on such assigned cells:

         for E in Eff'range loop

            if Eff(E).Kind in Arithmetic.Defining_Kind_T
            or Eff(E).Kind =  Arithmetic.Range_Rel
            then

               if Eff(E).Target.Kind = Arithmetic.Cell
               and then Is_Member (Eff(E).Target.Cell, Post_Demand)
               then
                  -- An assignment to a cell that is live after the step.

                  if not Storage.Is_Volatile (Eff(E).Target.Cell) then
                     -- The cell is not volatile, therefore the assignment
                     -- determines the value of the cell after the step,
                     -- and is a live assignment.

                     Living.Live(Step_Index, E - Eff'First) := True;

                     Arithmetic.Add_Cells_Used (
                        By    => Eff(E),
                        Refs  => For_Data /= Arithmetic.Opt.None,
                        Post  => False,
                        To    => Pre_Live,
                        Added => Grew);

                  elsif Opt.Trace_Volatile then

                     Output.Trace (
                        Text =>
                             "Liveness analysis ignores assignment "
                           & "to volatile cell"
                           & Output.Field_Separator
                           & Arithmetic.Image (Eff(E)),
                        Locus => Locus (Step));

                  end if;

               -- TBA handling dynamically addressed Targets.

               end if;

            end if;

         end loop;

         -- If a cell is live after this step, and is not assigned
         -- in this step, demand it directly from the predecessor
         -- cells (if not already demanded):

         Pass_Undefined_Cells (
            Post_Live => To_List (Post_Demand),
            Effect    => Eff,
            Pre_Live  => Pre_Live,
            Grew      => Grew);

         -- All Range_Pre constraints on cells that are live before
         -- this step are live assignments, and also their source
         -- cells must be demanded from the predecessors:

         Add_Target_Sources (
            Step   => Step,
            Kind   => Arithmetic.Range_Pre,
            Live   => Pre_Live,
            Living => Living.all);

         if Opt.Trace_Iteration then

             Trace (
                Action  => "Trans",
                Step    => Step,
                Source  => null,
                Input   => Post_Live,
                Output  => Pre_Live,
                Grew    => Grew);

         end if;

      end Deliver_And_Demand;


      procedure Local_Demand (
         Step     : in     Step_T;
         Pre_Live : in out Storage.Cell_Set_T)
      --
      -- Computes the live-set before the step (Pre_Live) without
      -- considering successor steps, including only the cells required
      -- by the step itself.
      --
      -- The cells required by the step itself include the basis cells
      -- of some of the dynamic boundable objects associated with the step,
      -- as follows:
      --
      -- > The boundable edges leaving the step.
      --
      -- > The boundable assignment targets in the effect of the step,
      --   when For_Data = All_Refs.
      --
      -- > The boundable pointers to operands (dynamic reads) in the
      --   effect of the step, when For_Data = All_Refs.
      --
      -- For an unbounded call step, also the basis cells for the calling
      -- protocol (parameter mapping) and the input cells for the callee
      -- are required.
      --
      -- If space-bounds are requested (For_Space) and there are some
      -- stack-height cells then these cell are considered to be demanded
      -- by all steps, because the stack-height can, in principle, be
      -- "observed" at any time.
      --
      -- If the target of a Range_Pre assignment is required for one of
      -- the above reasons, all cells used in the Min and Max expressions
      -- of the Range_Pre also become required, and transitively.
      --
      -- A cell that is used only by the assignments in the effect of
      -- the step, but not in the above ways, is not here considered
      -- to be required by the step, because it is not known if the
      -- assigned values will be required in the successor steps.
      --
      -- No cells used in edge conditions are included. See Self_Demand.
      --
      -- The demanded cells are added to Pre_Live.
      --
      -- Marks as Living any assignment in the step's effect that is
      -- used for the above (occurs only for Range_Pre assignments).
      --
      is

         CI : constant Call_Index_T := Call_Index (Index (Step));
         -- The call index in Calls, if this is a call step.

         Dynamic_Edges : constant Dynamic_Edge_List_T :=
            Computation.Unstable_Dynamic_Edges_From (
               Source => Step,
               Under  => Model.all);
         -- The feasible, but not yet stably resolved (or stably
         -- unresolved) dynamic edges leaving the step, if any.

         Protocol : Calling.Protocol_Ref;
         -- The protocol of the call, if this is a call step.

      begin

         -- Basis cells for dynamic edges:

         for D in Dynamic_Edges'Range loop

            Flow.Add_Basis_Cells (
               From => Dynamic_Edges(D).all,
               To   => Pre_Live);

         end loop;

         -- Cells needed by a call:

         if CI in Calls'Range then
            -- Call here.

            Protocol := Computation.Calling_Protocol (
               Call  => Calls(CI).Call,
               Under => Model.all);

            -- The basis cells of any dynamically-mapping calling
            -- protocol are needed.

            Calling.Add_Basis_Cells (
               From => Protocol.all,
               To   => Pre_Live);

            -- The input cells of an unbounded call are needed:

            if not Programs.Execution.Bounded (Calls(CI).Bounds)
            then
               -- The input parameter cells are needed, if they are
               -- defined in this (caller) subprogram:

               Add_Input_Cells (
                  Call     => Calls(CI),
                  Protocol => Protocol,
                  From     => Living.Outputs,
                  To       => Pre_Live);

               -- If some input cells are dynamically mapped, and this
               -- mapping has not yet been resolved, the above may not
               -- yet cover all input cells. TBM.

            end if;

         end if;

         -- The stack-height cells may be needed:

         if For_Space then

            Storage.Add (
               Cells => Heights,
               To    => Pre_Live);

         end if;

         if For_Data = Arithmetic.Opt.All_Item then
            -- The basis cells of all boundable data references are
            -- considered interesting (because we want to resolve
            -- or bound the set of cells that is accessed).

            -- All cells used to compute pointers to assignment targets
            -- are needed:

            Add_Dynamic_Target_Bases (
               Effect => Computation.Effect (Step, Model.all),
               Live   => Pre_Live);

            -- For now, all cells used to compute pointers to operands
            -- are also needed, but TBM by removal:

            Add_Dynamic_Operand_Bases (
               Step   => Step,
               Effect => Computation.Effect (Step, Model.all),
               Live   => Pre_Live);

         end if;

         -- All cells used by Range_Pre constraints on needed cells
         -- are also needed:

         Add_Target_Sources (
            Step   => Step,
            Kind   => Arithmetic.Range_Pre,
            Live   => Pre_Live,
            Living => Living.all);

      end Local_Demand;


      procedure Self_Demand (
         Step      : in     Step_T;
         Post_Live :    out Live_Cell_Set_T;
         Pre_Live  :    out Live_Cell_Set_T;
         Grew      :    out Boolean)
      --
      -- The "initialize" operation for Least_Fixpoint for liveness.
      --
      -- Initializes the live-set after the Step (Post_Live) to the empty
      -- set (or to the set of Finals cells, for a return Step), plus the
      -- cells used in the preconditions of all edges from this Step, and
      -- the live-set before the Step (Pre_Live) to the cells absolutely
      -- required by the Step which are the Local_Demand cells, plus the
      -- cells necessary for the computation of the cells live after the
      -- step (Post_Live).
      --
      -- The demands of the successor steps are ignored at this stage,
      -- per the principles of Least_Fixpoint.
      --
      -- Note that the parameters are named according to the normal flow
      -- order, not according to the reverse order as used in the fixpoint
      -- analysis and in the formal generic Initialize parameter.
      --
      is
         use Arithmetic;

         Edges : constant Step_Edge_List_T :=
            Flow.Computation.Edges_From (
               Step  => Step,
               Under => Model.all);
         -- The feasible edges leaving the step, if any.

         Cond : Condition_T;
         -- An edge-condition.

         Dummy_Grew : Boolean;
         -- Unused output flag from Deliver_And_Demand.

      begin

         -- Start from the empty sets:

         Pre_Live  := Empty;
         Post_Live := Empty;

         -- The cells demanded by the step itself are needed
         -- before the step:

         Local_Demand (
            Step     => Step,
            Pre_Live => Pre_Live);

         -- The cells used by the conditions of the edges leaving
         -- the step are needed after the step:

         for E in Edges'Range loop

            Cond := Computation.Condition (Edges(E), Model.all);

            if Cond /= Always then

               Add_Cells_Used (
                  By   => Cond,
                  Refs => For_Data /= Arithmetic.Opt.None,
                  To   => Post_Live);

            end if;

         end loop;

         -- The stack-height cells play a special role in returns:

         if Computation.Is_Final (Step, Model.all) then
            -- A return step. The stack-height cells for Unstable
            -- stacks are also live after this step, for the final
            -- stack-height analysis, although there is no successor
            -- step to demand them. However, the final stack-height
            -- for some Unstable stacks may already be known (from
            -- constant propagation), so only the Finals are added.

            Add (
               Cells => Finals,
               To    => Post_Live);

         end if;

         -- If some cells are needed after the step, the step has
         -- to deliver them, either by its own computations or by
         -- demanding them from its predecessors:

         if not Is_Empty (Post_Live) then

            Deliver_And_Demand (
               Post_Live => Post_Live,
               Step      => Step,
               Pre_Live  => Pre_Live,
               Grew      => Dummy_Grew);

         end if;

         -- Any life before this step?

         Grew := not Is_Empty (Pre_Live);

         if Opt.Trace_Iteration then

             Trace (
                Action  => "Init",
                Step    => Step,
                Source  => null,
                Input   => Post_Live,
                Output  => Pre_Live,
                Grew    => Grew);

         end if;

      end Self_Demand;


      procedure More_Demand (
         Source    : in     Step_T;
         Pre_Live  : in     Live_Cell_Set_T;
         Target    : in     Step_T;
         Post_Live : in out Live_Cell_Set_T;
         Grew      :    out Boolean)
      --
      -- The "merge" operation for Least_Fixpoint used for liveness.
      -- Updates the set of live cells after the Target step (Post_Live),
      -- using a new set of live cells (Pre_Live) demanded by the Source
      -- step, which is a successor of Target (in the real time-order).
      --
      -- Note that the naming of the parameters corresponds partly to the
      -- reversed flow-order used in the fixpoint analysis, and partly to
      -- the real flow-order.
      --
      is
         use Storage;

         Pre : constant Cell_List_T := To_List (Pre_Live);
         -- The cells demanded by the Source step.

      begin

         Grew := False;
         -- Default value unless changes noted below.

         for P in Pre'Range loop

            if not Is_Member (Cell => Pre(P), Of_Set => Post_Live) then

               -- This is a new, demanded cell.

               Add (Cell => Pre(P), To => Post_Live);

               Grew := True;

            end if;

         end loop;

         if Opt.Trace_Iteration then

            Trace (
               Action  => "Merge",
               Step    => Target,
               Source  => Source,
               Input   => Pre_Live,
               Output  => Post_Live,
               Grew    => Grew);

         end if;

      end More_Demand;


      function Live_Cells
      is new Least_Fixpoint (
         Graph       => Computation.Model_Ref,
         Node        => Step_T,
         Node_List   => Step_List_T,
         Index_Of    => Positive_Index,
         Successors  => Feasible_Predecessors,
         Value       => Live_Cell_Set_T,
         Value_List  => Cell_Set_List_T,
         Initialize  => Self_Demand,
         Transform   => Deliver_And_Demand,
         Merge       => More_Demand,
         Finalize    => Discard);
      --
      -- Instance of Least_Fixpoint for live-cell analysis.


      function Alive_Before (Step : Step_T)
      return Live_Cell_Set_T
      --
      -- The live cells before the given Step.
      -- Uses the live cells after the given Step, from the Alive vector,
      -- plus the cells needed by the Step itself, from Local_Demand.
      --
      -- The result is a new cell-set. The caller should Discard it
      -- when the caller has no further use for it.
      --
      is

         Pre_Live : Live_Cell_Set_T;
         -- The result, initially empty.

         Unused : Boolean;
         -- Unused output parameter.

      begin

         -- The cells demanded by the step itself are alive
         -- before the step:

         Local_Demand (
            Step     => Step,
            Pre_Live => Pre_Live);

         -- The cells required to compute the cells live after the
         -- step are also alive before the step:

         Deliver_And_Demand (
            Post_Live => Alive (Positive_Index (Step)),
            Step      => Step,
            Pre_Live  => Pre_Live,
            Grew      => Unused);

         return Pre_Live;

      end Alive_Before;


      procedure Show_Live_Cells
      --
      -- Display the set of live cells for each step.
      --
      is
      begin

         for A in Alive'Range loop

            Output.Trace (
               Text =>
                    "Live cells after step "
                  & Output.Image (A)
                  & Output.Field_Separator
                  & Image (Alive(A)),
               Locus => Locus (Steps(A)));

         end loop;

      end Show_Live_Cells;


      function Num_Live_Range_Ass (Step : Step_Index_T)
      return Natural
      --
      -- The number of live range-constraint assignments in
      -- the effect of the given Step.
      --
      is

         Full : constant Arithmetic.Effect_Ref :=
            Computation.Effect (Step, Living.Model.all);
         -- The full effect of the Step under the Model for Living.

         Num : Natural := 0;
         -- The number of live range-constraint assignments.

      begin

         for F in Full'Range loop

            if Living.Live(Step, F - Full'First)
            and then Full(F).Kind in Arithmetic.Range_Kind_T
            then
               -- This is a live range-constraint assignment.

               Num := Num + 1;

            end if;

         end loop;

         return Num;

      end Num_Live_Range_Ass;


      procedure Remove_Volatile_Cells (
         From : in out Live_Cell_Set_T;
         What : in     String)
      --
      -- Removes all the volatile cells From the given cell-set.
      -- The What parameter is only for tracing.
      --
      is

         All_Cells : constant Storage.Cell_List_T := To_List (From);
         -- All the cells in the given set, initially.

      begin

         for A in All_Cells'Range loop

            if Storage.Is_Volatile (All_Cells(A)) then

               if Opt.Trace_Volatile then

                  Output.Trace (
                       "Liveness analysis removes volatile cell "
                     & Storage.Image (All_Cells(A))
                     & " from "
                     & What);

               end if;

               Remove (
                  Cell => All_Cells(A),
                  From => From);

            end if;

         end loop;

      end Remove_Volatile_Cells;


   begin  -- Live_Computation

      if Opt.Trace_Iteration
      or Opt.Trace_Cells
      or Opt.Show_Per_Step
      or Opt.Show_Per_Node
      or Opt.Show_Dead
      then
         -- Show the Heights and Finals, too:

         Output.Trace (
              "Liveness analysis Heights cells"
            & Output.Field_Separator
            & Storage.Image (Heights));

         Output.Trace (
              "Liveness analysis Finals cells"
            & Output.Field_Separator
            & Storage.Image (Finals));

      end if;

      -- Create the table of assignment-liveness, initialized
      -- to "all dead":

      Living := new Living_Object_T (
         Max_Step   => Max_Step (Graph),
         Max_Offset => Assignment_Offset_T'Max (
            Computation.Max_Effect_Length (Model.all) - 1,
            Assignment_Offset_T'First));
      --
      -- The 'Max for Max_Offset comes into play when the subprogram
      -- has no assignments at all, where the -1 would give a negative
      -- value for Max_Offset.

      Living.Model := Model;

      Living.For_Data := For_Data;

      Living.Live := (others => (others => False));

      -- The output cells are those defined by the assignments
      -- in the flow-graph (including those output cells from
      -- lower-level calls that are already resolved to caller
      -- cells and thus represented in the assignments):

      Living.Outputs := Copy (Computation.Cells_Defined (By => Model.all));

      -- Mark the call steps for later reference:

      Mark_Call_Steps (
         Steps      => Steps,
         Calls      => Calls,
         Call_Index => Call_Index);

      -- Compute the live cells and mark all necessary assignments
      -- as live, too:

      Alive := Live_Cells (
         Nodes  => Steps,
         Edges  => Natural (Max_Step_Edge (Graph)),
         Within => Model.all);

      if Opt.Trace_Iteration
      or Opt.Trace_Cells
      then

         Show_Live_Cells;

      end if;

      -- Compute the input cells as the live cells before
      -- the entry step, but removing all the stack-height cells
      -- since their initial values (input values) are statically
      -- known, and also removing all volatile cells, since their
      -- (assigned) values on entry are irrelevant to the values
      -- read from them later:

      Living.Inputs := Alive_Before (Step => Entry_Step (Graph));

      Remove (
         Cells => Programs.Stack_Height_Cells (
            Computation.Program (Model.all)),
         From  => Living.Inputs);

      Remove_Volatile_Cells (
         From => Living.Inputs,
         What => "input cells");

      -- Compute the relevant cells as the input cells plus
      -- the live cells after any step:

      Living.Basis := Living.Inputs;

      for A in Alive'Range loop

         Add (
            Cells => Alive(A),
            To    => Living.Basis);

      end loop;

      -- Note that Living.Basis may contain volatile cells.
      -- This is because we want to include them in any later
      -- analysis (e.g. arithmetic analysis, since their values
      -- may be bounded by assertions.

      Living.Basis_Size := Card (Living.Basis);

      -- Compute the total number of range-constraint assignments:

      Living.Range_Ass := 0;

      for S in 1 .. Living.Max_Step loop

         if Computation.Is_Feasible (S, Living.Model.all) then

            Living.Range_Ass := Living.Range_Ass + Num_Live_Range_Ass (S);

         end if;

      end loop;

      -- Statistics come at the end, if desired:

      if Opt.Statistics then

         Show_Statistics (
            Living  => Living,
            Max_Pop => Max_Population (Alive));

      end if;

      -- Then the survivors, if desired:

      if Opt.Show_Per_Node then

         Flow.Life.Show.Show_Per_Node (Living);

      end if;

      if Opt.Show_Per_Step then

         Flow.Life.Show.Show_Per_Step (Living);

      end if;

      -- And after that, the dead, if desired:

      if Opt.Show_Dead then

         for S in Steps'Range loop

            Show_Dead_Assignments (
               Step  => Steps(S),
               Model => Model.all,
               Live  => Alive(S));

         end loop;

      end if;

      return Living;

   end Live_Computation;


   function Is_Live (
      Item   : Positive;
      Effect : Arithmetic.Effect_T;
      Step   : Step_T;
      Living : Living_T)
   return Boolean
   is
   begin

      return Living.Live(Index (Step), Item - Effect'First);

   end Is_Live;


   function Basis_Size (Living : Living_T) return Natural
   is
   begin

      return Living.Basis_Size;

   end Basis_Size;


   function Live_Assignments (
      Step   : Step_Index_T;
      Living : Living_T)
   return Arithmetic.Assignment_List_T
   is

      Full : constant Arithmetic.Effect_Ref :=
         Computation.Effect (Step, Living.Model.all);
      -- The full effect of the Step under the Model for Living.

      Live : Arithmetic.Assignment_List_T (1 .. Full'Length);
      -- The live assignments are collected here.

      Last : Natural := 0;
      -- The live assignments are Live(1 .. Last).

   begin

      for F in Full'Range loop

         if Living.Live(Step, F - Full'First) then
            -- This assignment is alive, keep it:

            Last := Last + 1;

            Live(Last) := Full(F);

         end if;

      end loop;

      return Live(1 .. Last);

   end Live_Assignments;


   function Dead_Definitions (
      Step   : Step_Index_T;
      Living : Living_T)
   return Arithmetic.Assignment_List_T
   is

      Full : constant Arithmetic.Effect_Ref :=
         Computation.Effect (Step, Living.Model.all);
      -- The full effect of the Step under the Model for Living.

      Dead : Arithmetic.Assignment_List_T (1 .. Full'Length);
      -- The dead Defining assignments are collected here.

      Last : Natural := 0;
      -- The dead Defining assignments are Dead(1 .. Last).

   begin

      for F in Full'Range loop

         if (not Living.Live(Step, F - Full'First))
         and then Full(F).Kind in Arithmetic.Defining_Kind_T
         then
            -- A dead Defining assignment. Pick this one.

            Last := Last + 1;

            Dead(Last) := Full(F);

         end if;

      end loop;

      return Dead(1 .. Last);

   end Dead_Definitions;


   function Live_Effect (
      Step   : Step_T;
      Living : Living_T)
   return Arithmetic.Effect_T
   is

      S : constant Step_Index_T := Index (Step);
      -- The index of this step.

      Full : constant Arithmetic.Effect_Ref :=
         Computation.Effect (Step, Living.Model.all);
      -- The full effect of the Step under the Model for Living.

      Live : Arithmetic.Effect_T (1 .. Full'Length);
      -- The live assignments are collected here.

      Last : Natural := 0;
      -- The live assignments are Live(1 .. Last).

   begin

      for F in Full'Range loop

         if Living.Live(S, F - Full'First) then
            -- This assignment is alive, keep it:

            Last := Last + 1;

            Live(Last) := Full(F);

         elsif Full(F).Kind in Arithmetic.Defining_Kind_T then
            -- This Defining assignment is dead, but still implies
            -- a change in the target value. We replace it with
            -- an unconstrained update of the target:

            Last := Last + 1;

            Live(Last) := Arithmetic.Set (Target => Full(F).Target);

         -- else
         --    Full(F) is a dead range constraint assignment and
         --    does not by itself imply any change in its target.
         --    This range constraint can be completely omitted
         --    from the Live effect.

         end if;

      end loop;

      return Live(1 .. Last);

   end Live_Effect;


   function Max_Joint_Effect_Length (
      Steps  : Step_List_T;
      Living : Living_T)
   return Natural
   is
   begin

      return Living.Basis_Size + Living.Range_Ass;
      --
      -- The joint effect contains at most one assignment to each
      -- basis cell, and all range-constraint assignments on the
      -- basis cells from any of the Steps. There can be several
      -- range constraints on the same cell, from different steps
      -- or even from the same step.

   end Max_Joint_Effect_Length;


   procedure Join_Effects (
      Steps  : in     Step_List_T;
      Living : in     Living_T;
      Basis  : in     Storage.Cell_Set_T;
      Into   :    out Arithmetic.Assignment_Set_T;
      Last   :    out Natural)
   is
      use type Arithmetic.Expr_Kind_T;

      package Join_Cell_Sets renames Storage.List_Cell_Sets;
      -- The cell-set implementation that we use.

      Defined : Join_Cell_Sets.Set_T;
      -- The set of Basis cells so far defined in the Into set (that
      -- is, the target cells of the Defining assignments in Into).
      -- Initially empty by default.

      Next : Positive := Steps'First;
      -- The index in Steps of the next step to be checked and
      -- perhaps joined.

      Next_Step : Step_Index_T;
      -- The step-index of the next step.

      Full_Effect : Arithmetic.Effect_Ref;
      -- The full effect of the Next step under the Model for Living.


      function Joinable (Item : Arithmetic.Assignment_T)
      return Boolean
      --
      -- Whether this "live" assignment can be joined Into the existing
      -- set of assignments, without creating a double def or a def-use
      -- dependency.
      --
      is
      begin

         if       Item.Kind in Arithmetic.Defining_Kind_T
         and then Item.Target.Kind = Arithmetic.Cell
         and then Join_Cell_Sets.Is_Member (
                     Cell   => Item.Target.Cell,
                     Of_Set => Defined)
         then
            -- The assignment is a Defining one and the target is a cell
            -- that is already defined in the Into set. Joining this
            -- assignment would create a double definition of this cell
            -- within the Into set, so we cannot join it.

            return False;

         elsif Arithmetic.Any_Cell_Is_Used (
                  From => Defined,
                  By   => Item)
         then
            -- The target reference (if not a cell), the condition (if
            -- any) or the assigned value(s) uses a cell that is already
            -- defined in the Into set. Joining this assignment would
            -- create a def-use dependency within the Into set, so we
            -- cannot join it.

            return False;

         else
            -- No problems, join away!

            return True;

         end if;

      end Joinable;


      procedure Join (Item : Arithmetic.Assignment_T)
      --
      -- Adds this "live" assignment to the Into set if its Target
      -- is a Basis cell and updates the Defined set accordingly.
      --
      is
      begin

         if Item.Target.Kind = Arithmetic.Cell
         and then Storage.Is_Member (
                     Cell   => Item.Target.Cell,
                     Of_Set => Basis)
         then
            -- This assignment is one we want to keep.

            Arithmetic.Add (
               To   => Into,
               More => Item);

            if Item.Kind in Arithmetic.Defining_Kind_T then

               Join_Cell_Sets.Add (
                  Cell => Item.Target.Cell,
                  To   => Defined);

            end if;

         end if;

      end Join;


      procedure Kill (Item : Arithmetic.Assignment_T)
      --
      -- Adds this "dead" assignment as an opaque assignment
      -- to the joint effect, if it is a Defining assignment and
      -- the target is a Basis cell that is not already the target
      -- of an assignment in the joint effect.
      --
      is
      begin

         if       Item.Kind in Arithmetic.Defining_Kind_T
         and then Item.Target.Kind = Arithmetic.Cell
         and then Storage.Is_Member (
                     Cell   => Item.Target.Cell,
                     Of_Set => Basis)
         and then (
            not Arithmetic.Is_Defined (
                   Cell => Item.Target.Cell,
                   By   => Into))
         then
            -- The Target.Cell is modified by a "dead" Defining
            -- assignment and is not defined by a "live" Defining
            -- assignment or an earlier kill. It needs an opaque
            -- assignment in the joint effect.

            if Opt.Trace_Joining then

               Output.Trace (
                   "Killing "
                  & Storage.Image (Item.Target.Cell)
                  & " in "
                  & Arithmetic.Image (Into));

            end if;

            Arithmetic.Add (
               To   => Into,
               More => Arithmetic.Set (Target => Item.Target));

         end if;

      end Kill;


   begin  -- Join_Effects

      if Opt.Trace_Joining then

         Output.Trace (
              "Joining steps"
            & Flow.Index_Image (Steps)
            & " under basis "
            & Storage.Image (Basis));

      end if;


      -- Clear the board:

      Arithmetic.Erase (Into);

      Last := Steps'First - 1;

      -- Join as many steps as possible:

      Join_Loop :
      loop

         Next_Step := Index (Steps(Next));

         Full_Effect := Computation.Effect (Next_Step, Living.Model.all);

         -- Can the step join the set?

         if Next > Steps'First then
            -- The first step can always join, only the later ones
            -- can have problems.

            for F in Full_Effect'Range loop

               if Living.Live(Next_Step, F - Full_Effect'First)
               and then (not Joinable (Full_Effect(F)))
               then
                  -- The Next step cannot be joined, so we stop here.

                  if Opt.Trace_Joining then

                     Output.Trace (
                          "Cannot join "
                        & Arithmetic.Image (Full_Effect(F))
                        & " from step"
                        & Flow.Index_Image (Steps(Next .. Next))
                        & " into "
                        & Arithmetic.Image (Into));

                  end if;

                  exit Join_Loop;

               end if;

            end loop;

         end if;

         -- Yes we can join the Next step Into the growing set.
         -- Collect the live assignments on the way, into the
         -- joint effect:

         for F in Full_Effect'Range loop

            if Living.Live(Next_Step, F - Full_Effect'First) then

               Join (Full_Effect(F));

            end if;

         end loop;

         -- We have joined the Next step:

         Last := Next;

         exit Join_Loop when Last = Steps'Last;

         Next := Next + 1;

      end loop Join_Loop;

      -- We will join Steps(Steps'First .. Last).
      -- The "live" assignments for these steps are now in the Into
      -- set, including both Defining and Range constraint assignments.

      -- Add opaque assignments for all "dead" target cells that are
      -- not masked by a "live" assignment to the same cell:

      for S in Steps'First .. Last loop

         Full_Effect := Computation.Effect (Steps(S), Living.Model.all);

         for F in Full_Effect'Range loop

            if not Living.Live(Next_Step, F - Full_Effect'First) then

               Kill (Full_Effect(F));

            end if;

         end loop;

      end loop;

      -- Perhaps trace:

      if (Opt.Trace_Joint_Effect or Opt.Trace_Joining)
      and Last > Steps'First
      then

         Output.Trace (
              "Joined steps"
            & Flow.Index_Image (Steps(Steps'First .. Last)));

         if Arithmetic.Length (Into) > 0 then

            Output.Trace (
                 "Joint effect is "
               & Arithmetic.Image (Arithmetic.To_Effect (Into)));

         else

            Output.Trace ("Joint effect is null.");

         end if;

      end if;

   end Join_Effects;


   procedure Add_Cells_Accessed (
      By     : in     Node_T;
      Living : in     Living_T;
      To     : in out Storage.Cell_Set_T)
   is

      Steps : constant Step_List_T := Steps_In (By);
      -- All the steps in the node.

   begin

      -- Add the cells accessed in the "live" effect of this node:

      for S in Steps'Range loop

         declare

            Effect : constant Arithmetic.Effect_T :=
               Live_Effect (Steps(S), Living);
            -- The live assignments in the step.

         begin

            Arithmetic.Add_Cells_Used (
               By   => Effect,
               Refs => Living.For_Data /= Arithmetic.Opt.None,
               Post => True,
               To   => To);

            Arithmetic.Add_Cells_Defined (
               By => Effect,
               To => To);

         end;

      end loop;

      -- Add the basis cells of any dynamic edges sourced in this node:

      Add_Dynamic_Edge_Basis_Cells (
        From  => By,
        Graph => Graph (Living),
        To    => To);

   end Add_Cells_Accessed;


   function Dynamic_Values (
      Step   : Step_T;
      Living : Living_T)
   return Boolean
   --
   -- Whether this Step has some live assignment where the assigned
   -- value expressions, or the assignment condition expression, uses
   -- a dynamically addressed memory reference. Dynamic memory
   -- references in target variables are not considered.
   --
   is

      S : constant Step_Index_T := Index (Step);
      -- The index of the Step.

      Full : constant Arithmetic.Effect_Ref :=
         Computation.Effect (Step, Living.Model.all);
      -- The full effect of the Step under the Model for Living.

   begin

      for F in Full'Range loop

         if Living.Live(S, F - Full'First)
         and then Arithmetic.Dynamic_Values (Full(F))
         then

            return True;

         end if;

      end loop;

      return False;

   end Dynamic_Values;


   function Steps_With_Dynamic_Effect (Living : Living_T)
   return Step_List_T
   is

      Model : Computation.Model_Ref renames Living.Model.all;
      -- The computation model on which the Living structure is based.

      Graph : constant Graph_T := Computation.Graph (Model);
      -- The underlying flow-graph.

      Step : Step_T;
      -- One of the steps in the Graph.

      Steps : Step_List_T (1 .. Natural (Max_Step (Graph)));
      -- Collects the result.

      Last : Natural := 0;
      -- Steps(1 .. Last) is the result.

   begin

      for S in 1 .. Max_Step (Graph) loop

         Step := Step_At (Index => S, Within => Graph);

         if Computation.Is_Feasible (Step, Model)
         and then Dynamic_Values (Step, Living)
         then
            -- This Step uses some dynamically addressed memory values.

            Last := Last + 1;

            Steps(Last) := Step;

         end if;

      end loop;

      return Steps(1 .. Last);

   end Steps_With_Dynamic_Effect;


   procedure Deallocate
   is new Ada.Unchecked_Deallocation (
      Object => Living_Object_T,
      Name   => Living_T);


   procedure Discard (Living : in out Living_T)
   is
   begin

      if Opt.Deallocate then

         Deallocate (Living);

      else

         Living := null;

      end if;

   exception when others => Faults.Deallocation;

   end Discard;


end Flow.Life;
