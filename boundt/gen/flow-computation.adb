-- Flow.Computation (body)
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
-- $Revision: 1.24 $
-- $Date: 2015/10/24 20:05:47 $
--
-- $Log: flow-computation.adb,v $
-- Revision 1.24  2015/10/24 20:05:47  niklas
-- Moved to free licence.
--
-- Revision 1.23  2011-09-06 17:50:00  niklas
-- Added Is_Final (Node_T, ..) for help with ALF export.
--
-- Revision 1.22  2009-10-07 19:26:10  niklas
-- BT-CH-0183: Cell-sets are a tagged-type class.
--
-- Revision 1.21  2009-05-15 12:16:47  niklas
-- BT-CH-0173: Handling cells used by Range_Post constraints.
--
-- Revision 1.20  2008/04/22 12:40:41  niklas
-- Added Mark_Infeasible for a node edge.
--
-- Revision 1.19  2008/02/18 12:58:27  niklas
-- Added functions Final_Nodes and Successors (return Node_List_T)
-- for use in SPARC RW-trap analysis.
--
-- Revision 1.18  2007/12/17 13:54:35  niklas
-- BT-CH-0098: Assertions on stack usage and final stack height, etc.
--
-- Revision 1.17  2007/10/28 09:32:45  niklas
-- BT-CH-0092: Arithmetic analysis of dynamic data refs is optional.
--
-- Revision 1.16  2007/10/26 12:44:34  niklas
-- BT-CH-0091: Reanalyse a boundable edge only if its domain grows.
--
-- Revision 1.15  2007/10/02 20:49:22  niklas
-- Added Unresolved_Dynamic_Edges_From (Step, Model).
--
-- Revision 1.14  2007/08/06 09:29:57  niklas
-- Added Model_T.Index to identify model objects. Added Next_Index and
-- Set_Index to keep count and extended Refer_To_Primitive_Model and
-- Ensure_Single_Ref to Set the Index of the new model object.
-- Implemented Opt.Trace_Models by extending Discard, Refer, Changed,
-- Mark_Clean, Ensure_Single_Ref and Set_Effect to trace their actions.
-- Added functions Spick and Ident_Ref to show model identity and state.
--
-- Revision 1.13  2007/03/29 15:18:01  niklas
-- BT-CH-0056.
--
-- Revision 1.12  2007/01/25 21:25:14  niklas
-- BT-CH-0043.
--
-- Revision 1.11  2007/01/13 13:51:03  niklas
-- BT-CH-0041.
--
-- Revision 1.10  2006/11/26 22:07:25  niklas
-- BT-CH-0039.
--
-- Revision 1.9  2006/10/24 08:44:30  niklas
-- BT-CH-0028.
--
-- Revision 1.8  2005/09/20 09:50:44  niklas
-- Added Mark_Infeasible (Loop).
--
-- Revision 1.7  2005/09/12 19:02:58  niklas
-- BT-CH-0008.
--
-- Revision 1.6  2005/09/03 11:50:28  niklas
-- BT-CH-0006.
--
-- Revision 1.5  2005/06/29 19:35:55  niklas
-- Added Exit_Edges.
--
-- Revision 1.4  2005/05/09 15:28:16  niklas
-- Added the functions Feasible (Steps), Number_Into (Step), Final_Steps,
-- Successors (Step) and Total_Defining_Assignments, all to support
-- value-origin analysis.
--
-- Revision 1.3  2005/03/20 15:12:06  niklas
-- Corrected Steps_With_Dynamic_Effect to return only feasible steps.
--
-- Revision 1.2  2005/02/23 09:05:17  niklas
-- BT-CH-0005.
--
-- Revision 1.1  2005/02/16 21:11:43  niklas
-- BT-CH-0002.
--


with Ada.Unchecked_Deallocation;
with Faults;
with Flow.Calls;
with Flow.Computation.Opt;
with Flow.Pruning;
with Flow.Pruning.Opt;
with Output;
with Storage.Bitvec_Cell_Sets;


package body Flow.Computation is


   Next_Index : Model_Index_T := Model_Index_T'First;
   --
   -- The index for the next Model_T object to be allocated from
   -- the heap.


   procedure Set_Index (Model : in out Model_T)
   --
   -- Gives the Model its unique Index.
   --
   is
   begin

      Model.Index := Next_Index;

      Next_Index := Next_Index + 1;

   end Set_Index;


   procedure Unchecked_Deallocate
   is new Ada.Unchecked_Deallocation (
      Name   => Model_Access_T,
      Object => Model_T);
   --
   -- Deallocates a heap-allocated computation model.


   procedure Deallocate (Item : in out Model_Access_T)
   --
   -- Deallocation controlled by Opt.Deallocate.
   --
   is
   begin

      if Opt.Deallocate then

         Unchecked_Deallocate (Item);

      else

         Item := null;

      end if;

   exception when others => Faults.Deallocation;

   end Deallocate;


   function Model (Ref : Model_Ref) return Model_T
   is
   begin

      return Ref.Ref.all;

   end Model;


   function Is_Null (Model : Model_Ref) return Boolean
   is
   begin

      return Model.Ref = null;

   end Is_Null;


   function Spick (Item : Model_Ref) return String
   --
   -- Says whether a model is clean or dirty.
   --
   is
   begin

      if Item.Ref.Clean then return "clean";
                        else return "dirty";
      end if;

   end Spick;


   function Ident_Ref (Item : Model_Ref) return String
   --
   -- A description of the identity of the referenced model, for
   -- tracing purposes.
   --
   is
   begin

      if Item.Ref = null then

         return "null model";

      else

         return "computation model #"
            & Model_Index_T'Image (Item.Ref.Index)
            & " ("
            & Output.Image (Item.Ref.References)
            & " refs, "
            & Spick (Item)
            & ')';

      end if;

   end Ident_Ref;


   procedure Discard (Model : in out Model_Ref)
   is
   begin

      if Opt.Trace_Models then

         Output.Trace ("Discarding ref to " & Ident_Ref (Model));

      end if;

      if Model.Ref /= null then

         Model.Ref.References := Model.Ref.References - 1;

         if Model.Ref.References = 0 then

            Deallocate (Model.Ref);

         end if;

         Model.Ref := null;

      end if;

   exception when others => Faults.Deallocation;

   end Discard;


   procedure Refer (
      From : in out Model_Ref;
      To   : in     Model_Ref)
   is
   begin

      if Opt.Trace_Models then

         Output.Trace (
              "Referring to "
            & Ident_Ref (To)
            & ", was "
            & Ident_Ref (From));

      end if;

      if From.Ref /= To.Ref then
         -- A real change in references, and no risk of
         -- killing To.Ref.all by Discarding From.

         Discard (From);

         To.Ref.References := To.Ref.References + 1;

         From.Ref := To.Ref;

      end if;

   end Refer;


   procedure Fill_Primitive_Model (
      Subprogram : in     Programs.Subprogram_T;
      Graph      : in     Graph_T;
      Model      :    out Model_T)
   --
   -- Fills the Model with the primitive computation model built
   -- into the Graph for the Subprogram.
   --
   is

      Calls : constant Programs.Call_List_T :=
         Programs.Calls_From (Subprogram);
      -- All the calls in this Subprogram, perhaps including calls of
      -- "unused" callees.

      Call : Programs.Call_T;
      -- One of the Calls.


      procedure Fill_Infeasible (Edges : Step_Edge_List_T)
      --
      -- Sets these edges to be infeasible. (We do not use the
      -- procedure Mark_Infeasible because we do not want to mess
      -- with copy-on-change at this point.)
      --
      is
      begin

         for E in Edges'Range loop

            Model.Condition(Index (Edges(E))) := Arithmetic.Never;

         end loop;

      end Fill_Infeasible;


   begin  -- Fill_Primitive_Model

      Model.References := 0;

      Model.Clean := True;
      -- This may be changed below if we find some feasible
      -- calls, because the protocols of these calls are
      -- considered to be changed.

      Model.Subprogram := Subprogram;
      Model.Graph      := Graph;

      Model.Pruned := True;
      -- This may be changed below if we find some infeasible
      -- parts in the graph.

      -- Initially all steps are feasible (except for calls
      -- to Unused subprograms, see later):

      for S in Model.Effect'Range loop

         Model.Effect(S) := Effect (Step_At (S, Graph));

         Model.Feasible(S) := True;
         -- May be reset below for unused calls.

      end loop;

      -- Initially all edges have the decoded condition:

      for E in Model.Condition'Range loop

         Model.Condition(E) := Condition (Edge_At (E, Graph));

      end loop;

      -- Set the initial properties of the calls:

      for C in Calls'Range loop

         Call := Calls(C);

         Model.Protocol(C) := Programs.Protocol (Call);

         Model.New_Proto(C) := True;
         -- This signals later analysis phases that the effect
         -- of this Call must be computed before any analysis
         -- of the data-flow in this Model.

         if Programs.Unused (Call) then
            -- Calls to unused subprograms are infeasible.

            Model.Feasible(Index (Programs.Step (Call))) := False;

            Model.Pruned := False;

         else

            Model.Clean := False;
            -- Because the protocol of this feasible Call was changed.

         end if;

         Model.Returns(C) := Programs.Returns (Programs.Callee (Call));

         if not Model.Returns(C) then
            -- All edges leaving non-returning calls are infeasible.

            Fill_Infeasible (Flow.Edges_From (Programs.Step (Call), Graph));

            Model.Pruned := False;

         end if;

      end loop;

   end Fill_Primitive_Model;


   function Primitive_Model (Subprogram : Programs.Subprogram_T)
   return Model_T
   is

      Graph : constant Graph_T := Programs.Flow_Graph (Subprogram);
      -- The flow-graph of the Subprogram, for the model.

      Model : Model_T (
         Steps => Max_Step (Graph),
         Edges => Max_Step_Edge (Graph),
         Calls => Programs.Number_Of_Calls_From (Subprogram));
      -- The model to be built.

   begin

      Fill_Primitive_Model (Subprogram, Graph, Model);

      return Model;

   end Primitive_Model;
         

   procedure Refer_To_Primitive_Model (
      Subprogram : in     Programs.Subprogram_T;
      Model      : in out Model_Ref)
   is

      Graph : constant Graph_T := Programs.Flow_Graph (Subprogram);
      -- The flow-graph of the Subprogram, for the model.

   begin

      Discard (Model);

      Model.Ref := new Model_T (
         Steps => Max_Step (Graph),
         Edges => Max_Step_Edge (Graph),
         Calls => Programs.Number_Of_Calls_From (Subprogram));
      -- The model to be built.

      Set_Index (Model.Ref.all);

      Fill_Primitive_Model (Subprogram, Graph, Model.Ref.all);

      Model.Ref.References := 1;
      -- There is one reference, Model.

      if Opt.Trace_Models then

         Output.Trace ("Created primitive " & Ident_Ref (Model));

      end if;

   end Refer_To_Primitive_Model;


   function Changed (Model : in Model_Ref) return Boolean
   is
   begin

      return not Model.Ref.Clean;

   end Changed;


   procedure Mark_Clean (Model : in Model_Ref)
   is
   begin

      if Opt.Trace_Models then

         Output.Trace ("Marking as clean " & Ident_Ref (Model));

      end if;

      for N in Model.Ref.New_Proto'Range loop

         Model.Ref.New_Proto(N) := False;

      end loop;

      Model.Ref.Clean := True;

   end Mark_Clean;


   function Subprogram (Model : Model_Ref) return Programs.Subprogram_T
   is
   begin

      return Model.Ref.Subprogram;

   end Subprogram;


   function Program (Model : Model_Ref) return Programs.Program_T
   is
   begin

      return Programs.Program (Model.Ref.Subprogram);

   end Program;


   function Graph (Under : Model_Ref) return Graph_T
   is
   begin

      return Under.Ref.Graph;

   end Graph;


   function Max_Step (Within : Model_Ref) return Step_Index_T
   is
   begin

      return Max_Step (Graph (Within));

   end Max_Step;


   function Effect (Step : Step_Index_T; Under : Model_Ref)
   return Arithmetic.Effect_Ref
   is
   begin

      return Under.Ref.Effect(Step);

   end Effect;


   function Effect (Step : Step_T; Under : Model_Ref)
   return Arithmetic.Effect_Ref
   is
   begin

      return Under.Ref.Effect(Index (Step));

   end Effect;


   function Effect (Step : Step_T; Under : Model_Ref)
   return Arithmetic.Effect_T
   is
   begin

      return Under.Ref.Effect(Index (Step)).all;

   end Effect;


   function Condition (Edge : Step_Edge_T; Under : Model_Ref)
   return Arithmetic.Condition_T
   is
   begin

      return Under.Ref.Condition(Index (Edge));

   end Condition;


   function Condition (Edge : Edge_T; Under : Model_Ref)
   return Arithmetic.Condition_T
   is
   begin

      return Condition (Step_Edge(Edge), Under);

   end Condition;


   function Is_Feasible (Model : Model_Ref) return Boolean
   is
   begin

      return Is_Feasible (Entry_Step (Graph (Model)), Model);

   end Is_Feasible;


   function Is_Feasible (Step : Step_Index_T; Under : Model_Ref)
   return Boolean
   is
   begin

      return Under.Ref.Feasible(Step);

   end Is_Feasible;


   function Is_Feasible (Step : Step_T; Under : Model_Ref)
   return Boolean
   is
   begin

      return Under.Ref.Feasible(Index (Step));

   end Is_Feasible;


   function Is_Feasible (Edge : Step_Edge_Index_T; Under : Model_Ref)
   return Boolean
   is
      use type Arithmetic.Condition_T;
   begin

      return Under.Ref.Condition(Edge) /= Arithmetic.Never;

   end Is_Feasible;


   function Is_Feasible (Edge : Step_Edge_T; Under : Model_Ref)
   return Boolean
   is
      use type Arithmetic.Condition_T;
   begin

      return Under.Ref.Condition(Index (Edge)) /= Arithmetic.Never;

   end Is_Feasible;


   function Is_Feasible (Node : Node_Index_T; Under : Model_Ref)
   return Boolean
   is
   begin

      return Is_Feasible (
         Step  => First_Step (Node_At (Node, Graph (Under))),
         Under => Under);

   end Is_Feasible;


   function Is_Feasible (Node : Node_T; Under : Model_Ref)
   return Boolean
   is
   begin

      return Is_Feasible (First_Step (Node), Under);

   end Is_Feasible;


   function Is_Feasible (Edge : Edge_Index_T; Under : Model_Ref)
   return Boolean
   is
   begin

      return Is_Feasible (
         Edge  => Step_Edge (Edge_At (Edge, Graph (Under))),
         Under => Under);

   end Is_Feasible;


   function Is_Feasible (Edge : Edge_T; Under : Model_Ref)
   return Boolean
   is
   begin

      return Is_Feasible (Step_Edge (Edge), Under);

   end Is_Feasible;


   function Some_Feasible (Edges : Edge_List_T; Under : Model_Ref)
   return Boolean
   is
   begin

      for E in Edges'Range loop

         if Is_Feasible (Edges(E), Under) then

            return True;

        end if;

      end loop;

      return False;

   end Some_Feasible;


   function Is_Feasible (Call : Programs.Call_T; Under : Model_Ref)
   return Boolean
   is
   begin

      return
         Is_Feasible (Programs.Step (Call), Under);
      -- and Programs.Is_Feasible (Programs.Callee (Call));  -- TBA

   end Is_Feasible;


   function Is_Feasible (Luup : Loops.Loop_T; Under : Model_Ref)
   return Boolean
   is
   begin

      return Is_Feasible (
         Step  => First_Step (Loops.Head_Node (Luup)),
         Under => Under);

   end Is_Feasible;


   function Is_Eternal (Luup : Loops.Loop_T; Under : Model_Ref)
   return Boolean
   is
   begin

      if Loops.Eternal (Luup) then
         -- The loop has no exit edges at all.

         return True;

      else
         -- The loop has some exit edges, but is eternal if none
         -- of the exit edges are feasible.

         return not Some_Feasible (
            Edges => Loops.Exit_Edges (Luup, Graph (Under)),
            Under => Under);

      end if;

   end Is_Eternal;


   function Is_Repeatable (Luup : Loops.Loop_T; Under : Model_Ref)
   return Boolean
   is
   begin

      return Some_Feasible (
         Edges => Loops.Repeat_Edges (Luup, Graph (Under)),
         Under => Under);

   end Is_Repeatable;


   function Target_Range (Step : Step_T; Under : Model_Ref)
   return Storage.Alias_Range_T
   is
   begin

      return Arithmetic.Target_Range (Under.Ref.Effect(Index (Step)).all);

   end Target_Range;


   function Calling_Protocol (
      Call_Index : Programs.Call_Index_T;
      Under      : Model_Ref)
   return Calling.Protocol_Ref
   is
   begin

      return Under.Ref.Protocol(Call_Index);

   end Calling_Protocol;     


   function Calling_Protocol (
      Call  : Programs.Call_T;
      Under : Model_Ref)
   return Calling.Protocol_Ref
   is
   begin

      return Under.Ref.Protocol(Programs.Index (Call));

   end Calling_Protocol;


   function Calling_Protocol_Is_Static (
      Call  : Programs.Call_T;
      Under : Model_Ref)
   return Boolean
   is
   begin

      return Calling.Static (Calling_Protocol (Call, Under).all);

   end Calling_Protocol_Is_Static;


   function Calling_Protocol_Changed (
      Call  : Programs.Call_T;
      Under : Model_Ref)
   return Boolean
   is
   begin

      return Under.Ref.New_Proto(Programs.Index (Call));

   end Calling_Protocol_Changed;


   function Returns (
      Call  : Programs.Call_T;
      Under : Model_Ref)
   return Boolean
   is
   begin

      return Under.Ref.Returns(Programs.Index (Call));

   end Returns;


   --
   --    Properties of the feasible parts of a model
   --


   function Feasible (Steps : Step_List_T; Under : Model_Ref)
   return Step_List_T
   is

      Result : Step_List_T (1 .. Steps'Length);
      Last   : Natural := 0;
      -- The result will be Result(1 .. Last).

   begin

      for S in Steps'Range loop

         if Is_Feasible (Steps(S), Under) then

            Last         := Last + 1;
            Result(Last) := Steps(S);

          end if;

      end loop;

      return Result(1 .. Last);

   end Feasible;


   function Edges_Into (Step : Step_T; Under : Model_Ref)
   return Step_Edge_List_T
   is

      Candidates : Step_Edge_List_T := Edges_Into (Step, Under.Ref.Graph);
      -- All the edges into the Step, whether feasible or not.

      Last : Natural := Candidates'First - 1;
      -- The final result will be Candidates(Candidates'First .. Last).

   begin

      for C in Candidates'Range loop

         if Is_Feasible (Candidates(C), Under) then

            Last := Last + 1;

            if Last < C then

               Candidates(Last) := Candidates(C);

            -- else Candidates(C) is already in place.

            end if;

         end if;

      end loop;

      return Candidates(Candidates'First .. Last);

   end Edges_Into;


   function Edges_From (Step : Step_T; Under : Model_Ref)
   return Step_Edge_List_T
   is

      Candidates : Step_Edge_List_T := Edges_From (Step, Under.Ref.Graph);
      -- All the edges from the Step, whether feasible or not.

      Last : Natural := Candidates'First - 1;
      -- The final result will be Candidates(Candidates'First .. Last).

   begin

      for C in Candidates'Range loop

         if Is_Feasible (Candidates(C), Under) then

            Last := Last + 1;

            if Last < C then

               Candidates(Last) := Candidates(C);

            -- else Candidates(C) is already in place.

            end if;

         end if;

      end loop;

      return Candidates(Candidates'First .. Last);

   end Edges_From;


   function Number_Into (Step : Step_T; Under : Model_Ref)
   return Natural
   is

      Candidates : constant Step_Edge_List_T :=
         Edges_Into (Step, Under.Ref.Graph);
      -- All the edges into the Step, whether feasible or not.

      Number : Natural := 0;
      -- The number of feasible Candidates.

   begin

      for C in Candidates'Range loop

         if Is_Feasible (Candidates(C), Under) then

            Number := Number + 1;

         end if;

      end loop;

      return Number;

   end Number_Into;


   function Is_Final (Step : Step_T; Under : Model_Ref)
   return Boolean
   is

      Candidates : Step_Edge_List_T := Edges_From (Step, Under.Ref.Graph);
      -- All the edges from the Step, whether feasible or not.

   begin

      for C in Candidates'Range loop

         if Is_Feasible (Candidates(C), Under) then
            -- This is not a final step.

            return False;

         end if;

      end loop;

      -- No feasible Candidates. The Step is final.

      return True;

   end Is_Final;


   function Final_Steps (Under : Model_Ref)
   return Step_List_T
   is
   begin

      return Feasible (
         Steps => Return_Steps (Graph (Under)),
         Under => Under);

   end Final_Steps;


   function Is_Final (Node : Node_T; Under : Model_Ref)
   return Boolean
   is
   begin

      return Number_From (Node, Under) = 0;

   end Is_Final;


   function Final_Nodes (Under : Model_Ref)
   return Node_List_T
   is
   begin

      return Nodes_Containing (
         Steps => Final_Steps (Under),
         Graph => Graph (Under));

   end Final_Nodes;


   function Returns (Under : Model_Ref) return Boolean
   is
   begin

      return Final_Steps (Under)'Length > 0;

   end Returns;


   function Non_Returning_Call_Steps (Under : Model_Ref)
   return Step_List_T
   is

      Steps : Step_List_T (1 .. Under.Ref.Calls);
      Last  : Natural := 0;
      -- The result will be Steps(1 .. Last).

      Step : Step_T;
      -- One of the call steps.

   begin

      for R in Under.Ref.Returns'Range loop

         if not Under.Ref.Returns(R) then
            -- This is a non-returning call.

            Step := Programs.Step (Programs.Call (
                From  => Under.Ref.Subprogram,
                Index => R));

            if Is_Feasible (Step, Under) then

               Last        := Last + 1;
               Steps(Last) := Step;

            end if;

         end if;

      end loop;

      return Steps(1 .. Last);

   end Non_Returning_Call_Steps;


   function Predecessors (Step : Step_T; Under : Model_Ref)
   return Step_List_T
   is
   begin

      return Sources (Edges_Into (Step, Under));

   end Predecessors;


   function Successors (Step : Step_T; Under : Model_Ref)
   return Step_List_T
   is
   begin

      return Targets (Edges_From (Step, Under));

   end Successors;


   function Edges_Into (Node : Node_T; Under : Model_Ref)
   return Edge_List_T
   is
   begin

      return Feasible (
         Edges => Edges_Into (Node, Graph (Under)),
         Under => Under);

   end Edges_Into;


   function Number_From (Node : Node_T; Under : Model_Ref)
   return Natural
   is
   begin

      return Edges_From (Node, Under)'Length;

   end Number_From;


   function Edges_From (Node : Node_T; Under : Model_Ref)
   return Edge_List_T
   is
   begin

      return Feasible (
         Edges => Edges_From (Node, Graph (Under)),
         Under => Under);

   end Edges_From;


   function Successors (Node : Node_T; Under : Model_Ref)
   return Node_List_T
   is
   begin

      return Targets (Edges_From (Node, Under));

   end Successors;


   function Is_Feasible (Edge : Dynamic_Edge_T; Under : Model_Ref)
   return Boolean
   is
   begin

      return Is_Feasible (Source (Edge.all), Under);

   end Is_Feasible;


   function Dynamic_Edges (Under : Model_Ref)
   return Dynamic_Edge_List_T
   is

      Cands  : constant Dynamic_Edge_List_T := Dynamic_Edges (Under.Ref.Graph);
      -- All the dynamic edges, feasible or not.

      Result : Dynamic_Edge_List_T (1 .. Cands'Length);
      Last   : Natural := 0;
      -- The result will be Result(1 .. Last).

   begin

      for C in Cands'Range loop

         if Is_Feasible (Cands(C), Under) then
            -- Good one.

            Last := Last + 1;

            Result(Last) := Cands(C);

         end if;

      end loop;

      return Result(1 .. Last);

   end Dynamic_Edges;


   function Unstable_Dynamic_Edges (Under : Model_Ref)
   return Dynamic_Edge_List_T
   is

      Cands : constant Dynamic_Edge_List_T := Dynamic_Edges (Under);
      -- All the feasible dynamic edges.

      Result : Dynamic_Edge_List_T (1 .. Cands'Length);
      Last   : Natural := 0;
      -- The result will be Result(1 .. Last).

   begin

      for C in Cands'Range loop

         if Cands(C).Unstable then

            Last := Last + 1;

            Result(Last) := Cands(C);

         end if;

      end loop;

      return Result(1 .. Last);

   end Unstable_Dynamic_Edges;


   function Unstable_Dynamic_Edges_From (
      Source : Step_T;
      Under  : Model_Ref)
   return Dynamic_Edge_List_T
   is

      Cands : constant Dynamic_Edge_List_T := Unstable_Dynamic_Edges (Under);
      -- All the feasible unstable dynamic edges.

      Result : Dynamic_Edge_List_T (1 .. Cands'Length);
      Last   : Natural := 0;
      -- The result will be Result(1 .. Last).

   begin

      for C in Cands'Range loop

         if Flow.Source (Cands(C).all) = Source then
            -- An unstable edge from this Source.

            Last := Last + 1;

            Result(Last) := Cands(C);

         end if;

      end loop;

      return Result(1 .. Last);

   end Unstable_Dynamic_Edges_From;


   function Dynamic_Flow (Under : Model_Ref) return Edge_Resolution_T
   is

      Edges : constant Dynamic_Edge_List_T := Dynamic_Edges (Under);
      -- The feasible dynamic edges.

      Overall_State : Edge_Resolution_T := Stable;
      -- The result state.

   begin

      for E in Edges'Range loop

         Overall_State := Edge_Resolution_T'Min (
            Overall_State,
            State (Edges(E).all));

      end loop;

      return Overall_State;

   end Dynamic_Flow;


   function Loops_Of (Model : Model_Ref)
   return Loops.Loop_List_T
   is

      Luups : constant Loops.Loops_T :=
         Programs.Loops_Of (Subprogram (Model));
      -- All the loops in the flow-graph, without regard to the Model.

      Result : Loops.Loop_List_T (1 .. Luups'Length);
      Last   : Natural := 0;
      -- The result will be Result(1 .. Last).

      Luup : Loops.Loop_T;
      -- One of the Loops.

      Index : Loops.Loop_Index_T;
      -- The index of the Luup.

      Feasible, Eternal, Repeatable : Boolean;
      -- Properties of the Luup under the Model.

   begin

      for L in Luups'Range loop

         Luup  := Luups(L);
         Index := Loops.Loop_Index (Luup);

         Feasible   := Is_Feasible   (Luup, Model);
         Eternal    := Is_Eternal    (Luup, Model);
         Repeatable := Is_Repeatable (Luup, Model);

         -- Remark on interesting changes:

         if not Feasible then

            Output.Note (
                 "Loop"
               & Loops.Loop_Index_T'Image (Index)
               & " is not feasibly enterable.");

         else
            -- The loop is at least feasible.

            if not Repeatable then

               Output.Note (
                    "Loop"
                  & Loops.Loop_Index_T'Image (Index)
                  & " is not feasibly repeatable.");

            end if;

            if  Eternal
            and not Loops.Eternal (Luup)
            then

               Output.Note (
                    "Loop"
                  & Loops.Loop_Index_T'Image (Index)
                  & " is not feasibly exitable.");

            end if;

         end if;

         -- Selection:

         if Feasible and Repeatable then
            -- This loop is still relevant under the Model.

            if Eternal then
               -- Under the Model, this loop is eternal.

               Loops.Mark_As_Eternal (Luup);

            end if;

            Last         := Last + 1;
            Result(Last) := Luup;

         end if;

      end loop;

      return Result(1 .. Last);

   end Loops_Of;


   function Feasible (Edges : Edge_List_T; Under : Model_Ref)
   return Edge_List_T
   is

      Result : Edge_List_T (1 .. Edges'Length);
      Last   : Natural := 0;
      -- The result will be Result(1 .. Last).

   begin

      for E in Edges'Range loop

         if Is_Feasible (Edges(E), Under) then

            Last         := Last + 1;
            Result(Last) := Edges(E);

          end if;

      end loop;

      return Result(1 .. Last);

   end Feasible;


   function Entry_Edges (
      Luup  : Loops.Loop_T;
      Under : Model_Ref)
   return Flow.Edge_List_T
   is
   begin

      return Feasible (
         Edges => Loops.Entry_Edges (Luup, Graph (Under)),
         Under => Under);

   end Entry_Edges;


   function Neck_Edges (
      Luup  : Loops.Loop_T;
      Under : Model_Ref)
   return Flow.Edge_List_T
   is
   begin

      return Feasible (
         Edges => Loops.Neck_Edges (Luup, Graph (Under)),
         Under => Under);

   end Neck_Edges;


   function Repeat_Edges (
      Luup  : Loops.Loop_T;
      Under : Model_Ref)
   return Flow.Edge_List_T
   is
   begin

      return Feasible (
         Edges => Loops.Repeat_Edges (Luup, Graph (Under)),
         Under => Under);

   end Repeat_Edges;


   function Exit_Edges (
      Luup  : Loops.Loop_T;
      Under : Model_Ref)
   return Flow.Edge_List_T
   is
   begin

      return Feasible (
         Edges => Loops.Exit_Edges (Luup, Graph (Under)),
         Under => Under);

   end Exit_Edges;


   function Cells_In (
      Model : Model_Ref;
      Calls : Boolean)
   return Storage.Cell_Set_T
   is
      use type Arithmetic.Condition_T;

      subtype Cell_Set_T is Storage.Bitvec_Cell_Sets.Set_T;
      -- The kind of cell-sets we use in this operation.

      Mo : Model_T renames Model.Ref.all;

      Cells : Cell_Set_T;
      -- The set to be built up, initially empty.

      Dyn_Edges : constant Dynamic_Edge_List_T := Dynamic_Edges (Model);
      -- All the feasible dynamic edges.

   begin

      -- Include all cells named in the effects of steps, S:

      for S in Mo.Effect'Range loop

         if Mo.Feasible(S)
         and then (
            Calls
            or else not Flow.Calls.Is_Call (Step_At (S, Mo.Graph)))
         then
            -- The effect of this step shall be included.

            Arithmetic.Add_Cells_Used (
               By   => Mo.Effect(S).all,
               Refs => True,
               Post => True,
               To   => Cells);

            Arithmetic.Add_Cells_Defined (
               By => Mo.Effect(S).all,
               To => Cells);

         end if;

      end loop;

      -- Include all cells used in the conditions of edges, E:

      for E in Mo.Condition'Range loop

         if Mo.Condition(E) /= Arithmetic.Never then

            Arithmetic.Add_Cells_Used (
               By   => Mo.Condition(E),
               Refs => True,
               To   => Cells);

         end if;

      end loop;

      -- Include all basis cells for dynamic boundable edges, D:

      for D in Dyn_Edges'Range loop

         if Is_Feasible (Dyn_Edges(D), Model) then

            Add_Basis_Cells (
               From => Dyn_Edges(D).all,
               To   => Cells);

         end if;

      end loop;

      -- Include all basis cells for protocols in calls, C:

      for C in Mo.Protocol'Range loop

         if Is_Feasible (Programs.Call (Mo.Subprogram, C), Model) then

            Calling.Add_Basis_Cells (
               From => Mo.Protocol(C).all,
               To   => Cells);

         end if;

      end loop;

      return Cells;

   end Cells_In;


   function Is_Defined (
      Cell  : Storage.Cell_T;
      By    : Step_T;
      Under : Model_Ref)
   return Boolean
   is
   begin

      return
         Under.Ref.Feasible(Index (By))
      and then
         Arithmetic.Is_Defined (
            Cell => Cell,
            By   => Under.Ref.Effect(Index (By)).all);

   end Is_Defined;


   function Cells_Defined (By : Model_Ref) return Storage.Cell_Set_T
   is

      subtype Cell_Set_T is Storage.Bitvec_Cell_Sets.Set_T;
      -- The kind of cell-set we use in this operation.

      Mo : Model_T renames By.Ref.all;

      Cells : Cell_Set_T;
      -- The set to be built up, initially empty.

   begin

      -- Include all cells defined in the effects of steps, S:

      for S in Mo.Effect'Range loop

         if Mo.Feasible(S) then

            Arithmetic.Add_Cells_Defined (
               By => Mo.Effect(S).all,
               To => Cells);

         end if;

      end loop;

      return Cells;

   end Cells_Defined;


   function Steps_Defining (Cell : Storage.Cell_T; Under : Model_Ref)
   return Step_List_T
   is

      Graph : constant Graph_T := Flow.Computation.Graph (Under);
      -- The flow-graph under this model.

      Steps : Step_List_T (1 .. Natural (Max_Step (Graph)));
      Num   : Natural := 0;
      -- Steps (1 .. Num) accumulates the steps to be returned.

      Step  : Step_T;
      -- A step being considered.

   begin

      -- Trivial implementation by scanning all the steps
      -- and collecting the ones which change the Cell:

      for I in 1 .. Max_Step (Graph) loop

         Step := Step_At (Index => I, Within => Graph);

         if Is_Feasible (Step, Under)
         and then
            Arithmetic.Is_Defined (Cell => Cell, By => Effect (Step, Under))
         then

           Num := Num + 1;
           Steps(Num) := Step;

         end if;

      end loop;

      return Steps(1 .. Num);

   end Steps_Defining;


   function Max_Effect_Length (Under : Computation.Model_Ref)
   return Natural
   is

       Max_Len : Natural := 0;
       -- The cumulative maximum length.

   begin

       for S in Under.Ref.Effect'Range loop

          if Under.Ref.Feasible(S) then

             Max_Len := Natural'Max (Max_Len, Under.Ref.Effect(S)'Length);

          end if;

       end loop;

       return Max_Len;

   end Max_Effect_Length;


   function Total_Defining_Assignments (Under : Model_Ref)
   return Natural
   is

       Total : Natural := 0;
       -- The cumulative total of defining assignments.

   begin

       for S in Under.Ref.Effect'Range loop

          if Under.Ref.Feasible(S) then

             Total := Total
                + Arithmetic.Defining_Assignments (Under.Ref.Effect(S).all);

          end if;

       end loop;

       return Total;

   end Total_Defining_Assignments;


   procedure Add_Cells_Used (
      By    : in     Step_Edge_List_T;
      Under : in     Model_Ref;
      To    : in out Storage.Cell_Set_T)
   --
   -- Adds the cells used By the preconditions of the given edges,
   -- Under a given computation model, To a set of cells.
   -- Basis cells for dynamic memory references are included.
   --
   is
   begin

      for B in By'Range loop

         Arithmetic.Add_Cells_Used (
            By   => Condition (By(B), Under),
            Refs => True,
            To   => To);

      end loop;

   end Add_Cells_Used;


   procedure Add_Cells_Used (
      By    : in     Step_List_T;
      Under : in     Model_Ref;
      To    : in out Storage.Cell_Set_T)
   --
   -- Adds the cells used By the given steps and edges leaving the
   -- steps, Under a given computation model, To a set of cells.
   -- Basis cells of boundable memory references are included.
   -- Basis cells of dynamic edges leaving the steps are included.
   -- Basis cells for calling protocols are omitted. TBA?
   --
   is
   begin

      for B in By'Range loop

         Arithmetic.Add_Cells_Used (
            By   => Effect (By(B), Under),
            Refs => True,
            Post => False,
            To   => To);

         Add_Cells_Used (
            By    => Flow.Edges_From (By(B), Graph (Under)),
            Under => Under,
            To    => To);

         Add_Basis_Cells (
            From => Dynamic_Edges_From (By(B), Graph (Under)),
            To   => To);

      end loop;

   end Add_Cells_Used;


   function Cells_Used (
      Nodes : Node_List_T;
      Under : Model_Ref)
   return Storage.Cell_List_T
   is

      package Cell_Sets renames Storage.Bitvec_Cell_Sets;
      -- The kind of cell-set we use in this operation.

      Used : Cell_Sets.Set_T;
      -- Collects the used cells, initially empty.

   begin

      for N in Nodes'Range loop

         Add_Cells_Used (
            By    => Steps_In (Nodes(N)),
            Under => Under,
            To    => Used);

      end loop;

      return Cell_Sets.To_List (Used);

   end Cells_Used;


   procedure Add_Cells_Defined (
      By    : in     Step_List_T;
      Under : in     Model_Ref;
      To    : in out Storage.Cell_Set_T)
   --
   -- Adds the cells defined By the given steps, Under a given
   -- computation model, To a set of cells.
   --
   is
   begin

      for B in By'Range loop

         Arithmetic.Add_Cells_Defined (
            By => Effect (By(B), Under),
            To => To);

      end loop;

   end Add_Cells_Defined;


   function Cells_Defined (
      Nodes : Node_List_T;
      Under : Model_Ref)
   return Storage.Cell_List_T
   is

      package Cell_Sets renames Storage.Bitvec_Cell_Sets;
      -- The kind of cell-set we use in this operation.

      Defined : Cell_Sets.Set_T;
      -- Collects the defined cells.

   begin

      for N in Nodes'Range loop

         Add_Cells_Defined (
            By    => Steps_In (Nodes(N)),
            Under => Under,
            To    => Defined);

      end loop;

      return Cell_Sets.To_List (Defined);

   end Cells_Defined;


   function Is_Used (
      Cell  : Storage.Cell_T;
      By    : Step_Edge_List_T;
      Under : Model_Ref)
   return Boolean
   --
   -- Whether the Cell is used By the precondition of some of the
   -- given edges, Under the given computation model.
   --
   is
   begin

      for B in By'Range loop

         if Arithmetic.Is_Used (
            Cell => Cell,
            By   => Condition (By(B), Under))
         then
            -- Yes, used in this edge.

            return True;

         end if;

      end loop;

      -- Not used in these edges.

      return False;

   end Is_Used;


   function Is_Used (
      Location : Storage.Location_T;
      By       : Step_T;
      Under    : Model_Ref)
   return Boolean
   --
   -- Whether the Location is used By the effect of the step or by a
   -- precondition on an edge leaving the step, Under a given
   -- computation model.
   --
   is

      Here : constant Processor.Code_Address_T := Prime_Address (By);
      -- The code address of the step, for mapping with the
      -- Location.

      Dynamic_Edges : constant Dynamic_Edge_List_T :=
         Dynamic_Edges_From (Source => By, Within => Graph (Under));
      -- The dynamic edges leaving the Step.

      Cell : Storage.Cell_T;
      -- One of the cells for the location.

   begin

      for L in Location'Range loop

         if Storage.In_Range (
            Address => Here,
            Rainge  => Location(L).Address)
         then
            -- The location maps Here to a cell.

            Cell := Location(L).Cell;

            -- Check the effect:

            if Arithmetic.Is_Used (
               Cell => Cell,
               By   => Effect (By, Under))
            then
               -- This cell is used in the effect.

               return True;

            end if;

            -- Check the leaving edges:

            if Is_Used (
               Cell  => Cell,
               By    => Flow.Edges_From (By, Graph (Under)),
               Under => Under)
            then
               -- This cell is used in a precondition on a
               -- leaving edge.

               return True;

            end if;

            -- Check the dynamic leaving edges:

            for D in Dynamic_Edges'Range loop

               if Storage.Is_Member (
                  Cell    => Cell,
                  Of_List => Basis (Dynamic_Edges(D).all))
               then
                  -- This cell is used in the basis of a dynamic edge.

                  return True;

               end if;

            end loop;

         end if;

      end loop;

      -- Location is not used:

      return False;

   end Is_Used;


   function Is_Used (
      Location : Storage.Location_T;
      By       : Step_List_T;
      Under    : Model_Ref)
   return Boolean
   --
   -- Whether the Location is used By some of the given steps or the
   -- edges leaving the steps, Under a given computation model.
   --
   is
   begin

      for B in By'Range loop

         if Is_Used (
            Location => Location,
            By       => By(B),
            Under    => Under)
         then
            -- Yes, used in this step.

            return True;

         end if;

      end loop;

      -- Not used in these steps.

      return False;

   end Is_Used;


   function Is_Used (
      Location : Storage.Location_T;
      By       : Node_List_T;
      Under    : Model_Ref)
   return Boolean
   is
   begin

      for B in By'Range loop

         if Is_Used (
            Location => Location, 
            By       => Steps_In (By(B)),
            Under    => Under)
         then
            -- Yes, used in this node.

            return True;

         end if;

      end loop;

      -- Not used in any of these nodes.

      return False;

   end Is_Used;


   function Is_Defined (
      Location : Storage.Location_T;
      By       : Step_T;
      Under    : Model_Ref)
   return Boolean
   --
   -- Whether the Location is defined By the effect of the step, Under
   -- a given computation model.
   --
   is

      Here : constant Processor.Code_Address_T := Prime_Address (By);
      -- The code address of the step, for mapping with the
      -- Location.

      Cell : Storage.Cell_T;
      -- One of the cells for the location.

   begin

      for L in Location'Range loop

         if Storage.In_Range (
            Address => Here,
            Rainge  => Location(L).Address)
         then
            -- The location maps Here to a cell.

            Cell := Location(L).Cell;

            -- Check the effect:

            if Arithmetic.Is_Defined (
               Cell => Cell,
               By   => Effect (By, Under))
            then
               -- This cell is defined in this step.

               return True;

            end if;

         end if;

      end loop;

      -- Location is not defined:

      return False;

   end Is_Defined;


   function Is_Defined (
      Location : Storage.Location_T;
      By       : Step_List_T;
      Under    : Model_Ref)
   return Boolean
   --
   -- Whether the Location is defined By some of the given steps,
   -- Under a given computation model.
   --
   is
   begin

      for B in By'Range loop

         if Is_Defined (
            Location => Location,
            By       => By(B),
            Under    => Under)
         then
            -- Yes, defined in this step.

            return True;

         end if;

      end loop;

      -- Not defined in these steps.

      return False;

   end Is_Defined;


   function Is_Defined (
      Location : Storage.Location_T;
      By       : Node_List_T;
      Under    : Model_Ref)
   return Boolean
   is
   begin

      for B in By'Range loop

         if Is_Defined (
            Location => Location, 
            By       => Steps_In (By(B)),
            Under    => Under)
         then
            -- Yes, defined in this node.

            return True;

         end if;

      end loop;

      -- Not defined in any of these nodes.

      return False;

   end Is_Defined;


   function Steps_With_Dynamic_Effect (Under  : Model_Ref)
   return Step_List_T
   is

      Effect : Effects_T renames Under.Ref.Effect;

      Feasible : Feasibility_T renames Under.Ref.Feasible;

      Steps : Step_List_T (1 .. Effect'Length);
      -- Collects the result.

      Last : Natural := 0;
      -- Steps(1 .. Last) is the result.

   begin

      for E in Effect'Range loop

         if       Feasible(E)
         and then Arithmetic.Dynamic (Effect(E).all)
         then

            Last := Last + 1;

            Steps(Last) := Step_At (
               Index  => E,
               Within => Under.Ref.Graph);

         end if;

      end loop;

      return Steps(1 .. Last);

   end Steps_With_Dynamic_Effect;


   function Edges_With_Dynamic_Condition (Under  : Model_Ref)
   return Step_Edge_List_T
   is

      Condition : Conditions_T renames Under.Ref.Condition;

      Edges: Step_Edge_List_T (1 .. Condition'Length);
      -- Collects the result.

      Last : Natural := 0;
      -- Edges(1 .. Last) is the result.

   begin

      for C in Condition'Range loop

         if Arithmetic.Dynamic (Condition(C)) then
            -- The condition has a dynamic reference. Also, the
            -- edge is feasible, because infeasible edges have a
            -- False condition which has no dynamic reference.

            Last := Last + 1;

            Edges(Last) := Edge_At (
               Index  => C,
               Within => Under.Ref.Graph);

         end if;

      end loop;

      return Edges(1 .. Last);

   end Edges_With_Dynamic_Condition;


   function Calls_From (Model : Model_Ref)
   return Programs.Call_List_T
   is

      Candidates : Programs.Call_List_T :=
         Programs.Calls_From (Subprogram (Model));
      -- All the calls from the subprogram, whether feasible or not.

      Last : Natural := Candidates'First - 1;
      -- The final result will be Candidates(Candidates'First .. Last).

   begin

      for C in Candidates'Range loop

         if Is_Feasible (Candidates(C), Model) then

            Last := Last + 1;

            if Last < C then

               Candidates(Last) := Candidates(C);

            -- else Candidates(C) is already in place.

            end if;

         end if;

      end loop;

      return Candidates(Candidates'First .. Last);

   end Calls_From;


   --
   --    Operations to modify a model
   --


   procedure Ensure_Single_Ref (Model : in out Model_Ref)
   --
   -- Ensures that Model is the only reference to its underlying
   -- computation model, by making a copy of the model if necessary.
   -- Since this is used only when making a change to the Model,
   -- we also mark the Model (the copy, if one is made) as "changed".
   --
   is

      Original : constant Model_Access_T := Model.Ref;
      -- Accesses the underlying Model_T.

      Copy : Model_Access_T;
      -- Access the copy, if one is made.

   begin

      if Original.References > 1 then
         -- There are other references to this model, so we
         -- will copy it and make Model refer to the copy.

         if Opt.Trace_Models then

            Output.Trace ("Copy " & Ident_Ref (Model));

         end if;

         if not Original.Clean then

            Output.Fault (
               Location => "Flow.Computation.Ensure_Single_Ref",
               Text     => "Original model is not clean.");

         end if;

         if not Original.Pruned then

            Output.Fault (
               Location => "Flow.Computation.Ensure_Single_Ref",
               Text     => "Original model is not pruned.");

         end if;

         Original.References := Original.References - 1;
         -- The Model will no longer refer to the Original.

         Copy := new Model_T'(Original.all);

         Set_Index (Copy.all);

         Copy.References := 1;
         -- The Model will refer to the Copy.

         Model.Ref := Copy;

      end if;

      if Opt.Trace_Models then

         Output.Trace ("Marking as dirty " & Ident_Ref (Model));

      end if;

      Model.Ref.Clean := False;

   end Ensure_Single_Ref;


   procedure Set_Effect (
      Step  : in     Step_T;
      To    : in     Arithmetic.Effect_Ref;
      Under : in out Model_Ref)
   is
      use type Arithmetic.Effect_Ref;

      S : constant Step_Index_T := Index (Step);

   begin

      if To /= Under.Ref.Effect(S) then
         -- A real change.

         if Opt.Trace_Models then

            Output.Trace (
                 "Setting effect of step"
               & Step_Index_T'Image (Index (Step))
               & " in "
               & Ident_Ref (Under)
               & " to "
               & Arithmetic.Image (To.all));

         end if;

         Ensure_Single_Ref (Under);

         Under.Ref.Effect(S) := To;

      end if;

   end Set_Effect;


   procedure Mark_Infeasible (
      Step  : in     Step_T;
      Under : in out Model_Ref)
   is

      S : constant Step_Index_T := Index (Step);

   begin

      if Under.Ref.Feasible(S) then
         -- A real change.

         Ensure_Single_Ref (Under);

         Under.Ref.Feasible(S) := False;

         Under.Ref.Pruned := False;

      end if;

   end Mark_Infeasible;


   procedure Set_Condition (
      On    : in     Step_Edge_T;
      To    : in     Arithmetic.Condition_T;
      Under : in out Model_Ref)
   is
      use type Arithmetic.Condition_T;

      E : constant Step_Edge_Index_T := Index (On);

   begin

      if To /= Under.Ref.Condition(E) then
         -- A real change.

         Ensure_Single_Ref (Under);

         Under.Ref.Condition(E) := To;

         if To = Arithmetic.Never then
            -- This edge is now infeasible.

            Under.Ref.Pruned := False;

         end if;

      end if;

   end Set_Condition;


   procedure Mark_Infeasible (
      Edge  : in     Step_Edge_T;
      Under : in out Model_Ref)
   is
   begin

      Set_Condition (
         On    => Edge,
         To    => Arithmetic.Never,
         Under => Under);

   end Mark_Infeasible;


   procedure Mark_Infeasible (
      Edges : in     Step_Edge_List_T;
      Under : in out Model_Ref)
   is
   begin

      for E in Edges'Range loop

         Mark_Infeasible (
            Edge => Edges(E),
            Under => Under);

      end loop;

   end Mark_Infeasible;


   procedure Mark_Infeasible (
      Edge  : in     Edge_T;
      Under : in out Model_Ref)
   is
   begin

      Mark_Infeasible (
         Edge  => Step_Edge (Edge),
         Under => Under);

   end Mark_Infeasible;


   procedure Mark_Infeasible (
      Edges : in     Edge_List_T;
      Under : in out Model_Ref)
   is
   begin

      for E in Edges'Range loop

         Mark_Infeasible (
            Edge  => Step_Edge (Edges(E)),
            Under => Under);

      end loop;

   end Mark_Infeasible;


   procedure Mark_Infeasible (
      Call  : in     Programs.Call_T;
      Under : in out Model_Ref)
   is
   begin

      Mark_Infeasible (Programs.Step (Call), Under);

   end Mark_Infeasible;


   procedure Mark_Infeasible (
      Luup  : in     Loops.Loop_T;
      Under : in out Model_Ref)
   is
   begin

      Mark_Infeasible (Loops.Head_Step (Luup), Under);

   end Mark_Infeasible;


   procedure Set_Calling_Protocol (
      Call  : in     Programs.Call_T;
      To    : in     Calling.Protocol_Ref;
      Under : in out Model_Ref)
   is
      use type Calling.Protocol_Ref;

      Index : constant Programs.Call_Index_T := Programs.Index (Call);
      -- The index of this Call in the calling subprogram.

   begin

      if To /= Under.Ref.Protocol(Index) then
         -- A real change.

         Ensure_Single_Ref (Under);

         Under.Ref.Protocol (Index) := To;
         Under.Ref.New_Proto(Index) := True;

         -- TBA discard the old protocol if no longer used elsewhere.

      end if;

   end Set_Calling_Protocol;


   procedure Mark_No_Return (
      From : in     Programs.Call_T;
      To   : in out Model_Ref)
   is

      Index : constant Programs.Call_Index_T := Programs.Index (From);
      -- The index of this call in the calling subprogram.

   begin

      if To.Ref.Returns(Index) then
         -- A real change.

         Ensure_Single_Ref (To);

         To.Ref.Returns(Index) := False;

         Mark_Infeasible (
            Edges => Edges_From (Programs.Step (From), To),
            Under => To);

      end if;

   end Mark_No_Return;


   procedure Prune (Model : in out Model_Ref)
   is
   begin

      if  Pruning.Opt.Prune
      and not Model.Ref.Pruned
      then

         Ensure_Single_Ref (Model);

         Flow.Pruning.Prune (Model);

         Model.Ref.Pruned := True;

      end if;

   end Prune;


end Flow.Computation;
