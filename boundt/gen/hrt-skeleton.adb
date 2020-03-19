-- HRT.Skeleton (body)
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
-- $Date: 2015/10/24 20:05:49 $
--
-- $Log: hrt-skeleton.adb,v $
-- Revision 1.17  2015/10/24 20:05:49  niklas
-- Moved to free licence.
--
-- Revision 1.16  2008-12-25 08:32:42  niklas
-- Removed unused context clauses and local variables.
--
-- Revision 1.15  2005/08/24 10:14:26  niklas
-- Replaced the local function Get_Call_Nodes with the new
-- inquiry function Programs.Nodes (Call_List).
--
-- Revision 1.14  2005/04/17 07:54:58  niklas
-- Changed Output.Error to Output.Fault in Unique_Edge.
--
-- Revision 1.13  2005/02/16 21:11:46  niklas
-- BT-CH-0002.
--
-- Revision 1.12  2004/05/01 10:38:49  niklas
-- First Tidorum version.
-- Updated (incompletely) to use separate Node_Times and Edge_Times.
-- Updated for changes in Programs.Execution and Analyser.
--
-- Revision 1.11  2003/03/11 08:23:57  holsti
-- Split execution-bounds stuff from the Programs package to
-- make the child package Programs.Execution.
--
-- Revision 1.10  2001/12/10 15:18:17  holsti
-- The Generate operation takes an assertion-set as parameter.
--
-- Revision 1.9  2001/06/19 21:41:50  holsti
-- HRT.Comments to ESF, not standard output.
--
-- Revision 1.8  2001/05/20 13:26:10  holsti
-- Use Flow.Count_Memory_Traffic (NC_120, NC_121, NC_122, NC_128).
--
-- Revision 1.7  2001/04/06 11:10:44  ville
-- NC_054 fixed
--
-- Revision 1.6  2001/04/05 10:48:42  ville
-- Deleted obsolete subroutines, variables etc
--
-- Revision 1.5  2001/04/04 13:27:48  ville
-- ESF file produced
--
-- Revision 1.4  2001/04/03 10:32:05  ville
-- ESF output in standard output
--
-- Revision 1.3  2001/03/20 13:37:54  ville
-- Memory reads and writes added to CCS
--
-- Revision 1.2  2001/03/19 12:36:14  ville
-- HRT rework third phase WCP converted to ESL lines
--
-- Revision 1.1  2001/03/14 14:50:58  ville
-- HRT rework second phase: skeleton package splitted
--
-- Revision 1.10  2001/03/13 12:43:47  ville
-- HRT rework first phase: WCP for a call
--
-- Revision 1.9  2001/02/19 09:38:14  holsti
-- Adapted to changes in Programs.Execution_Bounds_Ref.
--
-- Revision 1.8  2001/01/02 07:46:18  langback
-- Minor change to one Error message.
--
-- Revision 1.7  2000/12/28 12:35:36  holsti
-- Adapted to changes in Programs.Identify.
--
-- Revision 1.6  2000/11/29 14:10:28  saarinen
-- Cleaned procedure Get_TPOF.
--
-- Revision 1.5  2000/11/22 22:29:04  holsti
-- Using Processor.Code_Address_T instead of Processor.Address_T.
--
-- Revision 1.4  2000/09/08 13:17:13  saarinen
-- Made some minor corrections.
--
-- Revision 1.3  2000/08/18 18:09:01  holsti
-- Unbounded_Vectors Index_Type removed.
--
-- Revision 1.2  2000/08/11 12:54:33  saarinen
-- First implementation.
--


with Ada.Text_IO;
with Analyser;
with Assertions;
with ESF;
with Flow;
with Flow.Execution;
with HRT.TPOF;
with Loops;
with Output;
with Processor;


package body HRT.Skeleton is

   --    INTERNAL TYPES:

   type CCS_T is record
      Time   : Processor.Time_T;
      Reads  : Natural;
      Writes : Natural;
   end record;
   -- The wcet effort including:
   --    Time,   processing time 
   --    Reads,  memory reads
   --    Writes, memory reads
   
   
   type Loop_Fraction_T is record
      Numerator   : Positive;
      Denominator : Positive;
   end record;
   -- Indicates the fraction of node counts that are 
   -- used in each segment of an unrolled loop:
   -- "First" segment (containing nodes from loop head to exit)
   -- gets faction of 1 / Loop Repeats,
   -- "Second" segment (containing nodes from node following
   -- loop exit to source of loop repeat edge) gets fraction
   -- of 1 / 1.
   -- "Third" segment (containing again nodes from loop head
   -- to exit) gets fraction of (Loop Repeats - 1) / Loop Repeats.  
   
   
   function "*" (Left : Integer; Right : Loop_Fraction_T)
      return Integer 
   is
   begin
   
      if Left mod Right.Denominator > 0 then
      
         -- The "Left" parameter is not multiple of denominator
         -- of the "Right" parameter as it should be.
         
         Output.Fault (
            Location => "Multiplication of an integer by a fraction",
            Text     => "Integer is not multiple of the denominator of "
                        & "the fraction ("
                        & Integer'image(Left)
                        & " mod"
                        & Positive'image(Right.Denominator)
                        & " > 0)");
                        
         raise Internal_Error;
         
      end if;
      
      return (Left / Right.Denominator) * Right.Numerator;
   end "*";

   function "*" (Left : Loop_Fraction_T; Right : Loop_Fraction_T)
      return Loop_Fraction_T 
   is
   begin
      return (Numerator => Left.Numerator * Right.Numerator,
              Denominator => Left.Denominator * Right.Denominator);
   end "*";
         
   function "/" (Left : Integer; Right : Integer)
      return Loop_Fraction_T 
   is
   begin
      return (Numerator   => Left,
              Denominator => Right);
   end "/";

         
   type Edge_Set_T is array (Flow.Edge_Index_T range <>) of Boolean;


   -- Worst Case Path (WCP) definitions:
   
   type WCP_Node_Kind_T is (
      Real_Node,
      Loop_Head,
      Loop_Exit);
   -- Different types of WCP nodes:
   --   Real_Node, corresponds to some real node of the flow graph
   --              of a subprogram.
   --   Loop_Head, corresponds to the head of some loop in the
   --              flow graph of a subprogram.
   --   Loop_Exit, corresponds to the exit of some loop in the
   --              flow graph of a subprogram.
   
                     
   type WCP_Node_T(Kind : WCP_Node_Kind_T := Real_Node) is record            
      case Kind is      
         when Real_Node =>
            Node  : Flow.Node_T;
            Count : Positive;            
         when Loop_Head =>
            Repeats : Positive;            
         when Loop_Exit =>
            null;            
      end case;      
   end record;
   -- One node in the WCP:
   --    Kind, indicates type of the node (see WCP_Node_Kind_T).
   --    Node, reference to flow graph node for "Real Node" kind of
   --          WCP node.
   --    Count, execution count of a "Real Node".
   --    Repeats, loop repeats for a "Loop Head".
   
   
   type WCP_Table_T is array (Positive range <>) of WCP_Node_T;   
   -- Array for WCP nodes. The length of the array depends
   -- on the number nodes and nested loops in the flow graph
   -- of a subprogram (see "Longest_Unrolled_Path).
   
   
   type WCP_T(Max_Length : Positive) is record
      Length  : Natural := 0;
      Element : WCP_Table_T(1 .. Max_Length);
   end record;
   -- Record for WCP:
   --    Max_Length, maximum length of WCP array (see WCP_Table_T)
   --    Length,     number of used WCP nodes 
   --    Element,    WCP nodes (see WCP_Table_T)
   


--     INTERNAL DATA:
-- None

--     INTERNAL OPERATIONS:

   function Unique_Edge (
      What  : String;
      From  : Flow.Edge_List_T;
      Where : Edge_Set_T)
   return Flow.Edge_T
   -- Returnes unique edge from given edge list fullfilling the
   -- given criteria.
   
   is
      Found : Boolean := False;
      Edge  : Flow.Edge_T;
      
   begin
   
      for F in From'range loop
      
         if Where(Flow.Index(From(F))) then

            if Found then
               -- If more than one edges are found,
               -- there are illegal branches.

               Output.Error (
                  Text => "Unique edge expected, but second edge found ("
                     & Flow.Edge_Index_T'image(Flow.Index(From(F)))
                     & " after"
                     & Flow.Edge_Index_T'image(Flow.Index(Edge))
                     & " )");

               raise Path_Branch;

            else

               Edge  := From(F);
               Found := True;

            end if;

         end if;   
         
      end loop;
      
      if not Found then
         -- If requested edge was not found, the path is
         -- somehow broken.

         Output.Fault (
            Location => "HRT.Skeleton.Unique_Edge",
            Text     =>
                 "Unique "
               & What
               & " edge searched, but none found.");
                           
         raise Path_Broken;

      end if;
      
      return Edge;

   end Unique_Edge;


   function Longest_Unrolled_Path (
      Graph : Flow.Graph_T;
      Luups : Loops.Loops_T)
   return Positive
   --
   -- An upper bound on the length of a linear path through the
   -- flow-graph, after the loops have been partially unrolled
   -- into the bottom-exit form.
   --
   -- The result is only an upper bound, because it does not
   -- consider the actual path taken, and it assumes conservatively
   -- that any loop can be doubled in unrolling, without looking at
   -- how the "middle-exit" point divides up the loop.
   --
   is

      Full_Path : Natural := 0;
      -- The number of nodes in the full path, with all loops
      -- unrolled.

      Loop_Path : array (Luups'Range) of Natural := (others => 0);
      -- The number of nodes in the path for each loop, with
      -- inner loops unrolled.

      Counted : Flow.Node_Set_T (1 .. Flow.Max_Node (Graph)) :=
         (others => False);
      --
      -- The nodes that have been counted (included in the potential
      -- path length). Each node is counted once, but may be included
      -- multiple times on the path due to loop unrolling.


      procedure Count_Loop (
         Index   : Loops.Loop_Index_T;
         Members : Flow.Node_List_T;
         Parent  : Loops.Loop_List_T)
      --
      -- Compute the path length for a given loop, assuming that
      -- the contribution from inner loops is already in Loop_Path
      -- for this loop.
      --
      is

         Mem_Index : Flow.Node_Index_T;
         -- The index of a loop member node.

         Unrolled_Path : Natural;
         -- The length of this loop when partly unrolled.

         Parent_Index : Loops.Loop_Index_T;
         -- The index of the parent loop, if there is one.

      begin

         -- Count the loop-members that have not been counted in
         -- inner loops:

         for M in Members'Range loop

            Mem_Index := Flow.Index (Members(M));

            if not Counted(Mem_Index) then

               -- This node occurs once in the path through this loop.

               Loop_Path(Index) := Loop_Path(Index) + 1;
               Counted(Mem_Index) := True;

            end if;

         end loop;

         -- Count this loop in the path through its parent loop
         -- (or through the graph, if there is no parent loop).
         -- Through unrolling, the loop can be doubled.

         Unrolled_Path := 2 * Loop_Path(Index) + 1;

         if Parent'Length > 0 then
            -- There is an outer loop, so this loop contributes
            -- to the path through the outer loop.

            Parent_Index := Loops.Loop_Index (Parent(Parent'First));

            Loop_Path(Parent_Index) :=
               Loop_Path(Parent_Index) + Unrolled_Path;

         else
            -- There is no outer loop. This loop contributes
            -- directly to the full path.

            Full_Path := Full_Path + Unrolled_Path;

         end if;

      end Count_Loop;


   begin

      -- Scan the loops from innermost to outermost:

      for L in Luups'Range loop

         Count_Loop (
            Index   => L,
            Members =>
               Flow.To_List (
                  Set  => Loops.Members (Luups(L)).all,
                  From => Graph),
            Parent  =>
               Loops.Containing_Loop (
                  Loops => Luups,
                  Luup  => Luups(L)) );

      end loop;

      -- Count the nodes that are in no loop:

      for C in Counted'Range loop

         if not Counted(C) then

            Full_Path := Full_Path + 1;

            -- Should also set Counted(C) := True, in principle.

         end if;

      end loop;

      return Full_Path;

   end Longest_Unrolled_Path;
   
   
   procedure Convert_WCP_To_ESL (
      WCP         : in     WCP_T;
      HRT         : in     HRT_Struct_T;
      Interacting : in     Programs.Subprogram_Set_T;
      Bounds      : in     Programs.Execution.Bounds_Ref;
      Calls       : in     Programs.Call_List_T;
      Call_Nodes  : in     Flow.Node_List_T;
      Called      : in out Entry_Set_T;
      CCS         : in out CCS_T;
      Column      : in out Positive);
   -- Converts given WCP to Execution Skeleton Lines using given
   -- HRT sreucture, set of interacting subprograms, subroutine 
   -- calls and nodes containing the calls.
   -- The execution bounds is needed only for a parameter for
   -- possibly called procedure "Add_Call_To_ESL" (see below).
   -- The set of called PO Entries is updated when a some node
   -- in the WCP contains call to some PO Entry.
   -- The CCS is updated with the effort of the nodes.
   -- The column is updated when a loop head or a loop exit
   -- is found. 
      
      
   procedure Add_Call_To_ESL (
      Call        : in     Programs.Call_T;
      Bounds      : in     Programs.Execution.Bounds_Ref;
      HRT         : in     HRT_Struct_T;
      Interacting : in     Programs.Subprogram_Set_T;
      Called      : in out Entry_Set_T;
      CCS         : in out CCS_T;
      Column      : in out Positive);
   -- The Execution Skeleton Lines of the given call are added
   -- to the ESF file using given execution bounds. The rest of
   -- the parameters are needed for a parameters of procedure
   -- "Convert_WCP_To_ESL" (see above) which is called by this
   -- procedure.


   procedure Build_HRT_Skeletons (
      HRT        : in out HRT_Struct_T;
      Program    : in     Programs.Program_T;
      Bounds_Set : in     Programs.Execution.Bounds_Set_T;
      File_Name  : in     String);


   procedure Build_Skeleton (
      Call        : in Programs.Call_T;
      HRT         : in HRT_Struct_T;
      Interacting : in Programs.Subprogram_Set_T;
      Own_Entries : in Entry_Set_T;
      Bounds_Set  : in Programs.Execution.Bounds_Set_T);


   procedure List_Execution_Statements (
      Call        : in      Programs.Call_T;
      Bounds      : in      Programs.Execution.Bounds_Ref;
      HRT         : in      HRT_Struct_T;
      Interacting : in      Programs.Subprogram_Set_T;
      Called      :     out Entry_Set_t);

   --         using :
   --            A root call (with completed WCET analysis),
   --            execution bounds, 
   --            HRT structure,
   --            interacting subprograms,
   --         giving:
   --             execution statement list into the ESF file,
   --             set of called PO Entires


   -- OPERATION IMPLEMENTATIONS:

   procedure Put_ESL (
      Line   : in String;
      Column : in Positive)
   
   -- Insert the given line to the Execution Statement List
   -- intended according to the given column.
   
   is
   begin
   
      Ada.Text_IO.Set_Col (
         File => ESF.File,
         To   => Ada.Text_IO.Positive_Count(Column));
      Ada.Text_IO.Put_Line (
          File => ESF.File,
          Item => Line);
      
   end Put_ESL;
   
   
   procedure Convert_WCP_To_ESL (
      WCP         : in     WCP_T;
      HRT         : in     HRT_Struct_T;
      Interacting : in     Programs.Subprogram_Set_T;
      Bounds      : in     Programs.Execution.Bounds_Ref;
      Calls       : in     Programs.Call_List_T;
      Call_Nodes  : in     Flow.Node_List_T;
      Called      : in out Entry_Set_T;
      CCS         : in out CCS_T;
      Column      : in out Positive)
      
   -- Converts given WCP to Execution Skeleton Lines using given
   -- HRT structure, set of interacting subprograms, subroutine 
   -- calls and nodes containing the calls.
   --
   -- The set of called PO Entries is updated when a some node
   -- in the WCP contains call to some PO Entry.
   -- The CCS is updated with the effort of the nodes.
   -- The column is updated when a loop head or a loop exit
   -- is found. 

   is

      use type Processor.Time_T;

      Node_Times : constant Programs.Execution.Node_Times_T := 
         Programs.Execution.Node_Times (
            From       => Bounds,
            With_Calls => True);

      Edge_Times : constant Programs.Execution.Edge_Times_T :=
         Programs.Execution.Edge_Times (Bounds);

     
      procedure Add_Time (Node : in Flow.Node_T)
      --
      -- Adds node effort to CCS.
      --
      is

         Index : constant Flow.Node_Index_T := Flow.Index (Node);
         -- The index of the node.

         Reads, Writes : Natural;
         -- The number of reads and writes in the node.
               
      begin

         Flow.Execution.Count_Memory_Traffic (
           From   => Node,
           Reads  => Reads,
           Writes => Writes);

         CCS.Time := CCS.Time + Node_Times(Index);
         -- Add processing time.
         
         CCS.Reads := CCS.Reads + Reads;
         -- Add memory reads.
         
         CCS.Writes := CCS.Writes + Writes;
         -- Add memory writes.
         
      end Add_Time;


      procedure Check_Node (
         Node       : in     Flow.Node_T;
         Exec_Count : in     Positive)
      --
      -- Checks node for calls and adds node effort to ccs.
      --
      is
         use type Flow.Node_T;
         
         PO_Entry : Entry_T;
         
      begin
            for N in Call_Nodes'Range loop
               if Node = Call_Nodes(N) then
                  -- Call found.

                  declare

                     Call   : Programs.Call_T := Calls(N);

                     Call_Bounds : Programs.Execution.Bounds_Ref :=
                        Programs.Execution.Call_Bounds (
                           On     => Call,
                           Within => Bounds);

                     Callee : Programs.Subprogram_T :=
                        Programs.Callee(Call);

                  begin
                     if Is_Member(Callee, HRT.PO_Entries) then
                        -- PO_Entry call.
                        
                        if CCS.Time > 0 then
                              
                           Put_ESL (
                              Line => "wcet"
                                      & Processor.Time_T'image (CCS.Time)
                                      & " ,"
                                      & Natural'image(CCS.Reads)
                                      & " ,"
                                      & Natural'image(CCS.Writes),
                              Column => Column);
                              
                        end if;

                        PO_Entry := Search (
                           Sub    => Callee, 
                           Of_Set => HRT.PO_Entries);
                        -- Search the PO_Entry which is the same as the
                        -- called subprogram.
                        
                        case PO_Entry.Def.Kind is
                        
                           when Resource =>
                              Put_ESL (
                                 Line => "call_po "
                                         & PO_Entry.Def.Res_Name.all
                                         & " "
                                         & Programs.Name (Callee),
                                 Column => Column);

                           when Synchro =>
                              Put_ESL (
                                 Line => "call_po "
                                         & PO_Entry.Def.Sync_Name.all
                                         & " "
                                         & Programs.Name (Callee),
                                 Column => Column);
                           
                           when Thread =>
                              null;  -- impossible
                              
                        end case;

                        Add (To => Called, Adding => PO_Entry);
                        -- Add the entry to set of called PO entries.
                        
                     elsif Programs.Is_Member(Callee, Interacting) then
                        -- Call to interacting subprogram.

                        Add_Call_To_ESL (
                           Call        => Call,
                           Bounds      => Call_Bounds,
                           HRT         => HRT,
                           Interacting => Interacting,
                           Called      => Called,
                           CCS         => CCS,
                           Column      => Column);
                        -- Recursively include the execute statements
                        -- of the callee.


                     else

                        Add_Time(Node);
                        -- Other call, add node time including execution
                        -- time of the call.

                     end if;

                  end;

                  return;

               end if;

            end loop;

            -- No call => add only the node time.

            Add_Time(Node);

      end Check_Node;

        
   begin
         
      for N in 1 .. WCP.Length loop      
         -- Process all WCP nodes.
         
         case WCP.Element(N).Kind is
         
            when Real_Node =>
               -- Real Node: this corresponds to some real 
               -- flow graph node.
               
               Check_Node (
                  Node       => WCP.Element(N).Node,
                  Exec_Count => WCP.Element(N).Count);
               -- Check the effect of the flow graph node.
                     
            when Loop_Head =>
               -- This corresponds to a head of some loop.
               
               if CCS.Time > 0 then
               
                  Put_ESL (
                     Line => "wcet"
                             & Processor.Time_T'image (CCS.Time)
                             & " ,"
                             & Natural'image(CCS.Reads)
                             & " ,"
                             & Natural'image(CCS.Writes),
                     Column => Column);
                     
                  CCS := (0,0,0);
                  
               end if;
                  
               Put_ESL (
                  Line => "loop"
                          & Positive'image(WCP.Element(N).Repeats),
                  Column => Column);
               -- Mark the loop head into the ESF file.
                  
               Column := Column + 2;
               -- Increment the indention.
               
            when Loop_Exit =>
               -- This corresponds to end of some loop.
               
               if CCS.Time > 0 then
               
                  Put_ESL (
                     Line => "wcet"
                             & Processor.Time_T'image (CCS.Time)
                             & " ,"
                             & Natural'image(CCS.Reads)
                             & " ,"
                             & Natural'image(CCS.Writes),
                     Column => Column);
                                          
                  CCS := (0,0,0);
                  
               end if;

               Column := Column - 2;
               -- Decrement the indention.
               
               Put_ESL (
                  Line   => "end",
                  Column => Column);
               -- Mark the end of the loop into the ESF file.
            
         end case;
         
      end loop;
      
   end Convert_WCP_To_ESL;
   
   
   procedure Add_WCP_Node (
      Node : in     WCP_Node_T;
      WCP  : in out WCP_T)
      
   -- Add the given node to the given WCP node list.
       
   is
   begin
   
      if WCP.Length < WCP.Element'Last then      
         
         -- There is still room for new WCP nodes.
         
         WCP.Length               := WCP.Length + 1;
         WCP.Element (WCP.Length) := Node;

         if Trace then
            
            -- Trace option is selected: WCP information 
            -- is requested.
                  
            case Node.Kind is
         
               when Real_Node =>
            
                  Output.Note (Text => "WCP node " 
                     & Positive'image(WCP.Length)
                     & " : Real Node "
                     & Flow.Node_Index_T'image(Flow.Index(Node.Node))
                     & ", Execution count "
                     & Positive'image(Node.Count));

               when Loop_Head =>

                  Output.Note (Text => "WCP node " 
                     & Positive'image(WCP.Length)
                     & " : Loop Head, Repeats "
                     & Positive'image(Node.Repeats));
              
               when Loop_Exit =>
            
                  Output.Note (Text => "WCP node " 
                     & Positive'image(WCP.Length)
                     & " : Loop Exit");
                      
            end case;
         
         end if;
                  
      else
      
         -- WCP list is full.
         
         Output.Fault (
            Location => "Add_WCP_Node",
            Text     => "WCP index overflow (" 
               & Positive'image(WCP.Length + 1) & " >"
               & Positive'image(WCP.Element'Last) & ")");      
         raise Internal_Error;
         
      end if;
      
   end Add_WCP_Node;
   

   procedure Add_Call_To_ESL (
      Call        : in     Programs.Call_T;
      Bounds      : in     Programs.Execution.Bounds_Ref;
      HRT         : in     HRT_Struct_T;
      Interacting : in     Programs.Subprogram_Set_T;
      Called      : in out Entry_Set_T;
      CCS         : in out CCS_T;
      Column      : in out Positive)
   
   -- Add ESL lines corresponding to the given call to the ESL.
   -- First WCP of the callee of the call is built and that is
   -- then converted to the ESL lines by calling subroutine
   -- "Convert_WCP_To_ESL", which in turn can call this subroutine,
   -- if a call to new subroutine is from some node of the WCP.
   
   is
      use type Flow.Node_Index_T;
      
      Graph : constant Flow.Graph_T :=
         Programs.Flow_Graph (Programs.Callee (Call));
      -- Flow graph of the called subroutine.

      Calls : constant Programs.Call_List_T :=
         Programs.Calls_From (Programs.Callee (Call));
      -- All calls from this subroutine.
         
      Call_Nodes : constant Flow.Node_List_T := Programs.Nodes (Calls);
      -- All nodes containing subroutine call.
         
      Luups : constant Loops.Loops_T := 
         Programs.Loops_Of (Programs.Callee (Call));
      -- Loops contained in the called subroutine.

      Counts : constant Programs.Execution.Flow_Counts_Ref :=
         Programs.Execution.Counts (Bounds);
      -- Execution counts of the nodes and edges of the called
      -- subroutine.
                  
      Forward_Edges : constant Flow.Edge_List_T := Loops.Forward_Edges (
         Within   => Graph, 
         Avoiding => Luups);
      -- Edges pointing "forward" in the flow graph.
         
      Forward  : Edge_Set_T(1..Flow.Max_Edge(Graph));
      Executed : Edge_Set_T(1..Flow.Max_Edge(Graph));
      
      WCP : WCP_T (Longest_Unrolled_Path(
         Graph => Graph,
         Luups => Luups));
      -- WCP nodes storage (see comments of WCP_T and WCP_Table_T).
      
      
      procedure Add_Path_To_WCP (
         First     : in     Flow.Node_T;
         Last      : in     Flow.Node_T;
         Fraction  : in     Loop_Fraction_T;
         CEC       : in     Positive;
         Recursion : in out Positive;
         WCP       : in out WCP_T);      
      -- Add nodes corresponding to given path to WCP.

      procedure Add_Loop_To_WCP (
         Luup          : in     Loops.Loop_T;
         Head_Node     : in     Flow.Node_T;
         Loop_Factor   : in     Positive;
         Path_Fraction : in     Loop_Fraction_T;
         CEC           : in     Positive;
         Recursion     : in out Positive;
         WCP           : in out WCP_T;         
         Exit_Edge     :    out Flow.Edge_T);
      -- Add WCP nodes corresponding to the given loop
      -- taking into account loop unrolling when necessary.         


      procedure Add_Path_To_WCP (
         First     : in     Flow.Node_T;
         Last      : in     Flow.Node_T;
         Fraction  : in     Loop_Fraction_T;
         CEC       : in     Positive;
         Recursion : in out Positive;
         WCP       : in out WCP_T)
         
      -- Add path from the given First flow graph node to the
      -- given Last flow graph node using the given execution
      -- count Fraction and Current Execution Count.
      -- Recursion and WCP are passed to "Add_Loop_To_WCP" when
      -- a loop head is detected.
      -- The WCP is updated when a flow graph node is not a
      -- loop head. 
         
      is
         
         use type Flow.Node_T;
            
         Current       : Flow.Node_T := First;
         Current_Index : Flow.Node_Index_T;
         -- Node being processed and the index of it.
         
         Natural_Count : Positive;
         Factored_Count : Positive;
         -- Unmodified execution count of a flow graph node
         -- and the execution count multiplied by the Fraction.
      
         Forward_Edge : Flow.Edge_T;
         -- Edge that points to the next node to be processed.
                                       
      begin
      
         -- Loop from first node to last:
         
         loop
             
            Current_Index := Flow.Index(Current);
            Natural_Count := Counts.Node(Current_Index);
            Factored_Count := Natural_Count * Fraction;
            
            -- Check if the node is a loop head: if execution
            -- count of the node after taking into account given
            -- "Fraction" parameter is bigger than the given
            -- execution count (CEC), the node is a loop head.
        
            if Factored_Count > CEC 
            then
               -- This node is a loop head.

               -- The following commented outputs are TBA as a
               -- output of a TBD trace option (TBC):
               
               -- Output.Note (Text => "Loop head detected in WCP generation:"
               --   & Positive'image (Factored_Count / CEC)
               --   & " repeats, within"
               --   & Positive'image (CEC)
               --   & " times repeating path");
                  
               -- Output.Note (Text => "Loop head node :"
               --   & Flow.Node_Index_T'image (Flow.Index(Current)));
                  
               -- Output.Note (Text => "Excution count:"
               --   & Positive'image (Natural_Count)
               --   & " multiplied by:"
               --   & Positive'image (Fraction.Numerator)
               --   & " /"
               --   & Positive'image (Fraction.Denominator));
                     
               Add_Loop_To_WCP (
                  Luup          => Loops.Loop_Headed (
                                      By    => Current,
                                      Among => Luups),
                  Head_Node     => Current,
                  Loop_Factor   => Factored_Count/CEC,
                  Path_Fraction => Fraction,
                  CEC           => CEC,
                  Recursion     => Recursion,
                  WCP           => WCP,
                  Exit_Edge     => Forward_Edge);
               -- Add WCP nodes corresponding to the loop nodes.
               -- The iteration will continue from the target 
               -- of the returned loop exit edge.            
               
            else
               -- This is not a loop head and it
               -- can be added to the WCP as a new node.
         
               Add_WCP_Node (
                  Node => (
                     Kind  => Real_Node, 
                     Node  => Current,
                     Count => Factored_Count),
                  WCP  => WCP);
               -- Add this node to the WCP.

               if Current /= Last then
                  Forward_Edge := Unique_Edge (
                     What  => "forward",
                     From  => Flow.Edges_From (Node => Current, Within => Graph),
                     Where => Forward and Executed);
               end if;

            end if;
                     
            exit when Current = Last;
            -- The last node of the path is processed and it is time
            -- stop iteration.

            Current := Flow.Target (Forward_Edge);
            -- Move to next node.
               
         end loop;
                  
      end Add_Path_To_WCP;
      
      
      procedure Add_Loop_To_WCP (
         Luup          : in     Loops.Loop_T;
         Head_Node     : in     Flow.Node_T;
         Loop_Factor   : in     Positive;
         Path_Fraction : in     Loop_Fraction_T;
         CEC           : in     Positive;
         Recursion     : in out Positive;
         WCP           : in out WCP_T;         
         Exit_Edge     :    out Flow.Edge_T)

      -- Add WCP nodes corresponding to the given loop. If the
      -- loop exit edge does not leave from the "last" node of
      -- the loop, loop structure has to be modified so that
      -- edges to the nodes up to source of the exit edge are
      -- copied before the loop head and node following the
      -- source of the exit edge becomes new loop head.
      -- The loop head is marked with a "Loop Head" WCP
      -- node.   
         
      is
         use type Flow.Node_T;
                                 
         Exit_Node : Flow.Node_T;
         -- Source of the edge exiting the loop. 
               
         Repeat_Edge : Flow.Edge_T;
         -- The one repeat edge whose execution count > 0.
      
         Repeat_Node : Flow.Node_T;
         -- The source of the repeat edge (see above).
                                           
      begin
                     
         if Recursion > Luups'Length then
         
            -- The recursion is gone too far and has to
            -- be stopped.
            
            Output.Fault (
               Location => "Add loop to WCP",
               Text     => "Recursion level overflow ("
                  & Positive'image (Recursion)
                  & " )");
               
            raise Internal_Error;
            
         end if;

         Recursion := Recursion + 1;
         -- Increment recursion level.
            
         
         -- Search the effective loop exit edge. There should be
         -- exactly one. If there are none, the loop is eternal
         -- and if there are more than one, there are illegal
         -- branches in the loop.

         Exit_Edge := Unique_Edge (
            What  => "exit",
            From  => Loops.Exit_Edges(Exiting => Luup, Within => Graph),
            Where => Executed);
                  
         Exit_Node := Flow.Source (Exit_Edge);
                 

         -- Search the effective loop repeat edge. There should be
         -- exactly one. There has to be at least one repeat edge
         -- in any loop. If there are more than one repeat edges,
         -- there are illegal branches in the loop.

         Repeat_Edge := Unique_Edge (
            What  => "repeat",
            From  => Loops.Repeat_Edges (Repeating => Luup, Within => Graph),
            Where => Executed);
            
         Repeat_Node := Flow.Source (Repeat_Edge);

                  
         -- If the source of the effective loop exit edge is not
         -- the same as the source of the effective loop repeat 
         -- edge, the loop structure has to be modified for the
         -- WCP: edges to the nodes up to source of the exit node
         -- are "copied" to the WCP.
         
         if Repeat_Node = Exit_Node then
         
            -- "Repeat" node is same as "exit" node and the
            -- loop does need to be unrolled. 

            Add_WCP_Node (
               Node => (Kind    => Loop_Head, 
                        Repeats => Loop_Factor),
               WCP      => WCP);
            -- Mark the loop head with a virtual node.   
         
            Add_Path_To_WCP (
               First     => Head_Node,
               Last      => Exit_Node,
               Fraction  => Path_Fraction,
               CEC       => Loop_Factor * CEC,
               Recursion => Recursion,
               WCP       => WCP);
            -- Add recursively nodes from loop head to the source
            -- of the repeat edge.

            Add_WCP_Node (
               Node => (Kind => Loop_Exit),
               WCP      => WCP);
            -- Mark the loop exit with a virtual node.   
         
         else
         
            -- "Repeat" node is not same as "exit" node and
            -- the loop has to be unrolled.
                           
            Add_Path_To_WCP (
               First     => Head_Node,
               Last      => Exit_Node,
               Fraction  => (1/Loop_Factor)*Path_Fraction,
               CEC       => CEC,
               Recursion => Recursion,
               WCP       => WCP);
            -- Add recursively nodes from (original) head node to
            -- exit node.
                                                                                           
            Add_WCP_Node (
               Node => (Kind    => Loop_Head, 
                        Repeats => Loop_Factor - 1),
               WCP      => WCP);
            -- Mark the loop head with a virtual node.   
                        
            Add_Path_To_WCP (
               First     => Flow.Target (Unique_Edge (
                  What  => "loop continue",
                  From  => Loops.Edges_From (
                     Node => Exit_Node, 
                     Into => Luup,
                     Within => Graph),
                  Where => Forward and Executed)),
               Last      => Repeat_Node,
               Fraction  => Path_Fraction,
               CEC       => Loop_Factor * CEC,
               Recursion => Recursion,
               WCP       => WCP);
            -- Add recursively nodes from loop node following exit node
            -- until source of repeat edge.
                     
            Add_Path_To_WCP (
               First     => Head_Node,
               Last      => Exit_Node,
               Fraction  => ((Loop_Factor - 1)/Loop_Factor)*Path_Fraction,
               CEC       => (Loop_Factor - 1)*CEC,
               Recursion => Recursion,
               WCP       => WCP);
            -- Add recursively nodes from loop head to loop exit.

            Add_WCP_Node (
               Node => (Kind => Loop_Exit),
               WCP      => WCP);
            -- Mark the loop exit with a virtual node.   

         end if;
               
         Recursion := Recursion - 1;
         -- Decrement the recursion level.
         
      end Add_Loop_To_WCP;

      
      Return_Edges : constant Flow.Edge_List_T := Flow.Return_Edges (Graph);
      -- Edges whose target is the return node.
      
      Entry_Node   : constant Flow.Node_T := Flow.Entry_Node (Graph);
      -- First node of the flow graph.
      
      Recursion : Positive := 1;
      
   begin -- Add_Call_To_ESL
      
      WCP.Length := 0;
      -- WCP list initially empty.
      
      Forward  := (others => False);
      
      for F in Forward_Edges'range loop
         Forward (Flow.Index (Forward_Edges(F))) := True;
      end loop;
      
      for E in Counts.Edge'range loop
         Executed (E) := Counts.Edge (E) > 0;
      end loop;
      
      if Return_Edges'Length > 0 then
      
         -- There are more than one nodes in the flow graph
         -- and the path from entry node to the return node
         -- has to be added to the WCP.
         
         Add_Path_To_WCP (
            First     => Entry_Node,
            Last      => Flow.Target (Unique_Edge (
               What  => "return",
               From  => Return_Edges,
               Where => Executed)),
            Fraction  => (
               Numerator   => 1,
               Denominator => 1),
            CEC       => 1,
            Recursion => Recursion,
            WCP       => WCP);
      
      else
      
         -- There is only one node in the flow graph and it
         -- can be simply added to the WCP.
         
         Add_WCP_Node (
            Node => (
               Kind  => Real_Node, 
               Node  => Entry_Node,
               Count => Counts.Node(Flow.Index(Entry_Node))),
            WCP  => WCP);
            
      end if;
      
      Convert_WCP_To_ESL (
         WCP         => WCP,
         HRT         => HRT,
         Interacting => Interacting,
         Bounds      => Bounds,
         Calls       => Calls,
         Call_Nodes  => Call_Nodes,
         Called      => Called,
         CCS         => CCS,
         Column      => Column);
      -- Now the WCP is built and it has to be converted to
      -- ESL lines.   
                  
   end Add_Call_To_ESL;    


   procedure Generate (
      TPOF_Name  : in     String;
      Program    : in out Programs.Program_T;
      Asserts    : in out Assertions.Assertion_Set_T;
      Bounds_Set :    out Programs.Execution.Bounds_Set_T)
   is

      HRT      : HRT_Struct_T;

      ESF_Name : String := TPOF.Check_TPOF_Name(TPOF_Name);

   begin

      -- Read the information in TPO file.

      TPOF.Get_TPOF (TPOF_Name, Program, HRT);

      -- Simplifying assumption: Each thread body (= one activation)
      -- is already isolated in a subprogram, which is named in the
      -- TPOF. Automatic isolation of the body of an eternal
      -- thread-loop is not done (RB XS.TA.10 is not implemented).


      -- Analyse flow, calls, loops and WCET:

      Analyser.Analyse (
         Program    => Program,
         Asserts    => Asserts,
         Bounds_Set => Bounds_Set);

      -- All the results of the above analysis are available
      -- via the HRT structure, going from a task or entry to
      -- the corresponding subprogram and then to its flow-graph,
      -- annotated with worst-case execution counts.
      -- The analysis results for the non-root subprograms
      -- are accessed via the calls in the flow-graph which refer
      -- to the callee subprograms.

      -- Sanity checks on HRT structure:

      Check_Sanity (HRT => HRT);


      -- Construct the Execution Skeletons:

      Build_HRT_Skeletons (
         HRT        => HRT,
         Program    => Program,
         Bounds_Set => Bounds_Set,
         File_Name  => ESF_Name);

   end Generate;


   
   procedure Build_HRT_Skeletons (
      HRT        : in out HRT_Struct_T;
      Program    : in     Programs.Program_T;
      Bounds_Set : in     Programs.Execution.Bounds_Set_T;
      File_Name  : in     String)
   --
   -- Using :
   --    HRT structure from TPOF,
   --    program under analysis,
   --    execution bounds for the program's HRT entities,
   --    file-name for the ESF file.
   -- Giving:
   --    execution skeletons of all threads and PO entries,
   --    inserted into the ESF file.
   --
   is
      
      Interacting : Programs.Subprogram_Set_T;

      Definitions : constant Def_List_T := 
         Def_Vectors.To_Vector (HRT.Definitions);
      
      
      procedure Put_Thread (Def : in Def_T)
      --
      -- Inserts thread information into the ESF file using
      -- the given definition.
      --
      is
      
         Own_Entries : Entry_Set_T;
         
      begin

         Ada.Text_IO.Put_Line(
            File => ESF.File,
            Item => "   thread " & Def.Thread_Name.all);
         -- Begin the thread "block".

         if Def.Comments /= null then        
            -- Insert comments.
            for C in Def.Comments'Range loop
               Ada.Text_IO.Put_Line(
                  File => ESF.File,
                  Item => "     " & Def.Comments(C).all);
            end loop;
         end if;

         case Def.Thread_Kind is
            -- Insert the type of the thread.
            when Cyclic =>
               Ada.Text_IO.Put_Line(
                  File => ESF.File,
                  Item => "     type cyclic");
            when Sporadic =>
               Ada.Text_IO.Put_Line(
                  File => ESF.File,
                  Item => "     type sporadic");
            when Interrupt_Sporadic =>
               Ada.Text_IO.Put_Line(
                  File => ESF.File,
                  Item => "     type interrupt_sporadic");
         end case;

         Erase (Own_Entries);
         -- Threads don't have PO Entries.
         
         Build_Skeleton (
            Call        => Def.Root.Call,
            HRT         => HRT,
            Interacting => Interacting,
            Own_Entries => Own_Entries,
            Bounds_Set  => Bounds_Set);
         -- Build execution skeleton and insert it into the
         -- ESF file.

         Ada.Text_IO.Put_Line(
            File => ESF.File,
            Item => "   end " & Def.Thread_Name.all);
         -- End the thread "block".
      
      end Put_Thread;
      
      
      procedure Put_Resource (Def : in Def_T)
      
      -- Insert the resource object information into the ESF
      -- file using the given definition.
      
      is
      
         Entries : constant Entry_List_T := 
            Entry_Vectors.To_Vector (Def.PO_Entries);
         -- Entries of this object.
         
         Own_Entries : Entry_Set_T;
         -- Set of the entries.
         
      begin

         Entry_Bags.Assign (
            Object => Own_Entries, Value => Entry_Bags.List(Entries));
         -- Convert the entry list to entry set.
            
         Ada.Text_IO.Put_Line(
            File => ESF.File,
            Item => "   protected " & Def.Res_Name.all);
         -- Begin the protected object "block".

         if Def.Comments /= null then
            -- Insert comments.
            for C in Def.Comments'Range loop
               Ada.Text_IO.Put_Line(
                  File => ESF.File,
                  Item => "     " & Def.Comments(C).all);
            end loop;
         end if;

         Ada.Text_IO.Put_Line(
            File => ESF.File,
            Item => "     type resource");
         -- Insert the type of the object.
      
         for E in Entries'range loop
            -- Process the entries of the object.
            
            Ada.Text_IO.Put_Line(
               File => ESF.File,
               Item => "     entry " & Entries(E).Name.all);
            -- Insert the entry name.

            Build_Skeleton(
               Call        => Entries(E).Call,
               HRT         => HRT,
               Interacting => Interacting,
               Own_Entries => Own_Entries,
               Bounds_Set  => Bounds_Set);
            -- Build the execution skeleton and insert it into
            -- the ESF file.
               
         end loop;
         
         Ada.Text_IO.Put_Line(
            File => ESF.File,
            Item => "   end " & Def.Res_Name.all);
         -- End the protected object "block".
                   
      end Put_Resource;


      procedure Put_Synchro (Def : in Def_T)
      
      -- Insert the synchro object information into the
      -- ESF file using the given definition.
      
      is
      
         Own_Entries : Entry_Set_T;
            
      begin

         Erase (Own_Entries);
         -- Initialize the set of the entries of this PO
         -- to empty.
         
         Add (To => Own_Entries, Adding => Def.PO_Entry);
         Add (To => Own_Entries, Adding => Def.Barriered);
         -- Add the entries of this PO to the set.
         
         Ada.Text_IO.Put_Line(
            File => ESF.File,
            Item => "   protected " & Def.Sync_Name.all);
         -- Begin the protected object "block".

         if Def.Comments /= null then
            -- Insert comments.
            for C in Def.Comments'Range loop
               Ada.Text_IO.Put_Line(
                  File => ESF.File,
                  Item => "     " & Def.Comments(C).all);
            end loop;
         end if;

         Ada.Text_IO.Put_Line(
            File => ESF.File,
            Item => "     type synchro");
         -- Insert the type of the object.

         Ada.Text_IO.Put_Line(
            File => ESF.File,
            Item => "     entry " & Def.PO_Entry.Name.all);
         -- Insert the name of the entry.

         Build_Skeleton(
            Call        => Def.PO_Entry.Call,
            HRT         => HRT,
            Interacting => Interacting,
            Own_Entries => Own_Entries,
            Bounds_Set  => Bounds_Set);
         -- Build the execution skeleton and insert it into
         -- the ESF file.
      
         Ada.Text_IO.Put_Line(
            File => ESF.File,
            Item => "     barrier wcet tbd, tbd, tbd");
         -- Insert the barrier wcet.

         Ada.Text_IO.Put_Line(
            File => ESF.File,
            Item => "     entry " & Def.Barriered.Name.all);
         -- Insert the barriered entry name.

         Build_Skeleton(
            Call        => Def.Barriered.Call,
            HRT         => HRT,
            Interacting => Interacting,
            Own_Entries => Own_Entries,
            Bounds_Set  => Bounds_Set);
         -- Build execution skeleton and insert it into
         -- the ESF file.

         Ada.Text_IO.Put_Line(
            File => ESF.File,
            Item => "   end " & Def.Sync_Name.all);
         -- End the protected object "block".
      
      end Put_Synchro;
      
   begin

      Output.Note (Text =>
           "Execution Skeleton File"
         & Output.Field_Separator
         & File_Name);
      
      ESF.Create (File_Name);
      -- Create (and open) the ESF file.
                  
      Ada.Text_IO.Put_Line (
         File => ESF.File,
         Item => " program " & HRT.Prog_Name.all);
      -- Insert the name of the HRT program into the ESF file.

      if HRT.Comments /= null then
         -- Insert comments.
         for C in HRT.Comments'Range loop
            Ada.Text_IO.Put_Line(
               File => ESF.File,
               Item => " " & HRT.Comments(C).all);
         end loop;
      end if;

      Interacting :=
        Interacting_Subprograms (Program, HRT);
      -- Find the subprograms that call PO entries and for which
      -- we thus need worst-case execution paths.


      -- Loop through all definitions in the HRT structure:
      
      for D in Definitions'range loop
      
         Ada.Text_IO.New_Line(File => ESF.File);
         
         case Definitions(D).Kind is
	 
	    when Thread =>
	       Put_Thread(Def => Definitions(D));
               -- Insert the Thread information to the ESF file.
		     
            when Resource =>
	       Put_Resource(Def => Definitions(D));
               -- Insert the Resource Object information to the 
               -- ESF file.
	       
	    when Synchro =>
	       Put_Synchro(Def => Definitions(D));
               -- Insert the Synchro Object information to the
               -- ESF file.
	 end case;
	 
      end loop;

      Ada.Text_IO.Put_Line (
         File => ESF.File,
         Item => " end " & HRT.Prog_Name.all);

      ESF.Close;
      
   exception

      when Ada.Text_IO.Use_Error
         | Ada.Text_IO.Status_Error
         | Ada.Text_IO.Name_Error =>

         Output.Error (Text =>
              "Unable to create execution skeleton file "
            & File_Name & ".");

         raise ESF_Error;

   end Build_HRT_Skeletons;


   procedure Build_Skeleton (
      Call        : in Programs.Call_T;
      HRT         : in HRT_Struct_T;
      Interacting : in Programs.Subprogram_Set_T;
      Own_Entries : in Entry_Set_T;
      Bounds_Set  : in Programs.Execution.Bounds_Set_T)

   -- Using :
   --    A root call (with completed WCET analysis),
   --    HRT structure,
   --    interacting subprograms,
   --    set of Entries belonging to same PO (in case of PO)
   -- Giving:
   --    Execution statement list for the subprogram into
   --    the ESF file,
   --    List of other possible PO calls which however were
   --    not called in the worst case execution into the
   --    ESF File.
   is
   
      Called : Entry_Set_T;
      -- All PO Entries called in the path starting from the
      -- root subprogram.
      
      All_Entries : constant Entry_List_T := 
         To_List (HRT.PO_Entries);
      -- All PO Entries defined in the TPOF file.
         
      Possible_Entries : Entry_Set_T;
      -- PO Entries that could be called.
      
      
      procedure Add_PO_Calls
        (Root : Programs.Subprogram_T)
        
        -- Add all PO Entries that can be called from
        -- the given subroutine to the set of PO Entries
        -- that could be called in some execution.
      is
         Calls  : constant Programs.Call_List_T :=
           Programs.Calls_From (Root);

      begin
         for C in Calls'Range loop
            declare
               Callee    : constant Programs.Subprogram_T :=
                  Programs.Callee(Calls(C));

               New_Entry : constant Entry_T :=
                  Search(Callee, HRT.PO_Entries);
               -- Search the called subroutine from the
               -- set of all PO Entries. Null is returned
               -- if it is not found.

            begin

               if New_Entry /= null then
                  Add(To     => Possible_Entries,
                      Adding => New_Entry);
                  -- Add the found entry to the set of PO Entries
                  -- that could be called in some execution.
               end if;

               Add_PO_Calls(Callee);
               -- Include indirect calls by recursion.

            end;

         end loop;

      end Add_PO_Calls;
      
         
   begin
            
      -- Traverse the WCP and list Execution Statements:
      
      List_Execution_Statements (
         Call,
         Programs.Execution.Bounds_For (
            Root   => Call,
            Within => Bounds_Set),
         HRT,
         Interacting,
         Called);

      -- List the PO entries called by other paths:

      Erase (Possible_Entries);
      -- Initialize set of possible PO Entries to empty.
      
      Add_PO_Calls (Programs.Callee (Call));
      -- Add recursively all PO Entries that could be called
      -- by any execution path.
      
      for E in All_Entries'range loop
         -- Loop through all PO Entries
      
         if not Is_Member(Item => All_Entries(E), Of_Set => Called)
            -- The PO Entry was not called in the investigated path
         and not Is_Member(Item => All_Entries(E), Of_Set => Own_Entries)
            -- and is not a entry of a same protected object
         and Is_Member(Item => All_Entries(E), Of_Set => Possible_Entries)
            -- but could be called in some other execution path 
         then
         
            case All_Entries(E).Def.Kind is
            
               when Resource =>
                  Ada.Text_IO.Put_Line (
                     File => ESF.File, 
                     Item => "     po " 
                             & All_Entries(E).Def.Res_Name.all
                             & " "
                             & All_Entries(E).Name.all);
                             
               when Synchro =>
                  Ada.Text_IO.Put_Line (
                     File => ESF.File, 
                     Item => "     po " 
                             & All_Entries(E).Def.Sync_Name.all
                             & " "
                             & All_Entries(E).Name.all);
                 
               when Thread =>
                  null; -- impossible
               
            end case;
            
         end if;
         
      end loop;
      
   end Build_Skeleton;


   procedure List_Execution_Statements (
      Call        : in  Programs.Call_T;
      Bounds      : in  Programs.Execution.Bounds_Ref;
      HRT         : in  HRT_Struct_T;
      Interacting : in  Programs.Subprogram_Set_T;
      Called      : out Entry_Set_T)
   --
   -- Using :
   --    A root call (with completed WCET analysis),
   --    execution bounds, 
   --    HRT structure,
   --    interacting subprograms,
   -- Giving:
   --    execution statement list into the ESF file,
   --    set of called PO Entries

   is
      use type Processor.Time_T;

      CCS           : CCS_T := (0,0,0);
      -- Initialise the CCS to all zeroes.

      Column : Positive := 8;
      -- Initial indention.
      
   begin

      Erase (Called);
      -- Initialize set of called entries to empty.
      
      Add_Call_To_ESL (
         Call        => Call,
         Bounds      => Bounds,
         HRT         => HRT,
         Interacting => Interacting,
         Called      => Called,
         CCS         => CCS,
         Column      => Column);
      -- Add recursively execution statements of all called
      -- subroutines starting from the root subroutine to
      -- the execution statement list. 


      if CCS.Time > 0 then
         -- Some part of "wcet" is not yet included in the
         -- output (ESF file). 
              
         Put_ESL (
            Line => "wcet"
                    & Processor.Time_T'image (CCS.Time)
                    & " ,"
                    & Natural'image(CCS.Reads)
                    & " ,"
                    & Natural'image(CCS.Writes),
            Column => Column);
                                       
      end if;
      
   end List_Execution_Statements;


end HRT.Skeleton;
