-- Least_Fixpoint (body)
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
-- $Revision: 1.4 $
-- $Date: 2015/10/24 20:05:49 $
--
-- $Log: least_fixpoint.adb,v $
-- Revision 1.4  2015/10/24 20:05:49  niklas
-- Moved to free licence.
--
-- Revision 1.3  2005-05-09 15:31:01  niklas
-- Added the parameter Least_Fixpoint.Afters, to choose between the
-- pre-values and the post-values of the solution. The value-origin
-- analysis needs post-values.
--
-- Revision 1.2  2004/04/28 19:19:40  niklas
-- First Tidorum version.
-- Added a Value parameter to the generic formal Successors function so
-- that the successor node-set can depend on the current value, for
-- example to elide infeasible edges.
-- Removed the built-in iteration-tracing feature (the generic formals
-- Node_Image and Value_Image and the internal operation Show_Values
-- and its calls). If tracing is desired, it should be implemented in
-- the actual operations bound to the generic formals.
--
-- Revision 1.1  2001/01/07 21:54:22  holsti
-- First version.
--


with Bounded_Queues;
with Bounded_Set_Pool;

with Ada.Text_IO;
with Ada.Exceptions;


function Least_Fixpoint (
   Nodes  : Node_List;
   Edges  : Natural;
   Within : Graph;
   Afters : Boolean := False)
return Value_List
is

   subtype Node_Index_T is Positive range Nodes'Range;
   --
   -- Identifies a node in various arrays.

   subtype Node_Values_T is Value_List (Node_Index_T);
   --
   -- A value for each node.

   Pre  : Node_Values_T;
   Post : Node_Values_T;
   --
   -- The pre-value and post-value of each node.


   package Target_Nodes is new Bounded_Queues (
      Element_Type => Node_Index_T);
   --
   -- Implements queues of nodes (identified by node indices).
   -- These nodes will be the target nodes of the work-list edges.


   Work_Targets : Target_Nodes.Queue_Type (Max_Length => Nodes'Length);
   --
   -- The target nodes in the work-list.


   package Source_Nodes is new Bounded_Set_Pool (
      Element_Type => Node_Index_T,
      Max_Elements => Edges);
   --
   -- Implements sets of source-nodes (identified by node indices).


   Work_Sources : array (Node_Index_T) of Source_Nodes.Set_Type;
   --
   -- For each target node N in Work_Targets, Work_Sources(N)
   -- contains the source-nodes of the edges in the work-list,
   -- for the target node N.


   procedure Add_To_Work (Source, Target : in Node)
   --
   -- Adds the edge Source->Target to the work-list.
   --
   is

      S : constant Node_Index_T := Index_Of (Source);
      T : constant Node_Index_T := Index_Of (Target);
      -- The indices of the source and target nodes.

   begin

      if Source_Nodes.Empty (Work_Sources(T)) then
         -- The work-list contains no other edges for Target.
         -- The target-node must be put into Work_Targets:

         Target_Nodes.Put (Element => T, Into => Work_Targets);

      end if;

      -- In any case, the source-node is added to the work-list
      -- for this target-node:

      Source_Nodes.Add (Element => S, To => Work_Sources(T));

   end Add_To_Work;


   procedure Add_Edges_To_Work (
      Source : in Node;
      Index  : in Node_Index_T)
   --
   -- Adds, to the work list, all the edges from the given node
   -- to its successor nodes under the current Post value flowing
   -- from the node. Index is assumed to be Index_Of (Source).
   --
   is

      Succ : constant Node_List :=
         Successors (
            After  => Source,
            Post   => Post (Index),
            Within => Within);
      --
      -- The successor nodes for this Source for the
      -- current Post value of the Source.

   begin

      for S in Succ'range loop
         Add_To_Work (
            Source => Source,
            Target => Succ(S));
      end loop;

   end Add_Edges_To_Work;


   procedure Take_From_Work (
      Index : out Node_Index_T;
      Empty : out Boolean)
   --
   -- Takes the first target node from the work-list and returns
   -- its index if the work-list was not empty, else returns Empty
   -- as True.
   --
   is
   begin

      Target_Nodes.Get (
         From    => Work_Targets,
         Element => Index);

      Empty := False;

   exception
      when Target_Nodes.Underflow =>

         Empty := True;
         Index := Node_Index_T'First;
         -- To avoid invalid value.

   end Take_From_Work;


   procedure Process (Target_Index : Node_Index_T)
   --
   -- Processes a target node taken from the work-list, by re-merging
   -- all the changed post-values into the pre-value of the target.
   -- If the pre-value grows, the target's post-value is updated, and
   -- if the post-value grows, all the target's out-edges are put in
   -- the work-list.
   --
   is

      Target_Node : constant Node := Nodes(Target_Index);
      -- The target-node being processed.

      Sources : Source_Nodes.Set_Type
         renames Work_Sources (Target_Index);
      -- The source-nodes listed in the work-list for this target-node.
      -- These are the predecessors of the target which have changed
      -- post-values since the target's pre-value was last updated.

      Source_Index : Node_Index_T;
      -- The index of a source-node.

      Grew : Boolean;
      -- A Merge operation made the pre-value grow,

      Pre_Grew : Boolean := False;
      -- Some Merge operation made the pre-value grow.

      Post_Grew : Boolean;
      -- The Transform operation made the post-value grow.

   begin

      -- Merge the post-value for each edge listed in
      -- the work-list for this target:

      while not Source_Nodes.Empty (Sources)
      loop

         Source_Nodes.Take (
            From    => Sources,
            Element => Source_Index);

         Merge (
            Source => Nodes(Source_Index),
            Post   => Post (Source_Index),
            Target => Target_Node,
            Pre    => Pre  (Target_Index),
            Grew   => Grew);

         Pre_Grew := Pre_Grew or Grew;

      end loop;

      if Pre_Grew then

         -- The pre-value grew, so update the post-value:

         Transform (
            Pre  => Pre (Target_Index),
            Via  => Target_Node,
            Post => Post(Target_Index),
            Grew => Post_Grew);

         if Post_Grew then

            -- The post-value grew, so we must (by and by)
            -- update the pre-values of the successors:

            Add_Edges_To_Work (
               Source => Target_Node,
               Index  => Target_Index);

         end if;

      end if;

   end Process;


   --
   -- Iteration variables:
   --

   Post_Grew : Boolean;
   --
   -- If the initial Transform for a node gave a non-Bottom
   -- post-value.

   No_More_Work : Boolean;
   --
   -- If there is no more work to do (empty work-list).

   Working_Target : Node_Index_T;
   --
   -- The target node, from the work-list, on which to work.



   procedure Report_Exception (Occurrence : Ada.Exceptions.Exception_Occurrence)
   --
   -- Report an (unexpected) exception from this function.
   --
   is
      use Ada.Text_IO;
      use Ada.Exceptions;
 
      X_Info : constant String := Exception_Information (Occurrence);

      Target_Index : Node_Index_T;
      Source_Index : Node_Index_T;
      Empty : Boolean;
 
   begin

      Put_Line ("Exception: " & X_Info);

      Put_Line ("Work-lists:");

      loop

         Take_From_Work (Target_Index, Empty);

         exit when Empty;

         Put_Line (
              "Target Node "
            & Node_Index_T'Image (Target_Index));

         while not Source_Nodes.Empty (Work_Sources (Target_Index))
         loop

            Source_Nodes.Take (
               From    => Work_Sources (Target_Index),
               Element => Source_Index);

            Put_Line (
                 "   Source Node "
               & Node_Index_T'Image (Source_Index));
      
         end loop;

      end loop;

   end Report_Exception;


begin  -- Least_Fixpoint


   -- Initialise the pre- and post-values and the work-list:

   for N in Node_Index_T loop

      Initialize (
         Via  => Nodes (N),
         Pre  => Pre   (N),
         Post => Post  (N),
         Grew => Post_Grew);

      if Post_Grew then

         Add_Edges_To_Work (
            Source => Nodes(N),
            Index  => N);

      end if;

   end loop;


   -- Iterate:

   loop

      Take_From_Work (
         Index => Working_Target,
         Empty => No_More_Work);

      exit when No_More_Work;

      Process (Target_Index => Working_Target);

   end loop;

   -- Decide what to return and what to discard:

   if Afters then
      -- Discard the pre-values and return the post-values:

      for P in Pre'Range loop

         Finalize (Pre(P));

      end loop;

      return Post;

   else
      -- Discard the post-values and return the pre-values:

      for P in Pre'Range loop

         Finalize (Post(P));

      end loop;

      return Pre;

   end if;

exception

when X : Target_Nodes.Overflow =>

   Report_Exception (X);

   raise;

when X : Source_Nodes.Underflow =>

   Report_Exception (X);

   raise;

when X : Source_Nodes.Overflow =>

   Report_Exception (X);

   raise;

end Least_Fixpoint;
