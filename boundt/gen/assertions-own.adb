-- Assertions.Own (body)
--
-- Author: Niklas Holsti, Tidorum Ltd
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
-- $Revision: 1.20 $
-- $Date: 2015/10/24 20:05:45 $
--
-- $Log: assertions-own.adb,v $
-- Revision 1.20  2015/10/24 20:05:45  niklas
-- Moved to free licence.
--
-- Revision 1.19  2012-02-19 18:42:20  niklas
-- BT-CH-0231: Fix Constraint_Error in -trace parse, from BT-CH-0227.
--
-- Revision 1.18  2012-02-04 09:55:23  niklas
-- BT-CH-0227: Avoid GNAT bug re Unbounded_String in discr. record.
--
-- Revision 1.17  2011-09-01 21:28:09  niklas
-- Implemented Warn_File_Casing (-warn file_case).
--
-- Revision 1.16  2009-12-16 12:23:14  niklas
-- BT-CH-0196: Identify subprogram by offset from other subprogram.
--
-- Revision 1.15  2009-11-27 11:28:06  niklas
-- BT-CH-0184: Bit-widths, Word_T, failed modular analysis.
--
-- Revision 1.14  2009/03/27 13:57:12  niklas
-- BT-CH-0167: Assertion context identified by source-code markers.
--
-- Revision 1.13  2009/03/24 07:48:34  niklas
-- BT-CH-0166: String_Pool.Item_T for source-file and marker names.
--
-- Revision 1.12  2009/03/21 13:09:17  niklas
-- BT-CH-0165: Option -file_match for matching asserted file-names.
--
-- Revision 1.11  2009/03/20 18:19:28  niklas
-- BT-CH-0164: Assertion context identified by source-line number.
--
-- Revision 1.10  2009/01/18 08:00:44  niklas
-- Removed unused context clause.
--
-- Revision 1.9  2008/11/03 07:58:12  niklas
-- BT-CH-0155: Ignore assertions on absent subprograms.
--
-- Revision 1.8  2008/09/19 10:34:13  niklas
-- BT-CH-0144: Assertion "populations" work again, and more.
--
-- Revision 1.7  2008/07/14 19:16:54  niklas
-- BT-CH-0135: Assertions on "instructions".
--
-- Revision 1.6  2007/08/03 19:22:10  niklas
-- Extended Image (Actual_Part_T) to use Programs.Index_And_Name for
-- a subprogram, loop or jump and to show the index of a call.
-- Corrected Match (Part, Feature) to apply Applicable and Negated
-- also to features of type Marked, Labelled, Sourced and Executes.
-- Corrected Match (Part, Feature) to show the Answer in the trace.
-- Extended Match (Part, Feature) to check for duplicated subprograms.
--
-- Revision 1.5  2007/04/24 08:14:39  niklas
-- Added an Image function for Assertion_Subset_T that shows
-- the source location of each assertion in the subset.
--
-- Revision 1.4  2006/08/30 17:49:13  niklas
-- BT-CH-0027 corrects BT-NC-0160.
--
-- Revision 1.3  2006/08/22 13:12:00  niklas
-- Corrected function Match (Part, Feature, Goals) so that the
-- Within feature checks all containers of the part, not just the
-- immediate container (otherwise a call in a loop in a subprogram
-- would not match a call-assertion in a subprogram-block). Added
-- function Match_Some_Container, like Match_Immediate_Container,
-- to support this correction.
--
-- Revision 1.2  2006/05/29 11:22:32  niklas
-- BT-CH-0023.
--
-- Revision 1.1  2006/05/27 21:26:37  niklas
-- First version for BT-CH-0020.
--


with Ada.Characters.Handling;
with Ada.Strings.Unbounded;
with Arithmetic;
with Assertions.Opt;
with Assertions.Own.Text;
with Assertions.Source_Marks;
with Arithmetic;
with File_System;
with Flow.Calls;
with Flow.Computation;
with Flow.Show;
with Loops.Cells;
with Loops.Show;


package body Assertions.Own is


   use Ada.Strings.Unbounded;
   use String_Pool;
   use type Line_Number_T;


   function Image (Item : Source_T) return String
   is
   begin

      return
           Item.File_Name.all
         & Output.Field_Separator
         & Output.Image (Item.Line_Number);

   end Image;


   function Locus (Item : Source_T) return Output.Locus_T
   is
      use Output;
   begin

      return
         Locus (
            Statements =>
               + Locus (
                    Source_File => Item.File_Name.all,
                    Line_Number => Line_Number_T (Item.Line_Number)));

   end Locus;


   function Image (Item : Storage.Location_Ref) return String
   is
      use type Storage.Location_Ref;
   begin

      if Item /= null then

         return Storage.Image (Item.all);

      else

         return "unknown locations";

      end if;

   end Image;


   function To_String (Item : Name_String_T) return String
   is
   begin

      if Item = Name_String_T (String_Pool.Null_Item) then

         return "";

      else

         return String_Pool.To_String (String_Pool.Item_T (Item));

      end if;

   end To_String;


   function Image (Item : Variable_Name_T) return String
   is
   begin

      case Item.Kind is

      when Name =>

         return
               "variable """
             & To_String (Item.Name)
             & """ located at "
             & Image (Item.Location);

      when Address =>

         return
               "address """
             & To_String (Item.Address)
             & """ located at "
             & Image (Item.Location);

      end case;

   end Image;


   function Plain_Image (Item : Part_Name_T) return String
   is
      use type Processor.Code_Offset_T;

      Main : Name_String_T;
      -- The main part of the name, without the Offset.

   begin

      case Item.Kind is
      when Name    => Main := Item.Name;
      when Pattern => Main := Item.Pattern;
      when Address => Main := Item.Address;
      end case;

      if Item.Offset = Processor.Zero_Code_Offset then

         return To_String (Main);

      else

         return To_String (Main)
              & " offset "
              & Processor.Image (Item.Offset);

      end if;

   end Plain_Image;


   function Match (
      Part : Line_Number_T;
      Mark : Line_Number_T;
      Fuzz : Source_Fuzz_T)
   return Boolean
   is
      use type Line_Number_T;

      Yes : Boolean;

   begin

      Yes := Is_In (
         Value    => Arithmetic.Value_T (Part - Mark),
         Interval => Fuzz);

      if Opt.Trace_Matching then

         Output.Trace (
              "Matching part at line"
            & Line_Number_T'Image (Part)
            & " to asserted line"
            & Line_Number_T'Image (Mark)
            & " within "
            & Image (Fuzz)
            & ", result "
            & Boolean'Image (Yes));

      end if;

      return Yes;

   end Match;


   --
   ---   Calling contexts
   --


   function Applies (
      Context : Calling_Context_T;
      To      : Programs.Call_Path_T)
   return Boolean
   is
   begin

      return True;  -- TBA

   end Applies;


   --
   ---   Assertions
   --
   ---   Assertion sets and subsets
   --


   procedure Add (
      Assertion : in     Assertion_T;
      To        : in out Assertion_Bag_T)
   is
   begin

      Append (
         To    => To,
         Value => Assertion);

   end Add;


   function Image (Item : Assertion_Subset_T) return String
   is

      List : Unbounded_String;

   begin

      for I in Item'Range loop

         Append (List, Positive'Image (Item(I)));

      end loop;

      return To_String (List);

   end Image;


   function Image (
      Item   : Assertion_Subset_T;
      Within : Assertion_Bag_T)
   return String
   is

      List : Unbounded_String;
      -- The result.

      Member : Positive;
      -- The index of a member of the subset.

   begin

      for I in Item'Range loop

         Member := Item(I);

         Append (List,
              Positive'Image (Member)
            & '('
            & Image (Element (Within, Member).Source)
            & ')');

      end loop;

      return To_String (List);

   end Image;


   function Is_Member (
      Index  : Positive;
      Subset : Assertion_Subset_T)
   return Boolean
   is
   begin

      for S in Subset'Range loop

        if Subset(S) = Index then

           return True;

        end if;

      end loop;

      return False;

   end Is_Member;


   --
   ---   Container-Contained relations between parts
   --


   function Image (Item : Actual_Part_T) return String
   is
   begin

      case Item.Kind is

      when Program =>

         return "whole program";

      when Subprogram =>

         return "subprogram "
            & Programs.Index_And_Name (Item.Subprogram);

      when Luup =>

         return "loop #"
            & Loops.Loop_Index_T'Image (Loops.Index (Item.Luup))
            & " in "
            & Programs.Index_And_Name (Item.Subprogram);

      when Call_Static =>

         return "call #"
            & Programs.Call_Index_T'Image (Programs.Index (Item.Call))
            & ", "
            & Programs.Image (Item.Call);

      when Call_Dynamic =>

         return "dynamic call " & Flow.Image (Item.Boundable_Call.all);

      when Jump =>

         return "jump from step #"
            & Flow.Image (Flow.Tag (Flow.Source (Item.Jump.all)))
            & " in "
            & Programs.Index_And_Name (Item.Subprogram);

      when Instruction =>

         return "instruction in step #"
            & Flow.Image (Flow.Tag (Item.Step))
            & " in "
            & Programs.Index_And_Name (Item.Subprogram);

      end case;

   end Image;


   function Step (Part : Actual_Part_T) return Flow.Step_T
   is
   begin

      case Part.Kind is

      when Call_Static =>

         return Programs.Step (Part.Call);

      when Call_Dynamic =>

         return Flow.Source (Part.Boundable_Call.all);

      when Jump =>

         return Flow.Source (Part.Jump.all);

      when Instruction =>

         return Part.Step;

      when Extended_Part_Kind_T =>

         Output.Fault (
            Location => "Assertions.Own.Step",
            Text     => "Part is " & Image (Part));

         return Flow.No_Step;

      end case;

   end Step;


   function All_Loops (
      Within : Programs.Subprogram_T;
      Model  : Flow.Computation.Model_Handle_T)
   return Actual_Part_List_T
   --
   -- All the loops Within a subprogram as a part list.
   --
   is

      Luups : constant Loops.Loops_T := Programs.Loops_Of (Within);
      -- The loops in the subprogram.

      Parts : Actual_Part_List_T (1 .. Luups'Length);
      Last  : Natural := 0;
      -- The result will be Parts(1 .. Last = Luups'Length).

   begin

      for L in Luups'Range loop

         Last := Last + 1;

         Parts(Last) := (
            Kind       => Luup,
            Subprogram => Within,
            Model      => Model,
            Luup       => Luups(L));

      end loop;

      return Parts;

   end All_Loops;


   function All_Calls (
      From  : Programs.Subprogram_T;
      Model : Flow.Computation.Model_Handle_T)
   return Actual_Part_List_T
   --
   -- All the (static) calls From a subprogram as a part list.
   --
   is

      Calls : constant Programs.Call_List_T := Programs.Calls_From (From);
      -- The calls from the subprogram.

      Parts : Actual_Part_List_T (Calls'Range);
      -- The result.

   begin

      for P in Parts'Range loop

         Parts(P) := (
            Kind       => Call_Static,
            Subprogram => From,
            Model      => Model,
            Call       => Calls(P));

      end loop;

      return Parts;

   end All_Calls;


   function Unresolved_Calls (
      From  : Programs.Subprogram_T;
      Model : Flow.Computation.Model_Handle_T)
   return Flow.Dynamic_Edge_List_T
   --
   -- The unresolved calls From the subprogram, under the
   -- given Model or under the primitive model if Model is null.
   --
   is
      use type Flow.Computation.Model_Handle_T;
   begin

      if Model = null then
         -- Use the primitive model:

         return Flow.Calls.Unresolved_Calls (
            Within => Programs.Flow_Graph (From));

      else
         -- Use this Model:

         return Flow.Calls.Unresolved_Calls (Under => Model.all);

      end if;

   end Unresolved_Calls;


   function All_Dynamic_Calls (
      From  : Programs.Subprogram_T;
      Model : Flow.Computation.Model_Handle_T)
   return Actual_Part_List_T
   --
   -- All the dynamic calls From a subprogram, under a given Model,
   -- as a part list. If the Model is null, the dynamic calls
   -- under the primitive model are returned.
   --
   is

      Calls : constant Flow.Dynamic_Edge_List_T :=
         Unresolved_Calls (From, Model);
      -- The unresolved calls from the subprogram, under this Model
      -- or under the primitive model if Model is null.

      Parts : Actual_Part_List_T (Calls'Range);
      -- The result.

   begin

      for P in Parts'Range loop

         Parts(P) := (
            Kind           => Call_Dynamic,
            Subprogram     => From,
            Model          => Model,
            Boundable_Call => Calls(P));

      end loop;

      return Parts;

   end All_Dynamic_Calls;


   function Inner_Loops (
      Within  : Loops.Loop_T;
      Looping : Programs.Subprogram_T;
      Model   : Flow.Computation.Model_Handle_T)
   return Actual_Part_List_T
   --
   -- The immediate inner loops Within a given loop, Looping
   -- a given subprogram, as a part list.
   --
   is

      Inner : constant Loops.Loop_List_T := Loops.Loops_Contained_In (
         Loops => Programs.Loops_Of (Looping),
         Luup  => Within);
      -- The immediate inner loops.

      Parts : Actual_Part_List_T (Inner'Range);
      -- The result.

   begin

      for P in Parts'Range loop

         Parts(P) := (
            Kind       => Luup,
            Subprogram => Looping,
            Model      => Model,
            Luup       => Inner(P));

      end loop;

      return Parts;

   end Inner_Loops;


   function Looping_Calls (
      From   : Programs.Subprogram_T;
      Within : Loops.Loop_T;
      Model  : Flow.Computation.Model_Handle_T)
   return Actual_Part_List_T
   --
   -- The calls From a given subprogram and Within a given loop
   -- in this subprogram, as a list of parts. Note that calls in
   -- inner loops are also included, TBC.
   --
   is

      Calls : constant Programs.Call_List_T :=
         Programs.Calls_Within (Luup => Within, From => From);
      -- All the calls from the subprogram, within the loop.

      Parts : Actual_Part_List_T (Calls'Range);
      -- The result.

   begin

      for P in Parts'Range loop

         Parts(P) := (
            Kind       => Call_Static,
            Subprogram => From,
            Model      => Model,
            Call       => Calls(P));

      end loop;

      return Parts;

   end Looping_Calls;


   function Looping_Dynamic_Calls (
      From   : Programs.Subprogram_T;
      Within : Loops.Loop_T;
      Model  : Flow.Computation.Model_Handle_T)
   return Actual_Part_List_T
   --
   -- The dynamic calls From a given subprogram and Within a given loop
   -- in this subprogram, as a list of parts. Note that calls in
   -- inner loops are also included, TBC.
   --
   is

      All_Calls : constant Actual_Part_List_T :=
         All_Dynamic_Calls (From, Model);
      -- All the dynamic calls from the subprogram, under this Model.

      Parts : Actual_Part_List_T (1 .. All_Calls'Length);
      Last  : Natural := 0;
      -- The result will be Parts(1 .. Last).

      Call_Node : Flow.Node_T;
      -- The call-node for one of the dynamic calls.

   begin

      for A in All_Calls'Range loop

         Call_Node := Flow.Node_Containing (
            Step  => Flow.Source (All_Calls(A).Boundable_Call.all),
            Graph => Programs.Flow_Graph (From));

         if Loops.Contains (Luup => Within, Node => Call_Node) then
            -- This dynamic call is within this loop.

            Last := Last + 1;

            Parts(Last) := All_Calls(A);

         end if;

      end loop;

      return Parts(1 .. Last);

   end Looping_Dynamic_Calls;


   function Parts_Within (
      Container : Actual_Part_T;
      Kind      : Inner_Part_Kind_T)
   return Actual_Part_List_T
   is
   begin

      case Container.Kind is

      when Subprogram =>

         case Kind is

         when Luup =>

            return All_Loops (
               Within => Container.Subprogram,
               Model  => Container.Model);

         when Call_Static =>

            return All_Calls (
               From  => Container.Subprogram,
               Model => Container.Model);

         when Call_Dynamic =>

            return All_Dynamic_Calls (
               From  => Container.Subprogram,
               Model => Container.Model);

         when Jump =>

            return No_Parts;  -- TBA

         end case;

      when Luup =>

         case Kind is

         when Luup =>

            return Inner_Loops (
               Within  => Container.Luup,
               Looping => Container.Subprogram,
               Model   => Container.Model);

         when Call_Static =>

            return Looping_Calls (
               From   => Container.Subprogram,
               Within => Container.Luup,
               Model  => Container.Model);

         when Call_Dynamic =>

            return Looping_Dynamic_Calls (
               From   => Container.Subprogram,
               Within => Container.Luup,
               Model  => Container.Model);

         when Jump =>

            return No_Parts;  -- TBA

         end case;

      when Program | Call_Static | Call_Dynamic | Jump | Instruction =>
         -- Contains everything or nothing.

         Output.Fault (
            Location => "Assertions.Own.Parts_Within",
            Text     =>
                 "Container.Kind = "
               & Actual_Part_Kind_T'Image (Container.Kind));

         return No_Parts;

      end case;

   end Parts_Within;


   function Containers (
      Node   : Flow.Node_T;
      Within : Programs.Subprogram_T;
      Model  : Flow.Computation.Model_Handle_T)
   return Actual_Part_List_T
   --
   -- The containers of the given Node, Within the given subprogram.
   --
   is

      Luups : constant Loops.Loop_List_T :=
         Loops.Containing_Loops (
            Loops => Loops.All_Loops (Programs.Loops_Of (Within)),
            Node  => Node);
      -- The loops that contain this Node, if any, in top-down order.

      Parts : Actual_Part_List_T (1 .. Luups'Length + 1);
      Last  : Natural := 0;
      -- The result.

   begin

      for L in reverse Luups'Range loop

         Last := Last + 1;

         Parts(Last) := (
            Kind       => Luup,
            Subprogram => Within,
            Model      => Model,
            Luup       => Luups(L));

      end loop;

      Parts(Parts'Last) := (
         Kind       => Subprogram,
         Subprogram => Within,
         Model      => Model);

      return Parts;

   end Containers;


   function Containers (
      Step   : Flow.Step_T;
      Within : Programs.Subprogram_T;
      Model  : Flow.Computation.Model_Handle_T)
   return Actual_Part_List_T
   --
   -- The containers of the given Step, Within the given subprogram.
   --
   is
   begin

      return Containers (
         Node   => Flow.Node_Containing (Step, Programs.Flow_Graph (Within)),
         Within => Within,
         Model  => Model);

   end Containers;


   function Tail (List : Actual_Part_List_T)
   return Actual_Part_List_T
   --
   -- All of the List except the first element.
   --
   is
   begin

      return List(List'First + 1 .. List'Last);

   end Tail;


   function Containers (Part : Actual_Part_T)
   return Actual_Part_List_T
   --
   -- The parts that contain the given Part, in order from
   -- the immediate (innermost) container to the ultimate
   -- (outermost) container.
   --
   is
   begin

      case Part.Kind is

      when Program | Subprogram =>

         return No_Parts;

      when Luup =>

         return Tail (Containers (
            Node   => Loops.Head_Node (Part.Luup),
            Within => Part.Subprogram,
            Model  => Part.Model));
         --
         -- We take the Tail because to exclude the Luup itself.

      when Call_Static =>

         return Containers (
            Node   => Programs.Node (Part.Call),
            Within => Part.Subprogram,
            Model  => Part.Model);

      when Call_Dynamic =>

         return Containers (
            Step   => Flow.Source (Part.Boundable_Call.all),
            Within => Part.Subprogram,
            Model  => Part.Model);

      when Jump =>

         return Containers (
            Step   => Flow.Source (Part.Jump.all),
            Within => Part.Subprogram,
            Model  => Part.Model);

      when Instruction =>

         return Containers (
            Step   => Part.Step,
            Within => Part.Subprogram,
            Model  => Part.Model);

      end case;

   end Containers;


   function Match_Immediate_Container (
      Part  : Actual_Part_T;
      Whole : Parts_T;
      Goals : Goals_T)
   return Boolean
   --
   -- Whether the immediate container of the given Part matches
   -- the specification of the required Whole container, while
   -- matching the current Goals.
   --
   is

      All_Containers : constant Actual_Part_List_T := Containers (Part);
      -- All the containers of the given Part.

   begin

      if Opt.Trace_Matching then

         Output.Trace (
              Image (Part)
            & " has"
            & Natural'Image (All_Containers'Length)
            & " containers:");

         for C in All_Containers'Range loop

            Output.Trace (
                 Output.Image (C)
               & Output.Field_Separator
               & Image (All_Containers(C)));

         end loop;

      end if;

      return All_Containers'Length > 0
      and then Match (
         Part  => All_Containers(All_Containers'First),
         Parts => Whole,
         Goals => Goals);

   end Match_Immediate_Container;


   function Match_Some_Container (
      Part  : Actual_Part_T;
      Whole : Parts_T;
      Goals : Goals_T)
   return Boolean
   --
   -- Whether some container of the given Part matches the
   -- specification of the required Whole container, while
   -- matching the current Goals.
   --
   is

      All_Containers : constant Actual_Part_List_T := Containers (Part);
      -- All the containers of the given Part.

   begin

      if Opt.Trace_Matching then

         Output.Trace (
              Image (Part)
            & " has"
            & Natural'Image (All_Containers'Length)
            & " containers:");

         for C in All_Containers'Range loop

            Output.Trace (
                 Output.Image (C)
               & Output.Field_Separator
               & Image (All_Containers(C)));

         end loop;

      end if;

      for A in All_Containers'Range loop

         if Match (
            Part  => All_Containers(A),
            Parts => Whole,
            Goals => Goals)
         then

            return True;

         end if;

      end loop;

      if Opt.Trace_Matching then

         Output.Trace (
              "No container of "
            & Image (Part)
            & " matched the required whole.");

      end if;

      return False;

   end Match_Some_Container;


   function Match_Contained_Parts (
      Whole : Actual_Part_T;
      Parts : Parts_T;
      Goals : Goals_T)
   return Boolean
   --
   -- Whether the contained parts of the Whole match the specification
   -- of the required contained Parts and their population, while
   -- matching the current Goals.
   --
   is

      Contained : constant Actual_Part_List_T :=
         Parts_Within (Container => Whole, Kind => Parts.Kind);
      -- All the parts of the desired Kind, contained in the Whole.

      Tally : Natural := 0;
      -- The number of contained parts that match Parts.

      Result : Boolean;
      -- The result.

   begin

      if Opt.Trace_Matching then

         Output.Trace (
              "Matching"
            & Natural'Image (Contained'Length)
            & " parts in "
            & Image (Whole)
            & " to count "
            & Storage.Bounds.Image (
                 Item => Parts.Population,
                 Name => "#"));

      end if;

      for C in Contained'Range loop

         if Match (
            Part  => Contained(C),
            Parts => Parts,
            Goals => Goals)
         then

            Tally := Tally + 1;

         end if;

      end loop;

      Result := Storage.Bounds.Is_In (
         Value    => Arithmetic.Value_T (Tally),
         Interval => Parts.Population);

      if Opt.Trace_Matching then

         Output.Trace (
              "Tally of matching parts is"
            & Natural'Image (Tally)
            & ", result "
            & Boolean'Image (Result));

      end if;

      return Result;

   end Match_Contained_Parts;


   --
   ---   Matching program parts against features.
   --


   function Is_Member (
      Predicate : Part_Predicate_Ref;
      Of_Goals  : Goals_T)
   return Boolean
   is
   begin

      for G in Of_Goals'Range loop

         if Of_Goals(G) = Predicate then

            return True;

         end if;

      end loop;

      return False;

   end Is_Member;


   generic
      type Object (<>) is private;
      with function Match (
         Item    : Object;
         Feature : Feature_T;
         Goals   : Goals_T)
      return Boolean;
   function Generic_Match_Extension (
      Item      : Object;
      Predicate : Part_Predicate_Ref;
      Goals     : Goals_T)
   return Boolean;
   --
   -- Extends a Match function from a single Feature parameter
   -- to the conjunction of Features stated in a Part_Predicate.


   function Generic_Match_Extension (
      Item      : Object;
      Predicate : Part_Predicate_Ref;
      Goals     : Goals_T)
   return Boolean
   is
   begin

      for P in Predicate'Range loop

         if not Match (Item, Predicate(P), Goals) then
            -- This feature does not match, thus nor does the predicate.

            return False;

         end if;

      end loop;

      -- All the features matched, thus the predicate matches.

      return True;

   end Generic_Match_Extension;


   function Equal_Or_Tail (
      Major, Minor : String;
      Delimiter    : Character)
   return Boolean
   --
   -- Whether either Major = Minor exactly, or Minor is a suffix
   -- of Major that is separated from the rest of Major by a
   -- Delimiter character so that Major = X & Delimiter & Minor.
   --
   is

      Del : Positive;
      -- The possible separating Delimiter is Major(Del).

   begin

      if Major'Length <= Minor'Length then

         return Major = Minor;

      else

         Del := Major'Last - Minor'Length;

         return   Major(Del) = Delimiter
         and then Major(Del + 1 .. Major'Last) = Minor;

      end if;

   end Equal_Or_Tail;


   function Match_Code_Name (
      Code_Name    : String;
      Code_Address : Processor.Code_Address_T;
      Part_Name    : Part_Name_T)
   return Boolean
   --
   -- Whether the actual Name or Address of a code part matches
   -- the Part_Name specified in an assertion.
   --
   is
   begin

      case Part_Name.Kind is

      when Name =>

         if Opt.Trace_Matching then

            Output.Trace (
                 "Matching Code_Name """
               & Code_Name
               & " to Part_Name """
               & To_String (Part_Name.Name)
               & '"');

         end if;

         return Equal_Or_Tail (
            Major     => Code_Name,
            Minor     => To_String (Part_Name.Name),
            Delimiter => Part_Name.Delimiter);

      when Pattern =>

         return False;  -- TBA

      when Address =>

         return False;  -- TBA

      end case;

   end Match_Code_Name;


   function Match_Name (
      Subprogram : Programs.Subprogram_T;
      Name       : Part_Name_T)
   return Boolean
   --
   -- Whether the given Subprogram matches the specified Name.
   --
   is
   begin

      return Match_Code_Name (
         Code_Name =>
            Programs.Name (
               Subprogram => Subprogram,
               Qualified  => True,
               Delimiter  => Name.Delimiter),
         Code_Address => Programs.Entry_Address (Subprogram),
         Part_Name    => Name);

   end Match_Name;


   function Loop_Nest_Executes (
      Luup    : Loops.Loop_T;
      Address : Processor.Code_Address_T;
      Within  : Programs.Subprogram_T)
   return Boolean
   --
   -- Whether the loop, or a nested inner loop, contains a step that
   -- executes the instruction with the given address.
   --
   is
      use type Processor.Code_Address_T;

      Graph : constant Flow.Graph_T := Programs.Flow_Graph (Within);
      -- The flow-graph of the subprogram under analysis.

      Steps : constant Flow.Step_List_T := Loops.Steps_In (
         Luup   => Luup,
         Within => Graph);
      -- Steps in the given loop.

      Prime_Address : Processor.Code_Address_T;
      -- Address of a step in the given loop.

   begin

      for S in Steps'range loop
         -- Check all steps in the given loop (including nested loops).

         Prime_Address := Flow.Prime_Address(Steps(S));

         if Prime_Address = Address then
            -- The loop executes this address.

            return True;

         end if;

      end loop;

      return False;
      -- The instruction was not found at all.

   end Loop_Nest_Executes;


   function Loop_Itself_Executes (
      Luup    : Loops.Loop_T;
      Address : Processor.Code_Address_T;
      Within  : Programs.Subprogram_T)
   return Boolean
   --
   -- Whether the loop contains a step that executes the instruction
   -- with the given address, and the step is not in an inner loop.
   --
   is

      All_Loops : constant Loops.Loops_T := Programs.Loops_Of (Within);
      -- All loops in the given subprogram.

      Inner_Loops : constant Loops.Loop_List_T := Loops.Loops_Contained_In (
         Loops => All_Loops,
         Luup  => Luup);
      -- Inner loops contained in the given loop (one level down).

      Nest_Executes : Boolean;
      -- The loop nest executes the instruction.

      Inner_Executes : Boolean := False;
      -- Some inner loop executes the instruction.

   begin

      Nest_Executes := Loop_Nest_Executes (
         Luup    => Luup,
         Address => Address,
         Within  => Within);

      if Nest_Executes then
         -- The instruction is executed in this loop nest.
         -- Check if it is executed in some inner loop.

         for L in Inner_Loops'range loop
            -- Check all next-level inner loops in the given loop.

            Inner_Executes := Loop_Nest_Executes (
               Luup    => Inner_Loops(L),
               Address => Address,
               Within  => Within);

            exit when Inner_Executes;
            -- This property is false if the execution is in
            -- an inner loop.

         end loop;

      end if;

      return Nest_Executes and not Inner_Executes;

   end Loop_Itself_Executes;


   function Contains (
      Part    : Actual_Part_T;
      Address : Processor.Code_Address_T)
   return Boolean
   --
   -- Whether the Part contains (executes) the given Address.
   -- This checks the features Marked, Labelled, Sourced and
   -- Executes.
   --
   is
      use type Processor.Code_Address_T;
   begin

      case Part.Kind is

      when Program =>

         return False;   -- TBA (or not).

      when Subprogram =>

         return False;   -- TBA (or not).

      when Luup =>

         return Loop_Itself_Executes (
            Luup    => Part.Luup,
            Address => Address,
            Within  => Part.Subprogram);

      when Call_Static | Call_Dynamic =>

         return False;   -- TBA (or not).

      when Jump =>

         return False;   -- TBA (or not).

      when Instruction =>

         return Address = Flow.Prime_Address (Part.Step);

      end case;

   end Contains;


   subtype Subprogram_Identity_T is Feature_T (Kind => Subprogram_Identity);
   --
   -- A feature that defines the identity of the subprogram (and can
   -- thus sensibly apply only to subprogram parts).


   function Identical (
      Subprogram : Programs.Subprogram_T;
      Identity   : Subprogram_Identity_T)
   return Boolean
   --
   -- Whether the given Subprogram matches the Identity required.
   --
   is
      use type Processor.Code_Address_T;
      use type Programs.Subprogram_T;

      Yes : Boolean;
      -- The result.

   begin

      Yes := Subprogram = Identity.Subprogram;

      -- TBRemoved the following check:

      if not Yes
      and then (
           Programs.Entry_Address (Subprogram)
         = Programs.Entry_Address (Identity.Subprogram)
         or
           Programs.Name (Subprogram         , Qualified => True)
         = Programs.Name (Identity.Subprogram, Qualified => True))
      then

         Output.Fault (
            Location => "Assertions.Own.Identical (Subprogram, Identity)",
            Text     => "Duplicated subprogram objects.");

         Yes := True;

      end if;

      return Yes;

   end Identical;


   --
   ---   Source-position features
   --


   procedure Match_Cased_File_Names (
      Left, Right   : in     String;
      Yes,  Depends :    out Boolean)
   --
   -- Compares the two file names, Left and Right, to see if they match
   -- (are the same) for the purpose of matching assertions to program
   -- parts.
   --
   -- This depends on Opt.File_Matching but assumes that the other
   -- option, Opt.File_Casing, has been applied to Left and Right.
   --
   -- Left
   --    The file-name used in an assertion.
   -- Right
   --    The actual file name in the target program's symbol table.
   -- Yes
   --    Whether Left and Right match
   -- Depends
   --    Whether the result in Yes depends on Opt.File_Matching,
   --    that is, changing Opt.File_Matching would change Yes.
   --    This is valid only if Opt.Warn_File_Matching is chosen,
   --    and is otherwise False (and meaningless).
   --
   is

      Yes_Base, Yes_Full : Boolean;
      -- The results.

   begin

      if Opt.File_Matching = Base_Name
      or Opt.Warn_File_Matching
      then
         -- Compare the base-names only:

         Yes_Base := File_System.File_Name (Left )
                   = File_System.File_Name (Right);

      end if;

      if Opt.File_Matching = Full_Path
      or Opt.Warn_File_Matching
      then
         -- Compare the full names:

         Yes_Full := Left = Right;

      end if;

      -- The following case statement is retained to ensure
      -- that this subprogram fails to compile if the type
      -- File_Matching_T is modified.

      case Opt.File_Matching is
      when Base_Name => Yes := Yes_Base;
      when Full_Path => Yes := Yes_Full;
      end case;

      Depends := Opt.Warn_File_Matching and Yes_Base /= Yes_Full;

   end Match_Cased_File_Names;


   function Match_Source_File_Names (Left, Right : String)
   return Boolean
   --
   -- Whether the two file names, Left and Right, match (are the
   -- same) for the purpose of matching assertions to program parts.
   -- This depends on Opt.File_Matching and Opt.File_Casing.
   --
   -- Left
   --    The file-name used in an assertion.
   -- Right
   --    The actual file name in the target program's symbol table.
   --
   is

      Yes, Yes_Sensitive, Yes_Oblivious : Boolean;
      -- The results.

      Depends_Sensitive, Depends_Oblivious : Boolean;
      -- Whether the case-sensitive/case-oblivious matching
      -- depends on Opt.File_Matching.

   begin

      if Opt.File_Casing = Case_Sensitive
      or Opt.Warn_File_Matching
      then

         Match_Cased_File_Names (
            Left    => Left,
            Right   => Right,
            Yes     => Yes_Sensitive,
            Depends => Depends_Sensitive);

      end if;

      if Opt.File_Casing = Case_Oblivious
      or Opt.Warn_File_Matching
      then

         Match_Cased_File_Names (
            Left    => Ada.Characters.Handling.To_Lower (Left ),
            Right   => Ada.Characters.Handling.To_Lower (Right),
            Yes     => Yes_Oblivious,
            Depends => Depends_Oblivious);

      end if;

      -- The following case statement is retained to ensure
      -- that this subprogram fails to compile if the type
      -- File_Casing_T is modified.

      case Opt.File_Casing is
      when Case_Sensitive => Yes := Yes_Sensitive;
      when Case_Oblivious => Yes := Yes_Oblivious;
      end case;

      -- Warning for questionable cases:

      if (Opt.Warn_File_Matching
          and then (Depends_Sensitive or Depends_Oblivious))
      or (Opt.Warn_File_Casing
          and then Yes_Sensitive /= Yes_Oblivious)
      then

         Output.Warning (
              "File-names"
            & Output.Either (Yes, "", " do not")
            & " match"
            & Output.Field_Separator
            & Left
            & Output.Field_Separator
            & Right);

      end if;

      return Yes;

   end Match_Source_File_Names;


   function Match_Source_Files (
      Left  : Source_File_Name_T;
      Right : Source_File_Name_T)
   return Boolean
   --
   -- Like Match_Source_Files, above, but a null Left operand
   -- will match any Right operand.
   --
   -- Left
   --    The file-name used in assertion.
   -- Right
   --    The actual file name in the target program's symbol table.
   --
   is
      use Symbols;
   begin

      return  Left = Null_Name
      or else Match_Source_File_Names (Image (Left), Image (Right));

   end Match_Source_Files;


   function Within_Source_File (
      Part_Locus : Output.Locus_T;
      File_Name  : String)
   return Boolean
   --
   -- Whether an actual part lies within the source-code file with
   -- the given File_Name, as judged from the Part's Locus.
   --
   is
      use Output;

      Stmts : constant Statement_Range_T := Statements (Part_Locus);
      -- The source-line statement (ranges) in the locus.

   begin

      for N in 1 .. Number_Of_Sources (Stmts) loop

         if Match_Source_File_Names (File_Name, Source_File (Stmts, N)) then

            return True;

         end if;

      end loop;

      -- No Source_File in the Stmts has this File_Name.

      return False;

   end Within_Source_File;


   function Within_Source_File (
      Steps      : Flow.Step_List_T;
      File_Name  : String;
      Subprogram : Programs.Subprogram_T)
   return Boolean
   --
   -- Whether some of the Steps lie within the source-code file with
   -- the given File_Name, as judged from the Locus of the step.
   --
   -- The Steps are all assumed to lie in the given Subprogram, although
   -- the Subprogram is used only to supply line-number connections (via
   -- its symbol-table).
   --
   is

      Symbol_Table : constant Symbols.Symbol_Table_T :=
         Programs.Symbol_Table (Subprogram);

   begin

      for S in Steps'Range loop

         if Within_Source_File (
            Part_Locus => Flow.Show.Locus (Steps(S), Symbol_Table),
            File_Name  => File_Name)
         then

            return True;

         end if;

      end loop;

      -- None of the Steps has a locus in this File_Name.

      return False;

   end Within_Source_File;


   function Within_Source_File (
      Lines     : Symbols.Connection_Set_T;
      File_Name : String)
   return Boolean
   --
   -- Whether some of the Lines (connected to an actual part under
   -- matching) lie in the source-code file with the given File_Name.
   --
   -- Precondition: The Lines are all line-number connections.
   --
   is
   begin

      for L in Lines'Range loop

         if Match_Source_File_Names (
            File_Name,
            Symbols.Source_File_Of (Lines(L)))
         then

            return True;

         end if;

      end loop;

      -- None of the Lines has a source-file that matches File_Name.

      return False;

   end Within_Source_File;


   function Point_Lines (
      Step   : Flow.Step_T;
      Within : Programs.Subprogram_T)
   return Symbols.Connection_Set_T
   --
   -- The source-line connections for the Step, if any, and
   -- otherwise the Line_Before connections, possibly none.
   --
   is

      Address : constant Processor.Code_Address_T :=
         Flow.Prime_Address (Step);
      -- The single address for the Step.

      Table : constant Symbols.Symbol_Table_T :=
         Programs.Symbol_Table (Within);
      -- Access to the line-number/address connections.

      Lines_At_Point : constant Symbols.Connection_Set_T :=
         Symbols.Lines_For_Address (Address, Table);
      -- The lines connected to the address of the Step itself.

   begin

      if Lines_At_Point'Length > 0 then

         return Lines_At_Point;

      else

         return Symbols.Line_Before (Address, Table);

      end if;

   end Point_Lines;


   function Point_Lines (Part : Actual_Part_T)
   return Symbols.Connection_Set_T
   --
   -- The source-line connections for a "pointlike" Part.
   --
   -- Precondition: Part.Kind in Point_Part_Kind_T.
   --
   is
   begin

      return Point_Lines (Step (Part), Part.Subprogram);

   end Point_Lines;


   function Within_Source_File (
      Part      : Actual_Part_T;
      File_Name : String)
   return Boolean
   --
   -- Whether (any step in) this Part lies within the source-code
   -- file with the given File_Name.
   --
   is
   begin

      case Part.Kind is

      when Program =>

         return False;   -- TBA?

      when Subprogram =>

         return Within_Source_File (
            Part_Locus => Programs.Locus (Part.Subprogram),
            File_Name  => File_Name);

      when Luup =>

         return Within_Source_File (
            Steps => Loops.Steps_In (
               Luup   => Part.Luup,
               Within => Programs.Flow_Graph (Part.Subprogram)),
            File_Name  => File_Name,
            Subprogram => Part.Subprogram);

      when Point_Part_Kind_T =>

         return Within_Source_File (
            Lines     => Point_Lines (Part),
            File_Name => File_Name);

      end case;

   end Within_Source_File;


   --    Abstract class for reference lines to be matched with actual lines


   package Line_References is

      type Root_T is abstract tagged null record;
      --
      -- An abstract view of the reference source-code line(s) that
      -- should match the actual source-code lines connected to an
      -- actual program part. There will be two concrete types in
      -- this class: one for source-line numbers directly specified
      -- in assertions, and the other for source lines indirectly
      -- specified by source-code markers.
      --
      -- The significant properties of a Line_Reference_T are that
      -- it can be matched against a line-number connection, and
      -- checked for being "spanned" by a line-number interval in a
      -- a known source file.

      -- not overriding
      function Image (Item : Root_T) return String
      is abstract;
      --
      -- Describes the reference line for human understanding (tracing).

      -- not overriding
      function Connection_Matches_Line (
         Connection : Symbols.Connection_T;
         Line       : Root_T;
         Fuzz       : Source_Fuzz_T)
      return Boolean
      is abstract;
      --
      -- Whether this line-number Connection matches this reference Line,
      -- within the precision of the given fuzz.

      -- not overriding
      function Range_Spans_Line (
         File        : Source_File_Name_T;
         First, Last : Line_Number_T;
         Line        : Root_T)
      return Boolean
      is abstract;
      --
      -- Whether there is some representative of the reference Line in
      -- the interval First .. Last in the named source File.

   end Line_References;


   --    Directly specified reference line(s)


   type Source_Line_T is new Line_References.Root_T with record
      File : Source_File_Name_T;
      Line : Line_Number_T;
   end record;
   --
   -- Identifies a source-code Line in a source-code File.
   -- A null File means "any file".


   -- overriding
   function Image (Item : Source_Line_T) return String;


   -- overriding
   function Connection_Matches_Line (
      Connection : Symbols.Connection_T;
      Line       : Source_Line_T;
      Fuzz       : Source_Fuzz_T)
   return Boolean;


   -- overriding
   function Range_Spans_Line (
      File        : Source_File_Name_T;
      First, Last : Line_Number_T;
      Line        : Source_Line_T)
   return Boolean;


   -- Source_Line_T operation implementations:


   -- overriding
   function Image (Item : Source_Line_T) return String
   is
   begin

      return "line "
         & Output.Image (Item.Line)
         & " in file "
         & Symbols.Image (Item.File);

   end Image;


   -- overriding
   function Connection_Matches_Line (
      Connection : Symbols.Connection_T;
      Line       : Source_Line_T;
      Fuzz       : Source_Fuzz_T)
   return Boolean
   is
   begin

      return
         Match_Source_Files (
            Line.File,
            Symbols.Source_File_Of (Connection))
      and then
         Match (
            Part => Symbols.Line_Number_Of (Connection),
            Mark => Line.Line,
            Fuzz => Fuzz);

   end Connection_Matches_Line;


   -- overriding
   function Range_Spans_Line (
      File        : Source_File_Name_T;
      First, Last : Line_Number_T;
      Line        : Source_Line_T)
   return Boolean
   is
   begin

      return   Match_Source_Files (Line.File, File)
      and then First /= Output.No_Line_Number
      and then Last  /= Output.No_Line_Number
      and then Line.Line in First .. Last;

   end Range_Spans_Line;


   --   Reference line(s) specified indirectly by marks:


   type Marked_Line_T is new Line_References.Root_T with record
      File   : Source_File_Name_T;
      Marker : Marker_Name_T;
   end record;
   --
   -- Identifies those source-code Lines in a source-code File
   -- that are marked by a certain Marker name.
   -- A null File means "any file".


   -- overriding
   function Image (Item : Marked_Line_T) return String;


   -- overriding
   function Connection_Matches_Line (
      Connection : Symbols.Connection_T;
      Line       : Marked_Line_T;
      Fuzz       : Source_Fuzz_T)
   return Boolean;


   -- overriding
   function Range_Spans_Line (
      File        : Source_File_Name_T;
      First, Last : Line_Number_T;
      Line        : Marked_Line_T)
   return Boolean;


   -- Marked_Line_T operation implementations:


   -- overriding
   function Image (Item : Marked_Line_T) return String
   is
   begin

      return "mark "
         & To_String (Item.Marker)
         & " in file "
         & Symbols.Image (Item.File);

   end Image;


   function Min_Mark_Line (
      Part : Line_Number_T;
      Fuzz : Source_Fuzz_T)
   return Line_Number_T
   --
   -- The smallest "marked" line number that matches the actual
   -- line number connected to a Part, within the given fuzz.
   --
   -- The definition is that P - M is in Fuzz, where M is the
   -- marked line and P is the Part line. This is equivalent
   -- to M being in P - Fuzz, so the minimum value of M depends
   -- on the upper bound of Fuzz, Fuzz.Max.
   --
   is
      use Storage.Bounds;

      Mark : constant Source_Fuzz_T := (-Fuzz) + Arithmetic.Value_T (Part);
      -- The possible range for marked line numbers.

   begin

      if Known (Mark.Min) then

         return Line_Number_T (Arithmetic.Value_T'Max (1, Value (Mark.Min)));

      else

         return 1;

      end if;

   end Min_Mark_Line;


   function Max_Mark_Line (
      Part : Line_Number_T;
      Fuzz : Source_Fuzz_T)
   return Line_Number_T
   --
   -- The largest "marked" line number that matches the actual
   -- line number connected to a Part, within the given fuzz.
   --
   -- See Min_Marked_Line for discussion, mutatis mutandum.
   --
   is
      use Storage.Bounds;

      Mark : constant Source_Fuzz_T := (-Fuzz) + Arithmetic.Value_T (Part);
      -- The possible range for marked line numbers.

   begin

      if Known (Mark.Max) then

         return Line_Number_T (Arithmetic.Value_T'Min (
            Arithmetic.Value_T (Line_Number_T'Last),
            Value (Mark.Max)));

      else

         return Line_Number_T'Last;

      end if;

   end Max_Mark_Line;


   -- overriding
   function Connection_Matches_Line (
      Connection : Symbols.Connection_T;
      Line       : Marked_Line_T;
      Fuzz       : Source_Fuzz_T)
   return Boolean
   is

      Conn_Line : constant Line_Number_T :=
         Symbols.Line_Number_Of (Connection);
      -- The line number connected to the actual part.

      Marks : constant Source_Marks.Mark_List_T :=
         Source_Marks.Marks (
            Marker => Line.Marker,
            From   => Symbols.Source_File_Of (Connection),
            Min    => Min_Mark_Line (Conn_Line, Fuzz),
            Max    => Max_Mark_Line (Conn_Line, Fuzz));
      -- All the marks that may match.

   begin

      return Marks'Length > 0;
      --
      -- TBA: compare the kinds of parts that are marked and matched.

   end Connection_Matches_Line;


   -- overriding
   function Range_Spans_Line (
      File        : Source_File_Name_T;
      First, Last : Line_Number_T;
      Line        : Marked_Line_T)
   return Boolean
   is

      Marks : constant Source_Marks.Mark_List_T :=
         Source_Marks.Marks (
            Marker => Line.Marker,
            From   => File,
            Min    => First,
            Max    => Last);
      -- All the marks that may be spanned.

   begin

      return Marks'Length > 0;
      --
      -- TBA: compare the kinds of parts that are marked and matched.

   end Range_Spans_Line;


   --    Matching operations using Line_References.Root_T'Class


   function Match (
      Connections : Symbols.Connection_Set_T;
      Line        : Line_References.Root_T'Class;
      Fuzz        : Source_Fuzz_T)
   return Boolean
   --
   -- Whether any source-code line identified by any of the given
   -- line-number Connections matches (any representative of) the
   -- given reference Line(s) with a precision allowed by the given Fuzz.
   --
   is
   begin

      for C in Connections'Range loop

         if Opt.Trace_Matching then

            Output.Trace (
                 "Matching connection "
               & Symbols.Image (Connections(C))
               & " to "
               & Line_References.Image (Line)
               & " within "
               & Image (Fuzz));

         end if;

         if Line_References.Connection_Matches_Line (
               Connection => Connections(C),
               Line       => Line,
               Fuzz       => Fuzz)
         then

            return True;

         end if;

      end loop;

      -- No Connections match:

      return False;

   end Match;


   function Match (
      Step   : Flow.Step_T;
      Line   : Line_References.Root_T'Class;
      Fuzz   : Source_Fuzz_T;
      Within : Programs.Subprogram_T)
   return Boolean
   --
   -- Whether some (one or more) of the source-code lines connected to
   -- this Step match (any representative of) the specified reference
   -- Line(s), with a precision allowed by the given Fuzz.
   --
   -- The Step is assumed to lie Within the given subprogram, but
   -- the subprogram is used only for its symbol-table mappings.
   --
   is

      Step_Lines : constant Symbols.Connection_Set_T :=
         Symbols.Lines_For_Address (
            Address => Flow.Prime_Address (Step),
            Within  => Programs.Symbol_Table (Within));
      -- All the line-number connections for this Step.

   begin

      if Opt.Trace_Matching then

         Output.Trace (
              "Matching step #"
            & Flow.Step_Index_T'Image (Flow.Index (Step))
            & " with"
            & Natural'Image (Step_Lines'Length)
            & " connections to "
            & Line_References.Image (Line)
            & " within "
            & Image (Fuzz));

      end if;

      return Match (
         Connections => Step_Lines,
         Line        => Line,
         Fuzz        => Fuzz);

   end Match;


   function Match (
      Steps  : Flow.Step_List_T;
      Line   : Line_References.Root_T'Class;
      Fuzz   : Source_Fuzz_T;
      Within : Programs.Subprogram_T)
   return Boolean
   --
   -- Whether some (one or more) of the lines connected to some (one
   -- or more) of these Steps match (some representative of) the
   -- given reference Line(s), with a precision allowed by the given Fuzz.
   --
   -- The Steps are assumed to lie Within the given subprogram, but
   -- the subprogram is used only for its symbol-table mappings.
   --
   is
   begin

      -- Try each Step:

      for S in Steps'Range loop

         if Match (Steps(S), Line, Fuzz, Within) then

            return True;

         end if;

      end loop;

      -- No Steps match any of the Lines.

      return False;

   end Match;


   function Match (
      Luup     : Loops.Loop_T;
      Relation : Point_Relation_T;
      Line     : Line_References.Root_T'Class;
      Fuzz     : Source_Fuzz_T;
      Within   : Programs.Subprogram_T)
   return Boolean
   --
   -- Whether the given Luup has the expected Relation to (some
   -- representative of) the given reference Line(s),  with a
   -- precision allowed by the given Fuzz.
   --
   -- The choice of Relation influences the parts of the Luup that
   -- we compare to the Line:
   --
   -- Relation          Loop parts compared to the Line
   -- --------          -------------------------------
   -- loop after line   The loop head, or the Line_Before the loop head,
   --                   or any pre-head step, or the Line_Before that step.
   --
   -- loop on/at line   The loop head, or the Line_Before the loop head,
   --                   or any step in the pre-head nodes (but not the
   --                   Line_Before the pre-head steps).
   --
   -- loop before line  All steps in the loop, including the loop head
   --                   but not the pre-head steps/nodes.
   --
   is

      function Match_For_After (Step : Flow.Step_T) return Boolean
      --
      -- Match this Step, or the Line_Before this Step.
      --
      is
      begin

         return Match (
            Connections => Point_Lines (Step, Within),
            Line        => Line,
            Fuzz        => Fuzz);

      end Match_For_After;


      Graph : constant Flow.Graph_T := Programs.Flow_Graph (Within);
      -- The flow-graph of this subprogram.

      Pre_Heads : constant Flow.Step_List_T :=
         Loops.Pre_Head_Steps (Before => Luup, Within => Graph);
      -- The pre-head steps.

   begin  -- Match (Luup, Relation, ...)

      case Relation is

      when Before =>

         return Match (
            Steps  => Loops.Steps_In (Luup, Graph),
            Line   => Line,
            Fuzz   => Fuzz,
            Within => Within);

      when On | Exactly_On =>

         if Match_For_After (Loops.Head_Step (Luup)) then

            return True;

         else

            for P in Pre_Heads'Range loop

               if Match (
                     Steps => Flow.Steps_In (Flow.Node_Containing (
                        Step  => Pre_Heads(P),
                        Graph => Graph)),
                     Line   => Line,
                     Fuzz   => Fuzz,
                     Within => Within)
               then

                  return True;

               end if;

            end loop;

            return False;

         end if;

      when After =>

         if Match_For_After (Loops.Head_Step (Luup)) then

            return True;

         else

            for P in Pre_Heads'Range loop

               if Match_For_After (Pre_Heads(P)) then

                  return True;

               end if;

            end loop;

            return False;

         end if;

      end case;

   end Match;


   function Part_Spans_Line (
      Locus : Output.Locus_T;
      Line  : Line_References.Root_T'Class)
   return Boolean
   --
   -- Whether the "extended" part, described by its Locus, spans (some
   -- representative of) the given reference Line(s), in source-code terms.
   --
   is
      use Output;

      Stmts : constant Statement_Range_T := Statements (Locus);
      -- The source-line statement ranges in the locus.

   begin

      for N in 1 .. Number_Of_Sources (Stmts) loop

         if Line_References.Range_Spans_Line (
            File  => Source_File (Stmts, N),
            First => First (Stmts, N),
            Last  => Last  (Stmts, N),
            Line  => Line)
         then
            -- A match!

            return True;

         end if;

      end loop;

      -- No Source_File in the Stmts spans the Line.

      return False;

   end Part_Spans_Line;


   function Match (
      Part     : Actual_Part_T;
      Relation : Source_Relation_T;
      Line     : Line_References.Root_T'Class;
      Fuzz     : Source_Fuzz_T)
   return Boolean
   --
   -- Whether the source-code for this actual Part has the expected
   -- positional Relation to (some representative of) the given
   -- reference Line(s), within the given Fuzz.
   --
   is
      use type Flow.Step_List_T;
   begin

      case Part.Kind is

      when Program | Subprogram =>

         return False;   -- TBA?

      when Luup =>

         case Relation is

         when Point_Relation_T =>

            return Match (
               Luup     => Part.Luup,
               Relation => Relation,
               Line     => Line,
               Fuzz     => Fuzz,
               Within   => Part.Subprogram);

         when Contains =>

            return Match (
               Steps => Loops.Steps_In (
                  Luup   => Part.Luup,
                  Within => Programs.Flow_Graph (Part.Subprogram)),
               Line   => Line,
               Fuzz   => Fuzz,
               Within => Part.Subprogram);

         when Spans =>

            return Part_Spans_Line (
               Locus => Loops.Show.Locus (
                  Luup   => Part.Luup,
                  Within => Programs.Flow_Graph (Part.Subprogram),
                  Source => Programs.Symbol_Table (Part.Subprogram)),
               Line => Line);

         end case;

      when Point_Part_Kind_T =>

         if Relation not in Point_Relation_T then

            Output.Fault (
               Location => "Assertions.Own.Match (Part, Relation, Lines, ..)",
               Text     => "Relation " & Source_Relation_T'Image (Relation));

         end if;

         return Match (
            Connections => Point_Lines (Part),
            Line        => Line,
            Fuzz        => Fuzz);

      end case;

   end Match;


   --    Matching an Actual Part to a Source Position


   function Match (
      Part     : Actual_Part_T;
      Relation : Source_Relation_T;
      Position : Source_Position_T)
   return Boolean
   --
   -- Whether the source-code for this actual Part has the expected
   -- positional Relation to the given source Position.
   --
   is
   begin

      case Position.Kind is

      when Any =>
         -- Any place in this File.

         return Within_Source_File (Part, Symbols.Image (Position.File));

      when Line =>
         -- This Line_Number in this File (or any file if null).

         return Match (
            Part     => Part,
            Relation => Relation,
            Line     => Source_Line_T'(
                           File => Position.File,
                           Line => Position.Line_Number),
            Fuzz     => Position.Fuzz);

      when Mark =>
         -- Any line with this Marker in this File (or in any file).

         return Match (
            Part     => Part,
            Relation => Relation,
            Line     => Marked_Line_T'(
                           File   => Position.File,
                           Marker => Position.Marker),
            Fuzz     => Position.Fuzz);

      end case;

   end Match;


   function Match (
      Part    : Actual_Part_T;
      Feature : Feature_T;
      Goals   : Goals_T)
   return Boolean
   --
   -- Whether this actual Part matches the given Feature as
   -- part of the current Goals.
   --
   is
      use type Flow.Computation.Model_Handle_T;
      use type Programs.Subprogram_T;

      Yes : Boolean := False;
      -- The result before considering Feature.Negated or Applicable.

      Applicable : Boolean := True;
      -- Whether this kind of Feature is applicable (supported) for
      -- this kind of Part.

      Answer : Boolean;
      -- The overall answer.


      procedure Not_Applicable
      --
      -- Remarks that this feature is not applicable to this kind
      -- of part.
      --
      is
      begin

         Output.Note (
              "A "
            & Part_Kind_T'Image (Part.Kind)
            & " does not have a """
            & Feature_Kind_T'Image (Feature.Kind)
            & """ feature.");

         Applicable := False;

      end Not_Applicable;


   begin  -- Match

      if Opt.Trace_Matching then

         Output.Trace (
              "Matching "
            & Image (Part)
            & " against feature:");

         Text.Put (
            Item   => Feature,
            Indent => 6,
            Outer  => No_Goals);

      end if;

      case Feature.Kind is

      when Subprogram_Identity =>

         case Part.Kind is

         when Subprogram =>

            Yes := Identical (Part.Subprogram, Feature);

         when Call_Static =>

            Yes := Programs.Callee (Part.Call) = Feature.Subprogram;

         when others =>

            Not_Applicable;

         end case;

      when Subprogram_Absent =>

         case Part.Kind is

         when Subprogram | Call_Static =>

            Yes := False;

         when others =>

            Not_Applicable;

         end case;

      when Named =>

         case Part.Kind is

         when Subprogram =>

            Yes := Match_Name (
               Subprogram => Part.Subprogram,
               Name       => Feature.Name);

         when Call_Static =>

            Yes := Match_Name (
               Subprogram => Programs.Callee (Part.Call),
               Name       => Feature.Name);

         when others =>

            Not_Applicable;

         end case;

      when Contains =>

         if not Storage.Bounds.Known (Feature.Parts.Population) then
            -- Any number is acceptable.

            Yes := True;

         else
            -- Have to check the number of contained parts.

            Yes := Match_Contained_Parts (
               Whole => Part,
               Parts => Feature.Parts.all,
               Goals => Goals);

         end if;

      when Within =>

         Yes := Match_Some_Container (
            Part  => Part,
            Whole => Feature.Whole.all,
            Goals => Goals);

      when Uses =>

         case Part.Kind is

         when Luup =>

            if Part.Model /= null then

               Yes := Loops.Cells.Is_Used (
                  Location => Feature.Variable.Location.all,
                  By       => Part.Luup,
                  Under    => Part.Model.all);

            else
               -- No model yet; TBA use primitive model.

               Yes := False;

            end if;

         when Call_Static | Call_Dynamic =>

            Yes := False; -- TBA

         when others =>

            Not_Applicable;

         end case;

      when Defines =>

         case Part.Kind is

         when Luup =>

            if Part.Model /= null then

               Yes := Loops.Cells.Is_Defined (
                  Location => Feature.Variable.Location.all,
                  By       => Part.Luup,
                  Under    => Part.Model.all);

            else
               -- No model yet; TBA use primitive model.

               Yes := False;

            end if;

         when Call_Static | Call_Dynamic =>

            Yes := False; -- TBA

         when others =>

            Not_Applicable;

         end case;

      when Eternal =>

         case Part.Kind is

         when Luup =>

            if Part.Model /= null then

               Yes := Flow.Computation.Is_Eternal (
                  Luup  => Part.Luup,
                  Under => Part.Model.all);

            else

               Yes := Loops.Eternal (Part.Luup);

            end if;

         when others =>

            Not_Applicable;

         end case;

      when Labelled =>

         Yes := Contains (Part, Feature.Label_Address);

      when Sourced =>

         Yes := Match (Part, Feature.Source_Rel, Feature.Source_Pos);

      when Executes =>

         Yes := Contains (Part, Feature.Address);

      end case;

      -- Answer based on Yes, Negated, Applicable:

      Answer := Applicable and (Yes xor Feature.Negated);

      if Opt.Trace_Matching then

         Output.Trace (
              "Answer "
            & Boolean'Image (Answer)
            & ", applicable "
            & Boolean'Image (Applicable)
            & ", holds "
            & Boolean'Image (Yes));

      end if;

      return Answer;

   end Match;


   function Match_Part is new Generic_Match_Extension (
      Object => Actual_Part_T,
      Match  => Match);

   function Match (
      Part      : Actual_Part_T;
      Predicate : Part_Predicate_Ref;
      Goals     : Goals_T)
   return Boolean
   renames Match_Part;


   function Match (
      Part  : Actual_Part_T;
      Parts : Parts_T;
      Goals : Goals_T)
   return Boolean
   is

      Result : Boolean;

   begin

      if Opt.Trace_Matching then

         Output.Trace (
              "Match "
            & Image (Part)
            & " to "
            & Text.Image (Parts));

      end if;

      if Part.Kind /= Parts.Kind then
         -- Not even close.

         Result := False;

      elsif Is_Member (Parts.Predicate, Goals) then
         -- Checking this Predicate is already a goal, so
         -- we must not start checking it again.

         Result := True;
         --
         -- This may be wrong... but then the existing
         -- goal will show it.

      else
         -- This can be taken as a new goal.

         Result := Match (
            Part      => Part,
            Predicate => Parts.Predicate,
            Goals     => Goals & Parts.Predicate);

      end if;

      if Opt.Trace_Matching then

         Output.Trace ("Result " & Boolean'Image (Result));

      end if;

      return Result;

   end Match;


   function Match (
      Subprogram : Programs.Subprogram_T;
      Model      : Flow.Computation.Model_Handle_T;
      Predicate  : Part_Predicate_Ref;
      Goals      : Goals_T)
   return Boolean
   is
   begin

      return Match (
         Part    => (
            Kind       => Own.Subprogram,
            Subprogram => Subprogram,
            Model      => Model),
         Predicate => Predicate,
         Goals     => Goals);

   end Match;


   type Containment_T is (Denied, Silent, Required);
   --
   -- The answer to a question of the form: What does a part-predicate
   -- say about the possibility that these parts are contained
   -- within a given actual part?
   --
   -- Denied
   --    The predicate denies (prohibits) such containment.
   -- Silent
   --    The predicate has nothing to say about a possible
   --    containment relationship between these parts and the
   --    given actual part.
   -- Required
   --    The predicate requires the parts to be contained within
   --    the given actual part (or another actual part that also
   --    matches the requirements of the predicate).
   --
   -- For example, if the question is "is part P required to be
   -- contained in subprogram Foo", then the answers for some example
   -- predicates are as follows, where Bar is some other subprogram
   -- (or some subprogram predicate that does not match Foo), and Q
   -- is some other part that can contain P:
   --
   -- Predicate                            Answer    Note
   -- ---------                            ------    ----
   -- P within Foo                         Required
   -- P within Bar                         Denied    0
   -- P within (not Foo)                   Denied    0
   -- P within (not Bar)                   Silent
   -- P not within Foo                     Denied    1
   -- P not within Bar                     Silent    2
   --
   -- P within (Q within Foo)              Required  3
   -- P within (Q not within Foo)          Denied    4
   -- P not within (Q within Foo)          Silent    5
   -- P not within (Q not within Foo)      Silent    6
   --
   -- P within (Q within Bar)              Denied    4
   -- P within (Q not within Bar)          Silent    7
   -- P not within (Q within Bar)          Silent    7
   -- P not within (Q not within Bar)      Silent    8
   --
   -- Notes:
   -- 0. Assuming that a subprogram cannot be contained
   --    within another subprogram.
   -- 1. negate Required     = Denied.
   -- 2. negate Denied       = Silent.
   -- 3. within Required     = Required.
   -- 4. within Denied       = Denied.
   -- 5. not within Required = Silent, because the other predicates
   --    on Q may be false and thus satisfy "P not within (Q..)" even
   --    if Q is within Foo.
   -- 6. not within Denied   = Silent.
   -- 7. within Silent       = Silent.
   -- 8. not within Silent   = Silent.
   --
   -- This means that:
   --
   -- > Negation of a "Within subprogram" feature changes Required
   --   to Denied and Denied to Silent. The fate of Silent under
   --   negation is not shown in the examples, but it is natural
   --   to use (negate Silent) = Silent.
   --
   -- > Positive transitivity (P within (Q..)) leaves containment
   --   unchanged: within Required = Required, within Denied = Denied,
   --   within Silent = Silent.
   --
   -- > Negated transitivity (P not within (Q..)) results in Silent
   --   when Q is not a subprogram.


   procedure Conjoin (
      Item      : in     Containment_T;
      Feature   : in     Feature_T;
      Candidate : in     Programs.Subprogram_T;
      Total     : in out Containment_T)
   --
   -- Conjoins (combines conjunctively) the Item, resulting from
   -- one Feature that may say something about the Candidate as
   -- the possible container of some parts, with the Total result
   -- of other such features.
   --
   -- If Item and Total are contradictory, an error message is
   -- emitted and the Total is returned unchanged.
   --
   is
   begin

      if Item = Silent then
         -- No effect on the Total.

         null;

      elsif Total = Silent then
         -- First definition of the Total.

         Total := Item;

      elsif Item /= Total then

         Output.Error (
            Locus => Locus (Feature.Source),
            Text  =>
                 "Contradictory containment"
               & Programs.Name (Candidate)
               & Output.Field_Separator
               & Output.Image (Programs.Locus (Candidate)));

      end if;

   end Conjoin;


   function Subprogram_Container (
      Predicate : Part_Predicate_T;
      Candidate : Programs.Subprogram_T)
   return Containment_T
   --
   -- What the Predicate, which is the predicate of a "Within subprogram"
   -- feature, says about the actual Candidate as a possible container.
   -- At present, only Subprogram_Identity and Subprogram_Absent features
   -- are considered, and there is no recursion to any other containment
   -- level, whether higher or lower.
   --
   is

      Result : Containment_T := Silent;
      -- The result. Initially we known nothing.

      Id : Containment_T;
      -- The result of a single Subprogram_Identity feature.

   begin

      for P in Predicate'Range loop

         case Predicate(P).Kind is

         when Subprogram_Identity =>

            -- What does this feature say:

            if Identical (Candidate, Predicate(P)) then
               -- This Candidate is named as a container,
               -- positively or negatively.

               if Predicate(P).Negated then
                  -- Not within the Candidate.

                  Id := Denied;

               else
                  -- Within the Candidate.

                  Id := Required;

               end if;

            else
               -- Some other subprogram is named as a container,
               -- positively or negatively.

               if Predicate(P).Negated then
                  -- Not within some other subprogram.

                  Id := Silent;

               else
                  -- Within some other subprogram /= the Candidate.

                  Id := Denied;

               end if;

            end if;

         when Subprogram_Absent =>

            if Predicate(P).Negated then
               -- Not within a nonexistent subprogram.

               Id := Silent;

            else
               -- Within a nonexistent subprogram.

               Id := Denied;

            end if;

         when others =>

            Id := Silent;

         end case;

         if Opt.Trace_To_Be_Mapped then

            Output.Trace (
                 "Subprogram container "
               & Programs.Name (Candidate)
               & Output.Field_Separator
               & Feature_Kind_T'Image (Predicate(P).Kind)
               & " feature #"
               & Positive'Image (P)
               & " at "
               & Image (Predicate(P).Source)
               & Output.Field_Separator
               & Containment_T'Image (Id));

         end if;

         -- Combine this feature with earlier features:

         Conjoin (
            Item      => Id,
            Feature   => Predicate(P),
            Candidate => Candidate,
            Total     => Result);

      end loop;

      return Result;

   end Subprogram_Container;


   function To_Be_Mapped (
      Assertion : Assertion_T;
      Onto      : Programs.Subprogram_T)
   return Boolean
   is

      function Within_Onto (Predicate : Part_Predicate_T)
      return Containment_T
      --
      -- What the "Within subprogram" features in the Predicate
      -- say about the given Onto subprogram as a container of
      -- the parts in Assertion. This function is recursive
      -- towards higher containment levels.
      --
      is

         Result : Containment_T := Silent;
         -- The result. Nothing known so far.

         Item : Containment_T;
         -- The result from one "within" feature.

      begin

         for P in Predicate'Range loop

            if Predicate(P).Kind = Within then

               case Predicate(P).Whole.Kind is

               when Program =>
                  -- No new information.

                  Item := Silent;

               when Subprogram =>
                  -- This feature identifies the containing subprograms.

                  Item := Subprogram_Container (
                     Predicate => Predicate(P).Whole.Predicate.all,
                     Candidate => Onto);

                  if Predicate(P).Negated then
                     -- Not within (subprogram...)

                     case Item is
                     when Denied   => Item := Required;
                     when Silent   => null;
                     when Required => Item := Denied;
                     end case;

                  end if;

               when others =>
                  -- This feature specifies some other kind of container.
                  -- At present, only Luup is possible here, but we
                  -- accept other kinds of container, too.

                  if Predicate(P).Negated then
                     -- Not within (Q..).

                     Item := Silent;

                  else
                     -- Within (Q..).

                     Item := Within_Onto (Predicate(P).Whole.Predicate.all);

                  end if;

               end case;

               if Opt.Trace_To_Be_Mapped then

                  Output.Trace (
                       "Within "
                     & Programs.Name (Onto)
                     & Output.Field_Separator
                     & Feature_Kind_T'Image (Predicate(P).Kind)
                     & " feature #"
                     & Positive'Image (P)
                     & " at "
                     & Image (Predicate(P).Source)
                     & Output.Field_Separator
                     & Containment_T'Image (Item));

               end if;

               Conjoin (
                  Item      => Item,
                  Feature   => Predicate(P),
                  Candidate => Onto,
                  Total     => Result);

            end if;

         end loop;

         return Result;

      end Within_Onto;


      Within : Containment_T;
      -- The result of Within_Onto.


   begin  -- To_Be_Mapped

      case Assertion.Parts.Kind is

      when Mappable_Part_Kind_T =>
         -- A mappable kind of part.

         Within := Within_Onto (Assertion.Parts.Predicate.all);

         if Opt.Trace_To_Be_Mapped then

            Output.Trace (
                 "Assertion at "
               & Image (Assertion.Source)
               & " contained within "
               & Programs.Name (Onto)
               & Output.Field_Separator
               & Containment_T'Image (Within));

         end if;

         return Within /= Denied;
         --
         -- If the answer is Required, obviously this Assertion must
         -- be mapped Onto this subprogram, analogously for Denied.
         -- If the answer is Silent, the Assertion is a global one
         -- and should also be mapped, although the Assertion may
         -- contain other features that restrict the mapping upon
         -- a later precise matching.

      when others =>
         -- Not mappable.

         return False;

      end case;

   end To_Be_Mapped;


   function To_Be_Mapped (
      From : Assertion_Bag_T;
      Onto : Programs.Subprogram_T)
   return Assertion_Subset_T
   is

      List : Assertion_Subset_T (1 .. Length (From));
      Num  : Natural := 0;
      -- The growing subset is List(1..Num).

   begin

      if Opt.Trace_To_Be_Mapped then

         Output.Trace (
              "Picking assertions to be mapped onto "
            & Programs.Name (Onto));

      end if;

      for F in First (From) .. Last (From) loop

         if To_Be_Mapped (Element (From, F), Onto) then

            Num := Num + 1;
            List(Num) := F;

          end if;

      end loop;

      return List(1 .. Num);

   end To_Be_Mapped;


   function "+" (Part : Part_Kind_T) return Part_Kinds_T
   is
      Set : Part_Kinds_T := (others => False);
   begin

      Set(Part) := True;

      return Set;

   end "+";


   function "+" (Fact : Fact_Kind_T) return Fact_Kinds_T
   is
      Set : Fact_Kinds_T := (others => False);
   begin

      Set(Fact) := True;

      return Set;

   end "+";


end Assertions.Own;
