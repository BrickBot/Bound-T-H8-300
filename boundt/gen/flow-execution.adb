-- Flow.Execution (body)
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
-- $Revision: 1.13 $
-- $Date: 2015/10/24 20:05:48 $
--
-- $Log: flow-execution.adb,v $
-- Revision 1.13  2015/10/24 20:05:48  niklas
-- Moved to free licence.
--
-- Revision 1.12  2008-09-24 08:38:52  niklas
-- BT-CH-0146: Assertions on "loop starts <bound> times".
-- BT-CH-0146: Loop-repeat assertions set both lower and upper bound.
-- BT-CH-0146: Report locations of contradictory "count" assertions.
-- BT-CH-0146: Contradictory "count" assertions imply infeasibility.
--
-- Revision 1.11  2008/07/28 19:23:45  niklas
-- BT-CH-0140: Detect contradictory execution-count bounds.
--
-- Revision 1.10  2008/07/14 19:16:56  niklas
-- BT-CH-0135: Assertions on "instructions".
--
-- Revision 1.9  2007/12/17 13:54:36  niklas
-- BT-CH-0098: Assertions on stack usage and final stack height, etc.
--
-- Revision 1.8  2007/03/29 15:18:02  niklas
-- BT-CH-0056.
--
-- Revision 1.7  2007/01/25 21:25:15  niklas
-- BT-CH-0043.
--
-- Revision 1.6  2005/10/20 15:27:19  niklas
-- Using function Flow.Effort (Step).
--
-- Revision 1.5  2005/10/09 08:10:21  niklas
-- BT-CH-0013.
--
-- Revision 1.4  2005/08/24 10:16:12  niklas
-- Added the inquiry functions Total_Count (Node_List) and
-- Total_Count (Edge_List) to support the Analysis Workbench.
--
-- Revision 1.3  2005/02/16 21:11:44  niklas
-- BT-CH-0002.
--
-- Revision 1.2  2004/04/28 19:02:13  niklas
-- First Tidorum version.
-- Moved the operations Count_Memory_Traffic and Work from the parent
-- package to this child package, where they are more at home.
-- Added operations Node_Times and Edge_Times to compute the total
-- execution time per node and per (node-) edge.
--
-- Revision 1.1  2001/02/19 09:27:33  holsti
-- Infinite and Bounded added.
--


with Output;


package body Flow.Execution is


   function Image (Item : Count_T) return String
   is
   begin

      return Output.Image (Item);

   end Image;


   function "*" (
      Left  : Count_T;
      Right : Processor.Time_T)
   return Processor.Time_T
   is
      use type Processor.Time_T;
   begin

      return Processor.Time_T (Left) * Right;

   end "*";


   function Bounded (Count : Count_T) return Boolean
   is
   begin

      return Count < Infinite;

   end Bounded;


   function Bounded (Bound : Bound_T) return Boolean
   is
   begin

      return Bound.Min > 0
          or Bound.Max < Infinite;

   end Bounded;


   function Image (Item : Bound_T) return String
   is
   begin

      if Bounded (Item.Max) then

         return Image (Item.Min)
              & ".."
              & Image (Item.Max);

      else

         return Image (Item.Min) & "..";

      end if;

   end Image;


   function Void (Item : Bound_T) return Boolean
   is
   begin

      return Item.Min > Item.Max;

   end Void;


   function Is_In (Count : Count_T; Bound : Bound_T)
   return Boolean
   is
   begin

      return Count in Bound.Min .. Bound.Max;

   end Is_In;


   function "and" (Left, Right : Bound_T) return Bound_T
   is
   begin

      return (Min => Count_T'Max (Left.Min, Right.Min),
              Max => Count_T'Min (Left.Max, Right.Max));

   end "and";


   function Count (
      Node   : Node_T;
      Within : Counts_T)
   return Count_T
   is
   begin

      return Within.Node(Index (Node));

   end Count;


   function Total_Count (
     Nodes  : Node_List_T;
     Within : Counts_T)
   return Count_T
   is

      Total : Count_T := 0;
      -- The total to be.

   begin

      for N in Nodes'Range loop

         Total := Total + Within.Node(Index (Nodes(N)));

      end loop;

      return Total;

   end Total_Count;


   function Total_Count (
     Edges  : Edge_List_T;
     Within : Counts_T)
   return Count_T
   is

      Total : Count_T := 0;
      -- The total to be.

   begin

      for E in Edges'Range loop

         Total := Total + Within.Edge(Index (Edges(E)));

      end loop;

      return Total;

   end Total_Count;


   --
   --    Memory traffic counts
   --


   procedure Count_Memory_Traffic (
      From   : in     Node_T;
      Reads  :    out Natural;
      Writes :    out Natural)
   is

      Steps : constant Step_List_T := Steps_In (From);
      -- The steps in the node.

      Effort : Processor.Effort_T;
      -- The effort a step.

   begin

      Reads  := 0;
      Writes := 0;

      for S in Steps'Range loop

         Effort := Flow.Effort (Steps(S));

         Reads  := Reads  + Processor.Memory_Reads  (Effort);
         Writes := Writes + Processor.Memory_Writes (Effort);

      end loop;

   end Count_Memory_Traffic;


end Flow.Execution;
