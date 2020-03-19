-- Programs.Execution.Show (decl)
--
-- Display subprogram execution bounds in various forms.
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
-- $Revision: 1.11 $
-- $Date: 2015/10/24 19:36:51 $
--
-- $Log: programs-execution-show.ads,v $
-- Revision 1.11  2015/10/24 19:36:51  niklas
-- Moved to free licence.
--
-- Revision 1.10  2011-08-31 04:23:34  niklas
-- BT-CH-0222: Option registry. Option -dump. External help files.
--
-- Revision 1.9  2008/02/18 13:24:10  niklas
-- BT-CH-0111: Processor-specific info in execution bounds.
--
-- Revision 1.8  2007/12/17 13:54:39  niklas
-- BT-CH-0098: Assertions on stack usage and final stack height, etc.
--
-- Revision 1.7  2007/08/03 17:51:55  niklas
-- Moved the Show procedures for Subprograms and Calls, with no use
-- of execution bounds, to the recreated package Programs.Show.
--
-- Revision 1.6  2007/03/18 12:50:39  niklas
-- BT-CH-0050.
--
-- Revision 1.5  2007/01/13 13:51:06  niklas
-- BT-CH-0041.
--
-- Revision 1.4  2005/06/28 08:38:24  niklas
-- Changes for User Manual version 3 as follows.
-- Added the Feature_T items General, Usage and Luups.
-- Added Full_View for use in "-show full".
-- Added function Some_Content to show if some real output is chosen.
-- Added type Sequence_T and related stuff to number the displayed
-- execution bounds sequentially and to keep track of which bounds
-- have already been shown and under which number (back reference).
-- Modified and described several Show procedures.
--
-- Revision 1.3  2005/02/16 21:11:47  niklas
-- BT-CH-0002.
--
-- Revision 1.2  2004/05/01 10:49:56  niklas
-- First Tidorum version.
-- Added options Spaces and Callers to various procedures.
-- Added procedures Show_Stack and Show_Stack_Path.
-- Added display of inverse call tree (Show_Paths_To) leading to unbounded
-- subprograms. This implementation uses linear search and is rather slow.
-- Added display of reducible/irreducible nature of subprogram.
-- Added display of eternal nature of loop.
-- Added detail on state of path bounds, time bounds, space bounds.
-- Added display of space (stack) bounds.
-- Removed Program_T parameters where the target program can now be
-- accessed via the Execution Bounds to be shown.
--
-- Revision 1.1  2003/03/11 08:28:57  holsti
-- First version renamed to be a child of Programs.Execution.
--


with Ada.Text_IO;


package Programs.Execution.Show is


   --
   ---   Features to view
   --


   type Feature_T is (
      General,
      Bounds_Item,
      Model,
      Stacks,
      Times,
      Loops_Item,
      Counts,
      Spaces,
      Cells,
      Proc,
      Deeply,
      Callers,
      Full);
   --
   -- The optional features that can be shown for execution bounds, and
   -- some secondary options for further control over the output.
   --
   -- General
   --    General information: index of execution bounds object,
   --    call path, state of analysis (bounded / unbounded), etc.
   -- Bounds_Item
   --    The bounds on resource usage: time and/or space, depending
   --    on the requested analysis.
   -- Model
   --    The computation model.
   -- Stacks
   --    The final stack height (net push-pop effect on the stack).
   -- Times
   --    The WCET of each node in the flow-graph.
   -- Luups
   --    The loop repetition bounds (computed or asserted) and
   --    other loop properties (eternal loops etc).
   -- Counts
   --    The execution counts of nodes and edges.
   -- Spaces
   --    The memory usage bounds (stacks etc.).
   -- Cells
   --    The input, basis and output cells and entry bounds.
   -- Proc
   --    Information specific to the target processor.
   -- Deeply
   --    The nested bounds on calls.
   -- Callers
   --    All paths from a root to the subprogram.
   -- Full
   --    A meta-item to invoke a "full" set of other items.
   --   See Full_View below.


   subtype Real_Feature_T is Feature_T range General .. Proc;
   --
   -- The features that represent some real results of the analysis.
   -- Omits the features Deeply and Callers that are considered
   -- secondary output options. Omits the feature Full that represents
   -- a bundle of other output options.


   type View_T is array (Feature_T) of Boolean;
   --
   -- Selects the features to be shown by True values.
   --
   -- The value of the Full element is irrelevant, as this element
   -- is used (in the option parsing modules) just to set some of
   -- the other elements.


   Minimal_View : constant View_T := (others => False);
   --
   -- The smallest possible view.


   Full_View : constant View_T := (
      Real_Feature_T => True,
      others         => False);
   --
   -- Selects the "full" set of features for view.
   -- The features Deeply and Callers are considered secondary options
   -- and omitted. The feature Full is a meta-item and omitted.


   function Some_Content (View : View_T) return Boolean;
   --
   -- Whether the View selects some real features.
   -- The features Deeply and Callers are not considered such.


   --
   ---  View by reference
   --


   type Seq_Number_T is new Natural;
   --
   -- When a set of execution bounds is shown, they are numbered
   -- sequentially from 1. If, later, the same execution bounds
   -- object is referenced, it is not shown again, but just a
   -- reference to the earlier sequence number is given.
   --
   -- The sequence number zero means that the execution bounds have
   -- not yet been shown.


   Not_Shown : constant Seq_Number_T := 0;
   --
   -- The sequence number zero means that the execution bounds have
   -- not yet been shown.


   type Seq_Index_T is array (Bounds_Index_T range <>) of Seq_Number_T;
   --
   -- A mapping from execution bounds indices to the sequence number
   -- where those bounds were shown, or Not_Shown.


   type Sequence_T (Max_Size : Bounds_Count_T)
   is record
      Next  : Seq_Number_T := 1;
      Index : Seq_Index_T (1 .. Max_Size);
   end record;
   --
   -- Records the output of a set of execution bounds, with a
   -- sequential numbering (Next gives the next sequence number) and
   -- a mapping from the execution-bounds index to the sequence
   -- number it was given in the output sequence.


   procedure Show (
      Call_Path : in     Call_Path_T;
      Bounds    : in     Bounds_Set_T;
      Margin    : in     Ada.Text_IO.Positive_Count;
      View      : in     View_T;
      Sequence  : in out Sequence_T);
   --
   -- Shows the Bounds for the Call_Path, according to the View, with
   -- sequential numbering by the Sequence and indentation to the
   -- left Margin. If the Bounds were already shown, just refers to the
   -- corresponding sequence number.


   procedure Show (
      Calls  : in Call_List_T;
      Bounds : in Bounds_Set_T;
      View   : in View_T);
   --
   -- Shows each of the calls, as if they were root calls.
   --
   -- Shows each execution bounds object only once; if it occurs
   -- again, refers to earlier occurrence.


   procedure Show (
      Bounds   : in     Bounds_Ref;
      Place    : in     Call_Path_T;
      View     : in     View_T;
      Margin   : in     Ada.Text_IO.Positive_Count;
      Sequence : in out Sequence_T);
   --
   -- Shows the Bounds with sequential numbering by the Sequence,
   -- heading according to the Place in a bounds hierarchy, and
   -- indentation to the left Margin. If the Bounds were already
   -- shown, just refers to the corresponding sequence number.


   --
   ---   Show maximal stack paths
   --


   procedure Show_Stack_Path (
      Root_Call : Call_T;
      Within    : Bounds_Set_T);
   --
   -- Output the path in the call tree that creates the highest stack.
   -- For each subprogram report its own stack usage and the stack
   -- from this call down.


   --
   ---   Show unbounded parts
   --


   procedure Show_Unbounded (
      Bounds  : in Bounds_Ref;
      Within  : in Bounds_Set_T;
      Path    : in Call_Path_T;
      Callers : in Boolean);
   --
   -- Shows the unbounded aspects of the given execution bounds and
   -- recursively for the nested bounds on callees.
   --
   -- All (unbounded) bounds are shown only once.
   --
   -- Bounds
   --    The (top) bounds to be shown.
   -- Within
   --    The set of all execution bounds (or at least those in Bounds
   --    and its nested bounds).
   -- Path
   --    The initial (top-level) call-path to be displayed as the
   --    path to these Bounds. Whether this is the true call-path
   --    is the responsibility of the caller.
   -- Callers
   --    Whether to show the callers (inverse call tree) for any
   --    subprogram found to have unbounded basic parts (eg. loops).


end Programs.Execution.Show;
