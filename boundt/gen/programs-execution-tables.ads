-- Programs.Execution.Tables (decl)
--
-- Tabulating program execution bounds and their parts, as one way of
-- displaying the results of the analysis.
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
-- $Date: 2015/10/24 19:36:51 $
--
-- $Log: programs-execution-tables.ads,v $
-- Revision 1.4  2015/10/24 19:36:51  niklas
-- Moved to free licence.
--
-- Revision 1.3  2005-10-09 08:10:23  niklas
-- BT-CH-0013.
--
-- Revision 1.2  2005/09/23 10:51:41  niklas
-- BT-CH-0012.
--
-- Revision 1.1  2004/05/01 09:56:09  niklas
-- First version.
--


package Programs.Execution.Tables is


   --
   --    Time-table structures
   --


   type Time_Range_T is record
      Min : Processor.Time_T;
      Max : Processor.Time_T;
   end record;
   --
   -- A range of execution time (bounds), Min .. Max.
   -- If Min > Max, the range is empty and the specific values of
   -- Min and Max are irrelevant (logically undefined).


   No_Time_Range : constant Time_Range_T := (
      Min => Processor.Time_T'Last,
      Max => Processor.Time_T'First);
   --
   -- Represents an undefined (unconstrained) time range.


   function Is_Null (Item : Time_Range_T) return Boolean;
   --
   -- Whether the time-range is empty (Min > Max).


   type Share_T is record
      Link_Paths     : Natural;
      Counted_Paths  : Natural;
      Calls          : Natural;
      Timed_Paths    : Natural;
      Asserted_Paths : Natural;
      Time           : Time_Range_T;
      Time_Per_Call  : Time_Range_T;
      Bounded_Paths  : Natural;
      Bounded_Calls  : Natural;
      Total_Time     : Processor.Time_T;
   end record; 
   --
   -- Details the way in which some executions of a given subprogram
   -- form a share or portion of the execution of a root subprogram.
   --
   -- Link_Paths
   --    The number of link-paths that lead to these executions of
   --    the subprogram. A link-path is a path that starts from the
   --    execution bounds for the root subprogram and goes through
   --    a chain of caller-callee links (calls) between execution
   --    bounds until it reaches bounds for the given subprogram.
   -- Counted_Paths
   --    The number of link-paths (part of Link_Paths) where the
   --    execution count of the call is bounded (through the execution
   --    bounds of the callers on the path). The execution of the
   --    final callee (the given subprogram) is not necessarily
   --    bounded. These link-paths contribute to the Calls component.
   -- Calls
   --    The total execution count of the (bounded) link-paths,
   --    that is, of the link-paths counted in Counted_Paths.
   --    Zero if Counted_Paths is zero.
   -- Timed_Paths
   --    The number of link-paths (part of Link_Paths) where the
   --    execution time of the final callee (the given subprogram) is
   --    bounded. The execution count of the call is not necessarily
   --    bounded. These link-paths contribute to Time always, and to
   --    to Time_Per_Call except if the execution count is bounded to
   --    zero.
   -- Asserted_Paths
   --    The number of link-paths where the execution time of the
   --    final callee (the given subprogram) is asserted. This is a
   --    part of Timed_Paths.
   -- Time
   --    The range of callee execution time bounds, over the link-
   --    paths where such bounds exist, whether or not they are
   --    included in the overall worst-case execution. That is, the
   --    link-paths counted in Timed_Paths, whatever their execution
   --    count.
   -- Time_Per_Call
   --    The range of callee execution time bounds, over the link-
   --    paths where such bounds exist and might be included in the
   --    worst-case execution, that is, the link-paths counted
   --    in Timed_Paths with a positive or unbounded execution count.
   -- Bounded_Paths
   --    The number of link-paths (part of Link_Paths) where both the
   --    execution count of the call, and the execution time of the
   --    callee, are bounded. These paths contribute to Bounded_Calls
   --    and Total_Time.
   -- Bounded_Calls
   --    The total number of calls from link-paths that are counted in
   --    Bounded_Paths. These calls make up Total_Time. Zero if
   --    Bounded_Paths is zero.
   -- Total_Time
   --    The total execution time of the callee over all Bounded_Calls,
   --    giving the (known) share of these executions in the
   --    execution-time bound of the root subprogram.
   --
   -- A share_T is often displayed as follows:
   --
   --    Calls "calls from" Counted_Paths "of" Link_Paths
   --    "time" Total_Time "=" Bounded_Calls "*" Time_Per_Call
   --
   -- The number of Timed_Paths and Bounded_Paths is not shown.


   No_Share : constant Share_T := (
      Link_Paths     => 0,
      Counted_Paths  => 0,
      Calls          => 0,
      Timed_Paths    => 0,
      Asserted_Paths => 0,
      Time           => No_Time_Range,
      Time_Per_Call  => No_Time_Range,
      Bounded_Paths  => 0,
      Bounded_Calls  => 0,
      Total_Time     => 0);
   --
   -- A null share.


   --
   --    Tables of subprograms and their bounds
   --


   type Subprogram_Row_T is record
      Subprogram : Subprogram_T;
      Share      : Share_T;
      Self_Time  : Processor.Time_T;
   end record;
   --
   -- Summary information for all the executions of one subprogram in
   -- the closure of a root subprogram.
   --
   -- Subprogram
   --    The subprogram to which the execution-time bounds apply.
   -- Share
   --    The share of these executions, within the root execution.
   -- Self Time
   --    The execution time bound that is consumed specifically by
   --    this Subprogram as part of Share.Total_Time but omitting
   --    the execution of any lower-level callees of this Subprogram.
   --    In other words, the Total Time of this Subprogram minus the
   --    Total Times of its direct callees.


   type Subprogram_Table_T is array (Positive range <>) of Subprogram_Row_T;
   -- 
   -- A tabulation of execution time bounds, usually containing all
   -- the subprograms in the closure of a given root subprogram (or
   -- given execution bounds for the root).
   --
   -- The index is in principle arbitrary, but in practice represents
   -- the chosen sorting of the table (top down, by time, by number of
   -- calls etc.).


   type Call_Row_T is record
      Caller : Subprogram_T;
      Callee : Subprogram_T;
      Share  : Share_T;
   end record;
   --
   -- Summary information for all the calls from the Caller to the Callee,
   -- within the closure of one root subprogram.
   --
   -- Caller
   --    The calling subprogram.
   -- Callee
   --    The called subprogram.
   -- Share
   --    The share of all these calls (executions of the Callee),
   --    within the execution of the root.


   type Call_Table_T is array (Positive range <>) of Call_Row_T;
   --
   -- A tabulation of the execution-time bounds for calls within the
   -- closure of a root subprogram (or given execution bounds for the
   -- root).
   --
   -- The indexing is in principle arbitrary, but in practice represents
   -- the chosen sorting of the table (top down, by time, by number of
   -- calls etc.).


   function Subprogram_Table (Root : Bounds_Ref)
   return Subprogram_Table_T;
   --
   -- A tabulation of the execution-time bounds, showing how the Root
   -- bounds are composed of the bounds on the callees, assuming that
   -- Root and all its callees are bounded for execution time.
   --
   -- The table has one row for Root and one row for each subprogram
   -- called from Root (as represented in the Root bounds), even for
   -- those subprograms that are not actually called on the worst-case
   -- path (call count = 0).


   function Call_Table (Root : Bounds_Ref)
   return Call_Table_T;
   --
   -- A tabulation of the execution-time bounds from the point of
   -- view of the calls that occur in the closure of the Root
   -- subprogram.
   --
   -- The table has one row for each caller-callee relationship in
   -- the closure of the Root, even those that are not on the
   -- worst-case path (count = 0).


   procedure Tabulate_Subprogram_Bounds (
      Root      : in Bounds_Ref;
      Qualified : in Boolean);
   --
   -- Computes the Subprogram_Table (see above) of the Root bounds and
   -- emits it as basic output lines with the key "Time_Table", the locus
   -- of the Root subprogram, and the data fields described below.
   --
   -- The table has one row for Root and one row for each subprogram
   -- called from Root (as represented in the Root bounds) with a positive
   -- number of calls on the worst-case path.
   --
   -- Each row has the following data fields (columns), from left to right:
   --
   -- Total Time
   --    See Subprogram_Row_T.Share.Total_Time.
   -- Self Time
   --    See Subprogram_Row_T.Self_Time.
   -- Calls
   --    See Subprogram_Row_T.Share.Calls.
   -- Time Per Call
   --    See Subprogram_Row_T.Share.Time Per Call.
   -- Subprogram
   --    The name of the subprogram (Subprogram_Row_T.Subprogram),
   --    optionall fully qualified by its containing scope (when
   --    Qualified is True) or without qualification (otherwise).
   -- Locus
   --    The source-file location of the Subprogram, consisting of the
   --    source-file name (as one data field) and the source-line number
   --    range and code address range (as a second data field).


   function Subprograms_Under (
      Root   : Bounds_Ref;
      Within : Bounds_Set_T)
   return Subprogram_List_T;
   --
   -- All the subprograms in the call-closure of the given Root
   -- subprogram, under the given bounds, including the Root itself.
   --
   -- The order of subprograms in the result is undefined.


   --
   --    Tables of execution bounds for a single subprogram
   --


   type Bounds_Row_T is record
      Bounds    : Bounds_Ref;
      Share     : Share_T;
      Self_Time : Processor.Time_T;
   end record;
   --
   -- Execution bounds for a subprogram and the share that thesse
   -- bounds have in the closure of some root subprogram.
   --
   -- Bounds
   --    The execution bounds themselves.
   -- Share
   --    The share that the execution of these Bounds have, in
   --    the execution of the root.
   -- Self_Time
   --    The execution time bound that is consumed specifically by
   --    Subprogram (Bounds) as part of Share.Total_Time but omitting
   --    the execution of any lower-level callees of this subprogram
   --    as linked to these Bounds.


   No_Bounds_Row : constant Bounds_Row_T := (
      Bounds    => No_Bounds,
      Share     => No_Share,
      Self_Time => 0);
   --
   -- Represents the absence of a Bounds_Row value.


   type Bounds_Table_T is array (Positive range <>) of Bounds_Row_T;
   --
   -- A list of execution bounds for a subprogram (usually the same one
   -- in all the listed bounds) together with a count of the number of
   -- executions of each bounds, and the ratio of links to the bounds
   -- with bounded vs unbounded execution count.


   function All_Bounds_For (
      Subprogram : Subprogram_T;
      Under_Root : Bounds_Ref;
      Within     : Bounds_Set_T)
   return Bounds_Table_T;
   --
   -- All the execution bounds for the given Subprogram, as used
   -- within the call-closure of the given Root bounds, giving
   -- also the share of these bounds in the Root bounds. The result
   -- includes all bounds, even those that are executed zero times
   -- (that is, not used at all on any worst-case execution path).
   --
   -- The order of bounds in the result is undefined.


   function Min_Time (Bounds : Bounds_Table_T)
   return Bounds_Row_T;
   --
   -- The execution bounds with the smallest (known) worst-case time bound,
   -- including the count for these bounds as listed.
   -- Returns No_Count_Bounds if there are no Bounds with a bounded time.


   function Max_Time (Bounds : Bounds_Table_T)
   return Bounds_Row_T;
   --
   -- The execution bounds with the largest (known) worst-case time bound,
   -- including the count for these bounds as listed.
   -- Returns No_Count_Bounds if there are no Bounds with a bounded time.


   type Count_Range_T is record
      Min : Flow.Execution.Count_T;
      Max : Flow.Execution.Count_T;
   end record;
   --
   -- A range of execution counts Min .. Max. For example, this might
   -- summaries the execution counts of a given node (or edge) over a
   -- set of (context-dependent) execution bounds for the same
   -- subprogram.


   No_Count_Range : constant Count_Range_T := (
      Min => Flow.Execution.Count_T'Last,
      Max => Flow.Execution.Count_T'First);
   --
   -- Represents an undefined (unconstrained) range of execution counts.


   function Is_Null (Item : Count_Range_T) return Boolean;
   --
   -- Whether the count-range is empty (Min > Max).


   type Total_Time_Count_T is record
      Time        : Time_Range_T;
      Self        : Time_Range_T;
      Callees     : Time_Range_T;
      Count       : Count_Range_T;
      Total_Count : Flow.Execution.Count_T;
      Total_Time  : Processor.Time_T;
      Total_Self  : Processor.Time_T;
   end record;
   --
   -- A summary of the execution times and counts and the total execution
   -- time of a node or edge, over a set of execution bounds for the same
   -- subprogram, taken from some Bounds_Table_T.
   --
   -- Time
   --    The range of execution times of this node or edge, including
   --    time spent in callees, over all the summarised bounds where
   --    this time is known (bounded). Includes execution bounds that
   --    are never called, or with an unknown (unbounded) number of
   --    calls, or in which this node or edge is not executed, or has
   --    an unknown (unbounded) number of executions.
   -- Self
   --    Same as Time, but excluding time spent in callees.
   -- Callees
   --    The range of execution times for the time spent in callees
   --    called from this node (0..0 for edges).
   -- Count
   --    The the range of execution counts of this node or edge
   --    over all the execution bounds that are summarised here.
   --    Includes execution bounds that are never called or that have
   --    an unknown (unbounded) number of calls.
   -- Total_Count
   --    The total execution count: the sum of the execution count from
   --    each of the execution bounds, times the number of calls of these
   --    bounds. Includes only execution bounds for which some number of
   --    calls is known (bounded).
   -- Total_Time
   --    The total execution time, including time spent in callees
   --    (callees are relevant only for nodes, of course). Computed in
   --    the same was as Total_Count, using the same subset of the
   --    summarised execution bounds.
   -- Total_Self
   --    The total execution time, omitting time spent in callees.
   --    For an edge (or a node that has no calls) this equals the
   --    Total_Time.
   --
   -- The total execution time spent in callees can be computed as
   -- the difference Total_Time - Total_Self.


   No_Time_Count : constant Total_Time_Count_T := (
      Time        => No_Time_Range,
      Self        => No_Time_Range,
      Callees     => No_Time_Range,
      Count       => No_Count_Range,
      Total_Count => 0,
      Total_Time  => 0,
      Total_Self  => 0);
   --
   -- Represents an undefined (blank) summary.


   type Totals_Per_Node_T is
      array (Flow.Node_Index_T range <>) of Total_Time_Count_T;
   --
   -- Summary execution count and total time for all the nodes in a
   -- flow-graph.


   type Totals_Per_Edge_T is
      array (Flow.Edge_Index_T range <>) of Total_Time_Count_T;
   --
   -- Summary execution count and total time for all the edges in a
   -- flow-graph.


   type Total_Times_And_Counts_T (
      Nodes : Flow.Node_Index_T;
      Edges : Flow.Edge_Count_T)
   is record
      Subprogram : Subprogram_T;
      Share      : Share_T;
      Self_Time  : Processor.Time_T;
      Bounded    : Natural;
      Node       : Totals_Per_Node_T (1 .. Nodes);
      Edge       : Totals_Per_Edge_T (1 .. Edges);
   end record;
   --
   -- Summary execution count and total time for all the nodes and
   -- edges in the flow-graph of a Subprogram, as part of the
   -- execution of the call-closure of a root subprogram. The summary
   -- is computed from a set of Bounds_Row_Ts.
   --
   -- Subprogram
   --    The subprogram for which execution bounds are summarised.
   -- Share
   --    The share that the summarised Bounds_Row_Ts represent,
   --    in the execution of the root subprogram.
   -- Self_Time
   --    The execution time bound that is consumed specifically by the
   --    Subprogram itself, as part of Share.Total_Time, but omitting
   --    the execution of any lower-level callees of this subprogram
   --    as linked to these summarised bounds.
   -- Bounded
   --    The number of summarised Bound_Row_Ts that have some bounded
   --    properties without having an asserted execution time.
   -- Node, Edge
   --    The summarised execution counts and times per node and edge,
   --    for all summarised bounds that bound these properties.


   function Total_Times_And_Counts (
      Executing : Subprogram_T;
      Bounds    : Bounds_Table_T)
   return Total_Times_And_Counts_T;
   --
   -- Summarises a table of execution bounds (Bounds_Row_T), all for
   -- Executing the same subprogram.
   --
   -- A Fault message results if some of the Bounds are not for the
   -- given Executing subprogram.


   --
   --    Tables of execution bounds and links between bounds
   --
   -- One execution bound is represented by the type Bounds_Row_T.
   -- The set of links between two execution bounds (caller and callee)
   -- is represented by the type Links_Row_T.


   type Links_Row_T is record
      Caller : Bounds_Ref;
      Callee : Bounds_Ref;
      Share  : Share_T;
   end record;
   --
   -- A summary of the links between all the links from the Caller
   -- bounds to the Callee bounds, showing the total share that
   -- these executions of the Callee have in the execution bounds
   -- of some root subprogram (that is, in some number of executions
   -- of the Caller bounds, this number not being given here).


   type Links_Table_T is array (Positive range <>) of Links_Row_T;
   --
   -- A table of links, usually all the links for a given bounding of
   -- a caller subprogram. The order is not defined.


   function Bounds_Table (
      Root   : Bounds_Ref;
      Within : Bounds_Set_T)
   return Bounds_Table_T;
   --
   -- All the execution bounds used (linked) in the Root bounds.
   -- The Root itself is included. The order is not defined.


   function Links (From : Bounds_Row_T)
   return Links_Table_T;
   --
   -- A table of all the links From the bounds of a caller subprogram
   -- to the linked bounds of its callees, with all links to the same
   -- callee bounds summarised to one Links_Row_T.


end Programs.Execution.Tables;
